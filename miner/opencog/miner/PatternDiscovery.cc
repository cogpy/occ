/*
 * PatternDiscovery.cc
 *
 * Copyright (C) 2024 OpenCog Foundation
 *
 * Author: OpenCog AI
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License v3 as
 * published by the Free Software Foundation and including the exceptions
 * at http://opencog.org/wiki/Licenses
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program; if not, write to:
 * Free Software Foundation, Inc.,
 * 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
 */

#include "PatternDiscovery.h"
#include "MinerLogger.h"
#include "MinerUtils.h"

#include <opencog/util/algorithm.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <opencog/atoms/core/LambdaLink.h>
#include <opencog/atoms/core/PresentLink.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/ValueFactory.h>
#include <opencog/atomspace/AtomSpace.h>

#include <algorithm>
#include <chrono>
#include <sstream>

namespace opencog
{

PatternDiscovery::PatternDiscovery(const PatternDiscoveryConfig& config)
    : config_(config), temp_atomspace_(createAtomSpace())
{
    validate_config();
    initialize_miner();
    initialize_surprisingness();
    
    // Log initialization
    miner_logger().info("PatternDiscovery initialized with min_support=%d, max_depth=%d",
                       config_.minimum_support, config_.max_depth);
}

PatternDiscoveryResults PatternDiscovery::discover_patterns(const AtomSpace& atomspace)
{
    auto start_time = std::chrono::high_resolution_clock::now();
    
    miner_logger().info("Starting pattern discovery on AtomSpace with %zu atoms",
                       atomspace.get_size());
    
    PatternDiscoveryResults results;
    
    try {
        // Extract atoms from the AtomSpace
        HandleSeq atoms;
        atomspace.get_handles_by_type(atoms, opencog::ATOM, true);
        
        results = discover_patterns(atoms);
        
        // Update statistics
        results.statistics.atoms_processed = atoms.size();
        
    } catch (const std::exception& e) {
        miner_logger().error("Error in pattern discovery: %s", e.what());
        throw;
    }
    
    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time);
    results.statistics.discovery_time_seconds = duration.count() / 1000.0;
    
    miner_logger().info("Pattern discovery completed. Found %zu patterns in %.2f seconds",
                       results.total_patterns_found, results.statistics.discovery_time_seconds);
    
    return results;
}

PatternDiscoveryResults PatternDiscovery::discover_patterns(const HandleSeq& atoms)
{
    auto start_time = std::chrono::high_resolution_clock::now();
    
    miner_logger().info("Starting pattern discovery on %zu atoms", atoms.size());
    
    PatternDiscoveryResults results;
    
    try {
        // Preprocess the database
        HandleSeq processed_db = preprocess_database(atoms);
        log_discovery_progress("Preprocessing", processed_db.size());
        
        // Run the miner
        log_discovery_progress("Mining patterns");
        results.pattern_tree = (*miner_)(processed_db);
        
        // Extract patterns from the tree structure
        HandleSeq discovered_patterns = extract_patterns_from_tree(results.pattern_tree);
        results.total_patterns_found = discovered_patterns.size();
        
        log_discovery_progress("Evaluating surprisingness", discovered_patterns.size());
        
        // Calculate pattern statistics
        calculate_pattern_statistics(results, discovered_patterns, processed_db);
        
        results.statistics.atoms_processed = atoms.size();
        results.statistics.specializations_evaluated = discovered_patterns.size();
        
    } catch (const std::exception& e) {
        miner_logger().error("Error in pattern discovery: %s", e.what());
        throw;
    }
    
    auto end_time = std::chrono::high_resolution_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::milliseconds>(end_time - start_time);
    results.statistics.discovery_time_seconds = duration.count() / 1000.0;
    
    miner_logger().info("Pattern discovery completed. Found %zu patterns in %.2f seconds",
                       results.total_patterns_found, results.statistics.discovery_time_seconds);
    
    return results;
}

PatternDiscoveryResults PatternDiscovery::discover_temporal_patterns(const AtomSpace& atomspace, 
                                                                   int temporal_lag)
{
    miner_logger().info("Starting temporal pattern discovery with lag=%d", temporal_lag);
    
    // Create temporal variable to ignore in temporary atomspace
    Handle temporal_var = temp_atomspace_->add_node(VARIABLE_NODE, "$T");
    
    // Update config temporarily to ignore temporal variable
    PatternDiscoveryConfig temp_config = config_;
    temp_config.ignore_variables.push_back(temporal_var);
    
    // Save current config and update
    PatternDiscoveryConfig original_config = config_;
    update_config(temp_config);
    
    try {
        // Discover patterns with temporal constraints
        auto results = discover_patterns(atomspace);
        
        // Restore original configuration
        update_config(original_config);
        
        miner_logger().info("Temporal pattern discovery completed. Found %zu patterns",
                           results.total_patterns_found);
        
        return results;
        
    } catch (const std::exception& e) {
        // Restore original configuration on error
        update_config(original_config);
        throw;
    }
}

HandleSeq PatternDiscovery::filter_by_surprisingness(const HandleSeq& patterns, 
                                                     double min_surprisingness)
{
    miner_logger().debug("Filtering %zu patterns by surprisingness threshold %.3f",
                        patterns.size(), min_surprisingness);
    
    HandleSeq filtered_patterns;
    
    for (const Handle& pattern : patterns) {
        // Get surprisingness score from pattern's truth value or stored value
        // Since we don't have get_surprisingness_key, use a simple approach
        double surprisingness = evaluate_surprisingness(pattern, HandleSeq{});
        if (surprisingness >= min_surprisingness) {
            filtered_patterns.push_back(pattern);
        }
    }
    
    miner_logger().debug("Filtered to %zu patterns meeting surprisingness threshold",
                        filtered_patterns.size());
    
    return filtered_patterns;
}

HandleSeq PatternDiscovery::get_top_patterns(const HandleSeq& patterns, size_t k)
{
    miner_logger().debug("Getting top %zu patterns from %zu candidates", k, patterns.size());
    
    // Create pairs of (pattern, surprisingness) for sorting
    std::vector<std::pair<Handle, double>> pattern_scores;
    
    for (const Handle& pattern : patterns) {
        double score = evaluate_surprisingness(pattern, HandleSeq{});
        pattern_scores.emplace_back(pattern, score);
    }
    
    // Sort by surprisingness score (descending)
    std::sort(pattern_scores.begin(), pattern_scores.end(),
              [](const auto& a, const auto& b) {
                  return a.second > b.second;
              });
    
    // Extract top-k patterns
    HandleSeq top_patterns;
    size_t count = std::min(k, pattern_scores.size());
    
    for (size_t i = 0; i < count; ++i) {
        top_patterns.push_back(pattern_scores[i].first);
    }
    
    miner_logger().debug("Selected top %zu patterns", top_patterns.size());
    
    return top_patterns;
}

double PatternDiscovery::evaluate_surprisingness(const Handle& pattern, const HandleSeq& db)
{
    try {
        // Use the Surprisingness static methods
        return Surprisingness::isurp_old(pattern, db, true);
    } catch (const std::exception& e) {
        miner_logger().error("Error evaluating surprisingness for pattern: %s", e.what());
        return 0.0;
    }
}

void PatternDiscovery::update_config(const PatternDiscoveryConfig& new_config)
{
    config_ = new_config;
    validate_config();
    
    // Reinitialize miner with new config
    initialize_miner();
    
    miner_logger().info("Configuration updated: min_support=%d, max_depth=%d",
                       config_.minimum_support, config_.max_depth);
}

const PatternDiscoveryConfig& PatternDiscovery::get_config() const
{
    return config_;
}

std::string PatternDiscovery::export_patterns_as_scheme(const HandleSeq& patterns)
{
    std::ostringstream oss;
    oss << "(List\n";
    
    for (size_t i = 0; i < patterns.size(); ++i) {
        oss << "  ";  // Indent
        oss << patterns[i]->to_short_string();
        if (i < patterns.size() - 1) {
            oss << "\n";
        }
    }
    
    oss << ")\n";
    return oss.str();
}

std::string PatternDiscovery::export_patterns_as_json(const HandleSeq& patterns)
{
    std::ostringstream oss;
    oss << "{\n";
    oss << "  \"patterns\": [\n";
    
    for (size_t i = 0; i < patterns.size(); ++i) {
        oss << "    {\n";
        oss << "      \"pattern\": \"" << patterns[i]->to_short_string() << "\",\n";
        oss << "      \"type\": \"" << nameserver().getTypeName(patterns[i]->get_type()) << "\"\n";
        oss << "    }";
        if (i < patterns.size() - 1) {
            oss << ",";
        }
        oss << "\n";
    }
    
    oss << "  ],\n";
    oss << "  \"total_count\": " << patterns.size() << "\n";
    oss << "}\n";
    
    return oss.str();
}

// Private helper methods

MinerParameters PatternDiscovery::convert_to_miner_params(const PatternDiscoveryConfig& config)
{
    Handle init_pattern = config.initial_pattern;
    if (init_pattern == Handle::UNDEFINED) {
        init_pattern = create_default_initial_pattern();
    }
    
    return MinerParameters(config.minimum_support, 1, init_pattern, config.max_depth);
}

void PatternDiscovery::initialize_miner()
{
    auto miner_params = convert_to_miner_params(config_);
    miner_ = std::make_unique<Miner>(miner_params);
    
    miner_logger().debug("Miner initialized with parameters");
}

void PatternDiscovery::initialize_surprisingness()
{
    try {
        surprisingness_ = std::make_unique<Surprisingness>();
        miner_logger().debug("Surprisingness evaluator initialized");
    } catch (const std::exception& e) {
        miner_logger().error("Failed to initialize surprisingness evaluator: %s", e.what());
        throw;
    }
}

HandleSeq PatternDiscovery::extract_patterns_from_tree(const HandleTree& tree)
{
    HandleSeq patterns;
    
    // Traverse the tree to extract all patterns
    // HandleTree is a tree<Handle>, so we need to iterate through it properly
    for (auto it = tree.begin(); it != tree.end(); ++it) {
        if (*it != Handle::UNDEFINED) {
            patterns.push_back(*it);
        }
    }
    
    miner_logger().debug("Extracted %zu patterns from tree structure", patterns.size());
    
    return patterns;
}

void PatternDiscovery::calculate_pattern_statistics(PatternDiscoveryResults& results,
                                                   const HandleSeq& patterns,
                                                   const HandleSeq& db)
{
    miner_logger().debug("Calculating statistics for %zu patterns", patterns.size());
    
    for (const Handle& pattern : patterns) {
        try {
            // Calculate support
            unsigned support = MinerUtils::support(pattern, db, UINT_MAX);
            results.pattern_support[pattern] = temp_atomspace_->add_node(NUMBER_NODE, std::to_string(support));
            
            // Calculate surprisingness if not already calculated
            double surprisingness = evaluate_surprisingness(pattern, db);
            results.pattern_surprisingness[pattern] = temp_atomspace_->add_node(NUMBER_NODE, std::to_string(surprisingness));
            
            // Store as value in the pattern atom for later retrieval
            // Create FloatValue manually
            ValuePtr surp_value(new FloatValue(std::vector<double>{surprisingness}));
            pattern->setValue(temp_atomspace_->add_node(CONCEPT_NODE, "surprisingness"), surp_value);
            
        } catch (const std::exception& e) {
            miner_logger().warn("Failed to calculate statistics for pattern: %s", e.what());
        }
    }
    
    miner_logger().debug("Pattern statistics calculation completed");
}

Handle PatternDiscovery::create_default_initial_pattern()
{
    // Create the most abstract pattern: Lambda(Variable("$X"), Present(Variable("$X")))
    Handle var = temp_atomspace_->add_node(VARIABLE_NODE, "$X");
    Handle body = temp_atomspace_->add_link(PRESENT_LINK, var);
    return temp_atomspace_->add_link(LAMBDA_LINK, var, body);
}

void PatternDiscovery::validate_config() const
{
    if (config_.minimum_support == 0) {
        throw std::invalid_argument("Minimum support must be greater than 0");
    }
    
    if (config_.max_variables == 0) {
        throw std::invalid_argument("Maximum variables must be greater than 0");
    }
    
    if (config_.surprisingness_measure.empty()) {
        throw std::invalid_argument("Surprisingness measure cannot be empty");
    }
}

HandleSeq PatternDiscovery::preprocess_database(const HandleSeq& db)
{
    // For now, just return the original database
    // Future enhancements could include filtering, cleaning, etc.
    return db;
}

void PatternDiscovery::log_discovery_progress(const std::string& stage, size_t processed)
{
    if (processed > 0) {
        miner_logger().debug("Pattern discovery progress: %s (%zu items processed)", 
                           stage.c_str(), processed);
    } else {
        miner_logger().debug("Pattern discovery progress: %s", stage.c_str());
    }
}

} // namespace opencog