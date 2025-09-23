/*
 * PatternDiscovery.h
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
#ifndef OPENCOG_PATTERN_DISCOVERY_H_
#define OPENCOG_PATTERN_DISCOVERY_H_

#include <opencog/atoms/base/Handle.h>
#include <opencog/atomspace/AtomSpace.h>

#include "Miner.h"
#include "HandleTree.h"
#include "Surprisingness.h"

namespace opencog
{

/**
 * Configuration parameters for PatternDiscovery
 */
struct PatternDiscoveryConfig {
    // Minimum support threshold
    unsigned minimum_support;
    
    // Maximum depth for pattern specialization
    int max_depth;
    
    // Initial pattern (if not specified, uses most abstract pattern)
    Handle initial_pattern;
    
    // Surprisingness measure to use
    std::string surprisingness_measure;
    
    // Enable type constraints in patterns
    bool enable_type_constraints;
    
    // Enable glob nodes for variable arity matching
    bool enable_glob_nodes;
    
    // Variables to ignore during mining
    HandleSeq ignore_variables;
    
    // Maximum number of variables in discovered patterns
    unsigned max_variables;
    
    // Default constructor with sensible defaults
    PatternDiscoveryConfig(unsigned min_support = 10,
                          int max_d = -1,
                          const Handle& init_pat = Handle::UNDEFINED,
                          const std::string& surp_measure = "isurp",
                          bool type_constraints = false,
                          bool glob_nodes = false,
                          const HandleSeq& ignore_vars = {},
                          unsigned max_vars = UINT_MAX)
        : minimum_support(min_support), max_depth(max_d), 
          initial_pattern(init_pat), surprisingness_measure(surp_measure),
          enable_type_constraints(type_constraints), enable_glob_nodes(glob_nodes),
          ignore_variables(ignore_vars), max_variables(max_vars) {}
};

/**
 * Results from pattern discovery operation
 */
struct PatternDiscoveryResults {
    // Discovered patterns organized as a tree structure
    HandleTree pattern_tree;
    
    // Patterns with their surprisingness scores
    HandleMap pattern_surprisingness;
    
    // Patterns with their support counts
    HandleMap pattern_support;
    
    // Total number of patterns discovered
    size_t total_patterns_found;
    
    // Statistics about the discovery process
    struct Statistics {
        double discovery_time_seconds;
        size_t atoms_processed;
        size_t specializations_evaluated;
    } statistics;
    
    PatternDiscoveryResults() : total_patterns_found(0) {
        statistics.discovery_time_seconds = 0.0;
        statistics.atoms_processed = 0;
        statistics.specializations_evaluated = 0;
    }
};

/**
 * High-level pattern discovery system using the OpenCog pattern miner.
 * 
 * This class provides a sophisticated interface for discovering frequent
 * patterns in AtomSpace knowledge bases. It extends the basic Miner
 * functionality with additional features like surprisingness evaluation,
 * configurable constraints, and comprehensive result analysis.
 *
 * Key capabilities:
 * - Automatic pattern discovery from AtomSpace data
 * - Configurable mining parameters and constraints
 * - Surprisingness-based pattern ranking
 * - Support for temporal and structural patterns
 * - Integration with OpenCog reasoning systems
 *
 * Usage example:
 *   PatternDiscoveryConfig config(10); // minimum support = 10
 *   PatternDiscovery discovery(config);
 *   auto results = discovery.discover_patterns(atomspace);
 */
class PatternDiscovery
{
public:
    /**
     * Constructor with configuration
     */
    explicit PatternDiscovery(const PatternDiscoveryConfig& config = PatternDiscoveryConfig());

    /**
     * Discover patterns in the given AtomSpace
     * @param atomspace The AtomSpace to mine patterns from
     * @return PatternDiscoveryResults containing discovered patterns and statistics
     */
    PatternDiscoveryResults discover_patterns(const AtomSpace& atomspace);

    /**
     * Discover patterns from a specific set of atoms
     * @param atoms The collection of atoms to analyze
     * @return PatternDiscoveryResults containing discovered patterns and statistics
     */
    PatternDiscoveryResults discover_patterns(const HandleSeq& atoms);

    /**
     * Discover patterns with temporal constraints
     * @param atomspace The AtomSpace containing temporal data
     * @param temporal_lag The time lag to consider for temporal patterns
     * @return PatternDiscoveryResults with temporal patterns
     */
    PatternDiscoveryResults discover_temporal_patterns(const AtomSpace& atomspace, 
                                                      int temporal_lag = 1);

    /**
     * Filter discovered patterns by surprisingness threshold
     * @param patterns The patterns to filter
     * @param min_surprisingness Minimum surprisingness score
     * @return Filtered set of patterns
     */
    HandleSeq filter_by_surprisingness(const HandleSeq& patterns, 
                                      double min_surprisingness);

    /**
     * Get the top-k most surprising patterns
     * @param patterns The patterns to rank
     * @param k Number of top patterns to return
     * @return Top-k patterns ordered by surprisingness
     */
    HandleSeq get_top_patterns(const HandleSeq& patterns, size_t k);

    /**
     * Evaluate surprisingness of a single pattern
     * @param pattern The pattern to evaluate
     * @param db The database/context for evaluation
     * @return Surprisingness score
     */
    double evaluate_surprisingness(const Handle& pattern, const HandleSeq& db);

    /**
     * Update configuration parameters
     */
    void update_config(const PatternDiscoveryConfig& new_config);

    /**
     * Get current configuration
     */
    const PatternDiscoveryConfig& get_config() const;

    /**
     * Export patterns in various formats
     */
    std::string export_patterns_as_scheme(const HandleSeq& patterns);
    std::string export_patterns_as_json(const HandleSeq& patterns);

private:
    // Configuration parameters
    PatternDiscoveryConfig config_;
    
    // Internal miner instance
    std::unique_ptr<Miner> miner_;
    
    // Surprisingness evaluator
    std::unique_ptr<Surprisingness> surprisingness_;
    
    // Temporary AtomSpace for processing
    AtomSpacePtr temp_atomspace_;

    // Helper methods
    MinerParameters convert_to_miner_params(const PatternDiscoveryConfig& config);
    void initialize_miner();
    void initialize_surprisingness();
    HandleSeq extract_patterns_from_tree(const HandleTree& tree);
    void calculate_pattern_statistics(PatternDiscoveryResults& results, 
                                     const HandleSeq& patterns,
                                     const HandleSeq& db);
    Handle create_default_initial_pattern();
    void validate_config() const;
    HandleSeq preprocess_database(const HandleSeq& db);
    void log_discovery_progress(const std::string& stage, size_t processed = 0);
};

} // namespace opencog

#endif /* OPENCOG_PATTERN_DISCOVERY_H_ */