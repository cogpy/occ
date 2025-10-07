/**
 * PolicyOptimizer.cpp - Implementation of MOSES-based Policy Optimization
 * 
 * Part of AZ-LEARN-003: MOSES Policy Optimization Integration
 * Copyright (C) 2024 OpenCog Foundation
 */

#include <agentzero/learning/PolicyOptimizer.h>
#include <agentzero/learning/LearningUtils.h>

#include <opencog/util/Logger.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/value/FloatValue.h>
#include <opencog/atoms/value/StringValue.h>
#include <opencog/atoms/atom_types/atom_names.h>
#include <opencog/atoms/atom_types/NameServer.h>
#include <limits>

// MOSES includes
#include <moses/moses/moses/moses_main.h>
#include <moses/moses/moses/metapopulation/metapopulation.h>
#include <moses/moses/moses/optimization/hill-climbing.h>
#include <moses/moses/moses/scoring/behave_cscore.h>
#include <moses/comboreduct/combo/combo.h>

#include <chrono>
#include <sstream>
#include <algorithm>

namespace opencog {
namespace agentzero {
namespace learning {

using namespace opencog::combo;
using namespace opencog::moses;

/**
 * Custom fitness evaluator that bridges our PolicyFitnessFunction with MOSES
 */
class PolicyFitnessEvaluator : public opencog::moses::behave_cscore {
public:
    PolicyFitnessEvaluator(std::shared_ptr<PolicyFitnessFunction> fitness_func)
        : fitness_function_(fitness_func), evaluation_count_(0) {
        if (!fitness_func) {
            throw PolicyOptimizationException("Fitness function cannot be null");
        }
    }
    
    behavioral_score operator()(const combo_tree& tr) const override {
        try {
            evaluation_count_++;
            double fitness = fitness_function_->evaluate(tr);
            
            // MOSES expects behavioral scores (higher is better)
            behavioral_score score;
            score.push_back(fitness);
            
            return score;
        } catch (const std::exception& e) {
            logger().error("PolicyOptimizer: Error evaluating fitness: %s", e.what());
            behavioral_score score;
            score.push_back(-1000.0); // Large negative score for error
            return score;
        }
    }
    
    size_t getEvaluationCount() const { return evaluation_count_; }
    void resetEvaluationCount() { evaluation_count_ = 0; }

private:
    std::shared_ptr<PolicyFitnessFunction> fitness_function_;
    mutable std::atomic<size_t> evaluation_count_;
};

PolicyOptimizer::PolicyOptimizer(AtomSpacePtr atomspace, const LearningConfig& config)
    : atomspace_(atomspace), config_(config), optimization_running_(false),
      total_evaluations_(0), total_generations_(0), best_fitness_ever_(-std::numeric_limits<double>::infinity()),
      average_fitness_(0.0) {
    
    if (!atomspace_) {
        throw PolicyOptimizationException("AtomSpace cannot be null");
    }
    
    logger().info("PolicyOptimizer: Initializing with MOSES integration");
    
    // Initialize MOSES parameters
    initializeMOSESParameters();
    
    // Initialize AtomSpace structures
    initializeAtomSpaceStructures();
    
    // Record start time
    optimization_start_time_ = std::chrono::steady_clock::now();
    
    logger().info("PolicyOptimizer: Initialization complete");
}

PolicyOptimizer::~PolicyOptimizer() {
    // Stop continuous optimization if running
    if (optimization_running_) {
        stopContinuousOptimization();
    }
    
    logger().info("PolicyOptimizer: Destroyed");
}

bool PolicyOptimizer::initialize(std::shared_ptr<PolicyFitnessFunction> fitness_function) {
    if (!fitness_function) {
        logger().error("PolicyOptimizer: Cannot initialize with null fitness function");
        return false;
    }
    
    std::lock_guard<std::mutex> lock(optimization_mutex_);
    fitness_function_ = fitness_function;
    
    logger().info("PolicyOptimizer: Initialized with fitness function: %s", 
                  fitness_function_->getName().c_str());
    
    return true;
}

std::shared_ptr<Policy> PolicyOptimizer::evolvePolicy(const PolicyId& policy_id,
                                                     const combo_tree* initial_program,
                                                     size_t max_evaluations) {
    if (!fitness_function_) {
        logger().error("PolicyOptimizer: Cannot evolve policy without fitness function");
        return nullptr;
    }
    
    try {
        logger().info("PolicyOptimizer: Evolving policy '%s'", policy_id.c_str());
        
        // Use provided max_evaluations or default from config
        size_t max_evals = (max_evaluations > 0) ? max_evaluations : config_.max_evals;
        
        // Create MOSES evaluator
        auto evaluator = std::make_unique<PolicyFitnessEvaluator>(fitness_function_);
        
        // Set up MOSES parameters for this run
        moses_parameters moses_params = *moses_params_;
        moses_params.max_evals = max_evals;
        
        // Create initial population
        std::vector<combo_tree> initial_population;
        if (initial_program) {
            initial_population.push_back(*initial_program);
        } else {
            // Create a simple initial program based on input features
            auto input_features = fitness_function_->getInputFeatures();
            combo_tree simple_tree;
            
            if (!input_features.empty()) {
                // Create a simple tree that just returns the first input
                simple_tree = combo_tree(argument(1));
            } else {
                // Create a constant tree
                simple_tree = combo_tree(0.0);
            }
            initial_population.push_back(simple_tree);
        }
        
        // Create type signature (assume real-valued inputs and outputs)
        type_tree type_sig = gen_signature(type_node::lambda_type,
                                         fitness_function_->getInputFeatures().size());
        
        // Set up reduction rules
        rule si_ca = rule::logical_canonical_rules();
        rule si_kb = rule::logical_reduction_rules();
        
        // Create metapopulation
        metapopulation metapop(initial_population, *evaluator, *metapop_params_);
        
        // Create deme expander
        hill_climbing optimizer(*optim_params_, *hc_params_);
        deme_expander dex(type_sig, si_ca, si_kb, *evaluator, optimizer, *deme_params_);
        
        // Run MOSES evolution
        moses_statistics stats;
        
        logger().info("PolicyOptimizer: Starting MOSES evolution for policy '%s'", policy_id.c_str());
        
        size_t generation = 0;
        while (generation < moses_params.max_gens && 
               evaluator->getEvaluationCount() < max_evals &&
               !metapop.empty()) {
            
            // Expand demes
            dex.expand(metapop);
            
            generation++;
            
            // Log progress
            if (generation % 10 == 0 || generation == 1) {
                double best_score = metapop.empty() ? 0.0 : metapop.best_score();
                double avg_score = metapop.empty() ? 0.0 : metapop.avg_score();
                
                logEvolutionProgress(policy_id, generation, best_score, avg_score);
                
                // Update statistics
                updateStats(best_score, evaluator->getEvaluationCount(), generation);
            }
        }
        
        // Get the best evolved program
        if (metapop.empty()) {
            logger().error("PolicyOptimizer: Evolution failed - empty metapopulation");
            return nullptr;
        }
        
        const scored_combo_tree& best_candidate = metapop.best_candidates().front();
        combo_tree best_program = best_candidate.get_tree();
        double best_fitness = best_candidate.get_score();
        
        logger().info("PolicyOptimizer: Evolution completed for policy '%s'. Best fitness: %f", 
                      policy_id.c_str(), best_fitness);
        
        // Create policy object
        auto policy = comboTreeToPolicy(policy_id, best_program, best_fitness);
        
        // Store in cache and AtomSpace
        {
            std::lock_guard<std::mutex> lock(policy_cache_mutex_);
            policy_cache_[policy_id] = policy;
        }
        
        storePolicyInAtomSpace(*policy);
        
        return policy;
        
    } catch (const std::exception& e) {
        logger().error("PolicyOptimizer: Error evolving policy '%s': %s", 
                       policy_id.c_str(), e.what());
        return nullptr;
    }
}

bool PolicyOptimizer::improvePolicy(std::shared_ptr<Policy> policy, size_t max_evaluations) {
    if (!policy) {
        logger().error("PolicyOptimizer: Cannot improve null policy");
        return false;
    }
    
    try {
        logger().info("PolicyOptimizer: Improving policy '%s'", policy->id.c_str());
        
        // Use the existing program as initial seed
        auto improved_policy = evolvePolicy(policy->id + "_improved", 
                                          &policy->program, max_evaluations);
        
        if (!improved_policy || improved_policy->fitness_score <= policy->fitness_score) {
            logger().info("PolicyOptimizer: No improvement found for policy '%s'", policy->id.c_str());
            return false;
        }
        
        // Update the original policy
        policy->program = improved_policy->program;
        policy->fitness_score = improved_policy->fitness_score;
        policy->evaluation_count += improved_policy->evaluation_count;
        policy->program_source = improved_policy->program_source;
        policy->performance_metrics = improved_policy->performance_metrics;
        
        // Update in AtomSpace
        storePolicyInAtomSpace(*policy);
        
        logger().info("PolicyOptimizer: Successfully improved policy '%s'. New fitness: %f", 
                      policy->id.c_str(), policy->fitness_score);
        
        return true;
        
    } catch (const std::exception& e) {
        logger().error("PolicyOptimizer: Error improving policy '%s': %s", 
                       policy->id.c_str(), e.what());
        return false;
    }
}

double PolicyOptimizer::evaluatePolicy(const Policy& policy,
                                      const std::map<std::string, Handle>& context) {
    if (!fitness_function_) {
        logger().error("PolicyOptimizer: Cannot evaluate policy without fitness function");
        return -std::numeric_limits<double>::infinity();
    }
    
    try {
        return fitness_function_->evaluate(policy.program, context);
    } catch (const std::exception& e) {
        logger().error("PolicyOptimizer: Error evaluating policy '%s': %s", 
                       policy.id.c_str(), e.what());
        return -std::numeric_limits<double>::infinity();
    }
}

Handle PolicyOptimizer::storePolicyInAtomSpace(const Policy& policy) {
    try {
        // Create or get policy node
        Handle policy_node = atomspace_->add_node(CONCEPT_NODE, 
            config_.policy_atom_prefix + policy.id);
        
        // Store program as string value
        std::string program_str = utils::comboTreeToString(policy.program);
        policy_node->setValue(atomspace_->add_node(PREDICATE_NODE, constants::PROGRAM_VALUE_KEY),
                             createStringValue(program_str));
        
        // Store fitness score
        policy_node->setValue(atomspace_->add_node(PREDICATE_NODE, constants::FITNESS_VALUE_KEY),
                             createFloatValue(std::vector<double>{policy.fitness_score}));
        
        // Store metadata
        HandleSeq metadata_atoms;
        
        // Input features
        for (const auto& feature : policy.input_features) {
            metadata_atoms.push_back(atomspace_->add_node(CONCEPT_NODE, "input_" + feature));
        }
        
        // Output type
        if (!policy.output_type.empty()) {
            metadata_atoms.push_back(atomspace_->add_node(CONCEPT_NODE, "output_" + policy.output_type));
        }
        
        // Create metadata link
        if (!metadata_atoms.empty()) {
            Handle metadata_link = atomspace_->add_link(LIST_LINK, metadata_atoms);
            atomspace_->add_link(EVALUATION_LINK, {
                atomspace_->add_node(PREDICATE_NODE, "PolicyMetadata"),
                policy_node,
                metadata_link
            });
        }
        
        logger().debug("PolicyOptimizer: Stored policy '%s' in AtomSpace", policy.id.c_str());
        
        return policy_node;
        
    } catch (const std::exception& e) {
        logger().error("PolicyOptimizer: Error storing policy '%s' in AtomSpace: %s", 
                       policy.id.c_str(), e.what());
        return Handle::UNDEFINED;
    }
}

std::shared_ptr<Policy> PolicyOptimizer::retrievePolicyFromAtomSpace(const PolicyId& policy_id) {
    try {
        Handle policy_node = atomspace_->get_node(CONCEPT_NODE, 
            config_.policy_atom_prefix + policy_id);
        
        if (policy_node == Handle::UNDEFINED) {
            return nullptr;
        }
        
        return atomSpaceRepresentationToPolicy(policy_node);
        
    } catch (const std::exception& e) {
        logger().error("PolicyOptimizer: Error retrieving policy '%s' from AtomSpace: %s", 
                       policy_id.c_str(), e.what());
        return nullptr;
    }
}

std::vector<std::shared_ptr<Policy>> PolicyOptimizer::getAllPolicies() {
    std::vector<std::shared_ptr<Policy>> policies;
    
    try {
        // Get all policy nodes from AtomSpace
        HandleSeq policy_nodes;
        atomspace_->get_handles_by_type(policy_nodes, CONCEPT_NODE);
        
        for (Handle node : policy_nodes) {
            std::string node_name = node->get_name();
            if (node_name.find(config_.policy_atom_prefix) == 0) {
                auto policy = atomSpaceRepresentationToPolicy(node);
                if (policy) {
                    policies.push_back(policy);
                }
            }
        }
        
        // Also include cached policies
        std::lock_guard<std::mutex> lock(policy_cache_mutex_);
        for (const auto& pair : policy_cache_) {
            // Avoid duplicates
            bool found = false;
            for (const auto& existing : policies) {
                if (existing->id == pair.first) {
                    found = true;
                    break;
                }
            }
            if (!found) {
                policies.push_back(pair.second);
            }
        }
        
    } catch (const std::exception& e) {
        logger().error("PolicyOptimizer: Error getting all policies: %s", e.what());
    }
    
    return policies;
}

void PolicyOptimizer::setFitnessFunction(std::shared_ptr<PolicyFitnessFunction> fitness_function) {
    std::lock_guard<std::mutex> lock(optimization_mutex_);
    fitness_function_ = fitness_function;
    
    if (fitness_function_) {
        logger().info("PolicyOptimizer: Set new fitness function: %s", 
                      fitness_function_->getName().c_str());
    }
}

std::shared_ptr<PolicyFitnessFunction> PolicyOptimizer::getFitnessFunction() const {
    return fitness_function_;
}

bool PolicyOptimizer::startContinuousOptimization(std::shared_ptr<LearningEventCallback> callback) {
    if (optimization_running_) {
        logger().warn("PolicyOptimizer: Continuous optimization already running");
        return false;
    }
    
    if (!fitness_function_) {
        logger().error("PolicyOptimizer: Cannot start continuous optimization without fitness function");
        return false;
    }
    
    optimization_callback_ = callback;
    optimization_running_ = true;
    
    optimization_thread_ = std::make_unique<std::thread>(&PolicyOptimizer::continuousOptimizationLoop, this);
    
    logger().info("PolicyOptimizer: Started continuous optimization");
    return true;
}

void PolicyOptimizer::stopContinuousOptimization() {
    if (!optimization_running_) {
        return;
    }
    
    optimization_running_ = false;
    optimization_cv_.notify_all();
    
    if (optimization_thread_ && optimization_thread_->joinable()) {
        optimization_thread_->join();
    }
    
    optimization_thread_.reset();
    
    logger().info("PolicyOptimizer: Stopped continuous optimization");
}

bool PolicyOptimizer::isContinuousOptimizationRunning() const {
    return optimization_running_;
}

void PolicyOptimizer::updateConfig(const LearningConfig& config) {
    std::lock_guard<std::mutex> lock(optimization_mutex_);
    config_ = config;
    initializeMOSESParameters(); // Reinitialize with new config
    
    logger().info("PolicyOptimizer: Configuration updated");
}

const LearningConfig& PolicyOptimizer::getConfig() const {
    return config_;
}

std::map<std::string, double> PolicyOptimizer::getOptimizationStats() const {
    std::lock_guard<std::mutex> lock(stats_mutex_);
    
    auto now = std::chrono::steady_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::seconds>(now - optimization_start_time_);
    
    std::map<std::string, double> stats;
    stats["total_evaluations"] = static_cast<double>(total_evaluations_);
    stats["total_generations"] = static_cast<double>(total_generations_);
    stats["best_fitness_ever"] = best_fitness_ever_;
    stats["average_fitness"] = average_fitness_;
    stats["optimization_time_seconds"] = static_cast<double>(duration.count());
    stats["policies_in_cache"] = static_cast<double>(policy_cache_.size());
    
    return stats;
}

void PolicyOptimizer::resetStats() {
    std::lock_guard<std::mutex> lock(stats_mutex_);
    
    total_evaluations_ = 0;
    total_generations_ = 0;
    best_fitness_ever_ = -std::numeric_limits<double>::infinity();
    average_fitness_ = 0.0;
    optimization_start_time_ = std::chrono::steady_clock::now();
    
    logger().info("PolicyOptimizer: Statistics reset");
}

// Private method implementations

void PolicyOptimizer::initializeMOSESParameters() {
    // Initialize MOSES parameters from config
    moses_params_ = std::make_unique<moses_parameters>();
    moses_params_->max_evals = config_.max_evals;
    moses_params_->max_gens = config_.max_gens;
    moses_params_->local = true; // Use local MOSES (not distributed)
    
    metapop_params_ = std::make_unique<metapop_parameters>();
    metapop_params_->diversity_pressure = config_.diversity_pressure;
    metapop_params_->cap_coef = 50; // Capacity coefficient
    
    deme_params_ = std::make_unique<deme_parameters>();
    deme_params_->pop_size = config_.population_size;
    
    optim_params_ = std::make_unique<optim_parameters>();
    optim_params_->opt_algo = hc; // Hill climbing
    
    hc_params_ = std::make_unique<hc_parameters>();
    
    logger().debug("PolicyOptimizer: MOSES parameters initialized");
}

std::shared_ptr<Policy> PolicyOptimizer::comboTreeToPolicy(const PolicyId& policy_id,
                                                          const combo_tree& tree,
                                                          double fitness) {
    auto policy = std::make_shared<Policy>(policy_id, Handle::UNDEFINED, tree);
    policy->fitness_score = fitness;
    policy->evaluation_count = 1;
    policy->program_source = utils::comboTreeToString(tree);
    
    if (fitness_function_) {
        policy->input_features = fitness_function_->getInputFeatures();
        policy->output_type = "action"; // Default output type
    }
    
    return policy;
}

std::shared_ptr<Policy> PolicyOptimizer::atomSpaceRepresentationToPolicy(Handle policy_atom) {
    try {
        std::string node_name = policy_atom->get_name();
        std::string policy_id = node_name.substr(config_.policy_atom_prefix.length());
        
        // Get program string
        ValuePtr program_value = policy_atom->getValue(
            atomspace_->add_node(PREDICATE_NODE, constants::PROGRAM_VALUE_KEY));
        
        if (!program_value) {
            logger().error("PolicyOptimizer: No program value found for policy atom");
            return nullptr;
        }
        
        std::string program_str = program_value->to_string();
        combo_tree program = utils::stringToComboTree(program_str);
        
        // Get fitness score
        ValuePtr fitness_value = policy_atom->getValue(
            atomspace_->add_node(PREDICATE_NODE, constants::FITNESS_VALUE_KEY));
        
        double fitness = 0.0;
        if (fitness_value) {
            fitness = fitness_value->get_double();
        }
        
        auto policy = std::make_shared<Policy>(policy_id, policy_atom, program);
        policy->fitness_score = fitness;
        policy->program_source = program_str;
        
        return policy;
        
    } catch (const std::exception& e) {
        logger().error("PolicyOptimizer: Error converting AtomSpace representation to policy: %s", e.what());
        return nullptr;
    }
}

void PolicyOptimizer::continuousOptimizationLoop() {
    logger().info("PolicyOptimizer: Starting continuous optimization loop");
    
    size_t iteration = 0;
    
    while (optimization_running_) {
        try {
            iteration++;
            
            // Generate a policy ID for this iteration
            std::string policy_id = "continuous_" + std::to_string(iteration);
            
            // Evolve a new policy
            auto policy = evolvePolicy(policy_id, nullptr, config_.max_evals / 10); // Use smaller evaluation budget
            
            if (policy && optimization_callback_) {
                optimization_callback_->onPolicyEvolved(*policy);
                optimization_callback_->onLearningIteration(iteration, policy->fitness_score);
            }
            
            // Wait before next iteration
            std::unique_lock<std::mutex> lock(optimization_mutex_);
            optimization_cv_.wait_for(lock, std::chrono::seconds(5), 
                                    [this] { return !optimization_running_; });
            
        } catch (const std::exception& e) {
            logger().error("PolicyOptimizer: Error in continuous optimization loop: %s", e.what());
            
            // Wait before retrying
            std::unique_lock<std::mutex> lock(optimization_mutex_);
            optimization_cv_.wait_for(lock, std::chrono::seconds(10), 
                                    [this] { return !optimization_running_; });
        }
    }
    
    logger().info("PolicyOptimizer: Continuous optimization loop ended");
}

void PolicyOptimizer::updateStats(double fitness, size_t evaluations, size_t generations) {
    std::lock_guard<std::mutex> lock(stats_mutex_);
    
    total_evaluations_ += evaluations;
    total_generations_ = std::max(total_generations_, generations);
    
    if (fitness > best_fitness_ever_) {
        best_fitness_ever_ = fitness;
    }
    
    // Update average fitness with exponential moving average
    const double alpha = 0.1;
    average_fitness_ = alpha * fitness + (1.0 - alpha) * average_fitness_;
}

void PolicyOptimizer::initializeAtomSpaceStructures() {
    // Create base nodes for policy storage if they don't exist
    atomspace_->add_node(CONCEPT_NODE, "PolicyStorage");
    atomspace_->add_node(CONCEPT_NODE, "LearningModule");
    
    logger().debug("PolicyOptimizer: AtomSpace structures initialized");
}

PolicyId PolicyOptimizer::generatePolicyId() const {
    return utils::generateUniqueId("policy_");
}

void PolicyOptimizer::logEvolutionProgress(const PolicyId& policy_id, 
                                         size_t generation, 
                                         double best_fitness,
                                         double avg_fitness) const {
    logger().info("PolicyOptimizer: Policy '%s' - Generation %zu: best=%.4f, avg=%.4f", 
                  policy_id.c_str(), generation, best_fitness, avg_fitness);
}

bool PolicyOptimizer::validatePolicy(const Policy& policy) const {
    return !policy.id.empty() && !policy.program.empty();
}

void PolicyOptimizer::cleanupOldPolicies() {
    // Implementation for cleanup logic
    // Could remove policies with low fitness that haven't been used recently
}

// Factory function
std::unique_ptr<PolicyOptimizer> createPolicyOptimizer(
    AtomSpacePtr atomspace,
    std::shared_ptr<PolicyFitnessFunction> fitness_function,
    const std::string& config_preset) {
    
    LearningConfig config = utils::getDefaultConfig(config_preset);
    auto optimizer = std::make_unique<PolicyOptimizer>(atomspace, config);
    
    if (fitness_function && !optimizer->initialize(fitness_function)) {
        logger().error("Failed to initialize PolicyOptimizer with fitness function");
        return nullptr;
    }
    
    return optimizer;
}

} // namespace learning
} // namespace agentzero
} // namespace opencog