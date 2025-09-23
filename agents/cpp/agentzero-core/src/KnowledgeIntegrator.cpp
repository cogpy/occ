/*
 * src/KnowledgeIntegrator.cpp
 *
 * Copyright (C) 2024 OpenCog Foundation
 * SPDX-License-Identifier: AGPL-3.0-or-later
 *
 * Knowledge Integrator Implementation
 * Bridges knowledge representation with AtomSpace operations
 * Part of the AGENT-ZERO-GENESIS project
 */

#include <sstream>
#include <algorithm>
#include <cctype>

#include <opencog/atoms/atom_types/types.h>
#include <opencog/atoms/base/Node.h>
#include <opencog/atoms/base/Link.h>
#include <opencog/atoms/truthvalue/SimpleTruthValue.h>

#include "opencog/agentzero/KnowledgeIntegrator.h"
#include "opencog/agentzero/AgentZeroCore.h"

using namespace opencog;
using namespace opencog::agentzero;

KnowledgeIntegrator::KnowledgeIntegrator(AgentZeroCore* agent_core, AtomSpacePtr atomspace)
    : _agent_core(agent_core)
    , _atomspace(atomspace)
    , _knowledge_base(Handle::UNDEFINED)
    , _working_knowledge(Handle::UNDEFINED)
    , _semantic_network(Handle::UNDEFINED)
    , _episodic_memory(Handle::UNDEFINED)
    , _procedural_memory(Handle::UNDEFINED)
    , _enable_concept_formation(true)
    , _enable_semantic_integration(true)
    , _enable_memory_consolidation(true)
    , _knowledge_threshold(0.5)
{
    logger().info() << "[KnowledgeIntegrator] Constructor: Initializing knowledge integration";
    initializeKnowledgeStructures();
}

KnowledgeIntegrator::~KnowledgeIntegrator()
{
    logger().info() << "[KnowledgeIntegrator] Destructor: Cleaning up knowledge integration";
}

Handle KnowledgeIntegrator::addFact(const std::string& fact_description, ConfidenceLevel confidence)
{
    logger().debug() << "[KnowledgeIntegrator] Adding fact: " << fact_description;
    
    Handle fact_atom = createKnowledgeAtom(fact_description, KnowledgeType::FACTUAL);
    
    // Set truth value based on confidence
    double confidence_value = static_cast<double>(confidence) / 100.0;
    TruthValuePtr fact_tv = SimpleTruthValue::createTV(confidence_value, 0.9);
    fact_atom->setTruthValue(fact_tv);
    
    // Link to knowledge base
    HandleSeq fact_link;
    fact_link.push_back(_knowledge_base);
    fact_link.push_back(fact_atom);
    _atomspace->add_link(MEMBER_LINK, std::move(fact_link));
    
    return fact_atom;
}

Handle KnowledgeIntegrator::addProcedure(const std::string& procedure_description,
                                        const std::vector<std::string>& steps,
                                        ConfidenceLevel confidence)
{
    logger().debug() << "[KnowledgeIntegrator] Adding procedure: " << procedure_description;
    
    Handle procedure_atom = createKnowledgeAtom(procedure_description, KnowledgeType::PROCEDURAL);
    
    // Create step atoms and link them
    for (size_t i = 0; i < steps.size(); ++i) {
        Handle step_atom = _atomspace->add_node(CONCEPT_NODE, "Step_" + std::to_string(i) + "_" + steps[i]);
        
        HandleSeq step_link;
        step_link.push_back(procedure_atom);
        step_link.push_back(step_atom);
        _atomspace->add_link(SEQUENTIAL_AND_LINK, std::move(step_link));
    }
    
    // Set confidence
    double confidence_value = static_cast<double>(confidence) / 100.0;
    TruthValuePtr proc_tv = SimpleTruthValue::createTV(confidence_value, 0.9);
    procedure_atom->setTruthValue(proc_tv);
    
    return procedure_atom;
}

Handle KnowledgeIntegrator::addEpisode(const std::string& experience_description,
                                      const std::vector<Handle>& context_atoms,
                                      ConfidenceLevel confidence)
{
    logger().debug() << "[KnowledgeIntegrator] Adding episode: " << experience_description;
    
    Handle episode_atom = createKnowledgeAtom(experience_description, KnowledgeType::EPISODIC);
    
    // Link context atoms
    for (const Handle& context : context_atoms) {
        HandleSeq context_link;
        context_link.push_back(episode_atom);
        context_link.push_back(context);
        _atomspace->add_link(EVALUATION_LINK, std::move(context_link));
    }
    
    // Set confidence
    double confidence_value = static_cast<double>(confidence) / 100.0;
    TruthValuePtr episode_tv = SimpleTruthValue::createTV(confidence_value, 0.9);
    episode_atom->setTruthValue(episode_tv);
    
    return episode_atom;
}

Handle KnowledgeIntegrator::addSemanticRelation(const std::string& concept1_name,
                                               const std::string& relation_type,
                                               const std::string& concept2_name,
                                               ConfidenceLevel confidence)
{
    logger().debug() << "[KnowledgeIntegrator] Adding semantic relation: " 
                    << concept1_name << " " << relation_type << " " << concept2_name;
    
    Handle concept1 = findOrCreateConcept(concept1_name);
    Handle concept2 = findOrCreateConcept(concept2_name);
    
    establishConceptRelation(concept1, concept2, relation_type);
    
    // Create relation atom
    Handle relation_atom = _atomspace->add_node(CONCEPT_NODE, relation_type + "_" + concept1_name + "_" + concept2_name);
    
    // Set confidence
    double confidence_value = static_cast<double>(confidence) / 100.0;
    TruthValuePtr rel_tv = SimpleTruthValue::createTV(confidence_value, 0.9);
    relation_atom->setTruthValue(rel_tv);
    
    return relation_atom;
}

std::vector<Handle> KnowledgeIntegrator::queryKnowledge(const std::string& query_text, int max_results)
{
    logger().debug() << "[KnowledgeIntegrator] Querying knowledge: " << query_text;
    
    std::vector<Handle> results;
    
    // Simplified query implementation - search for atoms containing query terms
    HandleSeq all_atoms;
    _atomspace->get_handles_by_type(all_atoms, ATOM, true);
    
    for (const Handle& atom : all_atoms) {
        std::string atom_name = atom->get_name();
        if (atom_name.find(query_text) != std::string::npos) {
            results.push_back(atom);
            if (results.size() >= static_cast<size_t>(max_results)) {
                break;
            }
        }
    }
    
    return results;
}

std::vector<Handle> KnowledgeIntegrator::getFactsAbout(const std::string& concept_name)
{
    logger().debug() << "[KnowledgeIntegrator] Getting facts about: " << concept_name;
    
    std::vector<Handle> facts;
    
    // Find concept atom
    Handle concept = findOrCreateConcept(concept_name);
    
    // Find related knowledge atoms
    facts = findRelatedKnowledge(concept);
    
    return facts;
}

std::vector<Handle> KnowledgeIntegrator::getProceduresFor(const std::string& task_description)
{
    logger().debug() << "[KnowledgeIntegrator] Getting procedures for: " << task_description;
    
    // Simplified procedure search
    return queryKnowledge(task_description, 5);
}

std::vector<Handle> KnowledgeIntegrator::getEpisodesRelatedTo(const std::vector<Handle>& context_atoms)
{
    logger().debug() << "[KnowledgeIntegrator] Getting episodes related to context";
    
    std::vector<Handle> episodes;
    
    // Find episodes that share context atoms
    for (const Handle& context : context_atoms) {
        auto related = findRelatedKnowledge(context);
        episodes.insert(episodes.end(), related.begin(), related.end());
    }
    
    // Remove duplicates
    std::sort(episodes.begin(), episodes.end());
    episodes.erase(std::unique(episodes.begin(), episodes.end()), episodes.end());
    
    return episodes;
}

std::vector<Handle> KnowledgeIntegrator::getSemanticRelations(const std::string& concept_name,
                                                             const std::string& relation_type)
{
    logger().debug() << "[KnowledgeIntegrator] Getting semantic relations for: " << concept_name;
    
    Handle concept = findOrCreateConcept(concept_name);
    return findRelatedKnowledge(concept);
}

Handle KnowledgeIntegrator::registerConcept(const std::string& concept_name, 
                                           const std::string& concept_description)
{
    logger().debug() << "[KnowledgeIntegrator] Registering concept: " << concept_name;
    
    Handle concept = findOrCreateConcept(concept_name);
    
    if (!concept_description.empty()) {
        // Add description as knowledge
        addFact(concept_name + " is " + concept_description, ConfidenceLevel::HIGH);
    }
    
    return concept;
}

bool KnowledgeIntegrator::hasKnowledgeAbout(const std::string& concept_name)
{
    auto it = _concept_registry.find(concept_name);
    return it != _concept_registry.end();
}

std::vector<Handle> KnowledgeIntegrator::getAllConcepts()
{
    std::vector<Handle> concepts;
    
    for (const auto& pair : _concept_registry) {
        concepts.push_back(pair.second);
    }
    
    return concepts;
}

std::map<std::string, int> KnowledgeIntegrator::getKnowledgeStatistics()
{
    std::map<std::string, int> stats;
    
    stats["total_concepts"] = _concept_registry.size();
    stats["active_knowledge"] = _active_knowledge.size();
    stats["total_atoms"] = _atomspace->get_size();
    
    return stats;
}

std::string KnowledgeIntegrator::getStatusInfo() const
{
    std::ostringstream status;
    status << "{";
    status << "\"total_concepts\":" << _concept_registry.size() << ",";
    status << "\"active_knowledge\":" << _active_knowledge.size() << ",";
    status << "\"concept_formation_enabled\":" << (_enable_concept_formation ? "true" : "false") << ",";
    status << "\"semantic_integration_enabled\":" << (_enable_semantic_integration ? "true" : "false") << ",";
    status << "\"memory_consolidation_enabled\":" << (_enable_memory_consolidation ? "true" : "false") << ",";
    status << "\"knowledge_threshold\":" << _knowledge_threshold;
    status << "}";
    return status.str();
}

bool KnowledgeIntegrator::processKnowledgeIntegration()
{
    logger().debug() << "[KnowledgeIntegrator] Processing knowledge integration cycle";
    
    try {
        // Perform memory consolidation if enabled
        if (_enable_memory_consolidation) {
            consolidateMemory();
        }
        
        return true;
        
    } catch (const std::exception& e) {
        logger().error() << "[KnowledgeIntegrator] Error in knowledge processing: " << e.what();
        return false;
    }
}

// Private implementation methods

void KnowledgeIntegrator::initializeKnowledgeStructures()
{
    logger().debug() << "[KnowledgeIntegrator] Initializing knowledge structures";
    
    std::string agent_name = _agent_core->getAgentName();
    _knowledge_base = _atomspace->add_node(CONCEPT_NODE, std::string(agent_name + "_KnowledgeBase"));
    _working_knowledge = _atomspace->add_node(CONCEPT_NODE, std::string(agent_name + "_WorkingKnowledge"));
    _semantic_network = _atomspace->add_node(CONCEPT_NODE, std::string(agent_name + "_SemanticNetwork"));
    _episodic_memory = _atomspace->add_node(CONCEPT_NODE, std::string(agent_name + "_EpisodicMemory"));
    _procedural_memory = _atomspace->add_node(CONCEPT_NODE, std::string(agent_name + "_ProceduralMemory"));
    
    // Initialize knowledge categories
    _knowledge_categories[KnowledgeType::FACTUAL] = _atomspace->add_node(CONCEPT_NODE, std::string(agent_name + "_Facts"));
    _knowledge_categories[KnowledgeType::PROCEDURAL] = _procedural_memory;
    _knowledge_categories[KnowledgeType::EPISODIC] = _episodic_memory;
    _knowledge_categories[KnowledgeType::SEMANTIC] = _semantic_network;
}

Handle KnowledgeIntegrator::createKnowledgeAtom(const std::string& content, KnowledgeType type)
{
    std::string prefix;
    switch (type) {
        case KnowledgeType::FACTUAL: prefix = "Fact_"; break;
        case KnowledgeType::PROCEDURAL: prefix = "Proc_"; break;
        case KnowledgeType::EPISODIC: prefix = "Episode_"; break;
        case KnowledgeType::SEMANTIC: prefix = "Semantic_"; break;
        case KnowledgeType::CONDITIONAL: prefix = "Rule_"; break;
        case KnowledgeType::TEMPORAL: prefix = "Temporal_"; break;
    }
    
    Handle knowledge_atom = _atomspace->add_node(CONCEPT_NODE, std::string(prefix + content));
    
    // Link to appropriate category
    auto cat_it = _knowledge_categories.find(type);
    if (cat_it != _knowledge_categories.end()) {
        HandleSeq cat_link;
        cat_link.push_back(cat_it->second);
        cat_link.push_back(knowledge_atom);
        _atomspace->add_link(MEMBER_LINK, std::move(cat_link));
    }
    
    return knowledge_atom;
}

Handle KnowledgeIntegrator::findOrCreateConcept(const std::string& concept_name)
{
    auto it = _concept_registry.find(concept_name);
    if (it != _concept_registry.end()) {
        return it->second;
    }
    
    // Create new concept
    Handle concept = _atomspace->add_node(CONCEPT_NODE, std::string(std::string(concept_name)));
    _concept_registry[concept_name] = concept;
    
    return concept;
}

void KnowledgeIntegrator::establishConceptRelation(const Handle& concept1, const Handle& concept2, 
                                                  const std::string& relation_type)
{
    // Create appropriate link type based on relation
    Type link_type = EVALUATION_LINK;
    if (relation_type == "isa") {
        link_type = INHERITANCE_LINK;
    } else if (relation_type == "has") {
        link_type = MEMBER_LINK;
    }
    
    HandleSeq relation_link;
    relation_link.push_back(concept1);
    relation_link.push_back(concept2);
    _atomspace->add_link(link_type, std::move(relation_link));
}

std::vector<Handle> KnowledgeIntegrator::findRelatedKnowledge(const Handle& query_atom)
{
    std::vector<Handle> related;
    
    // Find incoming links to the query atom
    const IncomingSet& incoming = query_atom->getIncomingSet();
    
    for (const Handle& link : incoming) {
        const HandleSeq& outgoing = link->getOutgoingSet();
        for (const Handle& atom : outgoing) {
            if (atom != query_atom) {
                related.push_back(atom);
            }
        }
    }
    
    return related;
}

void KnowledgeIntegrator::consolidateMemory()
{
    logger().debug() << "[KnowledgeIntegrator] Consolidating memory";
    
    // Simplified memory consolidation - in a real implementation this would:
    // - Identify frequently accessed knowledge
    // - Strengthen important connections
    // - Weaken or remove unused knowledge
    // - Form higher-level abstractions
}

std::vector<Handle> KnowledgeIntegrator::formConceptsFrom(const std::vector<Handle>& experience_atoms)
{
    logger().debug() << "[KnowledgeIntegrator] Forming concepts from " << experience_atoms.size() << " experience atoms";
    
    std::vector<Handle> new_concepts;
    
    if (!_enable_concept_formation) {
        logger().debug() << "[KnowledgeIntegrator] Concept formation disabled";
        return new_concepts;
    }
    
    try {
        // Multi-strategy concept formation approach
        
        // 1. Pattern-based concept formation using structural similarities
        auto pattern_concepts = formConceptsByPatterns(experience_atoms);
        new_concepts.insert(new_concepts.end(), pattern_concepts.begin(), pattern_concepts.end());
        
        // 2. Semantic clustering based concept formation
        auto cluster_concepts = formConceptsByClustering(experience_atoms);
        new_concepts.insert(new_concepts.end(), cluster_concepts.begin(), cluster_concepts.end());
        
        // 3. Hierarchical concept formation with inheritance relationships
        auto hierarchical_concepts = formHierarchicalConcepts(experience_atoms);
        new_concepts.insert(new_concepts.end(), hierarchical_concepts.begin(), hierarchical_concepts.end());
        
        // 4. Traditional frequency-based concept formation (enhanced)
        auto frequency_concepts = formConceptsByFrequency(experience_atoms);
        new_concepts.insert(new_concepts.end(), frequency_concepts.begin(), frequency_concepts.end());
        
        // 5. Post-process concepts to establish relationships and validate
        validateAndRefineNewConcepts(new_concepts, experience_atoms);
        
        logger().info() << "[KnowledgeIntegrator] Formed " << new_concepts.size() 
                       << " new concepts using advanced algorithms";
        
    } catch (const std::exception& e) {
        logger().error() << "[KnowledgeIntegrator] Error forming concepts: " << e.what();
    }
    
    return new_concepts;
}

// Advanced concept formation methods

std::vector<Handle> KnowledgeIntegrator::formConceptsByPatterns(const std::vector<Handle>& experience_atoms)
{
    logger().debug() << "[KnowledgeIntegrator] Forming concepts by structural patterns";
    
    std::vector<Handle> pattern_concepts;
    
    try {
        // Group atoms by structural similarity using LinkType patterns
        std::map<Type, std::vector<Handle>> type_groups;
        std::map<std::string, std::vector<Handle>> structure_groups;
        
        for (const Handle& atom : experience_atoms) {
            Type atom_type = atom->get_type();
            type_groups[atom_type].push_back(atom);
            
            // Create structure signature for grouping
            std::string structure_sig = createStructureSignature(atom);
            if (!structure_sig.empty()) {
                structure_groups[structure_sig].push_back(atom);
            }
        }
        
        // Form concepts from significant structural patterns
        for (const auto& group : structure_groups) {
            if (group.second.size() >= 3) { // Minimum 3 instances to form a pattern concept
                std::string concept_name = "PatternConcept_" + std::to_string(std::hash<std::string>{}(group.first) % 10000);
                
                if (!hasKnowledgeAbout(concept_name)) {
                    Handle pattern_concept = registerConcept(concept_name, 
                        "Concept formed from structural pattern: " + group.first);
                    
                    // Set confidence based on pattern frequency and consistency
                    double pattern_strength = std::min(1.0, group.second.size() / 10.0);
                    TruthValuePtr pattern_tv = SimpleTruthValue::createTV(pattern_strength, 0.8);
                    pattern_concept->setTruthValue(pattern_tv);
                    
                    // Link pattern instances to the concept
                    for (const Handle& instance : group.second) {
                        establishConceptRelation(instance, pattern_concept, "instanceof");
                    }
                    
                    pattern_concepts.push_back(pattern_concept);
                    
                    logger().info() << "[KnowledgeIntegrator] Formed pattern concept: " << concept_name
                                   << " with " << group.second.size() << " instances";
                }
            }
        }
        
    } catch (const std::exception& e) {
        logger().error() << "[KnowledgeIntegrator] Error in pattern-based concept formation: " << e.what();
    }
    
    return pattern_concepts;
}

std::vector<Handle> KnowledgeIntegrator::formConceptsByClustering(const std::vector<Handle>& experience_atoms)
{
    logger().debug() << "[KnowledgeIntegrator] Forming concepts by semantic clustering";
    
    std::vector<Handle> cluster_concepts;
    
    try {
        // Calculate semantic similarity matrix between atoms
        std::vector<std::vector<double>> similarity_matrix;
        calculateSemanticSimilarityMatrix(experience_atoms, similarity_matrix);
        
        // Perform clustering using simple agglomerative clustering
        std::vector<std::vector<int>> clusters = performSimpleClustering(similarity_matrix, 0.6); // 60% similarity threshold
        
        int cluster_id = 0;
        for (const auto& cluster : clusters) {
            if (cluster.size() >= 2) { // Minimum 2 atoms to form a cluster concept
                std::string concept_name = "ClusterConcept_" + std::to_string(cluster_id++);
                
                if (!hasKnowledgeAbout(concept_name)) {
                    Handle cluster_concept = registerConcept(concept_name,
                        "Concept formed from semantic clustering with " + std::to_string(cluster.size()) + " members");
                    
                    // Set confidence based on cluster cohesion
                    double avg_similarity = calculateClusterCohesion(cluster, similarity_matrix);
                    TruthValuePtr cluster_tv = SimpleTruthValue::createTV(avg_similarity, 0.7);
                    cluster_concept->setTruthValue(cluster_tv);
                    
                    // Link cluster members to the concept
                    for (int atom_idx : cluster) {
                        if (atom_idx < experience_atoms.size()) {
                            establishConceptRelation(experience_atoms[atom_idx], cluster_concept, "memberof");
                        }
                    }
                    
                    cluster_concepts.push_back(cluster_concept);
                    
                    logger().info() << "[KnowledgeIntegrator] Formed cluster concept: " << concept_name
                                   << " with " << cluster.size() << " members (cohesion: " << avg_similarity << ")";
                }
            }
        }
        
    } catch (const std::exception& e) {
        logger().error() << "[KnowledgeIntegrator] Error in clustering-based concept formation: " << e.what();
    }
    
    return cluster_concepts;
}

std::vector<Handle> KnowledgeIntegrator::formHierarchicalConcepts(const std::vector<Handle>& experience_atoms)
{
    logger().debug() << "[KnowledgeIntegrator] Forming hierarchical concepts";
    
    std::vector<Handle> hierarchical_concepts;
    
    try {
        // Extract hierarchical relationships from atom structures
        std::map<std::string, std::set<std::string>> taxonomy;
        
        for (const Handle& atom : experience_atoms) {
            // Extract potential hierarchical information from atom names and structures
            std::vector<std::string> hierarchy_levels = extractHierarchyLevels(atom);
            
            if (hierarchy_levels.size() > 1) {
                for (size_t i = 0; i < hierarchy_levels.size() - 1; ++i) {
                    taxonomy[hierarchy_levels[i]].insert(hierarchy_levels[i + 1]);
                }
            }
        }
        
        // Create hierarchical concepts
        for (const auto& parent_children : taxonomy) {
            const std::string& parent_name = parent_children.first;
            const std::set<std::string>& children_names = parent_children.second;
            
            if (children_names.size() >= 2) { // At least 2 children to justify a parent concept
                std::string parent_concept_name = "HierarchicalConcept_" + parent_name;
                
                if (!hasKnowledgeAbout(parent_concept_name)) {
                    Handle parent_concept = registerConcept(parent_concept_name,
                        "Hierarchical parent concept with " + std::to_string(children_names.size()) + " children");
                    
                    // Set confidence based on number of children and consistency
                    double hierarchy_strength = std::min(1.0, children_names.size() / 5.0);
                    TruthValuePtr hierarchy_tv = SimpleTruthValue::createTV(hierarchy_strength, 0.85);
                    parent_concept->setTruthValue(hierarchy_tv);
                    
                    // Create child concepts and establish inheritance relationships
                    for (const std::string& child_name : children_names) {
                        std::string child_concept_name = "HierarchicalConcept_" + child_name;
                        Handle child_concept = findOrCreateConcept(child_concept_name);
                        
                        // Establish inheritance relationship
                        establishConceptRelation(child_concept, parent_concept, "isa");
                    }
                    
                    hierarchical_concepts.push_back(parent_concept);
                    
                    logger().info() << "[KnowledgeIntegrator] Formed hierarchical concept: " << parent_concept_name
                                   << " with " << children_names.size() << " children";
                }
            }
        }
        
    } catch (const std::exception& e) {
        logger().error() << "[KnowledgeIntegrator] Error in hierarchical concept formation: " << e.what();
    }
    
    return hierarchical_concepts;
}

std::vector<Handle> KnowledgeIntegrator::formConceptsByFrequency(const std::vector<Handle>& experience_atoms)
{
    logger().debug() << "[KnowledgeIntegrator] Forming concepts by enhanced frequency analysis";
    
    std::vector<Handle> frequency_concepts;
    
    try {
        // Enhanced frequency analysis with context awareness
        std::map<std::string, int> term_frequency;
        std::map<std::string, std::set<std::string>> term_contexts;
        std::map<std::string, double> term_significance;
        
        // Extract terms with context information
        for (const Handle& atom : experience_atoms) {
            std::string atom_name = atom->get_name();
            std::vector<std::string> terms = tokenizeWithContext(atom_name);
            
            for (const std::string& term : terms) {
                if (term.length() > 2) {
                    term_frequency[term]++;
                    
                    // Extract context (surrounding terms)
                    std::string context = extractTermContext(atom_name, term);
                    if (!context.empty()) {
                        term_contexts[term].insert(context);
                    }
                }
            }
        }
        
        // Calculate term significance (frequency + context diversity + semantic weight)
        for (const auto& term_freq : term_frequency) {
            const std::string& term = term_freq.first;
            int frequency = term_freq.second;
            int context_diversity = term_contexts[term].size();
            
            double semantic_weight = calculateSemanticWeight(term);
            double significance = (frequency * 0.5) + (context_diversity * 0.3) + (semantic_weight * 0.2);
            term_significance[term] = significance;
        }
        
        // Form concepts from significant terms
        double min_significance = calculateDynamicThreshold(term_significance, experience_atoms.size());
        
        for (const auto& term_sig : term_significance) {
            const std::string& term = term_sig.first;
            double significance = term_sig.second;
            
            if (significance >= min_significance) {
                std::string concept_name = "FrequencyConcept_" + term;
                
                if (!hasKnowledgeAbout(concept_name)) {
                    Handle freq_concept = registerConcept(concept_name,
                        "Concept formed from frequency analysis (significance: " + std::to_string(significance) + ")");
                    
                    // Set confidence based on significance and context diversity
                    double confidence = std::min(0.95, significance / (min_significance * 2.0));
                    TruthValuePtr freq_tv = SimpleTruthValue::createTV(confidence, 0.75);
                    freq_concept->setTruthValue(freq_tv);
                    
                    frequency_concepts.push_back(freq_concept);
                    
                    logger().info() << "[KnowledgeIntegrator] Formed frequency concept: " << concept_name
                                   << " (significance: " << significance << ", contexts: " << term_contexts[term].size() << ")";
                }
            }
        }
        
    } catch (const std::exception& e) {
        logger().error() << "[KnowledgeIntegrator] Error in frequency-based concept formation: " << e.what();
    }
    
    return frequency_concepts;
}

void KnowledgeIntegrator::validateAndRefineNewConcepts(std::vector<Handle>& new_concepts, 
                                                      const std::vector<Handle>& experience_atoms)
{
    logger().debug() << "[KnowledgeIntegrator] Validating and refining " << new_concepts.size() << " new concepts";
    
    try {
        std::vector<Handle> validated_concepts;
        validated_concepts.reserve(new_concepts.size());
        
        for (Handle& concept : new_concepts) {
            if (concept != Handle::UNDEFINED) {
                // Validate concept quality and utility
                double concept_quality = evaluateConceptQuality(concept, experience_atoms);
                
                if (concept_quality > 0.3) { // Keep concepts with reasonable quality
                    // Refine concept relationships
                    refineConceptRelationships(concept, new_concepts);
                    
                    // Update concept confidence based on validation
                    updateConceptConfidenceAfterValidation(concept, concept_quality);
                    
                    validated_concepts.push_back(concept);
                } else {
                    logger().debug() << "[KnowledgeIntegrator] Removing low-quality concept: " << concept->get_name()
                                    << " (quality: " << concept_quality << ")";
                }
            }
        }
        
        new_concepts = std::move(validated_concepts);
        
        logger().info() << "[KnowledgeIntegrator] Validated concepts: " << new_concepts.size() << " concepts retained";
        
    } catch (const std::exception& e) {
        logger().error() << "[KnowledgeIntegrator] Error validating concepts: " << e.what();
    }
}

std::vector<Handle> KnowledgeIntegrator::validateKnowledgeConsistency()
{
    logger().debug() << "[KnowledgeIntegrator] Validating knowledge consistency";
    
    std::vector<Handle> inconsistent_knowledge;
    
    try {
        // Check for contradictory facts
        HandleSeq all_facts;
        _atomspace->get_handles_by_type(all_facts, ATOM, true);
        
        std::map<std::string, std::vector<Handle>> fact_groups;
        
        for (const Handle& atom : all_facts) {
            std::string atom_name = atom->get_name();
            
            // Group facts by subject (simplified approach)
            size_t first_space = atom_name.find(' ');
            if (first_space != std::string::npos) {
                std::string subject = atom_name.substr(0, first_space);
                fact_groups[subject].push_back(atom);
            }
        }
        
        // Look for contradictions within fact groups
        for (const auto& group : fact_groups) {
            if (group.second.size() > 1) {
                // Check for opposing truth values or contradictory statements
                for (size_t i = 0; i < group.second.size(); ++i) {
                    for (size_t j = i + 1; j < group.second.size(); ++j) {
                        const Handle& atom1 = group.second[i];
                        const Handle& atom2 = group.second[j];
                        
                        TruthValuePtr tv1 = atom1->getTruthValue();
                        TruthValuePtr tv2 = atom2->getTruthValue();
                        
                        // Simple contradiction check: opposing truth values
                        if (tv1->get_mean() > 0.5 && tv2->get_mean() < 0.5) {
                            inconsistent_knowledge.push_back(atom1);
                            inconsistent_knowledge.push_back(atom2);
                            
                            logger().warn() << "[KnowledgeIntegrator] Inconsistency detected between: " 
                                           << atom1->get_name() << " and " << atom2->get_name();
                        }
                    }
                }
            }
        }
        
    } catch (const std::exception& e) {
        logger().error() << "[KnowledgeIntegrator] Error validating consistency: " << e.what();
    }
    
    return inconsistent_knowledge;
}

KnowledgeIntegrator::ConfidenceLevel KnowledgeIntegrator::updateKnowledgeConfidence(
    const Handle& knowledge_atom, const std::vector<Handle>& supporting_evidence)
{
    logger().debug() << "[KnowledgeIntegrator] Updating confidence for knowledge atom: " << knowledge_atom;
    
    if (knowledge_atom == Handle::UNDEFINED) {
        logger().error() << "[KnowledgeIntegrator] Cannot update confidence for undefined atom";
        return ConfidenceLevel::VERY_LOW;
    }
    
    try {
        TruthValuePtr current_tv = knowledge_atom->getTruthValue();
        double current_strength = current_tv->get_mean();
        double current_confidence = current_tv->get_confidence();
        
        // Calculate evidence strength
        double evidence_strength = 0.0;
        int valid_evidence_count = 0;
        
        for (const Handle& evidence : supporting_evidence) {
            if (evidence != Handle::UNDEFINED) {
                TruthValuePtr evidence_tv = evidence->getTruthValue();
                evidence_strength += evidence_tv->get_mean() * evidence_tv->get_confidence();
                valid_evidence_count++;
            }
        }
        
        if (valid_evidence_count > 0) {
            evidence_strength /= valid_evidence_count;
            
            // Update confidence based on evidence
            double new_strength = (current_strength + evidence_strength) / 2.0;
            double new_confidence = std::min(1.0, current_confidence + (evidence_strength * 0.1));
            
            TruthValuePtr new_tv = SimpleTruthValue::createTV(new_strength, new_confidence);
            knowledge_atom->setTruthValue(new_tv);
            
            // Convert to ConfidenceLevel
            int confidence_percent = static_cast<int>(new_confidence * 100);
            if (confidence_percent >= 90) return ConfidenceLevel::VERY_HIGH;
            else if (confidence_percent >= 70) return ConfidenceLevel::HIGH;
            else if (confidence_percent >= 40) return ConfidenceLevel::MEDIUM;
            else if (confidence_percent >= 20) return ConfidenceLevel::LOW;
            else return ConfidenceLevel::VERY_LOW;
        }
        
    } catch (const std::exception& e) {
        logger().error() << "[KnowledgeIntegrator] Error updating confidence: " << e.what();
    }
    
    return ConfidenceLevel::MEDIUM;
}

int KnowledgeIntegrator::cleanupOutdatedKnowledge(int age_threshold_days)
{
    logger().debug() << "[KnowledgeIntegrator] Cleaning up knowledge older than " << age_threshold_days << " days";
    
    int cleaned_count = 0;
    
    try {
        // In a real implementation, this would:
        // 1. Check atom timestamps (if available)
        // 2. Identify low-confidence, rarely accessed knowledge
        // 3. Remove or archive outdated knowledge
        // 4. Update statistics
        
        // For now, implement a simple cleanup based on confidence
        HandleSeq all_atoms;
        _atomspace->get_handles_by_type(all_atoms, ATOM, true);
        
        for (const Handle& atom : all_atoms) {
            TruthValuePtr tv = atom->getTruthValue();
            
            // Remove very low confidence knowledge
            if (tv->get_mean() < 0.1 && tv->get_confidence() < 0.1) {
                // Mark for removal (in a full implementation)
                logger().debug() << "[KnowledgeIntegrator] Marked for cleanup: " << atom->get_name();
                cleaned_count++;
            }
        }
        
    } catch (const std::exception& e) {
        logger().error() << "[KnowledgeIntegrator] Error during cleanup: " << e.what();
    }
    
    return cleaned_count;
}

int KnowledgeIntegrator::importKnowledge(const std::string& source_description,
                                        const std::map<std::string, std::string>& knowledge_data)
{
    logger().info() << "[KnowledgeIntegrator] Importing knowledge from: " << source_description;
    
    int imported_count = 0;
    
    try {
        for (const auto& pair : knowledge_data) {
            const std::string& key = pair.first;
            const std::string& value = pair.second;
            
            // Import as facts with medium confidence
            Handle fact_atom = addFact(key + ": " + value, ConfidenceLevel::MEDIUM);
            
            if (fact_atom != Handle::UNDEFINED) {
                imported_count++;
                
                // Add source metadata
                Handle source_atom = _atomspace->add_node(CONCEPT_NODE, "Source_" + source_description);
                HandleSeq source_link;
                source_link.push_back(fact_atom);
                source_link.push_back(source_atom);
                _atomspace->add_link(EVALUATION_LINK, std::move(source_link));
            }
        }
        
        logger().info() << "[KnowledgeIntegrator] Successfully imported " << imported_count << " knowledge items";
        
    } catch (const std::exception& e) {
        logger().error() << "[KnowledgeIntegrator] Error importing knowledge: " << e.what();
    }
    
    return imported_count;
}

std::string KnowledgeIntegrator::exportKnowledge(const std::string& export_format,
                                                KnowledgeType knowledge_filter)
{
    logger().debug() << "[KnowledgeIntegrator] Exporting knowledge in format: " << export_format;
    
    std::ostringstream exported_data;
    
    try {
        if (export_format == "json") {
            exported_data << "{\n";
            exported_data << "  \"knowledge_base\": {\n";
            exported_data << "    \"total_concepts\": " << _concept_registry.size() << ",\n";
            exported_data << "    \"active_knowledge\": " << _active_knowledge.size() << ",\n";
            exported_data << "    \"concepts\": [\n";
            
            bool first = true;
            for (const auto& pair : _concept_registry) {
                if (!first) exported_data << ",\n";
                exported_data << "      {\n";
                exported_data << "        \"name\": \"" << pair.first << "\",\n";
                exported_data << "        \"handle\": \"" << pair.second << "\"\n";
                exported_data << "      }";
                first = false;
            }
            
            exported_data << "\n    ]\n";
            exported_data << "  }\n";
            exported_data << "}";
            
        } else if (export_format == "text") {
            exported_data << "Knowledge Base Export\n";
            exported_data << "====================\n\n";
            exported_data << "Total Concepts: " << _concept_registry.size() << "\n";
            exported_data << "Active Knowledge Items: " << _active_knowledge.size() << "\n\n";
            exported_data << "Concepts:\n";
            
            for (const auto& pair : _concept_registry) {
                exported_data << "- " << pair.first << " [" << pair.second << "]\n";
            }
            
        } else {
            logger().error() << "[KnowledgeIntegrator] Unsupported export format: " << export_format;
            return "";
        }
        
    } catch (const std::exception& e) {
        logger().error() << "[KnowledgeIntegrator] Error exporting knowledge: " << e.what();
        return "";
    }
    
    return exported_data.str();
}

std::vector<Handle> KnowledgeIntegrator::getMostActiveKnowledge(int count)
{
    logger().debug() << "[KnowledgeIntegrator] Getting " << count << " most active knowledge items";
    
    std::vector<Handle> most_active;
    
    try {
        // For now, return most recently accessed items from active knowledge set
        most_active.reserve(std::min(count, static_cast<int>(_active_knowledge.size())));
        
        auto it = _active_knowledge.begin();
        for (int i = 0; i < count && it != _active_knowledge.end(); ++i, ++it) {
            most_active.push_back(*it);
        }
        
    } catch (const std::exception& e) {
        logger().error() << "[KnowledgeIntegrator] Error getting active knowledge: " << e.what();
    }
    
    return most_active;
}

TruthValuePtr KnowledgeIntegrator::assessKnowledgeReliability(const Handle& knowledge_atom)
{
    if (knowledge_atom == Handle::UNDEFINED) {
        return SimpleTruthValue::createTV(0.0, 0.0);
    }
    
    try {
        TruthValuePtr current_tv = knowledge_atom->getTruthValue();
        
        // Simple reliability assessment based on:
        // 1. Current truth value strength
        // 2. Number of supporting links
        // 3. Age of the knowledge (simplified)
        
        double strength = current_tv->get_mean();
        double confidence = current_tv->get_confidence();
        
        // Factor in connectivity (more connections = more reliable)
        const IncomingSet& incoming = knowledge_atom->getIncomingSet();
        double connectivity_factor = std::min(1.0, incoming.size() * 0.1);
        
        double reliability_strength = (strength + connectivity_factor) / 2.0;
        double reliability_confidence = std::min(1.0, confidence + (connectivity_factor * 0.2));
        
        return SimpleTruthValue::createTV(reliability_strength, reliability_confidence);
        
    } catch (const std::exception& e) {
        logger().error() << "[KnowledgeIntegrator] Error assessing reliability: " << e.what();
        return SimpleTruthValue::createTV(0.0, 0.0);
    }
}

// Helper methods for advanced concept formation

std::string KnowledgeIntegrator::createStructureSignature(const Handle& atom)
{
    if (atom == Handle::UNDEFINED) return "";
    
    std::ostringstream signature;
    signature << atom->get_type_name();
    
    if (atom->is_link()) {
        const HandleSeq& outgoing = atom->getOutgoingSet();
        signature << "[";
        for (size_t i = 0; i < outgoing.size(); ++i) {
            if (i > 0) signature << ",";
            signature << outgoing[i]->get_type_name();
        }
        signature << "]";
    }
    
    return signature.str();
}

void KnowledgeIntegrator::calculateSemanticSimilarityMatrix(const std::vector<Handle>& atoms, 
                                                          std::vector<std::vector<double>>& matrix)
{
    size_t n = atoms.size();
    matrix.assign(n, std::vector<double>(n, 0.0));
    
    for (size_t i = 0; i < n; ++i) {
        for (size_t j = i; j < n; ++j) {
            double similarity = calculateAtomSimilarity(atoms[i], atoms[j]);
            matrix[i][j] = similarity;
            matrix[j][i] = similarity; // Symmetric matrix
        }
    }
}

double KnowledgeIntegrator::calculateAtomSimilarity(const Handle& atom1, const Handle& atom2)
{
    if (atom1 == atom2) return 1.0;
    if (atom1 == Handle::UNDEFINED || atom2 == Handle::UNDEFINED) return 0.0;
    
    // Type similarity
    double type_sim = (atom1->get_type() == atom2->get_type()) ? 0.4 : 0.0;
    
    // Name similarity (for nodes)
    double name_sim = 0.0;
    if (atom1->is_node() && atom2->is_node()) {
        name_sim = calculateStringSimilarity(atom1->get_name(), atom2->get_name()) * 0.4;
    }
    
    // Structure similarity (for links)
    double struct_sim = 0.0;
    if (atom1->is_link() && atom2->is_link()) {
        const HandleSeq& out1 = atom1->getOutgoingSet();
        const HandleSeq& out2 = atom2->getOutgoingSet();
        
        if (out1.size() == out2.size()) {
            double avg_child_sim = 0.0;
            for (size_t i = 0; i < out1.size(); ++i) {
                avg_child_sim += calculateAtomSimilarity(out1[i], out2[i]);
            }
            struct_sim = (avg_child_sim / out1.size()) * 0.2;
        }
    }
    
    return type_sim + name_sim + struct_sim;
}

double KnowledgeIntegrator::calculateStringSimilarity(const std::string& str1, const std::string& str2)
{
    // Simple Levenshtein distance-based similarity
    if (str1.empty() && str2.empty()) return 1.0;
    if (str1.empty() || str2.empty()) return 0.0;
    
    size_t len1 = str1.length();
    size_t len2 = str2.length();
    std::vector<std::vector<int>> dp(len1 + 1, std::vector<int>(len2 + 1));
    
    for (size_t i = 0; i <= len1; ++i) dp[i][0] = i;
    for (size_t j = 0; j <= len2; ++j) dp[0][j] = j;
    
    for (size_t i = 1; i <= len1; ++i) {
        for (size_t j = 1; j <= len2; ++j) {
            int cost = (str1[i-1] == str2[j-1]) ? 0 : 1;
            dp[i][j] = std::min({dp[i-1][j] + 1, dp[i][j-1] + 1, dp[i-1][j-1] + cost});
        }
    }
    
    int max_len = std::max(len1, len2);
    return 1.0 - (static_cast<double>(dp[len1][len2]) / max_len);
}

std::vector<std::vector<int>> KnowledgeIntegrator::performSimpleClustering(
    const std::vector<std::vector<double>>& similarity_matrix, double threshold)
{
    size_t n = similarity_matrix.size();
    std::vector<std::vector<int>> clusters;
    std::vector<bool> assigned(n, false);
    
    for (size_t i = 0; i < n; ++i) {
        if (!assigned[i]) {
            std::vector<int> cluster;
            cluster.push_back(i);
            assigned[i] = true;
            
            // Find similar atoms to add to this cluster
            for (size_t j = i + 1; j < n; ++j) {
                if (!assigned[j] && similarity_matrix[i][j] >= threshold) {
                    cluster.push_back(j);
                    assigned[j] = true;
                }
            }
            
            clusters.push_back(cluster);
        }
    }
    
    return clusters;
}

double KnowledgeIntegrator::calculateClusterCohesion(const std::vector<int>& cluster, 
                                                   const std::vector<std::vector<double>>& similarity_matrix)
{
    if (cluster.size() < 2) return 1.0;
    
    double total_similarity = 0.0;
    int pair_count = 0;
    
    for (size_t i = 0; i < cluster.size(); ++i) {
        for (size_t j = i + 1; j < cluster.size(); ++j) {
            total_similarity += similarity_matrix[cluster[i]][cluster[j]];
            pair_count++;
        }
    }
    
    return (pair_count > 0) ? (total_similarity / pair_count) : 0.0;
}

std::vector<std::string> KnowledgeIntegrator::extractHierarchyLevels(const Handle& atom)
{
    std::vector<std::string> levels;
    
    if (atom == Handle::UNDEFINED) return levels;
    
    std::string name = atom->get_name();
    
    // Look for hierarchical patterns in the name
    // Pattern 1: "Category_Subcategory_Item"
    size_t pos = 0;
    while (pos < name.length()) {
        size_t next_pos = name.find('_', pos);
        if (next_pos == std::string::npos) {
            levels.push_back(name.substr(pos));
            break;
        } else {
            levels.push_back(name.substr(pos, next_pos - pos));
            pos = next_pos + 1;
        }
    }
    
    // Pattern 2: Look for type hierarchy in links
    if (atom->is_link() && atom->get_type() == INHERITANCE_LINK) {
        const HandleSeq& outgoing = atom->getOutgoingSet();
        if (outgoing.size() >= 2) {
            levels.push_back(outgoing[0]->get_name()); // child
            levels.push_back(outgoing[1]->get_name()); // parent
        }
    }
    
    return levels;
}

std::vector<std::string> KnowledgeIntegrator::tokenizeWithContext(const std::string& text)
{
    std::vector<std::string> tokens;
    std::istringstream iss(text);
    std::string token;
    
    while (iss >> token) {
        // Clean up token
        std::string clean_token;
        for (char c : token) {
            if (std::isalnum(c)) {
                clean_token += std::tolower(c);
            }
        }
        
        if (clean_token.length() > 2) {
            tokens.push_back(clean_token);
        }
    }
    
    return tokens;
}

std::string KnowledgeIntegrator::extractTermContext(const std::string& text, const std::string& term)
{
    size_t pos = text.find(term);
    if (pos == std::string::npos) return "";
    
    // Extract words before and after the term
    std::ostringstream context;
    std::istringstream iss(text);
    std::string word;
    std::vector<std::string> words;
    
    while (iss >> word) {
        words.push_back(word);
    }
    
    // Find term position in words and extract context
    for (size_t i = 0; i < words.size(); ++i) {
        if (words[i].find(term) != std::string::npos) {
            if (i > 0) context << words[i-1] << " ";
            if (i + 1 < words.size()) context << words[i+1];
            break;
        }
    }
    
    return context.str();
}

double KnowledgeIntegrator::calculateSemanticWeight(const std::string& term)
{
    // Simple semantic weight based on term characteristics
    double weight = 0.5; // Base weight
    
    // Longer terms tend to be more specific and meaningful
    if (term.length() > 6) weight += 0.2;
    if (term.length() > 10) weight += 0.1;
    
    // Check for common semantic indicators
    if (term.find("concept") != std::string::npos || 
        term.find("pattern") != std::string::npos ||
        term.find("relation") != std::string::npos) {
        weight += 0.2;
    }
    
    // Penalize very common words
    if (term == "the" || term == "and" || term == "for" || term == "with") {
        weight -= 0.4;
    }
    
    return std::max(0.1, std::min(1.0, weight));
}

double KnowledgeIntegrator::calculateDynamicThreshold(const std::map<std::string, double>& term_significance,
                                                    size_t experience_count)
{
    if (term_significance.empty()) return 1.0;
    
    // Calculate statistics
    double sum = 0.0, sum_squared = 0.0;
    for (const auto& pair : term_significance) {
        sum += pair.second;
        sum_squared += pair.second * pair.second;
    }
    
    double mean = sum / term_significance.size();
    double variance = (sum_squared / term_significance.size()) - (mean * mean);
    double std_dev = std::sqrt(variance);
    
    // Dynamic threshold based on mean + standard deviation, adjusted for experience count
    double base_threshold = mean + (0.5 * std_dev);
    double experience_factor = std::min(1.0, experience_count / 10.0);
    
    return base_threshold * experience_factor;
}

double KnowledgeIntegrator::evaluateConceptQuality(const Handle& concept, 
                                                 const std::vector<Handle>& experience_atoms)
{
    if (concept == Handle::UNDEFINED) return 0.0;
    
    double quality = 0.0;
    
    try {
        // Factor 1: Truth value confidence
        TruthValuePtr tv = concept->getTruthValue();
        quality += tv->get_confidence() * 0.3;
        
        // Factor 2: Connectivity (number of relationships)
        const IncomingSet& incoming = concept->getIncomingSet();
        double connectivity_score = std::min(1.0, incoming.size() / 5.0);
        quality += connectivity_score * 0.3;
        
        // Factor 3: Coherence with experience atoms
        int coherence_count = 0;
        for (const Handle& exp_atom : experience_atoms) {
            if (calculateAtomSimilarity(concept, exp_atom) > 0.4) {
                coherence_count++;
            }
        }
        double coherence_score = std::min(1.0, coherence_count / 3.0);
        quality += coherence_score * 0.2;
        
        // Factor 4: Uniqueness (not too similar to existing concepts)
        double uniqueness_score = evaluateConceptUniqueness(concept);
        quality += uniqueness_score * 0.2;
        
    } catch (const std::exception& e) {
        logger().error() << "[KnowledgeIntegrator] Error evaluating concept quality: " << e.what();
        return 0.0;
    }
    
    return std::min(1.0, quality);
}

double KnowledgeIntegrator::evaluateConceptUniqueness(const Handle& concept)
{
    double max_similarity = 0.0;
    
    for (const auto& existing_concept : _concept_registry) {
        if (existing_concept.second != concept) {
            double similarity = calculateAtomSimilarity(concept, existing_concept.second);
            max_similarity = std::max(max_similarity, similarity);
        }
    }
    
    return 1.0 - max_similarity; // Higher uniqueness = lower max similarity
}

void KnowledgeIntegrator::refineConceptRelationships(Handle& concept, const std::vector<Handle>& all_concepts)
{
    try {
        // Look for potential relationships with other new concepts
        for (const Handle& other_concept : all_concepts) {
            if (other_concept != concept && other_concept != Handle::UNDEFINED) {
                double similarity = calculateAtomSimilarity(concept, other_concept);
                
                // Establish relationships based on similarity
                if (similarity > 0.7) {
                    establishConceptRelation(concept, other_concept, "similar");
                } else if (similarity > 0.5) {
                    establishConceptRelation(concept, other_concept, "related");
                }
                
                // Look for hierarchical relationships based on names
                std::string concept_name = concept->get_name();
                std::string other_name = other_concept->get_name();
                
                if (concept_name.find(other_name) != std::string::npos && concept_name != other_name) {
                    establishConceptRelation(concept, other_concept, "isa");
                } else if (other_name.find(concept_name) != std::string::npos && concept_name != other_name) {
                    establishConceptRelation(other_concept, concept, "isa");
                }
            }
        }
        
    } catch (const std::exception& e) {
        logger().error() << "[KnowledgeIntegrator] Error refining concept relationships: " << e.what();
    }
}

void KnowledgeIntegrator::updateConceptConfidenceAfterValidation(Handle& concept, double quality_score)
{
    try {
        TruthValuePtr current_tv = concept->getTruthValue();
        double current_strength = current_tv->get_mean();
        double current_confidence = current_tv->get_confidence();
        
        // Adjust confidence based on quality score
        double new_confidence = std::min(0.95, current_confidence * quality_score);
        double new_strength = std::min(1.0, current_strength + (quality_score * 0.1));
        
        TruthValuePtr new_tv = SimpleTruthValue::createTV(new_strength, new_confidence);
        concept->setTruthValue(new_tv);
        
    } catch (const std::exception& e) {
        logger().error() << "[KnowledgeIntegrator] Error updating concept confidence: " << e.what();
    }
}