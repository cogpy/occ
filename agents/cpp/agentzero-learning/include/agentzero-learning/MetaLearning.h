/**
 * MetaLearning.h
 *
 * Learning how to learn more effectively
 * Part of Agent-Zero Learning & Adaptation Phase 5
 *
 * Copyright (C) 2024 OpenCog Foundation
 */

#ifndef _AGENTZERO_META_LEARNING_H
#define _AGENTZERO_META_LEARNING_H

#include <memory>
#include <map>
#include <string>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>

namespace opencog { namespace agentzero {

class MetaLearning
{
public:
    explicit MetaLearning(AtomSpacePtr atomspace);
    virtual ~MetaLearning();

    bool initialize();
    void optimizeLearningParams(const std::map<std::string, std::string>& current_params);
    
protected:
    AtomSpacePtr _atomspace;
    bool _initialized;
};

} // namespace agentzero
} // namespace opencog

#endif // _AGENTZERO_META_LEARNING_H