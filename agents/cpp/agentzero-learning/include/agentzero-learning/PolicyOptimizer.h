/**
 * PolicyOptimizer.h
 *
 * Uses MOSES for policy evolution and optimization
 * Part of Agent-Zero Learning & Adaptation Phase 5
 *
 * Copyright (C) 2024 OpenCog Foundation
 */

#ifndef _AGENTZERO_POLICY_OPTIMIZER_H
#define _AGENTZERO_POLICY_OPTIMIZER_H

#include <memory>
#include <vector>
#include <functional>
#include <opencog/atomspace/AtomSpace.h>
#include <opencog/atoms/base/Handle.h>

namespace opencog { namespace agentzero {

class PolicyOptimizer
{
public:
    explicit PolicyOptimizer(AtomSpacePtr atomspace);
    virtual ~PolicyOptimizer();

    bool initialize();
    Handle optimizePolicy(const Handle& policy, 
                         const std::vector<Handle>& experiences,
                         std::function<double(const Handle&, const std::vector<Handle>&)> reward_fn);
    
protected:
    AtomSpacePtr _atomspace;
    bool _initialized;
};

} // namespace agentzero
} // namespace opencog

#endif // _AGENTZERO_POLICY_OPTIMIZER_H