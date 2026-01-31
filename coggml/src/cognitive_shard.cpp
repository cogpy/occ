/**
 * Cognitive Shard Implementation
 */

#include "coggml/cognitive_shard.h"
#include <atomic>
#include <iostream>

namespace coggml {

class CognitiveShard::Impl {
public:
    std::string id;
    std::string purpose;
    std::string awarenessState{"inactive"};
    std::atomic<bool> active{false};
    ProcessCallback callback;

    Impl(const std::string& shardId, const std::string& shardPurpose)
        : id(shardId), purpose(shardPurpose) {}
};

CognitiveShard::CognitiveShard(const std::string& id, const std::string& purpose)
    : pImpl(std::make_unique<Impl>(id, purpose)) {
    pImpl->active = true;
    std::cout << "[CogShard] Created shard '" << id << "' with purpose: " << purpose << std::endl;
}

CognitiveShard::~CognitiveShard() {
    pImpl->active = false;
}

std::string CognitiveShard::getId() const {
    return pImpl->id;
}

std::string CognitiveShard::getPurpose() const {
    return pImpl->purpose;
}

void CognitiveShard::execute() {
    if (!pImpl->active) {
        return;
    }

    pImpl->awarenessState = "executing";
    
    if (pImpl->callback) {
        pImpl->callback("Shard " + pImpl->id + " executing: " + pImpl->purpose);
    }

    // Cognitive execution logic
    std::cout << "[CogShard] " << pImpl->id << " executing cognitive process" << std::endl;
    
    pImpl->awarenessState = "completed";
}

void CognitiveShard::updateAwareness(const std::string& state) {
    pImpl->awarenessState = state;
    std::cout << "[CogShard] " << pImpl->id << " awareness updated: " << state << std::endl;
}

std::string CognitiveShard::getAwarenessState() const {
    return pImpl->awarenessState;
}

void CognitiveShard::setProcessCallback(ProcessCallback callback) {
    pImpl->callback = callback;
}

bool CognitiveShard::isActive() const {
    return pImpl->active;
}

} // namespace coggml
