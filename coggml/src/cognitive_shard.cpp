/**
 * Cognitive Shard Implementation
 */

#include "coggml/cognitive_shard.h"
#include "coggml/shard_coordinator.h"
#include <atomic>
#include <iostream>
#include <queue>
#include <mutex>

namespace coggml {

class CognitiveShard::Impl {
public:
    std::string id;
    std::string purpose;
    std::string awarenessState{"inactive"};
    std::atomic<bool> active{false};
    ProcessCallback callback;
    MessageCallback messageCallback;
    ShardCoordinator* coordinator{nullptr};
    std::queue<ShardMessage> messageQueue;
    std::mutex queueMutex;

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
    
    // Process pending messages first
    {
        std::lock_guard<std::mutex> lock(pImpl->queueMutex);
        while (!pImpl->messageQueue.empty()) {
            const auto& msg = pImpl->messageQueue.front();
            if (pImpl->messageCallback) {
                pImpl->messageCallback(msg);
            }
            pImpl->messageQueue.pop();
        }
    }
    
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

void CognitiveShard::setMessageCallback(MessageCallback callback) {
    pImpl->messageCallback = callback;
}

bool CognitiveShard::isActive() const {
    return pImpl->active;
}

void CognitiveShard::sendMessage(const std::string& receiverId, MessageType type, 
                                const std::string& payload, MessagePriority priority) {
    if (!pImpl->coordinator) {
        std::cerr << "[CogShard] " << pImpl->id << " cannot send message: no coordinator set" << std::endl;
        return;
    }
    
    ShardMessage msg(pImpl->id, receiverId, type, payload, priority);
    pImpl->coordinator->routeMessage(msg);
}

void CognitiveShard::receiveMessage(const ShardMessage& message) {
    std::lock_guard<std::mutex> lock(pImpl->queueMutex);
    pImpl->messageQueue.push(message);
    std::cout << "[CogShard] " << pImpl->id << " received message from " 
              << message.getSenderId() << std::endl;
}

void CognitiveShard::setCoordinator(ShardCoordinator* coordinator) {
    pImpl->coordinator = coordinator;
}

} // namespace coggml
