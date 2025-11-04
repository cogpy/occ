/**
 * Shard Coordinator Implementation
 */

#include "coggml/shard_coordinator.h"
#include <algorithm>
#include <iostream>
#include <mutex>
#include <chrono>

namespace coggml {

class ShardCoordinator::Impl {
public:
    std::vector<std::shared_ptr<CognitiveShard>> shards;
    std::mutex coordMutex;
    
    // Communication statistics
    size_t totalMessagesSent{0};
    size_t totalMessagesDelivered{0};
    std::vector<double> deliveryTimes;

    Impl() = default;
};

ShardCoordinator::ShardCoordinator() : pImpl(std::make_unique<Impl>()) {
    std::cout << "[ShardCoordinator] Coordinator initialized" << std::endl;
}

ShardCoordinator::~ShardCoordinator() = default;

void ShardCoordinator::registerShard(std::shared_ptr<CognitiveShard> shard) {
    if (!shard) {
        return;
    }

    std::lock_guard<std::mutex> lock(pImpl->coordMutex);
    pImpl->shards.push_back(shard);
    
    // Set coordinator reference in the shard for message routing
    shard->setCoordinator(this);
    
    std::cout << "[ShardCoordinator] Registered shard: " << shard->getId() << std::endl;
}

void ShardCoordinator::unregisterShard(const std::string& shardId) {
    std::lock_guard<std::mutex> lock(pImpl->coordMutex);
    
    auto it = std::remove_if(pImpl->shards.begin(), pImpl->shards.end(),
        [&shardId](const std::shared_ptr<CognitiveShard>& shard) {
            return shard->getId() == shardId;
        });
    
    if (it != pImpl->shards.end()) {
        // Clear coordinator reference
        for (auto sit = it; sit != pImpl->shards.end(); ++sit) {
            (*sit)->setCoordinator(nullptr);
        }
        pImpl->shards.erase(it, pImpl->shards.end());
        std::cout << "[ShardCoordinator] Unregistered shard: " << shardId << std::endl;
    }
}

void ShardCoordinator::coordinate() {
    std::cout << "[ShardCoordinator] Coordinating " << pImpl->shards.size() << " shards" << std::endl;
    
    for (auto& shard : pImpl->shards) {
        if (shard && shard->isActive()) {
            shard->execute();
        }
    }
}

std::shared_ptr<CognitiveShard> ShardCoordinator::getShard(const std::string& shardId) const {
    auto it = std::find_if(pImpl->shards.begin(), pImpl->shards.end(),
        [&shardId](const std::shared_ptr<CognitiveShard>& shard) {
            return shard->getId() == shardId;
        });
    
    if (it != pImpl->shards.end()) {
        return *it;
    }
    
    return nullptr;
}

std::vector<std::shared_ptr<CognitiveShard>> ShardCoordinator::getAllShards() const {
    return pImpl->shards;
}

size_t ShardCoordinator::getActiveShardCount() const {
    return std::count_if(pImpl->shards.begin(), pImpl->shards.end(),
        [](const std::shared_ptr<CognitiveShard>& shard) {
            return shard && shard->isActive();
        });
}

void ShardCoordinator::optimizeSynergy() {
    std::lock_guard<std::mutex> lock(pImpl->coordMutex);
    
    std::cout << "[ShardCoordinator] Optimizing cognitive synergy across " 
              << getActiveShardCount() << " active shards" << std::endl;
    
    // Optimization logic for cognitive synergy
    for (auto& shard : pImpl->shards) {
        if (shard && shard->isActive()) {
            shard->updateAwareness("synergy-optimized");
        }
    }
}

void ShardCoordinator::routeMessage(const ShardMessage& message) {
    auto startTime = std::chrono::steady_clock::now();
    
    std::lock_guard<std::mutex> lock(pImpl->coordMutex);
    pImpl->totalMessagesSent++;
    
    if (message.isBroadcast()) {
        // Broadcast to all shards except sender
        size_t delivered = 0;
        for (auto& shard : pImpl->shards) {
            if (shard && shard->isActive() && shard->getId() != message.getSenderId()) {
                shard->receiveMessage(message);
                delivered++;
            }
        }
        pImpl->totalMessagesDelivered += delivered;
        std::cout << "[ShardCoordinator] Broadcast message from " << message.getSenderId() 
                  << " to " << delivered << " shards" << std::endl;
    } else {
        // Route to specific shard
        auto target = getShard(message.getReceiverId());
        if (target && target->isActive()) {
            target->receiveMessage(message);
            pImpl->totalMessagesDelivered++;
            std::cout << "[ShardCoordinator] Routed message from " << message.getSenderId() 
                      << " to " << message.getReceiverId() << std::endl;
        } else {
            std::cerr << "[ShardCoordinator] Failed to route message: receiver " 
                      << message.getReceiverId() << " not found or inactive" << std::endl;
        }
    }
    
    // Track delivery time
    auto endTime = std::chrono::steady_clock::now();
    auto duration = std::chrono::duration_cast<std::chrono::microseconds>(endTime - startTime);
    pImpl->deliveryTimes.push_back(duration.count() / 1000.0); // Convert to milliseconds
}

ShardCoordinator::CommunicationStats ShardCoordinator::getCommunicationStats() const {
    std::lock_guard<std::mutex> lock(pImpl->coordMutex);
    
    CommunicationStats stats;
    stats.totalMessagesSent = pImpl->totalMessagesSent;
    stats.totalMessagesDelivered = pImpl->totalMessagesDelivered;
    stats.messagesInFlight = stats.totalMessagesSent - stats.totalMessagesDelivered;
    
    // Calculate average delivery time
    if (!pImpl->deliveryTimes.empty()) {
        double sum = 0.0;
        for (double time : pImpl->deliveryTimes) {
            sum += time;
        }
        stats.averageDeliveryTimeMs = sum / pImpl->deliveryTimes.size();
    } else {
        stats.averageDeliveryTimeMs = 0.0;
    }
    
    return stats;
}

} // namespace coggml
