/**
 * Shard Coordinator Implementation
 */

#include "coggml/shard_coordinator.h"
#include <algorithm>
#include <iostream>

namespace coggml {

class ShardCoordinator::Impl {
public:
    std::vector<std::shared_ptr<CognitiveShard>> shards;

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

    pImpl->shards.push_back(shard);
    std::cout << "[ShardCoordinator] Registered shard: " << shard->getId() << std::endl;
}

void ShardCoordinator::unregisterShard(const std::string& shardId) {
    auto it = std::remove_if(pImpl->shards.begin(), pImpl->shards.end(),
        [&shardId](const std::shared_ptr<CognitiveShard>& shard) {
            return shard->getId() == shardId;
        });
    
    if (it != pImpl->shards.end()) {
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
    std::cout << "[ShardCoordinator] Optimizing cognitive synergy across " 
              << getActiveShardCount() << " active shards" << std::endl;
    
    // Optimization logic for cognitive synergy
    for (auto& shard : pImpl->shards) {
        if (shard && shard->isActive()) {
            shard->updateAwareness("synergy-optimized");
        }
    }
}

} // namespace coggml
