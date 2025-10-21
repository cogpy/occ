/**
 * Shard Coordinator
 * Coordinates multiple cognitive shards for synergistic processing
 */

#ifndef _COGGML_SHARD_COORDINATOR_H
#define _COGGML_SHARD_COORDINATOR_H

#include <memory>
#include <vector>
#include <string>
#include "cognitive_shard.h"

namespace coggml {

/**
 * ShardCoordinator - Manages and coordinates cognitive shards
 * Facilitates cognitive synergy through shard interaction
 */
class ShardCoordinator {
public:
    ShardCoordinator();
    ~ShardCoordinator();

    // Register a cognitive shard
    void registerShard(std::shared_ptr<CognitiveShard> shard);

    // Unregister a cognitive shard
    void unregisterShard(const std::string& shardId);

    // Coordinate all shards
    void coordinate();

    // Get shard by ID
    std::shared_ptr<CognitiveShard> getShard(const std::string& shardId) const;

    // Get all shards
    std::vector<std::shared_ptr<CognitiveShard>> getAllShards() const;

    // Get number of active shards
    size_t getActiveShardCount() const;

    // Optimize shard interactions for cognitive synergy
    void optimizeSynergy();

private:
    class Impl;
    std::unique_ptr<Impl> pImpl;
};

} // namespace coggml

#endif // _COGGML_SHARD_COORDINATOR_H
