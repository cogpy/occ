/**
 * Cognitive Shard
 * Self-contained cognitive processing unit with awareness
 */

#ifndef _COGGML_COGNITIVE_SHARD_H
#define _COGGML_COGNITIVE_SHARD_H

#include <memory>
#include <string>
#include <functional>

namespace coggml {

/**
 * CognitiveShard - Self-aware processing unit
 * Encapsulates a cognitive process with self-awareness capabilities
 */
class CognitiveShard {
public:
    using ProcessCallback = std::function<void(const std::string&)>;

    CognitiveShard(const std::string& id, const std::string& purpose);
    ~CognitiveShard();

    // Get shard ID
    std::string getId() const;

    // Get shard purpose/role
    std::string getPurpose() const;

    // Execute shard's cognitive process
    void execute();

    // Update self-awareness state
    void updateAwareness(const std::string& state);

    // Get current awareness state
    std::string getAwarenessState() const;

    // Set process callback
    void setProcessCallback(ProcessCallback callback);

    // Check if shard is active
    bool isActive() const;

private:
    class Impl;
    std::unique_ptr<Impl> pImpl;
};

} // namespace coggml

#endif // _COGGML_COGNITIVE_SHARD_H
