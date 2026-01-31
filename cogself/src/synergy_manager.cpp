/**
 * Synergy Manager Implementation
 */

#include "cogself/synergy_manager.h"
#include <algorithm>
#include <sstream>
#include <iostream>

namespace cogself {

class SynergyManager::Impl {
public:
    std::vector<std::string> components;
    double baselineSynergy{0.1};
    size_t optimizationCount{0};
};

SynergyManager::SynergyManager() : pImpl(std::make_unique<Impl>()) {
    std::cout << "[SynergyManager] Initialized" << std::endl;
}

SynergyManager::~SynergyManager() = default;

double SynergyManager::calculateSynergyLevel() const {
    if (pImpl->components.empty()) {
        return pImpl->baselineSynergy;
    }

    // Synergy increases with more integrated components
    double componentBonus = pImpl->components.size() * 0.05;
    double optimizationBonus = pImpl->optimizationCount * 0.02;
    
    double synergy = pImpl->baselineSynergy + componentBonus + optimizationBonus;
    return std::min(synergy, 1.0);
}

void SynergyManager::optimizeSynergyPathways() {
    pImpl->optimizationCount++;
    std::cout << "[SynergyManager] Optimizing synergy pathways (iteration " 
              << pImpl->optimizationCount << ")" << std::endl;
    
    // Optimization logic would analyze component interactions
    std::cout << "[SynergyManager] Analyzing " << pImpl->components.size() 
              << " component interactions" << std::endl;
}

void SynergyManager::addComponent(const std::string& componentName) {
    pImpl->components.push_back(componentName);
    std::cout << "[SynergyManager] Added component: " << componentName << std::endl;
}

void SynergyManager::removeComponent(const std::string& componentName) {
    auto it = std::remove(pImpl->components.begin(), pImpl->components.end(), componentName);
    if (it != pImpl->components.end()) {
        pImpl->components.erase(it, pImpl->components.end());
        std::cout << "[SynergyManager] Removed component: " << componentName << std::endl;
    }
}

std::vector<std::string> SynergyManager::getComponents() const {
    return pImpl->components;
}

std::string SynergyManager::getSynergyMetrics() const {
    std::stringstream ss;
    ss << "Synergy Metrics:\n";
    ss << "  Total Components: " << pImpl->components.size() << "\n";
    ss << "  Current Synergy Level: " << calculateSynergyLevel() << "\n";
    ss << "  Optimization Iterations: " << pImpl->optimizationCount << "\n";
    ss << "  Active Components:\n";
    for (const auto& comp : pImpl->components) {
        ss << "    - " << comp << "\n";
    }
    return ss.str();
}

} // namespace cogself
