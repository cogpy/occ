/**
 * AGI Goal Tracker Implementation
 */

#include "cogself/agi_goal_tracker.h"
#include <map>
#include <sstream>
#include <iostream>
#include <algorithm>

namespace cogself {

struct Goal {
    std::string name;
    double targetValue;
    double currentValue;

    Goal() : targetValue(1.0), currentValue(0.0) {}
    Goal(const std::string& n, double target) 
        : name(n), targetValue(target), currentValue(0.0) {}
};

class AGIGoalTracker::Impl {
public:
    std::map<std::string, Goal> goals;
};

AGIGoalTracker::AGIGoalTracker() : pImpl(std::make_unique<Impl>()) {
    std::cout << "[AGIGoalTracker] Initialized" << std::endl;
}

AGIGoalTracker::~AGIGoalTracker() = default;

void AGIGoalTracker::addGoal(const std::string& goalName, double targetValue) {
    pImpl->goals[goalName] = Goal(goalName, targetValue);
    std::cout << "[AGIGoalTracker] Added goal: " << goalName 
              << " (target: " << targetValue << ")" << std::endl;
}

void AGIGoalTracker::updateProgress(const std::string& goalName, double currentValue) {
    auto it = pImpl->goals.find(goalName);
    if (it != pImpl->goals.end()) {
        it->second.currentValue = currentValue;
        std::cout << "[AGIGoalTracker] Updated " << goalName 
                  << " progress: " << currentValue << std::endl;
    }
}

double AGIGoalTracker::getProgress(const std::string& goalName) const {
    auto it = pImpl->goals.find(goalName);
    if (it != pImpl->goals.end()) {
        return it->second.currentValue / it->second.targetValue;
    }
    return 0.0;
}

std::vector<std::string> AGIGoalTracker::getAllGoals() const {
    std::vector<std::string> goalNames;
    for (const auto& [name, goal] : pImpl->goals) {
        goalNames.push_back(name);
    }
    return goalNames;
}

double AGIGoalTracker::calculateOverallProgress() const {
    if (pImpl->goals.empty()) {
        return 0.0;
    }

    double totalProgress = 0.0;
    for (const auto& [name, goal] : pImpl->goals) {
        totalProgress += (goal.currentValue / goal.targetValue);
    }

    return totalProgress / pImpl->goals.size();
}

std::string AGIGoalTracker::generateProgressReport() const {
    std::stringstream ss;
    ss << "AGI Goal Progress Report:\n";
    ss << "Overall Progress: " << (calculateOverallProgress() * 100.0) << "%\n\n";
    
    for (const auto& [name, goal] : pImpl->goals) {
        double progress = (goal.currentValue / goal.targetValue) * 100.0;
        ss << "  " << name << ": " << progress << "% ";
        ss << "(" << goal.currentValue << "/" << goal.targetValue << ")\n";
    }
    
    return ss.str();
}

std::string AGIGoalTracker::identifyPriorityGoals() const {
    std::stringstream ss;
    ss << "Priority Goals for Improvement:\n";
    
    // Find goals with lowest progress
    std::vector<std::pair<std::string, double>> goalProgress;
    for (const auto& [name, goal] : pImpl->goals) {
        double progress = goal.currentValue / goal.targetValue;
        goalProgress.push_back({name, progress});
    }
    
    std::sort(goalProgress.begin(), goalProgress.end(),
        [](const auto& a, const auto& b) { return a.second < b.second; });
    
    for (size_t i = 0; i < std::min(size_t(3), goalProgress.size()); ++i) {
        ss << "  " << (i+1) << ". " << goalProgress[i].first 
           << " (current: " << (goalProgress[i].second * 100.0) << "%)\n";
    }
    
    return ss.str();
}

} // namespace cogself
