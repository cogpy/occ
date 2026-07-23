#!/usr/bin/env python3
"""
AGI-OS Timer and Clock Services Package

This package provides timer management and cognitive time tracking
for the OpenCog AGI-OS cognitive architecture.

Modules:
- cognitive_time: Cognitive time tracking (event-based logical time)
- coalescing: Timer coalescing for efficiency
"""

from core.time.cognitive_time import (
    CognitiveTimestamp,
    CognitiveTimeManager,
    TimeScale,
    get_cognitive_time_manager
)

from core.time.coalescing import (
    TimerCoalescer,
    CoalescedTimer,
    CoalescingPolicy,
    get_timer_coalescer
)

__all__ = [
    # Cognitive Time
    "CognitiveTimestamp",
    "CognitiveTimeManager",
    "TimeScale",
    "get_cognitive_time_manager",
    
    # Coalescing
    "TimerCoalescer",
    "CoalescedTimer",
    "CoalescingPolicy",
    "get_timer_coalescer",
]
