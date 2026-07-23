"""
AGI-OS Memory Module

Provides hierarchical memory management for the AGI-OS:
- Virtual memory abstraction
- NUMA-aware allocation
- Memory pressure monitoring
- AtomSpace memory coordination
"""

from .virtual import (
    VirtualMemoryManager,
    MemoryRegion,
    MemoryFlags,
    get_virtual_memory_manager
)

from .numa import (
    NUMAAllocator,
    NUMANode,
    NUMAAllocation,
    NUMAPolicy,
    get_numa_allocator
)

from .pressure import (
    PressureMonitor,
    PressureLevel,
    PressureThresholds,
    MemorySnapshot,
    ThrottleRecommendation,
    get_pressure_monitor
)

__all__ = [
    # Virtual Memory
    'VirtualMemoryManager',
    'MemoryRegion',
    'MemoryFlags',
    'get_virtual_memory_manager',
    
    # NUMA
    'NUMAAllocator',
    'NUMANode',
    'NUMAAllocation',
    'NUMAPolicy',
    'get_numa_allocator',
    
    # Pressure
    'PressureMonitor',
    'PressureLevel',
    'PressureThresholds',
    'MemorySnapshot',
    'ThrottleRecommendation',
    'get_pressure_monitor'
]
