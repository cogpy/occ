"""
AGI-OS Core Module

This module provides the core kernel components for the AGI-OS:
- AGI_Boot: Unified boot orchestrator
- AGI_Scheduler: Priority-aware task scheduler
- AGI_EventBus: Event/interrupt controller
- AGI_SecurityManager: Capability-based access control
- AGI_MemoryManager: Hierarchical memory management
- AGI_TimerService: Centralized timer management

These components address critical gaps identified in the AGI-Kern evaluation:
- Boot/Init: 65% -> target 90%
- Scheduling: 40% -> target 75%
- Interrupts: 20% -> target 60%
- Security: 30% -> target 80%
- Memory: 50% -> target 85%
- Timers: 50% -> target 80%
"""

from .agi_boot import (
    AGIBootOrchestrator,
    BootConfig,
    BootPhase,
    BootComponent,
    ComponentStatus,
    get_boot_orchestrator,
    boot_agi_os
)

from .agi_scheduler import (
    AGIScheduler,
    CognitiveTask,
    TaskPriority,
    TaskState,
    TaskType,
    get_scheduler
)

from .agi_event_bus import (
    AGIEventBus,
    CognitiveEvent,
    EventPriority,
    EventCategory,
    EventHandler,
    EventTypes,
    get_event_bus
)

from .agi_security_manager import (
    AGISecurityManager,
    SecuritySession,
    get_security_manager
)

from .agi_memory_manager import (
    AGI_MemoryManager,
    MemoryTier,
    MemoryAllocation,
    MappedAtomSpace,
    get_memory_manager
)

from .agi_timer_service import (
    AGI_TimerService,
    TimerHandle,
    TimerType,
    DeadlineAlert,
    get_timer_service
)

__all__ = [
    # Boot
    'AGIBootOrchestrator',
    'BootConfig',
    'BootPhase',
    'BootComponent',
    'ComponentStatus',
    'get_boot_orchestrator',
    'boot_agi_os',

    # Scheduler
    'AGIScheduler',
    'CognitiveTask',
    'TaskPriority',
    'TaskState',
    'TaskType',
    'get_scheduler',

    # Event Bus
    'AGIEventBus',
    'CognitiveEvent',
    'EventPriority',
    'EventCategory',
    'EventHandler',
    'EventTypes',
    'get_event_bus',
    
    # Security
    'AGI_SecurityManager',
    'SecuritySession',
    'get_security_manager',
    
    # Memory
    'AGI_MemoryManager',
    'MemoryTier',
    'MemoryAllocation',
    'MappedAtomSpace',
    'get_memory_manager',
    
    # Timer
    'AGI_TimerService',
    'TimerHandle',
    'TimerType',
    'DeadlineAlert',
    'get_timer_service',
]

__version__ = '0.2.0'
