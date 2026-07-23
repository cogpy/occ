# AGI-Kern API Reference

This document provides comprehensive API documentation for the AGI-OS kernel components implemented in the OCC (OpenCog Collection) project.

## Table of Contents

1. [Overview](#overview)
2. [Security Manager](#security-manager)
3. [Memory Manager](#memory-manager)
4. [Timer Service](#timer-service)
5. [PLN (Probabilistic Logic Networks)](#pln-probabilistic-logic-networks)
6. [ECAN (Economic Attention Network)](#ecan-economic-attention-network)
7. [Integration Examples](#integration-examples)

---

## Overview

The AGI-Kern provides core kernel services for the AGI-OS cognitive architecture:

| Component | Purpose | Status |
|-----------|---------|--------|
| AGISecurityManager | Capability-based access control | ✅ Complete |
| AGI_MemoryManager | Hierarchical memory management | ✅ Complete |
| AGI_TimerService | Cognitive and physical timing | ✅ Complete |
| PLN Integration | Probabilistic reasoning | ✅ Complete |
| ECAN Integration | Attention allocation | ✅ Complete |

### Quick Start

```python
from core import (
    get_security_manager,
    get_memory_manager,
    get_timer_service,
)
from core.pln import get_inference_scheduler
from core.ecan import get_attention_bank

# Initialize core services
security = get_security_manager()
memory = get_memory_manager()
timer = get_timer_service()
pln = get_inference_scheduler()
ecan = get_attention_bank()
```

---

## Security Manager

### Module: `core.agi_security_manager`

The security manager provides capability-based access control (CBAC) for AGI-OS.

### Classes

#### `SecurityLevel`
```python
from core.security.capabilities import SecurityLevel

class SecurityLevel(Enum):
    KERNEL = 0    # Full system access
    SYSTEM = 1    # System services
    USER = 2      # Normal user operations
    GUEST = 3     # Restricted access
```

#### `Permission`
```python
from core.security.capabilities import Permission

class Permission(Enum):
    READ = "read"
    WRITE = "write"
    EXECUTE = "execute"
    DELETE = "delete"
    ADMIN = "admin"
```

#### `ResourceType`
```python
from core.security.capabilities import ResourceType

class ResourceType(Enum):
    ATOMSPACE = "atomspace"
    MEMORY = "memory"
    FILE = "file"
    NETWORK = "network"
    PROCESS = "process"
    TIMER = "timer"
    COGNITIVE = "cognitive"
```

### CapabilityManager API

#### Creating Capabilities
```python
from core.security.capabilities import CapabilityManager, SecurityLevel, Permission, ResourceType

cap_mgr = CapabilityManager()

# Create a capability
capability = cap_mgr.create_capability(
    resource_type=ResourceType.ATOMSPACE,
    resource_id="main_atomspace",
    permissions={Permission.READ, Permission.WRITE},
    owner_id="cognitive_agent_1",
    security_level=SecurityLevel.USER,
    expires_in=3600,  # Optional: expires in 1 hour
    constraints={"max_atoms": 10000}  # Optional constraints
)

print(f"Token: {capability.token}")
print(f"Capability ID: {capability.capability_id}")
```

#### Validating Capabilities
```python
# Validate a capability token
is_valid = cap_mgr.validate_capability(
    token=capability.token,
    resource_type=ResourceType.ATOMSPACE,
    resource_id="main_atomspace",
    required_permission=Permission.READ
)

if is_valid:
    print("Access granted")
else:
    print("Access denied")
```

#### Revoking Capabilities
```python
# Revoke a capability
success = cap_mgr.revoke_capability(
    capability_id=capability.capability_id,
    revoker_id="cognitive_agent_1"  # Must be owner or admin
)
```

### AGISecurityManager API

The unified security manager integrates capabilities, sandboxing, and audit logging.

```python
from core import get_security_manager
from core.security.capabilities import SecurityLevel

# Get global security manager instance
security = get_security_manager()

# Create a security session
session = security.create_session(
    principal_id="agent_1",
    security_level=SecurityLevel.USER
)

# Check access
if security.check_access(session.session_id, "atomspace", "read"):
    # Perform operation
    pass

# End session
security.end_session(session.session_id)
```

---

## Memory Manager

### Module: `core.agi_memory_manager`

Hierarchical memory management with NUMA awareness and pressure monitoring.

### Memory Tiers

```python
from core.agi_memory_manager import MemoryTier

class MemoryTier(Enum):
    L1_CACHE = 1      # Hot atoms, active attention focus
    L2_CACHE = 2      # Warm atoms, recent activity
    L3_CACHE = 3      # Cold atoms, archival
    MAIN_MEMORY = 4   # Standard storage
    SWAP = 5          # Disk-backed overflow
```

### Virtual Memory API

```python
from core.memory.virtual import VirtualMemoryManager, MemoryFlags

vm = VirtualMemoryManager()

# Allocate memory
region = vm.allocate(
    size=1024 * 1024,  # 1MB in bytes
    name="cognitive_buffer",
    flags=MemoryFlags.RW
)

# Write data
data = b"cognitive state data"
vm.write(region.region_id, 0, data)

# Read data
read_data = vm.read(region.region_id, 0, len(data))

# Free memory
vm.free(region.region_id)
```

### NUMA-Aware Allocation

```python
from core.memory.numa import NUMAAllocator, NUMAPolicy

numa = NUMAAllocator()

# Allocate with NUMA policy
allocation = numa.allocate(
    size_mb=100,
    owner="pln_inference",
    policy=NUMAPolicy.COGNITIVE,  # Attention-weighted placement
    attention_weight=0.8
)

# Migrate to a different NUMA node
numa.migrate(allocation.allocation_id, target_node=1)
```

### Memory Pressure Monitoring

```python
from core.memory.pressure import PressureMonitor, PressureLevel

monitor = PressureMonitor()

# Get current memory snapshot
snapshot = monitor.take_snapshot()
print(f"Total: {snapshot.total_mb} MB")
print(f"Used: {snapshot.used_mb} MB")
print(f"Pressure: {snapshot.pressure_level}")

# Get throttling recommendation
rec = monitor.get_throttle_recommendation()
if rec.level >= PressureLevel.HIGH:
    print(f"Throttle by factor: {rec.throttle_factor}")
```

### AGI_MemoryManager API

```python
from core import get_memory_manager
from core.agi_memory_manager import MemoryTier

memory = get_memory_manager()

# Allocate in specific tier
allocation = memory.allocate(
    size_mb=50.0,
    tier=MemoryTier.L2_CACHE,
    owner="attention_system"
)

# Map an AtomSpace
mapped = memory.map_atomspace(
    atomspace_id="main_as",
    tier=MemoryTier.L1_CACHE
)

# Migrate between tiers
memory.migrate_tier(allocation.allocation_id, MemoryTier.L3_CACHE)

# Get statistics
stats = memory.get_statistics()
print(f"Total allocations: {stats['total_allocations']}")
```

---

## Timer Service

### Module: `core.agi_timer_service`

Unified timer management with cognitive time support.

### Timer Types

```python
from core.agi_timer_service import TimerType

class TimerType(Enum):
    ONESHOT = 1       # Single trigger
    PERIODIC = 2      # Repeating
    DEADLINE = 3      # Must-complete-by
    COGNITIVE = 4     # Event-based logical time
```

### Cognitive Time

```python
from core.time.cognitive_time import CognitiveTimeManager, TimeScale

ctm = CognitiveTimeManager()

# Get current cognitive time
now = ctm.now()
print(f"Micro: {now.micro_ticks}, Meso: {now.meso_ticks}, Macro: {now.macro_ticks}")

# Advance cognitive time
ctm.advance(
    scale=TimeScale.MICRO,
    ticks=10,
    event="inference_complete",
    source="pln"
)

# Get event history
history = ctm.get_history(count=10)
```

### Timer Coalescing

```python
from core.time.coalescing import TimerCoalescer, CoalescingPolicy, TimerPriority

coalescer = TimerCoalescer(policy=CoalescingPolicy.MODERATE)

# Schedule a timer
timer_id = coalescer.schedule(
    callback=lambda: print("Timer fired!"),
    delay_seconds=1.0,
    priority=TimerPriority.NORMAL,
    source="test"
)

# Cancel a timer
coalescer.cancel(timer_id)

# Process timers (in main loop)
coalescer.process()
```

### AGI_TimerService API

```python
from core import get_timer_service
from core.agi_timer_service import TimerType

timer = get_timer_service()

# Schedule oneshot timer
handle = timer.schedule_timer(
    callback=lambda: print("Done!"),
    delay_seconds=5.0,
    timer_type=TimerType.ONESHOT,
    name="task_timeout"
)

# Schedule periodic timer
periodic_handle = timer.schedule_timer(
    callback=lambda: print("Tick!"),
    delay_seconds=1.0,
    timer_type=TimerType.PERIODIC,
    name="heartbeat"
)

# Schedule cognitive timer
cognitive_handle = timer.schedule_cognitive_timer(
    callback=lambda: print("Cognitive event!"),
    micro_ticks=100,
    name="inference_trigger"
)

# Set deadline alert
timer.set_deadline_alert(
    task_id="critical_inference",
    deadline_seconds=10.0,
    callback=lambda task_id, remaining: print(f"Warning: {remaining}s left!")
)

# Cancel timer
timer.cancel_timer(handle)
```

---

## PLN (Probabilistic Logic Networks)

### Module: `core.pln`

Probabilistic reasoning with truth value propagation.

### Truth Values

```python
from core.pln.truth_value import SimpleTruthValue, IndefiniteTruthValue, DistributionalTruthValue

# Simple Truth Value (strength, confidence)
stv = SimpleTruthValue(strength=0.8, confidence_value=0.9)
print(f"Mean: {stv.mean}, Confidence: {stv.confidence}")

# Indefinite Truth Value (interval)
itv = IndefiniteTruthValue(lower=0.6, upper=0.9, confidence_level=0.95)
print(f"Mean: {itv.mean}, Width: {itv.width}")

# Distributional Truth Value (histogram)
dtv = DistributionalTruthValue(histogram=[0.1, 0.2, 0.4, 0.2, 0.1])
print(f"Mean: {dtv.mean}, Variance: {dtv.variance}")
```

### PLN Formulas

```python
from core.pln.truth_value import revision, deduction, modus_ponens, and_formula, or_formula

tv1 = SimpleTruthValue(0.8, 0.9)
tv2 = SimpleTruthValue(0.7, 0.85)

# Revision (merge evidence)
merged = revision(tv1, tv2)

# Deduction (A→B ∧ B→C ⇒ A→C)
tv_ab = SimpleTruthValue(0.9, 0.8)
tv_bc = SimpleTruthValue(0.85, 0.75)
tv_ac = deduction(tv_ab, tv_bc)

# Modus Ponens (A ∧ A→B ⇒ B)
tv_a = SimpleTruthValue(0.95, 0.9)
tv_b = modus_ponens(tv_a, tv_ab)

# Boolean operations
tv_and = and_formula(tv1, tv2)
tv_or = or_formula(tv1, tv2)
```

### Rule Executor

```python
from core.pln.rule_executor import RuleExecutor, Atom, RuleType
from core.pln.truth_value import SimpleTruthValue

executor = RuleExecutor()

# Create atoms
atom_ab = Atom(
    atom_id="impl_AB",
    atom_type="ImplicationLink",
    truth_value=SimpleTruthValue(0.9, 0.8),
    outgoing=["A", "B"]
)

atom_bc = Atom(
    atom_id="impl_BC",
    atom_type="ImplicationLink",
    truth_value=SimpleTruthValue(0.85, 0.75),
    outgoing=["B", "C"]
)

# Execute deduction rule
result = executor.execute("deduction", [atom_ab, atom_bc])
if result.success:
    print(f"A→C: {result.output_tv.mean}")
```

### Inference Scheduler

```python
from core.pln.inference_scheduler import InferenceScheduler, InferenceTask, InferenceMode, InferenceConfig
from core.pln.truth_value import SimpleTruthValue

scheduler = InferenceScheduler()

# Create inference task
task = scheduler.create_task(
    goal="find_path",
    premises=["A", "B"],
    target_confidence=0.8,
    mode=InferenceMode.FORWARD,
    max_steps=100
)

# Run inference
for step in range(10):
    if scheduler.step(task.task_id):
        break

# Get result
result = scheduler.get_result(task.task_id)
print(f"Steps: {result.steps_taken}, Conclusions: {len(result.conclusions)}")
```

---

## ECAN (Economic Attention Network)

### Module: `core.ecan`

Attention allocation based on economic principles.

### Attention Values

```python
from core.ecan.attention_bank import AttentionBank, AttentionConfig

bank = AttentionBank()

# Set attention value
av = bank.set("atom_1", sti=50.0, lti=20.0)
print(f"STI: {av.sti}, LTI: {av.lti}")

# Stimulate (increase STI)
av = bank.stimulate("atom_1", amount=30.0)

# Inhibit (decrease STI)
av = bank.inhibit("atom_1", amount=10.0)

# Get attentional focus (high-STI atoms)
focus = bank.get_attentional_focus()
for atom_id, av in focus:
    print(f"{atom_id}: STI={av.sti}")
```

### Importance Spreading

```python
from core.ecan.importance_spreading import ImportanceSpreader, SpreadingMode

spreader = ImportanceSpreader(attention_bank=bank)

# Add links between atoms
spreader._hypergraph.add_link("atom_1", "atom_2", weight=0.8)
spreader._hypergraph.add_link("atom_1", "atom_3", weight=0.5)

# Spread importance
events = spreader.spread("atom_1", SpreadingMode.DIFFUSION)
print(f"Spread to {len(events)} neighbors")

# Hebbian spreading (uses link weights)
events = spreader.spread("atom_1", SpreadingMode.HEBBIAN)
```

### Hebbian Learning

```python
from core.ecan.hebbian import HebbianManager

hebbian = HebbianManager(attention_bank=bank)

# Record activations (will create/strengthen links)
hebbian.record_activation("atom_1")
time.sleep(0.1)  # Within co-activation window
hebbian.record_activation("atom_2")

# Get Hebbian link
link = hebbian.get_link("atom_1", "atom_2")
if link:
    print(f"Link strength: {link.strength}")

# Simulate learning cycle
result = hebbian.simulate_learning_cycle(["atom_1", "atom_2", "atom_3"])
print(f"Links created: {result['created']}, strengthened: {result['strengthened']}")
```

---

## Integration Examples

### Example 1: Attention-Guided Inference

```python
from core.ecan.attention_bank import AttentionBank
from core.pln.inference_scheduler import InferenceScheduler, InferenceMode

# Setup
bank = AttentionBank()
scheduler = InferenceScheduler()

# Set attention on relevant atoms
bank.set("premise_1", sti=80.0)
bank.set("premise_2", sti=75.0)
bank.set("irrelevant", sti=10.0)

# Create inference task
task = scheduler.create_task(
    goal="derive_conclusion",
    premises=["premise_1", "premise_2"],
    mode=InferenceMode.FOCUSED  # Uses attention to guide inference
)

# Run with attention-weighted rule selection
while not scheduler.is_complete(task.task_id):
    scheduler.step(task.task_id)
```

### Example 2: Secure Cognitive Processing

```python
from core import get_security_manager, get_memory_manager
from core.security.capabilities import SecurityLevel, Permission, ResourceType

# Get services
security = get_security_manager()
memory = get_memory_manager()

# Create session
session = security.create_session("cognitive_agent", SecurityLevel.USER)

# Request capability for memory access
cap = security.request_capability(
    session.session_id,
    ResourceType.MEMORY,
    "cognitive_buffer",
    {Permission.READ, Permission.WRITE}
)

if cap:
    # Allocate memory with capability
    allocation = memory.allocate(
        size_mb=100.0,
        tier=MemoryTier.L1_CACHE,
        owner=session.session_id
    )
    
    # Use memory...
    
    # Free and end session
    memory.free(allocation.allocation_id)
    security.end_session(session.session_id)
```

### Example 3: Timed Cognitive Operations

```python
from core import get_timer_service
from core.time.cognitive_time import CognitiveTimeManager
from core.pln.inference_scheduler import InferenceScheduler

# Setup
timer = get_timer_service()
ctm = CognitiveTimeManager()
scheduler = InferenceScheduler()

# Create inference task
task = scheduler.create_task(goal="complex_inference")

# Set deadline
timer.set_deadline_alert(
    task_id=task.task_id,
    deadline_seconds=5.0,
    callback=lambda tid, remaining: print(f"Warning: {remaining}s remaining!")
)

# Run with cognitive time tracking
while not scheduler.is_complete(task.task_id):
    scheduler.step(task.task_id)
    ctm.advance(TimeScale.MICRO, 1, "inference_step", "pln")

print(f"Completed in {ctm.now().total_micro_ticks()} cognitive ticks")
```

---

## Module Summary

| Module | Key Classes | Global Accessor |
|--------|-------------|-----------------|
| `core.agi_security_manager` | `AGISecurityManager`, `SecuritySession` | `get_security_manager()` |
| `core.security.capabilities` | `CapabilityManager`, `Capability` | `get_capability_manager()` |
| `core.security.sandbox` | `Sandbox`, `SandboxedModule` | N/A |
| `core.security.audit` | `SecurityAuditLog`, `AuditEvent` | `get_audit_log()` |
| `core.agi_memory_manager` | `AGI_MemoryManager`, `MappedAtomSpace` | `get_memory_manager()` |
| `core.memory.virtual` | `VirtualMemoryManager`, `MemoryRegion` | N/A |
| `core.memory.numa` | `NUMAAllocator`, `NUMAAllocation` | N/A |
| `core.memory.pressure` | `PressureMonitor`, `PressureLevel` | N/A |
| `core.agi_timer_service` | `AGI_TimerService`, `TimerHandle` | `get_timer_service()` |
| `core.time.cognitive_time` | `CognitiveTimeManager`, `CognitiveTimestamp` | N/A |
| `core.time.coalescing` | `TimerCoalescer`, `CoalescingPolicy` | N/A |
| `core.pln.truth_value` | `SimpleTruthValue`, `IndefiniteTruthValue` | N/A |
| `core.pln.rule_executor` | `RuleExecutor`, `Atom`, `RuleResult` | `get_rule_executor()` |
| `core.pln.inference_scheduler` | `InferenceScheduler`, `InferenceTask` | `get_inference_scheduler()` |
| `core.ecan.attention_bank` | `AttentionBank`, `AttentionValue` | `get_attention_bank()` |
| `core.ecan.importance_spreading` | `ImportanceSpreader`, `SpreadingMode` | N/A |
| `core.ecan.hebbian` | `HebbianManager`, `HebbianLink` | N/A |

---

## Version History

| Version | Date | Changes |
|---------|------|---------|
| 0.2.0 | 2026-07-23 | Added PLN, ECAN, Timer, Memory, Security |
| 0.1.0 | 2026-07-22 | Initial AGI_Boot, AGI_Scheduler, AGI_EventBus |

---

## See Also

- [AGI-OS Architecture](AGI_OS_ARCHITECTURE.md)
- [Cognitive Synergy](cognitive-synergy.md)
- [Build Guide](BUILD_GUIDE.md)
