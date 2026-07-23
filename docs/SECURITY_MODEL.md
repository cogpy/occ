# AGI-OS Security Model

This document describes the security architecture and model for the AGI-OS cognitive operating system implemented in the OCC (OpenCog Collection) project.

## Table of Contents

1. [Overview](#overview)
2. [Security Principles](#security-principles)
3. [Capability-Based Access Control](#capability-based-access-control)
4. [Security Levels](#security-levels)
5. [Resource Protection](#resource-protection)
6. [Module Sandboxing](#module-sandboxing)
7. [Audit and Monitoring](#audit-and-monitoring)
8. [Threat Model](#threat-model)
9. [Implementation Details](#implementation-details)

---

## Overview

The AGI-OS security model is designed to protect cognitive resources and ensure safe operation of AGI components. It uses a **capability-based access control (CBAC)** system combined with **hierarchical security levels** and **comprehensive audit logging**.

### Key Features

- **Capability-Based Access Control**: Fine-grained permissions via unforgeable tokens
- **Hierarchical Security Levels**: KERNEL → SYSTEM → USER → GUEST
- **Module Sandboxing**: Resource-limited execution environments
- **Tamper-Resistant Audit Logging**: Cryptographically protected audit trails
- **Cognitive-Aware Security**: Integration with attention and inference systems

### Security Goals

| Goal | Description |
|------|-------------|
| **Confidentiality** | Protect sensitive knowledge and inferences |
| **Integrity** | Prevent unauthorized modification of cognitive state |
| **Availability** | Ensure cognitive services remain operational |
| **Accountability** | Track all security-relevant actions |
| **Isolation** | Contain compromised components |

---

## Security Principles

### 1. Principle of Least Privilege

Every cognitive component receives only the minimum capabilities required for its function.

```python
# Bad: Granting excessive permissions
cap = cap_mgr.create_capability(
    permissions={Permission.READ, Permission.WRITE, Permission.DELETE, Permission.ADMIN},
    ...
)

# Good: Granting minimal necessary permissions
cap = cap_mgr.create_capability(
    permissions={Permission.READ},  # Only what's needed
    ...
)
```

### 2. Defense in Depth

Multiple security layers protect critical resources:

1. **Capability Validation**: Token-based access control
2. **Security Level Check**: Hierarchical privilege verification
3. **Resource Constraints**: Quota and limit enforcement
4. **Audit Logging**: All actions recorded
5. **Sandboxing**: Process isolation

### 3. Fail-Safe Defaults

Access is denied by default; explicit grants are required.

```python
# Default: no access
def check_access(capability, resource, permission):
    if capability is None:
        return False  # Deny by default
    
    if capability.is_expired():
        return False  # Expired capabilities denied
    
    if permission not in capability.permissions:
        return False  # Missing permission denied
    
    return True  # Only explicitly granted access allowed
```

### 4. Complete Mediation

Every access to a cognitive resource is checked.

```python
class SecuredAtomSpace:
    def get_atom(self, atom_id, capability):
        # Always check capability
        if not self.security.check_access(capability, atom_id, Permission.READ):
            raise SecurityError("Access denied")
        return self._atomspace.get_atom(atom_id)
```

---

## Capability-Based Access Control

### Capability Structure

A capability is an unforgeable token that grants specific permissions on specific resources.

```python
@dataclass
class Capability:
    capability_id: str          # Unique identifier
    token: str                  # Cryptographic token (unforgeable)
    resource_type: ResourceType # Type of resource
    resource_id: str            # Specific resource
    permissions: Set[Permission]# Granted permissions
    owner_id: str               # Capability owner
    security_level: SecurityLevel
    created_at: datetime
    expires_at: Optional[datetime]
    constraints: Dict[str, Any] # Additional constraints
    parent_capability: Optional[str]  # For delegation
```

### Permission Types

| Permission | Description |
|------------|-------------|
| `READ` | Read/query access |
| `WRITE` | Modify/create access |
| `EXECUTE` | Execute/invoke access |
| `DELETE` | Remove/destroy access |
| `ADMIN` | Administrative control |

### Capability Lifecycle

```
┌─────────────┐
│   Create    │  Owner creates capability with permissions
└──────┬──────┘
       │
       ▼
┌─────────────┐
│   Active    │  Capability can be validated and used
└──────┬──────┘
       │
       ├──────────────┐
       ▼              ▼
┌─────────────┐ ┌─────────────┐
│   Revoked   │ │   Expired   │  Capability no longer valid
└─────────────┘ └─────────────┘
```

### Capability Delegation

Capabilities can be delegated with reduced permissions:

```python
# Original capability with full permissions
parent_cap = cap_mgr.create_capability(
    permissions={Permission.READ, Permission.WRITE, Permission.DELETE},
    ...
)

# Delegated capability with reduced permissions
child_cap = cap_mgr.delegate(
    parent_capability=parent_cap,
    new_permissions={Permission.READ},  # Subset only
    new_owner="delegated_agent",
    expires_in=3600
)
```

---

## Security Levels

### Hierarchy

```
KERNEL (0) ─── Full system access
    │
    ▼
SYSTEM (1) ─── System services
    │
    ▼
USER (2) ──── Normal operations
    │
    ▼
GUEST (3) ─── Restricted access
```

### Level Descriptions

| Level | Description | Typical Components |
|-------|-------------|-------------------|
| `KERNEL` | Full system access, can modify security policy | AGI_Boot, AGI_Scheduler |
| `SYSTEM` | System services, elevated privileges | PLN Inference Engine, ECAN |
| `USER` | Normal cognitive operations | Cognitive agents, modules |
| `GUEST` | Read-only, restricted access | External queries, probes |

### Level Enforcement

```python
def check_security_level(required: SecurityLevel, actual: SecurityLevel) -> bool:
    """Lower number = higher privilege"""
    return actual.value <= required.value

# Examples:
check_security_level(SecurityLevel.USER, SecurityLevel.KERNEL)  # True (kernel > user)
check_security_level(SecurityLevel.SYSTEM, SecurityLevel.USER)  # False (user < system)
```

---

## Resource Protection

### Protected Resource Types

| Resource | Protection |
|----------|------------|
| **AtomSpace** | Per-atom capabilities, attention-based priority |
| **Memory** | Region-based protection, tier-aware access |
| **Timers** | Scheduling priority, deadline protection |
| **Network** | Connection capabilities, bandwidth limits |
| **Processes** | Isolation, resource quotas |
| **Files** | Path-based capabilities |

### AtomSpace Protection

```python
class ProtectedAtomSpace:
    def __init__(self, atomspace, security_manager):
        self._as = atomspace
        self._security = security_manager
    
    def get_atom(self, atom_id, session_id):
        # Check capability
        if not self._security.has_capability(session_id, "atomspace", atom_id, Permission.READ):
            raise AccessDenied(f"No read access to atom {atom_id}")
        
        return self._as.get_atom(atom_id)
    
    def set_tv(self, atom_id, tv, session_id):
        if not self._security.has_capability(session_id, "atomspace", atom_id, Permission.WRITE):
            raise AccessDenied(f"No write access to atom {atom_id}")
        
        self._as.set_tv(atom_id, tv)
```

### Memory Protection

```python
# Memory regions have flags for protection
class MemoryFlags(IntFlag):
    READ = 0x01
    WRITE = 0x02
    EXECUTE = 0x04
    COGNITIVE = 0x08  # Cognitive-system accessible
    
    RW = READ | WRITE
    RX = READ | EXECUTE
    RWX = READ | WRITE | EXECUTE
```

---

## Module Sandboxing

### Sandbox Architecture

```
┌─────────────────────────────────────────┐
│           Host Environment              │
│  ┌────────────────────────────────────┐ │
│  │         Security Manager           │ │
│  └───────────────┬────────────────────┘ │
│                  │                      │
│  ┌───────────────┴────────────────────┐ │
│  │           Sandbox Layer            │ │
│  │  ┌──────────┐ ┌──────────────────┐ │ │
│  │  │ Resource │ │   Capability     │ │ │
│  │  │  Limits  │ │   Enforcement    │ │ │
│  │  └──────────┘ └──────────────────┘ │ │
│  └───────────────┬────────────────────┘ │
│                  │                      │
│  ┌───────────────┴────────────────────┐ │
│  │        Sandboxed Module            │ │
│  │  ┌──────────┐ ┌──────────────────┐ │ │
│  │  │  Module  │ │  Isolated State  │ │ │
│  │  │   Code   │ │                  │ │ │
│  │  └──────────┘ └──────────────────┘ │ │
│  └────────────────────────────────────┘ │
└─────────────────────────────────────────┘
```

### Resource Limits

```python
@dataclass
class ResourceLimits:
    max_memory_mb: float = 100.0       # Memory limit
    max_cpu_time_seconds: float = 60.0 # CPU time limit
    max_atoms: int = 10000             # AtomSpace limit
    max_inferences: int = 1000         # Inference limit
    max_network_connections: int = 10  # Network limit
    max_file_descriptors: int = 50     # File descriptor limit
```

### Sandboxed Execution

```python
from core.security.sandbox import Sandbox, SandboxConfig, ResourceLimits

# Configure sandbox
config = SandboxConfig(
    limits=ResourceLimits(
        max_memory_mb=50.0,
        max_cpu_time_seconds=30.0,
        max_atoms=5000
    ),
    allowed_capabilities=[capability],
    isolated=True
)

# Create sandbox
sandbox = Sandbox(config)

# Run module in sandbox
def cognitive_operation():
    # This runs with limited resources and capabilities
    return process_atoms()

result = sandbox.execute(cognitive_operation)
```

---

## Audit and Monitoring

### Audit Levels

| Level | Description |
|-------|-------------|
| `DEBUG` | Detailed debugging information |
| `INFO` | Normal operations |
| `WARNING` | Unusual but non-critical events |
| `SECURITY` | Security-relevant events |
| `CRITICAL` | Critical security events |

### Audit Categories

| Category | Events |
|----------|--------|
| `AUTHENTICATION` | Login, logout, session creation |
| `AUTHORIZATION` | Access checks, capability usage |
| `CAPABILITY_MANAGEMENT` | Create, revoke, delegate |
| `RESOURCE_ACCESS` | AtomSpace, memory, file access |
| `POLICY_CHANGE` | Security policy modifications |
| `ANOMALY` | Detected anomalies, threats |

### Audit Event Structure

```python
@dataclass
class AuditEvent:
    event_id: str                # Unique event ID
    timestamp: datetime          # When it occurred
    level: AuditLevel            # Severity
    category: AuditCategory      # Category
    principal_id: str            # Who performed action
    action: str                  # What action
    resource: str                # Target resource
    result: str                  # Outcome (success/failure)
    details: Dict[str, Any]      # Additional context
    signature: str               # Cryptographic signature
```

### Tamper-Resistant Logging

The audit log uses cryptographic chaining for tamper detection:

```python
def calculate_signature(event, previous_signature):
    """Chain signature using HMAC"""
    data = f"{event.event_id}:{event.timestamp}:{event.action}:{previous_signature}"
    return hmac.new(SECRET_KEY, data.encode(), hashlib.sha256).hexdigest()
```

### Querying Audit Log

```python
from core.security.audit import get_audit_log, AuditLevel, AuditCategory

audit = get_audit_log()

# Query recent security events
events = audit.query(
    start_time=datetime.now() - timedelta(hours=1),
    min_level=AuditLevel.SECURITY,
    category=AuditCategory.AUTHORIZATION,
    limit=100
)

# Verify log integrity
is_valid = audit.verify_chain()
```

---

## Threat Model

### Identified Threats

| Threat | Mitigation |
|--------|------------|
| **Capability Forgery** | Cryptographic tokens, validation |
| **Privilege Escalation** | Security level enforcement |
| **Resource Exhaustion** | Quotas, sandboxing |
| **Information Leakage** | Capability-based isolation |
| **Tampering** | Integrity checks, audit logging |
| **Denial of Service** | Rate limiting, prioritization |

### Threat Response

```python
class ThreatIndicator:
    threat_type: str           # Type of threat
    severity: ThreatSeverity   # Critical, High, Medium, Low
    source: str                # Where detected
    timestamp: datetime        # When detected
    details: Dict[str, Any]    # Context

class SecurityManager:
    def on_threat_detected(self, indicator: ThreatIndicator):
        """Respond to detected threat"""
        if indicator.severity == ThreatSeverity.CRITICAL:
            self._isolate_source(indicator.source)
            self._alert_administrators()
        
        self._audit.log_threat(indicator)
```

---

## Implementation Details

### File Structure

```
core/
├── agi_security_manager.py    # Unified security manager
└── security/
    ├── __init__.py
    ├── capabilities.py        # Capability management
    ├── sandbox.py             # Module sandboxing
    └── audit.py               # Audit logging
```

### Key Classes

| Class | Purpose |
|-------|---------|
| `AGISecurityManager` | Unified security interface |
| `CapabilityManager` | Capability CRUD operations |
| `Capability` | Capability data structure |
| `Sandbox` | Isolated execution environment |
| `SandboxedModule` | Module running in sandbox |
| `SecurityAuditLog` | Audit logging with integrity |
| `AuditEvent` | Individual audit record |

### Global Accessors

```python
from core import get_security_manager
from core.security.capabilities import get_capability_manager
from core.security.audit import get_audit_log

# Thread-safe singleton access
security = get_security_manager()
cap_mgr = get_capability_manager()
audit = get_audit_log()
```

---

## Best Practices

### 1. Always Use Capabilities

```python
# Bad: Direct access
atomspace.get_atom(atom_id)

# Good: Capability-controlled access
security.check_access(session, "atomspace", atom_id, Permission.READ)
atomspace.get_atom(atom_id)
```

### 2. Set Capability Expiration

```python
# Good: Time-limited capabilities
cap = cap_mgr.create_capability(
    ...,
    expires_in=3600  # 1 hour
)
```

### 3. Use Sandboxing for Untrusted Code

```python
# Good: Sandbox untrusted modules
result = sandbox.execute(untrusted_function)
```

### 4. Monitor Audit Logs

```python
# Good: Regular audit log review
for event in audit.query(min_level=AuditLevel.SECURITY):
    if event.result == "failure":
        investigate(event)
```

### 5. Apply Least Privilege

```python
# Good: Minimal permissions
cap = cap_mgr.create_capability(
    permissions={Permission.READ},  # Only what's needed
    ...
)
```

---

## See Also

- [AGI-Kern API Reference](AGI_KERN_API.md)
- [AGI-OS Architecture](AGI_OS_ARCHITECTURE.md)
- [Build Guide](BUILD_GUIDE.md)
