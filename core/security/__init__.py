"""
AGI-OS Security Module

Provides comprehensive security infrastructure for the AGI-OS:
- Capability-based access control (CBAC)
- Module sandboxing
- Security audit logging
- Cryptographic key management
"""

from .capabilities import (
    Capability,
    CapabilityManager,
    SecurityLevel,
    Permission,
    ResourceType
)

from .sandbox import (
    Sandbox,
    SandboxConfig,
    ResourceLimits,
    SandboxedModule
)

from .audit import (
    SecurityAuditLog,
    AuditEvent,
    AuditLevel,
    get_audit_log
)

__all__ = [
    # Capabilities
    'Capability',
    'CapabilityManager',
    'SecurityLevel',
    'Permission',
    'ResourceType',
    
    # Sandbox
    'Sandbox',
    'SandboxConfig',
    'ResourceLimits',
    'SandboxedModule',
    
    # Audit
    'SecurityAuditLog',
    'AuditEvent',
    'AuditLevel',
    'get_audit_log'
]
