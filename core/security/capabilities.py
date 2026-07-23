#!/usr/bin/env python3
"""
Capability-Based Access Control (CBAC) for AGI-OS

This module implements a capability-based security model where access to
resources is controlled through unforgeable capability tokens. This is
the foundation of AGI-OS security.

Features:
- Capability creation and validation
- Resource-type specific permissions
- Time-limited capabilities
- Capability delegation and revocation
- Constraint-based access control
"""

import time
import logging
import threading
import hashlib
import secrets
from typing import Dict, Set, Any, Optional, List
from dataclasses import dataclass, field
from enum import Enum, IntEnum, auto
from datetime import datetime, timedelta
import json

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("AGI_Security.Capabilities")


class SecurityLevel(IntEnum):
    """Security/privilege levels in AGI-OS"""
    KERNEL = 0      # Full system access, boot and core services
    SYSTEM = 1      # System services, can access most resources
    USER = 2        # Normal user/cognitive agent operations
    GUEST = 3       # Restricted access, sandboxed operations
    UNTRUSTED = 4   # Highly restricted, minimal permissions


class ResourceType(Enum):
    """Types of resources that can be protected"""
    ATOMSPACE = "atomspace"         # AtomSpace operations
    FILE = "file"                   # File system access
    NETWORK = "network"             # Network operations
    MEMORY = "memory"               # Memory allocation
    CPU = "cpu"                     # CPU scheduling priority
    INFERENCE = "inference"         # PLN/URE inference engine
    LEARNING = "learning"           # MOSES/pattern mining
    ATTENTION = "attention"         # Attention allocation
    EVENT_BUS = "event_bus"         # Event bus operations
    COGSERVER = "cogserver"         # CogServer access
    SCHEDULER = "scheduler"         # Task scheduler
    MODULE = "module"               # Module loading
    CRYPTO = "crypto"               # Cryptographic operations
    SYSTEM = "system"               # System-level operations


class Permission(Enum):
    """Available permissions for resources"""
    READ = "read"
    WRITE = "write"
    EXECUTE = "execute"
    DELETE = "delete"
    CREATE = "create"
    ADMIN = "admin"
    DELEGATE = "delegate"   # Can create sub-capabilities


@dataclass
class Capability:
    """
    A capability token granting access to a resource.
    
    Capabilities are unforgeable tokens that grant specific permissions
    to specific resources. They can have expiration times and constraints.
    """
    capability_id: str
    resource_type: ResourceType
    resource_id: str              # Specific resource (e.g., atomspace name, file path)
    permissions: Set[Permission]
    owner_id: str                 # Who owns this capability
    security_level: SecurityLevel
    created_at: float = field(default_factory=time.time)
    expires_at: Optional[float] = None
    constraints: Dict[str, Any] = field(default_factory=dict)
    parent_capability_id: Optional[str] = None  # For delegated capabilities
    revoked: bool = False
    token: str = field(default_factory=lambda: secrets.token_hex(32))
    
    def is_valid(self) -> bool:
        """Check if capability is still valid"""
        if self.revoked:
            return False
        if self.expires_at and time.time() > self.expires_at:
            return False
        return True
    
    def has_permission(self, permission: Permission) -> bool:
        """Check if capability grants a specific permission"""
        if not self.is_valid():
            return False
        return permission in self.permissions or Permission.ADMIN in self.permissions
    
    def check_constraints(self, context: Dict[str, Any]) -> bool:
        """Check if constraints are satisfied by context"""
        for key, value in self.constraints.items():
            if key not in context:
                return False
            if context[key] != value:
                return False
        return True
    
    def to_dict(self) -> Dict[str, Any]:
        """Serialize capability for storage/transmission"""
        return {
            "capability_id": self.capability_id,
            "resource_type": self.resource_type.value,
            "resource_id": self.resource_id,
            "permissions": [p.value for p in self.permissions],
            "owner_id": self.owner_id,
            "security_level": self.security_level.value,
            "created_at": self.created_at,
            "expires_at": self.expires_at,
            "constraints": self.constraints,
            "parent_capability_id": self.parent_capability_id,
            "revoked": self.revoked
            # Note: token is intentionally not serialized for security
        }
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any], token: str) -> 'Capability':
        """Deserialize capability from storage"""
        return cls(
            capability_id=data["capability_id"],
            resource_type=ResourceType(data["resource_type"]),
            resource_id=data["resource_id"],
            permissions={Permission(p) for p in data["permissions"]},
            owner_id=data["owner_id"],
            security_level=SecurityLevel(data["security_level"]),
            created_at=data["created_at"],
            expires_at=data.get("expires_at"),
            constraints=data.get("constraints", {}),
            parent_capability_id=data.get("parent_capability_id"),
            revoked=data.get("revoked", False),
            token=token
        )


class CapabilityManager:
    """
    Manages capability creation, validation, and revocation.
    
    This is the central authority for capability-based access control
    in AGI-OS.
    """
    
    def __init__(self):
        self._capabilities: Dict[str, Capability] = {}
        self._tokens: Dict[str, str] = {}  # token -> capability_id
        self._owner_capabilities: Dict[str, Set[str]] = {}  # owner_id -> capability_ids
        self._resource_capabilities: Dict[str, Set[str]] = {}  # resource_id -> capability_ids
        self._lock = threading.RLock()
        
        # Master capability for kernel
        self._create_kernel_capability()
        
        logger.info("CapabilityManager initialized")
    
    def _create_kernel_capability(self):
        """Create the master kernel capability"""
        kernel_cap = Capability(
            capability_id="kernel_master",
            resource_type=ResourceType.SYSTEM,
            resource_id="*",  # All resources
            permissions={Permission.ADMIN},
            owner_id="kernel",
            security_level=SecurityLevel.KERNEL,
            constraints={}
        )
        self._capabilities[kernel_cap.capability_id] = kernel_cap
        self._tokens[kernel_cap.token] = kernel_cap.capability_id
        self._owner_capabilities["kernel"] = {kernel_cap.capability_id}
    
    def create_capability(
        self,
        resource_type: ResourceType,
        resource_id: str,
        permissions: Set[Permission],
        owner_id: str,
        security_level: SecurityLevel,
        expires_in: Optional[float] = None,
        constraints: Optional[Dict[str, Any]] = None,
        parent_capability: Optional[Capability] = None
    ) -> Capability:
        """
        Create a new capability.
        
        Args:
            resource_type: Type of resource being protected
            resource_id: Specific resource identifier
            permissions: Set of permissions to grant
            owner_id: Owner of this capability
            security_level: Security level required
            expires_in: Seconds until expiration (None = no expiration)
            constraints: Additional constraints for access
            parent_capability: Parent capability if delegating
            
        Returns:
            New Capability object
        """
        with self._lock:
            # Validate delegation if parent provided
            if parent_capability:
                if not parent_capability.has_permission(Permission.DELEGATE):
                    raise PermissionError("Parent capability cannot delegate")
                if not parent_capability.is_valid():
                    raise PermissionError("Parent capability is not valid")
                # Delegated capability cannot have more permissions than parent
                permissions = permissions & parent_capability.permissions
                # Security level must be same or lower (higher number)
                security_level = max(security_level, parent_capability.security_level)
            
            # Generate unique capability ID
            cap_id = f"cap_{secrets.token_hex(8)}"
            
            # Calculate expiration
            expires_at = None
            if expires_in is not None:
                expires_at = time.time() + expires_in
            
            capability = Capability(
                capability_id=cap_id,
                resource_type=resource_type,
                resource_id=resource_id,
                permissions=permissions,
                owner_id=owner_id,
                security_level=security_level,
                expires_at=expires_at,
                constraints=constraints or {},
                parent_capability_id=parent_capability.capability_id if parent_capability else None
            )
            
            # Store capability
            self._capabilities[cap_id] = capability
            self._tokens[capability.token] = cap_id
            
            # Index by owner
            if owner_id not in self._owner_capabilities:
                self._owner_capabilities[owner_id] = set()
            self._owner_capabilities[owner_id].add(cap_id)
            
            # Index by resource
            if resource_id not in self._resource_capabilities:
                self._resource_capabilities[resource_id] = set()
            self._resource_capabilities[resource_id].add(cap_id)
            
            logger.debug(f"Created capability {cap_id} for {resource_type.value}:{resource_id}")
            return capability
    
    def validate_capability(
        self,
        token: str,
        resource_type: ResourceType,
        resource_id: str,
        required_permission: Permission,
        context: Optional[Dict[str, Any]] = None
    ) -> bool:
        """
        Validate a capability token for a specific operation.
        
        Args:
            token: Capability token
            resource_type: Required resource type
            resource_id: Required resource ID
            required_permission: Required permission
            context: Additional context for constraint checking
            
        Returns:
            True if access is granted, False otherwise
        """
        with self._lock:
            # Look up capability by token
            cap_id = self._tokens.get(token)
            if not cap_id:
                logger.warning(f"Invalid capability token")
                return False
            
            capability = self._capabilities.get(cap_id)
            if not capability:
                logger.warning(f"Capability {cap_id} not found")
                return False
            
            # Check basic validity
            if not capability.is_valid():
                logger.warning(f"Capability {cap_id} is not valid")
                return False
            
            # Check resource type
            if capability.resource_type != resource_type:
                # SYSTEM type with ADMIN permission can access any resource
                if not (capability.resource_type == ResourceType.SYSTEM and 
                        capability.has_permission(Permission.ADMIN)):
                    logger.warning(f"Resource type mismatch: {capability.resource_type} != {resource_type}")
                    return False
            
            # Check resource ID (wildcards supported)
            if capability.resource_id != "*" and capability.resource_id != resource_id:
                logger.warning(f"Resource ID mismatch: {capability.resource_id} != {resource_id}")
                return False
            
            # Check permission
            if not capability.has_permission(required_permission):
                logger.warning(f"Permission {required_permission} not granted")
                return False
            
            # Check constraints
            if context and not capability.check_constraints(context):
                logger.warning(f"Constraint check failed")
                return False
            
            return True
    
    def revoke_capability(self, capability_id: str, revoker_id: str) -> bool:
        """
        Revoke a capability.
        
        Args:
            capability_id: ID of capability to revoke
            revoker_id: ID of entity requesting revocation
            
        Returns:
            True if revoked, False if not found or unauthorized
        """
        with self._lock:
            capability = self._capabilities.get(capability_id)
            if not capability:
                return False
            
            # Only owner, parent owner, or kernel can revoke
            if revoker_id not in ["kernel", capability.owner_id]:
                # Check if revoker owns the parent
                if capability.parent_capability_id:
                    parent = self._capabilities.get(capability.parent_capability_id)
                    if parent and parent.owner_id != revoker_id:
                        logger.warning(f"Unauthorized revocation attempt by {revoker_id}")
                        return False
                else:
                    logger.warning(f"Unauthorized revocation attempt by {revoker_id}")
                    return False
            
            capability.revoked = True
            
            # Also revoke all child capabilities
            self._revoke_children(capability_id)
            
            logger.info(f"Capability {capability_id} revoked by {revoker_id}")
            return True
    
    def _revoke_children(self, parent_id: str):
        """Recursively revoke all child capabilities"""
        for cap_id, cap in self._capabilities.items():
            if cap.parent_capability_id == parent_id and not cap.revoked:
                cap.revoked = True
                self._revoke_children(cap_id)
    
    def get_capability(self, capability_id: str) -> Optional[Capability]:
        """Get capability by ID"""
        with self._lock:
            return self._capabilities.get(capability_id)
    
    def get_capabilities_for_owner(self, owner_id: str) -> List[Capability]:
        """Get all capabilities owned by an entity"""
        with self._lock:
            cap_ids = self._owner_capabilities.get(owner_id, set())
            return [self._capabilities[cid] for cid in cap_ids if cid in self._capabilities]
    
    def get_capabilities_for_resource(self, resource_id: str) -> List[Capability]:
        """Get all capabilities for a resource"""
        with self._lock:
            cap_ids = self._resource_capabilities.get(resource_id, set())
            return [self._capabilities[cid] for cid in cap_ids if cid in self._capabilities]
    
    def cleanup_expired(self) -> int:
        """Remove expired capabilities. Returns count removed."""
        with self._lock:
            expired = []
            for cap_id, cap in self._capabilities.items():
                if cap.expires_at and time.time() > cap.expires_at:
                    expired.append(cap_id)
            
            for cap_id in expired:
                cap = self._capabilities.pop(cap_id, None)
                if cap:
                    self._tokens.pop(cap.token, None)
                    self._owner_capabilities.get(cap.owner_id, set()).discard(cap_id)
                    self._resource_capabilities.get(cap.resource_id, set()).discard(cap_id)
            
            if expired:
                logger.info(f"Cleaned up {len(expired)} expired capabilities")
            
            return len(expired)
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get capability manager statistics"""
        with self._lock:
            total = len(self._capabilities)
            valid = sum(1 for c in self._capabilities.values() if c.is_valid())
            revoked = sum(1 for c in self._capabilities.values() if c.revoked)
            expired = total - valid - revoked
            
            return {
                "total_capabilities": total,
                "valid_capabilities": valid,
                "revoked_capabilities": revoked,
                "expired_capabilities": expired,
                "unique_owners": len(self._owner_capabilities),
                "unique_resources": len(self._resource_capabilities),
                "by_resource_type": {
                    rt.value: sum(1 for c in self._capabilities.values() if c.resource_type == rt)
                    for rt in ResourceType
                },
                "by_security_level": {
                    sl.name: sum(1 for c in self._capabilities.values() if c.security_level == sl)
                    for sl in SecurityLevel
                }
            }


# Global capability manager instance
_capability_manager: Optional[CapabilityManager] = None


def get_capability_manager() -> CapabilityManager:
    """Get the global capability manager instance"""
    global _capability_manager
    if _capability_manager is None:
        _capability_manager = CapabilityManager()
    return _capability_manager


if __name__ == "__main__":
    # Example usage
    manager = get_capability_manager()
    
    print("=== Creating Capabilities ===")
    
    # Create a system-level capability for AtomSpace
    atomspace_cap = manager.create_capability(
        resource_type=ResourceType.ATOMSPACE,
        resource_id="main_atomspace",
        permissions={Permission.READ, Permission.WRITE, Permission.DELEGATE},
        owner_id="pln_engine",
        security_level=SecurityLevel.SYSTEM
    )
    print(f"Created: {atomspace_cap.capability_id}")
    
    # Delegate a read-only capability
    delegated_cap = manager.create_capability(
        resource_type=ResourceType.ATOMSPACE,
        resource_id="main_atomspace",
        permissions={Permission.READ},
        owner_id="pattern_miner",
        security_level=SecurityLevel.USER,
        expires_in=3600,  # 1 hour
        parent_capability=atomspace_cap
    )
    print(f"Delegated: {delegated_cap.capability_id}")
    
    print("\n=== Validating Capabilities ===")
    
    # Valid access
    result = manager.validate_capability(
        token=atomspace_cap.token,
        resource_type=ResourceType.ATOMSPACE,
        resource_id="main_atomspace",
        required_permission=Permission.WRITE
    )
    print(f"WRITE access for owner: {result}")
    
    # Valid delegated access
    result = manager.validate_capability(
        token=delegated_cap.token,
        resource_type=ResourceType.ATOMSPACE,
        resource_id="main_atomspace",
        required_permission=Permission.READ
    )
    print(f"READ access for delegatee: {result}")
    
    # Invalid access (write with read-only capability)
    result = manager.validate_capability(
        token=delegated_cap.token,
        resource_type=ResourceType.ATOMSPACE,
        resource_id="main_atomspace",
        required_permission=Permission.WRITE
    )
    print(f"WRITE access for read-only: {result}")
    
    print("\n=== Statistics ===")
    print(json.dumps(manager.get_statistics(), indent=2))
