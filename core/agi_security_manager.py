#!/usr/bin/env python3
"""
AGI_SecurityManager - Unified Security Manager for AGI-OS

This module provides the top-level security management interface for AGI-OS,
integrating capability-based access control, sandboxing, and audit logging
into a coherent security framework.

Addresses the critical security gap (30% -> target 80%) identified in the
AGI-Kern evaluation.

Features:
- Unified security policy management
- Context-aware access control
- Security session management
- Threat detection and response
- Integration with cognitive components
"""

import time
import logging
import threading
import secrets
from typing import Dict, Set, Any, Optional, List, Callable
from dataclasses import dataclass, field
from enum import Enum

from .security.capabilities import (
    Capability,
    CapabilityManager,
    SecurityLevel,
    Permission,
    ResourceType,
    get_capability_manager
)
from .security.sandbox import (
    Sandbox,
    SandboxConfig,
    ResourceLimits,
    SandboxedModule
)
from .security.audit import (
    SecurityAuditLog,
    AuditEvent,
    AuditLevel,
    AuditCategory,
    get_audit_log
)

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("AGI_SecurityManager")


class SessionState(Enum):
    """State of a security session"""
    ACTIVE = "active"
    SUSPENDED = "suspended"
    EXPIRED = "expired"
    TERMINATED = "terminated"


@dataclass
class SecuritySession:
    """
    A security session for a principal.
    
    Sessions track authentication state and accumulated capabilities
    for a principal (user, process, or cognitive agent).
    """
    session_id: str
    principal_id: str
    security_level: SecurityLevel
    created_at: float = field(default_factory=time.time)
    expires_at: Optional[float] = None
    last_activity: float = field(default_factory=time.time)
    state: SessionState = SessionState.ACTIVE
    capabilities: Set[str] = field(default_factory=set)  # Capability IDs
    metadata: Dict[str, Any] = field(default_factory=dict)
    source_ip: Optional[str] = None
    
    def is_valid(self) -> bool:
        """Check if session is still valid"""
        if self.state != SessionState.ACTIVE:
            return False
        if self.expires_at and time.time() > self.expires_at:
            return False
        return True
    
    def touch(self):
        """Update last activity time"""
        self.last_activity = time.time()


@dataclass
class SecurityPolicy:
    """
    Security policy configuration.
    
    Defines security rules and thresholds for the AGI-OS.
    """
    # Session settings
    session_timeout: float = 3600.0         # 1 hour default
    max_sessions_per_principal: int = 5
    require_authentication: bool = True
    
    # Access control
    default_security_level: SecurityLevel = SecurityLevel.USER
    allow_anonymous: bool = False
    max_capability_lifetime: float = 86400.0  # 24 hours
    
    # Threat detection
    max_failed_auth_attempts: int = 5
    lockout_duration: float = 300.0         # 5 minutes
    alert_on_privilege_escalation: bool = True
    
    # Resource limits for untrusted code
    default_sandbox_limits: ResourceLimits = field(default_factory=ResourceLimits)
    
    # Audit settings
    audit_all_access: bool = True
    audit_retention_days: int = 90


@dataclass
class ThreatIndicator:
    """Tracks potential security threats"""
    principal_id: str
    threat_type: str
    timestamp: float
    severity: float  # 0.0 to 1.0
    details: Dict[str, Any] = field(default_factory=dict)
    resolved: bool = False


class AGISecurityManager:
    """
    Unified Security Manager for AGI-OS
    
    Provides comprehensive security management including:
    - Session management
    - Access control
    - Threat detection
    - Security policy enforcement
    - Integration with cognitive components
    """
    
    def __init__(self, policy: Optional[SecurityPolicy] = None):
        self.policy = policy or SecurityPolicy()
        
        # Core security components
        self._capability_manager = get_capability_manager()
        self._audit_log = get_audit_log()
        
        # Session management
        self._sessions: Dict[str, SecuritySession] = {}
        self._principal_sessions: Dict[str, Set[str]] = {}
        
        # Threat tracking
        self._failed_auth: Dict[str, List[float]] = {}  # principal -> timestamps
        self._locked_principals: Dict[str, float] = {}  # principal -> lockout_until
        self._threat_indicators: List[ThreatIndicator] = []
        
        # Lock for thread safety
        self._lock = threading.RLock()
        
        # Start cleanup thread
        self._cleanup_thread = threading.Thread(
            target=self._cleanup_loop,
            daemon=True
        )
        self._running = True
        self._cleanup_thread.start()
        
        logger.info("AGI Security Manager initialized")
    
    # ==================== Session Management ====================
    
    def create_session(
        self,
        principal_id: str,
        security_level: SecurityLevel,
        source_ip: Optional[str] = None,
        session_duration: Optional[float] = None,
        metadata: Optional[Dict[str, Any]] = None
    ) -> SecuritySession:
        """
        Create a new security session.
        
        Args:
            principal_id: Identifier for the principal
            security_level: Security level for the session
            source_ip: Source IP address
            session_duration: Session duration (defaults to policy)
            metadata: Additional session metadata
            
        Returns:
            New SecuritySession
        """
        with self._lock:
            # Check lockout
            if self._is_locked_out(principal_id):
                self._audit_log.log_authentication(
                    "session_create_denied",
                    principal_id,
                    outcome="denied",
                    details={"reason": "locked_out"}
                )
                raise PermissionError(f"Principal {principal_id} is locked out")
            
            # Check max sessions
            existing = self._principal_sessions.get(principal_id, set())
            valid_sessions = [sid for sid in existing if sid in self._sessions 
                           and self._sessions[sid].is_valid()]
            
            if len(valid_sessions) >= self.policy.max_sessions_per_principal:
                # Terminate oldest session
                oldest_sid = min(valid_sessions, 
                               key=lambda s: self._sessions[s].created_at)
                self.terminate_session(oldest_sid, "max_sessions_exceeded")
            
            # Create session
            session_id = f"session_{secrets.token_hex(16)}"
            duration = session_duration or self.policy.session_timeout
            
            session = SecuritySession(
                session_id=session_id,
                principal_id=principal_id,
                security_level=security_level,
                expires_at=time.time() + duration if duration else None,
                source_ip=source_ip,
                metadata=metadata or {}
            )
            
            self._sessions[session_id] = session
            if principal_id not in self._principal_sessions:
                self._principal_sessions[principal_id] = set()
            self._principal_sessions[principal_id].add(session_id)
            
            self._audit_log.log_authentication(
                "session_created",
                principal_id,
                outcome="success",
                session_id=session_id,
                source_ip=source_ip,
                details={"security_level": security_level.name}
            )
            
            logger.info(f"Created session {session_id} for {principal_id}")
            return session
    
    def get_session(self, session_id: str) -> Optional[SecuritySession]:
        """Get a session by ID"""
        with self._lock:
            session = self._sessions.get(session_id)
            if session and session.is_valid():
                session.touch()
                return session
            return None
    
    def terminate_session(self, session_id: str, reason: str = "manual"):
        """Terminate a session"""
        with self._lock:
            session = self._sessions.get(session_id)
            if session:
                session.state = SessionState.TERMINATED
                
                # Revoke all session capabilities
                for cap_id in session.capabilities:
                    self._capability_manager.revoke_capability(
                        cap_id, session.principal_id
                    )
                
                self._audit_log.log_authentication(
                    "session_terminated",
                    session.principal_id,
                    session_id=session_id,
                    details={"reason": reason}
                )
                
                logger.info(f"Terminated session {session_id}: {reason}")
    
    # ==================== Access Control ====================
    
    def check_access(
        self,
        session_id: str,
        resource_type: ResourceType,
        resource_id: str,
        permission: Permission,
        context: Optional[Dict[str, Any]] = None
    ) -> bool:
        """
        Check if a session has access to a resource.
        
        Args:
            session_id: Session ID
            resource_type: Type of resource
            resource_id: Resource identifier
            permission: Required permission
            context: Additional context for constraints
            
        Returns:
            True if access is granted
        """
        with self._lock:
            session = self.get_session(session_id)
            if not session:
                self._audit_log.log_authorization(
                    "access_check",
                    "unknown",
                    f"{resource_type.value}:{resource_id}",
                    outcome="denied",
                    details={"reason": "invalid_session"}
                )
                return False
            
            # Check each capability
            for cap_id in session.capabilities:
                capability = self._capability_manager.get_capability(cap_id)
                if capability and capability.is_valid():
                    if self._capability_manager.validate_capability(
                        capability.token,
                        resource_type,
                        resource_id,
                        permission,
                        context
                    ):
                        if self.policy.audit_all_access:
                            self._audit_log.log_authorization(
                                "access_granted",
                                session.principal_id,
                                f"{resource_type.value}:{resource_id}",
                                outcome="success",
                                capability_id=cap_id,
                                session_id=session_id
                            )
                        return True
            
            # Access denied
            self._audit_log.log_authorization(
                "access_denied",
                session.principal_id,
                f"{resource_type.value}:{resource_id}",
                outcome="denied",
                session_id=session_id,
                details={"permission": permission.value}
            )
            
            return False
    
    def grant_capability(
        self,
        session_id: str,
        resource_type: ResourceType,
        resource_id: str,
        permissions: Set[Permission],
        expires_in: Optional[float] = None,
        constraints: Optional[Dict[str, Any]] = None
    ) -> Capability:
        """
        Grant a capability to a session.
        
        Args:
            session_id: Session to grant capability to
            resource_type: Type of resource
            resource_id: Resource identifier
            permissions: Permissions to grant
            expires_in: Capability lifetime
            constraints: Access constraints
            
        Returns:
            New Capability
        """
        with self._lock:
            session = self.get_session(session_id)
            if not session:
                raise ValueError(f"Invalid session: {session_id}")
            
            # Enforce max lifetime
            if expires_in is None:
                expires_in = self.policy.max_capability_lifetime
            else:
                expires_in = min(expires_in, self.policy.max_capability_lifetime)
            
            capability = self._capability_manager.create_capability(
                resource_type=resource_type,
                resource_id=resource_id,
                permissions=permissions,
                owner_id=session.principal_id,
                security_level=session.security_level,
                expires_in=expires_in,
                constraints=constraints
            )
            
            session.capabilities.add(capability.capability_id)
            
            self._audit_log.log_capability_operation(
                "capability_granted",
                session.principal_id,
                capability.capability_id,
                details={
                    "resource": f"{resource_type.value}:{resource_id}",
                    "permissions": [p.value for p in permissions]
                }
            )
            
            return capability
    
    def revoke_capability(self, session_id: str, capability_id: str) -> bool:
        """Revoke a capability from a session"""
        with self._lock:
            session = self.get_session(session_id)
            if not session:
                return False
            
            if capability_id not in session.capabilities:
                return False
            
            result = self._capability_manager.revoke_capability(
                capability_id, session.principal_id
            )
            
            if result:
                session.capabilities.discard(capability_id)
                self._audit_log.log_capability_operation(
                    "capability_revoked",
                    session.principal_id,
                    capability_id
                )
            
            return result
    
    # ==================== Sandboxing ====================
    
    def create_sandbox(
        self,
        session_id: str,
        name: str,
        limits: Optional[ResourceLimits] = None
    ) -> SandboxedModule:
        """
        Create a sandbox for the session.
        
        Args:
            session_id: Session creating the sandbox
            name: Sandbox name
            limits: Resource limits (uses policy defaults if not provided)
            
        Returns:
            SandboxedModule instance
        """
        with self._lock:
            session = self.get_session(session_id)
            if not session:
                raise ValueError(f"Invalid session: {session_id}")
            
            # Use session security level to adjust limits
            if limits is None:
                limits = ResourceLimits(**self.policy.default_sandbox_limits.__dict__)
            
            # Stricter limits for lower security levels
            if session.security_level >= SecurityLevel.USER:
                limits.allow_network = False
            if session.security_level >= SecurityLevel.GUEST:
                limits.max_memory_mb = min(limits.max_memory_mb, 256)
                limits.max_cpu_time = min(limits.max_cpu_time, 30)
            
            sandbox = Sandbox.create(name, limits)
            
            self._audit_log.log(
                AuditLevel.INFO,
                AuditCategory.SANDBOX,
                "sandbox_created",
                session.principal_id,
                resource=sandbox.config.sandbox_id,
                session_id=session_id,
                details={"name": name, "limits": limits.to_dict()}
            )
            
            return sandbox
    
    def execute_sandboxed(
        self,
        session_id: str,
        func: Callable,
        args: tuple = (),
        kwargs: Optional[dict] = None,
        limits: Optional[ResourceLimits] = None
    ) -> Any:
        """
        Execute a function in a sandbox.
        
        Args:
            session_id: Session for the execution
            func: Function to execute
            args: Function arguments
            kwargs: Function keyword arguments
            limits: Resource limits
            
        Returns:
            Function result
        """
        sandbox = self.create_sandbox(session_id, f"exec_{func.__name__}", limits)
        try:
            result = sandbox.execute(func, args, kwargs)
            
            self._audit_log.log(
                AuditLevel.INFO,
                AuditCategory.SANDBOX,
                "sandboxed_execution_completed",
                self._sessions[session_id].principal_id,
                resource=sandbox.config.sandbox_id,
                session_id=session_id,
                details=sandbox.get_metrics()
            )
            
            return result
        except Exception as e:
            self._audit_log.log(
                AuditLevel.WARNING,
                AuditCategory.SANDBOX,
                "sandboxed_execution_failed",
                self._sessions[session_id].principal_id,
                resource=sandbox.config.sandbox_id,
                outcome="failure",
                session_id=session_id,
                details={"error": str(e)}
            )
            raise
        finally:
            Sandbox.destroy(sandbox.config.sandbox_id)
    
    # ==================== Threat Detection ====================
    
    def record_failed_auth(self, principal_id: str, details: Optional[Dict] = None):
        """Record a failed authentication attempt"""
        with self._lock:
            if principal_id not in self._failed_auth:
                self._failed_auth[principal_id] = []
            
            self._failed_auth[principal_id].append(time.time())
            
            # Clean old attempts
            cutoff = time.time() - 3600  # 1 hour
            self._failed_auth[principal_id] = [
                t for t in self._failed_auth[principal_id] if t > cutoff
            ]
            
            # Check for lockout
            if len(self._failed_auth[principal_id]) >= self.policy.max_failed_auth_attempts:
                self._locked_principals[principal_id] = (
                    time.time() + self.policy.lockout_duration
                )
                
                self._audit_log.log_security_violation(
                    "principal_locked_out",
                    principal_id,
                    details={
                        "failed_attempts": len(self._failed_auth[principal_id]),
                        "lockout_duration": self.policy.lockout_duration
                    }
                )
                
                self._add_threat_indicator(
                    principal_id,
                    "brute_force_attempt",
                    0.8,
                    details or {}
                )
    
    def _is_locked_out(self, principal_id: str) -> bool:
        """Check if principal is locked out"""
        lockout_until = self._locked_principals.get(principal_id)
        if lockout_until and time.time() < lockout_until:
            return True
        elif lockout_until:
            del self._locked_principals[principal_id]
        return False
    
    def _add_threat_indicator(
        self,
        principal_id: str,
        threat_type: str,
        severity: float,
        details: Dict[str, Any]
    ):
        """Add a threat indicator"""
        indicator = ThreatIndicator(
            principal_id=principal_id,
            threat_type=threat_type,
            timestamp=time.time(),
            severity=severity,
            details=details
        )
        self._threat_indicators.append(indicator)
        
        # Keep only recent indicators
        cutoff = time.time() - 86400  # 24 hours
        self._threat_indicators = [
            t for t in self._threat_indicators if t.timestamp > cutoff
        ]
        
        logger.warning(f"Threat indicator: {threat_type} from {principal_id} (severity: {severity})")
    
    def get_threat_indicators(
        self,
        principal_id: Optional[str] = None,
        min_severity: float = 0.0
    ) -> List[ThreatIndicator]:
        """Get threat indicators"""
        with self._lock:
            indicators = self._threat_indicators
            if principal_id:
                indicators = [t for t in indicators if t.principal_id == principal_id]
            indicators = [t for t in indicators if t.severity >= min_severity]
            return indicators
    
    # ==================== Cleanup and Maintenance ====================
    
    def _cleanup_loop(self):
        """Background cleanup of expired sessions and capabilities"""
        while self._running:
            try:
                time.sleep(60)  # Check every minute
                self._cleanup_expired()
            except Exception as e:
                logger.error(f"Cleanup error: {e}")
    
    def _cleanup_expired(self):
        """Clean up expired sessions and capabilities"""
        with self._lock:
            # Clean expired sessions
            expired_sessions = [
                sid for sid, session in self._sessions.items()
                if not session.is_valid()
            ]
            for sid in expired_sessions:
                self.terminate_session(sid, "expired")
            
            # Clean expired capabilities
            self._capability_manager.cleanup_expired()
    
    def shutdown(self):
        """Shutdown the security manager"""
        self._running = False
        
        # Terminate all sessions
        with self._lock:
            for sid in list(self._sessions.keys()):
                self.terminate_session(sid, "shutdown")
        
        logger.info("AGI Security Manager shutdown complete")
    
    # ==================== Statistics and Reporting ====================
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get security manager statistics"""
        with self._lock:
            active_sessions = sum(
                1 for s in self._sessions.values() if s.is_valid()
            )
            
            return {
                "sessions": {
                    "total": len(self._sessions),
                    "active": active_sessions,
                    "by_level": {
                        level.name: sum(
                            1 for s in self._sessions.values()
                            if s.security_level == level and s.is_valid()
                        )
                        for level in SecurityLevel
                    }
                },
                "capabilities": self._capability_manager.get_statistics(),
                "threats": {
                    "indicators": len(self._threat_indicators),
                    "locked_principals": len(self._locked_principals),
                    "by_type": {}
                },
                "audit": self._audit_log.get_statistics()
            }
    
    def get_security_status(self) -> Dict[str, Any]:
        """Get overall security status"""
        stats = self.get_statistics()
        
        # Calculate threat level
        threat_level = "low"
        if len(self._threat_indicators) > 10:
            threat_level = "high"
        elif len(self._threat_indicators) > 5:
            threat_level = "medium"
        
        return {
            "status": "healthy",
            "threat_level": threat_level,
            "active_sessions": stats["sessions"]["active"],
            "active_capabilities": stats["capabilities"]["valid_capabilities"],
            "recent_threats": len(self._threat_indicators),
            "audit_chain_verified": self._audit_log.verify_chain()
        }


# Global security manager instance
_security_manager: Optional[AGISecurityManager] = None


def get_security_manager(policy: Optional[SecurityPolicy] = None) -> AGISecurityManager:
    """Get the global security manager instance"""
    global _security_manager
    if _security_manager is None:
        _security_manager = AGISecurityManager(policy)
    return _security_manager


if __name__ == "__main__":
    import json
    
    # Example usage
    security = get_security_manager()
    
    print("=== AGI Security Manager Example ===\n")
    
    # Create a session
    session = security.create_session(
        principal_id="pln_engine",
        security_level=SecurityLevel.SYSTEM,
        source_ip="127.0.0.1"
    )
    print(f"Created session: {session.session_id}")
    
    # Grant capability
    capability = security.grant_capability(
        session.session_id,
        ResourceType.ATOMSPACE,
        "main_atomspace",
        {Permission.READ, Permission.WRITE}
    )
    print(f"Granted capability: {capability.capability_id}")
    
    # Check access
    has_access = security.check_access(
        session.session_id,
        ResourceType.ATOMSPACE,
        "main_atomspace",
        Permission.READ
    )
    print(f"Has READ access: {has_access}")
    
    # Sandboxed execution
    def safe_computation(x: int) -> int:
        return sum(range(x))
    
    result = security.execute_sandboxed(
        session.session_id,
        safe_computation,
        args=(1000,)
    )
    print(f"Sandboxed result: {result}")
    
    print("\n=== Security Status ===")
    print(json.dumps(security.get_security_status(), indent=2))
    
    print("\n=== Statistics ===")
    print(json.dumps(security.get_statistics(), indent=2, default=str))
    
    security.shutdown()
