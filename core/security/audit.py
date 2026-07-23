#!/usr/bin/env python3
"""
Security Audit Logging for AGI-OS

This module provides comprehensive audit logging for security-relevant
events in the AGI-OS. It maintains an immutable audit trail for
compliance, debugging, and forensic analysis.

Features:
- Tamper-resistant audit log
- Structured event logging
- Log rotation and archival
- Real-time alerting for critical events
- Query interface for log analysis
"""

import time
import logging
import threading
import hashlib
import json
import os
from typing import Dict, Any, Optional, List, Callable
from dataclasses import dataclass, field
from enum import Enum
from datetime import datetime
from collections import deque
import gzip

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("AGI_Security.Audit")


class AuditLevel(Enum):
    """Severity levels for audit events"""
    DEBUG = "debug"         # Verbose debugging
    INFO = "info"           # Normal operations
    WARNING = "warning"     # Potential issues
    ERROR = "error"         # Errors
    CRITICAL = "critical"   # Security violations
    ALERT = "alert"         # Immediate attention required


class AuditCategory(Enum):
    """Categories of audit events"""
    AUTHENTICATION = "authentication"   # Login/logout, token validation
    AUTHORIZATION = "authorization"     # Capability checks
    ACCESS = "access"                   # Resource access
    MODIFICATION = "modification"       # Data changes
    CONFIGURATION = "configuration"     # Config changes
    SYSTEM = "system"                   # System events
    SECURITY = "security"               # Security-specific events
    CAPABILITY = "capability"           # Capability operations
    SANDBOX = "sandbox"                 # Sandbox operations
    NETWORK = "network"                 # Network activity


@dataclass
class AuditEvent:
    """
    An immutable audit log entry.
    
    Each entry is chained to the previous one via a hash,
    creating a tamper-evident log.
    """
    event_id: str
    timestamp: float
    level: AuditLevel
    category: AuditCategory
    action: str                         # What happened
    principal: str                      # Who did it (user/process/component)
    resource: Optional[str] = None      # What was affected
    outcome: str = "success"            # success, failure, denied
    details: Dict[str, Any] = field(default_factory=dict)
    source_ip: Optional[str] = None
    session_id: Optional[str] = None
    capability_id: Optional[str] = None
    previous_hash: Optional[str] = None
    event_hash: str = ""
    
    def __post_init__(self):
        """Calculate event hash after initialization"""
        if not self.event_hash:
            self.event_hash = self._calculate_hash()
    
    def _calculate_hash(self) -> str:
        """Calculate hash of the event for integrity verification"""
        data = {
            "event_id": self.event_id,
            "timestamp": self.timestamp,
            "level": self.level.value,
            "category": self.category.value,
            "action": self.action,
            "principal": self.principal,
            "resource": self.resource,
            "outcome": self.outcome,
            "details": self.details,
            "previous_hash": self.previous_hash
        }
        json_data = json.dumps(data, sort_keys=True)
        return hashlib.sha256(json_data.encode()).hexdigest()
    
    def verify_integrity(self) -> bool:
        """Verify the event hash hasn't been tampered with"""
        return self.event_hash == self._calculate_hash()
    
    def to_dict(self) -> Dict[str, Any]:
        """Serialize event for storage"""
        return {
            "event_id": self.event_id,
            "timestamp": self.timestamp,
            "timestamp_iso": datetime.fromtimestamp(self.timestamp).isoformat(),
            "level": self.level.value,
            "category": self.category.value,
            "action": self.action,
            "principal": self.principal,
            "resource": self.resource,
            "outcome": self.outcome,
            "details": self.details,
            "source_ip": self.source_ip,
            "session_id": self.session_id,
            "capability_id": self.capability_id,
            "previous_hash": self.previous_hash,
            "event_hash": self.event_hash
        }
    
    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> 'AuditEvent':
        """Deserialize event from storage"""
        return cls(
            event_id=data["event_id"],
            timestamp=data["timestamp"],
            level=AuditLevel(data["level"]),
            category=AuditCategory(data["category"]),
            action=data["action"],
            principal=data["principal"],
            resource=data.get("resource"),
            outcome=data.get("outcome", "success"),
            details=data.get("details", {}),
            source_ip=data.get("source_ip"),
            session_id=data.get("session_id"),
            capability_id=data.get("capability_id"),
            previous_hash=data.get("previous_hash"),
            event_hash=data.get("event_hash", "")
        )


@dataclass
class AlertConfig:
    """Configuration for audit alerts"""
    min_level: AuditLevel = AuditLevel.WARNING
    categories: set = field(default_factory=lambda: {AuditCategory.SECURITY})
    callback: Optional[Callable[[AuditEvent], None]] = None


class SecurityAuditLog:
    """
    Tamper-resistant security audit log.
    
    Provides comprehensive logging of security-relevant events
    with integrity verification and query capabilities.
    """
    
    def __init__(
        self,
        log_file: Optional[str] = None,
        max_memory_events: int = 10000,
        enable_file_logging: bool = True,
        rotate_size_mb: int = 100,
        compress_rotated: bool = True
    ):
        self.log_file = log_file or "/tmp/agi_security_audit.log"
        self.max_memory_events = max_memory_events
        self.enable_file_logging = enable_file_logging
        self.rotate_size_mb = rotate_size_mb
        self.compress_rotated = compress_rotated
        
        self._events: deque = deque(maxlen=max_memory_events)
        self._event_counter = 0
        self._last_hash: Optional[str] = None
        self._lock = threading.RLock()
        
        # Alert configuration
        self._alerts: List[AlertConfig] = []
        
        # Statistics
        self._stats = {
            "total_events": 0,
            "by_level": {level.value: 0 for level in AuditLevel},
            "by_category": {cat.value: 0 for cat in AuditCategory},
            "by_outcome": {"success": 0, "failure": 0, "denied": 0}
        }
        
        logger.info(f"SecurityAuditLog initialized (file: {self.log_file})")
    
    def log(
        self,
        level: AuditLevel,
        category: AuditCategory,
        action: str,
        principal: str,
        resource: Optional[str] = None,
        outcome: str = "success",
        details: Optional[Dict[str, Any]] = None,
        source_ip: Optional[str] = None,
        session_id: Optional[str] = None,
        capability_id: Optional[str] = None
    ) -> str:
        """
        Log a security audit event.
        
        Args:
            level: Severity level
            category: Event category
            action: Description of the action
            principal: Who performed the action
            resource: Resource affected (optional)
            outcome: success, failure, or denied
            details: Additional details
            source_ip: Source IP address
            session_id: Session identifier
            capability_id: Capability used
            
        Returns:
            Event ID
        """
        with self._lock:
            self._event_counter += 1
            event_id = f"audit_{self._event_counter:012d}"
            
            event = AuditEvent(
                event_id=event_id,
                timestamp=time.time(),
                level=level,
                category=category,
                action=action,
                principal=principal,
                resource=resource,
                outcome=outcome,
                details=details or {},
                source_ip=source_ip,
                session_id=session_id,
                capability_id=capability_id,
                previous_hash=self._last_hash
            )
            
            # Update chain
            self._last_hash = event.event_hash
            
            # Store in memory
            self._events.append(event)
            
            # Update statistics
            self._stats["total_events"] += 1
            self._stats["by_level"][level.value] += 1
            self._stats["by_category"][category.value] += 1
            self._stats["by_outcome"][outcome] = self._stats["by_outcome"].get(outcome, 0) + 1
            
            # Write to file
            if self.enable_file_logging:
                self._write_to_file(event)
            
            # Check alerts
            self._check_alerts(event)
            
            # Log to Python logger for severe events
            if level in [AuditLevel.CRITICAL, AuditLevel.ALERT]:
                logger.warning(f"AUDIT [{level.value.upper()}] {action} by {principal}: {outcome}")
            
            return event_id
    
    def _write_to_file(self, event: AuditEvent):
        """Write event to log file"""
        try:
            # Check rotation
            if os.path.exists(self.log_file):
                size_mb = os.path.getsize(self.log_file) / (1024 * 1024)
                if size_mb >= self.rotate_size_mb:
                    self._rotate_log()
            
            with open(self.log_file, 'a') as f:
                f.write(json.dumps(event.to_dict()) + '\n')
        except Exception as e:
            logger.error(f"Failed to write audit log: {e}")
    
    def _rotate_log(self):
        """Rotate the log file"""
        timestamp = datetime.now().strftime("%Y%m%d_%H%M%S")
        rotated_name = f"{self.log_file}.{timestamp}"
        
        try:
            os.rename(self.log_file, rotated_name)
            
            if self.compress_rotated:
                with open(rotated_name, 'rb') as f_in:
                    with gzip.open(f"{rotated_name}.gz", 'wb') as f_out:
                        f_out.writelines(f_in)
                os.remove(rotated_name)
                
            logger.info(f"Rotated audit log to {rotated_name}")
        except Exception as e:
            logger.error(f"Failed to rotate audit log: {e}")
    
    def _check_alerts(self, event: AuditEvent):
        """Check if event should trigger alerts"""
        for alert in self._alerts:
            if event.level.value >= alert.min_level.value:
                if not alert.categories or event.category in alert.categories:
                    if alert.callback:
                        try:
                            alert.callback(event)
                        except Exception as e:
                            logger.error(f"Alert callback error: {e}")
    
    def add_alert(
        self,
        callback: Callable[[AuditEvent], None],
        min_level: AuditLevel = AuditLevel.WARNING,
        categories: Optional[set] = None
    ):
        """Add an alert handler"""
        self._alerts.append(AlertConfig(
            min_level=min_level,
            categories=categories or set(),
            callback=callback
        ))
    
    # Convenience methods for common logging patterns
    def log_authentication(
        self,
        action: str,
        principal: str,
        outcome: str = "success",
        **kwargs
    ) -> str:
        """Log an authentication event"""
        level = AuditLevel.INFO if outcome == "success" else AuditLevel.WARNING
        return self.log(level, AuditCategory.AUTHENTICATION, action, principal, outcome=outcome, **kwargs)
    
    def log_authorization(
        self,
        action: str,
        principal: str,
        resource: str,
        outcome: str = "success",
        capability_id: Optional[str] = None,
        **kwargs
    ) -> str:
        """Log an authorization event"""
        level = AuditLevel.INFO if outcome == "success" else AuditLevel.WARNING
        if outcome == "denied":
            level = AuditLevel.ALERT
        return self.log(
            level, AuditCategory.AUTHORIZATION, action, principal,
            resource=resource, outcome=outcome, capability_id=capability_id, **kwargs
        )
    
    def log_access(
        self,
        action: str,
        principal: str,
        resource: str,
        outcome: str = "success",
        **kwargs
    ) -> str:
        """Log a resource access event"""
        return self.log(
            AuditLevel.INFO, AuditCategory.ACCESS, action, principal,
            resource=resource, outcome=outcome, **kwargs
        )
    
    def log_security_violation(
        self,
        action: str,
        principal: str,
        resource: Optional[str] = None,
        **kwargs
    ) -> str:
        """Log a security violation"""
        return self.log(
            AuditLevel.ALERT, AuditCategory.SECURITY, action, principal,
            resource=resource, outcome="denied", **kwargs
        )
    
    def log_capability_operation(
        self,
        action: str,
        principal: str,
        capability_id: str,
        outcome: str = "success",
        **kwargs
    ) -> str:
        """Log a capability operation"""
        return self.log(
            AuditLevel.INFO, AuditCategory.CAPABILITY, action, principal,
            capability_id=capability_id, outcome=outcome, **kwargs
        )
    
    # Query interface
    def query(
        self,
        level: Optional[AuditLevel] = None,
        category: Optional[AuditCategory] = None,
        principal: Optional[str] = None,
        resource: Optional[str] = None,
        outcome: Optional[str] = None,
        since: Optional[float] = None,
        until: Optional[float] = None,
        limit: int = 100
    ) -> List[AuditEvent]:
        """
        Query audit events.
        
        Args:
            level: Filter by level
            category: Filter by category
            principal: Filter by principal
            resource: Filter by resource
            outcome: Filter by outcome
            since: Events after this timestamp
            until: Events before this timestamp
            limit: Maximum events to return
            
        Returns:
            List of matching events
        """
        with self._lock:
            results = []
            
            for event in reversed(self._events):
                if len(results) >= limit:
                    break
                
                # Apply filters
                if level and event.level != level:
                    continue
                if category and event.category != category:
                    continue
                if principal and event.principal != principal:
                    continue
                if resource and event.resource != resource:
                    continue
                if outcome and event.outcome != outcome:
                    continue
                if since and event.timestamp < since:
                    continue
                if until and event.timestamp > until:
                    continue
                
                results.append(event)
            
            return results
    
    def verify_chain(self, events: Optional[List[AuditEvent]] = None) -> bool:
        """
        Verify the integrity of the audit chain.
        
        Returns:
            True if chain is intact, False if tampering detected
        """
        with self._lock:
            events = events or list(self._events)
            
            if not events:
                return True
            
            previous_hash = None
            for event in events:
                # Verify event integrity
                if not event.verify_integrity():
                    logger.error(f"Event {event.event_id} failed integrity check")
                    return False
                
                # Verify chain
                if event.previous_hash != previous_hash:
                    logger.error(f"Event {event.event_id} chain break detected")
                    return False
                
                previous_hash = event.event_hash
            
            return True
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get audit log statistics"""
        with self._lock:
            stats = self._stats.copy()
            stats["events_in_memory"] = len(self._events)
            stats["chain_verified"] = self.verify_chain()
            return stats
    
    def get_recent_events(self, count: int = 50) -> List[Dict[str, Any]]:
        """Get most recent events"""
        with self._lock:
            recent = list(self._events)[-count:]
            return [e.to_dict() for e in recent]


# Global audit log instance
_audit_log: Optional[SecurityAuditLog] = None


def get_audit_log() -> SecurityAuditLog:
    """Get the global audit log instance"""
    global _audit_log
    if _audit_log is None:
        _audit_log = SecurityAuditLog()
    return _audit_log


if __name__ == "__main__":
    # Example usage
    audit = get_audit_log()
    
    # Add alert handler
    def alert_handler(event: AuditEvent):
        print(f"🚨 ALERT: {event.action} by {event.principal}")
    
    audit.add_alert(alert_handler, min_level=AuditLevel.WARNING)
    
    print("=== Security Audit Log Example ===\n")
    
    # Log some events
    audit.log_authentication("login", "user_alice", outcome="success")
    audit.log_authentication("login", "user_bob", outcome="failure", 
                            details={"reason": "invalid_password"})
    
    audit.log_authorization(
        "access_atomspace", "pln_engine", "main_atomspace",
        outcome="success", capability_id="cap_12345"
    )
    
    audit.log_access("read_atom", "pattern_miner", "atom_uuid_123")
    
    # Trigger alert
    audit.log_security_violation(
        "unauthorized_access_attempt", "unknown_module", "kernel_memory"
    )
    
    print("\n=== Query Results ===")
    
    # Query events
    auth_events = audit.query(category=AuditCategory.AUTHENTICATION)
    print(f"\nAuthentication events: {len(auth_events)}")
    for e in auth_events:
        print(f"  {e.action}: {e.outcome}")
    
    print("\n=== Statistics ===")
    print(json.dumps(audit.get_statistics(), indent=2))
    
    print("\n=== Chain Verification ===")
    print(f"Chain integrity: {audit.verify_chain()}")
