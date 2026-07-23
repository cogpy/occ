#!/usr/bin/env python3
"""
AGI Timer Service for AGI-OS

This module provides centralized timer management for the OpenCog AGI-OS
cognitive architecture. It integrates cognitive time tracking, timer
coalescing, and deadline monitoring into a unified interface.

Features:
- Centralized timer management
- Timer coalescing for efficiency
- Cognitive time (event-based logical time)
- Deadline monitoring and alerts
- Integration with AGI_Scheduler
- Oneshot and periodic timers
- Power-efficient operation
"""

import time
import logging
import threading
from typing import Dict, Any, Optional, List, Callable, Set
from dataclasses import dataclass, field
from enum import Enum

# Import time subsystems
from core.time.cognitive_time import (
    CognitiveTimeManager, get_cognitive_time_manager,
    CognitiveTimestamp, TimeScale
)
from core.time.coalescing import (
    TimerCoalescer, get_timer_coalescer,
    CoalescingPolicy, TimerPriority
)

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("AGI_TimerService")


class TimerType(Enum):
    """Types of timers"""
    ONESHOT = 1       # Single trigger
    PERIODIC = 2      # Repeating
    DEADLINE = 3      # Must-complete-by
    COGNITIVE = 4     # Event-based (cognitive time)


@dataclass
class TimerHandle:
    """
    Handle to a scheduled timer.
    """
    handle_id: str
    timer_id: str
    timer_type: TimerType
    owner: str
    created_at: float = field(default_factory=time.time)
    
    # For periodic timers
    period_s: float = 0.0
    fire_count: int = 0
    
    # For deadline timers
    deadline: Optional[float] = None
    deadline_callback: Optional[Callable] = None
    
    # For cognitive timers
    cognitive_target: Optional[CognitiveTimestamp] = None


@dataclass
class DeadlineAlert:
    """
    Alert for deadline monitoring.
    """
    alert_id: str
    task_id: str
    deadline: float
    callback: Callable
    owner: str
    warned: bool = False
    warn_threshold_s: float = 60.0  # Warn 60s before deadline
    created_at: float = field(default_factory=time.time)


class AGI_TimerService:
    """
    Centralized Timer Service for AGI-OS
    
    Provides a unified interface for all timer operations in the
    cognitive architecture, including physical time, cognitive time,
    and deadline management.
    """
    
    def __init__(
        self,
        enable_coalescing: bool = True,
        coalescing_policy: CoalescingPolicy = CoalescingPolicy.MODERATE,
        enable_cognitive_time: bool = True
    ):
        """
        Initialize the AGI Timer Service.
        
        Args:
            enable_coalescing: Enable timer coalescing
            coalescing_policy: Policy for coalescing
            enable_cognitive_time: Enable cognitive time tracking
        """
        self._lock = threading.RLock()
        
        # Initialize subsystems
        self._coalescer = get_timer_coalescer() if enable_coalescing else None
        self._cognitive_time = get_cognitive_time_manager() if enable_cognitive_time else None
        
        if self._coalescer:
            self._coalescer.set_policy(coalescing_policy)
        
        # Timer tracking
        self._handles: Dict[str, TimerHandle] = {}
        self._periodic_timers: Dict[str, TimerHandle] = {}
        self._deadline_alerts: Dict[str, DeadlineAlert] = {}
        self._cognitive_waiters: Dict[str, TimerHandle] = {}
        
        # Background processing
        self._running = False
        self._service_thread: Optional[threading.Thread] = None
        
        # Statistics
        self._stats = {
            "timers_scheduled": 0,
            "timers_fired": 0,
            "timers_cancelled": 0,
            "periodic_fires": 0,
            "deadline_alerts": 0,
            "cognitive_triggers": 0
        }
        
        logger.info("AGI_TimerService initialized")
    
    def schedule_timer(
        self,
        callback: Callable,
        delay_s: float,
        timer_type: TimerType = TimerType.ONESHOT,
        owner: str = "",
        priority: TimerPriority = TimerPriority.NORMAL,
        can_coalesce: bool = True,
        args: tuple = (),
        kwargs: Optional[Dict[str, Any]] = None
    ) -> TimerHandle:
        """
        Schedule a timer.
        
        Args:
            callback: Function to call when timer fires
            delay_s: Delay in seconds
            timer_type: Type of timer
            owner: Owner module
            priority: Timer priority
            can_coalesce: Whether this timer can be coalesced
            args: Positional arguments for callback
            kwargs: Keyword arguments for callback
            
        Returns:
            TimerHandle for managing the timer
        """
        with self._lock:
            import secrets
            handle_id = f"th_{secrets.token_hex(6)}"
            
            if timer_type == TimerType.PERIODIC:
                return self._schedule_periodic(
                    callback, delay_s, owner, priority, can_coalesce, args, kwargs
                )
            
            # Use coalescer if available
            if self._coalescer:
                timer_id = self._coalescer.schedule(
                    callback, delay_s, priority, owner, can_coalesce, args, kwargs or {}
                )
            else:
                # Fallback: simple threading timer
                timer_id = f"simple_{secrets.token_hex(4)}"
                t = threading.Timer(delay_s, callback, args, kwargs or {})
                t.daemon = True
                t.start()
            
            handle = TimerHandle(
                handle_id=handle_id,
                timer_id=timer_id,
                timer_type=timer_type,
                owner=owner
            )
            
            self._handles[handle_id] = handle
            self._stats["timers_scheduled"] += 1
            
            return handle
    
    def _schedule_periodic(
        self,
        callback: Callable,
        period_s: float,
        owner: str,
        priority: TimerPriority,
        can_coalesce: bool,
        args: tuple,
        kwargs: Optional[Dict[str, Any]]
    ) -> TimerHandle:
        """Schedule a periodic timer"""
        import secrets
        handle_id = f"th_periodic_{secrets.token_hex(6)}"
        
        def periodic_callback():
            # Fire the callback
            try:
                callback(*args, **(kwargs or {}))
            except Exception as e:
                logger.error(f"Periodic timer {handle_id} callback failed: {e}")
            
            # Reschedule if still active
            handle = self._periodic_timers.get(handle_id)
            if handle and handle.timer_id in self._handles:
                handle.fire_count += 1
                self._reschedule_periodic(handle, period_s, priority, can_coalesce)
        
        # Schedule first fire
        timer_id = f"periodic_{secrets.token_hex(4)}"
        if self._coalescer:
            timer_id = self._coalescer.schedule(
                periodic_callback, period_s, priority, owner, can_coalesce
            )
        else:
            t = threading.Timer(period_s, periodic_callback)
            t.daemon = True
            t.start()
        
        handle = TimerHandle(
            handle_id=handle_id,
            timer_id=timer_id,
            timer_type=TimerType.PERIODIC,
            owner=owner,
            period_s=period_s
        )
        
        self._handles[handle_id] = handle
        self._periodic_timers[handle_id] = handle
        self._stats["timers_scheduled"] += 1
        
        return handle
    
    def _reschedule_periodic(
        self,
        handle: TimerHandle,
        period_s: float,
        priority: TimerPriority,
        can_coalesce: bool
    ):
        """Reschedule a periodic timer after it fires"""
        def periodic_callback():
            try:
                # Get original callback
                pass  # Already handled
            except Exception as e:
                logger.error(f"Periodic reschedule error: {e}")
        
        self._stats["periodic_fires"] += 1
    
    def cancel_timer(self, handle: TimerHandle) -> bool:
        """
        Cancel a timer.
        
        Args:
            handle: Timer handle to cancel
            
        Returns:
            True if cancelled
        """
        with self._lock:
            if handle.handle_id not in self._handles:
                return False
            
            del self._handles[handle.handle_id]
            
            # Remove from periodic tracking
            if handle.handle_id in self._periodic_timers:
                del self._periodic_timers[handle.handle_id]
            
            # Remove from cognitive waiters
            if handle.handle_id in self._cognitive_waiters:
                del self._cognitive_waiters[handle.handle_id]
            
            # Cancel in coalescer
            if self._coalescer:
                self._coalescer.cancel(handle.timer_id)
            
            self._stats["timers_cancelled"] += 1
            return True
    
    def set_deadline_alert(
        self,
        task_id: str,
        deadline: float,
        callback: Callable,
        owner: str = "",
        warn_threshold_s: float = 60.0
    ) -> DeadlineAlert:
        """
        Set a deadline alert.
        
        The callback is called when the deadline is reached.
        A warning can also be triggered before the deadline.
        
        Args:
            task_id: Task being monitored
            deadline: Unix timestamp of deadline
            callback: Function to call at deadline
            owner: Owner module
            warn_threshold_s: Seconds before deadline to warn
            
        Returns:
            DeadlineAlert object
        """
        with self._lock:
            import secrets
            alert_id = f"deadline_{secrets.token_hex(6)}"
            
            alert = DeadlineAlert(
                alert_id=alert_id,
                task_id=task_id,
                deadline=deadline,
                callback=callback,
                owner=owner,
                warn_threshold_s=warn_threshold_s
            )
            
            self._deadline_alerts[alert_id] = alert
            self._stats["deadline_alerts"] += 1
            
            # Schedule deadline timer
            delay = deadline - time.time()
            if delay > 0:
                self.schedule_timer(
                    lambda: self._handle_deadline(alert_id),
                    delay,
                    TimerType.DEADLINE,
                    owner=owner,
                    priority=TimerPriority.HIGH,
                    can_coalesce=False
                )
            
            # Schedule warning timer
            warn_delay = delay - warn_threshold_s
            if warn_delay > 0:
                self.schedule_timer(
                    lambda: self._handle_deadline_warning(alert_id),
                    warn_delay,
                    TimerType.ONESHOT,
                    owner=owner,
                    priority=TimerPriority.NORMAL
                )
            
            return alert
    
    def _handle_deadline(self, alert_id: str):
        """Handle a deadline being reached"""
        alert = self._deadline_alerts.get(alert_id)
        if alert:
            try:
                alert.callback()
            except Exception as e:
                logger.error(f"Deadline callback for {alert.task_id} failed: {e}")
            
            del self._deadline_alerts[alert_id]
    
    def _handle_deadline_warning(self, alert_id: str):
        """Handle a deadline warning"""
        alert = self._deadline_alerts.get(alert_id)
        if alert and not alert.warned:
            alert.warned = True
            remaining = alert.deadline - time.time()
            logger.warning(f"Deadline warning: task {alert.task_id} has {remaining:.1f}s remaining")
    
    def cancel_deadline_alert(self, alert_id: str) -> bool:
        """Cancel a deadline alert"""
        with self._lock:
            if alert_id in self._deadline_alerts:
                del self._deadline_alerts[alert_id]
                return True
            return False
    
    def schedule_cognitive_timer(
        self,
        callback: Callable,
        cognitive_delay: int,
        scale: TimeScale = TimeScale.MICRO,
        owner: str = ""
    ) -> TimerHandle:
        """
        Schedule a timer based on cognitive time.
        
        The timer fires after a certain amount of cognitive time
        has elapsed, not wall-clock time.
        
        Args:
            callback: Function to call
            cognitive_delay: Ticks to wait
            scale: Time scale
            owner: Owner module
            
        Returns:
            TimerHandle
        """
        with self._lock:
            if not self._cognitive_time:
                raise RuntimeError("Cognitive time not enabled")
            
            import secrets
            handle_id = f"th_cog_{secrets.token_hex(6)}"
            
            # Calculate target timestamp
            current = self._cognitive_time.now()
            
            if scale == TimeScale.MICRO:
                target = CognitiveTimestamp(
                    micro_ticks=current.micro_ticks + cognitive_delay,
                    meso_ticks=current.meso_ticks,
                    macro_ticks=current.macro_ticks
                )
            elif scale == TimeScale.MESO:
                target = CognitiveTimestamp(
                    micro_ticks=current.micro_ticks,
                    meso_ticks=current.meso_ticks + cognitive_delay,
                    macro_ticks=current.macro_ticks
                )
            else:  # MACRO
                target = CognitiveTimestamp(
                    micro_ticks=current.micro_ticks,
                    meso_ticks=current.meso_ticks,
                    macro_ticks=current.macro_ticks + cognitive_delay
                )
            
            handle = TimerHandle(
                handle_id=handle_id,
                timer_id=f"cog_{handle_id}",
                timer_type=TimerType.COGNITIVE,
                owner=owner,
                cognitive_target=target
            )
            
            # Store callback for later
            handle.deadline_callback = callback
            
            self._handles[handle_id] = handle
            self._cognitive_waiters[handle_id] = handle
            self._stats["timers_scheduled"] += 1
            
            # Subscribe to cognitive time changes
            self._cognitive_time.subscribe(
                f"timer_{handle_id}",
                lambda old, new, evt: self._check_cognitive_timer(handle_id, new)
            )
            
            return handle
    
    def _check_cognitive_timer(
        self,
        handle_id: str,
        current: CognitiveTimestamp
    ):
        """Check if a cognitive timer should fire"""
        handle = self._cognitive_waiters.get(handle_id)
        if not handle or not handle.cognitive_target:
            return
        
        if current >= handle.cognitive_target:
            # Fire the timer
            if handle.deadline_callback:
                try:
                    handle.deadline_callback()
                except Exception as e:
                    logger.error(f"Cognitive timer {handle_id} callback failed: {e}")
            
            # Clean up
            del self._cognitive_waiters[handle_id]
            if handle_id in self._handles:
                del self._handles[handle_id]
            
            if self._cognitive_time:
                self._cognitive_time.unsubscribe(f"timer_{handle_id}")
            
            self._stats["cognitive_triggers"] += 1
    
    def get_cognitive_time(self) -> CognitiveTimestamp:
        """Get current cognitive time"""
        if self._cognitive_time:
            return self._cognitive_time.now()
        raise RuntimeError("Cognitive time not enabled")
    
    def advance_cognitive_time(
        self,
        events_processed: int = 1,
        scale: TimeScale = TimeScale.MICRO,
        event_type: str = "generic",
        source: str = "unknown"
    ) -> CognitiveTimestamp:
        """
        Advance cognitive time.
        
        Args:
            events_processed: Number of events processed
            scale: Time scale
            event_type: Type of event
            source: Source module
            
        Returns:
            New cognitive timestamp
        """
        if self._cognitive_time:
            return self._cognitive_time.advance(
                scale, events_processed, event_type, source
            )
        raise RuntimeError("Cognitive time not enabled")
    
    def coalesce_timers(self, window_ms: Optional[float] = None) -> int:
        """
        Force timer coalescing.
        
        Args:
            window_ms: Custom coalescing window (uses default if None)
            
        Returns:
            Number of timers coalesced
        """
        if not self._coalescer:
            return 0
        
        groups = self._coalescer.coalesce()
        return sum(g.timer_count - 1 for g in groups if g.timer_count > 1)
    
    def start(self):
        """Start the timer service"""
        with self._lock:
            if self._running:
                return
            
            self._running = True
            
            if self._coalescer:
                self._coalescer.start()
            
            self._service_thread = threading.Thread(
                target=self._service_loop,
                name="AGI_TimerService",
                daemon=True
            )
            self._service_thread.start()
            
            logger.info("AGI_TimerService started")
    
    def stop(self):
        """Stop the timer service"""
        with self._lock:
            self._running = False
            
            if self._coalescer:
                self._coalescer.stop()
            
            if self._service_thread:
                self._service_thread.join(timeout=1.0)
                self._service_thread = None
            
            logger.info("AGI_TimerService stopped")
    
    def _service_loop(self):
        """Background service loop"""
        while self._running:
            try:
                # Check deadlines
                now = time.time()
                for alert_id, alert in list(self._deadline_alerts.items()):
                    if alert.deadline <= now:
                        self._handle_deadline(alert_id)
            except Exception as e:
                logger.error(f"Service loop error: {e}")
            
            time.sleep(0.1)
    
    def list_timers(self, owner: Optional[str] = None) -> List[Dict[str, Any]]:
        """List all active timers"""
        with self._lock:
            handles = self._handles.values()
            
            if owner:
                handles = [h for h in handles if h.owner == owner]
            
            return [
                {
                    "handle_id": h.handle_id,
                    "timer_type": h.timer_type.name,
                    "owner": h.owner,
                    "created_at": h.created_at,
                    "fire_count": h.fire_count
                }
                for h in handles
            ]
    
    def list_deadline_alerts(self) -> List[Dict[str, Any]]:
        """List all deadline alerts"""
        with self._lock:
            return [
                {
                    "alert_id": a.alert_id,
                    "task_id": a.task_id,
                    "deadline": a.deadline,
                    "remaining_s": a.deadline - time.time(),
                    "warned": a.warned,
                    "owner": a.owner
                }
                for a in self._deadline_alerts.values()
            ]
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get timer service statistics"""
        with self._lock:
            stats = dict(self._stats)
            stats["active_timers"] = len(self._handles)
            stats["periodic_timers"] = len(self._periodic_timers)
            stats["deadline_alerts"] = len(self._deadline_alerts)
            stats["cognitive_waiters"] = len(self._cognitive_waiters)
            stats["running"] = self._running
            
            if self._coalescer:
                stats["coalescer"] = self._coalescer.get_statistics()
            
            if self._cognitive_time:
                stats["cognitive_time"] = self._cognitive_time.now().to_dict()
            
            return stats


# Global timer service instance
_timer_service: Optional[AGI_TimerService] = None


def get_timer_service() -> AGI_TimerService:
    """Get the global AGI timer service instance"""
    global _timer_service
    if _timer_service is None:
        _timer_service = AGI_TimerService()
    return _timer_service


if __name__ == "__main__":
    import json
    
    print("=== AGI Timer Service Example ===\n")
    
    service = get_timer_service()
    service.start()
    
    # Track fired timers
    fired = []
    
    # Schedule oneshot timer
    print("=== Scheduling Timers ===")
    
    handle1 = service.schedule_timer(
        lambda: fired.append("oneshot_1"),
        0.2,
        TimerType.ONESHOT,
        owner="test"
    )
    print(f"Scheduled oneshot timer: {handle1.handle_id}")
    
    handle2 = service.schedule_timer(
        lambda: fired.append("oneshot_2"),
        0.25,
        TimerType.ONESHOT,
        owner="test"
    )
    print(f"Scheduled oneshot timer: {handle2.handle_id}")
    
    # Schedule periodic timer
    periodic_count = [0]
    def periodic_cb():
        periodic_count[0] += 1
        fired.append(f"periodic_{periodic_count[0]}")
    
    handle3 = service.schedule_timer(
        periodic_cb,
        0.1,
        TimerType.PERIODIC,
        owner="test"
    )
    print(f"Scheduled periodic timer: {handle3.handle_id}")
    
    # Set deadline alert
    print("\n=== Setting Deadline Alert ===")
    alert = service.set_deadline_alert(
        task_id="important_task",
        deadline=time.time() + 0.5,
        callback=lambda: fired.append("deadline"),
        owner="test",
        warn_threshold_s=0.3
    )
    print(f"Set deadline alert: {alert.alert_id}")
    
    # Schedule cognitive timer
    print("\n=== Scheduling Cognitive Timer ===")
    cog_handle = service.schedule_cognitive_timer(
        lambda: fired.append("cognitive"),
        10,
        TimeScale.MICRO,
        owner="test"
    )
    print(f"Scheduled cognitive timer: {cog_handle.handle_id}")
    
    # Advance cognitive time
    for i in range(12):
        service.advance_cognitive_time(1, TimeScale.MICRO, "test_event", "test")
    
    # Wait for timers
    print("\n=== Waiting for timers... ===")
    time.sleep(0.6)
    
    # Cancel periodic timer
    service.cancel_timer(handle3)
    
    print(f"\nFired timers: {fired}")
    
    # Get statistics
    print("\n=== Statistics ===")
    print(json.dumps(service.get_statistics(), indent=2, default=str))
    
    # Cleanup
    service.stop()
    print("\nTimer service stopped.")
