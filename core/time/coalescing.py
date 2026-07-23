#!/usr/bin/env python3
"""
Timer Coalescing for AGI-OS

This module provides timer coalescing to improve efficiency of
timer-based cognitive operations. Multiple timers can be grouped
and fired together to reduce scheduling overhead.

Features:
- Timer grouping by deadline windows
- Configurable coalescing policies
- Priority-aware coalescing
- Integration with AGI_Scheduler
- Power-efficient timer management
"""

import time
import logging
import threading
import heapq
from typing import Dict, Any, Optional, List, Callable, Set
from dataclasses import dataclass, field
from enum import Enum

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("AGI_Time.Coalescing")


class CoalescingPolicy(Enum):
    """Timer coalescing policies"""
    NONE = "none"               # No coalescing
    RELAXED = "relaxed"         # Coalesce within large windows
    MODERATE = "moderate"       # Coalesce within moderate windows
    AGGRESSIVE = "aggressive"   # Maximize coalescing
    COGNITIVE = "cognitive"     # Coalesce based on attention


class TimerPriority(Enum):
    """Timer priority levels"""
    CRITICAL = 1    # Never coalesce, fire immediately
    HIGH = 2        # Minimal coalescing
    NORMAL = 3      # Standard coalescing
    LOW = 4         # Aggressive coalescing allowed
    BACKGROUND = 5  # Maximum coalescing


@dataclass(order=True)
class Timer:
    """
    A timer scheduled for future execution.
    """
    deadline: float = field(compare=True)
    timer_id: str = field(compare=False)
    callback: Callable = field(compare=False)
    priority: TimerPriority = field(default=TimerPriority.NORMAL, compare=False)
    args: tuple = field(default_factory=tuple, compare=False)
    kwargs: Dict[str, Any] = field(default_factory=dict, compare=False)
    
    # Coalescing metadata
    coalesce_window_ms: float = field(default=0.0, compare=False)
    can_coalesce: bool = field(default=True, compare=False)
    owner: str = field(default="", compare=False)
    
    # Tracking
    created_at: float = field(default_factory=time.time, compare=False)
    coalesced_with: Optional[str] = field(default=None, compare=False)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "timer_id": self.timer_id,
            "deadline": self.deadline,
            "priority": self.priority.name,
            "owner": self.owner,
            "can_coalesce": self.can_coalesce,
            "coalesce_window_ms": self.coalesce_window_ms,
            "created_at": self.created_at,
            "coalesced_with": self.coalesced_with
        }


@dataclass
class CoalescedTimer:
    """
    A group of coalesced timers that fire together.
    """
    group_id: str
    fire_time: float
    timers: List[Timer]
    
    @property
    def timer_count(self) -> int:
        return len(self.timers)
    
    @property
    def highest_priority(self) -> TimerPriority:
        return min(t.priority for t in self.timers)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "group_id": self.group_id,
            "fire_time": self.fire_time,
            "timer_count": self.timer_count,
            "highest_priority": self.highest_priority.name,
            "timers": [t.timer_id for t in self.timers]
        }


class TimerCoalescer:
    """
    Timer Coalescer for AGI-OS
    
    Groups timers with similar deadlines to fire together,
    reducing scheduling overhead and improving efficiency.
    """
    
    def __init__(
        self,
        policy: CoalescingPolicy = CoalescingPolicy.MODERATE,
        default_window_ms: float = 50.0,
        max_window_ms: float = 500.0
    ):
        """
        Initialize the timer coalescer.
        
        Args:
            policy: Default coalescing policy
            default_window_ms: Default coalescing window
            max_window_ms: Maximum coalescing window
        """
        self._lock = threading.RLock()
        
        self._policy = policy
        self._default_window_ms = default_window_ms
        self._max_window_ms = max_window_ms
        
        # Timer heap (priority queue by deadline)
        self._timers: List[Timer] = []
        heapq.heapify(self._timers)
        
        # Timer lookup
        self._timer_lookup: Dict[str, Timer] = {}
        
        # Coalesced groups
        self._coalesced_groups: Dict[str, CoalescedTimer] = {}
        
        # Background processing
        self._running = False
        self._process_thread: Optional[threading.Thread] = None
        
        # Statistics
        self._stats = {
            "timers_scheduled": 0,
            "timers_fired": 0,
            "timers_cancelled": 0,
            "groups_created": 0,
            "coalescing_savings": 0  # Timers saved by coalescing
        }
        
        logger.info(f"TimerCoalescer initialized with policy {policy.name}")
    
    def schedule(
        self,
        callback: Callable,
        delay_s: float,
        priority: TimerPriority = TimerPriority.NORMAL,
        owner: str = "",
        can_coalesce: bool = True,
        args: tuple = (),
        kwargs: Optional[Dict[str, Any]] = None
    ) -> str:
        """
        Schedule a timer.
        
        Args:
            callback: Function to call when timer fires
            delay_s: Delay in seconds
            priority: Timer priority
            owner: Owner module
            can_coalesce: Whether this timer can be coalesced
            args: Positional arguments for callback
            kwargs: Keyword arguments for callback
            
        Returns:
            Timer ID
        """
        with self._lock:
            import secrets
            timer_id = f"timer_{secrets.token_hex(6)}"
            
            deadline = time.time() + delay_s
            window_ms = self._get_coalesce_window(priority)
            
            timer = Timer(
                deadline=deadline,
                timer_id=timer_id,
                callback=callback,
                priority=priority,
                args=args,
                kwargs=kwargs or {},
                coalesce_window_ms=window_ms,
                can_coalesce=can_coalesce and priority != TimerPriority.CRITICAL,
                owner=owner
            )
            
            # Add to heap
            heapq.heappush(self._timers, timer)
            self._timer_lookup[timer_id] = timer
            
            self._stats["timers_scheduled"] += 1
            
            logger.debug(f"Scheduled timer {timer_id} for {delay_s}s")
            return timer_id
    
    def cancel(self, timer_id: str) -> bool:
        """
        Cancel a timer.
        
        Args:
            timer_id: Timer to cancel
            
        Returns:
            True if cancelled
        """
        with self._lock:
            timer = self._timer_lookup.pop(timer_id, None)
            if timer:
                # Mark as cancelled (will be skipped during processing)
                timer.callback = None
                self._stats["timers_cancelled"] += 1
                return True
            return False
    
    def _get_coalesce_window(self, priority: TimerPriority) -> float:
        """Get coalescing window for a priority level"""
        if self._policy == CoalescingPolicy.NONE:
            return 0.0
        
        base_window = self._default_window_ms
        
        if self._policy == CoalescingPolicy.RELAXED:
            base_window *= 2.0
        elif self._policy == CoalescingPolicy.AGGRESSIVE:
            base_window *= 4.0
        elif self._policy == CoalescingPolicy.COGNITIVE:
            base_window *= 1.5  # Adjusted dynamically
        
        # Adjust by priority
        multipliers = {
            TimerPriority.CRITICAL: 0.0,
            TimerPriority.HIGH: 0.25,
            TimerPriority.NORMAL: 1.0,
            TimerPriority.LOW: 2.0,
            TimerPriority.BACKGROUND: 4.0
        }
        
        window = base_window * multipliers.get(priority, 1.0)
        return min(window, self._max_window_ms)
    
    def coalesce(self) -> List[CoalescedTimer]:
        """
        Coalesce ready timers into groups.
        
        Returns:
            List of coalesced timer groups
        """
        with self._lock:
            now = time.time()
            groups: Dict[float, CoalescedTimer] = {}
            
            # Collect ready timers
            ready_timers = []
            
            while self._timers and self._timers[0].deadline <= now:
                timer = heapq.heappop(self._timers)
                
                # Skip cancelled timers
                if timer.callback is None:
                    continue
                
                if timer.timer_id in self._timer_lookup:
                    del self._timer_lookup[timer.timer_id]
                
                ready_timers.append(timer)
            
            if not ready_timers:
                return []
            
            # Group by coalescing window
            for timer in ready_timers:
                if not timer.can_coalesce:
                    # Non-coalescable timer gets its own group
                    import secrets
                    group_id = f"group_{secrets.token_hex(4)}"
                    groups[timer.deadline] = CoalescedTimer(
                        group_id=group_id,
                        fire_time=timer.deadline,
                        timers=[timer]
                    )
                else:
                    # Find a group within the coalescing window
                    window_s = timer.coalesce_window_ms / 1000.0
                    group_found = False
                    
                    for fire_time, group in groups.items():
                        if abs(fire_time - timer.deadline) <= window_s:
                            group.timers.append(timer)
                            timer.coalesced_with = group.group_id
                            group_found = True
                            break
                    
                    if not group_found:
                        import secrets
                        group_id = f"group_{secrets.token_hex(4)}"
                        groups[timer.deadline] = CoalescedTimer(
                            group_id=group_id,
                            fire_time=timer.deadline,
                            timers=[timer]
                        )
            
            result = list(groups.values())
            
            # Calculate savings
            if result:
                total_timers = sum(g.timer_count for g in result)
                savings = total_timers - len(result)
                self._stats["coalescing_savings"] += savings
                self._stats["groups_created"] += len(result)
            
            return result
    
    def fire_ready(self) -> int:
        """
        Fire all ready timers (after coalescing).
        
        Returns:
            Number of timers fired
        """
        groups = self.coalesce()
        fired = 0
        
        for group in groups:
            for timer in group.timers:
                if timer.callback is not None:
                    try:
                        timer.callback(*timer.args, **timer.kwargs)
                        fired += 1
                    except Exception as e:
                        logger.error(f"Timer {timer.timer_id} callback failed: {e}")
        
        self._stats["timers_fired"] += fired
        return fired
    
    def start(self, check_interval_s: float = 0.01):
        """
        Start background timer processing.
        
        Args:
            check_interval_s: How often to check for ready timers
        """
        with self._lock:
            if self._running:
                return
            
            self._running = True
            self._process_thread = threading.Thread(
                target=self._process_loop,
                args=(check_interval_s,),
                name="TimerCoalescer",
                daemon=True
            )
            self._process_thread.start()
            logger.info("Timer coalescer started")
    
    def stop(self):
        """Stop background timer processing"""
        with self._lock:
            self._running = False
            if self._process_thread:
                self._process_thread.join(timeout=1.0)
                self._process_thread = None
            logger.info("Timer coalescer stopped")
    
    def _process_loop(self, interval: float):
        """Background processing loop"""
        while self._running:
            self.fire_ready()
            time.sleep(interval)
    
    def get_pending_count(self) -> int:
        """Get number of pending timers"""
        with self._lock:
            return sum(1 for t in self._timer_lookup.values() if t.callback is not None)
    
    def get_next_deadline(self) -> Optional[float]:
        """Get the next timer deadline"""
        with self._lock:
            # Find first non-cancelled timer
            for timer in self._timers:
                if timer.callback is not None:
                    return timer.deadline
            return None
    
    def list_timers(
        self,
        owner: Optional[str] = None,
        priority: Optional[TimerPriority] = None
    ) -> List[Dict[str, Any]]:
        """List pending timers with optional filtering"""
        with self._lock:
            timers = [
                t for t in self._timer_lookup.values()
                if t.callback is not None
            ]
            
            if owner:
                timers = [t for t in timers if t.owner == owner]
            if priority:
                timers = [t for t in timers if t.priority == priority]
            
            return [t.to_dict() for t in timers]
    
    def set_policy(self, policy: CoalescingPolicy):
        """Change the coalescing policy"""
        with self._lock:
            self._policy = policy
            logger.info(f"Coalescing policy changed to {policy.name}")
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get coalescer statistics"""
        with self._lock:
            return {
                **self._stats,
                "policy": self._policy.name,
                "default_window_ms": self._default_window_ms,
                "max_window_ms": self._max_window_ms,
                "pending_timers": self.get_pending_count(),
                "running": self._running
            }


# Global timer coalescer
_timer_coalescer: Optional[TimerCoalescer] = None


def get_timer_coalescer() -> TimerCoalescer:
    """Get the global timer coalescer instance"""
    global _timer_coalescer
    if _timer_coalescer is None:
        _timer_coalescer = TimerCoalescer()
    return _timer_coalescer


if __name__ == "__main__":
    import json
    
    print("=== Timer Coalescer Example ===\n")
    
    coalescer = get_timer_coalescer()
    
    # Track fired timers
    fired_timers = []
    
    def make_callback(name):
        def cb():
            fired_timers.append((name, time.time()))
            print(f"  FIRED: {name}")
        return cb
    
    # Schedule multiple timers
    print("=== Scheduling Timers ===")
    
    coalescer.schedule(make_callback("timer_a"), 0.1, TimerPriority.NORMAL, "module_a")
    coalescer.schedule(make_callback("timer_b"), 0.11, TimerPriority.NORMAL, "module_b")
    coalescer.schedule(make_callback("timer_c"), 0.12, TimerPriority.LOW, "module_c")
    coalescer.schedule(make_callback("timer_d"), 0.2, TimerPriority.HIGH, "module_a")
    coalescer.schedule(make_callback("timer_e"), 0.21, TimerPriority.NORMAL, "module_b")
    coalescer.schedule(make_callback("critical"), 0.15, TimerPriority.CRITICAL, "system")
    
    print(f"Scheduled 6 timers")
    print(f"Pending: {coalescer.get_pending_count()}")
    
    # Wait for timers to be ready
    print("\n=== Waiting for timers... ===")
    time.sleep(0.25)
    
    # Fire with coalescing
    print("\n=== Firing Coalesced Timers ===")
    groups = coalescer.coalesce()
    
    print(f"\nCoalesced into {len(groups)} groups:")
    for group in groups:
        print(f"  Group {group.group_id}: {group.timer_count} timers "
              f"(priority: {group.highest_priority.name})")
    
    # Fire them
    fired = coalescer.fire_ready()
    print(f"\nFired {fired} timers")
    
    # Get statistics
    print("\n=== Statistics ===")
    print(json.dumps(coalescer.get_statistics(), indent=2))
