#!/usr/bin/env python3
"""
Cognitive Time Tracking for AGI-OS

This module provides cognitive time management for the OpenCog AGI-OS.
Cognitive time is event-based logical time that advances based on
cognitive processing rather than wall-clock time.

Features:
- Event-based logical time
- Multiple time scales (micro, meso, macro)
- Time synchronization across cognitive modules
- Temporal attention weighting
- Historical time tracking
"""

import time
import logging
import threading
from typing import Dict, Any, Optional, List, Tuple
from dataclasses import dataclass, field
from enum import Enum
from collections import deque

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("AGI_Time.Cognitive")


class TimeScale(Enum):
    """
    Cognitive time scales for different levels of processing.
    
    - MICRO: Individual atom operations, single inference steps
    - MESO: Patterns, sub-goal completion, attention cycles
    - MACRO: Goals, learning episodes, major state changes
    """
    MICRO = 1       # ~milliseconds, fine-grained operations
    MESO = 2        # ~seconds, cognitive cycles
    MACRO = 3       # ~minutes, goal-level changes


@dataclass
class CognitiveTimestamp:
    """
    A cognitive timestamp representing a point in cognitive time.
    
    Cognitive time is multi-scale and event-based, not tied to
    wall-clock time. It advances when cognitive processing occurs.
    """
    # Scale-specific tick counts
    micro_ticks: int = 0
    meso_ticks: int = 0
    macro_ticks: int = 0
    
    # Wall-clock reference (for synchronization)
    wall_time: float = field(default_factory=time.time)
    
    # Events that caused this timestamp
    event_count: int = 0
    
    def __lt__(self, other: "CognitiveTimestamp") -> bool:
        return (self.macro_ticks, self.meso_ticks, self.micro_ticks) < \
               (other.macro_ticks, other.meso_ticks, other.micro_ticks)
    
    def __eq__(self, other: object) -> bool:
        if not isinstance(other, CognitiveTimestamp):
            return False
        return (self.macro_ticks == other.macro_ticks and
                self.meso_ticks == other.meso_ticks and
                self.micro_ticks == other.micro_ticks)
    
    def __hash__(self):
        return hash((self.micro_ticks, self.meso_ticks, self.macro_ticks))
    
    def total_micro_ticks(self) -> int:
        """Get total time in micro-ticks"""
        return (self.macro_ticks * 10000 +
                self.meso_ticks * 100 +
                self.micro_ticks)
    
    def copy(self) -> "CognitiveTimestamp":
        """Create a copy of this timestamp"""
        return CognitiveTimestamp(
            micro_ticks=self.micro_ticks,
            meso_ticks=self.meso_ticks,
            macro_ticks=self.macro_ticks,
            wall_time=self.wall_time,
            event_count=self.event_count
        )
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "micro_ticks": self.micro_ticks,
            "meso_ticks": self.meso_ticks,
            "macro_ticks": self.macro_ticks,
            "wall_time": self.wall_time,
            "event_count": self.event_count,
            "total_micro_ticks": self.total_micro_ticks()
        }
    
    def __repr__(self) -> str:
        return f"CogT({self.macro_ticks}:{self.meso_ticks}:{self.micro_ticks})"


@dataclass
class CognitiveEvent:
    """
    An event that advances cognitive time.
    """
    event_id: str
    event_type: str
    scale: TimeScale
    source: str
    timestamp: CognitiveTimestamp
    wall_time: float = field(default_factory=time.time)
    metadata: Dict[str, Any] = field(default_factory=dict)


class CognitiveTimeManager:
    """
    Manages cognitive time for the AGI-OS.
    
    Cognitive time advances based on events processed by cognitive
    modules, not wall-clock time. This allows for consistent temporal
    reasoning regardless of processing speed.
    """
    
    def __init__(
        self,
        micro_per_meso: int = 100,
        meso_per_macro: int = 100,
        history_size: int = 1000
    ):
        """
        Initialize the cognitive time manager.
        
        Args:
            micro_per_meso: Micro-ticks before incrementing meso
            meso_per_macro: Meso-ticks before incrementing macro
            history_size: Number of events to keep in history
        """
        self._lock = threading.RLock()
        
        # Current time
        self._current = CognitiveTimestamp()
        
        # Scale ratios
        self._micro_per_meso = micro_per_meso
        self._meso_per_macro = meso_per_macro
        
        # Event history
        self._history: deque = deque(maxlen=history_size)
        
        # Subscribers for time changes
        self._subscribers: Dict[str, callable] = {}
        
        # Statistics
        self._stats = {
            "total_events": 0,
            "events_by_scale": {s: 0 for s in TimeScale},
            "events_by_type": {}
        }
        
        logger.info("CognitiveTimeManager initialized")
    
    def now(self) -> CognitiveTimestamp:
        """Get the current cognitive time"""
        with self._lock:
            return self._current.copy()
    
    def advance(
        self,
        scale: TimeScale,
        ticks: int = 1,
        event_type: str = "generic",
        source: str = "unknown",
        metadata: Optional[Dict[str, Any]] = None
    ) -> CognitiveTimestamp:
        """
        Advance cognitive time.
        
        Args:
            scale: Time scale to advance
            ticks: Number of ticks to advance
            event_type: Type of event causing the advance
            source: Source module
            metadata: Additional event metadata
            
        Returns:
            New timestamp after advancement
        """
        with self._lock:
            import secrets
            event_id = f"evt_{secrets.token_hex(4)}"
            
            # Record event before time change
            old_time = self._current.copy()
            
            # Advance time at the specified scale
            if scale == TimeScale.MICRO:
                self._advance_micro(ticks)
            elif scale == TimeScale.MESO:
                self._advance_meso(ticks)
            elif scale == TimeScale.MACRO:
                self._advance_macro(ticks)
            
            self._current.event_count += 1
            self._current.wall_time = time.time()
            
            # Create event record
            event = CognitiveEvent(
                event_id=event_id,
                event_type=event_type,
                scale=scale,
                source=source,
                timestamp=self._current.copy(),
                metadata=metadata or {}
            )
            
            self._history.append(event)
            
            # Update statistics
            self._stats["total_events"] += 1
            self._stats["events_by_scale"][scale] += 1
            self._stats["events_by_type"][event_type] = \
                self._stats["events_by_type"].get(event_type, 0) + 1
            
            # Notify subscribers
            self._notify_subscribers(old_time, self._current, event)
            
            return self._current.copy()
    
    def _advance_micro(self, ticks: int):
        """Advance micro-ticks with overflow"""
        self._current.micro_ticks += ticks
        
        # Check for meso overflow
        while self._current.micro_ticks >= self._micro_per_meso:
            self._current.micro_ticks -= self._micro_per_meso
            self._advance_meso(1)
    
    def _advance_meso(self, ticks: int):
        """Advance meso-ticks with overflow"""
        self._current.meso_ticks += ticks
        
        # Check for macro overflow
        while self._current.meso_ticks >= self._meso_per_macro:
            self._current.meso_ticks -= self._meso_per_macro
            self._advance_macro(1)
    
    def _advance_macro(self, ticks: int):
        """Advance macro-ticks"""
        self._current.macro_ticks += ticks
    
    def tick(
        self,
        event_type: str = "tick",
        source: str = "scheduler"
    ) -> CognitiveTimestamp:
        """
        Advance cognitive time by one micro-tick.
        
        Convenience method for the common case of incrementing
        by a single micro-tick.
        """
        return self.advance(TimeScale.MICRO, 1, event_type, source)
    
    def subscribe(
        self,
        name: str,
        callback: callable
    ) -> str:
        """
        Subscribe to time changes.
        
        Callback receives: (old_time, new_time, event)
        
        Returns:
            Subscription ID
        """
        with self._lock:
            self._subscribers[name] = callback
            return name
    
    def unsubscribe(self, name: str) -> bool:
        """Unsubscribe from time changes"""
        with self._lock:
            return self._subscribers.pop(name, None) is not None
    
    def _notify_subscribers(
        self,
        old_time: CognitiveTimestamp,
        new_time: CognitiveTimestamp,
        event: CognitiveEvent
    ):
        """Notify all subscribers of time change"""
        for name, callback in self._subscribers.items():
            try:
                callback(old_time, new_time, event)
            except Exception as e:
                logger.error(f"Subscriber {name} failed: {e}")
    
    def get_elapsed(
        self,
        since: CognitiveTimestamp,
        scale: TimeScale = TimeScale.MICRO
    ) -> int:
        """
        Get elapsed time since a timestamp.
        
        Args:
            since: Starting timestamp
            scale: Scale to measure in
            
        Returns:
            Elapsed ticks at the specified scale
        """
        with self._lock:
            now = self._current
            
            if scale == TimeScale.MICRO:
                return now.total_micro_ticks() - since.total_micro_ticks()
            elif scale == TimeScale.MESO:
                return (now.macro_ticks * self._meso_per_macro + now.meso_ticks) - \
                       (since.macro_ticks * self._meso_per_macro + since.meso_ticks)
            else:  # MACRO
                return now.macro_ticks - since.macro_ticks
    
    def compare(
        self,
        t1: CognitiveTimestamp,
        t2: CognitiveTimestamp
    ) -> int:
        """
        Compare two timestamps.
        
        Returns:
            -1 if t1 < t2, 0 if equal, 1 if t1 > t2
        """
        if t1 < t2:
            return -1
        elif t1 == t2:
            return 0
        else:
            return 1
    
    def get_history(
        self,
        count: Optional[int] = None,
        event_type: Optional[str] = None,
        scale: Optional[TimeScale] = None
    ) -> List[Dict[str, Any]]:
        """
        Get event history with optional filtering.
        
        Args:
            count: Maximum events to return
            event_type: Filter by event type
            scale: Filter by time scale
            
        Returns:
            List of event dictionaries
        """
        with self._lock:
            events = list(self._history)
            
            if event_type:
                events = [e for e in events if e.event_type == event_type]
            if scale:
                events = [e for e in events if e.scale == scale]
            if count:
                events = events[-count:]
            
            return [
                {
                    "event_id": e.event_id,
                    "event_type": e.event_type,
                    "scale": e.scale.name,
                    "source": e.source,
                    "timestamp": e.timestamp.to_dict(),
                    "wall_time": e.wall_time,
                    "metadata": e.metadata
                }
                for e in events
            ]
    
    def get_events_between(
        self,
        start: CognitiveTimestamp,
        end: CognitiveTimestamp
    ) -> List[CognitiveEvent]:
        """Get events between two timestamps"""
        with self._lock:
            return [
                e for e in self._history
                if start <= e.timestamp <= end
            ]
    
    def reset(self):
        """Reset cognitive time to zero"""
        with self._lock:
            self._current = CognitiveTimestamp()
            self._history.clear()
            logger.info("Cognitive time reset")
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get time manager statistics"""
        with self._lock:
            return {
                "current_time": self._current.to_dict(),
                "total_events": self._stats["total_events"],
                "events_by_scale": {
                    s.name: c for s, c in self._stats["events_by_scale"].items()
                },
                "events_by_type": self._stats["events_by_type"],
                "history_size": len(self._history),
                "subscribers": list(self._subscribers.keys())
            }
    
    def sync_with_wall_clock(self):
        """
        Synchronize cognitive time with wall clock.
        
        Updates the wall_time field without advancing cognitive time.
        Useful for establishing temporal correlations.
        """
        with self._lock:
            self._current.wall_time = time.time()
    
    def wall_to_cognitive_rate(self) -> float:
        """
        Calculate the ratio of cognitive time to wall clock time.
        
        Returns:
            Micro-ticks per second of wall time
        """
        with self._lock:
            if len(self._history) < 2:
                return 0.0
            
            first = self._history[0]
            last = self._history[-1]
            
            wall_elapsed = last.wall_time - first.wall_time
            if wall_elapsed <= 0:
                return 0.0
            
            cog_elapsed = last.timestamp.total_micro_ticks() - \
                         first.timestamp.total_micro_ticks()
            
            return cog_elapsed / wall_elapsed


# Global cognitive time manager
_cognitive_time_manager: Optional[CognitiveTimeManager] = None


def get_cognitive_time_manager() -> CognitiveTimeManager:
    """Get the global cognitive time manager instance"""
    global _cognitive_time_manager
    if _cognitive_time_manager is None:
        _cognitive_time_manager = CognitiveTimeManager()
    return _cognitive_time_manager


if __name__ == "__main__":
    import json
    
    print("=== Cognitive Time Manager Example ===\n")
    
    ctm = get_cognitive_time_manager()
    
    # Subscribe to time changes
    def on_time_change(old_time, new_time, event):
        print(f"  Time: {old_time} -> {new_time} ({event.event_type})")
    
    ctm.subscribe("printer", on_time_change)
    
    # Advance time
    print("=== Advancing Cognitive Time ===")
    
    ctm.advance(TimeScale.MICRO, 5, "inference_step", "pln_engine")
    ctm.advance(TimeScale.MICRO, 10, "attention_update", "ecan")
    ctm.advance(TimeScale.MESO, 1, "pattern_found", "pattern_miner")
    ctm.advance(TimeScale.MICRO, 50, "memory_access", "atomspace")
    ctm.advance(TimeScale.MESO, 2, "goal_progress", "goal_tracker")
    ctm.advance(TimeScale.MACRO, 1, "learning_episode", "moses")
    
    # Get current time
    print(f"\n=== Current Time ===")
    now = ctm.now()
    print(f"  Macro: {now.macro_ticks}")
    print(f"  Meso: {now.meso_ticks}")
    print(f"  Micro: {now.micro_ticks}")
    print(f"  Total micro-ticks: {now.total_micro_ticks()}")
    
    # Get statistics
    print("\n=== Statistics ===")
    print(json.dumps(ctm.get_statistics(), indent=2))
    
    # Get event history
    print("\n=== Recent History ===")
    for event in ctm.get_history(count=3):
        print(f"  {event['event_type']} at {event['timestamp']['micro_ticks']} micro-ticks")
