#!/usr/bin/env python3
"""
Memory Pressure Monitoring for AGI-OS

This module provides real-time memory pressure monitoring and throttling
for cognitive workloads. It enables proactive memory management to prevent
OOM situations and optimize cognitive performance under memory constraints.

Features:
- System memory pressure detection
- Pressure level classification (NORMAL, MODERATE, HIGH, CRITICAL)
- Pressure callbacks and notifications
- Automatic throttling recommendations
- Integration with garbage collection
- AtomSpace-aware pressure handling
"""

import os
import time
import logging
import threading
import gc
from typing import Dict, Any, Optional, List, Callable
from dataclasses import dataclass, field
from enum import Enum
from collections import deque

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("AGI_Memory.Pressure")


class PressureLevel(Enum):
    """Memory pressure levels"""
    NORMAL = 1       # < 50% usage, no concern
    MODERATE = 2     # 50-70% usage, start optimization
    HIGH = 3         # 70-85% usage, throttle allocations
    CRITICAL = 4     # > 85% usage, emergency measures
    OOM_IMMINENT = 5 # > 95% usage, stop all allocations


@dataclass
class MemorySnapshot:
    """Snapshot of memory state at a point in time"""
    timestamp: float
    total_mb: float
    available_mb: float
    used_mb: float
    cached_mb: float
    buffers_mb: float
    swap_used_mb: float
    swap_total_mb: float
    pressure_level: PressureLevel
    
    @property
    def usage_percent(self) -> float:
        if self.total_mb == 0:
            return 0.0
        return (self.used_mb / self.total_mb) * 100


@dataclass
class PressureThresholds:
    """
    Configurable thresholds for pressure levels.
    
    Values are percentages of total memory.
    """
    normal_max: float = 50.0
    moderate_max: float = 70.0
    high_max: float = 85.0
    critical_max: float = 95.0
    
    # Hysteresis to prevent oscillation
    hysteresis: float = 5.0


@dataclass
class ThrottleRecommendation:
    """Recommendation for throttling based on pressure"""
    level: PressureLevel
    should_throttle: bool
    throttle_factor: float  # 0.0 = fully throttled, 1.0 = no throttling
    gc_recommended: bool
    eviction_recommended: bool
    message: str


class PressureCallback:
    """
    Callback for pressure level changes.
    """
    def __init__(
        self,
        callback: Callable[[PressureLevel, PressureLevel], None],
        trigger_levels: Optional[List[PressureLevel]] = None,
        name: str = ""
    ):
        self.callback = callback
        self.trigger_levels = trigger_levels or list(PressureLevel)
        self.name = name or f"callback_{id(callback)}"
        self.invocation_count = 0
        self.last_invoked: Optional[float] = None
    
    def should_trigger(self, new_level: PressureLevel) -> bool:
        return new_level in self.trigger_levels
    
    def invoke(self, old_level: PressureLevel, new_level: PressureLevel):
        try:
            self.callback(old_level, new_level)
            self.invocation_count += 1
            self.last_invoked = time.time()
        except Exception as e:
            logger.error(f"Pressure callback {self.name} failed: {e}")


class PressureMonitor:
    """
    Memory Pressure Monitor for AGI-OS
    
    Continuously monitors system memory and provides pressure
    notifications for proactive memory management.
    """
    
    def __init__(
        self,
        thresholds: Optional[PressureThresholds] = None,
        poll_interval_s: float = 1.0,
        history_size: int = 60
    ):
        self._thresholds = thresholds or PressureThresholds()
        self._poll_interval = poll_interval_s
        self._history_size = history_size
        
        self._callbacks: Dict[str, PressureCallback] = {}
        self._history: deque = deque(maxlen=history_size)
        
        self._current_level = PressureLevel.NORMAL
        self._lock = threading.RLock()
        
        # Monitoring state
        self._monitoring = False
        self._monitor_thread: Optional[threading.Thread] = None
        
        # Statistics
        self._stats = {
            "samples": 0,
            "level_changes": 0,
            "gc_triggered": 0,
            "time_in_level": {level: 0.0 for level in PressureLevel}
        }
        self._level_start_time = time.time()
        
        logger.info("PressureMonitor initialized")
    
    def start_monitoring(self):
        """Start the background monitoring thread"""
        with self._lock:
            if self._monitoring:
                return
            
            self._monitoring = True
            self._monitor_thread = threading.Thread(
                target=self._monitor_loop,
                name="PressureMonitor",
                daemon=True
            )
            self._monitor_thread.start()
            logger.info("Pressure monitoring started")
    
    def stop_monitoring(self):
        """Stop the background monitoring thread"""
        with self._lock:
            self._monitoring = False
            if self._monitor_thread:
                self._monitor_thread.join(timeout=2.0)
                self._monitor_thread = None
            logger.info("Pressure monitoring stopped")
    
    def _monitor_loop(self):
        """Background monitoring loop"""
        while self._monitoring:
            try:
                snapshot = self.take_snapshot()
                self._process_snapshot(snapshot)
            except Exception as e:
                logger.error(f"Monitor loop error: {e}")
            
            time.sleep(self._poll_interval)
    
    def take_snapshot(self) -> MemorySnapshot:
        """Take a current memory snapshot"""
        memory_info = self._get_memory_info()
        
        # Calculate pressure level
        usage_percent = (memory_info["used"] / memory_info["total"] * 100 
                        if memory_info["total"] > 0 else 0)
        pressure_level = self._calculate_pressure_level(usage_percent)
        
        snapshot = MemorySnapshot(
            timestamp=time.time(),
            total_mb=memory_info["total"],
            available_mb=memory_info["available"],
            used_mb=memory_info["used"],
            cached_mb=memory_info["cached"],
            buffers_mb=memory_info["buffers"],
            swap_used_mb=memory_info["swap_used"],
            swap_total_mb=memory_info["swap_total"],
            pressure_level=pressure_level
        )
        
        return snapshot
    
    def _get_memory_info(self) -> Dict[str, float]:
        """Get current memory information from the system"""
        # Try to read from /proc/meminfo on Linux
        try:
            if os.path.exists("/proc/meminfo"):
                return self._parse_proc_meminfo()
        except:
            pass
        
        # Try psutil if available
        try:
            import psutil
            mem = psutil.virtual_memory()
            swap = psutil.swap_memory()
            return {
                "total": mem.total / (1024 * 1024),
                "available": mem.available / (1024 * 1024),
                "used": mem.used / (1024 * 1024),
                "cached": getattr(mem, 'cached', 0) / (1024 * 1024),
                "buffers": getattr(mem, 'buffers', 0) / (1024 * 1024),
                "swap_used": swap.used / (1024 * 1024),
                "swap_total": swap.total / (1024 * 1024)
            }
        except ImportError:
            pass
        
        # Fallback: simulate reasonable values
        return {
            "total": 16384,  # 16GB
            "available": 8192,
            "used": 8192,
            "cached": 2048,
            "buffers": 512,
            "swap_used": 0,
            "swap_total": 4096
        }
    
    def _parse_proc_meminfo(self) -> Dict[str, float]:
        """Parse /proc/meminfo on Linux"""
        info = {}
        with open("/proc/meminfo", "r") as f:
            for line in f:
                parts = line.split()
                if len(parts) >= 2:
                    key = parts[0].rstrip(":")
                    value = int(parts[1]) / 1024  # KB to MB
                    info[key] = value
        
        total = info.get("MemTotal", 0)
        available = info.get("MemAvailable", 0)
        free = info.get("MemFree", 0)
        buffers = info.get("Buffers", 0)
        cached = info.get("Cached", 0)
        swap_total = info.get("SwapTotal", 0)
        swap_free = info.get("SwapFree", 0)
        
        # Calculate used (excluding buffers/cache)
        used = total - available if available else total - free - buffers - cached
        
        return {
            "total": total,
            "available": available if available else free + buffers + cached,
            "used": used,
            "cached": cached,
            "buffers": buffers,
            "swap_used": swap_total - swap_free,
            "swap_total": swap_total
        }
    
    def _calculate_pressure_level(
        self,
        usage_percent: float
    ) -> PressureLevel:
        """Calculate pressure level from usage percentage"""
        t = self._thresholds
        
        if usage_percent < t.normal_max:
            return PressureLevel.NORMAL
        elif usage_percent < t.moderate_max:
            return PressureLevel.MODERATE
        elif usage_percent < t.high_max:
            return PressureLevel.HIGH
        elif usage_percent < t.critical_max:
            return PressureLevel.CRITICAL
        else:
            return PressureLevel.OOM_IMMINENT
    
    def _process_snapshot(self, snapshot: MemorySnapshot):
        """Process a memory snapshot"""
        with self._lock:
            # Record in history
            self._history.append(snapshot)
            self._stats["samples"] += 1
            
            old_level = self._current_level
            new_level = snapshot.pressure_level
            
            # Apply hysteresis
            if self._should_change_level(old_level, new_level, snapshot):
                self._update_level(old_level, new_level)
                self._notify_callbacks(old_level, new_level)
    
    def _should_change_level(
        self,
        old_level: PressureLevel,
        new_level: PressureLevel,
        snapshot: MemorySnapshot
    ) -> bool:
        """Determine if we should change level (with hysteresis)"""
        if old_level == new_level:
            return False
        
        # Always escalate immediately
        if new_level.value > old_level.value:
            return True
        
        # De-escalation requires hysteresis
        h = self._thresholds.hysteresis
        t = self._thresholds
        usage = snapshot.usage_percent
        
        if old_level == PressureLevel.MODERATE and usage < t.normal_max - h:
            return True
        if old_level == PressureLevel.HIGH and usage < t.moderate_max - h:
            return True
        if old_level == PressureLevel.CRITICAL and usage < t.high_max - h:
            return True
        if old_level == PressureLevel.OOM_IMMINENT and usage < t.critical_max - h:
            return True
        
        return False
    
    def _update_level(self, old_level: PressureLevel, new_level: PressureLevel):
        """Update the current pressure level"""
        # Record time in old level
        time_in_level = time.time() - self._level_start_time
        self._stats["time_in_level"][old_level] += time_in_level
        
        self._current_level = new_level
        self._level_start_time = time.time()
        self._stats["level_changes"] += 1
        
        logger.info(f"Pressure level changed: {old_level.name} -> {new_level.name}")
        
        # Trigger GC if escalating to HIGH or above
        if new_level.value >= PressureLevel.HIGH.value and new_level.value > old_level.value:
            self._trigger_gc(new_level)
    
    def _trigger_gc(self, level: PressureLevel):
        """Trigger garbage collection based on pressure level"""
        self._stats["gc_triggered"] += 1
        
        if level == PressureLevel.HIGH:
            gc.collect(0)  # Gen 0 only
        elif level == PressureLevel.CRITICAL:
            gc.collect(1)  # Gen 0 and 1
        else:  # OOM_IMMINENT
            gc.collect()   # Full collection
        
        logger.info(f"Triggered GC at pressure level {level.name}")
    
    def _notify_callbacks(
        self,
        old_level: PressureLevel,
        new_level: PressureLevel
    ):
        """Notify registered callbacks of pressure change"""
        for callback in self._callbacks.values():
            if callback.should_trigger(new_level):
                callback.invoke(old_level, new_level)
    
    def register_callback(
        self,
        callback: Callable[[PressureLevel, PressureLevel], None],
        trigger_levels: Optional[List[PressureLevel]] = None,
        name: str = ""
    ) -> str:
        """
        Register a callback for pressure level changes.
        
        Args:
            callback: Function(old_level, new_level) to call
            trigger_levels: Levels that trigger the callback
            name: Callback name
            
        Returns:
            Callback ID
        """
        with self._lock:
            pc = PressureCallback(callback, trigger_levels, name)
            self._callbacks[pc.name] = pc
            return pc.name
    
    def unregister_callback(self, name: str) -> bool:
        """Unregister a callback"""
        with self._lock:
            return self._callbacks.pop(name, None) is not None
    
    def get_current_level(self) -> PressureLevel:
        """Get the current pressure level"""
        with self._lock:
            return self._current_level
    
    def get_throttle_recommendation(self) -> ThrottleRecommendation:
        """
        Get throttling recommendation based on current pressure.
        
        Returns:
            ThrottleRecommendation with actionable guidance
        """
        level = self.get_current_level()
        
        if level == PressureLevel.NORMAL:
            return ThrottleRecommendation(
                level=level,
                should_throttle=False,
                throttle_factor=1.0,
                gc_recommended=False,
                eviction_recommended=False,
                message="Memory usage normal, no throttling needed"
            )
        
        if level == PressureLevel.MODERATE:
            return ThrottleRecommendation(
                level=level,
                should_throttle=False,
                throttle_factor=0.9,
                gc_recommended=True,
                eviction_recommended=False,
                message="Moderate memory pressure, consider optimizations"
            )
        
        if level == PressureLevel.HIGH:
            return ThrottleRecommendation(
                level=level,
                should_throttle=True,
                throttle_factor=0.5,
                gc_recommended=True,
                eviction_recommended=True,
                message="High memory pressure, throttle allocations"
            )
        
        if level == PressureLevel.CRITICAL:
            return ThrottleRecommendation(
                level=level,
                should_throttle=True,
                throttle_factor=0.2,
                gc_recommended=True,
                eviction_recommended=True,
                message="Critical memory pressure, aggressive throttling"
            )
        
        # OOM_IMMINENT
        return ThrottleRecommendation(
            level=level,
            should_throttle=True,
            throttle_factor=0.0,
            gc_recommended=True,
            eviction_recommended=True,
            message="OOM imminent, stop all allocations!"
        )
    
    def get_history(self, count: Optional[int] = None) -> List[Dict[str, Any]]:
        """Get recent memory snapshots"""
        with self._lock:
            history = list(self._history)
            if count:
                history = history[-count:]
            
            return [
                {
                    "timestamp": s.timestamp,
                    "total_mb": s.total_mb,
                    "used_mb": s.used_mb,
                    "available_mb": s.available_mb,
                    "usage_percent": s.usage_percent,
                    "pressure_level": s.pressure_level.name
                }
                for s in history
            ]
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get monitoring statistics"""
        with self._lock:
            # Update time in current level
            current_time = time.time() - self._level_start_time
            time_stats = dict(self._stats["time_in_level"])
            time_stats[self._current_level] += current_time
            
            return {
                "current_level": self._current_level.name,
                "samples": self._stats["samples"],
                "level_changes": self._stats["level_changes"],
                "gc_triggered": self._stats["gc_triggered"],
                "callbacks_registered": len(self._callbacks),
                "time_in_level": {k.name: v for k, v in time_stats.items()},
                "monitoring_active": self._monitoring
            }
    
    def force_snapshot(self) -> MemorySnapshot:
        """Force a snapshot and process it"""
        snapshot = self.take_snapshot()
        self._process_snapshot(snapshot)
        return snapshot


# Global pressure monitor instance
_pressure_monitor: Optional[PressureMonitor] = None


def get_pressure_monitor() -> PressureMonitor:
    """Get the global pressure monitor instance"""
    global _pressure_monitor
    if _pressure_monitor is None:
        _pressure_monitor = PressureMonitor()
    return _pressure_monitor


if __name__ == "__main__":
    import json
    
    print("=== Memory Pressure Monitor Example ===\n")
    
    monitor = get_pressure_monitor()
    
    # Register a callback
    def on_pressure_change(old_level, new_level):
        print(f"[CALLBACK] Pressure changed: {old_level.name} -> {new_level.name}")
    
    monitor.register_callback(
        on_pressure_change,
        trigger_levels=[PressureLevel.HIGH, PressureLevel.CRITICAL, PressureLevel.OOM_IMMINENT],
        name="alert_callback"
    )
    
    # Take a snapshot
    snapshot = monitor.force_snapshot()
    print(f"=== Current Memory State ===")
    print(f"  Total: {snapshot.total_mb:.0f} MB")
    print(f"  Used: {snapshot.used_mb:.0f} MB ({snapshot.usage_percent:.1f}%)")
    print(f"  Available: {snapshot.available_mb:.0f} MB")
    print(f"  Pressure Level: {snapshot.pressure_level.name}")
    
    # Get throttle recommendation
    rec = monitor.get_throttle_recommendation()
    print(f"\n=== Throttle Recommendation ===")
    print(f"  Level: {rec.level.name}")
    print(f"  Should Throttle: {rec.should_throttle}")
    print(f"  Throttle Factor: {rec.throttle_factor}")
    print(f"  GC Recommended: {rec.gc_recommended}")
    print(f"  Message: {rec.message}")
    
    # Start monitoring in background
    print("\n=== Starting Background Monitoring ===")
    monitor.start_monitoring()
    
    # Wait for some samples
    time.sleep(3)
    
    # Get statistics
    print("\n=== Statistics ===")
    print(json.dumps(monitor.get_statistics(), indent=2))
    
    # Get history
    print("\n=== Recent History ===")
    for h in monitor.get_history(count=3):
        print(f"  {h['timestamp']:.0f}: {h['usage_percent']:.1f}% - {h['pressure_level']}")
    
    # Stop monitoring
    monitor.stop_monitoring()
    print("\nMonitoring stopped.")
