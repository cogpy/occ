#!/usr/bin/env python3
"""
Module Sandboxing for AGI-OS

This module provides sandboxing capabilities for isolating cognitive modules
and enforcing resource limits. It enables safe execution of untrusted or
third-party cognitive components.

Features:
- Resource limit enforcement (CPU, memory, I/O)
- Isolated execution environments
- Capability-controlled access
- Monitoring and metrics
- Graceful degradation
"""

import time
import logging
import threading
import resource
import signal
import os
import sys
from typing import Dict, Any, Optional, Callable, Set, List
from dataclasses import dataclass, field
from enum import Enum
from contextlib import contextmanager
import multiprocessing
from queue import Queue, Empty
import traceback

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("AGI_Security.Sandbox")


@dataclass
class ResourceLimits:
    """Resource limits for sandboxed execution"""
    # CPU limits
    max_cpu_time: float = 60.0          # Max CPU seconds
    max_wall_time: float = 120.0        # Max wall clock seconds
    cpu_priority: int = 19              # Nice value (higher = lower priority)
    
    # Memory limits
    max_memory_mb: int = 512            # Max memory in MB
    max_stack_mb: int = 8               # Max stack size in MB
    
    # I/O limits
    max_file_size_mb: int = 100         # Max file size in MB
    max_open_files: int = 64            # Max open file descriptors
    allow_network: bool = False         # Allow network access
    allow_filesystem: bool = True       # Allow file system access
    allowed_paths: Set[str] = field(default_factory=set)  # Allowed paths
    
    # Process limits
    max_processes: int = 4              # Max child processes
    max_threads: int = 8                # Max threads
    
    # Capability limits
    allowed_capabilities: Set[str] = field(default_factory=set)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "max_cpu_time": self.max_cpu_time,
            "max_wall_time": self.max_wall_time,
            "cpu_priority": self.cpu_priority,
            "max_memory_mb": self.max_memory_mb,
            "max_stack_mb": self.max_stack_mb,
            "max_file_size_mb": self.max_file_size_mb,
            "max_open_files": self.max_open_files,
            "allow_network": self.allow_network,
            "allow_filesystem": self.allow_filesystem,
            "allowed_paths": list(self.allowed_paths),
            "max_processes": self.max_processes,
            "max_threads": self.max_threads,
            "allowed_capabilities": list(self.allowed_capabilities)
        }


class SandboxState(Enum):
    """State of a sandbox"""
    CREATED = "created"
    RUNNING = "running"
    COMPLETED = "completed"
    TIMEOUT = "timeout"
    ERROR = "error"
    TERMINATED = "terminated"


@dataclass
class SandboxConfig:
    """Configuration for a sandbox"""
    sandbox_id: str
    name: str
    limits: ResourceLimits = field(default_factory=ResourceLimits)
    enable_monitoring: bool = True
    monitoring_interval: float = 1.0    # Seconds between monitoring checks
    auto_terminate_on_limit: bool = True
    capture_output: bool = True
    isolate_globals: bool = True        # Isolate Python globals


@dataclass
class SandboxMetrics:
    """Metrics from sandboxed execution"""
    start_time: float = 0.0
    end_time: float = 0.0
    cpu_time: float = 0.0
    wall_time: float = 0.0
    peak_memory_mb: float = 0.0
    files_accessed: int = 0
    network_bytes: int = 0
    state: SandboxState = SandboxState.CREATED
    error_message: Optional[str] = None
    output: str = ""
    return_value: Any = None
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "start_time": self.start_time,
            "end_time": self.end_time,
            "cpu_time": self.cpu_time,
            "wall_time": self.wall_time,
            "peak_memory_mb": self.peak_memory_mb,
            "files_accessed": self.files_accessed,
            "network_bytes": self.network_bytes,
            "state": self.state.value,
            "error_message": self.error_message
        }


class SandboxedModule:
    """
    A sandboxed cognitive module.
    
    Wraps a module or function for execution within resource limits
    and capability constraints.
    """
    
    def __init__(self, config: SandboxConfig):
        self.config = config
        self.metrics = SandboxMetrics()
        self._lock = threading.Lock()
        self._process: Optional[multiprocessing.Process] = None
        self._result_queue: Optional[multiprocessing.Queue] = None
        self._monitor_thread: Optional[threading.Thread] = None
        self._running = False
        
        logger.info(f"Created sandbox: {config.sandbox_id}")
    
    def _apply_resource_limits(self):
        """Apply resource limits (called in child process)"""
        limits = self.config.limits
        
        try:
            # CPU time limit
            resource.setrlimit(
                resource.RLIMIT_CPU,
                (int(limits.max_cpu_time), int(limits.max_cpu_time) + 5)
            )
            
            # Memory limit
            memory_bytes = limits.max_memory_mb * 1024 * 1024
            resource.setrlimit(
                resource.RLIMIT_AS,
                (memory_bytes, memory_bytes)
            )
            
            # Stack size
            stack_bytes = limits.max_stack_mb * 1024 * 1024
            resource.setrlimit(
                resource.RLIMIT_STACK,
                (stack_bytes, stack_bytes)
            )
            
            # File size
            file_bytes = limits.max_file_size_mb * 1024 * 1024
            resource.setrlimit(
                resource.RLIMIT_FSIZE,
                (file_bytes, file_bytes)
            )
            
            # Open files
            resource.setrlimit(
                resource.RLIMIT_NOFILE,
                (limits.max_open_files, limits.max_open_files)
            )
            
            # Number of processes
            resource.setrlimit(
                resource.RLIMIT_NPROC,
                (limits.max_processes, limits.max_processes)
            )
            
            # Set nice value
            if limits.cpu_priority != 0:
                os.nice(limits.cpu_priority)
                
        except (ValueError, resource.error) as e:
            logger.warning(f"Could not set resource limit: {e}")
    
    def _sandbox_worker(
        self,
        func: Callable,
        args: tuple,
        kwargs: dict,
        result_queue: multiprocessing.Queue
    ):
        """Worker function that runs in sandboxed process"""
        try:
            # Apply resource limits
            self._apply_resource_limits()
            
            # Execute the function
            start_cpu = time.process_time()
            result = func(*args, **kwargs)
            end_cpu = time.process_time()
            
            result_queue.put({
                "success": True,
                "result": result,
                "cpu_time": end_cpu - start_cpu
            })
            
        except MemoryError:
            result_queue.put({
                "success": False,
                "error": "MemoryError: Exceeded memory limit",
                "error_type": "memory"
            })
        except Exception as e:
            result_queue.put({
                "success": False,
                "error": str(e),
                "traceback": traceback.format_exc(),
                "error_type": "exception"
            })
    
    def _monitor_execution(self, process: multiprocessing.Process, start_time: float):
        """Monitor sandboxed execution for limit violations"""
        limits = self.config.limits
        
        while self._running and process.is_alive():
            wall_time = time.time() - start_time
            
            # Check wall time limit
            if wall_time > limits.max_wall_time:
                logger.warning(f"Sandbox {self.config.sandbox_id}: Wall time limit exceeded")
                self.metrics.state = SandboxState.TIMEOUT
                if self.config.auto_terminate_on_limit:
                    process.terminate()
                    break
            
            # Check memory usage (if we can access it)
            try:
                import psutil
                proc_info = psutil.Process(process.pid)
                memory_mb = proc_info.memory_info().rss / (1024 * 1024)
                self.metrics.peak_memory_mb = max(self.metrics.peak_memory_mb, memory_mb)
                
                if memory_mb > limits.max_memory_mb * 1.1:  # 10% grace
                    logger.warning(f"Sandbox {self.config.sandbox_id}: Memory limit exceeded")
                    self.metrics.state = SandboxState.ERROR
                    self.metrics.error_message = "Memory limit exceeded"
                    if self.config.auto_terminate_on_limit:
                        process.terminate()
                        break
            except ImportError:
                pass
            except Exception:
                pass
            
            time.sleep(self.config.monitoring_interval)
    
    def execute(
        self,
        func: Callable,
        args: tuple = (),
        kwargs: Optional[dict] = None,
        timeout: Optional[float] = None
    ) -> Any:
        """
        Execute a function within the sandbox.
        
        Args:
            func: Function to execute
            args: Positional arguments
            kwargs: Keyword arguments
            timeout: Override timeout (defaults to wall_time limit)
            
        Returns:
            Function result
            
        Raises:
            TimeoutError: If execution exceeds time limit
            MemoryError: If execution exceeds memory limit
            Exception: Any exception from the function
        """
        with self._lock:
            if self._running:
                raise RuntimeError("Sandbox is already executing")
            self._running = True
        
        kwargs = kwargs or {}
        timeout = timeout or self.config.limits.max_wall_time
        
        self.metrics = SandboxMetrics()
        self.metrics.start_time = time.time()
        self.metrics.state = SandboxState.RUNNING
        
        try:
            # Create result queue and process
            self._result_queue = multiprocessing.Queue()
            self._process = multiprocessing.Process(
                target=self._sandbox_worker,
                args=(func, args, kwargs, self._result_queue)
            )
            
            # Start monitoring
            if self.config.enable_monitoring:
                self._monitor_thread = threading.Thread(
                    target=self._monitor_execution,
                    args=(self._process, self.metrics.start_time),
                    daemon=True
                )
                self._monitor_thread.start()
            
            # Start execution
            self._process.start()
            self._process.join(timeout=timeout)
            
            self.metrics.end_time = time.time()
            self.metrics.wall_time = self.metrics.end_time - self.metrics.start_time
            
            # Check if process timed out
            if self._process.is_alive():
                self._process.terminate()
                self._process.join(timeout=5)
                self.metrics.state = SandboxState.TIMEOUT
                raise TimeoutError(f"Sandbox execution timed out after {timeout}s")
            
            # Get result
            try:
                result = self._result_queue.get_nowait()
                
                if result.get("success"):
                    self.metrics.state = SandboxState.COMPLETED
                    self.metrics.cpu_time = result.get("cpu_time", 0)
                    self.metrics.return_value = result.get("result")
                    return self.metrics.return_value
                else:
                    self.metrics.state = SandboxState.ERROR
                    self.metrics.error_message = result.get("error")
                    
                    if result.get("error_type") == "memory":
                        raise MemoryError(result.get("error"))
                    else:
                        raise RuntimeError(result.get("error"))
                        
            except Empty:
                self.metrics.state = SandboxState.ERROR
                self.metrics.error_message = "No result from sandbox"
                raise RuntimeError("Sandbox process terminated without result")
                
        finally:
            self._running = False
            if self._process and self._process.is_alive():
                self._process.terminate()
    
    def terminate(self):
        """Forcefully terminate sandbox execution"""
        with self._lock:
            if self._process and self._process.is_alive():
                self._process.terminate()
                self.metrics.state = SandboxState.TERMINATED
                logger.info(f"Sandbox {self.config.sandbox_id} terminated")
            self._running = False
    
    def get_metrics(self) -> Dict[str, Any]:
        """Get sandbox execution metrics"""
        return self.metrics.to_dict()


class Sandbox:
    """
    Sandbox factory and manager.
    
    Creates and manages sandboxed execution environments for
    cognitive modules.
    """
    
    _instances: Dict[str, SandboxedModule] = {}
    _lock = threading.Lock()
    
    @classmethod
    def create(
        cls,
        name: str,
        limits: Optional[ResourceLimits] = None,
        **config_kwargs
    ) -> SandboxedModule:
        """
        Create a new sandbox.
        
        Args:
            name: Name for the sandbox
            limits: Resource limits (defaults used if not provided)
            **config_kwargs: Additional SandboxConfig options
            
        Returns:
            SandboxedModule instance
        """
        import secrets
        sandbox_id = f"sandbox_{secrets.token_hex(4)}"
        
        config = SandboxConfig(
            sandbox_id=sandbox_id,
            name=name,
            limits=limits or ResourceLimits(),
            **config_kwargs
        )
        
        sandbox = SandboxedModule(config)
        
        with cls._lock:
            cls._instances[sandbox_id] = sandbox
        
        return sandbox
    
    @classmethod
    def get(cls, sandbox_id: str) -> Optional[SandboxedModule]:
        """Get sandbox by ID"""
        with cls._lock:
            return cls._instances.get(sandbox_id)
    
    @classmethod
    def destroy(cls, sandbox_id: str):
        """Destroy a sandbox"""
        with cls._lock:
            sandbox = cls._instances.pop(sandbox_id, None)
            if sandbox:
                sandbox.terminate()
    
    @classmethod
    def list_sandboxes(cls) -> List[Dict[str, Any]]:
        """List all sandboxes"""
        with cls._lock:
            return [
                {
                    "sandbox_id": s.config.sandbox_id,
                    "name": s.config.name,
                    "state": s.metrics.state.value
                }
                for s in cls._instances.values()
            ]
    
    @classmethod
    @contextmanager
    def execute_isolated(
        cls,
        name: str,
        limits: Optional[ResourceLimits] = None
    ):
        """
        Context manager for isolated execution.
        
        Usage:
            with Sandbox.execute_isolated("my_task", limits) as sandbox:
                result = sandbox.execute(my_function, args=(1, 2))
        """
        sandbox = cls.create(name, limits)
        try:
            yield sandbox
        finally:
            cls.destroy(sandbox.config.sandbox_id)


# Convenience function for quick sandboxed execution
def sandboxed_execute(
    func: Callable,
    args: tuple = (),
    kwargs: Optional[dict] = None,
    limits: Optional[ResourceLimits] = None,
    timeout: Optional[float] = None
) -> Any:
    """
    Execute a function in a temporary sandbox.
    
    Args:
        func: Function to execute
        args: Positional arguments
        kwargs: Keyword arguments
        limits: Resource limits
        timeout: Execution timeout
        
    Returns:
        Function result
    """
    with Sandbox.execute_isolated("temp_sandbox", limits) as sandbox:
        return sandbox.execute(func, args, kwargs, timeout)


if __name__ == "__main__":
    # Example usage
    import json
    
    print("=== Sandbox Example ===\n")
    
    def cpu_intensive(n: int) -> int:
        """A CPU-intensive function"""
        total = 0
        for i in range(n):
            total += sum(range(1000))
        return total
    
    def memory_hungry() -> List[int]:
        """A memory-hungry function"""
        data = []
        for i in range(10):
            data.extend(range(1000000))
        return len(data)
    
    # Create a sandbox with limits
    limits = ResourceLimits(
        max_cpu_time=5.0,
        max_wall_time=10.0,
        max_memory_mb=256
    )
    
    sandbox = Sandbox.create("test_sandbox", limits)
    
    print("Running CPU-intensive task...")
    try:
        result = sandbox.execute(cpu_intensive, args=(1000,), timeout=5.0)
        print(f"Result: {result}")
    except TimeoutError as e:
        print(f"Timeout: {e}")
    except Exception as e:
        print(f"Error: {e}")
    
    print("\nMetrics:")
    print(json.dumps(sandbox.get_metrics(), indent=2))
    
    print("\n=== Sandboxed List ===")
    print(Sandbox.list_sandboxes())
    
    Sandbox.destroy(sandbox.config.sandbox_id)
