#!/usr/bin/env python3
"""
NUMA-Aware Memory Allocation for AGI-OS

This module provides NUMA (Non-Uniform Memory Access) aware memory allocation
for optimizing cognitive workloads across multi-socket systems. It enables
cognitive components to allocate memory close to their execution context.

Features:
- NUMA topology detection
- Node-affinity allocation
- Interleaved allocation for shared data
- Memory migration between nodes
- AtomSpace NUMA optimization
"""

import time
import logging
import threading
import os
from typing import Dict, Any, Optional, List, Set
from dataclasses import dataclass, field
from enum import Enum

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("AGI_Memory.NUMA")


class NUMAPolicy(Enum):
    """Memory allocation policy for NUMA systems"""
    DEFAULT = "default"           # System default
    BIND = "bind"                 # Bind to specific node(s)
    INTERLEAVE = "interleave"     # Round-robin across nodes
    PREFERRED = "preferred"       # Prefer specific node, fall back to others
    LOCAL = "local"               # Local node only
    COGNITIVE = "cognitive"       # Attention-based NUMA placement


@dataclass
class NUMANode:
    """
    Represents a NUMA node in the system.
    
    A NUMA node is a set of CPUs with local memory access.
    """
    node_id: int
    cpus: Set[int]                  # CPUs in this node
    total_memory_mb: int            # Total memory
    free_memory_mb: int             # Free memory
    distance: Dict[int, int] = field(default_factory=dict)  # Distance to other nodes
    
    # Usage tracking
    allocated_mb: int = 0
    allocations: int = 0
    
    def update_free_memory(self):
        """Update free memory from system (simulated)"""
        self.free_memory_mb = self.total_memory_mb - self.allocated_mb
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "node_id": self.node_id,
            "cpus": list(self.cpus),
            "total_memory_mb": self.total_memory_mb,
            "free_memory_mb": self.free_memory_mb,
            "allocated_mb": self.allocated_mb,
            "allocations": self.allocations
        }


@dataclass
class NUMAAllocation:
    """
    Tracks a NUMA-aware allocation.
    """
    allocation_id: str
    node_id: int
    size_mb: float
    policy: NUMAPolicy
    owner: str
    created_at: float = field(default_factory=time.time)
    
    # Cognitive attributes
    attention_weight: float = 0.0
    last_access: float = field(default_factory=time.time)
    access_count: int = 0
    
    def touch(self):
        self.last_access = time.time()
        self.access_count += 1


class NUMAAllocator:
    """
    NUMA-Aware Memory Allocator for AGI-OS
    
    Provides intelligent memory allocation across NUMA nodes
    with cognitive workload optimization.
    """
    
    def __init__(self, detect_topology: bool = True):
        self._nodes: Dict[int, NUMANode] = {}
        self._allocations: Dict[str, NUMAAllocation] = {}
        self._lock = threading.RLock()
        
        # Statistics
        self._stats = {
            "allocations": 0,
            "migrations": 0,
            "total_allocated_mb": 0
        }
        
        if detect_topology:
            self._detect_numa_topology()
        else:
            # Create a simulated single-node topology
            self._create_simulated_topology()
        
        logger.info(f"NUMAAllocator initialized with {len(self._nodes)} nodes")
    
    def _detect_numa_topology(self):
        """Detect NUMA topology from the system"""
        # Try to read from /sys/devices/system/node on Linux
        numa_path = "/sys/devices/system/node"
        
        if os.path.exists(numa_path):
            try:
                for entry in os.listdir(numa_path):
                    if entry.startswith("node"):
                        node_id = int(entry[4:])
                        self._detect_node(node_id, numa_path)
                return
            except Exception as e:
                logger.warning(f"Could not detect NUMA topology: {e}")
        
        # Fall back to simulated topology
        self._create_simulated_topology()
    
    def _detect_node(self, node_id: int, numa_path: str):
        """Detect information about a specific NUMA node"""
        node_path = os.path.join(numa_path, f"node{node_id}")
        
        # Get CPUs
        cpus = set()
        try:
            cpu_list_path = os.path.join(node_path, "cpulist")
            if os.path.exists(cpu_list_path):
                with open(cpu_list_path, 'r') as f:
                    cpu_str = f.read().strip()
                    cpus = self._parse_cpu_list(cpu_str)
        except:
            pass
        
        # Get memory info
        total_memory = 0
        free_memory = 0
        try:
            meminfo_path = os.path.join(node_path, "meminfo")
            if os.path.exists(meminfo_path):
                with open(meminfo_path, 'r') as f:
                    for line in f:
                        if "MemTotal:" in line:
                            total_memory = int(line.split()[3]) // 1024  # KB to MB
                        elif "MemFree:" in line:
                            free_memory = int(line.split()[3]) // 1024
        except:
            total_memory = 16384  # Default 16GB
            free_memory = 8192
        
        # Get distance to other nodes
        distance = {}
        try:
            distance_path = os.path.join(node_path, "distance")
            if os.path.exists(distance_path):
                with open(distance_path, 'r') as f:
                    distances = list(map(int, f.read().split()))
                    for i, d in enumerate(distances):
                        distance[i] = d
        except:
            distance[node_id] = 10  # Local
        
        node = NUMANode(
            node_id=node_id,
            cpus=cpus,
            total_memory_mb=total_memory,
            free_memory_mb=free_memory,
            distance=distance
        )
        
        self._nodes[node_id] = node
    
    def _parse_cpu_list(self, cpu_str: str) -> Set[int]:
        """Parse CPU list string (e.g., '0-3,8-11') into set of CPU IDs"""
        cpus = set()
        if not cpu_str:
            return cpus
        
        for part in cpu_str.split(','):
            if '-' in part:
                start, end = map(int, part.split('-'))
                cpus.update(range(start, end + 1))
            else:
                cpus.add(int(part))
        
        return cpus
    
    def _create_simulated_topology(self):
        """Create a simulated NUMA topology for non-NUMA systems"""
        # Simulate a 2-node topology
        import multiprocessing
        num_cpus = multiprocessing.cpu_count()
        half_cpus = num_cpus // 2
        
        self._nodes[0] = NUMANode(
            node_id=0,
            cpus=set(range(half_cpus)),
            total_memory_mb=8192,
            free_memory_mb=6144,
            distance={0: 10, 1: 20}
        )
        
        self._nodes[1] = NUMANode(
            node_id=1,
            cpus=set(range(half_cpus, num_cpus)),
            total_memory_mb=8192,
            free_memory_mb=6144,
            distance={0: 20, 1: 10}
        )
        
        logger.info("Created simulated NUMA topology")
    
    def allocate(
        self,
        size_mb: float,
        owner: str,
        policy: NUMAPolicy = NUMAPolicy.DEFAULT,
        preferred_node: Optional[int] = None,
        attention_weight: float = 0.0
    ) -> NUMAAllocation:
        """
        Allocate memory with NUMA awareness.
        
        Args:
            size_mb: Size in megabytes
            owner: Owner identifier
            policy: NUMA policy
            preferred_node: Preferred node (for PREFERRED/BIND policy)
            attention_weight: Cognitive importance
            
        Returns:
            NUMAAllocation object
        """
        with self._lock:
            # Select node based on policy
            node_id = self._select_node(size_mb, policy, preferred_node, attention_weight)
            
            if node_id is None:
                raise MemoryError("No suitable NUMA node available")
            
            node = self._nodes[node_id]
            
            # Check available memory
            if size_mb > node.free_memory_mb:
                # Try to find another node
                for nid, n in self._nodes.items():
                    if n.free_memory_mb >= size_mb:
                        node = n
                        node_id = nid
                        break
                else:
                    raise MemoryError(f"Insufficient memory on any NUMA node")
            
            # Create allocation
            import secrets
            allocation_id = f"numa_alloc_{secrets.token_hex(6)}"
            
            allocation = NUMAAllocation(
                allocation_id=allocation_id,
                node_id=node_id,
                size_mb=size_mb,
                policy=policy,
                owner=owner,
                attention_weight=attention_weight
            )
            
            # Update node state
            node.allocated_mb += size_mb
            node.allocations += 1
            node.update_free_memory()
            
            self._allocations[allocation_id] = allocation
            
            self._stats["allocations"] += 1
            self._stats["total_allocated_mb"] += size_mb
            
            logger.debug(f"Allocated {size_mb}MB on node {node_id} for {owner}")
            return allocation
    
    def _select_node(
        self,
        size_mb: float,
        policy: NUMAPolicy,
        preferred_node: Optional[int],
        attention_weight: float
    ) -> Optional[int]:
        """Select the best NUMA node based on policy"""
        
        if policy == NUMAPolicy.BIND:
            if preferred_node is not None and preferred_node in self._nodes:
                return preferred_node
            return None
        
        if policy == NUMAPolicy.PREFERRED:
            if preferred_node is not None and preferred_node in self._nodes:
                node = self._nodes[preferred_node]
                if node.free_memory_mb >= size_mb:
                    return preferred_node
            # Fall through to DEFAULT
        
        if policy == NUMAPolicy.LOCAL:
            # Return node with most free memory (simulated "local")
            return max(self._nodes.keys(), 
                      key=lambda n: self._nodes[n].free_memory_mb)
        
        if policy == NUMAPolicy.INTERLEAVE:
            # Round-robin selection
            total_allocs = sum(n.allocations for n in self._nodes.values())
            return total_allocs % len(self._nodes)
        
        if policy == NUMAPolicy.COGNITIVE:
            # Select based on attention patterns
            # Prefer nodes with high-attention allocations nearby
            return self._cognitive_node_selection(size_mb, attention_weight)
        
        # DEFAULT: Select node with most free memory
        suitable = [
            (nid, node) for nid, node in self._nodes.items()
            if node.free_memory_mb >= size_mb
        ]
        
        if not suitable:
            return None
        
        return max(suitable, key=lambda x: x[1].free_memory_mb)[0]
    
    def _cognitive_node_selection(
        self,
        size_mb: float,
        attention_weight: float
    ) -> Optional[int]:
        """
        Select node based on cognitive patterns.
        
        High-attention data should be co-located for cache efficiency.
        """
        # Score each node based on attention affinity
        node_scores = {}
        
        for node_id, node in self._nodes.items():
            if node.free_memory_mb < size_mb:
                continue
            
            score = node.free_memory_mb / node.total_memory_mb  # Base: free ratio
            
            # Add affinity score based on existing allocations
            node_allocs = [
                a for a in self._allocations.values()
                if a.node_id == node_id
            ]
            
            if attention_weight > 0.5:
                # High-attention: prefer nodes with other high-attention data
                high_attention = sum(
                    1 for a in node_allocs if a.attention_weight > 0.5
                )
                score += high_attention * 0.1
            else:
                # Low-attention: prefer nodes with free space
                score += 0.1 * (1 - len(node_allocs) / max(len(self._allocations), 1))
            
            node_scores[node_id] = score
        
        if not node_scores:
            return None
        
        return max(node_scores, key=node_scores.get)
    
    def free(self, allocation_id: str) -> bool:
        """
        Free a NUMA allocation.
        
        Args:
            allocation_id: Allocation to free
            
        Returns:
            True if freed
        """
        with self._lock:
            allocation = self._allocations.pop(allocation_id, None)
            if not allocation:
                return False
            
            node = self._nodes.get(allocation.node_id)
            if node:
                node.allocated_mb -= allocation.size_mb
                node.update_free_memory()
            
            self._stats["total_allocated_mb"] -= allocation.size_mb
            
            logger.debug(f"Freed allocation {allocation_id}")
            return True
    
    def migrate(self, allocation_id: str, target_node: int) -> bool:
        """
        Migrate an allocation to a different NUMA node.
        
        Args:
            allocation_id: Allocation to migrate
            target_node: Target NUMA node
            
        Returns:
            True if migrated
        """
        with self._lock:
            allocation = self._allocations.get(allocation_id)
            if not allocation:
                return False
            
            if target_node not in self._nodes:
                return False
            
            target = self._nodes[target_node]
            if target.free_memory_mb < allocation.size_mb:
                return False
            
            # Update source node
            source = self._nodes.get(allocation.node_id)
            if source:
                source.allocated_mb -= allocation.size_mb
                source.update_free_memory()
            
            # Update target node
            target.allocated_mb += allocation.size_mb
            target.update_free_memory()
            
            # Update allocation
            old_node = allocation.node_id
            allocation.node_id = target_node
            
            self._stats["migrations"] += 1
            
            logger.info(f"Migrated {allocation_id} from node {old_node} to {target_node}")
            return True
    
    def get_node(self, node_id: int) -> Optional[NUMANode]:
        """Get NUMA node info"""
        return self._nodes.get(node_id)
    
    def list_nodes(self) -> List[Dict[str, Any]]:
        """List all NUMA nodes"""
        return [node.to_dict() for node in self._nodes.values()]
    
    def list_allocations(self, node_id: Optional[int] = None) -> List[Dict[str, Any]]:
        """List allocations, optionally filtered by node"""
        with self._lock:
            allocs = self._allocations.values()
            if node_id is not None:
                allocs = [a for a in allocs if a.node_id == node_id]
            
            return [
                {
                    "allocation_id": a.allocation_id,
                    "node_id": a.node_id,
                    "size_mb": a.size_mb,
                    "policy": a.policy.value,
                    "owner": a.owner,
                    "attention_weight": a.attention_weight,
                    "access_count": a.access_count
                }
                for a in allocs
            ]
    
    def get_node_for_cpu(self, cpu_id: int) -> Optional[int]:
        """Get the NUMA node for a CPU"""
        for node_id, node in self._nodes.items():
            if cpu_id in node.cpus:
                return node_id
        return None
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get allocator statistics"""
        with self._lock:
            return {
                **self._stats,
                "nodes_count": len(self._nodes),
                "allocations_count": len(self._allocations),
                "node_utilization": {
                    nid: node.allocated_mb / node.total_memory_mb
                    for nid, node in self._nodes.items()
                }
            }


# Global NUMA allocator instance
_numa_allocator: Optional[NUMAAllocator] = None


def get_numa_allocator() -> NUMAAllocator:
    """Get the global NUMA allocator instance"""
    global _numa_allocator
    if _numa_allocator is None:
        _numa_allocator = NUMAAllocator()
    return _numa_allocator


if __name__ == "__main__":
    import json
    
    allocator = get_numa_allocator()
    
    print("=== NUMA Allocator Example ===\n")
    
    print("=== NUMA Topology ===")
    for node in allocator.list_nodes():
        print(f"  Node {node['node_id']}: {node['total_memory_mb']}MB total, "
              f"{node['free_memory_mb']}MB free, CPUs: {node['cpus']}")
    
    print("\n=== Allocations ===")
    
    # Allocate on default node
    alloc1 = allocator.allocate(512, "pln_engine", NUMAPolicy.DEFAULT)
    print(f"Default allocation: {alloc1.allocation_id} on node {alloc1.node_id}")
    
    # Allocate with cognitive policy
    alloc2 = allocator.allocate(
        256, "attention_bank", 
        NUMAPolicy.COGNITIVE,
        attention_weight=0.9
    )
    print(f"Cognitive allocation: {alloc2.allocation_id} on node {alloc2.node_id}")
    
    # Interleaved allocation
    alloc3 = allocator.allocate(128, "pattern_miner", NUMAPolicy.INTERLEAVE)
    print(f"Interleaved allocation: {alloc3.allocation_id} on node {alloc3.node_id}")
    
    print("\n=== Current Allocations ===")
    for alloc in allocator.list_allocations():
        print(f"  {alloc['allocation_id']}: {alloc['size_mb']}MB on node {alloc['node_id']} "
              f"({alloc['owner']}, attention={alloc['attention_weight']})")
    
    print("\n=== Statistics ===")
    print(json.dumps(allocator.get_statistics(), indent=2))
