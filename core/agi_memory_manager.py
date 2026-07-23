#!/usr/bin/env python3
"""
AGI Memory Manager for AGI-OS

This module provides the unified memory management interface for the
OpenCog AGI-OS cognitive architecture. It integrates virtual memory,
NUMA-aware allocation, and pressure monitoring into a cohesive system.

Features:
- Hierarchical memory tiers (L1/L2/L3 cache + main memory + swap)
- NUMA-aware allocation for multi-socket systems
- Memory pressure monitoring and throttling
- AtomSpace memory mapping
- Garbage collection coordination
- Cognitive-aware memory placement
- Memory migration for load balancing
"""

import time
import logging
import threading
import gc
from typing import Dict, Any, Optional, List, Tuple, Callable
from dataclasses import dataclass, field
from enum import Enum

# Import memory subsystems
from core.memory.virtual import (
    VirtualMemoryManager, get_virtual_memory_manager,
    MemoryRegion, MemoryFlags
)
from core.memory.numa import (
    NUMAAllocator, get_numa_allocator,
    NUMAPolicy, NUMAAllocation
)
from core.memory.pressure import (
    PressureMonitor, get_pressure_monitor,
    PressureLevel, ThrottleRecommendation
)

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("AGI_MemoryManager")


class MemoryTier(Enum):
    """Memory tier hierarchy for cognitive workloads"""
    L1_CACHE = 1      # Hot atoms, active attention focus
    L2_CACHE = 2      # Warm atoms, recent activity
    L3_CACHE = 3      # Cold atoms, archival
    MAIN_MEMORY = 4   # Standard storage
    SWAP = 5          # Disk-backed overflow


@dataclass
class TierConfig:
    """Configuration for a memory tier"""
    tier: MemoryTier
    max_size_mb: float
    eviction_threshold: float = 0.9   # Start eviction at 90% full
    promotion_threshold: float = 0.3  # Promote items accessed > 30% of average
    gc_threshold: float = 0.95        # GC at 95% full


@dataclass
class MappedAtomSpace:
    """A mapped AtomSpace region in memory"""
    mapping_id: str
    atomspace_id: str
    region: MemoryRegion
    numa_allocation: Optional[NUMAAllocation]
    tier: MemoryTier
    atom_count: int = 0
    created_at: float = field(default_factory=time.time)
    last_access: float = field(default_factory=time.time)
    access_count: int = 0
    
    def touch(self):
        self.last_access = time.time()
        self.access_count += 1


@dataclass
class MemoryAllocation:
    """
    Unified memory allocation tracking across all subsystems.
    """
    allocation_id: str
    owner: str
    size_mb: float
    tier: MemoryTier
    region: Optional[MemoryRegion] = None
    numa_allocation: Optional[NUMAAllocation] = None
    attention_weight: float = 0.0
    created_at: float = field(default_factory=time.time)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "allocation_id": self.allocation_id,
            "owner": self.owner,
            "size_mb": self.size_mb,
            "tier": self.tier.name,
            "attention_weight": self.attention_weight,
            "created_at": self.created_at
        }


class AGI_MemoryManager:
    """
    Unified Memory Manager for AGI-OS
    
    Provides a single interface for all memory operations in the
    cognitive architecture, coordinating virtual memory, NUMA
    allocation, and pressure monitoring.
    """
    
    def __init__(
        self,
        tier_configs: Optional[Dict[MemoryTier, TierConfig]] = None,
        enable_numa: bool = True,
        enable_pressure_monitoring: bool = True,
        gc_coordination: bool = True
    ):
        """
        Initialize the AGI Memory Manager.
        
        Args:
            tier_configs: Configuration for memory tiers
            enable_numa: Enable NUMA-aware allocation
            enable_pressure_monitoring: Enable background pressure monitoring
            gc_coordination: Coordinate with Python GC
        """
        self._lock = threading.RLock()
        
        # Initialize subsystems
        self._vm = get_virtual_memory_manager()
        self._numa = get_numa_allocator() if enable_numa else None
        self._pressure = get_pressure_monitor() if enable_pressure_monitoring else None
        
        # Configure tiers
        self._tier_configs = tier_configs or self._default_tier_configs()
        self._tier_usage: Dict[MemoryTier, float] = {t: 0.0 for t in MemoryTier}
        
        # Allocation tracking
        self._allocations: Dict[str, MemoryAllocation] = {}
        self._atomspace_mappings: Dict[str, MappedAtomSpace] = {}
        
        # GC coordination
        self._gc_coordination = gc_coordination
        if gc_coordination:
            self._setup_gc_coordination()
        
        # Statistics
        self._stats = {
            "allocations": 0,
            "deallocations": 0,
            "migrations": 0,
            "tier_promotions": 0,
            "tier_demotions": 0,
            "gc_cycles": 0,
            "total_allocated_mb": 0.0
        }
        
        # Start pressure monitoring if enabled
        if self._pressure:
            self._pressure.register_callback(
                self._on_pressure_change,
                name="memory_manager_callback"
            )
            self._pressure.start_monitoring()
        
        logger.info("AGI_MemoryManager initialized")
    
    def _default_tier_configs(self) -> Dict[MemoryTier, TierConfig]:
        """Create default tier configurations"""
        return {
            MemoryTier.L1_CACHE: TierConfig(
                tier=MemoryTier.L1_CACHE,
                max_size_mb=256,
                eviction_threshold=0.85,
                promotion_threshold=0.5
            ),
            MemoryTier.L2_CACHE: TierConfig(
                tier=MemoryTier.L2_CACHE,
                max_size_mb=1024,
                eviction_threshold=0.9,
                promotion_threshold=0.3
            ),
            MemoryTier.L3_CACHE: TierConfig(
                tier=MemoryTier.L3_CACHE,
                max_size_mb=4096,
                eviction_threshold=0.9,
                promotion_threshold=0.2
            ),
            MemoryTier.MAIN_MEMORY: TierConfig(
                tier=MemoryTier.MAIN_MEMORY,
                max_size_mb=16384,
                eviction_threshold=0.95,
                promotion_threshold=0.1
            ),
            MemoryTier.SWAP: TierConfig(
                tier=MemoryTier.SWAP,
                max_size_mb=32768,
                eviction_threshold=0.98,
                promotion_threshold=0.05
            )
        }
    
    def _setup_gc_coordination(self):
        """Set up coordination with Python's garbage collector"""
        # Set GC thresholds for better coordination
        gc.set_threshold(700, 10, 10)
        
        # Register callback for GC completion
        # Note: This is a simplified approach; production would use gc.callbacks
        logger.info("GC coordination enabled")
    
    def allocate(
        self,
        size_mb: float,
        owner: str,
        tier: MemoryTier = MemoryTier.MAIN_MEMORY,
        numa_policy: NUMAPolicy = NUMAPolicy.DEFAULT,
        attention_weight: float = 0.0,
        flags: Optional[MemoryFlags] = None
    ) -> MemoryAllocation:
        """
        Allocate memory with full AGI-OS integration.
        
        Args:
            size_mb: Size in megabytes
            owner: Owner identifier
            tier: Target memory tier
            numa_policy: NUMA allocation policy
            attention_weight: Cognitive importance (0.0-1.0)
            flags: Memory flags
            
        Returns:
            MemoryAllocation tracking object
            
        Raises:
            MemoryError: If allocation fails
        """
        with self._lock:
            # Check pressure first
            if self._pressure:
                rec = self._pressure.get_throttle_recommendation()
                if rec.throttle_factor == 0.0:
                    raise MemoryError("OOM imminent, allocation blocked")
                if rec.should_throttle and size_mb > 100:
                    # Scale down large allocations under pressure
                    logger.warning(f"Throttling allocation from {size_mb}MB to {size_mb * rec.throttle_factor}MB")
                    size_mb *= rec.throttle_factor
            
            # Check tier capacity
            tier_config = self._tier_configs[tier]
            current_usage = self._tier_usage[tier]
            
            if current_usage + size_mb > tier_config.max_size_mb:
                # Try to evict or demote
                if not self._make_room(tier, size_mb):
                    raise MemoryError(f"Insufficient capacity in tier {tier.name}")
            
            # Allocate virtual memory
            if flags is None:
                flags = MemoryFlags.READ | MemoryFlags.WRITE
            
            region = self._vm.allocate(
                size_mb=size_mb,
                owner=owner,
                flags=flags,
                attention_weight=attention_weight
            )
            
            # NUMA allocation if enabled
            numa_alloc = None
            if self._numa and tier != MemoryTier.SWAP:
                try:
                    numa_alloc = self._numa.allocate(
                        size_mb=size_mb,
                        owner=owner,
                        policy=numa_policy,
                        attention_weight=attention_weight
                    )
                except MemoryError:
                    logger.warning("NUMA allocation failed, proceeding without NUMA")
            
            # Create allocation record
            import secrets
            allocation_id = f"alloc_{tier.name.lower()}_{secrets.token_hex(6)}"
            
            allocation = MemoryAllocation(
                allocation_id=allocation_id,
                owner=owner,
                size_mb=size_mb,
                tier=tier,
                region=region,
                numa_allocation=numa_alloc,
                attention_weight=attention_weight
            )
            
            self._allocations[allocation_id] = allocation
            self._tier_usage[tier] += size_mb
            self._stats["allocations"] += 1
            self._stats["total_allocated_mb"] += size_mb
            
            logger.debug(f"Allocated {size_mb}MB in tier {tier.name} for {owner}")
            return allocation
    
    def free(self, allocation_id: str) -> bool:
        """
        Free a memory allocation.
        
        Args:
            allocation_id: Allocation to free
            
        Returns:
            True if freed successfully
        """
        with self._lock:
            allocation = self._allocations.pop(allocation_id, None)
            if not allocation:
                return False
            
            # Free virtual memory
            if allocation.region:
                self._vm.free(allocation.region.region_id)
            
            # Free NUMA allocation
            if allocation.numa_allocation:
                self._numa.free(allocation.numa_allocation.allocation_id)
            
            self._tier_usage[allocation.tier] -= allocation.size_mb
            self._stats["deallocations"] += 1
            self._stats["total_allocated_mb"] -= allocation.size_mb
            
            logger.debug(f"Freed allocation {allocation_id}")
            return True
    
    def _make_room(self, tier: MemoryTier, needed_mb: float) -> bool:
        """
        Try to make room in a tier by eviction or demotion.
        
        Returns:
            True if sufficient room was made
        """
        tier_allocs = [
            a for a in self._allocations.values()
            if a.tier == tier
        ]
        
        if not tier_allocs:
            return False
        
        # Sort by attention weight (evict lowest first)
        tier_allocs.sort(key=lambda a: a.attention_weight)
        
        freed = 0.0
        for alloc in tier_allocs:
            if freed >= needed_mb:
                break
            
            # Try to demote to lower tier
            lower_tier = self._get_lower_tier(tier)
            if lower_tier and self._migrate_tier(alloc.allocation_id, lower_tier):
                freed += alloc.size_mb
                self._stats["tier_demotions"] += 1
        
        return freed >= needed_mb
    
    def _get_lower_tier(self, tier: MemoryTier) -> Optional[MemoryTier]:
        """Get the next lower memory tier"""
        tier_order = [
            MemoryTier.L1_CACHE,
            MemoryTier.L2_CACHE,
            MemoryTier.L3_CACHE,
            MemoryTier.MAIN_MEMORY,
            MemoryTier.SWAP
        ]
        
        try:
            idx = tier_order.index(tier)
            if idx < len(tier_order) - 1:
                return tier_order[idx + 1]
        except ValueError:
            pass
        
        return None
    
    def _get_higher_tier(self, tier: MemoryTier) -> Optional[MemoryTier]:
        """Get the next higher memory tier"""
        tier_order = [
            MemoryTier.L1_CACHE,
            MemoryTier.L2_CACHE,
            MemoryTier.L3_CACHE,
            MemoryTier.MAIN_MEMORY,
            MemoryTier.SWAP
        ]
        
        try:
            idx = tier_order.index(tier)
            if idx > 0:
                return tier_order[idx - 1]
        except ValueError:
            pass
        
        return None
    
    def _migrate_tier(self, allocation_id: str, target_tier: MemoryTier) -> bool:
        """Migrate an allocation to a different tier"""
        allocation = self._allocations.get(allocation_id)
        if not allocation:
            return False
        
        # Check target tier capacity
        tier_config = self._tier_configs[target_tier]
        if self._tier_usage[target_tier] + allocation.size_mb > tier_config.max_size_mb:
            return False
        
        # Update tier tracking
        old_tier = allocation.tier
        self._tier_usage[old_tier] -= allocation.size_mb
        self._tier_usage[target_tier] += allocation.size_mb
        allocation.tier = target_tier
        
        self._stats["migrations"] += 1
        
        logger.debug(f"Migrated {allocation_id} from {old_tier.name} to {target_tier.name}")
        return True
    
    def promote(self, allocation_id: str) -> bool:
        """
        Promote an allocation to a higher (faster) tier.
        
        Args:
            allocation_id: Allocation to promote
            
        Returns:
            True if promoted
        """
        allocation = self._allocations.get(allocation_id)
        if not allocation:
            return False
        
        higher_tier = self._get_higher_tier(allocation.tier)
        if not higher_tier:
            return False
        
        if self._migrate_tier(allocation_id, higher_tier):
            self._stats["tier_promotions"] += 1
            return True
        
        return False
    
    def demote(self, allocation_id: str) -> bool:
        """
        Demote an allocation to a lower (slower) tier.
        
        Args:
            allocation_id: Allocation to demote
            
        Returns:
            True if demoted
        """
        allocation = self._allocations.get(allocation_id)
        if not allocation:
            return False
        
        lower_tier = self._get_lower_tier(allocation.tier)
        if not lower_tier:
            return False
        
        if self._migrate_tier(allocation_id, lower_tier):
            self._stats["tier_demotions"] += 1
            return True
        
        return False
    
    def map_atomspace(
        self,
        atomspace_id: str,
        estimated_atoms: int,
        tier: MemoryTier = MemoryTier.MAIN_MEMORY,
        numa_policy: NUMAPolicy = NUMAPolicy.COGNITIVE
    ) -> MappedAtomSpace:
        """
        Map an AtomSpace into memory with cognitive optimization.
        
        Args:
            atomspace_id: AtomSpace identifier
            estimated_atoms: Estimated number of atoms
            tier: Target memory tier
            numa_policy: NUMA policy
            
        Returns:
            MappedAtomSpace object
        """
        with self._lock:
            # Estimate memory needs (rough: 1KB per atom average)
            size_mb = (estimated_atoms * 1.0) / 1024
            size_mb = max(size_mb, 1.0)  # Minimum 1MB
            
            # Allocate memory
            allocation = self.allocate(
                size_mb=size_mb,
                owner=f"atomspace_{atomspace_id}",
                tier=tier,
                numa_policy=numa_policy,
                attention_weight=0.5,  # AtomSpaces get medium importance
                flags=MemoryFlags.READ | MemoryFlags.WRITE | MemoryFlags.COGNITIVE
            )
            
            import secrets
            mapping_id = f"as_map_{secrets.token_hex(6)}"
            
            mapping = MappedAtomSpace(
                mapping_id=mapping_id,
                atomspace_id=atomspace_id,
                region=allocation.region,
                numa_allocation=allocation.numa_allocation,
                tier=tier,
                atom_count=estimated_atoms
            )
            
            self._atomspace_mappings[mapping_id] = mapping
            
            logger.info(f"Mapped AtomSpace {atomspace_id} ({estimated_atoms} atoms, {size_mb:.1f}MB)")
            return mapping
    
    def unmap_atomspace(self, mapping_id: str) -> bool:
        """Unmap an AtomSpace from memory"""
        with self._lock:
            mapping = self._atomspace_mappings.pop(mapping_id, None)
            if not mapping:
                return False
            
            # Find and free the allocation
            for alloc_id, alloc in list(self._allocations.items()):
                if alloc.region and alloc.region.region_id == mapping.region.region_id:
                    self.free(alloc_id)
                    break
            
            logger.info(f"Unmapped AtomSpace {mapping.atomspace_id}")
            return True
    
    def _on_pressure_change(
        self,
        old_level: PressureLevel,
        new_level: PressureLevel
    ):
        """Handle memory pressure level changes"""
        if new_level.value > old_level.value:
            # Escalating pressure
            logger.warning(f"Memory pressure escalated: {old_level.name} -> {new_level.name}")
            
            if new_level == PressureLevel.HIGH:
                # Start aggressive eviction
                self._evict_cold_allocations(target_pct=0.1)
            elif new_level == PressureLevel.CRITICAL:
                # Emergency eviction
                self._evict_cold_allocations(target_pct=0.2)
                if self._gc_coordination:
                    gc.collect()
                    self._stats["gc_cycles"] += 1
            elif new_level == PressureLevel.OOM_IMMINENT:
                # Emergency measures
                self._evict_cold_allocations(target_pct=0.3)
                gc.collect()
                self._stats["gc_cycles"] += 1
    
    def _evict_cold_allocations(self, target_pct: float):
        """
        Evict cold (low-attention) allocations.
        
        Args:
            target_pct: Percentage of allocations to evict
        """
        with self._lock:
            # Sort by attention weight (evict lowest first)
            allocs = sorted(
                self._allocations.values(),
                key=lambda a: a.attention_weight
            )
            
            evict_count = int(len(allocs) * target_pct)
            evict_count = max(evict_count, 1)
            
            for alloc in allocs[:evict_count]:
                self.free(alloc.allocation_id)
            
            logger.info(f"Evicted {evict_count} cold allocations")
    
    def gc_hint(self, urgency: float = 0.5):
        """
        Hint to perform garbage collection.
        
        Args:
            urgency: 0.0-1.0, how urgent the GC is
        """
        if not self._gc_coordination:
            return
        
        if urgency > 0.8:
            gc.collect()  # Full collection
        elif urgency > 0.5:
            gc.collect(1)  # Gen 0 and 1
        else:
            gc.collect(0)  # Gen 0 only
        
        self._stats["gc_cycles"] += 1
    
    def get_pressure_level(self) -> PressureLevel:
        """Get current memory pressure level"""
        if self._pressure:
            return self._pressure.get_current_level()
        return PressureLevel.NORMAL
    
    def get_throttle_recommendation(self) -> ThrottleRecommendation:
        """Get throttling recommendation based on memory pressure"""
        if self._pressure:
            return self._pressure.get_throttle_recommendation()
        return ThrottleRecommendation(
            level=PressureLevel.NORMAL,
            should_throttle=False,
            throttle_factor=1.0,
            gc_recommended=False,
            eviction_recommended=False,
            message="Pressure monitoring disabled"
        )
    
    def get_tier_usage(self) -> Dict[str, Dict[str, float]]:
        """Get usage statistics for all tiers"""
        with self._lock:
            result = {}
            for tier, config in self._tier_configs.items():
                usage = self._tier_usage[tier]
                result[tier.name] = {
                    "used_mb": usage,
                    "max_mb": config.max_size_mb,
                    "usage_pct": (usage / config.max_size_mb * 100) if config.max_size_mb > 0 else 0
                }
            return result
    
    def list_allocations(
        self,
        tier: Optional[MemoryTier] = None,
        owner: Optional[str] = None
    ) -> List[Dict[str, Any]]:
        """List allocations with optional filtering"""
        with self._lock:
            allocs = self._allocations.values()
            
            if tier:
                allocs = [a for a in allocs if a.tier == tier]
            if owner:
                allocs = [a for a in allocs if a.owner == owner]
            
            return [a.to_dict() for a in allocs]
    
    def list_atomspace_mappings(self) -> List[Dict[str, Any]]:
        """List all AtomSpace mappings"""
        with self._lock:
            return [
                {
                    "mapping_id": m.mapping_id,
                    "atomspace_id": m.atomspace_id,
                    "tier": m.tier.name,
                    "atom_count": m.atom_count,
                    "access_count": m.access_count
                }
                for m in self._atomspace_mappings.values()
            ]
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get memory manager statistics"""
        with self._lock:
            stats = dict(self._stats)
            stats["tier_usage"] = self.get_tier_usage()
            stats["allocations_count"] = len(self._allocations)
            stats["atomspace_mappings"] = len(self._atomspace_mappings)
            
            if self._pressure:
                stats["pressure_level"] = self._pressure.get_current_level().name
                stats["pressure_stats"] = self._pressure.get_statistics()
            
            if self._numa:
                stats["numa_stats"] = self._numa.get_statistics()
            
            return stats
    
    def shutdown(self):
        """Shutdown the memory manager"""
        logger.info("Shutting down AGI_MemoryManager")
        
        if self._pressure:
            self._pressure.stop_monitoring()
        
        # Free all allocations
        with self._lock:
            for alloc_id in list(self._allocations.keys()):
                self.free(alloc_id)
        
        logger.info("AGI_MemoryManager shutdown complete")


# Global memory manager instance
_memory_manager: Optional[AGI_MemoryManager] = None


def get_memory_manager() -> AGI_MemoryManager:
    """Get the global AGI memory manager instance"""
    global _memory_manager
    if _memory_manager is None:
        _memory_manager = AGI_MemoryManager()
    return _memory_manager


if __name__ == "__main__":
    import json
    
    print("=== AGI Memory Manager Example ===\n")
    
    manager = get_memory_manager()
    
    # Allocate memory in different tiers
    print("=== Allocating Memory ===")
    
    alloc1 = manager.allocate(
        size_mb=64,
        owner="pln_engine",
        tier=MemoryTier.L2_CACHE,
        attention_weight=0.8
    )
    print(f"Allocated: {alloc1.allocation_id} in {alloc1.tier.name}")
    
    alloc2 = manager.allocate(
        size_mb=128,
        owner="pattern_miner",
        tier=MemoryTier.MAIN_MEMORY,
        attention_weight=0.5
    )
    print(f"Allocated: {alloc2.allocation_id} in {alloc2.tier.name}")
    
    alloc3 = manager.allocate(
        size_mb=32,
        owner="attention_bank",
        tier=MemoryTier.L1_CACHE,
        attention_weight=0.95
    )
    print(f"Allocated: {alloc3.allocation_id} in {alloc3.tier.name}")
    
    # Map an AtomSpace
    print("\n=== Mapping AtomSpace ===")
    as_map = manager.map_atomspace(
        atomspace_id="main_knowledge_base",
        estimated_atoms=100000,
        tier=MemoryTier.MAIN_MEMORY
    )
    print(f"Mapped AtomSpace: {as_map.atomspace_id}")
    
    # Get tier usage
    print("\n=== Tier Usage ===")
    for tier_name, usage in manager.get_tier_usage().items():
        print(f"  {tier_name}: {usage['used_mb']:.1f}MB / {usage['max_mb']:.0f}MB "
              f"({usage['usage_pct']:.1f}%)")
    
    # Get pressure
    print("\n=== Memory Pressure ===")
    rec = manager.get_throttle_recommendation()
    print(f"  Level: {rec.level.name}")
    print(f"  Throttle Factor: {rec.throttle_factor}")
    print(f"  Message: {rec.message}")
    
    # Get statistics
    print("\n=== Statistics ===")
    stats = manager.get_statistics()
    print(f"  Allocations: {stats['allocations']}")
    print(f"  Total Allocated: {stats['total_allocated_mb']:.1f}MB")
    print(f"  AtomSpace Mappings: {stats['atomspace_mappings']}")
    
    # Cleanup
    manager.shutdown()
    print("\nMemory manager shutdown.")
