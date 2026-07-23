#!/usr/bin/env python3
"""
Virtual Memory Abstraction for AGI-OS

This module provides a virtual memory abstraction layer that enables
cognitive components to manage memory through a unified interface,
supporting memory-mapped regions, access control, and AtomSpace integration.

Features:
- Memory region management
- Access protection (read/write/execute)
- Memory-mapped file support
- AtomSpace memory mapping
- Copy-on-write support
- Memory statistics
"""

import time
import logging
import threading
import mmap
import os
from typing import Dict, Any, Optional, List, Set, Callable
from dataclasses import dataclass, field
from enum import Flag, auto
import struct

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("AGI_Memory.Virtual")


class MemoryFlags(Flag):
    """Memory protection and behavior flags"""
    NONE = 0
    READ = auto()           # Region is readable
    WRITE = auto()          # Region is writable
    EXECUTE = auto()        # Region is executable (for code)
    SHARED = auto()         # Region is shared between processes
    PRIVATE = auto()        # Region is private (copy-on-write)
    LOCKED = auto()         # Region is locked in memory (no swap)
    GROWSDOWN = auto()      # Region grows downward (stack)
    GROWSUP = auto()        # Region grows upward (heap)
    HUGETLB = auto()        # Use huge pages
    ANONYMOUS = auto()      # No backing file
    COGNITIVE = auto()      # Cognitive memory (attention-aware)
    
    # Common combinations
    RW = READ | WRITE
    RWX = READ | WRITE | EXECUTE
    COW = READ | WRITE | PRIVATE


@dataclass
class PageFault:
    """Information about a page fault"""
    address: int
    region_id: str
    fault_type: str          # "read", "write", "execute"
    timestamp: float
    resolved: bool = False
    resolution: str = ""


@dataclass
class MemoryRegion:
    """
    A virtual memory region.
    
    Represents a contiguous region of virtual address space with
    associated protection and attributes.
    """
    region_id: str
    start_address: int
    size: int
    flags: MemoryFlags
    name: str = ""
    backing_file: Optional[str] = None
    created_at: float = field(default_factory=time.time)
    
    # Cognitive attributes
    attention_weight: float = 0.0    # Higher = more important to keep in memory
    last_access: float = field(default_factory=time.time)
    access_count: int = 0
    
    # Internal state
    _data: Optional[bytes] = field(default=None, repr=False)
    _mmap: Optional[mmap.mmap] = field(default=None, repr=False)
    
    @property
    def end_address(self) -> int:
        return self.start_address + self.size
    
    def contains(self, address: int) -> bool:
        """Check if address is within this region"""
        return self.start_address <= address < self.end_address
    
    def touch(self):
        """Update access statistics"""
        self.last_access = time.time()
        self.access_count += 1
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "region_id": self.region_id,
            "start_address": hex(self.start_address),
            "end_address": hex(self.end_address),
            "size": self.size,
            "size_kb": self.size / 1024,
            "flags": str(self.flags),
            "name": self.name,
            "backing_file": self.backing_file,
            "attention_weight": self.attention_weight,
            "access_count": self.access_count
        }


class VirtualMemoryManager:
    """
    Virtual Memory Manager for AGI-OS
    
    Manages virtual address spaces for cognitive components with
    support for memory mapping, protection, and AtomSpace integration.
    """
    
    # Default virtual address space layout
    USER_SPACE_START = 0x0000_0000_0000_0000
    USER_SPACE_END = 0x0000_7FFF_FFFF_FFFF
    KERNEL_SPACE_START = 0xFFFF_8000_0000_0000
    PAGE_SIZE = 4096  # 4KB pages
    
    def __init__(
        self,
        max_regions: int = 10000,
        enable_cognitive_paging: bool = True
    ):
        self.max_regions = max_regions
        self.enable_cognitive_paging = enable_cognitive_paging
        
        self._regions: Dict[str, MemoryRegion] = {}
        self._address_map: Dict[int, str] = {}  # start_address -> region_id
        self._next_address = self.USER_SPACE_START + self.PAGE_SIZE
        self._lock = threading.RLock()
        
        # Page fault handling
        self._fault_handlers: List[Callable[[PageFault], bool]] = []
        self._fault_history: List[PageFault] = []
        
        # Statistics
        self._stats = {
            "total_mapped": 0,
            "total_unmapped": 0,
            "page_faults": 0,
            "regions_created": 0,
            "regions_destroyed": 0
        }
        
        logger.info("VirtualMemoryManager initialized")
    
    def _align_address(self, address: int) -> int:
        """Align address to page boundary"""
        return (address + self.PAGE_SIZE - 1) & ~(self.PAGE_SIZE - 1)
    
    def _find_free_region(self, size: int) -> int:
        """Find a free region of at least 'size' bytes"""
        aligned_size = self._align_address(size)
        
        # Simple first-fit allocation
        current = self._next_address
        while current + aligned_size < self.USER_SPACE_END:
            # Check for overlaps
            overlap = False
            for region in self._regions.values():
                if (current < region.end_address and 
                    current + aligned_size > region.start_address):
                    current = self._align_address(region.end_address)
                    overlap = True
                    break
            
            if not overlap:
                return current
            
        raise MemoryError("No free virtual address space available")
    
    def allocate(
        self,
        size: int,
        flags: MemoryFlags = MemoryFlags.RW,
        name: str = "",
        address: Optional[int] = None,
        attention_weight: float = 0.0
    ) -> MemoryRegion:
        """
        Allocate a virtual memory region.
        
        Args:
            size: Size in bytes
            flags: Memory protection flags
            name: Human-readable name
            address: Specific address (optional, None = auto)
            attention_weight: Cognitive importance (for paging)
            
        Returns:
            MemoryRegion object
        """
        with self._lock:
            if len(self._regions) >= self.max_regions:
                raise MemoryError("Maximum number of regions exceeded")
            
            aligned_size = self._align_address(size)
            
            # Determine address
            if address is None:
                start_addr = self._find_free_region(aligned_size)
            else:
                start_addr = self._align_address(address)
                # Verify no overlap
                for region in self._regions.values():
                    if (start_addr < region.end_address and 
                        start_addr + aligned_size > region.start_address):
                        raise MemoryError(f"Address {hex(address)} overlaps existing region")
            
            # Create region
            import secrets
            region_id = f"region_{secrets.token_hex(8)}"
            
            region = MemoryRegion(
                region_id=region_id,
                start_address=start_addr,
                size=aligned_size,
                flags=flags,
                name=name,
                attention_weight=attention_weight
            )
            
            # Allocate backing memory if not memory-mapped file
            if MemoryFlags.ANONYMOUS in flags or not region.backing_file:
                region._data = bytes(aligned_size)
            
            self._regions[region_id] = region
            self._address_map[start_addr] = region_id
            
            # Update next address hint
            if start_addr >= self._next_address:
                self._next_address = start_addr + aligned_size
            
            self._stats["total_mapped"] += aligned_size
            self._stats["regions_created"] += 1
            
            logger.debug(f"Allocated region {region_id} at {hex(start_addr)}, size={aligned_size}")
            return region
    
    def free(self, region_id: str) -> bool:
        """
        Free a memory region.
        
        Args:
            region_id: Region to free
            
        Returns:
            True if freed, False if not found
        """
        with self._lock:
            region = self._regions.pop(region_id, None)
            if not region:
                return False
            
            self._address_map.pop(region.start_address, None)
            
            # Close mmap if present
            if region._mmap:
                region._mmap.close()
            
            self._stats["total_unmapped"] += region.size
            self._stats["regions_destroyed"] += 1
            
            logger.debug(f"Freed region {region_id}")
            return True
    
    def mmap_file(
        self,
        filepath: str,
        flags: MemoryFlags = MemoryFlags.READ,
        offset: int = 0,
        size: Optional[int] = None,
        name: str = ""
    ) -> MemoryRegion:
        """
        Memory-map a file.
        
        Args:
            filepath: Path to file
            flags: Memory protection flags
            offset: Offset into file
            size: Size to map (None = entire file)
            name: Human-readable name
            
        Returns:
            MemoryRegion object
        """
        if not os.path.exists(filepath):
            raise FileNotFoundError(filepath)
        
        file_size = os.path.getsize(filepath)
        if size is None:
            size = file_size - offset
        
        if offset + size > file_size:
            raise ValueError("Mapping extends beyond file size")
        
        # Determine mmap access mode
        access = mmap.ACCESS_READ
        if MemoryFlags.WRITE in flags:
            access = mmap.ACCESS_WRITE
        if MemoryFlags.PRIVATE in flags:
            access = mmap.ACCESS_COPY
        
        region = self.allocate(size, flags, name or filepath)
        region.backing_file = filepath
        
        # Create mmap
        with open(filepath, 'rb') as f:
            region._mmap = mmap.mmap(
                f.fileno(),
                size,
                access=access,
                offset=offset
            )
        
        return region
    
    def read(self, region_id: str, offset: int, size: int) -> bytes:
        """
        Read from a memory region.
        
        Args:
            region_id: Region to read from
            offset: Offset within region
            size: Bytes to read
            
        Returns:
            Bytes read
        """
        with self._lock:
            region = self._regions.get(region_id)
            if not region:
                raise KeyError(f"Region {region_id} not found")
            
            if MemoryFlags.READ not in region.flags:
                self._handle_fault(region, offset, "read")
                raise PermissionError("Region is not readable")
            
            if offset + size > region.size:
                raise ValueError("Read extends beyond region")
            
            region.touch()
            
            if region._mmap:
                return region._mmap[offset:offset + size]
            elif region._data:
                return region._data[offset:offset + size]
            else:
                return bytes(size)
    
    def write(self, region_id: str, offset: int, data: bytes):
        """
        Write to a memory region.
        
        Args:
            region_id: Region to write to
            offset: Offset within region
            data: Bytes to write
        """
        with self._lock:
            region = self._regions.get(region_id)
            if not region:
                raise KeyError(f"Region {region_id} not found")
            
            if MemoryFlags.WRITE not in region.flags:
                self._handle_fault(region, offset, "write")
                raise PermissionError("Region is not writable")
            
            if offset + len(data) > region.size:
                raise ValueError("Write extends beyond region")
            
            region.touch()
            
            if region._mmap:
                region._mmap[offset:offset + len(data)] = data
            elif region._data:
                # Create mutable version
                data_list = list(region._data)
                data_list[offset:offset + len(data)] = list(data)
                region._data = bytes(data_list)
    
    def protect(self, region_id: str, flags: MemoryFlags):
        """Change protection flags for a region"""
        with self._lock:
            region = self._regions.get(region_id)
            if not region:
                raise KeyError(f"Region {region_id} not found")
            region.flags = flags
    
    def _handle_fault(self, region: MemoryRegion, offset: int, fault_type: str):
        """Handle a page fault"""
        fault = PageFault(
            address=region.start_address + offset,
            region_id=region.region_id,
            fault_type=fault_type,
            timestamp=time.time()
        )
        
        self._stats["page_faults"] += 1
        self._fault_history.append(fault)
        
        # Trim history
        if len(self._fault_history) > 1000:
            self._fault_history = self._fault_history[-1000:]
        
        # Try handlers
        for handler in self._fault_handlers:
            if handler(fault):
                fault.resolved = True
                break
        
        logger.debug(f"Page fault: {fault_type} at {hex(fault.address)}")
    
    def register_fault_handler(self, handler: Callable[[PageFault], bool]):
        """Register a page fault handler"""
        self._fault_handlers.append(handler)
    
    def find_region(self, address: int) -> Optional[MemoryRegion]:
        """Find region containing address"""
        with self._lock:
            for region in self._regions.values():
                if region.contains(address):
                    return region
            return None
    
    def get_region(self, region_id: str) -> Optional[MemoryRegion]:
        """Get region by ID"""
        with self._lock:
            return self._regions.get(region_id)
    
    def list_regions(self) -> List[Dict[str, Any]]:
        """List all regions"""
        with self._lock:
            return [r.to_dict() for r in self._regions.values()]
    
    def get_cognitive_priority_list(self) -> List[str]:
        """
        Get regions sorted by cognitive priority (attention weight + recency).
        Used for cognitive-aware paging decisions.
        """
        with self._lock:
            # Score = attention_weight * recency_factor
            def priority_score(region: MemoryRegion) -> float:
                age = time.time() - region.last_access
                recency = 1.0 / (1.0 + age / 3600.0)  # Decay over hours
                return region.attention_weight * recency
            
            sorted_regions = sorted(
                self._regions.values(),
                key=priority_score,
                reverse=True
            )
            return [r.region_id for r in sorted_regions]
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get memory manager statistics"""
        with self._lock:
            total_size = sum(r.size for r in self._regions.values())
            return {
                **self._stats,
                "regions_count": len(self._regions),
                "total_size_bytes": total_size,
                "total_size_mb": total_size / (1024 * 1024),
                "recent_faults": len([f for f in self._fault_history 
                                    if time.time() - f.timestamp < 60])
            }


# Global virtual memory manager instance
_vmm_instance: Optional[VirtualMemoryManager] = None


def get_virtual_memory_manager() -> VirtualMemoryManager:
    """Get the global virtual memory manager instance"""
    global _vmm_instance
    if _vmm_instance is None:
        _vmm_instance = VirtualMemoryManager()
    return _vmm_instance


if __name__ == "__main__":
    import json
    
    vmm = get_virtual_memory_manager()
    
    print("=== Virtual Memory Manager Example ===\n")
    
    # Allocate some regions
    region1 = vmm.allocate(4096, MemoryFlags.RW, "test_region_1")
    print(f"Allocated: {region1.region_id} at {hex(region1.start_address)}")
    
    region2 = vmm.allocate(8192, MemoryFlags.READ, "test_region_2", attention_weight=0.8)
    print(f"Allocated: {region2.region_id} at {hex(region2.start_address)}")
    
    # Write and read
    vmm.write(region1.region_id, 0, b"Hello, AGI-OS!")
    data = vmm.read(region1.region_id, 0, 14)
    print(f"Read: {data.decode()}")
    
    # Try to write to read-only region
    try:
        vmm.write(region2.region_id, 0, b"test")
    except PermissionError as e:
        print(f"Expected error: {e}")
    
    print("\n=== Regions ===")
    for region in vmm.list_regions():
        print(f"  {region['name']}: {region['size_kb']:.1f} KB at {region['start_address']}")
    
    print("\n=== Statistics ===")
    print(json.dumps(vmm.get_statistics(), indent=2))
    
    print("\n=== Cognitive Priority ===")
    print(vmm.get_cognitive_priority_list())
