#!/usr/bin/env python3
"""
Attention Bank for ECAN

This module implements the attention bank for the Economic Attention Network.
It manages attention values (STI/LTI) for atoms and provides focus set
management for attention-based processing.

Features:
- Attention value storage and update
- Focus set management
- Attention fund allocation
- Rent collection and decay
- Statistics and monitoring
"""

import time
import logging
import threading
from typing import Dict, Any, Optional, List, Set, Tuple
from dataclasses import dataclass, field
from enum import Enum
from collections import OrderedDict
import heapq

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("AGI_ECAN.AttentionBank")


@dataclass
class AttentionValue:
    """
    Attention Value (AV) for an atom.
    
    - STI (Short-Term Importance): Current relevance (can be negative)
    - LTI (Long-Term Importance): Persistent importance (non-negative)
    - VLTI (Very Long-Term Importance): Flag for permanent atoms
    """
    sti: float = 0.0        # Short-term importance
    lti: float = 0.0        # Long-term importance
    vlti: bool = False      # Very long-term importance flag
    
    # Tracking
    last_update: float = field(default_factory=time.time)
    update_count: int = 0
    
    def __post_init__(self):
        # LTI is always non-negative
        self.lti = max(0.0, self.lti)
    
    @property
    def total_importance(self) -> float:
        """Combined importance measure"""
        return self.sti + self.lti
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "sti": self.sti,
            "lti": self.lti,
            "vlti": self.vlti,
            "total_importance": self.total_importance,
            "last_update": self.last_update,
            "update_count": self.update_count
        }
    
    def __repr__(self) -> str:
        return f"AV(sti={self.sti:.2f}, lti={self.lti:.2f}, vlti={self.vlti})"


@dataclass
class AttentionConfig:
    """Configuration for attention bank"""
    # Focus set parameters
    attentional_focus_boundary: float = 0.0    # STI threshold for focus
    af_size: int = 100                          # Max atoms in attentional focus
    
    # Rent parameters
    sti_rent: float = 0.1                       # STI rent per cycle
    lti_rent: float = 0.01                      # LTI rent per cycle
    
    # Fund parameters
    starting_funds: float = 10000.0             # Initial STI/LTI funds
    target_sti_sum: float = 1000.0              # Target sum of all STI
    target_lti_sum: float = 500.0               # Target sum of all LTI
    
    # Decay parameters
    sti_decay_rate: float = 0.01                # STI decay per second
    forgetting_threshold: float = -100.0        # Remove atoms below this STI


@dataclass(order=True)
class AtomAttention:
    """Attention tracking for an atom (sortable by STI)"""
    sti: float = field(compare=True)
    atom_id: str = field(compare=False)
    av: AttentionValue = field(compare=False)


class AttentionBank:
    """
    Attention Bank for ECAN
    
    Manages attention values for atoms and provides mechanisms for
    attention-based processing including focus sets, rent collection,
    and importance decay.
    """
    
    def __init__(self, config: Optional[AttentionConfig] = None):
        """
        Initialize the attention bank.
        
        Args:
            config: Attention configuration
        """
        self._lock = threading.RLock()
        self._config = config or AttentionConfig()
        
        # Attention value storage
        self._attention: Dict[str, AttentionValue] = {}
        
        # Focus set (heap sorted by STI)
        self._attentional_focus: List[AtomAttention] = []
        heapq.heapify(self._attentional_focus)
        self._af_set: Set[str] = set()
        
        # Attention funds
        self._sti_funds: float = self._config.starting_funds
        self._lti_funds: float = self._config.starting_funds
        
        # Rent collection tracking
        self._last_rent_cycle: float = time.time()
        
        # Statistics
        self._stats = {
            "atoms_tracked": 0,
            "sti_changes": 0,
            "lti_changes": 0,
            "rent_collected": 0.0,
            "atoms_forgotten": 0
        }
        
        logger.info("AttentionBank initialized")
    
    def get(self, atom_id: str) -> Optional[AttentionValue]:
        """Get attention value for an atom"""
        with self._lock:
            return self._attention.get(atom_id)
    
    def set(
        self,
        atom_id: str,
        sti: Optional[float] = None,
        lti: Optional[float] = None,
        vlti: Optional[bool] = None
    ) -> AttentionValue:
        """
        Set attention value for an atom.
        
        Args:
            atom_id: Atom identifier
            sti: Short-term importance (None = keep current)
            lti: Long-term importance (None = keep current)
            vlti: Very long-term importance flag (None = keep current)
            
        Returns:
            Updated AttentionValue
        """
        with self._lock:
            av = self._attention.get(atom_id)
            
            if av is None:
                av = AttentionValue()
                self._attention[atom_id] = av
                self._stats["atoms_tracked"] += 1
            
            old_sti = av.sti
            
            if sti is not None:
                # Transfer from/to funds
                delta = sti - av.sti
                self._sti_funds -= delta
                av.sti = sti
                self._stats["sti_changes"] += 1
            
            if lti is not None:
                delta = lti - av.lti
                self._lti_funds -= delta
                av.lti = max(0.0, lti)
                self._stats["lti_changes"] += 1
            
            if vlti is not None:
                av.vlti = vlti
            
            av.last_update = time.time()
            av.update_count += 1
            
            # Update focus set
            self._update_focus_set(atom_id, old_sti, av)
            
            return av
    
    def stimulate(self, atom_id: str, amount: float) -> AttentionValue:
        """
        Stimulate an atom by increasing its STI.
        
        Args:
            atom_id: Atom to stimulate
            amount: Amount to add to STI
            
        Returns:
            Updated AttentionValue
        """
        av = self.get(atom_id)
        current_sti = av.sti if av else 0.0
        return self.set(atom_id, sti=current_sti + amount)
    
    def inhibit(self, atom_id: str, amount: float) -> AttentionValue:
        """
        Inhibit an atom by decreasing its STI.
        
        Args:
            atom_id: Atom to inhibit
            amount: Amount to subtract from STI
            
        Returns:
            Updated AttentionValue
        """
        av = self.get(atom_id)
        current_sti = av.sti if av else 0.0
        return self.set(atom_id, sti=current_sti - amount)
    
    def _update_focus_set(
        self,
        atom_id: str,
        old_sti: float,
        av: AttentionValue
    ):
        """Update the attentional focus set"""
        boundary = self._config.attentional_focus_boundary
        max_size = self._config.af_size
        
        in_focus = atom_id in self._af_set
        should_be_in_focus = av.sti >= boundary
        
        if should_be_in_focus and not in_focus:
            # Add to focus
            heapq.heappush(
                self._attentional_focus,
                AtomAttention(sti=-av.sti, atom_id=atom_id, av=av)  # Negative for max-heap
            )
            self._af_set.add(atom_id)
            
            # Trim if over size
            while len(self._attentional_focus) > max_size:
                removed = heapq.heappop(self._attentional_focus)
                self._af_set.discard(removed.atom_id)
        
        elif not should_be_in_focus and in_focus:
            # Remove from focus (mark for lazy removal)
            self._af_set.discard(atom_id)
    
    def get_attentional_focus(self) -> List[Tuple[str, AttentionValue]]:
        """
        Get atoms in the attentional focus.
        
        Returns:
            List of (atom_id, AttentionValue) in focus
        """
        with self._lock:
            # Clean up stale entries
            self._clean_focus()
            
            result = []
            for entry in self._attentional_focus:
                if entry.atom_id in self._af_set:
                    result.append((entry.atom_id, entry.av))
            
            # Sort by STI descending
            result.sort(key=lambda x: -x[1].sti)
            return result
    
    def _clean_focus(self):
        """Remove stale entries from focus heap"""
        # Rebuild heap with only valid entries
        valid = [
            AtomAttention(sti=-av.sti, atom_id=aid, av=av)
            for aid, av in self._attention.items()
            if aid in self._af_set
        ]
        self._attentional_focus = valid
        heapq.heapify(self._attentional_focus)
    
    def in_attentional_focus(self, atom_id: str) -> bool:
        """Check if an atom is in the attentional focus"""
        with self._lock:
            return atom_id in self._af_set
    
    def collect_rent(self):
        """
        Collect rent from all atoms.
        
        Atoms pay rent proportional to their attention.
        This helps maintain attention economy balance.
        """
        with self._lock:
            now = time.time()
            elapsed = now - self._last_rent_cycle
            self._last_rent_cycle = now
            
            total_rent = 0.0
            to_forget = []
            
            for atom_id, av in self._attention.items():
                if av.vlti:
                    continue  # VLTI atoms don't pay rent
                
                # Calculate rent
                sti_rent = self._config.sti_rent * elapsed
                lti_rent = self._config.lti_rent * elapsed
                
                # Deduct rent
                av.sti -= sti_rent
                av.lti = max(0, av.lti - lti_rent)
                
                total_rent += sti_rent + lti_rent
                
                # Check for forgetting
                if av.sti < self._config.forgetting_threshold:
                    to_forget.append(atom_id)
            
            # Return rent to funds
            self._sti_funds += total_rent * 0.5
            self._lti_funds += total_rent * 0.5
            
            self._stats["rent_collected"] += total_rent
            
            # Forget atoms below threshold
            for atom_id in to_forget:
                self._forget_atom(atom_id)
    
    def _forget_atom(self, atom_id: str):
        """Remove an atom from attention tracking"""
        av = self._attention.pop(atom_id, None)
        if av:
            self._af_set.discard(atom_id)
            self._stats["atoms_forgotten"] += 1
            logger.debug(f"Forgot atom {atom_id}")
    
    def decay_sti(self, elapsed_seconds: float):
        """
        Apply STI decay over time.
        
        Args:
            elapsed_seconds: Time elapsed
        """
        with self._lock:
            decay = self._config.sti_decay_rate * elapsed_seconds
            
            for av in self._attention.values():
                if not av.vlti and av.sti > 0:
                    av.sti = max(0, av.sti - decay)
    
    def normalize(self):
        """
        Normalize STI/LTI to maintain target sums.
        
        Ensures attention economy doesn't inflate or deflate.
        """
        with self._lock:
            if not self._attention:
                return
            
            # Calculate current sums
            sti_sum = sum(av.sti for av in self._attention.values())
            lti_sum = sum(av.lti for av in self._attention.values())
            
            # Normalize STI
            if sti_sum > 0:
                sti_scale = self._config.target_sti_sum / sti_sum
                if abs(sti_scale - 1.0) > 0.01:  # Only if > 1% off
                    for av in self._attention.values():
                        av.sti *= sti_scale
            
            # Normalize LTI
            if lti_sum > 0:
                lti_scale = self._config.target_lti_sum / lti_sum
                if abs(lti_scale - 1.0) > 0.01:
                    for av in self._attention.values():
                        av.lti *= lti_scale
    
    def get_sti_funds(self) -> float:
        """Get available STI funds"""
        return self._sti_funds
    
    def get_lti_funds(self) -> float:
        """Get available LTI funds"""
        return self._lti_funds
    
    def allocate_sti(self, amount: float) -> bool:
        """
        Allocate STI from funds.
        
        Args:
            amount: Amount to allocate
            
        Returns:
            True if allocation succeeded
        """
        with self._lock:
            if amount <= self._sti_funds:
                self._sti_funds -= amount
                return True
            return False
    
    def get_top_atoms(self, n: int = 10) -> List[Tuple[str, AttentionValue]]:
        """Get top n atoms by STI"""
        with self._lock:
            sorted_atoms = sorted(
                self._attention.items(),
                key=lambda x: -x[1].sti
            )
            return sorted_atoms[:n]
    
    def get_bottom_atoms(self, n: int = 10) -> List[Tuple[str, AttentionValue]]:
        """Get bottom n atoms by STI"""
        with self._lock:
            sorted_atoms = sorted(
                self._attention.items(),
                key=lambda x: x[1].sti
            )
            return sorted_atoms[:n]
    
    def list_atoms(
        self,
        min_sti: Optional[float] = None,
        min_lti: Optional[float] = None,
        in_focus: Optional[bool] = None
    ) -> List[Tuple[str, AttentionValue]]:
        """List atoms with optional filtering"""
        with self._lock:
            result = []
            for atom_id, av in self._attention.items():
                if min_sti is not None and av.sti < min_sti:
                    continue
                if min_lti is not None and av.lti < min_lti:
                    continue
                if in_focus is not None:
                    atom_in_focus = atom_id in self._af_set
                    if in_focus != atom_in_focus:
                        continue
                result.append((atom_id, av))
            
            return result
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get attention bank statistics"""
        with self._lock:
            sti_values = [av.sti for av in self._attention.values()]
            lti_values = [av.lti for av in self._attention.values()]
            
            return {
                **self._stats,
                "sti_funds": self._sti_funds,
                "lti_funds": self._lti_funds,
                "focus_size": len(self._af_set),
                "sti_sum": sum(sti_values) if sti_values else 0,
                "lti_sum": sum(lti_values) if lti_values else 0,
                "sti_avg": sum(sti_values) / len(sti_values) if sti_values else 0,
                "lti_avg": sum(lti_values) / len(lti_values) if lti_values else 0,
                "sti_max": max(sti_values) if sti_values else 0,
                "sti_min": min(sti_values) if sti_values else 0
            }


# Global attention bank
_attention_bank: Optional[AttentionBank] = None


def get_attention_bank() -> AttentionBank:
    """Get the global attention bank instance"""
    global _attention_bank
    if _attention_bank is None:
        _attention_bank = AttentionBank()
    return _attention_bank


if __name__ == "__main__":
    import json
    
    print("=== ECAN Attention Bank Examples ===\n")
    
    bank = get_attention_bank()
    
    # Set attention for some atoms
    print("=== Setting Attention Values ===")
    bank.set("concept_cat", sti=80.0, lti=20.0)
    bank.set("concept_dog", sti=60.0, lti=15.0)
    bank.set("concept_animal", sti=50.0, lti=30.0)
    bank.set("concept_food", sti=40.0, lti=10.0)
    bank.set("concept_water", sti=30.0, lti=25.0, vlti=True)
    
    print(f"Atoms tracked: {bank._stats['atoms_tracked']}")
    
    # Stimulate an atom
    print("\n=== Stimulating 'concept_cat' ===")
    av = bank.stimulate("concept_cat", 20.0)
    print(f"After stimulation: {av}")
    
    # Get attentional focus
    print("\n=== Attentional Focus ===")
    focus = bank.get_attentional_focus()
    for atom_id, av in focus:
        print(f"  {atom_id}: {av}")
    
    # Get top atoms
    print("\n=== Top 3 Atoms by STI ===")
    for atom_id, av in bank.get_top_atoms(3):
        print(f"  {atom_id}: STI={av.sti:.1f}, LTI={av.lti:.1f}")
    
    # Collect rent
    print("\n=== Collecting Rent ===")
    bank.collect_rent()
    print(f"Rent collected: {bank._stats['rent_collected']:.2f}")
    
    # Get statistics
    print("\n=== Statistics ===")
    print(json.dumps(bank.get_statistics(), indent=2))
