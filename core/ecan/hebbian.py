#!/usr/bin/env python3
"""
Hebbian Learning for ECAN

This module implements Hebbian learning for the Economic Attention Network.
It creates and manages Hebbian links that strengthen connections between
atoms that are frequently co-activated.

Features:
- Automatic Hebbian link creation
- Link strength update (LTP/LTD)
- Asymmetric Hebbian learning
- Link pruning and decay
- Integration with attention bank
"""

import time
import logging
import threading
from typing import Dict, Any, Optional, List, Set, Tuple
from dataclasses import dataclass, field
from enum import Enum
import math

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("AGI_ECAN.Hebbian")


class HebbianLinkType(Enum):
    """Types of Hebbian links"""
    SYMMETRIC = "symmetric"          # Standard Hebbian: A-B
    ASYMMETRIC = "asymmetric"        # Directional: A->B
    ANTI_HEBBIAN = "anti_hebbian"    # Decorrelation link


@dataclass
class HebbianLink:
    """
    A Hebbian link between two atoms.
    
    The link strength represents learned co-activation patterns.
    """
    link_id: str
    source_id: str
    target_id: str
    link_type: HebbianLinkType = HebbianLinkType.SYMMETRIC
    
    # Link strength (0 to 1)
    strength: float = 0.5
    
    # Learning metadata
    creation_time: float = field(default_factory=time.time)
    last_update: float = field(default_factory=time.time)
    update_count: int = 0
    
    # Co-activation tracking
    total_coactivations: int = 0
    recent_coactivations: int = 0  # In current window
    
    # Decay tracking
    last_decay: float = field(default_factory=time.time)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "link_id": self.link_id,
            "source_id": self.source_id,
            "target_id": self.target_id,
            "link_type": self.link_type.value,
            "strength": self.strength,
            "update_count": self.update_count,
            "total_coactivations": self.total_coactivations,
            "age_seconds": time.time() - self.creation_time
        }


@dataclass
class HebbianConfig:
    """Configuration for Hebbian learning"""
    # Link creation
    creation_threshold: float = 0.3     # Min correlation to create link
    min_sti_for_learning: float = 1.0   # Min STI to participate in learning
    
    # Learning rates
    ltp_rate: float = 0.1               # Long-term potentiation rate
    ltd_rate: float = 0.05              # Long-term depression rate
    
    # Strength bounds
    min_strength: float = 0.0
    max_strength: float = 1.0
    initial_strength: float = 0.3
    
    # Decay
    decay_rate: float = 0.001           # Strength decay per second
    decay_interval_s: float = 10.0       # Seconds between decay applications
    
    # Pruning
    prune_threshold: float = 0.05       # Remove links below this strength
    prune_interval_s: float = 60.0       # Seconds between pruning cycles
    
    # Co-activation window
    coactivation_window_ms: float = 500.0  # Time window for co-activation
    coactivation_decay_factor: float = 0.9  # Decay for recent count


class HebbianManager:
    """
    Hebbian Learning Manager for ECAN
    
    Manages the creation, updating, and pruning of Hebbian links
    based on atom co-activation patterns.
    """
    
    def __init__(
        self,
        attention_bank = None,
        config: Optional[HebbianConfig] = None
    ):
        """
        Initialize the Hebbian manager.
        
        Args:
            attention_bank: AttentionBank instance
            config: Hebbian configuration
        """
        self._lock = threading.RLock()
        
        self._attention_bank = attention_bank
        self._config = config or HebbianConfig()
        
        # Link storage
        self._links: Dict[str, HebbianLink] = {}
        self._link_index: Dict[Tuple[str, str], str] = {}  # (source, target) -> link_id
        
        # Co-activation tracking
        self._recent_activations: Dict[str, float] = {}  # atom_id -> last_activation_time
        
        # Decay and pruning tracking
        self._last_decay: float = time.time()
        self._last_prune: float = time.time()
        
        # Statistics
        self._stats = {
            "links_created": 0,
            "links_pruned": 0,
            "ltp_events": 0,
            "ltd_events": 0,
            "coactivations_detected": 0
        }
        
        logger.info("HebbianManager initialized")
    
    def _get_attention_bank(self):
        """Lazy getter for attention bank"""
        if self._attention_bank is None:
            from core.ecan.attention_bank import get_attention_bank
            self._attention_bank = get_attention_bank()
        return self._attention_bank
    
    def record_activation(self, atom_id: str) -> List[HebbianLink]:
        """
        Record an atom activation and update Hebbian links.
        
        Args:
            atom_id: Activated atom
            
        Returns:
            List of affected Hebbian links
        """
        with self._lock:
            now = time.time()
            affected_links = []
            
            # Check for co-activations with recently active atoms
            window_s = self._config.coactivation_window_ms / 1000.0
            
            for other_id, last_time in list(self._recent_activations.items()):
                if other_id == atom_id:
                    continue
                
                if now - last_time < window_s:
                    # Co-activation detected!
                    self._stats["coactivations_detected"] += 1
                    
                    # Get or create Hebbian link
                    link = self._get_or_create_link(atom_id, other_id)
                    if link:
                        # Apply LTP (strengthening)
                        self._apply_ltp(link)
                        affected_links.append(link)
            
            # Record this activation
            self._recent_activations[atom_id] = now
            
            # Decay recent activations
            self._decay_recent_activations()
            
            return affected_links
    
    def _get_or_create_link(
        self,
        source_id: str,
        target_id: str
    ) -> Optional[HebbianLink]:
        """Get existing link or create new one"""
        # Check if link exists (either direction for symmetric)
        link_key = (source_id, target_id)
        reverse_key = (target_id, source_id)
        
        link_id = self._link_index.get(link_key) or self._link_index.get(reverse_key)
        
        if link_id:
            return self._links.get(link_id)
        
        # Check if atoms have sufficient attention
        bank = self._get_attention_bank()
        source_av = bank.get(source_id)
        target_av = bank.get(target_id)
        
        source_sti = source_av.sti if source_av else 0.0
        target_sti = target_av.sti if target_av else 0.0
        
        if source_sti < self._config.min_sti_for_learning or \
           target_sti < self._config.min_sti_for_learning:
            return None
        
        # Create new link
        return self._create_link(source_id, target_id)
    
    def _create_link(
        self,
        source_id: str,
        target_id: str,
        link_type: HebbianLinkType = HebbianLinkType.SYMMETRIC
    ) -> HebbianLink:
        """Create a new Hebbian link"""
        import secrets
        link_id = f"hebb_{secrets.token_hex(6)}"
        
        link = HebbianLink(
            link_id=link_id,
            source_id=source_id,
            target_id=target_id,
            link_type=link_type,
            strength=self._config.initial_strength
        )
        
        self._links[link_id] = link
        self._link_index[(source_id, target_id)] = link_id
        
        if link_type == HebbianLinkType.SYMMETRIC:
            self._link_index[(target_id, source_id)] = link_id
        
        self._stats["links_created"] += 1
        logger.debug(f"Created Hebbian link: {source_id} <-> {target_id}")
        
        return link
    
    def _apply_ltp(self, link: HebbianLink):
        """Apply Long-Term Potentiation (strengthening)"""
        # Hebbian learning: strength increases with co-activation
        delta = self._config.ltp_rate * (self._config.max_strength - link.strength)
        
        link.strength = min(self._config.max_strength, link.strength + delta)
        link.update_count += 1
        link.total_coactivations += 1
        link.recent_coactivations += 1
        link.last_update = time.time()
        
        self._stats["ltp_events"] += 1
    
    def apply_ltd(self, source_id: str, target_id: str):
        """
        Apply Long-Term Depression (weakening).
        
        Called when atoms are activated but not together.
        """
        with self._lock:
            link_key = (source_id, target_id)
            reverse_key = (target_id, source_id)
            
            link_id = self._link_index.get(link_key) or self._link_index.get(reverse_key)
            
            if link_id:
                link = self._links.get(link_id)
                if link:
                    # Decrease strength
                    delta = self._config.ltd_rate * link.strength
                    link.strength = max(self._config.min_strength, link.strength - delta)
                    link.update_count += 1
                    link.last_update = time.time()
                    
                    self._stats["ltd_events"] += 1
    
    def _decay_recent_activations(self):
        """Decay the recent activation counts"""
        now = time.time()
        cutoff = now - (self._config.coactivation_window_ms / 1000.0) * 2
        
        # Remove old activations
        to_remove = [
            atom_id for atom_id, last_time in self._recent_activations.items()
            if last_time < cutoff
        ]
        
        for atom_id in to_remove:
            del self._recent_activations[atom_id]
        
        # Decay link recent coactivation counts
        for link in self._links.values():
            link.recent_coactivations = int(
                link.recent_coactivations * self._config.coactivation_decay_factor
            )
    
    def decay_all_links(self):
        """Apply decay to all links"""
        with self._lock:
            now = time.time()
            elapsed = now - self._last_decay
            
            if elapsed < self._config.decay_interval_s:
                return
            
            self._last_decay = now
            
            decay = self._config.decay_rate * elapsed
            
            for link in self._links.values():
                link.strength = max(
                    self._config.min_strength,
                    link.strength - decay
                )
                link.last_decay = now
    
    def prune_weak_links(self) -> int:
        """
        Remove links below strength threshold.
        
        Returns:
            Number of links pruned
        """
        with self._lock:
            now = time.time()
            elapsed = now - self._last_prune
            
            if elapsed < self._config.prune_interval_s:
                return 0
            
            self._last_prune = now
            
            to_prune = [
                link_id for link_id, link in self._links.items()
                if link.strength < self._config.prune_threshold
            ]
            
            for link_id in to_prune:
                link = self._links.pop(link_id)
                
                # Remove from index
                key = (link.source_id, link.target_id)
                self._link_index.pop(key, None)
                
                if link.link_type == HebbianLinkType.SYMMETRIC:
                    reverse_key = (link.target_id, link.source_id)
                    self._link_index.pop(reverse_key, None)
            
            self._stats["links_pruned"] += len(to_prune)
            
            if to_prune:
                logger.debug(f"Pruned {len(to_prune)} weak Hebbian links")
            
            return len(to_prune)
    
    def get_link(self, source_id: str, target_id: str) -> Optional[HebbianLink]:
        """Get a specific Hebbian link"""
        with self._lock:
            link_key = (source_id, target_id)
            reverse_key = (target_id, source_id)
            
            link_id = self._link_index.get(link_key) or self._link_index.get(reverse_key)
            
            return self._links.get(link_id) if link_id else None
    
    def get_links_for_atom(self, atom_id: str) -> List[HebbianLink]:
        """Get all Hebbian links involving an atom"""
        with self._lock:
            return [
                link for link in self._links.values()
                if link.source_id == atom_id or link.target_id == atom_id
            ]
    
    def get_strongest_links(
        self,
        atom_id: str,
        n: int = 10
    ) -> List[HebbianLink]:
        """Get the n strongest links for an atom"""
        links = self.get_links_for_atom(atom_id)
        links.sort(key=lambda l: -l.strength)
        return links[:n]
    
    def get_link_strength_to(
        self,
        source_id: str,
        target_id: str
    ) -> float:
        """Get link strength between two atoms"""
        link = self.get_link(source_id, target_id)
        return link.strength if link else 0.0
    
    def list_links(
        self,
        min_strength: Optional[float] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """List Hebbian links with optional filtering"""
        with self._lock:
            links = list(self._links.values())
            
            if min_strength is not None:
                links = [l for l in links if l.strength >= min_strength]
            
            # Sort by strength descending
            links.sort(key=lambda l: -l.strength)
            
            return [l.to_dict() for l in links[:limit]]
    
    def simulate_learning_cycle(
        self,
        active_atoms: List[str]
    ) -> Dict[str, Any]:
        """
        Simulate a learning cycle with multiple co-activated atoms.
        
        Args:
            active_atoms: List of simultaneously active atoms
            
        Returns:
            Summary of learning events
        """
        ltp_count = 0
        links_created = 0
        
        # Record activation for each atom
        for atom_id in active_atoms:
            affected = self.record_activation(atom_id)
            ltp_count += len(affected)
        
        # Apply LTD for non-co-activated pairs
        for atom_id in active_atoms:
            for link in self.get_links_for_atom(atom_id):
                other_id = link.target_id if link.source_id == atom_id else link.source_id
                if other_id not in active_atoms:
                    self.apply_ltd(atom_id, other_id)
        
        # Decay and prune
        self.decay_all_links()
        pruned = self.prune_weak_links()
        
        return {
            "active_atoms": len(active_atoms),
            "ltp_events": ltp_count,
            "links_pruned": pruned,
            "total_links": len(self._links)
        }
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get Hebbian manager statistics"""
        with self._lock:
            strengths = [l.strength for l in self._links.values()]
            
            return {
                **self._stats,
                "total_links": len(self._links),
                "avg_strength": sum(strengths) / len(strengths) if strengths else 0,
                "max_strength": max(strengths) if strengths else 0,
                "min_strength": min(strengths) if strengths else 0,
                "recent_activations": len(self._recent_activations)
            }


# Global Hebbian manager
_hebbian_manager: Optional[HebbianManager] = None


def get_hebbian_manager() -> HebbianManager:
    """Get the global Hebbian manager instance"""
    global _hebbian_manager
    if _hebbian_manager is None:
        _hebbian_manager = HebbianManager()
    return _hebbian_manager


if __name__ == "__main__":
    import json
    from core.ecan.attention_bank import get_attention_bank
    
    print("=== ECAN Hebbian Learning Examples ===\n")
    
    # Set up attention bank
    bank = get_attention_bank()
    
    # Initialize atoms with attention
    bank.set("concept_cat", sti=50.0)
    bank.set("concept_fur", sti=40.0)
    bank.set("concept_meow", sti=35.0)
    bank.set("concept_pet", sti=45.0)
    bank.set("concept_dog", sti=50.0)
    bank.set("concept_bark", sti=30.0)
    
    # Set up Hebbian manager
    hebbian = get_hebbian_manager()
    
    # Simulate learning: cat-related concepts activated together
    print("=== Learning Cycle 1: Cat concepts ===")
    result = hebbian.simulate_learning_cycle(["concept_cat", "concept_fur", "concept_meow"])
    print(f"Result: {result}")
    
    print("\n=== Learning Cycle 2: More cat concepts ===")
    result = hebbian.simulate_learning_cycle(["concept_cat", "concept_fur", "concept_pet"])
    print(f"Result: {result}")
    
    print("\n=== Learning Cycle 3: Dog concepts ===")
    result = hebbian.simulate_learning_cycle(["concept_dog", "concept_bark", "concept_pet"])
    print(f"Result: {result}")
    
    # Check links
    print("\n=== Hebbian Links for 'concept_cat' ===")
    for link in hebbian.get_strongest_links("concept_cat", 5):
        other = link.target_id if link.source_id == "concept_cat" else link.source_id
        print(f"  {other}: strength={link.strength:.3f}, coactivations={link.total_coactivations}")
    
    print("\n=== All Links (sorted by strength) ===")
    for link_dict in hebbian.list_links(limit=10):
        print(f"  {link_dict['source_id']} <-> {link_dict['target_id']}: "
              f"strength={link_dict['strength']:.3f}")
    
    # Statistics
    print("\n=== Statistics ===")
    print(json.dumps(hebbian.get_statistics(), indent=2))
