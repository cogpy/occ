#!/usr/bin/env python3
"""
Importance Spreading for ECAN

This module implements importance (STI/LTI) spreading algorithms for the
Economic Attention Network. It propagates attention through the hypergraph
based on link structure and weights.

Features:
- Hebbian-weighted importance spreading
- Multiple spreading modes (diffusion, tournament, hybrid)
- Attention focus spreading
- Spreading depth control
- Integration with attention bank
"""

import time
import logging
import threading
from typing import Dict, Any, Optional, List, Set, Tuple, Callable
from dataclasses import dataclass, field
from enum import Enum
import random

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("AGI_ECAN.ImportanceSpreading")


class SpreadingMode(Enum):
    """Modes for importance spreading"""
    DIFFUSION = "diffusion"       # Even spreading to neighbors
    TOURNAMENT = "tournament"     # Competitive selection
    HEBBIAN = "hebbian"           # Weight-based spreading
    HYBRID = "hybrid"             # Combination approach


@dataclass
class SpreadingConfig:
    """Configuration for importance spreading"""
    # Spreading parameters
    spread_fraction: float = 0.5      # Fraction of STI to spread
    max_spread_depth: int = 3          # Maximum spreading depth
    spreading_threshold: float = 1.0   # Minimum STI to spread
    
    # Mode-specific parameters
    diffusion_decay: float = 0.9       # Decay per hop in diffusion
    tournament_size: int = 3           # Candidates in tournament selection
    hebbian_weight_factor: float = 0.5 # Weight influence in Hebbian mode
    
    # Filtering
    max_neighbors: int = 50            # Max neighbors to consider
    min_neighbor_sti: float = -50.0    # Minimum neighbor STI to receive
    
    # Rate limiting
    spread_interval_ms: float = 100.0  # Minimum time between spreads


@dataclass
class SpreadEvent:
    """Record of a spreading event"""
    source_id: str
    target_id: str
    amount: float
    mode: SpreadingMode
    depth: int
    timestamp: float = field(default_factory=time.time)


class HypergraphAdapter:
    """
    Adapter interface for hypergraph access.
    
    In a real implementation, this would connect to AtomSpace.
    This is a simplified version for demonstration.
    """
    
    def __init__(self):
        self._adjacency: Dict[str, List[Tuple[str, float]]] = {}
        self._link_weights: Dict[Tuple[str, str], float] = {}
    
    def add_link(
        self,
        source: str,
        target: str,
        weight: float = 1.0,
        bidirectional: bool = True
    ):
        """Add a link between atoms"""
        if source not in self._adjacency:
            self._adjacency[source] = []
        self._adjacency[source].append((target, weight))
        self._link_weights[(source, target)] = weight
        
        if bidirectional:
            if target not in self._adjacency:
                self._adjacency[target] = []
            self._adjacency[target].append((source, weight))
            self._link_weights[(target, source)] = weight
    
    def get_neighbors(
        self,
        atom_id: str,
        max_count: Optional[int] = None
    ) -> List[Tuple[str, float]]:
        """Get neighbors of an atom with link weights"""
        neighbors = self._adjacency.get(atom_id, [])
        if max_count and len(neighbors) > max_count:
            return neighbors[:max_count]
        return neighbors
    
    def get_link_weight(self, source: str, target: str) -> float:
        """Get weight of a link"""
        return self._link_weights.get((source, target), 0.0)


class ImportanceSpreader:
    """
    Importance Spreader for ECAN
    
    Spreads attention importance (STI) through the hypergraph
    based on link structure and configured spreading mode.
    """
    
    def __init__(
        self,
        attention_bank = None,
        hypergraph: Optional[HypergraphAdapter] = None,
        config: Optional[SpreadingConfig] = None
    ):
        """
        Initialize the importance spreader.
        
        Args:
            attention_bank: AttentionBank instance
            hypergraph: Hypergraph adapter
            config: Spreading configuration
        """
        self._lock = threading.RLock()
        
        # Lazy import to avoid circular dependency
        self._attention_bank = attention_bank
        self._hypergraph = hypergraph or HypergraphAdapter()
        self._config = config or SpreadingConfig()
        
        # Spreading history
        self._spread_history: List[SpreadEvent] = []
        self._last_spread_time: Dict[str, float] = {}
        
        # Statistics
        self._stats = {
            "spread_cycles": 0,
            "total_spread": 0.0,
            "atoms_affected": set()
        }
        
        logger.info("ImportanceSpreader initialized")
    
    def _get_attention_bank(self):
        """Lazy getter for attention bank"""
        if self._attention_bank is None:
            from core.ecan.attention_bank import get_attention_bank
            self._attention_bank = get_attention_bank()
        return self._attention_bank
    
    def spread(
        self,
        source_id: str,
        mode: Optional[SpreadingMode] = None,
        depth: int = 1
    ) -> List[SpreadEvent]:
        """
        Spread importance from a source atom.
        
        Args:
            source_id: Source atom ID
            mode: Spreading mode (uses config default if None)
            depth: Current spreading depth
            
        Returns:
            List of spreading events
        """
        with self._lock:
            mode = mode or SpreadingMode.DIFFUSION
            events = []
            
            # Rate limiting
            now = time.time()
            last_spread = self._last_spread_time.get(source_id, 0)
            if (now - last_spread) * 1000 < self._config.spread_interval_ms:
                return events
            
            # Get source attention
            bank = self._get_attention_bank()
            source_av = bank.get(source_id)
            
            if source_av is None or source_av.sti < self._config.spreading_threshold:
                return events
            
            # Calculate amount to spread
            spread_amount = source_av.sti * self._config.spread_fraction
            
            # Get neighbors
            neighbors = self._hypergraph.get_neighbors(
                source_id,
                self._config.max_neighbors
            )
            
            if not neighbors:
                return events
            
            # Spread based on mode
            if mode == SpreadingMode.DIFFUSION:
                events = self._diffusion_spread(
                    source_id, spread_amount, neighbors, depth
                )
            elif mode == SpreadingMode.TOURNAMENT:
                events = self._tournament_spread(
                    source_id, spread_amount, neighbors, depth
                )
            elif mode == SpreadingMode.HEBBIAN:
                events = self._hebbian_spread(
                    source_id, spread_amount, neighbors, depth
                )
            elif mode == SpreadingMode.HYBRID:
                events = self._hybrid_spread(
                    source_id, spread_amount, neighbors, depth
                )
            
            # Deduct from source
            if events:
                total_spread = sum(e.amount for e in events)
                bank.set(source_id, sti=source_av.sti - total_spread)
                
                self._last_spread_time[source_id] = now
                self._stats["spread_cycles"] += 1
                self._stats["total_spread"] += total_spread
                
                # Record history
                self._spread_history.extend(events)
                
                # Recursive spreading
                if depth < self._config.max_spread_depth:
                    for event in events:
                        self._stats["atoms_affected"].add(event.target_id)
                        # Continue spreading from targets
                        sub_events = self.spread(event.target_id, mode, depth + 1)
                        events.extend(sub_events)
            
            return events
    
    def _diffusion_spread(
        self,
        source_id: str,
        spread_amount: float,
        neighbors: List[Tuple[str, float]],
        depth: int
    ) -> List[SpreadEvent]:
        """Even diffusion spreading to all neighbors"""
        events = []
        bank = self._get_attention_bank()
        
        # Apply decay based on depth
        decayed_amount = spread_amount * (self._config.diffusion_decay ** depth)
        
        # Even distribution with weight adjustment
        total_weight = sum(w for _, w in neighbors)
        
        for target_id, weight in neighbors:
            target_av = bank.get(target_id)
            if target_av and target_av.sti < self._config.min_neighbor_sti:
                continue
            
            # Proportional to link weight
            share = (weight / total_weight) * decayed_amount if total_weight > 0 else decayed_amount / len(neighbors)
            
            # Apply to target
            current_sti = target_av.sti if target_av else 0.0
            bank.set(target_id, sti=current_sti + share)
            
            events.append(SpreadEvent(
                source_id=source_id,
                target_id=target_id,
                amount=share,
                mode=SpreadingMode.DIFFUSION,
                depth=depth
            ))
        
        return events
    
    def _tournament_spread(
        self,
        source_id: str,
        spread_amount: float,
        neighbors: List[Tuple[str, float]],
        depth: int
    ) -> List[SpreadEvent]:
        """Tournament selection - competitive spreading"""
        events = []
        bank = self._get_attention_bank()
        
        # Select tournament winners
        tournament_size = min(self._config.tournament_size, len(neighbors))
        remaining_amount = spread_amount
        
        while remaining_amount > 0.1 and neighbors:
            # Random tournament selection
            candidates = random.sample(neighbors, min(tournament_size, len(neighbors)))
            
            # Winner is highest weighted
            winner = max(candidates, key=lambda x: x[1])
            target_id, weight = winner
            
            target_av = bank.get(target_id)
            if target_av and target_av.sti < self._config.min_neighbor_sti:
                neighbors.remove(winner)
                continue
            
            # Winner takes a portion
            share = min(remaining_amount * 0.5, remaining_amount)
            current_sti = target_av.sti if target_av else 0.0
            bank.set(target_id, sti=current_sti + share)
            
            events.append(SpreadEvent(
                source_id=source_id,
                target_id=target_id,
                amount=share,
                mode=SpreadingMode.TOURNAMENT,
                depth=depth
            ))
            
            remaining_amount -= share
            neighbors.remove(winner)
        
        return events
    
    def _hebbian_spread(
        self,
        source_id: str,
        spread_amount: float,
        neighbors: List[Tuple[str, float]],
        depth: int
    ) -> List[SpreadEvent]:
        """Hebbian weight-based spreading"""
        events = []
        bank = self._get_attention_bank()
        
        # Get source STI for correlation
        source_av = bank.get(source_id)
        source_sti = source_av.sti if source_av else 0.0
        
        # Calculate Hebbian weights
        hebbian_weights = []
        for target_id, link_weight in neighbors:
            target_av = bank.get(target_id)
            if target_av and target_av.sti < self._config.min_neighbor_sti:
                continue
            
            target_sti = target_av.sti if target_av else 0.0
            
            # Hebbian correlation: proportional to both activities
            hebbian_factor = abs(source_sti * target_sti) * self._config.hebbian_weight_factor
            combined_weight = link_weight * (1 + hebbian_factor)
            
            hebbian_weights.append((target_id, combined_weight))
        
        if not hebbian_weights:
            return events
        
        # Normalize and distribute
        total_weight = sum(w for _, w in hebbian_weights)
        
        for target_id, weight in hebbian_weights:
            share = (weight / total_weight) * spread_amount if total_weight > 0 else spread_amount / len(hebbian_weights)
            
            target_av = bank.get(target_id)
            current_sti = target_av.sti if target_av else 0.0
            bank.set(target_id, sti=current_sti + share)
            
            events.append(SpreadEvent(
                source_id=source_id,
                target_id=target_id,
                amount=share,
                mode=SpreadingMode.HEBBIAN,
                depth=depth
            ))
        
        return events
    
    def _hybrid_spread(
        self,
        source_id: str,
        spread_amount: float,
        neighbors: List[Tuple[str, float]],
        depth: int
    ) -> List[SpreadEvent]:
        """Hybrid spreading combining multiple modes"""
        # Split amount between diffusion and Hebbian
        diffusion_amount = spread_amount * 0.5
        hebbian_amount = spread_amount * 0.5
        
        events = []
        
        # Diffusion to low-attention neighbors
        events.extend(self._diffusion_spread(
            source_id, diffusion_amount, neighbors, depth
        ))
        
        # Hebbian to high-correlation neighbors
        events.extend(self._hebbian_spread(
            source_id, hebbian_amount, neighbors, depth
        ))
        
        return events
    
    def spread_from_focus(
        self,
        mode: Optional[SpreadingMode] = None
    ) -> int:
        """
        Spread from all atoms in attentional focus.
        
        Returns:
            Number of spreading events
        """
        bank = self._get_attention_bank()
        focus = bank.get_attentional_focus()
        
        total_events = 0
        for atom_id, av in focus:
            events = self.spread(atom_id, mode)
            total_events += len(events)
        
        return total_events
    
    def get_spread_history(
        self,
        source_id: Optional[str] = None,
        limit: int = 100
    ) -> List[Dict[str, Any]]:
        """Get spreading history"""
        with self._lock:
            history = self._spread_history
            
            if source_id:
                history = [e for e in history if e.source_id == source_id]
            
            return [
                {
                    "source_id": e.source_id,
                    "target_id": e.target_id,
                    "amount": e.amount,
                    "mode": e.mode.value,
                    "depth": e.depth,
                    "timestamp": e.timestamp
                }
                for e in history[-limit:]
            ]
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get spreader statistics"""
        with self._lock:
            return {
                "spread_cycles": self._stats["spread_cycles"],
                "total_spread": self._stats["total_spread"],
                "atoms_affected": len(self._stats["atoms_affected"]),
                "history_size": len(self._spread_history)
            }


# Global importance spreader
_importance_spreader: Optional[ImportanceSpreader] = None


def get_importance_spreader() -> ImportanceSpreader:
    """Get the global importance spreader instance"""
    global _importance_spreader
    if _importance_spreader is None:
        _importance_spreader = ImportanceSpreader()
    return _importance_spreader


if __name__ == "__main__":
    import json
    from core.ecan.attention_bank import get_attention_bank
    
    print("=== ECAN Importance Spreading Examples ===\n")
    
    # Set up attention bank
    bank = get_attention_bank()
    
    # Initialize some atoms
    bank.set("concept_cat", sti=100.0, lti=20.0)
    bank.set("concept_animal", sti=50.0, lti=30.0)
    bank.set("concept_fur", sti=30.0, lti=10.0)
    bank.set("concept_mammal", sti=40.0, lti=25.0)
    bank.set("concept_pet", sti=35.0, lti=15.0)
    
    # Set up spreader with hypergraph
    spreader = get_importance_spreader()
    
    # Add links
    spreader._hypergraph.add_link("concept_cat", "concept_animal", weight=0.9)
    spreader._hypergraph.add_link("concept_cat", "concept_fur", weight=0.7)
    spreader._hypergraph.add_link("concept_cat", "concept_mammal", weight=0.85)
    spreader._hypergraph.add_link("concept_cat", "concept_pet", weight=0.8)
    spreader._hypergraph.add_link("concept_animal", "concept_mammal", weight=0.6)
    
    print("=== Initial Attention ===")
    for atom_id, av in bank.get_top_atoms(5):
        print(f"  {atom_id}: STI={av.sti:.1f}")
    
    # Spread from cat
    print("\n=== Diffusion Spreading from 'concept_cat' ===")
    events = spreader.spread("concept_cat", SpreadingMode.DIFFUSION)
    print(f"Spreading events: {len(events)}")
    for event in events:
        print(f"  {event.source_id} -> {event.target_id}: {event.amount:.2f}")
    
    print("\n=== Attention After Spreading ===")
    for atom_id, av in bank.get_top_atoms(5):
        print(f"  {atom_id}: STI={av.sti:.1f}")
    
    # Hebbian spreading
    print("\n=== Hebbian Spreading from 'concept_animal' ===")
    events = spreader.spread("concept_animal", SpreadingMode.HEBBIAN)
    print(f"Spreading events: {len(events)}")
    
    print("\n=== Final Attention ===")
    for atom_id, av in bank.get_top_atoms(5):
        print(f"  {atom_id}: STI={av.sti:.1f}")
    
    # Statistics
    print("\n=== Statistics ===")
    print(json.dumps(spreader.get_statistics(), indent=2))
