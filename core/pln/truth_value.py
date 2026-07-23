#!/usr/bin/env python3
"""
Truth Value System for PLN

This module implements the truth value system for Probabilistic Logic Networks.
Truth values represent uncertain knowledge with strength (probability) and
confidence (amount of evidence).

Features:
- Simple Truth Values (strength, confidence)
- Indefinite Truth Values (interval-based uncertainty)
- Distributional Truth Values (probability distributions)
- Truth value formulas (revision, deduction, induction, etc.)
- Attention-weighted truth value merging
"""

import math
import logging
from typing import Dict, Any, Optional, List, Tuple, Union
from dataclasses import dataclass, field
from enum import Enum
from abc import ABC, abstractmethod

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("AGI_PLN.TruthValue")


# Constants for truth value calculations
DEFAULT_K = 800  # Default "conceptual factor" for PLN
INDEFINITE_CONFIDENCE_EXPONENT = 2.0


class TruthValueType(Enum):
    """Types of truth values"""
    SIMPLE = "simple"                     # (strength, confidence)
    INDEFINITE = "indefinite"             # (L, U, confidence_level, prior)
    DISTRIBUTIONAL = "distributional"     # (histogram of probabilities)
    COUNT = "count"                        # (positive_count, total_count)


@dataclass
class TruthValue(ABC):
    """
    Abstract base class for truth values.
    
    A truth value represents uncertain knowledge about an atom's truth.
    """
    
    @property
    @abstractmethod
    def mean(self) -> float:
        """Get the mean strength (probability)"""
        pass
    
    @property
    @abstractmethod
    def confidence(self) -> float:
        """Get the confidence (0-1)"""
        pass
    
    @property
    @abstractmethod
    def tv_type(self) -> TruthValueType:
        """Get the truth value type"""
        pass
    
    @abstractmethod
    def to_dict(self) -> Dict[str, Any]:
        """Convert to dictionary"""
        pass
    
    def to_count(self, k: float = DEFAULT_K) -> Tuple[float, float]:
        """
        Convert to count representation (positive_count, total_count).
        
        Args:
            k: Conceptual factor
            
        Returns:
            (positive_count, total_count)
        """
        n = self.confidence * k / (1 - self.confidence) if self.confidence < 1 else k * 100
        p = self.mean * n
        return (p, n)
    
    @classmethod
    def from_count(cls, positive: float, total: float, k: float = DEFAULT_K) -> "SimpleTruthValue":
        """
        Create a truth value from count evidence.
        
        Args:
            positive: Positive evidence count
            total: Total evidence count
            k: Conceptual factor
            
        Returns:
            SimpleTruthValue
        """
        strength = positive / total if total > 0 else 0.5
        confidence = total / (total + k)
        return SimpleTruthValue(strength=strength, confidence=confidence)


@dataclass
class SimpleTruthValue(TruthValue):
    """
    Simple Truth Value with strength and confidence.
    
    - strength: Probability estimate (0-1)
    - confidence: Weight of evidence (0-1)
    """
    strength: float = 0.0
    confidence_value: float = 0.0
    
    def __post_init__(self):
        # Clamp values to valid range
        self.strength = max(0.0, min(1.0, self.strength))
        self.confidence_value = max(0.0, min(1.0, self.confidence_value))
    
    @property
    def mean(self) -> float:
        return self.strength
    
    @property
    def confidence(self) -> float:
        return self.confidence_value
    
    @property
    def tv_type(self) -> TruthValueType:
        return TruthValueType.SIMPLE
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "type": "simple",
            "strength": self.strength,
            "confidence": self.confidence_value
        }
    
    def __repr__(self) -> str:
        return f"STV({self.strength:.3f}, {self.confidence_value:.3f})"
    
    def __eq__(self, other: object) -> bool:
        if not isinstance(other, SimpleTruthValue):
            return False
        return (abs(self.strength - other.strength) < 1e-6 and
                abs(self.confidence_value - other.confidence_value) < 1e-6)


@dataclass
class IndefiniteTruthValue(TruthValue):
    """
    Indefinite Truth Value with interval-based uncertainty.
    
    Represents uncertainty as an interval [L, U] with a confidence level.
    """
    lower: float = 0.0          # Lower bound L
    upper: float = 1.0          # Upper bound U
    confidence_level: float = 0.9  # Confidence in interval
    prior: float = 0.5          # Prior probability
    
    def __post_init__(self):
        self.lower = max(0.0, min(1.0, self.lower))
        self.upper = max(0.0, min(1.0, self.upper))
        if self.lower > self.upper:
            self.lower, self.upper = self.upper, self.lower
        self.confidence_level = max(0.0, min(1.0, self.confidence_level))
        self.prior = max(0.0, min(1.0, self.prior))
    
    @property
    def mean(self) -> float:
        """Interval midpoint as mean"""
        return (self.lower + self.upper) / 2
    
    @property
    def confidence(self) -> float:
        """Confidence based on interval width"""
        width = self.upper - self.lower
        # Narrower interval = higher confidence
        return (1 - width) ** INDEFINITE_CONFIDENCE_EXPONENT * self.confidence_level
    
    @property
    def tv_type(self) -> TruthValueType:
        return TruthValueType.INDEFINITE
    
    @property
    def width(self) -> float:
        """Interval width"""
        return self.upper - self.lower
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "type": "indefinite",
            "lower": self.lower,
            "upper": self.upper,
            "confidence_level": self.confidence_level,
            "prior": self.prior
        }
    
    def to_simple(self) -> SimpleTruthValue:
        """Convert to a simple truth value"""
        return SimpleTruthValue(
            strength=self.mean,
            confidence_value=self.confidence
        )
    
    def __repr__(self) -> str:
        return f"ITV([{self.lower:.3f}, {self.upper:.3f}], {self.confidence_level:.2f})"


@dataclass
class DistributionalTruthValue(TruthValue):
    """
    Distributional Truth Value using a probability histogram.
    
    The distribution represents uncertainty over possible strength values.
    """
    histogram: List[float] = field(default_factory=lambda: [1.0])  # Probability buckets
    
    def __post_init__(self):
        # Normalize histogram
        total = sum(self.histogram)
        if total > 0:
            self.histogram = [h / total for h in self.histogram]
        else:
            self.histogram = [1.0 / len(self.histogram)] * len(self.histogram)
    
    @property
    def mean(self) -> float:
        """Expected value of the distribution"""
        n = len(self.histogram)
        bucket_width = 1.0 / n
        total = 0.0
        for i, prob in enumerate(self.histogram):
            bucket_center = (i + 0.5) * bucket_width
            total += prob * bucket_center
        return total
    
    @property
    def variance(self) -> float:
        """Variance of the distribution"""
        m = self.mean
        n = len(self.histogram)
        bucket_width = 1.0 / n
        var = 0.0
        for i, prob in enumerate(self.histogram):
            bucket_center = (i + 0.5) * bucket_width
            var += prob * (bucket_center - m) ** 2
        return var
    
    @property
    def confidence(self) -> float:
        """Confidence based on distribution entropy"""
        # Higher entropy = lower confidence
        entropy = 0.0
        for prob in self.histogram:
            if prob > 0:
                entropy -= prob * math.log2(prob)
        
        # Normalize by max entropy
        max_entropy = math.log2(len(self.histogram))
        if max_entropy > 0:
            normalized_entropy = entropy / max_entropy
        else:
            normalized_entropy = 0.0
        
        return 1.0 - normalized_entropy
    
    @property
    def tv_type(self) -> TruthValueType:
        return TruthValueType.DISTRIBUTIONAL
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "type": "distributional",
            "histogram": self.histogram,
            "mean": self.mean,
            "variance": self.variance
        }
    
    def to_simple(self) -> SimpleTruthValue:
        """Convert to a simple truth value"""
        return SimpleTruthValue(
            strength=self.mean,
            confidence_value=self.confidence
        )
    
    def to_indefinite(self, confidence_level: float = 0.9) -> IndefiniteTruthValue:
        """Convert to indefinite truth value"""
        # Find interval containing confidence_level of probability mass
        cumulative = 0.0
        lower_idx = 0
        upper_idx = len(self.histogram) - 1
        
        target_low = (1 - confidence_level) / 2
        target_high = 1 - target_low
        
        for i, prob in enumerate(self.histogram):
            cumulative += prob
            if cumulative >= target_low and lower_idx == 0:
                lower_idx = i
            if cumulative >= target_high:
                upper_idx = i
                break
        
        bucket_width = 1.0 / len(self.histogram)
        lower = lower_idx * bucket_width
        upper = (upper_idx + 1) * bucket_width
        
        return IndefiniteTruthValue(
            lower=lower,
            upper=upper,
            confidence_level=confidence_level
        )
    
    def __repr__(self) -> str:
        return f"DTV(mean={self.mean:.3f}, var={self.variance:.3f})"


# PLN Formulas

def revision(tv1: TruthValue, tv2: TruthValue, k: float = DEFAULT_K) -> SimpleTruthValue:
    """
    PLN Revision Rule.
    
    Combines two truth values for the same statement from independent sources.
    
    Args:
        tv1: First truth value
        tv2: Second truth value
        k: Conceptual factor
        
    Returns:
        Revised truth value
    """
    # Convert to count space
    p1, n1 = tv1.to_count(k)
    p2, n2 = tv2.to_count(k)
    
    # Add counts (assuming independence)
    p = p1 + p2
    n = n1 + n2
    
    return TruthValue.from_count(p, n, k)


def deduction(
    tv_ab: TruthValue,  # P(B|A)
    tv_bc: TruthValue,  # P(C|B)
    tv_a: TruthValue = None,   # P(A)
    tv_b: TruthValue = None,   # P(B)
    tv_c: TruthValue = None,   # P(C)
) -> SimpleTruthValue:
    """
    PLN Deduction Rule.
    
    Given A→B and B→C, infer A→C.
    
    Args:
        tv_ab: Truth value for A→B
        tv_bc: Truth value for B→C
        tv_a: Prior for A (optional)
        tv_b: Prior for B (optional)
        tv_c: Prior for C (optional)
        
    Returns:
        Truth value for A→C
    """
    sAB = tv_ab.mean
    sBC = tv_bc.mean
    
    # Default priors
    sA = tv_a.mean if tv_a else 0.5
    sB = tv_b.mean if tv_b else 0.5
    sC = tv_c.mean if tv_c else 0.5
    
    # Deduction formula
    if sB > 0:
        sAC = sAB * sBC + (1 - sAB) * (sC - sB * sBC) / (1 - sB) if sB < 1 else sAB * sBC
    else:
        sAC = sC
    
    # Clamp to valid range
    sAC = max(0.0, min(1.0, sAC))
    
    # Confidence is minimum of inputs, scaled down
    conf = min(tv_ab.confidence, tv_bc.confidence) * 0.9
    
    return SimpleTruthValue(strength=sAC, confidence_value=conf)


def induction(
    tv_ab: TruthValue,  # P(B|A)
    tv_ac: TruthValue,  # P(C|A)
    tv_a: TruthValue = None,   # P(A)
    tv_b: TruthValue = None,   # P(B)
    tv_c: TruthValue = None,   # P(C)
) -> SimpleTruthValue:
    """
    PLN Induction Rule.
    
    Given A→B and A→C, infer B→C.
    
    Args:
        tv_ab: Truth value for A→B
        tv_ac: Truth value for A→C
        tv_a: Prior for A (optional)
        tv_b: Prior for B (optional)
        tv_c: Prior for C (optional)
        
    Returns:
        Truth value for B→C
    """
    sAB = tv_ab.mean
    sAC = tv_ac.mean
    
    # Default priors
    sA = tv_a.mean if tv_a else 0.5
    sB = tv_b.mean if tv_b else 0.5
    sC = tv_c.mean if tv_c else 0.5
    
    # Induction formula (simplified)
    if sB > 0:
        sBC = sA * sAB * sAC / sB + (1 - sA * sAB / sB) * sC if sA * sAB < sB else sAC
    else:
        sBC = sC
    
    sBC = max(0.0, min(1.0, sBC))
    
    # Lower confidence for induction
    conf = min(tv_ab.confidence, tv_ac.confidence) * 0.8
    
    return SimpleTruthValue(strength=sBC, confidence_value=conf)


def abduction(
    tv_ab: TruthValue,  # P(B|A)
    tv_cb: TruthValue,  # P(B|C)
    tv_a: TruthValue = None,
    tv_b: TruthValue = None,
    tv_c: TruthValue = None,
) -> SimpleTruthValue:
    """
    PLN Abduction Rule.
    
    Given A→B and C→B, infer A→C.
    
    Args:
        tv_ab: Truth value for A→B
        tv_cb: Truth value for C→B
        
    Returns:
        Truth value for A→C
    """
    sAB = tv_ab.mean
    sCB = tv_cb.mean
    
    sA = tv_a.mean if tv_a else 0.5
    sB = tv_b.mean if tv_b else 0.5
    sC = tv_c.mean if tv_c else 0.5
    
    # Abduction formula (simplified)
    if sB > 0 and sCB > 0:
        sAC = sAB * sCB * sC / sB + (1 - sAB * sCB / sB) * sC
    else:
        sAC = sC
    
    sAC = max(0.0, min(1.0, sAC))
    
    # Lowest confidence for abduction
    conf = min(tv_ab.confidence, tv_cb.confidence) * 0.7
    
    return SimpleTruthValue(strength=sAC, confidence_value=conf)


def modus_ponens(
    tv_a: TruthValue,   # P(A)
    tv_ab: TruthValue,  # P(B|A)
    tv_b: TruthValue = None,  # Prior P(B)
) -> SimpleTruthValue:
    """
    PLN Modus Ponens.
    
    Given A and A→B, infer B.
    
    Args:
        tv_a: Truth value for A
        tv_ab: Truth value for A→B
        tv_b: Prior for B (optional)
        
    Returns:
        Truth value for B
    """
    sA = tv_a.mean
    sAB = tv_ab.mean
    sB = tv_b.mean if tv_b else 0.5
    
    # Modus ponens formula
    sB_new = sA * sAB + (1 - sA) * sB
    
    conf = min(tv_a.confidence, tv_ab.confidence) * 0.95
    
    return SimpleTruthValue(strength=sB_new, confidence_value=conf)


def and_formula(tvs: List[TruthValue]) -> SimpleTruthValue:
    """
    PLN AND formula.
    
    Computes truth value for conjunction A ∧ B ∧ ... 
    
    Args:
        tvs: List of truth values to conjoin
        
    Returns:
        Truth value for conjunction
    """
    if not tvs:
        return SimpleTruthValue(strength=1.0, confidence_value=0.0)
    
    # Independent assumption: P(A ∧ B) = P(A) * P(B)
    strength = 1.0
    for tv in tvs:
        strength *= tv.mean
    
    # Confidence is minimum
    conf = min(tv.confidence for tv in tvs)
    
    return SimpleTruthValue(strength=strength, confidence_value=conf)


def or_formula(tvs: List[TruthValue]) -> SimpleTruthValue:
    """
    PLN OR formula.
    
    Computes truth value for disjunction A ∨ B ∨ ...
    
    Args:
        tvs: List of truth values to disjoin
        
    Returns:
        Truth value for disjunction
    """
    if not tvs:
        return SimpleTruthValue(strength=0.0, confidence_value=0.0)
    
    # Independent assumption: P(A ∨ B) = 1 - (1-P(A)) * (1-P(B))
    neg_product = 1.0
    for tv in tvs:
        neg_product *= (1 - tv.mean)
    
    strength = 1 - neg_product
    conf = min(tv.confidence for tv in tvs)
    
    return SimpleTruthValue(strength=strength, confidence_value=conf)


def not_formula(tv: TruthValue) -> SimpleTruthValue:
    """
    PLN NOT formula.
    
    Computes truth value for negation ¬A.
    
    Args:
        tv: Truth value to negate
        
    Returns:
        Truth value for negation
    """
    return SimpleTruthValue(
        strength=1 - tv.mean,
        confidence_value=tv.confidence
    )


def merge_truth_values(
    tvs: List[TruthValue],
    weights: Optional[List[float]] = None
) -> SimpleTruthValue:
    """
    Merge multiple truth values with optional attention weights.
    
    Args:
        tvs: Truth values to merge
        weights: Attention weights (default: uniform)
        
    Returns:
        Merged truth value
    """
    if not tvs:
        return SimpleTruthValue(strength=0.5, confidence_value=0.0)
    
    if weights is None:
        weights = [1.0] * len(tvs)
    
    # Normalize weights
    total_weight = sum(weights)
    if total_weight > 0:
        weights = [w / total_weight for w in weights]
    else:
        weights = [1.0 / len(tvs)] * len(tvs)
    
    # Weighted average of strength
    strength = sum(w * tv.mean for w, tv in zip(weights, tvs))
    
    # Confidence combines individual confidences
    conf = sum(w * tv.confidence for w, tv in zip(weights, tvs))
    
    return SimpleTruthValue(strength=strength, confidence_value=conf)


if __name__ == "__main__":
    print("=== PLN Truth Value Examples ===\n")
    
    # Simple truth values
    print("=== Simple Truth Values ===")
    tv1 = SimpleTruthValue(strength=0.8, confidence_value=0.9)
    tv2 = SimpleTruthValue(strength=0.6, confidence_value=0.7)
    print(f"TV1: {tv1}")
    print(f"TV2: {tv2}")
    
    # Revision
    print("\n=== Revision ===")
    revised = revision(tv1, tv2)
    print(f"Revised: {revised}")
    
    # Deduction
    print("\n=== Deduction ===")
    tv_ab = SimpleTruthValue(strength=0.9, confidence_value=0.8)
    tv_bc = SimpleTruthValue(strength=0.85, confidence_value=0.75)
    deduced = deduction(tv_ab, tv_bc)
    print(f"A→B: {tv_ab}")
    print(f"B→C: {tv_bc}")
    print(f"A→C (deduced): {deduced}")
    
    # Modus Ponens
    print("\n=== Modus Ponens ===")
    tv_a = SimpleTruthValue(strength=0.95, confidence_value=0.9)
    mp_result = modus_ponens(tv_a, tv_ab)
    print(f"A: {tv_a}")
    print(f"A→B: {tv_ab}")
    print(f"B (modus ponens): {mp_result}")
    
    # Indefinite truth value
    print("\n=== Indefinite Truth Value ===")
    itv = IndefiniteTruthValue(lower=0.6, upper=0.9, confidence_level=0.95)
    print(f"ITV: {itv}")
    print(f"  Mean: {itv.mean:.3f}")
    print(f"  Confidence: {itv.confidence:.3f}")
    print(f"  As Simple: {itv.to_simple()}")
    
    # Distributional truth value
    print("\n=== Distributional Truth Value ===")
    dtv = DistributionalTruthValue(histogram=[0.1, 0.2, 0.4, 0.2, 0.1])
    print(f"DTV: {dtv}")
    print(f"  Mean: {dtv.mean:.3f}")
    print(f"  Variance: {dtv.variance:.5f}")
    print(f"  Confidence: {dtv.confidence:.3f}")
    print(f"  As Indefinite: {dtv.to_indefinite()}")
