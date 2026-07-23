#!/usr/bin/env python3
"""
AGI-OS ECAN (Economic Attention Network) Integration Package

This package provides attention allocation mechanisms for the OpenCog AGI-OS,
enabling resource-bounded cognition through attention-based prioritization.

Modules:
- attention_bank: Attention value storage and management
- importance_spreading: STI/LTI spreading algorithms
- hebbian: Hebbian link creation and management
"""

from core.ecan.attention_bank import (
    AttentionBank,
    AttentionValue,
    AttentionConfig,
    get_attention_bank
)

from core.ecan.importance_spreading import (
    ImportanceSpreader,
    SpreadingConfig,
    SpreadingMode,
    get_importance_spreader
)

from core.ecan.hebbian import (
    HebbianManager,
    HebbianLink,
    HebbianConfig,
    get_hebbian_manager
)

__all__ = [
    # Attention Bank
    "AttentionBank",
    "AttentionValue",
    "AttentionConfig",
    "get_attention_bank",
    
    # Importance Spreading
    "ImportanceSpreader",
    "SpreadingConfig",
    "SpreadingMode",
    "get_importance_spreader",
    
    # Hebbian Learning
    "HebbianManager",
    "HebbianLink",
    "HebbianConfig",
    "get_hebbian_manager",
]
