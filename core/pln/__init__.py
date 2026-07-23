#!/usr/bin/env python3
"""
AGI-OS PLN (Probabilistic Logic Networks) Integration Package

This package provides PLN integration for the OpenCog AGI-OS,
enabling probabilistic reasoning with attention-based inference control.

Modules:
- inference_scheduler: PLN inference scheduling with AGI_Scheduler
- rule_executor: Rule execution engine
- truth_value: Truth value computation and propagation
"""

from core.pln.truth_value import (
    TruthValue,
    TruthValueType,
    SimpleTruthValue,
    IndefiniteTruthValue,
    DistributionalTruthValue,
    merge_truth_values,
    revision,
    deduction,
    induction
)

from core.pln.rule_executor import (
    RuleExecutor,
    Rule,
    RuleType,
    RuleResult,
    get_rule_executor
)

from core.pln.inference_scheduler import (
    InferenceScheduler,
    InferenceTask,
    InferenceMode,
    get_inference_scheduler
)

__all__ = [
    # Truth Values
    "TruthValue",
    "TruthValueType",
    "SimpleTruthValue",
    "IndefiniteTruthValue",
    "DistributionalTruthValue",
    "merge_truth_values",
    "revision",
    "deduction",
    "induction",
    
    # Rule Executor
    "RuleExecutor",
    "Rule",
    "RuleType",
    "RuleResult",
    "get_rule_executor",
    
    # Inference Scheduler
    "InferenceScheduler",
    "InferenceTask",
    "InferenceMode",
    "get_inference_scheduler",
]
