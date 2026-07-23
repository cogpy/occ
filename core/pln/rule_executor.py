#!/usr/bin/env python3
"""
Rule Executor for PLN

This module implements the rule execution engine for Probabilistic Logic Networks.
It manages rule definitions, pattern matching, and execution with truth value
propagation.

Features:
- Rule definition and registration
- Pattern matching for rule application
- Forward and backward chaining support
- Attention-based rule selection
- Caching of inference results
"""

import time
import logging
import threading
from typing import Dict, Any, Optional, List, Set, Callable, Tuple
from dataclasses import dataclass, field
from enum import Enum
from collections import defaultdict

from core.pln.truth_value import (
    TruthValue, SimpleTruthValue,
    deduction, induction, abduction, revision,
    modus_ponens, and_formula, or_formula, not_formula
)

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("AGI_PLN.RuleExecutor")


class RuleType(Enum):
    """Types of PLN rules"""
    # Basic rules
    DEDUCTION = "deduction"
    INDUCTION = "induction"
    ABDUCTION = "abduction"
    REVISION = "revision"
    MODUS_PONENS = "modus_ponens"
    
    # Boolean rules
    AND = "and"
    OR = "or"
    NOT = "not"
    
    # Higher-order rules
    IMPLICATION_INTRODUCTION = "implication_introduction"
    EQUIVALENCE_INTRODUCTION = "equivalence_introduction"
    
    # Custom rules
    CUSTOM = "custom"


@dataclass
class Atom:
    """
    Simplified atom representation for PLN inference.
    
    In a real implementation, this would interface with AtomSpace.
    """
    atom_id: str
    atom_type: str
    name: str = ""
    outgoing: List["Atom"] = field(default_factory=list)
    truth_value: Optional[TruthValue] = None
    attention_value: float = 0.0  # STI for attention-based selection
    
    def __hash__(self):
        return hash(self.atom_id)
    
    def __eq__(self, other):
        if not isinstance(other, Atom):
            return False
        return self.atom_id == other.atom_id


@dataclass
class RuleResult:
    """
    Result of a rule execution.
    """
    success: bool
    output_atoms: List[Atom]
    output_tv: Optional[TruthValue] = None
    rule_name: str = ""
    execution_time_ms: float = 0.0
    metadata: Dict[str, Any] = field(default_factory=dict)


@dataclass
class Rule:
    """
    A PLN inference rule.
    """
    name: str
    rule_type: RuleType
    
    # Pattern specification (simplified)
    input_pattern: List[str]  # List of required atom types
    output_pattern: str       # Output atom type
    
    # Rule execution
    executor: Optional[Callable] = None  # Custom executor
    
    # Metadata
    priority: float = 1.0     # Rule priority
    cost: float = 1.0         # Computational cost estimate
    description: str = ""
    
    # Statistics
    executions: int = 0
    successes: int = 0
    total_time_ms: float = 0.0
    
    def execute(
        self,
        inputs: List[Atom],
        context: Optional[Dict[str, Any]] = None
    ) -> RuleResult:
        """
        Execute the rule on input atoms.
        
        Args:
            inputs: Input atoms
            context: Execution context
            
        Returns:
            RuleResult
        """
        start_time = time.time()
        self.executions += 1
        
        try:
            if self.executor:
                result = self.executor(inputs, context)
            else:
                result = self._default_execute(inputs, context)
            
            if result.success:
                self.successes += 1
            
            result.rule_name = self.name
            result.execution_time_ms = (time.time() - start_time) * 1000
            self.total_time_ms += result.execution_time_ms
            
            return result
            
        except Exception as e:
            logger.error(f"Rule {self.name} execution failed: {e}")
            return RuleResult(
                success=False,
                output_atoms=[],
                rule_name=self.name,
                metadata={"error": str(e)}
            )
    
    def _default_execute(
        self,
        inputs: List[Atom],
        context: Optional[Dict[str, Any]]
    ) -> RuleResult:
        """Default execution based on rule type"""
        
        # Get truth values from inputs
        tvs = [a.truth_value or SimpleTruthValue(0.5, 0.0) for a in inputs]
        
        output_tv = None
        
        if self.rule_type == RuleType.DEDUCTION:
            if len(tvs) >= 2:
                output_tv = deduction(tvs[0], tvs[1])
        
        elif self.rule_type == RuleType.INDUCTION:
            if len(tvs) >= 2:
                output_tv = induction(tvs[0], tvs[1])
        
        elif self.rule_type == RuleType.ABDUCTION:
            if len(tvs) >= 2:
                output_tv = abduction(tvs[0], tvs[1])
        
        elif self.rule_type == RuleType.REVISION:
            if len(tvs) >= 2:
                output_tv = revision(tvs[0], tvs[1])
        
        elif self.rule_type == RuleType.MODUS_PONENS:
            if len(tvs) >= 2:
                output_tv = modus_ponens(tvs[0], tvs[1])
        
        elif self.rule_type == RuleType.AND:
            output_tv = and_formula(tvs)
        
        elif self.rule_type == RuleType.OR:
            output_tv = or_formula(tvs)
        
        elif self.rule_type == RuleType.NOT:
            if len(tvs) >= 1:
                output_tv = not_formula(tvs[0])
        
        else:
            return RuleResult(
                success=False,
                output_atoms=[],
                metadata={"error": f"Unsupported rule type: {self.rule_type}"}
            )
        
        if output_tv is None:
            return RuleResult(success=False, output_atoms=[])
        
        # Create output atom
        import secrets
        output_atom = Atom(
            atom_id=f"result_{secrets.token_hex(4)}",
            atom_type=self.output_pattern,
            outgoing=inputs,
            truth_value=output_tv
        )
        
        return RuleResult(
            success=True,
            output_atoms=[output_atom],
            output_tv=output_tv
        )


class RuleExecutor:
    """
    Rule Executor for PLN inference.
    
    Manages rule registration, selection, and execution with
    attention-based prioritization.
    """
    
    def __init__(self, max_cache_size: int = 1000):
        self._lock = threading.RLock()
        
        # Rule registry
        self._rules: Dict[str, Rule] = {}
        self._rules_by_type: Dict[RuleType, List[Rule]] = defaultdict(list)
        
        # Result cache
        self._cache: Dict[str, RuleResult] = {}
        self._max_cache_size = max_cache_size
        
        # Statistics
        self._stats = {
            "total_executions": 0,
            "cache_hits": 0,
            "cache_misses": 0
        }
        
        # Register built-in rules
        self._register_builtin_rules()
        
        logger.info("RuleExecutor initialized")
    
    def _register_builtin_rules(self):
        """Register built-in PLN rules"""
        
        builtin_rules = [
            Rule(
                name="deduction",
                rule_type=RuleType.DEDUCTION,
                input_pattern=["ImplicationLink", "ImplicationLink"],
                output_pattern="ImplicationLink",
                priority=1.0,
                cost=1.0,
                description="A→B ∧ B→C ⇒ A→C"
            ),
            Rule(
                name="induction",
                rule_type=RuleType.INDUCTION,
                input_pattern=["ImplicationLink", "ImplicationLink"],
                output_pattern="ImplicationLink",
                priority=0.8,
                cost=1.2,
                description="A→B ∧ A→C ⇒ B→C"
            ),
            Rule(
                name="abduction",
                rule_type=RuleType.ABDUCTION,
                input_pattern=["ImplicationLink", "ImplicationLink"],
                output_pattern="ImplicationLink",
                priority=0.7,
                cost=1.3,
                description="A→B ∧ C→B ⇒ A→C"
            ),
            Rule(
                name="revision",
                rule_type=RuleType.REVISION,
                input_pattern=["Atom", "Atom"],
                output_pattern="Atom",
                priority=1.0,
                cost=0.5,
                description="Merge evidence from independent sources"
            ),
            Rule(
                name="modus_ponens",
                rule_type=RuleType.MODUS_PONENS,
                input_pattern=["Atom", "ImplicationLink"],
                output_pattern="Atom",
                priority=1.0,
                cost=0.8,
                description="A ∧ A→B ⇒ B"
            ),
            Rule(
                name="and_rule",
                rule_type=RuleType.AND,
                input_pattern=["Atom*"],  # Variable number of inputs
                output_pattern="AndLink",
                priority=0.9,
                cost=0.6,
                description="Compute conjunction truth value"
            ),
            Rule(
                name="or_rule",
                rule_type=RuleType.OR,
                input_pattern=["Atom*"],
                output_pattern="OrLink",
                priority=0.9,
                cost=0.6,
                description="Compute disjunction truth value"
            ),
            Rule(
                name="not_rule",
                rule_type=RuleType.NOT,
                input_pattern=["Atom"],
                output_pattern="NotLink",
                priority=0.95,
                cost=0.3,
                description="Compute negation truth value"
            ),
        ]
        
        for rule in builtin_rules:
            self.register_rule(rule)
    
    def register_rule(self, rule: Rule) -> bool:
        """
        Register a rule.
        
        Args:
            rule: Rule to register
            
        Returns:
            True if registered
        """
        with self._lock:
            if rule.name in self._rules:
                logger.warning(f"Rule {rule.name} already registered, overwriting")
            
            self._rules[rule.name] = rule
            self._rules_by_type[rule.rule_type].append(rule)
            
            logger.info(f"Registered rule: {rule.name} ({rule.rule_type.value})")
            return True
    
    def unregister_rule(self, name: str) -> bool:
        """Unregister a rule"""
        with self._lock:
            rule = self._rules.pop(name, None)
            if rule:
                self._rules_by_type[rule.rule_type].remove(rule)
                return True
            return False
    
    def execute(
        self,
        rule_name: str,
        inputs: List[Atom],
        context: Optional[Dict[str, Any]] = None,
        use_cache: bool = True
    ) -> RuleResult:
        """
        Execute a specific rule.
        
        Args:
            rule_name: Name of rule to execute
            inputs: Input atoms
            context: Execution context
            use_cache: Whether to use result caching
            
        Returns:
            RuleResult
        """
        with self._lock:
            rule = self._rules.get(rule_name)
            if not rule:
                return RuleResult(
                    success=False,
                    output_atoms=[],
                    metadata={"error": f"Unknown rule: {rule_name}"}
                )
            
            # Check cache
            cache_key = self._make_cache_key(rule_name, inputs)
            
            if use_cache and cache_key in self._cache:
                self._stats["cache_hits"] += 1
                return self._cache[cache_key]
            
            self._stats["cache_misses"] += 1
            self._stats["total_executions"] += 1
            
            # Execute rule
            result = rule.execute(inputs, context)
            
            # Cache result
            if use_cache and result.success:
                self._add_to_cache(cache_key, result)
            
            return result
    
    def execute_best(
        self,
        inputs: List[Atom],
        context: Optional[Dict[str, Any]] = None,
        rule_type: Optional[RuleType] = None
    ) -> RuleResult:
        """
        Execute the best matching rule for the inputs.
        
        Uses attention-based rule selection.
        
        Args:
            inputs: Input atoms
            context: Execution context
            rule_type: Filter by rule type (optional)
            
        Returns:
            RuleResult from best rule
        """
        with self._lock:
            # Find applicable rules
            candidates = self._find_applicable_rules(inputs, rule_type)
            
            if not candidates:
                return RuleResult(
                    success=False,
                    output_atoms=[],
                    metadata={"error": "No applicable rules found"}
                )
            
            # Select best rule based on priority and input attention
            best_rule = self._select_best_rule(candidates, inputs)
            
            return self.execute(best_rule.name, inputs, context)
    
    def _find_applicable_rules(
        self,
        inputs: List[Atom],
        rule_type: Optional[RuleType]
    ) -> List[Rule]:
        """Find rules that can be applied to inputs"""
        candidates = []
        
        rules = self._rules.values()
        if rule_type:
            rules = self._rules_by_type.get(rule_type, [])
        
        for rule in rules:
            if self._matches_pattern(inputs, rule.input_pattern):
                candidates.append(rule)
        
        return candidates
    
    def _matches_pattern(
        self,
        inputs: List[Atom],
        pattern: List[str]
    ) -> bool:
        """Check if inputs match rule pattern"""
        # Handle variable-length patterns
        if pattern and pattern[-1].endswith("*"):
            # Variable number of inputs
            base_pattern = pattern[:-1]
            if len(inputs) < len(base_pattern):
                return False
            # All inputs must match the star type
            star_type = pattern[-1][:-1]
            return all(
                self._type_matches(a.atom_type, star_type)
                for a in inputs
            )
        
        # Fixed-length pattern
        if len(inputs) != len(pattern):
            return False
        
        return all(
            self._type_matches(a.atom_type, p)
            for a, p in zip(inputs, pattern)
        )
    
    def _type_matches(self, atom_type: str, pattern: str) -> bool:
        """Check if atom type matches pattern"""
        if pattern == "Atom":
            return True  # Matches any atom
        return atom_type == pattern or atom_type.endswith(pattern)
    
    def _select_best_rule(
        self,
        candidates: List[Rule],
        inputs: List[Atom]
    ) -> Rule:
        """Select the best rule based on priority and attention"""
        # Compute attention-weighted score
        input_attention = sum(a.attention_value for a in inputs)
        
        def rule_score(rule: Rule) -> float:
            # Higher priority and lower cost is better
            base_score = rule.priority / rule.cost
            
            # Boost by success rate
            if rule.executions > 0:
                success_rate = rule.successes / rule.executions
                base_score *= (0.5 + 0.5 * success_rate)
            
            # Scale by input attention
            return base_score * (1 + input_attention)
        
        return max(candidates, key=rule_score)
    
    def _make_cache_key(self, rule_name: str, inputs: List[Atom]) -> str:
        """Create cache key from rule and inputs"""
        input_ids = tuple(sorted(a.atom_id for a in inputs))
        return f"{rule_name}:{hash(input_ids)}"
    
    def _add_to_cache(self, key: str, result: RuleResult):
        """Add result to cache, evicting if necessary"""
        if len(self._cache) >= self._max_cache_size:
            # Simple eviction: remove oldest
            oldest_key = next(iter(self._cache))
            del self._cache[oldest_key]
        
        self._cache[key] = result
    
    def clear_cache(self):
        """Clear the result cache"""
        with self._lock:
            self._cache.clear()
    
    def list_rules(
        self,
        rule_type: Optional[RuleType] = None
    ) -> List[Dict[str, Any]]:
        """List registered rules"""
        with self._lock:
            rules = self._rules.values()
            if rule_type:
                rules = self._rules_by_type.get(rule_type, [])
            
            return [
                {
                    "name": r.name,
                    "type": r.rule_type.value,
                    "priority": r.priority,
                    "cost": r.cost,
                    "executions": r.executions,
                    "successes": r.successes,
                    "description": r.description
                }
                for r in rules
            ]
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get executor statistics"""
        with self._lock:
            rule_stats = {}
            for name, rule in self._rules.items():
                if rule.executions > 0:
                    rule_stats[name] = {
                        "executions": rule.executions,
                        "successes": rule.successes,
                        "success_rate": rule.successes / rule.executions,
                        "avg_time_ms": rule.total_time_ms / rule.executions
                    }
            
            return {
                **self._stats,
                "rules_registered": len(self._rules),
                "cache_size": len(self._cache),
                "cache_hit_rate": (
                    self._stats["cache_hits"] / 
                    (self._stats["cache_hits"] + self._stats["cache_misses"])
                    if self._stats["cache_hits"] + self._stats["cache_misses"] > 0
                    else 0.0
                ),
                "rule_stats": rule_stats
            }


# Global rule executor
_rule_executor: Optional[RuleExecutor] = None


def get_rule_executor() -> RuleExecutor:
    """Get the global rule executor instance"""
    global _rule_executor
    if _rule_executor is None:
        _rule_executor = RuleExecutor()
    return _rule_executor


if __name__ == "__main__":
    import json
    
    print("=== PLN Rule Executor Examples ===\n")
    
    executor = get_rule_executor()
    
    # Create test atoms
    atom_a = Atom(
        atom_id="atom_a",
        atom_type="ConceptNode",
        name="A",
        truth_value=SimpleTruthValue(0.8, 0.9)
    )
    
    atom_ab = Atom(
        atom_id="atom_ab",
        atom_type="ImplicationLink",
        name="A→B",
        outgoing=[atom_a],
        truth_value=SimpleTruthValue(0.9, 0.85)
    )
    
    atom_bc = Atom(
        atom_id="atom_bc",
        atom_type="ImplicationLink",
        name="B→C",
        truth_value=SimpleTruthValue(0.85, 0.8)
    )
    
    # List rules
    print("=== Registered Rules ===")
    for rule in executor.list_rules():
        print(f"  {rule['name']}: {rule['description']}")
    
    # Execute deduction
    print("\n=== Deduction ===")
    result = executor.execute("deduction", [atom_ab, atom_bc])
    print(f"  Success: {result.success}")
    if result.output_tv:
        print(f"  Output TV: {result.output_tv}")
    print(f"  Time: {result.execution_time_ms:.3f}ms")
    
    # Execute modus ponens
    print("\n=== Modus Ponens ===")
    result = executor.execute("modus_ponens", [atom_a, atom_ab])
    print(f"  Success: {result.success}")
    if result.output_tv:
        print(f"  Output TV: {result.output_tv}")
    
    # Execute best rule
    print("\n=== Best Rule Selection ===")
    result = executor.execute_best([atom_ab, atom_bc])
    print(f"  Selected rule: {result.rule_name}")
    print(f"  Success: {result.success}")
    
    # Statistics
    print("\n=== Statistics ===")
    print(json.dumps(executor.get_statistics(), indent=2))
