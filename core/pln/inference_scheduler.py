#!/usr/bin/env python3
"""
PLN Inference Scheduler for AGI-OS

This module integrates PLN inference with the AGI_Scheduler, enabling
attention-based inference control and task scheduling for probabilistic
reasoning.

Features:
- Forward and backward chaining scheduling
- Attention-based inference priority
- Inference budget management
- Integration with AGI_Scheduler
- Inference step limiting
- Result caching and propagation
"""

import time
import logging
import threading
import heapq
from typing import Dict, Any, Optional, List, Set, Callable, Tuple
from dataclasses import dataclass, field
from enum import Enum
from collections import defaultdict

from core.pln.truth_value import TruthValue, SimpleTruthValue, merge_truth_values
from core.pln.rule_executor import (
    RuleExecutor, get_rule_executor,
    Rule, RuleType, RuleResult, Atom
)

logging.basicConfig(
    level=logging.INFO,
    format='%(asctime)s - %(name)s - %(levelname)s - %(message)s'
)
logger = logging.getLogger("AGI_PLN.InferenceScheduler")


class InferenceMode(Enum):
    """Inference modes"""
    FORWARD = "forward"       # Forward chaining from premises
    BACKWARD = "backward"     # Backward chaining from goals
    MIXED = "mixed"          # Combination of both
    FOCUSED = "focused"       # Attention-focused inference


class TaskStatus(Enum):
    """Inference task status"""
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


@dataclass
class InferenceGoal:
    """
    Goal for backward chaining inference.
    """
    goal_id: str
    target_atom: Atom
    target_tv: Optional[TruthValue] = None  # Minimum acceptable TV
    max_depth: int = 5
    created_at: float = field(default_factory=time.time)


@dataclass(order=True)
class InferenceTask:
    """
    A scheduled inference task.
    """
    priority: float = field(compare=True)  # Higher = more important
    task_id: str = field(compare=False)
    
    # Task configuration
    mode: InferenceMode = field(default=InferenceMode.FORWARD, compare=False)
    input_atoms: List[Atom] = field(default_factory=list, compare=False)
    goal: Optional[InferenceGoal] = field(default=None, compare=False)
    
    # Execution state
    status: TaskStatus = field(default=TaskStatus.PENDING, compare=False)
    depth: int = field(default=0, compare=False)
    max_steps: int = field(default=100, compare=False)
    steps_taken: int = field(default=0, compare=False)
    
    # Budget
    attention_budget: float = field(default=100.0, compare=False)
    spent_budget: float = field(default=0.0, compare=False)
    
    # Results
    results: List[RuleResult] = field(default_factory=list, compare=False)
    derived_atoms: List[Atom] = field(default_factory=list, compare=False)
    
    # Timing
    created_at: float = field(default_factory=time.time, compare=False)
    started_at: Optional[float] = field(default=None, compare=False)
    completed_at: Optional[float] = field(default=None, compare=False)
    
    # Owner
    owner: str = field(default="", compare=False)
    
    def to_dict(self) -> Dict[str, Any]:
        return {
            "task_id": self.task_id,
            "priority": self.priority,
            "mode": self.mode.value,
            "status": self.status.value,
            "steps_taken": self.steps_taken,
            "max_steps": self.max_steps,
            "attention_budget": self.attention_budget,
            "spent_budget": self.spent_budget,
            "results_count": len(self.results),
            "derived_atoms_count": len(self.derived_atoms),
            "owner": self.owner
        }


class InferenceScheduler:
    """
    PLN Inference Scheduler for AGI-OS
    
    Schedules and executes PLN inference tasks with attention-based
    prioritization and budget management.
    """
    
    def __init__(
        self,
        rule_executor: Optional[RuleExecutor] = None,
        max_concurrent: int = 4,
        default_budget: float = 100.0
    ):
        """
        Initialize the inference scheduler.
        
        Args:
            rule_executor: Rule executor instance
            max_concurrent: Maximum concurrent tasks
            default_budget: Default attention budget per task
        """
        self._lock = threading.RLock()
        
        self._rule_executor = rule_executor or get_rule_executor()
        self._max_concurrent = max_concurrent
        self._default_budget = default_budget
        
        # Task management
        self._pending_tasks: List[InferenceTask] = []  # Min-heap (negative priority)
        heapq.heapify(self._pending_tasks)
        
        self._running_tasks: Dict[str, InferenceTask] = {}
        self._completed_tasks: Dict[str, InferenceTask] = {}
        
        # Goal tracking for backward chaining
        self._goals: Dict[str, InferenceGoal] = {}
        
        # Derived knowledge
        self._derived_atoms: Dict[str, Atom] = {}
        
        # Background processing
        self._running = False
        self._scheduler_thread: Optional[threading.Thread] = None
        
        # Callbacks
        self._completion_callbacks: Dict[str, Callable] = {}
        
        # Statistics
        self._stats = {
            "tasks_created": 0,
            "tasks_completed": 0,
            "tasks_failed": 0,
            "total_steps": 0,
            "total_budget_spent": 0.0,
            "atoms_derived": 0
        }
        
        logger.info("InferenceScheduler initialized")
    
    def create_task(
        self,
        input_atoms: List[Atom],
        mode: InferenceMode = InferenceMode.FORWARD,
        priority: float = 1.0,
        max_steps: int = 100,
        attention_budget: Optional[float] = None,
        owner: str = ""
    ) -> InferenceTask:
        """
        Create an inference task.
        
        Args:
            input_atoms: Input atoms for inference
            mode: Inference mode
            priority: Task priority
            max_steps: Maximum inference steps
            attention_budget: Attention budget
            owner: Task owner
            
        Returns:
            InferenceTask
        """
        with self._lock:
            import secrets
            task_id = f"inf_task_{secrets.token_hex(6)}"
            
            # Compute priority based on input attention
            input_attention = sum(a.attention_value for a in input_atoms)
            effective_priority = priority * (1 + input_attention)
            
            task = InferenceTask(
                priority=-effective_priority,  # Negative for min-heap
                task_id=task_id,
                mode=mode,
                input_atoms=input_atoms,
                max_steps=max_steps,
                attention_budget=attention_budget or self._default_budget,
                owner=owner
            )
            
            heapq.heappush(self._pending_tasks, task)
            self._stats["tasks_created"] += 1
            
            logger.debug(f"Created inference task {task_id}")
            return task
    
    def create_goal(
        self,
        target_atom: Atom,
        target_tv: Optional[TruthValue] = None,
        max_depth: int = 5,
        priority: float = 1.0
    ) -> InferenceGoal:
        """
        Create a backward chaining goal.
        
        Args:
            target_atom: Target atom to prove
            target_tv: Minimum acceptable truth value
            max_depth: Maximum backward chaining depth
            priority: Goal priority
            
        Returns:
            InferenceGoal
        """
        with self._lock:
            import secrets
            goal_id = f"goal_{secrets.token_hex(6)}"
            
            goal = InferenceGoal(
                goal_id=goal_id,
                target_atom=target_atom,
                target_tv=target_tv or SimpleTruthValue(0.5, 0.5),
                max_depth=max_depth
            )
            
            self._goals[goal_id] = goal
            
            # Create backward chaining task
            self.create_task(
                input_atoms=[target_atom],
                mode=InferenceMode.BACKWARD,
                priority=priority
            ).goal = goal
            
            logger.debug(f"Created inference goal {goal_id}")
            return goal
    
    def cancel_task(self, task_id: str) -> bool:
        """Cancel an inference task"""
        with self._lock:
            # Check pending
            for task in self._pending_tasks:
                if task.task_id == task_id:
                    task.status = TaskStatus.CANCELLED
                    return True
            
            # Check running
            task = self._running_tasks.get(task_id)
            if task:
                task.status = TaskStatus.CANCELLED
                return True
            
            return False
    
    def step_task(self, task: InferenceTask) -> bool:
        """
        Execute one inference step for a task.
        
        Args:
            task: Task to step
            
        Returns:
            True if step was successful
        """
        if task.status != TaskStatus.RUNNING:
            return False
        
        if task.steps_taken >= task.max_steps:
            task.status = TaskStatus.COMPLETED
            return False
        
        if task.spent_budget >= task.attention_budget:
            task.status = TaskStatus.COMPLETED
            return False
        
        task.steps_taken += 1
        self._stats["total_steps"] += 1
        
        if task.mode == InferenceMode.FORWARD:
            return self._forward_step(task)
        elif task.mode == InferenceMode.BACKWARD:
            return self._backward_step(task)
        elif task.mode == InferenceMode.MIXED:
            # Alternate between forward and backward
            if task.steps_taken % 2 == 0:
                return self._forward_step(task)
            else:
                return self._backward_step(task)
        elif task.mode == InferenceMode.FOCUSED:
            return self._focused_step(task)
        
        return False
    
    def _forward_step(self, task: InferenceTask) -> bool:
        """Execute a forward chaining step"""
        # Select atoms to reason about
        atoms = task.input_atoms + task.derived_atoms
        
        if len(atoms) < 2:
            return False
        
        # Sort by attention and select top atoms
        atoms = sorted(atoms, key=lambda a: -a.attention_value)
        selected = atoms[:min(5, len(atoms))]
        
        # Try to apply rules
        result = self._rule_executor.execute_best(
            selected,
            rule_type=None  # Try any applicable rule
        )
        
        if result.success:
            task.results.append(result)
            task.derived_atoms.extend(result.output_atoms)
            
            # Track globally
            for atom in result.output_atoms:
                self._derived_atoms[atom.atom_id] = atom
            
            # Spend budget based on rule cost
            task.spent_budget += 1.0
            self._stats["atoms_derived"] += len(result.output_atoms)
            
            return True
        
        return False
    
    def _backward_step(self, task: InferenceTask) -> bool:
        """Execute a backward chaining step"""
        if not task.goal:
            return self._forward_step(task)  # Fall back to forward
        
        target = task.goal.target_atom
        
        # Find rules that could produce the target
        applicable_rules = self._find_rules_for_target(target)
        
        if not applicable_rules:
            return False
        
        # Try each rule
        for rule in applicable_rules:
            # Generate subgoals
            subgoals = self._generate_subgoals(rule, target)
            
            # Check if subgoals are satisfied
            if self._check_subgoals(subgoals, task):
                # Execute the rule
                result = self._rule_executor.execute(rule.name, subgoals)
                
                if result.success:
                    task.results.append(result)
                    task.derived_atoms.extend(result.output_atoms)
                    task.spent_budget += 1.0
                    
                    # Check if goal is achieved
                    if self._goal_achieved(task.goal, result):
                        task.status = TaskStatus.COMPLETED
                    
                    return True
        
        return False
    
    def _focused_step(self, task: InferenceTask) -> bool:
        """Execute an attention-focused inference step"""
        # Focus on highest-attention atoms
        all_atoms = task.input_atoms + task.derived_atoms
        
        if not all_atoms:
            return False
        
        # Select atoms in attention focus
        focus_atoms = [a for a in all_atoms if a.attention_value > 0.5]
        
        if len(focus_atoms) < 2:
            # Expand to include more atoms
            focus_atoms = sorted(all_atoms, key=lambda a: -a.attention_value)[:3]
        
        result = self._rule_executor.execute_best(focus_atoms)
        
        if result.success:
            task.results.append(result)
            task.derived_atoms.extend(result.output_atoms)
            task.spent_budget += 1.0
            
            # Propagate attention to derived atoms
            max_attention = max(a.attention_value for a in focus_atoms)
            for atom in result.output_atoms:
                atom.attention_value = max_attention * 0.9  # Slight decay
            
            return True
        
        return False
    
    def _find_rules_for_target(self, target: Atom) -> List[Rule]:
        """Find rules that could produce the target"""
        rules = []
        for rule in self._rule_executor._rules.values():
            if self._rule_could_produce(rule, target):
                rules.append(rule)
        return rules
    
    def _rule_could_produce(self, rule: Rule, target: Atom) -> bool:
        """Check if a rule could produce the target type"""
        return rule.output_pattern == target.atom_type or rule.output_pattern == "Atom"
    
    def _generate_subgoals(self, rule: Rule, target: Atom) -> List[Atom]:
        """Generate subgoals for backward chaining"""
        # This is a simplified version
        # Real implementation would do proper unification
        return list(target.outgoing) if target.outgoing else []
    
    def _check_subgoals(self, subgoals: List[Atom], task: InferenceTask) -> bool:
        """Check if subgoals are satisfied"""
        if not subgoals:
            return False
        
        # Check if all subgoals have acceptable truth values
        for subgoal in subgoals:
            if subgoal.truth_value is None:
                return False
            if subgoal.truth_value.confidence < 0.1:
                return False
        
        return True
    
    def _goal_achieved(self, goal: InferenceGoal, result: RuleResult) -> bool:
        """Check if a goal has been achieved"""
        if not result.output_tv:
            return False
        
        if goal.target_tv:
            return (result.output_tv.mean >= goal.target_tv.mean and
                    result.output_tv.confidence >= goal.target_tv.confidence)
        
        return result.output_tv.confidence > 0.5
    
    def run_task(self, task_id: str, max_steps: Optional[int] = None) -> InferenceTask:
        """
        Run a task to completion (or step limit).
        
        Args:
            task_id: Task to run
            max_steps: Maximum steps (overrides task setting)
            
        Returns:
            Completed task
        """
        with self._lock:
            # Find task
            task = None
            for t in self._pending_tasks:
                if t.task_id == task_id:
                    task = t
                    break
            
            if not task:
                task = self._running_tasks.get(task_id)
            
            if not task:
                raise ValueError(f"Task not found: {task_id}")
            
            # Start task
            task.status = TaskStatus.RUNNING
            task.started_at = time.time()
            self._running_tasks[task_id] = task
            
            steps = max_steps or task.max_steps
        
        # Run steps
        for _ in range(steps):
            if task.status != TaskStatus.RUNNING:
                break
            self.step_task(task)
        
        # Complete task
        with self._lock:
            if task.status == TaskStatus.RUNNING:
                task.status = TaskStatus.COMPLETED
            
            task.completed_at = time.time()
            self._running_tasks.pop(task_id, None)
            self._completed_tasks[task_id] = task
            
            if task.status == TaskStatus.COMPLETED:
                self._stats["tasks_completed"] += 1
                self._stats["total_budget_spent"] += task.spent_budget
            else:
                self._stats["tasks_failed"] += 1
            
            # Invoke callbacks
            callback = self._completion_callbacks.get(task_id)
            if callback:
                try:
                    callback(task)
                except Exception as e:
                    logger.error(f"Completion callback failed: {e}")
        
        return task
    
    def start(self):
        """Start background task processing"""
        with self._lock:
            if self._running:
                return
            
            self._running = True
            self._scheduler_thread = threading.Thread(
                target=self._scheduler_loop,
                name="InferenceScheduler",
                daemon=True
            )
            self._scheduler_thread.start()
            logger.info("InferenceScheduler started")
    
    def stop(self):
        """Stop background task processing"""
        with self._lock:
            self._running = False
            if self._scheduler_thread:
                self._scheduler_thread.join(timeout=2.0)
                self._scheduler_thread = None
            logger.info("InferenceScheduler stopped")
    
    def _scheduler_loop(self):
        """Background scheduling loop"""
        while self._running:
            try:
                with self._lock:
                    # Check for tasks to start
                    while (len(self._running_tasks) < self._max_concurrent and
                           self._pending_tasks):
                        task = heapq.heappop(self._pending_tasks)
                        if task.status == TaskStatus.PENDING:
                            task.status = TaskStatus.RUNNING
                            task.started_at = time.time()
                            self._running_tasks[task.task_id] = task
                    
                    # Step running tasks
                    for task in list(self._running_tasks.values()):
                        if task.status == TaskStatus.RUNNING:
                            self.step_task(task)
                            
                            # Check if completed
                            if task.status != TaskStatus.RUNNING:
                                task.completed_at = time.time()
                                self._running_tasks.pop(task.task_id, None)
                                self._completed_tasks[task.task_id] = task
                                
                                if task.status == TaskStatus.COMPLETED:
                                    self._stats["tasks_completed"] += 1
                                else:
                                    self._stats["tasks_failed"] += 1
                
            except Exception as e:
                logger.error(f"Scheduler loop error: {e}")
            
            time.sleep(0.01)  # Small delay between cycles
    
    def on_completion(self, task_id: str, callback: Callable[[InferenceTask], None]):
        """Register a completion callback"""
        self._completion_callbacks[task_id] = callback
    
    def get_task(self, task_id: str) -> Optional[InferenceTask]:
        """Get a task by ID"""
        with self._lock:
            for task in self._pending_tasks:
                if task.task_id == task_id:
                    return task
            
            task = self._running_tasks.get(task_id)
            if task:
                return task
            
            return self._completed_tasks.get(task_id)
    
    def list_tasks(
        self,
        status: Optional[TaskStatus] = None
    ) -> List[Dict[str, Any]]:
        """List tasks with optional status filter"""
        with self._lock:
            all_tasks = (
                list(self._pending_tasks) +
                list(self._running_tasks.values()) +
                list(self._completed_tasks.values())
            )
            
            if status:
                all_tasks = [t for t in all_tasks if t.status == status]
            
            return [t.to_dict() for t in all_tasks]
    
    def get_derived_atoms(self) -> List[Dict[str, Any]]:
        """Get all derived atoms"""
        with self._lock:
            return [
                {
                    "atom_id": a.atom_id,
                    "atom_type": a.atom_type,
                    "name": a.name,
                    "truth_value": a.truth_value.to_dict() if a.truth_value else None,
                    "attention_value": a.attention_value
                }
                for a in self._derived_atoms.values()
            ]
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get scheduler statistics"""
        with self._lock:
            return {
                **self._stats,
                "pending_tasks": len(self._pending_tasks),
                "running_tasks": len(self._running_tasks),
                "completed_tasks": len(self._completed_tasks),
                "active_goals": len(self._goals),
                "derived_atoms": len(self._derived_atoms),
                "running": self._running
            }


# Global inference scheduler
_inference_scheduler: Optional[InferenceScheduler] = None


def get_inference_scheduler() -> InferenceScheduler:
    """Get the global inference scheduler instance"""
    global _inference_scheduler
    if _inference_scheduler is None:
        _inference_scheduler = InferenceScheduler()
    return _inference_scheduler


if __name__ == "__main__":
    import json
    
    print("=== PLN Inference Scheduler Examples ===\n")
    
    scheduler = get_inference_scheduler()
    
    # Create test atoms
    atom_a = Atom(
        atom_id="concept_cat",
        atom_type="ConceptNode",
        name="Cat",
        truth_value=SimpleTruthValue(1.0, 0.99),
        attention_value=0.8
    )
    
    atom_b = Atom(
        atom_id="concept_animal",
        atom_type="ConceptNode",
        name="Animal",
        truth_value=SimpleTruthValue(1.0, 0.99),
        attention_value=0.6
    )
    
    impl_ab = Atom(
        atom_id="impl_cat_animal",
        atom_type="ImplicationLink",
        name="Cat→Animal",
        outgoing=[atom_a, atom_b],
        truth_value=SimpleTruthValue(0.95, 0.9),
        attention_value=0.7
    )
    
    atom_c = Atom(
        atom_id="concept_living",
        atom_type="ConceptNode",
        name="Living",
        truth_value=SimpleTruthValue(1.0, 0.99),
        attention_value=0.5
    )
    
    impl_bc = Atom(
        atom_id="impl_animal_living",
        atom_type="ImplicationLink",
        name="Animal→Living",
        outgoing=[atom_b, atom_c],
        truth_value=SimpleTruthValue(0.99, 0.95),
        attention_value=0.65
    )
    
    # Create forward chaining task
    print("=== Forward Chaining Task ===")
    task = scheduler.create_task(
        input_atoms=[impl_ab, impl_bc],
        mode=InferenceMode.FORWARD,
        priority=1.0,
        max_steps=10,
        owner="test"
    )
    print(f"Created task: {task.task_id}")
    
    # Run the task
    completed_task = scheduler.run_task(task.task_id)
    print(f"Task completed: {completed_task.status.value}")
    print(f"Steps taken: {completed_task.steps_taken}")
    print(f"Results: {len(completed_task.results)}")
    print(f"Derived atoms: {len(completed_task.derived_atoms)}")
    
    if completed_task.results:
        for i, result in enumerate(completed_task.results):
            print(f"  Result {i+1}: {result.rule_name} -> {result.output_tv}")
    
    # Create focused inference task
    print("\n=== Focused Inference Task ===")
    task2 = scheduler.create_task(
        input_atoms=[atom_a, impl_ab, atom_b, impl_bc],
        mode=InferenceMode.FOCUSED,
        priority=1.5,
        max_steps=5,
        owner="test"
    )
    
    completed_task2 = scheduler.run_task(task2.task_id)
    print(f"Task completed: {completed_task2.status.value}")
    print(f"Steps taken: {completed_task2.steps_taken}")
    print(f"Derived atoms: {len(completed_task2.derived_atoms)}")
    
    # Statistics
    print("\n=== Statistics ===")
    print(json.dumps(scheduler.get_statistics(), indent=2))
    
    # Derived atoms
    print("\n=== Derived Atoms ===")
    for atom in scheduler.get_derived_atoms():
        print(f"  {atom['atom_id']}: {atom['atom_type']} TV={atom['truth_value']}")
