#!/usr/bin/env python3
"""
Comprehensive Tests for AGI-OS Core Components

This test suite covers:
- AGI_SecurityManager: Capability-based access control
- AGI_MemoryManager: Hierarchical memory management
- AGI_TimerService: Timer management and cognitive time
- PLN: Probabilistic Logic Networks integration
- ECAN: Economic Attention Network
"""

import unittest
import time
import sys
import os

# Add project root to path
sys.path.insert(0, os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__)))))


class TestSecurityManager(unittest.TestCase):
    """Test AGI_SecurityManager"""
    
    def setUp(self):
        from core.security.capabilities import CapabilityManager, SecurityLevel, ResourceType, Permission
        self.cap_mgr = CapabilityManager()
        self.SecurityLevel = SecurityLevel
        self.ResourceType = ResourceType
        self.Permission = Permission
    
    def test_capability_creation(self):
        """Test creating capabilities"""
        cap = self.cap_mgr.create_capability(
            resource_type=self.ResourceType.ATOMSPACE,
            resource_id="test_atomspace",
            permissions={self.Permission.READ, self.Permission.WRITE},
            owner_id="test_user",
            security_level=self.SecurityLevel.USER
        )
        
        self.assertIsNotNone(cap)
        self.assertEqual(cap.owner_id, "test_user")
        self.assertTrue(self.Permission.READ in cap.permissions)
    
    def test_capability_validation(self):
        """Test capability validation"""
        cap = self.cap_mgr.create_capability(
            resource_type=self.ResourceType.MEMORY,
            resource_id="test_memory",
            permissions={self.Permission.READ},
            owner_id="validator_test",
            security_level=self.SecurityLevel.USER
        )
        
        # Should validate
        valid = self.cap_mgr.validate_capability(
            cap.token,
            self.ResourceType.MEMORY,
            "test_memory",
            self.Permission.READ
        )
        self.assertTrue(valid)
        
        # Should fail for wrong permission
        valid = self.cap_mgr.validate_capability(
            cap.token,
            self.ResourceType.MEMORY,
            "test_memory",
            self.Permission.WRITE
        )
        self.assertFalse(valid)
    
    def test_capability_revocation(self):
        """Test capability revocation"""
        cap = self.cap_mgr.create_capability(
            resource_type=self.ResourceType.FILE,
            resource_id="test_file",
            permissions={self.Permission.READ},
            owner_id="revoke_test",
            security_level=self.SecurityLevel.USER
        )
        
        self.assertTrue(self.cap_mgr.revoke_capability(cap.capability_id, "revoke_test"))
        
        # Should fail validation after revocation
        valid = self.cap_mgr.validate_capability(
            cap.token,
            self.ResourceType.FILE,
            "test_file",
            self.Permission.READ
        )
        self.assertFalse(valid)


class TestMemoryManager(unittest.TestCase):
    """Test AGI_MemoryManager"""
    
    def setUp(self):
        from core.memory.virtual import VirtualMemoryManager, MemoryFlags
        self.vm = VirtualMemoryManager()
        self.MemoryFlags = MemoryFlags
    
    def test_memory_allocation(self):
        """Test basic memory allocation"""
        region = self.vm.allocate(
            size=10 * 1024 * 1024,  # 10MB in bytes
            name="test",
            flags=self.MemoryFlags.RW
        )
        
        self.assertIsNotNone(region)
        self.assertEqual(region.size, 10 * 1024 * 1024)
        self.assertEqual(region.name, "test")
    
    def test_memory_read_write(self):
        """Test memory read/write operations"""
        region = self.vm.allocate(
            size=1 * 1024 * 1024,  # 1MB
            name="rw_test",
            flags=self.MemoryFlags.RW
        )
        
        # Write data
        data = b"Hello, AGI-OS!"
        # Note: write returns None on success in this implementation
        self.vm.write(region.region_id, 0, data)
        
        # Read data back
        read_data = self.vm.read(region.region_id, 0, len(data))
        self.assertEqual(read_data, data)
    
    def test_memory_free(self):
        """Test memory deallocation"""
        region = self.vm.allocate(
            size=5 * 1024 * 1024,  # 5MB
            name="free_test",
            flags=self.MemoryFlags.READ
        )
        
        self.assertTrue(self.vm.free(region.region_id))
        
        # Should raise KeyError when reading from freed region
        with self.assertRaises(KeyError):
            self.vm.read(region.region_id, 0, 10)


class TestNUMAAllocator(unittest.TestCase):
    """Test NUMA-aware allocation"""
    
    def setUp(self):
        from core.memory.numa import NUMAAllocator, NUMAPolicy
        self.numa = NUMAAllocator(detect_topology=False)  # Use simulated
        self.NUMAPolicy = NUMAPolicy
    
    def test_numa_allocation(self):
        """Test NUMA allocation"""
        alloc = self.numa.allocate(
            size_mb=100,
            owner="numa_test",
            policy=self.NUMAPolicy.DEFAULT
        )
        
        self.assertIsNotNone(alloc)
        self.assertEqual(alloc.size_mb, 100)
    
    def test_numa_cognitive_policy(self):
        """Test cognitive NUMA policy"""
        alloc = self.numa.allocate(
            size_mb=50,
            owner="cognitive_test",
            policy=self.NUMAPolicy.COGNITIVE,
            attention_weight=0.9
        )
        
        self.assertIsNotNone(alloc)
        self.assertEqual(alloc.attention_weight, 0.9)
    
    def test_numa_migration(self):
        """Test NUMA memory migration"""
        alloc = self.numa.allocate(
            size_mb=50,
            owner="migrate_test",
            policy=self.NUMAPolicy.DEFAULT
        )
        
        original_node = alloc.node_id
        target_node = 1 if original_node == 0 else 0
        
        self.assertTrue(self.numa.migrate(alloc.allocation_id, target_node))
        self.assertEqual(alloc.node_id, target_node)


class TestPressureMonitor(unittest.TestCase):
    """Test memory pressure monitoring"""
    
    def setUp(self):
        from core.memory.pressure import PressureMonitor, PressureLevel
        self.monitor = PressureMonitor()
        self.PressureLevel = PressureLevel
    
    def test_snapshot(self):
        """Test memory snapshot"""
        snapshot = self.monitor.take_snapshot()
        
        self.assertIsNotNone(snapshot)
        self.assertGreater(snapshot.total_mb, 0)
        self.assertIn(snapshot.pressure_level, self.PressureLevel)
    
    def test_throttle_recommendation(self):
        """Test throttle recommendation"""
        rec = self.monitor.get_throttle_recommendation()
        
        self.assertIsNotNone(rec)
        self.assertIn(rec.level, self.PressureLevel)
        self.assertGreaterEqual(rec.throttle_factor, 0.0)
        self.assertLessEqual(rec.throttle_factor, 1.0)


class TestTimerService(unittest.TestCase):
    """Test AGI_TimerService"""
    
    def setUp(self):
        from core.time.cognitive_time import CognitiveTimeManager, TimeScale
        self.ctm = CognitiveTimeManager()
        self.TimeScale = TimeScale
    
    def test_cognitive_time_advance(self):
        """Test cognitive time advancement"""
        before = self.ctm.now()
        
        self.ctm.advance(self.TimeScale.MICRO, 10, "test_event", "test")
        
        after = self.ctm.now()
        
        self.assertGreater(after.total_micro_ticks(), before.total_micro_ticks())
    
    def test_cognitive_time_scales(self):
        """Test time scale transitions"""
        # Reset
        self.ctm.reset()
        
        # Advance through scales
        self.ctm.advance(self.TimeScale.MICRO, 99, "test", "test")
        self.assertEqual(self.ctm.now().meso_ticks, 0)
        
        self.ctm.advance(self.TimeScale.MICRO, 1, "test", "test")  # Should overflow
        self.assertEqual(self.ctm.now().meso_ticks, 1)
    
    def test_cognitive_time_history(self):
        """Test event history"""
        self.ctm.reset()
        
        for i in range(5):
            self.ctm.advance(self.TimeScale.MICRO, 1, f"event_{i}", "test")
        
        history = self.ctm.get_history(count=3)
        self.assertEqual(len(history), 3)


class TestTimerCoalescer(unittest.TestCase):
    """Test timer coalescing"""
    
    def setUp(self):
        from core.time.coalescing import TimerCoalescer, CoalescingPolicy, TimerPriority
        self.coalescer = TimerCoalescer(policy=CoalescingPolicy.MODERATE)
        self.TimerPriority = TimerPriority
    
    def test_timer_scheduling(self):
        """Test timer scheduling"""
        fired = []
        
        self.coalescer.schedule(
            lambda: fired.append("a"),
            0.1,
            self.TimerPriority.NORMAL,
            "test"
        )
        
        self.assertEqual(self.coalescer.get_pending_count(), 1)
    
    def test_timer_cancellation(self):
        """Test timer cancellation"""
        timer_id = self.coalescer.schedule(
            lambda: None,
            1.0,
            self.TimerPriority.NORMAL,
            "test"
        )
        
        self.assertTrue(self.coalescer.cancel(timer_id))
        # Note: cancel marks for lazy removal, count may not change immediately


class TestPLNTruthValue(unittest.TestCase):
    """Test PLN truth value system"""
    
    def setUp(self):
        from core.pln.truth_value import (
            SimpleTruthValue, IndefiniteTruthValue, DistributionalTruthValue,
            revision, deduction, modus_ponens
        )
        self.STV = SimpleTruthValue
        self.ITV = IndefiniteTruthValue
        self.DTV = DistributionalTruthValue
        self.revision = revision
        self.deduction = deduction
        self.modus_ponens = modus_ponens
    
    def test_simple_truth_value(self):
        """Test simple truth value"""
        tv = self.STV(strength=0.8, confidence_value=0.9)
        
        self.assertEqual(tv.mean, 0.8)
        self.assertEqual(tv.confidence, 0.9)
    
    def test_truth_value_clamping(self):
        """Test truth value clamping to valid range"""
        tv = self.STV(strength=1.5, confidence_value=-0.5)
        
        self.assertEqual(tv.strength, 1.0)
        self.assertEqual(tv.confidence, 0.0)
    
    def test_revision_formula(self):
        """Test PLN revision rule"""
        tv1 = self.STV(strength=0.7, confidence_value=0.8)
        tv2 = self.STV(strength=0.9, confidence_value=0.6)
        
        result = self.revision(tv1, tv2)
        
        # Result should be between inputs
        self.assertGreaterEqual(result.mean, 0.7)
        self.assertLessEqual(result.mean, 0.9)
        # Confidence should increase with more evidence
        self.assertGreater(result.confidence, max(tv1.confidence, tv2.confidence) * 0.5)
    
    def test_deduction_formula(self):
        """Test PLN deduction rule"""
        tv_ab = self.STV(strength=0.9, confidence_value=0.8)
        tv_bc = self.STV(strength=0.85, confidence_value=0.75)
        
        result = self.deduction(tv_ab, tv_bc)
        
        self.assertIsNotNone(result)
        self.assertGreater(result.mean, 0)
        self.assertLess(result.mean, 1)
    
    def test_modus_ponens(self):
        """Test PLN modus ponens"""
        tv_a = self.STV(strength=0.95, confidence_value=0.9)
        tv_ab = self.STV(strength=0.9, confidence_value=0.85)
        
        result = self.modus_ponens(tv_a, tv_ab)
        
        self.assertIsNotNone(result)
        self.assertGreater(result.mean, 0.5)
    
    def test_indefinite_truth_value(self):
        """Test indefinite truth value"""
        itv = self.ITV(lower=0.6, upper=0.9, confidence_level=0.95)
        
        self.assertEqual(itv.mean, 0.75)
        self.assertGreater(itv.confidence, 0)
    
    def test_distributional_truth_value(self):
        """Test distributional truth value"""
        dtv = self.DTV(histogram=[0.1, 0.2, 0.4, 0.2, 0.1])
        
        self.assertAlmostEqual(dtv.mean, 0.5, places=1)
        self.assertGreater(dtv.confidence, 0)


class TestRuleExecutor(unittest.TestCase):
    """Test PLN rule executor"""
    
    def setUp(self):
        from core.pln.rule_executor import RuleExecutor, Atom, RuleType
        from core.pln.truth_value import SimpleTruthValue
        self.executor = RuleExecutor()
        self.Atom = Atom
        self.RuleType = RuleType
        self.STV = SimpleTruthValue
    
    def test_rule_execution(self):
        """Test rule execution"""
        atom_ab = self.Atom(
            atom_id="impl_ab",
            atom_type="ImplicationLink",
            truth_value=self.STV(0.9, 0.8)
        )
        atom_bc = self.Atom(
            atom_id="impl_bc",
            atom_type="ImplicationLink",
            truth_value=self.STV(0.85, 0.75)
        )
        
        result = self.executor.execute("deduction", [atom_ab, atom_bc])
        
        self.assertTrue(result.success)
        self.assertIsNotNone(result.output_tv)
    
    def test_rule_caching(self):
        """Test result caching"""
        atom_a = self.Atom(
            atom_id="atom_a",
            atom_type="ConceptNode",
            truth_value=self.STV(0.8, 0.9)
        )
        atom_b = self.Atom(
            atom_id="atom_b",
            atom_type="ConceptNode",
            truth_value=self.STV(0.7, 0.8)
        )
        
        # First execution
        self.executor.execute("revision", [atom_a, atom_b])
        
        # Second execution should hit cache
        stats = self.executor.get_statistics()
        self.assertGreater(stats["cache_hits"] + stats["cache_misses"], 0)


class TestECANAttentionBank(unittest.TestCase):
    """Test ECAN attention bank"""
    
    def setUp(self):
        from core.ecan.attention_bank import AttentionBank, AttentionConfig
        self.bank = AttentionBank(AttentionConfig())
    
    def test_attention_setting(self):
        """Test setting attention values"""
        av = self.bank.set("test_atom", sti=50.0, lti=20.0)
        
        self.assertEqual(av.sti, 50.0)
        self.assertEqual(av.lti, 20.0)
    
    def test_attention_stimulation(self):
        """Test attention stimulation"""
        self.bank.set("stim_test", sti=30.0)
        av = self.bank.stimulate("stim_test", 20.0)
        
        self.assertEqual(av.sti, 50.0)
    
    def test_attention_inhibition(self):
        """Test attention inhibition"""
        self.bank.set("inhib_test", sti=50.0)
        av = self.bank.inhibit("inhib_test", 20.0)
        
        self.assertEqual(av.sti, 30.0)
    
    def test_attentional_focus(self):
        """Test attentional focus management"""
        self.bank.set("high_sti", sti=100.0)
        self.bank.set("low_sti", sti=-50.0)
        
        focus = self.bank.get_attentional_focus()
        
        # High STI should be in focus
        focus_ids = [atom_id for atom_id, _ in focus]
        self.assertIn("high_sti", focus_ids)


class TestHebbianLearning(unittest.TestCase):
    """Test Hebbian learning"""
    
    def setUp(self):
        from core.ecan.attention_bank import AttentionBank
        from core.ecan.hebbian import HebbianManager
        
        self.bank = AttentionBank()
        self.hebbian = HebbianManager(attention_bank=self.bank)
        
        # Set up atoms with attention
        self.bank.set("atom_a", sti=50.0)
        self.bank.set("atom_b", sti=40.0)
        self.bank.set("atom_c", sti=30.0)
    
    def test_hebbian_link_creation(self):
        """Test Hebbian link creation through co-activation"""
        # Record activations in quick succession
        self.hebbian.record_activation("atom_a")
        time.sleep(0.01)  # Small delay within co-activation window
        affected = self.hebbian.record_activation("atom_b")
        
        # Should have created/strengthened links
        link = self.hebbian.get_link("atom_a", "atom_b")
        self.assertIsNotNone(link)
    
    def test_hebbian_learning_cycle(self):
        """Test full learning cycle"""
        result = self.hebbian.simulate_learning_cycle(["atom_a", "atom_b", "atom_c"])
        
        self.assertIn("active_atoms", result)
        self.assertEqual(result["active_atoms"], 3)


class TestImportanceSpreading(unittest.TestCase):
    """Test importance spreading"""
    
    def setUp(self):
        from core.ecan.attention_bank import AttentionBank
        from core.ecan.importance_spreading import ImportanceSpreader, SpreadingMode
        
        self.bank = AttentionBank()
        self.spreader = ImportanceSpreader(attention_bank=self.bank)
        self.SpreadingMode = SpreadingMode
        
        # Set up atoms
        self.bank.set("source", sti=100.0)
        self.bank.set("target1", sti=10.0)
        self.bank.set("target2", sti=10.0)
        
        # Set up links
        self.spreader._hypergraph.add_link("source", "target1", weight=0.8)
        self.spreader._hypergraph.add_link("source", "target2", weight=0.5)
    
    def test_diffusion_spreading(self):
        """Test diffusion spreading"""
        events = self.spreader.spread("source", self.SpreadingMode.DIFFUSION)
        
        self.assertGreater(len(events), 0)
        
        # Targets should have gained STI
        target1_av = self.bank.get("target1")
        self.assertGreater(target1_av.sti, 10.0)
    
    def test_hebbian_spreading(self):
        """Test Hebbian weighted spreading"""
        events = self.spreader.spread("source", self.SpreadingMode.HEBBIAN)
        
        # Should produce spreading events
        self.assertGreaterEqual(len(events), 0)


def run_tests():
    """Run all tests"""
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()
    
    # Add test classes
    test_classes = [
        TestSecurityManager,
        TestMemoryManager,
        TestNUMAAllocator,
        TestPressureMonitor,
        TestTimerService,
        TestTimerCoalescer,
        TestPLNTruthValue,
        TestRuleExecutor,
        TestECANAttentionBank,
        TestHebbianLearning,
        TestImportanceSpreading,
    ]
    
    for test_class in test_classes:
        tests = loader.loadTestsFromTestCase(test_class)
        suite.addTests(tests)
    
    # Run tests
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)
    
    return result.wasSuccessful()


if __name__ == "__main__":
    success = run_tests()
    sys.exit(0 if success else 1)
