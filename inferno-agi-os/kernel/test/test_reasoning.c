/*
 * Test suite for the Reasoning Engine kernel module
 *
 * Tests PLN inference rules, forward/backward chaining,
 * and pattern matching operations.
 *
 * Copyright (C) 2026 OpenCog Community
 * Licensed under AGPL-3.0
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "../atomspace/atomspace.h"
#include "../reasoning/reasoning.h"

/* Test counters */
static int tests_passed = 0;
static int tests_failed = 0;
static int tests_total = 0;

#define TEST_ASSERT(cond, msg) do { \
    tests_total++; \
    if (cond) { \
        tests_passed++; \
        printf("  PASS: %s\n", msg); \
    } else { \
        tests_failed++; \
        printf("  FAIL: %s (line %d)\n", msg, __LINE__); \
    } \
} while(0)

/* External globals from portable implementations */
extern AtomSpace *global_atomspace;
extern ReasoningEngine *global_reasoning;

/*
 * Test 1: Reasoning engine initialization
 */
static void
test_reasoning_init(void)
{
    printf("\n=== Test: Reasoning Engine Initialization ===\n");

    atomspace_init();
    TEST_ASSERT(global_atomspace != NULL, "AtomSpace initialized");

    reasoning_init();
    TEST_ASSERT(global_reasoning != NULL, "Reasoning engine initialized");
    TEST_ASSERT(global_reasoning->rule_count > 0, "Rules loaded");
}

/*
 * Test 2: PLN deduction rule
 * Given: Socrates -> Human, Human -> Mortal
 * Derive: Socrates -> Mortal
 */
static void
test_pln_deduction(void)
{
    printf("\n=== Test: PLN Deduction Rule ===\n");

    uint32_t socrates = atomspace_add_node(global_atomspace, ATOM_TYPE_CONCEPT, "Socrates");
    uint32_t human = atomspace_add_node(global_atomspace, ATOM_TYPE_CONCEPT, "Human");
    uint32_t mortal = atomspace_add_node(global_atomspace, ATOM_TYPE_CONCEPT, "Mortal");

    TEST_ASSERT(socrates > 0, "Created Socrates atom");
    TEST_ASSERT(human > 0, "Created Human atom");
    TEST_ASSERT(mortal > 0, "Created Mortal atom");

    /* Set truth values */
    atomspace_set_tv(global_atomspace, socrates, 0.9f, 0.8f);
    atomspace_set_tv(global_atomspace, human, 0.9f, 0.9f);
    atomspace_set_tv(global_atomspace, mortal, 1.0f, 0.95f);

    /* Create inheritance links */
    uint32_t out1[2] = { socrates, human };
    uint32_t link1 = atomspace_add_link(global_atomspace, ATOM_TYPE_INHERITANCE, out1, 2);
    atomspace_set_tv(global_atomspace, link1, 0.95f, 0.85f);

    uint32_t out2[2] = { human, mortal };
    uint32_t link2 = atomspace_add_link(global_atomspace, ATOM_TYPE_INHERITANCE, out2, 2);
    atomspace_set_tv(global_atomspace, link2, 0.99f, 0.9f);

    TEST_ASSERT(link1 > 0, "Created Socrates->Human link");
    TEST_ASSERT(link2 > 0, "Created Human->Mortal link");

    /* Perform deduction */
    uint32_t premises[2] = { link1, link2 };
    uint32_t conclusions[10];
    int count = reasoning_infer(global_reasoning, premises, 2, conclusions, 10);

    TEST_ASSERT(count > 0, "Deduction produced conclusions");

    if (count > 0) {
        /* Verify the conclusion has a truth value */
        float strength, confidence;
        atomspace_get_tv(global_atomspace, conclusions[0], &strength, &confidence);
        TEST_ASSERT(strength > 0.0f, "Conclusion has positive strength");
        TEST_ASSERT(confidence > 0.0f, "Conclusion has positive confidence");
        printf("  Deduction result: TV=<%.4f, %.4f>\n", strength, confidence);
    }
}

/*
 * Test 3: Forward chaining
 */
static void
test_forward_chaining(void)
{
    printf("\n=== Test: Forward Chaining ===\n");

    /* Create a chain: A -> B -> C -> D */
    uint32_t a = atomspace_add_node(global_atomspace, ATOM_TYPE_CONCEPT, "Alpha");
    uint32_t b = atomspace_add_node(global_atomspace, ATOM_TYPE_CONCEPT, "Beta");
    uint32_t c = atomspace_add_node(global_atomspace, ATOM_TYPE_CONCEPT, "Gamma");
    uint32_t d = atomspace_add_node(global_atomspace, ATOM_TYPE_CONCEPT, "Delta");

    atomspace_set_tv(global_atomspace, a, 0.9f, 0.9f);
    atomspace_set_tv(global_atomspace, b, 0.85f, 0.8f);
    atomspace_set_tv(global_atomspace, c, 0.8f, 0.75f);
    atomspace_set_tv(global_atomspace, d, 0.95f, 0.9f);

    uint32_t out_ab[2] = { a, b };
    uint32_t out_bc[2] = { b, c };
    uint32_t out_cd[2] = { c, d };

    uint32_t link_ab = atomspace_add_link(global_atomspace, ATOM_TYPE_INHERITANCE, out_ab, 2);
    uint32_t link_bc = atomspace_add_link(global_atomspace, ATOM_TYPE_INHERITANCE, out_bc, 2);
    uint32_t link_cd = atomspace_add_link(global_atomspace, ATOM_TYPE_INHERITANCE, out_cd, 2);

    atomspace_set_tv(global_atomspace, link_ab, 0.9f, 0.85f);
    atomspace_set_tv(global_atomspace, link_bc, 0.85f, 0.8f);
    atomspace_set_tv(global_atomspace, link_cd, 0.8f, 0.75f);

    TEST_ASSERT(link_ab > 0 && link_bc > 0 && link_cd > 0, "Created chain links");

    /* Forward chain from A */
    uint32_t initial[1] = { a };
    int steps = reasoning_forward_chain(global_reasoning, initial, 1, 5);

    TEST_ASSERT(steps >= 0, "Forward chaining completed without error");
    printf("  Forward chaining took %d steps\n", steps);
}

/*
 * Test 4: Backward chaining
 */
static void
test_backward_chaining(void)
{
    printf("\n=== Test: Backward Chaining ===\n");

    /* Try to prove something about an existing atom */
    uint32_t goal = atomspace_add_node(global_atomspace, ATOM_TYPE_CONCEPT, "Goal");
    atomspace_set_tv(global_atomspace, goal, 0.5f, 0.3f);

    uint32_t evidence[10];
    int count = reasoning_backward_chain(global_reasoning, goal, evidence, 10);

    TEST_ASSERT(count >= 0, "Backward chaining completed without error");
    printf("  Found %d pieces of evidence\n", count);
}

/*
 * Test 5: Pattern matching
 */
static void
test_pattern_matching(void)
{
    printf("\n=== Test: Pattern Matching ===\n");

    /* Create a pattern: find all X where X -> Human */
    uint32_t human = atomspace_add_node(global_atomspace, ATOM_TYPE_CONCEPT, "Human");
    uint32_t pattern = atomspace_add_node(global_atomspace, ATOM_TYPE_VARIABLE, "X");

    TEST_ASSERT(pattern > 0, "Created variable pattern");

    uint32_t matches[20];
    int count = reasoning_pattern_match(global_reasoning, pattern, matches, 20);

    TEST_ASSERT(count >= 0, "Pattern matching completed without error");
    printf("  Found %d pattern matches\n", count);
}

/*
 * Test 6: Multiple inference rules
 */
static void
test_multiple_rules(void)
{
    printf("\n=== Test: Multiple Inference Rules ===\n");

    /* Create atoms for different rule types */
    uint32_t cat = atomspace_add_node(global_atomspace, ATOM_TYPE_CONCEPT, "Cat");
    uint32_t animal = atomspace_add_node(global_atomspace, ATOM_TYPE_CONCEPT, "Animal");
    uint32_t pet = atomspace_add_node(global_atomspace, ATOM_TYPE_CONCEPT, "Pet");

    atomspace_set_tv(global_atomspace, cat, 0.9f, 0.9f);
    atomspace_set_tv(global_atomspace, animal, 0.95f, 0.95f);
    atomspace_set_tv(global_atomspace, pet, 0.8f, 0.7f);

    uint32_t out_ca[2] = { cat, animal };
    uint32_t out_cp[2] = { cat, pet };

    uint32_t link_ca = atomspace_add_link(global_atomspace, ATOM_TYPE_INHERITANCE, out_ca, 2);
    uint32_t link_cp = atomspace_add_link(global_atomspace, ATOM_TYPE_INHERITANCE, out_cp, 2);

    atomspace_set_tv(global_atomspace, link_ca, 1.0f, 0.95f);
    atomspace_set_tv(global_atomspace, link_cp, 0.7f, 0.6f);

    TEST_ASSERT(link_ca > 0, "Created Cat->Animal link");
    TEST_ASSERT(link_cp > 0, "Created Cat->Pet link");

    /* Test deduction */
    uint32_t premises[2] = { link_ca, link_cp };
    uint32_t conclusions[10];
    int count = reasoning_infer(global_reasoning, premises, 2, conclusions, 10);

    TEST_ASSERT(count >= 0, "Multi-rule inference completed");
    printf("  Derived %d conclusions from multiple rules\n", count);
}

/*
 * Main test runner
 */
int
main(int argc, char *argv[])
{
    printf("========================================\n");
    printf("Reasoning Engine Test Suite\n");
    printf("========================================\n");

    test_reasoning_init();
    test_pln_deduction();
    test_forward_chaining();
    test_backward_chaining();
    test_pattern_matching();
    test_multiple_rules();

    printf("\n========================================\n");
    printf("Results: %d/%d passed, %d failed\n",
           tests_passed, tests_total, tests_failed);
    printf("========================================\n");

    /* Clean up */
    reasoning_shutdown();
    atomspace_shutdown();

    return tests_failed > 0 ? 1 : 0;
}
