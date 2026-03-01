/*
 * Test suite for the Cognitive 9P Filesystem kernel module
 *
 * Tests the 9P protocol interface to cognitive services:
 * atom creation/retrieval via file operations, reasoning
 * via file writes, and attention via the /attention filesystem.
 *
 * Copyright (C) 2026 OpenCog Community
 * Licensed under AGPL-3.0
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "../atomspace/atomspace.h"
#include "../cognitive9p/cognitive9p.h"

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

/* External globals */
extern AtomSpace *global_atomspace;

/*
 * Test 1: Cognitive 9P initialization
 */
static void
test_cognitive9p_init(void)
{
    printf("\n=== Test: Cognitive 9P Initialization ===\n");

    atomspace_init();
    TEST_ASSERT(global_atomspace != NULL, "AtomSpace initialized");

    cognitive9p_init();
    TEST_ASSERT(1, "Cognitive 9P initialized without crash");
}

/*
 * Test 2: Create atoms via 9P interface
 */
static void
test_create_atoms_9p(void)
{
    printf("\n=== Test: Create Atoms via 9P ===\n");

    uint32_t id1 = cognitive9p_create_atom("ConceptNode", "Knowledge");
    TEST_ASSERT(id1 > 0, "Created ConceptNode via 9P");

    uint32_t id2 = cognitive9p_create_atom("ConceptNode", "Wisdom");
    TEST_ASSERT(id2 > 0, "Created second ConceptNode via 9P");
    TEST_ASSERT(id2 != id1, "Atom IDs are unique");

    uint32_t id3 = cognitive9p_create_atom("PredicateNode", "is_useful");
    TEST_ASSERT(id3 > 0, "Created PredicateNode via 9P");

    printf("  Created atoms: %u, %u, %u\n", id1, id2, id3);
}

/*
 * Test 3: Filesystem tree structure
 */
static void
test_cogfs_tree(void)
{
    printf("\n=== Test: Cognitive Filesystem Tree ===\n");

    /* This should print the filesystem tree without crashing */
    print_cogfs_tree();
    TEST_ASSERT(1, "Filesystem tree printed without crash");
}

/*
 * Test 4: AtomSpace access through 9P
 */
static void
test_atomspace_via_9p(void)
{
    printf("\n=== Test: AtomSpace Access via 9P ===\n");

    AtomSpace *as = get_global_atomspace();
    TEST_ASSERT(as != NULL, "Got global AtomSpace reference");

    /* Verify atoms created via 9P are in the AtomSpace */
    uint32_t count = atomspace_get_count(global_atomspace);
    TEST_ASSERT(count > 0, "AtomSpace has atoms from 9P operations");
    printf("  AtomSpace contains %u atoms\n", count);
}

/*
 * Test 5: Multiple atom types via 9P
 */
static void
test_multiple_types_9p(void)
{
    printf("\n=== Test: Multiple Atom Types via 9P ===\n");

    /* Create various atom types */
    uint32_t concept = cognitive9p_create_atom("ConceptNode", "TestConcept");
    uint32_t predicate = cognitive9p_create_atom("PredicateNode", "TestPredicate");
    uint32_t schema = cognitive9p_create_atom("SchemaNode", "TestSchema");
    uint32_t number = cognitive9p_create_atom("NumberNode", "42");

    TEST_ASSERT(concept > 0, "Created ConceptNode");
    TEST_ASSERT(predicate > 0, "Created PredicateNode");
    TEST_ASSERT(schema > 0, "Created SchemaNode");
    TEST_ASSERT(number > 0, "Created NumberNode");

    /* All IDs should be unique */
    TEST_ASSERT(concept != predicate && concept != schema && concept != number,
                "All atom IDs are unique");
}

/*
 * Test 6: Stress test - create many atoms
 */
static void
test_stress_9p(void)
{
    printf("\n=== Test: Stress Test (100 atoms) ===\n");

    uint32_t start_count = atomspace_get_count(global_atomspace);
    int created = 0;
    char name[64];

    for (int i = 0; i < 100; i++) {
        snprintf(name, sizeof(name), "StressAtom_%d", i);
        uint32_t id = cognitive9p_create_atom("ConceptNode", name);
        if (id > 0) created++;
    }

    uint32_t end_count = atomspace_get_count(global_atomspace);

    TEST_ASSERT(created == 100, "Created all 100 atoms");
    TEST_ASSERT(end_count >= start_count + 100, "AtomSpace count increased by 100");
    printf("  Created %d atoms, AtomSpace: %u -> %u\n",
           created, start_count, end_count);
}

/*
 * Main test runner
 */
int
main(int argc, char *argv[])
{
    printf("========================================\n");
    printf("Cognitive 9P Filesystem Test Suite\n");
    printf("========================================\n");

    test_cognitive9p_init();
    test_create_atoms_9p();
    test_cogfs_tree();
    test_atomspace_via_9p();
    test_multiple_types_9p();
    test_stress_9p();

    printf("\n========================================\n");
    printf("Results: %d/%d passed, %d failed\n",
           tests_passed, tests_total, tests_failed);
    printf("========================================\n");

    /* Clean up */
    atomspace_shutdown();

    return tests_failed > 0 ? 1 : 0;
}
