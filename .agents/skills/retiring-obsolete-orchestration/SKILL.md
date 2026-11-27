---
name: retiring-obsolete-orchestration
description: Remove a redundant Midgard family runner or orchestration adapter after a shared workflow replaces it. Use when deleting legacy execution paths while retaining authenticated evidence, builders, validators, descendant cleanup and operator/prover economics.
---

# Retiring obsolete orchestration

Read [the executable recovery gates](../../../docs/agents/contrib.md#reusable-behavior-gates-and-fixtures)
before constructing another family-specific replacement runner.

Map retained users and responsibilities through declared exports and
`contrib locate --symbol`. Keep evidence authentication, transaction builders,
validators and payout identities with their existing owners.

Run the shared `recovery-scenarios` gate and the removed path's nearest caller
tests. Check both proof directions, zero/one/multiple descendants, distinct
operators, already-slashed cleanup, restart stages and rollback after completion.
The shared reference fixture does not prove production-engine coverage. [review]

Delete the unused orchestration only once those responsibilities have actual
replacement coverage. Avoid undeployed compatibility adapters that preserve the
old path. Finish with compiled entrypoint checks, exact receipts and explicit
remaining coverage gaps. [review]
