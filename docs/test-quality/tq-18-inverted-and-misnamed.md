# TQ-18 — Separate readiness fixtures from ambient status pins

Status: Proposed
Last reviewed: 2026-09-07 (semantic source review)

- **Audit sections**: §13 and §14
- **Rules**: R1, R3, R5, R7 in [principles](principles.md)

## Problem and boundary

A test can legitimately assert that an explicitly missing capability, invalid
artifact, or untrusted aggregate remains blocked. Implementing the capability
elsewhere must not cause that negative fixture to become trusted. A different
kind of test pins the checkout's ambient readiness; that may need review when
readiness changes, but it is not automatically an inverted oracle.

A name should match what the body establishes. Before renaming or expanding a
test, trace its setup, production calls, assertions, and current consumers.
The original audit's counts and line references are not a current inventory.

## Confirmed source distinctions

- [State-correction acceptance tests](../../demo/midgard-node-tools/tests/e2e-state-correction-acceptance.test.ts)
  explicitly pass `availabilityChallengeCapability: "missing"`. A blocked
  availability gate is correct for that fixture. The aggregate-evidence case
  also remains blocked pending independent provenance; a complete-looking
  aggregate is not authority. Preserve both fail-closed checks.
- The same suite consults the compiled production workflow registry and pins
  its current blocked state/missing count. Review that ambient-status portion
  separately; a controlled readiness fixture or explicit installation
  invariant may make its intended scope clearer.
- [Consensus-profile tests](../../demo/midgard-core/tests/consensus-profile.test.ts)
  assert the currently absent release-evidence digest and that activation
  refuses. That is an explicit current-release gate. Do not make it
  conditionally pass for arbitrary present evidence or remove the refusal
  test to accommodate implementation progress. Activation requires the
  repository's validator-bound evidence verification contract.
- [Typed reason disposition](../../demo/midgard-fault-proofs/tests/typed-reason-disposition.test.ts)
  now requires an empty missing-installation set. The old four-entry residue
  example is stale, and the current assertion detects missing runner coverage.
- Lifecycle coverage checks that deliberately demonstrate missing reason
  arms should remain negative tests. If they instead pin accidental omissions
  in the live installed surface, record that distinction and the required
  completion gate before changing them.

## Naming review candidates

Process-harness tests that launch test-written scripts should be described as
harness/process-supervision checks unless they also drive real node election.
Router construction alone is narrower than route authorization; inspect the
routes and requests when the name claims those behaviors. A datum round-trip
claim about a beneficiary must actually vary or compare that beneficiary.
Handwritten configuration-key arrays do not by themselves establish the
production API's permitted fields.

Likewise, encoding mutation sensitivity is not validator refusal, and
catalogue internal wiring is not proof of every real applied category. TQ-08
and TQ-17 cover those distinctions. Do not label a suite permanently red or
universally skipped without checking its current execution and prerequisite
contract.

## Work and acceptance

1. Classify every proposed change as an explicit negative fixture, release
   policy pin, ambient installation observation, or overbroad name.
2. Preserve required missing-capability, absent-evidence, untrusted-provenance,
   and unsupported-profile refusals. Add controlled positive fixtures only
   when their authenticated prerequisites can be satisfied.
3. For a mismatched name, either narrow the name or strengthen the assertion
   to exercise the claimed boundary. Preserve useful narrower coverage and
   check CI/journal references before renaming an exact selector.
4. Run focused checks and report which readiness facts are fixture-controlled
   versus read from the current checkout. Do not replace gaps with an
   exemption list that silently converts incomplete coverage into acceptance.

Success is accurate claims and preserved fail-closed behavior, not zero tests
that mention `blocked`, `null`, or unfinished release work.
