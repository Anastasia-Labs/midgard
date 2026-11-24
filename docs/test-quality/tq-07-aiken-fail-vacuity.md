# TQ-07 — Check fixture attribution in Aiken expected-failure tests

Status: Proposed
Last reviewed: 2026-11-23 (cited tests and focused runner)

Rule: R3.

## Problem and current evidence

An Aiken `fail` test expects evaluation failure. If its intended claim concerns
a validator but setup can fail first, the result may not establish that claim.
A test specifically checking refusal by a decoder or field-opening function,
however, need not invoke an unrelated spending validator.

Locate cases by current test name and inspect their full setup and oracle
before classifying them. A current candidate is
`tier_three_variable_width_field_cannot_be_walked` in
`native-tx-machine-walk-v1.test.ak`, which exercises `open_certified`. Its
boundary claim requires review, not an automatic demand that it call a whole
transaction validator.

## Proposed work

1. Trace each candidate's fixture construction and intended refusal boundary.
   Add an adjacent successful control when it proves that shared setup reaches
   that boundary. A positive control alone does not establish the negative's
   exact failure site if the two paths diverge earlier.
2. When a total function returns `Bool`, directly assert its expected boolean
   result so unexpected setup aborts fail the test. Retain `fail` where an
   abort is the function's documented refusal semantics.
3. Review tests whose names promise semantic verification or content addressing
   but only check shape. Use independent expected encodings/digests and
   relevant field sensitivity where that is the contract; two differing hashes
   alone do not prove complete content-addressing correctness.
4. Keep changes scoped to confirmed oracle gaps. Do not remove production
   validator checks or construct a vulnerability reproduction as part of this
   documentation task.

## Focused verification procedure

Use the repository's guarded runner with the full source module name, omitting
only `.ak`, and exact test identifiers. For example, from `onchain/aiken`:

```bash
MIDGARD_AIKEN_ENV=testnet node scripts/run-focused-check.mjs \
  midgard/native-tx-machine-walk-v1.test \
  tier_three_variable_width_field_cannot_be_walked
```

The runner builds Aiken `-m 'module_prefix.{test_name}'` selectors internally
and validates the reported full module identity, exact collection count and
pass count. Do not pass a shortened module to the runner or replace it with
bare `-m validation_machine_v1 | tail`: those forms can hide zero collection
or the actual failure status. Follow the Aiken build skill for the pinned fork.

## Acceptance

- Each adjudicated candidate has an oracle matching its actual boundary and
  relevant successful setup controls.
- Current test names and recorded commands collect the intended tests and
  fail on missing or unexpected collection.
- Legitimate expected-abort tests remain intact.
- Record actual results and unresolved attribution limits; this proposal does
  not certify all Aiken failure tests or all protocol guards.
