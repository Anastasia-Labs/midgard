# TQ-08 — Remove literal self-checks without deleting regression coverage

Status: Proposed
Last reviewed: 2026-09-07 (semantic source review)

- **Audit sections**: §1.2 and non-provenance candidates in §1.1
- **Rules**: R1, R5, R8 in [principles](principles.md)
- **Coordination**: TQ-01 owns provenance; TQ-15 owns fit-ledger consolidation.

## Problem and boundary

An assertion that compares the same immutable value with itself provides no
regression signal. That does not describe every comparison involving a
production helper, a derived field, a checksum, or a property the current
producer guarantees. Those checks can detect changes to the producer, damaged
saved artifacts, or disagreements between projections.

The older audit's counts and deletion list are investigation leads, not a
verified set of redundant assertions. Review both operands and the consumer
before proposing deletion.

## Source corrections

- [Van Rossem fit construction](../../demo/midgard-fault-proofs/src/proof-fit/van-rossem-fit-ledger.ts)
  rejects non-positive margins. A test of that guarantee can still detect its
  removal, and checks against saved JSON do not necessarily invoke the current
  producer. Margin arithmetic, ledger digests, and reconstructed commitments
  may be integrity checks; they do not independently prove fresh measurement.
- [DA conflict evidence](../../demo/da-committee-node/tests/conflict-evidence.test.ts)
  intentionally uses the same header with different availability commitments
  and bond owners. Signatures authenticate those commitments. Replacing one
  header merely to make the hashes distinct would change the intended case.
- [The PlutusData corpus](../../demo/midgard-core/tests/plutus-data-wellformed.test.ts)
  combines fixed vectors with 6,000 generated entries. Its greater-than-6,000
  guard can detect omission of the fixed corpus; it is not an assertion about
  an isolated `Array.from({ length: 6_000 })`.
- [Redeemer mutation controls](../../demo/midgard-sdk/tests/fraud-proof-rebind-604.test.ts)
  invoke `Data.to` with SDK schemas. Comparing honest and mutated encodings
  legitimately establishes that a mutation landed. That check alone does not
  prove on-chain refusal; keep the test name and surrounding claim precise.
- The previously cited literal self-comparison in the native-script lifecycle
  and reverse-checkpoint example have changed. Re-read current source before
  reopening either candidate.

## Focused work

1. Identify literal self-comparisons and expectations populated directly from
   the observation they claim to predict. State the intended regression before
   replacing them with an independent oracle.
2. In regeneration modes, distinguish writing a new artifact from verifying a
   previously recorded one. Equality immediately after overwriting the file
   is not an independent freshness check, but other schema/semantic assertions
   in the same test may remain useful.
3. Review the final direct fake-client DML assertion in
   [public-retained-da-store.test.ts](../../demo/da-committee-node/tests/public-retained-da-store.test.ts).
   It checks the test stub's simulated rejection. Earlier calls exercise the
   production store and must not be discarded with it. A real PostgreSQL
   read-only claim needs a real PostgreSQL boundary test.
4. Preserve round trips, mutation sensitivity, deterministic encoding checks,
   and independently stated artifact invariants unless equivalent coverage is
   demonstrated elsewhere.

## Acceptance

For each changed assertion, record what it exercised, why it was redundant or
misstated, and the remaining regression detector. Run the affected suite and
show the replacement distinguishes its intended regression. A lower test count
or an unchanged green result before and after deletion is not sufficient
coverage evidence. Do not replace this review with a blanket syntax ban.
