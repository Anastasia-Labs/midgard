# TQ-15 — Consolidate fit evidence without losing live measurements

Status: Proposed
Last reviewed: 2026-09-07

- **Audit sections**: §7.1, §7, §10
- **Rules**: R5, R8, R9
- **Blocked by**: TQ-01, TQ-12

## Current behavior and boundary

A saved publication row and a test that builds, signs, and evaluates a current
transaction are different evidence. The saved row does not make the live test
redundant. Build-from-literals and rebuild-from-stored are workflows, not proof
that four mutually incompatible schemas exist.

After removal of unconsumed history, 43 fit-ledger JSON files remain under
`docs/fault-proofs/size-plans`: 25 have current-build/live comparison consumers
and 18 are retained self-consistency fixtures. There are 22
`*-publication-fit.test.ts` files and 40 `*-fit-ledger.test.ts` files. These are
file counts, not numbers of independent live measurements.

## What to do

1. Map each family from producer through artifact to consumer and CI command.
   Record whether the consumer evaluates current scripts, compares provenance,
   or checks only stored consistency. Preserve these distinct claims.
2. Share serialization/building code where the semantics match. Preserve
   family-specific accepted/forced lifecycle evidence and required fields;
   do not force unlike records into a lossy schema.
3. Use named bounds for their stated purpose. The 16,384-byte L1 ceiling and
   the 15,872-byte publication target encode different requirements. Check the
   family's governing requirement before replacing either. Report a failure
   against the required target; do not adjust the target to make a test pass.
4. Preserve the protocol maximum versus reserved execution basis distinction
   from GOAL_SPEC and TQ-03. Record blueprint, compiler, cost-model, environment,
   and measurement provenance where the evidence contract requires them.
5. Consolidate a live publication suite only after its replacement performs the
   same current build/sign/evaluation checks, relevant failures, row coverage,
   and provenance validation in the required lane. Adding a saved row alone is
   insufficient. Preserve current gates while developing the replacement.
6. Share family entry-name lists and repeated route-freedom helpers where doing
   so preserves an independent expected roster and isolated scenario state.
7. Correct stale emulator-limit prose, but keep explicit protocol parameters
   when they enforce the test's intended configuration even if they equal an
   upstream default today.

## Inventory and verification

From the repository root:

```bash
rg --files docs/fault-proofs/size-plans -g '*fit-ledger.json'
rg --files demo/midgard-fault-proofs/tests -g '*-publication-fit.test.ts' -g '*-fit-ledger.test.ts'
rg -n 'VAN_ROSSEM_PUBLICATION_TARGET_BYTES|maxTxSize|15_872|16_384' demo/midgard-fault-proofs/tests -g '*-publication-fit.test.ts'
```

Measure actual elapsed time before claiming a speedup. A 600-second timeout is
an upper bound, not the runtime of a suite. Report the exact family commands,
executed measurements, retained checks, and any missing prerequisites.

## Acceptance criteria

- Each removed live measurement has a named equivalent current producer and
  required consumer, demonstrated before removal.
- Required family row sets and byte/CPU/memory limits remain enforced.
- Stored consistency is never reported as fresh execution evidence.
- No required verifier input is removed or silently re-pinned.
