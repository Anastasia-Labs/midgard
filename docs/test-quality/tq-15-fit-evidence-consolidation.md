# TQ-15 — Consolidate fit evidence without losing live measurements

Status: Done
Last reviewed: 2026-10-16

- **Audit sections**: §7.1, §7, §10
- **Rules**: R5, R8, R9

## Outcome

Fit evidence is now one live check instead of saved files. The fault-proof
suite's setup file
[`van-rossem-submit-guard.ts`](../../demo/midgard-fault-proofs/tests/support/emulator/van-rossem-submit-guard.ts)
judges every transaction any emulator test submits, before the emulator sees
it: over 16,384 signed bytes, 16.5M memory units or 10G CPU units, or, for a
publication (zero execution units), over the 15,872-byte publication target,
the submission is refused and the test fails, naming the axis and its negative
margin. Its behavior is pinned by
[`van-rossem-submit-guard.test.ts`](../../demo/midgard-fault-proofs/tests/van-rossem-submit-guard.test.ts).

The per-family fit ledger JSON files under `docs/fault-proofs/size-plans`,
their writers, their regeneration command and its test were removed. No saved
row stood in for a live check: the guard measures every current transaction
of every run, which is what the ledgers recorded once. The
`*-publication-fit.test.ts` suites, the lifecycle suites' own per-transaction
assertions (including tighter reliability-margin bounds such as 13.2M memory
and 8G CPU where a family states them) and their coverage gates remain.

## Acceptance criteria

- Each removed live measurement has a current producer and consumer: the
  emulator test that submits the transaction, and the submit guard.
- Byte, CPU and memory limits remain enforced on every emulator submission.
- Stored consistency is never reported as fresh execution evidence.
