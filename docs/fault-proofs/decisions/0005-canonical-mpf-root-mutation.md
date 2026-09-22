# 0005 — Canonical compressed-prefix MPF root mutation

- Status: Accepted; implemented in production exclusion and mutation consumers
- Recorded: 2026-09-07, extracted from delivered plans and checked against source;
  this date is not a new protocol acceptance or measurement receipt.

## Context and decision

The pinned upstream forestry implementation drops the skipped prefix when
excluding a terminal Fork and selects the wrong neighbor nibble for a
nonterminal Leaf with a positive skip. Midgard uses its shared canonical
wrappers for exclusion, insertion, and deletion so authenticated roots agree
with the off-chain trie. Callers retain Midgard's empty-root sentinel
normalization. Membership continues to use the unaffected upstream `has`.

Do not restore direct upstream exclusion/mutation calls as a cleanup. Preserve
captured Fork/Leaf vectors, membership controls, and refusal of the dropped-prefix
root. Changing a shared verifier changes dependent scripts and requires a fresh
blueprint, deployment identity, and regenerated fit evidence.

## Authorities and regression evidence

- [Canonical wrappers](../../../onchain/aiken/lib/midgard/mpf-proof-v1.ak)
- [Exclusion validator](../../../onchain/aiken/validators/pexcludes.ak)
- [Transition root verification](../../../onchain/aiken/lib/midgard/transition-trace.ak)
- [Transition proof mutation](../../../onchain/aiken/lib/midgard/fraud-proofs/transition-trace/proof.ak)
- [Published consumer scenarios](../../../demo/midgard-fault-proofs/tests/mpf-prefix-consumers-emulator.test.ts)

The scenarios exercise real reference-script publication and execution for both
prefix shapes, and reject membership roots and the dropped-prefix exclusion root.
