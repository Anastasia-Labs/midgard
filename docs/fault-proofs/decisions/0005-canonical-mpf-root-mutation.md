# 0005 — Canonical compressed-prefix MPF root mutation

- Status: Accepted; implemented in production exclusion and mutation consumers
- Recorded: 2026-09-07, extracted from delivered plans and checked against source;
  this date is not a new protocol acceptance or measurement receipt.

## Context and decision

Upstream forestry 2.0.0 drops the skipped prefix when excluding a terminal Fork
and selects the wrong neighbor nibble for a nonterminal Leaf with a positive
skip; 2.0.1 corrects the nibble and 2.1.0 the prefix. The pinned implementation
is the `anastasia-labs/merkle-patricia-forestry` fork of upstream 2.1.0.
Midgard still uses its shared canonical wrappers for exclusion, insertion, and
deletion so authenticated roots agree with the off-chain trie through one path
it owns. Callers retain Midgard's empty-root sentinel normalization. Membership
continues to use the library `has`.

Leaf and branch node preimages are disjoint. A leaf at an even cursor commits
`0xff ‖ path[c/2..]`, and a leaf at an odd cursor commits
`0x10 ‖ nibble(path, c) ‖ path[(c+1)/2..]`, each followed by the value hash. A
branch commits its prefix nibbles (each below `0x10`) followed by its 32-byte
merkle root. So a leaf preimage is at least 33 bytes and starts with a byte of
at least `0x10`, while a branch preimage either starts with a byte below `0x10`
or is exactly 32 bytes: the first byte, or the length, decides the node kind.
Every Midgard encoder of these nodes (the Aiken verifiers, the TypeScript proof
fold, PHAS walk and catalogue reconstruction, and the native MPF owner) uses
this encoding; the `mpf-node-encoding-v1` golden channel pins all of them to the
library's own hashes.

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
