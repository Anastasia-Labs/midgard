# Redeemer-item traversal measurement binding

Status: Measurement binding requires current-build revalidation.

Documentation reduced: 2026-09-07. No measurements were rerun or re-pinned.

This file remains because `demo/midgard-validation/tests/validation-machine.test.ts`
parses its JSON fence and checks the listed scanner consumers, blueprint,
compiler, validator identities, and fit arithmetic. The preserved pin is an input
to that check, not a claim of current deployment acceptance. A mismatch must
remain a failure until the producing measurements are repeated; editing the
identity alone cannot establish fit.

## Purpose and reproduction

Carrying a complete redeemer preimage does not establish the cost of traversing
its nested Data. The proof must derive the typed summary required by the CEK
context rather than trust a supplied summary. Staged item ingestion and
`DataTraverse` actions authenticate bounded progress over that same item;
fitting complete-item carriage remains available.

This binding covers Data-level item ingestion in `redeemer-item-proof-v1.ak`,
not every script-purpose or CEK context decomposition. The retained measurements
include earlier inline/reference frontiers and a bounded chunk publication;
they do not prove a current full maximum-depth execution budget. The
398-validator blueprint and script identities remain unchanged so stale evidence
continues to fail the consumer test.

Reproduce with the complete-item proof-fit, carriage, equivalence, and
`nested-redeemer-data-boundary.test.ts` suites in
`demo/midgard-validation/tests/`. Current carriage semantics are in
[the transaction specification](../../../spec/midgard-tx.md), and the
continuation decision is in [Publishable semantic resolvers](../../../fault-proofs/decisions/0003-publishable-semantic-resolvers.md).

## Machine-readable pin

```json
{
  "artifact": "redeemer-item-traversal-v1",
  "blueprintSha256": "f49cae224f24cfab577f1ed10b5340384b75e541851eb7b77b507a79cb7d5e00",
  "compilerVersion": "v1.1.23+2a78108",
  "validatorCount": 398,
  "definitionCount": 702,
  "consumers": ["lib/midgard/redeemer-item-proof-v1.ak"],
  "boundValidators": [
    {
      "title": "fraud_proofs/validation_trace/canonical_decode_item_semantic_v1.main.spend",
      "hash": "62501cfe7cf63485a493c902060cd422acdd88757c319345eadb8819"
    },
    {
      "title": "fraud_proofs/validation_trace/proof_item_v1.main.else",
      "hash": "22c9a103ed3f2fa97c982d76d6e2af50c5d54ac306983b196c8fcdab"
    }
  ],
  "measurements": [
    {
      "name": "tier-1 signed inline carriage of the nominal 14,336-byte redeemer-item cap",
      "unit": "itemBytes",
      "measured": 14336,
      "limit": 14004,
      "fits": false
    },
    {
      "name": "tier-2 complete-preimage door at the 32,768-byte aggregate field cap",
      "unit": "itemBytes",
      "measured": 32768,
      "limit": 15148,
      "fits": false
    },
    {
      "name": "staged DataTraverse step, worst pinned publication",
      "unit": "txBytes",
      "measured": 4675,
      "limit": 16384,
      "fits": true
    },
    {
      "name": "staged DataTraverse step, bounded_item_v1 chunk reveal",
      "unit": "chunkBytes",
      "measured": 4095,
      "limit": 4095,
      "fits": true
    }
  ]
}
```
