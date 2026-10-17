# Redeemer-collection total-decode measurement binding

Status: Measurement binding requires current-build revalidation.

Documentation reduced: 2026-09-07. No measurements were rerun or re-pinned.

This file remains because `demo/midgard-validation/tests/validation-machine.test.ts`
parses its JSON fence and checks the listed scanner consumers, blueprint,
compiler, validator identities, and fit arithmetic. The preserved pin is an input
to that check, not a claim of current deployment acceptance. A mismatch must
remain a failure until the producing measurements are repeated; editing the
identity alone cannot establish fit.

## Purpose and reproduction

Missing-redeemer, redeemer-canonicity, and unused-redeemer proofs adjudicate the
complete committed redeemer collection. Absence and exhaustiveness require a
walk over every relevant item, not one favorable membership proof. The field's
maximum shape exceeds complete direct/publication framing, so the routes use
certified carriage and a resumable batched walk.

Within a batch, every item is available in full. `canonical_cbor_scan_v1.head_at_v1`
is needed for total header decoding: malformed bytes must return the rule's
exact result rather than abort through a partial typed decode. It does not
introduce an additional partial-byte publication protocol.

The preserved pin binds the three current consumers and measured lifecycle rows.
Its reproduction inputs are the missing-redeemer, redeemer-canonicity, and
unused-redeemer family emulator suites and fit ledgers under
`docs/fault-proofs/size-plans/`. Current coverage is in
[the family reference](../../../fault-proofs/family-reference.md); the
[narrow proof-thread decision](../../../fault-proofs/decisions/0002-bounded-proof-threads.md)
explains the exhaustive-walk requirement.

## Machine-readable pin

```json
{
  "artifact": "redeemer-collection-total-decode-v1",
  "blueprintSha256": "caaf9849fe9d66b1bba2a2ba082c18857e9c16059351e70fcdc6f61781191652",
  "compilerVersion": "v1.1.23+5adf783",
  "validatorCount": 1131,
  "definitionCount": 1844,
  "consumers": [
    "lib/midgard/fraud-proofs/missing-redeemer/rule.ak",
    "lib/midgard/fraud-proofs/redeemer-canonicity/rule.ak",
    "lib/midgard/fraud-proofs/unused-redeemer/rule.ak"
  ],
  "boundValidators": [],
  "measurements": [
    {
      "name": "complete field-8 redeemer collection direct in proof tx",
      "unit": "txBytes",
      "measured": 35186,
      "limit": 16384,
      "fits": false
    },
    {
      "name": "complete collection as inline-datum publication + reference",
      "unit": "txBytes",
      "measured": 35186,
      "limit": 16384,
      "fits": false
    },
    {
      "name": "certified §8 carriage, worst signed row (accepted-carriage-chunk-0)",
      "unit": "txBytes",
      "measured": 15872,
      "limit": 16384,
      "fits": true
    },
    {
      "name": "batched pointer walk, worst execution memory (missing-redeemer)",
      "unit": "memoryUnits",
      "measured": 5019938,
      "limit": 16500000,
      "fits": true
    },
    {
      "name": "batched pointer walk, worst execution cpu (missing-redeemer)",
      "unit": "cpuUnits",
      "measured": 2182994737,
      "limit": 10000000000,
      "fits": true
    },
    {
      "name": "batched pointer walk, worst execution memory (redeemer-canonicity)",
      "unit": "memoryUnits",
      "measured": 1678487,
      "limit": 16500000,
      "fits": true
    },
    {
      "name": "batched pointer walk, worst execution cpu (redeemer-canonicity)",
      "unit": "cpuUnits",
      "measured": 571611300,
      "limit": 10000000000,
      "fits": true
    }
  ]
}
```
