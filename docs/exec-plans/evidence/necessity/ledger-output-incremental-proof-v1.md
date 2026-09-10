# Ledger-output traversal measurement binding

Status: Measurement binding requires current-build revalidation.

Documentation reduced: 2026-09-07. No measurements were rerun or re-pinned.

This file remains because `demo/midgard-validation/tests/validation-machine.test.ts`
parses its JSON fence and checks the listed scanner consumers, blueprint,
compiler, validator identities, and fit arithmetic. The preserved pin is an input
to that check, not a claim of current deployment acceptance. A mismatch must
remain a failure until the producing measurements are repeated; editing the
identity alone cannot establish fit.

## Purpose and reproduction

One ledger output can occupy the entire L1 byte envelope before publication or
proof framing is added. Output verification also has to authenticate its Value,
datum, and native-script structure. The begin/step/finalize traversal bounds
that work while preserving the complete-item path for fitting outputs.

The former stage-four fold no longer needs a second full output reveal: it
passes the authenticated commitment and length to the structural verifier.
A reference-carriage ABI that changes the accepted witness type is not an
equivalent replacement. The current decomposition rationale is in
[Publishable semantic resolvers](../../../fault-proofs/decisions/0003-publishable-semantic-resolvers.md)
and [Checkpointed ledger-output facts](../../../fault-proofs/decisions/0004-checkpointed-ledger-output-facts.md).

The pin records earlier measurements, including a 16,384-byte output that did
not fit direct or single-publication framing. Its 398-validator blueprint and
bound script hashes are deliberately not refreshed by this cleanup. Reproduce
against the current pinned testnet build using the complete-item proof-fit,
carriage, and equivalence suites in `demo/midgard-validation/tests/`; use
[the transaction specification](../../../spec/midgard-tx.md) for the current
carriage rules. Do not treat an old byte frontier as the current profile.

## Machine-readable pin

```json
{
  "artifact": "ledger-output-incremental-proof-v1",
  "blueprintSha256": "f49cae224f24cfab577f1ed10b5340384b75e541851eb7b77b507a79cb7d5e00",
  "compilerVersion": "v1.1.23+2a78108",
  "validatorCount": 398,
  "definitionCount": 702,
  "consumers": ["lib/midgard/ledger-output-scan-v1.ak"],
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
      "name": "complete 16,384-byte output direct in proof tx (Verify redeemer framed)",
      "unit": "txBytes",
      "measured": 16900,
      "limit": 16384,
      "fits": false
    },
    {
      "name": "complete 16,384-byte output as inline-datum publication + reference",
      "unit": "txBytes",
      "measured": 18290,
      "limit": 16384,
      "fits": false
    },
    {
      "name": "maximum output preimage against the tier-2 single-step frontier",
      "unit": "itemBytes",
      "measured": 16384,
      "limit": 15148,
      "fits": false
    },
    {
      "name": "begin/step/finalize walk, worst pinned step publication",
      "unit": "txBytes",
      "measured": 4675,
      "limit": 16384,
      "fits": true
    },
    {
      "name": "begin/step/finalize walk, worst pinned step memory",
      "unit": "memoryUnits",
      "measured": 3398228,
      "limit": 13200000,
      "fits": true
    },
    {
      "name": "begin/step/finalize walk, worst pinned step cpu",
      "unit": "cpuUnits",
      "measured": 1209745039,
      "limit": 8000000000,
      "fits": true
    }
  ]
}
```
