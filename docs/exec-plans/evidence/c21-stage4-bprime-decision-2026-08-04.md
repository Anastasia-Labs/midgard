# C21-STAGE4 Option B′ owner-decision resolution — 2026-08-04

## Decision

Do not implement a proof-item reference-input ABI for validation resolver 8 /
semantic resolver 0. The earlier Option B′ directive was valid against the
pre-Option-A stage-4 shape, but is now structurally incompatible with the
deployed semantic transition. Adding it would not create equivalent direct and
reference proofs; it would advertise a reference path that the script must
reject. This is an unsafe ABI expansion, so the recorded directive is
superseded by the source proof below.

This decision does not change the canonical-decode complete-item route
(resolver 0 / semantic resolver 1): that route continues to support both
direct and proof-item-reference carriage.

## Source proof

Option A changed `script_sources_stage_four` to accept only
`TransactionRedeemerItemBeginWitness { collection_proof }` (constructor tag 29) and fold the authenticated tuple
`(field_index, item_index, item_length, item_commitment)`.
`bounded_collection_v1.verify_item` binds that tuple to `outputs_hash`; the
stage-four successor no longer consumes `item_cbor`.

`ValidationProofItemDatumV1`, by contrast, reconstructs only a
`TransactionFieldItemWitness { collection_proof, item_cbor }` (constructor tag
30). That datum is the reference carrier for the canonical-decode item
validator, not an encoding of the tag-29 stage-four proof.

The generic ScriptSources non-output resolver is selected by resolver 8 /
semantic resolver 0 for stage four. Its only deployed action carries the
actual `ValidationAuxiliaryWitnessV1`, and it hashes that exact auxiliary into
the prepared evidence before checking the transition. Replacing tag 29 with
the proof-item datum's tag 30 therefore changes the evidence hash and fails
the stage-four pattern match. A `VerifyNonOutputReference` action that reads
the existing proof-item datum could never be direct/reference equivalent.

Authoritative locations:

- `onchain/aiken/lib/midgard/validation-machine-v1.ak`:
  `script_sources_stage_four` and
  `verify_script_sources_non_output_semantics_v1`.
- `onchain/aiken/lib/midgard/validation-machine-v1.ak`:
  `ValidationProofItemDatumV1`.
- `onchain/aiken/validators/fraud-proofs/validation-trace/script-sources-non-output-semantic-v1.ak`.
- `onchain/aiken/validators/fraud-proofs/validation-trace/canonical-decode-item-semantic-v1.ak`.

## Evidence and ABI partition

`demo/midgard-fault-proofs/tests/validation-dispute-submit.test.ts` pins all
three relevant facts against the generated Aiken blueprint:

1. resolver 0 / semantic 1 accepts direct and reference variants of its
   five-field `Verify` complete-item ABI;
2. resolver 8 / semantic 0 accepts its direct tag-29 non-output ABI; and
3. resolver 8 / semantic 0 rejects a proof-item reference index before
   redeemer emission.

The applied resolver build remains unchanged: the SDK applies the generic
non-output semantic validator with exactly `[award_script_hash,
computation_thread_policy_id]`. The existing applied-hash fixture covers its
resolver-8 position in the authenticated ScriptSources resolver group. No
blueprint, applied hash, catalogue registration, or deployed reference script
is changed by this disposition.

## §3.2 consequence

No new §3.2 necessity artifact is required. The cancelled B′ route is neither
a fallback nor a representation that can be semantically equivalent. Option A
removed the oversized byte carriage from stage four; the retained bounded
ledger-output traversal is already justified by
`docs/exec-plans/evidence/necessity/ledger-output-incremental-proof-v1.md`.
No existing artifact is invalidated.
