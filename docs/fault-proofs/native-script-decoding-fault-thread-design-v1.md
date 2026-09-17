# Native-script-decoding fault thread — current design

The implemented and registered canonical
category is `nativeScriptDecoding` (`0000000d`). The family proves both
wrongful acceptance of an undecodable resolved reference native script and
wrongful forced rejection under `ResolvedReferenceScriptMalformed`,
`ResolvedReferenceScriptNodeLimit`, or `ResolvedReferenceScriptDepthLimit`.
Witness, own-output, and execution-source script decoding have separate
catalogue families.

## 1. Security statement

The challenged script bytes and their subject are authenticated by the
committed transaction, prior ledger state, and—where the operator rejected a
forced transaction—the typed `RejectionReason` subject. The proof must reach
the deterministic native-script decoder's true result without trusting a
prover-supplied verdict, byte offset, or unbound outpoint.

The family is single-party because decode/canonicity is a deterministic
predicate over public authenticated bytes. Plutus/CEK execution is outside this
family.

## 2. Claim directions

- Direction A: an operator-accepted transaction references a native script
  payload that the canonical decoder rejects.
- Direction B: an operator-rejected forced transaction names a scan-borne
  native-script decode fault, but the authenticated payload decodes
  successfully.

Direction B binds the exact source kind and input ordinal carried by the typed
rejection reason. The family does not universally scan unrelated outpoints.

## 3. Contract set and state machine

The on-chain implementation is under
`onchain/aiken/validators/fraud-proofs/native-script-decoding/` with reusable
state/engine logic under
`onchain/aiken/lib/midgard/fraud-proofs/native-script-decoding/`.

The logical chain is Init → step 01 → step 02 → resumable step 03 → step 04:

1. Step 01 authenticates the challenged transaction/source and claim direction.
2. Step 02 binds the exact script-bearing subject and committed descriptor.
3. Step 03 opens the subject and advances the bounded cursor/stack scan across
   as many L1 transactions as required. Its physical validators split subject
   opening, descriptor binding, and advance/close so each spend remains within
   the supported execution envelope.
4. Step 04 compares the deterministic verdict with the operator claim, burns
   the computation thread, and mints the permanent fault-proof token.

Every state transition carries the claim, subject, cursor, stack commitment,
and descriptor forward. Every step supports explicit prover cancellation.

## 4. Registration and deployment

`nativeScriptDecoding` is part of the canonical catalogue with explicit category IDs. Generic
Init, deployment inspection, node/core manifest identity, watcher proof-thread
authority, and all physical step validators are wired as mandatory
authenticated reference scripts. Catalogue changes require a fresh deployment;
there is no compatibility or migration branch.

## 5. Byte authentication

The scan consumes authenticated field openings and the
`reference_script_item_commitment` for the selected subject. Direct and
published carriage are transport choices only; neither changes the committed
hash or lets the prover substitute bytes. Item ordinals are checked against the
canonical field count and fixed item grammar. A prover's out-of-domain accepted-invalid accusation refuses. A forced
operator rejection carrying an impossible source kind or input ordinal instead
contradicts that rejection directly: the authenticated direction-B bind path
closes to step 04 without claiming to have decoded a nonexistent script.

## 6. Execution and proof-fit contract

The scan is resumable and carries only bounded cursor/stack state between L1
transactions. The authoritative measurements are the current ledgers under
`onchain/aiken/scripts/`, especially:

- `native-script-decoding-engine-exec-ledger-v1.json`
- `verify-native-script-decoding-engine-exec-ledger-v1.mjs`

Those measurements must be refreshed when the compiler, validator bytes,
decoder, native-script limits, field bounds, or Cardano protocol limits change.
No prose estimate overrides the measured ledger.

## 7. Adversarial requirements

Verification must retain controls for:

- honest accepted and rejected claims;
- substituted transaction, subject, source kind, or ordinal;
- descriptor/hash/chunk mutation;
- malformed, truncated, overlong, and out-of-domain payloads;
- node/depth guardrails and exact boundary behavior;
- crash/resume and explicit cancellation;
- terminal thread burn/permanent proof mint coupling; and
- faulty-block removal after conviction.

The Aiken selectors and emulator suites cover both directions,
both polarities, negative/resume behavior, permanent mint, and removal. The
compiled-size envelope suite protects the physical reference-script topology.

## 8. Acceptance

The manifest-bound runner and watcher installation exist. Keep the current
execution ledgers and applied-script identity paired; exercise installed recovery
and both claim directions on the release build. Live/preprod acceptance and the
complete maximum-shape corpus remain governed by the
[remaining acceptance plan](execution-plan.md).
