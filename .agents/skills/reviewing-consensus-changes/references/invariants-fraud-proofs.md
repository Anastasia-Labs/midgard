# Fraud-proof invariants

Scope: `onchain/aiken/validators/fraud-proofs/`,
`onchain/aiken/lib/midgard/fraud-proofs/`, the computation thread and
fraud-proof policies, `demo/midgard-sdk/src/fraud-proof/`, and
`demo/midgard-fault-proofs/`.

Status, recurrence and line conventions are as in
[invariants-state-queue.md](invariants-state-queue.md): **VERIFIED** means
code, a refusing test and provenance were all read; **PARTIAL** names what is
missing.

## FP1. Every script is applied with exactly its declared parameters

Status: PARTIAL (runtime door only; no lint stops a new bypass).

Rule: a validator is applied through one door that refuses any parameter list
whose length differs from the blueprint's declaration. Under Plutus V3 an
under-applied script is a different script that can succeed on anything.

Enforced: `applyBlueprintParams`
(`demo/midgard-sdk/src/fraud-proof/contracts/blueprint.ts:211-227`)
`[runtime: applyBlueprintParams]`; every strict prefix of the parameter list
must hash differently from the deployed script
(`demo/midgard-sdk/tests/validation-resolver-applied-hashes.test.ts:149-165`);
`demo/midgard-fault-proofs/tests/emulator-blueprint-boundary.test.ts`.

Provenance: #605 found ten semantic resolvers deployed with two of their three
parameters, which made them succeed for any prover; `ac54d01a1` (#609)
applied the third parameter and added the arity guard to every application.

Recurrence: 1 live instance, 10 validators. Blind spot: #610 (OPEN) —
`phas` membership is deployed bare from `compiledCode`, outside the door.

## FP2. On chain trusts deployment parameters

Status: VERIFIED.

Rule: a validator does not re-check the shape of its own deployment
parameters; the SDK checks them once at application.

Enforced: `assertParameterShapes`
(`demo/midgard-sdk/src/fraud-proof/contracts/blueprint.ts:364`)
`[runtime: assertParameterShapes]`; policy in
[contracts.md](../../../../docs/agents/contracts.md).

Provenance: `fa52b3844` (2026-09-04) removed about 22 parameter-only checks
from 15 validators and deleted the tests that pinned them.

Review action: a new on-chain check that only inspects a parameter is a
finding against the change, not a hardening.

## FP3. Fault proofs execute published reference scripts

Status: PARTIAL (runtime fail-closed only; one emulator-only escape remains).

Rule: fault-proof steps and their supporting witness scripts run as published
reference scripts, hash-checked against the script the transaction executes.
A missing entry fails; there is no inline fallback.

Enforced: `requireWitnessReferenceScriptUtxo`
(`demo/midgard-fault-proofs/src/witness-reference-scripts.ts:47`, throws at
`:96`) and `requireLinearFaultReferenceScript`
(`demo/midgard-fault-proofs/src/linear-fault-family.ts:69`)
`[runtime: requireWitnessReferenceScriptUtxo]`.

Provenance: the file header records the owner ruling of 2026-08-26
(`witness-reference-scripts.ts:7-8`); `d3df3323e` (2026-08-29) removed the
inline fallback.

Blind spot: `inline_emulator_only` certificate witnesses
(`demo/midgard-sdk/src/fraud-proof/field-preimage-carriage.ts:859`, `:920`)
still attach inline, by design for the emulator. Nothing scans for a new
inline attach.

## FP4. Only the pinned compiler builds consensus code

Status: VERIFIED.

Rule: blueprints and gate results come from the pinned Aiken fork, never
stock Aiken.

Enforced: `[ci: aiken-ci.yml/Assert the pinned compiler identity and put it on PATH]`
(`.github/workflows/aiken-ci.yml:56-60`),
`[script: onchain/aiken/scripts/pinned-compiler.mjs]`, `[hook: pre-commit]`.

Provenance: #521 — stock v1.1.22 generated under-strict decoders when two
modules declared the same type name (`ValueWitnessV1` in `cek_builtin_v1` and
`cek_machine_v1`); `a954669fc` (2026-08-04) renamed the collision, and the
fork carries the compiler fix.

Recurrence: 2. Blind spot: the pre-commit hook skips when no compiler is
found, and `aikup` can silently repoint the local `aiken` to stock.

## FP5. Witness-field openings check the anchored witness-set hash

Status: VERIFIED.

Rule: a step opening a witness-set field compares the recomputed
`witness_set_hash` with the one anchored in the thread, not only with the
transaction id. The transaction id does not cover the witness set
([midgard-tx.md](../../../../docs/spec/midgard-tx.md) §3, lines 159-170).

Enforced: `expect verified.tx_compact.witness_set_hash == witness_set_hash`
(`lib/midgard/fraud-proofs/field-opening-v1.ak:307`)
`[aiken-test: midgard/fraud-proofs/field-opening-v1.test/]`:
`forged_witness_set_hash_re_derives_the_same_tx_id` (`:308`, the premise),
`forged_witness_set_and_compact_is_refused_under_the_anchor` (`:326`, fail),
`tier_three_certificate_over_a_witness_field_binds_to_nothing_committed`
(`:488`), `certified_carriage_opens_the_address_witness_field_with_the_welded_hash`
(`:653`).

Provenance: the #575 review found the first rebind "re-derived the
witness-set hash but dropped the MPF anchoring", reproduced end to end;
`2fec6b0fb` restored it. `d14c3e9aa` (#606, 2026-08-16) welded `field_hash`
into the preimage-certificate mint, so a certificate minted over a fabricated
witness set carries the fabricated hash and fails the door equality.

Recurrence: 2. The opposite error also happened: proposing to add
`witness_set_hash` to the transaction-id preimage. That is not a finding (see
lens 5).

## FP6. Every refused coordinate is decided by the shared engine

Status: VERIFIED for live validators; one latent twin remains.

Rule: every family that binds a versioned script decides "malformed", "not
native" and "empty native payload" in the frozen engine, so an adversary
cannot make a coordinate both unprovable as a fault and unprovable as valid.

Enforced: `bind_machine_v1`
(`lib/midgard/fraud-proofs/native-script-decoding/engine.ak:344-367`,
`payload_length == 0` refused at `:355`), called from the
execution-source, output-reference and witness-script decoding rules and
native-script-decoding step-03
`[aiken-test: midgard/fraud-proofs/native-script-decoding/engine.test/]`.

Provenance: `fa9eeb41c` (2026-08-25, the BindOutOfDomain arm), then one fix
per family as each hand-rolled copy was found: `8788d768c`, `0c4001d77`
(2026-09-04), `8f62cb506` and `0023bba3c` (2026-09-05).

Recurrence: 5.

Latent twin: `bind_exact_item_v1` in
`lib/midgard/fraud-proofs/execution-native-script-invalid/rule.ak:323` still
parses the header by hand with no empty-payload arm. Today only its own tests
call it; the execution-source-script-decoding step imports its own rule's
copy. Any change that wires it into a validator reopens the defect.

## FP7. The scan window is authenticated at every stage

Status: VERIFIED.

Rule: whenever the prover supplies a chunk window, the step authenticates it
against the item commitment, whatever stage (frame or token) the segment
starts on. Authenticating only on some stages either admits an unauthenticated
chunk or leaves honest segments unsubmittable.

Enforced: `witness-script-decoding/step-03.ak:83-106`,
`output-reference-script-decoding/step-05.ak:69-91`,
`execution-source-script-decoding/step-04.ak:76-100` (under
`onchain/aiken/validators/fraud-proofs/`)
`[aiken-test: midgard/fraud-proofs/witness-script-decoding/rule.test/]`
(`refuses_a_substituted_item_chunk` `:148`),
`[aiken-test: midgard/fraud-proofs/output-reference-script-decoding/rule.test/]`
(`window_refuses_substituted_chunk_coordinate` `:315`),
`[aiken-test: midgard/fraud-proofs/execution-source-script-decoding/rule.test/]`
(`refuses_an_adjacent_chunk_for_a_single_chunk_item` `:332`,
`refuses_a_substituted_source_item_chunk` `:352`).

Provenance: `0c4001d77`, `8f62cb506`, `0023bba3c` — the same wave as FP6.
Before it, the window was checked only on token stages, so a segment that
opened on a frame step and ran into token steps could not be submitted,
stalling any proof over a container with eight or more children (comment at
`validators/fraud-proofs/witness-script-decoding/step-03.ak:83-89`).

Recurrence: 3.

## FP8. Gates fail when they collect nothing

Status: VERIFIED.

Rule: a focused Aiken selector that collects zero tests fails the gate.

Enforced: `onchain/aiken/scripts/guard-focused-selector.mjs:83-90`
`[script: onchain/aiken/scripts/guard-focused-selector.mjs]`.

Provenance: #519 found 24 gates that could not fail; #523 and `8a4b87707`
made zero collection fatal. The bare `-m native_script_decoding` selector in
`fa9eeb41c`'s era matched no module; the Q60 selector in SQ1 did the same.

Recurrence: at least 5. Blind spot: a selector that collects the wrong tests
still passes.

## FP9. A forced source carries no verdict

Status: VERIFIED.

Rule: the forced-transaction proof-source commitment is independent of the
operator's verdict; accepted and rejected leaves share the source and differ
only in the enclosing adjudicated value.

Enforced: `[aiken-test: midgard/forced-submission.test/]`
(`forced_source_verdict_independence` `:83`,
`forced_source_rejects_obsolete_envelope` `:38`, fail) and
`[aiken-test: midgard/forced-source-bindings.test/]`
(`forced_source_binds_both_verdicts` `:93`).

Provenance: #640 (`165db5ca6`, `11c5bf548`) bound forced disputes to the
committed leaf verdict; `742df026e` (2026-09-12) made forced submissions
immutable with one authoritative verdict.

## FP10. On-chain and off-chain decoders agree

Status: PARTIAL (a known divergence is open).

Rule: for every byte string both sides decode, the Aiken decoder and its
TypeScript twin accept the same set and produce the same structure.

Enforced:
`[ci: midgard-node-ci.yml/Check native V1 field-access golden vectors]` for
the field-access channel only.

Provenance: #633 (OPEN) — tag-0 native reference-script bytes are opaque on
chain (`parse_script_ref`, `lib/midgard/ledger-output-v1.ak:334`) but structurally parsed off
chain (`demo/midgard-core/src/codec/versioned-script.ts:93-94`). #635 (OPEN) — the
reference challenger cannot prove any fraud in a transaction carrying an
output its codec refuses.

Blind spot: the transaction-root and native-compact golden checks are not in
CI, and regenerating a golden file makes both sides green together.

## Miss patterns in this subtree

- A fix applied to one family's copy and not to its twins (FP6, five rounds).
- An ABI or parameter change that outran the builders (#592 to #605).
- A replacement path that drops a guard the old path had (FP5 via #575; SQ1
  via `c07ac1326`; SQ5 via `3e3090aa1`).
- Gates that pass while testing nothing (FP8).
- Cross-language drift between a validator and its SDK twin (FP10).
