# Fraud-proof invariants

Scope: `onchain/aiken/validators/fraud-proofs/`,
`onchain/aiken/lib/midgard/fraud-proofs/`, the computation thread and
fraud-proof policies, `demo/midgard-sdk/src/fraud-proof/`, and
`demo/midgard-fault-proofs/`.

Status and citation conventions are as in
[invariants-state-queue.md](invariants-state-queue.md): **VERIFIED** means
code and a refusing test were read; **PARTIAL** names what is missing.

## FP1. Every script is applied with exactly its declared parameters

Status: PARTIAL (runtime door only; no lint stops a new bypass).

Rule: a validator is applied through one door that refuses any parameter list
whose length differs from the blueprint's declaration. Under Plutus V3 an
under-applied script is a different script that can succeed on anything.

Enforced: `applyBlueprintParams`
(`demo/midgard-sdk/src/fraud-proof/contracts/blueprint.ts`)
`[runtime: applyBlueprintParams]`; every strict prefix of the parameter list
must hash differently from the deployed script
(`demo/midgard-sdk/tests/validation-resolver-applied-hashes.test.ts`);
`demo/midgard-fault-proofs/tests/emulator-blueprint-boundary.test.ts`.

Blind spot: a loader that deploys `compiledCode` bare never reaches the
door. The `phas` membership loader does, and refuses a blueprint that declares
a parameter for it.

## FP2. On chain trusts deployment parameters

Status: VERIFIED.

Rule: a validator does not re-check the shape of its own deployment
parameters; the SDK checks them once at application.

Enforced: `assertParameterShapes`
(`demo/midgard-sdk/src/fraud-proof/contracts/blueprint.ts`)
`[runtime: assertParameterShapes]`; policy in
[contracts.md](../../../../docs/agents/contracts.md).

Review action: a new on-chain check that only inspects a parameter is a
finding against the change, not a hardening.

## FP3. Fault proofs execute published reference scripts

Status: PARTIAL (runtime fail-closed only; one emulator-only escape remains).

Rule: fault-proof steps and their supporting witness scripts run as published
reference scripts, hash-checked against the script the transaction executes.
A missing entry fails; there is no inline fallback.

Enforced: `requireWitnessReferenceScriptUtxo`
(`demo/midgard-fault-proofs/src/witness-reference-scripts.ts`) and `requireLinearFaultReferenceScript`
(`demo/midgard-fault-proofs/src/linear-fault-family.ts`)
`[runtime: requireWitnessReferenceScriptUtxo]`.

Blind spot: the lint (`midgard/fault-proof-reference-scripts-only`) bans
inline attach only in the fault-proof source trees it lists; a fault-proof
script attached from another tree is not caught.

## FP4. Only the pinned compiler builds consensus code

Status: VERIFIED.

Rule: blueprints and gate results come from the pinned Aiken fork, never
stock Aiken.

Enforced: every script, the pre-commit hook and CI run the compiler only
through `scripts/aiken`, which builds the commit `demo/flake.lock` pins
`[script: scripts/lib/aiken.mjs]`, `[hook: pre-commit]`.

Blind spot: the pre-commit hook skips formatting when nix
cannot build the pinned compiler, and a hand-typed bare `aiken` runs whatever
is on `PATH`.

## FP5. Witness-field openings check the anchored witness-set hash

Status: VERIFIED.

Rule: a step opening a witness-set field compares the recomputed
`witness_set_hash` with the one anchored in the thread, not only with the
transaction id. The transaction id does not cover the witness set
([midgard-tx.md](../../../../docs/spec/midgard-tx.md) §3, lines 159-170).

Enforced: `expect verified.tx_compact.witness_set_hash == witness_set_hash`
(`lib/midgard/fraud-proofs/field-opening-v1.ak`)
`[aiken-test: midgard/fraud-proofs/field-opening-v1.test/]`:
`forged_witness_set_hash_re_derives_the_same_tx_id` (the premise),
`forged_witness_set_and_compact_is_refused_under_the_anchor` (fail),
`tier_three_certificate_over_a_witness_field_binds_to_nothing_committed`,
`certified_carriage_opens_the_address_witness_field_with_the_welded_hash`.

The preimage-certificate mint welds `field_hash`, so a certificate minted
over a fabricated witness set carries the fabricated hash and fails the door
equality.

## FP6. Every refused coordinate is decided by the shared engine

Status: VERIFIED for live validators; one latent twin remains.

Rule: every family that binds a versioned script decides "malformed", "not
native" and "empty native payload" in the frozen engine, so an adversary
cannot make a coordinate both unprovable as a fault and unprovable as valid.

Enforced: `bind_machine_v1`
(`lib/midgard/fraud-proofs/native-script-decoding/engine.ak`,
`payload_length == 0` refused), called from the
execution-source, output-reference and witness-script decoding rules and
native-script-decoding step-03
`[aiken-test: midgard/fraud-proofs/native-script-decoding/engine.test/]`.

Easy to miss when a family hand-rolls its own header parser: each copy needs
its own empty-payload arm.

## FP7. The scan window is authenticated at every stage

Status: VERIFIED.

Rule: whenever the prover supplies a chunk window, the step authenticates it
against the item commitment, whatever stage (frame or token) the segment
starts on. Authenticating only on some stages either admits an unauthenticated
chunk or leaves honest segments unsubmittable.

Enforced: `witness-script-decoding/step-03.ak`,
`output-reference-script-decoding/step-05.ak` (under
`onchain/aiken/validators/fraud-proofs/`)
`[aiken-test: midgard/fraud-proofs/witness-script-decoding/rule.test/]`
(`refuses_a_substituted_item_chunk`),
`[aiken-test: midgard/fraud-proofs/output-reference-script-decoding/rule.test/]`
(`window_refuses_substituted_chunk_coordinate`).

Easy to miss for a segment that opens on a frame step and runs into token
steps: with the window checked only on token stages, it cannot be submitted,
which stalls any proof over a container with eight or more children (comment
at `validators/fraud-proofs/witness-script-decoding/step-03.ak`).

## FP8. Gates fail when they collect nothing

Status: VERIFIED.

Rule: a focused Aiken selector that collects zero tests fails the gate.

Enforced: `onchain/aiken/scripts/guard-focused-selector.mjs`
`[script: onchain/aiken/scripts/guard-focused-selector.mjs]`.

Easy to miss because a loose selector looks right: a bare
`-m native_script_decoding` matches no module. Blind spot: a selector that
collects the wrong tests still passes.

## FP9. A forced source carries no verdict

Status: VERIFIED.

Rule: the forced-transaction proof-source commitment is independent of the
operator's verdict; accepted and rejected leaves share the source and differ
only in the enclosing adjudicated value.

Enforced: `[aiken-test: midgard/forced-submission.test/]`
(`forced_source_verdict_independence`,
`forced_source_rejects_a_four_element_envelope`, fail) and
`[aiken-test: midgard/forced-source-bindings.test/]`
(`forced_source_binds_both_verdicts`).

## FP10. On-chain and off-chain decoders agree

Status: PARTIAL (a known divergence is open).

Rule: for every byte string both sides decode, the Aiken decoder and its
TypeScript twin accept the same set and produce the same structure.

Enforced:
`[ci: midgard-node-ci.yml/Check native V1 field-access golden vectors]` for
the field-access channel only.

Open: tag-0 native reference-script bytes are opaque on chain
(`parse_script_ref`, `lib/midgard/ledger-output-v1.ak`) but structurally
parsed off chain (`demo/midgard-core/src/codec/versioned-script.ts`), #633;
the reference challenger cannot prove any fraud in a transaction carrying an
output its codec refuses, #635. Lens 10 has both.

Blind spot: the transaction-root and native-compact golden checks are not in
CI, and regenerating a golden file makes both sides green together.

## Miss patterns in this subtree

- A fix applied to one family's copy and not to its twins (FP6): each copy
  is reviewed inside its own family's change.
- An ABI or parameter change that outran the builders (FP1): the validator
  compiles and its unit tests pass.
- A replacement path that drops a guard the old path had (FP5, SQ1): the
  new path reads correctly on its own.
- Gates that pass while testing nothing (FP8).
- Cross-language drift between a validator and its SDK twin (FP10).
