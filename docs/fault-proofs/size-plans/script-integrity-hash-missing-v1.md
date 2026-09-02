# `scriptIntegrityHashMissing` V1 size plan

## Deployed seam

Proposed catalogue id: `00000023`. This family is transaction-global and has
no prover-selected coordinate. Its only accepted rejection reason is the exact
nullary `ScriptIntegrityHashMissing` constructor.

The logical two-step thread is implemented by seven narrow physical applied
spending validators:

1. `fraud_proofs/script_integrity_hash_missing/step_01.main.spend`
   parameters, in order: successor step-02 script hash, computation-thread
   policy id, hub-oracle script hash. It authenticates either an accepted
   native-V1 transaction leaf or a forced native-V1 leaf, applies the Wave-0
   `VerdictSubjectV1` binding, refuses every forced-rejection reason except
   `ScriptIntegrityHashMissing`, and carries only the canonical subject plus
   the three decisive authenticated scalars.
2. `fraud_proofs/script_integrity_hash_missing/step_02.main.spend`
   parameters: successor step-03 hash and computation-thread policy id. It
   binds the forced leaf/header where required and emits the exact subject and
   witness-set anchor.
3. `fraud_proofs/script_integrity_hash_missing/step_03.main.spend`
   parameters: successor step-04 hash, script-grammar hash,
   computation-thread policy id, and field certificate policy id. It handles
   direct evidence up to 64 items per field or starts certified field-6
   grammar certification.
4. `fraud_proofs/script_integrity_hash_missing/script_grammar.main.spend`
   parameters: script-scan hash, computation-thread policy id, certificate
   policy id. It resumes grammar certification in fit-safe 24-item batches and opens
   the authenticated semantic walk only from a terminal grammar checkpoint.
5. `fraud_proofs/script_integrity_hash_missing/script_scan.main.spend`
   parameters: redeemer-grammar hash, computation-thread policy id,
   certificate policy id. It scans field 6 in fit-safe 24-item batches and carries the
   monotone `contains_non_native_script` accumulator.
6. `fraud_proofs/script_integrity_hash_missing/redeemer_grammar.main.spend`
   parameters: successor step-04 hash, computation-thread policy id,
   certificate policy id. It certifies field 8 and derives `has_redeemers`
   from the authenticated terminal walk count.
7. `fraud_proofs/script_integrity_hash_missing/step_04.main.spend`
   parameters, in order: permanent fraud-proof policy id, permanent proof-token
   address, computation-thread policy id. It imports the family rule and the
   Wave-0 terminal-polarity helper, permits cancellation, and otherwise
   finalizes by burning the thread token and minting the permanent proof token.

All scripts import only their required continuity primitive. Step 01 imports
accepted carriage, step 02 forced subject binding, the four field-stage scripts
only the field doors/checkpoint engines they require, and step 04 the semantic
engine and terminal helper. Output,
mint, observer, signer, native-script decoding/evaluation, redeemer decoding,
language-view hashing, ledger-output, and validation-trace adapters do not
enter either applied script.

## Semantic engine and state

`midgard/fraud_proofs/script_integrity_hash_missing/rule.ak` is the only
decisive engine. It is the direct extraction of canonical validation-machine
precondition semantics:

`requires_plutus = contains_non_native_script || has_redeemers || integrity_hash != zero32`

`ScriptIntegrityHashMissing = requires_plutus && integrity_hash == zero32`.

Every carried state is constant size. Staged state commits only the canonical
subject, witness-set anchor, integrity hash, one domain-separated checkpoint
hash, and the monotone boolean accumulator. Checkpoint bytes ride the next
redeemer and are re-admitted only through the committed hash and authenticated
field view. The terminal decision state has a definite CBOR encoder with a
TypeScript twin and golden vectors.

## Maximum evidence

The carried semantic state remains constant regardless of cardinality: one
native compact source (including its three 32-byte witness-set commitments),
one counted-root membership proof, and the two presence claims opened from
authenticated field preimages. The direct route is capped by the repository's
native transaction membership envelope; deeper proofs use certified/published
chunks. The maximum field evidence is the consensus-bounded field-6 and
field-8 preimage frontier. The tested maximum supported certified frontier is
224 field-6 scripts and 224 field-8 redeemers, each with 70 payload bytes so
both preimages cross the 15,148-byte certified-carriage boundary. Grammar and
semantic work has a protocol cap of 32 items per L1 transaction. The real
maximum lifecycle uses 24-item batches: the aggregate Aiken vector demonstrates
that a 32-item semantic batch would cross the normal per-transaction CPU
frontier, while 24-item grammar, scan, and redeemer self-loops locally evaluate
and submit under the unmodified Van Rossem limits.

## Planned fit and lifecycle tests

- build with the pinned compiler using `aiken build --env testnet`;
- publish each freshly built applied script in a signed reference-script
  transaction and require `<= 15,872` bytes (hard limit `16,384`);
- record signed bytes, memory, CPU, and positive margins for direct step 01,
  published/certified step 01, cancel from both states, step 02 permanent mint,
  and target/descendant removal under the shared Van Rossem parameters;
- run the maximum legal witness/redeemer evidence and the adjacent-over-bound
  refusal with local UPLC evaluation enabled;
- run complete Lucid lifecycles for wrongful acceptance, wrongful forced
  rejection, both honest polarities, exact-reason and transaction/source
  substitutions, each cancellation point, restart reconciliation, permanent
  mint, and state-queue target/descendant removal.

No raised transaction-size/ExUnit setting, `oversized: true`, disabled local
evaluation, fabricated mid-thread datum, or validation-dispute fallback is an
acceptance route.

## Family-local off-chain wiring

The protected central catalogue is not edited by this slice. Registration can
wire the family without inference through these direct modules:

- `contracts-v1.ts` validates the seven ordered applied titles, hashes,
  addresses, and reference out-refs plus the computation-thread, permanent
  proof, and field-certificate policies;
- `schemas-v1.ts` is the exact datum/redeemer wire twin for every physical
  script and self-loop;
- `submitters-v1.ts` supplies concrete Lucid submissions for step 01, step 02,
  step 03, script grammar, script scan, redeemer grammar, terminal mint, and
  cancellation. Each uses reference scripts, resolves input/output indices at
  transaction construction, performs local UPLC evaluation, and admits staged
  evidence only as reference inputs containing the configured field-certificate
  policy token;
- `family-v1.ts` supplies retained-evidence preparation, exact accepted/forced
  source binding, semantic parity, carriage selection, classifier, durable
  journal reconciliation, and the precise central wiring manifest;
- `production-artifact-v1.ts`, `staged-plan-v1.ts`,
  `production-actuator-v1.ts`, and `production-v1.ts` reconstruct the selected
  contradiction from complete authenticated replay, persist a digest-bound
  JSON-safe artifact, derive every direct/staged checkpoint action, and expose
  the manifest-bound create/execute and standard runner surfaces. The runner
  owns its directory journal and applies the shared actuation/funding permits;
  no evidence, stage, submit, or journal callback enters the production config.

The seven submitters reuse generic initialization and canonical mutation-leased
removal. Deployment must publish the seven family steps, the field-preimage
certificate mint, all nine canonical removal references, and the five shared
witness references: `computationThreadMint`, `fraudProofMint`,
`phasMembershipWithdraw`, `chunkedVerifyWithdraw`, and `pexcludesWithdraw`.
Central integration registers id `00000023` and installs
`createScriptIntegrityHashMissingProductionWorkflowRunnerSurfaceV1`; the
family itself owns replay selection, dynamic actuation, restart reconciliation,
permanent mint, and removal.

## Local build evidence

Pinned Aiken `v1.1.23+5adf783` builds the disposable testnet tree. Raw applied
script bytes (diagnostic, before signed publication overhead) are: step-01
8,956; step-02 9,414; step-03 9,402; script-grammar 10,216; script-scan 8,885;
redeemer-grammar 9,246; step-04 1,971. The 224/224 certified frontier test
reaches both authenticated grammar terminals and completes the field-6
semantic walk. Signed Lucid publication/lifecycle measurements are retained in
the family ledger; there is no unsupported maximum evidence route inside this
family.

The maximum staged Aiken fixture passed with aggregate pure-test units
`191,801,577` memory and `93,622,279,547` CPU. Those totals deliberately cover
the complete sequence of 32-item logical transactions in one test evaluation;
they are not, and must not be reported as, per-transaction ledger fit.

## Integration status

The `00000023` topology supports genuine accepted and forced direct
lifecycles, cancellation, permanent mint, and state-queue removal. The maximum
test starts from generic Init, uses both authenticated certified fields,
resumes every grammar/semantic self-loop at the 224/224 frontier, mints the
permanent proof, removes the fraudulent state, and emits a
blueprint-digest-bound machine-readable Van Rossem ledger persisted at
`script-integrity-hash-missing-v1-fit-ledger.json`. Its staged start and first
same-script restart, plus the accepted and forced source-binding paths, execute
through the package-owned production transaction port. Production watcher
installation is now only the serial central registration step; no
installation-supplied evidence, stage resolver, submit authority, or journal
authority remains in the family API.
