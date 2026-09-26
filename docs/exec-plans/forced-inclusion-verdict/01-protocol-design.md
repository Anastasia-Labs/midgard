# Task 1: Specify immutable forced submissions and authoritative verdicts

- Status: Implemented (design and handoff complete; protocol implementation remains Task 2)
- Last reviewed: 2026-09-10
- Implementation boundary: design and implementation handoff; no runtime or validator changes.
- Dependencies: current repository state and the protocol guidance linked below.
- Non-goals: deployment, state resets, changes to normal transaction semantics, rejection policy, or the broader canonical V1 program.

## What this task accomplishes

A user submits one immutable transaction through L1. The operator includes that
same submission in a block and adds one claim: valid or invalid with a reason.
The submitted bytes contain no competing validity decision. This task decides
the exact bytes, hashes, and proof bindings before implementation begins.

Run these tasks sequentially on one integration branch:

1. This task fixes the design and the checks that establish correctness.
2. [Task 2](02-implementation.md) implements the complete change across contracts and runtime.
3. [Task 3](03-verification.md) reviews the final implementation and establishes local verification and deployment acceptance requirements.

These are one coordinated protocol change. Intermediate checkpoints are not
independently deployable. Tasks 2 and 3 remain proposed work; completing this
design is not evidence that the new format is implemented or a release accepted.

## Shared execution contract

This section applies to all three tasks. On assignment, read the repository
`AGENTS.md`, applicable nested guides, [production principles](../../agents/production-l2.md),
[contract obligations](../../agents/contracts.md), and [documentation policy](../../DOCUMENTATION_POLICY.md).
Use the declared toolchains and preserve unrelated work. Before a first commit,
install the repository hooks as required by `AGENTS.md`.

Record the starting commit, branch, staged/unstaged changes, and relevant untracked
files. This checkout already contains substantial overlapping work; a clean
worktree from HEAD alone would omit some of the actual input. Preserve a
reconstructible baseline, including relevant uncommitted input, before editing.
Keep the shared checkout intact when preparing an isolated copy. Use one
integration branch and carry its actual state between tasks.

Maintain a concise `Progress and handoff` section in each active brief: current
revision and dirty-state provenance, completed checks and evidence paths,
remaining work, and any concrete blocker. Refresh it before yielding or handing
off. Tests that fail at baseline remain visible; an affected required check must
pass before its gate is satisfied. Never weaken assertions, skip required cases,
lower capability limits, or edit measurement identities to manufacture a pass.
Update fixtures and assertions where the approved encoding changes, preserving
the security behavior they exercise.

Follow the current user-approved scope when reconciling older instructions.
The [canonical execution specification](../GOAL_SPEC.md) supplies applicable
protocol and verification obligations; these tasks do not reopen or claim closure
of its entire program. Resolve routine implementation choices from evidence.
Record any genuinely incompatible normative requirement and the exact decision
needed, while continuing independent work.

These briefs authorize local design, implementation, testing, and preparation of
deployment artifacts when assigned. Live submissions, resetting a persistent
deployment, and merging or publishing the change are outside this series alone.
Read the [state-reset rules](../../agents/state-reset.md) when preparing the
deployment handoff. Missing live evidence never becomes local release acceptance.

## Read the existing boundaries

All code paths below are relative to the repository root. Start with these
symbols, then follow callers and codecs; this is navigation, not a closed inventory.

| Boundary                            | Starting points                                                                                                                                                                                    |
| ----------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Wire format and identity            | `docs/spec/midgard-tx.md` §§2, 3, 6–8, 13; `onchain/aiken/lib/midgard/ledger-state.ak`: `NativeTxProofSourceV1`, `TxOrderPayloadV1`, `ForcedInclusionTxV1`; `demo/midgard-sdk/src/ledger-state.ts` |
| Compact bytes and commitments       | `onchain/aiken/lib/midgard/fraud-proofs/native-tx/`; `demo/midgard-core/src/codec/native.ts` and its dependencies                                                                                  |
| Order authentication and settlement | `onchain/aiken/lib/midgard/user-events/tx-order-v1.ak`; `onchain/aiken/lib/midgard/settlement.ak`; `demo/midgard-sdk/src/user-events/tx-order.ts`                                                  |
| Claims and replay                   | `onchain/aiken/lib/midgard/validation-claim-v1.ak`; `demo/midgard-validation/src/validation-machine/trace-builder.ts`; proof consumers under `demo/midgard-fault-proofs/src/`                      |
| DA, watcher, and release identity   | `demo/da-committee-node`; `demo/midgard-watcher`; `demo/midgard-core/src/deployment-manifest-identity.ts`; `docs/consensus-profile-v1.md`                                                          |

Read the decisions on [undeployed format replacement](../../midgard/decisions/prelaunch-format-replacement.md)
and [immutable dispute claims](../../midgard/decisions/immutable-dispute-claims.md).
The latter is essential: a rejection computes no ledger operations but retains
the disputed operator claim, even if the operator claimed a nonempty delta.

## Work and deliverables

1. **Trace the submission through settlement and challenges.** Produce an impact
   table in this brief covering codecs, commitments, L1 material authentication,
   settlement, DA reconstruction, replay, direct proofs, interactive disputes,
   deployment identity, specifications, and fixtures. Each row names concrete
   symbols, its dependency on the current source, and the required change or a
   justified unchanged boundary. Follow common field-access and carriage helpers;
   a search for the forced-leaf type alone is insufficient.
2. **Write the ADR** at `docs/midgard/decisions/forced-inclusion-submission-verdict.md`.
   Record the duplication being removed, the chosen encoding, alternatives,
   compatibility/deployment consequences, and the following exact decisions:
   - Define `SubmittedForcedTransaction`, the two-arm verdict, and the adjudicated
     leaf. Reuse existing verdict types where their meaning fits. A separate
     persisted adjudicated wrapper or serialized effective view is unnecessary.
   - Remove validity from the nested forced-source bytes, not only the outer
     record. Specify field order, constructor/array shape, canonical decoding,
     hash algorithm, domain separation, and version interpretation. Reuse shared
     body/witness/field primitives without accepting the ordinary validity-bearing
     source as a forced source. Define rejection at each relevant malformed-wire
     boundary without excluding malformed transaction content that the existing
     protocol intentionally admits for later challenge.
   - Preserve the current transaction-ID derivation. Define the submission
     commitment over all submitted validation-relevant material, including
     witness commitments and length declarations. Specify whether each existing
     carried hash remains necessary and where it is authenticated; avoid adding
     a duplicate source hash to the block leaf.
   - Distinguish the stable submission commitment from the block leaf hash, which
     also commits the verdict. Keep `TxOrderId` as the order identity: separate
     L1 orders for the same L2 transaction remain independently authenticated.
   - Fix what the validation machine's `transaction_commitment` authenticates
     and how all carriage and field openings bind to that same submission.
     Bind the operator claim separately through authenticated leaf membership.
   - Specify the submission API boundary: how a client constructs the new
     validity-free submission before L1 commitment, and how later consumers
     preserve those exact bytes. Do not rewrite already committed submissions.
   - Establish from actual consumers whether an effective validity scalar is
     needed. If needed, derive `Valid → 0`, `Invalid → 1` only from the authenticated
     operator verdict at use time. It is never an independent input or committed
     decision. Validation still computes the actual result independently.
   - Preserve execution context, ordering, reason/subject binding, rejection
     selection policy, and the immutable claimed delta. State the accepted ledger
     effect and rejected no-op without rewriting a malicious disputed claim.
3. **Freeze an acceptance matrix** in this brief. Use the mandatory scenarios in
   [Task 2](02-implementation.md#required-behavioral-evidence) and the verification
   obligations in [Task 3](03-verification.md#verification-contract). Map each to
   an existing test or a concrete new test location, exact command and working
   directory, required environment, expected nonzero collection, and artifact.
   Name affected proof families and the lifecycle tests that actually measure
   current fit. Mark tests to be authored as future work, not passing evidence.
4. **Prepare the implementation handoff.** Record relevant baseline failures,
   migration/deployment consequences, exact specification sections to update,
   the dependency order, and any necessary change to the initial effort estimate.
   Keep proposed semantics visibly proposed until Task 2 updates implementation
   and normative references together.

## Completion gate

- The ADR resolves every encoding, commitment, machine-view, and context-binding
  question above; it contains no implementation-blocking TBDs.
- Every affected boundary has a disposition and an executable acceptance owner.
- The matrix contains exact runnable existing commands or commands targeting
  explicitly specified new tests; Task 2 has no unspecified verification gate.
- Review the design against substituted witnesses, two orders for one `tx_id`,
  a false acceptance, a false rejection, an incorrect rejection reason, and a
  malicious nonempty claimed delta. Explain which authenticated binding or
  challenge rejects each fault.
- Check document links, referenced symbols, formatting, and `git diff --check`.
  Run documentation-policy checks when their scope applies. Report exactly what
  ran. Design completion does not claim any implementation tests passed.

## Resolved protocol design

The [ADR](../../midgard/decisions/forced-inclusion-submission-verdict.md) records
the rationale and consequences. This section is the exact pending specification
for Task 2, not a description of current implemented bytes. Task 2 transfers it
to `docs/spec/midgard-tx.md` with the implementation and retargets the ADR link.

### Wire format and identity

Use `H = blake2b_256`, `u(n)` = minimal CBOR unsigned integer, `b(x)` = minimal
definite CBOR byte string, and raw ASCII for domain strings. All inner arrays are
definite, minimally encoded, exact-arity, and fully consumed. Version is exactly
1; omitted, unsupported, non-minimal, or extra values fail decoding.

Let `B` be the unchanged canonical 12-field compact body, `W` the unchanged
three-hash compact witness set, and `L` the unchanged nine-length vector. Their
field order and validation remain `docs/spec/midgard-tx.md` §§2.1, 2.2, 2.4 and
2.5; script-witness lengths stay before address-witness lengths.

```text
C = 0x83 || u(1) || B || b(H(W))
S = 0x83 || b(C) || b(W) || b(L)
tx_id = H(ASCII("MidgardNativeTxBodyV1") || u(1) || B)
submission_commitment =
  H(ASCII("MidgardForcedTxProofSourceV1") || u(1) || S)
```

`C` is `ForcedTxCompactV1`: version, body, witness-set hash. It has no fourth
element. `S` is the hash preimage for `ForcedTxProofSourceV1`, whose three stored
byte fields remain `compact_cbor`, `witness_set_compact_cbor`, and
`field_preimage_lengths_cbor`. The ordinary native proof-source domain and its
four-element compact encoding remain unchanged. Forced decoders never try the
normal codec as a fallback.

The outer records use the existing canonical Plutus Data serialization, with
constructor 0 and the following positional fields. Array notation here describes
constructor fields, not an additional CBOR array wire format:

```text
ForcedTxProofSourceV1: [compact_cbor, witness_set_compact_cbor, field_preimage_lengths_cbor]
TxOrderPayloadV1:      [tx_id, transaction_commitment, submitted_source]
ForcedInclusionTxV1:   [tx_id, submitted_source, verdict]
```

Use `submitted_source` consistently in the Aiken declarations and SDK schemas;
remove the forced `source` alias in place. The order retains its current
`transaction_commitment` field, set to `submission_commitment` and rederived at
mint. The leaf carries no duplicate commitment field. Keep `OperatorVerdictV1`
constructors and reason encoding unchanged. `SubmittedForcedTransaction` is the
internal `{tx_id, submitted_source}` concept; the leaf is the adjudicated form.

The MPF key remains canonical `TxOrderId` (the existing L1 order reference), and
the value remains canonical serialized `ForcedInclusionTxV1`. Preserve the MPF
engine, key/value hashing, counted-root wrapper, and `ForcedTransactionsV1RootDomain`
from `transition-trace.ak`. Do not invent a replacement `H(submission || verdict)`
leaf scheme. Changing the verdict changes serialized leaf value and the resulting
nonempty root; it never changes `tx_id`, `C`, `W`, `L`, or `submission_commitment`.

For SDK submission, DA `forced_transaction_preimages`, and the forced journal,
use the exact full encoding `0x83 || u(1) || full_body || full_witness_set`.
The two full structures keep their current canonical field/preimage grammar;
only the top-level validity element is absent. Derive `B`, `W`, and `L` from
those bytes and authenticate them against the submitted source. Normal DA
preimages retain the current four-element native encoding. No separate forced
full-transaction hash is added: normal admission's existing full hash remains
normal-only; the forced source commitment and enclosing DA root supply the
required bindings.

### Independent commitment vector

Use the existing `vector` in
`demo/midgard-core/tests/fixtures/native-tx-vector-v1.generated.json` as static
body/witness/length input. Its old compact header is `84`, version is `01`, and
last element is `01`. For this vector only, replace the header with `83` and
remove that last element, retaining every intermediate byte; encode `S` with the
formula above and hash under the new domain. This is a test-vector derivation,
not permission for runtime rewriting of an L1 submission.

| Quantity                           | Expected value                                                     |
| ---------------------------------- | ------------------------------------------------------------------ |
| Transaction ID, unchanged          | `c27090b9b20b3e6da5906b85969adad0a7c8a8f01a899925b16b93ad8be476f4` |
| New submission commitment          | `29f0c824abc1283bf70e2d1774261027d7de033e16b28dac14681c8ba9035c06` |
| Compact bytes                      | 314, compared with the old native compact's 315                    |
| Encoded proof-source bytes `S`     | 434                                                                |
| Ledger fee/capability size         | 96 bytes, unchanged                                                |
| Actual forced full-submission size | 95 bytes                                                           |

These values were independently calculated with Python `cbor2` canonical
encoding and `hashlib.blake2b(digest_size=32)`, checking the existing body-ID
formula against the fixture before deriving the new hash. Task 2's Aiken and
TypeScript tests must agree with the literal result, and add nonempty witness,
distinct-length-order, malformed, and fee-boundary cases. This vector is not
evidence that the new codecs or validators already exist.

### Admission, decoding, and execution

Provide dedicated forced canonical/compact/material codecs and
`deriveMidgardForcedTxProofSource`, `computeMidgardForcedTxProofCommitment`, and
`verifyMidgardForcedTxProofSource` in the core codec package, with Aiken twins.
Reuse body, witness, length, field-hash, and transaction-ID primitives.

A client may explicitly convert an ordinary admission-valid native transaction
into a new forced submission before constructing the L1 order: verify the normal
input, project body/witness material, then encode the new three-element form.
This is an API construction boundary, not an old-format forced decoder. After
L1 commitment, SDK, node, DA, replay, and proofs consume only the exact forced
bytes. Neither verdict changes nor rejection-reason changes rewrite them.

Retain a raw outer-envelope/material extraction path for fault evidence. Exact
outer shapes and hashes are required; semantic validity of every inner field is
not a prerequisite to proving a malformed field. Preserve the current order
mint's whole-material availability/length checks and admissible language. Preserve
the existing canonical-decodability/committed-field-shape and DA-hash-preimage
fault routes for malformed operator commitments; malformed leaf bytes must not
make every challenge fail before the faulty boundary can be adjudicated.

The machine input becomes a discriminated normal/forced transaction input. Its
forced branch decodes the new full form, authenticates the submitted source, and
shares the body/witness execution rules. Normal admission still requires validity
zero. Do not route forced rejections through the normal `IsValidFalseForbidden`
admission check. Remove `committedForcedVerdict` from proof-source construction;
expected replay outcome and the committed claim remain distinct where disputes
need both. Never infer the operator's claim from the replay result.

No effective validity scalar is required. Source review found no direct
`validity_code` read in machine execution modules; their dependencies use it for
native codec shape and serialized size. Existing partial views can set optional
validity to `None` for forced content, while direct proof polarity comes from
`bind_forced_subject_v1` and the leaf verdict. Normal-only mistag/acceptance checks
continue to read the authenticated normal scalar. There is no encoder for a
validity-bearing effective forced view.

For machine controls, dispatch source decoding/hashing using authenticated
`pre.source_kind` and compare to `pre.transaction_commitment`. That field stores
the new `submission_commitment` on forced steps, unchanged on normal steps.
Keep machine-state and descriptor wire schemas and frozen rejection-code bytes.
All work-witness/carriage controls carry actual source bytes, not a re-encoded
normal compact. Control encoders and CEK/resolver helpers receive source kind
from the authenticated state and never from an unchecked caller flag.

For direct field-opening continuations that currently retain only `BodyAnchor`
or `WitnessAnchor`, add the normal/forced source-kind discriminator to the anchor,
seeded by the authenticated root-opening step and preserved in successor state.
The anchor selects the exact codec; it does not contain a validity bit. A body
anchor authenticates the version/body/ID only; a witness anchor additionally
authenticates the witness-set hash. Neither supplies claimed polarity: use the
authenticated verdict subject. Ordinary-only compiled paths use Normal. Preserve
minimal-field proofs; do not force complete-source carriage where the existing
body or witness anchor already establishes the predicate's subject.

### Size, claims, and persistent state

Preserve the existing ledger-size formula for the same body and witnesses:
`ledger_size = 1 + size(u(1)) + size(full_body) + size(full_witness_set) + 1`.
The final `1` is a constant size charge; both old validity values had that size.
It is not a field, padding byte, or independently chosen scalar. Minimum fee
remains `min_fee_a * ledger_size + min_fee_b`, identically in native/forced replay
and direct fee proofs. Apply existing per-transaction and logical canonical-byte
capability limits to this size. Measure actual DA/payload/L1 transport bytes from
the new encoding (one byte smaller); keep actual transport bounds and proof fit
separate. Test exact fee thresholds and CBOR integer-width boundaries so neither
the operator's verdict nor transport projection changes ledger eligibility.

Keep existing canonical phase/scan order and phase-A/phase-B first-failure
selection in execution. Direct typed-reason challenges continue to adjudicate
the named predicate and subject with their existing quantification; this change
does not add a global first-failure requirement to every direct family.
`forced_verdict_matches` retains the existing reason-to-coarse-code bridge.
It is intentionally many-to-one: matching that coarse code does not authenticate
reason coordinates or substitute for the corresponding typed-reason proof.

Bind header, event key, source-kind, order identity, validation context, execution
position, prior ledger root, claimed delta, and descriptor endpoints as today.
An honest rejected forced step is a no-op. A rejecting challenge against a
malicious nonempty delta preserves the original claimed delta commitment while
its derived work produces no operations and leaves the pre-state unchanged.

Remove the writable forced SQL `operator_validity` column and its insertion,
update, comparison, and selection APIs. Derive accepted/rejected classification
from `forced_inclusion_value.verdict` when needed; no duplicate boolean cache.
Retain operational status (pending, selected, settled) separately. Store the
validity-free full submission in forced journal/material rows and bind reads to
the matching deployment identity. Preserve existing atomic writes, replay,
leases, rollback and restart behavior.

Keep prelaunch wire version values at 1. Add
`forcedTransactionSourceEncoding: "midgard-forced-submission-v1"` to the hashed
`MIDGARD_CONSENSUS_PROFILE`; exact profile validation rejects its absence or
another value. Regenerate the profile digest, affected blueprints/applied hashes,
catalogue and deployment manifests, DA manifests, watcher rule bindings, journal
fixtures, and release identity. No root-domain or rejection-register version is
changed. Existing persistent deployments cannot attach to the new identity;
prepare an explicit fresh deployment/state lifecycle without executing it here.

## Impact inventory

All paths in the following tables are repository-relative. Symbols and files
were inspected at the baseline below; source-kind propagation through shared
helpers is part of each affected caller's implementation, not optional cleanup.

| Boundary and current dependency                                                                                                                                                                                                                                                                                                                      | Task 2 disposition                                                                                                                                              | Acceptance owner         |
| ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------- | ------------------------ |
| `ledger-state.ak`: `NativeTxProofSourceV1`, `TxOrderPayloadV1`, `ForcedInclusionTxV1`; SDK `src/ledger-state.ts`                                                                                                                                                                                                                                     | Add forced source type, rename forced field to `submitted_source`; preserve normal source and verdict constructors                                              | A1, T1, T2               |
| Aiken `fraud-proofs/native-tx/{compact,codec,types,transaction}.ak`; core `src/codec/{native,native-consistency,native-validation}.ts`                                                                                                                                                                                                               | New full/compact forced codecs; shared body/witness view, source hash, strict vs fault-evidence decoding, explicit client projection                            | A1, T1                   |
| Core `src/consensus-validation.ts`, validation `src/phase-a.ts`, `src/validation-machine/{types,trace-builder}.ts`                                                                                                                                                                                                                                   | Forced-specific admission projection and replay; remove source adjudication input; preserve independent outcome, reason order, and ledger size                  | T1, T3, F1               |
| Aiken `user-events/tx-order-v1.ak`: `material_directory`, `verify_order_material`, `forced_inclusion_key_value`; validators `user-events/tx-order-v1.ak`, `settlement.ak`; SDK `src/user-events/tx-order.ts`, `src/settlement.ts`                                                                                                                    | Immutable source mint/spend/settlement; exact source hash, order key and verdict binding; unchanged material availability and due/cancel policy                 | A2, T2, T4, F1, M1       |
| Node `src/fibers/fetch-and-insert-tx-order-utxos.ts`, `src/database/forcedTransactions.ts`, `src/mpf/{event-window,process}.ts`, `src/workers/commit-block-header.ts`, `src/commands/event-settlement-proof.ts`                                                                                                                                      | Reconstruct forced full form, persist one verdict, remove SQL duplicate bit; update journal, material and selected-state comparisons without changing lifecycle | T4, T7, F1               |
| DA `src/da/payload.ts`; core `src/da-payload-envelope.ts`, `src/da-payload-sizing.ts`; node `src/mpf/ledger-hydration.ts`                                                                                                                                                                                                                            | Forced preimage decoder/binding becomes direct; normal hydration remains normal; distinguish ledger bytes from actual DA transport bytes                        | T5, T7                   |
| Aiken `validation-claim-v1.ak`: `verify_source_authentication`, `source_proof_commitment`, `source_binding_is_exact`; `fraud-proofs/proof-thread-substrate-v1.ak`: `bind_forced_subject_v1`                                                                                                                                                          | New hash selected by source kind; verdict alone fixes polarity, exact reason remains authenticated; preserve root/context/endpoint checks                       | A3, F1, F2               |
| Aiken `validation-machine/{canonical-decode,static-rules,input-sets,resolve-inputs,signatures,native-scripts,phase-a-native-scripts,phase-a-script-preconditions,script-integrity,value-and-mint,witness,cek,script-sources,script-sources-early-stages,script-sources-middle-stages,script-sources-middle-semantics,script-sources-late-stages}.ak` | Thread authenticated source kind through every native decoder/hash/size call; unchanged semantic phases and state/descriptor wire                               | T3, F2, full Aiken suite |
| Aiken `resolve-inputs-control-v1.ak`, `cek-context-step.ak`, `script-sources-raw-frame-v1.ak`, `script-sources-redeemer-normalization-v1.ak`                                                                                                                                                                                                         | Source-aware decoders/control construction; preserve redeemer normalization semantics, which are separate from removed forced-validity normalization            | F2, full package suites  |
| Aiken `fraud-proofs/field-opening-v1.ak`, `native-tx-carriage-v1.ak`; core `src/codec/native-tx-carriage.ts`, `src/codec/native-tx-field-access.ts`; SDK `src/native-tx-field-access.ts`; fault-proofs `src/field-opening.ts`                                                                                                                        | Source-kind-aware anchors and control codecs; retain witness-hash authentication, exact lengths, and carriage tiers                                             | A4, T1, T2, F1, F3       |
| Aiken `native-tx-field-access-v1.ak`: `authenticated_field_view`, whole/resumable doors                                                                                                                                                                                                                                                              | Field hash/index/stride and certificate semantics unchanged; adapt decoded body/witness inputs where types change                                               | A4, F3, M1               |
| Aiken `fraud-proofs/{transition-trace/proof,da-hash-preimage/rule,native-script-decoding/engine}.ak`                                                                                                                                                                                                                                                 | Remove reconstructed validity bit; forced-aware strict/tolerant inspection; preserve wrong-source and malformed-source challenge reachability                   | F1, F2, F3               |
| Fault-proofs `src/transition-trace/reconstruct.ts`, `src/validation-dispute/{replay,submit,workflow-engine}.ts`, `src/evidence/rejected-transaction.ts`, family replay/builders and SDK fraud-proof builders                                                                                                                                         | Retained forced bytes bind directly; use typed verdict and source-kind for all preparations and continuations                                                   | F1–F4                    |
| Watcher `src/indexers/user-event-indexer.ts`, `src/verification/{block-replay,event-claims,local-event-replay-authority}.ts`                                                                                                                                                                                                                         | Public L1/DA reconstruction and comparison use immutable forced source; retain rejection routing, rollback and mismatch handling                                | T6, T7, F1               |
| Core `src/consensus-profile.ts`, `src/deployment-manifest-identity.ts`; SDK `src/fraud-proof/contracts/blueprint.ts`; node deployment-manifest and watcher runtime/deployment-identity modules                                                                                                                                                       | Explicit encoding identity in profile and regenerated artifact bindings; stale profile/deployment/journal rejected                                              | T7, full build/typecheck |
| Normal-only native source/admission/full hashes, body IDs, MPF roots, frozen machine/descriptor schemas, rejection constructors/codes, L1 due-time policy                                                                                                                                                                                            | Preserve semantics and bytes where not containing forced material; full regression suites required after shared-helper changes                                  | A3, T1, T3, F4           |

### Proof-family coverage and fresh-fit ownership

The current SDK catalogue has 55 categories. Its source is authoritative;
`catalogue-status.md` has baseline drift. No catalogue family may be dropped to
complete this change. F4 runs the full fault-proof suite; F2 includes all machine
emulator scenarios. The following groups name the source/proof impact and the
existing lifecycle producers to execute and extend with the new forced source.
Paths here are under `demo/midgard-fault-proofs/tests/`.

| Families                                                                                                                                                                                                                                                                                     | Current behavioral/measurement producers                                                                                                                                                                                                                                                                                         |
| -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `transitionTrace`, `validationTraceDispute`, `crossBlockDuplicateEvent`                                                                                                                                                                                                                      | `transition-trace-installed-lifecycle.test.ts`, `submit-init-emulator-transition-trace-subvariants.test.ts`, `validation-trace-dispute-installed-lifecycle.test.ts`, `cross-block-installed-lifecycle.test.ts`; extend forced cases rather than treating accepted/deposit-only rows as forced evidence                           |
| `canonicalDecodability`, `committedFieldShape`, `daHashPreimage`, `nativeScriptDecoding`                                                                                                                                                                                                     | `submit-init-emulator-canonical-decodability-adversarial.test.ts`, `submit-init-emulator-committed-field-shape-adversarial.test.ts`, `submit-init-emulator-da-hash-preimage.test.ts`, `native-script-decoding-installed-lifecycle.test.ts`                                                                                       |
| `zeroInput`, `nonExistentInput`, `noReferenceInput`, `invalidRange`, `invalidSignature`, `missingSignature`, `networkId`, `minFee`, `minAda`, `nativeScriptInvalid`                                                                                                                          | Respectively `zero-input-`, `non-existent-input-`, `no-reference-input-`, `invalid-range-`, `invalid-signature-`, `missing-signature-`, `network-id-`, `min-fee-`, `min-ada-`, `native-script-invalid-wrongful-rejection-lifecycle.test.ts` (each preceding prefix takes the same `wrongful-rejection-lifecycle.test.ts` suffix) |
| `inputSetUniqueness`, `mintAuthorization`, `valueNotPreserved`                                                                                                                                                                                                                               | `input-set-uniqueness-wrongful-rejection-lifecycle.test.ts`, `mint-authorization-installed-lifecycle.test.ts`, `mint-authorization-maximum-lifecycle.test.ts`, `value-conservation-lifecycle.test.ts`                                                                                                                            |
| `fieldPreimageLengthMismatch`, `fieldItemWidthIllegal`, `witnessScriptDecoding`, `scriptIntegrityHashMissing`, `transactionOutputNonCanonical`, `resolvedOutputNonCanonical`, `mintDeclaredAssetLimit`                                                                                       | Corresponding `field-preimage-length-mismatch-`, `field-item-width-illegal-`, `witness-script-decoding-`, `script-integrity-hash-missing-`, `transaction-output-non-canonical-`, `resolved-output-non-canonical-`, `mint-declared-asset-limit-lifecycle.test.ts`                                                                 |
| `spendInputSignerMissing`, `protectedOutputSignerMissing`, `observersForbiddenOnUntaggedNetwork`, `observerOrderInvalid`, `redeemerCanonicity`, `outputReferenceScriptDecoding`, `executionSourceScriptDecoding`, `receivePurposeLanguage`                                                   | Corresponding kebab-case `*-lifecycle.test.ts` files (all present at baseline)                                                                                                                                                                                                                                                   |
| `unusedScriptWitness`, `missingScriptSource`, `missingRedeemer`, `unusedRedeemer`, `executionNativeScriptInvalid`, `scriptIntegrityHashMismatch`, `distinctAssetAccumulationLimit`, `mintItemNonCanonical`                                                                                   | Corresponding kebab-case `*-lifecycle.test.ts` files (including the baseline's untracked `mint-item-non-canonical-lifecycle.test.ts`)                                                                                                                                                                                            |
| Ordinary/event regression: `doubleSpend`, `nonExistentInputNoIndex`, `referenceInputNoIdx`, `fabricatedDeposit`, `fabricatedWithdrawal`, `missingNativeScriptTx`, `withdrawnReferenceInput`, `withdrawalMistag`, `doubleWithdraw`, `l2TxMistag`, `withdrawnInput`, `missingNativeScriptUtxo` | F4 full package suite, plus Aiken full suite; source-kind/anchor changes must preserve existing normal/event proofs even where no forced entry is added                                                                                                                                                                          |

`min-fee-wrongful-rejection-lifecycle.test.ts` writes six maximum-shape classes
when `MIDGARD_WRITE_FIT_LEDGER=1`; its `afterAll` asserts coverage. The installed
transition suite writes `transition-trace-workflow-fit-ledger.json`, but its
current five completed-case labels are accepted/deposit cases, not forced closure.
Use the measured-fit recorder in the network-id lifecycle and the actual owning
producer for every other affected ledger, as directed by the
[fit guide](../../fault-proofs/size-plans/README.md). F4 with the write flag and
fresh-fragment environment below runs these producers; retain fresh results only
for their existing acceptance owner.
Any family lacking a required new-source maximum row must add one in its named
producer. Current snapshots are not final-tree evidence.

## Frozen acceptance matrix

These are Task 2/3 execution requirements. Only the baseline and document checks
in `Progress and handoff` have run during Task 1. Tests marked **new** do not yet
exist. All commands below run from the repository root inside `nix develop ./demo`
unless explicitly marked Aiken. Record the actual tools selected; use the pinned
Aiken fork from `.github/workflows/aiken-ci.yml`. Build the untraced testnet
blueprint before emulator runs and set `MIDGARD_REAL_BLUEPRINT_PATH` to its absolute
`onchain/aiken/plutus.json` path. A passing test report must record the count for
each selected file/module, not only an aggregate exit status.

Before F4, create a fresh measurement namespace from the repository root:

```bash
export MIDGARD_FIT_FRAGMENT_DIR="$(mktemp -d /tmp/midgard-forced-fit.XXXXXX)"
export MIDGARD_FIT_MEASUREMENT_RUN=forced-submission-final
```

`createMeasuredFitRecorder` requires both variables when writing. Record the
resolved directory and preserve the required fresh fragments in Task 3's evidence
handoff before cleaning temporary output. A new attempt uses a new directory;
never combine old and new measurements. These fragments record actual evaluated
transactions; their presence alone does not establish complete family coverage.

| ID  | Exact command                                                                                                                                                                                                                                                                                                                                                                                                  | Required cases and expected collection                                                                                                                                                                                                                                                                                        |
| --- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| T1  | `pnpm --dir demo/midgard-core exec vitest run tests/forced-submission-codec.test.ts tests/native-codec.test.ts tests/native-tx-field-access.test.ts tests/native-tx-carriage.test.ts`                                                                                                                                                                                                                          | **New** codec file: at least 12 cases covering normal/forced cross-language vectors, version/arity/minimal/trailing rejection, unchanged IDs, witness and length substitution, malformed inner material extraction, fee-width boundaries, and verdict-independent source identity; each existing file collects >0             |
| T2  | `pnpm --dir demo/midgard-sdk exec vitest run tests/tx-order.test.ts tests/native-tx-carriage-wire-goldens.test.ts tests/native-tx-field-access.test.ts tests/forced-submission.test.ts`                                                                                                                                                                                                                        | **New** file: at least 6 cases for explicit client projection, new payload/leaf serialization, unknown/obsolete shape refusal, differing verdicts over one submission, and exact source/witness commitment; each existing file >0                                                                                             |
| T3  | `pnpm --dir demo/midgard-validation exec vitest run tests/validation-machine.test.ts tests/validation-machine-event-replay.test.ts tests/forced-submission-replay.test.ts`                                                                                                                                                                                                                                     | **New** file: at least 8 cases for accepted/rejected outcomes in both claim directions, unchanged source commitment, fee equality with normal content, no-op vs immutable claimed delta, preserved multi-fault phase order; existing files >0                                                                                 |
| T4  | `pnpm --dir demo/midgard-node exec vitest run tests/forced-transactions.test.ts tests/forced-transactions-root.test.ts tests/tx-order-material-chain.test.ts tests/tx-order-carriage-l1-observation.test.ts tests/forced-submission-settlement.test.ts`                                                                                                                                                        | **New** file: at least 8 real-contract cases for honest accepted/rejected settlement, two order keys for one ID, substituted witnesses/source/lengths, obsolete source, and existing cancellation/expiry branch; each existing file >0                                                                                        |
| T5  | `pnpm --dir demo/da-committee-node exec vitest run tests/payload.test.ts tests/libp2p-payload-protocols.test.ts tests/forced-submission-payload.test.ts`                                                                                                                                                                                                                                                       | **New** file: at least 6 cases for forced three-element full preimage, normal four-element regression, verdict-independent material, duplicate/missing/mismatched material, and distinct logical/transport size accounting; existing files >0                                                                                 |
| T6  | `pnpm --dir demo/midgard-watcher exec vitest run tests/verification/forced-operator-verdict.test.ts tests/verification/block-replay.test.ts tests/verification/forced-submission.test.ts`                                                                                                                                                                                                                      | **New** file: at least 6 cases for authenticated L1+DA reconstruction, both dishonest verdicts, wrong reason coordinates, same-ID witness substitution, and missing public material; existing files >0                                                                                                                        |
| T7  | `pnpm --dir demo/midgard-node exec vitest run tests/forced-submission-recovery.test.ts tests/deployment-manifest.test.ts` followed by `pnpm --dir demo/midgard-watcher exec vitest run tests/runtime/deployment-identity.test.ts tests/runtime/native-recovery-boundary.test.ts` and `pnpm --dir demo/midgard-core exec vitest run tests/deployment-manifest-identity.test.ts tests/da-payload-sizing.test.ts` | **New** recovery file: at least 5 cases for persisted single verdict, atomic classification, matching restart/replay, rollback, and incompatible identity; every existing file >0                                                                                                                                             |
| F1  | `pnpm --dir demo/midgard-fault-proofs exec vitest run tests/forced-submission-lifecycle.test.ts tests/network-id-wrongful-rejection-lifecycle.test.ts tests/min-fee-wrongful-rejection-lifecycle.test.ts tests/input-set-uniqueness-wrongful-rejection-lifecycle.test.ts --no-file-parallelism`                                                                                                                | **New** lifecycle file: at least 10 actual deployed-in-emulator scenarios covering both correction directions, false challenges to both honest outcomes, source/key/header/context substitution, wrong same-coarse-code reason/subject, and immutable malicious delta; existing files >0 and measured maximum coverage passes |
| F2  | `pnpm --dir demo/midgard-fault-proofs exec vitest run tests/submit-init-emulator-validation-dispute tests/validation-trace-dispute-installed-lifecycle.test.ts tests/transition-trace-installed-lifecycle.test.ts tests/submit-init-emulator-transition-trace-subvariants.test.ts --no-file-parallelism`                                                                                                       | Every matching existing file collects >0; retain the complete matching file list in the result. Extend affected machine-stage scenarios with forced source; both directions and endpoint/source faults must run                                                                                                               |
| F3  | `pnpm --dir demo/midgard-fault-proofs exec vitest run tests/field-opening.test.ts tests/submit-init-emulator-canonical-decodability-adversarial.test.ts tests/submit-init-emulator-committed-field-shape-adversarial.test.ts tests/submit-init-emulator-da-hash-preimage.test.ts tests/native-script-decoding-installed-lifecycle.test.ts --no-file-parallelism`                                               | Each file >0; include raw malformed forced compact/source/field evidence and authenticated source-kind selection for shared openings                                                                                                                                                                                          |
| F4  | `MIDGARD_WRITE_FIT_LEDGER=1 pnpm --dir demo/midgard-fault-proofs test --no-file-parallelism`                                                                                                                                                                                                                                                                                                                   | Full suite, every catalogue group above retained, all required lifecycle maximum/adjacent coverage checks pass; no hidden skips or fixture-only fit substitution                                                                                                                                                              |

T4's new real-contract cases must enable the rebuilt order and settlement
validators explicitly in their harness. F1 uses registered proof init/steps/token,
removal, exact slash/reward, and final corrected state; a permissive substitute
for any validator whose behavior is being claimed is insufficient. Repeat the
normal testnet build and affected scenarios after diagnostic trace builds.

The following commands run from `onchain/aiken` with the pinned compiler.
New source module names below are exact guard-runner arguments; new test files
are named with hyphens and `.test.ak` following repository convention.

```bash
# A1: new midgard/forced-submission.test.ak; exactly these six collected tests.
node scripts/run-focused-check.mjs midgard/forced-submission.test \
  forced_source_golden_vectors forced_source_rejects_obsolete_envelope \
  forced_source_rejects_noncanonical_encoding forced_source_binds_witness_and_lengths \
  forced_source_preserves_id_and_fee_size forced_source_verdict_independence

# A2: existing order ABI/material cases plus the new settlement cases in T4.
node scripts/run-focused-check.mjs midgard/user-events/tx-order-v1.test \
  tx_order_v1_matches_the_canonical_typescript_abi_vectors \
  forced_inclusion_key_value_matches_the_canonical_v1_vectors \
  inline_carriage_authenticates_a_witness_set_field \
  certified_carriage_authenticates_a_chunked_field \
  declared_field_length_disagreeing_with_the_material_is_refused \
  declared_length_of_an_empty_field_other_than_one_is_refused

# A3: existing binding/reason/normal guards; exactly six tests.
node scripts/run-focused-check.mjs midgard/validation-claim-v1.test \
  forced_verdict_matches_accepts_valid_leaf_on_accepted_descriptor \
  forced_verdict_matches_rejects_valid_leaf_on_rejected_descriptor \
  forced_verdict_matches_accepts_invalid_leaf_with_matching_code_hash \
  forced_verdict_matches_rejects_invalid_leaf_with_wrong_code_hash \
  normal_source_accepts_validity_code_zero normal_source_rejects_validity_code_one

# A4: new midgard/forced-source-bindings.test.ak; exactly six tests.
node scripts/run-focused-check.mjs midgard/forced-source-bindings.test \
  forced_source_binds_both_verdicts forced_source_rejects_foreign_order \
  forced_source_rejects_foreign_witnesses forced_source_binds_reason_subject \
  forced_source_kind_is_authenticated forced_source_rejection_preserves_claimed_delta

# M1: existing fresh order-material measurements. Update first, then verify.
node scripts/verify-tx-order-mint-exec-ledger-v1.mjs --update
node scripts/verify-tx-order-mint-exec-ledger-v1.mjs
node scripts/run-focused-check.mjs midgard/user-events/tx-order-v1.test \
  tx_order_mint_material_at_the_aggregate_bound \
  tx_order_mint_material_at_the_aggregate_bound_refuses_a_tampered_tail_chunk
```

A1/A2/A3/A4 each require exactly six passing cases; M1's explicit guard requires
exactly two. The M1 ledger verifier additionally asserts its own nonzero row
coverage. M1 currently measures fixed-stride aggregate and smaller variable-width
controls; it is not sufficient for the variable-width maximum. Add maximum and
adjacent admitted variable-width order material to T4's real transaction cases
and measure signed size, CPU, memory, and complete carriage at the existing
limits. If that cannot fit, report the concrete required bounded-route dependency;
do not assert production capacity from M1 alone or lower the admitted limit.

The complete build, format, typecheck, lint, specification build, seven package
suites, retained-DA/breadth checks, fixture regeneration/check, and documentation
commands in [Task 3](03-verification.md#verification-contract) remain required.
The matrix supplements them with precise behavior owners. Store test/measurement
results with the final evidence index required by Task 3, with command, cwd,
environment, collected counts, exit status, duration and source/artifact identities.

## Design review and implementation handoff

| Adversarial case                                     | Binding or proof that must decide it                                                                                                                                     |
| ---------------------------------------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Same body/ID, changed witnesses or lengths           | L1 mint verifies `H(W)`, source hash and material lengths; settlement compares the exact immutable source; DA/proofs authenticate that source, never ID alone            |
| Two orders for one ID                                | Distinct `TxOrderId` MPF keys and authenticated event-to-step binding; replay uses the actual position/pre-state so the second transaction is not assumed valid          |
| Invalid transaction claimed valid                    | Forced leaf authenticates acceptance; independent replay/direct predicate proves contradiction; terminal correction uses original source and operator claim              |
| Valid transaction claimed invalid                    | Typed rejection subject is committed; the applicable direct reason proof or execution dispute proves the opposite result without changing submission bytes               |
| Wrong reason sharing a coarse code                   | Exact typed reason/coordinates remain in the authenticated leaf and proof subject; coarse descriptor equality alone does not clear that claim                            |
| Rejecting execution against a nonempty claimed delta | Machine preserves the immutable disputed delta; rejecting work/no-op and prior root establish the actual effect separately                                               |
| Old or malformed operator source encoding            | Forced strict decoder rejects it; raw DA/source-fault inspection still permits the existing appropriate challenge to authenticate and fault the committed malformed leaf |
| Forced/normal codec confusion or foreign context     | Root-opening/pre-state binds source kind; continuation preserves it; header/order/event, context, root and deployment checks retain their existing authority             |

Implement in dependency order: core/Aiken codecs and vectors → order/SDK and
source-kind-aware shared controls → node/DA/journal/replay → proof families and
watcher → identity/specification/artifact regeneration → full acceptance. Keep
these changes on one integration branch/state. The current branch is already a
shared dirty integration input; a future isolated implementation branch must
carry the captured input, not only HEAD.

Task 2 must update component-spec §§2.3, 3, 8.11 and 13.1–13.4; add the forced
full/compact definitions and ledger-vs-transport size distinction; reconcile
the source-authentication references under `technical-spec/1-ledger-state/5-transaction-order-event.tex`,
`technical-spec/6-offchain-data-architecture/1-da-layer.tex`, and the relevant
transaction-order/validation-rule sections found by symbol. Update the rejection
reference's normal/forced distinction without changing its register. Record the
new profile encoding identity and fresh-deployment consequence in
`docs/consensus-profile-v1.md`; preserve old persistent deployments.

No encoding, hash-domain, scalar-view, fee, reason-policy, or source-kind decision
is left open. Implementation measurements remain work to perform. The initial
5–10 engineer-day estimate is still a rough estimate for the schema/consumer
change, not a release-capacity promise. Source inspection found a pre-existing
order-mint maximum-shape gap: its verifier explicitly documents variable-width
cost exceeding its basis while smaller controls pass. No fresh maximum was
measured in Task 1. A bounded-route repair, if required by final measurements,
is a concrete additional dependency; no reliable upper estimate is established
for that work. Task 3 remains unable to claim full capacity until it is resolved.

## Progress and handoff

- Baseline commit: `9797ce41ce5d436e309eca07e2020ee29c395859` on
  `colll78/canonical-v1-watcher-l1-source-checkpoint`. The real index and branch
  were left unchanged during design work.
- Reconstructible local baseline: `.git/codex-task-baselines/forced-inclusion-verdict-task1/`.
  `baseline.json` records HEAD, branch, artifact digests and all 147 nonignored
  untracked source files; staged/unstaged binary patches and their source archive
  preserve the input. Apply only in a disposable checkout, in the recorded order.
  This is implementation handoff provenance, not release evidence.
- Baseline codec check: `nix develop ./demo --command bash -c 'pnpm --dir demo/midgard-core exec vitest run tests/native-codec.test.ts --reporter=verbose'`
  passed 17/17 tests in 2.64 seconds under Node 22.22.2, pnpm 9.15.9, Vitest 3.0.7.
  An earlier host-environment run also passed 17/17; only the declared-environment
  result is used here. No Aiken build or new-format implementation test ran.
- Documentation baseline: `pnpm --dir docs-site run check:facts` fails on the
  existing availability-challenge CLI and 55-category catalogue drift;
  `pnpm --dir docs-site run check:links` fails on the two existing test-quality
  references to removed `api-export-snapshot.test.ts`. These files were not edited.
- Concurrent work changed watcher transport/observation code and fixtures after
  the snapshot: `src/l1/l1-adapter.ts`, `src/l1/resolved-block-observation.ts`,
  `tests/l1/resolved-block-observation.test.ts`, and
  `tests/support/user-event-origin-fixture.ts` under `demo/midgard-watcher`, plus
  `docs/fault-proofs/automatic-watcher-journeys.md`. Those edits were preserved;
  they do not change the source encoding decisions or the codec baseline result.
  Task 2 must capture its own start state and retain these ongoing changes.
- ADR, resolved wire design, impact inventory, proof-family coverage, adversarial
  review, and exact test matrix are complete. Document checks verified local
  links/anchors, 47 existing test references and Aiken selectors, all 55 catalogue
  categories, and the independent commitment/size vector. Prettier and
  `git diff --check` passed; the new/previously untracked documents also receive
  direct whitespace checks. The two repository-wide documentation failures above
  remain existing baseline failures, not silently waived implementation gates.
- Task 2 is ready to start with this handoff. No protocol code, validator,
  deployment, or database was changed by Task 1. The only task-authored source-tree
  changes are this brief and the new ADR; other concurrent changes are preserved.
- After Task 2 transfers the pending definitions into the component specification,
  keep this acceptance inventory until Task 3 finishes. Then retain the ADR and
  required evidence and retire the task diaries under the documentation policy.
