# L2-tx-mistag fault: design and offchain implementation plan (v1)

> **Registration update (2026-08-26):** this family is now registered as
> `l2TxMistag` at `00000017`. Generic Init, catalogue/inspection, node/core
> deployment identity, watcher proof-thread topology, and both mandatory
> authenticated reference scripts are wired. Family-specific CLI, autonomous
> watcher detector/prover mounting, preprod, and live evidence remain open;
> D-S9's formal wording gate is unchanged. The identity change requires fresh
> genesis/redeployment; there is no migration or compatibility path.

Plan date: 2026-08-26. Audited against branch
`colll78/canonical-v1-watcher-l1-source-checkpoint` (HEAD `a1724e63`).
Catalogue row: `catalogue-status.md` §6 (`l2-tx-mistag`); coverage-matrix
§14 F7; work item W-C14; design-status gate D-S9. Unlike its sibling plans
this document ships TOGETHER with the implementation it describes: the
on-chain family, the offchain submitters, and the emulator suites land in
the same wave, so §4/§8 describe code in this tree, not future work.

The fault: the operator marks a VALID L2 transaction as invalid
(`validity_code != 0`) when committing it under the block's
`transactions_root`, so replay applies it as a no-op and the transaction is
silently censored while appearing included.

The parity bar is the `da-hash-preimage` / `zero-input` two-step families
for the on-chain and submitter shape, and the `native-script-decoding`
family (#635) for the explicit-record discipline: explicit contracts
record, canonical category wiring, reference-script deployment, both-polarity emulator
suites through fraud-proof mint AND fraudulent-commitment removal.

Standing rulings this plan implements and never re-opens:

- **Reference scripts always (owner ruling 2026-08-26):** fault-proof step
  validators deploy as reference scripts and are referenced, never attached
  inline, regardless of compiled size (§2.3, §10 D3).
- **Both-polarity emulator tests (owner directive 2026-08-25):** the real
  fault proves through the full lifecycle — token mint and fraudulent
  commitment removal — and an adversarial prover against an honest
  commitment is refused **on-chain at the exact check**, not merely by
  offchain guards (§8).
- **Explicit-record discipline:** contracts records remain explicit while the
  canonical catalogue and manifests route `l2TxMistag`; family-specific CLI is
  separate (§2.2).
- **Canonical id:** `00000017` is the production category id (§2.1).
- **Fraud-proof token permanence:** the token minted at finalize has no
  burn path by design; removal burns the state-queue node NFT and slashes
  the operator while the token survives as permanent evidence.
- **Binding exclusivity:** the committed transaction is bound through
  `verify_native_tx_in_state_queue_node` /
  `pass_native_tx_to_next_step[_carried]` (`common.ak`) only. This family
  introduces no new binding path.

## 1. The adjudication question, and its answer (D-S9)

**D-S9 status: formally open. This plan takes the most
canonical-V1-consistent reading and proceeds; the reading is recorded here
so a later ruling can be checked against it.**

The naive statement of this family — "the operator marked a valid
transaction invalid" — suggests conviction requires establishing on-chain
that the mis-tagged transaction was actually VALID, i.e. replaying its full
ledger validation. That is the wrong frame for canonical V1, and this plan
does not do it.

### 1.1 The committed scalar is itself the verdict

Canonical V1 already answered what a normal `transactions_root` leaf MEANS:

- `validation-claim-v1.ak` §2.4.3(d): "a Normal leaf under
  `transactions_root` IS an acceptance verdict — its embedded validity
  scalar must claim `TxIsValid` (0)". `verify_source_authentication`
  enforces `verified.tx_compact.validity_code == 0` for every
  `NormalValidationSource`, and `source_binding_is_exact` requires
  `descriptor.verdict == Accepted` for normal sources.
- `ledger_state.MidgardTxValidity` (the #640 format wave): the five
  rejection-reason arms were retired from the normal wire scalar; a
  rejected transaction's full `RejectionReasonV1` lives ONLY on the forced
  leaf's `OperatorVerdictV1`. The normal leaf has **no rejection channel**:
  there is no legitimate way for a normal leaf to say "included but
  invalid" and carry a reason.
- `transition-trace/proof.ak` `validate_l2_transaction_transition`:
  the registered transition family expects
  `unanchored_validity_code_of(anchored) == 0` for normal L2 leaves —
  the replay machinery itself refuses to process a code-1 normal leaf.
- The honest node (`mpf.ts`, "admission requires TxIsValid") only ever
  admits `TxIsValid` transactions into the normal pool; invalid
  submissions are dropped or routed through forced inclusion, where the
  operator's rejection verdict is carried explicitly and is separately
  disputable (`InvalidForcedTransactionNoOpTransition`,
  `validationTraceDispute`).

Consequence: **committing ANY normal `transactions_root` leaf whose
validity scalar is non-zero is itself the protocol violation.** No honest
operator can produce such a leaf under any execution of the honest
pipeline. The conviction predicate is therefore leaf-local, deterministic,
and single-party-total:

> membership of `(tx_id, compact_cbor)` under the header's counted
> `transactions_root`, plus the codec re-derivation of `tx_id` from the
> body bytes, plus `validity_code != 0` on the decoded compact.

No CEK evaluation, no state reconstruction, no second party.

### 1.2 Coverage boundary

- **All rejection arms are covered single-party.** The predicate is
  reason-independent: it does not matter WHY the operator claims the
  transaction was invalid (signature, fee, conservation, …), because the
  normal leaf has no channel to say why, and no reading of "invalid"
  legitimizes a code-1 normal leaf. There is no uncovered remainder and no
  multi-party escalation path is needed.
- **Honest operators are safe for every rejection arm.** An honest
  operator's handling of a genuinely invalid submission is (a) exclusion
  from the block, or (b) forced-inclusion with `ForcedTxInvalid` and the
  full `OperatorVerdictV1` on the FORCED leaf. Neither produces a code-1
  normal leaf, so neither is reachable by this family's binding: (a) has
  no leaf to open, (b)'s leaf lives under the forced map, not
  `transactions_root`, and this family only opens `transactions_root`
  (§1a of `catalogue-status.md`). The refusal point for an adversarial
  challenger is exact: a code-0 leaf fails `validity_code != 0`.
- **Deliberate edge:** a genuinely-invalid transaction smuggled into
  `transactions_root` as a code-1 leaf DOES convict. This is intended: no
  honest operator can commit that block (the honest pipeline admits only
  code-0 into the normal pool), and its presence corrupts the acceptance
  semantics of `transactions_root` regardless of the underlying
  transaction's merits. The family convicts the leaf, not the transaction.

### 1.3 Anti-framing soundness

`MidgardTxId` is `H32<MidgardTxBodyCompact>` — the id covers the BODY
only, not the validity scalar (`computeMidgardNativeTxIdV1` /
`native_tx_id_for_version`). So the codec check alone cannot stop a
challenger who takes an honest code-0 leaf, flips its scalar to 1, and
presents the forgery: the forged compact still re-derives the same
`tx_id`. What stops the framing is the **membership check**: the MPF proof
opens the exact committed leaf VALUE bytes — scalar included — under the
counted root the header commits. A flipped-scalar forgery is not a leaf of
the committed root, so `plutarch_phas_raw` membership fails on-chain. The
adversarial emulator suite drives exactly this forgery and pins the
refusal at the membership withdrawal (§8.3).

Additionally `expect_validity_code` (codec) constrains the decoded scalar
to `{0, 1}`, so `!= 0` is exactly `== 1`; a leaf carrying any other scalar
is not decodable native-V1 compact at all and fails the codec
precondition (it is then `da-hash-preimage`'s or the decodability
families' business, not this one's).

### 1.4 Rejected alternatives

- **(A) Full validity adjudication** — prove on-chain that the mis-tagged
  transaction satisfies every ledger rule, then convict on the
  contradiction. Rejected: single-party totality fails (script-witnessed
  transactions require CEK evaluation, which is the interactive
  `validationTraceDispute`'s domain); it duplicates the entire validation
  machine for a fault the committed bytes already prove; and §1.1 shows
  the extra work adjudicates nothing the scalar has not already conceded.
- **(B) Extend the transition-trace redeemer** — add an
  `InvalidL2TransactionNoOpTransition` constructor to
  `InvalidOneStepTransitionWitness` (the shape the withdrawal twin
  `validate_invalid_withdrawal_no_op_transition` has). Rejected: the
  predicate is leaf-local — it needs no pre/post state roots, no trace
  binding, and no replay context, so the transition-trace machinery buys
  nothing; it would churn a REGISTERED family's redeemer ABI outside a
  format wave; and it would edit `proof.ak` concurrently with the
  `withdrawal-mistag` work, which this wave is explicitly kept separable
  from. (Note the withdrawal twin genuinely needs the trace: a withdrawal
  no-op's effect is only visible against state roots. This family's fault
  is visible in the leaf bytes alone.)
- **(C) Verdict-refutation à la native-script-decoding direction B** —
  refute the operator's committed rejection reason. Inapplicable: after
  #640 the normal leaf commits no reason to refute; there is nothing to
  bind a refutation against.

**Decision: a standalone two-step family** (`l2-tx-mistag`), byte-for-byte
on the `da-hash-preimage`/`zero-input` chassis: step-01 binds the
committed leaf through the shared native binding path and forwards the
authenticated facts; step-02 finalizes and adjudicates `!= 0`.

## 2. Registration and deployment

### 2.1 Category id

Canonical category id: **`00000017`**. `l2TxMistag` is present in the SDK
catalogue, generic `submit-init`, deployment manifests/inspection, and watcher
proof-thread topology.

### 2.2 Registered deployment surface

`demo/midgard-sdk/src/fraud-proof/catalogue.ts`, generic `submit-init.ts`, and
the deployment manifest's canonical key set are wired. Every submitter still
takes the explicit `L2TxMistagContractsV1` record plus the category the thread
rides. Family-specific `bin.ts` verbs and watcher detector/prover mounting
remain open.

### 2.3 Script deployment: reference scripts

Both step validators deploy as reference scripts and every submitter takes
the published reference-script UTxO, verifying the carried script hash
against the step it spends before building anything (owner ruling
2026-08-26: size is irrelevant; inline attachment is not a fallback). The
emulator suites publish both steps and drive every spend with
`readFrom` + hash verification. Removal resolves the family through an
explicit-category record (`RemoveFraudulentBlockExplicitCategory`) whose
step-01 hash is pinned by the caller-chosen manifest entry
`fraudProofL2TxMistag` (`L2_TX_MISTAG_REMOVAL_DEPLOYMENT_ENTRY_V1`,
`tests/support/emulator/removal-deployment.ts`) — never by a non-canonical
manifest category key.

## 3. On-chain design

### 3.1 Step chain

Two steps, shared computation-thread and fraud-proof policies
(`fraud_proofs/l2_tx_mistag/step_01.main.spend`,
`fraud_proofs/l2_tx_mistag/step_02.main.spend`).

- **step-01** — parameters
  `(step_02_validator_script_hash, computation_thread_token_policy_id, hub_oracle)`.
  `ct.Continue(carriage)` runs `pass_native_tx_to_next_step_carried`
  (both #545 carriages: redeemer-carried membership proof, or published
  chunks), which authenticates the raw root against the header's counted
  `transactions_root`, proves `(tx_id, compact_cbor)` membership, and runs
  the codec precondition re-deriving `tx_id` from the body bytes
  (`verify_native_tx_compact_cbor_v1`; unsupported versions fail closed
  inside `expect_supported_native_tx_version`, so only native-V1 leaves
  ever reach the predicate). The step then:
  1. requires the thread to continue at `step_02_validator_script_hash`;
  2. reads `committed_validity_code` off the **authenticated view**
     (`bad_tx_view.tx_compact.validity_code`) — never off anything the
     prover carried loose;
  3. `expect committed_validity_code != 0` — fail-fast: a thread cannot
     even advance past step-01 against a code-0 leaf;
  4. forwards `step_02.State { bad_tx_id, committed_validity_code }`.
- **step-02** — parameters
  `(fraud_proof_token_policy_id, fraud_proof_token_address, computation_thread_token_policy_id)`.
  `ct.Continue(Args { input_index, output_index, fraud_proof_mint_redeemer_index })`
  runs `common.finalize` (thread NFT burn, permanent fraud-proof token
  mint at the always-fails address, prover signature), reads the forwarded
  state — trustworthy because step-01 wrote it under binding and the
  thread NFT authenticates the datum's location — and re-asserts
  `expect committed_validity_code != 0`. This is the decisive soundness
  check; the step-01 copy is prover UX. `ct.Cancel` at both steps burns
  the thread via `common.cancel`.

### 3.2 What is deliberately absent

No pre/post state roots, no trace binding, no field openings, no
certificate policy parameter (step-02 needs no §8.8 door — the scalar is a
fixed field of the compact frame the binding already decoded), no new
binding helper. The family touches no existing Aiken module; it adds two
lib schema twins and two validators.

## 4. Offchain modules (`demo/midgard-fault-proofs/src/l2-tx-mistag/`)

Native-script-decoding discipline: explicit records, no SDK edits, no CLI.

| Module | Role |
| --- | --- |
| `contracts-v1.ts` | Blueprint titles (declared parameter order pinned in the header comment), `L2TxMistagStepContractV1`, `L2TxMistagContractsV1` (steps, shared thread/fraud-proof policies, hub-oracle + state-queue policies), `buildL2TxMistagChainV1` applying step-02 then step-01. |
| `schemas-v1.ts` | Lucid `Data` twins of the lib types: step-01 datum/redeemer over the SDK's generic `NativeTxInclusionCarriageSchema`, `L2TxMistagStep02State { bad_tx_id, committed_validity_code }`, step-02 datum/args/redeemer. |
| `submit-common-v1.ts` | Family error label, thread-UTxO/step-datum fail-closed readers, reference-script verification (§2.3), `L2TxMistagCatalogueCategoryV1`. |
| `submit-l2-tx-mistag-init.ts` | Thread init mirroring `submitNativeScriptDecodingInit`: catalogue/hub-oracle/fraudulent-block reference inputs, PHAS category-membership withdrawal, `Init` mint. |
| `submit-l2-tx-mistag-step-01.ts` | Detection gate (`requireNativeTxMatchesCompactCbor`, then refuse unless `validity_code != 0` — "a code-0 leaf is an honest acceptance; a valid block cannot be challenged"), both #545 carriages, forwards the step-02 state. |
| `submit-l2-tx-mistag-step-02.ts` | Finalize: burn thread, mint permanent token, report asset name/unit/address. |
| `submit-l2-tx-mistag-cancel.ts` | `ct.Cancel` at either step. |

Detection in the watcher is out of scope for this wave (§10 D6): the
predicate is one comparison on bytes the watcher already ingests
(`validity_code != 0` on a normal `transactions_root` leaf), and the
finding-record/prover-core integration follows the missing-signature plan
§3 shape when detector/prover mounting lands.

## 5. Test-harness integration (extend, do not fork)

Additive edits only, each mirroring the native-script-decoding seam:

- `tests/support/emulator/contracts.ts`: `realL2TxMistag` option;
  the chain is applied from the shared double-spend thread/fraud-proof
  policies via `buildL2TxMistagChainV1`.
- `tests/support/emulator/harness.ts`:
  `L2_TX_MISTAG_TEST_CATEGORY_ID_V1 = "00000017"` and the extra-category
  spread into `buildCatalogueDeploymentInfo`.
- `tests/support/emulator/removal-deployment.ts`:
  `L2_TX_MISTAG_REMOVAL_DEPLOYMENT_ENTRY_V1 = "fraudProofL2TxMistag"` and
  the conditional manifest entry pinning the step-01 hash.
- Per-family fixture support lives in
  `tests/support/l2-tx-mistag-emulator-v1.ts` (NOT in the shared
  fixtures): the mis-tagged compact is materialized through
  `materializeMidgardNativeTxFromCanonicalV1` with
  `validity: "TxIsInvalid"` directly, so the shared `makeNativeTx` (which
  pins `TxIsValid`) is untouched and stays separable from the concurrent
  `withdrawal-mistag` work.

## 6. Economics

Identical to the other two-step native families: init + two steps + the
category-agnostic removal; thread min-ADA returns at finalize; the removal
slash and operator-bond routing are the shared handler's business. No
family-specific pacing concerns — the proof is two small transactions and
the membership proof is the only size-variable component (#545 chunked
carriage available when it outgrows the envelope).

## 7. Corner cases

- **Flipped-scalar framing** (§1.3): refused by membership; both planes
  tested.
- **Code-0 leaf challenged**: offchain refused fail-closed at the
  submitter's detection gate; on-chain refused at step-01's exact
  `committed_validity_code != 0` (driven via a direct on-chain attempt in
  the adversarial suite).
- **Scalar outside {0,1}**: not decodable native-V1 compact
  (`expect_validity_code`), so the binding's codec precondition fails
  before the predicate; such a leaf belongs to `da-hash-preimage`/the
  decodability families.
- **Version != 1 leaf**: fails closed inside the binding
  (`expect_supported_native_tx_version`), consistent with D-S13.
- **Forced code-1 leaf**: out of this family's reach by construction —
  binding opens `transactions_root` only; the forced map's verdicts are
  the forced families' and the trace machinery's business.
- **Cancel/resume**: `ct.Cancel` at each step burns the thread; a new
  thread can be re-initiated (shared machinery, exercised by the negative
  suite).
- **Committed-leaf convention residual**: `evidence-source-v1.ts`
  documents two leaf conventions (`payload_source_value` vs
  `native_compact_value`). This family adjudicates the
  `native_compact_value` convention — the one every deployed standalone
  native family and the emulator block fixtures commit, and the only one
  `verify_native_tx_in_state_queue_node` can open. The convention tension
  is family-wide and pre-existing (`catalogue-status.md` §1a); a block
  committing `payload_source_value` leaves is simply outside this family's
  binding, exactly as it is outside `zero-input`'s. Not resolved here.

## 8. Testing

### 8.1 Aiken (embedded selectors, `aiken check -m 'l2_tx_mistag/'`)

Fixture: `MidgardTransaction { ..valid_native_tx_v1(), validity: TxIsInvalid }`
through `native_block_fixture_v1` — a genuinely committed mis-tagged leaf.

- step-01 positive: binds and forwards `{ bad_tx_id, 1 }`.
- step-01 `fail`: honest code-0 leaf (the exact-check negative).
- step-01 `fail`: flipped-scalar forgery against an honest block's root
  (membership refusal — the §1.3 framing).
- step-01 `fail`: forged transactions root (counted-root refusal).
- step-01 `fail`: forwarded state lies about the code.
- step-01 published-chunk positive + forged-root negative (carriage parity
  with `zero-input`).
- step-02 positive: finalizes on `{ bad_tx_id, 1 }`.
- step-02 `fail`: state carries code 0 (the decisive-check negative).

### 8.2 Emulator lifecycle (`tests/submit-init-emulator-l2-tx-mistag.test.ts`)

Real-fault polarity, production submitters, real blueprint: harness with
`realL2TxMistag`, extra category `00000017`, reference scripts published;
mis-tagged fixture block committed via `setupFraudulentBlockV1`; init →
step-01 (datum asserted against the schema twin) → step-02 (permanent
token minted, thread burned) → `submitRemoveFraudulentBlock` with the
explicit category record and `requireReferenceScripts: true` (state-queue
node NFT burned, operator slashed, root emptied, fraud-proof token
retained at the same out-ref).

### 8.3 Adversarial polarity (`tests/submit-init-emulator-l2-tx-mistag-adversarial.test.ts`)

Honest block (code-0 leaf) committed; init succeeds (init does not
adjudicate); then:

- offchain plane: the step-01 submitter refuses fail-closed on the honest
  inclusion;
- on-chain plane, exact check: a forced submission of the honest leaf is
  refused inside step-01's spend (`/failed script execution/`);
- on-chain plane, framing: the flipped-scalar forgery is refused at the
  membership withdrawal (`/failed script execution Withdraw/`);
- the thread is untouched and the honest block remains queued.

### 8.4 Negative controls (`tests/submit-init-emulator-l2-tx-mistag-negatives.test.ts`)

Cancel at step-01 and at step-02 (thread burned, no token), then re-init
resume through the full proof. One emulator suite per file (wasm heap
discipline); `applyCompiledScript` only.

## 9. Sequencing

1. On-chain lib twins + validators; `aiken check -m 'l2_tx_mistag/'`
   (non-zero counts) and full `aiken check`; `aiken build --env testnet`.
2. Offchain modules (§4), harness seams (§5).
3. Emulator suites (§8.2–§8.4) against the regenerated blueprint.
4. `catalogue-status.md` §6 row update.

## 10. Decision register

- **D1 (the design core): conviction requires NO validity adjudication** —
  the committed code-1 normal leaf is the fault per se (§1.1); coverage is
  total and single-party (§1.2). D-S9 remains formally open; this is the
  recorded reading.
- **D2: standalone two-step family**, not a transition-trace extension and
  not a refutation game (§1.4).
- **D3: reference scripts always** (owner ruling; §2.3).
- **D4: canonical category id `00000017`** (§2.1).
- **D5: the exact check is `committed_validity_code != 0`** at BOTH steps
  (step-02 decisive, step-01 fail-fast), with the value read only off the
  authenticated view.
- **D6: watcher detection/finding integration remains open after registration**
  (§4); proof-thread topology alone does not mount it.
- **D7: leaf-convention tension recorded, not resolved** (§7).

## 11. Out of scope

Family-specific CLI, watcher finding records and the proving-core adapter
(§4/D6), the forced-leaf verdict families, the
`withdrawal-mistag` sibling (kept module-disjoint by construction), and
any change to existing transition-trace semantics.
