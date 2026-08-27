# Withdrawn-input fault: plan and as-built record (v1)

Plan date: 2026-08-26. Audited against branch
`colll78/canonical-v1-watcher-l1-source-checkpoint` (HEAD `a1724e63`).
Work item: **W-C3** (spend-side `withdrawn-input`,
`catalogue-status.md` §6; `coverage-matrix.md` §2 "Spend of withdrawn
input"). Unlike the Q16 missing-signature document this is a plan **and**
an as-built record: the same wave that wrote it delivers the on-chain
family, the offchain tooling, and the emulator suites it specifies.
Registration remains out of scope (§10).

The family is the spend-side mirror of `withdrawn-reference-input`
(`catalogue-status.md` §1 row 11): a transaction the operator committed
under a block's counted `transactions_root` **spends** an input that a
valid L2 withdrawal, committed under the same header's counted
`withdrawals_root`, already consumed. Ledger rule WITHDRAWN-INPUT
(`technical-spec/5-ledger-rules/1-cardano-ledger-rules.tex:102-119`).
Fund-theft class: the operator pays the UTxO out on L1 through the
withdrawal _and_ lets an L2 transaction spend it again.

Standing rulings this plan implements and never re-opens (the
missing-signature plan's list, inherited verbatim):

- **Reference scripts always (owner ruling 2026-08-26):** all three step
  validators deploy as reference scripts, never inline.
- **Both-polarity emulator tests (owner directive 2026-08-25):** the real
  fault proves through fraud-proof mint **and** fraudulent-commitment
  removal; an adversarial prover against an honest commitment is refused
  **on-chain at the exact check**.
- **Pre-registration explicit-record discipline:** no CLI verbs, no SDK
  catalogue append, no `submit-init.ts` union change; ids never route
  through the deployment manifest (`parseFraudProofCatalogueDeploymentInfo`
  silently drops non-canonical keys).
- **Reserved ids are expected, not promised:** the test-harness constant
  records the reserved index; the production id is written only by the
  registration wave.
- **Removal via explicit category:** the
  `RemoveFraudulentBlockExplicitCategory` seam (commit `fb7c0217`) drives
  removal for a pre-registration family with zero changes to
  `remove-fraudulent-block.ts`. The fraud-proof token is permanent by
  design; the state-queue node NFT burns at removal.

## 1. The on-chain family (as built)

`onchain/aiken/validators/fraud-proofs/withdrawn-input/step-0{1..3}.ak`,
lib wire twins `onchain/aiken/lib/midgard/fraud-proofs/withdrawn-input/`.
Step chain `Init → 01 → 02 → 03`, `ct.Cancel` on every step, five L1
transactions worst case (Init + three steps + removal ride-alongs
excluded). Transaction binding is exclusively
`verify_native_tx_in_state_queue_node` / `pass_native_tx_to_next_step`
(`common.ak:575-634`) — no new binding path. `Init` binds only step-01's
validator hash (`computation-thread.ak:42-61`); a future catalogue
registers the step-01 hash and nothing else.

Parameterization (acyclic, applied backwards, step-03 first):

| Validator                              | Parameters (blueprint-declared order)                                                                         |
| -------------------------------------- | ------------------------------------------------------------------------------------------------------------- |
| `fraud_proofs/withdrawn_input/step_01` | `step_02_validator_script_hash`, `computation_thread_token_policy_id`, `hub_oracle`                           |
| `…/step_02`                            | `step_03_validator_script_hash`, `computation_thread_token_policy_id`, `field_preimage_certificate_policy_id` |
| `…/step_03`                            | `fraud_proof_token_policy_id`, `fraud_proof_token_address`, `computation_thread_token_policy_id`              |

Compiled sizes from the worktree blueprint (patched fork `v1.1.23+6801f62`):
step-01 5,804 B · step-02 7,116 B · step-03 5,631 B. Reference-script
deployment makes these publication-accounting inputs only.

### 1.1 Evidence model, per step

**Step-01 — binding.** `Continue(NativeTxInclusionArgs)`: the shared
native binding (counted `transactions_root` authentication +
`plutarch_phas_raw` membership over the raw compact CBOR). Bare args only
— no published-chunk arm, exactly like the sibling family. Output at
step-02's address:
`step_02.State { bad_tx_id, blocks_withdrawals_root, blocks_withdrawal_count }`.
The counted withdrawals commitment is read off the **same authenticated
header** that proved the transaction's inclusion — this is what makes the
family's two roots commensurable: both are fields of one
`HeaderV1` the state-queue node NFT authenticates.

**Step-02 — selection.** Opens body field **0** (`spend_inputs`) through
the §8.8 door (`opened_field_view`, `BodyAnchor { tx_id: bad_tx_id }`,
`spend_inputs_field_index` as a **literal**) and selects the accused
spend input by `bad_input_index` via `spend_input_at`'s fixed 38-byte
stride (out-of-domain ordinal aborts, never clamps). Args:
`{ input_index, output_index, spend_inputs_opening: FieldOpeningV1,
bad_input_index }`. Output:
`step_03.State { withdrawn_input, blocks_withdrawals_root,
blocks_withdrawal_count }`.

**Step-03 — adjudication + finalize.** `common.finalize` burns the thread
NFT and mints the permanent fraud-proof token, gated on:

1. the prover-supplied
   `withdrawal_membership: RootMembershipProof<WithdrawalId, WithdrawalInfo>`
   carrying a leaf whose `validity` is **`WithdrawalIsValid`** — any other
   constructor refuses (§4.1);
2. the leaf's `body.l2_outref` equal, field-by-field, to the thread's
   `withdrawn_input`;
3. counted-root membership:
   `verify_root_membership_with_bytes(…, WithdrawalsRootDomain,
blocks_withdrawals_root, blocks_withdrawal_count,
cbor.serialise(key), cbor.serialise(value))` — the existing
   withdrawals-root walker, no new machinery.

### 1.2 Spend-vs-reference mirroring decisions

Decided by studying the `input-no-idx` / `reference-input-no-idx` sibling
pair (the repo's canonical spend↔reference mirror) and applied here:

- **The field index is the only semantic delta and it is a literal.**
  Step-02 reads `spend_inputs_field_index = 0` where the sibling reads
  `reference_inputs_field_index = 1`; §5.3 gives both collections the same
  fixed-stride item shape, so `spend_input_at` serves both (its own doc:
  "a spend- **or reference-input** item"). Because §4 removed the
  domain separation between the two preimages, the index being a
  compile-time literal — never a redeemer argument — is what keeps a
  spend-side accusation from being steered onto the reference collection,
  and vice versa.
- **State field renamed for truth:** the sibling's step-03 carries
  `missing_reference_input` (a name inherited from `no-reference-input`);
  this family carries `withdrawn_input` — the input the prover claims a
  withdrawal consumed. Same wire shape (`MidgardTxInput`), honest name.
- **Step-01 and step-03 are structural mirrors, not copies:** step-01
  forwards the same three-field state as the sibling (the withdrawals
  commitment ride-along is identical); step-03's adjudication is
  byte-for-byte the sibling's check with the accused input sourced from
  field 0. No shared helper was extracted: the sibling family's files are
  untouched, per the wave's isolation constraint, and the duplicated logic
  is the three-expect adjudication block — small enough that a shared
  module would couple two families' upgrade cadence for no soundness gain
  (the mirror-pair precedent: `input-no-idx`/`reference-input-no-idx`
  duplicate their shared shape the same way).
- **Aiken selector inventory exceeds the sibling's:** the sibling's
  step-03 never tests the invalid-withdrawal refusal; this family adds it
  (§4.1) because on the spend side a wrongful conviction through an
  invalid withdrawal leaf would be a slashing vector, not just a liveness
  bug.

Aiken tests (7/7 green, `aiken check -m 'withdrawn_input/'` — trailing
slash, non-zero count confirmed): step-01 positive binding; step-02
positive selection, substituted-preimage refusal (door authenticate-once),
out-of-range-ordinal abort; step-03 conviction on a committed valid
withdrawal of the spent input, valid-block refusal (withdrawals root
commits a different UTxO — fails at the membership check), and
invalid-withdrawal refusal (leaf targets the spent input but carries
`SpentWithdrawalUtxo` — membership verifies, the validity gate refuses).

---

## 2. Registration posture

### 2.1 Reserved emulator-test category id: `00000018`

**Assigned to this family by the parent orchestration — not chosen here.**
Standing reservations at HEAD stop at `0000000d`
(native-script-decoding's test id); parallel family waves hold the
intermediate indices. The constant lands as
`WITHDRAWN_INPUT_TEST_CATEGORY_ID_V1 = "00000018"` in the emulator
harness, with the inherited caveat: it is the emulator wiring's id only.
The production id is allocated by the registration wave, which re-verifies
next-free at allocation time. In code, the id appears only on test surfaces —
not in `FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER`, not in `bin.ts`, and never in
the deployment manifest.

### 2.2 What stays untouched until the registration wave

`demo/midgard-sdk/src/fraud-proof/catalogue.ts`, `submit-init.ts`'s
category union, `bin.ts`, `inspect-contracts` unions, watcher
`families[]`, and every pinned root/fixture. The family's contracts
record is explicit and parent-owned (§5), mirroring
fabricated-withdrawal's pre-registration shape.

### 2.3 Deployment: reference scripts

All three steps publish as plain reference-script UTxOs in the emulator
(the `publishPlainReferenceScriptUtxo` shape) and every submitter sources
its spending validator by reference with a fail-closed hash check. No
inline attachment anywhere, regardless of size.

---

## 3. Fault statement, boundaries, and corner cases

### 3.1 The predicate

Over one committed header `H`: ∃ tx `t` with `t ∈ transactions_root(H)`,
∃ spend input `i ∈ spend_inputs(t)`, ∃ withdrawal event `w ∈
withdrawals_root(H)` with `validity(w) = WithdrawalIsValid` and
`l2_outref(w) = i`. Deterministic over committed public data;
single-party per GOAL_SPEC §3 invariant 3.

### 3.2 Boundary: double-spend

Spending a withdrawn input _looks like_ a double-spend but is provable by
neither direction of that family, and vice versa — the two families
partition the "same UTxO consumed twice" space by **which root commits
the second consumer**:

- `double-spend` (§1 row 1): both consumers are transaction leaves under
  `transactions_root` (`tx1_id != tx2_id`, equal spent outref). A
  withdrawal event is not a transaction leaf; the double-spend chain
  cannot reach `withdrawals_root`.
- `withdrawn-input` (this family): one consumer is a transaction leaf,
  the other a **withdrawal leaf** under `withdrawals_root`. This chain
  carries no second transaction and cannot convict two transactions.

A block committing all three consumers (two txs and a withdrawal on one
outref) exhibits both faults; either family's proof suffices for removal,
no exclusivity is needed, and neither subsumes the other. The emulator
negatives pin the boundary from this family's side (§7).

### 3.3 Boundary: cross-block withdrawal is `no-input`'s fault

This family adjudicates the **same-header** collision only: both roots
hang off the one `HeaderV1` the thread authenticated. A transaction
spending an input withdrawn in an _earlier_ block is a different fault
with different evidence: the withdrawal's application removed the UTxO
from the ledger, so the offending block's `prev_utxos_root` lacks it and
the committed spend is exactly a `no-input` violation (§1 row 2 —
non-membership in pre-state, not produced in-block). No gap: the two
families tile the timeline, and the detector routes by block distance.

### 3.4 Boundary: withdrawn _reference_ input

The reference side is `withdrawn-reference-input` (§1 row 11), whose
offchain/emulator lifecycle is a separate concurrent wave. Same-header
predicate, field-1 selection, consistency class (a reference does not
consume). This wave does not touch that family's code; the two step-02s
cannot be cross-steered because each field index is a literal (§1.2).

### 3.5 Corner: an invalid withdrawal must NOT convict

A withdrawal leaf with any validity other than `WithdrawalIsValid`
(`NonExistentWithdrawalUtxo`, `SpentWithdrawalUtxo`, wrong owner/value/
signature, `TooManyTokensInWithdrawal`, `UnpayableWithdrawalValue`)
records an **order the operator refused**: it consumed nothing, so a
transaction spending its target UTxO is honest. Step-03's
pattern-match on `WithdrawalIsValid` is the gate; the Aiken
invalid-withdrawal selector and the emulator adversarial scenario (§7)
both pin it. Conversely, a withdrawal _wrongly marked_ invalid is the
`withdrawal-mistag` gap (§6 of `catalogue-status.md`) — mis-tagging is
adjudicated there, never here: this family takes the operator's own
verdict as the commitment it holds them to. Both-directions honesty:
convicting on a mistagged-valid leaf is impossible here (the leaf says
invalid), and that is correct — the mistag itself is the fault, and it
has its own row.

### 3.6 Corner: same-header ordering does not matter

The predicate is set-shaped: no order between the withdrawal's
application and the transaction's exists at this layer (trace-order
faults belong to `transition-trace`). Whichever way the operator's trace
sequenced them, a correct trace could not have applied both — one of the
two consumptions must have been applied against a state lacking the UTxO
— so committing both events under one header is itself the fraud. The
family therefore needs no trace evidence, which is what keeps it three
steps long.

### 3.7 Corner: duplicate spend inputs inside the disputed tx

If the disputed transaction lists the withdrawn outref twice, any
ordinal selecting it convicts — the family is indifferent to intra-tx
duplication (that is `input-set-uniqueness`'s row, machine-covered).

---

## 4. SDK and evidence preparation (as built)

`demo/midgard-sdk/src/fraud-proof/withdrawn-input-v1.ts` is the exact
TypeScript wire twin for the three Aiken datums/redeemers and cancellation,
including the counted withdrawals commitment and terminal membership proof.
Its public predicate accepts only a `WithdrawalIsValid` leaf whose
`l2_outref` equals the selected spend input.

`prepare-withdrawn-input.ts` supports retained material and canonical block
evidence. It decodes canonical native transactions, finds a valid colliding
withdrawal, rebuilds both tries, checks both counted roots, and emits the
transaction-membership, literal spend ordinal, and withdrawal-membership
evidence. Root mismatch, absent transaction, out-of-range requested ordinal,
and no valid collision are typed, fail-closed preparation rejections. Optional
file output contains evidence only; it never weakens the authenticated-header
checks.

## 5. Off-chain submission chain (as built)

The pre-registration family owns an explicit `WithdrawnInputContractsV1`
record and these submitters:

1. init binds the explicit step-01 hash under the reserved emulator category;
2. step-01 authenticates the native transaction under the header's counted
   transactions root and carries that same header's counted withdrawals root;
3. step-02 opens body field 0 through the shared authenticated-field door and
   selects the spend ordinal;
4. step-03 supplies the withdrawal membership, burns the computation thread,
   and mints the permanent fraud-proof token; and
5. cancel burns the thread at any of the three step validators with prover
   authorization.

Every step resolves the expected deployed reference-script UTxO and verifies
its script hash before building. The spending scripts are **referenced, never
attached inline**. Common state/address/thread-token checks reject stale or
cross-family UTxOs before submission.

## 6. Pre-registration emulator and removal wiring (as built)

`WITHDRAWN_INPUT_TEST_CATEGORY_ID_V1 = "00000018"` is enabled only by the
emulator harness's explicit extra-category path. `buildWithdrawnInputChainV1`
applies the three validators backwards in blueprint order. The scenario
publishes all three plain reference-script UTxOs and constructs a real compact
native transaction, an independent withdrawal MPF leaf, and one header whose
two counted roots authenticate them.

The removal deployment helper exposes the explicit entry name
`fraudProofWithdrawnInput`. The positive lifecycle passes an
`RemoveFraudulentBlockExplicitCategory` record, so the unregistered family can
exercise real state-queue removal without modifying production catalogue or
manifest parsing. Removal burns the queue node NFT and retains the permanent
fraud-proof token.

## 7. Test matrix (as built)

- SDK round trips cover the step-02 counted commitment, terminal membership
  redeemer, and the valid-vs-invalid/different-outref predicate.
- Aiken selectors cover the positive three-step path plus substituted field
  preimage, out-of-range ordinal, honest/different withdrawal, and invalid
  withdrawal refusals (7/7).
- The positive emulator lifecycle prepares evidence from the realistic
  fixture, advances all steps, mints the fraud token, and removes the
  fraudulent block through the explicit-category seam.
- Two adversarial emulator cases reach step-03 and are refused by the spending
  validator: a proof/value substitution against an honest different
  withdrawal, and a committed invalid withdrawal targeting the selected
  input. The computation thread remains in both cases.
- The negative/resume/cancel case rejects the wrong step reference locally,
  resumes the same thread through step-02, and cancels at step-03.

## 8. Verification gate

The required narrow gate is: focused Aiken selector with a non-zero collected
count, Aiken build, SDK unit/type/build, fault-proof typecheck and the four
withdrawn-input emulator suites against the worktree blueprint, followed by
Prettier and ESLint on every touched TypeScript/Markdown path. The exact
commands and counts belong in the delivery report so a reviewer sees executed
evidence rather than a copied promise.

## 9. Catalogue/documentation reconciliation

`catalogue-status.md`, `coverage-matrix.md`, and `execution-plan.md` record the
family as implemented and emulator-proven but not registered or release-ready.
They do not claim a production id, CLI route, watcher route, preprod run, or
mainnet readiness.

## 10. Deferred and out of scope

- Production catalogue id/allocation, catalogue deployment, generic
  `submit-init` union, CLI verbs, watcher registration, manifest routing,
  preprod execution, and release evidence are deferred to the registration
  wave.
- `withdrawn-reference-input` remains wholly outside this wave.
- `double-withdraw`, withdrawal mistagging, and cross-block withdrawn spends
  retain their separate proof routes described above.
- Reference scripts are deployment objects consumed by reference; no builder
  attaches them inline.
