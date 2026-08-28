# Value-not-preserved fault: offchain implementation plan (v1)

Plan date: 2026-08-26. Audited against branch
`colll78/canonical-v1-watcher-l1-source-checkpoint` (HEAD `a1724e63`) plus
this working tree, which contains the family's **as-built onchain step
chain** (`onchain/aiken/validators/fraud-proofs/value-not-preserved/step-0{1..4}.ak`,
lib modules under `onchain/aiken/lib/midgard/fraud-proofs/value-not-preserved/`,
21/21 family selectors green) that this plan's offchain builders drive.
Catalogue row: `catalogue-status.md` §6 (`value-not-preserved`,
MACHINE-COVERED, W-C14 single-party conversion — fund theft). This document
plans the offchain lane; the onchain contract it restates in §1 is built.

**Decision gate D-C1 (`execution-plan.md:106`) is resolved by this design
and recorded here prominently.** D-C1 asked for the evidence encoding of
multi-asset value sums — a bounded-size on-chain summation strategy for
VALUE-NOT-PRESERVED. The owner-mandated resolution is the **single-asset
claim**: the proof thread names ONE specific asset (policy id + asset name,
with ADA as the distinguished unit-claim case) and one imbalance direction,
and the step chain accumulates only that asset's balance — spent-input
values through ledger-membership witnesses, outputs, the mint field's
entries for that asset, and the fee for the ADA claim — convicting on the
inequality of that single asset's totals. The prover performs the search
for the unbalanced asset offchain; the chain verifies exactly one bounded
fold whose semantics are the validation machine's ValueAndMint accumulator
restricted to the claimed unit. No whole-value-map encoding ever goes
on-chain, so the D-C1 size question dissolves rather than being answered
with a compression scheme.

The parity bar is the `native-script-decoding` family as planned in
`native-script-decoding-offchain-plan-v1.md` and the missing-signature plan
(`missing-signature-offchain-plan-v1.md`): a consumer-agnostic proving core
with CLI and watcher adapters, per-step submitters plus cancel, a
pre-registration emulator harness under a reserved test category id, and
lucid-evolution emulator suites in both polarities — through fraud-proof
mint **and** fraudulent-block removal. Everything those plans decided
generically is inherited here, not re-decided.

Standing rulings this plan implements and never re-opens:

- **Reference scripts always (owner ruling 2026-08-26):** fault-proof step
  validators deploy as reference scripts and are referenced, never attached
  inline, regardless of compiled size (§2.3, §10 D2).
- **Both-polarity emulator tests (owner directive 2026-08-25):** the real
  fault proves through the full lifecycle (fraud-proof token mint and
  fraudulent-block removal), and an adversarial prover against an honest
  commitment is refused **on-chain at the exact check**, not merely by
  offchain guards.
- **Pre-registration explicit-record discipline:** the id never routes
  through the deployment manifest — `parseFraudProofCatalogueDeploymentInfo`
  silently drops non-canonical keys (`catalogue-status.md` §3). No `bin.ts`
  verbs, no entry in `FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER`
  (`demo/midgard-sdk/src/fraud-proof/catalogue.ts`) until the registration
  wave.
- **Reserved ids are expected, not promised:** `0x19` (`#"00000019"`) is
  this family's reserved emulator/test category id, assigned by the parent;
  the production id is written only by the registration wave.
- **Removal via explicit category** (commit `fb7c0217`):
  `RemoveFraudulentBlockExplicitCategory` /
  `buildExplicitCategoryRemovalContracts` / `assembleRemovalContracts` lets
  a pre-registration family drive removal with every fail-closed check
  intact. The fraud-proof token is permanent by design (the state-queue
  node NFT burns; the token survives as evidence and as the `alreadyProven`
  gate) — nothing may assert its burn.
- **Cancellation is an explicit prover decision:** the proving core never
  cancels on its own; an unexpected abort surfaces as a `stalled` outcome.

---

## 1. The contract the builders must satisfy

### 1.1 The rule

The validation machine's ValueAndMint stages
(`onchain/aiken/lib/midgard/validation-machine-v1.ak`) accumulate, per
accepted L2 transaction, a lovelace delta and a per-asset delta map: stage
two adds each spend input's descriptor lovelace and authenticated
asset-leaf quantities (reference inputs contribute nothing), stage three
subtracts each output's, stage four adds each mint entry's quantity, and
stage five rejects unless `lovelace_delta − body.fee == 0` and every
per-asset delta is zero (`reject_value_not_preserved`). A block whose
operator committed and **accepted** a transaction violating this rule has
stolen or destroyed funds.

### 1.2 The claim schema and the conservation equations

`ClaimedAssetV1` (lib `step-01.ak`):

| Claim | Wire form | Conservation equation the chain verifies |
| --- | --- | --- |
| `AdaAsset` | Constr 0 | `Σ_in descriptor.lovelace − Σ_out output.lovelace − fee == 0` |
| `TokenAsset { policy_id (28 B), asset_name (≤32 B) }` | Constr 1 | `Σ_in qty_u + mint_u − Σ_out qty_u == 0` for unit `u = policy ‖ name` |

`ClaimedImbalanceDirectionV1`: `ClaimedAssetInflated` (Constr 0) means the
final delta (`inflow − outflow`) is **negative** — outputs (plus fee for
ADA) exceed sources, value created from nothing; `ClaimedAssetDeflated`
(Constr 1) means it is **positive** — value destroyed. Step-04 convicts iff
the completed delta is non-zero *and* its sign matches the claimed
direction.

Structural facts that simplify the ADA case, proven at
`lib/midgard/fraud-proofs/native-tx/structural-na-q24-ada-minted.test.ak`
and `structural-na-q25-negative-output-value.test.ak`:

- **q24 — ADA-minted is unrepresentable:** a canonical mint item's policy
  id is exactly 28 bytes and the ADA policy is empty, so no committed mint
  preimage can carry lovelace. The ADA equation therefore has **no mint
  term**, and step-03 requires `mint_carriage == None` for an ADA claim
  rather than ignoring one.
- **q25 — negative output values are unrepresentable:** canonical output
  decoding enforces `lovelace >= 0` and per-asset `quantity > 0`, so
  output contributions are always genuine outflow.

### 1.3 The step chain (as built)

| Step | Redeemer arms | What it verifies | State forwarded |
| --- | --- | --- | --- |
| step-01 | `Continue(Args { tx_inclusion, claimed_asset, claimed_direction })` | The one blessed binding (`common.pass_native_tx_to_next_step`, `common.ak` — counted `transactions_root` + `plutarch_phas_raw`); **acceptance gate** `validity_code == 0`; claim well-formedness | `step_02.State { bad_tx_id, claimed_asset, claimed_direction, committed_fee, prev_utxos_root, input_cursor: 0, claimed_delta: 0 }` |
| step-02 | `FoldInput` (self-loop) / `FinishInputs` | Per input: §8.8 door read of field 0 at the cursor; ledger membership of the descriptor under `prev_utxos_root` (single witness path, §1.5); full authenticated asset-leaf walk; delta `+= qty_u` (or `+= lovelace`). Finish: cursor equals the **authenticated** field item count | self-loop with cursor+1 / `step_03.State { …, claimed_delta }` |
| step-03 | `Continue(Args { native_tx_compact_cbor, outputs_carriage, mint_carriage })` | §3 anchor paid once; whole-field fold of outputs (`delta −= qty`), whole-field fold of mint for a token claim (`delta += qty`), `delta −= fee` for ADA (with `mint_carriage == None`) | `step_04.State { bad_tx_id, claimed_asset, claimed_direction, final_delta }` |
| step-04 | `Continue` (finalize) | Category prefix `#"00000019"`; **decisive inequality** — `final_delta != 0` with sign matching the claimed direction | permanent fraud-proof token via `common.finalize` |

Every step keeps the standard `Cancel` arm. Validator parameters follow the
family templates: step-01 `(step_02_hash, ct_policy, hub_oracle)`, steps
02/03 `(next_step_hash, ct_policy, field_preimage_certificate_policy_id)`,
step-04 `(fraud_proof_policy, fraud_proof_address, ct_policy)`.

### 1.4 Acceptance-evidence binding

The acceptance gate reads `validity_code` off the compact structure that
`pass_native_tx_to_next_step` authenticated: those bytes are the **exact
leaf value** the block's counted `transactions_root` commits at the
challenged transaction id (`plutarch_phas_raw` proved membership of those
very bytes), so the code is committed by the root, not supplied by the
prover — the same argument the transition-trace family's
`unanchored_validity_code_of(anchored) == 0` call site documents
(`field-opening-v1.ak`; any new caller owes the argument in its own words,
and step-01 states it at the call site). Step-01 is the only step that
reads the code; downstream steps re-anchor body fields 0/2/5 by
`bad_tx_id` alone, which §3 makes a complete anchor for body fields. An
invalid transaction honestly recorded as a no-op (`validity_code != 0`)
refuses at step-01 and can never convict — the family's first soundness
guard, with a dedicated selector
(`value_not_preserved_step_01_rejects_a_rejected_transaction`).

### 1.5 Spent-input value evidence (per-step evidence model)

Spent-input values come **exclusively** from ledger-membership witnesses
against the challenged header's pre-state trie — the path
`apply_l2_spends` fetches values through
(`transition-trace/proof.ak`: `verify_ledger_membership`, key
`ledger_outref_key = encode_midgard_tx_input(input)`, made `pub` by this
wave; no second binding path exists). Per `FoldInput` iteration the
redeemer carries `SpentInputValueWitnessV1`:

- `descriptor_cbor` — the exact `LedgerOutputCommitmentV1` bytes committed
  at the input's out-ref key; decoded totally and canonically
  (re-encode check) before any field is read.
- `ledger_membership_proof` — the MPF proof under
  `state.prev_utxos_root`.
- `asset_peaks` + `asset_openings` — for a token claim, **every** asset
  leaf `0 .. asset_count−1`, each authenticated at its index via
  `ledger_output_commitment_v1.verify_asset_membership` (the machine's own
  per-leaf authentication). The claimed unit's quantity is the sum of
  matching leaves; a zero is an *established absence* because the walk saw
  every leaf. For an ADA claim the walk is empty-by-requirement and the
  value is the descriptor's own `lovelace` scalar.

Conviction-safety of pre-state values (documented in lib `step-02.ak`): an
out-ref's value is immutable (keyed by producing tx id, ids are content
hashes, at most one insertion), so for an accepted transaction either the
pre-state witnesses are the very values the machine resolved, or the block
already committed a distinct accepted-input-set fault (in-block
double-spend) — in both branches a completed unbalanced fold convicts a
genuinely fraudulent block. The corresponding **completeness gap** is §7.4.

### 1.6 Selector inventory (adversarial spec, all green in this tree)

`aiken check -m 'value_not_preserved/'` — 21 tests, 21 passed:

- step-01: binds-and-freezes positive; rejected-transaction refusal at the
  acceptance gate; malformed-claim refusal.
- step-02: token fold positive (walks a 2-leaf frontier, claimed + decoy
  units); ADA fold positive; forged-descriptor refusal at
  `verify_ledger_membership`; truncated-asset-walk refusal at the
  `openings == asset_count` check; forged-leaf-quantity refusal at
  `verify_asset_membership`; finish positive; premature-finish refusal at
  the authenticated-count check.
- step-03: token fold to `−10`; ADA fold to `−500_000`;
  minted-and-fully-paid-out folds to `0` (forwarded — refusal is
  step-04's); substituted-outputs-preimage refusal at the door's
  `field_commitment` check; missing-mint-carriage refusal.
- step-04: inflated-token conviction; deflated-token conviction; inflated-
  ADA conviction; balanced-fold refusal (`0 == 0` never convicts — covers
  the asset-absent and wrong-asset-claim cases, whose folds are balanced
  by construction); direction-mismatch refusal; wrong-category refusal.

Every fixture block commits a **real** single-leaf pre-state ledger MPF —
the descriptor bytes are `ledger_output_descriptor_v1.ledger_value_v1` of
the spent output, i.e. the protocol's own ledger reading
(lib `thread-fixture-v1.ak`).

---

## 2. Registration

### 2.1 Category id

Reserved test id `0x19` → `#"00000019"`, held family-locally as
`value_not_preserved_fraud_category_id` (lib `step-01.ak`) with the
standard pre-registration comment. The id appears **only** in emulator
wiring (`extraCategories` sidecar); never in
`FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER`, never in `bin.ts`, never through
the deployment manifest.

### 2.2 What registration touches (later wave, not this one)

Appending `valueNotPreserved` to the SDK catalogue order, the `bin.ts`
verbs, the category union in `submit-init.ts`, and re-verifying the id is
still the next free index after standing reservations
(`0x0b` fabricated-deposit, `0x0c` fabricated-withdrawal, `0x0d`
native-script-decoding, `0x0e` missing-signature, sibling waves' ids, then
this family).

### 2.3 Script deployment: reference scripts (owner ruling 2026-08-26)

All four step validators (parameterised via `applyCompiledScript`) deploy
as plain reference scripts through the `publishPlainReferenceScriptUtxo`
pattern the emulator support already provides; every step transaction
names them via `Lucid.readFrom`. Never inline, regardless of size.

---

## 3. Detection

The watcher's replay already runs the machine's ValueAndMint fold per
accepted transaction. Detection is a byproduct: when replay of an accepted
transaction ends with `lovelace_delta − fee != 0` or a non-zero per-asset
delta, the offchain finding records the first offending unit (deterministic
order: lovelace first, then the accumulator's unit order) and its delta
sign — which *is* the claim `(ClaimedAssetV1, direction)`. The prover-side
search in this wave (`finding-v1.ts`) re-derives the same from raw
materials — the committed compact tx, the pre-state descriptors of its
inputs, its outputs and mint — so the emulator and CLI can operate without
a watcher: fold each unit's totals offchain, pick the first non-zero
delta, emit `{ txId, claimedAsset, claimedDirection, expectedDelta,
perInputQuantities }`. A rejected (`validity_code != 0`) transaction is
never classified — mirror of the on-chain acceptance gate.

---

## 4. New offchain modules

All under `demo/midgard-fault-proofs/src/value-not-preserved/`, mirroring
the decoding/missing-signature layout; consumer-agnostic core, thin
adapters. No SDK file is touched.

| Module | Role |
| --- | --- |
| `contracts-v1.ts` | Explicit pre-registration contracts record: compiled step scripts from the blueprint (read-only; `MIDGARD_REAL_BLUEPRINT_PATH` override), parameter application (step-02 hash into step-01, …), reference-script deployment descriptors. Never reads the deployment manifest. |
| `finding-v1.ts` | The prover-side search (§3): fold per-unit totals of an accepted committed tx against pre-state descriptors; emit the claim + expected deltas; refuse rejected txs and unknown inputs (same-block-created inputs → `unprovable` finding, §7.4). |
| `evidence-v1.ts` | Builders for the on-chain evidence values: `NativeTxInclusionArgs` material, per-input `SpentInputValueWitnessV1` (descriptor bytes, MPF proof from the ledger trie, frontier peaks + full leaf openings in leaf order), outputs/mint preimage carriages (tier 1 `Inline` in v1). |
| `submit-common-v1.ts` | Shared step-transaction assembly: thread UTxO location, reference-script `readFrom`, redeemer schemas (`ClaimedAssetV1`, direction, witness, fold/finish arms), state (de)serialisation for `StepDatum`. |
| `submit-init.ts` fork (`submit-value-not-preserved-init-v1.ts`) | Thread Init under the sidecar category id (asset name `00000019 ‖ headerHash`), first-step datum, prover signature. |
| `submit-value-not-preserved-step-01..04-v1.ts` | Per-step submitters: 01 binding + claim; 02 `foldInput` (one tx per spend input) and `finishInputs`; 03 outputs+mint fold; 04 finalize (fraud-proof mint redeemer). |
| `submit-value-not-preserved-cancel-v1.ts` | Explicit cancel, any step. |
| `prover-v1.ts` | The proving core: drives finding → init → steps → finalize with crash-resume (re-locate thread by asset name, re-derive next arm from the carried state's `input_cursor`/step address), `stalled` outcome on refusal. |
| `adapters/` + `index.ts` | CLI/watcher adapters and exports. No `bin.ts` wiring. |

### 4.1 The proving core's step policy

One L1 transaction per step-02 input (self-loop), then `finishInputs`,
then step-03, then step-04. The per-input pacing is the D-C1 bounded-size
property surfacing offchain: cost grows linearly with the challenged
transaction's input count, each step bounded by one descriptor walk.

---

## 5. Carriage frontiers

v1 submits all §8.8 carriages **tier 1 (`Inline`)**: field-0 preimage
(38 B/input), outputs preimage, mint preimage. Outputs are the only field
that can grow large; the door's tier-2 (`RawUtxo`) and tier-3
(`Certified`) seams are already generic in the as-built validators
(carriage is a redeemer argument), so escalation is an offchain-only
change. The envelope suite discipline (never name `compiledCode` outside
the envelope suite — #610 text-scan gate) is inherited.

---

## 6. Economics and pacing

Selector-level costs (from `aiken check`, single-input fixtures): step-01
≈ 9.5 M mem, step-02 fold ≈ 10.9 M (2-leaf walk), step-03 ≈ 9.4 M,
step-04 ≈ 8.6 M — all comfortably inside per-tx budgets. The
linear-in-inputs loop means a worst-case C52-capped transaction costs
`inputs + 3` L1 transactions per proof; bond/deposit economics are the
generic thread economics, not re-decided here.

---

## 7. Corners

### 7.1 Asset absent from the transaction (`0 == 0` refusal)

A claim naming a unit the transaction never touches completes every fold
at zero and refuses at step-04's inequality — the balanced-fold selector
covers it. No absence short-cut exists earlier: the walks are total, so
"absent" is only ever established by having seen everything.

### 7.2 Minted-and-fully-paid-out refusal

`minted_paid_out_tx_v1`: 0 sourced, +25 minted, −25 paid out → `0`.
The mandatory whole-mint fold is what keeps this balanced; omitting the
mint carriage for a token claim is refused at step-03
(`rejects_a_missing_mint_carriage`), so a prover cannot present a minted
unit as inflated.

### 7.3 Fee handling on the ADA claim

The fee is read once, at step-01, off the root-committed compact body and
frozen in thread state; step-03 subtracts it exactly once, only for
`AdaAsset`. Token claims never touch it (the machine charges the fee
against lovelace only). q24/q25 (§1.2) remove the mint term and the
negative-output guard from the ADA equation.

### 7.4 Same-block-created inputs (completeness gap, recorded)

A challenged transaction spending an output created earlier in the same
block has no `prev_utxos_root` membership for that input; the fold cannot
complete and the thread can only be cancelled. This is a **refusal, never
a wrong conviction** (§1.5). The offchain finding classifies such faults
`unprovable-by-this-family` in v1. The recorded future extension is an
in-block production arm — a second `verify_native_tx_in_state_queue_node`
binding of the producing transaction under the same thread asset name,
reading the spent output from its field 2 and deriving the descriptor via
`ledger_value_v1` — which stays inside the blessed binding path; it is
out of scope for v1.

### 7.5 Duplicate spend inputs (separate family)

The fold counts each occurrence exactly as a naive field-0 walk does.
Input-set uniqueness is a separate family's rule; conviction here stays
sound because an *accepted* duplicate-input transaction is already
fraudulent (the machine's input-set stage rejects duplicates), so any
conviction reached through a duplicate-bearing fold convicts a genuinely
fraudulent block. The dedup-balanced completeness corner (a fold that
looks unbalanced only because a duplicate was counted twice cannot arise:
duplicates *increase* inflow, and if the resulting delta is non-zero the
block is fraudulent either way — by value or by input-set) is delegated to
the input-set-uniqueness family. Scope stays separable: nothing here reads
or asserts uniqueness.

### 7.6 Non-canonical mint preimages

`decode_mint_policy_item_cbor` (made `pub` this wave) enforces per-item
canonical form; a committed non-canonical mint preimage fails the fold
closed (and is separately canonical-decodability fraud). Duplicate
policies across items are summed, which can only convict a block that is
fraudulent anyway (lib `step-03.ak` header).

---

## 8. Testing

### 8.1 Aiken (as built, this tree)

`aiken check -m 'value_not_preserved/'` (trailing slash — the bare filter
is vacuous): 21/21. Extraction re-runs: `-m 'transition_trace/'` 74/74,
`-m 'native_tx/'` 21/21.

### 8.2 Emulator suites (new per-family files, this wave)

Under `demo/midgard-fault-proofs/tests/`, extending — not forking — the
decoding harness (`tests/support/emulator/catalogue.ts` `extraCategories`
sidecar, removal-deployment explicit-category entry, reference-script
publication, `expectOnchainRefusalV1`):

1. `submit-init-emulator-value-not-preserved-token.test.ts` — real token
   fault (inflated), full lifecycle: init → step-01 → per-input folds →
   finish → step-03 → step-04 fraud-proof token mint → explicit-category
   fraudulent-block removal. Asserts the permanent token's asset name and
   the removal's fail-closed checks.
2. `submit-init-emulator-value-not-preserved-ada.test.ts` — real ADA
   fault through the same lifecycle (mint + removal), exercising the
   no-mint-carriage ADA arm.
3. `submit-init-emulator-value-not-preserved-adversarial.test.ts` —
   adversarial prover against an honest (balanced) commitment: steps land
   up to the decisive check, and step-04 is refused **on-chain at the
   inequality** (`value_not_preserved_fault_is_established_v1`), via
   `expectOnchainRefusalV1`.
4. `submit-init-emulator-value-not-preserved-negatives.test.ts` —
   wrong-asset claim against a block unbalanced in a different unit
   (refused at the step-04 inequality); forged value witness (tampered
   descriptor/leaf quantity refused at the step-02 membership checks);
   rejected-transaction claim (refused at the step-01 acceptance gate).

Multi-input folds (≥2 spend inputs, multi-leaf ledger tries) are covered
here rather than in Aiken, using the SDK's MPF library for real proofs.

### 8.3 What lands at registration

CLI verbs, catalogue append, canonical-id switchover, deployment-manifest
entry — none of it in this wave.

---

## 9. Sequencing

1. **Onchain family** — DONE in this tree (§1.6), including the two lib
   extractions (`ledger_trie`/`verify_ledger_membership` →
   `pub` in `transition-trace/proof.ak`; `decode_mint_policy_item_cbor` →
   `pub` in `native-tx/preimages.ak`) with touched-module suites green.
2. **Blueprint** — `aiken build` with the repo's patched fork (stock
   v1.1.22 has the unsound codegen bug; the pin is v1.1.23+6801f62).
3. **Offchain modules** (§4), then **emulator suites** (§8.2).
4. **Catalogue-status update** — §6 row flip + §1-style pre-registration
   row.

---

## 10. Decision register

| # | Decision | Status |
| --- | --- | --- |
| D1 | Single-asset claim + direction resolves D-C1 (owner-mandated); machine parity by restriction, prover searches offchain | DECIDED (header) |
| D2 | Reference scripts always | Standing ruling |
| D3 | Spent-input values from pre-state ledger-membership witnesses only; same-block inputs are a recorded completeness gap (§7.4), the in-block production arm is future work | DECIDED — most canonical-V1-consistent reading of the single-witness-path mandate |
| D4 | Acceptance gate at step-01 via root-committed `validity_code`, argument restated at the call site | DECIDED (§1.4) |
| D5 | Direction is part of the claim and must match the proven sign — a permanent token records the fault actually proven | DECIDED |
| D6 | Whole-field folds for outputs and mint (absence by totality); ADA claim refuses a mint carriage (q24) | DECIDED |
| D7 | Tier-1 carriages in v1; tier-2/3 escalation is offchain-only later | DECIDED (§5) |
| D8 | Duplicate-input interplay stays in the input-set-uniqueness family | DECIDED (§7.5) |

---

## 11. Out of scope

Registration (§2.2, §8.3); the in-block production arm (§7.4); tier-2/3
carriage submission; watcher wiring beyond the adapter seam; any change to
`validation-machine-v1.ak` or existing validators (none was made — the two
lib visibility extractions aside, which left their suites green).
