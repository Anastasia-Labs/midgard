# Double-withdraw fault: offchain implementation plan (v1)

Plan date: 2026-08-26. Audited against branch
`colll78/canonical-v1-watcher-l1-source-checkpoint` (HEAD `a1724e63`). Work
item: **W-C3** (`catalogue-status.md` §6 row `double-withdraw`, class **fund
theft**; coverage-matrix row DOUBLE-WITHDRAW, evidence
`technical-spec/5-ledger-rules/1-cardano-ledger-rules.tex:143-161`). Unlike
the Q16 plan this document accompanies a **complete implementation in the
same change**: on-chain step chain, SDK codec twin, prepare/submit modules,
pre-registration emulator wiring and both-polarity emulator suites all land
together, uncommitted, for owner review.

> **Flagged owner-level refinement (decision D1, top of register).** The
> spec's bare predicate (`∃ w ≠ w1 ∈ wtxs: l2_outref(w) = l2_outref(w1)`)
> is **unsound against honest operators** as a conviction rule: due L1
> withdrawal events MUST be included in the block that covers their
> inclusion time, so when two orders drain the same L2 UTxO an honest block
> commits **both** leaves and tags the duplicate with a non-payable verdict
> (`SpentWithdrawalUtxo`/`NonExistentWithdrawalUtxo`) — only
> `WithdrawalIsValid` leaves pay out at settlement
> (`validators/user-events/withdrawal.ak`). The implemented predicate
> therefore additionally requires **both leaves payable**. This is a
> refinement of the spec text, taken as the most canonical-V1-consistent
> option rather than stalling; it needs an owner nod and, if confirmed, a
> one-line spec erratum adding the validity clause to DOUBLE-WITHDRAW.

Standing rulings this plan implements and never re-opens:

- **Reference scripts always (owner ruling 2026-08-26):** fault-proof step
  validators deploy as reference scripts and are referenced, never attached
  inline, regardless of compiled size (§2.3, §10 D2).
- **Both-polarity emulator tests (owner directive 2026-08-25):** the real
  fault proves through the full lifecycle — fraud-proof mint **and**
  fraudulent-commitment removal — and an adversarial prover against an
  honest commitment is refused **on-chain at the exact check** (§8).
- **Pre-registration explicit-record discipline:** no route through the
  deployment manifest (`parseFraudProofCatalogueDeploymentInfo` silently
  drops non-canonical keys); explicit contracts records; the SDK catalogue
  order, `submit-init.ts`'s category union and `bin.ts` are untouched until
  the registration wave.
- **Reserved ids are expected, not promised:** the test-harness constant
  `00000015` records the reserved emulator id; the production id is written
  only by the registration wave.
- **Explicit-category removal (fb7c0217):** removal of the convicted block
  rides `RemoveFraudulentBlockExplicitCategory` — zero changes to
  `src/remove-fraudulent-block.ts`.
- **Fraud-proof token is permanent by design:** the state-queue node NFT
  burns at removal; the fraud-proof token survives at its out-ref.

## 1. The fault predicate and its scope boundary

A block header `h` commits its withdrawal source set as the counted
`withdrawals_root`/`withdrawal_count` pair with
`(WithdrawalId, WithdrawalInfo)` leaves. **DoubleWithdraw** holds iff the
counted root commits two leaves `(id_1, info_1)`, `(id_2, info_2)` with:

1. `id_1 != id_2` — two distinct withdrawal events;
2. `info_1.body.l2_outref == info_2.body.l2_outref` — draining the same L2
   UTxO; and
3. `info_1.validity == WithdrawalIsValid == info_2.validity` — **both
   payable** (the D1 refinement).

Such a block pays the same L2 funds out twice at settlement — fund theft.

Scope boundary (recorded per the wave brief):

- **Same L1 event applied in two blocks** — the same `WithdrawalId` leaf
  committed by two different headers — is the
  **cross-block-duplicate-event** family's rule (built separately in this
  wave). This family never inspects a second header.
- **Cross-block distinct-id double drain** — block `B2` paying a fresh
  withdrawal id against an L2 UTxO that block `B1` already drained — is
  trace territory: `B2`'s `ValidWithdrawalTransition` cannot hold over a
  spent L2 UTxO, so the transition-trace/withdrawal-mistag families
  adjudicate it. This family is **same-block only**.
- A single leaf presented twice by the prover is not a fault: the identity
  inequality is enforced on chain (step-02) and mirrored offchain.

## 2. On-chain step chain

Two steps — leaf evidence is direct (full `WithdrawalInfo` rides the
redeemer's `RootMembershipProof`; no §8.8 field openings), following the
check+finalize terminal precedent of `invalid-range`/`zero-input`:

- `validators/fraud-proofs/double-withdraw/step-01.ak`
  (`fraud_proofs/double_withdraw/step_01.main.spend`), params
  `[step_02_validator_script_hash, computation_thread_token_policy_id, hub_oracle]`.
  Continue: generic `common.continue`; header authentication (hub →
  state-queue node whose key equals the thread NFT's 28-byte suffix); first
  leaf membership via the canonical
  `transition_trace.verify_root_membership_with_bytes` walk under
  `WithdrawalsRootDomain`; **entry condition** `validity == WithdrawalIsValid`;
  forwards fixed-size state
  `{challenged_header_hash, first_withdrawal_id, first_l2_outref}`.
- `validators/fraud-proofs/double-withdraw/step-02.ak` (terminal), params
  `[fraud_proof_token_policy_id, fraud_proof_token_address, computation_thread_token_policy_id, hub_oracle]`.
  Continue: generic `common.finalize` (burns the thread NFT, mints the
  permanent fraud-proof token under the same asset name); re-authenticates
  the header (node key == asset-name suffix == carried
  `challenged_header_hash`); second leaf membership via the same walk;
  decisive predicate `double_withdraw_fault_is_established_v1`:
  `id_2 != first_withdrawal_id ∧ l2_outref_2 == first_l2_outref ∧
validity_2 == WithdrawalIsValid`.
- `ct.Cancel` at both steps (prover-signed burn).

Binding is exclusively the existing canonical counted-root path; no new
binding machinery. There is **no on-chain category-id pin** — like
`native-script-decoding` (the newest owner-reviewed family), category
binding is Init's catalogue-membership proof of the step-01 hash; the older
contingent-pin style (`fabricated-withdrawal`'s `#"0000000c"`) is not
copied (§10 D3).

The Aiken fixtures commit a genuine **two-leaf** withdrawals MPF with real
`Leaf` neighbor proof steps (`double_withdraw_block_v1`); the second
`mpf.insert`'s own `excluding == root` assertion validates the
construction, and both membership witnesses are verified by the real
on-chain walk in the selectors.

## 3. Evidence model per step

| Step | Prover-supplied evidence                                              | Authenticated against                                                                                     | Forwarded                                                                                                                     |
| ---- | --------------------------------------------------------------------- | --------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------- |
| Init | catalogue membership proof of step-01 hash; header hash in asset name | fraud-proof catalogue root; state-queue node ref input                                                    | `StepDatum { fraud_prover, data: None }`                                                                                      |
| 01   | `RootMembershipProof<WithdrawalId, WithdrawalInfo>` for leaf 1        | header's counted `withdrawals_root`/`withdrawal_count` out of the hub → state-queue node the thread names | `{challenged_header_hash, first_withdrawal_id, first_l2_outref}` (all fixed-size — no withdrawer-chosen sizes in any handoff) |
| 02   | `RootMembershipProof` for leaf 2                                      | same counted-root walk, header re-authenticated                                                           | permanent fraud-proof token (asset name = category id ‖ header hash)                                                          |

Leaf bytes bind in `serialiseData` (definite-map) form on both planes:
on-chain via `cbor.serialise`, offchain via
`committedWithdrawalKeyBytesV1`/`committedWithdrawalValueBytesV1`
(canonicality is asserted fail-closed in the prepare module, exactly as
`prepare-fabricated-withdrawal` does).

## 4. Registration surface (deferred) and reserved test id

- Reserved emulator-test category id: **`00000015`** (wave-assigned). It
  appears **only** in the emulator wiring
  (`DOUBLE_WITHDRAW_TEST_CATEGORY_ID_V1`, harness `extraCategories`
  sidecar), never in the SDK catalogue order, never in the deployment
  manifest, never on chain.
- At registration (parent-owned): append `doubleWithdraw` to
  `FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER`, extend `submit-init.ts`'s union
  and `bin.ts` verbs, publish both steps as reference scripts, and add the
  removal manifest key `fraudProofDoubleWithdraw`.
- Reference scripts (owner ruling): both step validators publish as plain
  reference-script UTxOs; submitters accept the published UTxO and verify
  the carried hash before building (decoding-family idiom,
  `requireNativeScriptDecodingReferenceScriptV1` mirrored family-locally);
  the emulator lifecycle drives the reference-script path, and removal runs
  `requireReferenceScripts: true`.

## 5. New offchain modules

| Module                                                                            | Role                                                                                                                                                                                                                                      |
| --------------------------------------------------------------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `demo/midgard-sdk/src/fraud-proof/double-withdraw-v1.ts`                          | codec/rule twin: step datum/redeemer schemas, `DoubleWithdrawStep02State`, state builder, `isDoubleWithdrawFaultV1`, canonical leaf-byte helpers re-exported from the shared withdrawal helpers                                           |
| `demo/midgard-fault-proofs/src/prepare-double-withdraw.ts`                        | evidence builder: authenticates committed leaves against the header's counted root **and** count, finds/validates the payable duplicate pair, emits both membership proofs + the exact step-02 state; deterministic value-free rejections |
| `demo/midgard-fault-proofs/src/double-withdraw/contracts-v1.ts`                   | explicit pre-registration contracts record (no `categoryId` field; blueprint titles pinned)                                                                                                                                               |
| `demo/midgard-fault-proofs/src/double-withdraw/submit-common-v1.ts`               | thread-UTxO/step-datum guards + reference-script guard                                                                                                                                                                                    |
| `demo/midgard-fault-proofs/src/double-withdraw/submit-double-withdraw-step-01.ts` | step-01 submitter: re-derives the counted root from the **on-chain** header, serialiseData-form checks, `BuildTxWithRedeemer` layout, reference-script sourcing with inline fallback                                                      |
| `demo/midgard-fault-proofs/src/double-withdraw/submit-double-withdraw-step-02.ts` | finalize submitter: local twin of both last-chance checks (identity + established fault) refuses before it builds; thread burn + fraud-proof mint                                                                                         |
| `demo/midgard-fault-proofs/src/double-withdraw/index.ts`                          | family barrel (reached by direct import; the package barrel is untouched until registration)                                                                                                                                              |

No CLI verbs in `bin.ts`; no catalogue entry; ids never routed through the
deployment manifest.

## 6. Detection

A watcher recognises the fault from the retained-DA `withdrawals` entries
of a committed header alone: group the decoded leaves by
`body.l2_outref`, and report any group holding two or more
`WithdrawalIsValid` leaves with distinct ids. `prepare-double-withdraw`
re-derives `commit_counted_root(WithdrawalsRootDomain, phas_root, count)`
and requires equality with the on-chain header before it trusts any leaf,
so detection inputs are exactly as trustworthy as the header. Scan-plan
integration into the decoding-style prover adapters is registration-wave
work (§10 D6).

## 7. Economics, cancel, corners

- Two L1 transactions past Init; each handoff is fixed-size, so thread
  costs never scale with withdrawer-chosen `l2_value`/`l1_datum` sizes
  (the same griefing-lever argument as fabricated-withdrawal's info-hash).
- `ct.Cancel` at both steps under the prover signature; resume is
  re-submission from the surviving thread UTxO (crash-resume is state-free:
  everything the submitters need is re-derivable from chain + DA).
- Corners recorded:
  - **Same leaf twice** — refused at step-02's identity inequality (and
    locally by the submitter's twin).
  - **Honest duplicate (second leaf tagged non-payable)** — refused at
    step-01 entry (if bound first) and at step-02's decisive predicate;
    this is the adversarial-polarity emulator scenario.
  - **`withdrawal_count` restatement** — impossible: the count is hashed
    into the counted root.
  - **Order-of-leaves** — the pair is unordered; either leaf may enter at
    step-01 (positive selector covers both).
  - **Three-plus duplicates** — any payable pair convicts; the prover
    picks one pair.

## 8. Testing

All suites run green before this change is done; new per-family test files
only (uplc wasm heap — vitest isolates per file); blueprint read from
`onchain/aiken/plutus.json` (patched fork build), never rebuilt by tests;
`compiledCode` never named in test text (#610 gate).

1. **Aiken selectors** (in the two validator files): positive conviction
   (both leaf orders), non-payable entry refusal, forged root, wrong count,
   wrong header identity, same-leaf-twice, distinct outrefs, honest
   duplicate, wrong source identity, signed/unsigned cancel — each negative
   verified to fail at the exact intended `expect` (16 tests).
2. **SDK twin suite** (`demo/midgard-sdk/tests/double-withdraw-v1.test.ts`):
   schema/byte parity and the rule twin's truth table.
3. **Emulator direction A — real fault**
   (`demo/midgard-fault-proofs/tests/submit-init-emulator-double-withdraw.test.ts`):
   fraudulent two-payable-leaf commitment → Init under `00000015` (harness
   `extraCategories` sidecar) → step-01 → step-02 → fraud-proof token
   minted at the always-fails address → explicit-category removal
   (`RemoveFraudulentBlockExplicitCategory`, reference scripts required):
   state-queue node NFT burned, operator slashed/removed, fraud-proof token
   retained at its out-ref, second removal refused.
4. **Emulator adversarial polarity** (same file set): honest commitment
   (same outref, duplicate tagged `SpentWithdrawalUtxo`) — the honest
   submitter refuses locally (fail-closed regex) **and** a raw
   guard-bypassing builder is refused on-chain at the exact check
   (`expectOnchainRefusalV1`); same-leaf-twice adversary likewise refused
   at step-02; negative controls (wrong thread step, foreign signer).
5. **Cancel/resume controls**: prover cancel burns the thread; outsider
   cancel refused.

## 9. Sequencing

On-chain family (done, 16/16) → blueprint build (patched fork) → SDK twin →
prepare + submitters → emulator wiring (`contracts.ts` chain builder riding
double-spend's shared thread/fraud-proof policies; harness id constant +
`extraCategories`; removal-deployment entry) → emulator suites →
catalogue-status rows. Trailing-slash module filters
(`-m 'double_withdraw/'`) with non-zero-count confirmation, per the
standing trap note.

## 10. Decision register

- **D1 — both-payable refinement (FLAGGED, owner nod owed).** See the
  preamble. Chosen over the bare spec predicate because the bare predicate
  convicts honest operators that lawfully include and invalid-tag
  duplicate due events; over an "either-invalid ⇒ different family"
  routing because payability is the exact settlement-payout condition.
- **D2 — reference scripts.** Standing owner ruling; inline fallback kept
  only as the decoding-family idiom's local-attach path for harness setup
  steps that predate publication.
- **D3 — no on-chain category pin.** `native-script-decoding` precedent;
  Init's catalogue membership is the binding. The contingent-pin style is
  legacy.
- **D4 — 2-step chain.** Leaf evidence is direct; the check+finalize
  terminal follows `invalid-range`/`zero-input`. A 4-step shape would add
  two content-free hops and two more L1 transactions of prover cost.
- **D5 — state carries `first_l2_outref`, not an info hash.** The decisive
  comparison needs only the outref (fixed 34-ish bytes) and the identity;
  hashing the whole `WithdrawalInfo` (fabricated-withdrawal's need) buys
  nothing here since content fidelity is not adjudicated.
- **D6 — watcher/scan-plan adapters deferred to registration.** Detection
  core lands in `prepare-double-withdraw`; wiring it into the decoding
  prover-adapter surface is parent-owned integration.
- **D7 — test id `00000015`.** Wave-assigned; emulator-only.

## 11. Out of scope

Catalogue registration and production category id; `bin.ts` verbs; spec
erratum text for D1 (owner-owned); cross-block duplicate-event and
cross-block distinct-id coverage (other families); DA-payload watcher
service integration.
