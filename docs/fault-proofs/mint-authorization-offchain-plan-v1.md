# Mint-authorization offchain plan v1

Status: plan + on-chain family landed together (this wave); offchain builders
and emulator suites specified here and delivered in the same tree.
Category id **`0000001b`** is **reserved** for this family (verified unclaimed
repo-wide; assigned at catalogue registration, present today only in emulator
test wiring — never in `bin.ts`, never in
`FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER`, never routed through the deployment
manifest's category map, which silently drops non-canonical keys).

## 0. The D-S3 reading this family builds on

`docs/fault-proofs/execution-plan.md` D-S3 ("scope of mint/burn in the
consensus language") is formally OPEN. This family takes the most
canonical-V1-consistent reading and proceeds:

> Mint and burn remain in the consensus language. Canonical V1 must
> authenticate the mint field, enforce exact multi-asset conservation, and
> verify the corresponding native/Plutus policy authorization on L1.

Consequences adopted here:

- **Mint/burn are in scope** for canonical V1 and therefore adjudicable.
- **The native-policy leg is single-party adjudicable** (this family):
  the machine's stage-nine authorization of a native policy is a total,
  deterministic function of committed data (script bytes, field-7 signer
  frontier, body validity interval), so a step thread can reproduce it
  without any interactive session.
- **The Plutus (CEK) leg stays machine/interactive territory.** A mint
  purpose whose policy is a Plutus script is not convictable by this family
  in either direction: direction B pins a *native* payload by versioned
  hash (the `0x00` language tag is inside the hash preimage, so a Plutus
  script can never satisfy the pin), and direction A's claim — *no source
  of the hash at all* — is language-agnostic and remains sound.
- **ADA is structurally unmintable** (structural-na-q24): the §5.6 field-5
  grammar has no representation for the ADA policy — every item carries a
  28-byte non-ADA policy id and a non-empty map of non-zero quantities.
  There is no "mints ADA" fault to prove; the corner is closed by grammar,
  not by a check.

If D-S3 is later ruled the other way (mint outside consensus language), the
family's on-chain steps are inert (nothing registers the category) and this
plan is withdrawn; no other family depends on it.

## 1. The contract the builders must satisfy

Family: `onchain/aiken/validators/fraud-proofs/mint-authorization/`
(five steps), lib wire shapes + engine under
`onchain/aiken/lib/midgard/fraud-proofs/mint-authorization/`.

**Claim.** An operator-ACCEPTED committed L2 native-V1 transaction carries a
field-5 (mint) item for policy `H` — mint or burn, sign-agnostic — and `H`'s
authorization was not satisfied, in exactly one of two directions:

- **Direction A (`0`, script absent):** no script source with versioned
  hash `H` existed anywhere on the machine's stage-nine source surface —
  neither among the field-6 script witnesses (any language) nor among the
  reference scripts of the resolved reference inputs.
- **Direction B (`1`, script unsatisfied):** the native script with
  versioned hash `H` evaluates to `False` against the transaction's
  committed field-7 signer frontier and body validity interval, by the
  machine's own leaf/container semantics and limits.

### Step chain

| step | redeemer arm(s) | proves | state out |
| --- | --- | --- | --- |
| 01 Bind | `Continue{carriage}` | tx ∈ counted `transactions_root`; **`validity_code == 0`** | `{bad_tx_id, bad_tx_witness_set_hash, validity_interval_start, validity_interval_end}` |
| 02 Claim | `Continue{header, memberships, policy_index, direction, mint_opening}` | header ↔ thread NFT; committed pre-state root (`verify_committed_pre_state_v1`); policy id read off committed field-5 item (§5.6 walk); direction ∈ {0,1} | `{policy_id, direction, anchors…, prior_ledger_root}` |
| 03 Dispatch | `WitnessAbsence{script_tx_wits_opening}` | dir A: whole field-6 walk, no item's versioned hash == `policy_id` | → step-04 `{policy_id, bad_tx_id, prior_ledger_root, ref_cursor: 0}` |
| | `EvaluateUnsatisfied{script_bytes, addr_tx_wits_opening}` | dir B: `blake2b_224(0x00‖bytes) == policy_id`; field-7 frontier; machine-twin eval → `satisfied: False` | → step-05 `{policy_id, direction: 1}` |
| 04 Scan (self-loop) | `ResolveNext{reference_inputs_opening, descriptor_cbor, ledger_membership_proof}` | field-1 item at cursor → MPF member of `prior_ledger_root` → descriptor's ref-script is `-1` or hash ≠ `policy_id` | self-loop, `ref_cursor + 1` |
| | `AdvanceComplete{reference_inputs_opening}` | `ref_cursor == field_item_count(field 1)` | → step-05 `{policy_id, direction: 0}` |
| 05 Finalize | `Continue{fraud_proof_mint_redeemer_index}` | closed verdict shape; thread burn + permanent fraud-proof mint at `category_id ‖ header_hash` | — |

Every step also carries the standard `ct.Cancel` arm (prover signature).

### Soundness pillars (why the honest operator is safe)

1. **Acceptance binding (§2.4.3(d)).** Only step-01 may read
   `validity_code`, because the compact bytes it authenticates are the
   `transactions_root` leaf's exact bytes — the scalar is root-committed
   there and nowhere later (the §3 tx-id preimage is the body alone; see
   `field_opening_v1.unanchored_validity_code_of`). An honestly-recorded
   rejection (`validity_code != 0`) dies at step-01 and can never convict.
2. **Witness anchoring.** Every witness-set field walk (fields 6/7) is
   anchored to the *thread-carried* `bad_tx_witness_set_hash` read off the
   block-committed compact structure at step-01 — the both-sides-forgery
   (genuine body + invented witness tail) is refused by an anchor no
   redeemer can restate.
3. **Direction-A surface completeness.** The machine's
   `replay_source_frontier` for a mint purpose is exactly: field-6 script
   witnesses ∪ reference scripts of resolved field-1 outpoints (descriptor
   `reference_script_language != -1`), matched by hash alone across
   languages. Spend inputs and own outputs contribute nothing. Step-03
   walks all of field 6 (fold asserts completion); step-04 resolves every
   field-1 ordinal `0..count` against the same `pre_utxos_root` the machine
   used (deterministic trie lookup ⇒ the very bytes the machine read) and
   `AdvanceComplete` demands cursor == authenticated count. No gap.
4. **Direction-B needs no source-membership proof.** The hash pin
   `blake2b_224(0x00 ‖ script_bytes) == H` plus collision resistance means:
   if the machine consulted any source with hash `H`, its bytes were these
   bytes (a Plutus source cannot collide across the tag byte), and its
   deterministic evaluation is reproduced by the machine-twin evaluator; if
   it consulted none, acceptance was wrongful for the stronger
   missing-witness reason. Either way the conviction is sound, and an
   honest operator (who only accepts satisfied policies over existing
   sources) can never be caught.
5. **Machine-twin evaluation exactness.** The engine mirrors
   `validation-machine-v1` stage-nine leaf predicates verbatim: sig ⟺
   key_hash ∈ blake2b_224-of-every-field-7-vkey frontier; `after`:
   `start >= 0 && start >= slot`; `before`: `end >= 0 && end <= slot`;
   containers via `apply_child_v1`; empty containers via
   `empty_container_result_v1`; scan-level well-formedness via
   `token_at_v1`; node/depth budgets 16384/16384 (the machine's scan
   limits, NOT `native-script-v1.ak`'s 16/32). Any verdict other than
   `ScriptEvaluatedV1{satisfied: False}` — malformed, node budget, depth
   budget — **refuses** rather than convicts: those payloads are the
   native-script-decoding family's claims (state boundary).
6. **The claim names one specific policy.** `policy_id` is *read* off the
   committed field-5 item at the accused ordinal through the §8.8 door —
   the prover picks an ordinal, never a hash. Wrong-policy accusations are
   unrepresentable; zero-net entries are refused by the §5.6 walk
   (`quantity != 0`); burns convict identically (purpose derivation is
   sign-agnostic).
7. **Step-04 deliberately omits the
   `descriptor.output_index == outpoint.output_index` cross-check.**
   Absence must hold over what the machine consulted — the trie's answer
   for the key, consistent or not. Refusing an inconsistent committed
   descriptor would strand a genuine conviction on a fault that belongs to
   a ledger-construction family. A descriptor failing canonical `decode`
   aborts (refusal, never wrongful conviction) — decoding boundary.

On-chain selectors: 62/62 green under
`aiken check -m 'mint_authorization/'` (module filter with trailing slash —
a bare `-m` string is a vacuous test-name filter).

## 2. Evidence model — what the prover must derive

Per step, from DA payload + committed header (all reconstruction via
`reconstructDaPayloadV1`, with the dual-`transactions_root` twin-header
dance the decoding fixture uses):

- **step-01:** `NativeTxInclusionCarriage` — compact leaf bytes + counted
  transactions-root membership (`keyValuePhasProof` over
  `tx_id → compact_cbor`), exactly the decoding bind step's carriage.
- **step-02:** the committed `HeaderV1`; `L2TransactionEventKey{tx_id}`;
  event-to-step membership + transition-step membership (from
  `src/transition-trace/witnesses.ts` builders, as wrapped by
  `buildNativeScriptDecodingStep02EvidenceV1` — reuse the same wrapper
  shape); the accused `policy_index`; a tier-1 (inline) field-5 opening
  built from `encodeMidgardFieldItemsV1({fieldIndex: 5, …})`.
- **step-03 A:** tier-1 field-6 opening (whole preimage,
  `encodeMidgardVersionedScript` per item).
- **step-03 B:** the policy's native `script_bytes` (from the prover's own
  knowledge of the script — typically the very script the L2 wallet
  holds); tier-1 field-7 opening (`encodeMidgardAddressWitnessItemV1`).
- **step-04:** tier-1 field-1 opening; per ordinal: the ledger descriptor
  bytes (`buildMidgardLedgerOutputMaterialV1(...).descriptorCbor`) and an
  MPF membership proof under `pre_utxos_root`
  (`@aiken-lang/merkle-patricia-forestry` trie, key
  `nativeScriptDecodingOutpointKeyV1`-style 38-byte outpoint encoding).
- **step-05:** indices only.

**Prover-side scan (finding the fault).** For each accepted (validity 0)
committed tx with non-empty mint: for each field-5 item `H` — collect the
source surface (field-6 versioned hashes ∪ resolved reference-script
hashes); if `H` absent → direction-A finding; if present as native bytes
`P` → evaluate `P` against the field-7 frontier and validity interval with
a TS twin of the engine; `False` → direction-B finding; Plutus source or
undecodable payload → out of family (machine/decoding territory), skip.

## 3. Module inventory (`demo/midgard-fault-proofs/src/mint-authorization/`)

Modeled 1:1 on `src/native-script-decoding/` (the pre-registration
exemplar; the fabricated-deposit `categoryId`-in-contracts shape is
explicitly *not* followed):

- `contracts-v1.ts` — `MINT_AUTHORIZATION_CATEGORY_LABEL`,
  `MINT_AUTHORIZATION_BLUEPRINT_TITLES_V1` (`fraud_proofs/mint_authorization/step_0N.main.spend`),
  `MintAuthorizationContractsV1` (5 steps; **no categoryId field** — id
  assigned at registration), `buildMintAuthorizationChainV1` applying
  parameters backwards (step-05 first; step-03 takes both step-04 and
  step-05 hashes).
- `submit-common-v1.ts` — error helper, step labels,
  `requireMintAuthorizationThreadUtxoV1`,
  `requireMintAuthorizationReferenceScriptV1`,
  `requireMintAuthorizationStepStateV1`, the
  `MintAuthorizationCatalogueCategoryV1` record (`categoryId`,
  `scriptHash`, `membershipProofCbor`).
- `evidence-v1.ts` — step-02 evidence wrapper (header hash check +
  memberships + `pre_utxos_root` extraction), field-opening builders for
  fields 1/5/6/7, outpoint keys, ledger-trie handle type.
- `prover-v1.ts` — the §2 scan over a reconstructed block; returns
  `MintAuthorizationFindingV1 { txId, policyIndex, policyId, direction, … }`.
- `submit-mint-authorization-init.ts` — decoding-init clone: category
  guard, catalogue + hub-oracle + fraudulent-block reference inputs, PHAS
  membership withdraw, thread NFT mint at
  `categoryId ‖ headerHash`, first-step datum `{fraud_prover, data: null}`.
- `submit-mint-authorization-step-01..05.ts` — one submitter per step
  (step-03 exposes both arms; step-04 exposes `resolveNext` /
  `advanceComplete`), each with optional `referenceScriptUtxo` routed
  through the require-reference-script helper. **Fault proofs deploy as
  reference scripts, never inline** (owner ruling 2026-08-26) — the
  emulator publishes all five via `publishPlainReferenceScriptUtxo`.
- `cancel.ts`, `index.ts` barrel; one `export *` line added to
  `src/index.ts`.

**No CLI verbs in `bin.ts`. No entry in
`FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER`.** The id `0000001b` appears only in
`tests/support/emulator/harness.ts`
(`MINT_AUTHORIZATION_TEST_CATEGORY_ID_V1`) and flows through
`buildCatalogueDeploymentInfo`'s `extraCategories` (guaranteed not to move
any measured fixture) and the explicit
`RemoveFraudulentBlockExplicitCategory` record
(`MINT_AUTHORIZATION_REMOVAL_DEPLOYMENT_ENTRY_V1 = "fraudProofMintAuthorization"`,
pinning the step-01 hash through its own manifest entry, outside the
category map).

## 4. Submit-step walkthrough

Every step tx: spend thread UTxO (thread NFT `categoryId ‖ headerHash`) at
step-N address via reference script → produce continuation at step-N+1
address (step-04 `ResolveNext`: same address) with inline
`ct.StepDatum{fraud_prover, data: Some(state)}`; `addSignerKey(prover)`;
`localUPLCEval: true`. Field openings ride tier-1 (inline carriage in the
redeemer). Step-01 consumes the state-queue block as reference input via
the carriage; step-02 needs no L1 reference beyond the thread; step-05 is
the finalize tx: burn thread NFT (`{Success: {burning_token_asset_name}}`),
mint fraud-proof token (same asset name), pay to
`contracts.fraudProof.spendingScriptAddress` with the fraud-proof datum.
Removal then rides the category-agnostic `submitRemoveFraudulentBlock` with
the explicit category record — zero src changes (post-`fb7c0217`).

## 5. Emulator test plan (owner directive 2026-08-25: both polarities)

Support module `tests/support/mint-authorization-emulator-v1.ts`
(decoding-support clone): harness wrapper
(`contractOptions: { realMintAuthorization: true, alwaysFraudProofCatalogue: true }`),
scenario builder fabricating a block whose subject tx **populates field 5**
(`encodeMidgardFieldPreimageForFieldV1({fieldIndex: 5, …})` — first fixture
in the repo to do so), ledger trie with a controllable resolved-output
descriptor, reference-script publication for all five steps, raw
guard-bypassing step submitters, `expectOnchainRefusalV1`
(`/failed script execution/u` — an offchain builder error must never read
as a passing security assertion).

- **`submit-init-emulator-mint-authorization-direction-a.test.ts`**
  1. End-to-end journey: init → steps 01–03(A) → 04 `ResolveNext` ×1 →
     04 `AdvanceComplete` → 05 finalize → fraud-proof token minted →
     **fraudulent-block removal**: state-queue node NFT burned, root
     `next === "Empty"`, operator slashed off, scheduler
     `NoActiveOperators`, **fraud-proof token retained** (permanent by
     design — never assert its burn), second removal rejects.
  2. Adversarial polarity: same block but the policy script IS present
     inline — raw step-03 `WitnessAbsence` refused at
     `policy_script_is_present == False`; present as a resolved reference
     script — raw step-04 `ResolveNext` refused at the cleared-source
     predicate.
  3. Negative controls: honestly-rejected tx (validity 1) refused at
     step-01's acceptance gate; substituted field-5 preimage refused at
     the door.
- **`submit-init-emulator-mint-authorization-direction-b.test.ts`**
  1. Through the decisive step: init → 01 → 02 → 03 `EvaluateUnsatisfied`
     (unsigned `sig` policy) succeeds → (optionally 05).
  2. Adversarial: satisfied policy (signer present) — raw step-03 refused
     at the evaluator expect; wrong-hash script bytes refused at the pin;
     malformed payload (tag 7) refused (decoding boundary).

Timeouts inline (`}, 600_000)`); one file per direction (uplc wasm heap —
`pool: "forks"`, never `--no-isolate`).

## 6. Sizing

Step bodies are of the same order as the decoding family's (the heaviest
arm, step-03 B, carries the scan-twin evaluator ≈ decoding's step-03
machinery). All five deploy as reference scripts, so the 16,384-byte
envelope is not a constraint on any step; tier-1 openings bound redeemer
size by the subject tx's witness fields (test subjects are tiny). Measured
applied sizes to be recorded from the built blueprint at registration time.

## 7. Corner-case ledger

| corner | disposition | where enforced |
| --- | --- | --- |
| burn (negative qty) | **convicts** — purpose is sign-agnostic | §5.6 walk accepts; engine test `reads_a_burn_item` |
| zero-net entry | **refuses** — outside §5.6 grammar / machine purpose domain | `quantity != 0` in the item walk; step-02 selector |
| empty mint | refuses (no item to accuse) | authenticated `field_item_count` bound |
| ADA | structurally unmintable (structural-na-q24) | §5.6 grammar; no check needed |
| satisfied policy (any combinator: sig/all/any/at_least/after/before) | refuses at the evaluator expect | step-03 B selectors, one per class |
| malformed / over-budget script payload | refuses — decoding family's claim | verdict match on `ScriptEvaluatedV1` only |
| honestly-rejected tx | never enters the thread | step-01 `validity_code == 0` |
| Plutus policy | out of family (D-S3 machine leg) | hash pin (B); language-agnostic absence stays sound (A) |
| wrong-policy accusation | unrepresentable — id is read, not asserted | step-02 committed-item read |
| inconsistent committed descriptor | dir-A clears on the hash predicate regardless; undecodable bytes strand (refuse) | step-04 §1 pillar 7 |

## 8. Decision register

| # | decision | status |
| --- | --- | --- |
| Q1 | Category id `0000001b`, pre-registration only | DECIDED (reserved by this doc) |
| Q2 | Two directions in one family, selected at step-02 | DECIDED |
| Q3 | Direction B without source-membership proof (hash-pin argument) | DECIDED (§1 pillar 4) |
| Q4 | Machine limits 16384/16384, not native-script-v1's 16/32 | DECIDED (twin exactness) |
| Q5 | No `output_index` cross-check in the scan | DECIDED (§1 pillar 7) |
| Q6 | Reference-script deployment for all five steps | DECIDED (owner ruling) |
| Q7 | D-S3 reading | RECORDED §0 — formally still open at the program level |
| Q8 | Registration (catalogue append + CLI verbs + category order) | DEFERRED to owner, as for decoding/missing-signature |
