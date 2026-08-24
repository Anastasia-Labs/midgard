# Rejection-Reason Catalogue V1 — the full space of operator rejection verdicts on forced-inclusion transactions

> Audited 2026-08-24 against branch `wave/lane-o` (base `c75fb3bf`), from the
> staged validation machine itself. Companion docs:
> [`architecture.md`](architecture.md) (§2 proof-interaction classification),
> [`native-script-decoding-fault-thread-design-v1.md`](native-script-decoding-fault-thread-design-v1.md)
> (the standalone scan thread this catalogue's scan-borne arms delegate to; all
> citations to it here are to the committed `c75fb3bf` revision).
>
> Method: every claim below is grounded in an emission site read in
> `onchain/aiken/lib/midgard/validation-machine-v1.ak` (19,171 lines at this
> audit) or its companion libraries; anything not verified is marked OPEN.
> Line anchors are to the audited revision.

## 0. Scope and governing frame

Midgard blocks commit an operator verdict on every forced-inclusion L2
transaction. The frame this catalogue serves (owner rulings through
2026-08-24):

- **Rejection verdicts exist only for forced transactions.** The claim layer
  pins Normal-source descriptors to `Accepted` outright
  (`onchain/aiken/lib/midgard/validation-claim-v1.ak:311-316`), while a
  Forced-source descriptor's verdict must match the operator's recorded
  `operator_validity` through `forced_verdict_matches`
  (`validation-claim-v1.ak:204-213`, applied at `:302-310`). The forced leaf is
  `ForcedInclusionTxV1 { tx_id, source, operator_validity }`
  (`onchain/aiken/lib/midgard/ledger-state.ak:541-545`), committed under
  `header.forced_transactions_root` (`ledger-state.ak:62`).
- **Forced txs are full unrestricted L2 native txs**: nine field slots
  including `script_witnesses` (field 6) and `redeemers` (field 8)
  (`validation-machine-v1.ak:1222-1258`; `docs/spec/midgard-tx.md` §2.5), and
  they can run Plutus scripts under CEK (machine phases `ScriptIntegrity`,
  `Cek`).
- **The machine's rejection channel today** is a 32-byte code hashed into the
  terminal state: `rejected_successor_is_exact`
  (`validation-machine-v1.ak:2237-2270`) pins the rejecting terminal's
  `rejection_code_hash` to `hash_rejection_code(code)`
  (`onchain/aiken/lib/midgard/validation-trace-v1.ak:239-243`) and its
  `work_root` to `encode_terminal_rejection_witness(code, prior_ledger_root)`
  (`validation-machine-v1.ak:1175-1184`). Nothing else about the rejection —
  in particular **no subject coordinate** — survives to the descriptor. The
  committed scan-thread design already records this as its OPEN (B-1)
  ("no which-outpoint commitment", design doc §2.2 at `c75fb3bf`).
- **The target shape** (future format-revision wave): the forced leaf carries
  `verdict: OperatorVerdictV1 = ForcedTxValid | ForcedTxInvalid { reason: RejectionReasonV1 }`
  where `RejectionReasonV1` is a fully enumerated sum type — the constructor
  tag *is* the code, and each arm carries only the subject identifiers a
  single-party refuter needs ("name the subject, never carry the argument").
- **Refutability discipline**: `architecture.md` §2 — single-party wherever
  the statement is decidable from retained public authenticated evidence;
  multi-transaction is fine, interactivity is not, except where sound
  resolution intrinsically needs competing execution traces. The known
  carve-out is CEK execution itself.

The machine has **19 distinct rejection-code constants** (all in
`validation-machine-v1.ak`; hex decodes shown once here):

| # | constant | E_* label | declared at |
|---|----------|-----------|-------------|
| 1 | `reject_field_preimage_size` | `E_FIELD_PREIMAGE_SIZE` | :1186 |
| 2 | `reject_asset_count` | `E_ASSET_COUNT` | :1188 |
| 3 | `reject_invalid_field_type` | `E_INVALID_FIELD_TYPE` | :1190 |
| 4 | `reject_native_script_depth` | `E_NATIVE_SCRIPT_DEPTH` | :1192 |
| 5 | `reject_native_script_node_count` | `E_NATIVE_SCRIPT_NODE_COUNT` | :1194-1195 |
| 6 | `reject_min_ada` | `E_MIN_ADA` | :1205 |
| 7 | `reject_empty_inputs` | `E_EMPTY_INPUTS` | :2128 |
| 8 | `reject_duplicate_input` | `E_DUPLICATE_INPUT_IN_TX` | :2130 |
| 9 | `reject_network_id_mismatch` | `E_NETWORK_ID_MISMATCH` | :2132 |
| 10 | `reject_min_fee` | `E_MIN_FEE` | :2134 |
| 11 | `reject_invalid_validity_interval_format` | `E_INVALID_VALIDITY_INTERVAL_FORMAT` | :2277-2278 |
| 12 | `reject_missing_required_witness` | `E_MISSING_REQUIRED_WITNESS` | :2805-2806 |
| 13 | `reject_invalid_signature` | `E_INVALID_SIGNATURE` | :2808 |
| 14 | `reject_native_script_invalid` | `E_NATIVE_SCRIPT_INVALID` | :3450-3451 |
| 15 | `reject_plutus_script_invalid` | `E_PLUTUS_SCRIPT_INVALID` | :3453-3454 |
| 16 | `reject_validity_interval_mismatch` | `E_VALIDITY_INTERVAL_MISMATCH` | :6164-6165 |
| 17 | `reject_input_not_found` | `E_INPUT_NOT_FOUND` | :6167 |
| 18 | `reject_invalid_output` | `E_INVALID_OUTPUT` | :6169 |
| 19 | `reject_value_not_preserved` | `E_VALUE_NOT_PRESERVED` | :14338 |

The catalogue below derives the reason space from the **emission sites** (about
forty in the library, once semantic-entry-point duplicates of the same
condition are folded), not from this constant list.

## 1. Failure-space derivation, phase by phase

Phase order per `ValidationPhase`
(`onchain/aiken/lib/midgard/validation-trace-v1.ak:33-49`). "Subject space"
names the coordinate system of the thing the rejection is charged to. All line
anchors `validation-machine-v1.ak` unless stated.

### 1.1 CanonicalDecode

The phase walks all nine field preimages against the committed proof-source
triple (compact body, compact witness set, declared field preimage lengths —
authenticated by `verify_native_tx_proof_source_v1` against
`pre.transaction_id` / `pre.transaction_commitment`, `:1457-1461`).

| site | condition | subject space | code |
|------|-----------|---------------|------|
| `:1497-1502` (`verify_canonical_decode_empty`) | field commitment is the empty-field sentinel but the declared preimage length ≠ 1 (the bare `80` array header) | per-field (`field_index` 0–8) | `E_FIELD_PREIMAGE_SIZE` |
| `:1645-1650`, `:1957-1962` (`verify_canonical_decode_chunk` / `verify_canonical_decode_item_successor_v1`) | walk completes the last item and the accumulated canonical length ≠ the declared field preimage length | per-field | `E_FIELD_PREIMAGE_SIZE` |
| `:1620-1628`, `:1937-1942` (same two walkers, via `transaction_field_item_encoded_length` `:1296-1316`) | an item's width is not a legal encoding for its field — exactly two shapes: a zero-length field-5 mint item, or a field-2 output item above `max_serialized_output_preimage_bytes` = 16,384 (`:2275`) | per-(field, item) | `E_INVALID_FIELD_TYPE` |

The header comment at `:1260-1295` documents that these are the *only*
CanonicalDecode-reachable rejections: the six fixed-stride fields refuse a
wrong width "at the read itself", i.e. by making the step **unprovable**, not
by rejecting (see §5.2 on stall conditions).

### 1.2 CompactBinding

`verify_compact_binding` (`:2068-2126`): pure re-binding of the triple to the
machine state; always advances to StaticLedgerRules. **No rejection emissions.**

### 1.3 StaticLedgerRules

`static_rules_rejection` (`:2697-2712`), driven by `verify_static_rules`
(`:2714-2803`) against the validation context (block-derived: end time,
expected network id, fee parameters, block slot —
`decode_validation_context` `:2146-2177`, bound to the header by
`validation_context_is_exact`, `validation-claim-v1.ak:56-91`).

| site | condition | subject space | code |
|------|-----------|---------------|------|
| `:2702-2706` | `body.network_id != 255 && body.network_id != context.expected_network_id` | tx-global | `E_NETWORK_ID_MISMATCH` |
| `:2707-2708` | `body.fee < min_fee_a * canonical_tx_size + min_fee_b` (`native_tx_canonical_size_v1` over the declared lengths) | tx-global | `E_MIN_FEE` |

### 1.4 InputSets

Scans fields 0 (spend) and 1 (reference) in strictly descending out-ref key
order, building the resolution schedule.

| site | condition | subject space | code |
|------|-----------|---------------|------|
| `:2396-2406` (`verify_input_sets_empty`) | spend-inputs field commitment is the empty sentinel (`spend_count == 0` seeded at `:2771-2775`) | tx-global | `E_EMPTY_INPUTS` |
| `:2593-2622` (`verify_input_sets_item`) | the item's out-ref key equals the previously visited key (`duplicate` at `:2593-2596`) | per-(field, item) pair — the equal out-ref appears at two positions in the fields-0/1 union scan | `E_DUPLICATE_INPUT_IN_TX` |
| `:2443-2453` (`input_sets_item_successor_is_exact`, on scan completion) | `validity_interval_is_malformed` (`:2409-2422`): a bound below −1, or both bounds present with start > end | tx-global | `E_INVALID_VALIDITY_INTERVAL_FORMAT` |

Note the ordering conjunct `correctly_ordered` (`:2597-2600`, `:2625`) is a
**provability** condition on the prover's visitation order, not a property of
the transaction: any tx with distinct keys admits an ordered visitation, so
order violations never reject — duplicates do.

### 1.5 Signatures

Stage 0 scans field-7 address witnesses; stage 1 scans field-4 required
signers against the signer frontier accumulated in stage 0; stage 2 hands off.

| site | condition | subject space | code |
|------|-----------|---------------|------|
| `:3081-3092` (latch) → `:2999-3013` (emission, `signatures_after_required_successor_is_exact`) | some field-7 item's Ed25519 signature over `pre.transaction_id` fails verification; the machine latches `invalid_signature_seen = 1` at the **first** failing item in visitation order and emits the rejecting terminal only after the required-signer stage completes | per-witness (field-7 item) — but the terminal does **not** record which | `E_INVALID_SIGNATURE` |
| `:3255-3276` (`verify_required_signer_item`) | a field-4 required signer hash is proven absent from the stage-0 signer set (`required_signer_non_membership_is_valid`) | per-required-signer (field-4 item) | `E_MISSING_REQUIRED_WITNESS` |

The deferred-emission structure of `E_INVALID_SIGNATURE` matters for the
proposal: the machine's own rejecting run does not fix a subject, so the
verdict leaf must (§6, arm `AddressWitnessSignatureInvalid`).

### 1.6 PhaseANativeScripts

Structural scan **and** Phase-A evaluation of every field-6 script-witness
item; tag-0 (NativeCardano) items are token-scanned; tags 3/128 are skipped
here (marked `contains_non_native_script`). The same phase serves double duty:
with a non-empty `continuation_cbor` it is a **Phase-B per-execution**
evaluation re-entered from the NativeScripts phase
(`phase_a_native_complete_script_is_exact` `:3746-3786`;
`verify_native_execution_descriptor_step` `:13483-13530`), evaluating a native
script that may originate from a **resolved reference script**, not the
witness set.

| site | condition | subject space | code |
|------|-----------|---------------|------|
| `:3877-3883` (`verify_phase_a_native_item_scan`) | field-6 item's versioned-script header undecodable (`versioned_script_header_v1` = None; legal language tags are exactly {0, 3, 128}, `docs/spec/midgard-tx.md` §5.3) | per-script (field-6 item) | `E_INVALID_FIELD_TYPE` |
| `:4037-4042` (legacy whole-token scan), `:4524-4536` (token head), `:4725-4730`, `:4758-4763`, `:4791-4796`, `:4821-4829` (container payloads), `:4948-4972` (container token), `:5086-5091`, `:5105-5110`, `:5158-5162` (timelock token), `:5188-5209`, `:5255-5259` (signature token), `:5297-5302` (timelock payload), `:5367-5372` (signature payload) | a native-script token/payload at the cursor is undecodable or ill-formed | per-script × byte-cursor (deterministic scan position) | `E_INVALID_FIELD_TYPE` |
| `:4240-4245` (`verify_phase_a_native_finalize_scan`) | scan ended (`stack_depth == 0`) with `cursor != item_length` — trailing bytes after the script term | per-script | `E_INVALID_FIELD_TYPE` |
| `:4044-4050`, `:4540-4545`, `:4846-4852`, `:4895-4901`, `:5112-5118`, `:5211-5217` | node count would exceed `max_native_script_nodes` = 16,384 (`native-script-scan-v1.ak:24`) | per-script | `E_NATIVE_SCRIPT_NODE_COUNT` |
| `:4064-4070`, `:4581-4586`, `:4862-4867` | stack depth would exceed `max_native_script_depth` = 16,384 (`native-script-scan-v1.ak:26`) | per-script | `E_NATIVE_SCRIPT_DEPTH` |
| `:4246-4251` (`verify_phase_a_native_finalize_scan`) | the native script **evaluated false** (`result == 0`) against the tx's signer set and validity interval (signature nodes: `phase_a_native_signature_result` `:3961-3996`; timelock nodes: `:5120-5130`) | per-script (Phase-A mode) **or** per-execution (Phase-B continuation mode — the same site, mode split at `:3754`) | `E_NATIVE_SCRIPT_INVALID` |

All of these therefore have **two subject spaces**: field-6 item ordinal
(Phase-A mode, `continuation_cbor == ""`) and execution ordinal (Phase-B mode).
See §3.

### 1.7 PhaseAScriptPreconditions

`phase_a_script_preconditions_rejection` (`:5803-5827`) plus a field-3
observer-order scan.

| site | condition | subject space | code |
|------|-----------|---------------|------|
| `:5815-5818` | Plutus evaluation is required (a non-native witness script, or redeemers present, or a non-zero `script_integrity_hash`) but `script_integrity_hash` **is** the 32-byte zero sentinel | tx-global | `E_INVALID_FIELD_TYPE` |
| `:5819-5823` | required observers present (`observer_count > 0`) while `network_id == 255` (the untagged sentinel) | tx-global | `E_INVALID_FIELD_TYPE` |
| `:6084-6092` | field-3 observer items not strictly ascending (duplicate or misordered 28-byte observer hash) | per-observer (field-3 item) | `E_INVALID_FIELD_TYPE` |

### 1.8 ResolveInputs

Resolves every scheduled input against `pre.prior_ledger_root` (the transition
step's `pre_utxos_root`, bound at `validation-claim-v1.ak:407`) and runs the
full ledger-output canonicity scan over each resolved output
(`ledger_output_proof_v1.step_v1`).

| site | condition | subject space | code |
|------|-----------|---------------|------|
| `:6740-6746`, `:6840-6846` (cursor-0 step) | `validity_interval_contains_slot(body, context.block_slot)` is false — the block's slot lies outside the tx's validity interval | tx-global (vs block context) | `E_VALIDITY_INTERVAL_MISMATCH` |
| `:6553-6561` (`resolve_non_membership_step`) | MPF non-membership: the scheduled out-ref key is **not** in the prior ledger root | per-scheduled-input (source kind + ordinal) | `E_INPUT_NOT_FOUND` |
| `:6458-6463` (`resolve_membership_proof_step`) | the resolved output's committed bytes fail the output canonicity scan (`LedgerOutputProofInvalidOutput`, produced when `ledger_output_scan_v1.step_v1` refuses — `ledger-output-proof-v1.ak:672`) | per-scheduled-input | `E_INVALID_OUTPUT` |
| `:6464-6469` | the resolved output's tag-0 reference script fails structural canonicity (`LedgerOutputProofInvalidReferenceScript`: scan-invalid `ledger-output-proof-v1.ak:610`, or a tag-0 reference script of length 0, `:899`) | per-scheduled-input | `E_INVALID_FIELD_TYPE` |
| `:6470-6475` / `:6476-6481` | that reference-script scan hits the node / depth limit | per-scheduled-input | `E_NATIVE_SCRIPT_NODE_COUNT` / `E_NATIVE_SCRIPT_DEPTH` |
| `:6513-6519` (`resolve_membership_proof_finalize` via `input_signer_authorization` `:6293-6315`) | a **spend** input (source kind 0) whose resolved address's payment credential is a pub-key hash proven absent from the tx signer set (`payment_credential_signer_authorization` `:6317-6357`); reference inputs and script credentials never trip this | per-spend-input | `E_MISSING_REQUIRED_WITNESS` |

This is the phase whose embedded scan `architecture.md` §2 measures as
unexecutable per-step on L1 (15.3–16.2M mem vs the 14M ceiling,
`architecture.md:83-96`); the scan-borne arms here are exactly the standalone
thread's Direction-B domain.

### 1.9 ScriptSources

Thirteen stages (0–12) that re-scan witnesses, redeemers, the tx's own
outputs, and mint, then run script/purpose/redeemer discovery.

| site | stage | condition | subject space | code |
|------|-------|-----------|---------------|------|
| `:8267-8273`, `:8574-8580` | 0 | field-6 item header undecodable (re-scan of the §1.6 condition while hashing script payloads) | per-script (field-6 item) | `E_INVALID_FIELD_TYPE` |
| `:8859-8867` | 1 | a field-8 redeemer item's CBOR is malformed (`RedeemerItemProofInvalid`) | per-redeemer (field-8 item) | `E_INVALID_FIELD_TYPE` |
| `:9376-9381` | 5 (`script_sources_output_proof_step`) | the tx's **own** field-2 output fails the output canonicity scan | per-output (field-2 item) | `E_INVALID_OUTPUT` |
| `:9382-9387` | 5 | that output's tag-0 reference script fails structural canonicity | per-output | `E_INVALID_FIELD_TYPE` |
| `:9388-9393` / `:9394-9399` | 5 | node / depth limit inside that reference-script scan | per-output | `E_NATIVE_SCRIPT_NODE_COUNT` / `E_NATIVE_SCRIPT_DEPTH` |
| `:9427-9432` (`script_sources_output_proof_finalize` via `protected_output_authorization` `:9268-9286`) | 5 | a **protected** output address with a pub-key payment credential proven absent from the tx signer set | per-output | `E_MISSING_REQUIRED_WITNESS` |
| `:9644-9652` (`script_sources_begin_mint_policy`) | 6 | cumulative declared mint asset count would exceed `max_distinct_asset_count` = 16,384 (`ledger-output-v1.ak:22`) | per-mint-policy (field-5 item) | `E_ASSET_COUNT` |
| `:10108-10116`, `:10467-10476` | 7 | field-3 observer items not strictly ascending (re-scan of the §1.7 condition while building observer purposes) | per-observer | `E_INVALID_FIELD_TYPE` |
| `:10899-10907` (`script_sources_stage_nine`) | 9 | source scan exhausted: **no script source** (inline field-6 item or resolved reference script) has the hash the current purpose requires | per-purpose (kind, index, script hash) | `E_MISSING_REQUIRED_WITNESS` |
| `:11366-11375` (`script_sources_stage_ten`) | 10 | redeemer scan exhausted: **no redeemer** whose decoded pointer matches the current (Plutus-matched) purpose | per-purpose | `E_MISSING_REQUIRED_WITNESS` |
| `:11730-11738` (`script_sources_stage_eleven`) | 11 | an inline field-6 script witness was **used by no purpose** (`used_inline_bitmap` miss) — an extraneous witness | per-script (field-6 item) | `E_INVALID_FIELD_TYPE` |
| `:11996-12004` (`script_sources_stage_twelve`) | 12 | a field-8 redeemer was **used by no purpose** (`used_redeemer_bitmap` miss) — an extraneous redeemer | per-redeemer | `E_INVALID_FIELD_TYPE` |

### 1.10 NativeScripts

`verify_native_scripts` / `verify_native_execution_descriptor_step`
(`:13408-13600`): walks the execution frontier; tag-0 executions re-enter
PhaseANativeScripts in continuation mode (§1.6); tags 3/128 accumulate the
language bitmap. **No rejection emissions of its own** — every failure a
Phase-B native evaluation can produce surfaces through the §1.6 sites in
continuation mode.

### 1.11 ScriptIntegrity

| site | condition | subject space | code |
|------|-----------|---------------|------|
| `:14968-14987` (`verify_script_integrity_finalize`) | `body.script_integrity_hash` ≠ `expected_script_integrity_hash(redeemer_tx_wits_hash, language_bitmap)` (`script_language_views_v1`) | tx-global | `E_INVALID_FIELD_TYPE` |

### 1.12 Cek

| site | condition | subject space | code |
|------|-----------|---------------|------|
| `:15193-15201` (`cek_selection_successor_is_exact`) | selection-time static incompatibility: `language_tag == 3` (PlutusV3) with `purpose_kind == 3` (receive purpose) | per-execution | `E_PLUTUS_SCRIPT_INVALID` |
| `:15068-15080` (`verify_cek_core_step`) | the CEK machine halts in error, **or** a step's cumulative cost exceeds the execution's **declared** budget (`execution_cpu_limit` / `execution_memory_limit` = the matched redeemer's own declared ExUnits, set at `:15567-15568` from `next.execution_steps` / `next.execution_memory`) | per-execution | `E_PLUTUS_SCRIPT_INVALID` |

Note the split: the selection-time arm is a static language/purpose fact, the
core-step arm is a genuine CEK-execution verdict. Only the latter is
interactive-domain (§4).

### 1.13 ValueAndMint

| site | stage | condition | subject space | code |
|------|-------|-----------|---------------|------|
| `:17392-17397` | 2 (input value replay) | folding a resolved spend input's asset would push the tx-wide distinct-asset accumulator over its limit (`ValueAccumulatorAssetLimitExceeded` from `apply_value_asset_mutation` `:14409`) | per-(spend input, asset ordinal) | `E_ASSET_COUNT` |
| `:17486-17495` | 3 (output descriptor) | `output_meets_min_ada_v1(env.coins_per_utxo_byte, descriptor.total_length, descriptor.lovelace)` false — the C49/#618 parameterized floor (`:2220-2235`, rate is a compiled deployment constant per the #627 option-B ruling, comment `:17463-17485`) | per-output | `E_MIN_ADA` |
| `:17557-17562` | 3 (output asset) | accumulator limit crossed while folding an output asset | per-(output, asset ordinal) | `E_ASSET_COUNT` |
| `:17640-17645` | 4 (mint asset) | accumulator limit crossed while folding a mint asset | per-mint-asset (mint frontier ordinal) | `E_ASSET_COUNT` |
| `:17681-17689` | 5 (finalize) | `lovelace_delta - body.fee != 0` **or** `nonzero_asset_count != 0` — value non-preservation | tx-global | `E_VALUE_NOT_PRESERVED` |

### 1.14 LedgerDelta, Terminal

`verify_ledger_delta` (`:18571-18618`) derives the accepted tx's ledger
operations; **no rejection emissions** (a rejecting run never reaches it — the
rejecting terminal derives no operations, `:2248-2263`). `Terminal` admits no
further step (`:18866`).

## 2. Where "constructor = code" breaks: the ambiguity map

The target scheme makes the constructor tag the code. That is only coherent
where a code names one reason against one subject space. The audit finds:

**Codes that are unambiguous today** (one phase, one reason, one subject
space): `E_EMPTY_INPUTS`, `E_DUPLICATE_INPUT_IN_TX`,
`E_NETWORK_ID_MISMATCH`, `E_MIN_FEE`, `E_INVALID_VALIDITY_INTERVAL_FORMAT`,
`E_INVALID_SIGNATURE`, `E_VALIDITY_INTERVAL_MISMATCH`, `E_INPUT_NOT_FOUND`,
`E_MIN_ADA`, `E_VALUE_NOT_PRESERVED`, `E_FIELD_PREIMAGE_SIZE` (three sites,
one reason: declared length ≠ canonical walk length; one subject space:
the field ordinal).

**Codes that must split** — the heart of this catalogue:

1. **`E_INVALID_FIELD_TYPE`** — emitted from **seven phases**
   (CanonicalDecode, PhaseANativeScripts, PhaseAScriptPreconditions,
   ResolveInputs, ScriptSources, ScriptIntegrity — and PhaseANativeScripts
   again in Phase-B continuation mode) for **ten semantically distinct
   reasons** against **six subject spaces** (field item, witness script,
   observer, scheduled input, output, redeemer, tx-global). The committed
   scan-thread design already had to compensate with a terminal-*phase* check
   precisely because the code hash alone cannot attribute a rejection to the
   resolve-inputs scan (design doc §2.1 direction-B: "the code hash alone
   therefore **cannot** attribute a rejection…; the thread requires
   `terminal.phase == ResolveInputs`"). Even that is insufficient within a
   phase: in ScriptSources the same code means "malformed redeemer" (stage 1),
   "invalid reference script" (stage 5), "misordered observers" (stage 7),
   "unused witness" (stage 11), and "unused redeemer" (stage 12). The
   proposal splits it into **twelve constructors** (§6).
2. **`E_MISSING_REQUIRED_WITNESS`** — **three phases, five reasons, five
   subject spaces**: absent required signer (Signatures, field-4 ordinal);
   unsigned spend-input key credential (ResolveInputs, input ordinal);
   unsigned protected-output key credential (ScriptSources stage 5, output
   ordinal); missing script source for a purpose (stage 9, purpose
   coordinate); missing redeemer for a purpose (stage 10, purpose
   coordinate). Five constructors.
3. **`E_NATIVE_SCRIPT_NODE_COUNT` / `E_NATIVE_SCRIPT_DEPTH`** — **three
   phases, four subject spaces** each: witness script item (Phase A),
   Phase-B execution, resolved input's reference script (ResolveInputs), own
   output's reference script (ScriptSources stage 5). Four constructors each
   (execution arms kept for totality; see design note 5 on their
   reachability).
4. **`E_INVALID_OUTPUT`** — **two phases, two subject spaces**: a resolved
   *ledger* output (ResolveInputs, input ordinal) vs the tx's *own* output
   (ScriptSources stage 5, output ordinal). Two constructors.
5. **`E_ASSET_COUNT`** — **two phases, four subject spaces**: declared mint
   cardinality (ScriptSources stage 6, policy ordinal) vs the value-fold
   accumulator crossing at an input asset / output asset / mint asset
   (ValueAndMint stages 2/3/4). Four constructors.
6. **`E_NATIVE_SCRIPT_INVALID`** — one phase, one emission site
   (`:4246-4251`), but **two modes with different subject spaces**: Phase-A
   witness script (field-6 ordinal) vs Phase-B execution (execution ordinal),
   split by `continuation_cbor` (`:3754`). Two constructors.
7. **`E_PLUTUS_SCRIPT_INVALID`** — one phase, **two reasons with different
   refutation classes**: the static V3-for-receive selection rejection
   (single-party refutable) vs the CEK execution verdict (interactive-only).
   Folding these under one constructor would poison the single-party arm with
   the interactive carve-out; they must split. Two constructors.

## 3. Refutability analysis per (reason, subject-space)

Setting common to every refutation of a wrongful `ForcedTxInvalid { reason }`:

- Open the forced leaf: MPF membership of `(tx_order_id → ForcedInclusionTxV1)`
  under `header.forced_transactions_root` + `forced_transaction_count`
  (`validation-claim-v1.ak:229-240`). This yields the **entire transaction**:
  the proof-source triple `(compact_cbor, witness_set_compact_cbor,
  field_preimage_lengths_cbor)`, authenticated against `tx_id` by
  `verify_native_tx_proof_source_v1`, from which any field item is opened by
  the §10 resumable walk / §8.8 door of `docs/spec/midgard-tx.md`.
- Where the prior ledger state is needed: the transition-step membership
  gives `pre_utxos_root` for this tx's step
  (`validation-claim-v1.ak:164-185`, `:407`).
- Where the block context is needed: the header itself carries `end_time`,
  `expected_network_id`, `min_fee_a/b`, `block_slot`
  (`ledger-state.ak:75-80`).

Cost classes: **1-tx** (a single L1 transaction: one or two MPF openings, a
few chunk proofs, constant arithmetic), **thread** (a bounded single-party
computation-thread chain, the native-script-scan pushdown shape —
`architecture.md:98-109`), **interactive** (cannot be established
single-party; the carve-out).

| # | arm (proposed) | refuter opens | procedure | cost | single-party? |
|---|----------------|---------------|-----------|------|---------------|
| 1 | `FieldPreimageLengthMismatch{field_index}` | forced leaf; the named field's preimage chunks | Re-walk the field's §5.1 grammar accumulating canonical item widths; exhibit that the total equals the declared length (or, for the empty case, that the commitment/length pair is consistent). Field preimages are ≤ 32,768 bytes (§5.4), so the walk is bounded. | thread (small; 1-tx for short fields) | yes |
| 2 | `FieldItemWidthIllegal{field_index, item_index}` | forced leaf; the named item's header chunk | Open the item; show its width **is** legal: field-5 item non-empty / field-2 item ≤ 16,384. | 1-tx | yes |
| 3 | `EmptyInputs{}` | forced leaf | Show `body.spend_inputs_hash` ≠ the empty-field commitment. | 1-tx | yes |
| 4 | `DuplicateInput{first_field_index, first_item_index, second_field_index, second_item_index}` | forced leaf; both named items | Open both out-ref items; show their bytes differ (or the coordinates are not distinct). | 1-tx | yes |
| 5 | `ValidityIntervalMalformed{}` | forced leaf | Read `validity_interval_start/end` from the compact body; evaluate `validity_interval_is_malformed` and show false. | 1-tx | yes |
| 6 | `NetworkIdMismatch{}` | forced leaf + header | Show `network_id == 255` or `== expected_network_id`. | 1-tx | yes |
| 7 | `FeeBelowMinimum{}` | forced leaf + header | Recompute `min_fee_a * canonical_size + min_fee_b` from the declared lengths (pure arithmetic); show `fee` ≥ it. | 1-tx | yes |
| 8 | `AddressWitnessSignatureInvalid{witness_index}` | forced leaf; field-7 item | Open the 101-byte item (fixed stride, §5.3); run `verify_ed25519_signature(vkey, tx_id, sig)` on-chain; show it verifies. | 1-tx | yes |
| 9 | `RequiredSignerUnsigned{signer_index}` | forced leaf; field-4 item + one field-7 item | Open the named 28-byte signer hash; exhibit **some** field-7 witness whose vkey Blake2b-224-hashes to it with a valid signature. | 1-tx | yes |
| 10 | `WitnessScriptHeaderMalformed{script_index}` | forced leaf; field-6 item head | Open the item's first chunk; show `versioned_script_header_v1` succeeds. | 1-tx | yes |
| 11 | `WitnessNativeScriptMalformed{script_index}` | forced leaf; the item's chunks | Run the deterministic structure scan to a canonical terminal — the standalone pushdown thread (`native-tx-script-pushdown-v1.ak` shape). ≤ 16,384 nodes over ≤ 16,384* bytes. | thread | yes |
| 12 | `WitnessNativeScriptNodeLimit{script_index}` | same | Same scan; reach the terminal with node count ≤ 16,384. | thread | yes |
| 13 | `WitnessNativeScriptDepthLimit{script_index}` | same | Same scan; depth never exceeds 16,384. | thread | yes |
| 14 | `WitnessNativeScriptFalse{script_index}` | forced leaf; item chunks; field-7/4 witnesses | Scan **and evaluate**: signature nodes against the tx signer set (rebuilt from field 7 — bounded), timelock nodes against the body's interval; reach `result == true`. | thread | yes |
| 15 | `ScriptIntegrityHashMissing{}` | forced leaf | If `script_integrity_hash` ≠ zero-sentinel: 1-tx. Else show no Plutus evaluation is required: redeemer commitment empty **and** every field-6 item tag-0 (a bounded header sweep). | 1-tx / thread | yes |
| 16 | `ObserversForbiddenOnUntaggedNetwork{}` | forced leaf | Show `network_id != 255` or observers commitment empty. | 1-tx | yes |
| 17 | `ObserverOrderInvalid{observer_index}` | forced leaf; field-3 items i−1, i | Open both 28-byte items (fixed stride); show strict ascent. | 1-tx | yes |
| 18 | `ValidityIntervalExcludesBlockSlot{}` | forced leaf + header | Show `validity_interval_contains_slot(body, block_slot)`. | 1-tx | yes |
| 19 | `InputNotFound{source_kind, input_index}` | forced leaf; field-0/1 item; prior ledger root | Open the out-ref item; prove MPF **membership** of that key in `pre_utxos_root`. The mirror of the existing non-existent-input single-party proof. | 1-tx | yes |
| 20 | `InputSpentOutputNonCanonical{source_kind, input_index}` | + the ledger leaf's bytes | MPF-open the resolved output; run the full output canonicity scan to a canonical terminal. Direction B of the standalone thread. | thread | yes |
| 21 | `ResolvedReferenceScriptMalformed{source_kind, input_index}` | same | Same opening; the tag-0 reference-script payload scans to a canonical terminal (or the output has no tag-0 reference script at all). | thread | yes |
| 22 | `ResolvedReferenceScriptNodeLimit{source_kind, input_index}` | same | Same scan; node bound holds. | thread | yes |
| 23 | `ResolvedReferenceScriptDepthLimit{source_kind, input_index}` | same | Same scan; depth bound holds. | thread | yes |
| 24 | `SpendInputSignerMissing{input_index}` | + resolved output's address; one field-7 item | Open the resolved output far enough to extract the payment credential (address is the first output-map entry, §5.5); then either it is a script credential, or exhibit a validly-signed field-7 witness hashing to it. | thread (short) | yes |
| 25 | `RedeemerMalformed{redeemer_index}` | forced leaf; field-8 item chunks | Run the redeemer item scan (`redeemer_item_proof_v1`) to a valid terminal. | thread | yes |
| 26 | `OutputNonCanonical{output_index}` | forced leaf; field-2 item chunks | Output canonicity scan over the tx's own output bytes to a canonical terminal. | thread | yes |
| 27 | `OutputReferenceScriptMalformed{output_index}` | same | As 21, over the own-output bytes. | thread | yes |
| 28 | `OutputReferenceScriptNodeLimit{output_index}` | same | As 22. | thread | yes |
| 29 | `OutputReferenceScriptDepthLimit{output_index}` | same | As 23. | thread | yes |
| 30 | `ProtectedOutputSignerMissing{output_index}` | forced leaf; field-2 item; one field-7 item | As 24, address read from the own-output bytes: unprotected, or script credential, or a validly-signed witness. | thread (short) | yes |
| 31 | `MintDeclaredAssetLimit{policy_index}` | forced leaf; field-5 items 0..policy_index | Sum each policy item's asset-map cardinality (map headers; field ≤ 32,768 bytes); show the running sum stays ≤ 16,384. | thread | yes |
| 32 | `ScriptSourceMissing{purpose_kind, purpose_index}` | forced leaf; possibly ledger leaves | Two obligations: (a) the named purpose is real and requires hash H — re-derivable deterministically (spend purposes from resolved input credentials, mint from field-5 order, observer from field-3, receive from protected own outputs); (b) exhibit one source with hash H: a field-6 item whose payload Blake2b-224-hashes (tag-prefixed) to H, or a resolved output's reference-script hash. If (a) fails — the operator named a phantom purpose — that itself refutes. | thread | yes |
| 33 | `RedeemerMissing{purpose_kind, purpose_index}` | forced leaf; field-8 items | As 32(a) for the purpose, then exhibit a redeemer item whose decoded pointer `(tag, index)` matches it (`redeemer_pointer_matches_purpose_v1` `:11327-11341`). | thread | yes |
| 34 | `UnusedScriptWitness{script_index}` | forced leaf; possibly ledger leaves | Exhibit the purpose that uses source `script_index`: derive the purpose's required hash (as 32(a)) and show the named item hashes to it **and** is the discovery-order match (earlier sources miss). The discovery order is deterministic, so the replay is bounded. | thread | yes |
| 35 | `UnusedRedeemer{redeemer_index}` | forced leaf | Exhibit the purpose whose pointer the named redeemer matches, and that discovery reaches it (deterministic replay). | thread | yes |
| 36 | `ExecutionNativeScriptMalformed{execution_index}` | forced leaf; execution's source bytes | As 11/21 but the subject is fixed by the execution frontier — requires the deterministic discovery replay to pin execution `i`'s source, then the scan. | thread | yes |
| 37 | `ExecutionNativeScriptNodeLimit{execution_index}` | same | As 36. | thread | yes |
| 38 | `ExecutionNativeScriptDepthLimit{execution_index}` | same | As 36. | thread | yes |
| 39 | `ExecutionNativeScriptFalse{execution_index}` | same + witnesses | As 14 over the execution's source. | thread | yes |
| 40 | `ScriptIntegrityHashMismatch{}` | forced leaf | Recompute the expected hash: `redeemer_tx_wits_hash` is in the compact witness set; the language bitmap needs the executions' language tags — deterministic discovery replay; then show equality with the body field. | thread | yes |
| 41 | `ReceivePurposePlutusV3Forbidden{execution_index}` | forced leaf; ledger leaves as needed | Deterministic discovery replay pins execution `i`'s `(language_tag, purpose_kind)`; show the pair is not (3, 3). | thread | yes |
| 42 | `PlutusExecutionFailed{execution_index}` | — | Establishing "this CEK run halts successfully within its declared budget" requires re-running CEK; per `architecture.md` §2 this is the domain where competing authenticated execution traces are intrinsic. **Interactive-only.** | interactive | **no — carve-out** |
| 43 | `InputAssetAccumulationLimit{input_index, asset_index}` | forced leaf; resolved input leaves | Replay the value fold in schedule order up to the named coordinate; show the distinct-asset accumulator stays ≤ its limit at that point. Bounded: every operand set is ≤ 16,384. | thread | yes |
| 44 | `OutputAssetAccumulationLimit{output_index, asset_index}` | same + field-2 items | As 43 through the output stage. | thread | yes |
| 45 | `MintAssetAccumulationLimit{mint_index}` | same + field-5 | As 43 through the mint stage. | thread | yes |
| 46 | `OutputBelowMinAda{output_index}` | forced leaf; field-2 item | The item's own walk width **is** the serialized size; the lovelace is at the front of the value entry. Show `lovelace ≥ coins_per_utxo_byte × (160 + size)` (`:2220-2235`). | 1-tx | yes |
| 47 | `ValueNotPreserved{}` | forced leaf; all resolved input leaves | Full value-fold replay (inputs + outputs + mint) to the terminal; show lovelace delta = fee and zero asset remainder. The heaviest single-party arm, but every operand is committed and every bound is consensus-fixed. | thread | yes |

\* the witness-script byte bound is `max_aggregate_field_preimage_bytes` =
32,768 (`:3456`, `:3876`); the resolved-output cap is 16,384 (`:2275`).

**Summary**: 46 of 47 arms are single-party (13 in one transaction, 33 as
bounded threads); exactly **one** arm — `PlutusExecutionFailed` — is
interactive-domain, and the split of `E_PLUTUS_SCRIPT_INVALID` in §2 exists
precisely so that this carve-out contaminates nothing else.

## 4. Completeness check against the ledger rules

### 4.1 The spec's dispute-side rejection codes

`docs/spec/midgard-tx.md` has **no §12**; its sections end at §11
("Intra-item access"). There is no dispute-side rejection-code table in the
tx-format spec to check against. **OPEN (C-1)**: the spec should gain the
rejection-code register this catalogue proposes (or reference it) in the same
format-revision wave.

### 4.2 The mempool code set vs the machine

The Phase-A/B mempool defines 52 reject codes
(`demo/midgard-validation/src/types.ts:19-71`), deliberately not mapped to
fault categories ("operational evidence, not L1 fault proofs",
`architecture.md` §2 table, §3.1). Diffing against the machine's 19:

- Codes the machine **subsumes structurally**: `E_DOUBLE_SPEND` — the machine
  validates each transition against its own step's `pre_utxos_root`, so an
  intra-block double-spend surfaces at the later step as `E_INPUT_NOT_FOUND`;
  no separate code is needed. `E_CBOR_DESERIALIZATION` / `E_TX_HASH_MISMATCH`
  — unreachable for a committed forced tx, because the source triple is
  authenticated at every step (`verify_native_tx_proof_source_v1`); a
  mis-hashed source cannot be committed. `E_DEPENDENCY_CYCLE` /
  `E_DEPENDS_ON_REJECTED_TX` — scheduling concerns of the block builder, not
  per-tx validity; the per-step pre-root discipline replaces them.
- Codes the machine renders as **stalls, not rejections** — the significant
  gap class, treated in §4.3: `E_TX_SIZE`, `E_VALUE_SIZE`, the eight count
  caps (`E_INPUT_COUNT` … `E_OBSERVER_COUNT`), `E_LEDGER_OUTPUT_SIZE`,
  `E_DATUM_SIZE`, `E_SCRIPT_PROGRAM_SIZE`, `E_SCRIPT_PROGRAM_AGGREGATE_SIZE`,
  `E_REDEEMER_SIZE`, `E_SCRIPT_PROGRAM_ENCODING` (partially — tag-0 encoding
  faults do reject via the scan arms).
- Mempool-only **admission policy** with no machine meaning:
  `E_IS_VALID_FALSE_FORBIDDEN`, `E_AUX_DATA_FORBIDDEN`,
  `E_CERTIFICATES_FORBIDDEN`, `E_NONZERO_WITHDRAWAL`, `E_TX_VERSION`,
  `E_MINT_FORBIDDEN`, `E_REFERENCE_INPUT_FORBIDDEN`,
  `E_SCRIPT_FEATURE_FORBIDDEN`, `E_UNSUPPORTED_FIELD_NONEMPTY`,
  `E_PLUTUS_EVALUATION_UNAVAILABLE`, `E_CEK_PROGRAM_MATERIAL`. These do not
  need `RejectionReasonV1` arms: they are not reasons the *machine* can
  reject, and the verdict must be adjudicated against the machine.

### 4.3 Stall conditions — where an operator would be forced to mis-code

**OPEN (C-2), the material completeness gap — RESOLVED by owner ruling
(2026-08-24): the forced-order door excludes these preimages; see the
resolution note at the end of this subsection.** Several machine guardrails
are written as bare conjuncts of the step relation, so a violating
transaction has **no valid step at all** — neither an accepting nor a
rejecting successor exists. Verified instances:

- an oversized field-6 script item: `item_length <= max_aggregate_field_preimage_bytes`
  is a conjunct (`:3876`, `:8263`, `:8569-8570`), not a rejection branch;
- collection counts above `max_tx_size_derived_collection_item_count`:
  conjuncts at `:2611`, `:3125`, `:3267`, `:8261`, `:9634`, `:10460` et al.;
- a malformed out-ref item in fields 0/1: `encode_midgard_tx_input(input) == key`
  is a conjunct (`:2616`), and the decode itself `expect`s;
- a malformed field-7 address-witness item: `decode_midgard_address_witness_cbor`
  (`:3059`) `expect`s rather than rejecting;
- a field-4 item that is not 28 bytes: conjunct at `:3270`;
- a field-5 mint item whose deep shape is wrong (`expect item_count == 2`,
  `:9613`) — CanonicalDecode only checked item *widths* (§1.1), not deep
  grammar.

For a **forced** transaction the operator must commit some verdict and a
provable trace. If the true run stalls, no honest trace exists: the operator
is forced either to mis-code (commit a rejection code the machine cannot
prove from that state) or to commit an unprovable trace — both of which a
challenger can attack, but neither of which the operator can avoid. Whether
these shapes are excluded upstream — by the L1 forced-order publication path
(`docs/spec/midgard-tx.md` §8.11 forced-order material carriage) refusing to
finalize an order whose preimages violate them — was **not verified in this
audit**. Resolution options are in design note 6.

**Resolution (owner ruling, 2026-08-24).** The forced-order door **does**
exclude these preimages: an order whose material violates the guardrails
above cannot be finalized, so a stall-class transaction never becomes a
forced order and the operator always has an honest verdict. The reserved
`GuardrailExceeded` family (§6) therefore stays unpopulated (design note 6's
exclusion branch), and the leaf-format freeze is not gated on this
subsection. Residual evidence task (non-gating, tracked as #641): document
the per-conjunct coverage mapping — each conjunct above to the §8.11 door
check that excludes it — as the recorded invariant that keeps the family
unreserved.

### 4.4 Codes that can never legitimately apply to a forced tx

None of the 19 codes is Normal-only — every emission site sits on the shared
per-tx machine, and Normal sources simply may not use the rejecting terminals
(claim layer, §0). Conversely, three proposed arms
(`ExecutionNativeScript{Malformed,NodeLimit,DepthLimit}`, #36–38) are emitted
by code that is **plausibly unreachable** for any committable transaction:
every Phase-B native source is either an inline field-6 item (already
structure-scanned in Phase A, §1.6) or a resolved reference script (already
scanned during ResolveInputs / ScriptSources stage 5, §§1.8–1.9), so a
malformed one should have rejected earlier under a different arm. They are
retained for totality with the machine as written; see design note 5.

### 4.5 The coarse-verdict bridge is under-specified today

`MidgardTxValidity` (`ledger-state.ak:483-491`, marked `// TODO`) has five
rejection arms: `NonExistentInputUtxo`, `InvalidSignature`, `FailedScript`,
`FeeTooLow`, `UnbalancedTx`. Two findings:

- `forced_verdict_matches` (`validation-claim-v1.ak:204-213`) adjudicates only
  the valid/invalid **bit** — any invalid arm matches any rejecting
  descriptor, whatever the machine's code. The operator's choice among the
  five arms is uncommitted-to and unadjudicated today.
- **No total map** from the 19 codes (or the mempool's 52) to the five arms
  exists anywhere in the repo (the codec `demo/midgard-core/src/codec/native-validation.ts:6-13`
  only numbers the arms). And no honest bucket exists for the structural
  family (`E_FIELD_PREIMAGE_SIZE`, `E_INVALID_FIELD_TYPE` shapes,
  `E_EMPTY_INPUTS`, `E_DUPLICATE_INPUT_IN_TX`,
  `E_INVALID_VALIDITY_INTERVAL_FORMAT`, `E_NETWORK_ID_MISMATCH`,
  `E_VALIDITY_INTERVAL_MISMATCH`, `E_INVALID_OUTPUT`) — none of the five arms
  says "malformed". **OPEN (C-3)**; the proposal's `coarse_bucket_of` (§6.3)
  makes a documented convention total, and design note 3 recommends the arm
  set be revised (or retired) in the same wave that lands the verdict
  restructure.

## 5. Proposed `RejectionReasonV1`

**47 constructors** from the 19 raw codes. Grouping mirrors the phase order.
Coordinate conventions, used by every payload comment:

- *field ordinal*: the §2.5 slot index 0–8;
- *item ordinal*: 0-based position in that field's §5.1 preimage item
  sequence (**wire order**, not visitation order);
- *source kind*: 0 = spend (field 0), 1 = reference (field 1);
- *purpose coordinate*: `(purpose_kind, purpose_index)` with kind 0 = spend,
  1 = mint, 2 = observer, 3 = receive (`purpose_leaf_hash` call sites
  `:9665`, `:10127`, `:9302`; `redeemer_tag_for_purpose_kind_v1`
  `:11313-11326`), index = 0-based position within that kind's canonical
  enumeration order;
- *execution ordinal*: 0-based position in the execution frontier (discovery
  completion order, `append_script_execution` `:10861-10881`).

Payloads carry **only** subject coordinates — never expected values, never
hashes, never recomputable arguments. Where a scan position exists
(native-script token faults) it is deliberately **not** carried: the scan is
deterministic, so the item ordinal alone bounds the refutation (design
note 4).

```aiken
pub type RejectionReasonV1 {
  // ── CanonicalDecode ────────────────────────────────────────────────
  /// Declared field preimage length ≠ the canonical §5.1 walk length.
  /// field_index: field ordinal 0–8.
  FieldPreimageLengthMismatch { field_index: Int }
  /// Item width illegal for its field (empty mint item / oversized output).
  /// Coordinates: field ordinal × item ordinal.
  FieldItemWidthIllegal { field_index: Int, item_index: Int }

  // ── InputSets ──────────────────────────────────────────────────────
  /// Spend-input field is empty. Tx-global.
  EmptyInputs
  /// The same out-ref appears at two positions of the fields-0/1 union.
  /// Both coordinates: field ordinal × item ordinal; must be distinct.
  DuplicateInput {
    first_field_index: Int,
    first_item_index: Int,
    second_field_index: Int,
    second_item_index: Int,
  }
  /// validity_interval_is_malformed on the body's two bounds. Tx-global.
  ValidityIntervalMalformed

  // ── StaticLedgerRules ──────────────────────────────────────────────
  /// network_id ∉ {255, expected_network_id}. Tx-global.
  NetworkIdMismatch
  /// fee < min_fee_a · canonical_size + min_fee_b. Tx-global.
  FeeBelowMinimum

  // ── Signatures ─────────────────────────────────────────────────────
  /// Field-7 witness whose Ed25519 signature over tx_id fails.
  /// witness_index: field-7 item ordinal.
  AddressWitnessSignatureInvalid { witness_index: Int }
  /// Field-4 required signer with no matching valid address witness.
  /// signer_index: field-4 item ordinal.
  RequiredSignerUnsigned { signer_index: Int }

  // ── PhaseANativeScripts (witness scripts, field 6) ─────────────────
  /// Versioned-script header undecodable. script_index: field-6 ordinal.
  WitnessScriptHeaderMalformed { script_index: Int }
  /// Tag-0 payload fails the structural token scan (incl. trailing bytes).
  WitnessNativeScriptMalformed { script_index: Int }
  /// Structural scan exceeds 16,384 nodes.
  WitnessNativeScriptNodeLimit { script_index: Int }
  /// Structural scan exceeds depth 16,384.
  WitnessNativeScriptDepthLimit { script_index: Int }
  /// Phase-A evaluation of the witness script is false.
  WitnessNativeScriptFalse { script_index: Int }

  // ── PhaseAScriptPreconditions ──────────────────────────────────────
  /// Plutus evaluation required but script_integrity_hash is zero. Tx-global.
  ScriptIntegrityHashMissing
  /// Observers present while network_id == 255. Tx-global.
  ObserversForbiddenOnUntaggedNetwork
  /// Field-3 observers not strictly ascending at this position.
  /// observer_index: field-3 item ordinal of the offending (later) item.
  ObserverOrderInvalid { observer_index: Int }

  // ── ResolveInputs ──────────────────────────────────────────────────
  /// Block slot outside the tx validity interval. Tx-global vs context.
  ValidityIntervalExcludesBlockSlot
  /// Scheduled out-ref absent from the prior ledger root.
  /// source_kind 0/1 × item ordinal in field 0/1.
  InputNotFound { source_kind: Int, input_index: Int }
  /// Resolved output's committed bytes fail output canonicity.
  InputSpentOutputNonCanonical { source_kind: Int, input_index: Int }
  /// Resolved output's tag-0 reference script structurally invalid.
  ResolvedReferenceScriptMalformed { source_kind: Int, input_index: Int }
  /// That scan exceeds the node bound.
  ResolvedReferenceScriptNodeLimit { source_kind: Int, input_index: Int }
  /// That scan exceeds the depth bound.
  ResolvedReferenceScriptDepthLimit { source_kind: Int, input_index: Int }
  /// Spend input's pub-key payment credential has no valid signature.
  /// input_index: field-0 item ordinal (source kind 0 by definition).
  SpendInputSignerMissing { input_index: Int }

  // ── ScriptSources ──────────────────────────────────────────────────
  /// Field-8 redeemer item CBOR malformed. redeemer_index: field-8 ordinal.
  RedeemerMalformed { redeemer_index: Int }
  /// The tx's own field-2 output fails output canonicity.
  OutputNonCanonical { output_index: Int }
  /// Own output's tag-0 reference script structurally invalid.
  OutputReferenceScriptMalformed { output_index: Int }
  /// That scan exceeds the node bound.
  OutputReferenceScriptNodeLimit { output_index: Int }
  /// That scan exceeds the depth bound.
  OutputReferenceScriptDepthLimit { output_index: Int }
  /// Protected own output's pub-key credential has no valid signature.
  ProtectedOutputSignerMissing { output_index: Int }
  /// Cumulative declared mint asset count exceeds 16,384 at this policy.
  /// policy_index: field-5 item ordinal.
  MintDeclaredAssetLimit { policy_index: Int }
  /// No script source carries the hash this purpose requires.
  /// Purpose coordinate (kind 0–3 × per-kind ordinal).
  ScriptSourceMissing { purpose_kind: Int, purpose_index: Int }
  /// No redeemer's pointer matches this (Plutus-matched) purpose.
  RedeemerMissing { purpose_kind: Int, purpose_index: Int }
  /// Inline field-6 witness used by no purpose. script_index: field-6 ordinal.
  UnusedScriptWitness { script_index: Int }
  /// Field-8 redeemer used by no purpose. redeemer_index: field-8 ordinal.
  UnusedRedeemer { redeemer_index: Int }

  // ── NativeScripts / Phase-B executions ─────────────────────────────
  /// Execution's native source fails the structural scan.
  /// execution_index: execution-frontier ordinal.
  ExecutionNativeScriptMalformed { execution_index: Int }
  /// That scan exceeds the node bound.
  ExecutionNativeScriptNodeLimit { execution_index: Int }
  /// That scan exceeds the depth bound.
  ExecutionNativeScriptDepthLimit { execution_index: Int }
  /// Phase-B evaluation of the execution's native script is false.
  ExecutionNativeScriptFalse { execution_index: Int }

  // ── ScriptIntegrity ────────────────────────────────────────────────
  /// Body's script_integrity_hash ≠ recomputed language-views hash. Tx-global.
  ScriptIntegrityHashMismatch

  // ── Cek ────────────────────────────────────────────────────────────
  /// Static: PlutusV3 (tag 3) bound to a receive purpose (kind 3).
  ReceivePurposePlutusV3Forbidden { execution_index: Int }
  /// CEK halted in error, or exceeded the redeemer's declared ExUnits.
  /// INTERACTIVE-DOMAIN: not single-party refutable (architecture.md §2).
  PlutusExecutionFailed { execution_index: Int }

  // ── ValueAndMint ───────────────────────────────────────────────────
  /// Distinct-asset accumulator overflow while folding an input asset.
  /// input_index: field-0 ordinal; asset_index: 0-based in that output's
  /// canonical asset order.
  InputAssetAccumulationLimit { input_index: Int, asset_index: Int }
  /// Same crossing at an own-output asset.
  OutputAssetAccumulationLimit { output_index: Int, asset_index: Int }
  /// Same crossing at a mint asset. mint_index: mint-frontier ordinal.
  MintAssetAccumulationLimit { mint_index: Int }
  /// Output funds less than coins_per_utxo_byte · (160 + serialized size).
  OutputBelowMinAda { output_index: Int }
  /// Lovelace delta ≠ fee, or a nonzero net asset remains. Tx-global.
  ValueNotPreserved
}
```

A **reserved** family, not populated in V1 (see §4.3 / design note 6):
`GuardrailExceeded { guardrail: Int, field_index: Int, item_index: Int }` —
kept unpopulated per the 2026-08-24 owner ruling that the forced-order path
excludes guardrail-violating preimages (§4.3 resolution note); it would be
enacted only if the #641 coverage mapping refutes that exclusion, and
enacting it is a format revision like any other arm addition (design
note 1).

### 5.1 `rejection_code_of` — the total map back to the frozen descriptor codes

For the bridge to the frozen descriptor format (which keeps
`rejection_code_hash = hash_rejection_code(E_*)`), each arm maps to exactly
one legacy code; the map is total and non-injective by design:

| E_* code | arms |
|----------|------|
| `E_FIELD_PREIMAGE_SIZE` | FieldPreimageLengthMismatch |
| `E_INVALID_FIELD_TYPE` | FieldItemWidthIllegal, WitnessScriptHeaderMalformed, WitnessNativeScriptMalformed, ScriptIntegrityHashMissing, ObserversForbiddenOnUntaggedNetwork, ObserverOrderInvalid, ResolvedReferenceScriptMalformed, RedeemerMalformed, OutputReferenceScriptMalformed, UnusedScriptWitness, UnusedRedeemer, ExecutionNativeScriptMalformed, ScriptIntegrityHashMismatch |
| `E_NATIVE_SCRIPT_NODE_COUNT` | WitnessNativeScriptNodeLimit, ResolvedReferenceScriptNodeLimit, OutputReferenceScriptNodeLimit, ExecutionNativeScriptNodeLimit |
| `E_NATIVE_SCRIPT_DEPTH` | WitnessNativeScriptDepthLimit, ResolvedReferenceScriptDepthLimit, OutputReferenceScriptDepthLimit, ExecutionNativeScriptDepthLimit |
| `E_MIN_ADA` | OutputBelowMinAda |
| `E_ASSET_COUNT` | MintDeclaredAssetLimit, InputAssetAccumulationLimit, OutputAssetAccumulationLimit, MintAssetAccumulationLimit |
| `E_EMPTY_INPUTS` | EmptyInputs |
| `E_DUPLICATE_INPUT_IN_TX` | DuplicateInput |
| `E_NETWORK_ID_MISMATCH` | NetworkIdMismatch |
| `E_MIN_FEE` | FeeBelowMinimum |
| `E_INVALID_VALIDITY_INTERVAL_FORMAT` | ValidityIntervalMalformed |
| `E_MISSING_REQUIRED_WITNESS` | RequiredSignerUnsigned, SpendInputSignerMissing, ProtectedOutputSignerMissing, ScriptSourceMissing, RedeemerMissing |
| `E_INVALID_SIGNATURE` | AddressWitnessSignatureInvalid |
| `E_NATIVE_SCRIPT_INVALID` | WitnessNativeScriptFalse, ExecutionNativeScriptFalse |
| `E_PLUTUS_SCRIPT_INVALID` | ReceivePurposePlutusV3Forbidden, PlutusExecutionFailed |
| `E_VALIDITY_INTERVAL_MISMATCH` | ValidityIntervalExcludesBlockSlot |
| `E_INPUT_NOT_FOUND` | InputNotFound |
| `E_INVALID_OUTPUT` | InputSpentOutputNonCanonical, OutputNonCanonical |
| `E_VALUE_NOT_PRESERVED` | ValueNotPreserved |

(As Aiken: a single total `pub fn rejection_code_of(reason: RejectionReasonV1)
-> ByteArray` `when`-expression returning the §0 constants; mechanical from
the table.)

### 5.2 `coarse_bucket_of` — the total map to `MidgardTxValidity`

A documented convention (the enum offers no "malformed" arm — §4.5; the
convention routes every structural fault through `FailedScript`'s neighbor
semantics **except** where a more truthful arm exists):

| `MidgardTxValidity` arm | arms |
|-------------------------|------|
| `NonExistentInputUtxo` | InputNotFound |
| `InvalidSignature` | AddressWitnessSignatureInvalid, RequiredSignerUnsigned, SpendInputSignerMissing, ProtectedOutputSignerMissing |
| `FeeTooLow` | FeeBelowMinimum |
| `UnbalancedTx` | ValueNotPreserved, OutputBelowMinAda, InputAssetAccumulationLimit, OutputAssetAccumulationLimit, MintAssetAccumulationLimit, MintDeclaredAssetLimit |
| `FailedScript` | every remaining arm — all script/witness/redeemer arms **and, by convention,** the structural family (FieldPreimageLengthMismatch, FieldItemWidthIllegal, EmptyInputs, DuplicateInput, ValidityIntervalMalformed, NetworkIdMismatch, ValidityIntervalExcludesBlockSlot, InputSpentOutputNonCanonical, OutputNonCanonical) |

The `FailedScript`-as-catch-all row is honest about being a convention, not a
semantics; design note 3 recommends the enum be revised in the same wave so
the convention can die young.

### 5.3 Master table

Cost classes and single-party status are §3's; phases are §1's.

| arm | phase(s) | subject payload | cost | single-party? |
|-----|----------|-----------------|------|---------------|
| FieldPreimageLengthMismatch | CanonicalDecode | field_index | thread (small) | yes |
| FieldItemWidthIllegal | CanonicalDecode | field_index, item_index | 1-tx | yes |
| EmptyInputs | InputSets | — | 1-tx | yes |
| DuplicateInput | InputSets | 2 × (field, item) | 1-tx | yes |
| ValidityIntervalMalformed | InputSets | — | 1-tx | yes |
| NetworkIdMismatch | StaticLedgerRules | — | 1-tx | yes |
| FeeBelowMinimum | StaticLedgerRules | — | 1-tx | yes |
| AddressWitnessSignatureInvalid | Signatures | witness_index | 1-tx | yes |
| RequiredSignerUnsigned | Signatures | signer_index | 1-tx | yes |
| WitnessScriptHeaderMalformed | PhaseANativeScripts, ScriptSources(0) | script_index | 1-tx | yes |
| WitnessNativeScriptMalformed | PhaseANativeScripts | script_index | thread | yes |
| WitnessNativeScriptNodeLimit | PhaseANativeScripts | script_index | thread | yes |
| WitnessNativeScriptDepthLimit | PhaseANativeScripts | script_index | thread | yes |
| WitnessNativeScriptFalse | PhaseANativeScripts | script_index | thread | yes |
| ScriptIntegrityHashMissing | PhaseAScriptPreconditions | — | 1-tx/thread | yes |
| ObserversForbiddenOnUntaggedNetwork | PhaseAScriptPreconditions | — | 1-tx | yes |
| ObserverOrderInvalid | PhaseAScriptPreconditions, ScriptSources(7) | observer_index | 1-tx | yes |
| ValidityIntervalExcludesBlockSlot | ResolveInputs | — | 1-tx | yes |
| InputNotFound | ResolveInputs | source_kind, input_index | 1-tx | yes |
| InputSpentOutputNonCanonical | ResolveInputs | source_kind, input_index | thread | yes |
| ResolvedReferenceScriptMalformed | ResolveInputs | source_kind, input_index | thread | yes |
| ResolvedReferenceScriptNodeLimit | ResolveInputs | source_kind, input_index | thread | yes |
| ResolvedReferenceScriptDepthLimit | ResolveInputs | source_kind, input_index | thread | yes |
| SpendInputSignerMissing | ResolveInputs | input_index | thread (short) | yes |
| RedeemerMalformed | ScriptSources(1) | redeemer_index | thread | yes |
| OutputNonCanonical | ScriptSources(5) | output_index | thread | yes |
| OutputReferenceScriptMalformed | ScriptSources(5) | output_index | thread | yes |
| OutputReferenceScriptNodeLimit | ScriptSources(5) | output_index | thread | yes |
| OutputReferenceScriptDepthLimit | ScriptSources(5) | output_index | thread | yes |
| ProtectedOutputSignerMissing | ScriptSources(5) | output_index | thread (short) | yes |
| MintDeclaredAssetLimit | ScriptSources(6) | policy_index | thread | yes |
| ScriptSourceMissing | ScriptSources(9) | purpose_kind, purpose_index | thread | yes |
| RedeemerMissing | ScriptSources(10) | purpose_kind, purpose_index | thread | yes |
| UnusedScriptWitness | ScriptSources(11) | script_index | thread | yes |
| UnusedRedeemer | ScriptSources(12) | redeemer_index | thread | yes |
| ExecutionNativeScriptMalformed | PhaseANativeScripts (Phase-B mode) | execution_index | thread | yes |
| ExecutionNativeScriptNodeLimit | PhaseANativeScripts (Phase-B mode) | execution_index | thread | yes |
| ExecutionNativeScriptDepthLimit | PhaseANativeScripts (Phase-B mode) | execution_index | thread | yes |
| ExecutionNativeScriptFalse | PhaseANativeScripts (Phase-B mode) | execution_index | thread | yes |
| ScriptIntegrityHashMismatch | ScriptIntegrity | — | thread | yes |
| ReceivePurposePlutusV3Forbidden | Cek (selection) | execution_index | thread | yes |
| PlutusExecutionFailed | Cek (core) | execution_index | interactive | **no** |
| InputAssetAccumulationLimit | ValueAndMint(2) | input_index, asset_index | thread | yes |
| OutputAssetAccumulationLimit | ValueAndMint(3) | output_index, asset_index | thread | yes |
| MintAssetAccumulationLimit | ValueAndMint(4) | mint_index | thread | yes |
| OutputBelowMinAda | ValueAndMint(3) | output_index | 1-tx | yes |
| ValueNotPreserved | ValueAndMint(5) | — | thread | yes |

## 6. Design notes and open questions

1. **Adding codes is a format revision — say so.** A fully enumerated sum
   type with constructor-as-code means any new rejection reason changes the
   wire format of the forced leaf and therefore the meaning of
   `forced_transactions_root`. That is a protocol format revision, the same
   class of change as the verdict restructure itself; there is no in-band
   extension point, deliberately (an "other/unknown" arm would be a sentinel
   by another name and would break "constructor = code"). *Recommendation*:
   version the type in its name (`RejectionReasonV1`), and let
   `rejection_code_of` / `coarse_bucket_of` be the compatibility surface for
   frozen consumers; a `RejectionReasonV2` is a new leaf schema version.
2. **The subject payload is what retires the design doc's OPEN (B-1).** The
   committed scan-thread design must universally quantify over all resolved
   outpoints because the terminal state commits no subject. With
   `ResolvedReferenceScript*{source_kind, input_index}` on the leaf, a
   Direction-B thread checks exactly one outpoint. *Recommendation*: when the
   verdict restructure lands, re-scope that thread's Direction B to the named
   subject and drop the universal quantification (its §9 Q1).
3. **`MidgardTxValidity` should be revised or retired in the same wave**
   (§4.5). The five-arm enum cannot express the structural family, its arm
   choice is unadjudicated by `forced_verdict_matches`, and once
   `OperatorVerdictV1` carries the full reason the coarse arm is redundant on
   the forced leaf. *Recommendation*: retire the rejection arms of
   `MidgardTxValidity` from the forced leaf (keep `TxIsValid`-equivalent
   semantics inside `OperatorVerdictV1`), keeping `coarse_bucket_of` only for
   frozen off-chain consumers during the transition.
4. **Scan-position payloads were considered and dropped.** For the
   native-script/output/redeemer scan arms a byte cursor of the offending
   token is knowable, but it is an *argument* (recomputable by the
   deterministic scan), not a *subject*; carrying it would invite
   "operator names a position the scan never reaches" pathologies that the
   refuter would then have to adjudicate. The item/execution ordinal alone
   bounds the refutation. Judged ambiguous and resolved to the minimal
   payload.
5. **The `ExecutionNativeScript*` structural arms (#36–38) are plausibly
   dead code** (§4.4): every Phase-B native source is pre-scanned by an
   earlier phase, so the machine as written should never reach these
   rejections. They are kept because the type must be total over the machine
   as written, not over a reachability argument this audit did not formally
   close. **Ruled by owner (2026-08-24): keep all 47** — the format wave
   ships the type as written; the reachability proof rides #641 as
   non-gating evidence. If it later closes, dropping the three arms and
   asserting the invariant in the machine (`fail` instead of reject,
   shrinking the type to 44) is a subsequent format revision, not part of
   the #640 wave. `ExecutionNativeScriptFalse` (#39) is genuinely reachable
   (evaluation depends on the signer set, not structure) and stays
   regardless.
6. **The stall question (OPEN C-2) — RESOLVED by owner ruling
   (2026-08-24), exclusion branch.** The forced-order publication path
   (`midgard-tx.md` §8.11) excludes preimages that violate the
   bare-conjunct guardrails of §4.3, so no operator is ever handed an
   order with *no honest verdict* and the reserved `GuardrailExceeded`
   family stays unpopulated. The type freeze is not gated on this.
   Residual: record that exclusion as the invariant that keeps the family
   unreserved — the per-conjunct coverage mapping is tracked as #641
   (evidence, non-gating).
7. **`AddressWitnessSignatureInvalid` names a witness the machine's own
   terminal never named** (§1.5's latch structure). The arm is judged
   sound anyway: the operator picks the subject, and the refuter checks that
   *named* signature — a wrongful rejection with a valid named signature is
   refuted in one transaction regardless of which item the machine's latch
   would have tripped on. But the machine's trace and the leaf's subject can
   legitimately differ (two invalid signatures; operator names the second).
   *Recommendation*: the restructured claim layer should adjudicate the
   *reason*, not the trace's internal latch order — i.e.
   `forced_verdict_matches`'s successor checks reason-validity (the named
   subject exhibits the named fault), not trace-equality of subjects.
8. **`DuplicateInput` carries four ordinals, not the out-ref.** The out-ref
   key (36 bytes) is the natural subject but is derivable from either
   ordinal; carrying both coordinates keeps the "no arguments" rule while
   making the refutation a two-opening comparison. Judged the least
   ambiguous of the three candidate payloads (key alone / one ordinal /
   two ordinals): key alone would be an argument, one ordinal would leave
   the refuter searching for the partner.
9. **One arm, several sites, two phases** (`WitnessScriptHeaderMalformed`,
   `ObserverOrderInvalid`): the machine re-checks the same predicate against
   the same subject in a later phase (ScriptSources stages 0/7). The
   constructor deliberately does not encode *which* phase tripped — the
   reason is the predicate, and the refutation is identical. This is the
   correct collapse of what the raw-code scheme could not even distinguish.
10. **The interactive carve-out is exactly one arm.** `PlutusExecutionFailed`
    is the only reason whose truth is not a deterministic predicate over
    committed bytes that L1 can re-check in bounded single-party work — CEK
    re-execution is the definitionally interactive domain
    (`architecture.md` §2). The split from `ReceivePurposePlutusV3Forbidden`
    (§2 item 7) keeps the carve-out from leaking onto a static fact sharing
    its legacy code. *Recommendation*: the interactive dispute family for
    this arm should consume the `execution_index` subject to select the
    single execution under dispute, mirroring note 2.
