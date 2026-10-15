//// The operator's per-transaction verdict on a forced-inclusion L2
//// transaction, with the fully enumerated rejection-reason space.
////
//// Normative inventory: `docs/fault-proofs/rejection-reason-catalogue-v1.md`
//// (§5 arm table and coordinate conventions, §6 type). The constructor
//// declaration order of `RejectionReasonV1` is **wire-normative** — it fixes
//// the Plutus Data constructor indices of the forced leaf
//// (`ledger_state.ForcedInclusionTxV1.verdict`), so arms must never be
//// reordered, inserted, or removed except through a leaf-schema format
//// revision (`RejectionReasonV2`; catalogue design note 1 — deliberately no
//// in-band extension point).
////
//// The constructor tag IS the rejection reason; payloads carry **only**
//// subject coordinates (field/item/witness/output/redeemer/purpose
//// ordinals) — never expected values, hashes, or recomputable arguments
//// (catalogue design note 4).
////
//// Coordinate conventions (catalogue §5):
////   - field ordinal: the §2.5 slot index 0–8;
////   - item ordinal: 0-based position in that field's §5.1 preimage item
////     sequence (wire order, not visitation order);
////   - source kind: 0 = spend (field 0), 1 = reference (field 1);
////   - purpose coordinate: (purpose_kind, purpose_index), kind 0 = spend,
////     1 = mint, 2 = observer, 3 = receive; purpose_index is the redeemer
////     pointer index, the 0-based position among the transaction's sorted
////     spent inputs, mint policies, observers or receive hashes (for
////     receive: the sorted distinct-hash ordinal, not an output position).
////
//// The reason itself is the rejection preimage: the machine's terminal
//// rejection binds `validation_trace_v1.hash_rejection_code(reason)`, the
//// domain-separated blake2b-256 of the reason's Plutus Data CBOR, and the
//// forced-verdict check compares that hash with the descriptor's
//// `rejection_code_hash` directly.
////
//// A reserved family, not populated in V1: `GuardrailExceeded { guardrail,
//// field_index, item_index }` — kept unpopulated per the 2026-08-24 owner
//// ruling that the L1 forced-order door (`docs/spec/midgard-tx.md` §8.11)
//// excludes guardrail-violating preimages (catalogue §4.3 resolution,
//// design note 6); enacting it would be a format revision like any other
//// arm addition.

/// The operator's recorded verdict on a forced-inclusion transaction, as the
/// forced leaf carries it. The sum's collapse deliberately makes "valid with
/// a reason" and "invalid without one" unrepresentable.
pub type OperatorVerdictV1 {
  ForcedTxValid
  ForcedTxInvalid { reason: RejectionReasonV1 }
}

/// The full space of operator rejection reasons — 49 constructors (arms
/// 0–48), one per (reason, subject-space) pair. Declaration order is
/// wire-normative (catalogue §5/§6); arm 47 `MintItemMalformed` is the first
/// arm of the T238 wave and new arms only ever append.
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
  /// Purpose coordinate of the execution (kind 0–3 × per-kind ordinal).
  ExecutionNativeScriptMalformed { purpose_kind: Int, purpose_index: Int }
  /// That scan exceeds the node bound.
  ExecutionNativeScriptNodeLimit { purpose_kind: Int, purpose_index: Int }
  /// That scan exceeds the depth bound.
  ExecutionNativeScriptDepthLimit { purpose_kind: Int, purpose_index: Int }
  /// Phase-B evaluation of the execution's native script is false.
  ExecutionNativeScriptFalse { purpose_kind: Int, purpose_index: Int }

  // ── ScriptIntegrity ────────────────────────────────────────────────
  /// Body's script_integrity_hash ≠ recomputed language-views hash. Tx-global.
  ScriptIntegrityHashMismatch

  // ── Cek ────────────────────────────────────────────────────────────
  /// Static: PlutusV3 (tag 3) bound to a receive purpose (kind 3).
  /// Purpose coordinate of the execution.
  ReceivePurposePlutusV3Forbidden { purpose_kind: Int, purpose_index: Int }
  /// CEK halted in error, or exceeded the redeemer's declared ExUnits.
  /// INTERACTIVE-DOMAIN: not single-party refutable (architecture.md §2).
  /// Purpose coordinate of the execution.
  PlutusExecutionFailed { purpose_kind: Int, purpose_index: Int }

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

  // ── ScriptSources mint items ───────────────────────────────────────
  /// Field-5 mint item is non-canonical (policy or asset-name misorder).
  /// policy_index: field-5 item ordinal of the offending policy.
  MintItemMalformed { policy_index: Int }

  // ── ScriptDecode (Plutus) ──────────────────────────────────────────
  /// A Plutus script item whose program Cardano refuses to decode.
  /// item_kind: 0 = field-6 witness script, 1 = field-2 output reference
  /// script. item_index: that field's item ordinal. class: the decode
  /// record's refusal class (1–17).
  PlutusScriptUndecodable { item_kind: Int, item_index: Int, class: Int }
}

/// True when `reason` is an execution-scoped arm (35–38, 40, 41) whose purpose
/// coordinate is exactly `(purpose_kind, purpose_index)`. Execution families
/// use it to tie a bound rejection to the purpose leaf they open.
pub fn names_execution_purpose(
  reason: RejectionReasonV1,
  purpose_kind: Int,
  purpose_index: Int,
) -> Bool {
  let coordinate_matches = fn(reason_kind: Int, reason_index: Int) {
    and {
      reason_kind == purpose_kind,
      reason_index == purpose_index,
    }
  }
  when reason is {
    ExecutionNativeScriptMalformed { purpose_kind: k, purpose_index: i } ->
      coordinate_matches(k, i)
    ExecutionNativeScriptNodeLimit { purpose_kind: k, purpose_index: i } ->
      coordinate_matches(k, i)
    ExecutionNativeScriptDepthLimit { purpose_kind: k, purpose_index: i } ->
      coordinate_matches(k, i)
    ExecutionNativeScriptFalse { purpose_kind: k, purpose_index: i } ->
      coordinate_matches(k, i)
    ReceivePurposePlutusV3Forbidden { purpose_kind: k, purpose_index: i } ->
      coordinate_matches(k, i)
    PlutusExecutionFailed { purpose_kind: k, purpose_index: i } ->
      coordinate_matches(k, i)
    _ -> False
  }
}

/// The `DuplicateInput` reason for a duplicate pair the InputSets scan found.
/// `first` is the earlier-scanned position and `second` the later one; the
/// caller fixes that order, and this only maps the pair onto the arm.
pub fn reject_duplicate_input(
  first_field: Int,
  first_item: Int,
  second_field: Int,
  second_item: Int,
) -> RejectionReasonV1 {
  DuplicateInput {
    first_field_index: first_field,
    first_item_index: first_item,
    second_field_index: second_field,
    second_item_index: second_item,
  }
}
