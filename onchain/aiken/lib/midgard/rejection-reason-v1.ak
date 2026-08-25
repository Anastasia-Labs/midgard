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
//// subject coordinates (field/item/witness/output/redeemer/execution
//// ordinals) — never expected values, hashes, or recomputable arguments
//// (catalogue design note 4).
////
//// Coordinate conventions (catalogue §5):
////   - field ordinal: the §2.5 slot index 0–8;
////   - item ordinal: 0-based position in that field's §5.1 preimage item
////     sequence (wire order, not visitation order);
////   - source kind: 0 = spend (field 0), 1 = reference (field 1);
////   - purpose coordinate: (purpose_kind, purpose_index), kind 0 = spend,
////     1 = mint, 2 = observer, 3 = receive, index 0-based within that
////     kind's canonical enumeration order;
////   - execution ordinal: 0-based position in the execution frontier
////     (discovery completion order).
////
//// This module also hosts the canonical `pub` definitions of the machine's
//// 19 `E_*` rejection-code byte constants; `validation_machine_v1` imports
//// them (identical byte values — the machine's encodings are unchanged), and
//// `rejection_code_of` is the total 47 → 19 bridge back to the frozen
//// descriptor format via `hash_rejection_code`.
////
//// A reserved family, not populated in V1: `GuardrailExceeded { guardrail,
//// field_index, item_index }` — kept unpopulated per the 2026-08-24 owner
//// ruling that the L1 forced-order door (`docs/spec/midgard-tx.md` §8.11)
//// excludes guardrail-violating preimages (catalogue §4.3 resolution,
//// design note 6); enacting it would be a format revision like any other
//// arm addition.

// ── The 19 `E_*` rejection-code constants (canonical definitions) ──────
// ASCII of the label named in each comment; values identical to the
// machine's historical private copies — the descriptor bridge
// (`hash_rejection_code`) is frozen on these bytes.

/// `E_FIELD_PREIMAGE_SIZE`
pub const reject_field_preimage_size =
  #"455f4649454c445f505245494d4147455f53495a45"

/// `E_ASSET_COUNT`
pub const reject_asset_count = #"455f41535345545f434f554e54"

/// `E_INVALID_FIELD_TYPE`
pub const reject_invalid_field_type =
  #"455f494e56414c49445f4649454c445f54595045"

/// `E_NATIVE_SCRIPT_DEPTH`
pub const reject_native_script_depth =
  #"455f4e41544956455f5343524950545f4445505448"

/// `E_NATIVE_SCRIPT_NODE_COUNT`
pub const reject_native_script_node_count =
  #"455f4e41544956455f5343524950545f4e4f44455f434f554e54"

/// `E_MIN_ADA` (#618 ruling 1, R8; decision 0003 Q27). The stable rejection
/// code the parameterized minimum-Ada floor binds to. Raised by the ValueAndMint
/// stage-3 output-descriptor step when the authenticated descriptor of one
/// transaction output does not fund
/// `min_ada_lovelace_v1(env.coins_per_utxo_byte, descriptor.total_length)` --
/// the MIN-ADA-TX violation shape of technical-spec §5. MIN-ADA-UTXO, the
/// post-block UTxO-set shape of the same floor, is Q27's own membership shape
/// and is not raised here.
pub const reject_min_ada = #"455f4d494e5f414441"

/// `E_EMPTY_INPUTS`
pub const reject_empty_inputs = #"455f454d5054595f494e50555453"

/// `E_DUPLICATE_INPUT_IN_TX`
pub const reject_duplicate_input =
  #"455f4455504c49434154455f494e5055545f494e5f5458"

/// `E_NETWORK_ID_MISMATCH`
pub const reject_network_id_mismatch =
  #"455f4e4554574f524b5f49445f4d49534d41544348"

/// `E_MIN_FEE`
pub const reject_min_fee = #"455f4d494e5f464545"

/// `E_INVALID_VALIDITY_INTERVAL_FORMAT`
pub const reject_invalid_validity_interval_format =
  #"455f494e56414c49445f56414c49444954595f494e54455256414c5f464f524d4154"

/// `E_MISSING_REQUIRED_WITNESS`
pub const reject_missing_required_witness =
  #"455f4d495353494e475f52455155495245445f5749544e455353"

/// `E_INVALID_SIGNATURE`
pub const reject_invalid_signature = #"455f494e56414c49445f5349474e4154555245"

/// `E_NATIVE_SCRIPT_INVALID`
pub const reject_native_script_invalid =
  #"455f4e41544956455f5343524950545f494e56414c4944"

/// `E_PLUTUS_SCRIPT_INVALID`
pub const reject_plutus_script_invalid =
  #"455f504c555455535f5343524950545f494e56414c4944"

/// `E_VALIDITY_INTERVAL_MISMATCH`
pub const reject_validity_interval_mismatch =
  #"455f56414c49444954595f494e54455256414c5f4d49534d41544348"

/// `E_INPUT_NOT_FOUND`
pub const reject_input_not_found = #"455f494e5055545f4e4f545f464f554e44"

/// `E_INVALID_OUTPUT`
pub const reject_invalid_output = #"455f494e56414c49445f4f5554505554"

/// `E_VALUE_NOT_PRESERVED`
pub const reject_value_not_preserved =
  #"455f56414c55455f4e4f545f505245534552564544"

/// The operator's recorded verdict on a forced-inclusion transaction, as the
/// forced leaf carries it. The sum's collapse deliberately makes "valid with
/// a reason" and "invalid without one" unrepresentable.
pub type OperatorVerdictV1 {
  ForcedTxValid
  ForcedTxInvalid { reason: RejectionReasonV1 }
}

/// The full space of operator rejection reasons — 47 constructors from the
/// machine's 19 raw codes, one per (reason, subject-space) pair. Declaration
/// order is wire-normative (catalogue §5/§6).
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

/// The total, non-injective 47 → 19 bridge back to the frozen descriptor
/// codes: every arm returns exactly the legacy `E_*` code it was split from
/// (catalogue §5.1). The descriptor bridge is
/// `hash_rejection_code(rejection_code_of(reason)) ==
/// descriptor.rejection_code_hash`.
pub fn rejection_code_of(reason: RejectionReasonV1) -> ByteArray {
  when reason is {
    FieldPreimageLengthMismatch { .. } -> reject_field_preimage_size
    FieldItemWidthIllegal { .. } -> reject_invalid_field_type
    EmptyInputs -> reject_empty_inputs
    DuplicateInput { .. } -> reject_duplicate_input
    ValidityIntervalMalformed -> reject_invalid_validity_interval_format
    NetworkIdMismatch -> reject_network_id_mismatch
    FeeBelowMinimum -> reject_min_fee
    AddressWitnessSignatureInvalid { .. } -> reject_invalid_signature
    RequiredSignerUnsigned { .. } -> reject_missing_required_witness
    WitnessScriptHeaderMalformed { .. } -> reject_invalid_field_type
    WitnessNativeScriptMalformed { .. } -> reject_invalid_field_type
    WitnessNativeScriptNodeLimit { .. } -> reject_native_script_node_count
    WitnessNativeScriptDepthLimit { .. } -> reject_native_script_depth
    WitnessNativeScriptFalse { .. } -> reject_native_script_invalid
    ScriptIntegrityHashMissing -> reject_invalid_field_type
    ObserversForbiddenOnUntaggedNetwork -> reject_invalid_field_type
    ObserverOrderInvalid { .. } -> reject_invalid_field_type
    ValidityIntervalExcludesBlockSlot -> reject_validity_interval_mismatch
    InputNotFound { .. } -> reject_input_not_found
    InputSpentOutputNonCanonical { .. } -> reject_invalid_output
    ResolvedReferenceScriptMalformed { .. } -> reject_invalid_field_type
    ResolvedReferenceScriptNodeLimit { .. } -> reject_native_script_node_count
    ResolvedReferenceScriptDepthLimit { .. } -> reject_native_script_depth
    SpendInputSignerMissing { .. } -> reject_missing_required_witness
    RedeemerMalformed { .. } -> reject_invalid_field_type
    OutputNonCanonical { .. } -> reject_invalid_output
    OutputReferenceScriptMalformed { .. } -> reject_invalid_field_type
    OutputReferenceScriptNodeLimit { .. } -> reject_native_script_node_count
    OutputReferenceScriptDepthLimit { .. } -> reject_native_script_depth
    ProtectedOutputSignerMissing { .. } -> reject_missing_required_witness
    MintDeclaredAssetLimit { .. } -> reject_asset_count
    ScriptSourceMissing { .. } -> reject_missing_required_witness
    RedeemerMissing { .. } -> reject_missing_required_witness
    UnusedScriptWitness { .. } -> reject_invalid_field_type
    UnusedRedeemer { .. } -> reject_invalid_field_type
    ExecutionNativeScriptMalformed { .. } -> reject_invalid_field_type
    ExecutionNativeScriptNodeLimit { .. } -> reject_native_script_node_count
    ExecutionNativeScriptDepthLimit { .. } -> reject_native_script_depth
    ExecutionNativeScriptFalse { .. } -> reject_native_script_invalid
    ScriptIntegrityHashMismatch -> reject_invalid_field_type
    ReceivePurposePlutusV3Forbidden { .. } -> reject_plutus_script_invalid
    PlutusExecutionFailed { .. } -> reject_plutus_script_invalid
    InputAssetAccumulationLimit { .. } -> reject_asset_count
    OutputAssetAccumulationLimit { .. } -> reject_asset_count
    MintAssetAccumulationLimit { .. } -> reject_asset_count
    OutputBelowMinAda { .. } -> reject_min_ada
    ValueNotPreserved -> reject_value_not_preserved
  }
}
