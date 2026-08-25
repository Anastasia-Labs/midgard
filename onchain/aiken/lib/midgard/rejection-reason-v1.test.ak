//// #640 gates 3 and 4 for `midgard/rejection_reason_v1`.
////
//// Gate 3 — totality and value of `rejection_code_of`: every one of the 47
//// constructors is enumerated and its code compared against the **imported**
//// `reject_*` constant (never a retyped literal), so a drifted constant or a
//// mis-mapped arm fails here even if both sides drift together elsewhere.
//// The constants themselves are frozen separately against their normative
//// hex (the ASCII `E_*` labels) in
//// `rejection_code_constants_are_the_frozen_bytes`.
////
//// Gate 4 — per-arm bridge identity: for every arm,
//// `hash_rejection_code(rejection_code_of(reason))` equals the frozen
//// descriptor `rejection_code_hash` for the `E_*` code that arm maps to.
//// The 19 expected hashes are pinned as hex computed **independently**
//// (blake2b-256 over `"MidgardValidationRejectCodeV1" || code`, off-chain),
//// so this pins the whole leaf → descriptor bridge, not just its Aiken
//// composition.

use midgard/rejection_reason_v1.{
  AddressWitnessSignatureInvalid, DuplicateInput, EmptyInputs,
  ExecutionNativeScriptDepthLimit, ExecutionNativeScriptFalse,
  ExecutionNativeScriptMalformed, ExecutionNativeScriptNodeLimit,
  FeeBelowMinimum, FieldItemWidthIllegal, FieldPreimageLengthMismatch,
  InputAssetAccumulationLimit, InputNotFound, InputSpentOutputNonCanonical,
  MintAssetAccumulationLimit, MintDeclaredAssetLimit, NetworkIdMismatch,
  ObserverOrderInvalid, ObserversForbiddenOnUntaggedNetwork,
  OutputAssetAccumulationLimit, OutputBelowMinAda, OutputNonCanonical,
  OutputReferenceScriptDepthLimit, OutputReferenceScriptMalformed,
  OutputReferenceScriptNodeLimit, PlutusExecutionFailed,
  ProtectedOutputSignerMissing, ReceivePurposePlutusV3Forbidden,
  RedeemerMalformed, RedeemerMissing, RequiredSignerUnsigned,
  ResolvedReferenceScriptDepthLimit, ResolvedReferenceScriptMalformed,
  ResolvedReferenceScriptNodeLimit, ScriptIntegrityHashMismatch,
  ScriptIntegrityHashMissing, ScriptSourceMissing, SpendInputSignerMissing,
  UnusedRedeemer, UnusedScriptWitness, ValidityIntervalExcludesBlockSlot,
  ValidityIntervalMalformed, ValueNotPreserved, WitnessNativeScriptDepthLimit,
  WitnessNativeScriptFalse, WitnessNativeScriptMalformed,
  WitnessNativeScriptNodeLimit, WitnessScriptHeaderMalformed, rejection_code_of,
}
use midgard/validation_trace_v1.{hash_rejection_code}

// The 19 frozen descriptor hashes, one per `E_*` code: blake2b-256 over
// `"MidgardValidationRejectCodeV1" || code`, computed off-chain.
const h_field_preimage_size =
  #"f6ecc827e4b524dd12e9ac63922f7383e015ded70a5e3fa6acae766fa4fe9b20"

const h_asset_count =
  #"d4994d00dbdb66963e0a7ede81198fa5426e40075df9b5b1c1c44c56e79f159a"

const h_invalid_field_type =
  #"75fa486cb63f3fafa27429f66f4abd86359f96fd8cc3902fd3439d26edb5a1d3"

const h_native_script_depth =
  #"ba802de85a3d98baab831ab3155478fd90711b696c3ce1479729e2271dceebdc"

const h_native_script_node_count =
  #"9320e8acd64f25974da633a482366f8e44d727807e270102179db897d551f377"

const h_min_ada =
  #"1e4cdaa8212d75203665a9900b617b98a563e84639a67a5d314d4eb33829c796"

const h_empty_inputs =
  #"fb4133ff04336d3ef299bf76f6e6b4267be9a7931285e4fbc9f848e66c3624d8"

const h_duplicate_input =
  #"2ab509665609f1128b035b5e1468031ccb68de72c06813e01b81c65c29225bf6"

const h_network_id_mismatch =
  #"8504914f12c9176501e7227c82b24b59c1b7195c5214b361624c0d99a547da42"

const h_min_fee =
  #"46055c0fdb633167ed024b753cfe257f68a4845f3843ffca361e865cbdfc2809"

const h_invalid_validity_interval_format =
  #"dd5343586670b495f5627816ac45da3b4ecbee9c430ec1ba123d17a1c0ae3513"

const h_missing_required_witness =
  #"fa3d8e6273d90770a92ff701d086e80a071676e27f73dc39aba35890e4dd813e"

const h_invalid_signature =
  #"fd5ed3d40dcfcd4b987a07c17e5ba0812d898baf6b0aab8d7f9e832532a6b63a"

const h_native_script_invalid =
  #"2774df21b393d196d960a12b5ae7efb94bf49a1e5614745f676b8658b115a0fb"

const h_plutus_script_invalid =
  #"be811cb18fca768be5bc6cc1db0961972ac0b11275caae25be36feee95bdcdc8"

const h_validity_interval_mismatch =
  #"d6c7845e4be6c2bfb721e10ce8f6fdf1f717c62c6fa0e69396858280e6a03c2a"

const h_input_not_found =
  #"7010e02d40fd5e53e846ed857627684cd0eb66ea6bc07979257063505807a187"

const h_invalid_output =
  #"3ba0dc74796540d3744f8aae143290b166c051584ba241bfe9a71c29c5bd049d"

const h_value_not_preserved =
  #"f1084e1e7329699cf7acc4a57b07331de262749506b45d023fdcc69a7c975d71"

/// The frozen byte values of the 19 canonical `E_*` constants — the ASCII of
/// each label, pinned as hex. The descriptor bridge is frozen on these bytes;
/// any drift is a consensus break, not a refactor.
test rejection_code_constants_are_the_frozen_bytes() {
  and {
    rejection_reason_v1.reject_field_preimage_size == #"455f4649454c445f505245494d4147455f53495a45",
    rejection_reason_v1.reject_asset_count == #"455f41535345545f434f554e54",
    rejection_reason_v1.reject_invalid_field_type == #"455f494e56414c49445f4649454c445f54595045",
    rejection_reason_v1.reject_native_script_depth == #"455f4e41544956455f5343524950545f4445505448",
    rejection_reason_v1.reject_native_script_node_count == #"455f4e41544956455f5343524950545f4e4f44455f434f554e54",
    rejection_reason_v1.reject_min_ada == #"455f4d494e5f414441",
    rejection_reason_v1.reject_empty_inputs == #"455f454d5054595f494e50555453",
    rejection_reason_v1.reject_duplicate_input == #"455f4455504c49434154455f494e5055545f494e5f5458",
    rejection_reason_v1.reject_network_id_mismatch == #"455f4e4554574f524b5f49445f4d49534d41544348",
    rejection_reason_v1.reject_min_fee == #"455f4d494e5f464545",
    rejection_reason_v1.reject_invalid_validity_interval_format == #"455f494e56414c49445f56414c49444954595f494e54455256414c5f464f524d4154",
    rejection_reason_v1.reject_missing_required_witness == #"455f4d495353494e475f52455155495245445f5749544e455353",
    rejection_reason_v1.reject_invalid_signature == #"455f494e56414c49445f5349474e4154555245",
    rejection_reason_v1.reject_native_script_invalid == #"455f4e41544956455f5343524950545f494e56414c4944",
    rejection_reason_v1.reject_plutus_script_invalid == #"455f504c555455535f5343524950545f494e56414c4944",
    rejection_reason_v1.reject_validity_interval_mismatch == #"455f56414c49444954595f494e54455256414c5f4d49534d41544348",
    rejection_reason_v1.reject_input_not_found == #"455f494e5055545f4e4f545f464f554e44",
    rejection_reason_v1.reject_invalid_output == #"455f494e56414c49445f4f5554505554",
    rejection_reason_v1.reject_value_not_preserved == #"455f56414c55455f4e4f545f505245534552564544",
  }
}

/// Gate 3, arms 1–24 (CanonicalDecode through ResolveInputs): the code of
/// every arm equals the imported constant the catalogue §5.1 row names.
test rejection_code_of_is_exact_for_decode_through_resolve_arms() {
  and {
    rejection_code_of(FieldPreimageLengthMismatch { field_index: 3 }) == rejection_reason_v1.reject_field_preimage_size,
    rejection_code_of(FieldItemWidthIllegal { field_index: 5, item_index: 2 }) == rejection_reason_v1.reject_invalid_field_type,
    rejection_code_of(EmptyInputs) == rejection_reason_v1.reject_empty_inputs,
    rejection_code_of(
      DuplicateInput {
        first_field_index: 0,
        first_item_index: 1,
        second_field_index: 1,
        second_item_index: 0,
      },
    ) == rejection_reason_v1.reject_duplicate_input,
    rejection_code_of(ValidityIntervalMalformed) == rejection_reason_v1.reject_invalid_validity_interval_format,
    rejection_code_of(NetworkIdMismatch) == rejection_reason_v1.reject_network_id_mismatch,
    rejection_code_of(FeeBelowMinimum) == rejection_reason_v1.reject_min_fee,
    rejection_code_of(AddressWitnessSignatureInvalid { witness_index: 4 }) == rejection_reason_v1.reject_invalid_signature,
    rejection_code_of(RequiredSignerUnsigned { signer_index: 1 }) == rejection_reason_v1.reject_missing_required_witness,
    rejection_code_of(WitnessScriptHeaderMalformed { script_index: 0 }) == rejection_reason_v1.reject_invalid_field_type,
    rejection_code_of(WitnessNativeScriptMalformed { script_index: 1 }) == rejection_reason_v1.reject_invalid_field_type,
    rejection_code_of(WitnessNativeScriptNodeLimit { script_index: 2 }) == rejection_reason_v1.reject_native_script_node_count,
    rejection_code_of(WitnessNativeScriptDepthLimit { script_index: 3 }) == rejection_reason_v1.reject_native_script_depth,
    rejection_code_of(WitnessNativeScriptFalse { script_index: 4 }) == rejection_reason_v1.reject_native_script_invalid,
    rejection_code_of(ScriptIntegrityHashMissing) == rejection_reason_v1.reject_invalid_field_type,
    rejection_code_of(ObserversForbiddenOnUntaggedNetwork) == rejection_reason_v1.reject_invalid_field_type,
    rejection_code_of(ObserverOrderInvalid { observer_index: 2 }) == rejection_reason_v1.reject_invalid_field_type,
    rejection_code_of(ValidityIntervalExcludesBlockSlot) == rejection_reason_v1.reject_validity_interval_mismatch,
    rejection_code_of(InputNotFound { source_kind: 0, input_index: 3 }) == rejection_reason_v1.reject_input_not_found,
    rejection_code_of(
      InputSpentOutputNonCanonical { source_kind: 1, input_index: 0 },
    ) == rejection_reason_v1.reject_invalid_output,
    rejection_code_of(
      ResolvedReferenceScriptMalformed { source_kind: 0, input_index: 1 },
    ) == rejection_reason_v1.reject_invalid_field_type,
    rejection_code_of(
      ResolvedReferenceScriptNodeLimit { source_kind: 1, input_index: 2 },
    ) == rejection_reason_v1.reject_native_script_node_count,
    rejection_code_of(
      ResolvedReferenceScriptDepthLimit { source_kind: 0, input_index: 0 },
    ) == rejection_reason_v1.reject_native_script_depth,
    rejection_code_of(SpendInputSignerMissing { input_index: 2 }) == rejection_reason_v1.reject_missing_required_witness,
  }
}

/// Gate 3, arms 25–47 (ScriptSources through ValueAndMint).
test rejection_code_of_is_exact_for_script_through_value_arms() {
  and {
    rejection_code_of(RedeemerMalformed { redeemer_index: 0 }) == rejection_reason_v1.reject_invalid_field_type,
    rejection_code_of(OutputNonCanonical { output_index: 1 }) == rejection_reason_v1.reject_invalid_output,
    rejection_code_of(OutputReferenceScriptMalformed { output_index: 2 }) == rejection_reason_v1.reject_invalid_field_type,
    rejection_code_of(OutputReferenceScriptNodeLimit { output_index: 3 }) == rejection_reason_v1.reject_native_script_node_count,
    rejection_code_of(OutputReferenceScriptDepthLimit { output_index: 4 }) == rejection_reason_v1.reject_native_script_depth,
    rejection_code_of(ProtectedOutputSignerMissing { output_index: 5 }) == rejection_reason_v1.reject_missing_required_witness,
    rejection_code_of(MintDeclaredAssetLimit { policy_index: 0 }) == rejection_reason_v1.reject_asset_count,
    rejection_code_of(ScriptSourceMissing { purpose_kind: 1, purpose_index: 0 }) == rejection_reason_v1.reject_missing_required_witness,
    rejection_code_of(RedeemerMissing { purpose_kind: 3, purpose_index: 1 }) == rejection_reason_v1.reject_missing_required_witness,
    rejection_code_of(UnusedScriptWitness { script_index: 5 }) == rejection_reason_v1.reject_invalid_field_type,
    rejection_code_of(UnusedRedeemer { redeemer_index: 1 }) == rejection_reason_v1.reject_invalid_field_type,
    rejection_code_of(ExecutionNativeScriptMalformed { execution_index: 0 }) == rejection_reason_v1.reject_invalid_field_type,
    rejection_code_of(ExecutionNativeScriptNodeLimit { execution_index: 1 }) == rejection_reason_v1.reject_native_script_node_count,
    rejection_code_of(ExecutionNativeScriptDepthLimit { execution_index: 2 }) == rejection_reason_v1.reject_native_script_depth,
    rejection_code_of(ExecutionNativeScriptFalse { execution_index: 3 }) == rejection_reason_v1.reject_native_script_invalid,
    rejection_code_of(ScriptIntegrityHashMismatch) == rejection_reason_v1.reject_invalid_field_type,
    rejection_code_of(ReceivePurposePlutusV3Forbidden { execution_index: 4 }) == rejection_reason_v1.reject_plutus_script_invalid,
    rejection_code_of(PlutusExecutionFailed { execution_index: 5 }) == rejection_reason_v1.reject_plutus_script_invalid,
    rejection_code_of(
      InputAssetAccumulationLimit { input_index: 0, asset_index: 7 },
    ) == rejection_reason_v1.reject_asset_count,
    rejection_code_of(
      OutputAssetAccumulationLimit { output_index: 1, asset_index: 8 },
    ) == rejection_reason_v1.reject_asset_count,
    rejection_code_of(MintAssetAccumulationLimit { mint_index: 2 }) == rejection_reason_v1.reject_asset_count,
    rejection_code_of(OutputBelowMinAda { output_index: 0 }) == rejection_reason_v1.reject_min_ada,
    rejection_code_of(ValueNotPreserved) == rejection_reason_v1.reject_value_not_preserved,
  }
}

/// Gate 4, arms 1–24: `hash_rejection_code(rejection_code_of(reason))` is the
/// frozen descriptor hash for that arm's `E_*` code, per arm, against
/// independently computed pins.
test bridge_hash_is_frozen_for_decode_through_resolve_arms() {
  and {
    hash_rejection_code(
      rejection_code_of(FieldPreimageLengthMismatch { field_index: 0 }),
    ) == h_field_preimage_size,
    hash_rejection_code(
      rejection_code_of(FieldItemWidthIllegal { field_index: 5, item_index: 2 }),
    ) == h_invalid_field_type,
    hash_rejection_code(rejection_code_of(EmptyInputs)) == h_empty_inputs,
    hash_rejection_code(
      rejection_code_of(
        DuplicateInput {
          first_field_index: 0,
          first_item_index: 1,
          second_field_index: 1,
          second_item_index: 0,
        },
      ),
    ) == h_duplicate_input,
    hash_rejection_code(rejection_code_of(ValidityIntervalMalformed)) == h_invalid_validity_interval_format,
    hash_rejection_code(rejection_code_of(NetworkIdMismatch)) == h_network_id_mismatch,
    hash_rejection_code(rejection_code_of(FeeBelowMinimum)) == h_min_fee,
    hash_rejection_code(
      rejection_code_of(AddressWitnessSignatureInvalid { witness_index: 0 }),
    ) == h_invalid_signature,
    hash_rejection_code(
      rejection_code_of(RequiredSignerUnsigned { signer_index: 0 }),
    ) == h_missing_required_witness,
    hash_rejection_code(
      rejection_code_of(WitnessScriptHeaderMalformed { script_index: 0 }),
    ) == h_invalid_field_type,
    hash_rejection_code(
      rejection_code_of(WitnessNativeScriptMalformed { script_index: 1 }),
    ) == h_invalid_field_type,
    hash_rejection_code(
      rejection_code_of(WitnessNativeScriptNodeLimit { script_index: 2 }),
    ) == h_native_script_node_count,
    hash_rejection_code(
      rejection_code_of(WitnessNativeScriptDepthLimit { script_index: 3 }),
    ) == h_native_script_depth,
    hash_rejection_code(
      rejection_code_of(WitnessNativeScriptFalse { script_index: 4 }),
    ) == h_native_script_invalid,
    hash_rejection_code(rejection_code_of(ScriptIntegrityHashMissing)) == h_invalid_field_type,
    hash_rejection_code(rejection_code_of(ObserversForbiddenOnUntaggedNetwork)) == h_invalid_field_type,
    hash_rejection_code(
      rejection_code_of(ObserverOrderInvalid { observer_index: 1 }),
    ) == h_invalid_field_type,
    hash_rejection_code(rejection_code_of(ValidityIntervalExcludesBlockSlot)) == h_validity_interval_mismatch,
    hash_rejection_code(
      rejection_code_of(InputNotFound { source_kind: 0, input_index: 0 }),
    ) == h_input_not_found,
    hash_rejection_code(
      rejection_code_of(
        InputSpentOutputNonCanonical { source_kind: 1, input_index: 1 },
      ),
    ) == h_invalid_output,
    hash_rejection_code(
      rejection_code_of(
        ResolvedReferenceScriptMalformed { source_kind: 0, input_index: 2 },
      ),
    ) == h_invalid_field_type,
    hash_rejection_code(
      rejection_code_of(
        ResolvedReferenceScriptNodeLimit { source_kind: 1, input_index: 3 },
      ),
    ) == h_native_script_node_count,
    hash_rejection_code(
      rejection_code_of(
        ResolvedReferenceScriptDepthLimit { source_kind: 0, input_index: 4 },
      ),
    ) == h_native_script_depth,
    hash_rejection_code(
      rejection_code_of(SpendInputSignerMissing { input_index: 5 }),
    ) == h_missing_required_witness,
  }
}

/// Gate 4, arms 25–47.
test bridge_hash_is_frozen_for_script_through_value_arms() {
  and {
    hash_rejection_code(
      rejection_code_of(RedeemerMalformed { redeemer_index: 0 }),
    ) == h_invalid_field_type,
    hash_rejection_code(
      rejection_code_of(OutputNonCanonical { output_index: 1 }),
    ) == h_invalid_output,
    hash_rejection_code(
      rejection_code_of(OutputReferenceScriptMalformed { output_index: 2 }),
    ) == h_invalid_field_type,
    hash_rejection_code(
      rejection_code_of(OutputReferenceScriptNodeLimit { output_index: 3 }),
    ) == h_native_script_node_count,
    hash_rejection_code(
      rejection_code_of(OutputReferenceScriptDepthLimit { output_index: 4 }),
    ) == h_native_script_depth,
    hash_rejection_code(
      rejection_code_of(ProtectedOutputSignerMissing { output_index: 5 }),
    ) == h_missing_required_witness,
    hash_rejection_code(
      rejection_code_of(MintDeclaredAssetLimit { policy_index: 6 }),
    ) == h_asset_count,
    hash_rejection_code(
      rejection_code_of(
        ScriptSourceMissing { purpose_kind: 0, purpose_index: 0 },
      ),
    ) == h_missing_required_witness,
    hash_rejection_code(
      rejection_code_of(RedeemerMissing { purpose_kind: 2, purpose_index: 1 }),
    ) == h_missing_required_witness,
    hash_rejection_code(
      rejection_code_of(UnusedScriptWitness { script_index: 0 }),
    ) == h_invalid_field_type,
    hash_rejection_code(rejection_code_of(UnusedRedeemer { redeemer_index: 1 })) == h_invalid_field_type,
    hash_rejection_code(
      rejection_code_of(ExecutionNativeScriptMalformed { execution_index: 0 }),
    ) == h_invalid_field_type,
    hash_rejection_code(
      rejection_code_of(ExecutionNativeScriptNodeLimit { execution_index: 1 }),
    ) == h_native_script_node_count,
    hash_rejection_code(
      rejection_code_of(ExecutionNativeScriptDepthLimit { execution_index: 2 }),
    ) == h_native_script_depth,
    hash_rejection_code(
      rejection_code_of(ExecutionNativeScriptFalse { execution_index: 3 }),
    ) == h_native_script_invalid,
    hash_rejection_code(rejection_code_of(ScriptIntegrityHashMismatch)) == h_invalid_field_type,
    hash_rejection_code(
      rejection_code_of(ReceivePurposePlutusV3Forbidden { execution_index: 4 }),
    ) == h_plutus_script_invalid,
    hash_rejection_code(
      rejection_code_of(PlutusExecutionFailed { execution_index: 5 }),
    ) == h_plutus_script_invalid,
    hash_rejection_code(
      rejection_code_of(
        InputAssetAccumulationLimit { input_index: 0, asset_index: 1 },
      ),
    ) == h_asset_count,
    hash_rejection_code(
      rejection_code_of(
        OutputAssetAccumulationLimit { output_index: 2, asset_index: 3 },
      ),
    ) == h_asset_count,
    hash_rejection_code(
      rejection_code_of(MintAssetAccumulationLimit { mint_index: 4 }),
    ) == h_asset_count,
    hash_rejection_code(
      rejection_code_of(OutputBelowMinAda { output_index: 6 }),
    ) == h_min_ada,
    hash_rejection_code(rejection_code_of(ValueNotPreserved)) == h_value_not_preserved,
  }
}
