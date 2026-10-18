use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{MembershipCarriage}
use midgard/fraud_proofs/execution_native_script_invalid/accepted_reconstruction.{
  StateV1,
}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/native_tx_field_access_v1.{FieldCarriageV1}

/// Resolved-output source discovery over the spend inputs (field 0) and the
/// reference inputs (field 1).
pub type Action {
  /// Walk the next resolved output in canonical out-ref order: item
  /// `item_index` of field `field_index`, with its ledger descriptor.
  ScanResolvedOutput {
    input_index: Int,
    output_index: Int,
    field_index: Int,
    item_index: Int,
    inputs_opening: FieldOpeningV1,
    descriptor_cbor: ByteArray,
    membership: MembershipCarriage,
  }
  /// Prove both fields walked and hand the selected source to step 03.
  FinishResolvedOutputs {
    input_index: Int,
    output_index: Int,
    native_tx_compact_cbor: ByteArray,
    spend_inputs_carriage: FieldCarriageV1,
    reference_inputs_carriage: FieldCarriageV1,
  }
}

pub type Datum =
  ct.StepDatum<StateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Action>
