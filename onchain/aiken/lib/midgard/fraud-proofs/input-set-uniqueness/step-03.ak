use midgard/computation_thread as ct
use midgard/fraud_proofs/input_set_uniqueness/rule.{BoundDuplicateInputV1}
use midgard/native_tx_field_access_v1.{FieldCarriageV1}

pub type State {
  bound: BoundDuplicateInputV1,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  native_tx_compact_cbor: ByteArray,
  spend_inputs_carriage: FieldCarriageV1,
  reference_inputs_carriage: FieldCarriageV1,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
