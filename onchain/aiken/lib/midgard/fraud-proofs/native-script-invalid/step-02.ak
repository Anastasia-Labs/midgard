use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}

pub type State {
  bad_tx_id: ByteArray,
  bad_tx_witness_set_hash: ByteArray,
  validity_interval_start: Int,
  validity_interval_end: Int,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  script_index: Int,
  script_tx_wits_opening: FieldOpeningV1,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
