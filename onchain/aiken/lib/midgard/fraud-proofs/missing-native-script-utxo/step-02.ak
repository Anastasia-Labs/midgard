use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}

pub type State {
  bad_tx_id: ByteArray,
  bad_tx_witness_set_hash: ByteArray,
  prev_utxos_root: ByteArray,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  bad_input_index: Int,
  spend_inputs_opening: FieldOpeningV1,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
