use midgard/computation_thread as ct

pub type State {
  verified_tx1_spend_inputs_hash: ByteArray,
  verified_tx2_spend_inputs_hash: ByteArray,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  tx1_spend_inputs_ref_input_index: Int,
  double_spent_input_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
