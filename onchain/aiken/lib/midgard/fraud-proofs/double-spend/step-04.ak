use midgard/computation_thread as ct
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}

pub type State {
  verified_tx2_spend_inputs_hash: ByteArray,
  double_spent_input: MidgardTxInput,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  tx2_spend_inputs_ref_input_index: Int,
  double_spent_input_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
