use cardano/transaction.{OutputReference}
use midgard/common/types.{MerkleRoot}
use midgard/computation_thread as ct

pub type State {
  bad_tx_body_spend_inputs: MerkleRoot<Int, OutputReference>,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
