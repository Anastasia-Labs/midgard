use cardano/transaction.{OutputReference}
use midgard/common/types.{Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardInputs, MidgardInputsHash}

pub type State {
  verified_tx2_inputs_hash: MidgardInputsHash,
  double_spent_input_outref: OutputReference,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
  tx2_inputs_preimage: MidgardInputs,
  double_spent_input_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
