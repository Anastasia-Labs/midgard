use midgard/common/types.{Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardInputs, MidgardInputsHash}

pub type State {
  verified_tx1_inputs_hash: MidgardInputsHash,
  verified_tx2_inputs_hash: MidgardInputsHash,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  tx1_inputs_preimage: MidgardInputs,
  double_spent_input_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
