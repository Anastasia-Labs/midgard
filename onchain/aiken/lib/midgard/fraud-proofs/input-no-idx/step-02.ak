use midgard/common/types.{Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardInputId, MidgardInputsRoot, MidgardOutput}

pub type State {
  verified_tx_inputs_root: MidgardInputsRoot,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  bad_input_id: MidgardInputId,
  bad_input_resolved: MidgardOutput,
  bad_input_membership_proof: Proof,
  inclusion_proof_script_withdraw_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
