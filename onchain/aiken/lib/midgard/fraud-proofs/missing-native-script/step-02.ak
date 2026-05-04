use midgard/common/types.{Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{
  MidgardOutput, MidgardOutputsRoot, MidgardScriptWitsRoot,
}

pub type State {
  bad_tx_outputs_root: MidgardOutputsRoot,
  bad_tx_script_wits: MidgardScriptWitsRoot,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  bad_output_index: Int,
  bad_output: MidgardOutput,
  bad_output_membership_proof: Proof,
  missing_native_script_bytes: ByteArray,
  inclusion_proof_script_withdraw_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
