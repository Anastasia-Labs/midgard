use midgard/common/types.{Proof}
use midgard/computation_thread as ct
use midgard/ledger_state.{MidgardOutput, MidgardOutputsRoot}

pub type State {
  verified_outputs_root: MidgardOutputsRoot,
  bad_input_output_index: Int,
  output_index_to_delete: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  Recurse {
    input_index: Int,
    output_index: Int,
    output_to_delete: MidgardOutput,
    membership_proof: Proof,
    merkle_delete_script_withdraw_redeemer_index: Int,
  }
  Finalize {
    input_index: Int,
    output_index: Int,
    fraud_proof_mint_redeemer_index: Int,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
