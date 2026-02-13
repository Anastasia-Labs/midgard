use midgard/common/types.{MerkleRoot, Proof}
use midgard/computation_thread as ct

pub type State {
  verified_outputs_root: MerkleRoot<Int, Output>,
  non_existent_input_output_index: Int,
  output_index_to_delete: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  Recurse {
    input_index: Int,
    output_index: Int,
    output_to_delete: Data,
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
