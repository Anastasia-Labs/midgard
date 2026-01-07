use midgard/common/types.{MerkleRoot}
use midgard/computation_thread as ct

pub type State {
  verified_outputs_root: MerkleRoot,
  non_existent_input_output_index: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
