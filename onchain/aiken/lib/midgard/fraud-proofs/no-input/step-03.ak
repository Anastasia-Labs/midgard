use midgard/computation_thread as ct
use midgard/fraud_proofs/input_existence_v1/types

pub type State =
  types.SelectedInputStateV1

pub type Args =
  types.PreStateArgsV1

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
