use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/receive.{
  DeclarationStateV1, DeclaredCredentialV1,
}

pub type Action {
  Scan {
    input_index: Int,
    output_index: Int,
    entries: List<DeclaredCredentialV1>,
  }
  Finish { input_index: Int, output_index: Int }
}

pub type Datum =
  ct.StepDatum<DeclarationStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Action>

pub fn indices(action: Action) -> Pair<Int, Int> {
  when action is {
    Scan { input_index, output_index, .. } -> Pair(input_index, output_index)
    Finish { input_index, output_index } -> Pair(input_index, output_index)
  }
}
