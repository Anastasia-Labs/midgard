use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/receive.{
  OutputStateV1, OutputWitnessV1,
}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}

pub type Action {
  Scan {
    input_index: Int,
    output_index: Int,
    outputs_opening: FieldOpeningV1,
    checkpoint_bytes: ByteArray,
    witnesses: List<OutputWitnessV1>,
  }
  Finish { input_index: Int, output_index: Int }
}

pub type Datum =
  ct.StepDatum<OutputStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Action>

pub fn indices(action: Action) -> Pair<Int, Int> {
  when action is {
    Scan { input_index, output_index, .. } -> Pair(input_index, output_index)
    Finish { input_index, output_index } -> Pair(input_index, output_index)
  }
}
