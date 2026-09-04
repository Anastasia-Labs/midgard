use midgard/computation_thread as ct
use midgard/fraud_proofs/execution_native_script_invalid/accepted_reconstruction.{
  StateV1,
}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}

pub type Action {
  ScanInline {
    input_index: Int,
    output_index: Int,
    scripts_opening: FieldOpeningV1,
  }
  FinishInline {
    input_index: Int,
    output_index: Int,
    scripts_opening: FieldOpeningV1,
  }
}

pub type Datum =
  ct.StepDatum<StateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Action>
