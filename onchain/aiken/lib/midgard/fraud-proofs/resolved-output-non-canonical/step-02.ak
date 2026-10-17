use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/resolved_output_non_canonical/rule.{BoundInputV1}

pub type Args {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
}

pub type Datum =
  ct.StepDatum<BoundInputV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
