use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/resolved_output_non_canonical/rule.{BoundInputV1}

/// `opening` opens the bound input's field. It is `None` exactly when the bound
/// coordinate names neither input field or a negative position, which no
/// opening can read.
pub type Args {
  input_index: Int,
  output_index: Int,
  opening: Option<FieldOpeningV1>,
}

pub type Datum =
  ct.StepDatum<BoundInputV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
