use midgard/computation_thread as ct
use midgard/fraud_proofs/field_item_width_illegal/rule.{BoundCoordinateV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}

pub type Datum =
  ct.StepDatum<BoundCoordinateV1>

pub type Args {
  input_index: Int,
  output_index: Int,
  opening: Option<FieldOpeningV1>,
  /// Fields 2 and 5 only: `None` authenticates the count and the item's width
  /// by walking the field; `Some(i)` takes both from the field grammar
  /// certificate at reference input `i`, so the step's cost no longer grows
  /// with the field's item count.
  grammar_ref_input_index: Option<Int>,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
