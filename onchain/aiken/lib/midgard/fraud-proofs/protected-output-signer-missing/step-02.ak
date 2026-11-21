use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/protected_output_signer_missing/rule.{BoundOutputV1}

pub type State {
  bound: BoundOutputV1,
  witness_set_hash: ByteArray,
}

pub type Datum =
  ct.StepDatum<State>

/// `coordinate_out_of_range` names the direct arm that needs the field's
/// authenticated item count. Walking field 2, which is variable-width,
/// answers that count only where the whole preimage is present (tiers 1–2);
/// the arm is named by the redeemer because consulting the count on the
/// credential arms would abort every certified opening. The validator still
/// decides the claim from the authenticated field, never from the flag alone.
///
/// `grammar_ref_input_index`: `None` walks field 2 for the count or the
/// bound output; `Some(i)` reads both through the field grammar certificate
/// at reference input `i`, so the certified count also serves a chunked
/// field and the step's cost no longer grows with the field's item count.
pub type Args {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  coordinate_out_of_range: Bool,
  grammar_ref_input_index: Option<Int>,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
