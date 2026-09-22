use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/protected_output_signer_missing/rule.{BoundOutputV1}

pub type State {
  bound: BoundOutputV1,
  witness_set_hash: ByteArray,
}

pub type Datum =
  ct.StepDatum<State>

/// `coordinate_out_of_range` names the direct arm that needs the view's
/// authenticated item count. Field 2 is variable-width, so that count is
/// answerable only where the whole preimage is present (tiers 1–2); the arm
/// is named by the redeemer because consulting the count on the credential
/// arms would abort every certified opening. The validator still decides
/// the claim from the authenticated view, never from the flag alone.
pub type Args {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  coordinate_out_of_range: Bool,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
