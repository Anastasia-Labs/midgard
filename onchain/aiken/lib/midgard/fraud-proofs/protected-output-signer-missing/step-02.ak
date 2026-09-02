use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/protected_output_signer_missing/rule.{BoundOutputV1}

pub type State {
  bound: BoundOutputV1,
  witness_set_hash: ByteArray,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
