use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/spend_input_signer_missing/rule.{
  AuthenticatedCredentialV1,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  witnesses_opening: FieldOpeningV1,
}

pub type Datum =
  ct.StepDatum<AuthenticatedCredentialV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
