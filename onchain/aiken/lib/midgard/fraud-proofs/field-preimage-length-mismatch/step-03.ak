use midgard/computation_thread as ct
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}

pub type State {
  subject: VerdictSubjectV1,
  field_index: Int,
  declared_length: Int,
  actual_length: Int,
}

pub type Datum =
  ct.StepDatum<State>

pub type Args {
  input_index: Int,
  output_index: Int,
  fraud_proof_mint_redeemer_index: Int,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
