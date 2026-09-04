use midgard/computation_thread as ct
use midgard/fraud_proofs/script_integrity_hash_mismatch/rule.{
  AuthenticatedIntegrityV1,
}

pub type Args {
  input_index: Int,
  output_index: Int,
}

pub type Datum =
  ct.StepDatum<AuthenticatedIntegrityV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
