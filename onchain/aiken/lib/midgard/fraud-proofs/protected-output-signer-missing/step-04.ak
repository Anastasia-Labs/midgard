use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/protected_output_signer_missing/rule.{WitnessScanV1}

pub type Datum =
  ct.StepDatum<WitnessScanV1>

pub type Args {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  checkpoint_cbor: ByteArray,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
