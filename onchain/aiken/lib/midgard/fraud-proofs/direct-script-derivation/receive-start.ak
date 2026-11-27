use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}

pub type Args {
  input_index: Int,
  output_index: Int,
  outputs_opening: FieldOpeningV1,
}

pub type Datum =
  ct.StepDatum<BoundIndexV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
