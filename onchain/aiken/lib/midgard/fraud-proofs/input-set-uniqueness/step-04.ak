use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/input_set_uniqueness/rule.{UniqueScanStateV1}

pub type Datum =
  ct.StepDatum<UniqueScanStateV1>

pub type Args {
  Advance { input_index: Int, output_index: Int, field_opening: FieldOpeningV1 }
  Finalize {
    input_index: Int,
    output_index: Int,
    fraud_proof_mint_redeemer_index: Int,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
