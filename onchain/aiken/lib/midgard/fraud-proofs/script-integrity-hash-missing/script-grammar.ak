use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/script_integrity_hash_missing/state.{StagedStateV1}

pub type Datum =
  ct.StepDatum<StagedStateV1>

pub type Args {
  Resume {
    input_index: Int,
    output_index: Int,
    opening: FieldOpeningV1,
    checkpoint_bytes: ByteArray,
    item_budget: Int,
  }
  StartScan {
    input_index: Int,
    output_index: Int,
    opening: FieldOpeningV1,
    checkpoint_bytes: ByteArray,
    item_budget: Int,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
