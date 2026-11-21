use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/script_integrity_hash_missing/state.{StagedStateV1}

pub type Datum =
  ct.StepDatum<StagedStateV1>

pub type Args {
  Start {
    input_index: Int,
    output_index: Int,
    opening: FieldOpeningV1,
    item_budget: Int,
  }
  Resume {
    input_index: Int,
    output_index: Int,
    opening: FieldOpeningV1,
    checkpoint_bytes: ByteArray,
    item_budget: Int,
  }
  Finish {
    input_index: Int,
    output_index: Int,
    opening: FieldOpeningV1,
    checkpoint_bytes: ByteArray,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
