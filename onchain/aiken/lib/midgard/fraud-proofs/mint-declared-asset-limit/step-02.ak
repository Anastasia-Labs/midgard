use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/mint_declared_asset_limit/state.{AuthenticationStateV1}

pub type ActionV1 {
  AuthenticateDirect {
    input_index: Int,
    output_index: Int,
    opening: FieldOpeningV1,
  }
  StartGrammar {
    input_index: Int,
    output_index: Int,
    opening: FieldOpeningV1,
    item_budget: Int,
  }
  ResumeGrammar {
    input_index: Int,
    output_index: Int,
    opening: FieldOpeningV1,
    checkpoint_bytes: ByteArray,
    item_budget: Int,
  }
  FinishGrammar {
    input_index: Int,
    output_index: Int,
    opening: FieldOpeningV1,
    checkpoint_bytes: ByteArray,
  }
}

pub type Datum =
  ct.StepDatum<AuthenticationStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>
