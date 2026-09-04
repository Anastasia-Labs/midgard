use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/missing_redeemer/rule.{
  AuthenticatedPurposeV1, ScanStateV1,
}

pub type AuthenticationStateV1 {
  Ready { authenticated: AuthenticatedPurposeV1 }
  Grammar { authenticated: AuthenticatedPurposeV1, checkpoint_hash: ByteArray }
}

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

pub type OutputStateV1 =
  ScanStateV1
