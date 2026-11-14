use midgard/computation_thread as ct
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/observer_order_invalid/state.{AuthenticationStateV1}

pub type ActionV1 {
  Authenticate { input_index: Int, output_index: Int, opening: FieldOpeningV1 }
  Reserved
}

pub type Datum =
  ct.StepDatum<AuthenticationStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<ActionV1>
