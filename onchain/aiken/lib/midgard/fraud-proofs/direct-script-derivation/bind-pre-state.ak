use midgard/computation_thread as ct
use midgard/fraud_proofs/counted_membership_carriage_v1.{
  CarriedCountedMembershipV1,
}
use midgard/fraud_proofs/direct_script_derivation/bind_state.{EventSelectedV1}
use midgard/ledger_state

pub type Args {
  input_index: Int,
  output_index: Int,
  transition_opening: CarriedCountedMembershipV1<
    Int,
    ledger_state.TransitionStep,
  >,
}

pub type Datum =
  ct.StepDatum<EventSelectedV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
