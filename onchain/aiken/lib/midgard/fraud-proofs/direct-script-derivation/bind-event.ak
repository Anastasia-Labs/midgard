use midgard/computation_thread as ct
use midgard/fraud_proofs/counted_membership_carriage_v1.{
  CarriedCountedMembershipV1,
}
use midgard/fraud_proofs/direct_script_derivation/types.{SourceBoundV1}
use midgard/ledger_state

pub type Args {
  input_index: Int,
  output_index: Int,
  event_opening: CarriedCountedMembershipV1<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
}

pub type Datum =
  ct.StepDatum<SourceBoundV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
