use midgard/computation_thread as ct
use midgard/fraud_proofs/missing_redeemer/rule.{BoundPurposeV1}
use midgard/ledger_state
use midgard/transition_trace
use midgard/validation_trace_v1

pub type Args {
  input_index: Int,
  output_index: Int,
  trace_membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    validation_trace_v1.ValidationTraceDescriptorV1,
  >,
}

pub type Datum =
  ct.StepDatum<BoundPurposeV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
