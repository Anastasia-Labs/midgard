use midgard/computation_thread as ct
use midgard/fraud_proofs/distinct_asset_accumulation_limit/rule.{BoundV1}
use midgard/ledger_state
use midgard/transition_trace
use midgard/validation_machine_v1.{ValueAndMintControlV1}
use midgard/validation_trace_v1

pub type Args {
  input_index: Int,
  output_index: Int,
  trace_membership: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    validation_trace_v1.ValidationTraceDescriptorV1,
  >,
  pre: validation_trace_v1.ValidationMachineStateV1,
  trace_proof: validation_trace_v1.ValidationTraceProof,
  control: ValueAndMintControlV1,
}

pub type Datum =
  ct.StepDatum<BoundV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
