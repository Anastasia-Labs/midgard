use midgard/computation_thread as ct
use midgard/fraud_proofs/unused_script_witness/rule.{
  BoundWitnessV1, TerminalScriptSourcesWitnessV1,
}
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
  machine_state: validation_trace_v1.ValidationMachineStateV1,
  trace_proof: validation_trace_v1.ValidationTraceProof,
  control: TerminalScriptSourcesWitnessV1,
  language_tag: Int,
  script_hash: ByteArray,
  total_length: Int,
  item_commitment: ByteArray,
  source_siblings: List<ByteArray>,
}

pub type Datum =
  ct.StepDatum<BoundWitnessV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
