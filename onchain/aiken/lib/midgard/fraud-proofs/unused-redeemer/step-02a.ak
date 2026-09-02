use midgard/computation_thread as ct
use midgard/fraud_proofs/unused_redeemer/rule.{AuthenticatedDescriptorV1}
use midgard/validation_machine_v1
use midgard/validation_trace_v1

pub type Args {
  input_index: Int,
  output_index: Int,
  machine_state: validation_trace_v1.ValidationMachineStateV1,
  trace_proof: validation_trace_v1.ValidationTraceProof,
  control: validation_machine_v1.ScriptSourcesControlV1,
}

pub type Datum =
  ct.StepDatum<AuthenticatedDescriptorV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
