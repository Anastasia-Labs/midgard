use midgard/computation_thread as ct
use midgard/fraud_proofs/unused_redeemer/rule.{AuthenticatedDescriptorV1}
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_trace_v1

pub type Args {
  input_index: Int,
  output_index: Int,
  machine_state: validation_trace_v1.ValidationMachineStateV1,
  trace_proof: validation_trace_v1.ValidationTraceProof,
  control: vm_machine_types.ScriptSourcesControlV1,
}

pub type Datum =
  ct.StepDatum<AuthenticatedDescriptorV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
