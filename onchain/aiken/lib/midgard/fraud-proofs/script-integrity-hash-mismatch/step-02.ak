use midgard/computation_thread as ct
use midgard/fraud_proofs/script_integrity_hash_mismatch/rule.{BoundIntegrityV1}
use midgard/ledger_state
use midgard/transition_trace
use midgard/validation_machine/machine_types.{NativeScriptsControlV1}
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
  control: NativeScriptsControlV1,
  redeemer_witness_hash: ByteArray,
}

pub type Datum =
  ct.StepDatum<BoundIntegrityV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
