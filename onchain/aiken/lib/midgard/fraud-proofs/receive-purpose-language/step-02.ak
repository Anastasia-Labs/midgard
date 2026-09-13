use midgard/computation_thread as ct
use midgard/fraud_proofs/receive_purpose_language/rule.{BoundExecutionV1}
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
  purpose_index: Int,
  script_hash: ByteArray,
  purpose_subject: ByteArray,
  purpose_siblings: List<ByteArray>,
  source_index: Int,
  origin_kind: Int,
  source_key: ByteArray,
  language_tag: Int,
  total_length: Int,
  item_commitment: ByteArray,
  source_siblings: List<ByteArray>,
  redeemer_leaf: ByteArray,
  execution_siblings: List<ByteArray>,
}

pub type Datum =
  ct.StepDatum<BoundExecutionV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
