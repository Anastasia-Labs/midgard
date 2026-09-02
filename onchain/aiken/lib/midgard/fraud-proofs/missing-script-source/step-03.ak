use midgard/computation_thread as ct
use midgard/fraud_proofs/missing_script_source/rule.{AuthenticatedTraceStateV1}
use midgard/validation_machine_v1.{ScriptSourcesControlV1}

pub type Args {
  input_index: Int,
  output_index: Int,
  control: ScriptSourcesControlV1,
  absolute_purpose_index: Int,
  required_script_hash: ByteArray,
  purpose_subject: ByteArray,
  purpose_siblings: List<ByteArray>,
  transaction_source_count: Int,
}

pub type Datum =
  ct.StepDatum<AuthenticatedTraceStateV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
