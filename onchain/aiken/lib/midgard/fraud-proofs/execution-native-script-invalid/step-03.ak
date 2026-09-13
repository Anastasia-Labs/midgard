use midgard/computation_thread as ct
use midgard/fraud_proofs/execution_native_script_invalid/rule.{
  AuthenticatedExecutionSourceV1,
}

pub type State {
  direction: Int,
  execution_index: Int,
  source_index: Int,
  origin_kind: Int,
  item_commitment: ByteArray,
  bad_tx_id: ByteArray,
  bad_tx_witness_set_hash: ByteArray,
  validity_interval_start: Int,
  validity_interval_end: Int,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  script_item_cbor: ByteArray,
}

pub type Datum =
  ct.StepDatum<AuthenticatedExecutionSourceV1>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
