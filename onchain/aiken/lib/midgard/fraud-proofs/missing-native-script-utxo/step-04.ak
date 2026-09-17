use cardano/transaction.{OutputReference}
use midgard/computation_thread as ct

pub type State {
  out_ref: OutputReference,
  descriptor_cbor: ByteArray,
  bad_tx_id: ByteArray,
  bad_tx_witness_set_hash: ByteArray,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  missing_native_script_bytes: ByteArray,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
