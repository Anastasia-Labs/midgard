use cardano/transaction.{OutputReference}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{MembershipCarriage}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}

pub type State {
  input_with_missing_script: MidgardTxInput,
  bad_tx_id: ByteArray,
  bad_tx_witness_set_hash: ByteArray,
  prev_utxos_root: ByteArray,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  out_ref: OutputReference,
  descriptor_cbor: ByteArray,
  membership: MembershipCarriage,
}

pub type Datum =
  ct.StepDatum<State>

pub type SpendRedeemer =
  ct.StepRedeemer<Args>
