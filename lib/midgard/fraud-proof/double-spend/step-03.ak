use cardano/transaction.{OutputReference}
use midgard/ledger_state.{MidgardTx, MidgardTxBody}

pub type StepRedeemer {
  Cancel { index_output: Int }
  Continue {
    fraudulent_input: Data,
    tx1_input_utxos: ByteArray,
    tx1_body: MidgardTxBody,
    tx2_input_utxos: ByteArray,
    tx2_body: MidgardTxBody,
    tx1: MidgardTx,
    tx2: MidgardTx,
    index_output: Int,
  }
}

pub type StepState {
  verified_tx1_hash: ByteArray,
  verified_tx2_hash: ByteArray,
  verified_common_input: OutputReference,
}
