use cardano/transaction.{OutputReference, TransactionId}
use midgard/ledger_state.{MidgardTxBodyCompact, MidgardTxCompact}

pub type StepRedeemer {
  Cancel { index_output: Int }
  Continue {
    fraudulent_input: Data,
    tx1_input_utxos: ByteArray,
    tx1_body: MidgardTxBodyCompact,
    tx2_input_utxos: ByteArray,
    tx2_body: MidgardTxBodyCompact,
    tx1: MidgardTxCompact,
    tx2: MidgardTxCompact,
    index_output: Int,
  }
}

pub type StepState {
  verified_tx1_hash: TransactionId,
  verified_tx2_hash: TransactionId,
  verified_common_input: OutputReference,
}
