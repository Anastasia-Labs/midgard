use cardano/transaction.{OutputReference}
use midgard/common/ledger_state.{MidgardTx, MidgardTxBody}

pub type StepDatum {
  fraud_prover: ByteArray,
  data: Data,
}

pub type MembershipProofInputs {
  root: ByteArray,
  tx_hash: ByteArray,
  tx_info_hash: ByteArray,
  proof_path: ByteArray,
}

pub type InvalidInputSpendStep1State {
  tx_hash: ByteArray,
  proof_path: ByteArray,
}

pub type InvalidInputSpendStep2State {
  tx_hash: ByteArray,
  input: OutputReference,
}

//verify that given the array of inputs I get the hash that is actually written inside the Block
//the input is part of it

pub type Step2redeemer {
  Cancel { index_output: Int }
  Continue {
    bad_input: Data,
    input_utxos: Data,
    index_output: Int,
    tx_body: MidgardTxBody,
    tx: MidgardTx,
  }
}
