use aiken/crypto.{Blake2b_224, Hash}
use midgard/common/types.{MerkleRoot, POSIXTime, PubKeyHash}

pub type HeaderHash =
  Hash<Blake2b_224, Header>

pub type Header {
  prev_utxos_root: MerkleRoot,
  utxos_root: MerkleRoot,
  transactions_root: MerkleRoot,
  deposits_root: MerkleRoot,
  withdrawals_root: MerkleRoot,
  start_time: POSIXTime,
  end_time: POSIXTime,
  prev_header_hash: HeaderHash,
  operator_vkey: PubKeyHash,
  protocol_version: Int,
}

pub type ConfirmedState {
  header_hash: HeaderHash,
  prev_header_hash: HeaderHash,
  utxo_root: MerkleRoot,
  start_time: POSIXTime,
  end_time: POSIXTime,
  protocol_version: Int,
}

// TODO:
// Some helper types for handling transaction inputs in the fraud proof
// pub type TxInput =
//   Data

// pub type TxInputs =
//   List<TxInput>

// TODO:
pub type MidgardTx {
  body: ByteArray,
  wits: ByteArray,
  is_valid: Bool,
}

// TODO:
pub type MidgardTxBody {
  spend_inputs: ByteArray,
}
