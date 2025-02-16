use cardano/assets.{Value}
use midgard/common/types.{
  Coin, HashedBody, HashedLisOutputs, HashedListReferenceOutputs, HashedScripts,
  HashedVerificationKeys, HashedWitness, HeaderHash, MerkleRoot,
  MidgardSValidityInterval, Network, POSIXTime, PubKeyHash,
}

pub type Header {
  prev_utxos_root: MerkleRoot,
  utxos_root: MerkleRoot,
  transactions_root: MerkleRoot,
  deposits_root: MerkleRoot,
  withdrawals_root: MerkleRoot,
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
}

// TODO:
// Some helper types for handling transaction inputs in the fraud proof
// pub type TxInput =
//   Data

// pub type TxInputs =
//   List<TxInput>
pub type Block {
  header_hash: HeaderHash,
  header: Header,
  block_body: BlockBody,
}

pub type BlockBody {
  BlockBody
}

pub type MidgardTxBody {
  spend_inputs: HashedListReferenceOutputs,
  reference_inputs: Option<HashedListReferenceOutputs>,
  outputs: HashedLisOutputs,
  fee: Coin,
  validity_interval: Option<MidgardSValidityInterval>,
  required_observer: Option<HashedScripts>,
  required_signer_hashes: Option<HashedVerificationKeys>,
  mint: Option<Value>,
  script_integrity_hash: Option<ByteArray>,
  auxiliary_data_hash: Option<ByteArray>,
  network_id: Option<Network>,
}

pub type MidgardTx {
  body: HashedBody,
  wits: HashedWitness,
  is_valid: Bool,
}
