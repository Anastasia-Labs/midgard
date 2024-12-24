use aiken/crypto.{VerificationKey}
use cardano/assets.{Value}
use midgard/common/types.{
  Coin, HashedBody, HashedLisOutputs, HashedListReferenceOutputs, HashedScripts,
  HashedVerificationKeys, HashedWitness, HeaderHash, MidgardSValidityInterval,
  Network, POSIXTime, RootHash,
}

pub type StateQueueDatum {
  key: Option<HeaderHash>,
  next: Option<HeaderHash>,
  data: Header,
}

pub type Block {
  header_hash: HeaderHash,
  header: Header,
  block_body: BlockBody,
}

pub type Header {
  prev_utxos_root: RootHash,
  utxos_root: RootHash,
  transactions_root: RootHash,
  deposits_root: RootHash,
  withdrawals_root: RootHash,
  start_time: POSIXTime,
  end_time: POSIXTime,
  prev_header_hash: HeaderHash,
  operator_vkey: VerificationKey,
  protocol_version: Int,
}

pub type BlockBody {
  BlockBody
}

//TODO

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
