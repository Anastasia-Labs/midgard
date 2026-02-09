//// Midgard's ledger state types are described in the specification (§ 1).
//// Midgard uses two distinct data representations for ledger state:
////
////   - The *full* representation contains complete data structures.
////     It is stored offchain and only small parts of it may be occasionally
////     submitted onchain during fraud-proof verification.
////     The full representation sometimes includes fields that are compact
////     representations of its other variable-sized fields.
////   - The *compact* representation is a fixed-sized data structure derived from
////     the full representation by abbreviating all variable-sized fields.
////     Other than in fraud-proof scripts, all onchain scripts must use the
////     compact representation.
////
//// These types must correspond to the CBOR types defined in Midgard's
//// Codec Standard (/codec.cddl).

use aiken/builtin
use aiken/collection/dict.{Dict}
use aiken/crypto.{
  Blake2b_256, ScriptHash, Signature, VerificationKey, VerificationKeyHash,
}
use cardano/address.{Address}
use cardano/assets.{AssetName, Lovelace, PolicyId}
use cardano/transaction.{
  Datum as CardanoDatum, Output, OutputReference, Redeemer, ScriptPurpose,
  ValidityRange,
}
use midgard/common/types.{H28, H32, MerkleRoot, PosixTime, ValuePairs}

// # Ledger state types

// ## Block

/// Midgard block.
/// Full representation.
pub type Block {
  header_hash: HeaderHash,
  header: Header,
  block_body: BlockBody,
}

/// Midgard block header hash.
/// Compact representation.
pub type HeaderHash =
  H28<Header>

/// Midgard block header.
/// Compact representation.
pub type Header {
  prev_utxos_root: MerkleRoot<Blake2b_256, ByteArray>,
  utxos_root: MerkleRoot<Blake2b_256, ByteArray>,
  transactions_root: MerkleRoot<Blake2b_256, ByteArray>,
  deposits_root: MerkleRoot<Blake2b_256, ByteArray>,
  withdrawals_root: MerkleRoot<Blake2b_256, ByteArray>,
  tx_orders_root: MerkleRoot<Blake2b_256, ByteArray>,
  start_time: PosixTime,
  end_time: PosixTime,
  prev_header_hash: HeaderHash,
  operator_vkey: VerificationKeyHash,
  protocol_version: Int,
}

/// Midgard block body.
/// Full representation.
/// This type is not used in onchain code.
pub type BlockBody {
  utxos: Dict<OutputReference, Output>,
  transactions: Dict<MidgardTxId, MidgardTxCompact>,
  deposits: Dict<DepositId, DepositEvent>,
  withdrawals: Dict<WithdrawalId, WithdrawalEvent>,
}

// ## Confirmed state

/// Midgard confirmed state.
/// Compact representation.
pub type ConfirmedState {
  header_hash: HeaderHash,
  prev_header_hash: HeaderHash,
  utxo_root: MerkleRoot<Blake2b_256, ByteArray>,
  start_time: PosixTime,
  end_time: PosixTime,
  protocol_version: Int,
}

// ## User events

pub fn unsafe_event_to_id_data(event_datum_data: Data) -> Data {
  expect [id_data, ..] = builtin.unconstr_fields(event_datum_data)
  id_data
}

pub fn unsafe_event_to_key_value_pair(
  event_datum_data: Data,
) -> Pair<Data, Data> {
  expect [id_data, info_data, ..] = builtin.unconstr_fields(event_datum_data)
  Pair(id_data, info_data)
}

// ### Deposit event

pub type DepositId =
  OutputReference

pub type DepositInfo {
  l2_address: Address,
  l2_datum: Option<Data>,
}

pub type DepositEvent {
  id: DepositId,
  info: DepositInfo,
}

// ### Withdrawal event

pub type WithdrawalId =
  OutputReference

pub type WithdrawalBody {
  l2_outref: OutputReference,
  l2_owner: VerificationKeyHash,
  l2_value: ValuePairs,
  l1_address: Address,
  l1_datum: CardanoDatum,
}

pub type WithdrawalSignature =
  Pair<VerificationKey, Signature>

pub type WithdrawalValidity {
  WithdrawalIsValid
  NonExistentWithdrawalUtxo
  SpentWithdrawalUtxo { l2_tx_id: MidgardTxId }
  IncorrectWithdrawalOwner
  IncorrectWithdrawalValue
  IncorrectWithdrawalSignature
  TooManyTokensInWithdrawal
}

pub type WithdrawalInfo {
  body: WithdrawalBody,
  signature: WithdrawalSignature,
  validity: WithdrawalValidity,
}

pub type WithdrawalEvent {
  id: WithdrawalId,
  info: WithdrawalInfo,
}

// ### Transaction order event

pub type TxOrderId =
  OutputReference

pub type TxOrderEvent {
  id: TxOrderId,
  tx: MidgardTxCompact,
}

// ## Transaction

pub type MidgardTxId =
  H32<MidgardTxCompact>

/// Midgard transaction.
/// Compact representation.
pub type MidgardTxCompact {
  body: H32<MidgardTxBodyCompact>,
  wits: H32<MidgardTxWitnessSetCompact>,
  validity: MidgardTxValidity,
}

// TODO
pub type MidgardTxValidity {
  TxIsValid
  NonExistentInputUtxo
  InvalidSignature
  FailedScript
  FeeTooLow
  UnbalancedTx
}

// ### Transaction body

/// Datatype to allow inputs of a Midgard tx to be in custom order.
pub type MidgardInputId {
  input_index: Int,
  transaction_id: MidgardTxId,
  output_index: Int,
}

/// Midgard transaction body.
/// Compact representation.
pub type MidgardTxBodyCompact {
  spend_inputs: MerkleRoot<Int, OutputReference>,
  reference_inputs: MerkleRoot<Int, OutputReference>,
  outputs: MerkleRoot<Int, Output>,
  fee: Lovelace,
  validity_interval: ValidityRange,
  required_observers: MerkleRoot<Int, ScriptHash>,
  required_signer_hashes: MerkleRoot<Int, VerificationKeyHash>,
  mint: MerkleRoot<Int, MidgardValueCompact>,
  script_integrity_hash: H32<Data>,
  auxiliary_data_hash: H32<Data>,
  network_id: MidgardNetworkId,
}

/// Midgard network ID
/// It indicates the network for which the transaction is intended.
/// Compact representation.
pub type MidgardNetworkId {
  Mainnet
  Testnet
}

// ### Transaction witnesses

/// Midgard transaction witnesses.
/// Compact representation.
pub type MidgardTxWitnessSetCompact {
  addr_tx_wits: MerkleRoot<VerificationKeyHash, (VerificationKey, Signature)>,
  script_tx_wits: MerkleRoot<ScriptHash, MidgardScript>,
  redeemer_tx_wits: MerkleRoot<ScriptPurpose, Redeemer>,
}

/// Midgard script -- either timelock (native) or Plutus (V3).
/// Full representation.
pub type MidgardScript {
  Timelock { script: ByteArray }
  Plutus { script: ByteArray }
}

pub type MidgardValueCompact =
  MerkleRoot<PolicyId, MerkleRoot<AssetName, Int>>

pub type MidgardTxId =
  H32<MidgardTxCompact>

//TODO This is a stub!
pub type MidgardTx {
  body: MidgardTxBodyCompact,
  wits: MidgardTxWitnessSetCompact,
  validity: MidgardTxValidity,
}
