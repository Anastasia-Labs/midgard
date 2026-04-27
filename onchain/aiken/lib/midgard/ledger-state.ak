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
  ScriptHash, Signature, VerificationKey, VerificationKeyHash, blake2b_224,
}
use aiken/primitive/bytearray
use cardano/address.{Address, Credential}
use cardano/assets.{Lovelace}
use cardano/transaction.{
  Datum as CardanoDatum, OutputReference, Redeemer, ScriptPurpose, ValidityRange,
}
use midgard/common/constants.{empty_merkle_tree_root, plutus_version}
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

pub type MidgardLedger =
  Dict<OutputReference, MidgardOutput>

pub type MidgardLedgerRoot =
  MerkleRoot<OutputReference, MidgardOutput>

/// Midgard block header.
/// Compact representation.
pub type Header {
  prev_utxos_root: MidgardLedgerRoot,
  utxos_root: MidgardLedgerRoot,
  transactions_root: MidgardTxsRoot,
  deposits_root: DepositsRoot,
  withdrawals_root: WithdrawalsRoot,
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
  utxos: MidgardLedger,
  transactions: MidgardTxs,
  deposits: Deposits,
  withdrawals: Withdrawals,
}

// ## Confirmed state

/// Midgard confirmed state.
/// Compact representation.
pub type ConfirmedState {
  header_hash: HeaderHash,
  prev_header_hash: HeaderHash,
  utxo_root: MidgardLedgerRoot,
  start_time: PosixTime,
  end_time: PosixTime,
  protocol_version: Int,
}

pub const genesis_header_hash: HeaderHash =
  #"00000000000000000000000000000000000000000000000000000000"

pub const genesis_utxo_root: MidgardLedgerRoot = empty_merkle_tree_root

pub const genesis_protocol_version: Int = 0

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

pub type Deposits =
  Dict<DepositId, DepositInfo>

pub type DepositsRoot =
  MerkleRoot<DepositId, DepositInfo>

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

pub type Withdrawals =
  Dict<WithdrawalId, WithdrawalInfo>

pub type WithdrawalsRoot =
  MerkleRoot<WithdrawalId, WithdrawalInfo>

pub type WithdrawalId =
  OutputReference

pub type WithdrawalBody {
  l2_outref: OutputReference,
  l2_owner: VerificationKeyHash,
  l2_value: MidgardValue,
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

/// > [!NOTE]
/// > This is the hash of the compact representation.
pub type MidgardTxId =
  H32<MidgardTxCompact>

/// Midgard transaction. TODO: This is a stub!
pub type MidgardTx {
  body: MidgardTxBodyCompact,
  wits: MidgardTxWitnessSetCompact,
  validity: MidgardTxValidity,
}

pub type MidgardTxs =
  Dict<MidgardTxId, MidgardTx>

pub type MidgardTxsRoot =
  MerkleRoot<MidgardTxId, MidgardTxCompact>

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

/// TODO
pub type MidgardAddress =
  Credential

/// TODO
pub type MidgardValue =
  ValuePairs

pub type MidgardValueCompact =
  H32<MidgardValue>

/// TODO
pub type MidgardOutput {
  address: MidgardAddress,
  value: MidgardValue,
  datum: Option<Data>,
  reference_script: Option<ScriptHash>,
}

/// Datatype to allow inputs of a Midgard tx to be in custom order.
pub type MidgardInputId {
  input_index: Int,
  transaction_id: MidgardTxId,
  output_index: Int,
}

pub fn midgard_input_id_to_out_ref(input_id: MidgardInputId) -> OutputReference {
  let MidgardInputId { transaction_id, output_index, .. } = input_id
  OutputReference { transaction_id, output_index }
}

pub type MidgardInputsRoot =
  MerkleRoot<MidgardInputId, MidgardOutput>

pub type MidgardOutputsRoot =
  MerkleRoot<Int, MidgardOutput>

pub type ObserversRoot =
  MerkleRoot<Int, ScriptHash>

pub type SignersRoot =
  MerkleRoot<Int, VerificationKeyHash>

/// Midgard transaction body.
/// Compact representation.
pub type MidgardTxBodyCompact {
  spend_inputs: MidgardInputsRoot,
  reference_inputs: MidgardInputsRoot,
  outputs: MidgardOutputsRoot,
  fee: Lovelace,
  validity_interval: ValidityRange,
  required_observers: ObserversRoot,
  required_signer_hashes: SignersRoot,
  mint: MidgardValueCompact,
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

pub type AddressWitsRoot =
  MerkleRoot<VerificationKey, Signature>

pub type MidgardScriptWitsRoot =
  MerkleRoot<ScriptHash, MidgardScript>

pub type RedeemersRoot =
  MerkleRoot<ScriptPurpose, Redeemer>

/// Midgard transaction witnesses.
/// Compact representation.
pub type MidgardTxWitnessSetCompact {
  addr_tx_wits: AddressWitsRoot,
  script_tx_wits: MidgardScriptWitsRoot,
  redeemer_tx_wits: RedeemersRoot,
}

/// Midgard script -- either timelock (native) or Plutus (V3).
/// Full representation.
pub type MidgardScript {
  Timelock { script: ByteArray }
  Plutus { script: ByteArray }
}

/// TODO: This assumes the script stored under `Plutus` constructor of
///       `MidgardScript` is the single CBOR encoding of the Flat encoded
///       program bytes. Also uses a hardcoded Plutus version 3.
///
///       `Timelock` scripts are simply passed to the hash function.
pub fn hash_midgard_script(midgard_script: MidgardScript) -> ScriptHash {
  when midgard_script is {
    Timelock { script } -> script |> blake2b_224
    Plutus { script } ->
      builtin.integer_to_bytearray(True, 1, plutus_version)
        |> bytearray.concat(script)
        |> blake2b_224
  }
}
