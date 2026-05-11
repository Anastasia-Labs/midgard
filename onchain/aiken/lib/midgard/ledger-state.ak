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
use aiken/cbor
use aiken/collection/dict.{Dict}
use aiken/crypto.{
  ScriptHash, Signature, VerificationKey, VerificationKeyHash, blake2b_224,
  blake2b_256,
}
use aiken/primitive/bytearray
use cardano/address.{Address, Credential}
use cardano/assets.{Lovelace}
use cardano/transaction.{
  Datum as CardanoDatum, OutputReference, Redeemer, ScriptPurpose, ValidityRange,
}
use env
use midgard/common/types.{H28, H32, MerkleRoot, PosixTime, ValuePairs}
use midgard/common/utils

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

pub const genesis_utxo_root: MidgardLedgerRoot = env.empty_merkle_tree_root

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
  tx: MidgardTx,
}

// ## Transaction

/// > [!NOTE]
/// > This is the hash of the compact body.
pub type MidgardTxId =
  H32<MidgardTxBodyCompact>

pub type MidgardTxWitsHash =
  H32<MidgardTxWitnessSetCompact>

/// Midgard transaction. It includes both the full version and compact versions
/// of body and witness set.
/// TODO: This is a draft. Might need further tweaks.
pub type MidgardTx {
  body: MidgardTxBody,
  wits: MidgardTxWitnessSet,
  validity: MidgardTxValidity,
}

pub type MidgardTxs =
  Dict<MidgardTxId, MidgardTx>

pub type MidgardTxsRoot =
  MerkleRoot<MidgardTxId, MidgardTxCompact>

/// Compact representation.
pub type MidgardTxCompact {
  body: MidgardTxId,
  wits: MidgardTxWitsHash,
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

pub fn midgard_tx_body_to_compact(body: MidgardTxBody) -> MidgardTxBodyCompact {
  let MidgardTxBody {
    spend_inputs,
    reference_inputs,
    outputs,
    fee,
    validity_interval,
    required_observers,
    required_signer_hashes,
    mint,
    script_integrity_hash,
    auxiliary_data_hash,
    network_id,
  } = body
  MidgardTxBodyCompact {
    spend_inputs: spend_inputs |> utils.serialise_and_hash_32,
    reference_inputs: reference_inputs |> utils.serialise_and_hash_32,
    outputs: outputs |> utils.serialise_and_hash_32,
    fee,
    validity_interval,
    required_observers: required_observers |> utils.serialise_and_hash_32,
    required_signer_hashes: required_signer_hashes
      |> utils.serialise_and_hash_32,
    mint: mint |> utils.serialise_and_hash_32,
    script_integrity_hash,
    auxiliary_data_hash,
    network_id,
  }
}

pub fn midgard_tx_wits_to_compact(
  wits: MidgardTxWitnessSet,
) -> MidgardTxWitnessSetCompact {
  let MidgardTxWitnessSet { addr_tx_wits, script_tx_wits, redeemer_tx_wits } =
    wits
  MidgardTxWitnessSetCompact {
    addr_tx_wits: addr_tx_wits |> utils.serialise_and_hash_32,
    script_tx_wits: script_tx_wits |> utils.serialise_and_hash_32,
    redeemer_tx_wits: redeemer_tx_wits |> utils.serialise_and_hash_32,
  }
}

pub fn midgard_tx_to_compact(tx: MidgardTx) -> MidgardTxCompact {
  let MidgardTx { body, wits, validity } = tx
  MidgardTxCompact {
    body: body |> midgard_tx_body_to_compact |> utils.serialise_and_hash_32,
    wits: wits |> midgard_tx_wits_to_compact |> utils.serialise_and_hash_32,
    validity,
  }
}

pub fn midgard_full_tx_to_id(tx: MidgardTx) -> MidgardTxId {
  tx.body |> midgard_tx_body_to_compact |> utils.serialise_and_hash_32
}

// ### Transaction body

/// TODO
pub type MidgardAddress =
  Credential

/// TODO
pub type MidgardValue =
  ValuePairs

pub type MidgardValueHash =
  H32<MidgardValue>

/// TODO
pub type MidgardOutput {
  address: MidgardAddress,
  value: MidgardValue,
  datum: Option<Data>,
  reference_script: Option<ScriptHash>,
}

pub type MidgardInputs =
  List<OutputReference>

pub type MidgardInputsHash =
  H32<MidgardInputs>

pub type MidgardOutputs =
  List<MidgardOutput>

pub type MidgardOutputsHash =
  H32<MidgardOutputs>

pub type Observers =
  List<ScriptHash>

pub type ObserversHash =
  H32<Observers>

pub type Signers =
  List<VerificationKeyHash>

pub type SignersHash =
  H32<Signers>

/// Midgard transaction body.
/// Full representation.
pub type MidgardTxBody {
  spend_inputs: MidgardInputs,
  reference_inputs: MidgardInputs,
  outputs: MidgardOutputs,
  fee: Lovelace,
  validity_interval: ValidityRange,
  required_observers: Observers,
  required_signer_hashes: Signers,
  mint: MidgardValue,
  script_integrity_hash: H32<Data>,
  auxiliary_data_hash: H32<Data>,
  network_id: MidgardNetworkId,
}

/// Midgard transaction body.
/// Compact representation.
pub type MidgardTxBodyCompact {
  spend_inputs: MidgardInputsHash,
  reference_inputs: MidgardInputsHash,
  outputs: MidgardOutputsHash,
  fee: Lovelace,
  validity_interval: ValidityRange,
  required_observers: ObserversHash,
  required_signer_hashes: SignersHash,
  mint: MidgardValueHash,
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

pub type AddressWits =
  Dict<VerificationKey, Signature>

pub type AddressWitsHash =
  H32<AddressWits>

pub type MidgardScriptWits =
  Dict<ScriptHash, MidgardScript>

pub type MidgardScriptWitsHash =
  H32<MidgardScriptWits>

pub type Redeemers =
  Pairs<ScriptPurpose, Redeemer>

pub type RedeemersHash =
  H32<Redeemers>

/// Midgard transaction witnesses.
/// Full representation.
pub type MidgardTxWitnessSet {
  addr_tx_wits: AddressWits,
  script_tx_wits: MidgardScriptWits,
  redeemer_tx_wits: Redeemers,
}

/// Midgard transaction witnesses.
/// Compact representation.
pub type MidgardTxWitnessSetCompact {
  addr_tx_wits: AddressWitsHash,
  script_tx_wits: MidgardScriptWitsHash,
  redeemer_tx_wits: RedeemersHash,
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
    Timelock { script } ->
      builtin.integer_to_bytearray(True, 1, 0)
        |> bytearray.concat(script)
        |> blake2b_224
    Plutus { script } ->
      builtin.integer_to_bytearray(True, 1, env.plutus_version)
        |> bytearray.concat(script)
        |> blake2b_224
  }
}
