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

use aiken/collection/dict.{Dict}
use aiken/crypto.{ScriptHash, Signature, VerificationKey, VerificationKeyHash}
use cardano/assets.{Lovelace, Value}
use cardano/transaction.{
  Output, OutputReference, Redeemer, ScriptPurpose, TransactionId, ValidityRange,
}
use midgard/common/types.{H28, H32, MerkleRoot, PosixTime, PubKeyHash}

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
  prev_utxos_root: MerkleRoot,
  utxos_root: MerkleRoot,
  transactions_root: MerkleRoot,
  deposits_root: MerkleRoot,
  withdrawals_root: MerkleRoot,
  start_time: PosixTime,
  end_time: PosixTime,
  prev_header_hash: HeaderHash,
  operator_vkey: PubKeyHash,
  protocol_version: Int,
}

/// Midgard block body.
/// Full representation.
/// This type is not used in onchain code.
pub type BlockBody {
  utxos: Dict<OutputReference, Output>,
  transactions: Dict<TransactionId, MidgardTxCompact>,
  deposits: Dict<DepositId, DepositEvent>,
  withdrawals: Dict<WithdrawalId, WithdrawalEvent>,
}

// ## Confirmed state

/// Midgard confirmed state.
/// Compact representation.
pub type ConfirmedState {
  header_hash: HeaderHash,
  prev_header_hash: HeaderHash,
  utxo_root: MerkleRoot,
  start_time: PosixTime,
  end_time: PosixTime,
  protocol_version: Int,
}

// ## Deposit event

// TODO
pub type DepositId =
  Void

// TODO
pub type DepositEvent =
  Void

// ## Withdrawal event

// TODO
pub type WithdrawalId =
  Void

// TODO
pub type WithdrawalEvent =
  Void

// ## Transaction

/// Midgard transaction.
/// Compact representation.
pub type MidgardTxCompact {
  body: H32<MidgardTxBodyCompact>,
  wits: H32<MidgardTxWitnessSetCompact>,
  is_valid: Bool,
}

// ### Transaction body

/// Midgard transaction body.
/// Compact representation.
pub type MidgardTxBodyCompact {
  spend_inputs: H32<List<OutputReference>>,
  reference_inputs: H32<List<OutputReference>>,
  outputs: H32<List<Output>>,
  fee: Lovelace,
  validity_interval: ValidityRange,
  required_observers: H32<List<ScriptHash>>,
  required_signer_hashes: H32<List<VerificationKeyHash>>,
  mint: H32<Value>,
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
  addr_tx_wits: H32<Dict<VerificationKeyHash, (VerificationKey, Signature)>>,
  script_tx_wits: H32<Dict<ScriptHash, MidgardScript>>,
  redeemer_tx_wits: H32<Pairs<ScriptPurpose, Redeemer>>,
}

/// Midgard script -- either timelock (native) or Plutus (V3).
/// Full representation.
pub type MidgardScript {
  Timelock { script: ByteArray }
  Plutus { script: ByteArray }
}
