use aiken/crypto.{Blake2b_256, Hash, VerificationKey, VerificationKeyHash}
use cardano/assets.{AssetName, PolicyId}

/// A number of milliseconds since 00:00:00 UTC on 1 January 1970.
pub type POSIXTime =
  Int

pub type AssetClass {
  policy_id: PolicyId,
  asset_name: AssetName,
}

pub type PubKeyHash =
  VerificationKeyHash

pub type RootHash =
  ByteArray

pub type HeaderHash =
  ByteArray

pub type HashedListReferenceOutputs =
  ByteArray

pub type HashedLisOutputs =
  ByteArray

pub type HashedScripts =
  ByteArray

pub type HashedVerificationKeys =
  ByteArray

pub type HashedBody =
  ByteArray

pub type HashedWitness =
  ByteArray

pub type Coin =
  Int

pub type Network =
  Int

pub type MidgardSValidityInterval {
  lower: Option<POSIXTime>,
  upper: Option<POSIXTime>,
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

pub type MerkleRoot =
  Hash<Blake2b_256, ByteArray>
