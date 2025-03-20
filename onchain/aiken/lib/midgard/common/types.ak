use aiken/crypto.{Blake2b_256, Hash, VerificationKeyHash}
use cardano/assets.{AssetName, PolicyId}

/// A number of milliseconds since 00:00:00 UTC on 1 January 1970.
pub type POSIXTime =
  Int

pub type AssetClass {
  policy_id: PolicyId,
  asset_name: AssetName,
}

pub type Tokens =
  Pairs<PolicyId, Pairs<AssetName, Int>>

pub type PubKeyHash =
  VerificationKeyHash

pub type MerkleRoot =
  Hash<Blake2b_256, ByteArray>
