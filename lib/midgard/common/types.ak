use aiken/crypto.{VerificationKeyHash}
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
