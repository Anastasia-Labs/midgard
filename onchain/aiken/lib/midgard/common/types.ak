use aiken/crypto.{Blake2b_224, Blake2b_256, Hash}
use aiken/merkle_patricia_forestry as mpf
use cardano/assets.{AssetName, PolicyId}

pub type Empty =
  ByteArray

/// A number of milliseconds since 00:00:00 UTC on 1 January 1970.
pub type PosixTime =
  Int

/// A duration of milliseconds to be added/removed from a PosixTime
pub type PosixTimeDuration =
  Int

pub type ValuePairs =
  Pairs<PolicyId, Pairs<AssetName, Int>>

/// A 28-byte hash calculated with the Blake2b-224 algorithm.
pub type H28<data> =
  Hash<Blake2b_224, data>

/// A 32-byte hash calculated with the Blake2b-256 algorithm.
pub type H32<data> =
  Hash<Blake2b_256, data>

// TODO fix: use a more specific data type
/// A Merkle root hash, assumed to be a 32-byte hash
/// calculated with the Blake2b-256 algorithm.
pub type MerkleRoot =
  H32<ByteArray>

pub type Proof =
  mpf.Proof
