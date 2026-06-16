use aiken/crypto.{VerificationKeyHash}
use cardano/assets.{AssetName}

pub const asset_name: AssetName = "MIDGARD_DA_PARAMS"

pub const verification_key_byte_count: Int = 32

pub const verification_key_hash_byte_count: Int = 28

pub const max_indexed_signer_count: Int = 256

pub type Datum {
  /// Sorted verification keys, packed as 32-byte chunks. Governance owners
  /// remain key hashes because updates use transaction extra signatories.
  committee: ByteArray,
  da_threshold: Int,
  owners: List<VerificationKeyHash>,
  update_threshold: Int,
}
