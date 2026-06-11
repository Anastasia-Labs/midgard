use aiken/crypto.{VerificationKeyHash}
use cardano/assets.{AssetName}
use midgard/ledger_state.{HeaderHash}

pub const da_params_asset_name: AssetName = "MIDGARD_DA_PARAMS"

/// DAAT tokens carry attestation certificates. This stateless policy cannot
/// prove per-header global uniqueness; downstream consumers must validate a
/// threshold-attested UTxO and must not rely on unique supply.
pub const attestation_asset_name_prefix: AssetName = "DAAT"

pub const verification_key_byte_count: Int = 32

pub const signature_byte_count: Int = 64

pub const signature_witness_byte_count: Int = 65

pub const max_indexed_signer_count: Int = 256

pub type DaAttestationDatum {
  header_hash: HeaderHash,
  da_threshold: Int,
  /// Sorted verification keys, packed as 32-byte chunks.
  remaining_signers: ByteArray,
  attestation_count: Int,
}

pub type DaParamsDatum {
  /// Sorted verification keys, packed as 32-byte chunks. Governance owners
  /// remain key hashes because updates use transaction extra signatories.
  committee: ByteArray,
  da_threshold: Int,
  owners: List<VerificationKeyHash>,
  update_threshold: Int,
}

pub type MintRedeemer {
  Init {
    output_index: Int,
    da_params_ref_input_index: Int,
    state_queue_ref_input_index: Int,
  }
  ApplyToStateQueue {
    da_attestation_input_index: Int,
    state_queue_input_index: Int,
    state_queue_output_index: Int,
  }
}

/// `AddSignatures.signatures` is a packed sequence of
/// `1-byte signer index || 64-byte Ed25519 signature` chunks. Indexes must be
/// strictly ascending and point to 32-byte verification key slots in the input
/// datum's `remaining_signers`.
pub type SpendRedeemer {
  AddSignatures { output_index: Int, signatures: ByteArray }
  BurnForStateQueue { mint_redeemer_index: Int }
}
