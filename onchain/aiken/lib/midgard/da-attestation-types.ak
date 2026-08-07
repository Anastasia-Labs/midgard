use aiken/crypto.{VerificationKeyHash}
use cardano/assets.{AssetName}
use midgard/ledger_state.{HeaderHash}

pub const da_params_asset_name: AssetName = "MIDGARD_DA_PARAMS"

pub const state_queue_mint_reference_script_asset_name: AssetName =
  "StateQueueMint"

/// DAAT tokens carry attestation certificates. This stateless policy cannot
/// prove per-header global uniqueness; downstream consumers must validate a
/// threshold-attested UTxO and must not rely on unique supply.
pub const attestation_asset_name_prefix: AssetName = "DAAT"

pub const verification_key_byte_count: Int = 32

pub const signature_byte_count: Int = 64

pub const signature_witness_byte_count: Int = 65

pub const max_indexed_signer_count: Int = 256

pub const attested_signer_bitmap_byte_count: Int = 32

pub const empty_attested_signer_bitmap: ByteArray =
  #"0000000000000000000000000000000000000000000000000000000000000000"

pub type DaAttestationDatum {
  header_hash: HeaderHash,
  da_threshold: Int,
  /// Hash of the immutable sorted committee verification keys.
  committee_signers_hash: ByteArray,
  /// 256-bit MSB-first bitmap of committee indexes that have attested.
  attested_signers: ByteArray,
  attestation_count: Int,
}

pub type DaParamsDatum {
  /// Sorted verification keys, packed as 32-byte chunks. Governance owners
  /// remain key hashes because updates use transaction extra signatories.
  committee: ByteArray,
  /// Hash of `committee`, validated by the DA params governor.
  committee_signers_hash: ByteArray,
  da_threshold: Int,
  owners: List<VerificationKeyHash>,
  update_threshold: Int,
}

/// `ApplyToStateQueue.da_params_ref_input_index` and
/// `RescueStrandedAttestation.da_params_ref_input_index` both point at the
/// authenticated DA params UTxO holding the *current* governed committee and
/// threshold. Every redeemer that consumes an attestation reads them, which is
/// what makes committee rotation retroactive (decision row D-DA4): a quorum
/// frozen under a rotated-out committee no longer matches the governed values
/// and can no longer apply.
pub type MintRedeemer {
  Init {
    output_index: Int,
    da_params_ref_input_index: Int,
    state_queue_ref_input_index: Int,
    state_queue_mint_ref_script_input_index: Int,
  }
  ApplyToStateQueue {
    da_attestation_input_index: Int,
    da_params_ref_input_index: Int,
    state_queue_input_index: Int,
    state_queue_output_index: Int,
    state_queue_mint_ref_script_input_index: Int,
  }
  /// Refunds an attestation that a mid-flight committee rotation stranded
  /// (decision row D-DA5 clause c). Authorization is a *state condition*, not a
  /// deadline and not a new governed value: the attestation's pinned
  /// `committee_signers_hash` must no longer equal the current authenticated DA
  /// params' governed value. Such an attestation can never reach quorum again —
  /// `validate_add_signatures` pins the datum's committee hash to the governed
  /// one — so without this path its ADA is locked forever.
  RescueStrandedAttestation {
    da_attestation_input_index: Int,
    da_params_ref_input_index: Int,
    refund_output_index: Int,
  }
}

/// `AddSignatures.da_params_ref_input_index` points at the authenticated DA
/// params UTxO whose committee matches `DaAttestationDatum.committee_signers_hash`.
/// `AddSignatures.signatures` is a packed sequence of
/// `1-byte signer index || 64-byte Ed25519 signature` chunks. Indexes must be
/// strictly ascending and point to 32-byte verification key slots in the DA
/// params committee. Each signature must verify against the DA attestation
/// message for this header; transaction required signers are not a substitute
/// for DA-message attestations.
pub type SpendRedeemer {
  AddSignatures {
    output_index: Int,
    da_params_ref_input_index: Int,
    signatures: ByteArray,
  }
  BurnForStateQueue { mint_redeemer_index: Int }
  /// The spend half of `RescueStrandedAttestation`. It carries no authorization
  /// of its own: it binds this UTxO to the rescue mint redeemer at
  /// `mint_redeemer_index`, which must name *this* input. That binding is what
  /// stops one rescue authorization from being replayed onto a second
  /// attestation UTxO in the same transaction.
  BurnForRescue { mint_redeemer_index: Int }
}
