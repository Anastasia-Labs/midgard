use midgard/bounded_item_v1
use midgard/ledger_output_proof_v1
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

pub type AuthenticatedOutputMaterialV1 {
  item_commitment: ByteArray,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  initial: ledger_output_proof_v1.LedgerOutputProofControlV1,
  first_successor: ledger_output_proof_v1.LedgerOutputProofControlV1,
  terminal: ledger_output_proof_v1.LedgerOutputProofControlV1,
  /// The terminal with all four descriptor facts attached (the canonical
  /// three attachment groups applied in order), as the finalize verifiers
  /// require before the thin terminal transition.
  facts_terminal: ledger_output_proof_v1.LedgerOutputProofControlV1,
  descriptor_cbor: ByteArray,
}

pub type ResolutionScheduleTerminalVectorV1 {
  original_schedule_hash: ByteArray,
  terminal_accumulator: ByteArray,
  terminal_frontier_commitment: ByteArray,
  penultimate_accumulator: ByteArray,
  penultimate_remaining_schedule_hash: ByteArray,
  penultimate_spend_index: Int,
  penultimate_resolved_item_peaks: List<validation_merkle_v1.FrontierPeak>,
  last_source_kind: Int,
  last_key: ByteArray,
  last_value: ByteArray,
}

/// One honest PlutusV3 execution selection over the maximum program
/// envelope, plus the tampered successor the same evidence must refuse.
/// Shared by the selection test below and the R5 item 1 route-agreement
/// controls at the end of this module.
pub type CekSelectionFixtureV1 {
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: vm_machine_types.ValidationOneStepEvidenceV1,
  tampered_post: validation_trace_v1.ValidationMachineStateV1,
}

/// One genuine-Cardano maximum-cardinality field, at the terminal step of its
/// `CanonicalDecode` fold, as §8's door reaches it.
///
/// **What left this record, and why it is not a weaker fixture.** It used to
/// carry a `bounded_collection_v1.ItemProofV1` and a
/// `bounded_item_v1.ChunkProofV1` — a per-item Merkle frontier with siblings,
/// and the item's chunk with its own frontier — and the step checked the first
/// against the field's §4 commitment. That is the unsatisfiable predicate #592
/// is about, and both proofs are gone: the fixture now carries the field's whole
/// §5.1 **preimage**, the door hashes it once against the flat commitment the
/// compact structure declares, and the item's length, chunk and
/// `bounded_item_v1` commitment are all derived from those authenticated bytes.
/// Nothing per-item is asserted by the fixture, so nothing per-item can drift.
pub type MaximumFieldTerminalFixtureV1 {
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  field_preimage_cbor: ByteArray,
  field_index: Int,
  item_count: Int,
  item_index: Int,
  terminal_chunk_index: Int,
  encoded_length_before_item: Int,
  expected_pre_work_root: ByteArray,
  expected_post_work_root: ByteArray,
}

pub type ValueOutputDescriptorFixture {
  pre: validation_trace_v1.ValidationMachineStateV1,
  active_state: validation_trace_v1.ValidationMachineStateV1,
  final_state: validation_trace_v1.ValidationMachineStateV1,
  initial_work: ByteArray,
  active_work: ByteArray,
  descriptor_cbor: ByteArray,
  policy_id: ByteArray,
  asset_name: ByteArray,
  quantity: Int,
  asset_peaks: List<validation_merkle_v1.FrontierPeak>,
}

// --- cek -------------------------------------------------------------------

pub type CekKindFixtureV1 {
  kind: Int,
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: vm_machine_types.ValidationOneStepEvidenceV1,
}

// --- ValueAndMint ------------------------------------------------------------

pub type ValueAndMintKindFixtureV1 {
  kind: Int,
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: vm_machine_types.ValidationOneStepEvidenceV1,
}
