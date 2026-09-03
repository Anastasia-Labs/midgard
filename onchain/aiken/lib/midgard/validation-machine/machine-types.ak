use aiken/merkle_patricia_forestry.{Proof}
use midgard/blake2b_224_trace_v1
use midgard/bounded_item_v1
use midgard/cek_data_v1
use midgard/cek_machine_v1
use midgard/fraud_proofs/native_tx/types.{VerifiedMidgardNativeTxCompact}
use midgard/ledger_output_proof_v1
use midgard/mpf_proof_fold_v1
use midgard/native_script_scan_v1
use midgard/native_tx_field_access_v1
use midgard/redeemer_item_proof_v1
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

pub type ValidationOneStepWitnessV1 {
  work_witness_cbor: ByteArray,
  claimed_successor: validation_trace_v1.ValidationMachineStateV1,
}

pub type CekRedeemerContextControlV1 {
  cursor: Int,
  map_items: cek_data_v1.DataSequenceSummaryV1,
  active_scan_hash: ByteArray,
  active_redeemer_leaf: ByteArray,
  active_purpose: cek_data_v1.DataSummaryV1,
  current_redeemer: cek_data_v1.DataSummaryV1,
}

pub type CekFinalContextControlV1 {
  tx_info: cek_data_v1.DataSummaryV1,
  redeemer: cek_data_v1.DataSummaryV1,
  script_info: cek_data_v1.DataSummaryV1,
}

pub type CekContextPartsControlV1 {
  redeemer_items: cek_data_v1.DataSequenceSummaryV1,
  redeemer: cek_data_v1.DataSummaryV1,
  script_info: cek_data_v1.DataSummaryV1,
}

pub type CekTxInfoAssemblyControlV1 {
  tail_fields: cek_data_v1.DataSequenceSummaryV1,
  redeemer: cek_data_v1.DataSummaryV1,
  script_info: cek_data_v1.DataSummaryV1,
}

pub type LedgerDeltaOperationProofV1 {
  descriptor: mpf_proof_fold_v1.ProofDescriptorV1,
  operation_count: Int,
  operation_peaks: List<validation_merkle_v1.FrontierPeak>,
  operation_index: Int,
  operation_siblings: List<ByteArray>,
}

pub type ValidationAuxiliaryWitnessV1 {
  NoAuxiliaryWitness
  /// One item of one committed field, reached through §8's door
  /// (`midgard/validation_machine_field_door_v1`). Nine of the machine's
  /// fifteen per-item sites match this constructor, across all eight phases
  /// that read a field.
  ///
  /// It used to carry a counted `(ItemProofV1, ChunkProofV1)` pair checked
  /// against the §4 flat field commitment — a predicate no honest prover could
  /// satisfy (#592). What replaces the pair is not a smaller proof but *no*
  /// proof: the carriage names where the field's preimage is, the door
  /// authenticates the whole preimage once against the flat commitment, and the
  /// item is then a slice. Nothing per-item is asserted by the prover, so there
  /// is nothing per-item left to check.
  ///
  /// `field_index` is here rather than a call-site literal because §4 removed
  /// field-index domain separation and two phases read more than one slot:
  /// `CanonicalDecode`, which walks all nine and takes the slot from its own
  /// control (`control.field_index`), and `InputSets`, which alternates fields 0
  /// and 1. Every site checks it against the slot it means — against the control
  /// for those two, against a literal for the other six phases — so a wrong
  /// index fails closed. `item_index` is likewise
  /// checked against the phase's own cursor everywhere except the two sites
  /// where the item order is the prover's to choose and the successor pins it —
  /// `InputSets` (descending out-ref key order) and the field-7 address-witness
  /// scan (order key includes the index).
  TransactionFieldChunkWitness {
    field_index: Int,
    item_index: Int,
    carriage: native_tx_field_access_v1.FieldCarriageV1,
  }
  /// A field-4 required-signer item, plus the signer-set membership evidence the
  /// step decides on. No `field_index`/`item_index`: the field is 4 by
  /// construction and the item index is `control.required_seen`.
  RequiredSignerItemWitness {
    carriage: native_tx_field_access_v1.FieldCarriageV1,
    signer_proof: SignerSetProofV1,
  }
  NativeScriptTokenWitness {
    chunk_proof: bounded_item_v1.ChunkProofV1,
    next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
    signer_proof: SignerSetProofV1,
  }
  NativeScriptFrameWitness { frame: native_script_scan_v1.NativeScriptFrameV1 }
  ScheduledLedgerMembershipWitness {
    source_kind: Int,
    key: ByteArray,
    next_schedule_hash: ByteArray,
    value: ByteArray,
    proof: Proof,
    signer_proof: SignerSetProofV1,
  }
  ScheduledLedgerNonMembershipWitness {
    source_kind: Int,
    key: ByteArray,
    next_schedule_hash: ByteArray,
    proof: Proof,
  }
  ResolvedInputReplayWitness {
    source_kind: Int,
    key: ByteArray,
    next_schedule_hash: ByteArray,
    value: ByteArray,
  }
  ScriptPurposeScanWitness {
    purpose_kind: Int,
    purpose_index: Int,
    script_hash: ByteArray,
    subject: ByteArray,
    siblings: List<ByteArray>,
  }
  ScriptSourceScanWitness {
    source_index: Int,
    origin_kind: Int,
    source_key: ByteArray,
    script_language_tag: Int,
    script_hash: ByteArray,
    script_total_length: Int,
    script_item_commitment: ByteArray,
    siblings: List<ByteArray>,
  }
  RedeemerScanBeginWitness {
    item_index: Int,
    item_count: Int,
    total_length: Int,
    item_commitment: ByteArray,
    siblings: List<ByteArray>,
  }
  NativeExecutionScanWitness {
    execution_index: Int,
    language_tag: Int,
    purpose_kind: Int,
    purpose_index: Int,
    script_hash: ByteArray,
    subject: ByteArray,
    purpose_siblings: List<ByteArray>,
    source_index: Int,
    origin_kind: Int,
    source_key: ByteArray,
    script_total_length: Int,
    script_item_commitment: ByteArray,
    source_siblings: List<ByteArray>,
    redeemer_leaf: ByteArray,
    execution_siblings: List<ByteArray>,
    first_chunk_proof: bounded_item_v1.ChunkProofV1,
  }
  CekCoreStepWitness { step: cek_machine_v1.CoreStepEvidenceV1 }
  CekResolvedContextItemWitness {
    source_kind: Int,
    item_index: Int,
    key: ByteArray,
    descriptor_cbor: ByteArray,
    siblings: List<ByteArray>,
  }
  CekOutputContextItemWitness {
    output_index: Int,
    descriptor_cbor: ByteArray,
    siblings: List<ByteArray>,
  }
  CekSignerContextItemWitness {
    peaks: List<validation_merkle_v1.FrontierPeak>,
    signer_index: Int,
    signer_hash: ByteArray,
    siblings: List<ByteArray>,
  }
  CekMintContextItemWitness {
    mint_index: Int,
    policy_id: ByteArray,
    asset_name: ByteArray,
    quantity: Int,
    siblings: List<ByteArray>,
  }
  CekRedeemerContextSelectWitness {
    control: CekRedeemerContextControlV1,
    item_index: Int,
    item_count: Int,
    total_length: Int,
    item_commitment: ByteArray,
    redeemer_siblings: List<ByteArray>,
    purpose_frontier_index: Int,
    purpose_kind: Int,
    purpose_index: Int,
    script_hash: ByteArray,
    subject: ByteArray,
    purpose_siblings: List<ByteArray>,
  }
  RedeemerItemStepWitness {
    redeemer_control: Option<CekRedeemerContextControlV1>,
    control: redeemer_item_proof_v1.RedeemerItemProofControlV1,
    witness: redeemer_item_proof_v1.RedeemerItemProofWitnessV1,
  }
  CekContextFinalizeWitness { redeemer_control: CekRedeemerContextControlV1 }
  CekContextFinalizeSpendWitness {
    redeemer_control: CekRedeemerContextControlV1,
    item_index: Int,
    key: ByteArray,
    descriptor_cbor: ByteArray,
    siblings: List<ByteArray>,
  }
  CekContextAssembleWitness { control: CekContextPartsControlV1 }
  CekTxInfoFinalizeWitness { control: CekTxInfoAssemblyControlV1 }
  CekContextSeedWitness { control: CekFinalContextControlV1 }
  ValueInputAssetWitness {
    source_kind: Int,
    key: ByteArray,
    next_schedule_hash: ByteArray,
    descriptor_cbor: ByteArray,
    asset_index: Int,
    policy_id: ByteArray,
    asset_name: ByteArray,
    quantity: Int,
    asset_peaks: List<validation_merkle_v1.FrontierPeak>,
    asset_siblings: List<ByteArray>,
    mutation: ValueAssetMutationWitnessV1,
  }
  ValueOutputAssetWitness {
    output_index: Int,
    descriptor_cbor: ByteArray,
    asset_index: Int,
    policy_id: ByteArray,
    asset_name: ByteArray,
    quantity: Int,
    asset_peaks: List<validation_merkle_v1.FrontierPeak>,
    asset_siblings: List<ByteArray>,
    mutation: ValueAssetMutationWitnessV1,
  }
  ValueMintAssetWitness {
    mint_index: Int,
    policy_id: ByteArray,
    asset_name: ByteArray,
    quantity: Int,
    siblings: List<ByteArray>,
    mutation: ValueAssetMutationWitnessV1,
  }
  LedgerDeltaReplayWitness {
    source_kind: Int,
    key: ByteArray,
    next_schedule_hash: ByteArray,
    value: ByteArray,
  }
  LedgerDeltaOutputWitness {
    output_index: Int,
    descriptor_cbor: ByteArray,
    siblings: List<ByteArray>,
  }
  /// `ScriptSources` stage 1 (field 8, one redeemer item) and stage 4 (field 2,
  /// one output item). Both stages need the item's *length* and its
  /// `bounded_item_v1` commitment and never look at its bytes, so the door's
  /// derived commitment is all the carriage has to yield. Field index and item
  /// index are both fixed by the stage and its cursor.
  TransactionRedeemerItemBeginWitness {
    carriage: native_tx_field_access_v1.FieldCarriageV1,
  }
  /// `CanonicalDecode`'s complete-item step: one item read whole rather than
  /// chunk by chunk. Field index and item index come from the phase's control,
  /// so the carriage is the entire wire surface. The item bytes that used to
  /// ride here as `item_cbor` are now read out of the authenticated preimage,
  /// which is what made the old pairing checkable at all: `item_cbor` was
  /// checked against a `bounded_item_v1` commitment that was itself opened
  /// against the §4 flat field hash, and that outer step could not hold.
  TransactionFieldItemWitness {
    carriage: native_tx_field_access_v1.FieldCarriageV1,
  }
  LedgerOutputProofBeginWitness {
    output_index: Int,
    total_length: Int,
    item_commitment: ByteArray,
    siblings: List<ByteArray>,
  }
  LedgerOutputProofStepWitness {
    witness: ledger_output_proof_v1.LedgerOutputProofWitnessV1,
  }
  LedgerOutputProofFinalizeWitness {
    descriptor_cbor: ByteArray,
    signer_proof: SignerSetProofV1,
  }
  LedgerDeltaProofFrameWitness {
    frame: mpf_proof_fold_v1.ProofFrameV1,
    siblings: List<ByteArray>,
  }
  LedgerDeltaOperationWitness {
    operation_kind: Int,
    key: ByteArray,
    value: ByteArray,
    operation_proof: LedgerDeltaOperationProofV1,
  }
  ScriptSourceHashBlockWitness {
    chunk_proof: bounded_item_v1.ChunkProofV1,
    next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  }
  NativeExecutionDescriptorWitness {
    execution_index: Int,
    language_tag: Int,
    purpose_kind: Int,
    purpose_index: Int,
    script_hash: ByteArray,
    subject: ByteArray,
    purpose_siblings: List<ByteArray>,
    source_index: Int,
    origin_kind: Int,
    source_key: ByteArray,
    script_total_length: Int,
    script_item_commitment: ByteArray,
    source_siblings: List<ByteArray>,
    redeemer_leaf: ByteArray,
    execution_siblings: List<ByteArray>,
    first_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
    signer_peaks: List<validation_merkle_v1.FrontierPeak>,
  }
  ValueOutputDescriptorWitness {
    output_index: Int,
    descriptor_cbor: ByteArray,
    siblings: List<ByteArray>,
  }
  MintFoldAssetWitness {
    chunk_proof: bounded_item_v1.ChunkProofV1,
    next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  }
}

pub type SignerSetProofV1 {
  NoSignerSetProof
  SignerMembershipProof {
    peaks: List<validation_merkle_v1.FrontierPeak>,
    signer_index: Int,
    siblings: List<ByteArray>,
  }
  EmptySignerSetProof { peaks: List<validation_merkle_v1.FrontierPeak> }
  SignerBelowFirstProof {
    peaks: List<validation_merkle_v1.FrontierPeak>,
    first_signer_hash: ByteArray,
    siblings: List<ByteArray>,
  }
  SignerAboveLastProof {
    peaks: List<validation_merkle_v1.FrontierPeak>,
    last_signer_hash: ByteArray,
    siblings: List<ByteArray>,
  }
  SignerBetweenProof {
    peaks: List<validation_merkle_v1.FrontierPeak>,
    lower_index: Int,
    lower_signer_hash: ByteArray,
    lower_siblings: List<ByteArray>,
    upper_signer_hash: ByteArray,
    upper_siblings: List<ByteArray>,
  }
}

pub type ValidationOneStepEvidenceV1 {
  transition: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
}

/// A field preimage published once, at a script address, for the
/// `CanonicalDecode` complete-item steps of one disputed transaction to reach by
/// reference input instead of re-carrying it in every step's redeemer.
///
/// **It publishes the field's whole §5.1 preimage, not one item, and that is the
/// rebind.** Under the retired counted scheme it published one item's bytes
/// beside an `ItemProofV1` opening them against the field commitment — an
/// opening §4 made unsatisfiable (#592). Under §8 the unit that authenticates is
/// the whole preimage, so the whole preimage is what a publication has to carry;
/// a consuming step wraps it as `Inline` carriage and the door hashes it once
/// against the committed field hash. That makes this datum the script-address
/// twin of §8.2's tier-2 `RawUtxo` publication, with the two bindings the
/// counted version also carried — `transaction_id` and
/// `transaction_commitment` — so a look-alike UTxO cannot pass a preimage off as
/// belonging to a different dispute.
pub type ValidationProofItemDatumV1 {
  version: Int,
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  field_preimage: ByteArray,
}

pub type ScheduledInputV1 {
  source_kind: Int,
  key: ByteArray,
}

pub type ResolveInputOutputProofV1 {
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  descriptor_cbor: ByteArray,
  output_proof: ledger_output_proof_v1.LedgerOutputProofControlV1,
}

pub type ObserverPurposeScanControlV1 {
  total_count: Int,
  seen: Int,
  previous_hash: ByteArray,
}

pub type ReceivePurposeScanControlV1 {
  source_count: Int,
  source_peaks: List<validation_merkle_v1.FrontierPeak>,
  receive_count: Int,
  previous_hash: ByteArray,
  candidate_hash: ByteArray,
  descriptor_peaks: List<validation_merkle_v1.FrontierPeak>,
}

pub type CanonicalDecodeControlV1 {
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  field_index: Int,
  item_index: Int,
  chunk_index: Int,
  item_count: Int,
  encoded_length: Int,
}

pub type CanonicalDecodeItemSourceV1 {
  expected_field_commitment: ByteArray,
  expected_field_length: Int,
}

pub type CanonicalDecodeItemProofV1 {
  active_item_count: Int,
  item_encoding_is_valid: Bool,
  next_encoded_length: Int,
}

/// What opening the item through §8's door established about it: the field's
/// authenticated §5.2 item count and this item's §5.1 payload length.
///
/// **It carries no proof, and it cannot.** Under the retired counted scheme this
/// record held the prover's `ItemProofV1` and the item's `bounded_item_v1`
/// commitment, so a *later* stage of the staging chain could re-check the
/// observation against the field commitment from the datum alone. The door is a
/// computation over the consuming transaction's reference inputs, not a datum,
/// so what a later stage can re-check is the observation's *consistency* with
/// the control state — see `canonical_decode_item_observation_is_consistent` —
/// and the door work itself happens exactly once, in the stage that has the
/// transaction in hand (`canonical-decode-item-observe-v1`). The staging chain's
/// own typed hand-off is what makes that provenance sound: an
/// `ObservedCanonicalDecodeItemV1` can only be built by the observe validator,
/// and the thread carries it forward under that validator's script hash.
pub type CanonicalDecodeItemObservationV1 {
  item_count: Int,
  item_length: Int,
}

pub type ValidationContextV1 {
  block_end_time_ms: Int,
  expected_network_id: Int,
  min_fee_a: Int,
  min_fee_b: Int,
  block_slot: Int,
}

pub type InputSetsControlV1 {
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  spend_count: Int,
  reference_count: Int,
  spend_seen: Int,
  reference_seen: Int,
  previous_key: ByteArray,
  resolution_schedule_hash: ByteArray,
}

pub type SignaturesControlV1 {
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  resolution_schedule_hash: ByteArray,
  stage: Int,
  address_count: Int,
  required_count: Int,
  address_seen: Int,
  required_seen: Int,
  previous_order_key: ByteArray,
  previous_signer_hash: ByteArray,
  signer_count: Int,
  signer_peaks: List<validation_merkle_v1.FrontierPeak>,
  invalid_signature_seen: Int,
}

pub type PhaseANativeScriptsControlV1 {
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  resolution_schedule_hash: ByteArray,
  stage: Int,
  script_count: Int,
  script_seen: Int,
  contains_non_native_script: Int,
  item_length: Int,
  item_commitment: ByteArray,
  cursor: Int,
  stack_root: ByteArray,
  stack_depth: Int,
  node_count: Int,
  result: Int,
  signer_count: Int,
  signer_peaks: List<validation_merkle_v1.FrontierPeak>,
  continuation_cbor: ByteArray,
}

pub type NativeChunkWindowV1 {
  bytes: ByteArray,
  offset: Int,
}

pub type AuthenticatedNativeScriptTokenHeadV1 {
  verified: VerifiedMidgardNativeTxCompact,
  window: NativeChunkWindowV1,
  head: Option<native_script_scan_v1.NativeScriptTokenHeadV1>,
}

pub type AuthenticatedNativeScriptWindowV1 {
  verified: VerifiedMidgardNativeTxCompact,
  window: NativeChunkWindowV1,
}

pub type PhaseAScriptPreconditionsControlV1 {
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  resolution_schedule_hash: ByteArray,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  contains_non_native_script: Int,
  observer_count: Int,
  observer_seen: Int,
  previous_observer: ByteArray,
}

pub type ResolveInputsControlV1 {
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  cursor: Int,
  accumulator: ByteArray,
  remaining_schedule_hash: ByteArray,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  pending: Option<ResolveInputOutputProofV1>,
  resolution_schedule_hash: ByteArray,
}

pub type InputSignerAuthorizationV1 {
  InputSignerAuthorized
  InputSignerMissing
  InputSignerProofMalformed
}

pub type ScriptSourcesControlV1 {
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  resolved_input_count: Int,
  resolved_inputs_accumulator: ByteArray,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  resolved_item_peaks: List<validation_merkle_v1.FrontierPeak>,
  stage: Int,
  source_count: Int,
  source_peaks: List<validation_merkle_v1.FrontierPeak>,
  redeemer_count: Int,
  redeemer_peaks: List<validation_merkle_v1.FrontierPeak>,
  replay_cursor: Int,
  replay_accumulator: ByteArray,
  replay_remaining_schedule_hash: ByteArray,
  spend_index: Int,
  purpose_count: Int,
  purpose_peaks: List<validation_merkle_v1.FrontierPeak>,
  output_cursor: Int,
  output_count: Int,
  output_peaks: List<validation_merkle_v1.FrontierPeak>,
  output_total_count: Int,
  receive_scan: ReceivePurposeScanControlV1,
  source_total_count: Int,
  redeemer_total_count: Int,
  observer_scan: ObserverPurposeScanControlV1,
  discovery: ScriptDiscoveryControlV1,
  output_proof: Option<ledger_output_proof_v1.LedgerOutputProofControlV1>,
  pending_source_cbor: ByteArray,
  mint_fold: MintFoldControlV1,
  resolution_schedule_hash: ByteArray,
}

pub type MintFoldControlV1 {
  policy_count: Int,
  policy_cursor: Int,
  previous_policy: ByteArray,
  active_policy: ByteArray,
  item_length: Int,
  item_commitment: ByteArray,
  item_cursor: Int,
  assets_remaining: Int,
  policy_asset_cursor: Int,
  previous_asset: ByteArray,
  asset_count: Int,
  asset_peaks: List<validation_merkle_v1.FrontierPeak>,
}

pub type ScriptDiscoveryControlV1 {
  purpose_cursor: Int,
  source_cursor: Int,
  redeemer_cursor: Int,
  current_purpose_kind: Int,
  current_purpose_index: Int,
  current_script_hash: ByteArray,
  current_subject: ByteArray,
  matched_source_index: Int,
  matched_language_tag: Int,
  matched_source_leaf: ByteArray,
  used_inline_bitmap: Int,
  used_redeemer_bitmap: Int,
  redeemer_item_control_hash: ByteArray,
  execution_count: Int,
  execution_peaks: List<validation_merkle_v1.FrontierPeak>,
}

pub type InlineSourceHashControlV1 {
  version: Int,
  source_index: Int,
  source_total_count: Int,
  language_tag: Int,
  payload_offset: Int,
  payload_length: Int,
  item_length: Int,
  item_commitment: ByteArray,
  hash_control: blake2b_224_trace_v1.Blake2b224TraceControlV1,
}

pub type MintChunkWindowV1 {
  bytes: ByteArray,
  offset: Int,
  chunk_index: Int,
}

pub type NativeScriptsControlV1 {
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  resolved_input_count: Int,
  resolved_inputs_accumulator: ByteArray,
  spend_input_count: Int,
  resolved_item_peaks: List<validation_merkle_v1.FrontierPeak>,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  source_count: Int,
  source_peaks: List<validation_merkle_v1.FrontierPeak>,
  redeemer_count: Int,
  redeemer_peaks: List<validation_merkle_v1.FrontierPeak>,
  purpose_count: Int,
  purpose_peaks: List<validation_merkle_v1.FrontierPeak>,
  output_count: Int,
  output_peaks: List<validation_merkle_v1.FrontierPeak>,
  output_descriptor_peaks: List<validation_merkle_v1.FrontierPeak>,
  mint_count: Int,
  mint_peaks: List<validation_merkle_v1.FrontierPeak>,
  execution_count: Int,
  execution_peaks: List<validation_merkle_v1.FrontierPeak>,
  execution_cursor: Int,
  language_bitmap: Int,
  resolution_schedule_hash: ByteArray,
}

pub type CekContextControlV1 {
  stage: Int,
  language_tag: Int,
  program_term_root: ByteArray,
  /// Canonical V1 envelope identity, distinct from its executable root.
  /// This keeps version and complete-material claims bound until terminal.
  program_envelope_hash: ByteArray,
  purpose_kind: Int,
  purpose_index: Int,
  script_hash: ByteArray,
  subject: ByteArray,
  redeemer_leaf: ByteArray,
  redeemer_context_control_hash: ByteArray,
  execution_memory_limit: Int,
  execution_cpu_limit: Int,
  reference_items: cek_data_v1.DataSequenceSummaryV1,
  spend_items: cek_data_v1.DataSequenceSummaryV1,
  output_items: cek_data_v1.DataSequenceSummaryV1,
  signer_items: cek_data_v1.DataSequenceSummaryV1,
  observer_count: Int,
  observer_items: cek_data_v1.DataSequenceSummaryV1,
  previous_observer: ByteArray,
  observer_summary: cek_data_v1.DataSummaryV1,
  mint_cursor: Int,
  current_mint_policy: ByteArray,
  current_mint_assets: cek_data_v1.DataSequenceSummaryV1,
  mint_policies: cek_data_v1.DataSequenceSummaryV1,
  mint_summary: cek_data_v1.DataSummaryV1,
}

pub type ValueAccumulatorV1 {
  lovelace_delta: Int,
  asset_root: ByteArray,
  seen_asset_count: Int,
  nonzero_asset_count: Int,
}

pub type ValueAccumulatorUpdateV1 {
  ValueAccumulatorUpdated(ValueAccumulatorV1)
  ValueAccumulatorAssetLimitExceeded
  ValueAccumulatorMutationInvalid
}

pub type ValueAssetMutationWitnessV1 {
  delta_was_present: Bool,
  old_delta: Int,
  delta_proof: Proof,
}

pub type ValueAndMintControlV1 {
  native_control: NativeScriptsControlV1,
  stage: Int,
  replay_schedule_hash: ByteArray,
  replay_cursor: Int,
  replay_asset_cursor: Int,
  replay_value_hash: ByteArray,
  replay_accumulator: ByteArray,
  replay_remaining_schedule_hash: ByteArray,
  output_cursor: Int,
  output_asset_cursor: Int,
  mint_cursor: Int,
  value_accumulator: ValueAccumulatorV1,
}

pub type LedgerDeltaControlV1 {
  resolved_input_count: Int,
  resolved_inputs_accumulator: ByteArray,
  output_count: Int,
  output_descriptor_peaks: List<validation_merkle_v1.FrontierPeak>,
  stage: Int,
  replay_schedule_hash: ByteArray,
  replay_cursor: Int,
  replay_accumulator: ByteArray,
  replay_remaining_schedule_hash: ByteArray,
  current_ledger_root: ByteArray,
  output_cursor: Int,
  operation_count: Int,
  operation_peaks: List<validation_merkle_v1.FrontierPeak>,
  pending_mutation_cbor: ByteArray,
}

pub type LedgerDeltaPendingMutationV1 {
  authorization_stage: Int,
  operation_kind: Int,
  key: ByteArray,
  value: ByteArray,
  descriptor: mpf_proof_fold_v1.ProofDescriptorV1,
  fold_control: mpf_proof_fold_v1.ProofFoldControlV1,
}

/// The decoded cek work witness. `cek_witness_is_well_formed_v1` applies the
/// checks every cek step shares: the native transaction proof source (whose
/// `expect`s are load-bearing), the transaction commitment, the validation
/// context, the native control's well-formedness, the cursor/budget bounds
/// and the exact re-encoding of the witness. Each semantic resolver runs both
/// so no wrapper can skip a guard the aggregate `verify_cek` applies.
pub type CekWitnessControlV1 {
  control: NativeScriptsControlV1,
  context_control_cbor: ByteArray,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
  active_state_hash: ByteArray,
  program_envelope_hash: ByteArray,
  execution_cpu_limit: Int,
  execution_memory_limit: Int,
}
