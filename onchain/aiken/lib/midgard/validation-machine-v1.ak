use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_224, blake2b_256, verify_ed25519_signature}
use aiken/merkle_patricia_forestry.{Proof}
use aiken/merkle_patricia_forestry/merkling.{null_hash}
use aiken/primitive/bytearray
use midgard/blake2b_224_trace_v1
use midgard/bounded_collection_v1
use midgard/bounded_item_v1
use midgard/cek_constant_v1
use midgard/cek_data_v1
use midgard/cek_machine_v1
use midgard/cek_proof_v1
use midgard/fraud_proofs/native_tx/codec.{
  decode_canonical_int_at, decode_definite_array_header_at,
  encode_definite_array_header, encode_definite_bytes,
}
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_compact_v1, decode_native_tx_field_preimage_lengths_v1,
  decode_native_tx_witness_set_compact, native_tx_canonical_size_v1,
  native_tx_proof_commitment_v1, verify_native_tx_compact_cbor_v1,
  verify_native_tx_proof_source_v1,
}
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_address_witness_cbor, decode_midgard_tx_input_cbor,
  encode_midgard_tx_input,
}
use midgard/fraud_proofs/native_tx/preimages.{
  canonical_bytes_key_precedes, decode_canonical_bytes_at,
  decode_canonical_map_header_at,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardCredential, MidgardScriptWitness, MidgardTxInput,
  NativeTxBodyCompact, NativeTxCompact, NativeTxFieldPreimageLengthsV1,
  NativeTxWitnessSetCompact, PubKeyCredential, ScriptCredential,
  VerifiedMidgardNativeTxCompact,
}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_proof_v1
use midgard/ledger_output_v1
use midgard/mpf_proof_fold_v1
use midgard/mpf_proof_v1
use midgard/native_script_scan_v1
use midgard/redeemer_item_proof_v1
use midgard/script_context_v1
use midgard/script_language_views_v1
use midgard/script_proof_v1
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
  TransactionFieldPreimageWitness { preimage_cbor: ByteArray }
  TransactionFieldChunkWitness {
    collection_proof: bounded_collection_v1.ItemProofV1,
    chunk_proof: bounded_item_v1.ChunkProofV1,
  }
  RequiredSignerItemWitness {
    collection_proof: bounded_collection_v1.ItemProofV1,
    chunk_proof: bounded_item_v1.ChunkProofV1,
    signer_proof: SignerSetProofV1,
  }
  NativeScriptTokenWitness {
    chunk_proof: bounded_item_v1.ChunkProofV1,
    next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
    signer_proof: SignerSetProofV1,
  }
  NativeScriptFrameWitness { frame: native_script_scan_v1.NativeScriptFrameV1 }
  LedgerMembershipWitness { value: ByteArray, proof: Proof }
  LedgerNonMembershipWitness { proof: Proof }
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
  OutputReplayWitness {
    output_index: Int,
    output_cbor: ByteArray,
    siblings: List<ByteArray>,
    signer_proof: SignerSetProofV1,
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
    script: MidgardScriptWitness,
    source_siblings: List<ByteArray>,
    redeemer_leaf: ByteArray,
    execution_siblings: List<ByteArray>,
    signer_hashes: List<ByteArray>,
  }
  CekCoreStepWitness { step: cek_machine_v1.CoreStepEvidenceV1 }
  CekResolvedContextItemWitness {
    source_kind: Int,
    item_index: Int,
    key: ByteArray,
    value: ByteArray,
    siblings: List<ByteArray>,
  }
  CekOutputContextItemWitness {
    output_index: Int,
    output_cbor: ByteArray,
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
    value: ByteArray,
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
  TransactionRedeemerItemBeginWitness {
    collection_proof: bounded_collection_v1.ItemProofV1,
  }
  TransactionFieldItemWitness {
    collection_proof: bounded_collection_v1.ItemProofV1,
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

fn immutable_context_matches(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
) -> Bool {
  and {
    pre.machine_version == post.machine_version,
    pre.event_key_hash == post.event_key_hash,
    pre.transaction_id == post.transaction_id,
    pre.transaction_commitment == post.transaction_commitment,
    pre.validation_context_hash == post.validation_context_hash,
    pre.source_kind == post.source_kind,
    pre.prior_ledger_root == post.prior_ledger_root,
    pre.ledger_delta_root == post.ledger_delta_root,
  }
}

fn phase_successor_is_valid(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
) -> Bool {
  if post.phase == validation_trace_v1.Terminal {
    and {
      post.verdict != validation_trace_v1.Pending,
      pre.phase != validation_trace_v1.Terminal,
    }
  } else {
    and {
      post.verdict == validation_trace_v1.Pending,
      when pre.phase is {
        validation_trace_v1.CanonicalDecode -> or {
            post.phase == validation_trace_v1.CanonicalDecode,
            post.phase == validation_trace_v1.CompactBinding,
          }
        validation_trace_v1.CompactBinding ->
          post.phase == validation_trace_v1.StaticLedgerRules
        validation_trace_v1.StaticLedgerRules ->
          post.phase == validation_trace_v1.InputSets
        validation_trace_v1.InputSets ->
          post.phase == validation_trace_v1.Signatures
        validation_trace_v1.Signatures ->
          post.phase == validation_trace_v1.PhaseANativeScripts
        validation_trace_v1.PhaseANativeScripts ->
          post.phase == validation_trace_v1.PhaseAScriptPreconditions
        validation_trace_v1.PhaseAScriptPreconditions -> or {
            post.phase == validation_trace_v1.PhaseAScriptPreconditions,
            post.phase == validation_trace_v1.ResolveInputs,
          }
        validation_trace_v1.ResolveInputs -> or {
            post.phase == validation_trace_v1.ResolveInputs,
            post.phase == validation_trace_v1.ScriptSources,
          }
        validation_trace_v1.ScriptSources -> or {
            post.phase == validation_trace_v1.ScriptSources,
            post.phase == validation_trace_v1.NativeScripts,
          }
        validation_trace_v1.NativeScripts -> or {
            post.phase == validation_trace_v1.NativeScripts,
            post.phase == validation_trace_v1.ScriptIntegrity,
          }
        validation_trace_v1.ScriptIntegrity -> or {
            post.phase == validation_trace_v1.ScriptIntegrity,
            post.phase == validation_trace_v1.Cek,
          }
        validation_trace_v1.Cek -> or {
            post.phase == validation_trace_v1.Cek,
            post.phase == validation_trace_v1.ValueAndMint,
          }
        validation_trace_v1.ValueAndMint -> or {
            post.phase == validation_trace_v1.ValueAndMint,
            post.phase == validation_trace_v1.LedgerDelta,
          }
        validation_trace_v1.LedgerDelta ->
          post.phase == validation_trace_v1.LedgerDelta
        validation_trace_v1.Terminal -> False
      },
    }
  }
}

fn budget_progress_is_valid(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
) -> Bool {
  if pre.phase == validation_trace_v1.Cek {
    and {
      post.execution_cpu >= pre.execution_cpu,
      post.execution_memory >= pre.execution_memory,
    }
  } else {
    and {
      post.execution_cpu == pre.execution_cpu,
      post.execution_memory == pre.execution_memory,
    }
  }
}

pub fn structural_transition_is_valid(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let post = witness.claimed_successor
  and {
    validation_trace_v1.machine_state_is_well_formed(pre),
    validation_trace_v1.machine_state_is_well_formed(post),
    pre.verdict == validation_trace_v1.Pending,
    validation_trace_v1.hash_work_witness(
      pre.phase,
      pre.program_counter,
      witness.work_witness_cbor,
    ) == pre.work_root,
    post.program_counter == pre.program_counter + 1,
    immutable_context_matches(pre, post),
    phase_successor_is_valid(pre, post),
    budget_progress_is_valid(pre, post),
  }
}

pub fn encode_compact_binding_witness(
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
) -> ByteArray {
  #"86"
    |> bytearray.concat(encode_definite_bytes(transaction_id))
    |> bytearray.concat(encode_definite_bytes(transaction_commitment))
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
}

pub fn encode_transaction_field_scan_witness(
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  field_index: Int,
  item_index: Int,
  chunk_index: Int,
  item_count: Int,
  encoded_length: Int,
) -> ByteArray {
  expect field_index >= 0
  expect field_index <= 8
  expect item_index >= 0
  expect chunk_index >= 0
  expect item_count >= -1
  expect encoded_length >= 0
  #"89"
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
    |> bytearray.concat(cbor.serialise(field_index))
    |> bytearray.concat(cbor.serialise(item_index))
    |> bytearray.concat(cbor.serialise(chunk_index))
    |> bytearray.concat(cbor.serialise(item_count))
    |> bytearray.concat(cbor.serialise(encoded_length))
}

pub fn encode_static_rules_witness(
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
) -> ByteArray {
  #"84"
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
}

pub fn encode_input_sets_scan_witness(
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
) -> ByteArray {
  expect spend_count >= -1
  expect reference_count >= -1
  expect spend_seen >= 0
  expect reference_seen >= 0
  expect
    bytearray.length(previous_key) == 0 || bytearray.length(previous_key) == 36
  expect bytearray.length(resolution_schedule_hash) == 32
  encode_definite_array_header(10)
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
    |> bytearray.concat(cbor.serialise(spend_count))
    |> bytearray.concat(cbor.serialise(reference_count))
    |> bytearray.concat(cbor.serialise(spend_seen))
    |> bytearray.concat(cbor.serialise(reference_seen))
    |> bytearray.concat(encode_definite_bytes(previous_key))
    |> bytearray.concat(encode_definite_bytes(resolution_schedule_hash))
}

pub fn encode_phase_a_native_scripts_scan_witness(
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
) -> ByteArray {
  expect bytearray.length(resolution_schedule_hash) == 32
  expect stage >= 0
  expect stage <= 8
  expect script_count >= -1
  expect script_seen >= 0
  expect contains_non_native_script == 0 || contains_non_native_script == 1
  expect item_length >= 0
  expect
    bytearray.length(item_commitment) == 0 || bytearray.length(item_commitment) == 32
  expect cursor >= 0
  expect bytearray.length(stack_root) == 0 || bytearray.length(stack_root) == 32
  expect stack_depth >= 0
  expect node_count >= 0
  expect result >= -1
  expect result <= 1
  expect signer_count >= 0
  #"9f"
    |> bytearray.concat(cbor.serialise(builtin.b_data(compact_cbor)))
    |> bytearray.concat(
        cbor.serialise(builtin.b_data(witness_set_compact_cbor)),
      )
    |> bytearray.concat(
        cbor.serialise(builtin.b_data(field_preimage_lengths_cbor)),
      )
    |> bytearray.concat(cbor.serialise(builtin.b_data(context_cbor)))
    |> bytearray.concat(
        cbor.serialise(builtin.b_data(resolution_schedule_hash)),
      )
    |> bytearray.concat(cbor.serialise(stage))
    |> bytearray.concat(cbor.serialise(script_count))
    |> bytearray.concat(cbor.serialise(script_seen))
    |> bytearray.concat(cbor.serialise(contains_non_native_script))
    |> bytearray.concat(cbor.serialise(item_length))
    |> bytearray.concat(cbor.serialise(builtin.b_data(item_commitment)))
    |> bytearray.concat(cbor.serialise(cursor))
    |> bytearray.concat(cbor.serialise(builtin.b_data(stack_root)))
    |> bytearray.concat(cbor.serialise(stack_depth))
    |> bytearray.concat(cbor.serialise(node_count))
    |> bytearray.concat(cbor.serialise(result))
    |> bytearray.concat(cbor.serialise(signer_count))
    |> bytearray.concat(encode_definite_array_header(2))
    |> bytearray.concat(validation_merkle_v1.encode_frontier(signer_peaks))
    |> bytearray.concat(cbor.serialise(builtin.b_data(continuation_cbor)))
    |> bytearray.concat(#"ff")
}

pub fn encode_signatures_scan_witness(
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
) -> ByteArray {
  expect bytearray.length(resolution_schedule_hash) == 32
  expect stage >= 0
  expect stage <= 2
  expect address_count >= -1
  expect required_count >= -1
  expect address_seen >= 0
  expect required_seen >= 0
  expect
    bytearray.length(previous_signer_hash) == 0 || bytearray.length(
      previous_signer_hash,
    ) == 28
  expect signer_count >= 0
  expect
    validation_merkle_v1.frontier_is_well_formed(signer_count, signer_peaks)
  expect invalid_signature_seen == 0 || invalid_signature_seen == 1
  encode_definite_array_header(15)
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
    |> bytearray.concat(encode_definite_bytes(resolution_schedule_hash))
    |> bytearray.concat(cbor.serialise(stage))
    |> bytearray.concat(cbor.serialise(address_count))
    |> bytearray.concat(cbor.serialise(required_count))
    |> bytearray.concat(cbor.serialise(address_seen))
    |> bytearray.concat(cbor.serialise(required_seen))
    |> bytearray.concat(encode_definite_bytes(previous_order_key))
    |> bytearray.concat(encode_definite_bytes(previous_signer_hash))
    |> bytearray.concat(cbor.serialise(signer_count))
    |> bytearray.concat(validation_merkle_v1.encode_frontier(signer_peaks))
    |> bytearray.concat(cbor.serialise(invalid_signature_seen))
}

pub fn encode_phase_a_script_preconditions_witness(
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  resolution_schedule_hash: ByteArray,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  has_non_native_script: Bool,
  observer_count: Int,
  observer_seen: Int,
  previous_observer: ByteArray,
) -> ByteArray {
  expect bytearray.length(resolution_schedule_hash) == 32
  expect signer_count >= 0
  expect bytearray.length(signer_frontier_commitment) == 32
  expect observer_count >= 0
  expect observer_seen >= 0
  expect
    bytearray.length(previous_observer) == 0 || bytearray.length(
      previous_observer,
    ) == 28
  encode_definite_array_header(11)
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
    |> bytearray.concat(encode_definite_bytes(resolution_schedule_hash))
    |> bytearray.concat(cbor.serialise(signer_count))
    |> bytearray.concat(encode_definite_bytes(signer_frontier_commitment))
    |> bytearray.concat(
        cbor.serialise(
          if has_non_native_script {
            1
          } else {
            0
          },
        ),
      )
    |> bytearray.concat(encode_definite_bytes(previous_observer))
    |> bytearray.concat(cbor.serialise(observer_count))
    |> bytearray.concat(cbor.serialise(observer_seen))
}

const resolution_accumulator_domain =
  #"4d6964676172645265736f6c766564496e70757473416363756d756c61746f725631"

const resolution_schedule_domain =
  #"4d696467617264496e7075745265736f6c7574696f6e5363686564756c655631"

pub fn initial_resolution_accumulator() -> ByteArray {
  blake2b_256(resolution_accumulator_domain)
}

pub fn empty_resolution_schedule_hash() -> ByteArray {
  blake2b_256(resolution_schedule_domain)
}

pub fn resolution_schedule_node_hash(
  source_kind: Int,
  key: ByteArray,
  next_hash: ByteArray,
) -> ByteArray {
  expect source_kind == 0 || source_kind == 1
  expect bytearray.length(next_hash) == 32
  resolution_schedule_domain
    |> bytearray.concat(cbor.serialise(source_kind))
    |> bytearray.concat(encode_definite_bytes(key))
    |> bytearray.concat(next_hash)
    |> blake2b_256
}

pub type ScheduledInputV1 {
  source_kind: Int,
  key: ByteArray,
}

fn scheduled_input_resolution_hash(inputs: List<ScheduledInputV1>) -> ByteArray {
  when inputs is {
    [] -> empty_resolution_schedule_hash()
    [input, ..rest] ->
      resolution_schedule_node_hash(
        input.source_kind,
        input.key,
        scheduled_input_resolution_hash(rest),
      )
  }
}

fn compare_scheduled_inputs(
  left: ScheduledInputV1,
  right: ScheduledInputV1,
) -> Ordering {
  bytearray.compare(left.key, right.key)
}

pub fn transaction_resolution_schedule_hash(
  spend_inputs: List<MidgardTxInput>,
  reference_inputs: List<MidgardTxInput>,
) -> ByteArray {
  list.concat(
    spend_inputs
      |> list.map(
          fn(input) {
            ScheduledInputV1 {
              source_kind: 0,
              key: encode_midgard_tx_input(input),
            }
          },
        ),
    reference_inputs
      |> list.map(
          fn(input) {
            ScheduledInputV1 {
              source_kind: 1,
              key: encode_midgard_tx_input(input),
            }
          },
        ),
  )
    |> list.sort(compare_scheduled_inputs)
    |> scheduled_input_resolution_hash
}

pub type ResolveInputOutputProofV1 {
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  descriptor_cbor: ByteArray,
  output_proof: ledger_output_proof_v1.LedgerOutputProofControlV1,
}

fn encode_resolve_input_output_proof(
  pending: ResolveInputOutputProofV1,
) -> ByteArray {
  #"85"
    |> bytearray.concat(cbor.serialise(pending.source_kind))
    |> bytearray.concat(encode_definite_bytes(pending.key))
    |> bytearray.concat(encode_definite_bytes(pending.next_schedule_hash))
    |> bytearray.concat(encode_definite_bytes(pending.descriptor_cbor))
    |> bytearray.concat(
        encode_definite_bytes(
          ledger_output_proof_v1.encode_control_v1(pending.output_proof),
        ),
      )
}

fn resolve_input_output_proof_from_cbor(
  pending_cbor: ByteArray,
) -> ResolveInputOutputProofV1 {
  expect Some(data) = cbor.deserialise(pending_cbor)
  expect
      [
        source_kind_data, key_data, next_schedule_hash_data,
        descriptor_cbor_data, output_proof_cbor_data,
      ]
    = builtin.un_list_data(data)
  ResolveInputOutputProofV1 {
    source_kind: builtin.un_i_data(source_kind_data),
    key: builtin.un_b_data(key_data),
    next_schedule_hash: builtin.un_b_data(next_schedule_hash_data),
    descriptor_cbor: builtin.un_b_data(descriptor_cbor_data),
    output_proof: output_proof_cbor_data
      |> builtin.un_b_data
      |> ledger_output_proof_v1.decode_control_v1,
  }
}

fn encode_optional_resolve_input_output_proof(
  pending: Option<ResolveInputOutputProofV1>,
) -> ByteArray {
  when pending is {
    None -> encode_definite_bytes(#"00")
    Some(active) ->
      encode_resolve_input_output_proof(active)
        |> encode_definite_bytes
  }
}

pub fn encode_resolve_inputs_witness(
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
) -> ByteArray {
  expect cursor >= 0
  expect bytearray.length(accumulator) == 32
  expect bytearray.length(remaining_schedule_hash) == 32
  expect signer_count >= 0
  expect bytearray.length(signer_frontier_commitment) == 32
  expect bytearray.length(resolution_schedule_hash) == 32
  #"8b"
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
    |> bytearray.concat(cbor.serialise(cursor))
    |> bytearray.concat(encode_definite_bytes(accumulator))
    |> bytearray.concat(encode_definite_bytes(remaining_schedule_hash))
    |> bytearray.concat(cbor.serialise(signer_count))
    |> bytearray.concat(encode_definite_bytes(signer_frontier_commitment))
    |> bytearray.concat(encode_optional_resolve_input_output_proof(pending))
    |> bytearray.concat(encode_definite_bytes(resolution_schedule_hash))
}

pub type ObserverPurposeScanControlV1 {
  total_count: Int,
  seen: Int,
  previous_hash: ByteArray,
}

pub fn empty_observer_purpose_scan_control() -> ObserverPurposeScanControlV1 {
  ObserverPurposeScanControlV1 { total_count: 0, seen: 0, previous_hash: #"" }
}

pub fn encode_observer_purpose_scan_control(
  control: ObserverPurposeScanControlV1,
) -> ByteArray {
  expect control.total_count >= 0
  expect control.seen >= 0
  expect control.seen <= control.total_count
  expect
    if control.seen == 0 {
      control.previous_hash == #""
    } else {
      bytearray.length(control.previous_hash) == 28
    }
  #"83"
    |> bytearray.concat(cbor.serialise(control.total_count))
    |> bytearray.concat(encode_definite_bytes(control.previous_hash))
    |> bytearray.concat(cbor.serialise(control.seen))
}

pub type ReceivePurposeScanControlV1 {
  source_count: Int,
  source_peaks: List<validation_merkle_v1.FrontierPeak>,
  receive_count: Int,
  previous_hash: ByteArray,
  candidate_hash: ByteArray,
  descriptor_peaks: List<validation_merkle_v1.FrontierPeak>,
}

pub fn empty_receive_purpose_scan_control() -> ReceivePurposeScanControlV1 {
  ReceivePurposeScanControlV1 {
    source_count: 0,
    source_peaks: [],
    receive_count: 0,
    previous_hash: #"",
    candidate_hash: #"",
    descriptor_peaks: [],
  }
}

pub fn encode_receive_purpose_scan_control(
  control: ReceivePurposeScanControlV1,
) -> ByteArray {
  expect control.source_count >= 0
  expect
    validation_merkle_v1.frontier_is_well_formed(
      control.source_count,
      control.source_peaks,
    )
  expect control.receive_count >= 0
  expect
    if control.receive_count == 0 {
      control.previous_hash == #""
    } else {
      bytearray.length(control.previous_hash) == 28
    }
  expect or {
      control.candidate_hash == #"",
      and {
        bytearray.length(control.candidate_hash) == 28,
        or {
          control.previous_hash == #"",
          bytearray.compare(control.previous_hash, control.candidate_hash) == Less,
        },
      },
    }
  #"86"
    |> bytearray.concat(cbor.serialise(control.source_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.source_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.receive_count))
    |> bytearray.concat(encode_definite_bytes(control.previous_hash))
    |> bytearray.concat(encode_definite_bytes(control.candidate_hash))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.descriptor_peaks),
      )
}

pub fn encode_script_sources_witness(
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
  mint_fold: MintFoldControlV1,
  resolution_schedule_hash: ByteArray,
) -> ByteArray {
  expect resolved_input_count >= 0
  expect bytearray.length(resolved_inputs_accumulator) == 32
  expect signer_count >= 0
  expect bytearray.length(signer_frontier_commitment) == 32
  expect
    validation_merkle_v1.frontier_is_well_formed(
      replay_cursor,
      resolved_item_peaks,
    )
  expect stage >= 0
  expect source_count >= 0
  expect
    validation_merkle_v1.frontier_is_well_formed(source_count, source_peaks)
  expect redeemer_count >= 0
  expect
    validation_merkle_v1.frontier_is_well_formed(redeemer_count, redeemer_peaks)
  expect replay_cursor >= 0
  expect bytearray.length(replay_accumulator) == 32
  expect bytearray.length(replay_remaining_schedule_hash) == 32
  expect spend_index >= 0
  expect purpose_count >= 0
  expect
    validation_merkle_v1.frontier_is_well_formed(purpose_count, purpose_peaks)
  expect output_cursor >= 0
  expect output_count >= 0
  expect
    validation_merkle_v1.frontier_is_well_formed(output_count, output_peaks)
  expect output_cursor <= output_count
  expect output_total_count >= output_count
  expect source_total_count >= source_count
  expect redeemer_total_count >= redeemer_count
  expect bytearray.length(resolution_schedule_hash) == 32
  encode_definite_array_header(30)
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
    |> bytearray.concat(cbor.serialise(resolved_input_count))
    |> bytearray.concat(encode_definite_bytes(resolved_inputs_accumulator))
    |> bytearray.concat(cbor.serialise(signer_count))
    |> bytearray.concat(encode_definite_bytes(signer_frontier_commitment))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(resolved_item_peaks),
      )
    |> bytearray.concat(cbor.serialise(stage))
    |> bytearray.concat(cbor.serialise(source_count))
    |> bytearray.concat(validation_merkle_v1.encode_frontier(source_peaks))
    |> bytearray.concat(cbor.serialise(redeemer_count))
    |> bytearray.concat(validation_merkle_v1.encode_frontier(redeemer_peaks))
    |> bytearray.concat(cbor.serialise(replay_cursor))
    |> bytearray.concat(encode_definite_bytes(replay_accumulator))
    |> bytearray.concat(encode_definite_bytes(replay_remaining_schedule_hash))
    |> bytearray.concat(cbor.serialise(spend_index))
    |> bytearray.concat(cbor.serialise(purpose_count))
    |> bytearray.concat(validation_merkle_v1.encode_frontier(purpose_peaks))
    |> bytearray.concat(cbor.serialise(output_cursor))
    |> bytearray.concat(cbor.serialise(output_count))
    |> bytearray.concat(validation_merkle_v1.encode_frontier(output_peaks))
    |> bytearray.concat(cbor.serialise(output_total_count))
    |> bytearray.concat(encode_receive_purpose_scan_control(receive_scan))
    |> bytearray.concat(cbor.serialise(source_total_count))
    |> bytearray.concat(cbor.serialise(redeemer_total_count))
    |> bytearray.concat(encode_observer_purpose_scan_control(observer_scan))
    |> bytearray.concat(encode_mint_fold_control(mint_fold))
    |> bytearray.concat(encode_definite_bytes(resolution_schedule_hash))
}

pub fn resolved_input_accumulator_successor(
  accumulator: ByteArray,
  source_kind: Int,
  key: ByteArray,
  value: ByteArray,
) -> ByteArray {
  resolution_accumulator_domain
    |> bytearray.concat(accumulator)
    |> bytearray.concat(cbor.serialise(source_kind))
    |> bytearray.concat(encode_definite_bytes(key))
    |> bytearray.concat(encode_definite_bytes(value))
    |> blake2b_256
}

pub fn encode_terminal_rejection_witness(
  rejection_code: ByteArray,
  prior_ledger_root: ByteArray,
) -> ByteArray {
  #"84"
    |> bytearray.concat(cbor.serialise(2))
    |> bytearray.concat(encode_definite_bytes(rejection_code))
    |> bytearray.concat(encode_definite_bytes(prior_ledger_root))
    |> bytearray.concat(encode_definite_bytes(#"80"))
}

const reject_field_preimage_size = #"455f4649454c445f505245494d4147455f53495a45"

const reject_asset_count = #"455f41535345545f434f554e54"

const reject_invalid_field_type = #"455f494e56414c49445f4649454c445f54595045"

const reject_native_script_depth = #"455f4e41544956455f5343524950545f4445505448"

const reject_native_script_node_count =
  #"455f4e41544956455f5343524950545f4e4f44455f434f554e54"

fn canonical_argument_header_size(value: Int) -> Int {
  expect value >= 0
  if value < 24 {
    1
  } else if value < 256 {
    2
  } else if value < 65536 {
    3
  } else if value < 4294967296 {
    5
  } else {
    9
  }
}

fn transaction_field_preimage_length(
  lengths: NativeTxFieldPreimageLengthsV1,
  field_index: Int,
) -> Int {
  when field_index is {
    0 -> lengths.spend_inputs
    1 -> lengths.reference_inputs
    2 -> lengths.outputs
    3 -> lengths.required_observers
    4 -> lengths.required_signers
    5 -> lengths.mint
    6 -> lengths.script_witnesses
    7 -> lengths.address_witnesses
    8 -> lengths.redeemers
    _ -> fail @"unknown transaction field"
  }
}

fn transaction_field_commitment(
  compact: NativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
  field_index: Int,
) -> ByteArray {
  let NativeTxCompact { body, .. } = compact
  when field_index is {
    0 -> body.spend_inputs_hash
    1 -> body.reference_inputs_hash
    2 -> body.outputs_hash
    3 -> body.required_observers_hash
    4 -> body.required_signers_hash
    5 -> body.mint_hash
    6 -> witness_set.script_tx_wits_hash
    7 -> witness_set.addr_tx_wits_hash
    8 -> witness_set.redeemer_tx_wits_hash
    _ -> fail @"unknown transaction field"
  }
}

fn transaction_field_item_encoded_length(
  field_index: Int,
  item_length: Int,
) -> Option<Int> {
  expect item_length >= 0
  if or {
    field_index == 0,
    field_index == 1,
    field_index == 2,
    field_index == 3,
    field_index == 4,
    field_index == 7,
  } {
    Some(canonical_argument_header_size(item_length) + item_length)
  } else if or {
    field_index == 6,
    field_index == 8,
  } {
    Some(item_length)
  } else {
    expect field_index == 5
    if item_length == 0 {
      None
    } else {
      // A mint-policy item is [policy-id, asset-map]. The canonical field is
      // the corresponding map entry, so its one-byte array wrapper is absent.
      Some(item_length - 1)
    }
  }
}

fn canonical_scan_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  field_index: Int,
  item_index: Int,
  chunk_index: Int,
  item_count: Int,
  encoded_length: Int,
) -> Bool {
  and {
    post.phase == validation_trace_v1.CanonicalDecode,
    post.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      pre.program_counter + 1,
      encode_transaction_field_scan_witness(
        compact_cbor,
        witness_set_compact_cbor,
        field_preimage_lengths_cbor,
        context_cbor,
        field_index,
        item_index,
        chunk_index,
        item_count,
        encoded_length,
      ),
    ),
  }
}

fn canonical_scan_field_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  field_index: Int,
) -> Bool {
  if field_index < 8 {
    canonical_scan_successor_is_exact(
      pre,
      post,
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
      context_cbor,
      field_index + 1,
      0,
      0,
      -1,
      0,
    )
  } else {
    let compact_binding_witness_cbor = encode_compact_binding_witness(
      pre.transaction_id,
      pre.transaction_commitment,
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
      context_cbor,
    )
    and {
      post.phase == validation_trace_v1.CompactBinding,
      post.work_root == validation_trace_v1.hash_work_witness(
        validation_trace_v1.CompactBinding,
        pre.program_counter + 1,
        compact_binding_witness_cbor,
      ),
    }
  }
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

fn canonical_decode_control_from_witness(
  witness_cbor: ByteArray,
) -> CanonicalDecodeControlV1 {
  expect Some(data) = cbor.deserialise(witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data, field_index_data,
        item_index_data, chunk_index_data, item_count_data, encoded_length_data,
      ]
    = builtin.un_list_data(data)
  CanonicalDecodeControlV1 {
    compact_cbor: builtin.un_b_data(compact_cbor_data),
    witness_set_compact_cbor: builtin.un_b_data(witness_set_compact_cbor_data),
    field_preimage_lengths_cbor: builtin.un_b_data(
      field_preimage_lengths_cbor_data,
    ),
    context_cbor: builtin.un_b_data(context_cbor_data),
    field_index: builtin.un_i_data(field_index_data),
    item_index: builtin.un_i_data(item_index_data),
    chunk_index: builtin.un_i_data(chunk_index_data),
    item_count: builtin.un_i_data(item_count_data),
    encoded_length: builtin.un_i_data(encoded_length_data),
  }
}

fn canonical_decode_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: CanonicalDecodeControlV1,
  verified: VerifiedMidgardNativeTxCompact,
) -> Bool {
  let exact_witness = encode_transaction_field_scan_witness(
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
    control.context_cbor,
    control.field_index,
    control.item_index,
    control.chunk_index,
    control.item_count,
    control.encoded_length,
  )
  and {
    verified.version == 1,
    control.field_index >= 0,
    control.field_index <= 8,
    control.item_index >= 0,
    control.chunk_index >= 0,
    control.item_count >= -1,
    control.encoded_length >= 0,
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    witness.work_witness_cbor == exact_witness,
  }
}

fn verify_canonical_decode_empty(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: CanonicalDecodeControlV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let field_lengths = decode_native_tx_field_preimage_lengths_v1(
    control.field_preimage_lengths_cbor,
  )
  let expected_field_length = transaction_field_preimage_length(
    field_lengths,
    control.field_index,
  )
  let expected_field_commitment = transaction_field_commitment(
    verified.tx_compact,
    witness_set,
    control.field_index,
  )
  and {
    canonical_decode_control_is_bound(pre, witness, control, verified),
    expected_field_commitment == bounded_collection_v1.from_items(
      control.field_index,
      [],
    ),
    control.item_index == 0,
    control.chunk_index == 0,
    control.item_count == -1,
    control.encoded_length == 0,
    if expected_field_length != 1 {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_field_preimage_size,
      )
    } else {
      canonical_scan_field_successor_is_exact(
        pre,
        witness.claimed_successor,
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
        control.context_cbor,
        control.field_index,
      )
    },
  }
}

fn verify_canonical_decode_chunk(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: CanonicalDecodeControlV1,
  collection_proof: bounded_collection_v1.ItemProofV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let expected_field_commitment = transaction_field_commitment(
    verified.tx_compact,
    witness_set,
    control.field_index,
  )
  let field_lengths = decode_native_tx_field_preimage_lengths_v1(
    control.field_preimage_lengths_cbor,
  )
  let expected_field_length = transaction_field_preimage_length(
    field_lengths,
    control.field_index,
  )
  let first_chunk = and {
    control.item_index == 0,
    control.chunk_index == 0,
    control.item_count == -1,
    control.encoded_length == 0,
  }
  let continuing_chunk = and {
    control.item_count > 0,
    control.item_index < control.item_count,
    collection_proof.item_count == control.item_count,
  }
  let active_item_count = if first_chunk {
    collection_proof.item_count
  } else {
    control.item_count
  }
  let length_before_item = if first_chunk {
    canonical_argument_header_size(active_item_count)
  } else {
    control.encoded_length
  }
  let chunk_count = bounded_item_v1.chunk_count(chunk_proof.total_length)
  let item_finished = control.chunk_index + 1 == chunk_count
  and {
    canonical_decode_control_is_bound(pre, witness, control, verified),
    expected_field_commitment != bounded_collection_v1.from_items(
      control.field_index,
      [],
    ),
    or {
      first_chunk,
      continuing_chunk,
    },
    active_item_count > 0,
    collection_proof.field_index == control.field_index,
    collection_proof.item_count == active_item_count,
    collection_proof.item_index == control.item_index,
    collection_proof.item_length == chunk_proof.total_length,
    chunk_proof.field_index == control.field_index,
    chunk_proof.item_index == control.item_index,
    chunk_proof.chunk_index == control.chunk_index,
    bounded_collection_v1.verify_item(
      expected_field_commitment,
      collection_proof,
    ),
    bounded_item_v1.verify_chunk(collection_proof.item_commitment, chunk_proof),
    if !item_finished {
      canonical_scan_successor_is_exact(
        pre,
        witness.claimed_successor,
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
        control.context_cbor,
        control.field_index,
        control.item_index,
        control.chunk_index + 1,
        active_item_count,
        length_before_item,
      )
    } else {
      when
        transaction_field_item_encoded_length(
          control.field_index,
          chunk_proof.total_length,
        )
      is {
        None ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          )
        Some(item_encoded_length) -> {
          let next_encoded_length = length_before_item + item_encoded_length
          if control.item_index + 1 < active_item_count {
            canonical_scan_successor_is_exact(
              pre,
              witness.claimed_successor,
              control.compact_cbor,
              control.witness_set_compact_cbor,
              control.field_preimage_lengths_cbor,
              control.context_cbor,
              control.field_index,
              control.item_index + 1,
              0,
              active_item_count,
              next_encoded_length,
            )
          } else if next_encoded_length != expected_field_length {
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_field_preimage_size,
            )
          } else {
            canonical_scan_field_successor_is_exact(
              pre,
              witness.claimed_successor,
              control.compact_cbor,
              control.witness_set_compact_cbor,
              control.field_preimage_lengths_cbor,
              control.context_cbor,
              control.field_index,
            )
          }
        }
      }
    },
  }
}

pub fn verify_canonical_decode_empty_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  verify_canonical_decode_empty(
    pre,
    witness,
    canonical_decode_control_from_witness(witness.work_witness_cbor),
  )
}

pub fn verify_canonical_decode_chunk_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  collection_proof: bounded_collection_v1.ItemProofV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
) -> Bool {
  verify_canonical_decode_chunk(
    pre,
    witness,
    canonical_decode_control_from_witness(witness.work_witness_cbor),
    collection_proof,
    chunk_proof,
  )
}

fn verify_canonical_decode(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let control = canonical_decode_control_from_witness(witness.work_witness_cbor)
  when auxiliary is {
    NoAuxiliaryWitness -> verify_canonical_decode_empty(pre, witness, control)
    TransactionFieldChunkWitness { collection_proof, chunk_proof } ->
      verify_canonical_decode_chunk(
        pre,
        witness,
        control,
        collection_proof,
        chunk_proof,
      )
    _ -> False
  }
}

fn verify_compact_binding(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  expect Some(data) = cbor.deserialise(witness.work_witness_cbor)
  expect
      [
        transaction_id_data, commitment_data, compact_cbor_data,
        witness_set_compact_cbor_data, field_preimage_lengths_cbor_data,
        context_cbor_data,
      ]
    = builtin.un_list_data(data)
  let transaction_id = builtin.un_b_data(transaction_id_data)
  let commitment = builtin.un_b_data(commitment_data)
  let compact_cbor = builtin.un_b_data(compact_cbor_data)
  let witness_set_compact_cbor = builtin.un_b_data(
    witness_set_compact_cbor_data,
  )
  let field_preimage_lengths_cbor = builtin.un_b_data(
    field_preimage_lengths_cbor_data,
  )
  let context_cbor = builtin.un_b_data(context_cbor_data)
  let Pair(verified_source, _) = verify_native_tx_proof_source_v1(
    transaction_id,
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
  )
  and {
    verified_source.version == 1,
    transaction_id == pre.transaction_id,
    commitment == pre.transaction_commitment,
    commitment == native_tx_proof_commitment_v1(
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
    ),
    validation_trace_v1.hash_validation_context(context_cbor) == pre.validation_context_hash,
    witness.work_witness_cbor == encode_compact_binding_witness(
      transaction_id,
      commitment,
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
      context_cbor,
    ),
    witness.claimed_successor.phase == validation_trace_v1.StaticLedgerRules,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.StaticLedgerRules,
      pre.program_counter + 1,
      encode_static_rules_witness(
        compact_cbor,
        witness_set_compact_cbor,
        field_preimage_lengths_cbor,
        context_cbor,
      ),
    ),
  }
}

const reject_empty_inputs = #"455f454d5054595f494e50555453"

const reject_duplicate_input = #"455f4455504c49434154455f494e5055545f494e5f5458"

const reject_network_id_mismatch = #"455f4e4554574f524b5f49445f4d49534d41544348"

const reject_min_fee = #"455f4d494e5f464545"

const consensus_profile_v1_id = #"6d6964676172642d636f6e73656e7375732d7631"

pub type ValidationContextV1 {
  block_end_time_ms: Int,
  expected_network_id: Int,
  min_fee_a: Int,
  min_fee_b: Int,
  block_slot: Int,
}

fn decode_validation_context(context_cbor: ByteArray) -> ValidationContextV1 {
  expect Some(data) = cbor.deserialise(context_cbor)
  expect
      [
        version_data, profile_id_data, block_end_time_data,
        expected_network_id_data, min_fee_a_data, min_fee_b_data,
        block_slot_data,
      ]
    = builtin.un_list_data(data)
  expect builtin.un_i_data(version_data) == 1
  expect builtin.un_b_data(profile_id_data) == consensus_profile_v1_id
  let block_end_time_ms = builtin.un_i_data(block_end_time_data)
  let expected_network_id = builtin.un_i_data(expected_network_id_data)
  let min_fee_a = builtin.un_i_data(min_fee_a_data)
  let min_fee_b = builtin.un_i_data(min_fee_b_data)
  let block_slot = builtin.un_i_data(block_slot_data)
  expect block_end_time_ms >= 0
  expect or {
      expected_network_id == 0,
      expected_network_id == 1,
    }
  expect min_fee_a >= 0
  expect min_fee_b >= 0
  expect block_slot >= 0
  ValidationContextV1 {
    block_end_time_ms,
    expected_network_id,
    min_fee_a,
    min_fee_b,
    block_slot,
  }
}

fn rejected_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  rejection_code: ByteArray,
) -> Bool {
  and {
    post.phase == validation_trace_v1.Terminal,
    post.verdict == validation_trace_v1.Rejected,
    post.rejection_code_hash == validation_trace_v1.hash_rejection_code(
      rejection_code,
    ),
    post.ledger_delta_root == validation_merkle_v1.frontier_commitment(0, []),
    post.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      pre.program_counter + 1,
      encode_terminal_rejection_witness(rejection_code, pre.prior_ledger_root),
    ),
  }
}

const max_tx_size_derived_collection_item_count: Int =
  bounded_collection_v1.max_tx_size_derived_item_count

const max_serialized_output_preimage_bytes: Int = 16384

const reject_invalid_validity_interval_format =
  #"455f494e56414c49445f56414c49444954595f494e54455256414c5f464f524d4154"

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

fn input_sets_control_from_witness(
  witness_cbor: ByteArray,
) -> InputSetsControlV1 {
  expect Some(data) = cbor.deserialise(witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data, spend_count_data,
        reference_count_data, spend_seen_data, reference_seen_data,
        previous_key_data, resolution_schedule_hash_data,
      ]
    = builtin.un_list_data(data)
  InputSetsControlV1 {
    compact_cbor: builtin.un_b_data(compact_cbor_data),
    witness_set_compact_cbor: builtin.un_b_data(witness_set_compact_cbor_data),
    field_preimage_lengths_cbor: builtin.un_b_data(
      field_preimage_lengths_cbor_data,
    ),
    context_cbor: builtin.un_b_data(context_cbor_data),
    spend_count: builtin.un_i_data(spend_count_data),
    reference_count: builtin.un_i_data(reference_count_data),
    spend_seen: builtin.un_i_data(spend_seen_data),
    reference_seen: builtin.un_i_data(reference_seen_data),
    previous_key: builtin.un_b_data(previous_key_data),
    resolution_schedule_hash: builtin.un_b_data(resolution_schedule_hash_data),
  }
}

fn input_sets_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: InputSetsControlV1,
  verified: VerifiedMidgardNativeTxCompact,
) -> Bool {
  let body = verified.tx_compact.body
  let spend_is_empty = body.spend_inputs_hash == bounded_collection_v1.from_items(
    0,
    [],
  )
  let reference_is_empty = body.reference_inputs_hash == bounded_collection_v1.from_items(
    1,
    [],
  )
  let seen = control.spend_seen + control.reference_seen
  and {
    verified.version == 1,
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    control.spend_count >= -1,
    control.spend_count <= max_tx_size_derived_collection_item_count,
    control.reference_count >= -1,
    control.reference_count <= max_tx_size_derived_collection_item_count,
    control.spend_seen >= 0,
    control.reference_seen >= 0,
    if spend_is_empty {
      control.spend_count == 0 && control.spend_seen == 0
    } else if control.spend_count == -1 {
      control.spend_seen == 0
    } else {
      control.spend_count > 0 && control.spend_seen <= control.spend_count
    },
    if reference_is_empty {
      control.reference_count == 0 && control.reference_seen == 0
    } else if control.reference_count == -1 {
      control.reference_seen == 0
    } else {
      control.reference_count > 0 && control.reference_seen <= control.reference_count
    },
    if seen == 0 {
      and {
        control.previous_key == #"",
        control.resolution_schedule_hash == empty_resolution_schedule_hash(),
      }
    } else {
      bytearray.length(control.previous_key) == 36
    },
    bytearray.length(control.resolution_schedule_hash) == 32,
    witness.work_witness_cbor == encode_input_sets_scan_witness(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      control.context_cbor,
      control.spend_count,
      control.reference_count,
      control.spend_seen,
      control.reference_seen,
      control.previous_key,
      control.resolution_schedule_hash,
    ),
  }
}

fn verify_input_sets_empty(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: InputSetsControlV1,
) -> Bool {
  let Pair(verified, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  and {
    input_sets_control_is_bound(pre, witness, control, verified),
    control.spend_count == 0,
    control.spend_seen == 0,
    control.reference_seen == 0,
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_empty_inputs,
    ),
  }
}

fn input_sets_item_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  control: InputSetsControlV1,
  body: NativeTxBodyCompact,
  witness_set: NativeTxWitnessSetCompact,
  spend_count: Int,
  reference_count: Int,
  spend_seen: Int,
  reference_seen: Int,
  key: ByteArray,
  resolution_schedule_hash: ByteArray,
) -> Bool {
  let complete = and {
    spend_count > 0,
    reference_count >= 0,
    spend_seen == spend_count,
    reference_seen == reference_count,
  }
  if complete {
    let malformed_validity_interval = or {
      body.validity_interval_start < -1,
      body.validity_interval_end < -1,
      and {
        body.validity_interval_start >= 0,
        body.validity_interval_end >= 0,
        body.validity_interval_start > body.validity_interval_end,
      },
    }
    if malformed_validity_interval {
      rejected_successor_is_exact(
        pre,
        post,
        reject_invalid_validity_interval_format,
      )
    } else {
      let address_count = if witness_set.addr_tx_wits_hash == bounded_collection_v1.from_items(
        7,
        [],
      ) {
        0
      } else {
        -1
      }
      let required_count = if body.required_signers_hash == bounded_collection_v1.from_items(
        4,
        [],
      ) {
        0
      } else {
        -1
      }
      and {
        post.phase == validation_trace_v1.Signatures,
        post.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.Signatures,
          pre.program_counter + 1,
          encode_signatures_scan_witness(
            control.compact_cbor,
            control.witness_set_compact_cbor,
            control.field_preimage_lengths_cbor,
            control.context_cbor,
            resolution_schedule_hash,
            0,
            address_count,
            required_count,
            0,
            0,
            #"",
            #"",
            0,
            [],
            0,
          ),
        ),
      }
    }
  } else {
    and {
      post.phase == validation_trace_v1.InputSets,
      post.work_root == validation_trace_v1.hash_work_witness(
        validation_trace_v1.InputSets,
        pre.program_counter + 1,
        encode_input_sets_scan_witness(
          control.compact_cbor,
          control.witness_set_compact_cbor,
          control.field_preimage_lengths_cbor,
          control.context_cbor,
          spend_count,
          reference_count,
          spend_seen,
          reference_seen,
          key,
          resolution_schedule_hash,
        ),
      ),
    }
  }
}

fn verify_input_sets_item(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: InputSetsControlV1,
  collection_proof: bounded_collection_v1.ItemProofV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let field_index = collection_proof.field_index
  let source_kind = field_index
  let expected_field_commitment = transaction_field_commitment(
    verified.tx_compact,
    witness_set,
    field_index,
  )
  let prior_count = if field_index == 0 {
    control.spend_count
  } else {
    control.reference_count
  }
  let active_count = if prior_count == -1 {
    collection_proof.item_count
  } else {
    prior_count
  }
  let prior_seen = if field_index == 0 {
    control.spend_seen
  } else {
    control.reference_seen
  }
  let key = chunk_proof.chunk
  let input = decode_midgard_tx_input_cbor(key)
  let next_schedule_hash = resolution_schedule_node_hash(
    source_kind,
    key,
    control.resolution_schedule_hash,
  )
  let next_spend_count = if field_index == 0 {
    active_count
  } else {
    control.spend_count
  }
  let next_reference_count = if field_index == 1 {
    active_count
  } else {
    control.reference_count
  }
  let next_spend_seen = if field_index == 0 {
    control.spend_seen + 1
  } else {
    control.spend_seen
  }
  let next_reference_seen = if field_index == 1 {
    control.reference_seen + 1
  } else {
    control.reference_seen
  }
  let duplicate = and {
    bytearray.length(control.previous_key) > 0,
    bytearray.compare(key, control.previous_key) == Equal,
  }
  let correctly_ordered = or {
    control.previous_key == #"",
    bytearray.compare(key, control.previous_key) == Less,
  }
  and {
    input_sets_control_is_bound(pre, witness, control, verified),
    control.spend_count != 0,
    or {
      field_index == 0,
      field_index == 1,
    },
    prior_count == -1 || prior_count == collection_proof.item_count,
    active_count > 0,
    active_count <= max_tx_size_derived_collection_item_count,
    prior_seen < active_count,
    collection_proof.item_count == active_count,
    collection_proof.item_index >= 0,
    collection_proof.item_index < active_count,
    collection_proof.item_length == chunk_proof.total_length,
    chunk_proof.field_index == field_index,
    chunk_proof.item_index == collection_proof.item_index,
    chunk_proof.chunk_index == 0,
    bounded_item_v1.chunk_count(chunk_proof.total_length) == 1,
    encode_midgard_tx_input(input) == key,
    bounded_collection_v1.verify_item(
      expected_field_commitment,
      collection_proof,
    ),
    bounded_item_v1.verify_chunk(collection_proof.item_commitment, chunk_proof),
    if duplicate {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_duplicate_input,
      )
    } else {
      and {
        correctly_ordered,
        input_sets_item_successor_is_exact(
          pre,
          witness.claimed_successor,
          control,
          verified.tx_compact.body,
          witness_set,
          next_spend_count,
          next_reference_count,
          next_spend_seen,
          next_reference_seen,
          key,
          next_schedule_hash,
        ),
      }
    },
  }
}

pub fn verify_input_sets_empty_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  verify_input_sets_empty(
    pre,
    witness,
    input_sets_control_from_witness(witness.work_witness_cbor),
  )
}

pub fn verify_input_sets_item_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  collection_proof: bounded_collection_v1.ItemProofV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
) -> Bool {
  verify_input_sets_item(
    pre,
    witness,
    input_sets_control_from_witness(witness.work_witness_cbor),
    collection_proof,
    chunk_proof,
  )
}

fn verify_input_sets(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let control = input_sets_control_from_witness(witness.work_witness_cbor)
  when auxiliary is {
    NoAuxiliaryWitness -> verify_input_sets_empty(pre, witness, control)
    TransactionFieldChunkWitness { collection_proof, chunk_proof } ->
      verify_input_sets_item(
        pre,
        witness,
        control,
        collection_proof,
        chunk_proof,
      )
    _ -> False
  }
}

fn static_rules_rejection(
  body: NativeTxBodyCompact,
  canonical_tx_size: Int,
  context: ValidationContextV1,
) -> Option<ByteArray> {
  if and {
    body.network_id != 255,
    body.network_id != context.expected_network_id,
  } {
    Some(reject_network_id_mismatch)
  } else if body.fee < context.min_fee_a * canonical_tx_size + context.min_fee_b {
    Some(reject_min_fee)
  } else {
    None
  }
}

fn verify_static_rules(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  expect Some(data) = cbor.deserialise(witness.work_witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data,
      ]
    = builtin.un_list_data(data)
  let compact_cbor = builtin.un_b_data(compact_cbor_data)
  let witness_set_compact_cbor = builtin.un_b_data(
    witness_set_compact_cbor_data,
  )
  let field_preimage_lengths_cbor = builtin.un_b_data(
    field_preimage_lengths_cbor_data,
  )
  let context_cbor = builtin.un_b_data(context_cbor_data)
  let Pair(verified, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
  )
  let field_lengths = decode_native_tx_field_preimage_lengths_v1(
    field_preimage_lengths_cbor,
  )
  let context = decode_validation_context(context_cbor)
  and {
    native_tx_proof_commitment_v1(
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(context_cbor) == pre.validation_context_hash,
    witness.work_witness_cbor == encode_static_rules_witness(
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
      context_cbor,
    ),
    when
      static_rules_rejection(
        verified.tx_compact.body,
        native_tx_canonical_size_v1(verified.tx_compact, field_lengths),
        context,
      )
    is {
      Some(rejection_code) ->
        rejected_successor_is_exact(
          pre,
          witness.claimed_successor,
          rejection_code,
        )
      None -> {
        let body = verified.tx_compact.body
        let spend_count = if body.spend_inputs_hash == bounded_collection_v1.from_items(
          0,
          [],
        ) {
          0
        } else {
          -1
        }
        let reference_count = if body.reference_inputs_hash == bounded_collection_v1.from_items(
          1,
          [],
        ) {
          0
        } else {
          -1
        }
        and {
          witness.claimed_successor.phase == validation_trace_v1.InputSets,
          witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
            validation_trace_v1.InputSets,
            pre.program_counter + 1,
            encode_input_sets_scan_witness(
              compact_cbor,
              witness_set_compact_cbor,
              field_preimage_lengths_cbor,
              context_cbor,
              spend_count,
              reference_count,
              0,
              0,
              #"",
              empty_resolution_schedule_hash(),
            ),
          ),
        }
      }
    },
  }
}

const reject_missing_required_witness =
  #"455f4d495353494e475f52455155495245445f5749544e455353"

const reject_invalid_signature = #"455f494e56414c49445f5349474e4154555245"

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

fn signatures_control_from_witness(
  witness_cbor: ByteArray,
) -> SignaturesControlV1 {
  expect Some(data) = cbor.deserialise(witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data,
        resolution_schedule_hash_data, stage_data, address_count_data,
        required_count_data, address_seen_data, required_seen_data,
        previous_order_key_data, previous_signer_hash_data, signer_count_data,
        signer_peaks_data, invalid_signature_seen_data,
      ]
    = builtin.un_list_data(data)
  SignaturesControlV1 {
    compact_cbor: builtin.un_b_data(compact_cbor_data),
    witness_set_compact_cbor: builtin.un_b_data(witness_set_compact_cbor_data),
    field_preimage_lengths_cbor: builtin.un_b_data(
      field_preimage_lengths_cbor_data,
    ),
    context_cbor: builtin.un_b_data(context_cbor_data),
    resolution_schedule_hash: builtin.un_b_data(resolution_schedule_hash_data),
    stage: builtin.un_i_data(stage_data),
    address_count: builtin.un_i_data(address_count_data),
    required_count: builtin.un_i_data(required_count_data),
    address_seen: builtin.un_i_data(address_seen_data),
    required_seen: builtin.un_i_data(required_seen_data),
    previous_order_key: builtin.un_b_data(previous_order_key_data),
    previous_signer_hash: builtin.un_b_data(previous_signer_hash_data),
    signer_count: builtin.un_i_data(signer_count_data),
    signer_peaks: decode_frontier_peaks(signer_peaks_data),
    invalid_signature_seen: builtin.un_i_data(invalid_signature_seen_data),
  }
}

fn signatures_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: SignaturesControlV1,
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
) -> Bool {
  let address_is_empty = witness_set.addr_tx_wits_hash == bounded_collection_v1.from_items(
    7,
    [],
  )
  let required_is_empty = verified.tx_compact.body.required_signers_hash == bounded_collection_v1.from_items(
    4,
    [],
  )
  let address_complete = and {
    control.address_count >= 0,
    control.address_seen == control.address_count,
  }
  let required_complete = and {
    control.required_count >= 0,
    control.required_seen == control.required_count,
  }
  and {
    verified.version == 1,
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    bytearray.length(control.resolution_schedule_hash) == 32,
    control.stage >= 0,
    control.stage <= 2,
    control.address_count >= -1,
    control.address_count <= max_tx_size_derived_collection_item_count,
    control.required_count >= -1,
    control.required_count <= max_tx_size_derived_collection_item_count,
    control.address_seen >= 0,
    control.required_seen >= 0,
    if address_is_empty {
      control.address_count == 0 && control.address_seen == 0
    } else if control.address_count == -1 {
      control.address_seen == 0
    } else {
      control.address_count > 0 && control.address_seen <= control.address_count
    },
    if required_is_empty {
      control.required_count == 0 && control.required_seen == 0
    } else if control.required_count == -1 {
      control.required_seen == 0
    } else {
      control.required_count > 0 && control.required_seen <= control.required_count
    },
    control.signer_count >= 0,
    control.signer_count <= control.address_seen,
    validation_merkle_v1.frontier_is_well_formed(
      control.signer_count,
      control.signer_peaks,
    ),
    control.invalid_signature_seen == 0 || control.invalid_signature_seen == 1,
    control.invalid_signature_seen <= control.address_seen,
    if control.stage == 0 {
      and {
        control.required_seen == 0,
        if control.address_seen == 0 {
          and {
            control.previous_order_key == #"",
            control.previous_signer_hash == #"",
            control.signer_count == 0,
          }
        } else {
          and {
            bytearray.length(control.previous_order_key) > 0,
            bytearray.length(control.previous_signer_hash) == 28,
            control.signer_count > 0,
          }
        },
      }
    } else {
      and {
        address_complete,
        control.previous_order_key == #"",
        control.previous_signer_hash == #"",
        if control.stage == 2 {
          required_complete
        } else {
          True
        },
      }
    },
    witness.work_witness_cbor == encode_signatures_scan_witness(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      control.context_cbor,
      control.resolution_schedule_hash,
      control.stage,
      control.address_count,
      control.required_count,
      control.address_seen,
      control.required_seen,
      control.previous_order_key,
      control.previous_signer_hash,
      control.signer_count,
      control.signer_peaks,
      control.invalid_signature_seen,
    ),
  }
}

fn signatures_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  control: SignaturesControlV1,
) -> Bool {
  and {
    post.phase == validation_trace_v1.Signatures,
    post.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.Signatures,
      pre.program_counter + 1,
      encode_signatures_scan_witness(
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
        control.context_cbor,
        control.resolution_schedule_hash,
        control.stage,
        control.address_count,
        control.required_count,
        control.address_seen,
        control.required_seen,
        control.previous_order_key,
        control.previous_signer_hash,
        control.signer_count,
        control.signer_peaks,
        control.invalid_signature_seen,
      ),
    ),
  }
}

fn signatures_after_required_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  control: SignaturesControlV1,
) -> Bool {
  if control.invalid_signature_seen == 1 {
    rejected_successor_is_exact(pre, post, reject_invalid_signature)
  } else {
    signatures_successor_is_exact(
      pre,
      post,
      SignaturesControlV1 { ..control, stage: 2 },
    )
  }
}

fn signature_address_order_key(
  signer_hash: ByteArray,
  witness_cbor: ByteArray,
  item_index: Int,
) -> ByteArray {
  signer_hash
    |> bytearray.concat(witness_cbor)
    |> bytearray.concat(cbor.serialise(item_index))
}

fn verify_signature_address_item(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: SignaturesControlV1,
  collection_proof: bounded_collection_v1.ItemProofV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let active_count = if control.address_count == -1 {
    collection_proof.item_count
  } else {
    control.address_count
  }
  let address_witness = decode_midgard_address_witness_cbor(chunk_proof.chunk)
  let signer_hash = blake2b_224(address_witness.verification_key)
  let order_key = signature_address_order_key(
    signer_hash,
    chunk_proof.chunk,
    collection_proof.item_index,
  )
  let new_signer = control.previous_signer_hash != signer_hash
  let next_signer_count = if new_signer {
    control.signer_count + 1
  } else {
    control.signer_count
  }
  let next_signer_peaks = if new_signer {
    validation_merkle_v1.append_leaf(
      control.signer_count,
      control.signer_peaks,
      script_proof_v1.signer_leaf_hash(signer_hash),
    )
  } else {
    control.signer_peaks
  }
  let next_invalid_signature_seen = if and {
    control.invalid_signature_seen == 0,
    !verify_ed25519_signature(
      address_witness.verification_key,
      pre.transaction_id,
      address_witness.signature,
    ),
  } {
    1
  } else {
    control.invalid_signature_seen
  }
  let next_address_seen = control.address_seen + 1
  let next_control = if next_address_seen == active_count {
    SignaturesControlV1 {
      ..control,
      stage: 1,
      address_count: active_count,
      address_seen: next_address_seen,
      previous_order_key: #"",
      previous_signer_hash: #"",
      signer_count: next_signer_count,
      signer_peaks: next_signer_peaks,
      invalid_signature_seen: next_invalid_signature_seen,
    }
  } else {
    SignaturesControlV1 {
      ..control,
      address_count: active_count,
      address_seen: next_address_seen,
      previous_order_key: order_key,
      previous_signer_hash: signer_hash,
      signer_count: next_signer_count,
      signer_peaks: next_signer_peaks,
      invalid_signature_seen: next_invalid_signature_seen,
    }
  }
  and {
    signatures_control_is_bound(pre, witness, control, verified, witness_set),
    control.stage == 0,
    control.address_count != 0,
    collection_proof.field_index == 7,
    chunk_proof.field_index == 7,
    control.address_count == -1 || control.address_count == collection_proof.item_count,
    active_count > 0,
    active_count <= max_tx_size_derived_collection_item_count,
    control.address_seen < active_count,
    collection_proof.item_count == active_count,
    collection_proof.item_length == chunk_proof.total_length,
    chunk_proof.item_index == collection_proof.item_index,
    chunk_proof.chunk_index == 0,
    bounded_item_v1.chunk_count(chunk_proof.total_length) == 1,
    bounded_collection_v1.verify_item(
      witness_set.addr_tx_wits_hash,
      collection_proof,
    ),
    bounded_item_v1.verify_chunk(collection_proof.item_commitment, chunk_proof),
    or {
      control.previous_order_key == #"",
      bytearray.compare(control.previous_order_key, order_key) == Less,
    },
    signatures_successor_is_exact(pre, witness.claimed_successor, next_control),
  }
}

fn verify_signatures_advance(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: SignaturesControlV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  and {
    signatures_control_is_bound(pre, witness, control, verified, witness_set),
    if control.stage == 0 {
      and {
        control.address_count == 0,
        signatures_successor_is_exact(
          pre,
          witness.claimed_successor,
          SignaturesControlV1 { ..control, stage: 1 },
        ),
      }
    } else if control.stage == 1 {
      and {
        control.required_count == 0,
        signatures_after_required_successor_is_exact(
          pre,
          witness.claimed_successor,
          control,
        ),
      }
    } else {
      False
    },
  }
}

fn required_signer_membership_is_valid(
  signer_hash: ByteArray,
  control: SignaturesControlV1,
  proof: SignerSetProofV1,
) -> Bool {
  when proof is {
    SignerMembershipProof { peaks, signer_index, siblings } -> and {
        peaks == control.signer_peaks,
        signer_membership_is_valid(
          signer_hash,
          control.signer_count,
          validation_merkle_v1.frontier_commitment(
            control.signer_count,
            control.signer_peaks,
          ),
          peaks,
          signer_index,
          siblings,
        ),
      }
    _ -> False
  }
}

fn required_signer_non_membership_is_valid(
  signer_hash: ByteArray,
  control: SignaturesControlV1,
  proof: SignerSetProofV1,
) -> Bool {
  signer_non_membership_is_valid(
    signer_hash,
    control.signer_count,
    validation_merkle_v1.frontier_commitment(
      control.signer_count,
      control.signer_peaks,
    ),
    proof,
  )
}

fn verify_required_signer_item(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: SignaturesControlV1,
  collection_proof: bounded_collection_v1.ItemProofV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  signer_proof: SignerSetProofV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let active_count = if control.required_count == -1 {
    collection_proof.item_count
  } else {
    control.required_count
  }
  let signer_hash = chunk_proof.chunk
  let next_required_seen = control.required_seen + 1
  let next_control = SignaturesControlV1 {
    ..control,
    required_count: active_count,
    required_seen: next_required_seen,
  }
  let present = required_signer_membership_is_valid(
    signer_hash,
    control,
    signer_proof,
  )
  let absent = required_signer_non_membership_is_valid(
    signer_hash,
    control,
    signer_proof,
  )
  and {
    signatures_control_is_bound(pre, witness, control, verified, witness_set),
    control.stage == 1,
    control.required_count != 0,
    collection_proof.field_index == 4,
    chunk_proof.field_index == 4,
    control.required_count == -1 || control.required_count == collection_proof.item_count,
    active_count > 0,
    active_count <= max_tx_size_derived_collection_item_count,
    control.required_seen < active_count,
    collection_proof.item_count == active_count,
    collection_proof.item_index == control.required_seen,
    collection_proof.item_length == 28,
    chunk_proof.total_length == 28,
    chunk_proof.item_index == collection_proof.item_index,
    chunk_proof.chunk_index == 0,
    bytearray.length(signer_hash) == 28,
    bounded_collection_v1.verify_item(
      verified.tx_compact.body.required_signers_hash,
      collection_proof,
    ),
    bounded_item_v1.verify_chunk(collection_proof.item_commitment, chunk_proof),
    if absent {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_missing_required_witness,
      )
    } else if present {
      if next_required_seen == active_count {
        signatures_after_required_successor_is_exact(
          pre,
          witness.claimed_successor,
          next_control,
        )
      } else {
        signatures_successor_is_exact(
          pre,
          witness.claimed_successor,
          next_control,
        )
      }
    } else {
      False
    },
  }
}

fn verify_signatures_handoff(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: SignaturesControlV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let script_count = if witness_set.script_tx_wits_hash == bounded_collection_v1.from_items(
    6,
    [],
  ) {
    0
  } else {
    -1
  }
  and {
    signatures_control_is_bound(pre, witness, control, verified, witness_set),
    control.stage == 2,
    witness.claimed_successor.phase == validation_trace_v1.PhaseANativeScripts,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseANativeScripts,
      pre.program_counter + 1,
      encode_phase_a_native_scripts_scan_witness(
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
        control.context_cbor,
        control.resolution_schedule_hash,
        0,
        script_count,
        0,
        0,
        0,
        #"",
        0,
        #"",
        0,
        0,
        0,
        control.signer_count,
        control.signer_peaks,
        #"",
      ),
    ),
  }
}

fn verify_signatures(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let control = signatures_control_from_witness(witness.work_witness_cbor)
  when auxiliary is {
    NoAuxiliaryWitness ->
      if control.stage == 2 {
        verify_signatures_handoff(pre, witness, control)
      } else {
        verify_signatures_advance(pre, witness, control)
      }
    TransactionFieldChunkWitness { collection_proof, chunk_proof } ->
      verify_signature_address_item(
        pre,
        witness,
        control,
        collection_proof,
        chunk_proof,
      )
    RequiredSignerItemWitness { collection_proof, chunk_proof, signer_proof } ->
      verify_required_signer_item(
        pre,
        witness,
        control,
        collection_proof,
        chunk_proof,
        signer_proof,
      )
    _ -> False
  }
}

pub fn verify_signatures_advance_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  verify_signatures_advance(
    pre,
    witness,
    signatures_control_from_witness(witness.work_witness_cbor),
  )
}

pub fn verify_signature_address_item_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  collection_proof: bounded_collection_v1.ItemProofV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
) -> Bool {
  verify_signature_address_item(
    pre,
    witness,
    signatures_control_from_witness(witness.work_witness_cbor),
    collection_proof,
    chunk_proof,
  )
}

pub fn verify_required_signer_item_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  collection_proof: bounded_collection_v1.ItemProofV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  signer_proof: SignerSetProofV1,
) -> Bool {
  verify_required_signer_item(
    pre,
    witness,
    signatures_control_from_witness(witness.work_witness_cbor),
    collection_proof,
    chunk_proof,
    signer_proof,
  )
}

pub fn verify_signatures_handoff_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  verify_signatures_handoff(
    pre,
    witness,
    signatures_control_from_witness(witness.work_witness_cbor),
  )
}

const reject_native_script_invalid =
  #"455f4e41544956455f5343524950545f494e56414c4944"

const reject_plutus_script_invalid =
  #"455f504c555455535f5343524950545f494e56414c4944"

const max_aggregate_field_preimage_bytes: Int = 32768

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

type NativeChunkWindowV1 {
  bytes: ByteArray,
  offset: Int,
}

fn phase_a_native_control_from_witness(
  witness_cbor: ByteArray,
) -> PhaseANativeScriptsControlV1 {
  expect Some(data) = cbor.deserialise(witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data,
        resolution_schedule_hash_data, stage_data, script_count_data,
        script_seen_data, contains_non_native_script_data, item_length_data,
        item_commitment_data, cursor_data, stack_root_data, stack_depth_data,
        node_count_data, result_data, signer_count_data,
        signer_continuation_data,
      ]
    = builtin.un_list_data(data)
  expect [signer_peaks_data,
        continuation_cbor_data] = builtin.un_list_data(signer_continuation_data)
  PhaseANativeScriptsControlV1 {
    compact_cbor: builtin.un_b_data(compact_cbor_data),
    witness_set_compact_cbor: builtin.un_b_data(witness_set_compact_cbor_data),
    field_preimage_lengths_cbor: builtin.un_b_data(
      field_preimage_lengths_cbor_data,
    ),
    context_cbor: builtin.un_b_data(context_cbor_data),
    resolution_schedule_hash: builtin.un_b_data(resolution_schedule_hash_data),
    stage: builtin.un_i_data(stage_data),
    script_count: builtin.un_i_data(script_count_data),
    script_seen: builtin.un_i_data(script_seen_data),
    contains_non_native_script: builtin.un_i_data(
      contains_non_native_script_data,
    ),
    item_length: builtin.un_i_data(item_length_data),
    item_commitment: builtin.un_b_data(item_commitment_data),
    cursor: builtin.un_i_data(cursor_data),
    stack_root: builtin.un_b_data(stack_root_data),
    stack_depth: builtin.un_i_data(stack_depth_data),
    node_count: builtin.un_i_data(node_count_data),
    result: builtin.un_i_data(result_data),
    signer_count: builtin.un_i_data(signer_count_data),
    signer_peaks: decode_frontier_peaks(signer_peaks_data),
    continuation_cbor: builtin.un_b_data(continuation_cbor_data),
  }
}

fn phase_a_native_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  verified: VerifiedMidgardNativeTxCompact,
  witness_set: NativeTxWitnessSetCompact,
) -> Bool {
  let scripts_are_empty = witness_set.script_tx_wits_hash == bounded_collection_v1.from_items(
    6,
    [],
  )
  let is_late_continuation = control.continuation_cbor != #""
  and {
    verified.version == 1,
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    bytearray.length(control.resolution_schedule_hash) == 32,
    control.stage >= 0,
    control.stage <= 8,
    control.script_count >= -1,
    control.script_count <= max_tx_size_derived_collection_item_count,
    control.script_seen >= 0,
    if is_late_continuation {
      let native_control = native_scripts_control_from_witness(
        control.continuation_cbor,
      )
      and {
        control.resolution_schedule_hash == blake2b_256(
          control.continuation_cbor,
        ),
        control.stage > 0,
        control.script_count == 1,
        control.script_seen == 0,
        control.contains_non_native_script == 0,
        native_scripts_control_is_well_formed(native_control),
        control.continuation_cbor == encode_native_scripts_control_v1(
          native_control,
        ),
        native_control.compact_cbor == control.compact_cbor,
        native_control.witness_set_compact_cbor == control.witness_set_compact_cbor,
        native_control.field_preimage_lengths_cbor == control.field_preimage_lengths_cbor,
        native_control.context_cbor == control.context_cbor,
        native_control.execution_cursor < native_control.execution_count,
        native_control.signer_count == control.signer_count,
        native_control.signer_frontier_commitment == validation_merkle_v1.frontier_commitment(
          control.signer_count,
          control.signer_peaks,
        ),
      }
    } else if scripts_are_empty {
      control.script_count == 0 && control.script_seen == 0
    } else if control.script_count == -1 {
      control.script_seen == 0
    } else {
      control.script_count > 0 && control.script_seen <= control.script_count
    },
    control.contains_non_native_script == 0 || control.contains_non_native_script == 1,
    control.signer_count >= 0,
    control.signer_count <= max_tx_size_derived_collection_item_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.signer_count,
      control.signer_peaks,
    ),
    control.item_length >= 0,
    control.item_length <= max_aggregate_field_preimage_bytes,
    control.cursor >= 0,
    control.cursor <= control.item_length,
    control.stack_depth >= 0,
    control.stack_depth <= native_script_scan_v1.max_native_script_depth,
    control.node_count >= 0,
    control.node_count <= native_script_scan_v1.max_native_script_nodes,
    control.result >= -1,
    control.result <= 1,
    if control.stage == 0 {
      and {
        !is_late_continuation,
        control.item_length == 0,
        control.item_commitment == #"",
        control.cursor == 0,
        control.stack_root == #"",
        control.stack_depth == 0,
        control.node_count == 0,
        control.result == -1,
      }
    } else {
      and {
        control.script_count > 0,
        control.script_seen < control.script_count,
        control.item_length > 0,
        bytearray.length(control.item_commitment) == 32,
        if control.stack_root == #"" {
          control.stack_depth == 0
        } else {
          and {
            bytearray.length(control.stack_root) == 32,
            control.stack_depth > 0,
          }
        },
        if control.stage == 2 {
          control.result >= 0
        } else {
          control.result == -1
        },
      }
    },
    witness.work_witness_cbor == encode_phase_a_native_scripts_scan_witness(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      control.context_cbor,
      control.resolution_schedule_hash,
      control.stage,
      control.script_count,
      control.script_seen,
      control.contains_non_native_script,
      control.item_length,
      control.item_commitment,
      control.cursor,
      control.stack_root,
      control.stack_depth,
      control.node_count,
      control.result,
      control.signer_count,
      control.signer_peaks,
      control.continuation_cbor,
    ),
  }
}

fn phase_a_native_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  control: PhaseANativeScriptsControlV1,
) -> Bool {
  and {
    post.phase == validation_trace_v1.PhaseANativeScripts,
    post.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseANativeScripts,
      pre.program_counter + 1,
      encode_phase_a_native_scripts_scan_witness(
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
        control.context_cbor,
        control.resolution_schedule_hash,
        control.stage,
        control.script_count,
        control.script_seen,
        control.contains_non_native_script,
        control.item_length,
        control.item_commitment,
        control.cursor,
        control.stack_root,
        control.stack_depth,
        control.node_count,
        control.result,
        control.signer_count,
        control.signer_peaks,
        control.continuation_cbor,
      ),
    ),
  }
}

fn phase_a_native_to_preconditions_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  control: PhaseANativeScriptsControlV1,
) -> Bool {
  and {
    post.phase == validation_trace_v1.PhaseAScriptPreconditions,
    post.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseAScriptPreconditions,
      pre.program_counter + 1,
      encode_phase_a_script_preconditions_witness(
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
        control.context_cbor,
        control.resolution_schedule_hash,
        control.signer_count,
        validation_merkle_v1.frontier_commitment(
          control.signer_count,
          control.signer_peaks,
        ),
        control.contains_non_native_script == 1,
        0,
        0,
        #"",
      ),
    ),
  }
}

fn reset_phase_a_native_control(
  control: PhaseANativeScriptsControlV1,
  script_count: Int,
  script_seen: Int,
  contains_non_native_script: Int,
) -> PhaseANativeScriptsControlV1 {
  PhaseANativeScriptsControlV1 {
    ..control,
    stage: 0,
    script_count,
    script_seen,
    contains_non_native_script,
    item_length: 0,
    item_commitment: #"",
    cursor: 0,
    stack_root: #"",
    stack_depth: 0,
    node_count: 0,
    result: -1,
  }
}

fn phase_a_native_complete_script_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  control: PhaseANativeScriptsControlV1,
  script_count: Int,
  script_seen: Int,
  contains_non_native_script: Int,
) -> Bool {
  if control.continuation_cbor != #"" {
    let native_control = native_scripts_control_from_witness(
      control.continuation_cbor,
    )
    let next_native_control = NativeScriptsControlV1 {
      ..native_control,
      execution_cursor: native_control.execution_cursor + 1,
    }
    and {
      script_count == 1,
      script_seen == 1,
      contains_non_native_script == 0,
      post.phase == validation_trace_v1.NativeScripts,
      post.work_root == validation_trace_v1.hash_work_witness(
        validation_trace_v1.NativeScripts,
        pre.program_counter + 1,
        encode_native_scripts_control_v1(next_native_control),
      ),
    }
  } else {
    let next_control = reset_phase_a_native_control(
      control,
      script_count,
      script_seen,
      contains_non_native_script,
    )
    if script_seen == script_count {
      phase_a_native_to_preconditions_is_exact(pre, post, next_control)
    } else {
      phase_a_native_successor_is_exact(pre, post, next_control)
    }
  }
}

fn verify_phase_a_native_advance_scan(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  and {
    phase_a_native_control_is_bound(
      pre,
      witness,
      control,
      verified,
      witness_set,
    ),
    control.stage == 0,
    control.script_count == 0,
    phase_a_native_to_preconditions_is_exact(
      pre,
      witness.claimed_successor,
      control,
    ),
  }
}

fn verify_phase_a_native_item_scan(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  collection_proof: bounded_collection_v1.ItemProofV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let active_count = if control.script_count == -1 {
    collection_proof.item_count
  } else {
    control.script_count
  }
  let header = native_script_scan_v1.versioned_script_header_v1(
    chunk_proof.chunk,
    collection_proof.item_length,
  )
  and {
    phase_a_native_control_is_bound(
      pre,
      witness,
      control,
      verified,
      witness_set,
    ),
    control.stage == 0,
    control.script_count != 0,
    control.script_count == -1 || control.script_count == collection_proof.item_count,
    active_count > 0,
    active_count <= max_tx_size_derived_collection_item_count,
    collection_proof.field_index == 6,
    collection_proof.item_count == active_count,
    collection_proof.item_index == control.script_seen,
    collection_proof.item_length > 0,
    collection_proof.item_length <= max_aggregate_field_preimage_bytes,
    chunk_proof.field_index == 6,
    chunk_proof.item_index == collection_proof.item_index,
    chunk_proof.total_length == collection_proof.item_length,
    chunk_proof.chunk_index == 0,
    bounded_collection_v1.verify_item(
      witness_set.script_tx_wits_hash,
      collection_proof,
    ),
    bounded_item_v1.verify_chunk(collection_proof.item_commitment, chunk_proof),
    when header is {
      None ->
        rejected_successor_is_exact(
          pre,
          witness.claimed_successor,
          reject_invalid_field_type,
        )
      Some(item_header) ->
        if item_header.language_tag == 0 {
          phase_a_native_successor_is_exact(
            pre,
            witness.claimed_successor,
            PhaseANativeScriptsControlV1 {
              ..control,
              stage: 1,
              script_count: active_count,
              item_length: collection_proof.item_length,
              item_commitment: collection_proof.item_commitment,
              cursor: item_header.payload_offset,
            },
          )
        } else {
          phase_a_native_complete_script_is_exact(
            pre,
            witness.claimed_successor,
            control,
            active_count,
            control.script_seen + 1,
            1,
          )
        }
    },
  }
}

fn phase_a_native_chunk_window(
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Option<NativeChunkWindowV1> {
  let expected_chunk_index = control.cursor / bounded_item_v1.chunk_bytes
  let count = bounded_item_v1.chunk_count(control.item_length)
  let current_matches = and {
    chunk_proof.field_index == 6,
    chunk_proof.item_index == control.script_seen,
    chunk_proof.total_length == control.item_length,
    chunk_proof.chunk_index == expected_chunk_index,
    bounded_item_v1.verify_chunk(control.item_commitment, chunk_proof),
  }
  if !current_matches {
    None
  } else if expected_chunk_index + 1 < count {
    when next_chunk_proof is {
      None -> None
      Some(next) ->
        if and {
          next.field_index == 6,
          next.item_index == control.script_seen,
          next.total_length == control.item_length,
          next.chunk_index == expected_chunk_index + 1,
          bounded_item_v1.verify_chunk(control.item_commitment, next),
        } {
          Some(
            NativeChunkWindowV1 {
              bytes: bytearray.concat(chunk_proof.chunk, next.chunk),
              offset: control.cursor - expected_chunk_index * bounded_item_v1.chunk_bytes,
            },
          )
        } else {
          None
        }
    }
  } else if next_chunk_proof == None {
    Some(
      NativeChunkWindowV1 {
        bytes: chunk_proof.chunk,
        offset: control.cursor - expected_chunk_index * bounded_item_v1.chunk_bytes,
      },
    )
  } else {
    None
  }
}

fn phase_a_native_signature_result(
  key_hash: ByteArray,
  control: PhaseANativeScriptsControlV1,
  proof: SignerSetProofV1,
) -> Option<Bool> {
  let frontier_commitment = validation_merkle_v1.frontier_commitment(
    control.signer_count,
    control.signer_peaks,
  )
  when proof is {
    SignerMembershipProof { peaks, signer_index, siblings } ->
      if signer_membership_is_valid(
        key_hash,
        control.signer_count,
        frontier_commitment,
        peaks,
        signer_index,
        siblings,
      ) {
        Some(True)
      } else {
        None
      }
    _ ->
      if signer_non_membership_is_valid(
        key_hash,
        control.signer_count,
        frontier_commitment,
        proof,
      ) {
        Some(False)
      } else {
        None
      }
  }
}

fn verify_phase_a_native_token_scan(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  signer_proof: SignerSetProofV1,
  token_mode: Int,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let window = phase_a_native_chunk_window(
    control,
    chunk_proof,
    next_chunk_proof,
  )
  and {
    phase_a_native_control_is_bound(
      pre,
      witness,
      control,
      verified,
      witness_set,
    ),
    control.stage == 1,
    when window is {
      None -> False
      Some(authenticated) ->
        when
          native_script_scan_v1.token_at_v1(
            authenticated.bytes,
            authenticated.offset,
            control.cursor,
          )
        is {
          None ->
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_invalid_field_type,
            )
          Some(token) -> {
            let next_node_count = control.node_count + 1
            if next_node_count > native_script_scan_v1.max_native_script_nodes {
              rejected_successor_is_exact(
                pre,
                witness.claimed_successor,
                reject_native_script_node_count,
              )
            } else if and {
              token.kind >= native_script_scan_v1.all_node,
              token.kind <= native_script_scan_v1.at_least_node,
              token.child_count > 0,
            } {
              when
                native_script_scan_v1.frame_for_token_v1(
                  token,
                  control.stack_root,
                )
              is {
                None -> False
                Some(frame) -> {
                  let next_depth = control.stack_depth + 1
                  if next_depth > native_script_scan_v1.max_native_script_depth {
                    rejected_successor_is_exact(
                      pre,
                      witness.claimed_successor,
                      reject_native_script_depth,
                    )
                  } else {
                    and {
                      token_mode != 1,
                      signer_proof == NoSignerSetProof,
                      token.next_offset <= control.item_length,
                      phase_a_native_successor_is_exact(
                        pre,
                        witness.claimed_successor,
                        PhaseANativeScriptsControlV1 {
                          ..control,
                          cursor: token.next_offset,
                          stack_root: native_script_scan_v1.hash_frame_v1(frame),
                          stack_depth: next_depth,
                          node_count: next_node_count,
                        },
                      ),
                    }
                  }
                }
              }
            } else {
              let result = if token.kind == native_script_scan_v1.signature_node {
                if token_mode != 0 {
                  phase_a_native_signature_result(
                    token.key_hash,
                    control,
                    signer_proof,
                  )
                } else {
                  None
                }
              } else if token.kind == native_script_scan_v1.after_node {
                if and {
                  token_mode != 1,
                  signer_proof == NoSignerSetProof,
                } {
                  Some(and {
                      verified.tx_compact.body.validity_interval_start >= 0,
                      verified.tx_compact.body.validity_interval_start >= token.slot,
                    })
                } else {
                  None
                }
              } else if token.kind == native_script_scan_v1.before_node {
                if and {
                  token_mode != 1,
                  signer_proof == NoSignerSetProof,
                } {
                  Some(and {
                      verified.tx_compact.body.validity_interval_end >= 0,
                      verified.tx_compact.body.validity_interval_end <= token.slot,
                    })
                } else {
                  None
                }
              } else if and {
                token_mode != 1,
                signer_proof == NoSignerSetProof,
              } {
                native_script_scan_v1.empty_container_result_v1(token)
              } else {
                None
              }
              when result is {
                None -> False
                Some(valid) -> and {
                    token.next_offset <= control.item_length,
                    phase_a_native_successor_is_exact(
                      pre,
                      witness.claimed_successor,
                      PhaseANativeScriptsControlV1 {
                        ..control,
                        stage: 2,
                        cursor: token.next_offset,
                        node_count: next_node_count,
                        result: if valid {
                          1
                        } else {
                          0
                        },
                      },
                    ),
                  }
              }
            }
          }
        }
    },
  }
}

fn verify_phase_a_native_frame_scan(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  frame: native_script_scan_v1.NativeScriptFrameV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  and {
    phase_a_native_control_is_bound(
      pre,
      witness,
      control,
      verified,
      witness_set,
    ),
    control.stage == 2,
    bytearray.length(control.stack_root) == 32,
    control.stack_depth > 0,
    native_script_scan_v1.hash_frame_v1(frame) == control.stack_root,
    when native_script_scan_v1.apply_child_v1(frame, control.result == 1) is {
      None -> False
      Some(native_script_scan_v1.NativeFramePending { frame: next_frame }) ->
        phase_a_native_successor_is_exact(
          pre,
          witness.claimed_successor,
          PhaseANativeScriptsControlV1 {
            ..control,
            stage: 1,
            stack_root: native_script_scan_v1.hash_frame_v1(next_frame),
            result: -1,
          },
        )
      Some(native_script_scan_v1.NativeFrameComplete { tail, valid }) ->
        phase_a_native_successor_is_exact(
          pre,
          witness.claimed_successor,
          PhaseANativeScriptsControlV1 {
            ..control,
            stack_root: tail,
            stack_depth: control.stack_depth - 1,
            result: if valid {
              1
            } else {
              0
            },
          },
        )
    },
  }
}

fn verify_phase_a_native_finalize_scan(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
) -> Bool {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  and {
    phase_a_native_control_is_bound(
      pre,
      witness,
      control,
      verified,
      witness_set,
    ),
    control.stage == 2,
    control.stack_root == #"",
    control.stack_depth == 0,
    if control.cursor != control.item_length {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_invalid_field_type,
      )
    } else if control.result == 0 {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_native_script_invalid,
      )
    } else {
      phase_a_native_complete_script_is_exact(
        pre,
        witness.claimed_successor,
        control,
        control.script_count,
        control.script_seen + 1,
        control.contains_non_native_script,
      )
    },
  }
}

fn verify_phase_a_native_scripts_scan(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let control = phase_a_native_control_from_witness(witness.work_witness_cbor)
  when auxiliary is {
    NoAuxiliaryWitness ->
      if control.stage == 2 {
        verify_phase_a_native_finalize_scan(pre, witness, control)
      } else {
        verify_phase_a_native_advance_scan(pre, witness, control)
      }
    TransactionFieldChunkWitness { collection_proof, chunk_proof } ->
      verify_phase_a_native_item_scan(
        pre,
        witness,
        control,
        collection_proof,
        chunk_proof,
      )
    NativeScriptTokenWitness { chunk_proof, next_chunk_proof, signer_proof } ->
      if control.stage == 1 {
        and {
          signer_proof == NoSignerSetProof,
          verify_phase_a_native_token_head_scan_v1(
            pre,
            witness,
            control,
            chunk_proof,
            next_chunk_proof,
          ),
        }
      } else if control.stage == 3 {
        verify_phase_a_native_signature_payload_scan_v1(
          pre,
          witness,
          control,
          chunk_proof,
          next_chunk_proof,
          signer_proof,
        )
      } else if or {
        control.stage == 4,
        control.stage == 5,
      } {
        and {
          signer_proof == NoSignerSetProof,
          or {
            verify_phase_a_native_all_or_any_container_frame_payload_scan_v1(
              pre,
              witness,
              control,
              chunk_proof,
              next_chunk_proof,
            ),
            verify_phase_a_native_all_or_any_empty_container_payload_scan_v1(
              pre,
              witness,
              control,
              chunk_proof,
              next_chunk_proof,
            ),
          },
        }
      } else if control.stage == 6 {
        and {
          signer_proof == NoSignerSetProof,
          or {
            verify_phase_a_native_at_least_container_frame_payload_scan_v1(
              pre,
              witness,
              control,
              chunk_proof,
              next_chunk_proof,
            ),
            verify_phase_a_native_at_least_empty_container_payload_scan_v1(
              pre,
              witness,
              control,
              chunk_proof,
              next_chunk_proof,
            ),
          },
        }
      } else if or {
        control.stage == 7,
        control.stage == 8,
      } {
        and {
          signer_proof == NoSignerSetProof,
          verify_phase_a_native_timelock_payload_scan_v1(
            pre,
            witness,
            control,
            chunk_proof,
            next_chunk_proof,
          ),
        }
      } else {
        False
      }
    NativeScriptFrameWitness { frame } ->
      verify_phase_a_native_frame_scan(pre, witness, control, frame)
    _ -> False
  }
}

pub fn verify_phase_a_native_advance_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = phase_a_native_control_from_witness(witness.work_witness_cbor)
  if control.stage == 2 {
    verify_phase_a_native_finalize_scan(pre, witness, control)
  } else {
    verify_phase_a_native_advance_scan(pre, witness, control)
  }
}

pub fn verify_phase_a_native_item_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  collection_proof: bounded_collection_v1.ItemProofV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
) -> Bool {
  verify_phase_a_native_item_scan(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    collection_proof,
    chunk_proof,
  )
}

pub fn verify_phase_a_native_token_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  signer_proof: SignerSetProofV1,
) -> Bool {
  verify_phase_a_native_token_scan(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
    signer_proof,
    2,
  )
}

type AuthenticatedNativeScriptTokenHeadV1 {
  verified: VerifiedMidgardNativeTxCompact,
  window: NativeChunkWindowV1,
  head: Option<native_script_scan_v1.NativeScriptTokenHeadV1>,
}

type AuthenticatedNativeScriptWindowV1 {
  verified: VerifiedMidgardNativeTxCompact,
  window: NativeChunkWindowV1,
}

fn authenticated_phase_a_native_window_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Option<AuthenticatedNativeScriptWindowV1> {
  let Pair(verified, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  if phase_a_native_control_is_bound(
    pre,
    witness,
    control,
    verified,
    witness_set,
  ) {
    when
      phase_a_native_chunk_window(control, chunk_proof, next_chunk_proof)
    is {
      None -> None
      Some(authenticated) ->
        Some(
          AuthenticatedNativeScriptWindowV1 { verified, window: authenticated },
        )
    }
  } else {
    None
  }
}

fn authenticated_phase_a_native_token_head_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Option<AuthenticatedNativeScriptTokenHeadV1> {
  when
    authenticated_phase_a_native_window_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> None
    Some(authenticated) ->
      if control.stage != 1 {
        None
      } else {
        Some(
          AuthenticatedNativeScriptTokenHeadV1 {
            verified: authenticated.verified,
            window: authenticated.window,
            head: native_script_scan_v1.token_head_at_v1(
              authenticated.window.bytes,
              authenticated.window.offset,
            ),
          },
        )
      }
  }
}

fn verify_phase_a_native_token_head_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  when
    authenticated_phase_a_native_token_head_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(authenticated) ->
      when authenticated.head is {
        None ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          )
        Some(head) ->
          if !native_script_scan_v1.token_head_is_well_formed_v1(head) {
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_invalid_field_type,
            )
          } else {
            let next_node_count = control.node_count + 1
            let next_offset = control.cursor + head.next_offset - authenticated.window.offset
            if next_node_count > native_script_scan_v1.max_native_script_nodes {
              rejected_successor_is_exact(
                pre,
                witness.claimed_successor,
                reject_native_script_node_count,
              )
            } else {
              and {
                next_offset <= control.item_length,
                phase_a_native_successor_is_exact(
                  pre,
                  witness.claimed_successor,
                  PhaseANativeScriptsControlV1 {
                    ..control,
                    stage: head.tag + 3,
                    cursor: next_offset,
                    node_count: next_node_count,
                  },
                ),
              }
            }
          }
      }
  }
}

fn phase_a_native_container_frame_payload_successor_is_exact_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  token: native_script_scan_v1.NativeScriptTokenV1,
) -> Bool {
  if token.child_count <= 0 {
    False
  } else {
    when
      native_script_scan_v1.frame_for_token_v1(token, control.stack_root)
    is {
      None -> False
      Some(frame) -> {
        let next_depth = control.stack_depth + 1
        if next_depth > native_script_scan_v1.max_native_script_depth {
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_native_script_depth,
          )
        } else {
          and {
            token.next_offset <= control.item_length,
            phase_a_native_successor_is_exact(
              pre,
              witness.claimed_successor,
              PhaseANativeScriptsControlV1 {
                ..control,
                stage: 1,
                cursor: token.next_offset,
                stack_root: native_script_scan_v1.hash_frame_v1(frame),
                stack_depth: next_depth,
              },
            ),
          }
        }
      }
    }
  }
}

fn phase_a_native_empty_container_payload_successor_is_exact_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  token: native_script_scan_v1.NativeScriptTokenV1,
) -> Bool {
  if token.child_count != 0 {
    False
  } else {
    when native_script_scan_v1.empty_container_result_v1(token) is {
      None -> False
      Some(valid) -> and {
          token.next_offset <= control.item_length,
          phase_a_native_successor_is_exact(
            pre,
            witness.claimed_successor,
            PhaseANativeScriptsControlV1 {
              ..control,
              stage: 2,
              cursor: token.next_offset,
              result: if valid {
                1
              } else {
                0
              },
            },
          ),
        }
    }
  }
}

fn authenticated_phase_a_native_all_or_any_payload_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Option<Option<native_script_scan_v1.NativeScriptTokenV1>> {
  when
    authenticated_phase_a_native_window_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> None
    Some(authenticated) ->
      if and {
        control.stage != 4,
        control.stage != 5,
      } {
        None
      } else {
        Some(
          native_script_scan_v1.all_or_any_payload_at_v1(
            authenticated.window.bytes,
            authenticated.window.offset,
            control.cursor,
            control.stage - 3,
          ),
        )
      }
  }
}

fn authenticated_phase_a_native_at_least_payload_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Option<Option<native_script_scan_v1.NativeScriptTokenV1>> {
  when
    authenticated_phase_a_native_window_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> None
    Some(authenticated) ->
      if control.stage != 6 {
        None
      } else {
        Some(
          native_script_scan_v1.at_least_payload_at_v1(
            authenticated.window.bytes,
            authenticated.window.offset,
            control.cursor,
          ),
        )
      }
  }
}

fn verify_phase_a_native_all_or_any_container_frame_payload_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  when
    authenticated_phase_a_native_all_or_any_payload_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(None) ->
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_invalid_field_type,
      )
    Some(Some(token)) ->
      phase_a_native_container_frame_payload_successor_is_exact_v1(
        pre,
        witness,
        control,
        token,
      )
  }
}

fn verify_phase_a_native_all_or_any_empty_container_payload_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  when
    authenticated_phase_a_native_all_or_any_payload_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(None) ->
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_invalid_field_type,
      )
    Some(Some(token)) ->
      phase_a_native_empty_container_payload_successor_is_exact_v1(
        pre,
        witness,
        control,
        token,
      )
  }
}

fn verify_phase_a_native_at_least_container_frame_payload_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  when
    authenticated_phase_a_native_at_least_payload_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(None) ->
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_invalid_field_type,
      )
    Some(Some(token)) ->
      phase_a_native_container_frame_payload_successor_is_exact_v1(
        pre,
        witness,
        control,
        token,
      )
  }
}

fn verify_phase_a_native_at_least_empty_container_payload_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  when
    authenticated_phase_a_native_at_least_payload_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(None) ->
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_invalid_field_type,
      )
    Some(Some(token)) ->
      phase_a_native_empty_container_payload_successor_is_exact_v1(
        pre,
        witness,
        control,
        token,
      )
  }
}

fn phase_a_native_container_frame_successor_is_exact_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  token: native_script_scan_v1.NativeScriptTokenV1,
) -> Bool {
  let next_node_count = control.node_count + 1
  if next_node_count > native_script_scan_v1.max_native_script_nodes {
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_native_script_node_count,
    )
  } else if token.child_count <= 0 {
    False
  } else {
    when
      native_script_scan_v1.frame_for_token_v1(token, control.stack_root)
    is {
      None -> False
      Some(frame) -> {
        let next_depth = control.stack_depth + 1
        if next_depth > native_script_scan_v1.max_native_script_depth {
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_native_script_depth,
          )
        } else {
          and {
            token.next_offset <= control.item_length,
            phase_a_native_successor_is_exact(
              pre,
              witness.claimed_successor,
              PhaseANativeScriptsControlV1 {
                ..control,
                cursor: token.next_offset,
                stack_root: native_script_scan_v1.hash_frame_v1(frame),
                stack_depth: next_depth,
                node_count: next_node_count,
              },
            ),
          }
        }
      }
    }
  }
}

fn phase_a_native_empty_container_successor_is_exact_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  token: native_script_scan_v1.NativeScriptTokenV1,
) -> Bool {
  let next_node_count = control.node_count + 1
  if next_node_count > native_script_scan_v1.max_native_script_nodes {
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_native_script_node_count,
    )
  } else if token.child_count != 0 {
    False
  } else {
    when native_script_scan_v1.empty_container_result_v1(token) is {
      None -> False
      Some(valid) -> and {
          token.next_offset <= control.item_length,
          phase_a_native_successor_is_exact(
            pre,
            witness.claimed_successor,
            PhaseANativeScriptsControlV1 {
              ..control,
              stage: 2,
              cursor: token.next_offset,
              node_count: next_node_count,
              result: if valid {
                1
              } else {
                0
              },
            },
          ),
        }
    }
  }
}

fn verify_phase_a_native_container_frame_token_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  when
    authenticated_phase_a_native_token_head_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(authenticated) ->
      when authenticated.head is {
        None ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          )
        Some(head) ->
          if and {
            head.tag >= native_script_scan_v1.all_node,
            head.tag <= native_script_scan_v1.at_least_node,
          } {
            when
              native_script_scan_v1.container_token_from_head_v1(
                authenticated.window.bytes,
                authenticated.window.offset,
                control.cursor,
                head,
              )
            is {
              None ->
                rejected_successor_is_exact(
                  pre,
                  witness.claimed_successor,
                  reject_invalid_field_type,
                )
              Some(token) ->
                phase_a_native_container_frame_successor_is_exact_v1(
                  pre,
                  witness,
                  control,
                  token,
                )
            }
          } else if or {
            head.tag == native_script_scan_v1.signature_node,
            head.tag == native_script_scan_v1.after_node,
            head.tag == native_script_scan_v1.before_node,
          } {
            False
          } else {
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_invalid_field_type,
            )
          }
      }
  }
}

fn verify_phase_a_native_empty_container_token_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  when
    authenticated_phase_a_native_token_head_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(authenticated) ->
      when authenticated.head is {
        None ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          )
        Some(head) ->
          if and {
            head.tag >= native_script_scan_v1.all_node,
            head.tag <= native_script_scan_v1.at_least_node,
          } {
            when
              native_script_scan_v1.container_token_from_head_v1(
                authenticated.window.bytes,
                authenticated.window.offset,
                control.cursor,
                head,
              )
            is {
              None ->
                rejected_successor_is_exact(
                  pre,
                  witness.claimed_successor,
                  reject_invalid_field_type,
                )
              Some(token) ->
                phase_a_native_empty_container_successor_is_exact_v1(
                  pre,
                  witness,
                  control,
                  token,
                )
            }
          } else if or {
            head.tag == native_script_scan_v1.signature_node,
            head.tag == native_script_scan_v1.after_node,
            head.tag == native_script_scan_v1.before_node,
          } {
            False
          } else {
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_invalid_field_type,
            )
          }
      }
  }
}

fn verify_phase_a_native_timelock_token_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  when
    authenticated_phase_a_native_token_head_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(authenticated) ->
      when authenticated.head is {
        None ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          )
        Some(head) ->
          if or {
            head.tag == native_script_scan_v1.after_node,
            head.tag == native_script_scan_v1.before_node,
          } {
            when
              native_script_scan_v1.timelock_token_from_head_v1(
                authenticated.window.bytes,
                authenticated.window.offset,
                control.cursor,
                head,
              )
            is {
              None ->
                rejected_successor_is_exact(
                  pre,
                  witness.claimed_successor,
                  reject_invalid_field_type,
                )
              Some(token) -> {
                let next_node_count = control.node_count + 1
                if next_node_count > native_script_scan_v1.max_native_script_nodes {
                  rejected_successor_is_exact(
                    pre,
                    witness.claimed_successor,
                    reject_native_script_node_count,
                  )
                } else {
                  let valid = if token.kind == native_script_scan_v1.after_node {
                    and {
                      authenticated.verified.tx_compact.body.validity_interval_start >= 0,
                      authenticated.verified.tx_compact.body.validity_interval_start >= token.slot,
                    }
                  } else {
                    and {
                      authenticated.verified.tx_compact.body.validity_interval_end >= 0,
                      authenticated.verified.tx_compact.body.validity_interval_end <= token.slot,
                    }
                  }
                  and {
                    token.next_offset <= control.item_length,
                    phase_a_native_successor_is_exact(
                      pre,
                      witness.claimed_successor,
                      PhaseANativeScriptsControlV1 {
                        ..control,
                        stage: 2,
                        cursor: token.next_offset,
                        node_count: next_node_count,
                        result: if valid {
                          1
                        } else {
                          0
                        },
                      },
                    ),
                  }
                }
              }
            }
          } else if and {
            head.tag >= native_script_scan_v1.signature_node,
            head.tag <= native_script_scan_v1.at_least_node,
          } {
            False
          } else {
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_invalid_field_type,
            )
          }
      }
  }
}

fn verify_phase_a_native_signature_token_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  signer_proof: SignerSetProofV1,
) -> Bool {
  when
    authenticated_phase_a_native_token_head_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(authenticated) ->
      when authenticated.head is {
        None ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          )
        Some(head) ->
          if head.tag == native_script_scan_v1.signature_node {
            when
              native_script_scan_v1.signature_token_from_head_v1(
                authenticated.window.bytes,
                authenticated.window.offset,
                control.cursor,
                head,
              )
            is {
              None ->
                rejected_successor_is_exact(
                  pre,
                  witness.claimed_successor,
                  reject_invalid_field_type,
                )
              Some(token) -> {
                let next_node_count = control.node_count + 1
                if next_node_count > native_script_scan_v1.max_native_script_nodes {
                  rejected_successor_is_exact(
                    pre,
                    witness.claimed_successor,
                    reject_native_script_node_count,
                  )
                } else {
                  when
                    phase_a_native_signature_result(
                      token.key_hash,
                      control,
                      signer_proof,
                    )
                  is {
                    None -> False
                    Some(valid) -> and {
                        token.next_offset <= control.item_length,
                        phase_a_native_successor_is_exact(
                          pre,
                          witness.claimed_successor,
                          PhaseANativeScriptsControlV1 {
                            ..control,
                            stage: 2,
                            cursor: token.next_offset,
                            node_count: next_node_count,
                            result: if valid {
                              1
                            } else {
                              0
                            },
                          },
                        ),
                      }
                  }
                }
              }
            }
          } else if and {
            head.tag >= native_script_scan_v1.all_node,
            head.tag <= native_script_scan_v1.before_node,
          } {
            False
          } else {
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_invalid_field_type,
            )
          }
      }
  }
}

fn verify_phase_a_native_timelock_payload_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  when
    authenticated_phase_a_native_window_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(authenticated) ->
      if and {
        control.stage != 7,
        control.stage != 8,
      } {
        False
      } else {
        when
          native_script_scan_v1.timelock_payload_at_v1(
            authenticated.window.bytes,
            authenticated.window.offset,
            control.cursor,
            control.stage - 3,
          )
        is {
          None ->
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_invalid_field_type,
            )
          Some(token) -> {
            let valid = if token.kind == native_script_scan_v1.after_node {
              and {
                authenticated.verified.tx_compact.body.validity_interval_start >= 0,
                authenticated.verified.tx_compact.body.validity_interval_start >= token.slot,
              }
            } else {
              and {
                authenticated.verified.tx_compact.body.validity_interval_end >= 0,
                authenticated.verified.tx_compact.body.validity_interval_end <= token.slot,
              }
            }
            and {
              token.next_offset <= control.item_length,
              phase_a_native_successor_is_exact(
                pre,
                witness.claimed_successor,
                PhaseANativeScriptsControlV1 {
                  ..control,
                  stage: 2,
                  cursor: token.next_offset,
                  result: if valid {
                    1
                  } else {
                    0
                  },
                },
              ),
            }
          }
        }
      }
  }
}

fn verify_phase_a_native_signature_payload_with_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  signer_result: fn(ByteArray, PhaseANativeScriptsControlV1) -> Option<Bool>,
) -> Bool {
  when
    authenticated_phase_a_native_window_v1(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  is {
    None -> False
    Some(authenticated) ->
      if control.stage != 3 {
        False
      } else {
        when
          native_script_scan_v1.signature_payload_at_v1(
            authenticated.window.bytes,
            authenticated.window.offset,
            control.cursor,
          )
        is {
          None ->
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_invalid_field_type,
            )
          Some(token) ->
            when signer_result(token.key_hash, control) is {
              None -> False
              Some(valid) -> and {
                  token.next_offset <= control.item_length,
                  phase_a_native_successor_is_exact(
                    pre,
                    witness.claimed_successor,
                    PhaseANativeScriptsControlV1 {
                      ..control,
                      stage: 2,
                      cursor: token.next_offset,
                      result: if valid {
                        1
                      } else {
                        0
                      },
                    },
                  ),
                }
            }
        }
      }
  }
}

fn verify_phase_a_native_signature_payload_scan_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseANativeScriptsControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  signer_proof: SignerSetProofV1,
) -> Bool {
  verify_phase_a_native_signature_payload_with_v1(
    pre,
    witness,
    control,
    chunk_proof,
    next_chunk_proof,
    fn(key_hash, bound_control) {
      phase_a_native_signature_result(key_hash, bound_control, signer_proof)
    },
  )
}

pub fn verify_phase_a_native_token_head_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  verify_phase_a_native_token_head_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
  )
}

pub fn verify_phase_a_native_all_or_any_container_frame_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  verify_phase_a_native_all_or_any_container_frame_payload_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
  )
}

pub fn verify_phase_a_native_all_or_any_empty_container_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  verify_phase_a_native_all_or_any_empty_container_payload_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
  )
}

pub fn verify_phase_a_native_at_least_container_frame_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  verify_phase_a_native_at_least_container_frame_payload_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
  )
}

pub fn verify_phase_a_native_at_least_empty_container_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  verify_phase_a_native_at_least_empty_container_payload_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
  )
}

pub fn verify_phase_a_native_timelock_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  verify_phase_a_native_timelock_payload_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
  )
}

pub fn verify_phase_a_native_signature_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  signer_proof: SignerSetProofV1,
) -> Bool {
  verify_phase_a_native_signature_payload_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
    signer_proof,
  )
}

pub fn verify_phase_a_native_signature_membership_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  peaks: List<validation_merkle_v1.FrontierPeak>,
  signer_index: Int,
  siblings: List<ByteArray>,
) -> Bool {
  verify_phase_a_native_signature_payload_with_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
    fn(key_hash, control) {
      let frontier_commitment = validation_merkle_v1.frontier_commitment(
        control.signer_count,
        control.signer_peaks,
      )
      if signer_membership_is_valid(
        key_hash,
        control.signer_count,
        frontier_commitment,
        peaks,
        signer_index,
        siblings,
      ) {
        Some(True)
      } else {
        None
      }
    },
  )
}

pub fn verify_phase_a_native_signature_empty_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  peaks: List<validation_merkle_v1.FrontierPeak>,
) -> Bool {
  verify_phase_a_native_signature_payload_with_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
    fn(_key_hash, control) {
      let frontier_commitment = validation_merkle_v1.frontier_commitment(
        control.signer_count,
        control.signer_peaks,
      )
      if and {
        control.signer_count == 0,
        signer_frontier_matches(
          control.signer_count,
          frontier_commitment,
          peaks,
        ),
      } {
        Some(False)
      } else {
        None
      }
    },
  )
}

pub fn verify_phase_a_native_signature_below_first_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  peaks: List<validation_merkle_v1.FrontierPeak>,
  first_signer_hash: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  verify_phase_a_native_signature_payload_with_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
    fn(key_hash, control) {
      let frontier_commitment = validation_merkle_v1.frontier_commitment(
        control.signer_count,
        control.signer_peaks,
      )
      if and {
        bytearray.compare(key_hash, first_signer_hash) == Less,
        signer_membership_is_valid(
          first_signer_hash,
          control.signer_count,
          frontier_commitment,
          peaks,
          0,
          siblings,
        ),
      } {
        Some(False)
      } else {
        None
      }
    },
  )
}

pub fn verify_phase_a_native_signature_above_last_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  peaks: List<validation_merkle_v1.FrontierPeak>,
  last_signer_hash: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  verify_phase_a_native_signature_payload_with_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
    fn(key_hash, control) {
      let frontier_commitment = validation_merkle_v1.frontier_commitment(
        control.signer_count,
        control.signer_peaks,
      )
      if and {
        control.signer_count > 0,
        bytearray.compare(last_signer_hash, key_hash) == Less,
        signer_membership_is_valid(
          last_signer_hash,
          control.signer_count,
          frontier_commitment,
          peaks,
          control.signer_count - 1,
          siblings,
        ),
      } {
        Some(False)
      } else {
        None
      }
    },
  )
}

pub fn verify_phase_a_native_signature_between_payload_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  peaks: List<validation_merkle_v1.FrontierPeak>,
  lower_index: Int,
  lower_signer_hash: ByteArray,
  lower_siblings: List<ByteArray>,
  upper_signer_hash: ByteArray,
  upper_siblings: List<ByteArray>,
) -> Bool {
  verify_phase_a_native_signature_payload_with_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
    fn(key_hash, control) {
      let frontier_commitment = validation_merkle_v1.frontier_commitment(
        control.signer_count,
        control.signer_peaks,
      )
      if and {
        lower_index >= 0,
        lower_index + 1 < control.signer_count,
        bytearray.compare(lower_signer_hash, key_hash) == Less,
        bytearray.compare(key_hash, upper_signer_hash) == Less,
        signer_membership_is_valid(
          lower_signer_hash,
          control.signer_count,
          frontier_commitment,
          peaks,
          lower_index,
          lower_siblings,
        ),
        signer_membership_is_valid(
          upper_signer_hash,
          control.signer_count,
          frontier_commitment,
          peaks,
          lower_index + 1,
          upper_siblings,
        ),
      } {
        Some(False)
      } else {
        None
      }
    },
  )
}

pub fn verify_phase_a_native_container_frame_token_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  verify_phase_a_native_container_frame_token_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
  )
}

pub fn verify_phase_a_native_empty_container_token_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  verify_phase_a_native_empty_container_token_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
  )
}

pub fn verify_phase_a_native_timelock_token_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  verify_phase_a_native_timelock_token_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
  )
}

pub fn verify_phase_a_native_signature_token_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
  signer_proof: SignerSetProofV1,
) -> Bool {
  verify_phase_a_native_signature_token_scan_v1(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    chunk_proof,
    next_chunk_proof,
    signer_proof,
  )
}

pub fn verify_phase_a_native_frame_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  frame: native_script_scan_v1.NativeScriptFrameV1,
) -> Bool {
  verify_phase_a_native_frame_scan(
    pre,
    witness,
    phase_a_native_control_from_witness(witness.work_witness_cbor),
    frame,
  )
}

fn phase_a_script_preconditions_rejection(
  observer_count: Int,
  has_redeemers: Bool,
  contains_non_native_script: Bool,
  script_integrity_hash: ByteArray,
  network_id: Int,
) -> Option<ByteArray> {
  let requires_plutus_evaluation = or {
    contains_non_native_script,
    has_redeemers,
    script_integrity_hash != #"0000000000000000000000000000000000000000000000000000000000000000",
  }
  if !requires_plutus_evaluation {
    None
  } else if script_integrity_hash == #"0000000000000000000000000000000000000000000000000000000000000000" {
    Some(reject_invalid_field_type)
  } else if and {
    observer_count > 0,
    network_id == 255,
  } {
    Some(reject_invalid_field_type)
  } else {
    None
  }
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

fn phase_a_script_preconditions_control_from_witness(
  witness_cbor: ByteArray,
) -> PhaseAScriptPreconditionsControlV1 {
  expect Some(data) = cbor.deserialise(witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data,
        resolution_schedule_hash_data, signer_count_data,
        signer_frontier_commitment_data, contains_non_native_script_data,
        previous_observer_data, observer_count_data, observer_seen_data,
      ]
    = builtin.un_list_data(data)
  PhaseAScriptPreconditionsControlV1 {
    compact_cbor: builtin.un_b_data(compact_cbor_data),
    witness_set_compact_cbor: builtin.un_b_data(witness_set_compact_cbor_data),
    field_preimage_lengths_cbor: builtin.un_b_data(
      field_preimage_lengths_cbor_data,
    ),
    context_cbor: builtin.un_b_data(context_cbor_data),
    resolution_schedule_hash: builtin.un_b_data(resolution_schedule_hash_data),
    signer_count: builtin.un_i_data(signer_count_data),
    signer_frontier_commitment: builtin.un_b_data(
      signer_frontier_commitment_data,
    ),
    contains_non_native_script: builtin.un_i_data(
      contains_non_native_script_data,
    ),
    observer_count: builtin.un_i_data(observer_count_data),
    observer_seen: builtin.un_i_data(observer_seen_data),
    previous_observer: builtin.un_b_data(previous_observer_data),
  }
}

fn phase_a_script_preconditions_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseAScriptPreconditionsControlV1,
  observer_commitment: ByteArray,
) -> Bool {
  let observers_are_empty = observer_commitment == bounded_collection_v1.from_items(
    3,
    [],
  )
  and {
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    bytearray.length(control.resolution_schedule_hash) == 32,
    control.signer_count >= 0,
    bytearray.length(control.signer_frontier_commitment) == 32,
    control.contains_non_native_script == 0 || control.contains_non_native_script == 1,
    control.observer_count >= 0,
    control.observer_count <= max_tx_size_derived_collection_item_count,
    control.observer_seen >= 0,
    if observers_are_empty {
      and {
        control.observer_count == 0,
        control.observer_seen == 0,
        control.previous_observer == #"",
      }
    } else if control.observer_count == 0 {
      and {
        control.observer_seen == 0,
        control.previous_observer == #"",
      }
    } else {
      and {
        control.observer_count > 0,
        control.observer_seen <= control.observer_count,
        if control.observer_seen == 0 {
          control.previous_observer == #""
        } else {
          bytearray.length(control.previous_observer) == 28
        },
      }
    },
    witness.work_witness_cbor == encode_phase_a_script_preconditions_witness(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      control.context_cbor,
      control.resolution_schedule_hash,
      control.signer_count,
      control.signer_frontier_commitment,
      control.contains_non_native_script == 1,
      control.observer_count,
      control.observer_seen,
      control.previous_observer,
    ),
  }
}

fn phase_a_script_preconditions_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  post: validation_trace_v1.ValidationMachineStateV1,
  control: PhaseAScriptPreconditionsControlV1,
) -> Bool {
  and {
    post.phase == validation_trace_v1.PhaseAScriptPreconditions,
    post.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseAScriptPreconditions,
      pre.program_counter + 1,
      encode_phase_a_script_preconditions_witness(
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
        control.context_cbor,
        control.resolution_schedule_hash,
        control.signer_count,
        control.signer_frontier_commitment,
        control.contains_non_native_script == 1,
        control.observer_count,
        control.observer_seen,
        control.previous_observer,
      ),
    ),
  }
}

fn phase_a_script_preconditions_finalize(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: PhaseAScriptPreconditionsControlV1,
  verified_source: VerifiedMidgardNativeTxCompact,
  has_redeemers: Bool,
  observer_count: Int,
) -> Bool {
  when
    phase_a_script_preconditions_rejection(
      observer_count,
      has_redeemers,
      control.contains_non_native_script == 1,
      verified_source.tx_compact.body.script_integrity_hash,
      verified_source.tx_compact.body.network_id,
    )
  is {
    Some(rejection_code) ->
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        rejection_code,
      )
    None -> and {
        witness.claimed_successor.phase == validation_trace_v1.ResolveInputs,
        witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.ResolveInputs,
          pre.program_counter + 1,
          encode_resolve_inputs_witness(
            control.compact_cbor,
            control.witness_set_compact_cbor,
            control.field_preimage_lengths_cbor,
            control.context_cbor,
            0,
            initial_resolution_accumulator(),
            control.resolution_schedule_hash,
            control.signer_count,
            control.signer_frontier_commitment,
            None,
            control.resolution_schedule_hash,
          ),
        ),
      }
  }
}

fn verify_phase_a_script_preconditions(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let control = phase_a_script_preconditions_control_from_witness(
    witness.work_witness_cbor,
  )
  let Pair(verified_source, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let observer_commitment = verified_source.tx_compact.body.required_observers_hash
  let empty_observer_commitment = bounded_collection_v1.from_items(3, [])
  let has_redeemers = witness_set.redeemer_tx_wits_hash != bounded_collection_v1.from_items(
    8,
    [],
  )
  and {
    verified_source.version == 1,
    phase_a_script_preconditions_control_is_bound(
      pre,
      witness,
      control,
      observer_commitment,
    ),
    if observer_commitment == empty_observer_commitment {
      and {
        auxiliary == NoAuxiliaryWitness,
        phase_a_script_preconditions_finalize(
          pre,
          witness,
          control,
          verified_source,
          has_redeemers,
          0,
        ),
      }
    } else if control.observer_count > 0 && control.observer_seen == control.observer_count {
      and {
        auxiliary == NoAuxiliaryWitness,
        phase_a_script_preconditions_finalize(
          pre,
          witness,
          control,
          verified_source,
          has_redeemers,
          control.observer_count,
        ),
      }
    } else {
      expect TransactionFieldChunkWitness { collection_proof, chunk_proof } = auxiliary
      let active_count = if control.observer_count == 0 {
        collection_proof.item_count
      } else {
        control.observer_count
      }
      let observer_hash = chunk_proof.chunk
      and {
        active_count > 0,
        active_count <= max_tx_size_derived_collection_item_count,
        collection_proof.field_index == 3,
        collection_proof.item_count == active_count,
        collection_proof.item_index == control.observer_seen,
        collection_proof.item_length == 28,
        chunk_proof.field_index == 3,
        chunk_proof.item_index == collection_proof.item_index,
        chunk_proof.total_length == 28,
        chunk_proof.chunk_index == 0,
        bounded_item_v1.chunk_count(chunk_proof.total_length) == 1,
        bounded_collection_v1.verify_item(observer_commitment, collection_proof),
        bounded_item_v1.verify_chunk(
          collection_proof.item_commitment,
          chunk_proof,
        ),
        if and {
          control.observer_seen > 0,
          bytearray.compare(control.previous_observer, observer_hash) != Less,
        } {
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          )
        } else {
          phase_a_script_preconditions_successor_is_exact(
            pre,
            witness.claimed_successor,
            PhaseAScriptPreconditionsControlV1 {
              ..control,
              observer_count: active_count,
              observer_seen: control.observer_seen + 1,
              previous_observer: observer_hash,
            },
          )
        },
      }
    },
  }
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

fn resolve_inputs_control_from_witness(
  witness_cbor: ByteArray,
) -> ResolveInputsControlV1 {
  expect Some(data) = cbor.deserialise(witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data, cursor_data,
        accumulator_data, remaining_schedule_hash_data, signer_count_data,
        signer_frontier_commitment_data, pending_cbor_data,
        resolution_schedule_hash_data,
      ]
    = builtin.un_list_data(data)
  ResolveInputsControlV1 {
    compact_cbor: builtin.un_b_data(compact_cbor_data),
    witness_set_compact_cbor: builtin.un_b_data(witness_set_compact_cbor_data),
    field_preimage_lengths_cbor: builtin.un_b_data(
      field_preimage_lengths_cbor_data,
    ),
    context_cbor: builtin.un_b_data(context_cbor_data),
    cursor: builtin.un_i_data(cursor_data),
    accumulator: builtin.un_b_data(accumulator_data),
    remaining_schedule_hash: builtin.un_b_data(remaining_schedule_hash_data),
    signer_count: builtin.un_i_data(signer_count_data),
    signer_frontier_commitment: builtin.un_b_data(
      signer_frontier_commitment_data,
    ),
    pending: if builtin.un_b_data(pending_cbor_data) == #"00" {
      None
    } else {
      Some(
        pending_cbor_data
          |> builtin.un_b_data
          |> resolve_input_output_proof_from_cbor,
      )
    },
    resolution_schedule_hash: builtin.un_b_data(resolution_schedule_hash_data),
  }
}

const reject_validity_interval_mismatch =
  #"455f56414c49444954595f494e54455256414c5f4d49534d41544348"

const reject_input_not_found = #"455f494e5055545f4e4f545f464f554e44"

const reject_invalid_output = #"455f494e56414c49445f4f5554505554"

fn validity_interval_contains_slot(
  body: NativeTxBodyCompact,
  block_slot: Int,
) -> Bool {
  and {
    or {
      body.validity_interval_start < 0,
      block_slot >= body.validity_interval_start,
    },
    or {
      body.validity_interval_end < 0,
      block_slot <= body.validity_interval_end,
    },
  }
}

pub type InputSignerAuthorizationV1 {
  InputSignerAuthorized
  InputSignerMissing
  InputSignerProofMalformed
}

fn signer_frontier_matches(
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  peaks: List<validation_merkle_v1.FrontierPeak>,
) -> Bool {
  and {
    validation_merkle_v1.frontier_is_well_formed(signer_count, peaks),
    validation_merkle_v1.frontier_commitment(signer_count, peaks) == signer_frontier_commitment,
  }
}

fn signer_membership_is_valid(
  signer_hash: ByteArray,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  peaks: List<validation_merkle_v1.FrontierPeak>,
  signer_index: Int,
  siblings: List<ByteArray>,
) -> Bool {
  and {
    bytearray.length(signer_hash) == 28,
    signer_frontier_matches(signer_count, signer_frontier_commitment, peaks),
    validation_merkle_v1.verify_membership(
      signer_count,
      peaks,
      signer_index,
      script_proof_v1.signer_leaf_hash(signer_hash),
      siblings,
    ),
  }
}

fn signer_non_membership_is_valid(
  signer_hash: ByteArray,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  proof: SignerSetProofV1,
) -> Bool {
  when proof is {
    EmptySignerSetProof { peaks } -> and {
        signer_count == 0,
        signer_frontier_matches(signer_count, signer_frontier_commitment, peaks),
      }
    SignerBelowFirstProof { peaks, first_signer_hash, siblings } -> and {
        bytearray.compare(signer_hash, first_signer_hash) == Less,
        signer_membership_is_valid(
          first_signer_hash,
          signer_count,
          signer_frontier_commitment,
          peaks,
          0,
          siblings,
        ),
      }
    SignerAboveLastProof { peaks, last_signer_hash, siblings } -> and {
        signer_count > 0,
        bytearray.compare(last_signer_hash, signer_hash) == Less,
        signer_membership_is_valid(
          last_signer_hash,
          signer_count,
          signer_frontier_commitment,
          peaks,
          signer_count - 1,
          siblings,
        ),
      }
    SignerBetweenProof {
      peaks,
      lower_index,
      lower_signer_hash,
      lower_siblings,
      upper_signer_hash,
      upper_siblings,
    } -> and {
        lower_index >= 0,
        lower_index + 1 < signer_count,
        bytearray.compare(lower_signer_hash, signer_hash) == Less,
        bytearray.compare(signer_hash, upper_signer_hash) == Less,
        signer_membership_is_valid(
          lower_signer_hash,
          signer_count,
          signer_frontier_commitment,
          peaks,
          lower_index,
          lower_siblings,
        ),
        signer_membership_is_valid(
          upper_signer_hash,
          signer_count,
          signer_frontier_commitment,
          peaks,
          lower_index + 1,
          upper_siblings,
        ),
      }
    NoSignerSetProof -> False
    SignerMembershipProof { .. } -> False
  }
}

fn input_signer_authorization(
  source_kind: Int,
  address: MidgardAddress,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  signer_proof: SignerSetProofV1,
) -> InputSignerAuthorizationV1 {
  if source_kind == 1 {
    if signer_proof == NoSignerSetProof {
      InputSignerAuthorized
    } else {
      InputSignerProofMalformed
    }
  } else {
    expect source_kind == 0
    payment_credential_signer_authorization(
      address.payment_credential,
      signer_count,
      signer_frontier_commitment,
      signer_proof,
    )
  }
}

fn payment_credential_signer_authorization(
  payment_credential: MidgardCredential,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  signer_proof: SignerSetProofV1,
) -> InputSignerAuthorizationV1 {
  when payment_credential is {
    ScriptCredential(_) ->
      if signer_proof == NoSignerSetProof {
        InputSignerAuthorized
      } else {
        InputSignerProofMalformed
      }
    PubKeyCredential(signer_hash) ->
      when signer_proof is {
        SignerMembershipProof { peaks, signer_index, siblings } ->
          if signer_membership_is_valid(
            signer_hash,
            signer_count,
            signer_frontier_commitment,
            peaks,
            signer_index,
            siblings,
          ) {
            InputSignerAuthorized
          } else {
            InputSignerProofMalformed
          }
        _ ->
          if signer_non_membership_is_valid(
            signer_hash,
            signer_count,
            signer_frontier_commitment,
            signer_proof,
          ) {
            InputSignerMissing
          } else {
            InputSignerProofMalformed
          }
      }
  }
}

fn resolve_inputs_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  next_control: ResolveInputsControlV1,
) -> Bool {
  and {
    witness.claimed_successor.phase == validation_trace_v1.ResolveInputs,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ResolveInputs,
      pre.program_counter + 1,
      encode_resolve_inputs_witness(
        next_control.compact_cbor,
        next_control.witness_set_compact_cbor,
        next_control.field_preimage_lengths_cbor,
        next_control.context_cbor,
        next_control.cursor,
        next_control.accumulator,
        next_control.remaining_schedule_hash,
        next_control.signer_count,
        next_control.signer_frontier_commitment,
        next_control.pending,
        next_control.resolution_schedule_hash,
      ),
    ),
  }
}

fn resolve_membership_begin_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ResolveInputsControlV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  descriptor_cbor: ByteArray,
  proof: Proof,
  signer_proof: SignerSetProofV1,
) -> Bool {
  let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
  let input = decode_midgard_tx_input_cbor(key)
  and {
    control.pending == None,
    signer_proof == NoSignerSetProof,
    source_kind == 0 || source_kind == 1,
    resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == control.remaining_schedule_hash,
    input.output_index == descriptor.output_index,
    descriptor.total_length > 0,
    mpf_proof_v1.has(pre.prior_ledger_root, key, descriptor_cbor, proof),
    resolve_inputs_successor_is_exact(
      pre,
      witness,
      ResolveInputsControlV1 {
        ..control,
        pending: Some(
          ResolveInputOutputProofV1 {
            source_kind,
            key,
            next_schedule_hash,
            descriptor_cbor,
            output_proof: ledger_output_proof_v1.initial_control_v1(
              descriptor.output_index,
              descriptor.total_length,
              descriptor.item_commitment,
            ),
          },
        ),
      },
    ),
  }
}

fn resolve_membership_proof_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ResolveInputsControlV1,
  pending: ResolveInputOutputProofV1,
  proof_witness: ledger_output_proof_v1.LedgerOutputProofWitnessV1,
) -> Bool {
  when ledger_output_proof_v1.step_v1(pending.output_proof, proof_witness) is {
    None -> False
    Some(result) ->
      when result is {
        ledger_output_proof_v1.LedgerOutputProofAdvanced {
          control: next_output_proof,
        } ->
          resolve_inputs_successor_is_exact(
            pre,
            witness,
            ResolveInputsControlV1 {
              ..control,
              pending: Some(
                ResolveInputOutputProofV1 {
                  ..pending,
                  output_proof: next_output_proof,
                },
              ),
            },
          )
        ledger_output_proof_v1.LedgerOutputProofInvalidOutput ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_output,
          )
        ledger_output_proof_v1.LedgerOutputProofInvalidReferenceScript ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          )
        ledger_output_proof_v1.LedgerOutputProofNativeScriptNodeLimit ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_native_script_node_count,
          )
        ledger_output_proof_v1.LedgerOutputProofNativeScriptDepthLimit ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_native_script_depth,
          )
      }
  }
}

fn resolve_membership_proof_finalize(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ResolveInputsControlV1,
  pending: ResolveInputOutputProofV1,
  descriptor_cbor: ByteArray,
  signer_proof: SignerSetProofV1,
) -> Bool {
  let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
  if and {
    descriptor_cbor == pending.descriptor_cbor,
    ledger_output_proof_v1.descriptor_is_exact_v1(
      pending.output_proof,
      descriptor,
    ),
  } {
    expect Some(address) = ledger_output_v1.decode_canonical_address_bytes(
      descriptor.address,
    )
    when
      input_signer_authorization(
        pending.source_kind,
        address,
        control.signer_count,
        control.signer_frontier_commitment,
        signer_proof,
      )
    is {
      InputSignerMissing ->
        rejected_successor_is_exact(
          pre,
          witness.claimed_successor,
          reject_missing_required_witness,
        )
      InputSignerProofMalformed -> False
      InputSignerAuthorized ->
        resolve_inputs_successor_is_exact(
          pre,
          witness,
          ResolveInputsControlV1 {
            ..control,
            cursor: control.cursor + 1,
            accumulator: resolved_input_accumulator_successor(
              control.accumulator,
              pending.source_kind,
              pending.key,
              descriptor_cbor,
            ),
            remaining_schedule_hash: pending.next_schedule_hash,
            pending: None,
          },
        )
    }
  } else {
    False
  }
}

fn resolve_non_membership_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ResolveInputsControlV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  proof: Proof,
) -> Bool {
  and {
    resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == control.remaining_schedule_hash,
    mpf_proof_v1.does_not_have(pre.prior_ledger_root, key, proof),
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_input_not_found,
    ),
  }
}

fn resolve_lookup_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ResolveInputsControlV1,
) -> Bool {
  when control.pending is {
    Some(pending) ->
      when auxiliary is {
        LedgerOutputProofStepWitness { witness: proof_witness } ->
          resolve_membership_proof_step(
            pre,
            witness,
            control,
            pending,
            proof_witness,
          )
        LedgerOutputProofFinalizeWitness { descriptor_cbor, signer_proof } ->
          resolve_membership_proof_finalize(
            pre,
            witness,
            control,
            pending,
            descriptor_cbor,
            signer_proof,
          )
        _ -> False
      }
    None ->
      when auxiliary is {
        ScheduledLedgerMembershipWitness {
          source_kind,
          key,
          next_schedule_hash,
          value,
          proof,
          signer_proof,
        } ->
          resolve_membership_begin_step(
            pre,
            witness,
            control,
            source_kind,
            key,
            next_schedule_hash,
            value,
            proof,
            signer_proof,
          )
        ScheduledLedgerNonMembershipWitness {
          source_kind,
          key,
          next_schedule_hash,
          proof,
        } ->
          resolve_non_membership_step(
            pre,
            witness,
            control,
            source_kind,
            key,
            next_schedule_hash,
            proof,
          )
        _ -> False
      }
  }
}

fn resolve_inputs_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ResolveInputsControlV1,
) -> Bool {
  let pending_is_well_formed = when control.pending is {
    None -> True
    Some(pending) -> {
      let descriptor = ledger_output_commitment_v1.decode(
        pending.descriptor_cbor,
      )
      let input = decode_midgard_tx_input_cbor(pending.key)
      and {
        control.cursor > 0,
        pending.source_kind == 0 || pending.source_kind == 1,
        resolution_schedule_node_hash(
          pending.source_kind,
          pending.key,
          pending.next_schedule_hash,
        ) == control.remaining_schedule_hash,
        input.output_index == descriptor.output_index,
        ledger_output_proof_v1.control_is_well_formed(pending.output_proof),
        pending.output_proof.output_index == descriptor.output_index,
        pending.output_proof.total_length == descriptor.total_length,
        pending.output_proof.item_commitment == descriptor.item_commitment,
      }
    }
  }
  and {
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    bytearray.length(control.accumulator) == 32,
    bytearray.length(control.remaining_schedule_hash) == 32,
    bytearray.length(control.resolution_schedule_hash) == 32,
    if control.cursor <= 1 {
      control.remaining_schedule_hash == control.resolution_schedule_hash
    } else {
      True
    },
    control.signer_count >= 0,
    bytearray.length(control.signer_frontier_commitment) == 32,
    control.cursor >= 0,
    pending_is_well_formed,
    witness.work_witness_cbor == encode_resolve_inputs_witness(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      control.context_cbor,
      control.cursor,
      control.accumulator,
      control.remaining_schedule_hash,
      control.signer_count,
      control.signer_frontier_commitment,
      control.pending,
      control.resolution_schedule_hash,
    ),
  }
}

fn verify_resolve_inputs(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let control = resolve_inputs_control_from_witness(witness.work_witness_cbor)
  let Pair(verified, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let context = decode_validation_context(control.context_cbor)
  and {
    verified.version == 1,
    resolve_inputs_control_is_bound(pre, witness, control),
    if control.cursor == 0 {
      and {
        auxiliary == NoAuxiliaryWitness,
        if validity_interval_contains_slot(
          verified.tx_compact.body,
          context.block_slot,
        ) {
          let next_control = encode_resolve_inputs_witness(
            control.compact_cbor,
            control.witness_set_compact_cbor,
            control.field_preimage_lengths_cbor,
            control.context_cbor,
            1,
            control.accumulator,
            control.remaining_schedule_hash,
            control.signer_count,
            control.signer_frontier_commitment,
            None,
            control.resolution_schedule_hash,
          )
          and {
            witness.claimed_successor.phase == validation_trace_v1.ResolveInputs,
            witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
              validation_trace_v1.ResolveInputs,
              pre.program_counter + 1,
              next_control,
            ),
          }
        } else {
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_validity_interval_mismatch,
          )
        },
      }
    } else if and {
      control.pending == None,
      control.remaining_schedule_hash == empty_resolution_schedule_hash(),
    } {
      and {
        auxiliary == NoAuxiliaryWitness,
        witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
        witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.ScriptSources,
          pre.program_counter + 1,
          encode_script_sources_witness(
            control.compact_cbor,
            control.witness_set_compact_cbor,
            control.field_preimage_lengths_cbor,
            control.context_cbor,
            control.cursor - 1,
            control.accumulator,
            control.signer_count,
            control.signer_frontier_commitment,
            validation_merkle_v1.empty_frontier(),
            0,
            0,
            validation_merkle_v1.empty_frontier(),
            0,
            validation_merkle_v1.empty_frontier(),
            0,
            initial_resolution_accumulator(),
            empty_resolution_schedule_hash(),
            0,
            0,
            validation_merkle_v1.empty_frontier(),
            0,
            0,
            validation_merkle_v1.empty_frontier(),
            0,
            empty_receive_purpose_scan_control(),
            0,
            0,
            empty_observer_purpose_scan_control(),
            empty_mint_fold_control(),
            control.resolution_schedule_hash,
          ),
        ),
      }
    } else {
      resolve_lookup_step(pre, witness, auxiliary, control)
    },
  }
}

pub fn verify_resolve_inputs_initial_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = resolve_inputs_control_from_witness(witness.work_witness_cbor)
  let Pair(verified, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let context = decode_validation_context(control.context_cbor)
  and {
    verified.version == 1,
    resolve_inputs_control_is_bound(pre, witness, control),
    control.cursor == 0,
    control.pending == None,
    if validity_interval_contains_slot(
      verified.tx_compact.body,
      context.block_slot,
    ) {
      let next_control = encode_resolve_inputs_witness(
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
        control.context_cbor,
        1,
        control.accumulator,
        control.remaining_schedule_hash,
        control.signer_count,
        control.signer_frontier_commitment,
        None,
        control.resolution_schedule_hash,
      )
      and {
        witness.claimed_successor.phase == validation_trace_v1.ResolveInputs,
        witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.ResolveInputs,
          pre.program_counter + 1,
          next_control,
        ),
      }
    } else {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_validity_interval_mismatch,
      )
    },
  }
}

pub fn verify_resolve_inputs_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = resolve_inputs_control_from_witness(witness.work_witness_cbor)
  and {
    resolve_inputs_control_is_bound(pre, witness, control),
    control.cursor > 0,
    control.pending == None,
    control.remaining_schedule_hash == empty_resolution_schedule_hash(),
    witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      pre.program_counter + 1,
      encode_script_sources_witness(
        control.compact_cbor,
        control.witness_set_compact_cbor,
        control.field_preimage_lengths_cbor,
        control.context_cbor,
        control.cursor - 1,
        control.accumulator,
        control.signer_count,
        control.signer_frontier_commitment,
        validation_merkle_v1.empty_frontier(),
        0,
        0,
        validation_merkle_v1.empty_frontier(),
        0,
        validation_merkle_v1.empty_frontier(),
        0,
        initial_resolution_accumulator(),
        empty_resolution_schedule_hash(),
        0,
        0,
        validation_merkle_v1.empty_frontier(),
        0,
        0,
        validation_merkle_v1.empty_frontier(),
        0,
        empty_receive_purpose_scan_control(),
        0,
        0,
        empty_observer_purpose_scan_control(),
        empty_mint_fold_control(),
        control.resolution_schedule_hash,
      ),
    ),
  }
}

pub fn verify_resolve_inputs_membership_begin_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  value: ByteArray,
  proof: Proof,
  signer_proof: SignerSetProofV1,
) -> Bool {
  let control = resolve_inputs_control_from_witness(witness.work_witness_cbor)
  and {
    resolve_inputs_control_is_bound(pre, witness, control),
    control.cursor > 0,
    control.pending == None,
    control.remaining_schedule_hash != empty_resolution_schedule_hash(),
    resolve_membership_begin_step(
      pre,
      witness,
      control,
      source_kind,
      key,
      next_schedule_hash,
      value,
      proof,
      signer_proof,
    ),
  }
}

pub fn verify_resolve_inputs_membership_step_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  proof_witness: ledger_output_proof_v1.LedgerOutputProofWitnessV1,
) -> Bool {
  let control = resolve_inputs_control_from_witness(witness.work_witness_cbor)
  when control.pending is {
    None -> False
    Some(pending) -> and {
        resolve_inputs_control_is_bound(pre, witness, control),
        resolve_membership_proof_step(
          pre,
          witness,
          control,
          pending,
          proof_witness,
        ),
      }
  }
}

pub fn verify_resolve_inputs_membership_finalize_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  descriptor_cbor: ByteArray,
  signer_proof: SignerSetProofV1,
) -> Bool {
  let control = resolve_inputs_control_from_witness(witness.work_witness_cbor)
  when control.pending is {
    None -> False
    Some(pending) -> and {
        resolve_inputs_control_is_bound(pre, witness, control),
        resolve_membership_proof_finalize(
          pre,
          witness,
          control,
          pending,
          descriptor_cbor,
          signer_proof,
        ),
      }
  }
}

pub fn verify_resolve_inputs_non_membership_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  proof: Proof,
) -> Bool {
  let control = resolve_inputs_control_from_witness(witness.work_witness_cbor)
  and {
    resolve_inputs_control_is_bound(pre, witness, control),
    control.cursor > 0,
    control.pending == None,
    control.remaining_schedule_hash != empty_resolution_schedule_hash(),
    resolve_non_membership_step(
      pre,
      witness,
      control,
      source_kind,
      key,
      next_schedule_hash,
      proof,
    ),
  }
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

pub fn empty_mint_fold_control() -> MintFoldControlV1 {
  MintFoldControlV1 {
    policy_count: -1,
    policy_cursor: 0,
    previous_policy: #"",
    active_policy: #"",
    item_length: 0,
    item_commitment: #"",
    item_cursor: 0,
    assets_remaining: 0,
    policy_asset_cursor: 0,
    previous_asset: #"",
    asset_count: 0,
    asset_peaks: validation_merkle_v1.empty_frontier(),
  }
}

pub fn encode_mint_fold_control(control: MintFoldControlV1) -> ByteArray {
  encode_definite_array_header(12)
    |> bytearray.concat(cbor.serialise(control.policy_count))
    |> bytearray.concat(cbor.serialise(control.policy_cursor))
    |> bytearray.concat(encode_definite_bytes(control.previous_policy))
    |> bytearray.concat(encode_definite_bytes(control.active_policy))
    |> bytearray.concat(cbor.serialise(control.item_length))
    |> bytearray.concat(encode_definite_bytes(control.item_commitment))
    |> bytearray.concat(cbor.serialise(control.item_cursor))
    |> bytearray.concat(cbor.serialise(control.assets_remaining))
    |> bytearray.concat(cbor.serialise(control.policy_asset_cursor))
    |> bytearray.concat(encode_definite_bytes(control.previous_asset))
    |> bytearray.concat(cbor.serialise(control.asset_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.asset_peaks),
      )
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
  execution_count: Int,
  execution_peaks: List<validation_merkle_v1.FrontierPeak>,
  redeemer_item_control_hash: ByteArray,
}

pub fn empty_script_discovery_control() -> ScriptDiscoveryControlV1 {
  ScriptDiscoveryControlV1 {
    purpose_cursor: 0,
    source_cursor: 0,
    redeemer_cursor: 0,
    current_purpose_kind: -1,
    current_purpose_index: -1,
    current_script_hash: #"",
    current_subject: #"",
    matched_source_index: -1,
    matched_language_tag: -1,
    matched_source_leaf: #"",
    used_inline_bitmap: 0,
    used_redeemer_bitmap: 0,
    execution_count: 0,
    execution_peaks: validation_merkle_v1.empty_frontier(),
    redeemer_item_control_hash: #"",
  }
}

pub fn encode_script_discovery_control(
  control: ScriptDiscoveryControlV1,
) -> ByteArray {
  encode_definite_array_header(15)
    |> bytearray.concat(cbor.serialise(control.purpose_cursor))
    |> bytearray.concat(cbor.serialise(control.source_cursor))
    |> bytearray.concat(cbor.serialise(control.redeemer_cursor))
    |> bytearray.concat(cbor.serialise(control.current_purpose_kind))
    |> bytearray.concat(cbor.serialise(control.current_purpose_index))
    |> bytearray.concat(encode_definite_bytes(control.current_script_hash))
    |> bytearray.concat(encode_definite_bytes(control.current_subject))
    |> bytearray.concat(cbor.serialise(control.matched_source_index))
    |> bytearray.concat(cbor.serialise(control.matched_language_tag))
    |> bytearray.concat(encode_definite_bytes(control.matched_source_leaf))
    |> bytearray.concat(cbor.serialise(control.used_inline_bitmap))
    |> bytearray.concat(cbor.serialise(control.used_redeemer_bitmap))
    |> bytearray.concat(cbor.serialise(control.execution_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.execution_peaks),
      )
    |> bytearray.concat(
        encode_definite_bytes(control.redeemer_item_control_hash),
      )
}

fn script_discovery_control_from_cbor(
  discovery_cbor: ByteArray,
) -> ScriptDiscoveryControlV1 {
  expect Some(data) = cbor.deserialise(discovery_cbor)
  expect
      [
        purpose_cursor_data, source_cursor_data, redeemer_cursor_data,
        current_purpose_kind_data, current_purpose_index_data,
        current_script_hash_data, current_subject_data,
        matched_source_index_data, matched_language_tag_data,
        matched_source_leaf_data, used_inline_bitmap_data,
        used_redeemer_bitmap_data, execution_count_data, execution_peaks_data,
        redeemer_item_control_hash_data,
      ]
    = builtin.un_list_data(data)
  ScriptDiscoveryControlV1 {
    purpose_cursor: builtin.un_i_data(purpose_cursor_data),
    source_cursor: builtin.un_i_data(source_cursor_data),
    redeemer_cursor: builtin.un_i_data(redeemer_cursor_data),
    current_purpose_kind: builtin.un_i_data(current_purpose_kind_data),
    current_purpose_index: builtin.un_i_data(current_purpose_index_data),
    current_script_hash: builtin.un_b_data(current_script_hash_data),
    current_subject: builtin.un_b_data(current_subject_data),
    matched_source_index: builtin.un_i_data(matched_source_index_data),
    matched_language_tag: builtin.un_i_data(matched_language_tag_data),
    matched_source_leaf: builtin.un_b_data(matched_source_leaf_data),
    used_inline_bitmap: builtin.un_i_data(used_inline_bitmap_data),
    used_redeemer_bitmap: builtin.un_i_data(used_redeemer_bitmap_data),
    execution_count: builtin.un_i_data(execution_count_data),
    execution_peaks: decode_frontier_peaks(execution_peaks_data),
    redeemer_item_control_hash: builtin.un_b_data(
      redeemer_item_control_hash_data,
    ),
  }
}

fn decode_frontier_peak_items(
  items: List<Data>,
) -> List<validation_merkle_v1.FrontierPeak> {
  when items is {
    [] -> []
    [item, ..rest] -> {
      expect [height_data, hash_data] = builtin.un_list_data(item)
      [
        validation_merkle_v1.FrontierPeak {
          height: builtin.un_i_data(height_data),
          hash: builtin.un_b_data(hash_data),
        },
        ..decode_frontier_peak_items(rest)
      ]
    }
  }
}

fn decode_frontier_peaks(data: Data) -> List<validation_merkle_v1.FrontierPeak> {
  data |> builtin.un_list_data |> decode_frontier_peak_items
}

fn split_last_data(items: List<Data>) -> Pair<List<Data>, Data> {
  expect [first, ..rest] = items
  when rest is {
    [last] -> Pair([first], last)
    _ -> {
      let Pair(prefix, last) = split_last_data(rest)
      Pair([first, ..prefix], last)
    }
  }
}

fn mint_fold_control_from_data(data: Data) -> MintFoldControlV1 {
  expect
      [
        policy_count_data, policy_cursor_data, previous_policy_data,
        active_policy_data, item_length_data, item_commitment_data,
        item_cursor_data, assets_remaining_data, policy_asset_cursor_data,
        previous_asset_data, asset_count_data, asset_peaks_data,
      ]
    = builtin.un_list_data(data)
  MintFoldControlV1 {
    policy_count: builtin.un_i_data(policy_count_data),
    policy_cursor: builtin.un_i_data(policy_cursor_data),
    previous_policy: builtin.un_b_data(previous_policy_data),
    active_policy: builtin.un_b_data(active_policy_data),
    item_length: builtin.un_i_data(item_length_data),
    item_commitment: builtin.un_b_data(item_commitment_data),
    item_cursor: builtin.un_i_data(item_cursor_data),
    assets_remaining: builtin.un_i_data(assets_remaining_data),
    policy_asset_cursor: builtin.un_i_data(policy_asset_cursor_data),
    previous_asset: builtin.un_b_data(previous_asset_data),
    asset_count: builtin.un_i_data(asset_count_data),
    asset_peaks: decode_frontier_peaks(asset_peaks_data),
  }
}

fn script_sources_control_from_data_items(
  items: List<Data>,
  discovery: ScriptDiscoveryControlV1,
  output_proof: Option<ledger_output_proof_v1.LedgerOutputProofControlV1>,
) -> ScriptSourcesControlV1 {
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data,
        resolved_input_count_data, resolved_inputs_accumulator_data,
        signer_count_data, signer_frontier_commitment_data,
        resolved_item_peaks_data, stage_data, source_count_data,
        source_peaks_data, redeemer_count_data, redeemer_peaks_data,
        replay_cursor_data, replay_accumulator_data,
        replay_remaining_schedule_hash_data, spend_index_data,
        purpose_count_data, purpose_peaks_data, output_cursor_data,
        output_count_data, output_peaks_data, output_total_count_data,
        receive_scan_data, source_total_count_data, redeemer_total_count_data,
        observer_scan_data, mint_fold_data, resolution_schedule_hash_data,
      ]
    = items
  expect
      [
        observer_total_count_data, observer_previous_hash_data,
        observer_seen_data,
      ]
    = builtin.un_list_data(observer_scan_data)
  expect
      [
        receive_source_count_data, receive_source_peaks_data, receive_count_data,
        receive_previous_hash_data, receive_candidate_hash_data,
        receive_descriptor_peaks_data,
      ]
    = builtin.un_list_data(receive_scan_data)
  expect
    bytearray.length(builtin.un_b_data(resolution_schedule_hash_data)) == 32
  ScriptSourcesControlV1 {
    compact_cbor: builtin.un_b_data(compact_cbor_data),
    witness_set_compact_cbor: builtin.un_b_data(witness_set_compact_cbor_data),
    field_preimage_lengths_cbor: builtin.un_b_data(
      field_preimage_lengths_cbor_data,
    ),
    context_cbor: builtin.un_b_data(context_cbor_data),
    resolved_input_count: builtin.un_i_data(resolved_input_count_data),
    resolved_inputs_accumulator: builtin.un_b_data(
      resolved_inputs_accumulator_data,
    ),
    signer_count: builtin.un_i_data(signer_count_data),
    signer_frontier_commitment: builtin.un_b_data(
      signer_frontier_commitment_data,
    ),
    resolved_item_peaks: decode_frontier_peaks(resolved_item_peaks_data),
    stage: builtin.un_i_data(stage_data),
    source_count: builtin.un_i_data(source_count_data),
    source_peaks: decode_frontier_peaks(source_peaks_data),
    redeemer_count: builtin.un_i_data(redeemer_count_data),
    redeemer_peaks: decode_frontier_peaks(redeemer_peaks_data),
    replay_cursor: builtin.un_i_data(replay_cursor_data),
    replay_accumulator: builtin.un_b_data(replay_accumulator_data),
    replay_remaining_schedule_hash: builtin.un_b_data(
      replay_remaining_schedule_hash_data,
    ),
    spend_index: builtin.un_i_data(spend_index_data),
    purpose_count: builtin.un_i_data(purpose_count_data),
    purpose_peaks: decode_frontier_peaks(purpose_peaks_data),
    output_cursor: builtin.un_i_data(output_cursor_data),
    output_count: builtin.un_i_data(output_count_data),
    output_peaks: decode_frontier_peaks(output_peaks_data),
    output_total_count: builtin.un_i_data(output_total_count_data),
    receive_scan: ReceivePurposeScanControlV1 {
      source_count: builtin.un_i_data(receive_source_count_data),
      source_peaks: decode_frontier_peaks(receive_source_peaks_data),
      receive_count: builtin.un_i_data(receive_count_data),
      previous_hash: builtin.un_b_data(receive_previous_hash_data),
      candidate_hash: builtin.un_b_data(receive_candidate_hash_data),
      descriptor_peaks: decode_frontier_peaks(receive_descriptor_peaks_data),
    },
    source_total_count: builtin.un_i_data(source_total_count_data),
    redeemer_total_count: builtin.un_i_data(redeemer_total_count_data),
    observer_scan: ObserverPurposeScanControlV1 {
      total_count: builtin.un_i_data(observer_total_count_data),
      seen: builtin.un_i_data(observer_seen_data),
      previous_hash: builtin.un_b_data(observer_previous_hash_data),
    },
    discovery,
    output_proof,
    pending_source_cbor: #"",
    mint_fold: mint_fold_control_from_data(mint_fold_data),
    resolution_schedule_hash: builtin.un_b_data(resolution_schedule_hash_data),
  }
}

fn script_sources_control_from_witness(
  work_witness_cbor: ByteArray,
) -> ScriptSourcesControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let items = builtin.un_list_data(data)
  if list.length(items) == 30 {
    script_sources_control_from_data_items(
      items,
      empty_script_discovery_control(),
      None,
    )
  } else {
    expect list.length(items) == 31
    let Pair(base_items, extension_data) = split_last_data(items)
    let base_control = script_sources_control_from_data_items(
      base_items,
      empty_script_discovery_control(),
      None,
    )
    if base_control.stage == 0 {
      ScriptSourcesControlV1 {
        ..base_control,
        pending_source_cbor: builtin.un_b_data(extension_data),
      }
    } else if base_control.stage == 1 {
      ScriptSourcesControlV1 {
        ..base_control,
        pending_source_cbor: builtin.un_b_data(extension_data),
      }
    } else if base_control.stage == 5 {
      ScriptSourcesControlV1 {
        ..base_control,
        output_proof: Some(
          extension_data
            |> builtin.un_b_data
            |> ledger_output_proof_v1.decode_control_v1,
        ),
      }
    } else {
      expect base_control.stage >= 8
      ScriptSourcesControlV1 {
        ..base_control,
        discovery: extension_data
          |> builtin.un_b_data
          |> script_discovery_control_from_cbor,
      }
    }
  }
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

pub fn encode_inline_source_hash_control_v1(
  control: InlineSourceHashControlV1,
) -> ByteArray {
  expect control.version == 1
  expect control.source_index >= 0
  expect control.source_total_count > control.source_index
  expect or {
      control.language_tag == 0,
      control.language_tag == 3,
      control.language_tag == 128,
    }
  expect control.payload_offset > 0
  expect control.payload_length >= 0
  expect control.item_length == control.payload_offset + control.payload_length
  expect control.item_length > 0
  expect control.item_length <= max_aggregate_field_preimage_bytes
  expect bytearray.length(control.item_commitment) == 32
  expect blake2b_224_trace_v1.control_is_well_formed(control.hash_control)
  #"89"
    |> bytearray.concat(cbor.serialise(1))
    |> bytearray.concat(cbor.serialise(control.source_index))
    |> bytearray.concat(cbor.serialise(control.source_total_count))
    |> bytearray.concat(cbor.serialise(control.language_tag))
    |> bytearray.concat(cbor.serialise(control.payload_offset))
    |> bytearray.concat(cbor.serialise(control.payload_length))
    |> bytearray.concat(cbor.serialise(control.item_length))
    |> bytearray.concat(encode_definite_bytes(control.item_commitment))
    |> bytearray.concat(
        encode_definite_bytes(
          blake2b_224_trace_v1.encode_control_v1(control.hash_control),
        ),
      )
}

fn inline_source_hash_control_from_cbor(
  control_cbor: ByteArray,
) -> InlineSourceHashControlV1 {
  expect Some(data) = cbor.deserialise(control_cbor)
  expect
      [
        version_data, source_index_data, source_total_count_data,
        language_tag_data, payload_offset_data, payload_length_data,
        item_length_data, item_commitment_data, hash_control_data,
      ]
    = builtin.un_list_data(data)
  let control = InlineSourceHashControlV1 {
    version: builtin.un_i_data(version_data),
    source_index: builtin.un_i_data(source_index_data),
    source_total_count: builtin.un_i_data(source_total_count_data),
    language_tag: builtin.un_i_data(language_tag_data),
    payload_offset: builtin.un_i_data(payload_offset_data),
    payload_length: builtin.un_i_data(payload_length_data),
    item_length: builtin.un_i_data(item_length_data),
    item_commitment: builtin.un_b_data(item_commitment_data),
    hash_control: builtin.un_b_data(hash_control_data)
      |> blake2b_224_trace_v1.decode_control_v1,
  }
  expect encode_inline_source_hash_control_v1(control) == control_cbor
  control
}

pub fn encode_script_sources_pending_source_witness(
  control: ScriptSourcesControlV1,
  pending_source_cbor: ByteArray,
) -> ByteArray {
  expect control.stage == 0
  expect pending_source_cbor != #""
  let base = encode_script_sources_witness(
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
    control.context_cbor,
    control.resolved_input_count,
    control.resolved_inputs_accumulator,
    control.signer_count,
    control.signer_frontier_commitment,
    control.resolved_item_peaks,
    control.stage,
    control.source_count,
    control.source_peaks,
    control.redeemer_count,
    control.redeemer_peaks,
    control.replay_cursor,
    control.replay_accumulator,
    control.replay_remaining_schedule_hash,
    control.spend_index,
    control.purpose_count,
    control.purpose_peaks,
    control.output_cursor,
    control.output_count,
    control.output_peaks,
    control.output_total_count,
    control.receive_scan,
    control.source_total_count,
    control.redeemer_total_count,
    control.observer_scan,
    control.mint_fold,
    control.resolution_schedule_hash,
  )
  encode_definite_array_header(31)
    |> bytearray.concat(bytearray.slice(base, 2, bytearray.length(base) - 1))
    |> bytearray.concat(encode_definite_bytes(pending_source_cbor))
}

pub fn encode_script_sources_output_proof_witness(
  control: ScriptSourcesControlV1,
  output_proof: ledger_output_proof_v1.LedgerOutputProofControlV1,
) -> ByteArray {
  expect control.stage == 5
  let base = encode_script_sources_witness(
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
    control.context_cbor,
    control.resolved_input_count,
    control.resolved_inputs_accumulator,
    control.signer_count,
    control.signer_frontier_commitment,
    control.resolved_item_peaks,
    control.stage,
    control.source_count,
    control.source_peaks,
    control.redeemer_count,
    control.redeemer_peaks,
    control.replay_cursor,
    control.replay_accumulator,
    control.replay_remaining_schedule_hash,
    control.spend_index,
    control.purpose_count,
    control.purpose_peaks,
    control.output_cursor,
    control.output_count,
    control.output_peaks,
    control.output_total_count,
    control.receive_scan,
    control.source_total_count,
    control.redeemer_total_count,
    control.observer_scan,
    control.mint_fold,
    control.resolution_schedule_hash,
  )
  encode_definite_array_header(31)
    |> bytearray.concat(bytearray.slice(base, 2, bytearray.length(base) - 1))
    |> bytearray.concat(
        encode_definite_bytes(
          ledger_output_proof_v1.encode_control_v1(output_proof),
        ),
      )
}

pub fn encode_script_sources_redeemer_item_witness(
  control: ScriptSourcesControlV1,
  redeemer_item_control_hash: ByteArray,
) -> ByteArray {
  expect control.stage == 1
  expect bytearray.length(redeemer_item_control_hash) == 32
  let base = encode_script_sources_witness(
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
    control.context_cbor,
    control.resolved_input_count,
    control.resolved_inputs_accumulator,
    control.signer_count,
    control.signer_frontier_commitment,
    control.resolved_item_peaks,
    control.stage,
    control.source_count,
    control.source_peaks,
    control.redeemer_count,
    control.redeemer_peaks,
    control.replay_cursor,
    control.replay_accumulator,
    control.replay_remaining_schedule_hash,
    control.spend_index,
    control.purpose_count,
    control.purpose_peaks,
    control.output_cursor,
    control.output_count,
    control.output_peaks,
    control.output_total_count,
    control.receive_scan,
    control.source_total_count,
    control.redeemer_total_count,
    control.observer_scan,
    control.mint_fold,
    control.resolution_schedule_hash,
  )
  encode_definite_array_header(31)
    |> bytearray.concat(bytearray.slice(base, 2, bytearray.length(base) - 1))
    |> bytearray.concat(encode_definite_bytes(redeemer_item_control_hash))
}

pub fn encode_script_sources_discovery_witness(
  control: ScriptSourcesControlV1,
  stage: Int,
  discovery: ScriptDiscoveryControlV1,
) -> ByteArray {
  expect stage >= 8
  let base = encode_script_sources_witness(
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
    control.context_cbor,
    control.resolved_input_count,
    control.resolved_inputs_accumulator,
    control.signer_count,
    control.signer_frontier_commitment,
    control.resolved_item_peaks,
    stage,
    control.source_count,
    control.source_peaks,
    control.redeemer_count,
    control.redeemer_peaks,
    control.replay_cursor,
    control.replay_accumulator,
    control.replay_remaining_schedule_hash,
    control.spend_index,
    control.purpose_count,
    control.purpose_peaks,
    control.output_cursor,
    control.output_count,
    control.output_peaks,
    control.output_total_count,
    control.receive_scan,
    control.source_total_count,
    control.redeemer_total_count,
    control.observer_scan,
    control.mint_fold,
    control.resolution_schedule_hash,
  )
  encode_definite_array_header(31)
    |> bytearray.concat(bytearray.slice(base, 2, bytearray.length(base) - 1))
    |> bytearray.concat(
        encode_definite_bytes(encode_script_discovery_control(discovery)),
      )
}

fn script_discovery_control_is_well_formed(
  control: ScriptDiscoveryControlV1,
) -> Bool {
  and {
    control.purpose_cursor >= 0,
    control.source_cursor >= 0,
    control.redeemer_cursor >= 0,
    control.current_purpose_kind >= -1,
    control.current_purpose_kind <= 3,
    control.current_purpose_index >= -1,
    or {
      control.current_script_hash == #"",
      bytearray.length(control.current_script_hash) == 28,
    },
    control.matched_source_index >= -1,
    or {
      control.matched_language_tag == -1,
      control.matched_language_tag == 0,
      control.matched_language_tag == 3,
      control.matched_language_tag == 128,
    },
    or {
      control.matched_source_leaf == #"",
      bytearray.length(control.matched_source_leaf) == 32,
    },
    control.used_inline_bitmap >= 0,
    control.used_redeemer_bitmap >= 0,
    validation_merkle_v1.frontier_is_well_formed(
      control.execution_count,
      control.execution_peaks,
    ),
    bytearray.length(control.redeemer_item_control_hash) == 0 || bytearray.length(
      control.redeemer_item_control_hash,
    ) == 32,
  }
}

fn exact_script_sources_control(control: ScriptSourcesControlV1) -> ByteArray {
  if control.stage == 0 && control.pending_source_cbor != #"" {
    encode_script_sources_pending_source_witness(
      control,
      control.pending_source_cbor,
    )
  } else if control.stage == 1 && control.pending_source_cbor != #"" {
    encode_script_sources_redeemer_item_witness(
      control,
      control.pending_source_cbor,
    )
  } else if control.stage == 5 {
    when control.output_proof is {
      None ->
        encode_script_sources_witness(
          control.compact_cbor,
          control.witness_set_compact_cbor,
          control.field_preimage_lengths_cbor,
          control.context_cbor,
          control.resolved_input_count,
          control.resolved_inputs_accumulator,
          control.signer_count,
          control.signer_frontier_commitment,
          control.resolved_item_peaks,
          control.stage,
          control.source_count,
          control.source_peaks,
          control.redeemer_count,
          control.redeemer_peaks,
          control.replay_cursor,
          control.replay_accumulator,
          control.replay_remaining_schedule_hash,
          control.spend_index,
          control.purpose_count,
          control.purpose_peaks,
          control.output_cursor,
          control.output_count,
          control.output_peaks,
          control.output_total_count,
          control.receive_scan,
          control.source_total_count,
          control.redeemer_total_count,
          control.observer_scan,
          control.mint_fold,
          control.resolution_schedule_hash,
        )
      Some(output_proof) ->
        encode_script_sources_output_proof_witness(control, output_proof)
    }
  } else if control.stage < 8 {
    expect control.output_proof == None
    encode_script_sources_witness(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      control.context_cbor,
      control.resolved_input_count,
      control.resolved_inputs_accumulator,
      control.signer_count,
      control.signer_frontier_commitment,
      control.resolved_item_peaks,
      control.stage,
      control.source_count,
      control.source_peaks,
      control.redeemer_count,
      control.redeemer_peaks,
      control.replay_cursor,
      control.replay_accumulator,
      control.replay_remaining_schedule_hash,
      control.spend_index,
      control.purpose_count,
      control.purpose_peaks,
      control.output_cursor,
      control.output_count,
      control.output_peaks,
      control.output_total_count,
      control.receive_scan,
      control.source_total_count,
      control.redeemer_total_count,
      control.observer_scan,
      control.mint_fold,
      control.resolution_schedule_hash,
    )
  } else {
    expect control.output_proof == None
    encode_script_sources_discovery_witness(
      control,
      control.stage,
      control.discovery,
    )
  }
}

fn script_sources_stage_zero_control_from_witness(
  work_witness_cbor: ByteArray,
) -> ScriptSourcesControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let items = builtin.un_list_data(data)
  if list.length(items) == 30 {
    script_sources_control_from_data_items(
      items,
      empty_script_discovery_control(),
      None,
    )
  } else {
    expect list.length(items) == 31
    let Pair(base_items, extension_data) = split_last_data(items)
    let base_control = script_sources_control_from_data_items(
      base_items,
      empty_script_discovery_control(),
      None,
    )
    expect base_control.stage == 0
    ScriptSourcesControlV1 {
      ..base_control,
      pending_source_cbor: builtin.un_b_data(extension_data),
    }
  }
}

fn script_sources_stage_zero_work_witness_has_canonical_shape(
  work_witness_cbor: ByteArray,
  pending_source_cbor: ByteArray,
) -> Bool {
  let Pair(offset, item_count) = decode_definite_array_header_at(
    work_witness_cbor,
    0,
  )
  let expected_item_count = if pending_source_cbor == #"" {
    30
  } else {
    31
  }
  and {
    offset == 2,
    item_count == expected_item_count,
    if pending_source_cbor == #"" {
      True
    } else {
      let encoded_pending = encode_definite_bytes(pending_source_cbor)
      let pending_offset = bytearray.length(work_witness_cbor) - bytearray.length(
        encoded_pending,
      )
      and {
        pending_offset >= offset,
        builtin.slice_bytearray(
          pending_offset,
          bytearray.length(encoded_pending),
          work_witness_cbor,
        ) == encoded_pending,
      }
    },
  }
}

fn script_sources_stage_zero_prefix_before_stage(
  control: ScriptSourcesControlV1,
  item_count: Int,
) -> ByteArray {
  encode_definite_array_header(item_count)
    |> bytearray.concat(encode_definite_bytes(control.compact_cbor))
    |> bytearray.concat(encode_definite_bytes(control.witness_set_compact_cbor))
    |> bytearray.concat(
        encode_definite_bytes(control.field_preimage_lengths_cbor),
      )
    |> bytearray.concat(encode_definite_bytes(control.context_cbor))
    |> bytearray.concat(cbor.serialise(control.resolved_input_count))
    |> bytearray.concat(
        encode_definite_bytes(control.resolved_inputs_accumulator),
      )
    |> bytearray.concat(cbor.serialise(control.signer_count))
    |> bytearray.concat(
        encode_definite_bytes(control.signer_frontier_commitment),
      )
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.resolved_item_peaks),
      )
}

fn script_sources_stage_zero_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let pending_is_bound = if control.pending_source_cbor == #"" {
    True
  } else {
    let pending = inline_source_hash_control_from_cbor(
      control.pending_source_cbor,
    )
    and {
      pending.source_index == control.source_count,
      pending.source_total_count == control.source_total_count,
    }
  }
  and {
    control.stage == 0,
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    control.resolved_input_count >= 0,
    bytearray.length(control.resolved_inputs_accumulator) == 32,
    control.signer_count >= 0,
    bytearray.length(control.signer_frontier_commitment) == 32,
    control.resolved_item_peaks == [],
    control.replay_cursor == 0,
    bytearray.length(control.replay_accumulator) == 32,
    bytearray.length(control.replay_remaining_schedule_hash) == 32,
    bytearray.length(control.resolution_schedule_hash) == 32,
    control.spend_index == 0,
    validation_merkle_v1.frontier_is_well_formed(
      control.source_count,
      control.source_peaks,
    ),
    control.source_total_count >= control.source_count,
    control.source_total_count <= max_tx_size_derived_collection_item_count,
    control.redeemer_count == 0,
    control.redeemer_peaks == [],
    control.redeemer_total_count == 0,
    control.purpose_count == 0,
    control.purpose_peaks == [],
    control.output_cursor == 0,
    control.output_count == 0,
    control.output_peaks == [],
    control.output_total_count == 0,
    control.receive_scan == empty_receive_purpose_scan_control(),
    control.observer_scan == empty_observer_purpose_scan_control(),
    control.mint_fold == empty_mint_fold_control(),
    control.discovery == empty_script_discovery_control(),
    control.output_proof == None,
    pending_is_bound,
    script_sources_stage_zero_work_witness_has_canonical_shape(
      witness.work_witness_cbor,
      control.pending_source_cbor,
    ),
  }
}

fn script_sources_stage_zero_successor_work_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  next_work_witness_cbor: ByteArray,
) -> Bool {
  and {
    witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      pre.program_counter + 1,
      next_work_witness_cbor,
    ),
  }
}

fn script_sources_stage_zero_finish_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let prefix = script_sources_stage_zero_prefix_before_stage(control, 30)
  let stage_offset = bytearray.length(prefix)
  let work_length = bytearray.length(witness.work_witness_cbor)
  expect
    builtin.slice_bytearray(0, stage_offset, witness.work_witness_cbor) == prefix
  expect
    builtin.slice_bytearray(stage_offset, 1, witness.work_witness_cbor) == #"00"
  let next_work_witness_cbor = prefix
    |> bytearray.concat(#"01")
    |> bytearray.concat(
        builtin.slice_bytearray(
          stage_offset + 1,
          work_length - stage_offset - 1,
          witness.work_witness_cbor,
        ),
      )
  script_sources_stage_zero_successor_work_is_exact(
    pre,
    witness,
    next_work_witness_cbor,
  )
}

fn script_sources_stage_zero_begin_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  source_total_count: Int,
  pending_source_cbor: ByteArray,
) -> Bool {
  let encoded_observer_scan = encode_observer_purpose_scan_control(
    control.observer_scan,
  )
  let old_suffix = cbor.serialise(control.source_total_count)
    |> bytearray.concat(cbor.serialise(control.redeemer_total_count))
    |> bytearray.concat(encoded_observer_scan)
    |> bytearray.concat(encode_mint_fold_control(control.mint_fold))
    |> bytearray.concat(encode_definite_bytes(control.resolution_schedule_hash))
  let work_length = bytearray.length(witness.work_witness_cbor)
  let suffix_offset = work_length - bytearray.length(old_suffix)
  expect suffix_offset >= 2
  expect
    builtin.slice_bytearray(
      suffix_offset,
      bytearray.length(old_suffix),
      witness.work_witness_cbor,
    ) == old_suffix
  let next_work_witness_cbor = encode_definite_array_header(31)
    |> bytearray.concat(
        builtin.slice_bytearray(2, suffix_offset - 2, witness.work_witness_cbor),
      )
    |> bytearray.concat(cbor.serialise(source_total_count))
    |> bytearray.concat(cbor.serialise(control.redeemer_total_count))
    |> bytearray.concat(encoded_observer_scan)
    |> bytearray.concat(encode_mint_fold_control(control.mint_fold))
    |> bytearray.concat(encode_definite_bytes(control.resolution_schedule_hash))
    |> bytearray.concat(encode_definite_bytes(pending_source_cbor))
  script_sources_stage_zero_successor_work_is_exact(
    pre,
    witness,
    next_work_witness_cbor,
  )
}

fn script_sources_stage_zero_pending_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  pending_source_cbor: ByteArray,
) -> Bool {
  let old_pending = encode_definite_bytes(control.pending_source_cbor)
  let work_length = bytearray.length(witness.work_witness_cbor)
  let pending_offset = work_length - bytearray.length(old_pending)
  expect pending_offset >= 2
  expect
    builtin.slice_bytearray(
      pending_offset,
      bytearray.length(old_pending),
      witness.work_witness_cbor,
    ) == old_pending
  let next_work_witness_cbor = builtin.slice_bytearray(
    0,
    pending_offset,
    witness.work_witness_cbor,
  )
    |> bytearray.concat(encode_definite_bytes(pending_source_cbor))
  script_sources_stage_zero_successor_work_is_exact(
    pre,
    witness,
    next_work_witness_cbor,
  )
}

fn script_sources_stage_zero_terminal_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  source_count: Int,
  source_peaks: List<validation_merkle_v1.FrontierPeak>,
) -> Bool {
  let prefix = script_sources_stage_zero_prefix_before_stage(control, 31)
    |> bytearray.concat(#"00")
  let source_offset = bytearray.length(prefix)
  let old_source_fields = cbor.serialise(control.source_count)
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.source_peaks),
      )
  let after_source_offset = source_offset + bytearray.length(old_source_fields)
  let old_pending = encode_definite_bytes(control.pending_source_cbor)
  let work_length = bytearray.length(witness.work_witness_cbor)
  let pending_offset = work_length - bytearray.length(old_pending)
  expect pending_offset >= after_source_offset
  expect
    builtin.slice_bytearray(0, source_offset, witness.work_witness_cbor) == prefix
  expect
    builtin.slice_bytearray(
      source_offset,
      bytearray.length(old_source_fields),
      witness.work_witness_cbor,
    ) == old_source_fields
  expect
    builtin.slice_bytearray(
      pending_offset,
      bytearray.length(old_pending),
      witness.work_witness_cbor,
    ) == old_pending
  let next_work_witness_cbor = encode_definite_array_header(30)
    |> bytearray.concat(builtin.slice_bytearray(2, source_offset - 2, prefix))
    |> bytearray.concat(cbor.serialise(source_count))
    |> bytearray.concat(validation_merkle_v1.encode_frontier(source_peaks))
    |> bytearray.concat(
        builtin.slice_bytearray(
          after_source_offset,
          pending_offset - after_source_offset,
          witness.work_witness_cbor,
        ),
      )
  script_sources_stage_zero_successor_work_is_exact(
    pre,
    witness,
    next_work_witness_cbor,
  )
}

fn script_sources_control_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  next_control: ScriptSourcesControlV1,
) -> Bool {
  and {
    witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      pre.program_counter + 1,
      exact_script_sources_control(next_control),
    ),
  }
}

pub fn inline_source_hash_block_v1(
  pending: InlineSourceHashControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Option<ByteArray> {
  let hash_control = pending.hash_control
  if hash_control.stage != blake2b_224_trace_v1.stage_ready {
    None
  } else {
    let remaining = hash_control.total_length - hash_control.cursor
    let block_length = if remaining < blake2b_224_trace_v1.block_bytes {
      remaining
    } else {
      blake2b_224_trace_v1.block_bytes
    }
    let content_length = if hash_control.cursor == 0 {
      block_length - 1
    } else {
      block_length
    }
    let item_cursor = if hash_control.cursor == 0 {
      pending.payload_offset
    } else {
      pending.payload_offset + hash_control.cursor - 1
    }
    let expected_chunk_index = if content_length == 0 {
      0
    } else {
      item_cursor / bounded_item_v1.chunk_bytes
    }
    let offset = if content_length == 0 {
      pending.payload_offset
    } else {
      item_cursor - expected_chunk_index * bounded_item_v1.chunk_bytes
    }
    if !and {
      hash_control.total_length == pending.payload_length + 1,
      chunk_proof.field_index == 6,
      chunk_proof.item_index == pending.source_index,
      chunk_proof.total_length == pending.item_length,
      chunk_proof.chunk_index == expected_chunk_index,
      bounded_item_v1.verify_chunk(pending.item_commitment, chunk_proof),
      offset >= 0,
      offset <= bytearray.length(chunk_proof.chunk),
    } {
      None
    } else {
      let available = bytearray.length(chunk_proof.chunk) - offset
      let crosses_chunk = content_length > available
      let authenticated_window = if crosses_chunk {
        when next_chunk_proof is {
          None -> None
          Some(next) ->
            if and {
              next.field_index == 6,
              next.item_index == pending.source_index,
              next.total_length == pending.item_length,
              next.chunk_index == expected_chunk_index + 1,
              bounded_item_v1.verify_chunk(pending.item_commitment, next),
            } {
              Some(bytearray.concat(chunk_proof.chunk, next.chunk))
            } else {
              None
            }
        }
      } else if next_chunk_proof == None {
        Some(chunk_proof.chunk)
      } else {
        None
      }
      when authenticated_window is {
        None -> None
        Some(window) -> {
          let content = builtin.slice_bytearray(offset, content_length, window)
          let block = if hash_control.cursor == 0 {
            #""
              |> bytearray.push(pending.language_tag)
              |> bytearray.concat(content)
          } else {
            content
          }
          if bytearray.length(block) == block_length {
            Some(block)
          } else {
            None
          }
        }
      }
    }
  }
}

pub fn verify_script_sources_stage_zero_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_stage_zero_control_from_witness(
    witness.work_witness_cbor,
  )
  let witness_set = decode_native_tx_witness_set_compact(
    control.witness_set_compact_cbor,
  )
  let script_commitment = witness_set.script_tx_wits_hash
  let empty_script_commitment = bounded_collection_v1.from_items(6, [])
  let source_scan_is_complete = if script_commitment == empty_script_commitment {
    and {
      control.source_count == 0,
      control.source_total_count == 0,
      control.source_peaks == [],
    }
  } else {
    and {
      control.source_total_count > 0,
      control.source_count == control.source_total_count,
    }
  }
  and {
    script_sources_stage_zero_control_is_bound(pre, witness, control),
    control.pending_source_cbor == #"",
    source_scan_is_complete,
    script_sources_stage_zero_finish_successor_is_exact(pre, witness, control),
  }
}

pub fn verify_script_sources_stage_zero_begin_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  collection_proof: bounded_collection_v1.ItemProofV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
) -> Bool {
  let control = script_sources_stage_zero_control_from_witness(
    witness.work_witness_cbor,
  )
  let witness_set = decode_native_tx_witness_set_compact(
    control.witness_set_compact_cbor,
  )
  let active_total_count = if control.source_total_count == 0 {
    collection_proof.item_count
  } else {
    control.source_total_count
  }
  let header = native_script_scan_v1.versioned_script_header_v1(
    chunk_proof.chunk,
    collection_proof.item_length,
  )
  and {
    script_sources_stage_zero_control_is_bound(pre, witness, control),
    control.pending_source_cbor == #"",
    active_total_count > 0,
    active_total_count <= max_tx_size_derived_collection_item_count,
    collection_proof.item_length > 0,
    collection_proof.item_length <= max_aggregate_field_preimage_bytes,
    collection_proof.field_index == 6,
    collection_proof.item_count == active_total_count,
    collection_proof.item_index == control.source_count,
    chunk_proof.field_index == 6,
    chunk_proof.item_index == collection_proof.item_index,
    chunk_proof.total_length == collection_proof.item_length,
    chunk_proof.chunk_index == 0,
    bounded_collection_v1.verify_item(
      witness_set.script_tx_wits_hash,
      collection_proof,
    ),
    bounded_item_v1.verify_chunk(collection_proof.item_commitment, chunk_proof),
    when header is {
      None ->
        rejected_successor_is_exact(
          pre,
          witness.claimed_successor,
          reject_invalid_field_type,
        )
      Some(item_header) -> {
        let pending_source_cbor = encode_inline_source_hash_control_v1(
          InlineSourceHashControlV1 {
            version: 1,
            source_index: control.source_count,
            source_total_count: active_total_count,
            language_tag: item_header.language_tag,
            payload_offset: item_header.payload_offset,
            payload_length: item_header.payload_length,
            item_length: collection_proof.item_length,
            item_commitment: collection_proof.item_commitment,
            hash_control: blake2b_224_trace_v1.initial_control_v1(
              item_header.payload_length + 1,
            ),
          },
        )
        script_sources_stage_zero_begin_successor_is_exact(
          pre,
          witness,
          control,
          active_total_count,
          pending_source_cbor,
        )
      }
    },
  }
}

pub fn verify_script_sources_stage_zero_hash_block_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  let control = script_sources_stage_zero_control_from_witness(
    witness.work_witness_cbor,
  )
  expect control.pending_source_cbor != #""
  let pending = inline_source_hash_control_from_cbor(
    control.pending_source_cbor,
  )
  when inline_source_hash_block_v1(pending, chunk_proof, next_chunk_proof) is {
    None -> False
    Some(block) -> {
      expect Some(next_hash_control) = blake2b_224_trace_v1.step_v1(
        pending.hash_control,
        Some(block),
      )
      and {
        script_sources_stage_zero_control_is_bound(pre, witness, control),
        script_sources_stage_zero_pending_successor_is_exact(
          pre,
          witness,
          control,
          encode_inline_source_hash_control_v1(
            InlineSourceHashControlV1 {
              ..pending,
              hash_control: next_hash_control,
            },
          ),
        ),
      }
    }
  }
}

pub fn verify_script_sources_stage_zero_hash_advance_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_stage_zero_control_from_witness(
    witness.work_witness_cbor,
  )
  expect control.pending_source_cbor != #""
  let pending = inline_source_hash_control_from_cbor(
    control.pending_source_cbor,
  )
  expect pending.hash_control.stage != blake2b_224_trace_v1.stage_ready
  expect pending.hash_control.stage != blake2b_224_trace_v1.stage_terminal
  expect Some(next_hash_control) = blake2b_224_trace_v1.step_v1(
    pending.hash_control,
    None,
  )
  and {
    script_sources_stage_zero_control_is_bound(pre, witness, control),
    script_sources_stage_zero_pending_successor_is_exact(
      pre,
      witness,
      control,
      encode_inline_source_hash_control_v1(
        InlineSourceHashControlV1 { ..pending, hash_control: next_hash_control },
      ),
    ),
  }
}

pub fn verify_script_sources_stage_zero_hash_terminal_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_stage_zero_control_from_witness(
    witness.work_witness_cbor,
  )
  expect control.pending_source_cbor != #""
  let pending = inline_source_hash_control_from_cbor(
    control.pending_source_cbor,
  )
  expect Some(script_hash) = blake2b_224_trace_v1.digest_v1(
    pending.hash_control,
  )
  let next_source_count = control.source_count + 1
  let next_source_peaks = validation_merkle_v1.append_leaf(
    control.source_count,
    control.source_peaks,
    script_proof_v1.inline_source_leaf_hash(
      control.source_count,
      pending.language_tag,
      script_hash,
      pending.item_length,
      pending.item_commitment,
    ),
  )
  and {
    script_sources_stage_zero_control_is_bound(pre, witness, control),
    script_sources_stage_zero_terminal_successor_is_exact(
      pre,
      witness,
      control,
      next_source_count,
      next_source_peaks,
    ),
  }
}

fn script_sources_stage_zero(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let Pair(_, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let script_commitment = witness_set.script_tx_wits_hash
  let empty_script_commitment = bounded_collection_v1.from_items(6, [])
  let common_control_is_initial = and {
    control.redeemer_count == 0,
    control.redeemer_total_count == 0,
    control.redeemer_peaks == [],
    control.resolved_item_peaks == [],
    control.replay_cursor == 0,
    control.spend_index == 0,
    control.purpose_count == 0,
    control.output_cursor == 0,
    control.output_count == 0,
    control.receive_scan == empty_receive_purpose_scan_control(),
  }
  if control.pending_source_cbor != #"" {
    let pending = inline_source_hash_control_from_cbor(
      control.pending_source_cbor,
    )
    let hash_control = pending.hash_control
    if hash_control.stage == blake2b_224_trace_v1.stage_ready {
      expect ScriptSourceHashBlockWitness { chunk_proof, next_chunk_proof } = auxiliary
      when
        inline_source_hash_block_v1(pending, chunk_proof, next_chunk_proof)
      is {
        None -> False
        Some(block) -> {
          expect Some(next_hash_control) = blake2b_224_trace_v1.step_v1(
            hash_control,
            Some(block),
          )
          and {
            common_control_is_initial,
            script_sources_control_successor_is_exact(
              pre,
              witness,
              ScriptSourcesControlV1 {
                ..control,
                pending_source_cbor: encode_inline_source_hash_control_v1(
                  InlineSourceHashControlV1 {
                    ..pending,
                    hash_control: next_hash_control,
                  },
                ),
              },
            ),
          }
        }
      }
    } else if hash_control.stage == blake2b_224_trace_v1.stage_terminal {
      expect Some(script_hash) = blake2b_224_trace_v1.digest_v1(hash_control)
      and {
        common_control_is_initial,
        auxiliary == NoAuxiliaryWitness,
        script_sources_control_successor_is_exact(
          pre,
          witness,
          ScriptSourcesControlV1 {
            ..control,
            source_count: control.source_count + 1,
            source_peaks: validation_merkle_v1.append_leaf(
              control.source_count,
              control.source_peaks,
              script_proof_v1.inline_source_leaf_hash(
                control.source_count,
                pending.language_tag,
                script_hash,
                pending.item_length,
                pending.item_commitment,
              ),
            ),
            pending_source_cbor: #"",
          },
        ),
      }
    } else {
      expect Some(next_hash_control) = blake2b_224_trace_v1.step_v1(
        hash_control,
        None,
      )
      and {
        common_control_is_initial,
        auxiliary == NoAuxiliaryWitness,
        script_sources_control_successor_is_exact(
          pre,
          witness,
          ScriptSourcesControlV1 {
            ..control,
            pending_source_cbor: encode_inline_source_hash_control_v1(
              InlineSourceHashControlV1 {
                ..pending,
                hash_control: next_hash_control,
              },
            ),
          },
        ),
      }
    }
  } else if script_commitment == empty_script_commitment {
    and {
      common_control_is_initial,
      auxiliary == NoAuxiliaryWitness,
      control.source_count == 0,
      control.source_total_count == 0,
      control.source_peaks == [],
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 { ..control, stage: 1 },
      ),
    }
  } else if and {
    control.source_total_count > 0,
    control.source_count == control.source_total_count,
  } {
    and {
      common_control_is_initial,
      auxiliary == NoAuxiliaryWitness,
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 { ..control, stage: 1 },
      ),
    }
  } else {
    expect TransactionFieldChunkWitness { collection_proof, chunk_proof } = auxiliary
    let active_total_count = if control.source_total_count == 0 {
      collection_proof.item_count
    } else {
      control.source_total_count
    }
    let header = native_script_scan_v1.versioned_script_header_v1(
      chunk_proof.chunk,
      collection_proof.item_length,
    )
    and {
      common_control_is_initial,
      active_total_count > 0,
      active_total_count <= max_tx_size_derived_collection_item_count,
      collection_proof.item_length > 0,
      collection_proof.item_length <= max_aggregate_field_preimage_bytes,
      collection_proof.field_index == 6,
      collection_proof.item_count == active_total_count,
      collection_proof.item_index == control.source_count,
      chunk_proof.field_index == 6,
      chunk_proof.item_index == collection_proof.item_index,
      chunk_proof.total_length == collection_proof.item_length,
      chunk_proof.chunk_index == 0,
      bounded_collection_v1.verify_item(script_commitment, collection_proof),
      bounded_item_v1.verify_chunk(
        collection_proof.item_commitment,
        chunk_proof,
      ),
      when header is {
        None ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          )
        Some(item_header) ->
          script_sources_control_successor_is_exact(
            pre,
            witness,
            ScriptSourcesControlV1 {
              ..control,
              source_total_count: active_total_count,
              pending_source_cbor: encode_inline_source_hash_control_v1(
                InlineSourceHashControlV1 {
                  version: 1,
                  source_index: control.source_count,
                  source_total_count: active_total_count,
                  language_tag: item_header.language_tag,
                  payload_offset: item_header.payload_offset,
                  payload_length: item_header.payload_length,
                  item_length: collection_proof.item_length,
                  item_commitment: collection_proof.item_commitment,
                  hash_control: blake2b_224_trace_v1.initial_control_v1(
                    item_header.payload_length + 1,
                  ),
                },
              ),
            },
          )
      },
    }
  }
}

fn script_sources_stage_one_raw_common_control_is_initial(
  resolved_item_peaks_data: Data,
  source_count_data: Data,
  replay_cursor_data: Data,
  spend_index_data: Data,
  purpose_count_data: Data,
  output_cursor_data: Data,
  output_count_data: Data,
  receive_scan_data: Data,
  source_total_count_data: Data,
) -> Bool {
  expect
      [
        receive_source_count_data, receive_source_peaks_data, receive_count_data,
        receive_previous_hash_data, receive_candidate_hash_data,
        receive_descriptor_peaks_data,
      ]
    = builtin.un_list_data(receive_scan_data)
  and {
    builtin.un_i_data(source_total_count_data) == builtin.un_i_data(
      source_count_data,
    ),
    builtin.un_list_data(resolved_item_peaks_data) == [],
    builtin.un_i_data(replay_cursor_data) == 0,
    builtin.un_i_data(spend_index_data) == 0,
    builtin.un_i_data(purpose_count_data) == 0,
    builtin.un_i_data(output_cursor_data) == 0,
    builtin.un_i_data(output_count_data) == 0,
    builtin.un_i_data(receive_source_count_data) == 0,
    builtin.un_list_data(receive_source_peaks_data) == [],
    builtin.un_i_data(receive_count_data) == 0,
    builtin.un_b_data(receive_previous_hash_data) == #"",
    builtin.un_b_data(receive_candidate_hash_data) == #"",
    builtin.un_list_data(receive_descriptor_peaks_data) == [],
  }
}

pub fn verify_script_sources_stage_one_finish_raw_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  expect Some(control_data) = cbor.deserialise(witness.work_witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data,
        resolved_input_count_data, resolved_inputs_accumulator_data,
        signer_count_data, signer_frontier_commitment_data,
        resolved_item_peaks_data, stage_data, source_count_data, _,
        redeemer_count_data, redeemer_peaks_data, replay_cursor_data, _, _,
        spend_index_data, purpose_count_data, _, output_cursor_data,
        output_count_data, _, _, receive_scan_data, source_total_count_data,
        redeemer_total_count_data, _, _, resolution_schedule_hash_data,
      ]
    = builtin.un_list_data(control_data)
  let compact_cbor = builtin.un_b_data(compact_cbor_data)
  let witness_set_compact_cbor = builtin.un_b_data(
    witness_set_compact_cbor_data,
  )
  let field_preimage_lengths_cbor = builtin.un_b_data(
    field_preimage_lengths_cbor_data,
  )
  let witness_set = decode_native_tx_witness_set_compact(
    witness_set_compact_cbor,
  )
  let redeemer_count = builtin.un_i_data(redeemer_count_data)
  let redeemer_peaks = decode_frontier_peaks(redeemer_peaks_data)
  let redeemer_total_count = builtin.un_i_data(redeemer_total_count_data)
  let redeemer_commitment = witness_set.redeemer_tx_wits_hash
  let empty_redeemer_commitment = bounded_collection_v1.from_items(8, [])
  let scan_is_complete = if redeemer_commitment == empty_redeemer_commitment {
    and {
      redeemer_count == 0,
      redeemer_total_count == 0,
      redeemer_peaks == [],
    }
  } else {
    and {
      redeemer_total_count > 0,
      redeemer_count == redeemer_total_count,
      validation_merkle_v1.frontier_is_well_formed(
        redeemer_count,
        redeemer_peaks,
      ),
    }
  }
  let stage_prefix = encode_definite_array_header(30)
    |> bytearray.concat(cbor.serialise(compact_cbor_data))
    |> bytearray.concat(cbor.serialise(witness_set_compact_cbor_data))
    |> bytearray.concat(cbor.serialise(field_preimage_lengths_cbor_data))
    |> bytearray.concat(cbor.serialise(context_cbor_data))
    |> bytearray.concat(cbor.serialise(resolved_input_count_data))
    |> bytearray.concat(cbor.serialise(resolved_inputs_accumulator_data))
    |> bytearray.concat(cbor.serialise(signer_count_data))
    |> bytearray.concat(cbor.serialise(signer_frontier_commitment_data))
    |> bytearray.concat(cbor.serialise(resolved_item_peaks_data))
  let stage_offset = bytearray.length(stage_prefix)
  let next_work_witness_cbor = stage_prefix
    |> bytearray.concat(#"02")
    |> bytearray.concat(
        builtin.slice_bytearray(
          stage_offset + 1,
          bytearray.length(witness.work_witness_cbor) - stage_offset - 1,
          witness.work_witness_cbor,
        ),
      )
  and {
    cbor.serialise(control_data) == witness.work_witness_cbor,
    builtin.un_i_data(stage_data) == 1,
    native_tx_proof_commitment_v1(
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    script_sources_stage_one_raw_common_control_is_initial(
      resolved_item_peaks_data,
      source_count_data,
      replay_cursor_data,
      spend_index_data,
      purpose_count_data,
      output_cursor_data,
      output_count_data,
      receive_scan_data,
      source_total_count_data,
    ),
    bytearray.length(redeemer_commitment) == 32,
    bytearray.length(builtin.un_b_data(resolution_schedule_hash_data)) == 32,
    redeemer_total_count <= max_tx_size_derived_collection_item_count,
    scan_is_complete,
    builtin.slice_bytearray(0, stage_offset, witness.work_witness_cbor) == stage_prefix,
    builtin.slice_bytearray(stage_offset, 1, witness.work_witness_cbor) == #"01",
    script_sources_stage_zero_successor_work_is_exact(
      pre,
      witness,
      next_work_witness_cbor,
    ),
  }
}

fn script_sources_stage_one(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let Pair(_, witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let redeemer_commitment = witness_set.redeemer_tx_wits_hash
  let empty_redeemer_commitment = bounded_collection_v1.from_items(8, [])
  let common_control_is_initial = and {
    control.source_total_count == control.source_count,
    control.resolved_item_peaks == [],
    control.replay_cursor == 0,
    control.spend_index == 0,
    control.purpose_count == 0,
    control.output_cursor == 0,
    control.output_count == 0,
    control.receive_scan == empty_receive_purpose_scan_control(),
  }
  if redeemer_commitment == empty_redeemer_commitment {
    and {
      common_control_is_initial,
      auxiliary == NoAuxiliaryWitness,
      control.pending_source_cbor == #"",
      control.redeemer_count == 0,
      control.redeemer_total_count == 0,
      control.redeemer_peaks == [],
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 { ..control, stage: 2 },
      ),
    }
  } else if and {
    control.pending_source_cbor == #"",
    control.redeemer_total_count > 0,
    control.redeemer_count == control.redeemer_total_count,
  } {
    and {
      common_control_is_initial,
      auxiliary == NoAuxiliaryWitness,
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 { ..control, stage: 2 },
      ),
    }
  } else if control.pending_source_cbor == #"" {
    expect TransactionRedeemerItemBeginWitness { collection_proof } = auxiliary
    let active_total_count = if control.redeemer_total_count == 0 {
      collection_proof.item_count
    } else {
      control.redeemer_total_count
    }
    let item_control = redeemer_item_proof_v1.initial_control_v1(
      redeemer_item_proof_v1.mode_data,
      collection_proof.item_index,
      active_total_count,
      collection_proof.item_length,
      collection_proof.item_commitment,
      -1,
      -1,
    )
    and {
      common_control_is_initial,
      active_total_count > 0,
      active_total_count <= max_tx_size_derived_collection_item_count,
      collection_proof.field_index == 8,
      collection_proof.item_count == active_total_count,
      collection_proof.item_index == control.redeemer_count,
      bounded_collection_v1.verify_item(redeemer_commitment, collection_proof),
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 {
          ..control,
          redeemer_total_count: active_total_count,
          pending_source_cbor: redeemer_item_proof_v1.hash_control_v1(
            item_control,
          ),
        },
      ),
    }
  } else {
    expect
        RedeemerItemStepWitness {
          redeemer_control: None,
          control: item_control,
          witness: item_witness,
        }
      = auxiliary
    let current_matches = and {
      common_control_is_initial,
      redeemer_item_proof_v1.control_is_well_formed(item_control),
      item_control.mode == redeemer_item_proof_v1.mode_data,
      item_control.item_index == control.redeemer_count,
      item_control.item_count == control.redeemer_total_count,
      redeemer_item_proof_v1.hash_control_v1(item_control) == control.pending_source_cbor,
    }
    when redeemer_item_proof_v1.step_v1(item_control, item_witness) is {
      None -> False
      Some(redeemer_item_proof_v1.RedeemerItemProofInvalid) -> and {
          current_matches,
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          ),
        }
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced { control: next }) ->
        if next.stage == redeemer_item_proof_v1.stage_terminal {
          and {
            current_matches,
            redeemer_item_proof_v1.finalize_v1(next) != None,
            script_sources_control_successor_is_exact(
              pre,
              witness,
              ScriptSourcesControlV1 {
                ..control,
                redeemer_count: control.redeemer_count + 1,
                redeemer_peaks: validation_merkle_v1.append_leaf(
                  control.redeemer_count,
                  control.redeemer_peaks,
                  script_proof_v1.redeemer_item_leaf_hash(
                    control.redeemer_count,
                    next.item_commitment,
                  ),
                ),
                pending_source_cbor: #"",
              },
            ),
          }
        } else {
          and {
            current_matches,
            script_sources_control_successor_is_exact(
              pre,
              witness,
              ScriptSourcesControlV1 {
                ..control,
                pending_source_cbor: redeemer_item_proof_v1.hash_control_v1(
                  next,
                ),
              },
            ),
          }
        }
    }
  }
}

fn script_sources_stage_two(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  and {
    auxiliary == NoAuxiliaryWitness,
    control.resolved_input_count > 0,
    control.replay_cursor == 0,
    control.resolved_item_peaks == [],
    control.replay_accumulator == initial_resolution_accumulator(),
    control.replay_remaining_schedule_hash == empty_resolution_schedule_hash(),
    control.spend_index == 0,
    control.purpose_count == 0,
    control.purpose_peaks == [],
    control.resolution_schedule_hash != empty_resolution_schedule_hash(),
    script_sources_control_successor_is_exact(
      pre,
      witness,
      ScriptSourcesControlV1 {
        ..control,
        stage: 3,
        replay_remaining_schedule_hash: control.resolution_schedule_hash,
      },
    ),
  }
}

fn replay_source_frontier(
  control: ScriptSourcesControlV1,
  source_kind: Int,
  key: ByteArray,
  descriptor: ledger_output_commitment_v1.LedgerOutputCommitmentV1,
) -> validation_merkle_v1.BuiltFrontier {
  if and {
    source_kind == 1,
    descriptor.reference_script_language != -1,
  } {
    validation_merkle_v1.BuiltFrontier {
      count: control.source_count + 1,
      peaks: validation_merkle_v1.append_leaf(
        control.source_count,
        control.source_peaks,
        script_proof_v1.reference_source_leaf_hash(
          key,
          descriptor.reference_script_language,
          descriptor.reference_script_hash,
          descriptor.reference_script_total_length,
          descriptor.reference_script_item_commitment,
        ),
      ),
    }
  } else {
    validation_merkle_v1.BuiltFrontier {
      count: control.source_count,
      peaks: control.source_peaks,
    }
  }
}

fn replay_purpose_frontier(
  control: ScriptSourcesControlV1,
  source_kind: Int,
  key: ByteArray,
  descriptor: ledger_output_commitment_v1.LedgerOutputCommitmentV1,
) -> validation_merkle_v1.BuiltFrontier {
  expect Some(address) = ledger_output_v1.decode_canonical_address_bytes(
    descriptor.address,
  )
  if source_kind == 0 {
    when address.payment_credential is {
      ScriptCredential(script_hash) ->
        validation_merkle_v1.BuiltFrontier {
          count: control.purpose_count + 1,
          peaks: validation_merkle_v1.append_leaf(
            control.purpose_count,
            control.purpose_peaks,
            script_proof_v1.purpose_leaf_hash(
              0,
              control.spend_index,
              script_hash,
              key,
            ),
          ),
        }
      PubKeyCredential(_) ->
        validation_merkle_v1.BuiltFrontier {
          count: control.purpose_count,
          peaks: control.purpose_peaks,
        }
    }
  } else {
    validation_merkle_v1.BuiltFrontier {
      count: control.purpose_count,
      peaks: control.purpose_peaks,
    }
  }
}

fn script_sources_replay_item(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  value: ByteArray,
) -> Bool {
  let descriptor = ledger_output_commitment_v1.decode(value)
  if resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == control.replay_remaining_schedule_hash {
    let next_sources = replay_source_frontier(
      control,
      source_kind,
      key,
      descriptor,
    )
    let next_purposes = replay_purpose_frontier(
      control,
      source_kind,
      key,
      descriptor,
    )
    let next_resolved_item_peaks = validation_merkle_v1.append_leaf(
      control.replay_cursor,
      control.resolved_item_peaks,
      script_proof_v1.resolved_context_item_leaf_hash(
        source_kind,
        control.replay_cursor,
        key,
        value,
      ),
    )
    let next_control = encode_script_sources_witness(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      control.context_cbor,
      control.resolved_input_count,
      control.resolved_inputs_accumulator,
      control.signer_count,
      control.signer_frontier_commitment,
      next_resolved_item_peaks,
      3,
      next_sources.count,
      next_sources.peaks,
      control.redeemer_count,
      control.redeemer_peaks,
      control.replay_cursor + 1,
      resolved_input_accumulator_successor(
        control.replay_accumulator,
        source_kind,
        key,
        value,
      ),
      next_schedule_hash,
      control.spend_index + if source_kind == 0 {
        1
      } else {
        0
      },
      next_purposes.count,
      next_purposes.peaks,
      control.output_cursor,
      control.output_count,
      control.output_peaks,
      control.output_total_count,
      control.receive_scan,
      control.source_total_count,
      control.redeemer_total_count,
      control.observer_scan,
      control.mint_fold,
      control.resolution_schedule_hash,
    )
    and {
      witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
      witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
        validation_trace_v1.ScriptSources,
        pre.program_counter + 1,
        next_control,
      ),
    }
  } else {
    False
  }
}

fn script_sources_stage_three(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  if control.replay_remaining_schedule_hash == empty_resolution_schedule_hash() {
    let next_control = encode_script_sources_witness(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      control.context_cbor,
      control.resolved_input_count,
      control.resolved_inputs_accumulator,
      control.signer_count,
      control.signer_frontier_commitment,
      control.resolved_item_peaks,
      4,
      control.source_count,
      control.source_peaks,
      control.redeemer_count,
      control.redeemer_peaks,
      control.replay_cursor,
      control.replay_accumulator,
      control.replay_remaining_schedule_hash,
      control.spend_index,
      control.purpose_count,
      control.purpose_peaks,
      control.output_cursor,
      control.output_count,
      control.output_peaks,
      control.output_total_count,
      control.receive_scan,
      control.source_total_count,
      control.redeemer_total_count,
      control.observer_scan,
      control.mint_fold,
      control.resolution_schedule_hash,
    )
    and {
      auxiliary == NoAuxiliaryWitness,
      control.replay_cursor == control.resolved_input_count,
      control.replay_accumulator == control.resolved_inputs_accumulator,
      witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
      witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
        validation_trace_v1.ScriptSources,
        pre.program_counter + 1,
        next_control,
      ),
    }
  } else {
    when auxiliary is {
      ResolvedInputReplayWitness { source_kind, key, next_schedule_hash, value } ->
        script_sources_replay_item(
          pre,
          witness,
          control,
          source_kind,
          key,
          next_schedule_hash,
          value,
        )
      _ -> False
    }
  }
}

fn script_sources_stage_four(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let Pair(verified_source, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let output_commitment = verified_source.tx_compact.body.outputs_hash
  let empty_output_commitment = bounded_collection_v1.from_items(2, [])
  if output_commitment == empty_output_commitment {
    and {
      auxiliary == NoAuxiliaryWitness,
      control.output_cursor == 0,
      control.output_count == 0,
      control.output_total_count == 0,
      control.output_peaks == [],
      control.receive_scan == empty_receive_purpose_scan_control(),
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 { ..control, stage: 5 },
      ),
    }
  } else if and {
    control.output_total_count > 0,
    control.output_count == control.output_total_count,
  } {
    and {
      auxiliary == NoAuxiliaryWitness,
      control.output_cursor == 0,
      control.receive_scan == empty_receive_purpose_scan_control(),
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 { ..control, stage: 5 },
      ),
    }
  } else {
    expect TransactionFieldItemWitness { collection_proof } = auxiliary
    let active_total_count = if control.output_total_count == 0 {
      collection_proof.item_count
    } else {
      control.output_total_count
    }
    and {
      control.output_cursor == 0,
      control.receive_scan == empty_receive_purpose_scan_control(),
      active_total_count > 0,
      active_total_count <= max_tx_size_derived_collection_item_count,
      collection_proof.field_index == 2,
      collection_proof.item_count == active_total_count,
      collection_proof.item_index == control.output_count,
      collection_proof.item_length <= max_serialized_output_preimage_bytes,
      bounded_collection_v1.verify_item(output_commitment, collection_proof),
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 {
          ..control,
          output_count: control.output_count + 1,
          output_peaks: validation_merkle_v1.append_leaf(
            control.output_count,
            control.output_peaks,
            script_proof_v1.output_item_leaf_hash(
              control.output_count,
              collection_proof.item_commitment,
            ),
          ),
          output_total_count: active_total_count,
        },
      ),
    }
  }
}

fn protected_output_authorization(
  address: MidgardAddress,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  signer_proof: SignerSetProofV1,
) -> InputSignerAuthorizationV1 {
  if address.protected {
    payment_credential_signer_authorization(
      address.payment_credential,
      signer_count,
      signer_frontier_commitment,
      signer_proof,
    )
  } else if signer_proof == NoSignerSetProof {
    InputSignerAuthorized
  } else {
    InputSignerProofMalformed
  }
}

fn receive_source_successor(
  scan: ReceivePurposeScanControlV1,
  address: MidgardAddress,
) -> ReceivePurposeScanControlV1 {
  if address.protected {
    when address.payment_credential is {
      PubKeyCredential(_) -> scan
      ScriptCredential(script_hash) ->
        ReceivePurposeScanControlV1 {
          ..scan,
          source_count: scan.source_count + 1,
          source_peaks: validation_merkle_v1.append_leaf(
            scan.source_count,
            scan.source_peaks,
            script_proof_v1.purpose_leaf_hash(
              3,
              scan.source_count,
              script_hash,
              script_hash,
            ),
          ),
        }
    }
  } else {
    scan
  }
}

fn script_sources_output_proof_begin(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  output_index: Int,
  total_length: Int,
  item_commitment: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  and {
    control.output_proof == None,
    output_index == control.output_cursor,
    total_length > 0,
    bytearray.length(item_commitment) == 32,
    validation_merkle_v1.verify_membership(
      control.output_count,
      control.output_peaks,
      output_index,
      script_proof_v1.output_item_leaf_hash(output_index, item_commitment),
      siblings,
    ),
    script_sources_control_successor_is_exact(
      pre,
      witness,
      ScriptSourcesControlV1 {
        ..control,
        output_proof: Some(
          ledger_output_proof_v1.initial_control_v1(
            output_index,
            total_length,
            item_commitment,
          ),
        ),
      },
    ),
  }
}

fn script_sources_output_proof_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  output_proof: ledger_output_proof_v1.LedgerOutputProofControlV1,
  proof_witness: ledger_output_proof_v1.LedgerOutputProofWitnessV1,
) -> Bool {
  when ledger_output_proof_v1.step_v1(output_proof, proof_witness) is {
    None -> False
    Some(result) ->
      when result is {
        ledger_output_proof_v1.LedgerOutputProofAdvanced {
          control: next_output_proof,
        } ->
          script_sources_control_successor_is_exact(
            pre,
            witness,
            ScriptSourcesControlV1 {
              ..control,
              output_proof: Some(next_output_proof),
            },
          )
        ledger_output_proof_v1.LedgerOutputProofInvalidOutput ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_output,
          )
        ledger_output_proof_v1.LedgerOutputProofInvalidReferenceScript ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_invalid_field_type,
          )
        ledger_output_proof_v1.LedgerOutputProofNativeScriptNodeLimit ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_native_script_node_count,
          )
        ledger_output_proof_v1.LedgerOutputProofNativeScriptDepthLimit ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_native_script_depth,
          )
      }
  }
}

fn script_sources_output_proof_finalize(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  output_proof: ledger_output_proof_v1.LedgerOutputProofControlV1,
  descriptor_cbor: ByteArray,
  signer_proof: SignerSetProofV1,
) -> Bool {
  let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
  if !ledger_output_proof_v1.descriptor_is_exact_v1(output_proof, descriptor) {
    False
  } else {
    expect Some(address) = ledger_output_v1.decode_canonical_address_bytes(
      descriptor.address,
    )
    when
      protected_output_authorization(
        address,
        control.signer_count,
        control.signer_frontier_commitment,
        signer_proof,
      )
    is {
      InputSignerMissing ->
        rejected_successor_is_exact(
          pre,
          witness.claimed_successor,
          reject_missing_required_witness,
        )
      InputSignerProofMalformed -> False
      InputSignerAuthorized ->
        script_sources_control_successor_is_exact(
          pre,
          witness,
          ScriptSourcesControlV1 {
            ..control,
            output_cursor: control.output_cursor + 1,
            receive_scan: receive_source_successor(
              ReceivePurposeScanControlV1 {
                ..control.receive_scan,
                descriptor_peaks: validation_merkle_v1.append_leaf(
                  control.output_cursor,
                  control.receive_scan.descriptor_peaks,
                  script_proof_v1.output_descriptor_leaf_hash(
                    control.output_cursor,
                    descriptor_cbor,
                  ),
                ),
              },
              address,
            ),
            output_proof: None,
          },
        )
    }
  }
}

fn script_sources_stage_five(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  if control.output_cursor == control.output_count {
    and {
      control.output_proof == None,
      auxiliary == NoAuxiliaryWitness,
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 { ..control, stage: 6 },
      ),
    }
  } else {
    when control.output_proof is {
      None ->
        when auxiliary is {
          LedgerOutputProofBeginWitness {
            output_index,
            total_length,
            item_commitment,
            siblings,
          } ->
            script_sources_output_proof_begin(
              pre,
              witness,
              control,
              output_index,
              total_length,
              item_commitment,
              siblings,
            )
          _ -> False
        }
      Some(output_proof) ->
        if ledger_output_proof_v1.terminal_is_exact_v1(output_proof) {
          when auxiliary is {
            LedgerOutputProofFinalizeWitness { descriptor_cbor, signer_proof } ->
              script_sources_output_proof_finalize(
                pre,
                witness,
                control,
                output_proof,
                descriptor_cbor,
                signer_proof,
              )
            _ -> False
          }
        } else {
          when auxiliary is {
            LedgerOutputProofStepWitness { witness: proof_witness } ->
              script_sources_output_proof_step(
                pre,
                witness,
                control,
                output_proof,
                proof_witness,
              )
            _ -> False
          }
        }
    }
  }
}

type MintChunkWindowV1 {
  bytes: ByteArray,
  offset: Int,
  chunk_index: Int,
}

fn mint_fold_chunk_window(
  control: MintFoldControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Option<MintChunkWindowV1> {
  let expected_chunk_index = control.item_cursor / bounded_item_v1.chunk_bytes
  let count = bounded_item_v1.chunk_count(control.item_length)
  let current_matches = and {
    chunk_proof.field_index == 5,
    chunk_proof.item_index == control.policy_cursor,
    chunk_proof.total_length == control.item_length,
    chunk_proof.chunk_index == expected_chunk_index,
    bounded_item_v1.verify_chunk(control.item_commitment, chunk_proof),
  }
  if !current_matches {
    None
  } else if expected_chunk_index + 1 < count {
    when next_chunk_proof is {
      None -> None
      Some(next) ->
        if and {
          next.field_index == 5,
          next.item_index == control.policy_cursor,
          next.total_length == control.item_length,
          next.chunk_index == expected_chunk_index + 1,
          bounded_item_v1.verify_chunk(control.item_commitment, next),
        } {
          Some(
            MintChunkWindowV1 {
              bytes: bytearray.concat(chunk_proof.chunk, next.chunk),
              offset: control.item_cursor - expected_chunk_index * bounded_item_v1.chunk_bytes,
              chunk_index: expected_chunk_index,
            },
          )
        } else {
          None
        }
    }
  } else if next_chunk_proof == None {
    Some(
      MintChunkWindowV1 {
        bytes: chunk_proof.chunk,
        offset: control.item_cursor - expected_chunk_index * bounded_item_v1.chunk_bytes,
        chunk_index: expected_chunk_index,
      },
    )
  } else {
    None
  }
}

fn script_sources_begin_mint_policy(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  expected_commitment: ByteArray,
  collection_proof: bounded_collection_v1.ItemProofV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
) -> Bool {
  let fold = control.mint_fold
  let Pair(policy_offset, item_count) = decode_definite_array_header_at(
    chunk_proof.chunk,
    0,
  )
  expect item_count == 2
  let Pair(assets_header_offset, policy_id) = decode_canonical_bytes_at(
    chunk_proof.chunk,
    policy_offset,
  )
  let Pair(assets_offset, asset_count) = decode_canonical_map_header_at(
    chunk_proof.chunk,
    assets_header_offset,
  )
  let active_policy_count = if fold.policy_count == -1 {
    collection_proof.item_count
  } else {
    fold.policy_count
  }
  let common_is_valid = and {
    fold.active_policy == #"",
    fold.policy_cursor < active_policy_count,
    collection_proof.field_index == 5,
    collection_proof.item_count == active_policy_count,
    collection_proof.item_count <= max_tx_size_derived_collection_item_count,
    collection_proof.item_index == fold.policy_cursor,
    collection_proof.item_length > assets_offset,
    collection_proof.item_length <= max_aggregate_field_preimage_bytes,
    chunk_proof.field_index == 5,
    chunk_proof.item_index == fold.policy_cursor,
    chunk_proof.total_length == collection_proof.item_length,
    chunk_proof.chunk_index == 0,
    bounded_collection_v1.verify_item(expected_commitment, collection_proof),
    bounded_item_v1.verify_chunk(collection_proof.item_commitment, chunk_proof),
    bytearray.length(policy_id) == 28,
    asset_count > 0,
    or {
      fold.policy_cursor == 0,
      bytearray.compare(fold.previous_policy, policy_id) == Less,
    },
  }
  if and {
    common_is_valid,
    fold.asset_count + asset_count > ledger_output_v1.max_distinct_asset_count,
  } {
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_asset_count,
    )
  } else {
    and {
      common_is_valid,
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 {
          ..control,
          purpose_count: control.purpose_count + 1,
          purpose_peaks: validation_merkle_v1.append_leaf(
            control.purpose_count,
            control.purpose_peaks,
            script_proof_v1.purpose_leaf_hash(
              1,
              fold.policy_cursor,
              policy_id,
              policy_id,
            ),
          ),
          mint_fold: MintFoldControlV1 {
            ..fold,
            policy_count: active_policy_count,
            active_policy: policy_id,
            item_length: collection_proof.item_length,
            item_commitment: collection_proof.item_commitment,
            item_cursor: assets_offset,
            assets_remaining: asset_count,
            policy_asset_cursor: 0,
            previous_asset: #"",
          },
        },
      ),
    }
  }
}

fn script_sources_fold_mint_asset(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
  next_chunk_proof: Option<bounded_item_v1.ChunkProofV1>,
) -> Bool {
  let fold = control.mint_fold
  expect Some(window) = mint_fold_chunk_window(
    fold,
    chunk_proof,
    next_chunk_proof,
  )
  let Pair(quantity_offset, asset_name) = decode_canonical_bytes_at(
    window.bytes,
    window.offset,
  )
  let Pair(next_window_offset, quantity) = decode_canonical_int_at(
    window.bytes,
    quantity_offset,
  )
  let next_item_cursor = window.chunk_index * bounded_item_v1.chunk_bytes + next_window_offset
  let next_asset_count = fold.asset_count + 1
  let common_is_valid = and {
    bytearray.length(fold.active_policy) == 28,
    fold.assets_remaining > 0,
    fold.policy_asset_cursor >= 0,
    bytearray.length(asset_name) <= 32,
    quantity != 0,
    next_asset_count <= ledger_output_v1.max_distinct_asset_count,
    or {
      fold.policy_asset_cursor == 0,
      canonical_bytes_key_precedes(fold.previous_asset, asset_name),
    },
    if fold.assets_remaining == 1 {
      next_item_cursor == fold.item_length
    } else {
      next_item_cursor < fold.item_length
    },
  }
  let next_fold = if fold.assets_remaining == 1 {
    MintFoldControlV1 {
      ..fold,
      policy_cursor: fold.policy_cursor + 1,
      previous_policy: fold.active_policy,
      active_policy: #"",
      item_length: 0,
      item_commitment: #"",
      item_cursor: 0,
      assets_remaining: 0,
      policy_asset_cursor: 0,
      previous_asset: #"",
      asset_count: next_asset_count,
      asset_peaks: validation_merkle_v1.append_leaf(
        fold.asset_count,
        fold.asset_peaks,
        mint_asset_leaf_hash(fold.active_policy, asset_name, quantity),
      ),
    }
  } else {
    MintFoldControlV1 {
      ..fold,
      item_cursor: next_item_cursor,
      assets_remaining: fold.assets_remaining - 1,
      policy_asset_cursor: fold.policy_asset_cursor + 1,
      previous_asset: asset_name,
      asset_count: next_asset_count,
      asset_peaks: validation_merkle_v1.append_leaf(
        fold.asset_count,
        fold.asset_peaks,
        mint_asset_leaf_hash(fold.active_policy, asset_name, quantity),
      ),
    }
  }
  and {
    common_is_valid,
    script_sources_control_successor_is_exact(
      pre,
      witness,
      ScriptSourcesControlV1 { ..control, mint_fold: next_fold },
    ),
  }
}

fn script_sources_stage_six(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let Pair(verified, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let expected_commitment = verified.tx_compact.body.mint_hash
  let fold = control.mint_fold
  if fold.policy_count == -1 && expected_commitment == bounded_collection_v1.from_items(
    5,
    [],
  ) {
    and {
      auxiliary == NoAuxiliaryWitness,
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 {
          ..control,
          stage: 7,
          output_cursor: 0,
          mint_fold: MintFoldControlV1 { ..fold, policy_count: 0 },
        },
      ),
    }
  } else if and {
    fold.policy_count >= 0,
    fold.policy_cursor == fold.policy_count,
    fold.active_policy == #"",
  } {
    and {
      auxiliary == NoAuxiliaryWitness,
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 { ..control, stage: 7, output_cursor: 0 },
      ),
    }
  } else if fold.active_policy == #"" {
    expect TransactionFieldChunkWitness { collection_proof, chunk_proof } = auxiliary
    script_sources_begin_mint_policy(
      pre,
      witness,
      control,
      expected_commitment,
      collection_proof,
      chunk_proof,
    )
  } else {
    expect MintFoldAssetWitness { chunk_proof, next_chunk_proof } = auxiliary
    script_sources_fold_mint_asset(
      pre,
      witness,
      control,
      chunk_proof,
      next_chunk_proof,
    )
  }
}

fn observer_purpose_scan_is_well_formed(
  stage: Int,
  scan: ObserverPurposeScanControlV1,
) -> Bool {
  and {
    scan.total_count >= 0,
    scan.seen >= 0,
    scan.seen <= scan.total_count,
    if scan.seen == 0 {
      scan.previous_hash == #""
    } else {
      bytearray.length(scan.previous_hash) == 28
    },
    if stage == 7 {
      True
    } else {
      and {
        scan.total_count == 0,
        scan.seen == 0,
        scan.previous_hash == #"",
      }
    },
  }
}

fn receive_purpose_scan_is_well_formed(
  stage: Int,
  output_cursor: Int,
  output_count: Int,
  scan: ReceivePurposeScanControlV1,
) -> Bool {
  let descriptor_count = if stage < 5 {
    0
  } else if stage == 5 {
    output_cursor
  } else {
    output_count
  }
  and {
    validation_merkle_v1.frontier_is_well_formed(
      descriptor_count,
      scan.descriptor_peaks,
    ),
    scan.source_count >= 0,
    scan.source_count <= output_count,
    validation_merkle_v1.frontier_is_well_formed(
      scan.source_count,
      scan.source_peaks,
    ),
    scan.receive_count >= 0,
    scan.receive_count <= scan.source_count,
    if scan.receive_count == 0 {
      scan.previous_hash == #""
    } else {
      bytearray.length(scan.previous_hash) == 28
    },
    or {
      scan.candidate_hash == #"",
      and {
        bytearray.length(scan.candidate_hash) == 28,
        or {
          scan.previous_hash == #"",
          bytearray.compare(scan.previous_hash, scan.candidate_hash) == Less,
        },
      },
    },
    if stage < 5 {
      scan == empty_receive_purpose_scan_control()
    } else if stage < 7 {
      and {
        scan.receive_count == 0,
        scan.previous_hash == #"",
        scan.candidate_hash == #"",
      }
    } else if stage == 7 {
      True
    } else {
      and {
        scan.source_count == 0,
        scan.source_peaks == [],
        scan.receive_count == 0,
        scan.previous_hash == #"",
        scan.candidate_hash == #"",
      }
    },
  }
}

fn next_receive_candidate(
  scan: ReceivePurposeScanControlV1,
  script_hash: ByteArray,
) -> ByteArray {
  if and {
    or {
      scan.previous_hash == #"",
      bytearray.compare(scan.previous_hash, script_hash) == Less,
    },
    or {
      scan.candidate_hash == #"",
      bytearray.compare(script_hash, scan.candidate_hash) == Less,
    },
  } {
    script_hash
  } else {
    scan.candidate_hash
  }
}

fn script_sources_receive_scan_item(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  expect
      ScriptPurposeScanWitness {
        purpose_kind,
        purpose_index,
        script_hash,
        subject,
        siblings,
      }
    = auxiliary
  and {
    purpose_kind == 3,
    purpose_index == control.output_cursor,
    bytearray.length(script_hash) == 28,
    subject == script_hash,
    validation_merkle_v1.verify_membership(
      control.receive_scan.source_count,
      control.receive_scan.source_peaks,
      purpose_index,
      script_proof_v1.purpose_leaf_hash(
        purpose_kind,
        purpose_index,
        script_hash,
        subject,
      ),
      siblings,
    ),
    script_sources_control_successor_is_exact(
      pre,
      witness,
      ScriptSourcesControlV1 {
        ..control,
        output_cursor: control.output_cursor + 1,
        receive_scan: ReceivePurposeScanControlV1 {
          ..control.receive_scan,
          candidate_hash: next_receive_candidate(
            control.receive_scan,
            script_hash,
          ),
        },
      },
    ),
  }
}

fn script_sources_receive_scan_finish(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  and {
    auxiliary == NoAuxiliaryWitness,
    if control.receive_scan.candidate_hash == #"" {
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 {
          ..control,
          stage: 8,
          output_cursor: control.output_count,
          receive_scan: ReceivePurposeScanControlV1 {
            ..empty_receive_purpose_scan_control(),
            descriptor_peaks: control.receive_scan.descriptor_peaks,
          },
          observer_scan: empty_observer_purpose_scan_control(),
          discovery: empty_script_discovery_control(),
        },
      )
    } else {
      let script_hash = control.receive_scan.candidate_hash
      script_sources_control_successor_is_exact(
        pre,
        witness,
        ScriptSourcesControlV1 {
          ..control,
          output_cursor: 0,
          purpose_count: control.purpose_count + 1,
          purpose_peaks: validation_merkle_v1.append_leaf(
            control.purpose_count,
            control.purpose_peaks,
            script_proof_v1.purpose_leaf_hash(
              3,
              control.receive_scan.receive_count,
              script_hash,
              script_hash,
            ),
          ),
          receive_scan: ReceivePurposeScanControlV1 {
            source_count: control.receive_scan.source_count,
            source_peaks: control.receive_scan.source_peaks,
            receive_count: control.receive_scan.receive_count + 1,
            previous_hash: script_hash,
            candidate_hash: #"",
            descriptor_peaks: control.receive_scan.descriptor_peaks,
          },
        },
      )
    },
  }
}

fn script_sources_stage_seven_with_observer_commitment(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
  observer_commitment: ByteArray,
) -> Bool {
  let empty_observer_commitment = bounded_collection_v1.from_items(3, [])
  let observer_scan_is_complete = if observer_commitment == empty_observer_commitment {
    control.observer_scan == empty_observer_purpose_scan_control()
  } else {
    and {
      control.observer_scan.total_count > 0,
      control.observer_scan.seen == control.observer_scan.total_count,
    }
  }
  if observer_scan_is_complete {
    if control.output_cursor == control.receive_scan.source_count {
      script_sources_receive_scan_finish(pre, witness, auxiliary, control)
    } else {
      script_sources_receive_scan_item(pre, witness, auxiliary, control)
    }
  } else {
    expect TransactionFieldChunkWitness { collection_proof, chunk_proof } = auxiliary
    let active_count = if control.observer_scan.total_count == 0 {
      collection_proof.item_count
    } else {
      control.observer_scan.total_count
    }
    let observer_hash = chunk_proof.chunk
    and {
      control.output_cursor == 0,
      control.receive_scan.receive_count == 0,
      control.receive_scan.previous_hash == #"",
      control.receive_scan.candidate_hash == #"",
      active_count > 0,
      active_count <= max_tx_size_derived_collection_item_count,
      collection_proof.field_index == 3,
      collection_proof.item_count == active_count,
      collection_proof.item_index == control.observer_scan.seen,
      collection_proof.item_length == 28,
      chunk_proof.field_index == 3,
      chunk_proof.item_index == collection_proof.item_index,
      chunk_proof.total_length == 28,
      chunk_proof.chunk_index == 0,
      bounded_item_v1.chunk_count(chunk_proof.total_length) == 1,
      bounded_collection_v1.verify_item(observer_commitment, collection_proof),
      bounded_item_v1.verify_chunk(
        collection_proof.item_commitment,
        chunk_proof,
      ),
      if and {
        control.observer_scan.seen > 0,
        bytearray.compare(control.observer_scan.previous_hash, observer_hash) != Less,
      } {
        rejected_successor_is_exact(
          pre,
          witness.claimed_successor,
          reject_invalid_field_type,
        )
      } else {
        script_sources_control_successor_is_exact(
          pre,
          witness,
          ScriptSourcesControlV1 {
            ..control,
            purpose_count: control.purpose_count + 1,
            purpose_peaks: validation_merkle_v1.append_leaf(
              control.purpose_count,
              control.purpose_peaks,
              script_proof_v1.purpose_leaf_hash(
                2,
                control.observer_scan.seen,
                observer_hash,
                observer_hash,
              ),
            ),
            observer_scan: ObserverPurposeScanControlV1 {
              total_count: active_count,
              seen: control.observer_scan.seen + 1,
              previous_hash: observer_hash,
            },
          },
        )
      },
    }
  }
}

fn script_sources_stage_seven(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let Pair(verified_source, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  script_sources_stage_seven_with_observer_commitment(
    pre,
    witness,
    auxiliary,
    control,
    verified_source.tx_compact.body.required_observers_hash,
  )
}

fn script_sources_stage_seven_control_from_witness(
  work_witness_cbor: ByteArray,
) -> ScriptSourcesControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let items = builtin.un_list_data(data)
  expect list.length(items) == 30
  let control = script_sources_control_from_data_items(
    items,
    empty_script_discovery_control(),
    None,
  )
  expect control.stage == 7
  control
}

fn script_sources_stage_seven_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  expect Some(control_data) = cbor.deserialise(witness.work_witness_cbor)
  let receive_scan = control.receive_scan
  let observer_scan = control.observer_scan
  let mint_fold = control.mint_fold
  and {
    cbor.serialise(control_data) == witness.work_witness_cbor,
    control.stage == 7,
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    control.source_total_count == control.source_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.source_count,
      control.source_peaks,
    ),
    control.redeemer_total_count == control.redeemer_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.redeemer_count,
      control.redeemer_peaks,
    ),
    validation_merkle_v1.frontier_is_well_formed(
      control.purpose_count,
      control.purpose_peaks,
    ),
    control.output_total_count == control.output_count,
    control.output_count <= max_tx_size_derived_collection_item_count,
    control.output_cursor >= 0,
    control.output_cursor <= receive_scan.source_count,
    receive_scan.source_count >= 0,
    receive_scan.source_count <= control.output_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.output_count,
      control.output_peaks,
    ),
    validation_merkle_v1.frontier_is_well_formed(
      receive_scan.source_count,
      receive_scan.source_peaks,
    ),
    validation_merkle_v1.frontier_is_well_formed(
      control.output_count,
      receive_scan.descriptor_peaks,
    ),
    receive_scan.receive_count >= 0,
    receive_scan.receive_count <= receive_scan.source_count,
    if receive_scan.receive_count == 0 {
      receive_scan.previous_hash == #""
    } else {
      bytearray.length(receive_scan.previous_hash) == 28
    },
    or {
      receive_scan.candidate_hash == #"",
      and {
        bytearray.length(receive_scan.candidate_hash) == 28,
        or {
          receive_scan.previous_hash == #"",
          bytearray.compare(
            receive_scan.previous_hash,
            receive_scan.candidate_hash,
          ) == Less,
        },
      },
    },
    observer_scan.total_count >= 0,
    observer_scan.total_count <= max_tx_size_derived_collection_item_count,
    observer_scan.seen >= 0,
    observer_scan.seen <= observer_scan.total_count,
    if observer_scan.seen == 0 {
      observer_scan.previous_hash == #""
    } else {
      bytearray.length(observer_scan.previous_hash) == 28
    },
    mint_fold.policy_count >= 0,
    mint_fold.policy_count <= max_tx_size_derived_collection_item_count,
    mint_fold.policy_cursor == mint_fold.policy_count,
    mint_fold_active_state_is_empty(mint_fold),
    mint_fold.asset_count >= 0,
    mint_fold.asset_count <= ledger_output_v1.max_distinct_asset_count,
    validation_merkle_v1.frontier_is_well_formed(
      mint_fold.asset_count,
      mint_fold.asset_peaks,
    ),
    if mint_fold.policy_cursor == 0 {
      mint_fold.previous_policy == #""
    } else {
      bytearray.length(mint_fold.previous_policy) == 28
    },
  }
}

fn script_sources_stage_seven_observer_scan_is_complete(
  control: ScriptSourcesControlV1,
  observer_commitment: ByteArray,
) -> Bool {
  if observer_commitment == bounded_collection_v1.from_items(3, []) {
    control.observer_scan == empty_observer_purpose_scan_control()
  } else {
    and {
      control.observer_scan.total_count > 0,
      control.observer_scan.seen == control.observer_scan.total_count,
    }
  }
}

fn replace_script_sources_data_item(
  items: List<Data>,
  index: Int,
  replacement: Data,
) -> List<Data> {
  expect index >= 0
  when items is {
    [] -> fail
    [item, ..rest] ->
      if index == 0 {
        [replacement, ..rest]
      } else {
        [item, ..replace_script_sources_data_item(rest, index - 1, replacement)]
      }
  }
}

fn script_sources_stage_seven_successor_items_are_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  next_items: List<Data>,
) -> Bool {
  script_sources_stage_zero_successor_work_is_exact(
    pre,
    witness,
    next_items |> builtin.list_data |> cbor.serialise,
  )
}

fn script_sources_stage_seven_observer_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  observer_hash: ByteArray,
  active_count: Int,
) -> Bool {
  let next_purpose_peaks = validation_merkle_v1.append_leaf(
    control.purpose_count,
    control.purpose_peaks,
    script_proof_v1.purpose_leaf_hash(
      2,
      control.observer_scan.seen,
      observer_hash,
      observer_hash,
    ),
  )
  expect Some(next_purpose_peaks_data) = cbor.deserialise(
    validation_merkle_v1.encode_frontier(next_purpose_peaks),
  )
  expect Some(next_observer_scan_data) = cbor.deserialise(
    encode_observer_purpose_scan_control(
      ObserverPurposeScanControlV1 {
        total_count: active_count,
        seen: control.observer_scan.seen + 1,
        previous_hash: observer_hash,
      },
    ),
  )
  expect Some(control_data) = cbor.deserialise(witness.work_witness_cbor)
  let items = builtin.un_list_data(control_data)
  let next_items = replace_script_sources_data_item(
    items,
    18,
    builtin.i_data(control.purpose_count + 1),
  )
    |> replace_script_sources_data_item(19, next_purpose_peaks_data)
    |> replace_script_sources_data_item(27, next_observer_scan_data)
  script_sources_stage_seven_successor_items_are_exact(pre, witness, next_items)
}

fn script_sources_stage_seven_receive_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  candidate_hash: ByteArray,
) -> Bool {
  let next_receive_scan = ReceivePurposeScanControlV1 {
    ..control.receive_scan,
    candidate_hash,
  }
  expect Some(next_receive_scan_data) = cbor.deserialise(
    encode_receive_purpose_scan_control(next_receive_scan),
  )
  expect Some(control_data) = cbor.deserialise(witness.work_witness_cbor)
  let next_items = control_data
    |> builtin.un_list_data
    |> replace_script_sources_data_item(
        20,
        builtin.i_data(control.output_cursor + 1),
      )
    |> replace_script_sources_data_item(24, next_receive_scan_data)
  script_sources_stage_seven_successor_items_are_exact(pre, witness, next_items)
}

fn script_sources_stage_seven_finish_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let scan = control.receive_scan
  expect Some(control_data) = cbor.deserialise(witness.work_witness_cbor)
  let items = builtin.un_list_data(control_data)
  if scan.candidate_hash == #"" {
    let next_receive_scan = ReceivePurposeScanControlV1 {
      ..empty_receive_purpose_scan_control(),
      descriptor_peaks: scan.descriptor_peaks,
    }
    expect Some(next_receive_scan_data) = cbor.deserialise(
      encode_receive_purpose_scan_control(next_receive_scan),
    )
    expect Some(next_observer_scan_data) = cbor.deserialise(
      encode_observer_purpose_scan_control(
        empty_observer_purpose_scan_control(),
      ),
    )
    let next_base_items = items
      |> replace_script_sources_data_item(9, builtin.i_data(8))
      |> replace_script_sources_data_item(
          20,
          builtin.i_data(control.output_count),
        )
      |> replace_script_sources_data_item(24, next_receive_scan_data)
      |> replace_script_sources_data_item(27, next_observer_scan_data)
    let next_items = list.concat(
      next_base_items,
      [
        builtin.b_data(
          encode_script_discovery_control(empty_script_discovery_control()),
        ),
      ],
    )
    script_sources_stage_seven_successor_items_are_exact(
      pre,
      witness,
      next_items,
    )
  } else {
    let script_hash = scan.candidate_hash
    let next_purpose_peaks = validation_merkle_v1.append_leaf(
      control.purpose_count,
      control.purpose_peaks,
      script_proof_v1.purpose_leaf_hash(
        3,
        scan.receive_count,
        script_hash,
        script_hash,
      ),
    )
    let next_receive_scan = ReceivePurposeScanControlV1 {
      source_count: scan.source_count,
      source_peaks: scan.source_peaks,
      receive_count: scan.receive_count + 1,
      previous_hash: script_hash,
      candidate_hash: #"",
      descriptor_peaks: scan.descriptor_peaks,
    }
    expect Some(next_purpose_peaks_data) = cbor.deserialise(
      validation_merkle_v1.encode_frontier(next_purpose_peaks),
    )
    expect Some(next_receive_scan_data) = cbor.deserialise(
      encode_receive_purpose_scan_control(next_receive_scan),
    )
    let next_items = items
      |> replace_script_sources_data_item(
          18,
          builtin.i_data(control.purpose_count + 1),
        )
      |> replace_script_sources_data_item(19, next_purpose_peaks_data)
      |> replace_script_sources_data_item(20, builtin.i_data(0))
      |> replace_script_sources_data_item(24, next_receive_scan_data)
    script_sources_stage_seven_successor_items_are_exact(
      pre,
      witness,
      next_items,
    )
  }
}

pub fn verify_script_sources_stage_seven_observer_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  collection_proof: bounded_collection_v1.ItemProofV1,
  chunk_proof: bounded_item_v1.ChunkProofV1,
) -> Bool {
  let control = script_sources_stage_seven_control_from_witness(
    witness.work_witness_cbor,
  )
  let observer_commitment = decode_native_tx_compact_v1(control.compact_cbor).body.required_observers_hash
  let observer_scan = control.observer_scan
  let active_count = if observer_scan.total_count == 0 {
    collection_proof.item_count
  } else {
    observer_scan.total_count
  }
  let observer_hash = chunk_proof.chunk
  let common_is_valid = and {
    script_sources_stage_seven_control_is_bound(pre, witness, control),
    observer_commitment != bounded_collection_v1.from_items(3, []),
    !script_sources_stage_seven_observer_scan_is_complete(
      control,
      observer_commitment,
    ),
    control.output_cursor == 0,
    control.receive_scan.receive_count == 0,
    control.receive_scan.previous_hash == #"",
    control.receive_scan.candidate_hash == #"",
    active_count > 0,
    active_count <= max_tx_size_derived_collection_item_count,
    collection_proof.field_index == 3,
    collection_proof.item_count == active_count,
    collection_proof.item_index == observer_scan.seen,
    collection_proof.item_length == 28,
    chunk_proof.field_index == 3,
    chunk_proof.item_index == collection_proof.item_index,
    chunk_proof.total_length == 28,
    chunk_proof.chunk_index == 0,
    bytearray.length(observer_hash) == 28,
    bounded_item_v1.chunk_count(chunk_proof.total_length) == 1,
    bounded_collection_v1.verify_item(observer_commitment, collection_proof),
    bounded_item_v1.verify_chunk(collection_proof.item_commitment, chunk_proof),
  }
  if and {
    common_is_valid,
    observer_scan.seen > 0,
    bytearray.compare(observer_scan.previous_hash, observer_hash) != Less,
  } {
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_invalid_field_type,
    )
  } else {
    and {
      common_is_valid,
      script_sources_stage_seven_observer_successor_is_exact(
        pre,
        witness,
        control,
        observer_hash,
        active_count,
      ),
    }
  }
}

pub fn verify_script_sources_stage_seven_receive_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  purpose_kind: Int,
  purpose_index: Int,
  script_hash: ByteArray,
  subject: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let control = script_sources_stage_seven_control_from_witness(
    witness.work_witness_cbor,
  )
  let observer_commitment = decode_native_tx_compact_v1(control.compact_cbor).body.required_observers_hash
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    purpose_kind,
    purpose_index,
    script_hash,
    subject,
  )
  and {
    script_sources_stage_seven_control_is_bound(pre, witness, control),
    script_sources_stage_seven_observer_scan_is_complete(
      control,
      observer_commitment,
    ),
    control.output_cursor < control.receive_scan.source_count,
    purpose_kind == 3,
    purpose_index == control.output_cursor,
    bytearray.length(script_hash) == 28,
    subject == script_hash,
    validation_merkle_v1.verify_membership(
      control.receive_scan.source_count,
      control.receive_scan.source_peaks,
      purpose_index,
      purpose_leaf,
      siblings,
    ),
    script_sources_stage_seven_receive_successor_is_exact(
      pre,
      witness,
      control,
      next_receive_candidate(control.receive_scan, script_hash),
    ),
  }
}

pub fn verify_script_sources_stage_seven_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_stage_seven_control_from_witness(
    witness.work_witness_cbor,
  )
  let observer_commitment = decode_native_tx_compact_v1(control.compact_cbor).body.required_observers_hash
  and {
    script_sources_stage_seven_control_is_bound(pre, witness, control),
    script_sources_stage_seven_observer_scan_is_complete(
      control,
      observer_commitment,
    ),
    control.output_cursor == control.receive_scan.source_count,
    script_sources_stage_seven_finish_successor_is_exact(pre, witness, control),
  }
}

fn script_discovery_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  stage: Int,
  discovery: ScriptDiscoveryControlV1,
) -> Bool {
  let prefix = script_sources_stage_zero_prefix_before_stage(control, 31)
  let stage_offset = bytearray.length(prefix)
  let old_stage = cbor.serialise(control.stage)
  let old_discovery = encode_definite_bytes(
    encode_script_discovery_control(control.discovery),
  )
  let work_length = bytearray.length(witness.work_witness_cbor)
  let discovery_offset = work_length - bytearray.length(old_discovery)
  expect discovery_offset >= stage_offset + bytearray.length(old_stage)
  expect
    builtin.slice_bytearray(0, stage_offset, witness.work_witness_cbor) == prefix
  expect
    builtin.slice_bytearray(
      stage_offset,
      bytearray.length(old_stage),
      witness.work_witness_cbor,
    ) == old_stage
  expect
    builtin.slice_bytearray(
      discovery_offset,
      bytearray.length(old_discovery),
      witness.work_witness_cbor,
    ) == old_discovery
  let next_control = builtin.slice_bytearray(
    0,
    stage_offset,
    witness.work_witness_cbor,
  )
    |> bytearray.concat(cbor.serialise(stage))
    |> bytearray.concat(
        builtin.slice_bytearray(
          stage_offset + bytearray.length(old_stage),
          discovery_offset - stage_offset - bytearray.length(old_stage),
          witness.work_witness_cbor,
        ),
      )
    |> bytearray.concat(
        encode_definite_bytes(encode_script_discovery_control(discovery)),
      )
  script_sources_stage_zero_successor_work_is_exact(pre, witness, next_control)
}

fn script_discovery_bit(index: Int) -> Int {
  expect index >= 0
  if index == 0 {
    1
  } else {
    2 * script_discovery_bit(index - 1)
  }
}

fn script_discovery_bitmap_has(bitmap: Int, index: Int) -> Bool {
  let bit = script_discovery_bit(index)
  bitmap / bit % 2 == 1
}

fn script_discovery_bitmap_insert(bitmap: Int, index: Int) -> Int {
  if script_discovery_bitmap_has(bitmap, index) {
    bitmap
  } else {
    bitmap + script_discovery_bit(index)
  }
}

fn reset_script_discovery_current(
  control: ScriptDiscoveryControlV1,
) -> ScriptDiscoveryControlV1 {
  ScriptDiscoveryControlV1 {
    ..control,
    source_cursor: 0,
    redeemer_cursor: 0,
    current_purpose_kind: -1,
    current_purpose_index: -1,
    current_script_hash: #"",
    current_subject: #"",
    matched_source_index: -1,
    matched_language_tag: -1,
    matched_source_leaf: #"",
    redeemer_item_control_hash: #"",
  }
}

fn script_sources_stage_eight(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let discovery = control.discovery
  if discovery.purpose_cursor == control.purpose_count {
    and {
      auxiliary == NoAuxiliaryWitness,
      script_discovery_successor_is_exact(
        pre,
        witness,
        control,
        11,
        reset_script_discovery_current(discovery),
      ),
    }
  } else {
    expect
        ScriptPurposeScanWitness {
          purpose_kind,
          purpose_index,
          script_hash,
          subject,
          siblings,
        }
      = auxiliary
    let purpose_leaf = script_proof_v1.purpose_leaf_hash(
      purpose_kind,
      purpose_index,
      script_hash,
      subject,
    )
    and {
      discovery.purpose_cursor < control.purpose_count,
      validation_merkle_v1.verify_membership(
        control.purpose_count,
        control.purpose_peaks,
        discovery.purpose_cursor,
        purpose_leaf,
        siblings,
      ),
      script_discovery_successor_is_exact(
        pre,
        witness,
        control,
        9,
        ScriptDiscoveryControlV1 {
          ..discovery,
          source_cursor: 0,
          redeemer_cursor: 0,
          current_purpose_kind: purpose_kind,
          current_purpose_index: purpose_index,
          current_script_hash: script_hash,
          current_subject: subject,
          matched_source_index: -1,
          matched_language_tag: -1,
          matched_source_leaf: #"",
        },
      ),
    }
  }
}

fn script_sources_stage_eight_control_from_witness(
  work_witness_cbor: ByteArray,
) -> ScriptSourcesControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let items = builtin.un_list_data(data)
  expect list.length(items) == 31
  let Pair(base_items, extension_data) = split_last_data(items)
  let base_control = script_sources_control_from_data_items(
    base_items,
    empty_script_discovery_control(),
    None,
  )
  expect base_control.stage == 8
  ScriptSourcesControlV1 {
    ..base_control,
    discovery: extension_data
      |> builtin.un_b_data
      |> script_discovery_control_from_cbor,
  }
}

fn script_sources_stage_eight_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  expect Some(control_data) = cbor.deserialise(witness.work_witness_cbor)
  let discovery = control.discovery
  and {
    cbor.serialise(control_data) == witness.work_witness_cbor,
    control.stage == 8,
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    control.source_total_count == control.source_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.source_count,
      control.source_peaks,
    ),
    control.redeemer_total_count == control.redeemer_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.redeemer_count,
      control.redeemer_peaks,
    ),
    discovery.purpose_cursor >= 0,
    discovery.purpose_cursor <= control.purpose_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.purpose_count,
      control.purpose_peaks,
    ),
    discovery.source_cursor == 0,
    discovery.redeemer_cursor == 0,
    discovery.execution_count == discovery.purpose_cursor,
    validation_merkle_v1.frontier_is_well_formed(
      discovery.execution_count,
      discovery.execution_peaks,
    ),
    discovery.current_purpose_kind == -1,
    discovery.current_purpose_index == -1,
    discovery.current_script_hash == #"",
    discovery.current_subject == #"",
    discovery.matched_source_index == -1,
    discovery.matched_language_tag == -1,
    discovery.matched_source_leaf == #"",
    discovery.used_inline_bitmap >= 0,
    discovery.used_inline_bitmap < script_discovery_bit(control.source_count),
    discovery.used_redeemer_bitmap >= 0,
    discovery.used_redeemer_bitmap < script_discovery_bit(
      control.redeemer_count,
    ),
  }
}

pub fn verify_script_sources_stage_eight_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_stage_eight_control_from_witness(
    witness.work_witness_cbor,
  )
  let discovery = control.discovery
  and {
    script_sources_stage_eight_control_is_bound(pre, witness, control),
    discovery.purpose_cursor == control.purpose_count,
    script_discovery_successor_is_exact(
      pre,
      witness,
      control,
      11,
      reset_script_discovery_current(discovery),
    ),
  }
}

pub fn verify_script_sources_stage_eight_purpose_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  purpose_kind: Int,
  purpose_index: Int,
  script_hash: ByteArray,
  subject: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let control = script_sources_stage_eight_control_from_witness(
    witness.work_witness_cbor,
  )
  let discovery = control.discovery
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    purpose_kind,
    purpose_index,
    script_hash,
    subject,
  )
  and {
    script_sources_stage_eight_control_is_bound(pre, witness, control),
    discovery.purpose_cursor < control.purpose_count,
    purpose_kind >= 0,
    purpose_kind <= 3,
    purpose_index >= 0,
    bytearray.length(script_hash) == 28,
    subject != #"",
    validation_merkle_v1.verify_membership(
      control.purpose_count,
      control.purpose_peaks,
      discovery.purpose_cursor,
      purpose_leaf,
      siblings,
    ),
    script_discovery_successor_is_exact(
      pre,
      witness,
      control,
      9,
      ScriptDiscoveryControlV1 {
        ..discovery,
        source_cursor: 0,
        redeemer_cursor: 0,
        current_purpose_kind: purpose_kind,
        current_purpose_index: purpose_index,
        current_script_hash: script_hash,
        current_subject: subject,
        matched_source_index: -1,
        matched_language_tag: -1,
        matched_source_leaf: #"",
      },
    ),
  }
}

fn append_script_execution(
  discovery: ScriptDiscoveryControlV1,
  purpose_leaf: ByteArray,
  source_leaf: ByteArray,
  language_tag: Int,
  redeemer_leaf: ByteArray,
) -> validation_merkle_v1.BuiltFrontier {
  validation_merkle_v1.BuiltFrontier {
    count: discovery.execution_count + 1,
    peaks: validation_merkle_v1.append_leaf(
      discovery.execution_count,
      discovery.execution_peaks,
      script_proof_v1.execution_leaf_hash(
        language_tag,
        purpose_leaf,
        source_leaf,
        redeemer_leaf,
      ),
    ),
  }
}

fn current_script_purpose_leaf(discovery: ScriptDiscoveryControlV1) -> ByteArray {
  script_proof_v1.purpose_leaf_hash(
    discovery.current_purpose_kind,
    discovery.current_purpose_index,
    discovery.current_script_hash,
    discovery.current_subject,
  )
}

fn script_sources_stage_nine(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let discovery = control.discovery
  if discovery.source_cursor == control.source_count {
    and {
      auxiliary == NoAuxiliaryWitness,
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_missing_required_witness,
      ),
    }
  } else {
    expect
        ScriptSourceScanWitness {
          source_index,
          origin_kind,
          source_key,
          script_language_tag,
          script_hash,
          script_total_length,
          script_item_commitment,
          siblings,
        }
      = auxiliary
    let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
      origin_kind,
      source_key,
      script_language_tag,
      script_hash,
      script_total_length,
      script_item_commitment,
    )
    if and {
      source_index == discovery.source_cursor,
      validation_merkle_v1.verify_membership(
        control.source_count,
        control.source_peaks,
        source_index,
        source_leaf,
        siblings,
      ),
    } {
      if script_hash == discovery.current_script_hash {
        let next_used_inline = if origin_kind == 0 {
          script_discovery_bitmap_insert(
            discovery.used_inline_bitmap,
            source_index,
          )
        } else {
          discovery.used_inline_bitmap
        }
        if script_language_tag == 0 {
          let next_execution = append_script_execution(
            discovery,
            current_script_purpose_leaf(discovery),
            source_leaf,
            script_language_tag,
            #"",
          )
          let completed = reset_script_discovery_current(
            ScriptDiscoveryControlV1 {
              ..discovery,
              purpose_cursor: discovery.purpose_cursor + 1,
              used_inline_bitmap: next_used_inline,
              execution_count: next_execution.count,
              execution_peaks: next_execution.peaks,
            },
          )
          script_discovery_successor_is_exact(
            pre,
            witness,
            control,
            8,
            completed,
          )
        } else {
          script_discovery_successor_is_exact(
            pre,
            witness,
            control,
            10,
            ScriptDiscoveryControlV1 {
              ..discovery,
              source_cursor: source_index + 1,
              redeemer_cursor: 0,
              matched_source_index: source_index,
              matched_language_tag: script_language_tag,
              matched_source_leaf: source_leaf,
              used_inline_bitmap: next_used_inline,
            },
          )
        }
      } else {
        script_discovery_successor_is_exact(
          pre,
          witness,
          control,
          9,
          ScriptDiscoveryControlV1 {
            ..discovery,
            source_cursor: source_index + 1,
          },
        )
      }
    } else {
      False
    }
  }
}

fn script_sources_stage_nine_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let discovery = control.discovery
  let prefix = script_sources_stage_zero_prefix_before_stage(control, 31)
  let stage_offset = bytearray.length(prefix)
  let encoded_discovery = encode_definite_bytes(
    encode_script_discovery_control(discovery),
  )
  let discovery_offset = bytearray.length(witness.work_witness_cbor) - bytearray.length(
    encoded_discovery,
  )
  and {
    control.stage == 9,
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    control.source_total_count == control.source_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.source_count,
      control.source_peaks,
    ),
    control.redeemer_total_count == control.redeemer_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.redeemer_count,
      control.redeemer_peaks,
    ),
    control.purpose_count > discovery.purpose_cursor,
    validation_merkle_v1.frontier_is_well_formed(
      control.purpose_count,
      control.purpose_peaks,
    ),
    discovery.source_cursor <= control.source_count,
    discovery.redeemer_cursor <= control.redeemer_count,
    discovery.execution_count == discovery.purpose_cursor,
    validation_merkle_v1.frontier_is_well_formed(
      discovery.execution_count,
      discovery.execution_peaks,
    ),
    discovery.current_purpose_kind >= 0,
    discovery.current_purpose_kind <= 3,
    discovery.current_purpose_index >= 0,
    bytearray.length(discovery.current_script_hash) == 28,
    discovery.current_subject != #"",
    discovery.matched_source_index == -1,
    discovery.matched_language_tag == -1,
    discovery.matched_source_leaf == #"",
    control.pending_source_cbor == #"",
    control.output_proof == None,
    discovery_offset >= stage_offset + 1,
    builtin.slice_bytearray(0, stage_offset, witness.work_witness_cbor) == prefix,
    builtin.slice_bytearray(stage_offset, 1, witness.work_witness_cbor) == #"09",
    builtin.slice_bytearray(
      discovery_offset,
      bytearray.length(encoded_discovery),
      witness.work_witness_cbor,
    ) == encoded_discovery,
  }
}

fn script_sources_stage_nine_source_is_authenticated(
  control: ScriptSourcesControlV1,
  source_index: Int,
  origin_kind: Int,
  source_key: ByteArray,
  script_language_tag: Int,
  script_hash: ByteArray,
  script_total_length: Int,
  script_item_commitment: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    origin_kind,
    source_key,
    script_language_tag,
    script_hash,
    script_total_length,
    script_item_commitment,
  )
  and {
    source_index == control.discovery.source_cursor,
    validation_merkle_v1.verify_membership(
      control.source_count,
      control.source_peaks,
      source_index,
      source_leaf,
      siblings,
    ),
  }
}

fn script_sources_stage_nine_control_from_witness(
  work_witness_cbor: ByteArray,
) -> ScriptSourcesControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let items = builtin.un_list_data(data)
  expect list.length(items) == 31
  let Pair(base_items, extension_data) = split_last_data(items)
  let base_control = script_sources_control_from_data_items(
    base_items,
    empty_script_discovery_control(),
    None,
  )
  expect base_control.stage == 9
  ScriptSourcesControlV1 {
    ..base_control,
    discovery: extension_data
      |> builtin.un_b_data
      |> script_discovery_control_from_cbor,
  }
}

pub fn verify_script_sources_stage_nine_mismatch_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_index: Int,
  origin_kind: Int,
  source_key: ByteArray,
  script_language_tag: Int,
  script_hash: ByteArray,
  script_total_length: Int,
  script_item_commitment: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let control = script_sources_stage_nine_control_from_witness(
    witness.work_witness_cbor,
  )
  let discovery = control.discovery
  and {
    script_sources_stage_nine_control_is_bound(pre, witness, control),
    discovery.source_cursor < control.source_count,
    script_sources_stage_nine_source_is_authenticated(
      control,
      source_index,
      origin_kind,
      source_key,
      script_language_tag,
      script_hash,
      script_total_length,
      script_item_commitment,
      siblings,
    ),
    script_hash != discovery.current_script_hash,
    script_discovery_successor_is_exact(
      pre,
      witness,
      control,
      9,
      ScriptDiscoveryControlV1 { ..discovery, source_cursor: source_index + 1 },
    ),
  }
}

pub fn verify_script_sources_stage_nine_native_match_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_index: Int,
  origin_kind: Int,
  source_key: ByteArray,
  script_hash: ByteArray,
  script_total_length: Int,
  script_item_commitment: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let control = script_sources_stage_nine_control_from_witness(
    witness.work_witness_cbor,
  )
  let discovery = control.discovery
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    origin_kind,
    source_key,
    0,
    script_hash,
    script_total_length,
    script_item_commitment,
  )
  let next_used_inline = if origin_kind == 0 {
    script_discovery_bitmap_insert(discovery.used_inline_bitmap, source_index)
  } else {
    discovery.used_inline_bitmap
  }
  let next_execution = append_script_execution(
    discovery,
    current_script_purpose_leaf(discovery),
    source_leaf,
    0,
    #"",
  )
  let completed = reset_script_discovery_current(
    ScriptDiscoveryControlV1 {
      ..discovery,
      purpose_cursor: discovery.purpose_cursor + 1,
      used_inline_bitmap: next_used_inline,
      execution_count: next_execution.count,
      execution_peaks: next_execution.peaks,
    },
  )
  and {
    script_sources_stage_nine_control_is_bound(pre, witness, control),
    discovery.source_cursor < control.source_count,
    script_sources_stage_nine_source_is_authenticated(
      control,
      source_index,
      origin_kind,
      source_key,
      0,
      script_hash,
      script_total_length,
      script_item_commitment,
      siblings,
    ),
    script_hash == discovery.current_script_hash,
    script_discovery_successor_is_exact(pre, witness, control, 8, completed),
  }
}

pub fn verify_script_sources_stage_nine_effectful_match_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_index: Int,
  origin_kind: Int,
  source_key: ByteArray,
  script_language_tag: Int,
  script_hash: ByteArray,
  script_total_length: Int,
  script_item_commitment: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let control = script_sources_stage_nine_control_from_witness(
    witness.work_witness_cbor,
  )
  let discovery = control.discovery
  and {
    or {
      script_language_tag == 3,
      script_language_tag == 128,
    },
    script_sources_stage_nine_control_is_bound(pre, witness, control),
    discovery.source_cursor < control.source_count,
    script_sources_stage_nine_source_is_authenticated(
      control,
      source_index,
      origin_kind,
      source_key,
      script_language_tag,
      script_hash,
      script_total_length,
      script_item_commitment,
      siblings,
    ),
    script_hash == discovery.current_script_hash,
    script_discovery_successor_is_exact(
      pre,
      witness,
      control,
      10,
      ScriptDiscoveryControlV1 {
        ..discovery,
        source_cursor: source_index + 1,
        redeemer_cursor: 0,
        matched_source_index: source_index,
        matched_language_tag: script_language_tag,
        matched_source_leaf: script_proof_v1.source_descriptor_leaf_hash(
          origin_kind,
          source_key,
          script_language_tag,
          script_hash,
          script_total_length,
          script_item_commitment,
        ),
        used_inline_bitmap: if origin_kind == 0 {
          script_discovery_bitmap_insert(
            discovery.used_inline_bitmap,
            source_index,
          )
        } else {
          discovery.used_inline_bitmap
        },
      },
    ),
  }
}

pub fn verify_script_sources_stage_nine_missing_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_stage_nine_control_from_witness(
    witness.work_witness_cbor,
  )
  and {
    script_sources_stage_nine_control_is_bound(pre, witness, control),
    control.discovery.source_cursor == control.source_count,
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_missing_required_witness,
    ),
  }
}

fn purpose_kind_to_redeemer_tag(purpose_kind: Int) -> Int {
  if purpose_kind == 0 {
    0
  } else if purpose_kind == 1 {
    1
  } else if purpose_kind == 2 {
    3
  } else {
    expect purpose_kind == 3
    6
  }
}

fn redeemer_descriptor_matches_current_purpose(
  descriptor: redeemer_item_proof_v1.RedeemerItemDescriptorV1,
  discovery: ScriptDiscoveryControlV1,
) -> Bool {
  and {
    descriptor.pointer_index == discovery.current_purpose_index,
    descriptor.purpose_tag == purpose_kind_to_redeemer_tag(
      discovery.current_purpose_kind,
    ),
  }
}

fn script_sources_stage_ten(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let discovery = control.discovery
  if discovery.redeemer_cursor == control.redeemer_count {
    and {
      discovery.redeemer_item_control_hash == #"",
      auxiliary == NoAuxiliaryWitness,
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_missing_required_witness,
      ),
    }
  } else if discovery.redeemer_item_control_hash == #"" {
    expect
        RedeemerScanBeginWitness {
          item_index,
          item_count,
          total_length,
          item_commitment,
          siblings,
        }
      = auxiliary
    let redeemer_leaf = script_proof_v1.redeemer_item_leaf_hash(
      item_index,
      item_commitment,
    )
    let item_control = redeemer_item_proof_v1.initial_control_v1(
      redeemer_item_proof_v1.mode_descriptor,
      item_index,
      item_count,
      total_length,
      item_commitment,
      -1,
      -1,
    )
    and {
      item_index == discovery.redeemer_cursor,
      item_count == control.redeemer_count,
      validation_merkle_v1.verify_membership(
        control.redeemer_count,
        control.redeemer_peaks,
        item_index,
        redeemer_leaf,
        siblings,
      ),
      script_discovery_successor_is_exact(
        pre,
        witness,
        control,
        10,
        ScriptDiscoveryControlV1 {
          ..discovery,
          redeemer_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
            item_control,
          ),
        },
      ),
    }
  } else {
    expect
        RedeemerItemStepWitness {
          redeemer_control: None,
          control: item_control,
          witness: item_witness,
        }
      = auxiliary
    let current_matches = and {
      item_control.mode == redeemer_item_proof_v1.mode_descriptor,
      item_control.item_index == discovery.redeemer_cursor,
      item_control.item_count == control.redeemer_count,
      redeemer_item_proof_v1.hash_control_v1(item_control) == discovery.redeemer_item_control_hash,
    }
    when redeemer_item_proof_v1.step_v1(item_control, item_witness) is {
      None -> False
      Some(redeemer_item_proof_v1.RedeemerItemProofInvalid) -> False
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced { control: next }) ->
        if next.stage == redeemer_item_proof_v1.stage_terminal {
          when redeemer_item_proof_v1.descriptor_v1(next) is {
            None -> False
            Some(descriptor) -> {
              let redeemer_leaf = script_proof_v1.redeemer_item_leaf_hash(
                descriptor.item_index,
                descriptor.item_commitment,
              )
              if redeemer_descriptor_matches_current_purpose(
                descriptor,
                discovery,
              ) {
                let next_execution = append_script_execution(
                  discovery,
                  current_script_purpose_leaf(discovery),
                  discovery.matched_source_leaf,
                  discovery.matched_language_tag,
                  redeemer_leaf,
                )
                let completed = reset_script_discovery_current(
                  ScriptDiscoveryControlV1 {
                    ..discovery,
                    purpose_cursor: discovery.purpose_cursor + 1,
                    used_redeemer_bitmap: script_discovery_bitmap_insert(
                      discovery.used_redeemer_bitmap,
                      descriptor.item_index,
                    ),
                    execution_count: next_execution.count,
                    execution_peaks: next_execution.peaks,
                  },
                )
                and {
                  current_matches,
                  script_discovery_successor_is_exact(
                    pre,
                    witness,
                    control,
                    8,
                    completed,
                  ),
                }
              } else {
                and {
                  current_matches,
                  script_discovery_successor_is_exact(
                    pre,
                    witness,
                    control,
                    10,
                    ScriptDiscoveryControlV1 {
                      ..discovery,
                      redeemer_cursor: descriptor.item_index + 1,
                      redeemer_item_control_hash: #"",
                    },
                  ),
                }
              }
            }
          }
        } else {
          and {
            current_matches,
            script_discovery_successor_is_exact(
              pre,
              witness,
              control,
              10,
              ScriptDiscoveryControlV1 {
                ..discovery,
                redeemer_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
                  next,
                ),
              },
            ),
          }
        }
    }
  }
}

fn script_sources_stage_ten_control_from_witness(
  work_witness_cbor: ByteArray,
) -> ScriptSourcesControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let items = builtin.un_list_data(data)
  expect list.length(items) == 31
  let Pair(base_items, extension_data) = split_last_data(items)
  let base_control = script_sources_control_from_data_items(
    base_items,
    empty_script_discovery_control(),
    None,
  )
  expect base_control.stage == 10
  ScriptSourcesControlV1 {
    ..base_control,
    discovery: extension_data
      |> builtin.un_b_data
      |> script_discovery_control_from_cbor,
  }
}

fn script_sources_stage_ten_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  expect Some(control_data) = cbor.deserialise(witness.work_witness_cbor)
  let discovery = control.discovery
  and {
    cbor.serialise(control_data) == witness.work_witness_cbor,
    control.stage == 10,
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    control.source_total_count == control.source_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.source_count,
      control.source_peaks,
    ),
    control.redeemer_total_count == control.redeemer_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.redeemer_count,
      control.redeemer_peaks,
    ),
    discovery.purpose_cursor < control.purpose_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.purpose_count,
      control.purpose_peaks,
    ),
    discovery.source_cursor >= 0,
    discovery.source_cursor <= control.source_count,
    discovery.redeemer_cursor >= 0,
    discovery.redeemer_cursor <= control.redeemer_count,
    discovery.execution_count == discovery.purpose_cursor,
    validation_merkle_v1.frontier_is_well_formed(
      discovery.execution_count,
      discovery.execution_peaks,
    ),
    discovery.current_purpose_kind >= 0,
    discovery.current_purpose_kind <= 3,
    discovery.current_purpose_index >= 0,
    bytearray.length(discovery.current_script_hash) == 28,
    discovery.current_subject != #"",
    discovery.matched_source_index >= 0,
    discovery.matched_source_index < control.source_count,
    or {
      discovery.matched_language_tag == 3,
      discovery.matched_language_tag == 128,
    },
    bytearray.length(discovery.matched_source_leaf) == 32,
    discovery.used_inline_bitmap >= 0,
    discovery.used_inline_bitmap < script_discovery_bit(control.source_count),
    discovery.used_redeemer_bitmap >= 0,
    discovery.used_redeemer_bitmap < script_discovery_bit(
      control.redeemer_count,
    ),
  }
}

pub fn verify_script_sources_stage_ten_missing_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_stage_ten_control_from_witness(
    witness.work_witness_cbor,
  )
  and {
    script_sources_stage_ten_control_is_bound(pre, witness, control),
    control.discovery.redeemer_cursor == control.redeemer_count,
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_missing_required_witness,
    ),
  }
}

pub fn encode_native_scripts_witness_v1(
  control: ScriptSourcesControlV1,
  execution_cursor: Int,
  language_bitmap: Int,
) -> ByteArray {
  expect execution_cursor >= 0
  expect execution_cursor <= control.discovery.execution_count
  expect language_bitmap >= 0
  expect language_bitmap <= 3
  encode_definite_array_header(26)
    |> bytearray.concat(encode_definite_bytes(control.compact_cbor))
    |> bytearray.concat(encode_definite_bytes(control.witness_set_compact_cbor))
    |> bytearray.concat(
        encode_definite_bytes(control.field_preimage_lengths_cbor),
      )
    |> bytearray.concat(encode_definite_bytes(control.context_cbor))
    |> bytearray.concat(cbor.serialise(control.resolved_input_count))
    |> bytearray.concat(
        encode_definite_bytes(control.resolved_inputs_accumulator),
      )
    |> bytearray.concat(cbor.serialise(control.spend_index))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.resolved_item_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.signer_count))
    |> bytearray.concat(
        encode_definite_bytes(control.signer_frontier_commitment),
      )
    |> bytearray.concat(cbor.serialise(control.source_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.source_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.redeemer_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.redeemer_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.purpose_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.purpose_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.output_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.output_peaks),
      )
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(
          control.receive_scan.descriptor_peaks,
        ),
      )
    |> bytearray.concat(cbor.serialise(control.mint_fold.asset_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.mint_fold.asset_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.discovery.execution_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.discovery.execution_peaks),
      )
    |> bytearray.concat(cbor.serialise(execution_cursor))
    |> bytearray.concat(cbor.serialise(language_bitmap))
    |> bytearray.concat(encode_definite_bytes(control.resolution_schedule_hash))
}

fn script_sources_stage_eleven(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let discovery = control.discovery
  if discovery.source_cursor == control.source_count {
    and {
      auxiliary == NoAuxiliaryWitness,
      script_discovery_successor_is_exact(
        pre,
        witness,
        control,
        12,
        ScriptDiscoveryControlV1 { ..discovery, redeemer_cursor: 0 },
      ),
    }
  } else {
    expect
        ScriptSourceScanWitness {
          source_index,
          origin_kind,
          source_key,
          script_language_tag,
          script_hash,
          script_total_length,
          script_item_commitment,
          siblings,
        }
      = auxiliary
    let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
      origin_kind,
      source_key,
      script_language_tag,
      script_hash,
      script_total_length,
      script_item_commitment,
    )
    if and {
      source_index == discovery.source_cursor,
      validation_merkle_v1.verify_membership(
        control.source_count,
        control.source_peaks,
        source_index,
        source_leaf,
        siblings,
      ),
    } {
      if and {
        origin_kind == 0,
        !script_discovery_bitmap_has(discovery.used_inline_bitmap, source_index),
      } {
        rejected_successor_is_exact(
          pre,
          witness.claimed_successor,
          reject_invalid_field_type,
        )
      } else {
        script_discovery_successor_is_exact(
          pre,
          witness,
          control,
          11,
          ScriptDiscoveryControlV1 {
            ..discovery,
            source_cursor: source_index + 1,
          },
        )
      }
    } else {
      False
    }
  }
}

fn script_sources_stage_eleven_control_from_witness(
  work_witness_cbor: ByteArray,
) -> ScriptSourcesControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let items = builtin.un_list_data(data)
  expect list.length(items) == 31
  let Pair(base_items, extension_data) = split_last_data(items)
  let base_control = script_sources_control_from_data_items(
    base_items,
    empty_script_discovery_control(),
    None,
  )
  expect base_control.stage == 11
  ScriptSourcesControlV1 {
    ..base_control,
    discovery: extension_data
      |> builtin.un_b_data
      |> script_discovery_control_from_cbor,
  }
}

fn script_sources_stage_eleven_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  expect Some(control_data) = cbor.deserialise(witness.work_witness_cbor)
  let discovery = control.discovery
  and {
    cbor.serialise(control_data) == witness.work_witness_cbor,
    control.stage == 11,
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    control.source_total_count == control.source_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.source_count,
      control.source_peaks,
    ),
    control.redeemer_total_count == control.redeemer_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.redeemer_count,
      control.redeemer_peaks,
    ),
    control.purpose_count == discovery.purpose_cursor,
    validation_merkle_v1.frontier_is_well_formed(
      control.purpose_count,
      control.purpose_peaks,
    ),
    discovery.source_cursor >= 0,
    discovery.source_cursor <= control.source_count,
    discovery.redeemer_cursor >= 0,
    discovery.redeemer_cursor <= control.redeemer_count,
    discovery.execution_count == discovery.purpose_cursor,
    validation_merkle_v1.frontier_is_well_formed(
      discovery.execution_count,
      discovery.execution_peaks,
    ),
    discovery.current_purpose_kind == -1,
    discovery.current_purpose_index == -1,
    discovery.current_script_hash == #"",
    discovery.current_subject == #"",
    discovery.matched_source_index == -1,
    discovery.matched_language_tag == -1,
    discovery.matched_source_leaf == #"",
    discovery.used_inline_bitmap >= 0,
    discovery.used_inline_bitmap < script_discovery_bit(control.source_count),
    discovery.used_redeemer_bitmap >= 0,
    discovery.used_redeemer_bitmap < script_discovery_bit(
      control.redeemer_count,
    ),
  }
}

pub fn verify_script_sources_stage_eleven_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_stage_eleven_control_from_witness(
    witness.work_witness_cbor,
  )
  let discovery = control.discovery
  and {
    script_sources_stage_eleven_control_is_bound(pre, witness, control),
    discovery.source_cursor == control.source_count,
    script_discovery_successor_is_exact(
      pre,
      witness,
      control,
      12,
      ScriptDiscoveryControlV1 { ..discovery, redeemer_cursor: 0 },
    ),
  }
}

pub fn verify_script_sources_stage_eleven_source_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_index: Int,
  origin_kind: Int,
  source_key: ByteArray,
  script_language_tag: Int,
  script_hash: ByteArray,
  script_total_length: Int,
  script_item_commitment: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let control = script_sources_stage_eleven_control_from_witness(
    witness.work_witness_cbor,
  )
  let discovery = control.discovery
  and {
    script_sources_stage_eleven_control_is_bound(pre, witness, control),
    discovery.source_cursor < control.source_count,
    script_sources_stage_nine_source_is_authenticated(
      control,
      source_index,
      origin_kind,
      source_key,
      script_language_tag,
      script_hash,
      script_total_length,
      script_item_commitment,
      siblings,
    ),
    if and {
      origin_kind == 0,
      !script_discovery_bitmap_has(discovery.used_inline_bitmap, source_index),
    } {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_invalid_field_type,
      )
    } else {
      script_discovery_successor_is_exact(
        pre,
        witness,
        control,
        11,
        ScriptDiscoveryControlV1 {
          ..discovery,
          source_cursor: source_index + 1,
        },
      )
    },
  }
}

fn script_sources_stage_twelve(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  let discovery = control.discovery
  if discovery.redeemer_cursor == control.redeemer_count {
    let next_control = encode_native_scripts_witness_v1(control, 0, 0)
    and {
      discovery.redeemer_item_control_hash == #"",
      auxiliary == NoAuxiliaryWitness,
      witness.claimed_successor.phase == validation_trace_v1.NativeScripts,
      witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
        validation_trace_v1.NativeScripts,
        pre.program_counter + 1,
        next_control,
      ),
    }
  } else if discovery.redeemer_item_control_hash == #"" {
    expect
        RedeemerScanBeginWitness {
          item_index,
          item_count,
          total_length,
          item_commitment,
          siblings,
        }
      = auxiliary
    let redeemer_leaf = script_proof_v1.redeemer_item_leaf_hash(
      item_index,
      item_commitment,
    )
    let item_control = redeemer_item_proof_v1.initial_control_v1(
      redeemer_item_proof_v1.mode_descriptor,
      item_index,
      item_count,
      total_length,
      item_commitment,
      -1,
      -1,
    )
    and {
      item_index == discovery.redeemer_cursor,
      item_count == control.redeemer_count,
      validation_merkle_v1.verify_membership(
        control.redeemer_count,
        control.redeemer_peaks,
        item_index,
        redeemer_leaf,
        siblings,
      ),
      script_discovery_successor_is_exact(
        pre,
        witness,
        control,
        12,
        ScriptDiscoveryControlV1 {
          ..discovery,
          redeemer_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
            item_control,
          ),
        },
      ),
    }
  } else {
    expect
        RedeemerItemStepWitness {
          redeemer_control: None,
          control: item_control,
          witness: item_witness,
        }
      = auxiliary
    let current_matches = and {
      item_control.mode == redeemer_item_proof_v1.mode_descriptor,
      item_control.item_index == discovery.redeemer_cursor,
      item_control.item_count == control.redeemer_count,
      redeemer_item_proof_v1.hash_control_v1(item_control) == discovery.redeemer_item_control_hash,
    }
    when redeemer_item_proof_v1.step_v1(item_control, item_witness) is {
      None -> False
      Some(redeemer_item_proof_v1.RedeemerItemProofInvalid) -> False
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced { control: next }) ->
        if next.stage == redeemer_item_proof_v1.stage_terminal {
          when redeemer_item_proof_v1.descriptor_v1(next) is {
            None -> False
            Some(descriptor) -> and {
                current_matches,
                if !script_discovery_bitmap_has(
                  discovery.used_redeemer_bitmap,
                  descriptor.item_index,
                ) {
                  rejected_successor_is_exact(
                    pre,
                    witness.claimed_successor,
                    reject_invalid_field_type,
                  )
                } else {
                  script_discovery_successor_is_exact(
                    pre,
                    witness,
                    control,
                    12,
                    ScriptDiscoveryControlV1 {
                      ..discovery,
                      redeemer_cursor: descriptor.item_index + 1,
                      redeemer_item_control_hash: #"",
                    },
                  )
                },
              }
          }
        } else {
          and {
            current_matches,
            script_discovery_successor_is_exact(
              pre,
              witness,
              control,
              12,
              ScriptDiscoveryControlV1 {
                ..discovery,
                redeemer_item_control_hash: redeemer_item_proof_v1.hash_control_v1(
                  next,
                ),
              },
            ),
          }
        }
    }
  }
}

fn script_sources_stage_twelve_control_from_witness(
  work_witness_cbor: ByteArray,
) -> ScriptSourcesControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let items = builtin.un_list_data(data)
  expect list.length(items) == 31
  let Pair(base_items, extension_data) = split_last_data(items)
  let base_control = script_sources_control_from_data_items(
    base_items,
    empty_script_discovery_control(),
    None,
  )
  expect base_control.stage == 12
  ScriptSourcesControlV1 {
    ..base_control,
    discovery: extension_data
      |> builtin.un_b_data
      |> script_discovery_control_from_cbor,
  }
}

fn script_sources_stage_twelve_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  expect Some(control_data) = cbor.deserialise(witness.work_witness_cbor)
  let discovery = control.discovery
  and {
    cbor.serialise(control_data) == witness.work_witness_cbor,
    control.stage == 12,
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    control.source_total_count == control.source_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.source_count,
      control.source_peaks,
    ),
    control.redeemer_total_count == control.redeemer_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.redeemer_count,
      control.redeemer_peaks,
    ),
    control.purpose_count == discovery.purpose_cursor,
    validation_merkle_v1.frontier_is_well_formed(
      control.purpose_count,
      control.purpose_peaks,
    ),
    discovery.source_cursor == control.source_count,
    discovery.redeemer_cursor >= 0,
    discovery.redeemer_cursor <= control.redeemer_count,
    discovery.execution_count == discovery.purpose_cursor,
    validation_merkle_v1.frontier_is_well_formed(
      discovery.execution_count,
      discovery.execution_peaks,
    ),
    discovery.current_purpose_kind == -1,
    discovery.current_purpose_index == -1,
    discovery.current_script_hash == #"",
    discovery.current_subject == #"",
    discovery.matched_source_index == -1,
    discovery.matched_language_tag == -1,
    discovery.matched_source_leaf == #"",
    discovery.used_inline_bitmap >= 0,
    discovery.used_inline_bitmap < script_discovery_bit(control.source_count),
    discovery.used_redeemer_bitmap >= 0,
    discovery.used_redeemer_bitmap < script_discovery_bit(
      control.redeemer_count,
    ),
  }
}

pub fn verify_script_sources_stage_twelve_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_stage_twelve_control_from_witness(
    witness.work_witness_cbor,
  )
  let next_control = encode_native_scripts_witness_v1(control, 0, 0)
  and {
    script_sources_stage_twelve_control_is_bound(pre, witness, control),
    control.discovery.redeemer_cursor == control.redeemer_count,
    witness.claimed_successor.phase == validation_trace_v1.NativeScripts,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.NativeScripts,
      pre.program_counter + 1,
      next_control,
    ),
  }
}

fn script_discovery_stage_is_well_formed(
  control: ScriptSourcesControlV1,
) -> Bool {
  let discovery = control.discovery
  if control.stage < 8 {
    discovery == empty_script_discovery_control()
  } else {
    and {
      control.stage <= 12,
      discovery.purpose_cursor <= control.purpose_count,
      discovery.source_cursor <= control.source_count,
      discovery.redeemer_cursor <= control.redeemer_count,
      discovery.execution_count == discovery.purpose_cursor,
      discovery.used_inline_bitmap < script_discovery_bit(control.source_count),
      discovery.used_redeemer_bitmap < script_discovery_bit(
        control.redeemer_count,
      ),
      if control.stage == 8 {
        and {
          discovery.redeemer_item_control_hash == #"",
          discovery.current_purpose_kind == -1,
          discovery.current_purpose_index == -1,
          discovery.current_script_hash == #"",
          discovery.current_subject == #"",
          discovery.matched_source_index == -1,
          discovery.matched_language_tag == -1,
          discovery.matched_source_leaf == #"",
        }
      } else if control.stage == 9 {
        and {
          discovery.redeemer_item_control_hash == #"",
          discovery.purpose_cursor < control.purpose_count,
          discovery.current_purpose_kind >= 0,
          discovery.current_purpose_kind <= 3,
          discovery.current_purpose_index >= 0,
          bytearray.length(discovery.current_script_hash) == 28,
          discovery.current_subject != #"",
          discovery.matched_source_index == -1,
          discovery.matched_language_tag == -1,
          discovery.matched_source_leaf == #"",
        }
      } else if control.stage == 10 {
        and {
          discovery.purpose_cursor < control.purpose_count,
          discovery.current_purpose_kind >= 0,
          discovery.current_purpose_kind <= 3,
          discovery.current_purpose_index >= 0,
          bytearray.length(discovery.current_script_hash) == 28,
          discovery.current_subject != #"",
          discovery.matched_source_index >= 0,
          discovery.matched_source_index < control.source_count,
          or {
            discovery.matched_language_tag == 3,
            discovery.matched_language_tag == 128,
          },
          bytearray.length(discovery.matched_source_leaf) == 32,
        }
      } else {
        and {
          if control.stage == 11 {
            discovery.redeemer_item_control_hash == #""
          } else {
            True
          },
          discovery.purpose_cursor == control.purpose_count,
          discovery.current_purpose_kind == -1,
          discovery.current_purpose_index == -1,
          discovery.current_script_hash == #"",
          discovery.current_subject == #"",
          discovery.matched_source_index == -1,
          discovery.matched_language_tag == -1,
          discovery.matched_source_leaf == #"",
        }
      },
    }
  }
}

fn mint_fold_active_state_is_empty(control: MintFoldControlV1) -> Bool {
  and {
    control.active_policy == #"",
    control.item_length == 0,
    control.item_commitment == #"",
    control.item_cursor == 0,
    control.assets_remaining == 0,
    control.policy_asset_cursor == 0,
    control.previous_asset == #"",
  }
}

fn mint_fold_control_is_well_formed(
  stage: Int,
  control: MintFoldControlV1,
) -> Bool {
  let common_is_valid = and {
    control.policy_count >= -1,
    control.policy_count <= max_tx_size_derived_collection_item_count,
    control.policy_cursor >= 0,
    control.asset_count >= 0,
    control.asset_count <= ledger_output_v1.max_distinct_asset_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.asset_count,
      control.asset_peaks,
    ),
    if control.policy_cursor == 0 {
      control.previous_policy == #""
    } else {
      bytearray.length(control.previous_policy) == 28
    },
  }
  if stage < 6 {
    control == empty_mint_fold_control()
  } else if stage == 6 {
    and {
      common_is_valid,
      if control.policy_count == -1 {
        control == empty_mint_fold_control()
      } else if control.active_policy == #"" {
        and {
          control.policy_cursor <= control.policy_count,
          mint_fold_active_state_is_empty(control),
        }
      } else {
        and {
          control.policy_cursor < control.policy_count,
          bytearray.length(control.active_policy) == 28,
          control.item_length > 0,
          control.item_length <= max_aggregate_field_preimage_bytes,
          bytearray.length(control.item_commitment) == 32,
          control.item_cursor > 0,
          control.item_cursor < control.item_length,
          control.assets_remaining > 0,
          control.policy_asset_cursor >= 0,
          if control.policy_asset_cursor == 0 {
            control.previous_asset == #""
          } else {
            bytearray.length(control.previous_asset) <= 32
          },
          control.asset_count + control.assets_remaining <= ledger_output_v1.max_distinct_asset_count,
        }
      },
    }
  } else {
    and {
      common_is_valid,
      control.policy_count >= 0,
      control.policy_cursor == control.policy_count,
      mint_fold_active_state_is_empty(control),
    }
  }
}

fn script_sources_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  and {
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    bytearray.length(control.resolved_inputs_accumulator) == 32,
    bytearray.length(control.signer_frontier_commitment) == 32,
    control.resolved_input_count >= 0,
    control.signer_count >= 0,
    validation_merkle_v1.frontier_is_well_formed(
      control.source_count,
      control.source_peaks,
    ),
    control.source_total_count >= control.source_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.redeemer_count,
      control.redeemer_peaks,
    ),
    control.redeemer_total_count >= control.redeemer_count,
    bytearray.length(control.replay_accumulator) == 32,
    bytearray.length(control.replay_remaining_schedule_hash) == 32,
    control.replay_cursor >= 0,
    validation_merkle_v1.frontier_is_well_formed(
      control.replay_cursor,
      control.resolved_item_peaks,
    ),
    control.spend_index >= 0,
    control.spend_index <= control.replay_cursor,
    validation_merkle_v1.frontier_is_well_formed(
      control.purpose_count,
      control.purpose_peaks,
    ),
    control.output_cursor >= 0,
    control.output_cursor <= control.output_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.output_count,
      control.output_peaks,
    ),
    control.output_total_count >= control.output_count,
    control.output_total_count <= max_tx_size_derived_collection_item_count,
    if control.stage == 0 {
      if control.pending_source_cbor == #"" {
        True
      } else {
        let pending = inline_source_hash_control_from_cbor(
          control.pending_source_cbor,
        )
        and {
          pending.source_index == control.source_count,
          control.source_total_count == 0 || pending.source_total_count == control.source_total_count,
        }
      }
    } else if control.stage == 1 {
      bytearray.length(control.pending_source_cbor) == 0 || bytearray.length(
        control.pending_source_cbor,
      ) == 32
    } else {
      control.pending_source_cbor == #""
    },
    if control.stage == 5 {
      when control.output_proof is {
        None -> True
        Some(output_proof) -> and {
            ledger_output_proof_v1.control_is_well_formed(output_proof),
            output_proof.output_index == control.output_cursor,
            output_proof.output_index < control.output_count,
          }
      }
    } else {
      control.output_proof == None
    },
    if control.stage < 4 {
      and {
        control.output_count == 0,
        control.output_total_count == 0,
      }
    } else if control.stage == 4 {
      control.output_cursor == 0
    } else if control.stage == 5 {
      True
    } else if control.stage == 6 {
      control.output_cursor == control.output_count
    } else if control.stage == 7 {
      True
    } else {
      control.output_cursor == control.output_count
    },
    if control.stage == 4 {
      True
    } else {
      control.output_total_count == control.output_count
    },
    receive_purpose_scan_is_well_formed(
      control.stage,
      control.output_cursor,
      control.output_count,
      control.receive_scan,
    ),
    observer_purpose_scan_is_well_formed(control.stage, control.observer_scan),
    mint_fold_control_is_well_formed(control.stage, control.mint_fold),
    script_discovery_control_is_well_formed(control.discovery),
    script_discovery_stage_is_well_formed(control),
    witness.work_witness_cbor == exact_script_sources_control(control),
  }
}

pub fn verify_script_sources_stage_three_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  let next_control = encode_script_sources_witness(
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
    control.context_cbor,
    control.resolved_input_count,
    control.resolved_inputs_accumulator,
    control.signer_count,
    control.signer_frontier_commitment,
    control.resolved_item_peaks,
    4,
    control.source_count,
    control.source_peaks,
    control.redeemer_count,
    control.redeemer_peaks,
    control.replay_cursor,
    control.replay_accumulator,
    control.replay_remaining_schedule_hash,
    control.spend_index,
    control.purpose_count,
    control.purpose_peaks,
    control.output_cursor,
    control.output_count,
    control.output_peaks,
    control.output_total_count,
    control.receive_scan,
    control.source_total_count,
    control.redeemer_total_count,
    control.observer_scan,
    control.mint_fold,
    control.resolution_schedule_hash,
  )
  and {
    script_sources_control_is_bound(pre, witness, control),
    control.stage == 3,
    control.replay_remaining_schedule_hash == empty_resolution_schedule_hash(),
    control.replay_cursor == control.resolved_input_count,
    control.replay_accumulator == control.resolved_inputs_accumulator,
    witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      pre.program_counter + 1,
      next_control,
    ),
  }
}

pub fn verify_script_sources_stage_three_replay_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  value: ByteArray,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  and {
    script_sources_control_is_bound(pre, witness, control),
    control.stage == 3,
    control.replay_remaining_schedule_hash != empty_resolution_schedule_hash(),
    script_sources_replay_item(
      pre,
      witness,
      control,
      source_kind,
      key,
      next_schedule_hash,
      value,
    ),
  }
}

/// Stage-specific transition check used only after a prepare validator has
/// authenticated the complete ScriptSources control. Keeping preparation and
/// transition execution in separate computation-thread steps prevents the
/// same common-control work from making the semantic step undeployable.
pub fn verify_prepared_script_sources_stage_three_replay_transition_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  value: ByteArray,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  and {
    control.stage == 3,
    control.replay_remaining_schedule_hash != empty_resolution_schedule_hash(),
    script_sources_replay_item(
      pre,
      witness,
      control,
      source_kind,
      key,
      next_schedule_hash,
      value,
    ),
  }
}

pub fn verify_script_sources_stage_five_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  let next_control = encode_script_sources_witness(
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
    control.context_cbor,
    control.resolved_input_count,
    control.resolved_inputs_accumulator,
    control.signer_count,
    control.signer_frontier_commitment,
    control.resolved_item_peaks,
    6,
    control.source_count,
    control.source_peaks,
    control.redeemer_count,
    control.redeemer_peaks,
    control.replay_cursor,
    control.replay_accumulator,
    control.replay_remaining_schedule_hash,
    control.spend_index,
    control.purpose_count,
    control.purpose_peaks,
    control.output_cursor,
    control.output_count,
    control.output_peaks,
    control.output_total_count,
    control.receive_scan,
    control.source_total_count,
    control.redeemer_total_count,
    control.observer_scan,
    control.mint_fold,
    control.resolution_schedule_hash,
  )
  and {
    script_sources_control_is_bound(pre, witness, control),
    control.stage == 5,
    control.output_cursor == control.output_count,
    witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      pre.program_counter + 1,
      next_control,
    ),
  }
}

pub fn verify_script_sources_output_proof_begin_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  output_index: Int,
  total_length: Int,
  item_commitment: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  and {
    script_sources_control_is_bound(pre, witness, control),
    control.stage == 5,
    control.output_cursor < control.output_count,
    script_sources_output_proof_begin(
      pre,
      witness,
      control,
      output_index,
      total_length,
      item_commitment,
      siblings,
    ),
  }
}

pub fn verify_script_sources_output_proof_step_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  proof_witness: ledger_output_proof_v1.LedgerOutputProofWitnessV1,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  expect Some(output_proof) = control.output_proof
  and {
    script_sources_control_is_bound(pre, witness, control),
    control.stage == 5,
    control.output_cursor < control.output_count,
    !ledger_output_proof_v1.terminal_is_exact_v1(output_proof),
    script_sources_output_proof_step(
      pre,
      witness,
      control,
      output_proof,
      proof_witness,
    ),
  }
}

pub fn verify_script_sources_output_proof_finalize_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  descriptor_cbor: ByteArray,
  signer_proof: SignerSetProofV1,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  expect Some(output_proof) = control.output_proof
  and {
    script_sources_control_is_bound(pre, witness, control),
    control.stage == 5,
    control.output_cursor < control.output_count,
    ledger_output_proof_v1.terminal_is_exact_v1(output_proof),
    script_sources_output_proof_finalize(
      pre,
      witness,
      control,
      output_proof,
      descriptor_cbor,
      signer_proof,
    ),
  }
}

pub fn verify_script_sources_non_output_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  let witness = evidence.transition
  let auxiliary = evidence.auxiliary
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  let Pair(verified_source, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  and {
    verified_source.version == 1,
    script_sources_control_is_bound(pre, witness, control),
    control.stage != 5,
    if control.stage == 0 {
      script_sources_stage_zero(pre, witness, auxiliary, control)
    } else if control.stage == 1 {
      script_sources_stage_one(pre, witness, auxiliary, control)
    } else if control.stage == 2 {
      script_sources_stage_two(pre, witness, auxiliary, control)
    } else if control.stage == 3 {
      script_sources_stage_three(pre, witness, auxiliary, control)
    } else if control.stage == 4 {
      script_sources_stage_four(pre, witness, auxiliary, control)
    } else if control.stage == 6 {
      script_sources_stage_six(pre, witness, auxiliary, control)
    } else if control.stage == 7 {
      script_sources_stage_seven(pre, witness, auxiliary, control)
    } else if control.stage == 8 {
      script_sources_stage_eight(pre, witness, auxiliary, control)
    } else if control.stage == 9 {
      script_sources_stage_nine(pre, witness, auxiliary, control)
    } else if control.stage == 10 {
      script_sources_stage_ten(pre, witness, auxiliary, control)
    } else if control.stage == 11 {
      script_sources_stage_eleven(pre, witness, auxiliary, control)
    } else if control.stage == 12 {
      script_sources_stage_twelve(pre, witness, auxiliary, control)
    } else {
      False
    },
  }
}

fn verify_script_sources_semantic_stage(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  expected_stage: Int,
  verify_stage: fn(
    validation_trace_v1.ValidationMachineStateV1,
    ValidationOneStepWitnessV1,
    ValidationAuxiliaryWitnessV1,
    ScriptSourcesControlV1,
  ) ->
    Bool,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  and {
    script_sources_control_is_bound(pre, witness, control),
    control.stage == expected_stage,
    verify_stage(pre, witness, auxiliary, control),
  }
}

pub fn verify_script_sources_stage_zero_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    0,
    script_sources_stage_zero,
  )
}

pub fn verify_script_sources_stage_one_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    1,
    script_sources_stage_one,
  )
}

pub fn verify_script_sources_stage_two_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    2,
    script_sources_stage_two,
  )
}

pub fn verify_script_sources_stage_three_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    3,
    script_sources_stage_three,
  )
}

pub fn verify_script_sources_stage_four_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    4,
    script_sources_stage_four,
  )
}

pub fn verify_script_sources_stage_five_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    5,
    script_sources_stage_five,
  )
}

pub fn verify_script_sources_stage_six_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    6,
    script_sources_stage_six,
  )
}

pub fn verify_script_sources_stage_seven_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    7,
    script_sources_stage_seven,
  )
}

pub fn verify_script_sources_stage_eight_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    8,
    script_sources_stage_eight,
  )
}

pub fn verify_script_sources_stage_nine_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    9,
    script_sources_stage_nine,
  )
}

pub fn verify_script_sources_stage_ten_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    10,
    script_sources_stage_ten,
  )
}

pub fn verify_script_sources_stage_eleven_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    11,
    script_sources_stage_eleven,
  )
}

pub fn verify_script_sources_stage_twelve_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  verify_script_sources_semantic_stage(
    pre,
    witness,
    auxiliary,
    12,
    script_sources_stage_twelve,
  )
}

fn verify_script_sources(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  let Pair(verified_source, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  and {
    verified_source.version == 1,
    script_sources_control_is_bound(pre, witness, control),
    if control.stage == 0 {
      script_sources_stage_zero(pre, witness, auxiliary, control)
    } else if control.stage == 1 {
      script_sources_stage_one(pre, witness, auxiliary, control)
    } else if control.stage == 2 {
      script_sources_stage_two(pre, witness, auxiliary, control)
    } else if control.stage == 3 {
      script_sources_stage_three(pre, witness, auxiliary, control)
    } else if control.stage == 4 {
      script_sources_stage_four(pre, witness, auxiliary, control)
    } else if control.stage == 5 {
      script_sources_stage_five(pre, witness, auxiliary, control)
    } else if control.stage == 6 {
      script_sources_stage_six(pre, witness, auxiliary, control)
    } else if control.stage == 7 {
      script_sources_stage_seven_with_observer_commitment(
        pre,
        witness,
        auxiliary,
        control,
        verified_source.tx_compact.body.required_observers_hash,
      )
    } else if control.stage == 8 {
      script_sources_stage_eight(pre, witness, auxiliary, control)
    } else if control.stage == 9 {
      script_sources_stage_nine(pre, witness, auxiliary, control)
    } else if control.stage == 10 {
      script_sources_stage_ten(pre, witness, auxiliary, control)
    } else if control.stage == 11 {
      script_sources_stage_eleven(pre, witness, auxiliary, control)
    } else if control.stage == 12 {
      script_sources_stage_twelve(pre, witness, auxiliary, control)
    } else {
      False
    },
  }
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

fn native_scripts_control_from_witness(
  work_witness_cbor: ByteArray,
) -> NativeScriptsControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data,
        resolved_input_count_data, resolved_inputs_accumulator_data,
        spend_input_count_data, resolved_item_peaks_data, signer_count_data,
        signer_frontier_commitment_data, source_count_data, source_peaks_data,
        redeemer_count_data, redeemer_peaks_data, purpose_count_data,
        purpose_peaks_data, output_count_data, output_peaks_data,
        output_descriptor_peaks_data, mint_count_data, mint_peaks_data,
        execution_count_data, execution_peaks_data, execution_cursor_data,
        language_bitmap_data, resolution_schedule_hash_data,
      ]
    = builtin.un_list_data(data)
  NativeScriptsControlV1 {
    compact_cbor: builtin.un_b_data(compact_cbor_data),
    witness_set_compact_cbor: builtin.un_b_data(witness_set_compact_cbor_data),
    field_preimage_lengths_cbor: builtin.un_b_data(
      field_preimage_lengths_cbor_data,
    ),
    context_cbor: builtin.un_b_data(context_cbor_data),
    resolved_input_count: builtin.un_i_data(resolved_input_count_data),
    resolved_inputs_accumulator: builtin.un_b_data(
      resolved_inputs_accumulator_data,
    ),
    spend_input_count: builtin.un_i_data(spend_input_count_data),
    resolved_item_peaks: decode_frontier_peaks(resolved_item_peaks_data),
    signer_count: builtin.un_i_data(signer_count_data),
    signer_frontier_commitment: builtin.un_b_data(
      signer_frontier_commitment_data,
    ),
    source_count: builtin.un_i_data(source_count_data),
    source_peaks: decode_frontier_peaks(source_peaks_data),
    redeemer_count: builtin.un_i_data(redeemer_count_data),
    redeemer_peaks: decode_frontier_peaks(redeemer_peaks_data),
    purpose_count: builtin.un_i_data(purpose_count_data),
    purpose_peaks: decode_frontier_peaks(purpose_peaks_data),
    output_count: builtin.un_i_data(output_count_data),
    output_peaks: decode_frontier_peaks(output_peaks_data),
    output_descriptor_peaks: decode_frontier_peaks(output_descriptor_peaks_data),
    mint_count: builtin.un_i_data(mint_count_data),
    mint_peaks: decode_frontier_peaks(mint_peaks_data),
    execution_count: builtin.un_i_data(execution_count_data),
    execution_peaks: decode_frontier_peaks(execution_peaks_data),
    execution_cursor: builtin.un_i_data(execution_cursor_data),
    language_bitmap: builtin.un_i_data(language_bitmap_data),
    resolution_schedule_hash: builtin.un_b_data(resolution_schedule_hash_data),
  }
}

pub fn encode_native_scripts_control_v1(
  control: NativeScriptsControlV1,
) -> ByteArray {
  encode_definite_array_header(26)
    |> bytearray.concat(encode_definite_bytes(control.compact_cbor))
    |> bytearray.concat(encode_definite_bytes(control.witness_set_compact_cbor))
    |> bytearray.concat(
        encode_definite_bytes(control.field_preimage_lengths_cbor),
      )
    |> bytearray.concat(encode_definite_bytes(control.context_cbor))
    |> bytearray.concat(cbor.serialise(control.resolved_input_count))
    |> bytearray.concat(
        encode_definite_bytes(control.resolved_inputs_accumulator),
      )
    |> bytearray.concat(cbor.serialise(control.spend_input_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.resolved_item_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.signer_count))
    |> bytearray.concat(
        encode_definite_bytes(control.signer_frontier_commitment),
      )
    |> bytearray.concat(cbor.serialise(control.source_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.source_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.redeemer_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.redeemer_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.purpose_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.purpose_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.output_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.output_peaks),
      )
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.output_descriptor_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.mint_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.mint_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.execution_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.execution_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.execution_cursor))
    |> bytearray.concat(cbor.serialise(control.language_bitmap))
    |> bytearray.concat(encode_definite_bytes(control.resolution_schedule_hash))
}

fn native_resolved_context_is_well_formed(
  control: NativeScriptsControlV1,
) -> Bool {
  and {
    control.spend_input_count >= 0,
    control.spend_input_count <= control.resolved_input_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.resolved_input_count,
      control.resolved_item_peaks,
    ),
  }
}

fn native_scripts_control_is_well_formed(
  control: NativeScriptsControlV1,
) -> Bool {
  and {
    control.resolved_input_count >= 0,
    bytearray.length(control.resolved_inputs_accumulator) == 32,
    bytearray.length(control.resolution_schedule_hash) == 32,
    native_resolved_context_is_well_formed(control),
    control.signer_count >= 0,
    bytearray.length(control.signer_frontier_commitment) == 32,
    validation_merkle_v1.frontier_is_well_formed(
      control.source_count,
      control.source_peaks,
    ),
    validation_merkle_v1.frontier_is_well_formed(
      control.redeemer_count,
      control.redeemer_peaks,
    ),
    validation_merkle_v1.frontier_is_well_formed(
      control.purpose_count,
      control.purpose_peaks,
    ),
    validation_merkle_v1.frontier_is_well_formed(
      control.output_count,
      control.output_peaks,
    ),
    validation_merkle_v1.frontier_is_well_formed(
      control.output_count,
      control.output_descriptor_peaks,
    ),
    control.mint_count >= 0,
    control.mint_count <= ledger_output_v1.max_distinct_asset_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.mint_count,
      control.mint_peaks,
    ),
    validation_merkle_v1.frontier_is_well_formed(
      control.execution_count,
      control.execution_peaks,
    ),
    control.execution_count == control.purpose_count,
    control.execution_cursor >= 0,
    control.execution_cursor <= control.execution_count,
    control.language_bitmap >= 0,
    control.language_bitmap <= 3,
  }
}

pub fn encode_script_integrity_witness_v1(
  control: NativeScriptsControlV1,
) -> ByteArray {
  #"82"
    |> bytearray.concat(
        encode_definite_bytes(encode_native_scripts_control_v1(control)),
      )
    |> bytearray.concat(cbor.serialise(0))
}

pub fn encode_script_integrity_compact_witness_v1(
  control: NativeScriptsControlV1,
) -> ByteArray {
  #"82"
    |> bytearray.concat(
        encode_definite_bytes(encode_native_scripts_control_v1(control)),
      )
    |> bytearray.concat(cbor.serialise(1))
}

pub fn encode_script_integrity_witness_set_witness_v1(
  control: NativeScriptsControlV1,
  script_integrity_hash: ByteArray,
  witness_set_hash: ByteArray,
) -> ByteArray {
  expect bytearray.length(script_integrity_hash) == 32
  expect bytearray.length(witness_set_hash) == 32
  #"84"
    |> bytearray.concat(
        encode_definite_bytes(encode_native_scripts_control_v1(control)),
      )
    |> bytearray.concat(cbor.serialise(2))
    |> bytearray.concat(encode_definite_bytes(script_integrity_hash))
    |> bytearray.concat(encode_definite_bytes(witness_set_hash))
}

pub fn encode_script_integrity_finalize_witness_v1(
  control: NativeScriptsControlV1,
  script_integrity_hash: ByteArray,
  redeemer_tx_wits_hash: ByteArray,
) -> ByteArray {
  expect bytearray.length(script_integrity_hash) == 32
  expect bytearray.length(redeemer_tx_wits_hash) == 32
  #"84"
    |> bytearray.concat(
        encode_definite_bytes(encode_native_scripts_control_v1(control)),
      )
    |> bytearray.concat(cbor.serialise(3))
    |> bytearray.concat(encode_definite_bytes(script_integrity_hash))
    |> bytearray.concat(encode_definite_bytes(redeemer_tx_wits_hash))
}

fn script_integrity_control_and_stage(
  work_witness_cbor: ByteArray,
) -> Pair<NativeScriptsControlV1, Int> {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let fields = builtin.un_list_data(data)
  expect Some(native_control_cbor_data) = list.at(fields, 0)
  expect Some(stage_data) = list.at(fields, 1)
  Pair(
    native_scripts_control_from_witness(
      builtin.un_b_data(native_control_cbor_data),
    ),
    builtin.un_i_data(stage_data),
  )
}

fn native_scripts_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: NativeScriptsControlV1,
  next_language_bitmap: Int,
) -> Bool {
  let next_control = NativeScriptsControlV1 {
    ..control,
    execution_cursor: control.execution_cursor + 1,
    language_bitmap: next_language_bitmap,
  }
  and {
    witness.claimed_successor.phase == validation_trace_v1.NativeScripts,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.NativeScripts,
      pre.program_counter + 1,
      encode_native_scripts_control_v1(next_control),
    ),
  }
}

fn native_language_tag_matches_script(
  language_tag: Int,
  script: MidgardScriptWitness,
) -> Bool {
  language_tag == script_proof_v1.language_tag(script.language)
}

fn verify_native_execution_descriptor_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: NativeScriptsControlV1,
) -> Bool {
  expect
      NativeExecutionDescriptorWitness {
        execution_index,
        language_tag,
        purpose_kind,
        purpose_index,
        script_hash,
        subject,
        purpose_siblings,
        source_index,
        origin_kind,
        source_key,
        script_total_length,
        script_item_commitment,
        source_siblings,
        redeemer_leaf,
        execution_siblings,
        first_chunk_proof,
        signer_peaks,
      }
    = auxiliary
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    purpose_kind,
    purpose_index,
    script_hash,
    subject,
  )
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    origin_kind,
    source_key,
    language_tag,
    script_hash,
    script_total_length,
    script_item_commitment,
  )
  let execution_leaf = script_proof_v1.execution_leaf_hash(
    language_tag,
    purpose_leaf,
    source_leaf,
    redeemer_leaf,
  )
  if and {
    execution_index == control.execution_cursor,
    control.execution_count == control.purpose_count,
    script_total_length > 0,
    script_total_length <= max_aggregate_field_preimage_bytes,
    bytearray.length(script_item_commitment) == 32,
    validation_merkle_v1.verify_membership(
      control.purpose_count,
      control.purpose_peaks,
      execution_index,
      purpose_leaf,
      purpose_siblings,
    ),
    validation_merkle_v1.verify_membership(
      control.source_count,
      control.source_peaks,
      source_index,
      source_leaf,
      source_siblings,
    ),
    validation_merkle_v1.verify_membership(
      control.execution_count,
      control.execution_peaks,
      execution_index,
      execution_leaf,
      execution_siblings,
    ),
  } {
    if language_tag == 0 {
      expect Some(chunk_proof) = first_chunk_proof
      expect Some(header) = native_script_scan_v1.versioned_script_header_v1(
        chunk_proof.chunk,
        script_total_length,
      )
      let continuation_cbor = encode_native_scripts_control_v1(control)
      and {
        redeemer_leaf == #"",
        chunk_proof.chunk_index == 0,
        chunk_proof.total_length == script_total_length,
        bounded_item_v1.verify_chunk(script_item_commitment, chunk_proof),
        header.language_tag == 0,
        validation_merkle_v1.frontier_is_well_formed(
          control.signer_count,
          signer_peaks,
        ),
        validation_merkle_v1.frontier_commitment(
          control.signer_count,
          signer_peaks,
        ) == control.signer_frontier_commitment,
        witness.claimed_successor.phase == validation_trace_v1.PhaseANativeScripts,
        witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.PhaseANativeScripts,
          pre.program_counter + 1,
          encode_phase_a_native_scripts_scan_witness(
            control.compact_cbor,
            control.witness_set_compact_cbor,
            control.field_preimage_lengths_cbor,
            control.context_cbor,
            blake2b_256(continuation_cbor),
            1,
            1,
            0,
            0,
            script_total_length,
            script_item_commitment,
            header.payload_offset,
            #"",
            0,
            0,
            -1,
            control.signer_count,
            signer_peaks,
            continuation_cbor,
          ),
        ),
      }
    } else if and {
      or {
        language_tag == 3,
        language_tag == 128,
      },
      bytearray.length(redeemer_leaf) == 32,
      first_chunk_proof == None,
      signer_peaks == [],
    } {
      native_scripts_successor_is_exact(
        pre,
        witness,
        control,
        if language_tag == 3 {
          control.language_bitmap + if control.language_bitmap % 2 == 0 {
            1
          } else {
            0
          }
        } else {
          control.language_bitmap + if control.language_bitmap < 2 {
            2
          } else {
            0
          }
        },
      )
    } else {
      False
    }
  } else {
    False
  }
}

fn verify_native_scripts(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let control = native_scripts_control_from_witness(witness.work_witness_cbor)
  let Pair(verified_source, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  and {
    verified_source.version == 1,
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    native_scripts_control_is_well_formed(control),
    witness.work_witness_cbor == encode_native_scripts_control_v1(control),
    if control.execution_cursor == control.execution_count {
      and {
        auxiliary == NoAuxiliaryWitness,
        witness.claimed_successor.phase == validation_trace_v1.ScriptIntegrity,
        witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.ScriptIntegrity,
          pre.program_counter + 1,
          encode_script_integrity_witness_v1(control),
        ),
      }
    } else {
      verify_native_execution_descriptor_step(pre, witness, auxiliary, control)
    },
  }
}

pub fn verify_native_scripts_terminal_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  verify_native_scripts(pre, witness, NoAuxiliaryWitness)
}

pub fn verify_native_scripts_native_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  execution_index: Int,
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
  signer_peaks: List<validation_merkle_v1.FrontierPeak>,
) -> Bool {
  verify_native_scripts(
    pre,
    witness,
    NativeExecutionDescriptorWitness {
      execution_index,
      language_tag: 0,
      purpose_kind,
      purpose_index,
      script_hash,
      subject,
      purpose_siblings,
      source_index,
      origin_kind,
      source_key,
      script_total_length,
      script_item_commitment,
      source_siblings,
      redeemer_leaf,
      execution_siblings,
      first_chunk_proof: Some(first_chunk_proof),
      signer_peaks,
    },
  )
}

pub fn verify_native_scripts_effectful_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
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
) -> Bool {
  verify_native_scripts(
    pre,
    witness,
    NativeExecutionDescriptorWitness {
      execution_index,
      language_tag,
      purpose_kind,
      purpose_index,
      script_hash,
      subject,
      purpose_siblings,
      source_index,
      origin_kind,
      source_key,
      script_total_length,
      script_item_commitment,
      source_siblings,
      redeemer_leaf,
      execution_siblings,
      first_chunk_proof: None,
      signer_peaks: [],
    },
  )
}

fn encode_data_sequence_summary_v1(
  summary: cek_data_v1.DataSequenceSummaryV1,
) -> ByteArray {
  #"84"
    |> bytearray.concat(encode_definite_bytes(summary.root))
    |> bytearray.concat(cbor.serialise(summary.length))
    |> bytearray.concat(cbor.serialise(summary.payload_cbor_length))
    |> bytearray.concat(cbor.serialise(summary.memory))
}

fn data_sequence_summary_from_data(
  data: Data,
) -> cek_data_v1.DataSequenceSummaryV1 {
  expect [root_data,
        length_data, payload_data, memory_data] = builtin.un_list_data(data)
  cek_data_v1.DataSequenceSummaryV1 {
    root: builtin.un_b_data(root_data),
    length: builtin.un_i_data(length_data),
    payload_cbor_length: builtin.un_i_data(payload_data),
    memory: builtin.un_i_data(memory_data),
  }
}

fn encode_data_summary_v1(summary: cek_data_v1.DataSummaryV1) -> ByteArray {
  #"83"
    |> bytearray.concat(encode_definite_bytes(summary.root))
    |> bytearray.concat(cbor.serialise(summary.cbor_length))
    |> bytearray.concat(cbor.serialise(summary.memory))
}

fn data_summary_from_data(data: Data) -> cek_data_v1.DataSummaryV1 {
  expect [root_data, cbor_length_data, memory_data] = builtin.un_list_data(data)
  cek_data_v1.DataSummaryV1 {
    root: builtin.un_b_data(root_data),
    cbor_length: builtin.un_i_data(cbor_length_data),
    memory: builtin.un_i_data(memory_data),
  }
}

fn empty_data_summary_v1() -> cek_data_v1.DataSummaryV1 {
  cek_data_v1.DataSummaryV1 { root: #"", cbor_length: 0, memory: 0 }
}

pub type CekContextControlV1 {
  stage: Int,
  language_tag: Int,
  program_term_root: ByteArray,
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

pub fn encode_cek_context_control_v1(control: CekContextControlV1) -> ByteArray {
  encode_definite_array_header(24)
    |> bytearray.concat(cbor.serialise(control.stage))
    |> bytearray.concat(cbor.serialise(control.language_tag))
    |> bytearray.concat(encode_definite_bytes(control.program_term_root))
    |> bytearray.concat(cbor.serialise(control.purpose_kind))
    |> bytearray.concat(cbor.serialise(control.purpose_index))
    |> bytearray.concat(encode_definite_bytes(control.script_hash))
    |> bytearray.concat(encode_definite_bytes(control.subject))
    |> bytearray.concat(encode_definite_bytes(control.redeemer_leaf))
    |> bytearray.concat(
        encode_definite_bytes(control.redeemer_context_control_hash),
      )
    |> bytearray.concat(cbor.serialise(control.execution_memory_limit))
    |> bytearray.concat(cbor.serialise(control.execution_cpu_limit))
    |> bytearray.concat(
        encode_data_sequence_summary_v1(control.reference_items),
      )
    |> bytearray.concat(encode_data_sequence_summary_v1(control.spend_items))
    |> bytearray.concat(encode_data_sequence_summary_v1(control.output_items))
    |> bytearray.concat(encode_data_sequence_summary_v1(control.signer_items))
    |> bytearray.concat(cbor.serialise(control.observer_count))
    |> bytearray.concat(encode_data_sequence_summary_v1(control.observer_items))
    |> bytearray.concat(encode_definite_bytes(control.previous_observer))
    |> bytearray.concat(encode_data_summary_v1(control.observer_summary))
    |> bytearray.concat(cbor.serialise(control.mint_cursor))
    |> bytearray.concat(encode_definite_bytes(control.current_mint_policy))
    |> bytearray.concat(
        encode_data_sequence_summary_v1(control.current_mint_assets),
      )
    |> bytearray.concat(encode_data_sequence_summary_v1(control.mint_policies))
    |> bytearray.concat(encode_data_summary_v1(control.mint_summary))
}

fn cek_context_control_from_cbor(control_cbor: ByteArray) -> CekContextControlV1 {
  expect Some(data) = cbor.deserialise(control_cbor)
  expect
      [
        stage_data, language_tag_data, program_term_root_data, purpose_kind_data,
        purpose_index_data, script_hash_data, subject_data, redeemer_leaf_data,
        redeemer_context_control_hash_data, execution_memory_limit_data,
        execution_cpu_limit_data, reference_items_data, spend_items_data,
        output_items_data, signer_items_data, observer_count_data,
        observer_items_data, previous_observer_data, observer_summary_data,
        mint_cursor_data, current_mint_policy_data, current_mint_assets_data,
        mint_policies_data, mint_summary_data,
      ]
    = builtin.un_list_data(data)
  CekContextControlV1 {
    stage: builtin.un_i_data(stage_data),
    language_tag: builtin.un_i_data(language_tag_data),
    program_term_root: builtin.un_b_data(program_term_root_data),
    purpose_kind: builtin.un_i_data(purpose_kind_data),
    purpose_index: builtin.un_i_data(purpose_index_data),
    script_hash: builtin.un_b_data(script_hash_data),
    subject: builtin.un_b_data(subject_data),
    redeemer_leaf: builtin.un_b_data(redeemer_leaf_data),
    redeemer_context_control_hash: builtin.un_b_data(
      redeemer_context_control_hash_data,
    ),
    execution_memory_limit: builtin.un_i_data(execution_memory_limit_data),
    execution_cpu_limit: builtin.un_i_data(execution_cpu_limit_data),
    reference_items: data_sequence_summary_from_data(reference_items_data),
    spend_items: data_sequence_summary_from_data(spend_items_data),
    output_items: data_sequence_summary_from_data(output_items_data),
    signer_items: data_sequence_summary_from_data(signer_items_data),
    observer_count: builtin.un_i_data(observer_count_data),
    observer_items: data_sequence_summary_from_data(observer_items_data),
    previous_observer: builtin.un_b_data(previous_observer_data),
    observer_summary: data_summary_from_data(observer_summary_data),
    mint_cursor: builtin.un_i_data(mint_cursor_data),
    current_mint_policy: builtin.un_b_data(current_mint_policy_data),
    current_mint_assets: data_sequence_summary_from_data(
      current_mint_assets_data,
    ),
    mint_policies: data_sequence_summary_from_data(mint_policies_data),
    mint_summary: data_summary_from_data(mint_summary_data),
  }
}

fn data_sequence_summary_is_well_formed(
  summary: cek_data_v1.DataSequenceSummaryV1,
) -> Bool {
  and {
    bytearray.length(summary.root) == 32,
    summary.length >= 0,
    summary.payload_cbor_length >= 0,
    summary.memory >= 0,
  }
}

fn optional_data_summary_is_well_formed(
  summary: cek_data_v1.DataSummaryV1,
) -> Bool {
  if summary.root == #"" {
    and {
      summary.cbor_length == 0,
      summary.memory == 0,
    }
  } else {
    and {
      bytearray.length(summary.root) == 32,
      summary.cbor_length >= 0,
      summary.memory >= 0,
    }
  }
}

const cek_redeemer_context_domain =
  #"4d69646761726443656b52656465656d6572436f6e74657874436f6e74726f6c5631"

const cek_final_context_domain =
  #"4d69646761726443656b46696e616c436f6e74657874436f6e74726f6c5631"

const cek_context_parts_domain =
  #"4d69646761726443656b436f6e746578745061727473436f6e74726f6c5631"

const cek_tx_info_assembly_domain =
  #"4d69646761726443656b5478496e666f417373656d626c79436f6e74726f6c5631"

pub fn encode_cek_redeemer_context_control_v1(
  control: CekRedeemerContextControlV1,
) -> ByteArray {
  encode_definite_array_header(6)
    |> bytearray.concat(cbor.serialise(control.cursor))
    |> bytearray.concat(encode_data_sequence_summary_v1(control.map_items))
    |> bytearray.concat(encode_definite_bytes(control.active_scan_hash))
    |> bytearray.concat(encode_definite_bytes(control.active_redeemer_leaf))
    |> bytearray.concat(encode_data_summary_v1(control.active_purpose))
    |> bytearray.concat(encode_data_summary_v1(control.current_redeemer))
}

pub fn hash_cek_redeemer_context_control_v1(
  control: CekRedeemerContextControlV1,
) -> ByteArray {
  cek_redeemer_context_domain
    |> bytearray.concat(encode_cek_redeemer_context_control_v1(control))
    |> blake2b_256
}

fn cek_redeemer_context_control_is_well_formed(
  redeemer_count: Int,
  control: CekRedeemerContextControlV1,
) -> Bool {
  and {
    control.cursor >= 0,
    control.cursor <= redeemer_count,
    data_sequence_summary_is_well_formed(control.map_items),
    control.map_items.length <= control.cursor,
    optional_data_summary_is_well_formed(control.active_purpose),
    optional_data_summary_is_well_formed(control.current_redeemer),
    if control.active_scan_hash == #"" {
      and {
        control.active_redeemer_leaf == #"",
        control.active_purpose == empty_data_summary_v1(),
      }
    } else {
      and {
        control.cursor < redeemer_count,
        bytearray.length(control.active_scan_hash) == 32,
        bytearray.length(control.active_redeemer_leaf) == 32,
        control.active_purpose == empty_data_summary_v1() || bytearray.length(
          control.active_purpose.root,
        ) == 32,
      }
    },
  }
}

pub fn initial_cek_redeemer_context_control_v1() -> CekRedeemerContextControlV1 {
  CekRedeemerContextControlV1 {
    cursor: 0,
    map_items: cek_data_v1.empty_data_pair_summary_v1(),
    active_scan_hash: #"",
    active_redeemer_leaf: #"",
    active_purpose: empty_data_summary_v1(),
    current_redeemer: empty_data_summary_v1(),
  }
}

pub fn encode_cek_final_context_control_v1(
  control: CekFinalContextControlV1,
) -> ByteArray {
  encode_definite_array_header(3)
    |> bytearray.concat(encode_data_summary_v1(control.tx_info))
    |> bytearray.concat(encode_data_summary_v1(control.redeemer))
    |> bytearray.concat(encode_data_summary_v1(control.script_info))
}

pub fn hash_cek_final_context_control_v1(
  control: CekFinalContextControlV1,
) -> ByteArray {
  expect optional_data_summary_is_well_formed(control.tx_info)
  expect optional_data_summary_is_well_formed(control.redeemer)
  expect optional_data_summary_is_well_formed(control.script_info)
  expect control.tx_info.root != #""
  expect control.redeemer.root != #""
  expect control.script_info.root != #""
  cek_final_context_domain
    |> bytearray.concat(encode_cek_final_context_control_v1(control))
    |> blake2b_256
}

pub fn encode_cek_context_parts_control_v1(
  control: CekContextPartsControlV1,
) -> ByteArray {
  encode_definite_array_header(3)
    |> bytearray.concat(encode_data_sequence_summary_v1(control.redeemer_items))
    |> bytearray.concat(encode_data_summary_v1(control.redeemer))
    |> bytearray.concat(encode_data_summary_v1(control.script_info))
}

pub fn hash_cek_context_parts_control_v1(
  control: CekContextPartsControlV1,
) -> ByteArray {
  expect data_sequence_summary_is_well_formed(control.redeemer_items)
  expect optional_data_summary_is_well_formed(control.redeemer)
  expect optional_data_summary_is_well_formed(control.script_info)
  expect control.redeemer.root != #""
  expect control.script_info.root != #""
  cek_context_parts_domain
    |> bytearray.concat(encode_cek_context_parts_control_v1(control))
    |> blake2b_256
}

pub fn encode_cek_tx_info_assembly_control_v1(
  control: CekTxInfoAssemblyControlV1,
) -> ByteArray {
  encode_definite_array_header(3)
    |> bytearray.concat(encode_data_sequence_summary_v1(control.tail_fields))
    |> bytearray.concat(encode_data_summary_v1(control.redeemer))
    |> bytearray.concat(encode_data_summary_v1(control.script_info))
}

pub fn hash_cek_tx_info_assembly_control_v1(
  control: CekTxInfoAssemblyControlV1,
) -> ByteArray {
  expect data_sequence_summary_is_well_formed(control.tail_fields)
  expect optional_data_summary_is_well_formed(control.redeemer)
  expect optional_data_summary_is_well_formed(control.script_info)
  expect control.tail_fields.length > 0
  expect control.redeemer.root != #""
  expect control.script_info.root != #""
  cek_tx_info_assembly_domain
    |> bytearray.concat(encode_cek_tx_info_assembly_control_v1(control))
    |> blake2b_256
}

fn mint_context_is_pristine(control: CekContextControlV1) -> Bool {
  and {
    control.mint_cursor == 0,
    control.current_mint_policy == #"",
    control.current_mint_assets == cek_data_v1.empty_data_pair_summary_v1(),
    control.mint_policies == cek_data_v1.empty_data_pair_summary_v1(),
    control.mint_summary == empty_data_summary_v1(),
  }
}

fn empty_observer_items_v1(
  language_tag: Int,
) -> cek_data_v1.DataSequenceSummaryV1 {
  if language_tag == 128 {
    cek_data_v1.empty_data_list_summary_v1()
  } else {
    cek_data_v1.empty_data_pair_summary_v1()
  }
}

fn observer_context_is_pristine(control: CekContextControlV1) -> Bool {
  and {
    control.observer_count == 0,
    control.observer_items == empty_observer_items_v1(control.language_tag),
    control.previous_observer == #"",
    control.observer_summary == empty_data_summary_v1(),
  }
}

fn observer_context_is_partial(control: CekContextControlV1) -> Bool {
  and {
    control.observer_count >= 0,
    control.observer_count <= max_tx_size_derived_collection_item_count,
    control.observer_items.length <= control.observer_count,
    control.observer_summary == empty_data_summary_v1(),
    if control.observer_items.length == 0 {
      and {
        control.observer_count == 0,
        control.observer_items == empty_observer_items_v1(control.language_tag),
        control.previous_observer == #"",
      }
    } else {
      and {
        control.observer_count > 0,
        bytearray.length(control.previous_observer) == 28,
      }
    },
  }
}

fn observer_context_is_complete(control: CekContextControlV1) -> Bool {
  and {
    control.observer_count >= 0,
    control.observer_count <= max_tx_size_derived_collection_item_count,
    control.observer_items.length == control.observer_count,
    bytearray.length(control.observer_summary.root) == 32,
    control.observer_summary == finalize_cek_observer_items_v1(
      control.observer_items,
      control.language_tag == 128,
    ),
    if control.observer_count == 0 {
      and {
        control.observer_items == empty_observer_items_v1(control.language_tag),
        control.previous_observer == #"",
      }
    } else {
      bytearray.length(control.previous_observer) == 28
    },
  }
}

fn redeemer_data_scan_is_initial(control: CekContextControlV1) -> Bool {
  bytearray.length(control.redeemer_context_control_hash) == 32
}

fn redeemer_data_scan_is_complete(control: CekContextControlV1) -> Bool {
  bytearray.length(control.redeemer_context_control_hash) == 32
}

fn mint_context_is_final(
  native_control: NativeScriptsControlV1,
  control: CekContextControlV1,
) -> Bool {
  and {
    observer_context_is_complete(control),
    control.mint_cursor == native_control.mint_count,
    control.current_mint_policy == #"",
    control.current_mint_assets == cek_data_v1.empty_data_pair_summary_v1(),
    bytearray.length(control.mint_summary.root) == 32,
  }
}

fn cek_context_control_is_well_formed(
  native_control: NativeScriptsControlV1,
  control: CekContextControlV1,
) -> Bool {
  let reference_count = native_control.resolved_input_count - native_control.spend_input_count
  and {
    control.stage >= 0,
    control.stage <= 13,
    or {
      control.language_tag == 3,
      control.language_tag == 128,
    },
    bytearray.length(control.program_term_root) == 32,
    control.purpose_kind >= 0,
    control.purpose_kind <= 3,
    control.purpose_index >= 0,
    bytearray.length(control.script_hash) == 28,
    bytearray.length(control.redeemer_leaf) == 32,
    or {
      control.redeemer_context_control_hash == #"",
      bytearray.length(control.redeemer_context_control_hash) == 32,
    },
    control.execution_memory_limit >= 0,
    control.execution_cpu_limit >= 0,
    data_sequence_summary_is_well_formed(control.reference_items),
    data_sequence_summary_is_well_formed(control.spend_items),
    data_sequence_summary_is_well_formed(control.output_items),
    data_sequence_summary_is_well_formed(control.signer_items),
    data_sequence_summary_is_well_formed(control.observer_items),
    optional_data_summary_is_well_formed(control.observer_summary),
    control.mint_cursor >= 0,
    control.mint_cursor <= native_control.mint_count,
    or {
      control.current_mint_policy == #"",
      bytearray.length(control.current_mint_policy) == 28,
    },
    data_sequence_summary_is_well_formed(control.current_mint_assets),
    data_sequence_summary_is_well_formed(control.mint_policies),
    optional_data_summary_is_well_formed(control.mint_summary),
    control.reference_items.length <= reference_count,
    control.spend_items.length <= native_control.spend_input_count,
    control.output_items.length <= native_control.output_count,
    if control.stage == 0 {
      and {
        control.execution_memory_limit == 0,
        control.execution_cpu_limit == 0,
        control.reference_items.length == 0,
        control.spend_items.length == 0,
        control.output_items.length == 0,
        control.signer_items.length == 0,
        observer_context_is_pristine(control),
        mint_context_is_pristine(control),
      }
    } else if control.stage == 1 {
      and {
        redeemer_data_scan_is_initial(control),
        control.spend_items.length == 0,
        control.output_items.length == 0,
        control.signer_items.length == 0,
        observer_context_is_pristine(control),
        mint_context_is_pristine(control),
      }
    } else if control.stage == 2 {
      and {
        redeemer_data_scan_is_initial(control),
        control.reference_items.length == reference_count,
        control.output_items.length == 0,
        control.signer_items.length == 0,
        observer_context_is_pristine(control),
        mint_context_is_pristine(control),
      }
    } else if control.stage == 3 {
      and {
        redeemer_data_scan_is_initial(control),
        control.reference_items.length == reference_count,
        control.spend_items.length == native_control.spend_input_count,
        control.signer_items.length == 0,
        observer_context_is_pristine(control),
        mint_context_is_pristine(control),
      }
    } else if control.stage == 4 {
      and {
        redeemer_data_scan_is_initial(control),
        control.reference_items.length == reference_count,
        control.spend_items.length == native_control.spend_input_count,
        control.output_items.length == native_control.output_count,
        control.signer_items.length <= native_control.signer_count,
        observer_context_is_pristine(control),
        mint_context_is_pristine(control),
      }
    } else if control.stage == 5 {
      and {
        redeemer_data_scan_is_initial(control),
        control.signer_items.length == native_control.signer_count,
        observer_context_is_partial(control),
        mint_context_is_pristine(control),
      }
    } else if control.stage == 6 {
      and {
        redeemer_data_scan_is_initial(control),
        control.signer_items.length == native_control.signer_count,
        observer_context_is_complete(control),
        mint_context_is_pristine(control),
      }
    } else if control.stage == 7 {
      False
    } else if control.stage == 8 {
      and {
        redeemer_data_scan_is_initial(control),
        control.signer_items.length == native_control.signer_count,
        observer_context_is_complete(control),
        native_control.mint_count > 0,
        if control.mint_cursor == 0 {
          and {
            control.current_mint_policy == #"",
            control.current_mint_assets == cek_data_v1.empty_data_pair_summary_v1(),
            control.mint_policies == cek_data_v1.empty_data_pair_summary_v1(),
          }
        } else {
          and {
            bytearray.length(control.current_mint_policy) == 28,
            control.current_mint_assets.length > 0,
          }
        },
        control.mint_summary.root == #"",
      }
    } else if control.stage == 9 {
      and {
        control.signer_items.length == native_control.signer_count,
        mint_context_is_final(native_control, control),
        redeemer_data_scan_is_initial(control),
      }
    } else {
      and {
        control.signer_items.length == native_control.signer_count,
        mint_context_is_final(native_control, control),
        redeemer_data_scan_is_complete(control),
      }
    },
  }
}

pub fn encode_cek_witness_v1(
  native_control: NativeScriptsControlV1,
  context_control_cbor: ByteArray,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
  active_state_hash: ByteArray,
  execution_cpu_limit: Int,
  execution_memory_limit: Int,
) -> ByteArray {
  expect execution_cursor >= 0
  expect execution_cursor <= native_control.execution_count
  expect completed_cpu >= 0
  expect completed_memory >= 0
  expect or {
      active_state_hash == #"",
      bytearray.length(active_state_hash) == 32,
    }
  expect execution_cpu_limit >= 0
  expect execution_memory_limit >= 0
  #"88"
    |> bytearray.concat(
        encode_definite_bytes(encode_native_scripts_control_v1(native_control)),
      )
    |> bytearray.concat(encode_definite_bytes(context_control_cbor))
    |> bytearray.concat(cbor.serialise(execution_cursor))
    |> bytearray.concat(cbor.serialise(completed_cpu))
    |> bytearray.concat(cbor.serialise(completed_memory))
    |> bytearray.concat(encode_definite_bytes(active_state_hash))
    |> bytearray.concat(cbor.serialise(execution_cpu_limit))
    |> bytearray.concat(cbor.serialise(execution_memory_limit))
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

const mint_asset_leaf_domain = #"4d6964676172644d696e7441737365744c6561665631"

const reject_value_not_preserved = #"455f56414c55455f4e4f545f505245534552564544"

const no_rejection_code_hash =
  #"0000000000000000000000000000000000000000000000000000000000000000"

const value_map_maximum_proof_steps = 16

pub fn encode_value_accumulator_v1(accumulator: ValueAccumulatorV1) -> ByteArray {
  #"84"
    |> bytearray.concat(cbor.serialise(accumulator.lovelace_delta))
    |> bytearray.concat(encode_definite_bytes(accumulator.asset_root))
    |> bytearray.concat(cbor.serialise(accumulator.seen_asset_count))
    |> bytearray.concat(cbor.serialise(accumulator.nonzero_asset_count))
}

fn value_accumulator_from_cbor(
  accumulator_cbor: ByteArray,
) -> ValueAccumulatorV1 {
  expect Some(data) = cbor.deserialise(accumulator_cbor)
  expect
      [
        lovelace_data, asset_root_data, seen_asset_count_data,
        nonzero_asset_count_data,
      ]
    = builtin.un_list_data(data)
  let accumulator = ValueAccumulatorV1 {
    lovelace_delta: builtin.un_i_data(lovelace_data),
    asset_root: builtin.un_b_data(asset_root_data),
    seen_asset_count: builtin.un_i_data(seen_asset_count_data),
    nonzero_asset_count: builtin.un_i_data(nonzero_asset_count_data),
  }
  expect bytearray.length(accumulator.asset_root) == 32
  expect accumulator.seen_asset_count >= 0
  expect
    accumulator.seen_asset_count <= ledger_output_v1.max_distinct_asset_count
  expect accumulator.nonzero_asset_count >= 0
  expect accumulator.nonzero_asset_count <= accumulator.seen_asset_count
  expect encode_value_accumulator_v1(accumulator) == accumulator_cbor
  accumulator
}

fn update_value_asset_delta_root(
  root: ByteArray,
  unit: ByteArray,
  quantity_delta: Int,
  mutation: ValueAssetMutationWitnessV1,
) -> Option<ByteArray> {
  if mutation.delta_was_present {
    let old_value = cbor.serialise(mutation.old_delta)
    let next_delta = mutation.old_delta + quantity_delta
    mpf_proof_v1.update_root(
      root,
      unit,
      old_value,
      cbor.serialise(next_delta),
      mutation.delta_proof,
    )
  } else {
    if mutation.old_delta != 0 {
      None
    } else {
      mpf_proof_v1.insert_root(
        root,
        unit,
        cbor.serialise(quantity_delta),
        mutation.delta_proof,
      )
    }
  }
}

fn apply_value_asset_mutation(
  accumulator: ValueAccumulatorV1,
  unit: ByteArray,
  quantity_delta: Int,
  mutation: ValueAssetMutationWitnessV1,
) -> ValueAccumulatorUpdateV1 {
  if or {
    bytearray.length(unit) < 28,
    bytearray.length(unit) > 60,
    quantity_delta == 0,
    !mpf_proof_v1.proof_has_at_most_steps(
      mutation.delta_proof,
      value_map_maximum_proof_steps,
    ),
  } {
    ValueAccumulatorMutationInvalid
  } else {
    when
      update_value_asset_delta_root(
        accumulator.asset_root,
        unit,
        quantity_delta,
        mutation,
      )
    is {
      None -> ValueAccumulatorMutationInvalid
      Some(next_asset_root) -> {
        let old_delta = if mutation.delta_was_present {
          mutation.old_delta
        } else {
          0
        }
        let next_delta = old_delta + quantity_delta
        let next_nonzero_count = if old_delta == 0 && next_delta != 0 {
          accumulator.nonzero_asset_count + 1
        } else if old_delta != 0 && next_delta == 0 {
          accumulator.nonzero_asset_count - 1
        } else {
          accumulator.nonzero_asset_count
        }
        if and {
          !mutation.delta_was_present,
          accumulator.seen_asset_count >= ledger_output_v1.max_distinct_asset_count,
        } {
          ValueAccumulatorAssetLimitExceeded
        } else if or {
          next_nonzero_count < 0,
          next_nonzero_count > accumulator.seen_asset_count + if mutation.delta_was_present {
            0
          } else {
            1
          },
        } {
          ValueAccumulatorMutationInvalid
        } else {
          ValueAccumulatorUpdated(
            ValueAccumulatorV1 {
              ..accumulator,
              asset_root: next_asset_root,
              seen_asset_count: accumulator.seen_asset_count + if mutation.delta_was_present {
                0
              } else {
                1
              },
              nonzero_asset_count: next_nonzero_count,
            },
          )
        }
      }
    }
  }
}

pub fn mint_asset_leaf_hash(
  policy_id: ByteArray,
  asset_name: ByteArray,
  quantity: Int,
) -> ByteArray {
  expect bytearray.length(policy_id) == 28
  expect bytearray.length(asset_name) <= 32
  expect quantity != 0
  mint_asset_leaf_domain
    |> bytearray.concat(encode_definite_bytes(policy_id))
    |> bytearray.concat(encode_definite_bytes(asset_name))
    |> bytearray.concat(cbor.serialise(quantity))
    |> blake2b_256
}

pub fn encode_value_and_mint_control_v1(
  control: ValueAndMintControlV1,
) -> ByteArray {
  encode_definite_array_header(12)
    |> bytearray.concat(
        encode_definite_bytes(
          encode_native_scripts_control_v1(control.native_control),
        ),
      )
    |> bytearray.concat(cbor.serialise(control.stage))
    |> bytearray.concat(encode_definite_bytes(control.replay_schedule_hash))
    |> bytearray.concat(cbor.serialise(control.replay_cursor))
    |> bytearray.concat(cbor.serialise(control.replay_asset_cursor))
    |> bytearray.concat(encode_definite_bytes(control.replay_value_hash))
    |> bytearray.concat(encode_definite_bytes(control.replay_accumulator))
    |> bytearray.concat(
        encode_definite_bytes(control.replay_remaining_schedule_hash),
      )
    |> bytearray.concat(cbor.serialise(control.output_cursor))
    |> bytearray.concat(cbor.serialise(control.output_asset_cursor))
    |> bytearray.concat(cbor.serialise(control.mint_cursor))
    |> bytearray.concat(
        encode_definite_bytes(
          encode_value_accumulator_v1(control.value_accumulator),
        ),
      )
}

fn value_and_mint_control_from_witness(
  work_witness_cbor: ByteArray,
) -> ValueAndMintControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  expect
      [
        native_control_cbor_data, stage_data, replay_schedule_hash_data,
        replay_cursor_data, replay_asset_cursor_data, replay_value_hash_data,
        replay_accumulator_data, replay_remaining_schedule_hash_data,
        output_cursor_data, output_asset_cursor_data, mint_cursor_data,
        value_accumulator_cbor_data,
      ]
    = builtin.un_list_data(data)
  ValueAndMintControlV1 {
    native_control: native_scripts_control_from_witness(
      builtin.un_b_data(native_control_cbor_data),
    ),
    stage: builtin.un_i_data(stage_data),
    replay_schedule_hash: builtin.un_b_data(replay_schedule_hash_data),
    replay_cursor: builtin.un_i_data(replay_cursor_data),
    replay_asset_cursor: builtin.un_i_data(replay_asset_cursor_data),
    replay_value_hash: builtin.un_b_data(replay_value_hash_data),
    replay_accumulator: builtin.un_b_data(replay_accumulator_data),
    replay_remaining_schedule_hash: builtin.un_b_data(
      replay_remaining_schedule_hash_data,
    ),
    output_cursor: builtin.un_i_data(output_cursor_data),
    output_asset_cursor: builtin.un_i_data(output_asset_cursor_data),
    mint_cursor: builtin.un_i_data(mint_cursor_data),
    value_accumulator: value_accumulator_from_cbor(
      builtin.un_b_data(value_accumulator_cbor_data),
    ),
  }
}

fn initial_value_accumulator() -> ValueAccumulatorV1 {
  ValueAccumulatorV1 {
    lovelace_delta: 0,
    asset_root: null_hash,
    seen_asset_count: 0,
    nonzero_asset_count: 0,
  }
}

pub fn encode_value_and_mint_witness_v1(
  native_control: NativeScriptsControlV1,
) -> ByteArray {
  encode_value_and_mint_control_v1(
    ValueAndMintControlV1 {
      native_control,
      stage: 0,
      replay_schedule_hash: empty_resolution_schedule_hash(),
      replay_cursor: 0,
      replay_asset_cursor: 0,
      replay_value_hash: no_rejection_code_hash,
      replay_accumulator: initial_resolution_accumulator(),
      replay_remaining_schedule_hash: empty_resolution_schedule_hash(),
      output_cursor: 0,
      output_asset_cursor: 0,
      mint_cursor: 0,
      value_accumulator: initial_value_accumulator(),
    },
  )
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

const ledger_delta_operation_domain =
  #"4d69646761726456616c69646174696f6e4c656467657244656c74614f7065726174696f6e5631"

pub fn ledger_delta_operation_leaf_hash(
  operation_kind: Int,
  key: ByteArray,
  value: ByteArray,
  descriptor: mpf_proof_fold_v1.ProofDescriptorV1,
) -> ByteArray {
  expect operation_kind == 0 || operation_kind == 1
  expect mpf_proof_fold_v1.descriptor_is_well_formed_v1(descriptor)
  expect
    if operation_kind == 0 {
      value == #""
    } else {
      True
    }
  ledger_delta_operation_domain
    |> bytearray.concat(cbor.serialise(operation_kind))
    |> bytearray.concat(encode_definite_bytes(key))
    |> bytearray.concat(encode_definite_bytes(value))
    |> bytearray.concat(
        mpf_proof_fold_v1.encode_proof_descriptor_v1(descriptor),
      )
    |> blake2b_256
}

fn ledger_delta_proof_descriptor_from_cbor(
  descriptor_cbor: ByteArray,
) -> mpf_proof_fold_v1.ProofDescriptorV1 {
  expect Some(data) = cbor.deserialise(descriptor_cbor)
  expect
      [
        version_data, frame_count_data, terminal_cursor_data,
        operation_peaks_data,
      ]
    = builtin.un_list_data(data)
  let descriptor = mpf_proof_fold_v1.ProofDescriptorV1 {
    version: builtin.un_i_data(version_data),
    frame_count: builtin.un_i_data(frame_count_data),
    terminal_cursor: builtin.un_i_data(terminal_cursor_data),
    peaks: decode_frontier_peaks(operation_peaks_data),
  }
  expect
    descriptor_cbor == mpf_proof_fold_v1.encode_proof_descriptor_v1(descriptor)
  descriptor
}

pub fn encode_ledger_delta_pending_mutation_v1(
  pending: LedgerDeltaPendingMutationV1,
) -> ByteArray {
  encode_definite_array_header(10)
    |> bytearray.concat(cbor.serialise(1))
    |> bytearray.concat(cbor.serialise(pending.authorization_stage))
    |> bytearray.concat(cbor.serialise(pending.operation_kind))
    |> bytearray.concat(encode_definite_bytes(pending.key))
    |> bytearray.concat(encode_definite_bytes(pending.value))
    |> bytearray.concat(
        encode_definite_bytes(
          mpf_proof_fold_v1.encode_proof_descriptor_v1(pending.descriptor),
        ),
      )
    |> bytearray.concat(cbor.serialise(pending.fold_control.next_frame_index))
    |> bytearray.concat(
        encode_definite_bytes(pending.fold_control.including_root),
      )
    |> bytearray.concat(
        encode_definite_bytes(pending.fold_control.excluding_root),
      )
    |> bytearray.concat(
        cbor.serialise(pending.fold_control.expected_next_cursor),
      )
}

fn ledger_delta_pending_mutation_from_cbor(
  pending_cbor: ByteArray,
) -> LedgerDeltaPendingMutationV1 {
  expect Some(data) = cbor.deserialise(pending_cbor)
  expect
      [
        version_data, authorization_stage_data, operation_kind_data, key_data,
        value_data, descriptor_cbor_data, next_frame_index_data,
        including_root_data, excluding_root_data, expected_next_cursor_data,
      ]
    = builtin.un_list_data(data)
  expect builtin.un_i_data(version_data) == 1
  let pending = LedgerDeltaPendingMutationV1 {
    authorization_stage: builtin.un_i_data(authorization_stage_data),
    operation_kind: builtin.un_i_data(operation_kind_data),
    key: builtin.un_b_data(key_data),
    value: builtin.un_b_data(value_data),
    descriptor: ledger_delta_proof_descriptor_from_cbor(
      builtin.un_b_data(descriptor_cbor_data),
    ),
    fold_control: mpf_proof_fold_v1.ProofFoldControlV1 {
      next_frame_index: builtin.un_i_data(next_frame_index_data),
      expected_next_cursor: builtin.un_i_data(expected_next_cursor_data),
      including_root: builtin.un_b_data(including_root_data),
      excluding_root: builtin.un_b_data(excluding_root_data),
    },
  }
  expect
    if pending.authorization_stage == 0 {
      and {
        pending.fold_control.next_frame_index == -1,
        pending.fold_control.expected_next_cursor == 0,
        pending.fold_control.including_root == #"",
        pending.fold_control.excluding_root == #"",
      }
    } else {
      pending.authorization_stage == 1
    }
  expect pending.operation_kind == 0 || pending.operation_kind == 1
  expect pending_cbor == encode_ledger_delta_pending_mutation_v1(pending)
  pending
}

pub fn encode_ledger_delta_control_v1(
  control: LedgerDeltaControlV1,
) -> ByteArray {
  encode_definite_array_header(14)
    |> bytearray.concat(cbor.serialise(control.resolved_input_count))
    |> bytearray.concat(
        encode_definite_bytes(control.resolved_inputs_accumulator),
      )
    |> bytearray.concat(cbor.serialise(control.output_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.output_descriptor_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.stage))
    |> bytearray.concat(encode_definite_bytes(control.replay_schedule_hash))
    |> bytearray.concat(cbor.serialise(control.replay_cursor))
    |> bytearray.concat(encode_definite_bytes(control.replay_accumulator))
    |> bytearray.concat(
        encode_definite_bytes(control.replay_remaining_schedule_hash),
      )
    |> bytearray.concat(encode_definite_bytes(control.current_ledger_root))
    |> bytearray.concat(cbor.serialise(control.output_cursor))
    |> bytearray.concat(cbor.serialise(control.operation_count))
    |> bytearray.concat(encode_definite_bytes(control.pending_mutation_cbor))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.operation_peaks),
      )
}

fn ledger_delta_control_from_witness(
  work_witness_cbor: ByteArray,
) -> LedgerDeltaControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  expect
      [
        resolved_input_count_data, resolved_inputs_accumulator_data,
        output_count_data, output_descriptor_peaks_data, stage_data,
        replay_schedule_hash_data, replay_cursor_data, replay_accumulator_data,
        replay_remaining_schedule_hash_data, current_ledger_root_data,
        output_cursor_data, operation_count_data, pending_mutation_cbor_data,
        operation_peaks_data,
      ]
    = builtin.un_list_data(data)
  LedgerDeltaControlV1 {
    resolved_input_count: builtin.un_i_data(resolved_input_count_data),
    resolved_inputs_accumulator: builtin.un_b_data(
      resolved_inputs_accumulator_data,
    ),
    output_count: builtin.un_i_data(output_count_data),
    output_descriptor_peaks: decode_frontier_peaks(output_descriptor_peaks_data),
    stage: builtin.un_i_data(stage_data),
    replay_schedule_hash: builtin.un_b_data(replay_schedule_hash_data),
    replay_cursor: builtin.un_i_data(replay_cursor_data),
    replay_accumulator: builtin.un_b_data(replay_accumulator_data),
    replay_remaining_schedule_hash: builtin.un_b_data(
      replay_remaining_schedule_hash_data,
    ),
    current_ledger_root: builtin.un_b_data(current_ledger_root_data),
    output_cursor: builtin.un_i_data(output_cursor_data),
    operation_count: builtin.un_i_data(operation_count_data),
    operation_peaks: decode_frontier_peaks(operation_peaks_data),
    pending_mutation_cbor: builtin.un_b_data(pending_mutation_cbor_data),
  }
}

pub fn encode_ledger_delta_witness_v1(
  native_control: NativeScriptsControlV1,
  prior_ledger_root: ByteArray,
  replay_schedule_hash: ByteArray,
) -> ByteArray {
  expect bytearray.length(prior_ledger_root) == 32
  expect bytearray.length(replay_schedule_hash) == 32
  encode_ledger_delta_control_v1(
    LedgerDeltaControlV1 {
      resolved_input_count: native_control.resolved_input_count,
      resolved_inputs_accumulator: native_control.resolved_inputs_accumulator,
      output_count: native_control.output_count,
      output_descriptor_peaks: native_control.output_descriptor_peaks,
      stage: 0,
      replay_schedule_hash,
      replay_cursor: 0,
      replay_accumulator: initial_resolution_accumulator(),
      replay_remaining_schedule_hash: replay_schedule_hash,
      current_ledger_root: prior_ledger_root,
      output_cursor: 0,
      operation_count: 0,
      operation_peaks: [],
      pending_mutation_cbor: #"",
    },
  )
}

pub fn encode_terminal_acceptance_witness_v1(
  post_ledger_root: ByteArray,
  operation_count: Int,
  operation_peaks: List<validation_merkle_v1.FrontierPeak>,
) -> ByteArray {
  expect bytearray.length(post_ledger_root) == 32
  expect
    validation_merkle_v1.frontier_is_well_formed(
      operation_count,
      operation_peaks,
    )
  let frontier_cbor = #"82"
    |> bytearray.concat(cbor.serialise(operation_count))
    |> bytearray.concat(validation_merkle_v1.encode_frontier(operation_peaks))
  #"84"
    |> bytearray.concat(cbor.serialise(1))
    |> bytearray.concat(encode_definite_bytes(#""))
    |> bytearray.concat(encode_definite_bytes(post_ledger_root))
    |> bytearray.concat(encode_definite_bytes(frontier_cbor))
}

fn verify_script_integrity_authentication(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let Pair(control, stage) = script_integrity_control_and_stage(
    witness.work_witness_cbor,
  )
  and {
    auxiliary == NoAuxiliaryWitness,
    stage == 0,
    control.execution_cursor == control.execution_count,
    native_resolved_context_is_well_formed(control),
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    witness.work_witness_cbor == encode_script_integrity_witness_v1(control),
    witness.claimed_successor.phase == validation_trace_v1.ScriptIntegrity,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptIntegrity,
      pre.program_counter + 1,
      encode_script_integrity_compact_witness_v1(control),
    ),
  }
}

fn verify_script_integrity_compact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let Pair(control, stage) = script_integrity_control_and_stage(
    witness.work_witness_cbor,
  )
  let verified_source = verify_native_tx_compact_cbor_v1(
    pre.transaction_id,
    control.compact_cbor,
  )
  and {
    auxiliary == NoAuxiliaryWitness,
    stage == 1,
    witness.work_witness_cbor == encode_script_integrity_compact_witness_v1(
      control,
    ),
    witness.claimed_successor.phase == validation_trace_v1.ScriptIntegrity,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptIntegrity,
      pre.program_counter + 1,
      encode_script_integrity_witness_set_witness_v1(
        control,
        verified_source.tx_compact.body.script_integrity_hash,
        verified_source.tx_compact.witness_set_hash,
      ),
    ),
  }
}

fn verify_script_integrity_witness_set(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  expect Some(data) = cbor.deserialise(witness.work_witness_cbor)
  expect
      [
        native_control_cbor_data, stage_data, script_integrity_hash_data,
        witness_set_hash_data,
      ]
    = builtin.un_list_data(data)
  let control = native_scripts_control_from_witness(
    builtin.un_b_data(native_control_cbor_data),
  )
  let stage = builtin.un_i_data(stage_data)
  let script_integrity_hash = builtin.un_b_data(script_integrity_hash_data)
  let witness_set_hash = builtin.un_b_data(witness_set_hash_data)
  let verified_witness_set = decode_native_tx_witness_set_compact(
    control.witness_set_compact_cbor,
  )
  and {
    auxiliary == NoAuxiliaryWitness,
    stage == 2,
    bytearray.length(script_integrity_hash) == 32,
    bytearray.length(witness_set_hash) == 32,
    blake2b_256(control.witness_set_compact_cbor) == witness_set_hash,
    witness.work_witness_cbor == encode_script_integrity_witness_set_witness_v1(
      control,
      script_integrity_hash,
      witness_set_hash,
    ),
    witness.claimed_successor.phase == validation_trace_v1.ScriptIntegrity,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptIntegrity,
      pre.program_counter + 1,
      encode_script_integrity_finalize_witness_v1(
        control,
        script_integrity_hash,
        verified_witness_set.redeemer_tx_wits_hash,
      ),
    ),
  }
}

fn verify_script_integrity_finalize(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  expect Some(data) = cbor.deserialise(witness.work_witness_cbor)
  expect
      [
        native_control_cbor_data, stage_data, script_integrity_hash_data,
        redeemer_tx_wits_hash_data,
      ]
    = builtin.un_list_data(data)
  let control = native_scripts_control_from_witness(
    builtin.un_b_data(native_control_cbor_data),
  )
  let stage = builtin.un_i_data(stage_data)
  let script_integrity_hash = builtin.un_b_data(script_integrity_hash_data)
  let redeemer_tx_wits_hash = builtin.un_b_data(redeemer_tx_wits_hash_data)
  let expected_hash = script_language_views_v1.expected_script_integrity_hash(
    redeemer_tx_wits_hash,
    control.language_bitmap,
  )
  and {
    auxiliary == NoAuxiliaryWitness,
    stage == 3,
    bytearray.length(script_integrity_hash) == 32,
    bytearray.length(redeemer_tx_wits_hash) == 32,
    witness.work_witness_cbor == encode_script_integrity_finalize_witness_v1(
      control,
      script_integrity_hash,
      redeemer_tx_wits_hash,
    ),
    if script_integrity_hash != expected_hash {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_invalid_field_type,
      )
    } else {
      and {
        witness.claimed_successor.phase == validation_trace_v1.Cek,
        witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.Cek,
          pre.program_counter + 1,
          encode_cek_witness_v1(control, #"", 0, 0, 0, #"", 0, 0),
        ),
      }
    },
  }
}

fn verify_script_integrity(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let Pair(_, stage) = script_integrity_control_and_stage(
    witness.work_witness_cbor,
  )
  if stage == 0 {
    verify_script_integrity_authentication(pre, witness, auxiliary)
  } else if stage == 1 {
    verify_script_integrity_compact(pre, witness, auxiliary)
  } else if stage == 2 {
    verify_script_integrity_witness_set(pre, witness, auxiliary)
  } else if stage == 3 {
    verify_script_integrity_finalize(pre, witness, auxiliary)
  } else {
    False
  }
}

fn cek_machine_state_hash(state: cek_machine_v1.MachineStateV1) -> ByteArray {
  cek_proof_v1.hash_machine_state_v1(
    state.mode,
    state.execution_index,
    state.focus_root,
    state.environment_root,
    state.continuation_root,
    state.auxiliary,
    state.cpu,
    state.memory,
  )
}

fn verify_cek_core_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: NativeScriptsControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
  active_state_hash: ByteArray,
  execution_cpu_limit: Int,
  execution_memory_limit: Int,
) -> Bool {
  when auxiliary is {
    CekCoreStepWitness { step } -> {
      let next_cpu = completed_cpu + step.post.cpu
      let next_memory = completed_memory + step.post.memory
      let budget_exceeded = or {
        step.post.cpu > execution_cpu_limit,
        step.post.memory > execution_memory_limit,
      }
      and {
        bytearray.length(active_state_hash) == 32,
        execution_cursor >= 0,
        execution_cursor < control.execution_count,
        execution_cpu_limit > 0,
        execution_memory_limit > 0,
        step.pre.execution_index == execution_cursor,
        cek_machine_state_hash(step.pre) == active_state_hash,
        pre.execution_cpu == completed_cpu + step.pre.cpu,
        pre.execution_memory == completed_memory + step.pre.memory,
        cek_machine_v1.verify_core_step_v1(step.pre, step.post, step.witness),
        if or {
          budget_exceeded,
          step.post.mode == cek_machine_v1.mode_halt_error,
        } {
          and {
            witness.claimed_successor.execution_cpu == next_cpu,
            witness.claimed_successor.execution_memory == next_memory,
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_plutus_script_invalid,
            ),
          }
        } else if step.post.mode == cek_machine_v1.mode_halt_success {
          let next_cursor = execution_cursor + 1
          and {
            step.post.cpu <= execution_cpu_limit,
            step.post.memory <= execution_memory_limit,
            witness.claimed_successor.execution_cpu == next_cpu,
            witness.claimed_successor.execution_memory == next_memory,
            if next_cursor == control.execution_count {
              and {
                witness.claimed_successor.phase == validation_trace_v1.ValueAndMint,
                witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
                  validation_trace_v1.ValueAndMint,
                  pre.program_counter + 1,
                  encode_value_and_mint_witness_v1(control),
                ),
              }
            } else {
              and {
                witness.claimed_successor.phase == validation_trace_v1.Cek,
                witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
                  validation_trace_v1.Cek,
                  pre.program_counter + 1,
                  encode_cek_witness_v1(
                    control,
                    #"",
                    next_cursor,
                    next_cpu,
                    next_memory,
                    #"",
                    0,
                    0,
                  ),
                ),
              }
            },
          }
        } else {
          and {
            witness.claimed_successor.phase == validation_trace_v1.Cek,
            witness.claimed_successor.execution_cpu == next_cpu,
            witness.claimed_successor.execution_memory == next_memory,
            witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
              validation_trace_v1.Cek,
              pre.program_counter + 1,
              encode_cek_witness_v1(
                control,
                #"",
                execution_cursor,
                completed_cpu,
                completed_memory,
                cek_machine_state_hash(step.post),
                execution_cpu_limit,
                execution_memory_limit,
              ),
            ),
          }
        },
      }
    }
    _ -> False
  }
}

fn cek_context_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  and {
    witness.claimed_successor.phase == validation_trace_v1.Cek,
    witness.claimed_successor.execution_cpu == pre.execution_cpu,
    witness.claimed_successor.execution_memory == pre.execution_memory,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      pre.program_counter + 1,
      encode_cek_witness_v1(
        native_control,
        encode_cek_context_control_v1(context_control),
        execution_cursor,
        completed_cpu,
        completed_memory,
        #"",
        0,
        0,
      ),
    ),
  }
}

fn cek_selection_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: NativeScriptsControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
  language_tag: Int,
  purpose_kind: Int,
  purpose_index: Int,
  script_hash: ByteArray,
  subject: ByteArray,
  redeemer_leaf: ByteArray,
  script: MidgardScriptWitness,
) -> Bool {
  if and {
    language_tag == 3,
    purpose_kind == 3,
  } {
    rejected_successor_is_exact(
      pre,
      witness.claimed_successor,
      reject_plutus_script_invalid,
    )
  } else if language_tag == 0 {
    let next_cursor = execution_cursor + 1
    if next_cursor == control.execution_count {
      and {
        witness.claimed_successor.phase == validation_trace_v1.ValueAndMint,
        witness.claimed_successor.execution_cpu == pre.execution_cpu,
        witness.claimed_successor.execution_memory == pre.execution_memory,
        witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.ValueAndMint,
          pre.program_counter + 1,
          encode_value_and_mint_witness_v1(control),
        ),
      }
    } else {
      and {
        witness.claimed_successor.phase == validation_trace_v1.Cek,
        witness.claimed_successor.execution_cpu == pre.execution_cpu,
        witness.claimed_successor.execution_memory == pre.execution_memory,
        witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.Cek,
          pre.program_counter + 1,
          encode_cek_witness_v1(
            control,
            #"",
            next_cursor,
            completed_cpu,
            completed_memory,
            #"",
            0,
            0,
          ),
        ),
      }
    }
  } else {
    when cek_proof_v1.inspect_program_envelope_v1(script.script_bytes) is {
      None -> False
      Some(envelope) ->
        cek_context_successor_is_exact(
          pre,
          witness,
          control,
          CekContextControlV1 {
            stage: 0,
            language_tag,
            program_term_root: envelope.term_root,
            purpose_kind,
            purpose_index,
            script_hash,
            subject,
            redeemer_leaf,
            redeemer_context_control_hash: #"",
            execution_memory_limit: 0,
            execution_cpu_limit: 0,
            reference_items: cek_data_v1.empty_data_list_summary_v1(),
            spend_items: cek_data_v1.empty_data_list_summary_v1(),
            output_items: cek_data_v1.empty_data_list_summary_v1(),
            signer_items: cek_data_v1.empty_data_list_summary_v1(),
            observer_count: 0,
            observer_items: empty_observer_items_v1(language_tag),
            previous_observer: #"",
            observer_summary: empty_data_summary_v1(),
            mint_cursor: 0,
            current_mint_policy: #"",
            current_mint_assets: cek_data_v1.empty_data_pair_summary_v1(),
            mint_policies: cek_data_v1.empty_data_pair_summary_v1(),
            mint_summary: empty_data_summary_v1(),
          },
          execution_cursor,
          completed_cpu,
          completed_memory,
        )
    }
  }
}

fn verify_cek_execution_selection(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: NativeScriptsControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  expect
      NativeExecutionScanWitness {
        execution_index,
        language_tag,
        purpose_kind,
        purpose_index,
        script_hash,
        subject,
        purpose_siblings,
        source_index,
        origin_kind,
        source_key,
        script,
        source_siblings,
        redeemer_leaf,
        execution_siblings,
        signer_hashes,
      }
    = auxiliary
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    purpose_kind,
    purpose_index,
    script_hash,
    subject,
  )
  let source_leaf = script_proof_v1.source_leaf_hash(
    origin_kind,
    source_key,
    script,
  )
  let execution_leaf = script_proof_v1.execution_leaf_hash(
    language_tag,
    purpose_leaf,
    source_leaf,
    redeemer_leaf,
  )
  and {
    execution_index == execution_cursor,
    execution_cursor < control.execution_count,
    signer_hashes == [],
    control.execution_count == control.purpose_count,
    script_proof_v1.versioned_script_hash(script) == script_hash,
    native_language_tag_matches_script(language_tag, script),
    if language_tag == 0 {
      redeemer_leaf == #""
    } else {
      bytearray.length(redeemer_leaf) == 32
    },
    validation_merkle_v1.verify_membership(
      control.purpose_count,
      control.purpose_peaks,
      execution_index,
      purpose_leaf,
      purpose_siblings,
    ),
    validation_merkle_v1.verify_membership(
      control.source_count,
      control.source_peaks,
      source_index,
      source_leaf,
      source_siblings,
    ),
    validation_merkle_v1.verify_membership(
      control.execution_count,
      control.execution_peaks,
      execution_index,
      execution_leaf,
      execution_siblings,
    ),
    cek_selection_successor_is_exact(
      pre,
      witness,
      control,
      execution_cursor,
      completed_cpu,
      completed_memory,
      language_tag,
      purpose_kind,
      purpose_index,
      script_hash,
      subject,
      redeemer_leaf,
      script,
    ),
  }
}

fn verify_cek_redeemer_selection(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  if context_control.redeemer_context_control_hash == #"" {
    expect
        RedeemerScanBeginWitness {
          item_index,
          item_count,
          total_length,
          item_commitment,
          siblings,
        }
      = auxiliary
    let leaf = script_proof_v1.redeemer_item_leaf_hash(
      item_index,
      item_commitment,
    )
    let item_control = redeemer_item_proof_v1.initial_control_v1(
      redeemer_item_proof_v1.mode_descriptor,
      item_index,
      item_count,
      total_length,
      item_commitment,
      purpose_kind_to_redeemer_tag(context_control.purpose_kind),
      context_control.purpose_index,
    )
    and {
      context_control.stage == 0,
      item_count == native_control.redeemer_count,
      leaf == context_control.redeemer_leaf,
      validation_merkle_v1.verify_membership(
        native_control.redeemer_count,
        native_control.redeemer_peaks,
        item_index,
        leaf,
        siblings,
      ),
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 {
          ..context_control,
          redeemer_context_control_hash: redeemer_item_proof_v1.hash_control_v1(
            item_control,
          ),
        },
        execution_cursor,
        completed_cpu,
        completed_memory,
      ),
    }
  } else {
    expect
        RedeemerItemStepWitness {
          redeemer_control: None,
          control: item_control,
          witness: item_witness,
        }
      = auxiliary
    let leaf = script_proof_v1.redeemer_item_leaf_hash(
      item_control.item_index,
      item_control.item_commitment,
    )
    let current_matches = and {
      context_control.stage == 0,
      item_control.mode == redeemer_item_proof_v1.mode_descriptor,
      item_control.item_count == native_control.redeemer_count,
      item_control.expected_purpose_tag == purpose_kind_to_redeemer_tag(
        context_control.purpose_kind,
      ),
      item_control.expected_pointer_index == context_control.purpose_index,
      leaf == context_control.redeemer_leaf,
      redeemer_item_proof_v1.hash_control_v1(item_control) == context_control.redeemer_context_control_hash,
    }
    when redeemer_item_proof_v1.step_v1(item_control, item_witness) is {
      None -> False
      Some(redeemer_item_proof_v1.RedeemerItemProofInvalid) -> False
      Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced { control: next }) ->
        if next.stage == redeemer_item_proof_v1.stage_terminal {
          let redeemer_context = initial_cek_redeemer_context_control_v1()
          and {
            current_matches,
            cek_context_successor_is_exact(
              pre,
              witness,
              native_control,
              CekContextControlV1 {
                ..context_control,
                stage: 1,
                redeemer_context_control_hash: hash_cek_redeemer_context_control_v1(
                  redeemer_context,
                ),
                execution_memory_limit: next.execution_memory,
                execution_cpu_limit: next.execution_steps,
              },
              execution_cursor,
              completed_cpu,
              completed_memory,
            ),
          }
        } else {
          and {
            current_matches,
            cek_context_successor_is_exact(
              pre,
              witness,
              native_control,
              CekContextControlV1 {
                ..context_control,
                redeemer_context_control_hash: redeemer_item_proof_v1.hash_control_v1(
                  next,
                ),
              },
              execution_cursor,
              completed_cpu,
              completed_memory,
            ),
          }
        }
    }
  }
}

fn verify_cek_reference_context_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  let reference_count = native_control.resolved_input_count - native_control.spend_input_count
  if context_control.reference_items.length == reference_count {
    and {
      auxiliary == NoAuxiliaryWitness,
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 { ..context_control, stage: 2 },
        execution_cursor,
        completed_cpu,
        completed_memory,
      ),
    }
  } else {
    expect
        CekResolvedContextItemWitness {
          source_kind,
          item_index,
          key,
          value,
          siblings,
        }
      = auxiliary
    expect Some(next_items) = script_context_v1.prepend_resolved_tx_in_info_v1(
      native_control.resolved_input_count,
      native_control.resolved_item_peaks,
      native_control.spend_input_count,
      source_kind,
      item_index,
      key,
      value,
      siblings,
      context_control.language_tag == 128,
      context_control.reference_items,
    )
    and {
      source_kind == 1,
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 { ..context_control, reference_items: next_items },
        execution_cursor,
        completed_cpu,
        completed_memory,
      ),
    }
  }
}

fn verify_cek_spend_context_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  if context_control.spend_items.length == native_control.spend_input_count {
    and {
      auxiliary == NoAuxiliaryWitness,
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 { ..context_control, stage: 3 },
        execution_cursor,
        completed_cpu,
        completed_memory,
      ),
    }
  } else {
    expect
        CekResolvedContextItemWitness {
          source_kind,
          item_index,
          key,
          value,
          siblings,
        }
      = auxiliary
    expect Some(next_items) = script_context_v1.prepend_resolved_tx_in_info_v1(
      native_control.resolved_input_count,
      native_control.resolved_item_peaks,
      native_control.spend_input_count,
      source_kind,
      item_index,
      key,
      value,
      siblings,
      context_control.language_tag == 128,
      context_control.spend_items,
    )
    and {
      source_kind == 0,
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 { ..context_control, spend_items: next_items },
        execution_cursor,
        completed_cpu,
        completed_memory,
      ),
    }
  }
}

fn verify_cek_output_context_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  if context_control.output_items.length == native_control.output_count {
    and {
      auxiliary == NoAuxiliaryWitness,
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 { ..context_control, stage: 4 },
        execution_cursor,
        completed_cpu,
        completed_memory,
      ),
    }
  } else {
    expect CekOutputContextItemWitness { output_index, output_cbor, siblings } = auxiliary
    expect Some(next_items) = script_context_v1.prepend_output_v1(
      native_control.output_count,
      native_control.output_peaks,
      output_index,
      output_cbor,
      siblings,
      context_control.language_tag == 128,
      context_control.output_items,
    )
    cek_context_successor_is_exact(
      pre,
      witness,
      native_control,
      CekContextControlV1 { ..context_control, output_items: next_items },
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  }
}

fn verify_cek_signer_context_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  if context_control.signer_items.length == native_control.signer_count {
    and {
      auxiliary == NoAuxiliaryWitness,
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 { ..context_control, stage: 5 },
        execution_cursor,
        completed_cpu,
        completed_memory,
      ),
    }
  } else {
    expect
        CekSignerContextItemWitness {
          peaks,
          signer_index,
          signer_hash,
          siblings,
        }
      = auxiliary
    expect Some(next_items) = script_context_v1.prepend_signer_v1(
      native_control.signer_count,
      native_control.signer_frontier_commitment,
      peaks,
      signer_index,
      signer_hash,
      siblings,
      context_control.signer_items,
    )
    cek_context_successor_is_exact(
      pre,
      witness,
      native_control,
      CekContextControlV1 { ..context_control, signer_items: next_items },
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  }
}

pub fn prepend_cek_observer_item_v1(
  observer_hash: ByteArray,
  midgard_encoding: Bool,
  tail: cek_data_v1.DataSequenceSummaryV1,
) -> cek_data_v1.DataSequenceSummaryV1 {
  expect bytearray.length(observer_hash) == 28
  if midgard_encoding {
    cek_data_v1.prepend_data_list_summary_v1(
      builtin.b_data(observer_hash)
        |> cek_data_v1.semantic_data_summary_v1,
      tail,
    )
  } else {
    cek_data_v1.prepend_data_pair_summary_v1(
      builtin.constr_data(1, [builtin.b_data(observer_hash)])
        |> cek_data_v1.semantic_data_summary_v1,
      builtin.i_data(0)
        |> cek_data_v1.semantic_data_summary_v1,
      tail,
    )
  }
}

pub fn finalize_cek_observer_items_v1(
  items: cek_data_v1.DataSequenceSummaryV1,
  midgard_encoding: Bool,
) -> cek_data_v1.DataSummaryV1 {
  if midgard_encoding {
    cek_data_v1.list_data_summary_v1(items)
  } else {
    cek_data_v1.map_data_summary_v1(items)
  }
}

pub fn verify_cek_observer_context_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  let Pair(verified_source, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    native_control.compact_cbor,
    native_control.witness_set_compact_cbor,
    native_control.field_preimage_lengths_cbor,
  )
  let observer_commitment = verified_source.tx_compact.body.required_observers_hash
  let empty_observer_commitment = bounded_collection_v1.from_items(3, [])
  let midgard_encoding = context_control.language_tag == 128
  and {
    verified_source.version == 1,
    if observer_commitment == empty_observer_commitment {
      and {
        auxiliary == NoAuxiliaryWitness,
        observer_context_is_pristine(context_control),
        cek_context_successor_is_exact(
          pre,
          witness,
          native_control,
          CekContextControlV1 {
            ..context_control,
            stage: 6,
            observer_summary: finalize_cek_observer_items_v1(
              context_control.observer_items,
              midgard_encoding,
            ),
          },
          execution_cursor,
          completed_cpu,
          completed_memory,
        ),
      }
    } else if and {
      context_control.observer_count > 0,
      context_control.observer_items.length == context_control.observer_count,
    } {
      and {
        auxiliary == NoAuxiliaryWitness,
        cek_context_successor_is_exact(
          pre,
          witness,
          native_control,
          CekContextControlV1 {
            ..context_control,
            stage: 6,
            observer_summary: finalize_cek_observer_items_v1(
              context_control.observer_items,
              midgard_encoding,
            ),
          },
          execution_cursor,
          completed_cpu,
          completed_memory,
        ),
      }
    } else {
      expect TransactionFieldChunkWitness { collection_proof, chunk_proof } = auxiliary
      let active_count = if context_control.observer_count == 0 {
        collection_proof.item_count
      } else {
        context_control.observer_count
      }
      let expected_index = active_count - context_control.observer_items.length - 1
      let observer_hash = chunk_proof.chunk
      let next_items = prepend_cek_observer_item_v1(
        observer_hash,
        midgard_encoding,
        context_control.observer_items,
      )
      and {
        active_count > 0,
        active_count <= max_tx_size_derived_collection_item_count,
        expected_index >= 0,
        collection_proof.field_index == 3,
        collection_proof.item_count == active_count,
        collection_proof.item_index == expected_index,
        collection_proof.item_length == 28,
        chunk_proof.field_index == 3,
        chunk_proof.item_index == expected_index,
        chunk_proof.total_length == 28,
        chunk_proof.chunk_index == 0,
        bounded_item_v1.chunk_count(chunk_proof.total_length) == 1,
        bounded_collection_v1.verify_item(observer_commitment, collection_proof),
        bounded_item_v1.verify_chunk(
          collection_proof.item_commitment,
          chunk_proof,
        ),
        if context_control.observer_items.length == 0 {
          context_control.previous_observer == #""
        } else {
          bytearray.compare(observer_hash, context_control.previous_observer) == Less
        },
        cek_context_successor_is_exact(
          pre,
          witness,
          native_control,
          CekContextControlV1 {
            ..context_control,
            observer_count: active_count,
            observer_items: next_items,
            previous_observer: observer_hash,
          },
          execution_cursor,
          completed_cpu,
          completed_memory,
        ),
      }
    },
  }
}

fn verify_cek_mint_context_init(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  and {
    auxiliary == NoAuxiliaryWitness,
    if native_control.mint_count == 0 {
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 {
          ..context_control,
          stage: 9,
          mint_summary: cek_data_v1.map_data_summary_v1(
            cek_data_v1.empty_data_pair_summary_v1(),
          ),
        },
        execution_cursor,
        completed_cpu,
        completed_memory,
      )
    } else {
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 { ..context_control, stage: 8 },
        execution_cursor,
        completed_cpu,
        completed_memory,
      )
    },
  }
}

fn prepend_mint_asset_summary(
  asset_name: ByteArray,
  quantity: Int,
  tail: cek_data_v1.DataSequenceSummaryV1,
) -> cek_data_v1.DataSequenceSummaryV1 {
  cek_data_v1.prepend_data_pair_summary_v1(
    builtin.b_data(asset_name) |> cek_data_v1.semantic_data_summary_v1,
    builtin.i_data(quantity) |> cek_data_v1.semantic_data_summary_v1,
    tail,
  )
}

fn finalize_current_mint_policy(
  control: CekContextControlV1,
) -> cek_data_v1.DataSequenceSummaryV1 {
  expect bytearray.length(control.current_mint_policy) == 28
  cek_data_v1.prepend_data_pair_summary_v1(
    builtin.b_data(control.current_mint_policy)
      |> cek_data_v1.semantic_data_summary_v1,
    cek_data_v1.map_data_summary_v1(control.current_mint_assets),
    control.mint_policies,
  )
}

fn verify_cek_mint_context_item(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  if context_control.mint_cursor == native_control.mint_count {
    let final_policies = finalize_current_mint_policy(context_control)
    and {
      auxiliary == NoAuxiliaryWitness,
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 {
          ..context_control,
          stage: 9,
          current_mint_policy: #"",
          current_mint_assets: cek_data_v1.empty_data_pair_summary_v1(),
          mint_policies: final_policies,
          mint_summary: cek_data_v1.map_data_summary_v1(final_policies),
        },
        execution_cursor,
        completed_cpu,
        completed_memory,
      ),
    }
  } else {
    expect
        CekMintContextItemWitness {
          mint_index,
          policy_id,
          asset_name,
          quantity,
          siblings,
        }
      = auxiliary
    let expected_index = native_control.mint_count - context_control.mint_cursor - 1
    let membership_verified = and {
      mint_index == expected_index,
      validation_merkle_v1.verify_membership(
        native_control.mint_count,
        native_control.mint_peaks,
        mint_index,
        mint_asset_leaf_hash(policy_id, asset_name, quantity),
        siblings,
      ),
    }
    if context_control.current_mint_policy == #"" {
      and {
        membership_verified,
        cek_context_successor_is_exact(
          pre,
          witness,
          native_control,
          CekContextControlV1 {
            ..context_control,
            mint_cursor: context_control.mint_cursor + 1,
            current_mint_policy: policy_id,
            current_mint_assets: prepend_mint_asset_summary(
              asset_name,
              quantity,
              context_control.current_mint_assets,
            ),
          },
          execution_cursor,
          completed_cpu,
          completed_memory,
        ),
      }
    } else if policy_id == context_control.current_mint_policy {
      and {
        membership_verified,
        cek_context_successor_is_exact(
          pre,
          witness,
          native_control,
          CekContextControlV1 {
            ..context_control,
            mint_cursor: context_control.mint_cursor + 1,
            current_mint_assets: prepend_mint_asset_summary(
              asset_name,
              quantity,
              context_control.current_mint_assets,
            ),
          },
          execution_cursor,
          completed_cpu,
          completed_memory,
        ),
      }
    } else {
      let next_policies = finalize_current_mint_policy(context_control)
      and {
        membership_verified,
        bytearray.compare(policy_id, context_control.current_mint_policy) == Less,
        cek_context_successor_is_exact(
          pre,
          witness,
          native_control,
          CekContextControlV1 {
            ..context_control,
            mint_cursor: context_control.mint_cursor + 1,
            current_mint_policy: policy_id,
            current_mint_assets: prepend_mint_asset_summary(
              asset_name,
              quantity,
              cek_data_v1.empty_data_pair_summary_v1(),
            ),
            mint_policies: next_policies,
          },
          execution_cursor,
          completed_cpu,
          completed_memory,
        ),
      }
    }
  }
}

fn cek_redeemer_context_main_successor(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  next_redeemers: CekRedeemerContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  let completed = next_redeemers.cursor == native_control.redeemer_count
  let next_context = if completed {
    expect next_redeemers.active_scan_hash == #""
    expect bytearray.length(next_redeemers.current_redeemer.root) == 32
    CekContextControlV1 {
      ..context_control,
      stage: 10,
      redeemer_context_control_hash: hash_cek_redeemer_context_control_v1(
        next_redeemers,
      ),
    }
  } else {
    CekContextControlV1 {
      ..context_control,
      redeemer_context_control_hash: hash_cek_redeemer_context_control_v1(
        next_redeemers,
      ),
    }
  }
  and {
    cek_redeemer_context_control_is_well_formed(
      native_control.redeemer_count,
      next_redeemers,
    ),
    cek_context_successor_is_exact(
      pre,
      witness,
      native_control,
      next_context,
      execution_cursor,
      completed_cpu,
      completed_memory,
    ),
  }
}

fn verify_cek_redeemer_data_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  when auxiliary is {
    CekRedeemerContextSelectWitness {
      control,
      item_index,
      item_count,
      total_length,
      item_commitment,
      redeemer_siblings,
      purpose_frontier_index,
      purpose_kind,
      purpose_index,
      script_hash,
      subject,
      purpose_siblings,
    } -> {
      let redeemer_leaf = script_proof_v1.redeemer_item_leaf_hash(
        item_index,
        item_commitment,
      )
      let purpose_leaf = script_proof_v1.purpose_leaf_hash(
        purpose_kind,
        purpose_index,
        script_hash,
        subject,
      )
      let current_matches = and {
        cek_redeemer_context_control_is_well_formed(
          native_control.redeemer_count,
          control,
        ),
        hash_cek_redeemer_context_control_v1(control) == context_control.redeemer_context_control_hash,
        control.active_scan_hash == #"",
      }
      let membership_matches = and {
        item_index == native_control.redeemer_count - control.cursor - 1,
        item_count == native_control.redeemer_count,
        validation_merkle_v1.verify_membership(
          native_control.redeemer_count,
          native_control.redeemer_peaks,
          item_index,
          redeemer_leaf,
          redeemer_siblings,
        ),
        purpose_frontier_index >= 0,
        purpose_frontier_index < native_control.purpose_count,
        validation_merkle_v1.verify_membership(
          native_control.purpose_count,
          native_control.purpose_peaks,
          purpose_frontier_index,
          purpose_leaf,
          purpose_siblings,
        ),
      }
      when
        script_context_v1.script_purpose_summary_v1(
          purpose_kind,
          script_hash,
          subject,
          context_control.language_tag == 128,
        )
      is {
        None ->
          if and {
            current_matches,
            membership_matches,
            purpose_kind == 3,
            context_control.language_tag == 3,
          } {
            let item_control = redeemer_item_proof_v1.initial_control_v1(
              redeemer_item_proof_v1.mode_descriptor,
              item_index,
              item_count,
              total_length,
              item_commitment,
              purpose_kind_to_redeemer_tag(purpose_kind),
              purpose_index,
            )
            cek_redeemer_context_main_successor(
              pre,
              witness,
              native_control,
              context_control,
              CekRedeemerContextControlV1 {
                ..control,
                active_scan_hash: redeemer_item_proof_v1.hash_control_v1(
                  item_control,
                ),
                active_redeemer_leaf: redeemer_leaf,
              },
              execution_cursor,
              completed_cpu,
              completed_memory,
            )
          } else {
            False
          }
        Some(purpose) -> {
          let item_control = redeemer_item_proof_v1.initial_control_v1(
            redeemer_item_proof_v1.mode_data,
            item_index,
            item_count,
            total_length,
            item_commitment,
            purpose_kind_to_redeemer_tag(purpose_kind),
            purpose_index,
          )
          and {
            current_matches,
            membership_matches,
            cek_redeemer_context_main_successor(
              pre,
              witness,
              native_control,
              context_control,
              CekRedeemerContextControlV1 {
                ..control,
                active_scan_hash: redeemer_item_proof_v1.hash_control_v1(
                  item_control,
                ),
                active_redeemer_leaf: redeemer_leaf,
                active_purpose: purpose,
              },
              execution_cursor,
              completed_cpu,
              completed_memory,
            ),
          }
        }
      }
    }
    RedeemerItemStepWitness {
      redeemer_control: Some(redeemer_control),
      control: item_control,
      witness: item_witness,
    } -> {
      let current_matches = and {
        cek_redeemer_context_control_is_well_formed(
          native_control.redeemer_count,
          redeemer_control,
        ),
        hash_cek_redeemer_context_control_v1(redeemer_control) == context_control.redeemer_context_control_hash,
        item_control.item_count == native_control.redeemer_count,
        item_control.item_index == native_control.redeemer_count - redeemer_control.cursor - 1,
        script_proof_v1.redeemer_item_leaf_hash(
          item_control.item_index,
          item_control.item_commitment,
        ) == redeemer_control.active_redeemer_leaf,
        redeemer_control.active_scan_hash == redeemer_item_proof_v1.hash_control_v1(
          item_control,
        ),
      }
      when redeemer_item_proof_v1.step_v1(item_control, item_witness) is {
        None -> False
        Some(redeemer_item_proof_v1.RedeemerItemProofInvalid) -> False
        Some(redeemer_item_proof_v1.RedeemerItemProofAdvanced { control: next }) -> {
          let next_redeemers = if next.stage == redeemer_item_proof_v1.stage_terminal {
            if next.mode == redeemer_item_proof_v1.mode_descriptor {
              CekRedeemerContextControlV1 {
                ..redeemer_control,
                cursor: redeemer_control.cursor + 1,
                active_scan_hash: #"",
                active_redeemer_leaf: #"",
                active_purpose: empty_data_summary_v1(),
              }
            } else {
              expect Some(summary) = redeemer_item_proof_v1.finalize_v1(next)
              let next_current = if redeemer_control.active_redeemer_leaf == context_control.redeemer_leaf {
                summary
              } else {
                redeemer_control.current_redeemer
              }
              CekRedeemerContextControlV1 {
                cursor: redeemer_control.cursor + 1,
                map_items: cek_data_v1.prepend_data_pair_summary_v1(
                  redeemer_control.active_purpose,
                  summary,
                  redeemer_control.map_items,
                ),
                active_scan_hash: #"",
                active_redeemer_leaf: #"",
                active_purpose: empty_data_summary_v1(),
                current_redeemer: next_current,
              }
            }
          } else {
            CekRedeemerContextControlV1 {
              ..redeemer_control,
              active_scan_hash: redeemer_item_proof_v1.hash_control_v1(next),
            }
          }
          and {
            current_matches,
            if next.mode == redeemer_item_proof_v1.mode_descriptor {
              and {
                item_control.expected_purpose_tag == 6,
                context_control.language_tag == 3,
                redeemer_control.active_purpose == empty_data_summary_v1(),
              }
            } else {
              bytearray.length(redeemer_control.active_purpose.root) == 32
            },
            cek_redeemer_context_main_successor(
              pre,
              witness,
              native_control,
              context_control,
              next_redeemers,
              execution_cursor,
              completed_cpu,
              completed_memory,
            ),
          }
        }
      }
    }
    _ -> False
  }
}

fn cek_context_execution_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  context_summary: cek_data_v1.DataSummaryV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  let context_value = cek_constant_v1.semantic_data_constant_root_v1(
    context_summary,
  )
  let context_term = cek_proof_v1.hash_context_constant_term_v1(context_value)
  let initial_state = cek_machine_v1.MachineStateV1 {
    mode: cek_machine_v1.mode_compute,
    execution_index: execution_cursor,
    focus_root: cek_proof_v1.hash_application_term_v1(
      context_control.program_term_root,
      context_term,
    ),
    environment_root: cek_proof_v1.empty_environment_root_v1,
    continuation_root: cek_proof_v1.empty_continuation_root_v1,
    auxiliary: 0,
    cpu: 0,
    memory: 0,
  }
  and {
    witness.claimed_successor.phase == validation_trace_v1.Cek,
    witness.claimed_successor.execution_cpu == pre.execution_cpu,
    witness.claimed_successor.execution_memory == pre.execution_memory,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      pre.program_counter + 1,
      encode_cek_witness_v1(
        native_control,
        #"",
        execution_cursor,
        completed_cpu,
        completed_memory,
        cek_machine_v1.hash_state_v1(initial_state),
        context_control.execution_cpu_limit,
        context_control.execution_memory_limit,
      ),
    ),
  }
}

fn completed_redeemer_context_matches(
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  redeemers: CekRedeemerContextControlV1,
) -> Bool {
  and {
    cek_redeemer_context_control_is_well_formed(
      native_control.redeemer_count,
      redeemers,
    ),
    hash_cek_redeemer_context_control_v1(redeemers) == context_control.redeemer_context_control_hash,
    redeemers.cursor == native_control.redeemer_count,
    redeemers.active_scan_hash == #"",
    bytearray.length(redeemers.current_redeemer.root) == 32,
  }
}

fn verify_cek_context_finalize(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  let midgard_encoding = context_control.language_tag == 128
  let Pair(redeemers, script_info) = if and {
    !midgard_encoding,
    context_control.purpose_kind == 0,
  } {
    expect
        CekContextFinalizeSpendWitness {
          redeemer_control,
          item_index,
          key,
          value,
          siblings,
        }
      = auxiliary
    expect item_index == context_control.purpose_index
    expect key == context_control.subject
    expect
      validation_merkle_v1.verify_membership(
        native_control.resolved_input_count,
        native_control.resolved_item_peaks,
        item_index,
        script_proof_v1.resolved_context_item_leaf_hash(
          0,
          item_index,
          key,
          value,
        ),
        siblings,
      )
    expect Some(summary) = script_context_v1.cardano_script_info_summary_v1(
      context_control.purpose_kind,
      context_control.script_hash,
      context_control.subject,
      Some(value),
    )
    Pair(redeemer_control, summary)
  } else {
    expect CekContextFinalizeWitness { redeemer_control } = auxiliary
    expect Some(summary) = if midgard_encoding {
      script_context_v1.script_purpose_summary_v1(
        context_control.purpose_kind,
        context_control.script_hash,
        context_control.subject,
        True,
      )
    } else {
      script_context_v1.cardano_script_info_summary_v1(
        context_control.purpose_kind,
        context_control.script_hash,
        context_control.subject,
        None,
      )
    }
    Pair(redeemer_control, summary)
  }
  let parts = CekContextPartsControlV1 {
    redeemer_items: redeemers.map_items,
    redeemer: redeemers.current_redeemer,
    script_info,
  }
  and {
    completed_redeemer_context_matches(
      native_control,
      context_control,
      redeemers,
    ),
    cek_context_successor_is_exact(
      pre,
      witness,
      native_control,
      CekContextControlV1 {
        ..context_control,
        stage: 11,
        redeemer_context_control_hash: hash_cek_context_parts_control_v1(parts),
      },
      execution_cursor,
      completed_cpu,
      completed_memory,
    ),
  }
}

fn verify_cek_context_assemble(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  expect CekContextAssembleWitness { control } = auxiliary
  let assembly = CekTxInfoAssemblyControlV1 {
    tail_fields: script_context_v1.tx_info_tail_fields_summary_v1(
      context_control.language_tag == 128,
      context_control.observer_summary,
      context_control.signer_items,
      context_control.mint_summary,
      control.redeemer_items,
      pre.transaction_id,
    ),
    redeemer: control.redeemer,
    script_info: control.script_info,
  }
  and {
    hash_cek_context_parts_control_v1(control) == context_control.redeemer_context_control_hash,
    cek_context_successor_is_exact(
      pre,
      witness,
      native_control,
      CekContextControlV1 {
        ..context_control,
        stage: 12,
        redeemer_context_control_hash: hash_cek_tx_info_assembly_control_v1(
          assembly,
        ),
      },
      execution_cursor,
      completed_cpu,
      completed_memory,
    ),
  }
}

fn verify_cek_tx_info_finalize(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  expect CekTxInfoFinalizeWitness { control } = auxiliary
  let compact = decode_native_tx_compact_v1(native_control.compact_cbor)
  let tx_info = script_context_v1.tx_info_from_tail_summary_v1(
    context_control.language_tag == 128,
    context_control.spend_items,
    context_control.reference_items,
    context_control.output_items,
    compact.body.fee,
    compact.body.validity_interval_start,
    compact.body.validity_interval_end,
    context_control.observer_summary,
    context_control.mint_summary,
    control.tail_fields,
  )
  let final_context = CekFinalContextControlV1 {
    tx_info,
    redeemer: control.redeemer,
    script_info: control.script_info,
  }
  and {
    hash_cek_tx_info_assembly_control_v1(control) == context_control.redeemer_context_control_hash,
    cek_context_successor_is_exact(
      pre,
      witness,
      native_control,
      CekContextControlV1 {
        ..context_control,
        stage: 13,
        redeemer_context_control_hash: hash_cek_final_context_control_v1(
          final_context,
        ),
      },
      execution_cursor,
      completed_cpu,
      completed_memory,
    ),
  }
}

fn verify_cek_context_seed(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  expect CekContextSeedWitness { control } = auxiliary
  let context = script_context_v1.script_context_summary_v1(
    control.tx_info,
    control.redeemer,
    control.script_info,
  )
  and {
    hash_cek_final_context_control_v1(control) == context_control.redeemer_context_control_hash,
    cek_context_execution_successor_is_exact(
      pre,
      witness,
      native_control,
      context_control,
      context,
      execution_cursor,
      completed_cpu,
      completed_memory,
    ),
  }
}

fn verify_cek_context_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  if context_control.stage == 0 {
    verify_cek_redeemer_selection(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 1 {
    verify_cek_reference_context_step(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 2 {
    verify_cek_spend_context_step(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 3 {
    verify_cek_output_context_step(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 4 {
    verify_cek_signer_context_step(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 5 {
    verify_cek_observer_context_step(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 6 {
    verify_cek_mint_context_init(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 7 {
    False
  } else if context_control.stage == 8 {
    verify_cek_mint_context_item(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 9 {
    verify_cek_redeemer_data_step(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 10 {
    verify_cek_context_finalize(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 11 {
    verify_cek_context_assemble(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 12 {
    verify_cek_tx_info_finalize(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else if context_control.stage == 13 {
    verify_cek_context_seed(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
  } else {
    // Remaining scalar/dynamic fields, the outer context root, and initial
    // CEK application state must all be proven before execution can start.
    False
  }
}

fn verify_cek(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  expect Some(data) = cbor.deserialise(witness.work_witness_cbor)
  expect
      [
        native_control_cbor_data, context_control_cbor_data,
        execution_cursor_data, completed_cpu_data, completed_memory_data,
        active_state_hash_data, execution_cpu_limit_data,
        execution_memory_limit_data,
      ]
    = builtin.un_list_data(data)
  let native_control_cbor = builtin.un_b_data(native_control_cbor_data)
  let context_control_cbor = builtin.un_b_data(context_control_cbor_data)
  let execution_cursor = builtin.un_i_data(execution_cursor_data)
  let completed_cpu = builtin.un_i_data(completed_cpu_data)
  let completed_memory = builtin.un_i_data(completed_memory_data)
  let active_state_hash = builtin.un_b_data(active_state_hash_data)
  let execution_cpu_limit = builtin.un_i_data(execution_cpu_limit_data)
  let execution_memory_limit = builtin.un_i_data(execution_memory_limit_data)
  let control = native_scripts_control_from_witness(native_control_cbor)
  let Pair(verified_source, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  and {
    verified_source.version == 1,
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    native_scripts_control_is_well_formed(control),
    control.execution_cursor == control.execution_count,
    execution_cursor >= 0,
    execution_cursor <= control.execution_count,
    completed_cpu >= 0,
    completed_memory >= 0,
    witness.work_witness_cbor == encode_cek_witness_v1(
      control,
      context_control_cbor,
      execution_cursor,
      completed_cpu,
      completed_memory,
      active_state_hash,
      execution_cpu_limit,
      execution_memory_limit,
    ),
    if active_state_hash == #"" {
      if context_control_cbor == #"" {
        and {
          execution_cpu_limit == 0,
          execution_memory_limit == 0,
          if or {
            execution_cursor == control.execution_count,
            control.language_bitmap == 0,
          } {
            and {
              auxiliary == NoAuxiliaryWitness,
              witness.claimed_successor.phase == validation_trace_v1.ValueAndMint,
              witness.claimed_successor.execution_cpu == pre.execution_cpu,
              witness.claimed_successor.execution_memory == pre.execution_memory,
              witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
                validation_trace_v1.ValueAndMint,
                pre.program_counter + 1,
                encode_value_and_mint_witness_v1(control),
              ),
            }
          } else {
            verify_cek_execution_selection(
              pre,
              witness,
              auxiliary,
              control,
              execution_cursor,
              completed_cpu,
              completed_memory,
            )
          },
        }
      } else {
        let context_control = cek_context_control_from_cbor(
          context_control_cbor,
        )
        and {
          execution_cursor < control.execution_count,
          execution_cpu_limit == 0,
          execution_memory_limit == 0,
          context_control_cbor == encode_cek_context_control_v1(context_control),
          cek_context_control_is_well_formed(control, context_control),
          verify_cek_context_step(
            pre,
            witness,
            auxiliary,
            control,
            context_control,
            execution_cursor,
            completed_cpu,
            completed_memory,
          ),
        }
      }
    } else {
      and {
        context_control_cbor == #"",
        verify_cek_core_step(
          pre,
          witness,
          auxiliary,
          control,
          execution_cursor,
          completed_cpu,
          completed_memory,
          active_state_hash,
          execution_cpu_limit,
          execution_memory_limit,
        ),
      }
    },
  }
}

fn value_and_mint_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  next_control: ValueAndMintControlV1,
) -> Bool {
  and {
    witness.claimed_successor.phase == validation_trace_v1.ValueAndMint,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ValueAndMint,
      pre.program_counter + 1,
      encode_value_and_mint_control_v1(next_control),
    ),
  }
}

fn value_and_mint_stage_zero(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ValueAndMintControlV1,
) -> Bool {
  let native_control = control.native_control
  let next_control = ValueAndMintControlV1 {
    ..control,
    stage: 1,
    replay_schedule_hash: native_control.resolution_schedule_hash,
    replay_remaining_schedule_hash: native_control.resolution_schedule_hash,
  }
  and {
    auxiliary == NoAuxiliaryWitness,
    control.replay_schedule_hash == empty_resolution_schedule_hash(),
    control.replay_cursor == 0,
    control.replay_asset_cursor == 0,
    control.replay_value_hash == no_rejection_code_hash,
    control.replay_accumulator == initial_resolution_accumulator(),
    control.replay_remaining_schedule_hash == empty_resolution_schedule_hash(),
    control.output_cursor == 0,
    control.output_asset_cursor == 0,
    control.mint_cursor == 0,
    control.value_accumulator == initial_value_accumulator(),
    native_control.resolution_schedule_hash != empty_resolution_schedule_hash(),
    value_and_mint_successor_is_exact(pre, witness, next_control),
  }
}

fn value_and_mint_stage_one(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ValueAndMintControlV1,
) -> Bool {
  and {
    auxiliary == NoAuxiliaryWitness,
    value_and_mint_successor_is_exact(
      pre,
      witness,
      ValueAndMintControlV1 { ..control, stage: 2 },
    ),
  }
}

fn complete_value_input_replay(
  control: ValueAndMintControlV1,
  source_kind: Int,
  key: ByteArray,
  value: ByteArray,
  next_schedule_hash: ByteArray,
  next_value_accumulator: ValueAccumulatorV1,
) -> ValueAndMintControlV1 {
  ValueAndMintControlV1 {
    ..control,
    replay_cursor: control.replay_cursor + 1,
    replay_asset_cursor: 0,
    replay_value_hash: no_rejection_code_hash,
    replay_accumulator: resolved_input_accumulator_successor(
      control.replay_accumulator,
      source_kind,
      key,
      value,
    ),
    replay_remaining_schedule_hash: next_schedule_hash,
    value_accumulator: next_value_accumulator,
  }
}

fn value_and_mint_stage_two(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ValueAndMintControlV1,
) -> Bool {
  if control.replay_remaining_schedule_hash == empty_resolution_schedule_hash() {
    and {
      auxiliary == NoAuxiliaryWitness,
      control.replay_cursor == control.native_control.resolved_input_count,
      control.replay_asset_cursor == 0,
      control.replay_value_hash == no_rejection_code_hash,
      control.replay_accumulator == control.native_control.resolved_inputs_accumulator,
      value_and_mint_successor_is_exact(
        pre,
        witness,
        ValueAndMintControlV1 { ..control, stage: 3 },
      ),
    }
  } else if control.replay_asset_cursor == 0 {
    expect
        ResolvedInputReplayWitness {
          source_kind,
          key,
          next_schedule_hash,
          value,
        }
      = auxiliary
    if and {
      source_kind == 0 || source_kind == 1,
      resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == control.replay_remaining_schedule_hash,
      control.replay_cursor < control.native_control.resolved_input_count,
    } {
      let descriptor = ledger_output_commitment_v1.decode(value)
      let next_value = if source_kind == 0 {
        ValueAccumulatorV1 {
          ..control.value_accumulator,
          lovelace_delta: control.value_accumulator.lovelace_delta + descriptor.lovelace,
        }
      } else {
        control.value_accumulator
      }
      let next_control = if or {
        source_kind == 1,
        descriptor.asset_count == 0,
      } {
        complete_value_input_replay(
          control,
          source_kind,
          key,
          value,
          next_schedule_hash,
          next_value,
        )
      } else {
        ValueAndMintControlV1 {
          ..control,
          replay_asset_cursor: 1,
          replay_value_hash: blake2b_256(value),
          value_accumulator: next_value,
        }
      }
      value_and_mint_successor_is_exact(pre, witness, next_control)
    } else {
      False
    }
  } else {
    expect
        ValueInputAssetWitness {
          source_kind,
          key,
          next_schedule_hash,
          descriptor_cbor,
          asset_index,
          policy_id,
          asset_name,
          quantity,
          asset_peaks,
          asset_siblings,
          mutation,
        }
      = auxiliary
    if and {
      source_kind == 0,
      resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == control.replay_remaining_schedule_hash,
      control.replay_cursor < control.native_control.resolved_input_count,
      blake2b_256(descriptor_cbor) == control.replay_value_hash,
      asset_index == control.replay_asset_cursor - 1,
    } {
      let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
      if ledger_output_commitment_v1.verify_asset_membership(
        descriptor,
        asset_index,
        policy_id,
        asset_name,
        quantity,
        asset_peaks,
        asset_siblings,
      ) {
        when
          apply_value_asset_mutation(
            control.value_accumulator,
            policy_id |> bytearray.concat(asset_name),
            quantity,
            mutation,
          )
        is {
          ValueAccumulatorAssetLimitExceeded ->
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_asset_count,
            )
          ValueAccumulatorMutationInvalid -> False
          ValueAccumulatorUpdated(next_value) -> {
            let next_control = if control.replay_asset_cursor == descriptor.asset_count {
              complete_value_input_replay(
                control,
                source_kind,
                key,
                descriptor_cbor,
                next_schedule_hash,
                next_value,
              )
            } else {
              ValueAndMintControlV1 {
                ..control,
                replay_asset_cursor: control.replay_asset_cursor + 1,
                value_accumulator: next_value,
              }
            }
            value_and_mint_successor_is_exact(pre, witness, next_control)
          }
        }
      } else {
        False
      }
    } else {
      False
    }
  }
}

fn value_and_mint_stage_three(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ValueAndMintControlV1,
) -> Bool {
  if control.output_cursor == control.native_control.output_count {
    and {
      auxiliary == NoAuxiliaryWitness,
      control.output_asset_cursor == 0,
      control.replay_value_hash == no_rejection_code_hash,
      value_and_mint_successor_is_exact(
        pre,
        witness,
        ValueAndMintControlV1 { ..control, stage: 4 },
      ),
    }
  } else if control.output_asset_cursor == 0 {
    expect
        ValueOutputDescriptorWitness { output_index, descriptor_cbor, siblings }
      = auxiliary
    if and {
      output_index == control.output_cursor,
      validation_merkle_v1.verify_membership(
        control.native_control.output_count,
        control.native_control.output_descriptor_peaks,
        output_index,
        script_proof_v1.output_descriptor_leaf_hash(
          output_index,
          descriptor_cbor,
        ),
        siblings,
      ),
    } {
      let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
      let next_value = ValueAccumulatorV1 {
        ..control.value_accumulator,
        lovelace_delta: control.value_accumulator.lovelace_delta - descriptor.lovelace,
      }
      let next_control = if descriptor.asset_count == 0 {
        ValueAndMintControlV1 {
          ..control,
          output_cursor: control.output_cursor + 1,
          value_accumulator: next_value,
        }
      } else {
        ValueAndMintControlV1 {
          ..control,
          output_asset_cursor: 1,
          replay_value_hash: blake2b_256(descriptor_cbor),
          value_accumulator: next_value,
        }
      }
      value_and_mint_successor_is_exact(pre, witness, next_control)
    } else {
      False
    }
  } else {
    expect
        ValueOutputAssetWitness {
          output_index,
          descriptor_cbor,
          asset_index,
          policy_id,
          asset_name,
          quantity,
          asset_peaks,
          asset_siblings,
          mutation,
        }
      = auxiliary
    if and {
      output_index == control.output_cursor,
      asset_index == control.output_asset_cursor - 1,
      blake2b_256(descriptor_cbor) == control.replay_value_hash,
    } {
      let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
      if ledger_output_commitment_v1.verify_asset_membership(
        descriptor,
        asset_index,
        policy_id,
        asset_name,
        quantity,
        asset_peaks,
        asset_siblings,
      ) {
        when
          apply_value_asset_mutation(
            control.value_accumulator,
            policy_id |> bytearray.concat(asset_name),
            0 - quantity,
            mutation,
          )
        is {
          ValueAccumulatorAssetLimitExceeded ->
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              reject_asset_count,
            )
          ValueAccumulatorMutationInvalid -> False
          ValueAccumulatorUpdated(next_value) -> {
            let next_control = if control.output_asset_cursor == descriptor.asset_count {
              ValueAndMintControlV1 {
                ..control,
                output_cursor: control.output_cursor + 1,
                output_asset_cursor: 0,
                replay_value_hash: no_rejection_code_hash,
                value_accumulator: next_value,
              }
            } else {
              ValueAndMintControlV1 {
                ..control,
                output_asset_cursor: control.output_asset_cursor + 1,
                value_accumulator: next_value,
              }
            }
            value_and_mint_successor_is_exact(pre, witness, next_control)
          }
        }
      } else {
        False
      }
    } else {
      False
    }
  }
}

fn value_and_mint_stage_four(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ValueAndMintControlV1,
) -> Bool {
  let native_control = control.native_control
  if control.mint_cursor == native_control.mint_count {
    and {
      auxiliary == NoAuxiliaryWitness,
      value_and_mint_successor_is_exact(
        pre,
        witness,
        ValueAndMintControlV1 { ..control, stage: 5 },
      ),
    }
  } else {
    expect
        ValueMintAssetWitness {
          mint_index,
          policy_id,
          asset_name,
          quantity,
          siblings,
          mutation,
        }
      = auxiliary
    if and {
      mint_index == control.mint_cursor,
      bytearray.length(policy_id) == 28,
      bytearray.length(asset_name) <= 32,
      quantity != 0,
      validation_merkle_v1.verify_membership(
        native_control.mint_count,
        native_control.mint_peaks,
        mint_index,
        mint_asset_leaf_hash(policy_id, asset_name, quantity),
        siblings,
      ),
    } {
      when
        apply_value_asset_mutation(
          control.value_accumulator,
          policy_id |> bytearray.concat(asset_name),
          quantity,
          mutation,
        )
      is {
        ValueAccumulatorAssetLimitExceeded ->
          rejected_successor_is_exact(
            pre,
            witness.claimed_successor,
            reject_asset_count,
          )
        ValueAccumulatorMutationInvalid -> False
        ValueAccumulatorUpdated(next_value) ->
          value_and_mint_successor_is_exact(
            pre,
            witness,
            ValueAndMintControlV1 {
              ..control,
              mint_cursor: control.mint_cursor + 1,
              value_accumulator: next_value,
            },
          )
      }
    } else {
      False
    }
  }
}

fn value_and_mint_stage_five(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ValueAndMintControlV1,
  body: NativeTxBodyCompact,
) -> Bool {
  and {
    auxiliary == NoAuxiliaryWitness,
    control.replay_cursor == control.native_control.resolved_input_count,
    control.replay_asset_cursor == 0,
    control.replay_value_hash == no_rejection_code_hash,
    control.replay_accumulator == control.native_control.resolved_inputs_accumulator,
    control.replay_remaining_schedule_hash == empty_resolution_schedule_hash(),
    control.output_cursor == control.native_control.output_count,
    control.output_asset_cursor == 0,
    control.mint_cursor == control.native_control.mint_count,
    if or {
      control.value_accumulator.lovelace_delta - body.fee != 0,
      control.value_accumulator.nonzero_asset_count != 0,
    } {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_value_not_preserved,
      )
    } else {
      and {
        witness.claimed_successor.phase == validation_trace_v1.LedgerDelta,
        witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.LedgerDelta,
          pre.program_counter + 1,
          encode_ledger_delta_witness_v1(
            control.native_control,
            pre.prior_ledger_root,
            control.replay_schedule_hash,
          ),
        ),
      }
    },
  }
}

fn verify_value_and_mint(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let control = value_and_mint_control_from_witness(witness.work_witness_cbor)
  let native_control = control.native_control
  let Pair(verified_source, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    native_control.compact_cbor,
    native_control.witness_set_compact_cbor,
    native_control.field_preimage_lengths_cbor,
  )
  and {
    native_tx_proof_commitment_v1(
      native_control.compact_cbor,
      native_control.witness_set_compact_cbor,
      native_control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(native_control.context_cbor) == pre.validation_context_hash,
    native_scripts_control_is_well_formed(native_control),
    native_control.execution_cursor == native_control.execution_count,
    bytearray.length(control.replay_schedule_hash) == 32,
    bytearray.length(control.replay_value_hash) == 32,
    bytearray.length(control.replay_accumulator) == 32,
    bytearray.length(control.replay_remaining_schedule_hash) == 32,
    control.replay_cursor >= 0,
    control.replay_cursor <= native_control.resolved_input_count,
    control.replay_asset_cursor >= 0,
    control.replay_asset_cursor <= ledger_output_v1.max_distinct_asset_count,
    if or {
      control.stage == 2 && control.replay_asset_cursor > 0,
      control.stage == 3 && control.output_asset_cursor > 0,
    } {
      True
    } else {
      control.replay_value_hash == no_rejection_code_hash
    },
    control.output_cursor >= 0,
    control.output_cursor <= native_control.output_count,
    control.output_asset_cursor >= 0,
    control.output_asset_cursor <= ledger_output_v1.max_distinct_asset_count,
    bytearray.length(control.value_accumulator.asset_root) == 32,
    control.value_accumulator.seen_asset_count >= 0,
    control.value_accumulator.seen_asset_count <= ledger_output_v1.max_distinct_asset_count,
    control.value_accumulator.nonzero_asset_count >= 0,
    control.value_accumulator.nonzero_asset_count <= control.value_accumulator.seen_asset_count,
    control.mint_cursor >= 0,
    control.mint_cursor <= native_control.mint_count,
    control.stage >= 0,
    control.stage <= 5,
    witness.work_witness_cbor == encode_value_and_mint_control_v1(control),
    if control.stage == 0 {
      value_and_mint_stage_zero(pre, witness, auxiliary, control)
    } else if control.stage == 1 {
      value_and_mint_stage_one(pre, witness, auxiliary, control)
    } else if control.stage == 2 {
      value_and_mint_stage_two(pre, witness, auxiliary, control)
    } else if control.stage == 3 {
      value_and_mint_stage_three(pre, witness, auxiliary, control)
    } else if control.stage == 4 {
      value_and_mint_stage_four(pre, witness, auxiliary, control)
    } else {
      value_and_mint_stage_five(
        pre,
        witness,
        auxiliary,
        control,
        verified_source.tx_compact.body,
      )
    },
  }
}

fn ledger_delta_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  next_control: LedgerDeltaControlV1,
) -> Bool {
  and {
    witness.claimed_successor.phase == validation_trace_v1.LedgerDelta,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.LedgerDelta,
      pre.program_counter + 1,
      encode_ledger_delta_control_v1(next_control),
    ),
  }
}

fn ledger_delta_operation_proof_is_valid(
  pre: validation_trace_v1.ValidationMachineStateV1,
  control: LedgerDeltaControlV1,
  operation_kind: Int,
  key: ByteArray,
  value: ByteArray,
  proof: LedgerDeltaOperationProofV1,
) -> Bool {
  and {
    proof.operation_count >= 0,
    proof.operation_count <= max_tx_size_derived_collection_item_count,
    validation_merkle_v1.frontier_is_well_formed(
      proof.operation_count,
      proof.operation_peaks,
    ),
    validation_merkle_v1.frontier_commitment(
      proof.operation_count,
      proof.operation_peaks,
    ) == pre.ledger_delta_root,
    proof.operation_index == control.operation_count,
    proof.operation_index >= 0,
    proof.operation_index < proof.operation_count,
    validation_merkle_v1.verify_membership(
      proof.operation_count,
      proof.operation_peaks,
      proof.operation_index,
      ledger_delta_operation_leaf_hash(
        operation_kind,
        key,
        value,
        proof.descriptor,
      ),
      proof.operation_siblings,
    ),
  }
}

fn ledger_delta_operation_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: LedgerDeltaControlV1,
) -> Bool {
  expect
      LedgerDeltaOperationWitness {
        operation_kind,
        key,
        value,
        operation_proof,
      }
    = auxiliary
  let expected_output_key = encode_midgard_tx_input(
    MidgardTxInput {
      tx_id: pre.transaction_id,
      output_index: control.output_cursor,
    },
  )
  and {
    control.pending_mutation_cbor == #"",
    if control.stage == 0 {
      and {
        operation_kind == 0,
        value == #"",
      }
    } else {
      and {
        control.stage == 1,
        operation_kind == 1,
        key == expected_output_key,
      }
    },
    ledger_delta_operation_proof_is_valid(
      pre,
      control,
      operation_kind,
      key,
      value,
      operation_proof,
    ),
    ledger_delta_successor_is_exact(
      pre,
      witness,
      LedgerDeltaControlV1 {
        ..control,
        pending_mutation_cbor: encode_ledger_delta_pending_mutation_v1(
          LedgerDeltaPendingMutationV1 {
            authorization_stage: 0,
            operation_kind,
            key,
            value,
            descriptor: operation_proof.descriptor,
            fold_control: mpf_proof_fold_v1.ProofFoldControlV1 {
              next_frame_index: -1,
              expected_next_cursor: 0,
              including_root: #"",
              excluding_root: #"",
            },
          },
        ),
      },
    ),
  }
}

fn ledger_delta_pending_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: LedgerDeltaControlV1,
) -> Bool {
  let pending = ledger_delta_pending_mutation_from_cbor(
    control.pending_mutation_cbor,
  )
  expect pending.authorization_stage == 1
  if mpf_proof_fold_v1.fold_is_complete_v1(pending.fold_control) {
    let Pair(expected_current_root, next_root) = if pending.operation_kind == 0 {
      Pair(
        pending.fold_control.including_root,
        pending.fold_control.excluding_root,
      )
    } else {
      Pair(
        pending.fold_control.excluding_root,
        pending.fold_control.including_root,
      )
    }
    let operation_value = if pending.operation_kind == 0 {
      #""
    } else {
      pending.value
    }
    let next_peaks = validation_merkle_v1.append_leaf(
      control.operation_count,
      control.operation_peaks,
      ledger_delta_operation_leaf_hash(
        pending.operation_kind,
        pending.key,
        operation_value,
        pending.descriptor,
      ),
    )
    and {
      auxiliary == NoAuxiliaryWitness,
      control.current_ledger_root == expected_current_root,
      ledger_delta_successor_is_exact(
        pre,
        witness,
        LedgerDeltaControlV1 {
          ..control,
          current_ledger_root: next_root,
          operation_count: control.operation_count + 1,
          operation_peaks: next_peaks,
          pending_mutation_cbor: #"",
        },
      ),
    }
  } else {
    expect LedgerDeltaProofFrameWitness { frame, siblings } = auxiliary
    when
      mpf_proof_fold_v1.fold_proof_frame_v1(
        pending.key,
        pending.descriptor,
        pending.fold_control,
        frame,
        siblings,
      )
    is {
      None -> False
      Some(next_fold_control) ->
        ledger_delta_successor_is_exact(
          pre,
          witness,
          LedgerDeltaControlV1 {
            ..control,
            pending_mutation_cbor: encode_ledger_delta_pending_mutation_v1(
              LedgerDeltaPendingMutationV1 {
                ..pending,
                fold_control: next_fold_control,
              },
            ),
          },
        )
    }
  }
}

fn ledger_delta_replay_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: LedgerDeltaControlV1,
) -> Bool {
  expect
      LedgerDeltaReplayWitness { source_kind, key, next_schedule_hash, value }
    = auxiliary
  let descriptor = ledger_output_commitment_v1.decode(value)
  let input = decode_midgard_tx_input_cbor(key)
  if and {
    source_kind == 0 || source_kind == 1,
    resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == control.replay_remaining_schedule_hash,
    control.replay_cursor < control.resolved_input_count,
    input.output_index == descriptor.output_index,
  } {
    let next_accumulator = resolved_input_accumulator_successor(
      control.replay_accumulator,
      source_kind,
      key,
      value,
    )
    if source_kind == 1 {
      and {
        control.pending_mutation_cbor == #"",
        ledger_delta_successor_is_exact(
          pre,
          witness,
          LedgerDeltaControlV1 {
            ..control,
            replay_cursor: control.replay_cursor + 1,
            replay_accumulator: next_accumulator,
            replay_remaining_schedule_hash: next_schedule_hash,
          },
        ),
      }
    } else {
      expect control.pending_mutation_cbor != #""
      let pending = ledger_delta_pending_mutation_from_cbor(
        control.pending_mutation_cbor,
      )
      when
        mpf_proof_fold_v1.initial_fold_control_v1(
          key,
          value,
          pending.descriptor,
        )
      is {
        None -> False
        Some(fold_control) -> and {
            pending.authorization_stage == 0,
            pending.operation_kind == 0,
            pending.key == key,
            pending.value == #"",
            ledger_delta_successor_is_exact(
              pre,
              witness,
              LedgerDeltaControlV1 {
                ..control,
                replay_cursor: control.replay_cursor + 1,
                replay_accumulator: next_accumulator,
                replay_remaining_schedule_hash: next_schedule_hash,
                pending_mutation_cbor: encode_ledger_delta_pending_mutation_v1(
                  LedgerDeltaPendingMutationV1 {
                    ..pending,
                    authorization_stage: 1,
                    value,
                    fold_control,
                  },
                ),
              },
            ),
          }
      }
    }
  } else {
    False
  }
}

fn ledger_delta_stage_one(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: LedgerDeltaControlV1,
) -> Bool {
  if control.pending_mutation_cbor != #"" {
    let pending = ledger_delta_pending_mutation_from_cbor(
      control.pending_mutation_cbor,
    )
    if pending.authorization_stage == 0 {
      ledger_delta_replay_step(pre, witness, auxiliary, control)
    } else {
      ledger_delta_pending_step(pre, witness, auxiliary, control)
    }
  } else if control.replay_remaining_schedule_hash == empty_resolution_schedule_hash() {
    and {
      auxiliary == NoAuxiliaryWitness,
      control.replay_cursor == control.resolved_input_count,
      control.replay_accumulator == control.resolved_inputs_accumulator,
      ledger_delta_successor_is_exact(
        pre,
        witness,
        LedgerDeltaControlV1 { ..control, stage: 1 },
      ),
    }
  } else {
    ledger_delta_replay_step(pre, witness, auxiliary, control)
  }
}

fn ledger_delta_output_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: LedgerDeltaControlV1,
) -> Bool {
  expect LedgerDeltaOutputWitness { output_index, descriptor_cbor, siblings } = auxiliary
  let key = encode_midgard_tx_input(
    MidgardTxInput { tx_id: pre.transaction_id, output_index },
  )
  let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
  expect control.pending_mutation_cbor != #""
  let pending = ledger_delta_pending_mutation_from_cbor(
    control.pending_mutation_cbor,
  )
  when
    mpf_proof_fold_v1.initial_fold_control_v1(
      key,
      descriptor_cbor,
      pending.descriptor,
    )
  is {
    None -> False
    Some(fold_control) -> and {
        output_index == control.output_cursor,
        descriptor.output_index == output_index,
        pending.authorization_stage == 0,
        pending.operation_kind == 1,
        pending.key == key,
        pending.value == descriptor_cbor,
        validation_merkle_v1.verify_membership(
          control.output_count,
          control.output_descriptor_peaks,
          output_index,
          script_proof_v1.output_descriptor_leaf_hash(
            output_index,
            descriptor_cbor,
          ),
          siblings,
        ),
        ledger_delta_successor_is_exact(
          pre,
          witness,
          LedgerDeltaControlV1 {
            ..control,
            output_cursor: control.output_cursor + 1,
            pending_mutation_cbor: encode_ledger_delta_pending_mutation_v1(
              LedgerDeltaPendingMutationV1 {
                ..pending,
                authorization_stage: 1,
                fold_control,
              },
            ),
          },
        ),
      }
  }
}

fn ledger_delta_stage_two(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: LedgerDeltaControlV1,
) -> Bool {
  if and {
    control.pending_mutation_cbor != #"",
    ledger_delta_pending_mutation_from_cbor(control.pending_mutation_cbor).authorization_stage == 1,
  } {
    ledger_delta_pending_step(pre, witness, auxiliary, control)
  } else if and {
    control.pending_mutation_cbor == #"",
    control.output_cursor == control.output_count,
  } {
    and {
      auxiliary == NoAuxiliaryWitness,
      ledger_delta_successor_is_exact(
        pre,
        witness,
        LedgerDeltaControlV1 { ..control, stage: 2 },
      ),
    }
  } else {
    expect LedgerDeltaOutputWitness { output_index, descriptor_cbor, siblings }
      = auxiliary
    let key = encode_midgard_tx_input(
      MidgardTxInput { tx_id: pre.transaction_id, output_index },
    )
    let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
    expect control.pending_mutation_cbor != #""
    let pending = ledger_delta_pending_mutation_from_cbor(
      control.pending_mutation_cbor,
    )
    when
      mpf_proof_fold_v1.initial_fold_control_v1(
        key,
        descriptor_cbor,
        pending.descriptor,
      )
    is {
      None -> False
      Some(fold_control) -> and {
          output_index == control.output_cursor,
          descriptor.output_index == output_index,
          pending.authorization_stage == 0,
          pending.operation_kind == 1,
          pending.key == key,
          pending.value == descriptor_cbor,
          validation_merkle_v1.verify_membership(
            control.output_count,
            control.output_descriptor_peaks,
            output_index,
            script_proof_v1.output_descriptor_leaf_hash(
              output_index,
              descriptor_cbor,
            ),
            siblings,
          ),
          ledger_delta_successor_is_exact(
            pre,
            witness,
            LedgerDeltaControlV1 {
              ..control,
              output_cursor: control.output_cursor + 1,
              pending_mutation_cbor: encode_ledger_delta_pending_mutation_v1(
                LedgerDeltaPendingMutationV1 {
                  ..pending,
                  authorization_stage: 1,
                  fold_control,
                },
              ),
            },
          ),
        }
    }
  }
}

fn ledger_delta_stage_three(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: LedgerDeltaControlV1,
) -> Bool {
  and {
    auxiliary == NoAuxiliaryWitness,
    control.replay_cursor == control.resolved_input_count,
    control.replay_accumulator == control.resolved_inputs_accumulator,
    control.replay_remaining_schedule_hash == empty_resolution_schedule_hash(),
    control.output_cursor == control.output_count,
    control.pending_mutation_cbor == #"",
    validation_merkle_v1.frontier_commitment(
      control.operation_count,
      control.operation_peaks,
    ) == pre.ledger_delta_root,
    witness.claimed_successor.phase == validation_trace_v1.Terminal,
    witness.claimed_successor.verdict == validation_trace_v1.Accepted,
    witness.claimed_successor.rejection_code_hash == no_rejection_code_hash,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      pre.program_counter + 1,
      encode_terminal_acceptance_witness_v1(
        control.current_ledger_root,
        control.operation_count,
        control.operation_peaks,
      ),
    ),
  }
}

fn verify_ledger_delta(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  let control = ledger_delta_control_from_witness(witness.work_witness_cbor)
  and {
    control.resolved_input_count >= 0,
    control.resolved_input_count <= max_tx_size_derived_collection_item_count,
    bytearray.length(control.resolved_inputs_accumulator) == 32,
    control.output_count >= 0,
    control.output_count <= max_tx_size_derived_collection_item_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.output_count,
      control.output_descriptor_peaks,
    ),
    bytearray.length(control.replay_schedule_hash) == 32,
    bytearray.length(control.replay_accumulator) == 32,
    bytearray.length(control.replay_remaining_schedule_hash) == 32,
    bytearray.length(control.current_ledger_root) == 32,
    control.replay_cursor >= 0,
    control.replay_cursor <= control.resolved_input_count,
    control.output_cursor >= 0,
    control.output_cursor <= control.output_count,
    control.operation_count >= 0,
    control.operation_count <= max_tx_size_derived_collection_item_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.operation_count,
      control.operation_peaks,
    ),
    control.stage >= 0,
    control.stage <= 2,
    witness.work_witness_cbor == encode_ledger_delta_control_v1(control),
    when auxiliary is {
      LedgerDeltaOperationWitness { .. } ->
        ledger_delta_operation_step(pre, witness, auxiliary, control)
      _ ->
        if control.stage == 0 {
          ledger_delta_stage_one(pre, witness, auxiliary, control)
        } else if control.stage == 1 {
          ledger_delta_stage_two(pre, witness, auxiliary, control)
        } else {
          ledger_delta_stage_three(pre, witness, auxiliary, control)
        }
    },
  }
}

fn ledger_delta_control_is_well_formed(
  witness: ValidationOneStepWitnessV1,
  control: LedgerDeltaControlV1,
) -> Bool {
  and {
    control.resolved_input_count >= 0,
    control.resolved_input_count <= max_tx_size_derived_collection_item_count,
    bytearray.length(control.resolved_inputs_accumulator) == 32,
    control.output_count >= 0,
    control.output_count <= max_tx_size_derived_collection_item_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.output_count,
      control.output_descriptor_peaks,
    ),
    bytearray.length(control.replay_schedule_hash) == 32,
    bytearray.length(control.replay_accumulator) == 32,
    bytearray.length(control.replay_remaining_schedule_hash) == 32,
    bytearray.length(control.current_ledger_root) == 32,
    control.replay_cursor >= 0,
    control.replay_cursor <= control.resolved_input_count,
    control.output_cursor >= 0,
    control.output_cursor <= control.output_count,
    control.operation_count >= 0,
    control.operation_count <= max_tx_size_derived_collection_item_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.operation_count,
      control.operation_peaks,
    ),
    control.stage >= 0,
    control.stage <= 2,
    witness.work_witness_cbor == encode_ledger_delta_control_v1(control),
  }
}

pub fn verify_ledger_delta_replay_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = ledger_delta_control_from_witness(witness.work_witness_cbor)
  and {
    ledger_delta_control_is_well_formed(witness, control),
    control.pending_mutation_cbor == #"",
    control.stage == 0,
    control.replay_remaining_schedule_hash == empty_resolution_schedule_hash(),
    control.replay_cursor == control.resolved_input_count,
    control.replay_accumulator == control.resolved_inputs_accumulator,
    ledger_delta_successor_is_exact(
      pre,
      witness,
      LedgerDeltaControlV1 { ..control, stage: 1 },
    ),
  }
}

pub fn verify_ledger_delta_output_finish_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = ledger_delta_control_from_witness(witness.work_witness_cbor)
  and {
    ledger_delta_control_is_well_formed(witness, control),
    control.pending_mutation_cbor == #"",
    control.stage == 1,
    control.output_cursor == control.output_count,
    ledger_delta_successor_is_exact(
      pre,
      witness,
      LedgerDeltaControlV1 { ..control, stage: 2 },
    ),
  }
}

pub fn verify_ledger_delta_terminal_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = ledger_delta_control_from_witness(witness.work_witness_cbor)
  and {
    ledger_delta_control_is_well_formed(witness, control),
    control.pending_mutation_cbor == #"",
    control.stage == 2,
    ledger_delta_stage_three(pre, witness, NoAuxiliaryWitness, control),
  }
}

pub fn verify_ledger_delta_operation_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  operation_kind: Int,
  key: ByteArray,
  value: ByteArray,
  operation_proof: LedgerDeltaOperationProofV1,
) -> Bool {
  let control = ledger_delta_control_from_witness(witness.work_witness_cbor)
  and {
    ledger_delta_control_is_well_formed(witness, control),
    control.pending_mutation_cbor == #"",
    control.stage == 0 || control.stage == 1,
    ledger_delta_operation_step(
      pre,
      witness,
      LedgerDeltaOperationWitness {
        operation_kind,
        key,
        value,
        operation_proof,
      },
      control,
    ),
  }
}

pub fn verify_ledger_delta_replay_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  value: ByteArray,
) -> Bool {
  let control = ledger_delta_control_from_witness(witness.work_witness_cbor)
  and {
    ledger_delta_control_is_well_formed(witness, control),
    control.stage == 0,
    control.replay_remaining_schedule_hash != empty_resolution_schedule_hash(),
    if source_kind == 0 {
      control.pending_mutation_cbor != #""
    } else {
      control.pending_mutation_cbor == #""
    },
    ledger_delta_replay_step(
      pre,
      witness,
      LedgerDeltaReplayWitness { source_kind, key, next_schedule_hash, value },
      control,
    ),
  }
}

pub fn verify_ledger_delta_output_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  output_index: Int,
  descriptor_cbor: ByteArray,
  siblings: List<ByteArray>,
) -> Bool {
  let control = ledger_delta_control_from_witness(witness.work_witness_cbor)
  and {
    ledger_delta_control_is_well_formed(witness, control),
    control.stage == 1,
    control.pending_mutation_cbor != #"",
    control.output_cursor < control.output_count,
    ledger_delta_output_step(
      pre,
      witness,
      LedgerDeltaOutputWitness { output_index, descriptor_cbor, siblings },
      control,
    ),
  }
}

pub fn verify_ledger_delta_proof_frame_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  frame: mpf_proof_fold_v1.ProofFrameV1,
  siblings: List<ByteArray>,
) -> Bool {
  let control = ledger_delta_control_from_witness(witness.work_witness_cbor)
  expect control.pending_mutation_cbor != #""
  let pending = ledger_delta_pending_mutation_from_cbor(
    control.pending_mutation_cbor,
  )
  and {
    ledger_delta_control_is_well_formed(witness, control),
    control.stage == 0 || control.stage == 1,
    pending.authorization_stage == 1,
    !mpf_proof_fold_v1.fold_is_complete_v1(pending.fold_control),
    ledger_delta_pending_step(
      pre,
      witness,
      LedgerDeltaProofFrameWitness { frame, siblings },
      control,
    ),
  }
}

pub fn verify_ledger_delta_finalize_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  let control = ledger_delta_control_from_witness(witness.work_witness_cbor)
  expect control.pending_mutation_cbor != #""
  let pending = ledger_delta_pending_mutation_from_cbor(
    control.pending_mutation_cbor,
  )
  and {
    ledger_delta_control_is_well_formed(witness, control),
    control.stage == 0 || control.stage == 1,
    pending.authorization_stage == 1,
    mpf_proof_fold_v1.fold_is_complete_v1(pending.fold_control),
    ledger_delta_pending_step(pre, witness, NoAuxiliaryWitness, control),
  }
}

// Midgard V1 is deliberately closed over the instructions implemented here.
// Returning False for an unimplemented phase prevents a deployment from
// treating structural trace commitments as semantic L1 verification.
fn verify_one_step_with_auxiliary(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  and {
    structural_transition_is_valid(pre, witness),
    when pre.phase is {
      validation_trace_v1.CanonicalDecode ->
        verify_canonical_decode(pre, witness, auxiliary)
      validation_trace_v1.CompactBinding -> and {
          auxiliary == NoAuxiliaryWitness,
          verify_compact_binding(pre, witness),
        }
      validation_trace_v1.InputSets ->
        verify_input_sets(pre, witness, auxiliary)
      validation_trace_v1.StaticLedgerRules -> and {
          auxiliary == NoAuxiliaryWitness,
          verify_static_rules(pre, witness),
        }
      validation_trace_v1.ResolveInputs ->
        verify_resolve_inputs(pre, witness, auxiliary)
      validation_trace_v1.Signatures ->
        verify_signatures(pre, witness, auxiliary)
      validation_trace_v1.ValueAndMint ->
        verify_value_and_mint(pre, witness, auxiliary)
      validation_trace_v1.ScriptSources ->
        verify_script_sources(pre, witness, auxiliary)
      validation_trace_v1.NativeScripts ->
        verify_native_scripts(pre, witness, auxiliary)
      validation_trace_v1.PhaseANativeScripts ->
        verify_phase_a_native_scripts_scan(pre, witness, auxiliary)
      validation_trace_v1.PhaseAScriptPreconditions ->
        verify_phase_a_script_preconditions(pre, witness, auxiliary)
      validation_trace_v1.ScriptIntegrity ->
        verify_script_integrity(pre, witness, auxiliary)
      validation_trace_v1.Cek -> verify_cek(pre, witness, auxiliary)
      validation_trace_v1.LedgerDelta ->
        verify_ledger_delta(pre, witness, auxiliary)
      validation_trace_v1.Terminal -> False
    },
  }
}

pub fn verify_one_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  verify_one_step_with_auxiliary(pre, witness, NoAuxiliaryWitness)
}

pub fn verify_one_step_evidence(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  verify_one_step_with_auxiliary(pre, evidence.transition, evidence.auxiliary)
}

pub fn verify_canonical_decode_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.CanonicalDecode,
    structural_transition_is_valid(pre, evidence.transition),
    verify_canonical_decode(pre, evidence.transition, evidence.auxiliary),
  }
}

pub fn verify_canonical_decode_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  verify_canonical_decode(pre, evidence.transition, evidence.auxiliary)
}

pub fn verify_compact_binding_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.CompactBinding,
    structural_transition_is_valid(pre, evidence.transition),
    evidence.auxiliary == NoAuxiliaryWitness,
    verify_compact_binding(pre, evidence.transition),
  }
}

/// Semantic half used after a resolver preparation step has authenticated the
/// common machine-state transition and the challenger successor commitment.
pub fn verify_compact_binding_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    evidence.auxiliary == NoAuxiliaryWitness,
    verify_compact_binding(pre, evidence.transition),
  }
}

pub fn verify_static_ledger_rules_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.StaticLedgerRules,
    structural_transition_is_valid(pre, evidence.transition),
    evidence.auxiliary == NoAuxiliaryWitness,
    verify_static_rules(pre, evidence.transition),
  }
}

pub fn verify_static_ledger_rules_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    evidence.auxiliary == NoAuxiliaryWitness,
    verify_static_rules(pre, evidence.transition),
  }
}

pub fn verify_input_sets_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.InputSets,
    structural_transition_is_valid(pre, evidence.transition),
    verify_input_sets(pre, evidence.transition, evidence.auxiliary),
  }
}

pub fn verify_input_sets_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  verify_input_sets(pre, evidence.transition, evidence.auxiliary)
}

pub fn verify_signatures_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.Signatures,
    structural_transition_is_valid(pre, evidence.transition),
    verify_signatures(pre, evidence.transition, evidence.auxiliary),
  }
}

pub fn verify_phase_a_native_scripts_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.PhaseANativeScripts,
    structural_transition_is_valid(pre, evidence.transition),
    verify_phase_a_native_scripts_scan(
      pre,
      evidence.transition,
      evidence.auxiliary,
    ),
  }
}

pub fn verify_phase_a_native_scripts_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  verify_phase_a_native_scripts_scan(
    pre,
    evidence.transition,
    evidence.auxiliary,
  )
}

pub fn verify_phase_a_script_preconditions_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.PhaseAScriptPreconditions,
    structural_transition_is_valid(pre, evidence.transition),
    verify_phase_a_script_preconditions(
      pre,
      evidence.transition,
      evidence.auxiliary,
    ),
  }
}

pub fn verify_phase_a_script_preconditions_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  verify_phase_a_script_preconditions(
    pre,
    evidence.transition,
    evidence.auxiliary,
  )
}

pub fn verify_resolve_inputs_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.ResolveInputs,
    structural_transition_is_valid(pre, evidence.transition),
    verify_resolve_inputs(pre, evidence.transition, evidence.auxiliary),
  }
}

pub fn verify_script_sources_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.ScriptSources,
    structural_transition_is_valid(pre, evidence.transition),
    verify_script_sources(pre, evidence.transition, evidence.auxiliary),
  }
}

pub fn verify_native_scripts_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.NativeScripts,
    structural_transition_is_valid(pre, evidence.transition),
    verify_native_scripts(pre, evidence.transition, evidence.auxiliary),
  }
}

pub fn verify_script_integrity_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.ScriptIntegrity,
    structural_transition_is_valid(pre, evidence.transition),
    verify_script_integrity(pre, evidence.transition, evidence.auxiliary),
  }
}

pub fn verify_script_integrity_authentication_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.ScriptIntegrity,
    structural_transition_is_valid(pre, witness),
    verify_script_integrity_authentication(pre, witness, NoAuxiliaryWitness),
  }
}

pub fn verify_script_integrity_compact_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.ScriptIntegrity,
    structural_transition_is_valid(pre, witness),
    verify_script_integrity_compact(pre, witness, NoAuxiliaryWitness),
  }
}

pub fn verify_script_integrity_witness_set_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.ScriptIntegrity,
    structural_transition_is_valid(pre, witness),
    verify_script_integrity_witness_set(pre, witness, NoAuxiliaryWitness),
  }
}

pub fn verify_script_integrity_finalize_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.ScriptIntegrity,
    structural_transition_is_valid(pre, witness),
    verify_script_integrity_finalize(pre, witness, NoAuxiliaryWitness),
  }
}

pub fn verify_cek_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.Cek,
    structural_transition_is_valid(pre, evidence.transition),
    verify_cek(pre, evidence.transition, evidence.auxiliary),
  }
}

pub fn verify_value_and_mint_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.ValueAndMint,
    structural_transition_is_valid(pre, evidence.transition),
    verify_value_and_mint(pre, evidence.transition, evidence.auxiliary),
  }
}

pub fn verify_ledger_delta_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.LedgerDelta,
    structural_transition_is_valid(pre, evidence.transition),
    verify_ledger_delta(pre, evidence.transition, evidence.auxiliary),
  }
}
