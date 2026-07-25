use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_224, blake2b_256, verify_ed25519_signature}
use aiken/merkle_patricia_forestry.{Proof}
use aiken/merkle_patricia_forestry/merkling.{null_hash}
use aiken/primitive/bytearray
use midgard/bounded_collection_v1
use midgard/bounded_item_v1
use midgard/cek_constant_v1
use midgard/cek_data_scan_v1
use midgard/cek_data_v1
use midgard/cek_machine_v1
use midgard/cek_proof_v1
use midgard/fraud_proofs/native_tx/codec.{
  decode_canonical_int_at, encode_definite_array_header, encode_definite_bytes,
  encode_native_byte_list,
}
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_compact_v1, decode_native_tx_field_preimage_lengths_v1,
  native_tx_canonical_size_v1, native_tx_proof_commitment_v1,
  verify_native_tx_proof_source_v1,
}
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_tx_input_cbor, encode_midgard_tx_input,
}
use midgard/fraud_proofs/native_tx/preimages.{
  canonical_bytes_key_precedes, decode_canonical_bytes_at,
  decode_canonical_map_header_at,
  decode_midgard_tx_address_witnesses_preimage_cbor,
  decode_midgard_tx_byte_list_preimage_cbor,
  decode_midgard_tx_inputs_preimage_cbor, decode_midgard_tx_mint_preimage_cbor,
  decode_midgard_tx_redeemer_witnesses_preimage_cbor,
  decode_midgard_tx_script_witnesses_preimage_cbor, encode_mint_preimage,
}
use midgard/fraud_proofs/native_tx/transaction.{
  verify_midgard_transaction_field_preimage_v1,
  verify_midgard_transaction_mint_preimage_commitment_v1,
}
use midgard/fraud_proofs/native_tx/types.{
  CertRedeemer, MidgardAddressWitness, MidgardRedeemerWitness,
  MidgardScriptWitness, MidgardTxInput, MidgardTxOutput, MidgardValue,
  MintRedeemer, NativeCardanoScript, NativeTxBodyCompact, NativeTxCompact,
  NativeTxFieldPreimageLengthsV1, NativeTxWitnessSetCompact, ProposeRedeemer,
  PubKeyCredential, ReceiveRedeemer, RewardRedeemer, ScriptCredential,
  SpendRedeemer, VerifiedMidgardNativeTxCompact, VoteRedeemer,
}
use midgard/ledger_output_v1
use midgard/mpf_proof_v1
use midgard/native_script_v1
use midgard/script_context_v1
use midgard/script_language_views_v1
use midgard/script_proof_v1
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

pub type ValidationOneStepWitnessV1 {
  work_witness_cbor: ByteArray,
  claimed_successor: validation_trace_v1.ValidationMachineStateV1,
}

pub type CekDataScanStepV1 {
  CekDataOpenConstructor {
    raw_cbor: ByteArray,
    parent: Option<cek_data_scan_v1.DataScanFrameV1>,
    constructor: Int,
    expected_children: Int,
  }
  CekDataOpenList {
    raw_cbor: ByteArray,
    parent: Option<cek_data_scan_v1.DataScanFrameV1>,
    expected_children: Int,
  }
  CekDataOpenMap {
    raw_cbor: ByteArray,
    parent: Option<cek_data_scan_v1.DataScanFrameV1>,
  }
  CekDataRevealLeaf {
    raw_cbor: ByteArray,
    parent: Option<cek_data_scan_v1.DataScanFrameV1>,
    item_length: Int,
  }
  CekDataCloseSequence {
    raw_cbor: ByteArray,
    frame: cek_data_scan_v1.DataScanFrameV1,
  }
  CekDataFoldList {
    frame: cek_data_scan_v1.DataScanFrameV1,
    child_index: Int,
    child: cek_data_v1.DataSummaryV1,
    siblings: List<ByteArray>,
  }
  CekDataFoldMap {
    frame: cek_data_scan_v1.DataScanFrameV1,
    pair_index: Int,
    key: cek_data_v1.DataSummaryV1,
    value: cek_data_v1.DataSummaryV1,
    key_siblings: List<ByteArray>,
    value_siblings: List<ByteArray>,
  }
  CekDataFinalizeFrame {
    frame: cek_data_scan_v1.DataScanFrameV1,
    parent: Option<cek_data_scan_v1.DataScanFrameV1>,
  }
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

pub type ValidationAuxiliaryWitnessV1 {
  NoAuxiliaryWitness
  TransactionFieldPreimageWitness { preimage_cbor: ByteArray }
  TransactionFieldChunkWitness {
    collection_proof: bounded_collection_v1.ItemProofV1,
    chunk_proof: bounded_item_v1.ChunkProofV1,
  }
  TransactionFieldPairPreimageWitness {
    first_field_index: Int,
    first_preimage_cbor: ByteArray,
    second_field_index: Int,
    second_preimage_cbor: ByteArray,
  }
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
    script: MidgardScriptWitness,
    siblings: List<ByteArray>,
  }
  RedeemerScanWitness {
    redeemer_index: Int,
    redeemer: MidgardRedeemerWitness,
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
    redeemer_index: Int,
    redeemer: MidgardRedeemerWitness,
    redeemer_siblings: List<ByteArray>,
    purpose_frontier_index: Int,
    purpose_kind: Int,
    purpose_index: Int,
    script_hash: ByteArray,
    subject: ByteArray,
    purpose_siblings: List<ByteArray>,
  }
  CekDataScanStepWitness {
    redeemer_control: CekRedeemerContextControlV1,
    control: cek_data_scan_v1.DataScanControlV1,
    step: CekDataScanStepV1,
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
    value: ByteArray,
    asset_index: Int,
    mutation: ValueAssetMutationWitnessV1,
  }
  ValueOutputAssetWitness {
    output_index: Int,
    output_cbor: ByteArray,
    siblings: List<ByteArray>,
    asset_index: Int,
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
    proof: Proof,
  }
  LedgerDeltaOutputWitness {
    output_index: Int,
    output_cbor: ByteArray,
    siblings: List<ByteArray>,
    proof: Proof,
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
        validation_trace_v1.PhaseAScriptPreconditions ->
          post.phase == validation_trace_v1.ResolveInputs
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
        validation_trace_v1.ScriptIntegrity ->
          post.phase == validation_trace_v1.Cek
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
  expect bytearray.length(previous_key) == 0 || bytearray.length(previous_key) == 36
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

pub fn encode_phase_a_native_scripts_witness(
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  resolution_schedule_hash: ByteArray,
  witness_signer_hashes_cbor: ByteArray,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
) -> ByteArray {
  expect bytearray.length(resolution_schedule_hash) == 32
  expect signer_count >= 0
  expect bytearray.length(signer_frontier_commitment) == 32
  #"88"
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
    |> bytearray.concat(encode_definite_bytes(resolution_schedule_hash))
    |> bytearray.concat(encode_definite_bytes(witness_signer_hashes_cbor))
    |> bytearray.concat(cbor.serialise(signer_count))
    |> bytearray.concat(encode_definite_bytes(signer_frontier_commitment))
}

pub fn encode_signatures_witness(
  compact_cbor: ByteArray,
  witness_set_compact_cbor: ByteArray,
  field_preimage_lengths_cbor: ByteArray,
  context_cbor: ByteArray,
  resolution_schedule_hash: ByteArray,
) -> ByteArray {
  expect bytearray.length(resolution_schedule_hash) == 32
  #"85"
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
    |> bytearray.concat(encode_definite_bytes(resolution_schedule_hash))
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
) -> ByteArray {
  expect bytearray.length(resolution_schedule_hash) == 32
  expect signer_count >= 0
  expect bytearray.length(signer_frontier_commitment) == 32
  #"88"
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

fn scheduled_input_resolution_hash(
  inputs: List<ScheduledInputV1>,
) -> ByteArray {
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
) -> ByteArray {
  expect cursor >= 0
  expect bytearray.length(accumulator) == 32
  expect bytearray.length(remaining_schedule_hash) == 32
  expect signer_count >= 0
  expect bytearray.length(signer_frontier_commitment) == 32
  #"89"
    |> bytearray.concat(encode_definite_bytes(compact_cbor))
    |> bytearray.concat(encode_definite_bytes(witness_set_compact_cbor))
    |> bytearray.concat(encode_definite_bytes(field_preimage_lengths_cbor))
    |> bytearray.concat(encode_definite_bytes(context_cbor))
    |> bytearray.concat(cbor.serialise(cursor))
    |> bytearray.concat(encode_definite_bytes(accumulator))
    |> bytearray.concat(encode_definite_bytes(remaining_schedule_hash))
    |> bytearray.concat(cbor.serialise(signer_count))
    |> bytearray.concat(encode_definite_bytes(signer_frontier_commitment))
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
  receive_hashes: List<ByteArray>,
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
  encode_definite_array_header(24)
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
    |> bytearray.concat(encode_native_byte_list(receive_hashes))
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

const reject_script_execution_count =
  #"455f5343524950545f455845435554494f4e5f434f554e54"

const reject_script_program_encoding =
  #"455f5343524950545f50524f4752414d5f454e434f44494e47"

const reject_field_preimage_size = #"455f4649454c445f505245494d4147455f53495a45"

const reject_ledger_output_size = #"455f4c45444745525f4f55545055545f53495a45"

const reject_asset_count = #"455f41535345545f434f554e54"

const reject_invalid_field_type = #"455f494e56414c49445f4649454c445f54595045"

const reject_native_script_depth = #"455f4e41544956455f5343524950545f4445505448"

const reject_native_script_node_count =
  #"455f4e41544956455f5343524950545f4e4f44455f434f554e54"

fn first_native_script_bound_rejection(
  scripts: List<MidgardScriptWitness>,
) -> Option<ByteArray> {
  when scripts is {
    [] -> None
    [script, ..rest] ->
      if script.language == NativeCardanoScript {
        when
          native_script_v1.inspect_native_script(
            script.script_bytes,
            -1,
            -1,
            [],
          )
        is {
          None -> Some(reject_invalid_field_type)
          Some(checked) ->
            if checked.depth > native_script_v1.max_native_script_depth {
              Some(reject_native_script_depth)
            } else if checked.node_count > native_script_v1.max_native_script_node_count {
              Some(reject_native_script_node_count)
            } else {
              first_native_script_bound_rejection(rest)
            }
        }
      } else {
        when cek_proof_v1.inspect_program_envelope_v1(script.script_bytes) is {
          None -> Some(reject_script_program_encoding)
          Some(_) -> first_native_script_bound_rejection(rest)
        }
      }
  }
}

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
    6 -> lengths.address_witnesses
    7 -> lengths.script_witnesses
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
    6 -> witness_set.addr_tx_wits_hash
    7 -> witness_set.script_tx_wits_hash
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
    field_index == 6,
  } {
    Some(canonical_argument_header_size(item_length) + item_length)
  } else if or {
    field_index == 7,
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
    witness_set_compact_cbor: builtin.un_b_data(
      witness_set_compact_cbor_data,
    ),
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
    bounded_item_v1.verify_chunk(
      collection_proof.item_commitment,
      chunk_proof,
    ),
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
  let control = canonical_decode_control_from_witness(
    witness.work_witness_cbor,
  )
  when auxiliary is {
    NoAuxiliaryWitness ->
      verify_canonical_decode_empty(pre, witness, control)
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

const reject_output_count = #"455f4f55545055545f434f554e54"

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
  let empty_delta_cbor = #"80"
  and {
    post.phase == validation_trace_v1.Terminal,
    post.verdict == validation_trace_v1.Rejected,
    post.rejection_code_hash == validation_trace_v1.hash_rejection_code(
      rejection_code,
    ),
    post.ledger_delta_root == validation_trace_v1.hash_ledger_delta(
      empty_delta_cbor,
    ),
    post.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      pre.program_counter + 1,
      encode_terminal_rejection_witness(rejection_code, pre.prior_ledger_root),
    ),
  }
}

const max_tx_size_derived_collection_item_count: Int = 16384

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
    witness_set_compact_cbor: builtin.un_b_data(
      witness_set_compact_cbor_data,
    ),
    field_preimage_lengths_cbor: builtin.un_b_data(
      field_preimage_lengths_cbor_data,
    ),
    context_cbor: builtin.un_b_data(context_cbor_data),
    spend_count: builtin.un_i_data(spend_count_data),
    reference_count: builtin.un_i_data(reference_count_data),
    spend_seen: builtin.un_i_data(spend_seen_data),
    reference_seen: builtin.un_i_data(reference_seen_data),
    previous_key: builtin.un_b_data(previous_key_data),
    resolution_schedule_hash: builtin.un_b_data(
      resolution_schedule_hash_data,
    ),
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
      and {
        post.phase == validation_trace_v1.Signatures,
        post.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.Signatures,
          pre.program_counter + 1,
          encode_signatures_witness(
            control.compact_cbor,
            control.witness_set_compact_cbor,
            control.field_preimage_lengths_cbor,
            control.context_cbor,
            resolution_schedule_hash,
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
    bounded_item_v1.verify_chunk(
      collection_proof.item_commitment,
      chunk_proof,
    ),
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
  let control = input_sets_control_from_witness(
    witness.work_witness_cbor,
  )
  when auxiliary is {
    NoAuxiliaryWitness ->
      verify_input_sets_empty(pre, witness, control)
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

fn witness_key_hashes(witnesses: List<MidgardAddressWitness>) -> List<ByteArray> {
  when witnesses is {
    [] -> []
    [witness, ..rest] ->
      [blake2b_224(witness.verification_key), ..witness_key_hashes(rest)]
  }
}

fn canonical_signer_hashes(
  witnesses: List<MidgardAddressWitness>,
) -> List<ByteArray> {
  witness_key_hashes(witnesses)
    |> list.sort(bytearray.compare)
    |> list.unique
}

fn signer_frontier(
  signer_hashes: List<ByteArray>,
) -> validation_merkle_v1.BuiltFrontier {
  signer_hashes
    |> list.map(script_proof_v1.signer_leaf_hash)
    |> validation_merkle_v1.build_frontier
}

fn required_signers_are_witnessed(
  required_signers: List<ByteArray>,
  witnessed_signers: List<ByteArray>,
) -> Bool {
  when required_signers is {
    [] -> True
    [required_signer, ..rest] -> and {
        list.has(witnessed_signers, required_signer),
        required_signers_are_witnessed(rest, witnessed_signers),
      }
  }
}

fn every_address_signature_is_valid(
  transaction_id: ByteArray,
  witnesses: List<MidgardAddressWitness>,
) -> Bool {
  when witnesses is {
    [] -> True
    [witness, ..rest] -> and {
        verify_ed25519_signature(
          witness.verification_key,
          transaction_id,
          witness.signature,
        ),
        every_address_signature_is_valid(transaction_id, rest),
      }
  }
}

fn signatures_rejection(
  transaction_id: ByteArray,
  required_signers: List<ByteArray>,
  address_witnesses: List<MidgardAddressWitness>,
) -> Option<ByteArray> {
  if !required_signers_are_witnessed(
    required_signers,
    witness_key_hashes(address_witnesses),
  ) {
    Some(reject_missing_required_witness)
  } else if !every_address_signature_is_valid(transaction_id, address_witnesses) {
    Some(reject_invalid_signature)
  } else {
    None
  }
}

fn verify_signatures(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  expect Some(data) = cbor.deserialise(witness.work_witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data,
        resolution_schedule_hash_data,
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
  let resolution_schedule_hash = builtin.un_b_data(
    resolution_schedule_hash_data,
  )
  expect bytearray.length(resolution_schedule_hash) == 32
  expect
      TransactionFieldPairPreimageWitness {
        first_field_index,
        first_preimage_cbor,
        second_field_index,
        second_preimage_cbor,
      }
    = auxiliary
  expect first_field_index == 4
  expect second_field_index == 6
  let first_verified = verify_midgard_transaction_field_preimage_v1(
    pre.transaction_id,
    pre.transaction_commitment,
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
    first_field_index,
    first_preimage_cbor,
  )
  let second_verified = verify_midgard_transaction_field_preimage_v1(
    pre.transaction_id,
    pre.transaction_commitment,
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
    second_field_index,
    second_preimage_cbor,
  )
  let required_signers = decode_midgard_tx_byte_list_preimage_cbor(
    first_preimage_cbor,
  )
  let address_witnesses = decode_midgard_tx_address_witnesses_preimage_cbor(
    second_preimage_cbor,
  )
  let canonical_signers = canonical_signer_hashes(address_witnesses)
  let built_signer_frontier = signer_frontier(canonical_signers)
  and {
    first_verified,
    second_verified,
    validation_trace_v1.hash_validation_context(context_cbor) == pre.validation_context_hash,
    witness.work_witness_cbor == encode_signatures_witness(
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
      context_cbor,
      resolution_schedule_hash,
    ),
    when
      signatures_rejection(
        pre.transaction_id,
        required_signers,
        address_witnesses,
      )
    is {
      Some(rejection_code) ->
        rejected_successor_is_exact(
          pre,
          witness.claimed_successor,
          rejection_code,
        )
      None -> and {
          witness.claimed_successor.phase == validation_trace_v1.PhaseANativeScripts,
          witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
            validation_trace_v1.PhaseANativeScripts,
            pre.program_counter + 1,
            encode_phase_a_native_scripts_witness(
              compact_cbor,
              witness_set_compact_cbor,
              field_preimage_lengths_cbor,
              context_cbor,
              resolution_schedule_hash,
              encode_native_byte_list(canonical_signers),
              built_signer_frontier.count,
              validation_merkle_v1.frontier_commitment(
                built_signer_frontier.count,
                built_signer_frontier.peaks,
              ),
            ),
          ),
        }
    },
  }
}

const reject_native_script_invalid =
  #"455f4e41544956455f5343524950545f494e56414c4944"

const reject_plutus_script_invalid =
  #"455f504c555455535f5343524950545f494e56414c4944"

fn phase_a_native_scripts_rejection(
  scripts: List<MidgardScriptWitness>,
  validity_interval_start: Int,
  validity_interval_end: Int,
  witness_signers: List<ByteArray>,
) -> Option<ByteArray> {
  when scripts is {
    [] -> None
    [script, ..rest] ->
      if script.language == NativeCardanoScript {
        when
          native_script_v1.inspect_native_script(
            script.script_bytes,
            validity_interval_start,
            validity_interval_end,
            witness_signers,
          )
        is {
          None -> Some(reject_invalid_field_type)
          Some(checked) ->
            if checked.depth > native_script_v1.max_native_script_depth {
              Some(reject_native_script_depth)
            } else if checked.node_count > native_script_v1.max_native_script_node_count {
              Some(reject_native_script_node_count)
            } else if !checked.valid {
              Some(reject_native_script_invalid)
            } else {
              phase_a_native_scripts_rejection(
                rest,
                validity_interval_start,
                validity_interval_end,
                witness_signers,
              )
            }
        }
      } else {
        phase_a_native_scripts_rejection(
          rest,
          validity_interval_start,
          validity_interval_end,
          witness_signers,
        )
      }
  }
}

fn verify_phase_a_native_scripts(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  expect Some(data) = cbor.deserialise(witness.work_witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data,
        resolution_schedule_hash_data, witness_signer_hashes_cbor_data,
        signer_count_data, signer_frontier_commitment_data,
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
  let resolution_schedule_hash = builtin.un_b_data(
    resolution_schedule_hash_data,
  )
  expect bytearray.length(resolution_schedule_hash) == 32
  let witness_signer_hashes_cbor = builtin.un_b_data(
    witness_signer_hashes_cbor_data,
  )
  let witness_signers = decode_midgard_tx_byte_list_preimage_cbor(
    witness_signer_hashes_cbor,
  )
  let signer_count = builtin.un_i_data(signer_count_data)
  let signer_frontier_commitment = builtin.un_b_data(
    signer_frontier_commitment_data,
  )
  let built_signer_frontier = signer_frontier(witness_signers)
  expect TransactionFieldPreimageWitness { preimage_cbor } = auxiliary
  let field_verified = verify_midgard_transaction_field_preimage_v1(
    pre.transaction_id,
    pre.transaction_commitment,
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
    7,
    preimage_cbor,
  )
  let scripts = decode_midgard_tx_script_witnesses_preimage_cbor(preimage_cbor)
  let Pair(verified_source, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
  )
  and {
    field_verified,
    list.all(witness_signers, fn(hash) { bytearray.length(hash) == 28 }),
    witness_signers == (
      witness_signers |> list.sort(bytearray.compare) |> list.unique
    ),
    signer_count == built_signer_frontier.count,
    signer_frontier_commitment == validation_merkle_v1.frontier_commitment(
      built_signer_frontier.count,
      built_signer_frontier.peaks,
    ),
    validation_trace_v1.hash_validation_context(context_cbor) == pre.validation_context_hash,
    witness.work_witness_cbor == encode_phase_a_native_scripts_witness(
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
      context_cbor,
      resolution_schedule_hash,
      witness_signer_hashes_cbor,
      signer_count,
      signer_frontier_commitment,
    ),
    when
      phase_a_native_scripts_rejection(
        scripts,
        verified_source.tx_compact.body.validity_interval_start,
        verified_source.tx_compact.body.validity_interval_end,
        witness_signers,
      )
    is {
      Some(rejection_code) ->
        rejected_successor_is_exact(
          pre,
          witness.claimed_successor,
          rejection_code,
        )
      None -> and {
          witness.claimed_successor.phase == validation_trace_v1.PhaseAScriptPreconditions,
          witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
            validation_trace_v1.PhaseAScriptPreconditions,
            pre.program_counter + 1,
            encode_phase_a_script_preconditions_witness(
              compact_cbor,
              witness_set_compact_cbor,
              field_preimage_lengths_cbor,
              context_cbor,
              resolution_schedule_hash,
              signer_count,
              signer_frontier_commitment,
              has_non_native_script(scripts),
            ),
          ),
        }
    },
  }
}

fn no_duplicate_bytes(items: List<ByteArray>) -> Bool {
  when items is {
    [] -> True
    [item, ..rest] -> and {
        !list.has(rest, item),
        no_duplicate_bytes(rest),
      }
  }
}

fn has_non_native_script(scripts: List<MidgardScriptWitness>) -> Bool {
  when scripts is {
    [] -> False
    [script, ..rest] -> or {
        script.language != NativeCardanoScript,
        has_non_native_script(rest),
      }
  }
}

fn phase_a_script_preconditions_rejection(
  required_observers: List<ByteArray>,
  redeemers: List<MidgardRedeemerWitness>,
  contains_non_native_script: Bool,
  script_integrity_hash: ByteArray,
  network_id: Int,
) -> Option<ByteArray> {
  if !no_duplicate_bytes(required_observers) {
    Some(reject_invalid_field_type)
  } else {
    let requires_plutus_evaluation = or {
      contains_non_native_script,
      redeemers != [],
      script_integrity_hash != #"0000000000000000000000000000000000000000000000000000000000000000",
    }
    if !requires_plutus_evaluation {
      None
    } else if script_integrity_hash == #"0000000000000000000000000000000000000000000000000000000000000000" {
      Some(reject_invalid_field_type)
    } else if and {
      required_observers != [],
      network_id == 255,
    } {
      Some(reject_invalid_field_type)
    } else {
      None
    }
  }
}

fn verify_phase_a_script_preconditions(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  expect Some(data) = cbor.deserialise(witness.work_witness_cbor)
  expect
      [
        compact_cbor_data, witness_set_compact_cbor_data,
        field_preimage_lengths_cbor_data, context_cbor_data,
        resolution_schedule_hash_data, signer_count_data,
        signer_frontier_commitment_data, contains_non_native_script_data,
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
  let resolution_schedule_hash = builtin.un_b_data(
    resolution_schedule_hash_data,
  )
  expect bytearray.length(resolution_schedule_hash) == 32
  let signer_count = builtin.un_i_data(signer_count_data)
  expect signer_count >= 0
  let signer_frontier_commitment = builtin.un_b_data(
    signer_frontier_commitment_data,
  )
  expect bytearray.length(signer_frontier_commitment) == 32
  let contains_non_native_script_code = builtin.un_i_data(
    contains_non_native_script_data,
  )
  expect or {
      contains_non_native_script_code == 0,
      contains_non_native_script_code == 1,
    }
  let contains_non_native_script = contains_non_native_script_code == 1
  expect
      TransactionFieldPairPreimageWitness {
        first_field_index,
        first_preimage_cbor,
        second_field_index,
        second_preimage_cbor,
      }
    = auxiliary
  expect first_field_index == 3
  expect second_field_index == 8
  let first_verified = verify_midgard_transaction_field_preimage_v1(
    pre.transaction_id,
    pre.transaction_commitment,
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
    first_field_index,
    first_preimage_cbor,
  )
  let second_verified = verify_midgard_transaction_field_preimage_v1(
    pre.transaction_id,
    pre.transaction_commitment,
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
    second_field_index,
    second_preimage_cbor,
  )
  let required_observers = decode_midgard_tx_byte_list_preimage_cbor(
    first_preimage_cbor,
  )
  let redeemers = decode_midgard_tx_redeemer_witnesses_preimage_cbor(
    second_preimage_cbor,
  )
  let Pair(verified_source, _) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    compact_cbor,
    witness_set_compact_cbor,
    field_preimage_lengths_cbor,
  )
  and {
    first_verified,
    second_verified,
    validation_trace_v1.hash_validation_context(context_cbor) == pre.validation_context_hash,
    witness.work_witness_cbor == encode_phase_a_script_preconditions_witness(
      compact_cbor,
      witness_set_compact_cbor,
      field_preimage_lengths_cbor,
      context_cbor,
      resolution_schedule_hash,
      signer_count,
      signer_frontier_commitment,
      contains_non_native_script,
    ),
    when
      phase_a_script_preconditions_rejection(
        required_observers,
        redeemers,
        contains_non_native_script,
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
              compact_cbor,
              witness_set_compact_cbor,
              field_preimage_lengths_cbor,
              context_cbor,
              0,
              initial_resolution_accumulator(),
              resolution_schedule_hash,
              signer_count,
              signer_frontier_commitment,
            ),
          ),
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
        signer_frontier_commitment_data,
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
  output: MidgardTxOutput,
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
    when output.address.payment_credential is {
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
}

fn resolve_membership_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ResolveInputsControlV1,
  source_kind: Int,
  key: ByteArray,
  next_schedule_hash: ByteArray,
  value: ByteArray,
  proof: Proof,
  signer_proof: SignerSetProofV1,
) -> Bool {
  if and {
    resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == control.remaining_schedule_hash,
    bytearray.length(value) <= ledger_output_v1.max_output_canonical_cbor_bytes,
    mpf_proof_v1.has(pre.prior_ledger_root, key, value, proof),
  } {
    when ledger_output_v1.decode_canonical_output(value) is {
      None ->
        rejected_successor_is_exact(
          pre,
          witness.claimed_successor,
          reject_invalid_output,
        )
      Some(output) ->
        when
          input_signer_authorization(
            source_kind,
            output,
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
          InputSignerAuthorized -> {
            let next_accumulator = resolved_input_accumulator_successor(
              control.accumulator,
              source_kind,
              key,
              value,
            )
            let next_control = encode_resolve_inputs_witness(
              control.compact_cbor,
              control.witness_set_compact_cbor,
              control.field_preimage_lengths_cbor,
              control.context_cbor,
              control.cursor + 1,
              next_accumulator,
              next_schedule_hash,
              control.signer_count,
              control.signer_frontier_commitment,
            )
            and {
              witness.claimed_successor.phase == validation_trace_v1.ResolveInputs,
              witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
                validation_trace_v1.ResolveInputs,
                pre.program_counter + 1,
                next_control,
              ),
            }
          }
        }
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
  when auxiliary is {
    ScheduledLedgerMembershipWitness {
      source_kind,
      key,
      next_schedule_hash,
      value,
      proof,
      signer_proof,
    } ->
      resolve_membership_step(
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
    NoAuxiliaryWitness -> False
    TransactionFieldPreimageWitness { .. } -> False
    TransactionFieldChunkWitness { .. } -> False
    TransactionFieldPairPreimageWitness { .. } -> False
    LedgerMembershipWitness { .. } -> False
    LedgerNonMembershipWitness { .. } -> False
    ResolvedInputReplayWitness { .. } -> False
    OutputReplayWitness { .. } -> False
    ScriptPurposeScanWitness { .. } -> False
    ScriptSourceScanWitness { .. } -> False
    RedeemerScanWitness { .. } -> False
    NativeExecutionScanWitness { .. } -> False
    CekCoreStepWitness { .. } -> False
    CekResolvedContextItemWitness { .. } -> False
    CekOutputContextItemWitness { .. } -> False
    CekSignerContextItemWitness { .. } -> False
    CekMintContextItemWitness { .. } -> False
    CekRedeemerContextSelectWitness { .. } -> False
    CekDataScanStepWitness { .. } -> False
    CekContextFinalizeWitness { .. } -> False
    CekContextFinalizeSpendWitness { .. } -> False
    CekContextAssembleWitness { .. } -> False
    CekTxInfoFinalizeWitness { .. } -> False
    CekContextSeedWitness { .. } -> False
    ValueInputAssetWitness { .. } -> False
    ValueOutputAssetWitness { .. } -> False
    ValueMintAssetWitness { .. } -> False
    LedgerDeltaReplayWitness { .. } -> False
    LedgerDeltaOutputWitness { .. } -> False
  }
}

fn resolve_inputs_control_is_bound(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ResolveInputsControlV1,
) -> Bool {
  and {
    native_tx_proof_commitment_v1(
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
    ) == pre.transaction_commitment,
    validation_trace_v1.hash_validation_context(control.context_cbor) == pre.validation_context_hash,
    bytearray.length(control.accumulator) == 32,
    bytearray.length(control.remaining_schedule_hash) == 32,
    control.signer_count >= 0,
    bytearray.length(control.signer_frontier_commitment) == 32,
    control.cursor >= 0,
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
    } else if control.remaining_schedule_hash == empty_resolution_schedule_hash() {
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
            [],
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
        [],
      ),
    ),
  }
}

pub fn verify_resolve_inputs_membership_semantics_v1(
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
    control.remaining_schedule_hash != empty_resolution_schedule_hash(),
    resolve_membership_step(
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
  receive_hashes: List<ByteArray>,
  discovery: ScriptDiscoveryControlV1,
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
  }
}

pub fn encode_script_discovery_control(
  control: ScriptDiscoveryControlV1,
) -> ByteArray {
  #"8e"
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

fn script_sources_control_from_data_items(
  items: List<Data>,
  discovery: ScriptDiscoveryControlV1,
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
        output_count_data, output_peaks_data, receive_hashes_data,
      ]
    = items
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
    receive_hashes: receive_hashes_data
      |> builtin.un_list_data
      |> list.map(builtin.un_b_data),
    discovery,
  }
}

fn script_sources_control_from_witness(
  work_witness_cbor: ByteArray,
) -> ScriptSourcesControlV1 {
  expect Some(data) = cbor.deserialise(work_witness_cbor)
  let items = builtin.un_list_data(data)
  if list.length(items) == 24 {
    script_sources_control_from_data_items(
      items,
      empty_script_discovery_control(),
    )
  } else {
    expect list.length(items) == 25
    let Pair(base_items, discovery_data) = split_last_data(items)
    script_sources_control_from_data_items(
      base_items,
      discovery_data
        |> builtin.un_b_data
        |> script_discovery_control_from_cbor,
    )
  }
}

fn inline_script_source_leaves(
  scripts: List<MidgardScriptWitness>,
  source_index: Int,
) -> List<ByteArray> {
  when scripts is {
    [] -> []
    [script, ..rest] ->
      [
        script_proof_v1.source_leaf_hash(
          0,
          cbor.serialise(source_index),
          script,
        ),
        ..inline_script_source_leaves(rest, source_index + 1)
      ]
  }
}

fn inline_script_source_frontier(
  scripts: List<MidgardScriptWitness>,
) -> validation_merkle_v1.BuiltFrontier {
  inline_script_source_leaves(scripts, 0)
    |> validation_merkle_v1.build_frontier
}

fn script_sources_stage_zero_rejection(
  scripts: List<MidgardScriptWitness>,
) -> Option<ByteArray> {
  if list.length(scripts) > 16 {
    // At most sixteen inline sources can be used by the bounded execution
    // schedule. A seventeenth source is necessarily an extraneous witness.
    Some(reject_invalid_field_type)
  } else {
    first_native_script_bound_rejection(scripts)
  }
}

fn redeemer_purpose_is_supported(redeemer: MidgardRedeemerWitness) -> Bool {
  when redeemer.purpose is {
    SpendRedeemer -> True
    MintRedeemer -> True
    RewardRedeemer -> True
    ReceiveRedeemer -> True
    CertRedeemer -> False
    VoteRedeemer -> False
    ProposeRedeemer -> False
  }
}

fn redeemer_data_is_canonical(redeemer: MidgardRedeemerWitness) -> Bool {
  when cbor.deserialise(redeemer.redeemer_cbor) is {
    None -> False
    Some(data) -> cbor.serialise(data) == redeemer.redeemer_cbor
  }
}

fn redeemer_pointer_is_absent(
  redeemer: MidgardRedeemerWitness,
  remaining: List<MidgardRedeemerWitness>,
) -> Bool {
  when remaining is {
    [] -> True
    [other, ..rest] -> and {
        or {
          other.purpose != redeemer.purpose,
          other.index != redeemer.index,
        },
        redeemer_pointer_is_absent(redeemer, rest),
      }
  }
}

fn redeemers_are_well_formed(redeemers: List<MidgardRedeemerWitness>) -> Bool {
  when redeemers is {
    [] -> True
    [redeemer, ..rest] -> and {
        redeemer_purpose_is_supported(redeemer),
        redeemer_data_is_canonical(redeemer),
        redeemer_pointer_is_absent(redeemer, rest),
        redeemers_are_well_formed(rest),
      }
  }
}

fn redeemer_frontier(
  redeemers: List<MidgardRedeemerWitness>,
) -> validation_merkle_v1.BuiltFrontier {
  redeemers
    |> list.map(script_proof_v1.redeemer_leaf_hash)
    |> validation_merkle_v1.build_frontier
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
    control.receive_hashes,
  )
  encode_definite_array_header(25)
    |> bytearray.concat(bytearray.slice(base, 1, bytearray.length(base) - 1))
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
  }
}

fn exact_script_sources_control(control: ScriptSourcesControlV1) -> ByteArray {
  if control.stage < 8 {
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
      control.receive_hashes,
    )
  } else {
    encode_script_sources_discovery_witness(
      control,
      control.stage,
      control.discovery,
    )
  }
}

fn script_sources_stage_zero(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  expect TransactionFieldPreimageWitness { preimage_cbor } = auxiliary
  let field_verified = verify_midgard_transaction_field_preimage_v1(
    pre.transaction_id,
    pre.transaction_commitment,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
    7,
    preimage_cbor,
  )
  let scripts = decode_midgard_tx_script_witnesses_preimage_cbor(preimage_cbor)
  let built_sources = inline_script_source_frontier(scripts)
  and {
    field_verified,
    control.source_count == 0,
    control.source_peaks == [],
    control.redeemer_count == 0,
    control.redeemer_peaks == [],
    control.resolved_item_peaks == [],
    when script_sources_stage_zero_rejection(scripts) is {
      Some(rejection_code) ->
        rejected_successor_is_exact(
          pre,
          witness.claimed_successor,
          rejection_code,
        )
      None -> {
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
          1,
          built_sources.count,
          built_sources.peaks,
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
          [],
        )
        and {
          witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
          witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
            validation_trace_v1.ScriptSources,
            pre.program_counter + 1,
            next_control,
          ),
        }
      }
    },
  }
}

fn script_sources_stage_one(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  expect TransactionFieldPreimageWitness { preimage_cbor } = auxiliary
  let field_verified = verify_midgard_transaction_field_preimage_v1(
    pre.transaction_id,
    pre.transaction_commitment,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
    8,
    preimage_cbor,
  )
  let redeemers = decode_midgard_tx_redeemer_witnesses_preimage_cbor(
    preimage_cbor,
  )
  let built_redeemers = redeemer_frontier(redeemers)
  let rejection_code = if list.length(redeemers) > 16 {
    Some(reject_script_execution_count)
  } else if !redeemers_are_well_formed(redeemers) {
    Some(reject_invalid_field_type)
  } else {
    None
  }
  and {
    field_verified,
    control.source_count <= 16,
    control.redeemer_count == 0,
    control.redeemer_peaks == [],
    control.resolved_item_peaks == [],
    when rejection_code is {
      Some(code) ->
        rejected_successor_is_exact(pre, witness.claimed_successor, code)
      None -> {
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
          2,
          control.source_count,
          control.source_peaks,
          built_redeemers.count,
          built_redeemers.peaks,
          0,
          initial_resolution_accumulator(),
          empty_resolution_schedule_hash(),
          0,
          0,
          validation_merkle_v1.empty_frontier(),
          0,
          0,
          validation_merkle_v1.empty_frontier(),
          [],
        )
        and {
          witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
          witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
            validation_trace_v1.ScriptSources,
            pre.program_counter + 1,
            next_control,
          ),
        }
      }
    },
  }
}

fn script_sources_stage_two(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  expect
      TransactionFieldPairPreimageWitness {
        first_field_index,
        first_preimage_cbor,
        second_field_index,
        second_preimage_cbor,
      }
    = auxiliary
  expect first_field_index == 0
  expect second_field_index == 1
  let spend_inputs = decode_midgard_tx_inputs_preimage_cbor(first_preimage_cbor)
  let reference_inputs = decode_midgard_tx_inputs_preimage_cbor(
    second_preimage_cbor,
  )
  let initial_schedule = transaction_resolution_schedule_hash(
    spend_inputs,
    reference_inputs,
  )
  let fields_verified = and {
    verify_midgard_transaction_field_preimage_v1(
      pre.transaction_id,
      pre.transaction_commitment,
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      first_field_index,
      first_preimage_cbor,
    ),
    verify_midgard_transaction_field_preimage_v1(
      pre.transaction_id,
      pre.transaction_commitment,
      control.compact_cbor,
      control.witness_set_compact_cbor,
      control.field_preimage_lengths_cbor,
      second_field_index,
      second_preimage_cbor,
    ),
  }
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
    3,
    control.source_count,
    control.source_peaks,
    control.redeemer_count,
    control.redeemer_peaks,
    0,
    initial_resolution_accumulator(),
    initial_schedule,
    0,
    0,
    validation_merkle_v1.empty_frontier(),
    0,
    0,
    validation_merkle_v1.empty_frontier(),
    [],
  )
  and {
    fields_verified,
    control.resolved_input_count == list.length(spend_inputs) + list.length(
      reference_inputs,
    ),
    control.replay_cursor == 0,
    control.resolved_item_peaks == [],
    control.replay_accumulator == initial_resolution_accumulator(),
    control.replay_remaining_schedule_hash == empty_resolution_schedule_hash(),
    control.spend_index == 0,
    control.purpose_count == 0,
    control.purpose_peaks == [],
    witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      pre.program_counter + 1,
      next_control,
    ),
  }
}

fn reference_script_bound_rejection(
  output: MidgardTxOutput,
) -> Option<ByteArray> {
  when output.script_ref is {
    None -> None
    Some(script) ->
      if script.language == NativeCardanoScript {
        when
          native_script_v1.inspect_native_script(
            script.script_bytes,
            -1,
            -1,
            [],
          )
        is {
          None -> Some(reject_invalid_field_type)
          Some(checked) ->
            if checked.depth > native_script_v1.max_native_script_depth {
              Some(reject_native_script_depth)
            } else if checked.node_count > native_script_v1.max_native_script_node_count {
              Some(reject_native_script_node_count)
            } else {
              None
            }
        }
      } else {
        None
      }
  }
}

fn replay_source_frontier(
  control: ScriptSourcesControlV1,
  source_kind: Int,
  key: ByteArray,
  output: MidgardTxOutput,
) -> validation_merkle_v1.BuiltFrontier {
  when output.script_ref is {
    Some(script) ->
      if source_kind == 1 {
        validation_merkle_v1.BuiltFrontier {
          count: control.source_count + 1,
          peaks: validation_merkle_v1.append_leaf(
            control.source_count,
            control.source_peaks,
            script_proof_v1.source_leaf_hash(1, key, script),
          ),
        }
      } else {
        validation_merkle_v1.BuiltFrontier {
          count: control.source_count,
          peaks: control.source_peaks,
        }
      }
    None ->
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
  output: MidgardTxOutput,
) -> validation_merkle_v1.BuiltFrontier {
  if source_kind == 0 {
    when output.address.payment_credential is {
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
  if and {
    resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == control.replay_remaining_schedule_hash,
    bytearray.length(value) <= 4095,
  } {
    when ledger_output_v1.decode_canonical_output(value) is {
      None -> False
      Some(output) ->
        when
          if source_kind == 1 {
            reference_script_bound_rejection(output)
          } else {
            None
          }
        is {
          Some(rejection_code) ->
            rejected_successor_is_exact(
              pre,
              witness.claimed_successor,
              rejection_code,
            )
          None -> {
            let next_sources = replay_source_frontier(
              control,
              source_kind,
              key,
              output,
            )
            let next_purposes = replay_purpose_frontier(
              control,
              source_kind,
              key,
              output,
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
              control.receive_hashes,
            )
            and {
              witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
              witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
                validation_trace_v1.ScriptSources,
                pre.program_counter + 1,
                next_control,
              ),
            }
          }
        }
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
      control.receive_hashes,
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

fn output_leaf_hashes(
  output_cbors: List<ByteArray>,
  output_index: Int,
) -> List<ByteArray> {
  when output_cbors is {
    [] -> []
    [output_cbor, ..rest] ->
      [
        script_proof_v1.output_leaf_hash(output_index, output_cbor),
        ..output_leaf_hashes(rest, output_index + 1)
      ]
  }
}

fn output_field_rejection(output_cbors: List<ByteArray>) -> Option<ByteArray> {
  when output_cbors is {
    [] -> None
    [output_cbor, ..rest] ->
      if bytearray.length(output_cbor) > 4095 {
        Some(reject_ledger_output_size)
      } else {
        when ledger_output_v1.decode_canonical_output(output_cbor) is {
          None -> Some(reject_invalid_output)
          Some(output) ->
            when reference_script_bound_rejection(output) is {
              Some(rejection_code) -> Some(rejection_code)
              None -> output_field_rejection(rest)
            }
        }
      }
  }
}

fn script_sources_stage_four(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  expect TransactionFieldPreimageWitness { preimage_cbor } = auxiliary
  let field_verified = verify_midgard_transaction_field_preimage_v1(
    pre.transaction_id,
    pre.transaction_commitment,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
    2,
    preimage_cbor,
  )
  let output_cbors = decode_midgard_tx_byte_list_preimage_cbor(preimage_cbor)
  let built_outputs = output_leaf_hashes(output_cbors, 0)
    |> validation_merkle_v1.build_frontier
  let rejection = if list.length(output_cbors) > 64 {
    Some(reject_output_count)
  } else {
    output_field_rejection(output_cbors)
  }
  and {
    field_verified,
    control.output_cursor == 0,
    control.output_count == 0,
    control.output_peaks == [],
    control.receive_hashes == [],
    when rejection is {
      Some(rejection_code) ->
        rejected_successor_is_exact(
          pre,
          witness.claimed_successor,
          rejection_code,
        )
      None -> {
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
          5,
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
          0,
          built_outputs.count,
          built_outputs.peaks,
          [],
        )
        and {
          witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
          witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
            validation_trace_v1.ScriptSources,
            pre.program_counter + 1,
            next_control,
          ),
        }
      }
    },
  }
}

fn receive_hashes_are_canonical(hashes: List<ByteArray>) -> Bool {
  when hashes is {
    [] -> True
    [hash, ..rest] -> and {
        bytearray.length(hash) == 28,
        when rest is {
          [] -> True
          [next, ..] -> bytearray.compare(hash, next) == Less
        },
        receive_hashes_are_canonical(rest),
      }
  }
}

fn insert_receive_hash(
  hashes: List<ByteArray>,
  script_hash: ByteArray,
) -> List<ByteArray> {
  [script_hash, ..hashes]
    |> list.sort(bytearray.compare)
    |> list.unique
}

fn protected_output_authorization(
  output: MidgardTxOutput,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
  signer_proof: SignerSetProofV1,
) -> InputSignerAuthorizationV1 {
  if output.address.protected {
    input_signer_authorization(
      0,
      output,
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

fn receive_purpose_frontier(
  hashes: List<ByteArray>,
  receive_index: Int,
  count: Int,
  peaks: List<validation_merkle_v1.FrontierPeak>,
) -> validation_merkle_v1.BuiltFrontier {
  when hashes is {
    [] -> validation_merkle_v1.BuiltFrontier { count, peaks }
    [script_hash, ..rest] ->
      receive_purpose_frontier(
        rest,
        receive_index + 1,
        count + 1,
        validation_merkle_v1.append_leaf(
          count,
          peaks,
          script_proof_v1.purpose_leaf_hash(
            3,
            receive_index,
            script_hash,
            script_hash,
          ),
        ),
      )
  }
}

fn script_sources_output_item(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  output_index: Int,
  output_cbor: ByteArray,
  siblings: List<ByteArray>,
  signer_proof: SignerSetProofV1,
) -> Bool {
  if and {
    output_index == control.output_cursor,
    bytearray.length(output_cbor) <= 4095,
    validation_merkle_v1.verify_membership(
      control.output_count,
      control.output_peaks,
      output_index,
      script_proof_v1.output_leaf_hash(output_index, output_cbor),
      siblings,
    ),
  } {
    when ledger_output_v1.decode_canonical_output(output_cbor) is {
      None -> False
      Some(output) ->
        when
          protected_output_authorization(
            output,
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
          InputSignerAuthorized -> {
            let next_receive_hashes = if output.address.protected {
              when output.address.payment_credential is {
                ScriptCredential(script_hash) ->
                  insert_receive_hash(control.receive_hashes, script_hash)
                PubKeyCredential(_) -> control.receive_hashes
              }
            } else {
              control.receive_hashes
            }
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
              5,
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
              control.output_cursor + 1,
              control.output_count,
              control.output_peaks,
              next_receive_hashes,
            )
            and {
              witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
              witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
                validation_trace_v1.ScriptSources,
                pre.program_counter + 1,
                next_control,
              ),
            }
          }
        }
    }
  } else {
    False
  }
}

fn script_sources_stage_five(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  if control.output_cursor == control.output_count {
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
      control.receive_hashes,
    )
    and {
      auxiliary == NoAuxiliaryWitness,
      witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
      witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
        validation_trace_v1.ScriptSources,
        pre.program_counter + 1,
        next_control,
      ),
    }
  } else {
    when auxiliary is {
      OutputReplayWitness { output_index, output_cbor, siblings, signer_proof } ->
        script_sources_output_item(
          pre,
          witness,
          control,
          output_index,
          output_cbor,
          siblings,
          signer_proof,
        )
      _ -> False
    }
  }
}

fn proof_data_is_map(data: Data) -> Bool {
  builtin.choose_data(data, False, True, False, False, False)
}

fn proof_data_is_int(data: Data) -> Bool {
  builtin.choose_data(data, False, False, False, True, False)
}

fn proof_data_is_bytes(data: Data) -> Bool {
  builtin.choose_data(data, False, False, False, False, True)
}

fn canonical_asset_name_precedes(left: ByteArray, right: ByteArray) -> Bool {
  let left_length = bytearray.length(left)
  let right_length = bytearray.length(right)
  if left_length < right_length {
    True
  } else if left_length > right_length {
    False
  } else {
    bytearray.compare(left, right) == Less
  }
}

fn mint_assets_are_canonical(
  assets: List<Pair<Data, Data>>,
  previous_name: Option<ByteArray>,
) -> Bool {
  when assets is {
    [] -> True
    [Pair(name_data, quantity_data), ..rest] ->
      if and {
        proof_data_is_bytes(name_data),
        proof_data_is_int(quantity_data),
      } {
        let name = builtin.un_b_data(name_data)
        let quantity = builtin.un_i_data(quantity_data)
        and {
          bytearray.length(name) <= 32,
          quantity != 0,
          when previous_name is {
            None -> True
            Some(previous) -> canonical_asset_name_precedes(previous, name)
          },
          mint_assets_are_canonical(rest, Some(name)),
        }
      } else {
        False
      }
  }
}

fn mint_policies_are_canonical(
  policies: List<Pair<Data, Data>>,
  previous_policy: Option<ByteArray>,
) -> Bool {
  when policies is {
    [] -> True
    [Pair(policy_data, assets_data), ..rest] ->
      if and {
        proof_data_is_bytes(policy_data),
        proof_data_is_map(assets_data),
      } {
        let policy_id = builtin.un_b_data(policy_data)
        let assets = builtin.un_map_data(assets_data)
        and {
          bytearray.length(policy_id) == 28,
          !list.is_empty(assets),
          when previous_policy is {
            None -> True
            Some(previous) -> bytearray.compare(previous, policy_id) == Less
          },
          mint_assets_are_canonical(assets, None),
          mint_policies_are_canonical(rest, Some(policy_id)),
        }
      } else {
        False
      }
  }
}

fn mint_is_canonical(mint: Data, preimage_cbor: ByteArray) -> Bool {
  let empty_mint: Data = []
  if mint == empty_mint {
    preimage_cbor == #"80"
  } else if proof_data_is_map(mint) {
    and {
      mint_policies_are_canonical(builtin.un_map_data(mint), None),
      encode_mint_preimage(mint) == preimage_cbor,
    }
  } else {
    False
  }
}

fn mint_purpose_frontier(
  policies: List<Pair<Data, Data>>,
  policy_index: Int,
  count: Int,
  peaks: List<validation_merkle_v1.FrontierPeak>,
) -> validation_merkle_v1.BuiltFrontier {
  when policies is {
    [] -> validation_merkle_v1.BuiltFrontier { count, peaks }
    [Pair(policy_data, _assets_data), ..rest] -> {
      let policy_id = builtin.un_b_data(policy_data)
      mint_purpose_frontier(
        rest,
        policy_index + 1,
        count + 1,
        validation_merkle_v1.append_leaf(
          count,
          peaks,
          script_proof_v1.purpose_leaf_hash(
            1,
            policy_index,
            policy_id,
            policy_id,
          ),
        ),
      )
    }
  }
}

fn script_sources_stage_six(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  expect TransactionFieldPreimageWitness { preimage_cbor } = auxiliary
  let field_verified = verify_midgard_transaction_field_preimage_v1(
    pre.transaction_id,
    pre.transaction_commitment,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
    5,
    preimage_cbor,
  )
  let mint = decode_midgard_tx_mint_preimage_cbor(preimage_cbor)
  let empty_mint: Data = []
  if and {
    field_verified,
    mint_is_canonical(mint, preimage_cbor),
  } {
    let next_purposes = if mint == empty_mint {
      validation_merkle_v1.BuiltFrontier {
        count: control.purpose_count,
        peaks: control.purpose_peaks,
      }
    } else {
      mint_purpose_frontier(
        builtin.un_map_data(mint),
        0,
        control.purpose_count,
        control.purpose_peaks,
      )
    }
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
      7,
      control.source_count,
      control.source_peaks,
      control.redeemer_count,
      control.redeemer_peaks,
      control.replay_cursor,
      control.replay_accumulator,
      control.replay_remaining_schedule_hash,
      control.spend_index,
      next_purposes.count,
      next_purposes.peaks,
      control.output_cursor,
      control.output_count,
      control.output_peaks,
      control.receive_hashes,
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

fn observer_purpose_frontier(
  observers: List<ByteArray>,
  observer_index: Int,
  count: Int,
  peaks: List<validation_merkle_v1.FrontierPeak>,
) -> validation_merkle_v1.BuiltFrontier {
  when observers is {
    [] -> validation_merkle_v1.BuiltFrontier { count, peaks }
    [script_hash, ..rest] ->
      observer_purpose_frontier(
        rest,
        observer_index + 1,
        count + 1,
        validation_merkle_v1.append_leaf(
          count,
          peaks,
          script_proof_v1.purpose_leaf_hash(
            2,
            observer_index,
            script_hash,
            script_hash,
          ),
        ),
      )
  }
}

fn observer_hashes_are_well_formed(observers: List<ByteArray>) -> Bool {
  when observers is {
    [] -> True
    [observer, ..rest] -> and {
        bytearray.length(observer) == 28,
        !list.has(rest, observer),
        observer_hashes_are_well_formed(rest),
      }
  }
}

fn script_sources_stage_seven(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ScriptSourcesControlV1,
) -> Bool {
  expect TransactionFieldPreimageWitness { preimage_cbor } = auxiliary
  let field_verified = verify_midgard_transaction_field_preimage_v1(
    pre.transaction_id,
    pre.transaction_commitment,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
    3,
    preimage_cbor,
  )
  let observers = decode_midgard_tx_byte_list_preimage_cbor(preimage_cbor)
  let canonical_observers = observers |> list.sort(bytearray.compare)
  if and {
    field_verified,
    list.length(observers) <= 16,
    observer_hashes_are_well_formed(observers),
  } {
    let next_purposes = observer_purpose_frontier(
      canonical_observers,
      0,
      control.purpose_count,
      control.purpose_peaks,
    )
    let final_purposes = receive_purpose_frontier(
      control.receive_hashes,
      0,
      next_purposes.count,
      next_purposes.peaks,
    )
    if final_purposes.count > 16 {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_script_execution_count,
      )
    } else {
      let discovery_base = ScriptSourcesControlV1 {
        ..control,
        stage: 8,
        purpose_count: final_purposes.count,
        purpose_peaks: final_purposes.peaks,
        receive_hashes: [],
      }
      let next_control = encode_script_sources_discovery_witness(
        discovery_base,
        8,
        empty_script_discovery_control(),
      )
      and {
        witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
        witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
          validation_trace_v1.ScriptSources,
          pre.program_counter + 1,
          next_control,
        ),
      }
    }
  } else {
    False
  }
}

fn script_discovery_successor_is_exact(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  control: ScriptSourcesControlV1,
  stage: Int,
  discovery: ScriptDiscoveryControlV1,
) -> Bool {
  let next_control = encode_script_sources_discovery_witness(
    control,
    stage,
    discovery,
  )
  and {
    witness.claimed_successor.phase == validation_trace_v1.ScriptSources,
    witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
      validation_trace_v1.ScriptSources,
      pre.program_counter + 1,
      next_control,
    ),
  }
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
          script,
          siblings,
        }
      = auxiliary
    let source_leaf = script_proof_v1.source_leaf_hash(
      origin_kind,
      source_key,
      script,
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
      if script_proof_v1.versioned_script_hash(script) == discovery.current_script_hash {
        let language_tag = script_proof_v1.language_tag(script.language)
        let next_used_inline = if origin_kind == 0 {
          script_discovery_bitmap_insert(
            discovery.used_inline_bitmap,
            source_index,
          )
        } else {
          discovery.used_inline_bitmap
        }
        if language_tag == 0 {
          let next_execution = append_script_execution(
            discovery,
            current_script_purpose_leaf(discovery),
            source_leaf,
            language_tag,
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
              matched_language_tag: language_tag,
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

fn redeemer_matches_current_purpose(
  redeemer: MidgardRedeemerWitness,
  discovery: ScriptDiscoveryControlV1,
) -> Bool {
  and {
    redeemer.index == discovery.current_purpose_index,
    if discovery.current_purpose_kind == 0 {
      redeemer.purpose == SpendRedeemer
    } else if discovery.current_purpose_kind == 1 {
      redeemer.purpose == MintRedeemer
    } else if discovery.current_purpose_kind == 2 {
      redeemer.purpose == RewardRedeemer
    } else if discovery.current_purpose_kind == 3 {
      redeemer.purpose == ReceiveRedeemer
    } else {
      False
    },
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
      auxiliary == NoAuxiliaryWitness,
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_missing_required_witness,
      ),
    }
  } else {
    expect RedeemerScanWitness { redeemer_index, redeemer, siblings } = auxiliary
    let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(redeemer)
    if and {
      redeemer_index == discovery.redeemer_cursor,
      validation_merkle_v1.verify_membership(
        control.redeemer_count,
        control.redeemer_peaks,
        redeemer_index,
        redeemer_leaf,
        siblings,
      ),
    } {
      if redeemer_matches_current_purpose(redeemer, discovery) {
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
              redeemer_index,
            ),
            execution_count: next_execution.count,
            execution_peaks: next_execution.peaks,
          },
        )
        script_discovery_successor_is_exact(pre, witness, control, 8, completed)
      } else {
        script_discovery_successor_is_exact(
          pre,
          witness,
          control,
          10,
          ScriptDiscoveryControlV1 {
            ..discovery,
            redeemer_cursor: redeemer_index + 1,
          },
        )
      }
    } else {
      False
    }
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
  encode_definite_array_header(22)
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
    |> bytearray.concat(cbor.serialise(control.discovery.execution_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.discovery.execution_peaks),
      )
    |> bytearray.concat(cbor.serialise(execution_cursor))
    |> bytearray.concat(cbor.serialise(language_bitmap))
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
          script,
          siblings,
        }
      = auxiliary
    let source_leaf = script_proof_v1.source_leaf_hash(
      origin_kind,
      source_key,
      script,
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
      auxiliary == NoAuxiliaryWitness,
      witness.claimed_successor.phase == validation_trace_v1.NativeScripts,
      witness.claimed_successor.work_root == validation_trace_v1.hash_work_witness(
        validation_trace_v1.NativeScripts,
        pre.program_counter + 1,
        next_control,
      ),
    }
  } else {
    expect RedeemerScanWitness { redeemer_index, redeemer, siblings } = auxiliary
    let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(redeemer)
    if and {
      redeemer_index == discovery.redeemer_cursor,
      validation_merkle_v1.verify_membership(
        control.redeemer_count,
        control.redeemer_peaks,
        redeemer_index,
        redeemer_leaf,
        siblings,
      ),
    } {
      if !script_discovery_bitmap_has(
        discovery.used_redeemer_bitmap,
        redeemer_index,
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
            redeemer_cursor: redeemer_index + 1,
          },
        )
      }
    } else {
      False
    }
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
      control.purpose_count <= 16,
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
    validation_merkle_v1.frontier_is_well_formed(
      control.redeemer_count,
      control.redeemer_peaks,
    ),
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
    receive_hashes_are_canonical(control.receive_hashes),
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
    control.receive_hashes,
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
    control.receive_hashes,
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

pub fn verify_script_sources_stage_five_replay_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  output_index: Int,
  output_cbor: ByteArray,
  siblings: List<ByteArray>,
  signer_proof: SignerSetProofV1,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  and {
    script_sources_control_is_bound(pre, witness, control),
    control.stage == 5,
    control.output_cursor < control.output_count,
    script_sources_output_item(
      pre,
      witness,
      control,
      output_index,
      output_cbor,
      siblings,
      signer_proof,
    ),
  }
}

pub fn verify_prepared_script_sources_stage_five_replay_transition_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  output_index: Int,
  output_cbor: ByteArray,
  siblings: List<ByteArray>,
  signer_proof: SignerSetProofV1,
) -> Bool {
  let control = script_sources_control_from_witness(witness.work_witness_cbor)
  and {
    control.stage == 5,
    control.output_cursor < control.output_count,
    script_sources_output_item(
      pre,
      witness,
      control,
      output_index,
      output_cbor,
      siblings,
      signer_proof,
    ),
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
  execution_count: Int,
  execution_peaks: List<validation_merkle_v1.FrontierPeak>,
  execution_cursor: Int,
  language_bitmap: Int,
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
        execution_count_data, execution_peaks_data, execution_cursor_data,
        language_bitmap_data,
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
    execution_count: builtin.un_i_data(execution_count_data),
    execution_peaks: decode_frontier_peaks(execution_peaks_data),
    execution_cursor: builtin.un_i_data(execution_cursor_data),
    language_bitmap: builtin.un_i_data(language_bitmap_data),
  }
}

pub fn encode_native_scripts_control_v1(
  control: NativeScriptsControlV1,
) -> ByteArray {
  encode_definite_array_header(22)
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
    |> bytearray.concat(cbor.serialise(control.execution_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.execution_peaks),
      )
    |> bytearray.concat(cbor.serialise(control.execution_cursor))
    |> bytearray.concat(cbor.serialise(control.language_bitmap))
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

pub fn encode_script_integrity_witness_v1(
  control: NativeScriptsControlV1,
) -> ByteArray {
  #"82"
    |> bytearray.concat(
        encode_definite_bytes(encode_native_scripts_control_v1(control)),
      )
    |> bytearray.concat(cbor.serialise(0))
}

fn native_signer_hashes_are_canonical(signer_hashes: List<ByteArray>) -> Bool {
  and {
    list.all(signer_hashes, fn(hash) { bytearray.length(hash) == 28 }),
    signer_hashes == list.sort(signer_hashes, bytearray.compare),
    signer_hashes == list.unique(signer_hashes),
  }
}

fn native_signer_frontier_matches(
  signer_hashes: List<ByteArray>,
  signer_count: Int,
  signer_frontier_commitment: ByteArray,
) -> Bool {
  let frontier = signer_hashes
    |> list.map(script_proof_v1.signer_leaf_hash)
    |> validation_merkle_v1.build_frontier
  and {
    native_signer_hashes_are_canonical(signer_hashes),
    frontier.count == signer_count,
    validation_merkle_v1.frontier_commitment(frontier.count, frontier.peaks) == signer_frontier_commitment,
  }
}

fn native_language_tag_matches_script(
  language_tag: Int,
  script: MidgardScriptWitness,
) -> Bool {
  language_tag == script_proof_v1.language_tag(script.language)
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

fn verify_native_execution_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: NativeScriptsControlV1,
  body: NativeTxBodyCompact,
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
  if and {
    execution_index == control.execution_cursor,
    control.execution_count == control.purpose_count,
    script_proof_v1.versioned_script_hash(script) == script_hash,
    native_language_tag_matches_script(language_tag, script),
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
      if and {
        redeemer_leaf == #"",
        native_signer_frontier_matches(
          signer_hashes,
          control.signer_count,
          control.signer_frontier_commitment,
        ),
      } {
        when
          native_script_v1.check_native_script(
            script.script_bytes,
            body.validity_interval_start,
            body.validity_interval_end,
            signer_hashes,
          )
        is {
          None -> False
          Some(checked) ->
            if checked.valid {
              native_scripts_successor_is_exact(
                pre,
                witness,
                control,
                control.language_bitmap,
              )
            } else {
              rejected_successor_is_exact(
                pre,
                witness.claimed_successor,
                reject_native_script_invalid,
              )
            }
        }
      } else {
        False
      }
    } else if and {
      or {
        language_tag == 3,
        language_tag == 128,
      },
      bytearray.length(redeemer_leaf) == 32,
      signer_hashes == [],
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
    control.resolved_input_count >= 0,
    bytearray.length(control.resolved_inputs_accumulator) == 32,
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
      control.execution_count,
      control.execution_peaks,
    ),
    control.execution_count == control.purpose_count,
    control.execution_cursor >= 0,
    control.execution_cursor <= control.execution_count,
    control.language_bitmap >= 0,
    control.language_bitmap <= 3,
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
      verify_native_execution_step(
        pre,
        witness,
        auxiliary,
        control,
        verified_source.tx_compact.body,
      )
    },
  }
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
  observer_summary: cek_data_v1.DataSummaryV1,
  mint_preimage_hash: ByteArray,
  mint_preimage_length: Int,
  mint_offset: Int,
  mint_policy_remaining: Int,
  mint_asset_remaining: Int,
  previous_mint_asset: ByteArray,
  mint_count: Int,
  mint_peaks: List<validation_merkle_v1.FrontierPeak>,
  mint_cursor: Int,
  current_mint_policy: ByteArray,
  current_mint_assets: cek_data_v1.DataSequenceSummaryV1,
  mint_policies: cek_data_v1.DataSequenceSummaryV1,
  mint_summary: cek_data_v1.DataSummaryV1,
}

pub fn encode_cek_context_control_v1(control: CekContextControlV1) -> ByteArray {
  encode_definite_array_header(29)
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
    |> bytearray.concat(encode_data_summary_v1(control.observer_summary))
    |> bytearray.concat(encode_definite_bytes(control.mint_preimage_hash))
    |> bytearray.concat(cbor.serialise(control.mint_preimage_length))
    |> bytearray.concat(cbor.serialise(control.mint_offset))
    |> bytearray.concat(cbor.serialise(control.mint_policy_remaining))
    |> bytearray.concat(cbor.serialise(control.mint_asset_remaining))
    |> bytearray.concat(encode_definite_bytes(control.previous_mint_asset))
    |> bytearray.concat(cbor.serialise(control.mint_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.mint_peaks),
      )
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
        output_items_data, signer_items_data, observer_summary_data,
        mint_preimage_hash_data, mint_preimage_length_data, mint_offset_data,
        mint_policy_remaining_data, mint_asset_remaining_data,
        previous_mint_asset_data, mint_count_data, mint_peaks_data,
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
    observer_summary: data_summary_from_data(observer_summary_data),
    mint_preimage_hash: builtin.un_b_data(mint_preimage_hash_data),
    mint_preimage_length: builtin.un_i_data(mint_preimage_length_data),
    mint_offset: builtin.un_i_data(mint_offset_data),
    mint_policy_remaining: builtin.un_i_data(mint_policy_remaining_data),
    mint_asset_remaining: builtin.un_i_data(mint_asset_remaining_data),
    previous_mint_asset: builtin.un_b_data(previous_mint_asset_data),
    mint_count: builtin.un_i_data(mint_count_data),
    mint_peaks: decode_frontier_peaks(mint_peaks_data),
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
        bytearray.length(control.active_purpose.root) == 32,
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
    control.mint_preimage_hash == #"",
    control.mint_preimage_length == 0,
    control.mint_offset == 0,
    control.mint_policy_remaining == 0,
    control.mint_asset_remaining == 0,
    control.previous_mint_asset == #"",
    control.mint_count == 0,
    control.mint_peaks == [],
    control.mint_cursor == 0,
    control.current_mint_policy == #"",
    control.current_mint_assets == cek_data_v1.empty_data_pair_summary_v1(),
    control.mint_policies == cek_data_v1.empty_data_pair_summary_v1(),
    control.mint_summary == empty_data_summary_v1(),
  }
}

fn redeemer_data_scan_is_pristine(control: CekContextControlV1) -> Bool {
  control.redeemer_context_control_hash == #""
}

fn redeemer_data_scan_is_initial(control: CekContextControlV1) -> Bool {
  bytearray.length(control.redeemer_context_control_hash) == 32
}

fn redeemer_data_scan_is_complete(control: CekContextControlV1) -> Bool {
  bytearray.length(control.redeemer_context_control_hash) == 32
}

fn mint_context_is_final(control: CekContextControlV1) -> Bool {
  and {
    bytearray.length(control.observer_summary.root) == 32,
    bytearray.length(control.mint_preimage_hash) == 32,
    control.mint_offset == control.mint_preimage_length,
    control.mint_policy_remaining == 0,
    control.mint_asset_remaining == 0,
    control.previous_mint_asset == #"",
    control.mint_cursor == control.mint_count,
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
    optional_data_summary_is_well_formed(control.observer_summary),
    or {
      control.mint_preimage_hash == #"",
      bytearray.length(control.mint_preimage_hash) == 32,
    },
    control.mint_preimage_length >= 0,
    control.mint_preimage_length <= 9215,
    control.mint_offset >= 0,
    control.mint_offset <= control.mint_preimage_length,
    control.mint_policy_remaining >= 0,
    control.mint_policy_remaining <= ledger_output_v1.max_distinct_asset_count,
    control.mint_asset_remaining >= 0,
    control.mint_asset_remaining <= ledger_output_v1.max_distinct_asset_count,
    bytearray.length(control.previous_mint_asset) <= 32,
    validation_merkle_v1.frontier_is_well_formed(
      control.mint_count,
      control.mint_peaks,
    ),
    control.mint_count <= ledger_output_v1.max_distinct_asset_count,
    control.mint_cursor >= 0,
    control.mint_cursor <= control.mint_count,
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
        control.observer_summary.root == #"",
        redeemer_data_scan_is_pristine(control),
        mint_context_is_pristine(control),
      }
    } else if control.stage == 1 {
      and {
        redeemer_data_scan_is_initial(control),
        control.spend_items.length == 0,
        control.output_items.length == 0,
        control.signer_items.length == 0,
        mint_context_is_pristine(control),
      }
    } else if control.stage == 2 {
      and {
        redeemer_data_scan_is_initial(control),
        control.reference_items.length == reference_count,
        control.output_items.length == 0,
        control.signer_items.length == 0,
        mint_context_is_pristine(control),
      }
    } else if control.stage == 3 {
      and {
        redeemer_data_scan_is_initial(control),
        control.reference_items.length == reference_count,
        control.spend_items.length == native_control.spend_input_count,
        control.signer_items.length == 0,
        mint_context_is_pristine(control),
      }
    } else if control.stage == 4 {
      and {
        redeemer_data_scan_is_initial(control),
        control.reference_items.length == reference_count,
        control.spend_items.length == native_control.spend_input_count,
        control.output_items.length == native_control.output_count,
        control.signer_items.length <= native_control.signer_count,
        control.observer_summary.root == #"",
        mint_context_is_pristine(control),
      }
    } else if control.stage == 5 {
      and {
        redeemer_data_scan_is_initial(control),
        control.signer_items.length == native_control.signer_count,
        control.observer_summary.root == #"",
        mint_context_is_pristine(control),
      }
    } else if control.stage == 6 {
      and {
        redeemer_data_scan_is_initial(control),
        control.signer_items.length == native_control.signer_count,
        bytearray.length(control.observer_summary.root) == 32,
        mint_context_is_pristine(control),
      }
    } else if control.stage == 7 {
      and {
        redeemer_data_scan_is_initial(control),
        control.signer_items.length == native_control.signer_count,
        bytearray.length(control.observer_summary.root) == 32,
        bytearray.length(control.mint_preimage_hash) == 32,
        control.mint_preimage_length > 1,
        control.mint_offset > 0,
        control.mint_offset < control.mint_preimage_length,
        control.mint_policy_remaining + control.mint_asset_remaining > 0,
        control.mint_cursor == 0,
        if control.mint_count == 0 {
          control.current_mint_policy == #""
        } else {
          bytearray.length(control.current_mint_policy) == 28
        },
        if control.mint_asset_remaining > 0 {
          bytearray.length(control.current_mint_policy) == 28
        } else {
          True
        },
        control.current_mint_assets.length == 0,
        control.mint_policies.length == 0,
        control.mint_summary.root == #"",
      }
    } else if control.stage == 8 {
      and {
        redeemer_data_scan_is_initial(control),
        control.signer_items.length == native_control.signer_count,
        bytearray.length(control.observer_summary.root) == 32,
        bytearray.length(control.mint_preimage_hash) == 32,
        control.mint_offset == control.mint_preimage_length,
        control.mint_policy_remaining == 0,
        control.mint_asset_remaining == 0,
        control.previous_mint_asset == #"",
        control.mint_count > 0,
        control.mint_preimage_length > 1,
        control.mint_summary.root == #"",
      }
    } else if control.stage == 9 {
      and {
        control.signer_items.length == native_control.signer_count,
        mint_context_is_final(control),
        redeemer_data_scan_is_initial(control),
      }
    } else {
      and {
        control.signer_items.length == native_control.signer_count,
        mint_context_is_final(control),
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
  mint_count: Int,
  mint_peaks: List<validation_merkle_v1.FrontierPeak>,
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

fn mint_asset_leaf_hashes(
  policy_id: ByteArray,
  assets: List<Pair<Data, Data>>,
) -> List<ByteArray> {
  when assets is {
    [] -> []
    [Pair(asset_name_data, quantity_data), ..rest] -> {
      let asset_name = builtin.un_b_data(asset_name_data)
      let quantity = builtin.un_i_data(quantity_data)
      [
        mint_asset_leaf_hash(policy_id, asset_name, quantity),
        ..mint_asset_leaf_hashes(policy_id, rest)
      ]
    }
  }
}

fn append_byte_arrays(
  left: List<ByteArray>,
  right: List<ByteArray>,
) -> List<ByteArray> {
  when left is {
    [] -> right
    [item, ..rest] -> [item, ..append_byte_arrays(rest, right)]
  }
}

fn mint_policy_leaf_hashes(policies: List<Pair<Data, Data>>) -> List<ByteArray> {
  when policies is {
    [] -> []
    [Pair(policy_id_data, assets_data), ..rest] ->
      append_byte_arrays(
        mint_asset_leaf_hashes(
          builtin.un_b_data(policy_id_data),
          builtin.un_map_data(assets_data),
        ),
        mint_policy_leaf_hashes(rest),
      )
  }
}

fn mint_leaf_hashes(mint: Data) -> List<ByteArray> {
  let empty_mint: Data = []
  if mint == empty_mint {
    []
  } else {
    mint_policy_leaf_hashes(builtin.un_map_data(mint))
  }
}

pub fn encode_value_and_mint_control_v1(
  control: ValueAndMintControlV1,
) -> ByteArray {
  encode_definite_array_header(14)
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
    |> bytearray.concat(cbor.serialise(control.mint_count))
    |> bytearray.concat(
        validation_merkle_v1.encode_frontier(control.mint_peaks),
      )
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
        output_cursor_data, output_asset_cursor_data, mint_count_data,
        mint_peaks_data, mint_cursor_data, value_accumulator_cbor_data,
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
    mint_count: builtin.un_i_data(mint_count_data),
    mint_peaks: decode_frontier_peaks(mint_peaks_data),
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
      mint_count: 0,
      mint_peaks: [],
      mint_cursor: 0,
      value_accumulator: initial_value_accumulator(),
    },
  )
}

pub type LedgerDeltaControlV1 {
  native_control: NativeScriptsControlV1,
  stage: Int,
  replay_schedule_hash: ByteArray,
  replay_cursor: Int,
  replay_accumulator: ByteArray,
  replay_remaining_schedule_hash: ByteArray,
  current_ledger_root: ByteArray,
  output_cursor: Int,
  operation_count: Int,
  operation_peaks: List<validation_merkle_v1.FrontierPeak>,
}

const ledger_delta_operation_domain =
  #"4d69646761726456616c69646174696f6e4c656467657244656c74614f7065726174696f6e5631"

pub fn ledger_delta_operation_leaf_hash(
  operation_kind: Int,
  key: ByteArray,
  value: ByteArray,
) -> ByteArray {
  expect operation_kind == 0 || operation_kind == 1
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
    |> blake2b_256
}

pub fn encode_ledger_delta_control_v1(
  control: LedgerDeltaControlV1,
) -> ByteArray {
  encode_definite_array_header(10)
    |> bytearray.concat(
        encode_definite_bytes(
          encode_native_scripts_control_v1(control.native_control),
        ),
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
        native_control_cbor_data, stage_data, replay_schedule_hash_data,
        replay_cursor_data, replay_accumulator_data,
        replay_remaining_schedule_hash_data, current_ledger_root_data,
        output_cursor_data, operation_count_data, operation_peaks_data,
      ]
    = builtin.un_list_data(data)
  LedgerDeltaControlV1 {
    native_control: native_scripts_control_from_witness(
      builtin.un_b_data(native_control_cbor_data),
    ),
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
  }
}

pub fn encode_ledger_delta_witness_v1(
  native_control: NativeScriptsControlV1,
  prior_ledger_root: ByteArray,
) -> ByteArray {
  expect bytearray.length(prior_ledger_root) == 32
  encode_ledger_delta_control_v1(
    LedgerDeltaControlV1 {
      native_control,
      stage: 0,
      replay_schedule_hash: empty_resolution_schedule_hash(),
      replay_cursor: 0,
      replay_accumulator: initial_resolution_accumulator(),
      replay_remaining_schedule_hash: empty_resolution_schedule_hash(),
      current_ledger_root: prior_ledger_root,
      output_cursor: 0,
      operation_count: 0,
      operation_peaks: [],
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

fn verify_script_integrity(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
) -> Bool {
  expect Some(data) = cbor.deserialise(witness.work_witness_cbor)
  expect [native_control_cbor_data, stage_data] = builtin.un_list_data(data)
  let native_control_cbor = builtin.un_b_data(native_control_cbor_data)
  let stage = builtin.un_i_data(stage_data)
  let control = native_scripts_control_from_witness(native_control_cbor)
  let Pair(verified_source, verified_witness_set) = verify_native_tx_proof_source_v1(
    pre.transaction_id,
    control.compact_cbor,
    control.witness_set_compact_cbor,
    control.field_preimage_lengths_cbor,
  )
  let expected_hash = script_language_views_v1.expected_script_integrity_hash(
    verified_witness_set.redeemer_tx_wits_hash,
    control.language_bitmap,
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
    if verified_source.tx_compact.body.script_integrity_hash != expected_hash {
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
            observer_summary: empty_data_summary_v1(),
            mint_preimage_hash: #"",
            mint_preimage_length: 0,
            mint_offset: 0,
            mint_policy_remaining: 0,
            mint_asset_remaining: 0,
            previous_mint_asset: #"",
            mint_count: 0,
            mint_peaks: validation_merkle_v1.empty_frontier(),
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

fn redeemer_matches_cek_context(
  redeemer: MidgardRedeemerWitness,
  control: CekContextControlV1,
) -> Bool {
  and {
    redeemer.index == control.purpose_index,
    if control.purpose_kind == 0 {
      redeemer.purpose == SpendRedeemer
    } else if control.purpose_kind == 1 {
      redeemer.purpose == MintRedeemer
    } else if control.purpose_kind == 2 {
      redeemer.purpose == RewardRedeemer
    } else if control.purpose_kind == 3 {
      redeemer.purpose == ReceiveRedeemer
    } else {
      False
    },
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
  expect RedeemerScanWitness { redeemer_index, redeemer, siblings } = auxiliary
  let leaf = script_proof_v1.redeemer_leaf_hash(redeemer)
  let selected = CekContextControlV1 {
    ..context_control,
    stage: 1,
    execution_memory_limit: redeemer.execution_units.memory,
    execution_cpu_limit: redeemer.execution_units.steps,
  }
  let redeemer_context = initial_cek_redeemer_context_control_v1()
  let next = CekContextControlV1 {
    ..selected,
    redeemer_context_control_hash: hash_cek_redeemer_context_control_v1(
      redeemer_context,
    ),
  }
  and {
    context_control.stage == 0,
    leaf == context_control.redeemer_leaf,
    redeemer_matches_cek_context(redeemer, context_control),
    validation_merkle_v1.verify_membership(
      native_control.redeemer_count,
      native_control.redeemer_peaks,
      redeemer_index,
      leaf,
      siblings,
    ),
    cek_context_successor_is_exact(
      pre,
      witness,
      native_control,
      next,
      execution_cursor,
      completed_cpu,
      completed_memory,
    ),
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

fn verify_cek_observer_context_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  expect TransactionFieldPreimageWitness { preimage_cbor } = auxiliary
  let field_verified = verify_midgard_transaction_field_preimage_v1(
    pre.transaction_id,
    pre.transaction_commitment,
    native_control.compact_cbor,
    native_control.witness_set_compact_cbor,
    native_control.field_preimage_lengths_cbor,
    3,
    preimage_cbor,
  )
  let observers = decode_midgard_tx_byte_list_preimage_cbor(preimage_cbor)
  when
    script_context_v1.observer_collection_summary_v1(
      observers,
      context_control.language_tag == 128,
    )
  is {
    None -> False
    Some(summary) -> and {
        field_verified,
        cek_context_successor_is_exact(
          pre,
          witness,
          native_control,
          CekContextControlV1 {
            ..context_control,
            stage: 6,
            observer_summary: summary,
          },
          execution_cursor,
          completed_cpu,
          completed_memory,
        ),
      }
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
  expect TransactionFieldPreimageWitness { preimage_cbor } = auxiliary
  let field_verified = verify_midgard_transaction_mint_preimage_commitment_v1(
    pre.transaction_id,
    pre.transaction_commitment,
    native_control.compact_cbor,
    native_control.witness_set_compact_cbor,
    native_control.field_preimage_lengths_cbor,
    preimage_cbor,
  )
  let preimage_length = bytearray.length(preimage_cbor)
  if preimage_cbor == #"80" {
    and {
      field_verified,
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 {
          ..context_control,
          stage: 9,
          mint_preimage_hash: blake2b_256(preimage_cbor),
          mint_preimage_length: preimage_length,
          mint_offset: preimage_length,
          mint_summary: cek_data_v1.map_data_summary_v1(
            cek_data_v1.empty_data_pair_summary_v1(),
          ),
        },
        execution_cursor,
        completed_cpu,
        completed_memory,
      ),
    }
  } else {
    let Pair(offset, policy_count) = decode_canonical_map_header_at(
      preimage_cbor,
      0,
    )
    and {
      field_verified,
      policy_count > 0,
      policy_count <= ledger_output_v1.max_distinct_asset_count,
      offset < preimage_length,
      cek_context_successor_is_exact(
        pre,
        witness,
        native_control,
        CekContextControlV1 {
          ..context_control,
          stage: 7,
          mint_preimage_hash: blake2b_256(preimage_cbor),
          mint_preimage_length: preimage_length,
          mint_offset: offset,
          mint_policy_remaining: policy_count,
        },
        execution_cursor,
        completed_cpu,
        completed_memory,
      ),
    }
  }
}

type MintScanDecodedV1 {
  policy_id: ByteArray,
  asset_name: ByteArray,
  quantity: Int,
  next_offset: Int,
  next_policy_remaining: Int,
  next_asset_remaining: Int,
}

fn mint_scan_successor(
  preimage_cbor: ByteArray,
  control: CekContextControlV1,
) -> CekContextControlV1 {
  expect bytearray.length(preimage_cbor) == control.mint_preimage_length
  expect blake2b_256(preimage_cbor) == control.mint_preimage_hash
  let
      MintScanDecodedV1 {
        policy_id,
        asset_name,
        quantity,
        next_offset,
        next_policy_remaining,
        next_asset_remaining,
      }
    = if control.mint_asset_remaining == 0 {
    expect control.mint_policy_remaining > 0
    let Pair(policy_offset, policy_id) = decode_canonical_bytes_at(
      preimage_cbor,
      control.mint_offset,
    )
    expect bytearray.length(policy_id) == 28
    expect or {
        control.current_mint_policy == #"",
        canonical_bytes_key_precedes(control.current_mint_policy, policy_id),
      }
    let Pair(asset_offset, asset_count) = decode_canonical_map_header_at(
      preimage_cbor,
      policy_offset,
    )
    expect asset_count > 0
    let Pair(quantity_offset, asset_name) = decode_canonical_bytes_at(
      preimage_cbor,
      asset_offset,
    )
    expect bytearray.length(asset_name) <= 32
    let Pair(next_offset, quantity) = decode_canonical_int_at(
      preimage_cbor,
      quantity_offset,
    )
    expect quantity != 0
    MintScanDecodedV1 {
      policy_id,
      asset_name,
      quantity,
      next_offset,
      next_policy_remaining: control.mint_policy_remaining - 1,
      next_asset_remaining: asset_count - 1,
    }
  } else {
    expect bytearray.length(control.current_mint_policy) == 28
    let Pair(quantity_offset, asset_name) = decode_canonical_bytes_at(
      preimage_cbor,
      control.mint_offset,
    )
    expect bytearray.length(asset_name) <= 32
    expect canonical_bytes_key_precedes(control.previous_mint_asset, asset_name)
    let Pair(next_offset, quantity) = decode_canonical_int_at(
      preimage_cbor,
      quantity_offset,
    )
    expect quantity != 0
    MintScanDecodedV1 {
      policy_id: control.current_mint_policy,
      asset_name,
      quantity,
      next_offset,
      next_policy_remaining: control.mint_policy_remaining,
      next_asset_remaining: control.mint_asset_remaining - 1,
    }
  }
  let next_count = control.mint_count + 1
  expect next_count <= ledger_output_v1.max_distinct_asset_count
  let next_peaks = validation_merkle_v1.append_leaf(
    control.mint_count,
    control.mint_peaks,
    mint_asset_leaf_hash(policy_id, asset_name, quantity),
  )
  let finished = and {
    next_policy_remaining == 0,
    next_asset_remaining == 0,
  }
  expect
    if finished {
      next_offset == control.mint_preimage_length
    } else {
      next_offset < control.mint_preimage_length
    }
  CekContextControlV1 {
    ..control,
    stage: if finished {
      8
    } else {
      7
    },
    mint_offset: next_offset,
    mint_policy_remaining: next_policy_remaining,
    mint_asset_remaining: next_asset_remaining,
    previous_mint_asset: if finished {
      #""
    } else {
      asset_name
    },
    mint_count: next_count,
    mint_peaks: next_peaks,
    mint_cursor: 0,
    current_mint_policy: if finished {
      #""
    } else {
      policy_id
    },
  }
}

fn verify_cek_mint_preimage_scan_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  native_control: NativeScriptsControlV1,
  context_control: CekContextControlV1,
  execution_cursor: Int,
  completed_cpu: Int,
  completed_memory: Int,
) -> Bool {
  expect TransactionFieldPreimageWitness { preimage_cbor } = auxiliary
  cek_context_successor_is_exact(
    pre,
    witness,
    native_control,
    mint_scan_successor(preimage_cbor, context_control),
    execution_cursor,
    completed_cpu,
    completed_memory,
  )
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
  if context_control.mint_cursor == context_control.mint_count {
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
    let expected_index = context_control.mint_count - context_control.mint_cursor - 1
    let membership_verified = and {
      mint_index == expected_index,
      validation_merkle_v1.verify_membership(
        context_control.mint_count,
        context_control.mint_peaks,
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

fn cek_data_scan_successor(
  scan: cek_data_scan_v1.DataScanControlV1,
  step: CekDataScanStepV1,
) -> Option<cek_data_scan_v1.DataScanControlV1> {
  when step is {
    CekDataOpenConstructor { raw_cbor, parent, constructor, expected_children } ->
      cek_data_scan_v1.open_constructor_control_step_v1(
        scan,
        raw_cbor,
        parent,
        constructor,
        expected_children,
      )
    CekDataOpenList { raw_cbor, parent, expected_children } ->
      cek_data_scan_v1.open_list_control_step_v1(
        scan,
        raw_cbor,
        parent,
        expected_children,
      )
    CekDataOpenMap { raw_cbor, parent } ->
      cek_data_scan_v1.open_map_control_step_v1(scan, raw_cbor, parent)
    CekDataRevealLeaf { raw_cbor, parent, item_length } ->
      cek_data_scan_v1.reveal_leaf_control_step_v1(
        scan,
        raw_cbor,
        parent,
        item_length,
      )
    CekDataCloseSequence { raw_cbor, frame } ->
      cek_data_scan_v1.close_sequence_control_step_v1(scan, raw_cbor, frame)
    CekDataFoldList { frame, child_index, child, siblings } ->
      cek_data_scan_v1.fold_list_control_step_v1(
        scan,
        frame,
        child_index,
        child,
        siblings,
      )
    CekDataFoldMap {
      frame,
      pair_index,
      key,
      value,
      key_siblings,
      value_siblings,
    } ->
      cek_data_scan_v1.fold_map_control_step_v1(
        scan,
        frame,
        pair_index,
        key,
        value,
        key_siblings,
        value_siblings,
      )
    CekDataFinalizeFrame { frame, parent } ->
      cek_data_scan_v1.finalize_frame_control_step_v1(scan, frame, parent)
  }
}

fn redeemer_matches_purpose_fields(
  redeemer: MidgardRedeemerWitness,
  purpose_kind: Int,
  purpose_index: Int,
) -> Bool {
  and {
    redeemer.index == purpose_index,
    if purpose_kind == 0 {
      redeemer.purpose == SpendRedeemer
    } else if purpose_kind == 1 {
      redeemer.purpose == MintRedeemer
    } else if purpose_kind == 2 {
      redeemer.purpose == RewardRedeemer
    } else if purpose_kind == 3 {
      redeemer.purpose == ReceiveRedeemer
    } else {
      False
    },
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
      redeemer_index,
      redeemer,
      redeemer_siblings,
      purpose_frontier_index,
      purpose_kind,
      purpose_index,
      script_hash,
      subject,
      purpose_siblings,
    } -> {
      let redeemer_leaf = script_proof_v1.redeemer_leaf_hash(redeemer)
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
        redeemer_index == native_control.redeemer_count - control.cursor - 1,
        validation_merkle_v1.verify_membership(
          native_control.redeemer_count,
          native_control.redeemer_peaks,
          redeemer_index,
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
        redeemer_matches_purpose_fields(redeemer, purpose_kind, purpose_index),
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
            cek_redeemer_context_main_successor(
              pre,
              witness,
              native_control,
              context_control,
              CekRedeemerContextControlV1 {
                ..control,
                cursor: control.cursor + 1,
              },
              execution_cursor,
              completed_cpu,
              completed_memory,
            )
          } else {
            False
          }
        Some(purpose) -> {
          let scan = cek_data_scan_v1.initial_control_v1(
            blake2b_256(redeemer.redeemer_cbor),
            bytearray.length(redeemer.redeemer_cbor),
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
                active_scan_hash: cek_data_scan_v1.hash_control_v1(scan),
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
    CekDataScanStepWitness { redeemer_control, control: scan, step } -> {
      let current_matches = and {
        cek_redeemer_context_control_is_well_formed(
          native_control.redeemer_count,
          redeemer_control,
        ),
        hash_cek_redeemer_context_control_v1(redeemer_control) == context_control.redeemer_context_control_hash,
        redeemer_control.active_scan_hash == cek_data_scan_v1.hash_control_v1(
          scan,
        ),
      }
      when cek_data_scan_successor(scan, step) is {
        None -> False
        Some(next_scan) -> {
          let scan_completed = bytearray.length(next_scan.result.root) == 32
          let next_redeemers = if scan_completed {
            let next_current = if redeemer_control.active_redeemer_leaf == context_control.redeemer_leaf {
              next_scan.result
            } else {
              redeemer_control.current_redeemer
            }
            CekRedeemerContextControlV1 {
              ..redeemer_control,
              cursor: redeemer_control.cursor + 1,
              map_items: cek_data_v1.prepend_data_pair_summary_v1(
                redeemer_control.active_purpose,
                next_scan.result,
                redeemer_control.map_items,
              ),
              active_scan_hash: #"",
              active_redeemer_leaf: #"",
              active_purpose: empty_data_summary_v1(),
              current_redeemer: next_current,
            }
          } else {
            CekRedeemerContextControlV1 {
              ..redeemer_control,
              active_scan_hash: cek_data_scan_v1.hash_control_v1(next_scan),
            }
          }
          and {
            current_matches,
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
    verify_cek_mint_preimage_scan_step(
      pre,
      witness,
      auxiliary,
      native_control,
      context_control,
      execution_cursor,
      completed_cpu,
      completed_memory,
    )
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
    native_resolved_context_is_well_formed(control),
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
      control.execution_count,
      control.execution_peaks,
    ),
    control.execution_count == control.purpose_count,
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
  expect
      TransactionFieldPairPreimageWitness {
        first_field_index,
        first_preimage_cbor,
        second_field_index,
        second_preimage_cbor,
      }
    = auxiliary
  expect first_field_index == 0
  expect second_field_index == 1
  let native_control = control.native_control
  let fields_verified = and {
    verify_midgard_transaction_field_preimage_v1(
      pre.transaction_id,
      pre.transaction_commitment,
      native_control.compact_cbor,
      native_control.witness_set_compact_cbor,
      native_control.field_preimage_lengths_cbor,
      first_field_index,
      first_preimage_cbor,
    ),
    verify_midgard_transaction_field_preimage_v1(
      pre.transaction_id,
      pre.transaction_commitment,
      native_control.compact_cbor,
      native_control.witness_set_compact_cbor,
      native_control.field_preimage_lengths_cbor,
      second_field_index,
      second_preimage_cbor,
    ),
  }
  let schedule_hash = transaction_resolution_schedule_hash(
    decode_midgard_tx_inputs_preimage_cbor(first_preimage_cbor),
    decode_midgard_tx_inputs_preimage_cbor(second_preimage_cbor),
  )
  let next_control = ValueAndMintControlV1 {
    ..control,
    stage: 1,
    replay_schedule_hash: schedule_hash,
    replay_remaining_schedule_hash: schedule_hash,
  }
  and {
    fields_verified,
    control.replay_schedule_hash == empty_resolution_schedule_hash(),
    control.replay_cursor == 0,
    control.replay_asset_cursor == 0,
    control.replay_value_hash == no_rejection_code_hash,
    control.replay_accumulator == initial_resolution_accumulator(),
    control.replay_remaining_schedule_hash == empty_resolution_schedule_hash(),
    control.output_cursor == 0,
    control.output_asset_cursor == 0,
    control.mint_count == 0,
    control.mint_peaks == [],
    control.mint_cursor == 0,
    control.value_accumulator == initial_value_accumulator(),
    value_and_mint_successor_is_exact(pre, witness, next_control),
  }
}

fn value_and_mint_stage_one(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: ValueAndMintControlV1,
) -> Bool {
  expect TransactionFieldPreimageWitness { preimage_cbor } = auxiliary
  let native_control = control.native_control
  let field_verified = verify_midgard_transaction_field_preimage_v1(
    pre.transaction_id,
    pre.transaction_commitment,
    native_control.compact_cbor,
    native_control.witness_set_compact_cbor,
    native_control.field_preimage_lengths_cbor,
    5,
    preimage_cbor,
  )
  let mint = decode_midgard_tx_mint_preimage_cbor(preimage_cbor)
  let leaves = mint_leaf_hashes(mint)
  let mint_frontier = validation_merkle_v1.build_frontier(leaves)
  and {
    field_verified,
    if mint_frontier.count > ledger_output_v1.max_distinct_asset_count {
      rejected_successor_is_exact(
        pre,
        witness.claimed_successor,
        reject_asset_count,
      )
    } else {
      value_and_mint_successor_is_exact(
        pre,
        witness,
        ValueAndMintControlV1 {
          ..control,
          stage: 2,
          mint_count: mint_frontier.count,
          mint_peaks: mint_frontier.peaks,
        },
      )
    },
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
      when ledger_output_v1.decode_canonical_output(value) is {
        None -> False
        Some(output) -> {
          let next_value = if source_kind == 0 {
            ValueAccumulatorV1 {
              ..control.value_accumulator,
              lovelace_delta: control.value_accumulator.lovelace_delta + output.value.lovelace,
            }
          } else {
            control.value_accumulator
          }
          let asset_count = list.length(output.value.assets)
          let next_control = if or {
            source_kind == 1,
            asset_count == 0,
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
        }
      }
    } else {
      False
    }
  } else {
    expect
        ValueInputAssetWitness {
          source_kind,
          key,
          next_schedule_hash,
          value,
          asset_index,
          mutation,
        }
      = auxiliary
    if and {
      source_kind == 0,
      resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == control.replay_remaining_schedule_hash,
      control.replay_cursor < control.native_control.resolved_input_count,
      blake2b_256(value) == control.replay_value_hash,
      asset_index == control.replay_asset_cursor - 1,
    } {
      when ledger_output_v1.decode_canonical_output(value) is {
        None -> False
        Some(output) ->
          when list.at(output.value.assets, asset_index) is {
            None -> False
            Some(Pair(unit, quantity)) ->
              when
                apply_value_asset_mutation(
                  control.value_accumulator,
                  unit,
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
                  let next_control = if control.replay_asset_cursor == list.length(
                    output.value.assets,
                  ) {
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
                      replay_asset_cursor: control.replay_asset_cursor + 1,
                      value_accumulator: next_value,
                    }
                  }
                  value_and_mint_successor_is_exact(pre, witness, next_control)
                }
              }
          }
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
      value_and_mint_successor_is_exact(
        pre,
        witness,
        ValueAndMintControlV1 { ..control, stage: 4 },
      ),
    }
  } else if control.output_asset_cursor == 0 {
    expect
        OutputReplayWitness {
          output_index,
          output_cbor,
          siblings,
          signer_proof,
        }
      = auxiliary
    if and {
      signer_proof == NoSignerSetProof,
      output_index == control.output_cursor,
      validation_merkle_v1.verify_membership(
        control.native_control.output_count,
        control.native_control.output_peaks,
        output_index,
        script_proof_v1.output_leaf_hash(output_index, output_cbor),
        siblings,
      ),
    } {
      when ledger_output_v1.decode_canonical_output(output_cbor) is {
        None -> False
        Some(output) -> {
          let next_value = ValueAccumulatorV1 {
            ..control.value_accumulator,
            lovelace_delta: control.value_accumulator.lovelace_delta - output.value.lovelace,
          }
          let next_control = if list.length(output.value.assets) == 0 {
            ValueAndMintControlV1 {
              ..control,
              output_cursor: control.output_cursor + 1,
              value_accumulator: next_value,
            }
          } else {
            ValueAndMintControlV1 {
              ..control,
              output_asset_cursor: 1,
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
    expect
        ValueOutputAssetWitness {
          output_index,
          output_cbor,
          siblings,
          asset_index,
          mutation,
        }
      = auxiliary
    if and {
      output_index == control.output_cursor,
      asset_index == control.output_asset_cursor - 1,
      validation_merkle_v1.verify_membership(
        control.native_control.output_count,
        control.native_control.output_peaks,
        output_index,
        script_proof_v1.output_leaf_hash(output_index, output_cbor),
        siblings,
      ),
    } {
      when ledger_output_v1.decode_canonical_output(output_cbor) is {
        None -> False
        Some(output) ->
          when list.at(output.value.assets, asset_index) is {
            None -> False
            Some(Pair(unit, quantity)) ->
              when
                apply_value_asset_mutation(
                  control.value_accumulator,
                  unit,
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
                  let next_control = if control.output_asset_cursor == list.length(
                    output.value.assets,
                  ) {
                    ValueAndMintControlV1 {
                      ..control,
                      output_cursor: control.output_cursor + 1,
                      output_asset_cursor: 0,
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
          }
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
  if control.mint_cursor == control.mint_count {
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
        control.mint_count,
        control.mint_peaks,
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
    control.mint_cursor == control.mint_count,
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
    native_resolved_context_is_well_formed(native_control),
    native_control.execution_cursor == native_control.execution_count,
    bytearray.length(control.replay_schedule_hash) == 32,
    bytearray.length(control.replay_value_hash) == 32,
    bytearray.length(control.replay_accumulator) == 32,
    bytearray.length(control.replay_remaining_schedule_hash) == 32,
    control.replay_cursor >= 0,
    control.replay_cursor <= native_control.resolved_input_count,
    control.replay_asset_cursor >= 0,
    control.replay_asset_cursor <= ledger_output_v1.max_distinct_asset_count,
    control.output_cursor >= 0,
    control.output_cursor <= native_control.output_count,
    control.output_asset_cursor >= 0,
    control.output_asset_cursor <= ledger_output_v1.max_distinct_asset_count,
    bytearray.length(control.value_accumulator.asset_root) == 32,
    control.value_accumulator.seen_asset_count >= 0,
    control.value_accumulator.seen_asset_count <= ledger_output_v1.max_distinct_asset_count,
    control.value_accumulator.nonzero_asset_count >= 0,
    control.value_accumulator.nonzero_asset_count <= control.value_accumulator.seen_asset_count,
    validation_merkle_v1.frontier_is_well_formed(
      control.mint_count,
      control.mint_peaks,
    ),
    control.mint_count >= 0,
    control.mint_count <= ledger_output_v1.max_distinct_asset_count,
    control.mint_cursor >= 0,
    control.mint_cursor <= control.mint_count,
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

fn ledger_delta_stage_zero(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: LedgerDeltaControlV1,
) -> Bool {
  expect
      TransactionFieldPairPreimageWitness {
        first_field_index,
        first_preimage_cbor,
        second_field_index,
        second_preimage_cbor,
      }
    = auxiliary
  expect first_field_index == 0
  expect second_field_index == 1
  let native_control = control.native_control
  let fields_verified = and {
    verify_midgard_transaction_field_preimage_v1(
      pre.transaction_id,
      pre.transaction_commitment,
      native_control.compact_cbor,
      native_control.witness_set_compact_cbor,
      native_control.field_preimage_lengths_cbor,
      first_field_index,
      first_preimage_cbor,
    ),
    verify_midgard_transaction_field_preimage_v1(
      pre.transaction_id,
      pre.transaction_commitment,
      native_control.compact_cbor,
      native_control.witness_set_compact_cbor,
      native_control.field_preimage_lengths_cbor,
      second_field_index,
      second_preimage_cbor,
    ),
  }
  let schedule_hash = transaction_resolution_schedule_hash(
    decode_midgard_tx_inputs_preimage_cbor(first_preimage_cbor),
    decode_midgard_tx_inputs_preimage_cbor(second_preimage_cbor),
  )
  and {
    fields_verified,
    control.replay_schedule_hash == empty_resolution_schedule_hash(),
    control.replay_cursor == 0,
    control.replay_accumulator == initial_resolution_accumulator(),
    control.replay_remaining_schedule_hash == empty_resolution_schedule_hash(),
    control.current_ledger_root == pre.prior_ledger_root,
    control.output_cursor == 0,
    control.operation_count == 0,
    control.operation_peaks == [],
    ledger_delta_successor_is_exact(
      pre,
      witness,
      LedgerDeltaControlV1 {
        ..control,
        stage: 1,
        replay_schedule_hash: schedule_hash,
        replay_remaining_schedule_hash: schedule_hash,
      },
    ),
  }
}

fn ledger_delta_replay_step(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: LedgerDeltaControlV1,
) -> Bool {
  expect
      LedgerDeltaReplayWitness {
        source_kind,
        key,
        next_schedule_hash,
        value,
        proof,
      }
    = auxiliary
  if and {
    source_kind == 0 || source_kind == 1,
    resolution_schedule_node_hash(source_kind, key, next_schedule_hash) == control.replay_remaining_schedule_hash,
    control.replay_cursor < control.native_control.resolved_input_count,
    ledger_output_v1.decode_canonical_output(value) != None,
  } {
    let next_accumulator = resolved_input_accumulator_successor(
      control.replay_accumulator,
      source_kind,
      key,
      value,
    )
    if source_kind == 1 {
      and {
        proof == [],
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
    } else if mpf_proof_v1.proof_has_at_most_steps(proof, 16) {
      when
        mpf_proof_v1.delete_root(control.current_ledger_root, key, value, proof)
      is {
        None -> False
        Some(next_root) -> {
          let next_peaks = validation_merkle_v1.append_leaf(
            control.operation_count,
            control.operation_peaks,
            ledger_delta_operation_leaf_hash(0, key, #""),
          )
          ledger_delta_successor_is_exact(
            pre,
            witness,
            LedgerDeltaControlV1 {
              ..control,
              replay_cursor: control.replay_cursor + 1,
              replay_accumulator: next_accumulator,
              replay_remaining_schedule_hash: next_schedule_hash,
              current_ledger_root: next_root,
              operation_count: control.operation_count + 1,
              operation_peaks: next_peaks,
            },
          )
        }
      }
    } else {
      False
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
  if control.replay_remaining_schedule_hash == empty_resolution_schedule_hash() {
    and {
      auxiliary == NoAuxiliaryWitness,
      control.replay_cursor == control.native_control.resolved_input_count,
      control.replay_accumulator == control.native_control.resolved_inputs_accumulator,
      ledger_delta_successor_is_exact(
        pre,
        witness,
        LedgerDeltaControlV1 { ..control, stage: 2 },
      ),
    }
  } else {
    ledger_delta_replay_step(pre, witness, auxiliary, control)
  }
}

fn ledger_delta_stage_two(
  pre: validation_trace_v1.ValidationMachineStateV1,
  witness: ValidationOneStepWitnessV1,
  auxiliary: ValidationAuxiliaryWitnessV1,
  control: LedgerDeltaControlV1,
) -> Bool {
  if control.output_cursor == control.native_control.output_count {
    and {
      auxiliary == NoAuxiliaryWitness,
      ledger_delta_successor_is_exact(
        pre,
        witness,
        LedgerDeltaControlV1 { ..control, stage: 3 },
      ),
    }
  } else {
    expect
        LedgerDeltaOutputWitness { output_index, output_cbor, siblings, proof }
      = auxiliary
    let key = encode_midgard_tx_input(
      MidgardTxInput { tx_id: pre.transaction_id, output_index },
    )
    if and {
      output_index == control.output_cursor,
      bytearray.length(output_cbor) <= ledger_output_v1.max_output_canonical_cbor_bytes,
      ledger_output_v1.decode_canonical_output(output_cbor) != None,
      validation_merkle_v1.verify_membership(
        control.native_control.output_count,
        control.native_control.output_peaks,
        output_index,
        script_proof_v1.output_leaf_hash(output_index, output_cbor),
        siblings,
      ),
      mpf_proof_v1.proof_has_at_most_steps(proof, 16),
    } {
      when
        mpf_proof_v1.insert_root(
          control.current_ledger_root,
          key,
          output_cbor,
          proof,
        )
      is {
        None -> False
        Some(next_root) -> {
          let next_peaks = validation_merkle_v1.append_leaf(
            control.operation_count,
            control.operation_peaks,
            ledger_delta_operation_leaf_hash(1, key, output_cbor),
          )
          ledger_delta_successor_is_exact(
            pre,
            witness,
            LedgerDeltaControlV1 {
              ..control,
              current_ledger_root: next_root,
              output_cursor: control.output_cursor + 1,
              operation_count: control.operation_count + 1,
              operation_peaks: next_peaks,
            },
          )
        }
      }
    } else {
      False
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
    control.replay_cursor == control.native_control.resolved_input_count,
    control.replay_accumulator == control.native_control.resolved_inputs_accumulator,
    control.replay_remaining_schedule_hash == empty_resolution_schedule_hash(),
    control.output_cursor == control.native_control.output_count,
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
  let native_control = control.native_control
  let Pair(_, _) = verify_native_tx_proof_source_v1(
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
    native_resolved_context_is_well_formed(native_control),
    native_control.execution_cursor == native_control.execution_count,
    bytearray.length(control.replay_schedule_hash) == 32,
    bytearray.length(control.replay_accumulator) == 32,
    bytearray.length(control.replay_remaining_schedule_hash) == 32,
    bytearray.length(control.current_ledger_root) == 32,
    control.replay_cursor >= 0,
    control.replay_cursor <= native_control.resolved_input_count,
    control.output_cursor >= 0,
    control.output_cursor <= native_control.output_count,
    control.operation_count >= 0,
    control.operation_count <= 128,
    validation_merkle_v1.frontier_is_well_formed(
      control.operation_count,
      control.operation_peaks,
    ),
    control.stage >= 0,
    control.stage <= 3,
    witness.work_witness_cbor == encode_ledger_delta_control_v1(control),
    if control.stage == 0 {
      ledger_delta_stage_zero(pre, witness, auxiliary, control)
    } else if control.stage == 1 {
      ledger_delta_stage_one(pre, witness, auxiliary, control)
    } else if control.stage == 2 {
      ledger_delta_stage_two(pre, witness, auxiliary, control)
    } else {
      ledger_delta_stage_three(pre, witness, auxiliary, control)
    },
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
        verify_phase_a_native_scripts(pre, witness, auxiliary)
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

pub fn verify_signatures_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  verify_signatures(pre, evidence.transition, evidence.auxiliary)
}

pub fn verify_phase_a_native_scripts_one_step_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  and {
    pre.phase == validation_trace_v1.PhaseANativeScripts,
    structural_transition_is_valid(pre, evidence.transition),
    verify_phase_a_native_scripts(pre, evidence.transition, evidence.auxiliary),
  }
}

pub fn verify_phase_a_native_scripts_semantics_v1(
  pre: validation_trace_v1.ValidationMachineStateV1,
  evidence: ValidationOneStepEvidenceV1,
) -> Bool {
  verify_phase_a_native_scripts(pre, evidence.transition, evidence.auxiliary)
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
