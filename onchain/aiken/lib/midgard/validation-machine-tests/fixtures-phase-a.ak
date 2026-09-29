use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/crypto.{blake2b_224, blake2b_256}
use aiken/merkle_patricia_forestry/helpers.{combine, suffix}
use aiken/primitive/bytearray
use midgard/bounded_collection_v1
use midgard/bounded_item_v1
use midgard/cek_context_chain as context_chain
use midgard/cek_context_observer as bounded_observer
use midgard/cek_context_step as bounded_context
use midgard/cek_data_v1
use midgard/cek_proof_v1
use midgard/forced_submission_fixture
use midgard/fraud_proofs/native_tx/compact.{
  encode_native_tx_body_compact, forced_tx_proof_commitment_v1,
  native_tx_id_for_version,
}
use midgard/fraud_proofs/native_tx/transaction as native_transaction
use midgard/fraud_proofs/native_tx/transaction.{midgard_transaction_to_compact}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddressWitness, MidgardTransaction, MidgardTransactionBody,
  MidgardTransactionWitnessSet, MidgardTxInput, MidgardVersionedScript,
  NativeCardanoScript, TxIsValid, native_tx_version_v1,
}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_proof_v1
use midgard/ledger_state.{ForcedTxProofSourceV1, NativeTxProofSourceV1}
use midgard/mpf_proof_fold_v1
use midgard/native_tx_field_access_v1
use midgard/script_proof_v1
use midgard/validation_machine/cek as vm_cek
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/shared as vm_shared
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_field_door_v1 as field_door
use midgard/validation_machine_tests/constants.{
  h_28_a, h_a, h_b, h_c, maximum_mint_policy_item_cbor, validation_context_cbor,
  zero,
}
use midgard/validation_machine_tests/fixture_types.{
  AuthenticatedOutputMaterialV1,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

/// These hand-written machine scenarios adjudicate forced submissions. Existing
/// native transaction builders supply body/witness fixture data only; the proof
/// source removes their validity before it is committed or placed in a witness.
pub fn submitted_source(tx: MidgardTransaction) -> ForcedTxProofSourceV1 {
  let source = native_transaction.midgard_transaction_proof_source_v1(tx)
  forced_submission_fixture.from_native_source(
    NativeTxProofSourceV1 {
      compact_cbor: source.compact_cbor,
      witness_set_compact_cbor: source.witness_set_compact_cbor,
      field_preimage_lengths_cbor: source.field_preimage_lengths_cbor,
    },
  )
}

pub fn submitted_commitment(tx: MidgardTransaction) -> ByteArray {
  let source = submitted_source(tx)
  forced_tx_proof_commitment_v1(
    source.compact_cbor,
    source.witness_set_compact_cbor,
    source.field_preimage_lengths_cbor,
  )
}

/// The pending forced-source machine state the hand-written steps start from:
/// event `h_a` over `tx`'s forced commitment, prior ledger root `h_c`, no
/// execution spent, and a work root committing `work_witness_cbor` at
/// `(phase, program_counter)`. A step that differs elsewhere overrides the
/// field with a record update rather than spelling out the whole state.
pub fn forced_pending_state(
  transaction_id: ByteArray,
  tx: MidgardTransaction,
  context_cbor: ByteArray,
  phase: validation_trace_v1.ValidationPhase,
  program_counter: Int,
  work_witness_cbor: ByteArray,
  ledger_delta_root: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: h_a,
    transaction_id,
    transaction_commitment: submitted_commitment(tx),
    validation_context_hash: validation_trace_v1.hash_validation_context(
      context_cbor,
    ),
    source_kind: validation_trace_v1.Forced,
    prior_ledger_root: h_c,
    phase,
    program_counter,
    work_root: validation_trace_v1.hash_work_witness(
      phase,
      program_counter,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root,
  }
}

/// One step's evidence: the claimed successor with the work witness that
/// commits to it, and the step's auxiliary witness.
pub fn one_step_evidence(
  work_witness_cbor: ByteArray,
  claimed_successor: validation_trace_v1.ValidationMachineStateV1,
  auxiliary: vm_machine_types.ValidationAuxiliaryWitnessV1,
) -> vm_machine_types.ValidationOneStepEvidenceV1 {
  vm_machine_types.ValidationOneStepEvidenceV1 {
    transition: vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor,
    },
    auxiliary,
  }
}

/// §8's door context for a step whose carriage is tier 1.
///
/// Every hand-written step in this module names an `Inline` carriage — the
/// preimage travels in the evidence — so the door needs neither the
/// consuming transaction's reference inputs (§8.2, tier 2) nor the §8.6
/// certificate minting policy id (§8.4, tier 3). Both are supplied empty, and
/// that is a *fact about these vectors* rather than a convenience: the tier-2
/// and tier-3 ladders are exercised where they belong, in
/// `native-tx-field-access-v1.test.ak`, against the door itself. A step here
/// that accidentally named a reference tier would fail closed rather than pass
/// with an unused context.
pub fn inline_field_door() -> field_door.MachineFieldDoorV1 {
  field_door.MachineFieldDoorV1 {
    reference_inputs: [],
    certificate_policy_id: #"",
  }
}

/// A tier-1 carriage over a field whose §5.1 preimage is exactly `items`.
///
/// The replacement for the retired `singleton_field_item_proofs` collection
/// half: instead of asserting a per-item Merkle opening against the field's
/// flat commitment, a step now carries the field's whole preimage and the door
/// hashes it once (§4, §8.8). Built through `encode_field_preimage`, so a
/// fixture cannot drift away from §5.1's array header and byte-string wrappers.
pub fn inline_field_carriage(
  items: List<ByteArray>,
) -> native_tx_field_access_v1.FieldCarriageV1 {
  native_tx_field_access_v1.Inline {
    preimage: native_tx_field_access_v1.encode_field_preimage(items),
  }
}

/// §5.1/§5.6 — the enveloped field-5 preimage carrying that one policy item.
///
/// The item's own bytes are unchanged by the flat reversion; what changed is
/// that field 5 is an enveloped **list** of per-policy items rather than the
/// retired raw map, so the preimage is the item under a §5.1 wrapper. Built
/// through the canonical encoder rather than hand-spelled, so a fixture cannot
/// drift away from §5.1's array and byte-string heads.
pub fn maximum_mint_preimage_cbor() -> ByteArray {
  native_tx_field_access_v1.encode_field_preimage(
    [maximum_mint_policy_item_cbor],
  )
}

pub fn v1_program_envelope() -> ByteArray {
  cek_proof_v1.encode_program_envelope_v1(1, 1, 0, h_a, 3, 144)
}

pub fn empty_mpf_proof_descriptor() -> mpf_proof_fold_v1.ProofDescriptorV1 {
  mpf_proof_fold_v1.ProofDescriptorV1 {
    version: 1,
    frame_count: 0,
    terminal_cursor: 0,
    peaks: [],
  }
}

pub fn empty_v1_transaction() -> MidgardTransaction {
  let empty_mint: Data = []
  MidgardTransaction {
    version: native_tx_version_v1,
    validity: TxIsValid,
    body: MidgardTransactionBody {
      inputs: [],
      reference_inputs: [],
      outputs: [],
      fee: 0,
      validity_interval_start: -1,
      validity_interval_end: -1,
      required_observers: [],
      required_signers: [],
      mint: empty_mint,
      script_integrity_hash: blake2b_256(#"f6"),
      auxiliary_data_hash: zero,
      network_id: 255,
    },
    witness_set: MidgardTransactionWitnessSet {
      addr_tx_wits: [],
      script_tx_wits: [],
      redeemer_tx_wits: [],
    },
  }
}

pub fn repeated_input(input: MidgardTxInput, count: Int) -> List<MidgardTxInput> {
  if count <= 0 {
    []
  } else {
    [input, ..repeated_input(input, count - 1)]
  }
}

pub fn address_witness_key_hashes(
  witnesses: List<MidgardAddressWitness>,
) -> List<ByteArray> {
  when witnesses is {
    [] -> []
    [witness, ..rest] ->
      [
        blake2b_224(witness.verification_key),
        ..address_witness_key_hashes(rest)
      ]
  }
}

pub fn signer_frontier_for_tx(
  tx: MidgardTransaction,
) -> validation_merkle_v1.BuiltFrontier {
  tx.witness_set.addr_tx_wits
    |> address_witness_key_hashes
    |> list.sort(bytearray.compare)
    |> list.unique
    |> list.map(script_proof_v1.signer_leaf_hash)
    |> validation_merkle_v1.build_frontier
}

pub fn contains_non_native_script(scripts: List<MidgardVersionedScript>) -> Bool {
  when scripts is {
    [] -> False
    [script, ..rest] -> or {
        script.language != NativeCardanoScript,
        contains_non_native_script(rest),
      }
  }
}

pub fn signatures_scan_witness(
  tx: MidgardTransaction,
  stage: Int,
  address_count: Int,
  required_count: Int,
  address_seen: Int,
  required_seen: Int,
  previous_order_key: ByteArray,
  previous_signer_hash: ByteArray,
  signer_frontier: validation_merkle_v1.BuiltFrontier,
  invalid_signature_seen: Int,
) -> ByteArray {
  let proof_source = submitted_source(tx)
  vm_witness.encode_signatures_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    stage,
    address_count,
    required_count,
    address_seen,
    required_seen,
    previous_order_key,
    previous_signer_hash,
    signer_frontier.count,
    signer_frontier.peaks,
    invalid_signature_seen,
  )
}

pub fn initial_signatures_witness(tx: MidgardTransaction) -> ByteArray {
  signatures_scan_witness(
    tx,
    0,
    if tx.witness_set.addr_tx_wits == [] {
      0
    } else {
      -1
    },
    if tx.body.required_signers == [] {
      0
    } else {
      -1
    },
    0,
    0,
    #"",
    #"",
    validation_merkle_v1.build_frontier([]),
    0,
  )
}

pub fn signatures_step_fixture(
  tx: MidgardTransaction,
  ledger_delta_root: ByteArray,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let work_witness_cbor = initial_signatures_witness(tx)
  Pair(
    forced_pending_state(
      tx_id,
      tx,
      validation_context_cbor,
      validation_trace_v1.Signatures,
      4,
      work_witness_cbor,
      ledger_delta_root,
    ),
    work_witness_cbor,
  )
}

pub fn phase_a_native_step_fixture(
  tx: MidgardTransaction,
  ledger_delta_root: ByteArray,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let Pair(signature_pre, _signature_work_witness_cbor) = signatures_step_fixture(
    tx,
    ledger_delta_root,
  )
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let script_count = if tx.witness_set.script_tx_wits == [] {
    0
  } else {
    -1
  }
  let work_witness_cbor = vm_witness.encode_phase_a_native_scripts_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
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
    -1,
    signer_frontier.count,
    signer_frontier.peaks,
    #"",
  )
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      ..signature_pre,
      phase: validation_trace_v1.PhaseANativeScripts,
      program_counter: 5,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.PhaseANativeScripts,
        5,
        work_witness_cbor,
      ),
    },
    work_witness_cbor,
  )
}

pub fn phase_a_native_scan_witness(
  tx: MidgardTransaction,
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
) -> ByteArray {
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  vm_witness.encode_phase_a_native_scripts_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    stage,
    script_count,
    script_seen,
    contains_non_native_script,
    item_length,
    item_commitment,
    cursor,
    stack_root,
    stack_depth,
    node_count,
    result,
    signer_frontier.count,
    signer_frontier.peaks,
    #"",
  )
}

pub fn next_phase_a_native_state(
  pre: validation_trace_v1.ValidationMachineStateV1,
  work_witness_cbor: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: pre.program_counter + 1,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.PhaseANativeScripts,
      pre.program_counter + 1,
      work_witness_cbor,
    ),
  }
}

/// The single-chunk `bounded_item_v1` proof over item 0 of a single-item field.
///
/// This is what survives of the retired `singleton_field_item_proofs`. Its
/// collection half is gone: no step opens a per-item collection proof against a
/// §4 flat field commitment any more, and the carriage that replaced it is
/// `inline_field_carriage([key])`. The **chunk** half stays because four phases
/// hand a partially-read item forward across steps and their control state
/// carries a genuine `bounded_item_v1` root the later step opens against
/// (`NativeScriptTokenWitness`, `NativeExecutionScanWitness.first_chunk_proof`,
/// the redeemer item proof) — a satisfiable opening the flat reversion never
/// touched.
pub fn singleton_field_item_chunk_proof(
  field_index: Int,
  key: ByteArray,
) -> bounded_item_v1.ChunkProofV1 {
  let chunk_frontier = [
    validation_merkle_v1.FrontierPeak {
      height: 0,
      hash: bounded_item_v1.hash_chunk(field_index, 0, 0, key),
    },
  ]
  bounded_item_v1.ChunkProofV1 {
    version: bounded_item_v1.version,
    field_index,
    item_index: 0,
    total_length: bytearray.length(key),
    chunk_index: 0,
    chunk: key,
    frontier: chunk_frontier,
    siblings: [],
  }
}

pub fn indexed_inline_source_leaves(
  script: MidgardVersionedScript,
  source_index: Int,
  source_count: Int,
) -> List<ByteArray> {
  if source_index == source_count {
    []
  } else {
    [
      script_proof_v1.source_leaf_hash(0, cbor.serialise(source_index), script),
      ..indexed_inline_source_leaves(script, source_index + 1, source_count)
    ]
  }
}

pub fn indexed_spend_purpose_leaves(
  purpose_index: Int,
  purpose_count: Int,
) -> List<ByteArray> {
  if purpose_index == purpose_count {
    []
  } else {
    [
      script_proof_v1.purpose_leaf_hash(
        0,
        purpose_index,
        h_28_a,
        cbor.serialise(purpose_index),
      ),
      ..indexed_spend_purpose_leaves(purpose_index + 1, purpose_count)
    ]
  }
}

pub fn merkle_parent_layer(leaves: List<ByteArray>) -> List<ByteArray> {
  when leaves is {
    [] -> []
    [left, right, ..rest] ->
      [
        validation_merkle_v1.hash_branch(left, right),
        ..merkle_parent_layer(rest)
      ]
    _ -> fail @"power-of-two membership layer must contain complete pairs"
  }
}

pub fn first_leaf_power_of_two_siblings(
  leaves: List<ByteArray>,
) -> List<ByteArray> {
  when leaves is {
    [_leaf] -> []
    [_leaf, sibling, ..] ->
      [sibling, ..first_leaf_power_of_two_siblings(merkle_parent_layer(leaves))]
    _ -> fail @"membership tree must be a non-empty power of two"
  }
}

pub fn input_sets_scan_witness(
  tx: MidgardTransaction,
  spend_count: Int,
  reference_count: Int,
  spend_seen: Int,
  reference_seen: Int,
  previous_key: ByteArray,
  schedule_hash: ByteArray,
) -> ByteArray {
  let proof_source = submitted_source(tx)
  vm_witness.encode_input_sets_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    spend_count,
    reference_count,
    spend_seen,
    reference_seen,
    previous_key,
    schedule_hash,
  )
}

pub fn input_sets_pre(
  tx: MidgardTransaction,
  program_counter: Int,
  work_witness_cbor: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  forced_pending_state(
    tx_id,
    tx,
    validation_context_cbor,
    validation_trace_v1.InputSets,
    program_counter,
    work_witness_cbor,
    validation_trace_v1.hash_ledger_delta(#"80"),
  )
}

// The canonical exact-rejection successor. It deliberately CARRIES FORWARD
// `pre.ledger_delta_root` (via `..pre`) rather than clearing it: the claimed
// delta commitment is immutable context, and clearing it here is what made
// every rejection one-step unprovable from a realistic (non-empty) pre-state.
// The rejecting terminal's no-op obligation lives in
// `encode_terminal_rejection_witness` (empty operation list, post root = prior
// root) and in the claim layer, not in this field.
pub fn exact_rejection_post(
  pre: validation_trace_v1.ValidationMachineStateV1,
  rejection_code: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.Terminal,
    program_counter: pre.program_counter + 1,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Terminal,
      pre.program_counter + 1,
      vm_shared.encode_terminal_rejection_witness(
        rejection_code,
        pre.prior_ledger_root,
      ),
    ),
    verdict: validation_trace_v1.Rejected,
    rejection_code_hash: validation_trace_v1.hash_rejection_code(rejection_code),
  }
}

// A realistic NON-EMPTY claimed ledger delta root: the frontier commitment over
// one authenticated deletion and one authenticated insertion. This is the shape
// every real transaction carries (it spends inputs and creates outputs), and it
// is therefore the shape the rejection machinery must work against. Using
// `frontier_commitment(0, [])` for rejection fixtures was the workaround that
// hid the `rejected_successor_is_exact` / `immutable_context_matches`
// contradiction, because it is the one pre-state in which the contradiction is
// invisible.
pub fn non_empty_claimed_delta_root() -> ByteArray {
  let descriptor = empty_mpf_proof_descriptor()
  let deletion = vm_shared.ledger_delta_operation_leaf_hash(
    0,
    #"010203",
    #"",
    descriptor,
  )
  let insertion = vm_shared.ledger_delta_operation_leaf_hash(
    1,
    #"0405",
    #"060708",
    descriptor,
  )
  let frontier = validation_merkle_v1.build_frontier([deletion, insertion])
  validation_merkle_v1.frontier_commitment(frontier.count, frontier.peaks)
}

pub fn phase_a_script_preconditions_fixture(
  tx: MidgardTransaction,
  ledger_delta_root: ByteArray,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let Pair(native_pre, _native_work_witness_cbor) = phase_a_native_step_fixture(
    tx,
    ledger_delta_root,
  )
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let work_witness_cbor = vm_witness.encode_phase_a_script_preconditions_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    signer_frontier.count,
    validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    contains_non_native_script(tx.witness_set.script_tx_wits),
    0,
    0,
    #"",
  )
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      ..native_pre,
      phase: validation_trace_v1.PhaseAScriptPreconditions,
      program_counter: 6,
      work_root: validation_trace_v1.hash_work_witness(
        validation_trace_v1.PhaseAScriptPreconditions,
        6,
        work_witness_cbor,
      ),
    },
    work_witness_cbor,
  )
}

/// The field-3 preimage of the duplicate-observer fixture: two items, both the
/// same 28-byte observer hash.
///
/// Under the retired counted scheme this had to be a pair of per-item openings
/// with a shared collection frontier and a sibling each. Under §8's door the
/// duplicate *is* the preimage, and which of the two items a step reads is the
/// `item_index` it names — which is the whole point of the fixture, and is now
/// visible in one line instead of thirty.
pub fn duplicate_observer_field_carriage() -> native_tx_field_access_v1.FieldCarriageV1 {
  inline_field_carriage([h_28_a, h_28_a])
}

pub fn observer_hash_at(item_index: Int) -> ByteArray {
  bytearray.from_int_big_endian(item_index + 1, 28)
}

pub fn observer_hashes_from(item_index: Int, item_count: Int) -> List<ByteArray> {
  if item_index == item_count {
    []
  } else {
    [
      observer_hash_at(item_index),
      ..observer_hashes_from(item_index + 1, item_count)
    ]
  }
}

pub fn observer_collection_leaves_from(
  item_index: Int,
  item_count: Int,
) -> List<ByteArray> {
  if item_index == item_count {
    []
  } else {
    let observer_hash = observer_hash_at(item_index)
    let item_commitment = bounded_item_v1.from_bytes(
      3,
      item_index,
      observer_hash,
    )
    [
      bounded_collection_v1.hash_item(3, item_index, 28, item_commitment),
      ..observer_collection_leaves_from(item_index + 1, item_count)
    ]
  }
}

pub fn observer_collection_subtree_root(start: Int, size: Int) -> ByteArray {
  if size == 1 {
    let observer_hash = observer_hash_at(start)
    bounded_collection_v1.hash_item(
      3,
      start,
      28,
      bounded_item_v1.from_bytes(3, start, observer_hash),
    )
  } else {
    let half = size / 2
    validation_merkle_v1.hash_branch(
      observer_collection_subtree_root(start, half),
      observer_collection_subtree_root(start + half, half),
    )
  }
}

/// The field-3 preimage of an `item_count`-observer fixture, as a tier-1
/// carriage.
///
/// Replaces `observer_field_item_proofs`: no per-item frontier, no siblings
/// argument. The item a step reads is chosen by the `item_index` in its
/// evidence, and the field's §5.2 count is the authenticated header of these
/// very bytes, so a step cannot understate it.
pub fn observer_field_carriage(
  item_count: Int,
) -> native_tx_field_access_v1.FieldCarriageV1 {
  inline_field_carriage(observer_hashes_from(0, item_count))
}

pub fn resolve_inputs_step_fixture(
  tx: MidgardTransaction,
  prior_ledger_root: ByteArray,
  cursor: Int,
  accumulator: ByteArray,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let work_witness_cbor = vm_witness.encode_resolve_inputs_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    cursor,
    accumulator,
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
    signer_frontier.count,
    validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    None,
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  )
  Pair(
    validation_trace_v1.ValidationMachineStateV1 {
      ..forced_pending_state(
        tx_id,
        tx,
        validation_context_cbor,
        validation_trace_v1.ResolveInputs,
        program_counter,
        work_witness_cbor,
        // Re-based off `frontier_commitment(0, [])`: this fixture backs rejection
        // proofs, so it must carry the realistic non-empty claimed delta.
        non_empty_claimed_delta_root(),
      ),
      prior_ledger_root,
    },
    work_witness_cbor,
  )
}

pub fn script_sources_output_step_fixture_with_resolved_items(
  tx: MidgardTransaction,
  resolved_input_count: Int,
  resolved_inputs_accumulator: ByteArray,
  stage: Int,
  source_frontier: validation_merkle_v1.BuiltFrontier,
  redeemer_frontier: validation_merkle_v1.BuiltFrontier,
  replay_cursor: Int,
  resolved_item_frontier: validation_merkle_v1.BuiltFrontier,
  replay_accumulator: ByteArray,
  replay_remaining_schedule_hash: ByteArray,
  spend_index: Int,
  purpose_frontier: validation_merkle_v1.BuiltFrontier,
  output_cursor: Int,
  output_frontier: validation_merkle_v1.BuiltFrontier,
  output_total_count: Int,
  receive_scan: vm_machine_types.ReceivePurposeScanControlV1,
  observer_scan: vm_machine_types.ObserverPurposeScanControlV1,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  // The mint fold only carries the `policy_count == -1` "not yet begun"
  // sentinel while the machine is at stage 6 or earlier. `script_sources_stage_six`
  // always leaves stage 6 with `policy_count >= 0` (validation-machine-v1.ak:9548
  // for the empty-mint case, :9553 otherwise), and both stage-7 binders --
  // `mint_fold_control_is_well_formed` (:12072) on the evidence path and
  // `script_sources_stage_seven_control_is_bound` (:10005) on the deployed path --
  // reject the sentinel from stage 7 on. Fixtures at stage >= 7 must therefore
  // use the settled fold, matching `script_discovery_step_fixture_with_output_frontier`.
  let mint_fold = if stage >= 7 {
    vm_machine_types.MintFoldControlV1 {
      ..vm_witness.empty_mint_fold_control(),
      policy_count: 0,
    }
  } else {
    vm_witness.empty_mint_fold_control()
  }
  let work_witness_cbor = vm_witness.encode_script_sources_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    resolved_input_count,
    resolved_inputs_accumulator,
    signer_frontier.count,
    validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    resolved_item_frontier.peaks,
    stage,
    source_frontier.count,
    source_frontier.peaks,
    redeemer_frontier.count,
    redeemer_frontier.peaks,
    replay_cursor,
    replay_accumulator,
    replay_remaining_schedule_hash,
    spend_index,
    purpose_frontier.count,
    purpose_frontier.peaks,
    output_cursor,
    output_frontier.count,
    output_frontier.peaks,
    output_total_count,
    receive_scan,
    source_frontier.count,
    redeemer_frontier.count,
    observer_scan,
    mint_fold,
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  )
  Pair(
    forced_pending_state(
      tx_id,
      tx,
      validation_context_cbor,
      validation_trace_v1.ScriptSources,
      program_counter,
      work_witness_cbor,
      validation_trace_v1.hash_ledger_delta(#"80"),
    ),
    work_witness_cbor,
  )
}

pub fn script_sources_output_step_fixture(
  tx: MidgardTransaction,
  resolved_input_count: Int,
  resolved_inputs_accumulator: ByteArray,
  stage: Int,
  source_frontier: validation_merkle_v1.BuiltFrontier,
  redeemer_frontier: validation_merkle_v1.BuiltFrontier,
  replay_cursor: Int,
  replay_accumulator: ByteArray,
  replay_remaining_schedule_hash: ByteArray,
  spend_index: Int,
  purpose_frontier: validation_merkle_v1.BuiltFrontier,
  output_cursor: Int,
  output_frontier: validation_merkle_v1.BuiltFrontier,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  script_sources_output_step_fixture_with_resolved_items(
    tx,
    resolved_input_count,
    resolved_inputs_accumulator,
    stage,
    source_frontier,
    redeemer_frontier,
    replay_cursor,
    validation_merkle_v1.build_frontier([]),
    replay_accumulator,
    replay_remaining_schedule_hash,
    spend_index,
    purpose_frontier,
    output_cursor,
    output_frontier,
    output_frontier.count,
    vm_witness.empty_receive_purpose_scan_control(),
    vm_witness.empty_observer_purpose_scan_control(),
    program_counter,
  )
}

pub fn script_sources_mint_fold_fixture(
  tx: MidgardTransaction,
  purpose_frontier: validation_merkle_v1.BuiltFrontier,
  mint_fold: vm_machine_types.MintFoldControlV1,
  stage: Int,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let work_witness_cbor = vm_witness.encode_script_sources_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
    0,
    vm_witness.initial_resolution_accumulator(),
    signer_frontier.count,
    validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    empty_frontier.peaks,
    stage,
    0,
    empty_frontier.peaks,
    0,
    empty_frontier.peaks,
    0,
    vm_witness.initial_resolution_accumulator(),
    vm_witness.empty_resolution_schedule_hash(),
    0,
    purpose_frontier.count,
    purpose_frontier.peaks,
    0,
    0,
    empty_frontier.peaks,
    0,
    vm_witness.empty_receive_purpose_scan_control(),
    0,
    0,
    vm_witness.empty_observer_purpose_scan_control(),
    mint_fold,
    vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  )
  Pair(
    forced_pending_state(
      tx_id,
      tx,
      validation_context_cbor,
      validation_trace_v1.ScriptSources,
      program_counter,
      work_witness_cbor,
      validation_trace_v1.hash_ledger_delta(#"80"),
    ),
    work_witness_cbor,
  )
}

pub fn advanced_output_proof(
  result: Option<ledger_output_proof_v1.LedgerOutputProofStepResultV1>,
) -> ledger_output_proof_v1.LedgerOutputProofControlV1 {
  expect Some(ledger_output_proof_v1.LedgerOutputProofAdvanced { control }) = result
  control
}

pub fn authenticated_output_material(
  output_cbor: ByteArray,
) -> AuthenticatedOutputMaterialV1 {
  let chunk_proof = singleton_field_item_chunk_proof(2, output_cbor)
  let item_commitment = bounded_item_v1.from_bytes(2, 0, output_cbor)
  let initial = ledger_output_proof_v1.initial_control_v1(
    0,
    bytearray.length(output_cbor),
    item_commitment,
  )
  let first_successor = advanced_output_proof(
    ledger_output_proof_v1.step_v1(
      initial,
      ledger_output_proof_v1.LedgerOutputProofChunks {
        chunk_proof,
        next_chunk_proof: None,
      },
    ),
  )
  let optional_field = advanced_output_proof(
    ledger_output_proof_v1.step_v1(
      first_successor,
      ledger_output_proof_v1.LedgerOutputProofChunks {
        chunk_proof,
        next_chunk_proof: None,
      },
    ),
  )
  let scan_terminal = advanced_output_proof(
    ledger_output_proof_v1.step_v1(
      optional_field,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  let value_ready = advanced_output_proof(
    ledger_output_proof_v1.step_v1(
      scan_terminal,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  let value_finalize = advanced_output_proof(
    ledger_output_proof_v1.step_v1(
      value_ready,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  let value_terminal = advanced_output_proof(
    ledger_output_proof_v1.step_v1(
      value_finalize,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  let terminal = advanced_output_proof(
    ledger_output_proof_v1.step_v1(
      value_terminal,
      ledger_output_proof_v1.LedgerOutputProofNoWitness,
    ),
  )
  expect Some(cardano_tx_out) = ledger_output_proof_v1.cardano_tx_out_summary_v1(
    terminal,
  )
  expect Some(midgard_tx_out) = ledger_output_proof_v1.midgard_tx_out_summary_v1(
    terminal,
  )
  expect Some(cardano_spend_datum) = ledger_output_proof_v1.cardano_spend_datum_summary_v1(
    terminal,
  )
  let descriptor = ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
    version: ledger_output_commitment_v1.version,
    output_index: terminal.output_index,
    total_length: terminal.total_length,
    item_commitment: terminal.item_commitment,
    address: terminal.output_scan.address,
    lovelace: terminal.output_scan.lovelace,
    asset_count: terminal.output_scan.asset_count,
    asset_frontier_commitment: validation_merkle_v1.frontier_commitment(
      terminal.output_scan.asset_count,
      terminal.output_scan.asset_peaks,
    ),
    cardano_value_size: terminal.output_scan.cardano_value_size,
    reference_script_language: -1,
    reference_script_hash: #"",
    reference_script_total_length: 0,
    reference_script_item_commitment: #"",
    cardano_tx_out,
    midgard_tx_out,
    cardano_spend_datum,
  }
  let descriptor_cbor = ledger_output_commitment_v1.encode(descriptor)
  expect Some(facts_one) = ledger_output_proof_v1.fact_attach_v1(
    terminal,
    descriptor_cbor,
  )
  expect Some(facts_two) = ledger_output_proof_v1.fact_attach_v1(
    facts_one,
    descriptor_cbor,
  )
  expect Some(facts_terminal) = ledger_output_proof_v1.fact_attach_v1(
    facts_two,
    descriptor_cbor,
  )
  AuthenticatedOutputMaterialV1 {
    item_commitment,
    chunk_proof,
    initial,
    first_successor,
    terminal,
    facts_terminal,
    descriptor_cbor,
  }
}

pub fn script_sources_active_output_proof_fixture(
  tx: MidgardTransaction,
  output_cursor: Int,
  output_frontier: validation_merkle_v1.BuiltFrontier,
  receive_scan: vm_machine_types.ReceivePurposeScanControlV1,
  output_proof: ledger_output_proof_v1.LedgerOutputProofControlV1,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let control = vm_machine_types.ScriptSourcesControlV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
    context_cbor: validation_context_cbor,
    resolved_input_count: 0,
    resolved_inputs_accumulator: vm_witness.initial_resolution_accumulator(),
    signer_count: signer_frontier.count,
    signer_frontier_commitment: validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    resolved_item_peaks: validation_merkle_v1.empty_frontier(),
    stage: 5,
    source_count: 0,
    source_peaks: validation_merkle_v1.empty_frontier(),
    redeemer_count: 0,
    redeemer_peaks: validation_merkle_v1.empty_frontier(),
    replay_cursor: 0,
    replay_accumulator: vm_witness.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
    spend_index: 0,
    purpose_count: 0,
    purpose_peaks: validation_merkle_v1.empty_frontier(),
    output_cursor,
    output_count: output_frontier.count,
    output_peaks: output_frontier.peaks,
    output_total_count: output_frontier.count,
    receive_scan,
    source_total_count: 0,
    redeemer_total_count: 0,
    observer_scan: vm_witness.empty_observer_purpose_scan_control(),
    discovery: vm_witness.empty_script_discovery_control(),
    output_proof: Some(output_proof),
    pending_source_cbor: #"",
    mint_fold: vm_witness.empty_mint_fold_control(),
    resolution_schedule_hash: vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  }
  let work_witness_cbor = vm_witness.encode_script_sources_output_proof_witness(
    control,
    output_proof,
  )
  Pair(
    forced_pending_state(
      tx_id,
      tx,
      validation_context_cbor,
      validation_trace_v1.ScriptSources,
      program_counter,
      work_witness_cbor,
      validation_trace_v1.hash_ledger_delta(#"80"),
    ),
    work_witness_cbor,
  )
}

pub fn script_sources_step_fixture_with_resolved_items(
  tx: MidgardTransaction,
  resolved_input_count: Int,
  resolved_inputs_accumulator: ByteArray,
  stage: Int,
  source_frontier: validation_merkle_v1.BuiltFrontier,
  redeemer_frontier: validation_merkle_v1.BuiltFrontier,
  replay_cursor: Int,
  resolved_item_frontier: validation_merkle_v1.BuiltFrontier,
  replay_accumulator: ByteArray,
  replay_remaining_schedule_hash: ByteArray,
  spend_index: Int,
  purpose_frontier: validation_merkle_v1.BuiltFrontier,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  script_sources_output_step_fixture_with_resolved_items(
    tx,
    resolved_input_count,
    resolved_inputs_accumulator,
    stage,
    source_frontier,
    redeemer_frontier,
    replay_cursor,
    resolved_item_frontier,
    replay_accumulator,
    replay_remaining_schedule_hash,
    spend_index,
    purpose_frontier,
    0,
    validation_merkle_v1.build_frontier([]),
    0,
    vm_witness.empty_receive_purpose_scan_control(),
    vm_witness.empty_observer_purpose_scan_control(),
    program_counter,
  )
}

pub fn script_sources_receive_scan_step_fixture(
  tx: MidgardTransaction,
  purpose_frontier: validation_merkle_v1.BuiltFrontier,
  output_cursor: Int,
  output_frontier: validation_merkle_v1.BuiltFrontier,
  receive_scan: vm_machine_types.ReceivePurposeScanControlV1,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let accumulator = vm_witness.initial_resolution_accumulator()
  script_sources_output_step_fixture_with_resolved_items(
    tx,
    0,
    accumulator,
    7,
    validation_merkle_v1.build_frontier([]),
    validation_merkle_v1.build_frontier([]),
    0,
    validation_merkle_v1.build_frontier([]),
    accumulator,
    vm_witness.empty_resolution_schedule_hash(),
    0,
    purpose_frontier,
    output_cursor,
    output_frontier,
    output_frontier.count,
    receive_scan,
    vm_witness.empty_observer_purpose_scan_control(),
    program_counter,
  )
}

pub fn script_sources_step_fixture(
  tx: MidgardTransaction,
  resolved_input_count: Int,
  resolved_inputs_accumulator: ByteArray,
  stage: Int,
  source_frontier: validation_merkle_v1.BuiltFrontier,
  redeemer_frontier: validation_merkle_v1.BuiltFrontier,
  replay_cursor: Int,
  replay_accumulator: ByteArray,
  replay_remaining_schedule_hash: ByteArray,
  spend_index: Int,
  purpose_frontier: validation_merkle_v1.BuiltFrontier,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  script_sources_step_fixture_with_resolved_items(
    tx,
    resolved_input_count,
    resolved_inputs_accumulator,
    stage,
    source_frontier,
    redeemer_frontier,
    replay_cursor,
    validation_merkle_v1.build_frontier([]),
    replay_accumulator,
    replay_remaining_schedule_hash,
    spend_index,
    purpose_frontier,
    program_counter,
  )
}

/// Stage-1 state whose work witness carries a pending redeemer-item control
/// hash (the 31-item `encode_script_sources_redeemer_item_witness` form). This
/// is the shape `script_sources_stage_one` commits to between the
/// `TransactionRedeemerItemBeginWitness` step and the terminal item step.
pub fn script_sources_pending_redeemer_item_fixture(
  tx: MidgardTransaction,
  redeemer_frontier: validation_merkle_v1.BuiltFrontier,
  redeemer_total_count: Int,
  redeemer_item_control_hash: ByteArray,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let empty_frontier = validation_merkle_v1.build_frontier([])
  let control = vm_machine_types.ScriptSourcesControlV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
    context_cbor: validation_context_cbor,
    resolved_input_count: 0,
    resolved_inputs_accumulator: vm_witness.initial_resolution_accumulator(),
    signer_count: signer_frontier.count,
    signer_frontier_commitment: validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    resolved_item_peaks: validation_merkle_v1.empty_frontier(),
    stage: 1,
    source_count: 0,
    source_peaks: empty_frontier.peaks,
    redeemer_count: redeemer_frontier.count,
    redeemer_peaks: redeemer_frontier.peaks,
    replay_cursor: 0,
    replay_accumulator: vm_witness.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
    spend_index: 0,
    purpose_count: 0,
    purpose_peaks: empty_frontier.peaks,
    output_cursor: 0,
    output_count: 0,
    output_peaks: empty_frontier.peaks,
    output_total_count: 0,
    receive_scan: vm_witness.empty_receive_purpose_scan_control(),
    source_total_count: 0,
    redeemer_total_count,
    observer_scan: vm_witness.empty_observer_purpose_scan_control(),
    discovery: vm_witness.empty_script_discovery_control(),
    output_proof: None,
    pending_source_cbor: redeemer_item_control_hash,
    mint_fold: vm_witness.empty_mint_fold_control(),
    resolution_schedule_hash: vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  }
  let work_witness_cbor = vm_witness.encode_script_sources_redeemer_item_witness(
    control,
    redeemer_item_control_hash,
  )
  Pair(
    forced_pending_state(
      tx_id,
      tx,
      validation_context_cbor,
      validation_trace_v1.ScriptSources,
      program_counter,
      work_witness_cbor,
      validation_trace_v1.hash_ledger_delta(#"80"),
    ),
    work_witness_cbor,
  )
}

pub fn script_discovery_step_fixture_with_output_frontier(
  tx: MidgardTransaction,
  stage: Int,
  source_frontier: validation_merkle_v1.BuiltFrontier,
  redeemer_frontier: validation_merkle_v1.BuiltFrontier,
  purpose_frontier: validation_merkle_v1.BuiltFrontier,
  output_cursor: Int,
  output_frontier: validation_merkle_v1.BuiltFrontier,
  output_descriptor_frontier: validation_merkle_v1.BuiltFrontier,
  discovery: vm_machine_types.ScriptDiscoveryControlV1,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let base_control = vm_machine_types.ScriptSourcesControlV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
    context_cbor: validation_context_cbor,
    resolved_input_count: 0,
    resolved_inputs_accumulator: vm_witness.initial_resolution_accumulator(),
    resolved_item_peaks: validation_merkle_v1.empty_frontier(),
    signer_count: signer_frontier.count,
    signer_frontier_commitment: validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    stage,
    source_count: source_frontier.count,
    source_peaks: source_frontier.peaks,
    redeemer_count: redeemer_frontier.count,
    redeemer_peaks: redeemer_frontier.peaks,
    replay_cursor: 0,
    replay_accumulator: vm_witness.initial_resolution_accumulator(),
    replay_remaining_schedule_hash: vm_witness.empty_resolution_schedule_hash(),
    spend_index: 0,
    purpose_count: purpose_frontier.count,
    purpose_peaks: purpose_frontier.peaks,
    output_cursor,
    output_count: output_frontier.count,
    output_peaks: output_frontier.peaks,
    output_total_count: output_frontier.count,
    receive_scan: vm_machine_types.ReceivePurposeScanControlV1 {
      ..vm_witness.empty_receive_purpose_scan_control(),
      descriptor_peaks: output_descriptor_frontier.peaks,
    },
    source_total_count: source_frontier.count,
    redeemer_total_count: redeemer_frontier.count,
    observer_scan: vm_witness.empty_observer_purpose_scan_control(),
    discovery,
    output_proof: None,
    pending_source_cbor: #"",
    mint_fold: vm_machine_types.MintFoldControlV1 {
      ..vm_witness.empty_mint_fold_control(),
      policy_count: 0,
    },
    resolution_schedule_hash: vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  }
  let work_witness_cbor = vm_witness.encode_script_sources_discovery_witness(
    base_control,
    stage,
    discovery,
  )
  Pair(
    forced_pending_state(
      tx_id,
      tx,
      validation_context_cbor,
      validation_trace_v1.ScriptSources,
      program_counter,
      work_witness_cbor,
      validation_trace_v1.hash_ledger_delta(#"80"),
    ),
    work_witness_cbor,
  )
}

pub fn script_discovery_step_fixture(
  tx: MidgardTransaction,
  stage: Int,
  source_frontier: validation_merkle_v1.BuiltFrontier,
  redeemer_frontier: validation_merkle_v1.BuiltFrontier,
  purpose_frontier: validation_merkle_v1.BuiltFrontier,
  discovery: vm_machine_types.ScriptDiscoveryControlV1,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  script_discovery_step_fixture_with_output_frontier(
    tx,
    stage,
    source_frontier,
    redeemer_frontier,
    purpose_frontier,
    0,
    validation_merkle_v1.build_frontier([]),
    validation_merkle_v1.build_frontier([]),
    discovery,
    program_counter,
  )
}

pub fn native_scripts_step_fixture(
  tx: MidgardTransaction,
  source_frontier: validation_merkle_v1.BuiltFrontier,
  redeemer_frontier: validation_merkle_v1.BuiltFrontier,
  purpose_frontier: validation_merkle_v1.BuiltFrontier,
  execution_frontier: validation_merkle_v1.BuiltFrontier,
  execution_cursor: Int,
  language_bitmap: Int,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let control = vm_machine_types.NativeScriptsControlV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
    context_cbor: validation_context_cbor,
    resolved_input_count: 0,
    resolved_inputs_accumulator: vm_witness.initial_resolution_accumulator(),
    spend_input_count: 0,
    resolved_item_peaks: validation_merkle_v1.empty_frontier(),
    signer_count: signer_frontier.count,
    signer_frontier_commitment: validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    source_count: source_frontier.count,
    source_peaks: source_frontier.peaks,
    redeemer_count: redeemer_frontier.count,
    redeemer_peaks: redeemer_frontier.peaks,
    purpose_count: purpose_frontier.count,
    purpose_peaks: purpose_frontier.peaks,
    output_count: 0,
    output_peaks: validation_merkle_v1.empty_frontier(),
    output_descriptor_peaks: validation_merkle_v1.empty_frontier(),
    mint_count: 0,
    mint_peaks: validation_merkle_v1.empty_frontier(),
    execution_count: execution_frontier.count,
    execution_peaks: execution_frontier.peaks,
    execution_cursor,
    language_bitmap,
    resolution_schedule_hash: vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  }
  let work_witness_cbor = vm_witness.encode_native_scripts_control_v1(control)
  Pair(
    forced_pending_state(
      tx_id,
      tx,
      validation_context_cbor,
      validation_trace_v1.NativeScripts,
      program_counter,
      work_witness_cbor,
      validation_trace_v1.hash_ledger_delta(#"80"),
    ),
    work_witness_cbor,
  )
}

pub fn empty_native_control_fixture(
  tx: MidgardTransaction,
) -> vm_machine_types.NativeScriptsControlV1 {
  let proof_source = submitted_source(tx)
  let signer_frontier = signer_frontier_for_tx(tx)
  vm_machine_types.NativeScriptsControlV1 {
    compact_cbor: proof_source.compact_cbor,
    witness_set_compact_cbor: proof_source.witness_set_compact_cbor,
    field_preimage_lengths_cbor: proof_source.field_preimage_lengths_cbor,
    context_cbor: validation_context_cbor,
    resolved_input_count: 0,
    resolved_inputs_accumulator: vm_witness.initial_resolution_accumulator(),
    spend_input_count: 0,
    resolved_item_peaks: validation_merkle_v1.empty_frontier(),
    signer_count: signer_frontier.count,
    signer_frontier_commitment: validation_merkle_v1.frontier_commitment(
      signer_frontier.count,
      signer_frontier.peaks,
    ),
    source_count: 0,
    source_peaks: validation_merkle_v1.empty_frontier(),
    redeemer_count: 0,
    redeemer_peaks: validation_merkle_v1.empty_frontier(),
    purpose_count: 0,
    purpose_peaks: validation_merkle_v1.empty_frontier(),
    output_count: 0,
    output_peaks: validation_merkle_v1.empty_frontier(),
    output_descriptor_peaks: validation_merkle_v1.empty_frontier(),
    mint_count: 0,
    mint_peaks: validation_merkle_v1.empty_frontier(),
    execution_count: 0,
    execution_peaks: validation_merkle_v1.empty_frontier(),
    execution_cursor: 0,
    language_bitmap: 0,
    resolution_schedule_hash: vm_witness.transaction_resolution_schedule_hash(
      tx.body.inputs,
      tx.body.reference_inputs,
    ),
  }
}

pub fn cek_state_fixture(
  tx: MidgardTransaction,
  control: vm_machine_types.NativeScriptsControlV1,
  context_control_cbor: ByteArray,
  execution_cursor: Int,
  program_counter: Int,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let program_envelope_hash = if context_control_cbor == #"" {
    #""
  } else {
    h_b
  }
  let work_witness_cbor = vm_witness.encode_cek_witness_v1(
    control,
    context_control_cbor,
    execution_cursor,
    0,
    0,
    #"",
    0,
    0,
    program_envelope_hash,
  )
  Pair(
    forced_pending_state(
      tx_id,
      tx,
      validation_context_cbor,
      validation_trace_v1.Cek,
      program_counter,
      work_witness_cbor,
      validation_trace_v1.hash_ledger_delta(#"80"),
    ),
    work_witness_cbor,
  )
}

pub fn empty_cek_context_control_fixture(
  stage: Int,
) -> vm_machine_types.CekContextControlV1 {
  let redeemer_context = vm_witness.initial_cek_redeemer_context_control_v1()
  vm_machine_types.CekContextControlV1 {
    stage,
    language_tag: 3,
    program_term_root: h_a,
    program_envelope_hash: h_b,
    purpose_kind: 0,
    purpose_index: 0,
    script_hash: h_28_a,
    subject: #"00",
    redeemer_leaf: h_c,
    redeemer_context_control_hash: if stage == 0 {
      #""
    } else {
      vm_witness.hash_cek_redeemer_context_control_v1(redeemer_context)
    },
    execution_memory_limit: 10,
    execution_cpu_limit: 20,
    reference_items: cek_data_v1.empty_data_list_summary_v1(),
    spend_items: cek_data_v1.empty_data_list_summary_v1(),
    output_items: cek_data_v1.empty_data_list_summary_v1(),
    signer_items: cek_data_v1.empty_data_list_summary_v1(),
    observer_count: 0,
    observer_items: cek_data_v1.empty_data_pair_summary_v1(),
    previous_observer: #"",
    observer_summary: cek_data_v1.DataSummaryV1 {
      root: #"",
      cbor_length: 0,
      memory: 0,
    },
    mint_cursor: 0,
    current_mint_policy: #"",
    current_mint_assets: cek_data_v1.empty_data_pair_summary_v1(),
    mint_policies: cek_data_v1.empty_data_pair_summary_v1(),
    mint_summary: cek_data_v1.DataSummaryV1 {
      root: #"",
      cbor_length: 0,
      memory: 0,
    },
  }
}

pub fn verify_cek_context_transition_fixture(
  tx: MidgardTransaction,
  native_control: vm_machine_types.NativeScriptsControlV1,
  context_control: vm_machine_types.CekContextControlV1,
  successor_control: vm_machine_types.CekContextControlV1,
  auxiliary: vm_machine_types.ValidationAuxiliaryWitnessV1,
  program_counter: Int,
) -> Bool {
  let Pair(pre, work_witness_cbor) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(context_control),
    0,
    program_counter,
  )
  let Pair(post, _) = cek_state_fixture(
    tx,
    native_control,
    vm_witness.encode_cek_context_control_v1(successor_control),
    0,
    program_counter + 1,
  )
  vm_cek.verify_cek_observer_context_step(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    auxiliary,
    native_control,
    context_control,
    0,
    0,
    0,
    inline_field_door(),
  )
}

pub fn verify_cek_context_source_transition_fixture(
  transaction_id: ByteArray,
  transaction_commitment: ByteArray,
  native_control: vm_machine_types.NativeScriptsControlV1,
  context_control: vm_machine_types.CekContextControlV1,
  successor_control: vm_machine_types.CekContextControlV1,
  auxiliary: vm_machine_types.ValidationAuxiliaryWitnessV1,
  program_counter: Int,
) -> Bool {
  let base = empty_v1_transaction()
  let Pair(base_pre, work_witness_cbor) = cek_state_fixture(
    base,
    native_control,
    vm_witness.encode_cek_context_control_v1(context_control),
    0,
    program_counter,
  )
  let Pair(base_post, _) = cek_state_fixture(
    base,
    native_control,
    vm_witness.encode_cek_context_control_v1(successor_control),
    0,
    program_counter + 1,
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    ..base_pre,
    source_kind: validation_trace_v1.Normal,
    transaction_id,
    transaction_commitment,
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..base_post,
    source_kind: validation_trace_v1.Normal,
    transaction_id,
    transaction_commitment,
  }
  let original = vm_cek.verify_cek_observer_context_step(
    pre,
    vm_machine_types.ValidationOneStepWitnessV1 {
      work_witness_cbor,
      claimed_successor: post,
    },
    auxiliary,
    native_control,
    context_control,
    0,
    0,
    0,
    inline_field_door(),
  )
  if !original {
    False
  } else {
    let empty: Data = builtin.constr_data(0, [])
    let staged = context_chain.ContextStaged {
      bound: context_chain.ContextBound {
        prepared: empty,
        control: empty,
        auxiliary_hash: #"",
        transaction_id,
        source_kind: pre.source_kind,
      },
      context: context_control,
      native: bounded_context.native_facts(native_control),
      execution_cursor: 0,
    }
    let opening = if successor_control.stage == 6 {
      if context_control.observer_count == 0 {
        bounded_observer.EmptyObservers
      } else {
        bounded_observer.FinishedObservers
      }
    } else {
      bounded_observer.ObserverItem {
        count: successor_control.observer_count,
        hash: successor_control.previous_observer,
      }
    }
    bounded_observer.next_observer(staged, opening) == context_chain.ContinueContext {
      next: successor_control,
    }
  }
}

pub fn cek_context_native_control_fixture(
  tx: MidgardTransaction,
) -> vm_machine_types.NativeScriptsControlV1 {
  let singleton = validation_merkle_v1.build_frontier([h_a])
  vm_machine_types.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    purpose_count: 1,
    purpose_peaks: singleton.peaks,
    execution_count: 1,
    execution_peaks: singleton.peaks,
    execution_cursor: 1,
    language_bitmap: 1,
  }
}

pub fn test_power_of_two(exponent: Int) -> Int {
  if exponent == 0 {
    1
  } else {
    2 * test_power_of_two(exponent - 1)
  }
}

pub fn maximum_mint_subtree_root(start: Int, size: Int) -> ByteArray {
  if size == 1 {
    vm_shared.mint_asset_leaf_hash(
      h_28_a,
      bytearray.from_int_big_endian(start, 32),
      1,
    )
  } else {
    let half = size / 2
    validation_merkle_v1.hash_branch(
      maximum_mint_subtree_root(start, half),
      maximum_mint_subtree_root(start + half, half),
    )
  }
}

pub fn maximum_mint_last_asset_siblings(level: Int) -> List<ByteArray> {
  if level == 7 {
    []
  } else {
    let sibling_size = test_power_of_two(level)
    [
      maximum_mint_subtree_root(128 - 2 * sibling_size, sibling_size),
      ..maximum_mint_last_asset_siblings(level + 1)
    ]
  }
}

pub fn singleton_root(key: ByteArray, value: ByteArray) -> ByteArray {
  combine(suffix(blake2b_256(key), 0), blake2b_256(value))
}

pub fn canonical_step_fixture() -> Pair<
  validation_trace_v1.ValidationMachineStateV1,
  vm_machine_types.ValidationOneStepEvidenceV1,
> {
  let tx = empty_v1_transaction()
  let context_cbor = validation_context_cbor
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let proof_source = submitted_source(tx)
  // A field scan that has not yet counted the field's items carries the
  // `item_count == -1` sentinel; `verify_canonical_decode_empty` requires it.
  let work_witness_cbor = vm_witness.encode_transaction_field_scan_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    context_cbor,
    0,
    0,
    0,
    -1,
    0,
  )
  let pre = forced_pending_state(
    tx_id,
    tx,
    context_cbor,
    validation_trace_v1.CanonicalDecode,
    0,
    work_witness_cbor,
    h_a,
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 1,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      1,
      vm_witness.encode_transaction_field_scan_witness(
        proof_source.compact_cbor,
        proof_source.witness_set_compact_cbor,
        proof_source.field_preimage_lengths_cbor,
        context_cbor,
        1,
        0,
        0,
        -1,
        0,
      ),
    ),
  }
  Pair(
    pre,
    one_step_evidence(
      work_witness_cbor,
      post,
      vm_machine_types.NoAuxiliaryWitness,
    ),
  )
}

// Shared StaticLedgerRules fixture for the delta-root controls below. `valid`
// selects between a network-conformant transaction (whose true one-step is an
// advance) and a network-mismatched one (whose true one-step is a rejection).
// `claimed_delta_root` is the operator's immutable claimed delta commitment.
pub fn static_rules_delta_root_fixture(
  valid: Bool,
  claimed_delta_root: ByteArray,
) -> Pair<validation_trace_v1.ValidationMachineStateV1, ByteArray> {
  let base = empty_v1_transaction()
  let tx = if valid {
    base
  } else {
    MidgardTransaction {
      ..base,
      body: MidgardTransactionBody { ..base.body, network_id: 1 },
    }
  }
  let compact = midgard_transaction_to_compact(tx)
  let proof_source = submitted_source(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let work_witness_cbor = vm_witness.encode_static_rules_witness(
    proof_source.compact_cbor,
    proof_source.witness_set_compact_cbor,
    proof_source.field_preimage_lengths_cbor,
    validation_context_cbor,
  )
  Pair(
    forced_pending_state(
      tx_id,
      tx,
      validation_context_cbor,
      validation_trace_v1.StaticLedgerRules,
      2,
      work_witness_cbor,
      claimed_delta_root,
    ),
    work_witness_cbor,
  )
}
