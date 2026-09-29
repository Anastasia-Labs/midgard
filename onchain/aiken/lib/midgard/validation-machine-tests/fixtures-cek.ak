use aiken/builtin
use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/bounded_item_v1
use midgard/cek_data_v1
use midgard/cek_machine_v1
use midgard/cek_proof_v1
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_compact_v1, decode_native_tx_field_preimage_lengths_v1,
  decode_native_tx_witness_set_compact, encode_native_tx_body_compact,
  encode_native_tx_field_preimage_lengths_v1, native_tx_id_for_version,
  native_tx_proof_commitment_v1,
}
use midgard/fraud_proofs/native_tx/components.{encode_midgard_versioned_script}
use midgard/fraud_proofs/native_tx/preimages.{
  decode_midgard_tx_byte_list_preimage_cbor,
}
use midgard/fraud_proofs/native_tx/transaction.{midgard_transaction_to_compact}
use midgard/fraud_proofs/native_tx/types.{
  MidgardVersionedScript, NativeTxFieldPreimageLengthsV1, PlutusV3Script,
  native_tx_version_v1,
}
use midgard/ledger_output_commitment_v1
use midgard/native_tx_field_access_v1
use midgard/script_context_v1
use midgard/script_proof_v1
use midgard/validation_machine/canonical_decode as vm_canonical_decode
use midgard/validation_machine/machine_types as vm_machine_types
use midgard/validation_machine/one_step as vm_one_step
use midgard/validation_machine/witness as vm_witness
use midgard/validation_machine_tests/constants.{
  h_28_a, h_a, h_b, h_c, maximum_mint_terminal_compact_cbor,
  maximum_mint_terminal_encoded_length_before_item,
  maximum_mint_terminal_field_preimage_cbor,
  maximum_mint_terminal_field_preimage_lengths_cbor,
  maximum_mint_terminal_item_count, maximum_mint_terminal_item_index,
  maximum_mint_terminal_post_work_root, maximum_mint_terminal_pre_work_root,
  maximum_mint_terminal_terminal_chunk_index,
  maximum_mint_terminal_transaction_commitment,
  maximum_mint_terminal_transaction_id,
  maximum_mint_terminal_witness_set_compact_cbor,
  maximum_observer_terminal_compact_cbor,
  maximum_observer_terminal_encoded_length_before_item,
  maximum_observer_terminal_field_preimage_cbor,
  maximum_observer_terminal_field_preimage_lengths_cbor,
  maximum_observer_terminal_item_count, maximum_observer_terminal_item_index,
  maximum_observer_terminal_post_work_root,
  maximum_observer_terminal_pre_work_root,
  maximum_observer_terminal_terminal_chunk_index,
  maximum_observer_terminal_transaction_commitment,
  maximum_observer_terminal_transaction_id,
  maximum_observer_terminal_witness_set_compact_cbor,
  maximum_output_terminal_compact_cbor,
  maximum_output_terminal_encoded_length_before_item,
  maximum_output_terminal_field_preimage_cbor,
  maximum_output_terminal_field_preimage_lengths_cbor,
  maximum_output_terminal_item_count, maximum_output_terminal_item_index,
  maximum_output_terminal_post_work_root, maximum_output_terminal_pre_work_root,
  maximum_output_terminal_terminal_chunk_index,
  maximum_output_terminal_transaction_commitment,
  maximum_output_terminal_transaction_id,
  maximum_output_terminal_witness_set_compact_cbor,
  maximum_reference_input_terminal_compact_cbor,
  maximum_reference_input_terminal_encoded_length_before_item,
  maximum_reference_input_terminal_field_preimage_cbor,
  maximum_reference_input_terminal_field_preimage_lengths_cbor,
  maximum_reference_input_terminal_item_count,
  maximum_reference_input_terminal_item_index,
  maximum_reference_input_terminal_post_work_root,
  maximum_reference_input_terminal_pre_work_root,
  maximum_reference_input_terminal_terminal_chunk_index,
  maximum_reference_input_terminal_transaction_commitment,
  maximum_reference_input_terminal_transaction_id,
  maximum_reference_input_terminal_witness_set_compact_cbor,
  maximum_required_signer_terminal_compact_cbor,
  maximum_required_signer_terminal_encoded_length_before_item,
  maximum_required_signer_terminal_field_preimage_cbor,
  maximum_required_signer_terminal_field_preimage_lengths_cbor,
  maximum_required_signer_terminal_item_count,
  maximum_required_signer_terminal_item_index,
  maximum_required_signer_terminal_post_work_root,
  maximum_required_signer_terminal_pre_work_root,
  maximum_required_signer_terminal_terminal_chunk_index,
  maximum_required_signer_terminal_transaction_commitment,
  maximum_required_signer_terminal_transaction_id,
  maximum_required_signer_terminal_witness_set_compact_cbor,
  maximum_spend_input_terminal_compact_cbor,
  maximum_spend_input_terminal_encoded_length_before_item,
  maximum_spend_input_terminal_field_preimage_cbor,
  maximum_spend_input_terminal_field_preimage_lengths_cbor,
  maximum_spend_input_terminal_item_count,
  maximum_spend_input_terminal_item_index,
  maximum_spend_input_terminal_post_work_root,
  maximum_spend_input_terminal_pre_work_root,
  maximum_spend_input_terminal_terminal_chunk_index,
  maximum_spend_input_terminal_transaction_commitment,
  maximum_spend_input_terminal_transaction_id,
  maximum_spend_input_terminal_witness_set_compact_cbor, validation_context_cbor,
  zero,
}
use midgard/validation_machine_tests/fixture_types.{
  CekSelectionFixtureV1, MaximumFieldTerminalFixtureV1,
}
use midgard/validation_machine_tests/fixtures_phase_a.{
  cek_state_fixture, empty_native_control_fixture, empty_v1_transaction,
  exact_rejection_post, forced_pending_state, inline_field_door,
  one_step_evidence, singleton_field_item_chunk_proof,
}
use midgard/validation_merkle_v1
use midgard/validation_trace_v1

pub fn maximum_program_envelope_selection_fixture() -> CekSelectionFixtureV1 {
  let tx = empty_v1_transaction()
  let maximum_program_envelope = cek_proof_v1.encode_program_envelope_v1(
    1,
    1,
    0,
    h_a,
    cek_proof_v1.max_program_node_count,
    cek_proof_v1.max_program_material_byte_length,
  )
  let maximum_program_envelope_hash = cek_proof_v1.hash_program_envelope_v1(
    1,
    1,
    0,
    h_a,
    cek_proof_v1.max_program_node_count,
    cek_proof_v1.max_program_material_byte_length,
  )
  let script = MidgardVersionedScript {
    language: PlutusV3Script,
    script_bytes: maximum_program_envelope,
  }
  let script_cbor = encode_midgard_versioned_script(script)
  let first_chunk_proof = singleton_field_item_chunk_proof(6, script_cbor)
  let script_item_commitment = bounded_item_v1.from_bytes(6, 0, script_cbor)
  let script_hash = script_proof_v1.versioned_script_hash(script)
  let subject = #"00"
  let source_leaf = script_proof_v1.source_descriptor_leaf_hash(
    0,
    #"00",
    3,
    script_hash,
    bytearray.length(script_cbor),
    script_item_commitment,
  )
  let purpose_leaf = script_proof_v1.purpose_leaf_hash(
    0,
    0,
    script_hash,
    subject,
  )
  let execution_leaf = script_proof_v1.execution_leaf_hash(
    3,
    purpose_leaf,
    source_leaf,
    h_c,
  )
  let source_frontier = validation_merkle_v1.build_frontier([source_leaf])
  let purpose_frontier = validation_merkle_v1.build_frontier([purpose_leaf])
  let execution_frontier = validation_merkle_v1.build_frontier([execution_leaf])
  let control = vm_machine_types.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    source_count: 1,
    source_peaks: source_frontier.peaks,
    purpose_count: 1,
    purpose_peaks: purpose_frontier.peaks,
    execution_count: 1,
    execution_peaks: execution_frontier.peaks,
    execution_cursor: 1,
    language_bitmap: 1,
  }
  let context_control = vm_machine_types.CekContextControlV1 {
    stage: 0,
    language_tag: 3,
    program_term_root: h_a,
    program_envelope_hash: maximum_program_envelope_hash,
    purpose_kind: 0,
    purpose_index: 0,
    script_hash,
    subject,
    redeemer_leaf: h_c,
    redeemer_context_control_hash: #"",
    execution_memory_limit: 0,
    execution_cpu_limit: 0,
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
  let Pair(pre, work_witness_cbor) = cek_state_fixture(tx, control, #"", 0, 40)
  let Pair(post_base, _) = cek_state_fixture(
    tx,
    control,
    vm_witness.encode_cek_context_control_v1(context_control),
    0,
    41,
  )
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..post_base,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      41,
      vm_witness.encode_cek_witness_v1(
        control,
        vm_witness.encode_cek_context_control_v1(context_control),
        0,
        0,
        0,
        #"",
        0,
        0,
        maximum_program_envelope_hash,
      ),
    ),
  }
  let tampered_post = validation_trace_v1.ValidationMachineStateV1 {
    ..post,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      41,
      vm_witness.encode_cek_witness_v1(
        control,
        vm_witness.encode_cek_context_control_v1(
          vm_machine_types.CekContextControlV1 {
            ..context_control,
            program_envelope_hash: h_b,
          },
        ),
        0,
        0,
        0,
        #"",
        0,
        0,
        h_b,
      ),
    ),
  }
  let evidence = one_step_evidence(
    work_witness_cbor,
    post,
    vm_machine_types.NativeExecutionScanWitness {
      execution_index: 0,
      language_tag: 3,
      purpose_kind: 0,
      purpose_index: 0,
      script_hash,
      subject,
      purpose_siblings: [],
      source_index: 0,
      origin_kind: 0,
      source_key: #"00",
      script_total_length: bytearray.length(script_cbor),
      script_item_commitment,
      source_siblings: [],
      redeemer_leaf: h_c,
      execution_siblings: [],
      first_chunk_proof,
    },
  )
  expect and {
      bytearray.length(maximum_program_envelope) == 50,
      maximum_program_envelope_hash == #"6f25a78e8bbffffce7c062e1281984afa4b5fae7a479db7f191c56ebe4fd1260",
    }
  CekSelectionFixtureV1 { pre, evidence, tampered_post }
}

pub fn maximum_program_envelope_selection_is_exact() -> Bool {
  let f = maximum_program_envelope_selection_fixture()
  and {
    bytearray.length(cbor.serialise(f.evidence)) < 16_384,
    vm_one_step.verify_one_step_evidence(f.pre, f.evidence, inline_field_door()),
    !vm_one_step.verify_one_step_evidence(
      f.pre,
      vm_machine_types.ValidationOneStepEvidenceV1 {
        transition: vm_machine_types.ValidationOneStepWitnessV1 {
          ..f.evidence.transition,
          claimed_successor: f.tampered_post,
        },
        auxiliary: f.evidence.auxiliary,
      },
      inline_field_door(),
    ),
  }
}

pub fn exact_output_descriptor_fixture(
  output_index: Int,
  output_cbor: ByteArray,
) -> ByteArray {
  expect Some(cardano_tx_out) = script_context_v1.tx_out_summary_v1(
    output_cbor,
    False,
  )
  expect Some(midgard_tx_out) = script_context_v1.tx_out_summary_v1(
    output_cbor,
    True,
  )
  expect Some(cardano_spend_datum) = script_context_v1.spend_datum_summary_v1(
    output_cbor,
  )
  ledger_output_commitment_v1.encode(
    ledger_output_commitment_v1.LedgerOutputCommitmentV1 {
      version: ledger_output_commitment_v1.version,
      output_index,
      total_length: bytearray.length(output_cbor),
      item_commitment: bounded_item_v1.from_bytes(2, output_index, output_cbor),
      address: #"60" |> bytearray.concat(h_28_a),
      lovelace: 1,
      asset_count: 0,
      asset_frontier_commitment: validation_merkle_v1.frontier_commitment(0, []),
      cardano_value_size: 0,
      reference_script_language: -1,
      reference_script_hash: #"",
      reference_script_total_length: 0,
      reference_script_item_commitment: #"",
      cardano_tx_out,
      midgard_tx_out,
      cardano_spend_datum,
    },
  )
}

/// The §4 slot a field index names, read out of the compact structures the
/// fixture pins.
///
/// §4 removed field-index domain separation, so which of the nine 32-byte hashes
/// a preimage is *supposed* to match is positional and nothing else. Spelling
/// the map here — rather than reaching for the machine's private reader — is what
/// lets these fixtures state "this preimage commits to *this* slot" as a
/// checkable fact.
pub fn terminal_fixture_field_slot_commitment(
  fixture: MaximumFieldTerminalFixtureV1,
) -> ByteArray {
  let compact = decode_native_tx_compact_v1(fixture.compact_cbor)
  let witness_set = decode_native_tx_witness_set_compact(
    fixture.witness_set_compact_cbor,
  )
  when fixture.field_index is {
    0 -> compact.body.spend_inputs_hash
    1 -> compact.body.reference_inputs_hash
    2 -> compact.body.outputs_hash
    3 -> compact.body.required_observers_hash
    4 -> compact.body.required_signers_hash
    5 -> compact.body.mint_hash
    6 -> witness_set.script_tx_wits_hash
    7 -> witness_set.addr_tx_wits_hash
    _ -> witness_set.redeemer_tx_wits_hash
  }
}

/// §5.1's items of the fixture's field preimage, through the one uniform reader
/// all nine fields share since #567.
pub fn terminal_fixture_items(
  fixture: MaximumFieldTerminalFixtureV1,
) -> List<ByteArray> {
  decode_midgard_tx_byte_list_preimage_cbor(fixture.field_preimage_cbor)
}

/// The terminal item's payload bytes — what the retired
/// `ChunkProofV1.chunk` carried at this boundary, now read out of the preimage.
pub fn terminal_fixture_item(
  fixture: MaximumFieldTerminalFixtureV1,
) -> ByteArray {
  expect Some(item) = list.last(terminal_fixture_items(fixture))
  item
}

pub fn terminal_fixture_carriage(
  fixture: MaximumFieldTerminalFixtureV1,
) -> native_tx_field_access_v1.FieldCarriageV1 {
  native_tx_field_access_v1.Inline { preimage: fixture.field_preimage_cbor }
}

pub fn verify_maximum_field_terminal_fixture_v1(
  fixture: MaximumFieldTerminalFixtureV1,
) -> Bool {
  let derived_source_commitment = native_tx_proof_commitment_v1(
    fixture.compact_cbor,
    fixture.witness_set_compact_cbor,
    fixture.field_preimage_lengths_cbor,
  )
  let terminal_item = terminal_fixture_item(fixture)
  let work_witness_cbor = vm_witness.encode_transaction_field_scan_witness(
    fixture.compact_cbor,
    fixture.witness_set_compact_cbor,
    fixture.field_preimage_lengths_cbor,
    validation_context_cbor,
    fixture.field_index,
    fixture.item_index,
    fixture.terminal_chunk_index,
    fixture.item_count,
    fixture.encoded_length_before_item,
  )
  let successor_work_witness_cbor = vm_witness.encode_transaction_field_scan_witness(
    fixture.compact_cbor,
    fixture.witness_set_compact_cbor,
    fixture.field_preimage_lengths_cbor,
    validation_context_cbor,
    fixture.field_index + 1,
    0,
    0,
    -1,
    0,
  )
  let pre = terminal_fixture_pre_state(fixture, work_witness_cbor)
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 41,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      41,
      successor_work_witness_cbor,
    ),
  }
  let transition = vm_machine_types.ValidationOneStepWitnessV1 {
    work_witness_cbor,
    claimed_successor: post,
  }
  // A *different* transaction body: the compact structure with one field's
  // declared preimage length bumped. Under the counted scheme this fed
  // `verify_midgard_transaction_field_chunk_v1`'s substitution controls; under §4
  // it is the door's own binding that refuses it, and the two statements below
  // say so without needing that retired reader.
  let alternate_length_pre = terminal_fixture_alternate_length_pre_state(
    fixture,
  )
  and {
    derived_source_commitment == fixture.transaction_commitment,
    // The published preimage is the committed one, for the slot this fixture
    // names. This is the whole authentication the door performs, stated once.
    native_tx_field_access_v1.field_commitment(fixture.field_preimage_cbor) == terminal_fixture_field_slot_commitment(
      fixture,
    ),
    pre.work_root == fixture.expected_pre_work_root,
    post.work_root == fixture.expected_post_work_root,
    fixture.item_index + 1 == fixture.item_count,
    list.length(terminal_fixture_items(fixture)) == fixture.item_count,
    fixture.terminal_chunk_index + 1 == bounded_item_v1.chunk_count(
      bytearray.length(terminal_item),
    ),
    // The terminal fold authenticates.
    vm_canonical_decode.verify_canonical_decode_chunk_semantics_v1(
      pre,
      transition,
      inline_field_door(),
      fixture.field_index,
      fixture.item_index,
      terminal_fixture_carriage(fixture),
    ),
    // Evidence naming a neighbouring slot does not authenticate this one: §4
    // removed domain separation and the machine re-establishes it by checking the
    // index against the control's.
    !vm_canonical_decode.verify_canonical_decode_chunk_semantics_v1(
      pre,
      transition,
      inline_field_door(),
      fixture.field_index + 1,
      fixture.item_index,
      terminal_fixture_carriage(fixture),
    ),
    // Nor does evidence naming a different item of the same field.
    !vm_canonical_decode.verify_canonical_decode_chunk_semantics_v1(
      pre,
      transition,
      inline_field_door(),
      fixture.field_index,
      fixture.item_index - 1,
      terminal_fixture_carriage(fixture),
    ),
    // A declared §5.1 length that disagrees with the authenticated preimage's own
    // length renders `reject_field_preimage_size` rather than authenticating —
    // one of the two `CanonicalDecode` verdicts the door leaves reachable (the
    // other being `reject_invalid_field_type`; committed bytes that are not a
    // §5.1 envelope at all are #596's).
    vm_canonical_decode.verify_canonical_decode_chunk_semantics_v1(
      alternate_length_pre,
      vm_machine_types.ValidationOneStepWitnessV1 {
        work_witness_cbor: terminal_fixture_alternate_length_work_witness(
          fixture,
        ),
        claimed_successor: exact_rejection_post(
          alternate_length_pre,
          #"455f4649454c445f505245494d4147455f53495a45",
        ),
      },
      inline_field_door(),
      fixture.field_index,
      fixture.item_index,
      terminal_fixture_carriage(fixture),
    ),
  }
}

pub fn terminal_fixture_pre_state(
  fixture: MaximumFieldTerminalFixtureV1,
  work_witness_cbor: ByteArray,
) -> validation_trace_v1.ValidationMachineStateV1 {
  validation_trace_v1.ValidationMachineStateV1 {
    machine_version: validation_trace_v1.machine_version,
    event_key_hash: fixture.transaction_id,
    transaction_id: fixture.transaction_id,
    transaction_commitment: fixture.transaction_commitment,
    validation_context_hash: validation_trace_v1.hash_validation_context(
      validation_context_cbor,
    ),
    // These cross-language vectors commit the normal native wire format.
    source_kind: validation_trace_v1.Normal,
    prior_ledger_root: fixture.transaction_commitment,
    phase: validation_trace_v1.CanonicalDecode,
    program_counter: 40,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.CanonicalDecode,
      40,
      work_witness_cbor,
    ),
    execution_cpu: 0,
    execution_memory: 0,
    verdict: validation_trace_v1.Pending,
    rejection_code_hash: zero,
    ledger_delta_root: validation_trace_v1.hash_ledger_delta(#"80"),
  }
}

/// The same transaction with the *declared* §5.1 length of this fixture's field
/// bumped by one, and the compact structures re-derived around it.
///
/// The two committed structures disagree, which §4's flat hash cannot collapse:
/// `NativeTxFieldPreimageLengthsV1` is its own commitment, so a declared length
/// that the authenticated preimage's own length contradicts is exactly the case
/// `reject_field_preimage_size` exists for.
pub fn terminal_fixture_alternate_length_cbor(
  fixture: MaximumFieldTerminalFixtureV1,
) -> ByteArray {
  let lengths = decode_native_tx_field_preimage_lengths_v1(
    fixture.field_preimage_lengths_cbor,
  )
  encode_native_tx_field_preimage_lengths_v1(
    when fixture.field_index is {
      0 ->
        NativeTxFieldPreimageLengthsV1 {
          ..lengths,
          spend_inputs: lengths.spend_inputs + 1,
        }
      1 ->
        NativeTxFieldPreimageLengthsV1 {
          ..lengths,
          reference_inputs: lengths.reference_inputs + 1,
        }
      2 ->
        NativeTxFieldPreimageLengthsV1 {
          ..lengths,
          outputs: lengths.outputs + 1,
        }
      3 ->
        NativeTxFieldPreimageLengthsV1 {
          ..lengths,
          required_observers: lengths.required_observers + 1,
        }
      4 ->
        NativeTxFieldPreimageLengthsV1 {
          ..lengths,
          required_signers: lengths.required_signers + 1,
        }
      _ -> NativeTxFieldPreimageLengthsV1 { ..lengths, mint: lengths.mint + 1 }
    },
  )
}

pub fn terminal_fixture_alternate_length_work_witness(
  fixture: MaximumFieldTerminalFixtureV1,
) -> ByteArray {
  vm_witness.encode_transaction_field_scan_witness(
    fixture.compact_cbor,
    fixture.witness_set_compact_cbor,
    terminal_fixture_alternate_length_cbor(fixture),
    validation_context_cbor,
    fixture.field_index,
    fixture.item_index,
    fixture.terminal_chunk_index,
    fixture.item_count,
    fixture.encoded_length_before_item,
  )
}

pub fn terminal_fixture_alternate_length_pre_state(
  fixture: MaximumFieldTerminalFixtureV1,
) -> validation_trace_v1.ValidationMachineStateV1 {
  let alternate_length_cbor = terminal_fixture_alternate_length_cbor(fixture)
  let work_witness_cbor = terminal_fixture_alternate_length_work_witness(
    fixture,
  )
  validation_trace_v1.ValidationMachineStateV1 {
    ..terminal_fixture_pre_state(fixture, work_witness_cbor),
    transaction_commitment: native_tx_proof_commitment_v1(
      fixture.compact_cbor,
      fixture.witness_set_compact_cbor,
      alternate_length_cbor,
    ),
    prior_ledger_root: fixture.transaction_commitment,
  }
}

pub fn maximum_spend_input_field_terminal_fixture_v1() -> MaximumFieldTerminalFixtureV1 {
  MaximumFieldTerminalFixtureV1 {
    transaction_id: maximum_spend_input_terminal_transaction_id,
    transaction_commitment: maximum_spend_input_terminal_transaction_commitment,
    compact_cbor: maximum_spend_input_terminal_compact_cbor,
    witness_set_compact_cbor: maximum_spend_input_terminal_witness_set_compact_cbor,
    field_preimage_lengths_cbor: maximum_spend_input_terminal_field_preimage_lengths_cbor,
    field_preimage_cbor: maximum_spend_input_terminal_field_preimage_cbor,
    field_index: 0,
    item_count: maximum_spend_input_terminal_item_count,
    item_index: maximum_spend_input_terminal_item_index,
    terminal_chunk_index: maximum_spend_input_terminal_terminal_chunk_index,
    encoded_length_before_item: maximum_spend_input_terminal_encoded_length_before_item,
    expected_pre_work_root: maximum_spend_input_terminal_pre_work_root,
    expected_post_work_root: maximum_spend_input_terminal_post_work_root,
  }
}

pub fn maximum_reference_input_field_terminal_fixture_v1() -> MaximumFieldTerminalFixtureV1 {
  MaximumFieldTerminalFixtureV1 {
    transaction_id: maximum_reference_input_terminal_transaction_id,
    transaction_commitment: maximum_reference_input_terminal_transaction_commitment,
    compact_cbor: maximum_reference_input_terminal_compact_cbor,
    witness_set_compact_cbor: maximum_reference_input_terminal_witness_set_compact_cbor,
    field_preimage_lengths_cbor: maximum_reference_input_terminal_field_preimage_lengths_cbor,
    field_preimage_cbor: maximum_reference_input_terminal_field_preimage_cbor,
    field_index: 1,
    item_count: maximum_reference_input_terminal_item_count,
    item_index: maximum_reference_input_terminal_item_index,
    terminal_chunk_index: maximum_reference_input_terminal_terminal_chunk_index,
    encoded_length_before_item: maximum_reference_input_terminal_encoded_length_before_item,
    expected_pre_work_root: maximum_reference_input_terminal_pre_work_root,
    expected_post_work_root: maximum_reference_input_terminal_post_work_root,
  }
}

pub fn maximum_observer_field_terminal_fixture_v1() -> MaximumFieldTerminalFixtureV1 {
  MaximumFieldTerminalFixtureV1 {
    transaction_id: maximum_observer_terminal_transaction_id,
    transaction_commitment: maximum_observer_terminal_transaction_commitment,
    compact_cbor: maximum_observer_terminal_compact_cbor,
    witness_set_compact_cbor: maximum_observer_terminal_witness_set_compact_cbor,
    field_preimage_lengths_cbor: maximum_observer_terminal_field_preimage_lengths_cbor,
    field_preimage_cbor: maximum_observer_terminal_field_preimage_cbor,
    field_index: 3,
    item_count: maximum_observer_terminal_item_count,
    item_index: maximum_observer_terminal_item_index,
    terminal_chunk_index: maximum_observer_terminal_terminal_chunk_index,
    encoded_length_before_item: maximum_observer_terminal_encoded_length_before_item,
    expected_pre_work_root: maximum_observer_terminal_pre_work_root,
    expected_post_work_root: maximum_observer_terminal_post_work_root,
  }
}

pub fn maximum_output_field_terminal_fixture_v1() -> MaximumFieldTerminalFixtureV1 {
  MaximumFieldTerminalFixtureV1 {
    transaction_id: maximum_output_terminal_transaction_id,
    transaction_commitment: maximum_output_terminal_transaction_commitment,
    compact_cbor: maximum_output_terminal_compact_cbor,
    witness_set_compact_cbor: maximum_output_terminal_witness_set_compact_cbor,
    field_preimage_lengths_cbor: maximum_output_terminal_field_preimage_lengths_cbor,
    field_preimage_cbor: maximum_output_terminal_field_preimage_cbor,
    field_index: 2,
    item_count: maximum_output_terminal_item_count,
    item_index: maximum_output_terminal_item_index,
    terminal_chunk_index: maximum_output_terminal_terminal_chunk_index,
    encoded_length_before_item: maximum_output_terminal_encoded_length_before_item,
    expected_pre_work_root: maximum_output_terminal_pre_work_root,
    expected_post_work_root: maximum_output_terminal_post_work_root,
  }
}

pub fn maximum_required_signer_field_terminal_fixture_v1() -> MaximumFieldTerminalFixtureV1 {
  MaximumFieldTerminalFixtureV1 {
    transaction_id: maximum_required_signer_terminal_transaction_id,
    transaction_commitment: maximum_required_signer_terminal_transaction_commitment,
    compact_cbor: maximum_required_signer_terminal_compact_cbor,
    witness_set_compact_cbor: maximum_required_signer_terminal_witness_set_compact_cbor,
    field_preimage_lengths_cbor: maximum_required_signer_terminal_field_preimage_lengths_cbor,
    field_preimage_cbor: maximum_required_signer_terminal_field_preimage_cbor,
    field_index: 4,
    item_count: maximum_required_signer_terminal_item_count,
    item_index: maximum_required_signer_terminal_item_index,
    terminal_chunk_index: maximum_required_signer_terminal_terminal_chunk_index,
    encoded_length_before_item: maximum_required_signer_terminal_encoded_length_before_item,
    expected_pre_work_root: maximum_required_signer_terminal_pre_work_root,
    expected_post_work_root: maximum_required_signer_terminal_post_work_root,
  }
}

pub fn maximum_mint_field_terminal_fixture_v1() -> MaximumFieldTerminalFixtureV1 {
  MaximumFieldTerminalFixtureV1 {
    transaction_id: maximum_mint_terminal_transaction_id,
    transaction_commitment: maximum_mint_terminal_transaction_commitment,
    compact_cbor: maximum_mint_terminal_compact_cbor,
    witness_set_compact_cbor: maximum_mint_terminal_witness_set_compact_cbor,
    field_preimage_lengths_cbor: maximum_mint_terminal_field_preimage_lengths_cbor,
    field_preimage_cbor: maximum_mint_terminal_field_preimage_cbor,
    field_index: 5,
    item_count: maximum_mint_terminal_item_count,
    item_index: maximum_mint_terminal_item_index,
    terminal_chunk_index: maximum_mint_terminal_terminal_chunk_index,
    encoded_length_before_item: maximum_mint_terminal_encoded_length_before_item,
    expected_pre_work_root: maximum_mint_terminal_pre_work_root,
    expected_post_work_root: maximum_mint_terminal_post_work_root,
  }
}

/// The mint boundary's terminal fold, plus the four Merkle attack classes the
/// counted scheme needed per-item openings to refute.
///
/// **How the four controls changed shape, and why they are stronger.** Under the
/// counted scheme each control forged an `ItemProofV1` — a substituted item
/// commitment, a reordered pair of leaves, an omitted leaf, a duplicated one —
/// and asserted that `bounded_collection_v1.verify_item` refused it against the
/// field root. §4 replaced that root with a flat `blake2b_256` over the field's
/// whole §5.1 preimage, so each attack is now a claim about *bytes*: it produces
/// a different preimage, whose commitment is not the committed one, and §8's
/// door refuses it before any rule runs. The controls therefore construct the
/// perturbed preimage — from the authenticated one, through §5.1's own reader
/// and encoder, so no attack is hand-spelled — and measure that its commitment
/// differs from the field's. That is a statement about every one of the field's
/// 130 items at once, where the counted controls only ever bound the two leaves
/// their siblings reached.
pub fn verify_maximum_mint_field_terminal_fixture_v1(
  fixture: MaximumFieldTerminalFixtureV1,
) -> Bool {
  let terminal = terminal_fixture_item(fixture)
  let chunk_length = bytearray.length(terminal)
  let policy_mutation = replace_byte_v1(terminal, 3, #"fe")
  let asset_name_mutation = replace_byte_v1(terminal, 33, #"4e")
  let sign_mutation = replace_byte_v1(terminal, 42, #"20")

  and {
    verify_maximum_field_terminal_fixture_v1(fixture),
    chunk_length == 43,
    builtin.slice_bytearray(3, 1, terminal) == #"ff",
    builtin.slice_bytearray(33, 1, terminal) == #"4d",
    builtin.slice_bytearray(42, 1, terminal) == #"01",
    verify_terminal_substitution_control_v1(fixture, policy_mutation),
    verify_terminal_substitution_control_v1(fixture, asset_name_mutation),
    verify_terminal_substitution_control_v1(fixture, sign_mutation),
    verify_terminal_reorder_control_v1(fixture),
    verify_terminal_omission_control_v1(fixture),
    verify_terminal_duplicate_control_v1(fixture),
  }
}

pub fn replace_byte_v1(
  bytes: ByteArray,
  offset: Int,
  replacement: ByteArray,
) -> ByteArray {
  expect bytearray.length(replacement) == 1
  expect offset >= 0
  expect offset < bytearray.length(bytes)
  builtin.slice_bytearray(0, offset, bytes)
    |> bytearray.concat(replacement)
    |> bytearray.concat(
        builtin.slice_bytearray(
          offset + 1,
          bytearray.length(bytes) - offset - 1,
          bytes,
        ),
      )
}

/// `items` with its last element replaced by `replacement`.
pub fn with_last_item_replaced(
  items: List<ByteArray>,
  replacement: ByteArray,
) -> List<ByteArray> {
  let count = list.length(items)
  list.concat(list.take(items, count - 1), [replacement])
}

/// The commitment of a perturbed field preimage, re-encoded through §5.1 so the
/// header count and every wrapper follow from the item list rather than from a
/// hand-spelled byte string.
pub fn perturbed_field_commitment(items: List<ByteArray>) -> ByteArray {
  native_tx_field_access_v1.field_commitment(
    native_tx_field_access_v1.encode_field_preimage(items),
  )
}

/// Substitution: one item's bytes replaced. The committed preimage is not the
/// substituted one, so the door never opens it.
pub fn verify_terminal_substitution_control_v1(
  fixture: MaximumFieldTerminalFixtureV1,
  alternate_item: ByteArray,
) -> Bool {
  let items = terminal_fixture_items(fixture)
  and {
    alternate_item != terminal_fixture_item(fixture),
    perturbed_field_commitment(with_last_item_replaced(items, alternate_item)) != terminal_fixture_field_slot_commitment(
      fixture,
    ),
  }
}

/// Reorder: the last two items swapped. The counted scheme needed this because a
/// per-item opening says nothing about position beyond the leaf it hashes; a
/// flat preimage hash is order-sensitive by construction, and this measures it
/// on the field the fixture actually pins.
pub fn verify_terminal_reorder_control_v1(
  fixture: MaximumFieldTerminalFixtureV1,
) -> Bool {
  let items = terminal_fixture_items(fixture)
  let count = list.length(items)
  expect Some(terminal) = list.last(items)
  expect Some(penultimate) = list.at(items, count - 2)
  let reordered = list.concat(
    list.take(items, count - 2),
    [terminal, penultimate],
  )
  and {
    terminal != penultimate,
    perturbed_field_commitment(reordered) != terminal_fixture_field_slot_commitment(
      fixture,
    ),
  }
}

/// Omission: the terminal item dropped, with §5.1's header count following it
/// down. Both halves of the envelope move, which is why the encoder rather than
/// a byte edit builds it.
pub fn verify_terminal_omission_control_v1(
  fixture: MaximumFieldTerminalFixtureV1,
) -> Bool {
  let items = terminal_fixture_items(fixture)
  let count = list.length(items)
  let omitted = list.take(items, count - 1)
  and {
    list.length(omitted) + 1 == fixture.item_count,
    perturbed_field_commitment(omitted) != terminal_fixture_field_slot_commitment(
      fixture,
    ),
  }
}

/// Duplication: the terminal item appended a second time. The retired control
/// forged a leaf at `item_count`; the flat commitment refuses the whole preimage.
pub fn verify_terminal_duplicate_control_v1(
  fixture: MaximumFieldTerminalFixtureV1,
) -> Bool {
  let items = terminal_fixture_items(fixture)
  let duplicated = list.concat(items, [terminal_fixture_item(fixture)])
  and {
    list.length(duplicated) == fixture.item_count + 1,
    perturbed_field_commitment(duplicated) != terminal_fixture_field_slot_commitment(
      fixture,
    ),
  }
}

/// One honest active core step (a `ComputeApplication` over two leaf terms)
/// and its exact successor. Shared by the replay test below and the R5 item 1
/// route-agreement controls at the end of this module.
pub fn cek_active_core_step_fixture() -> Pair<
  validation_trace_v1.ValidationMachineStateV1,
  vm_machine_types.ValidationOneStepEvidenceV1,
> {
  let tx = empty_v1_transaction()
  let purpose_frontier = validation_merkle_v1.build_frontier([h_a])
  let execution_frontier = validation_merkle_v1.build_frontier([h_b])
  let control = vm_machine_types.NativeScriptsControlV1 {
    ..empty_native_control_fixture(tx),
    purpose_count: 1,
    purpose_peaks: purpose_frontier.peaks,
    execution_count: 1,
    execution_peaks: execution_frontier.peaks,
    execution_cursor: 1,
    language_bitmap: 1,
  }
  let application_root = cek_proof_v1.hash_application_term_v1(h_a, h_b)
  let machine_pre = cek_machine_v1.MachineStateV1 {
    mode: cek_machine_v1.mode_compute,
    execution_index: 0,
    focus_root: application_root,
    environment_root: cek_proof_v1.empty_environment_root_v1,
    continuation_root: cek_proof_v1.empty_continuation_root_v1,
    auxiliary: 0,
    cpu: 10,
    memory: 11,
  }
  let continuation_root = cek_proof_v1.hash_apply_argument_continuation_v1(
    h_b,
    cek_proof_v1.empty_environment_root_v1,
    cek_proof_v1.empty_continuation_root_v1,
  )
  let machine_post = cek_machine_v1.MachineStateV1 {
    ..machine_pre,
    focus_root: h_a,
    continuation_root,
    cpu: 16010,
    memory: 111,
  }
  let active_pre_hash = cek_proof_v1.hash_machine_state_v1(
    machine_pre.mode,
    machine_pre.execution_index,
    machine_pre.focus_root,
    machine_pre.environment_root,
    machine_pre.continuation_root,
    machine_pre.auxiliary,
    machine_pre.cpu,
    machine_pre.memory,
  )
  let active_post_hash = cek_proof_v1.hash_machine_state_v1(
    machine_post.mode,
    machine_post.execution_index,
    machine_post.focus_root,
    machine_post.environment_root,
    machine_post.continuation_root,
    machine_post.auxiliary,
    machine_post.cpu,
    machine_post.memory,
  )
  let work_witness_cbor = vm_witness.encode_cek_witness_v1(
    control,
    #"",
    0,
    0,
    0,
    active_pre_hash,
    1000000,
    1000000,
    h_b,
  )
  let compact = midgard_transaction_to_compact(tx)
  let tx_id = native_tx_id_for_version(
    native_tx_version_v1,
    encode_native_tx_body_compact(compact.body),
  )
  let pre = validation_trace_v1.ValidationMachineStateV1 {
    ..forced_pending_state(
      tx_id,
      tx,
      validation_context_cbor,
      validation_trace_v1.Cek,
      40,
      work_witness_cbor,
      validation_trace_v1.hash_ledger_delta(#"80"),
    ),
    execution_cpu: 10,
    execution_memory: 11,
  }
  let post = validation_trace_v1.ValidationMachineStateV1 {
    ..pre,
    program_counter: 41,
    work_root: validation_trace_v1.hash_work_witness(
      validation_trace_v1.Cek,
      41,
      vm_witness.encode_cek_witness_v1(
        control,
        #"",
        0,
        0,
        0,
        active_post_hash,
        1000000,
        1000000,
        h_b,
      ),
    ),
    execution_cpu: 16010,
    execution_memory: 111,
  }
  let evidence = one_step_evidence(
    work_witness_cbor,
    post,
    vm_machine_types.CekCoreStepWitness {
      step: cek_machine_v1.CoreStepEvidenceV1 {
        pre: machine_pre,
        post: machine_post,
        witness: cek_machine_v1.ComputeApplication {
          function: h_a,
          argument: h_b,
        },
      },
    },
  )
  Pair(pre, evidence)
}
