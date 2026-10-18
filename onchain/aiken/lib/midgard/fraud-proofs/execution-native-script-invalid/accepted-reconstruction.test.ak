use midgard/fraud_proofs/execution_native_script_invalid/accepted_reconstruction as reconstruction
use midgard/fraud_proofs/execution_native_script_invalid/rule
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate

const h28 = #"22222222222222222222222222222222222222222222222222222222"

const other_h28 = #"33333333333333333333333333333333333333333333333333333333"

const h32 = #"1111111111111111111111111111111111111111111111111111111111111111"

fn bound() -> rule.BoundExecutionV1 {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  rule.BoundExecutionV1 {
    subject: substrate.VerdictSubjectV1 {
      version: substrate.version,
      direction: substrate.direction_wrongful_acceptance,
      source_kind: substrate.source_kind_accepted,
      transaction_id: block.tx_id,
      source_key: #"",
      rejection_reason: None,
    },
    validation_traces_root: h32,
    validation_trace_count: 1,
    execution_index: 1,
    accused_class: rule.result_pending,
    prior_ledger_root: h32,
    compact_cbor: block.compact_cbor,
  }
}

test purpose_prefix_binds_the_absolute_execution_coordinate() {
  let initial = reconstruction.initial_v1(bound(), h28)
  let first = reconstruction.append_purpose_v1(
    initial,
    reconstruction.phase_spend,
    0,
    other_h28,
    #"aa",
    #"00",
    h28,
  )
  let second = reconstruction.append_purpose_v1(
    first,
    reconstruction.phase_spend,
    2,
    h28,
    #"bb",
    #"01",
    h28,
  )
  expect Some(selected) = second.selected_purpose
  and {
    selected.purpose_kind == reconstruction.phase_spend,
    selected.purpose_index == 2,
    selected.script_hash == h28,
    reconstruction.state_is_authentic_v1(second),
  }
}

fn selected_for(script_hash: ByteArray) -> reconstruction.StateV1 {
  reconstruction.append_purpose_v1(
    reconstruction.initial_v1(
      rule.BoundExecutionV1 { ..bound(), execution_index: 0 },
      h28,
    ),
    reconstruction.phase_spend,
    0,
    script_hash,
    #"aa",
    #"00",
    h28,
  )
}

fn source(
  source_index: Int,
  origin_kind: Int,
  source_key: ByteArray,
  script_hash: ByteArray,
) -> reconstruction.SelectedSourceV1 {
  reconstruction.SelectedSourceV1 {
    source_index,
    origin_kind,
    source_key,
    language_tag: 0,
    script_hash,
    total_length: 4,
    item_commitment: h32,
  }
}

test source_lookup_selects_a_resolved_source_before_an_inline_one() {
  let inline = reconstruction.append_inline_source_v1(
    selected_for(h28),
    source(0, 0, #"00", h28),
    h28,
  )
  let resolved = reconstruction.scan_resolved_output_v1(
    reconstruction.finish_inline_sources_v1(inline, h28),
    #"01",
    Some(source(1, 1, #"01", h28)),
    h28,
  )
  let selected = reconstruction.finish_resolved_outputs_v1(resolved, 1)
  and {
    inline.selected_source == Some(source(0, 0, #"00", h28)),
    selected.origin_kind == 1,
    selected.source_index == 1,
  }
}

test source_lookup_keeps_an_inline_source_when_no_resolved_output_matches() {
  let inline = reconstruction.append_inline_source_v1(
    reconstruction.append_inline_source_v1(
      selected_for(h28),
      source(0, 0, #"00", other_h28),
      h28,
    ),
    source(1, 0, #"01", h28),
    h28,
  )
  let resolved = reconstruction.scan_resolved_output_v1(
    reconstruction.finish_inline_sources_v1(inline, h28),
    #"01",
    Some(source(2, 1, #"01", other_h28)),
    h28,
  )
  let selected = reconstruction.finish_resolved_outputs_v1(resolved, 1)
  and {
    selected.origin_kind == 0,
    selected.source_index == 1,
  }
}

test source_lookup_keeps_the_first_resolved_match() {
  let references = reconstruction.finish_inline_sources_v1(
    selected_for(h28),
    h28,
  )
  let first = reconstruction.scan_resolved_output_v1(
    references,
    #"01",
    Some(source(0, 1, #"01", h28)),
    h28,
  )
  let second = reconstruction.scan_resolved_output_v1(
    first,
    #"02",
    Some(source(1, 1, #"02", h28)),
    h28,
  )
  reconstruction.finish_resolved_outputs_v1(second, 2).source_key == #"01"
}

test resolved_walk_refuses_a_non_ascending_key() fail {
  let references = reconstruction.finish_inline_sources_v1(
    selected_for(h28),
    h28,
  )
  let first = reconstruction.scan_resolved_output_v1(
    references,
    #"02",
    None,
    h28,
  )
  reconstruction.scan_resolved_output_v1(first, #"02", None, h28).field_cursor == 2
}

test resolved_walk_refuses_to_finish_before_every_output() fail {
  let references = reconstruction.finish_inline_sources_v1(
    selected_for(h28),
    h28,
  )
  let first = reconstruction.scan_resolved_output_v1(
    references,
    #"01",
    Some(source(0, 1, #"01", h28)),
    h28,
  )
  reconstruction.finish_resolved_outputs_v1(first, 2).origin_kind == 1
}

test resolved_walk_refuses_a_source_keyed_off_its_output() fail {
  let references = reconstruction.finish_inline_sources_v1(
    selected_for(h28),
    h28,
  )
  reconstruction.scan_resolved_output_v1(
    references,
    #"01",
    Some(source(0, 1, #"02", h28)),
    h28,
  ).source_cursor == 1
}

test checkpoint_refuses_cursor_mutation() fail {
  let state = reconstruction.initial_v1(bound(), h28)
  reconstruction.state_is_authentic_v1(
    reconstruction.StateV1 { ..state, field_cursor: 1 },
  )
}

test receive_passes_enumerate_unique_hashes_in_canonical_order() {
  let spend_done = reconstruction.finish_purpose_phase_v1(
    reconstruction.initial_v1(bound(), h28),
    h28,
  )
  let mint_done = reconstruction.finish_purpose_phase_v1(spend_done, h28)
  let receive = reconstruction.finish_purpose_phase_v1(mint_done, h28)
  let pass_one = reconstruction.scan_receive_output_v1(
    reconstruction.scan_receive_output_v1(receive, Some(other_h28), h28),
    Some(h28),
    h28,
  )
  let after_first = reconstruction.finish_receive_pass_v1(pass_one, h28, h28)
  let pass_two = reconstruction.scan_receive_output_v1(
    reconstruction.scan_receive_output_v1(after_first, Some(other_h28), h28),
    Some(other_h28),
    h28,
  )
  let after_second = reconstruction.finish_receive_pass_v1(pass_two, h28, h28)
  expect Some(selected) = after_second.selected_purpose
  and {
    after_first.previous_key == h28,
    selected.purpose_index == 1,
    selected.script_hash == other_h28,
    reconstruction.state_is_authentic_v1(after_second),
  }
}

test resolved_source_index_ignores_outputs_without_a_script() {
  let references = reconstruction.finish_inline_sources_v1(
    selected_for(h28),
    h28,
  )
  let skipped = reconstruction.scan_resolved_output_v1(
    references,
    #"01",
    None,
    h28,
  )
  let found = reconstruction.scan_resolved_output_v1(
    skipped,
    #"02",
    Some(source(0, 1, #"02", h28)),
    h28,
  )
  let selected = reconstruction.finish_resolved_outputs_v1(found, 2)
  and {
    skipped.field_cursor == 1,
    skipped.source_cursor == 0,
    selected.source_index == 0,
  }
}
