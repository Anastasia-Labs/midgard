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

test source_discovery_prefers_inline_before_reference() {
  let selected = reconstruction.append_purpose_v1(
    reconstruction.initial_v1(
      rule.BoundExecutionV1 { ..bound(), execution_index: 0 },
      h28,
    ),
    reconstruction.phase_spend,
    0,
    h28,
    #"aa",
    #"00",
    h28,
  )
  let inline = reconstruction.append_source_v1(
    selected,
    reconstruction.SelectedSourceV1 {
      source_index: 0,
      origin_kind: 0,
      source_key: #"00",
      language_tag: 0,
      script_hash: h28,
      total_length: 4,
      item_commitment: h32,
    },
    h28,
  )
  let complete = reconstruction.finish_inline_sources_v1(inline, h28)
  and {
    complete.phase == reconstruction.phase_complete,
    complete.selected_source != None,
    reconstruction.state_is_authentic_v1(complete),
  }
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

test reference_source_index_ignores_reference_without_a_script() {
  let selected = reconstruction.append_purpose_v1(
    reconstruction.initial_v1(
      rule.BoundExecutionV1 { ..bound(), execution_index: 0 },
      h28,
    ),
    reconstruction.phase_spend,
    0,
    h28,
    #"aa",
    #"00",
    h28,
  )
  let references = reconstruction.finish_inline_sources_v1(selected, h28)
  let skipped = reconstruction.advance_reference_without_source_v1(
    references,
    h28,
  )
  let found = reconstruction.append_source_v1(
    skipped,
    reconstruction.SelectedSourceV1 {
      source_index: 0,
      origin_kind: 1,
      source_key: #"01",
      language_tag: 0,
      script_hash: h28,
      total_length: 4,
      item_commitment: h32,
    },
    h28,
  )
  expect Some(source) = found.selected_source
  and {
    skipped.field_cursor == 1,
    skipped.source_cursor == 0,
    source.source_index == 0,
    reconstruction.state_is_authentic_v1(found),
  }
}
