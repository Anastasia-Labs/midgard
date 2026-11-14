use aiken/collection/list
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/purpose
use midgard/fraud_proofs/direct_script_derivation/purpose_workers as door
use midgard/fraud_proofs/direct_script_derivation/selection_types.{SelectionV1}
use midgard/fraud_proofs/direct_script_derivation/source.{SourceV1}
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1, PurposeV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1}
use midgard/fraud_proofs/first_fault_v1.{
  NetworkOrderContextV1, PurposeCoordinateV1,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1.{VerdictSubjectV1}
use midgard/native_tx_machine_walk_v1 as walk

const hash = #"33333333333333333333333333333333333333333333333333333333"

const root = #"4444444444444444444444444444444444444444444444444444444444444444"

fn source(field: Int) -> SourceV1 {
  SourceV1 {
    field_index: field,
    item_index: 2,
    source_key: #"02",
    language_tag: 3,
    script_hash: hash,
    total_length: 8,
    item_commitment: root,
  }
}

fn bound(index: source_index.StateV1) -> BoundIndexV1 {
  BoundIndexV1 {
    bound: BoundV1 {
      subject: VerdictSubjectV1 {
        version: 1,
        direction: 0,
        source_kind: 0,
        transaction_id: root,
        source_key: #"",
        rejection_reason: None,
      },
      compact_cbor: #"",
      network_order_context: NetworkOrderContextV1 {
        body_network_mismatch: False,
      },
      event_pre_root: root,
    },
    source_index: index,
  }
}

fn selection(field: Int) -> SelectionV1 {
  let candidate = source(field)
  let coordinate = PurposeCoordinateV1 { kind: 3, index: 7 }
  door.selected_v1(
    bound(
      source_index.admit_v1(
        source_index.initial_v1(),
        candidate,
        source_index.First { proof: [] },
      ),
    ),
    coordinate,
    Some(PurposeV1 { coordinate, script_hash: hash, subject: hash }),
  )
}

test lookup_preserves_exact_bound_map_and_source_for_every_source_class() {
  let check = fn(field) {
    let selected = selection(field)
    let actual = door.lookup_v1(selected, Some(source(field)), [])
    and {
      actual.selection == selected,
      actual.source == Some(source(field)),
    }
  }
  and {
    check(0),
    check(1),
    check(6),
  }
}

test lookup_cannot_substitute_source_language() fail {
  door.lookup_v1(
    selection(6),
    Some(SourceV1 { ..source(6), language_tag: 128 }),
    [],
  ).source != None
}

test lookup_cannot_hide_a_present_source() fail {
  door.lookup_v1(selection(1), None, []).source == None
}

test lookup_cannot_substitute_the_source_map_root() fail {
  let selected = selection(0)
  door.lookup_v1(
    SelectionV1 { ..selected, source_index: source_index.initial_v1() },
    Some(source(0)),
    [],
  ).source != None
}

test selection_cannot_relabel_an_actual_purpose_coordinate() fail {
  let selected = selection(6)
  door.selected_v1(
    bound(selected.source_index),
    PurposeCoordinateV1 { kind: 3, index: 8 },
    selected.purpose,
  ).coordinate.index == 8
}

fn spend_state() -> (door.SpendStateV1, ByteArray, FieldOpeningV1) {
  let input = fn(index) {
    native_types.MidgardTxInput { tx_id: root, output_index: index }
  }
  // Raw ordinal two is sorted-spend ordinal one. References straddle the
  // target but must not contribute to its Spending pointer.
  let inputs = [input(9), input(1), input(5)]
  let tx = fixture.valid_native_tx_v1()
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody {
        ..tx.body,
        inputs,
        reference_inputs: [input(0), input(3)],
      },
    },
  )
  let opening = fixture.inline_body_opening(
    block.compact_cbor,
    list.map(inputs, encode_midgard_tx_input),
  )
  let original = bound(source_index.StateV1 { root, count: 7 })
  let actual_bound = BoundV1 {
    ..original.bound,
    subject: VerdictSubjectV1 {
      ..original.bound.subject,
      transaction_id: block.tx_id,
    },
    compact_cbor: block.compact_cbor,
  }
  let (field_cursor, checkpoint, complete) = cursor.initial_with_completion_v1(
    actual_bound,
    0,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  (
    door.SpendStateV1 {
      source_index: original.source_index,
      cursor: field_cursor,
      raw_item_index: 2,
      target: purpose.SpendCandidateV1 {
        script_hash: hash,
        subject: encode_midgard_tx_input(input(5)),
      },
      rank: 0,
      complete,
    },
    checkpoint,
    opening,
  )
}

test spend_pointer_is_spends_only_sorted_rank_not_raw_or_reference_ordinal() {
  let (initial, checkpoint, opening) = spend_state()
  let args = door.ProgressArgsV1 {
    input_index: 0,
    output_index: 0,
    opening,
    checkpoint_bytes: checkpoint,
    budget: 1,
  }
  let partial = door.advance_spend_v1(
    initial,
    args,
    [],
    fixture.test_certificate_policy_id,
  )
  let checkpoint = cursor.resume_at_v1(
    initial.cursor,
    0,
    opening,
    checkpoint,
    [],
    fixture.test_certificate_policy_id,
  ).2nd
  let Pair(_, first) = walk.walk_next_extent(
    cursor.resume_at_v1(
      initial.cursor,
      0,
      opening,
      args.checkpoint_bytes,
      [],
      fixture.test_certificate_policy_id,
    ).1st,
    checkpoint,
  )
  let finished = door.advance_spend_v1(
    partial,
    door.ProgressArgsV1 {
      ..args,
      checkpoint_bytes: walk.encode_field_walk_checkpoint(first),
      budget: 2,
    },
    [],
    fixture.test_certificate_policy_id,
  )
  let selected = door.finish_spend_v1(finished)
  and {
    !partial.complete,
    finished.raw_item_index == 2,
    selected.coordinate.index == 1,
    selected.coordinate.kind == 0,
    selected.source_index == initial.source_index,
    selected.bound == initial.cursor.bound,
  }
}

test incomplete_spend_rank_cannot_finish() fail {
  let (initial, _, _) = spend_state()
  door.finish_spend_v1(initial).coordinate.index == 0
}
