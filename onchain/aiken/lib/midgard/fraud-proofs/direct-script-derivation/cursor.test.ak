use aiken/collection/list
use aiken/primitive/bytearray
use env
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/receive
use midgard/fraud_proofs/direct_script_derivation/receive_prefix as prefix
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/field_opening_v1.{
  FieldOpeningV1, outputs_field_index, redeemers_field_index,
}
use midgard/fraud_proofs/first_fault_v1.{NetworkOrderContextV1}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{
  decode_native_tx_compact_v1, encode_forced_tx_compact_v1,
}
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_output}
use midgard/fraud_proofs/native_tx/types.{
  ForcedTxCompact, MidgardTxOutput, ScriptCredential,
} as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/native_tx_machine_walk_v1 as walk

const a = #"22222222222222222222222222222222222222222222222222222222"

const b = #"33333333333333333333333333333333333333333333333333333333"

fn script_output(hash: ByteArray) -> MidgardTxOutput {
  let sample = fixture.sample_native_output()
  MidgardTxOutput {
    ..sample,
    address: native_types.MidgardAddress {
      ..sample.address,
      protected: True,
      payment_credential: ScriptCredential(hash),
    },
  }
}

fn opened(outputs: List<MidgardTxOutput>) -> Pair<BoundV1, FieldOpeningV1> {
  let tx = fixture.valid_native_tx_v1()
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody { ..tx.body, outputs },
    },
  )
  Pair(
    BoundV1 {
      subject: substrate.VerdictSubjectV1 {
        version: 1,
        direction: 0,
        source_kind: 0,
        transaction_id: block.tx_id,
        source_key: #"",
        rejection_reason: None,
      },
      compact_cbor: block.compact_cbor,
      network_order_context: NetworkOrderContextV1 {
        body_network_mismatch: False,
      },
      event_pre_root: env.empty_merkle_tree_root,
    },
    fixture.inline_body_opening(
      block.compact_cbor,
      list.map(outputs, encode_midgard_tx_output),
    ),
  )
}

fn semantic(
  outputs: List<MidgardTxOutput>,
) -> (cursor.CursorV1, ByteArray, FieldOpeningV1) {
  semantic_of(opened(outputs))
}

fn semantic_of(
  source: Pair<BoundV1, FieldOpeningV1>,
) -> (cursor.CursorV1, ByteArray, FieldOpeningV1) {
  let Pair(bound, opening) = source
  let Pair(initial, checkpoint) = cursor.initial_v1(
    bound,
    outputs_field_index,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(ready, checkpoint) = cursor.certify_v1(
    initial,
    opening,
    checkpoint,
    1000,
    [],
    fixture.test_certificate_policy_id,
  )
  (ready, checkpoint, opening)
}

test receive_location_prefix_resumes_and_proves_first_occurrence() {
  let (ready, checkpoint, opening) = semantic(
    [
      script_output(b),
      fixture.sample_native_output(),
      script_output(a),
      script_output(a),
    ],
  )
  let initial = prefix.initial_v1(ready, 2, a)
  let Pair(partial, next) = prefix.advance_v1(
    initial,
    opening,
    checkpoint,
    2,
    [],
    fixture.test_certificate_policy_id,
  )
  let Pair(finished, _) = prefix.advance_v1(
    partial,
    opening,
    next,
    2,
    [],
    fixture.test_certificate_policy_id,
  )
  let purpose = prefix.location_v1(finished)
  and {
    !partial.complete,
    purpose.first_output_index == 2,
    purpose.script_hash == a,
  }
}

test named_receive_later_duplicate_is_refused() fail {
  let (ready, checkpoint, opening) = semantic(
    [script_output(a), script_output(b), script_output(a)],
  )
  prefix.advance_v1(
    prefix.initial_v1(ready, 2, a),
    opening,
    checkpoint,
    3,
    [],
    fixture.test_certificate_policy_id,
  ).1st.complete
}

test named_receive_wrong_credential_is_refused() fail {
  let (ready, checkpoint, opening) = semantic([script_output(b)])
  prefix.advance_v1(
    prefix.initial_v1(ready, 0, a),
    opening,
    checkpoint,
    1,
    [],
    fixture.test_certificate_policy_id,
  ).1st.complete
}

test modified_cursor_checkpoint_is_refused() fail {
  let (ready, checkpoint, opening) = semantic([script_output(a)])
  let changed = bytearray.concat(bytearray.take(checkpoint, 52), #"00")
  prefix.advance_v1(
    prefix.initial_v1(ready, 0, a),
    opening,
    changed,
    1,
    [],
    fixture.test_certificate_policy_id,
  ).1st.complete
}

test different_transaction_opening_is_refused() fail {
  let (ready, checkpoint, _) = semantic([script_output(a)])
  let Pair(_, changed) = opened([script_output(b)])
  prefix.advance_v1(
    prefix.initial_v1(ready, 0, a),
    changed,
    checkpoint,
    1,
    [],
    fixture.test_certificate_policy_id,
  ).1st.complete
}

fn ready_outputs(
  ready: cursor.CursorV1,
  opening: FieldOpeningV1,
) -> receive.OutputStateV1 {
  let Pair(initial, _) = receive.initial_outputs_v1(
    BoundIndexV1 { bound: ready.bound, source_index: source_index.initial_v1() },
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  receive.OutputStateV1 { ..initial, cursor: ready }
}

test empty_output_set_finishes_without_a_synthetic_item() {
  let (ready, checkpoint, opening) = semantic([])
  let finished = receive.finish_outputs_v1(
    ready_outputs(ready, opening),
    opening,
    checkpoint,
    [],
    fixture.test_certificate_policy_id,
  )
  receive.declaration_is_complete_v1(
    receive.initial_declaration_v1(finished, None),
  )
}

test nonempty_output_set_cannot_finish_early() fail {
  let (ready, checkpoint, opening) = semantic([script_output(a)])
  receive.finish_outputs_v1(
    ready_outputs(ready, opening),
    opening,
    checkpoint,
    [],
    fixture.test_certificate_policy_id,
  ).complete
}

// ## Body-only resume and the redeemers-field opening
//
// `resume_body_at_v1` returns exactly what `resume_at_v1` returns at the
// outputs field, and `initial_redeemers_v1` exactly what
// `initial_with_completion_v1` returns at the redeemers field. Each refusal
// row fails one check only.

fn resume_body(
  ready: cursor.CursorV1,
  field_index: Int,
  opening: FieldOpeningV1,
  checkpoint: ByteArray,
) -> Int {
  walk.walk_next_item_index(
    cursor.resume_body_at_v1(
      ready,
      field_index,
      opening,
      checkpoint,
      [],
      fixture.test_certificate_policy_id,
    ).2nd,
  )
}

test body_resume_agrees_with_resume_at() {
  let (ready, checkpoint, opening) = semantic([script_output(a)])
  cursor.resume_body_at_v1(
    ready,
    outputs_field_index,
    opening,
    checkpoint,
    [],
    fixture.test_certificate_policy_id,
  ) == cursor.resume_at_v1(
    ready,
    outputs_field_index,
    opening,
    checkpoint,
    [],
    fixture.test_certificate_policy_id,
  )
}

test body_resume_refuses_a_cursor_at_another_field() fail {
  let (ready, checkpoint, opening) = semantic([script_output(a)])
  expect _opened = resume_body(
    cursor.CursorV1 { ..ready, field_index: 0 },
    outputs_field_index,
    opening,
    checkpoint,
  )
}

test body_resume_refuses_a_certifying_cursor() fail {
  let (ready, checkpoint, opening) = semantic([script_output(a)])
  expect _opened = resume_body(
    cursor.CursorV1 { ..ready, phase: cursor.Certifying },
    outputs_field_index,
    opening,
    checkpoint,
  )
}

/// The cursor and the requested index agree, but the committed checkpoint is
/// at the outputs field.
test body_resume_refuses_a_checkpoint_at_another_field() fail {
  let (ready, checkpoint, opening) = semantic([script_output(a)])
  expect _opened = resume_body(
    cursor.CursorV1 { ..ready, field_index: 0 },
    0,
    opening,
    checkpoint,
  )
}

fn redeemer_bound() -> Pair<BoundV1, FieldOpeningV1> {
  let Pair(bound, _) = opened([])
  let tx = fixture.valid_native_tx_v1()
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody { ..tx.body, outputs: [] },
    },
  )
  expect block.compact_cbor == bound.compact_cbor
  Pair(
    bound,
    fixture.inline_witness_opening(
      block.compact_cbor,
      block.witness_set_compact,
      [],
    ),
  )
}

test redeemers_opening_agrees_with_initial_with_completion() {
  let Pair(bound, opening) = redeemer_bound()
  cursor.initial_redeemers_v1(
    bound,
    opening,
    [],
    fixture.test_certificate_policy_id,
  ) == cursor.initial_with_completion_v1(
    bound,
    redeemers_field_index,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
}

test redeemers_opening_refuses_a_body_opening() fail {
  let Pair(bound, body) = opened([])
  expect _opened = cursor.initial_redeemers_v1(
    bound,
    body,
    [],
    fixture.test_certificate_policy_id,
  )
}

// ## The thread's source kind selects the compact envelope
//
// A forced-source thread records source kind 1, and its compact transaction
// uses the forced envelope. Both resume paths read the envelope under the
// thread's recorded kind: they accept a forced envelope under kind 1 and
// refuse a normal envelope under kind 1.

/// The `opened` transaction in its forced envelope, under source kind 1. The
/// transaction id is unchanged: both envelopes carry the same body bytes.
fn forced_opened(
  outputs: List<MidgardTxOutput>,
) -> Pair<BoundV1, FieldOpeningV1> {
  let Pair(bound, _) = opened(outputs)
  let normal = decode_native_tx_compact_v1(bound.compact_cbor)
  let forced_cbor = encode_forced_tx_compact_v1(
    ForcedTxCompact {
      body: normal.body,
      witness_set_hash: normal.witness_set_hash,
    },
  )
  Pair(
    BoundV1 {
      ..bound,
      subject: substrate.VerdictSubjectV1 { ..bound.subject, source_kind: 1 },
      compact_cbor: forced_cbor,
    },
    fixture.inline_body_opening(
      forced_cbor,
      list.map(outputs, encode_midgard_tx_output),
    ),
  )
}

/// A normal-envelope cursor whose thread records source kind 1.
fn normal_envelope_under_forced_kind(ready: cursor.CursorV1) -> cursor.CursorV1 {
  cursor.CursorV1 {
    ..ready,
    bound: BoundV1 {
      ..ready.bound,
      subject: substrate.VerdictSubjectV1 {
        ..ready.bound.subject,
        source_kind: 1,
      },
    },
  }
}

test forced_body_resume_reads_the_forced_envelope() {
  let (ready, checkpoint, opening) = semantic_of(
    forced_opened([script_output(a)]),
  )
  resume_body(ready, outputs_field_index, opening, checkpoint) == 0
}

test forced_resume_at_reads_the_forced_envelope() {
  let (ready, checkpoint, opening) = semantic_of(
    forced_opened([script_output(a)]),
  )
  walk.walk_next_item_index(
    cursor.resume_at_v1(
      ready,
      outputs_field_index,
      opening,
      checkpoint,
      [],
      fixture.test_certificate_policy_id,
    ).2nd,
  ) == 0
}

test body_resume_refuses_a_normal_envelope_under_forced_kind() fail {
  let (ready, checkpoint, opening) = semantic([script_output(a)])
  expect _opened = resume_body(
    normal_envelope_under_forced_kind(ready),
    outputs_field_index,
    opening,
    checkpoint,
  )
}

test resume_at_refuses_a_normal_envelope_under_forced_kind() fail {
  let (ready, checkpoint, opening) = semantic([script_output(a)])
  expect _opened = cursor.resume_at_v1(
    normal_envelope_under_forced_kind(ready),
    outputs_field_index,
    opening,
    checkpoint,
    [],
    fixture.test_certificate_policy_id,
  )
}
