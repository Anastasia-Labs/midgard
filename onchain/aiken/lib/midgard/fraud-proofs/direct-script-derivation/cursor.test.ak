use aiken/collection/list
use aiken/primitive/bytearray
use env
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/receive
use midgard/fraud_proofs/direct_script_derivation/receive_prefix as prefix
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1, outputs_field_index}
use midgard/fraud_proofs/first_fault_v1.{NetworkOrderContextV1}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_output}
use midgard/fraud_proofs/native_tx/types.{MidgardTxOutput,
  ScriptCredential} as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate

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
  let Pair(bound, opening) = opened(outputs)
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
