use aiken/collection/list
use cardano/address
use cardano/transaction.{Output, Transaction}
use env
use fraud_proofs/direct_script_derivation/receive_grammar as grammar_validator
use fraud_proofs/direct_script_derivation/receive_start as start_validator
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/first_fault_seam.{
  NetworkOrderContextV1,
}
use midgard/fraud_proofs/direct_script_derivation/receive
use midgard/fraud_proofs/direct_script_derivation/receive_grammar as door
use midgard/fraud_proofs/direct_script_derivation/receive_start as start_door
use midgard/fraud_proofs/direct_script_derivation/source_index
use midgard/fraud_proofs/direct_script_derivation/source_scan.{BoundIndexV1}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1}
use midgard/fraud_proofs/field_opening_v1.{FieldOpeningV1, outputs_field_index}
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

fn grammar_step(
  budget: Int,
  route_to_outputs: Bool,
  changed_count: Bool,
  changed_source_receipt: Int,
) -> Bool {
  let Pair(bound, opening) = opened(
    [script_output(a), script_output(b), fixture.sample_native_output()],
  )
  let Pair(initial, checkpoint) = receive.initial_outputs_v1(
    BoundIndexV1 { bound, source_index: source_receipt() },
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  let initial_cursor = initial.cursor
  let input = if changed_count {
    receive.OutputStateV1 { ..initial, credential_count: 1 }
  } else {
    initial
  }
  let Pair(advanced, _) = cursor.certify_at_v1(
    initial_cursor,
    outputs_field_index,
    opening,
    checkpoint,
    budget,
    [],
    fixture.test_certificate_policy_id,
  )
  let expected: Data = receive.OutputStateV1 {
    ..initial,
    cursor: advanced,
    source_index: source_index.StateV1 {
      root: if changed_source_receipt == 1 {
        #"5555555555555555555555555555555555555555555555555555555555555555"
      } else {
        initial.source_index.root
      },
      count: if changed_source_receipt == 2 {
        8
      } else {
        7
      },
    },
  }
  let tx = fixture.continue_same_step_l1_tx_v1(
    bound.subject.transaction_id,
    expected,
  )
  expect Some(own_output) = list.at(tx.outputs, 0)
  let tx = if route_to_outputs {
    Transaction {
      ..tx,
      outputs: [
        Output {
          ..own_output,
          address: address.from_script(fixture.next_step_script_hash),
        },
      ],
    }
  } else {
    tx
  }
  grammar_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(input),
      },
    ),
    ct.Continue(
      door.Args {
        input_index: 0,
        output_index: 0,
        outputs_opening: opening,
        checkpoint_bytes: checkpoint,
        budget,
      },
    ),
    fixture.own_out_ref(),
    tx,
  )
}

test grammar_partial_batch_stays_at_the_certification_door() {
  grammar_step(2, False, False, 0)
}

test grammar_completion_routes_to_the_output_semantic_door() {
  grammar_step(3, True, False, 0)
}

test grammar_completion_cannot_stay_at_the_old_door() {
  !grammar_step(3, False, False, 0)
}

test grammar_cannot_reset_a_nonempty_credential_map() fail {
  grammar_step(2, False, True, 0)
}

test grammar_preserves_completed_source_root() {
  !grammar_step(2, False, False, 1)
}

test grammar_preserves_completed_source_count() {
  !grammar_step(3, True, False, 2)
}

fn source_receipt() -> source_index.StateV1 {
  source_index.StateV1 {
    root: #"4444444444444444444444444444444444444444444444444444444444444444",
    count: 7,
  }
}

fn receive_start(
  changed_receipt: Int,
  empty: Bool,
  substituted_opening: Bool,
  fresh_none: Bool,
) -> Bool {
  let Pair(bound, opening) = opened(
    if empty {
      []
    } else {
      [script_output(a)]
    },
  )
  let Pair(initial_cursor, _) = cursor.initial_v1(
    bound,
    outputs_field_index,
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  let source = BoundIndexV1 { bound, source_index: source_receipt() }
  let expected: Data = receive.OutputStateV1 {
    cursor: if changed_receipt == 3 {
      cursor.CursorV1 {
        ..initial_cursor,
        bound: BoundV1 {
          ..bound,
          event_pre_root: #"5555555555555555555555555555555555555555555555555555555555555555",
        },
      }
    } else {
      initial_cursor
    },
    source_index: source_index.StateV1 {
      root: if changed_receipt == 1 {
        #"5555555555555555555555555555555555555555555555555555555555555555"
      } else {
        source.source_index.root
      },
      count: if changed_receipt == 2 {
        8
      } else {
        7
      },
    },
    credential_root: env.empty_merkle_tree_root,
    credential_count: 0,
    complete: False,
  }
  let Pair(_, other_opening) = opened([script_output(b)])
  let tx = fixture.continue_mid_step_l1_tx_v1(
    bound.subject.transaction_id,
    expected,
  )
  start_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: if fresh_none {
          None
        } else {
          Some(source)
        },
      },
    ),
    ct.Continue(
      start_door.Args {
        input_index: 0,
        output_index: 0,
        outputs_opening: if substituted_opening {
          other_opening
        } else {
          opening
        },
      },
    ),
    fixture.own_out_ref(),
    Transaction { ..tx, extra_signatories: [] },
  )
}

test receive_initializer_preserves_completed_nonempty_source_receipt() {
  receive_start(0, False, False, False)
}

test receive_initializer_opens_an_authentic_empty_output_field() {
  receive_start(0, True, False, False)
}

test receive_initializer_refuses_substituted_source_root() {
  !receive_start(1, False, False, False)
}

test receive_initializer_refuses_substituted_source_count() {
  !receive_start(2, False, False, False)
}

test receive_initializer_refuses_substituted_event_bound() {
  !receive_start(3, False, False, False)
}

test receive_initializer_refuses_another_transaction_output_opening() fail {
  receive_start(0, False, True, False)
}

test receive_initializer_refuses_fresh_init_without_source_receipt() fail {
  receive_start(0, False, False, True)
}
