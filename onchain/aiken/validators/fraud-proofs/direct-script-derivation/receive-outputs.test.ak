use aiken/collection/list
use cardano/transaction.{Transaction}
use env
use fraud_proofs/direct_script_derivation/receive_finish as finish_validator
use fraud_proofs/direct_script_derivation/receive_outputs as output_validator
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/cursor
use midgard/fraud_proofs/direct_script_derivation/receive
use midgard/fraud_proofs/direct_script_derivation/receive_finish as finish_door
use midgard/fraud_proofs/direct_script_derivation/receive_outputs as door
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

fn ready_outputs(
  ready: cursor.CursorV1,
  opening: FieldOpeningV1,
) -> receive.OutputStateV1 {
  let Pair(initial, _) = receive.initial_outputs_v1(
    BoundIndexV1 {
      bound: ready.bound,
      source_index: source_index.StateV1 {
        root: #"4444444444444444444444444444444444444444444444444444444444444444",
        count: 7,
      },
    },
    opening,
    [],
    fixture.test_certificate_policy_id,
  )
  receive.OutputStateV1 { ..initial, cursor: ready }
}

fn applied_output_batch(changed_root: Bool, changed_receipt: Int) -> Bool {
  let (ready, checkpoint, opening) = semantic(
    [script_output(a), script_output(a), fixture.sample_native_output()],
  )
  let initial = ready_outputs(ready, opening)
  let witnesses = [
    receive.FirstCredential { proof: [] },
    receive.RepeatedCredential { first_output_index: 0, proof: [] },
    receive.NoCredential,
  ]
  let Pair(advanced, _) = receive.advance_outputs_v1(
    initial,
    opening,
    checkpoint,
    witnesses,
    [],
    fixture.test_certificate_policy_id,
  )
  let expected: Data = receive.OutputStateV1 {
    ..advanced,
    credential_root: if changed_root {
      env.empty_merkle_tree_root
    } else {
      advanced.credential_root
    },
    source_index: source_index.StateV1 {
      root: if changed_receipt == 1 {
        #"5555555555555555555555555555555555555555555555555555555555555555"
      } else {
        advanced.source_index.root
      },
      count: if changed_receipt == 2 {
        8
      } else {
        7
      },
    },
  }
  output_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(initial),
      },
    ),
    ct.Continue(
      door.Scan {
        input_index: 0,
        output_index: 0,
        outputs_opening: opening,
        checkpoint_bytes: checkpoint,
        witnesses,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_same_step_l1_tx_v1(
      ready.bound.subject.transaction_id,
      expected,
    ),
  )
}

test applied_receive_batch_admits_three_actual_outputs() {
  applied_output_batch(False, 0)
}

test applied_receive_batch_refuses_substituted_map_root() {
  !applied_output_batch(True, 0)
}

test applied_receive_batch_preserves_completed_source_root() {
  !applied_output_batch(False, 1)
}

test applied_receive_batch_preserves_completed_source_count() {
  !applied_output_batch(False, 2)
}

fn applied_finish(
  empty: Bool,
  advance: Bool,
  forged_complete: Bool,
  substituted_source: Bool,
  fresh_none: Bool,
) -> Bool {
  let items = if empty {
    []
  } else {
    [fixture.sample_native_output()]
  }
  let (ready, checkpoint, opening) = semantic(items)
  let initial = ready_outputs(ready, opening)
  let Pair(state, checkpoint) = if advance {
    receive.advance_outputs_v1(
      initial,
      opening,
      checkpoint,
      [receive.NoCredential],
      [],
      fixture.test_certificate_policy_id,
    )
  } else {
    Pair(initial, checkpoint)
  }
  let state = if forged_complete {
    receive.OutputStateV1 { ..state, complete: True }
  } else {
    state
  }
  let (_, _, another_opening) = semantic([script_output(b)])
  let expected: Data = receive.initial_declaration_v1(
    receive.OutputStateV1 { ..state, complete: True },
    Some(0),
  )
  let tx = fixture.continue_mid_step_l1_tx_v1(
    ready.bound.subject.transaction_id,
    expected,
  )
  let tx = Transaction {
    ..tx,
    extra_signatories: [fixture.fraud_prover_key_hash],
  }
  finish_validator.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: if fresh_none {
          None
        } else {
          Some(state)
        },
      },
    ),
    ct.Continue(
      finish_door.Args {
        input_index: 0,
        output_index: 0,
        outputs_opening: if substituted_source {
          another_opening
        } else {
          opening
        },
        checkpoint_bytes: checkpoint,
        target_ordinal: Some(0),
      },
    ),
    fixture.own_out_ref(),
    tx,
  )
}

test physical_finish_accepts_an_authentic_empty_output_field() {
  applied_finish(True, False, False, False, False)
}

test physical_finish_accepts_actual_consumed_output_exhaustion() {
  applied_finish(False, True, False, False, False)
}

test physical_finish_refuses_a_forged_true_flag_before_exhaustion() fail {
  applied_finish(False, False, True, False, False)
}

test physical_finish_refuses_early_finish_with_the_initial_false_flag() fail {
  applied_finish(False, False, False, False, False)
}

test physical_finish_refuses_a_substituted_retained_field_source() fail {
  applied_finish(False, True, False, True, False)
}

test physical_finish_cannot_supply_a_true_flag_from_fresh_init_none() fail {
  applied_finish(True, False, True, False, True)
}
