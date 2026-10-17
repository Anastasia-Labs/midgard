use aiken/cbor
use cardano/transaction.{Transaction}
use fraud_proofs/direct_script_derivation/bind_event as event_entry
use fraud_proofs/direct_script_derivation/bind_pre_state as pre_entry
use midgard/computation_thread as ct
use midgard/fraud_proofs/direct_script_derivation/bind
use midgard/fraud_proofs/direct_script_derivation/bind_event as event_door
use midgard/fraud_proofs/direct_script_derivation/bind_pre_state as pre_door
use midgard/fraud_proofs/direct_script_derivation/bind_state.{EventSelectedV1}
use midgard/fraud_proofs/direct_script_derivation/first_fault_seam.{
  NetworkOrderContextV1,
}
use midgard/fraud_proofs/direct_script_derivation/types.{BoundV1, SourceBoundV1}
use midgard/fraud_proofs/input_existence_fixture_v1 as carriage
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/rejection_reason_v1.{EmptyInputs, ForcedTxInvalid, ForcedTxValid}

fn source_network(
  direction: Int,
  network_id: Int,
) -> (SourceBoundV1, tf.ThreadClaimFixtureV1) {
  let tx = fixture.valid_native_tx_v1()
  let block = fixture.native_block_fixture_v1(
    native_types.MidgardTransaction {
      ..tx,
      body: native_types.MidgardTransactionBody { ..tx.body, network_id },
    },
  )
  let forced = direction != 0
  let reason = if direction == 2 {
    Some(EmptyInputs)
  } else {
    None
  }
  let phase = if forced {
    ledger_state.ForcedTransaction
  } else {
    ledger_state.L2Transaction
  }
  let event = if forced {
    ledger_state.ForcedTransactionEventKey {
      tx_order_id: tf.forced_order_key_v1(),
    }
  } else {
    ledger_state.L2TransactionEventKey { tx_id: block.tx_id }
  }
  let leaf = tf.forced_leaf_v1(
    block.tx,
    if direction == 2 {
      ForcedTxInvalid { reason: EmptyInputs }
    } else {
      ForcedTxValid
    },
  )
  let forced_membership = tf.forced_leaf_membership_v1(
    tf.forced_order_key_v1(),
    leaf,
  )
  let claim = tf.thread_claim_v1(
    event,
    phase,
    tf.h32_d,
    if forced {
      Some(forced_membership.root)
    } else {
      None
    },
  )
  let subject = substrate.VerdictSubjectV1 {
    version: substrate.version,
    direction: if direction == 2 {
      substrate.direction_wrongful_rejection
    } else {
      substrate.direction_wrongful_acceptance
    },
    source_kind: if forced {
      substrate.source_kind_forced
    } else {
      substrate.source_kind_accepted
    },
    transaction_id: if forced {
      leaf.tx_id
    } else {
      block.tx_id
    },
    source_key: if forced {
      cbor.serialise(tf.forced_order_key_v1())
    } else {
      #""
    },
    rejection_reason: reason,
  }
  (
    SourceBoundV1 {
      subject,
      header: claim.header,
      compact_cbor: if forced {
        leaf.submitted_source.compact_cbor
      } else {
        block.compact_cbor
      },
    },
    claim,
  )
}

fn source(direction: Int) -> (SourceBoundV1, tf.ThreadClaimFixtureV1) {
  source_network(direction, 0)
}

fn event_step(direction: Int, changed_output: Bool) -> Bool {
  let (bound, claim) = source(direction)
  let membership = claim.event_to_step_membership
  let redeemers = [
    fixture.phas_withdraw_redeemer_entry(
      membership.phas_root,
      cbor.serialise(membership.key),
      cbor.serialise(membership.value),
    ),
  ]
  let opening = carriage.direct_carried_membership_v1(membership, 0)
  let selected = bind.select_event_v1(bound, opening, [], redeemers)
  let expected: Data = if changed_output {
    EventSelectedV1 { ..selected, step_index: selected.step_index + 1 }
  } else {
    selected
  }
  event_entry.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(bound),
      },
    ),
    ct.Continue(
      event_door.Args {
        input_index: 0,
        output_index: 0,
        event_opening: opening,
      },
    ),
    fixture.own_out_ref(),
    Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(
        tf.header_asset_name_v1(claim.header),
        expected,
      ),
      redeemers,
    },
  )
}

fn pre_step_context(
  direction: Int,
  changed_index: Bool,
  changed_compact: Bool,
  network_id: Int,
  body_network_mismatch: Bool,
) -> Bool {
  let (bound, claim) = source_network(direction, network_id)
  let event = claim.event_to_step_membership
  let event_redeemers = [
    fixture.phas_withdraw_redeemer_entry(
      event.phas_root,
      cbor.serialise(event.key),
      cbor.serialise(event.value),
    ),
  ]
  let selected = bind.select_event_v1(
    bound,
    carriage.direct_carried_membership_v1(event, 0),
    [],
    event_redeemers,
  )
  let selected = if changed_index {
    EventSelectedV1 { ..selected, step_index: selected.step_index + 1 }
  } else {
    selected
  }
  let step = claim.transition_step_membership
  let expected: Data = BoundV1 {
    subject: bound.subject,
    compact_cbor: if changed_compact {
      #"00"
    } else {
      bound.compact_cbor
    },
    network_order_context: NetworkOrderContextV1 { body_network_mismatch },
    event_pre_root: tf.h32_d,
  }
  pre_entry.main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(selected),
      },
    ),
    ct.Continue(
      pre_door.Args {
        input_index: 0,
        output_index: 0,
        transition_opening: carriage.direct_carried_membership_v1(step, 0),
      },
    ),
    fixture.own_out_ref(),
    Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(
        tf.header_asset_name_v1(claim.header),
        expected,
      ),
      redeemers: [
        fixture.phas_withdraw_redeemer_entry(
          step.phas_root,
          cbor.serialise(step.key),
          cbor.serialise(step.value),
        ),
      ],
    },
  )
}

fn pre_step(direction: Int, changed_index: Bool, changed_compact: Bool) -> Bool {
  pre_step_context(direction, changed_index, changed_compact, 0, False)
}

test actual_event_door_preserves_accepted_normal_custody() {
  event_step(0, False)
}

test actual_event_door_preserves_accepted_forced_custody() {
  event_step(1, False)
}

test actual_event_door_preserves_rejected_forced_custody() {
  event_step(2, False)
}

test event_door_cannot_write_another_selected_index() {
  !event_step(0, True)
}

test actual_transition_door_binds_normal_event_pre_root() {
  pre_step(0, False, False)
}

test actual_transition_door_binds_accepted_forced_event_pre_root() {
  pre_step(1, False, False)
}

test actual_transition_door_binds_rejected_forced_event_pre_root() {
  pre_step(2, False, False)
}

test transition_door_refuses_an_unauthenticated_selected_index() fail {
  pre_step(0, True, False)
}

test transition_door_cannot_erase_the_carried_compact_source() {
  !pre_step(0, False, True)
}

test transition_door_derives_body_network_mismatch_from_bound_source() {
  pre_step_context(0, False, False, 1, True)
}

test transition_door_does_not_misclassify_untagged_network_as_body_mismatch() {
  pre_step_context(0, False, False, 255, False)
}

test transition_door_refuses_erasing_a_body_network_mismatch() {
  !pre_step_context(0, False, False, 1, False)
}

test transition_door_refuses_inventing_a_body_network_mismatch() {
  !pre_step_context(0, False, False, 0, True)
}

test transition_door_derives_forced_network_context_before_header_is_dropped() {
  pre_step_context(2, False, False, 1, True)
}
