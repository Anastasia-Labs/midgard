use aiken/cbor
use aiken/crypto.{ScriptHash}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/input_existence_fixture_v1 as carriage_fixture
use midgard/fraud_proofs/input_existence_v1/pre_state
use midgard/fraud_proofs/input_existence_v1/types.{
  LedgerStateV1, PreStateArgsV1, SelectedInputStateV1,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/fraud_proofs/no_input/step_03.{Datum, SpendRedeemer}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/ledger_state
use midgard/transition_trace

validator main(
  step_04_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    pre_state.pre_state_step_v1(
      step_04_validator_script_hash,
      computation_thread_token_policy_id,
      datum,
      redeemer,
      own_out_ref,
      tx,
    )
  }

  else(_) {
    fail
  }
}

// Event-relative root controls execute this family's actual step-03 entry.

fn claim_v1() -> tf.ThreadClaimFixtureV1 {
  tf.thread_claim_v1(
    ledger_state.L2TransactionEventKey {
      tx_id: fixture.challenged_block_v1().tx_id,
    },
    ledger_state.L2Transaction,
    tf.h32_d,
    None,
  )
}

fn normal_subject_v1() -> substrate.VerdictSubjectV1 {
  substrate.VerdictSubjectV1 {
    version: 1,
    direction: 0,
    source_kind: 0,
    transaction_id: fixture.challenged_block_v1().tx_id,
    source_key: #"",
    rejection_reason: None,
  }
}

fn run_pre_state_v1(
  claim: tf.ThreadClaimFixtureV1,
  event: transition_trace.RootMembershipProof<
    ledger_state.EventKey,
    ledger_state.EventToStepValue,
  >,
  step: transition_trace.RootMembershipProof<Int, ledger_state.TransitionStep>,
  expected_root: ByteArray,
) -> Bool {
  let subject = normal_subject_v1()
  // Unit composition explicitly executes the crypto walk performed by the
  // separately merkelized verifier in a real transaction, for both claims.
  expect
    transition_trace.verify_root_membership_with_bytes(
      event,
      transition_trace.EventToStepRootDomain,
      claim.header.event_to_step_root,
      claim.header.total_event_count,
      cbor.serialise(event.key),
      cbor.serialise(event.value),
    )
  expect
    transition_trace.verify_root_membership_with_bytes(
      step,
      transition_trace.TransitionTraceRootDomain,
      claim.header.transition_trace_root,
      claim.header.transition_step_count,
      cbor.serialise(step.key),
      cbor.serialise(step.value),
    )
  let event_tx = Transaction {
    ..transaction.placeholder,
    redeemers: [
      fixture.phas_withdraw_redeemer_entry(
        event.phas_root,
        cbor.serialise(event.key),
        cbor.serialise(event.value),
      ),
    ],
  }
  let selected_step_index = substrate.subject_event_step_index_v1(
    claim.header,
    subject,
    carriage_fixture.direct_carried_membership_v1(event, 0),
    event_tx.reference_inputs,
    event_tx.redeemers,
  )
  let selected_input = Some(fixture.absent_midgard_tx_input())
  let input_state = SelectedInputStateV1 {
    compact: carriage_fixture.compact_view_v1(
      fixture.challenged_block_v1().compact,
    ),
    source_kind: 0,
    input_index: 0,
    claimed_input: None,
    subject,
    header: claim.header,
    selected_input,
    step_index: selected_step_index,
  }
  let expected: Data = LedgerStateV1 {
    compact: carriage_fixture.compact_view_v1(
      fixture.challenged_block_v1().compact,
    ),
    source_kind: 0,
    input_index: 0,
    claimed_input: None,
    subject,
    selected_input,
    pre_utxos_root: expected_root,
  }
  let name = tf.header_asset_name_v1(claim.header)
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(input_state),
      },
    ),
    ct.Continue(
      PreStateArgsV1 {
        input_index: 0,
        output_index: 0,
        transition_membership: carriage_fixture.direct_carried_membership_v1(
          step,
          0,
        ),
      },
    ),
    fixture.own_out_ref(),
    Transaction {
      ..fixture.continue_mid_step_l1_tx_v1(name, expected),
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

test event_pre_state_uses_committed_event_root_instead_of_block_initial_root() {
  let claim = claim_v1()
  claim.header.prev_utxos_root != tf.h32_d && run_pre_state_v1(
    claim,
    claim.event_to_step_membership,
    claim.transition_step_membership,
    tf.h32_d,
  )
}

test event_pre_state_rejects_forwarding_the_block_initial_root() fail {
  let claim = claim_v1()
  run_pre_state_v1(
    claim,
    claim.event_to_step_membership,
    claim.transition_step_membership,
    claim.header.prev_utxos_root,
  )
}

test event_pre_state_rejects_substituted_event_key() fail {
  let claim = claim_v1()
  run_pre_state_v1(
    claim,
    transition_trace.RootMembershipProof {
      ..claim.event_to_step_membership,
      key: ledger_state.L2TransactionEventKey { tx_id: tf.h32_c },
    },
    claim.transition_step_membership,
    tf.h32_d,
  )
}

test event_pre_state_rejects_substituted_step_root() fail {
  let claim = claim_v1()
  run_pre_state_v1(
    claim,
    claim.event_to_step_membership,
    transition_trace.RootMembershipProof {
      ..claim.transition_step_membership,
      root: tf.h32_c,
    },
    tf.h32_d,
  )
}

test event_pre_state_rejects_substituted_step_domain() fail {
  let claim = claim_v1()
  run_pre_state_v1(
    claim,
    claim.event_to_step_membership,
    transition_trace.RootMembershipProof {
      ..claim.transition_step_membership,
      domain: transition_trace.EventToStepRootDomain,
    },
    tf.h32_d,
  )
}

test event_pre_state_rejects_substituted_step_count() fail {
  let claim = claim_v1()
  run_pre_state_v1(
    claim,
    claim.event_to_step_membership,
    transition_trace.RootMembershipProof {
      ..claim.transition_step_membership,
      count: 2,
    },
    tf.h32_d,
  )
}

test event_pre_state_rejects_substituted_event_index() fail {
  let claim = claim_v1()
  run_pre_state_v1(
    claim,
    transition_trace.RootMembershipProof {
      ..claim.event_to_step_membership,
      value: ledger_state.EventToStepValue {
        ..claim.event_to_step_membership.value,
        step_index: 1,
      },
    },
    claim.transition_step_membership,
    tf.h32_d,
  )
}

test event_pre_state_rejects_substituted_step_pre_root() fail {
  let claim = claim_v1()
  run_pre_state_v1(
    claim,
    claim.event_to_step_membership,
    transition_trace.RootMembershipProof {
      ..claim.transition_step_membership,
      value: ledger_state.TransitionStep {
        ..claim.transition_step_membership.value,
        pre_utxos_root: tf.h32_c,
      },
    },
    tf.h32_d,
  )
}
