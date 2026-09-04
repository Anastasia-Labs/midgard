//// ## Native-Script Decoding Fault Proof — Step 02 (#635, #633)
////
//// Binds the thread to the committed claim: the disputed header rides the
//// redeemer and must hash to the thread NFT's asset-name tail; the event's
//// transition step and event-to-step leaf are opened under the header's
//// counted roots (verbatim mirrors of the frozen validation-claim
//// openings), fixing `prior_ledger_root := transition_step.pre_utxos_root`.
////
//// Forced threads additionally open the forced leaf — the explicit operator
//// verdict this thread disputes. Direction A requires `ForcedTxValid`;
//// direction B requires `ForcedTxInvalid` with one of the three decoding
//// arms, and copies the accused (source-kind, ordinal) pair verbatim from
//// the leaf's reason (2026-08-25 ruling). Direction A's pair is
//// prover-chosen.

use aiken/cbor
use aiken/crypto.{ScriptHash}
use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proof_catalogue
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/engine.{BindStateV1}
use midgard/fraud_proofs/native_script_decoding/step_02.{
  Args, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/ledger_state
use midgard/rejection_reason_v1.{ForcedTxInvalid, ForcedTxValid}
use midgard/transition_trace

validator main(
  step_03_open_subject_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_token_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args {
        input_index,
        output_index,
        header,
        event_to_step_membership,
        transition_step_membership,
        forced_membership,
        chosen_outpoint_source_kind,
        chosen_outpoint_cursor,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. Generic thread-advancement validations must pass.
        let
            _own_script_hash,
            computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data,
            output_script_hash,
            output_state_data
          <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )

        expect Some(BindStateV1 { direction, source_kind, verified_tx_id }) = m_input_state_data

        // 2. The header is the thread's: the NFT asset name is
        //    `category_id ‖ header_hash`, so nothing but the disputed block's
        //    header can pass. Every root opened below is this header's.
        let header_data: Data = header
        expect
          utils.serialise_and_hash_28(header_data) == bytearray.drop(
            computation_thread_token_asset_name,
            fraud_proof_catalogue.id_byte_count,
          )

        // 3. Open the committed claim and build the scan-thread state.
        let scan_state = if source_kind == engine.source_kind_normal {
          // A normal-source thread is structurally direction A: the leaf
          // bound at step-01 was an acceptance verdict.
          expect direction == engine.direction_wrongful_acceptance
          expect None = forced_membership
          expect and {
              or {
                chosen_outpoint_source_kind == engine.outpoint_source_spend,
                chosen_outpoint_source_kind == engine.outpoint_source_reference,
              },
              chosen_outpoint_cursor >= 0,
            }
          let event_key = ledger_state.L2TransactionEventKey {
            tx_id: verified_tx_id,
          }
          let prior_ledger_root = engine.verify_committed_pre_state_v1(
            header,
            event_key,
            event_to_step_membership,
            transition_step_membership,
          )
          engine.pre_bind_scan_state_v1(
            direction,
            source_kind,
            verified_tx_id,
            #"",
            engine.class_pending,
            prior_ledger_root,
            chosen_outpoint_source_kind,
            chosen_outpoint_cursor,
          )
        } else {
          expect source_kind == engine.source_kind_forced
          // Step-01's forced arm forwarded the sentinel; the leaf is the
          // subject's sole authority.
          expect verified_tx_id == #""
          expect Some(forced) = forced_membership
          let event_key = ledger_state.ForcedTransactionEventKey {
            tx_order_id: forced.key,
          }
          let prior_ledger_root = engine.verify_committed_pre_state_v1(
            header,
            event_key,
            event_to_step_membership,
            transition_step_membership,
          )
          let leaf = engine.verify_forced_leaf_v1(header, forced)
          let order_key_bytes = cbor.serialise(forced.key)
          if direction == engine.direction_wrongful_acceptance {
            // Direction A disputes an explicit acceptance; the accused
            // pair is prover-chosen (2026-08-25 ruling).
            expect ForcedTxValid = leaf.verdict
            expect and {
                or {
                  chosen_outpoint_source_kind == engine.outpoint_source_spend,
                  chosen_outpoint_source_kind == engine.outpoint_source_reference,
                },
                chosen_outpoint_cursor >= 0,
              }
            engine.pre_bind_scan_state_v1(
              direction,
              source_kind,
              leaf.tx_id,
              order_key_bytes,
              engine.class_pending,
              prior_ledger_root,
              chosen_outpoint_source_kind,
              chosen_outpoint_cursor,
            )
          } else {
            expect direction == engine.direction_wrongful_rejection
            // Direction B disputes an explicit rejection by one of this
            // family's three arms; the accused pair and class are copied
            // verbatim from the leaf (2026-08-25 ruling) — the chosen_*
            // redeemer fields are ignored. Deliberately unguarded: a pair
            // naming a subject outside the transaction's domain is
            // step-03's `BindOutOfDomain` close (#633 §7.2), so nothing
            // may filter it here.
            expect ForcedTxInvalid { reason } = leaf.verdict
            let accusation = engine.scan_accusation_of_v1(reason)
            engine.pre_bind_scan_state_v1(
              direction,
              source_kind,
              leaf.tx_id,
              order_key_bytes,
              accusation.scan_reason_class,
              prior_ledger_root,
              accusation.outpoint_source_kind,
              accusation.outpoint_cursor,
            )
          }
        }

        // 4. The thread advances to OpenSubject with the pre-bind scan state.
        expect output_script_hash == step_03_open_subject_validator_script_hash
        let expected_output_state: Data = scan_state
        expect output_state_data == expected_output_state

        True
      }
    }
  }

  else(_) {
    fail
  }
}

// ## Family selectors (GOAL_SPEC.md §9.1 output 4)
//
// Step-02 is where the thread stops trusting the prover about the claim: the
// header must be the NFT's, the event must be the header's, and — for forced
// threads — the disputed verdict must be the leaf's own.

const prior_root =
  #"5555555555555555555555555555555555555555555555555555555555555555"

fn step_datum_with(state: BindStateV1) -> Datum {
  ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(state),
  }
}

fn run_step_02(
  input_state: BindStateV1,
  claim: tf.ThreadClaimFixtureV1,
  asset_name: ByteArray,
  forced_membership: Option<
    transition_trace.RootMembershipProof<
      ledger_state.TxOrderId,
      ledger_state.ForcedInclusionTxV1,
    >,
  >,
  chosen_outpoint_source_kind: Int,
  chosen_outpoint_cursor: Int,
  expected_output_state: Data,
) -> Bool {
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(step_datum_with(input_state)),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        header: claim.header,
        event_to_step_membership: claim.event_to_step_membership,
        transition_step_membership: claim.transition_step_membership,
        forced_membership,
        chosen_outpoint_source_kind,
        chosen_outpoint_cursor,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
  )
}

fn normal_claim() -> (ByteArray, tf.ThreadClaimFixtureV1) {
  let tx_id = tf.native_tx_id_of_v1(tf.subject_tx_v1(native_types.TxIsValid))
  (
    tx_id,
    tf.thread_claim_v1(
      ledger_state.L2TransactionEventKey { tx_id },
      ledger_state.L2Transaction,
      prior_root,
      None,
    ),
  )
}

fn forced_claim(
  leaf: ledger_state.ForcedInclusionTxV1,
) -> (
  transition_trace.RootMembershipProof<
    ledger_state.TxOrderId,
    ledger_state.ForcedInclusionTxV1,
  >,
  tf.ThreadClaimFixtureV1,
) {
  let membership = tf.forced_leaf_membership_v1(tf.forced_order_key_v1(), leaf)
  (
    membership,
    tf.thread_claim_v1(
      ledger_state.ForcedTransactionEventKey {
        tx_order_id: tf.forced_order_key_v1(),
      },
      ledger_state.ForcedTransaction,
      prior_root,
      Some(membership.root),
    ),
  )
}

/// Positive selector, direction A over a normal subject: the committed claim
/// opens, the pre-state ledger root is frozen, and the prover-chosen pair
/// rides into the scan state.
test decoding_step_02_opens_a_normal_claim() {
  let (tx_id, claim) = normal_claim()
  let expected: Data = engine.pre_bind_scan_state_v1(
    engine.direction_wrongful_acceptance,
    engine.source_kind_normal,
    tx_id,
    #"",
    engine.class_pending,
    prior_root,
    engine.outpoint_source_spend,
    0,
  )
  run_step_02(
    BindStateV1 {
      direction: engine.direction_wrongful_acceptance,
      source_kind: engine.source_kind_normal,
      verified_tx_id: tx_id,
    },
    claim,
    tf.header_asset_name_v1(claim.header),
    None,
    engine.outpoint_source_spend,
    0,
    expected,
  )
}

/// Positive selector, direction A over a forced subject: the leaf's explicit
/// `ForcedTxValid` verdict is the disputed acceptance; the prover chooses
/// the accused pair.
test decoding_step_02_opens_a_forced_acceptance() {
  let leaf = tf.forced_leaf_v1(
    tf.subject_tx_v1(native_types.TxIsValid),
    ForcedTxValid,
  )
  let (membership, claim) = forced_claim(leaf)
  let expected: Data = engine.pre_bind_scan_state_v1(
    engine.direction_wrongful_acceptance,
    engine.source_kind_forced,
    leaf.tx_id,
    cbor.serialise(tf.forced_order_key_v1()),
    engine.class_pending,
    prior_root,
    engine.outpoint_source_reference,
    0,
  )
  run_step_02(
    BindStateV1 {
      direction: engine.direction_wrongful_acceptance,
      source_kind: engine.source_kind_forced,
      verified_tx_id: #"",
    },
    claim,
    tf.header_asset_name_v1(claim.header),
    Some(membership),
    engine.outpoint_source_reference,
    0,
    expected,
  )
}

fn run_step_02_rejection(
  reason: rejection_reason_v1.RejectionReasonV1,
  expected_class: Int,
  expected_kind: Int,
  expected_cursor: Int,
) -> Bool {
  let leaf = tf.forced_leaf_v1(
    tf.subject_tx_v1(native_types.TxIsInvalid),
    ForcedTxInvalid { reason },
  )
  let (membership, claim) = forced_claim(leaf)
  let expected: Data = engine.pre_bind_scan_state_v1(
    engine.direction_wrongful_rejection,
    engine.source_kind_forced,
    leaf.tx_id,
    cbor.serialise(tf.forced_order_key_v1()),
    expected_class,
    prior_root,
    expected_kind,
    expected_cursor,
  )
  run_step_02(
    BindStateV1 {
      direction: engine.direction_wrongful_rejection,
      source_kind: engine.source_kind_forced,
      verified_tx_id: #"",
    },
    claim,
    tf.header_asset_name_v1(claim.header),
    Some(membership),
    // The chosen_* fields are direction-A's; for B they are ignored and the
    // pair below comes from the leaf.
    engine.outpoint_source_spend,
    9,
    expected,
  )
}

/// Positive selector, direction B: the leaf's `Malformed` accusation — class
/// and pair — is copied verbatim into the scan state, ignoring the redeemer's
/// chosen pair.
test decoding_step_02_copies_a_malformed_accusation_verbatim() {
  run_step_02_rejection(
    rejection_reason_v1.ResolvedReferenceScriptMalformed {
      source_kind: 1,
      input_index: 0,
    },
    engine.refusal_class_malformed,
    engine.outpoint_source_reference,
    0,
  )
}

/// Positive selector, direction B: the `NodeLimit` arm maps to class 1 with
/// the same verbatim pair copy.
test decoding_step_02_copies_a_node_limit_accusation_verbatim() {
  run_step_02_rejection(
    rejection_reason_v1.ResolvedReferenceScriptNodeLimit {
      source_kind: 0,
      input_index: 0,
    },
    engine.refusal_class_node_limit,
    engine.outpoint_source_spend,
    0,
  )
}

/// A rejection outside this family's three arms cannot seed a direction-B
/// thread.
test decoding_step_02_rejects_a_foreign_rejection_arm() fail {
  run_step_02_rejection(
    rejection_reason_v1.FeeBelowMinimum,
    engine.refusal_class_malformed,
    engine.outpoint_source_spend,
    0,
  )
}

/// Direction B needs an explicit rejection: an accepting leaf cannot seed it.
test decoding_step_02_rejects_direction_b_on_an_accepting_leaf() fail {
  let leaf = tf.forced_leaf_v1(
    tf.subject_tx_v1(native_types.TxIsValid),
    ForcedTxValid,
  )
  let (membership, claim) = forced_claim(leaf)
  let expected: Data = engine.pre_bind_scan_state_v1(
    engine.direction_wrongful_rejection,
    engine.source_kind_forced,
    leaf.tx_id,
    cbor.serialise(tf.forced_order_key_v1()),
    engine.refusal_class_malformed,
    prior_root,
    engine.outpoint_source_spend,
    0,
  )
  run_step_02(
    BindStateV1 {
      direction: engine.direction_wrongful_rejection,
      source_kind: engine.source_kind_forced,
      verified_tx_id: #"",
    },
    claim,
    tf.header_asset_name_v1(claim.header),
    Some(membership),
    engine.outpoint_source_spend,
    0,
    expected,
  )
}

/// Direction A over a forced subject needs an explicit acceptance: a
/// rejecting leaf cannot seed it.
test decoding_step_02_rejects_direction_a_on_a_rejecting_leaf() fail {
  let leaf = tf.forced_leaf_v1(
    tf.subject_tx_v1(native_types.TxIsInvalid),
    ForcedTxInvalid {
      reason: rejection_reason_v1.ResolvedReferenceScriptMalformed {
        source_kind: 0,
        input_index: 0,
      },
    },
  )
  let (membership, claim) = forced_claim(leaf)
  let expected: Data = engine.pre_bind_scan_state_v1(
    engine.direction_wrongful_acceptance,
    engine.source_kind_forced,
    leaf.tx_id,
    cbor.serialise(tf.forced_order_key_v1()),
    engine.class_pending,
    prior_root,
    engine.outpoint_source_spend,
    0,
  )
  run_step_02(
    BindStateV1 {
      direction: engine.direction_wrongful_acceptance,
      source_kind: engine.source_kind_forced,
      verified_tx_id: #"",
    },
    claim,
    tf.header_asset_name_v1(claim.header),
    Some(membership),
    engine.outpoint_source_spend,
    0,
    expected,
  )
}

/// §2.4.3(e) leaf-internal authoritativeness: a leaf whose verdict disagrees
/// with its own compact bytes' validity scalar never opens.
test decoding_step_02_rejects_a_verdict_scalar_mismatch() fail {
  let leaf = tf.forced_leaf_v1(
    tf.subject_tx_v1(native_types.TxIsInvalid),
    // TxIsInvalid bytes, accepting verdict: (e) must refuse.
    ForcedTxValid,
  )
  let (membership, claim) = forced_claim(leaf)
  let expected: Data = engine.pre_bind_scan_state_v1(
    engine.direction_wrongful_acceptance,
    engine.source_kind_forced,
    leaf.tx_id,
    cbor.serialise(tf.forced_order_key_v1()),
    engine.class_pending,
    prior_root,
    engine.outpoint_source_spend,
    0,
  )
  run_step_02(
    BindStateV1 {
      direction: engine.direction_wrongful_acceptance,
      source_kind: engine.source_kind_forced,
      verified_tx_id: #"",
    },
    claim,
    tf.header_asset_name_v1(claim.header),
    Some(membership),
    engine.outpoint_source_spend,
    0,
    expected,
  )
}

/// The header must be the thread NFT's: the same claim under another
/// header's asset name never binds.
test decoding_step_02_rejects_a_header_that_is_not_the_threads() fail {
  let (tx_id, claim) = normal_claim()
  let other_claim = tf.thread_claim_v1(
    ledger_state.L2TransactionEventKey { tx_id },
    ledger_state.L2Transaction,
    tf.h32_d,
    None,
  )
  let expected: Data = engine.pre_bind_scan_state_v1(
    engine.direction_wrongful_acceptance,
    engine.source_kind_normal,
    tx_id,
    #"",
    engine.class_pending,
    prior_root,
    engine.outpoint_source_spend,
    0,
  )
  run_step_02(
    BindStateV1 {
      direction: engine.direction_wrongful_acceptance,
      source_kind: engine.source_kind_normal,
      verified_tx_id: tx_id,
    },
    claim,
    tf.header_asset_name_v1(other_claim.header),
    None,
    engine.outpoint_source_spend,
    0,
    expected,
  )
}

/// The event's committed phase must be the key's: an event-to-step leaf
/// carrying the wrong phase fails the cross-checks.
test decoding_step_02_rejects_a_phase_mismatch() fail {
  let tx_id = tf.native_tx_id_of_v1(tf.subject_tx_v1(native_types.TxIsValid))
  let claim = tf.thread_claim_v1(
    ledger_state.L2TransactionEventKey { tx_id },
    ledger_state.Deposit,
    prior_root,
    None,
  )
  let expected: Data = engine.pre_bind_scan_state_v1(
    engine.direction_wrongful_acceptance,
    engine.source_kind_normal,
    tx_id,
    #"",
    engine.class_pending,
    prior_root,
    engine.outpoint_source_spend,
    0,
  )
  run_step_02(
    BindStateV1 {
      direction: engine.direction_wrongful_acceptance,
      source_kind: engine.source_kind_normal,
      verified_tx_id: tx_id,
    },
    claim,
    tf.header_asset_name_v1(claim.header),
    None,
    engine.outpoint_source_spend,
    0,
    expected,
  )
}

/// A normal-source thread is direction A by construction.
test decoding_step_02_rejects_direction_b_on_a_normal_source() fail {
  let (tx_id, claim) = normal_claim()
  let expected: Data = engine.pre_bind_scan_state_v1(
    engine.direction_wrongful_rejection,
    engine.source_kind_normal,
    tx_id,
    #"",
    engine.class_pending,
    prior_root,
    engine.outpoint_source_spend,
    0,
  )
  run_step_02(
    BindStateV1 {
      direction: engine.direction_wrongful_rejection,
      source_kind: engine.source_kind_normal,
      verified_tx_id: tx_id,
    },
    claim,
    tf.header_asset_name_v1(claim.header),
    None,
    engine.outpoint_source_spend,
    0,
    expected,
  )
}

// ## Cancellation

fn run_cancel(signed_by_prover: Bool) -> Bool {
  let (tx_id, claim) = normal_claim()
  let asset_name = tf.header_asset_name_v1(claim.header)
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      step_datum_with(
        BindStateV1 {
          direction: engine.direction_wrongful_acceptance,
          source_kind: engine.source_kind_normal,
          verified_tx_id: tx_id,
        },
      ),
    ),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(asset_name, asset_name, signed_by_prover),
  )
}

test decoding_step_02_cancels_under_the_prover_signature() {
  run_cancel(True)
}

test decoding_step_02_rejects_an_unsigned_cancellation() fail {
  run_cancel(False)
}
