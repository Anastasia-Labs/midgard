//// ## Native-Script Decoding Fault Proof — Step 04 (#635, #633)
////
//// The thread's proven verdict finalizes into a permanent fraud-proof token.
//// Everything substantive was proven upstream; this step checks the classed
//// state's shape per direction:
////
//// - Direction A (wrongful acceptance): the scan refused with some class in
////   {0, 1, 2} — the accepted transaction resolved an undecodable, or
////   limit-breaking, native reference script.
//// - Direction B (wrongful rejection): only a forced leaf carries an
////   explicit rejection to dispute; the accused class must itself be in the
////   family's domain and the thread must have closed with the class-0
////   contradiction marker (exact terminal or descriptor contradiction).

use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/engine.{ScanThreadStateV1}
use midgard/fraud_proofs/native_script_decoding/step_04.{
  Args, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf

validator main(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
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
        fraud_proof_mint_redeemer_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx

        expect Some(step_datum) = datum

        // 1. The thread NFT burns and the fraud-proof token mints to the
        //    permanent address under the generic finalization validations.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data
          <- finalize(
          computation_thread_token_policy_id,
          fraud_proof_token_policy_id,
          fraud_proof_token_address,
          step_datum,
          input_index,
          output_index,
          fraud_proof_mint_redeemer_index,
          own_out_ref,
          inputs,
          outputs,
          redeemers,
        )

        expect Some(state) = m_input_state_data

        // 2. The classed state must be a closed verdict of its direction.
        if state.direction == engine.direction_wrongful_rejection {
          expect and {
              state.source_kind == engine.source_kind_forced,
              state.refusal_class == engine.refusal_class_malformed,
              or {
                state.scan_reason_class == engine.refusal_class_malformed,
                state.scan_reason_class == engine.refusal_class_node_limit,
                state.scan_reason_class == engine.refusal_class_depth_limit,
              },
            }
        } else {
          expect state.direction == engine.direction_wrongful_acceptance
          expect or {
              state.refusal_class == engine.refusal_class_malformed,
              state.refusal_class == engine.refusal_class_node_limit,
              state.refusal_class == engine.refusal_class_depth_limit,
            }
        }

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
// Step-04 admits only closed verdicts: a pending class, an out-of-domain
// class, or a direction-B state whose provenance or marker is wrong all die
// here even if the L1 finalization mechanics are satisfied.

fn proven_state(
  direction: Int,
  source_kind: Int,
  scan_reason_class: Int,
  refusal_class: Int,
) -> ScanThreadStateV1 {
  ScanThreadStateV1 {
    ..engine.pre_bind_scan_state_v1(
      direction,
      source_kind,
      tf.h32_c,
      if source_kind == engine.source_kind_forced {
        tf.h32_d
      } else {
        #""
      },
      scan_reason_class,
      tf.h32_c,
      engine.outpoint_source_spend,
      0,
    ),
    refusal_class,
  }
}

fn run_step_04(state: ScanThreadStateV1) -> Bool {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let asset_name = fixture.ct_token_asset_name(block)
  main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

/// Positive selector, direction A over a normal subject: a class-0 refusal
/// finalizes.
test decoding_step_04_finalizes_a_wrongful_acceptance() {
  run_step_04(
    proven_state(
      engine.direction_wrongful_acceptance,
      engine.source_kind_normal,
      engine.class_pending,
      engine.refusal_class_malformed,
    ),
  )
}

/// Positive selector, direction A over a forced subject: the limit classes
/// finalize the same way.
test decoding_step_04_finalizes_a_forced_limit_refusal() {
  and {
    run_step_04(
      proven_state(
        engine.direction_wrongful_acceptance,
        engine.source_kind_forced,
        engine.class_pending,
        engine.refusal_class_node_limit,
      ),
    ),
    run_step_04(
      proven_state(
        engine.direction_wrongful_acceptance,
        engine.source_kind_forced,
        engine.class_pending,
        engine.refusal_class_depth_limit,
      ),
    ),
  }
}

/// Positive selector, direction B: a forced rejection accused of a
/// depth-limit decoding fault, contradicted by the class-0 marker.
test decoding_step_04_finalizes_a_wrongful_rejection() {
  run_step_04(
    proven_state(
      engine.direction_wrongful_rejection,
      engine.source_kind_forced,
      engine.refusal_class_depth_limit,
      engine.refusal_class_malformed,
    ),
  )
}

/// A thread that never reached a verdict cannot finalize.
test decoding_step_04_rejects_a_pending_class() fail {
  run_step_04(
    proven_state(
      engine.direction_wrongful_acceptance,
      engine.source_kind_normal,
      engine.class_pending,
      engine.class_pending,
    ),
  )
}

/// The refusal class domain is exactly {0, 1, 2}.
test decoding_step_04_rejects_an_out_of_domain_class() fail {
  run_step_04(
    proven_state(
      engine.direction_wrongful_acceptance,
      engine.source_kind_normal,
      engine.class_pending,
      3,
    ),
  )
}

/// Direction B closes only through the class-0 contradiction marker.
test decoding_step_04_rejects_direction_b_without_the_marker() fail {
  run_step_04(
    proven_state(
      engine.direction_wrongful_rejection,
      engine.source_kind_forced,
      engine.refusal_class_node_limit,
      engine.refusal_class_node_limit,
    ),
  )
}

/// Direction B disputes explicit rejections, which only forced leaves carry.
test decoding_step_04_rejects_direction_b_on_a_normal_source() fail {
  run_step_04(
    proven_state(
      engine.direction_wrongful_rejection,
      engine.source_kind_normal,
      engine.refusal_class_malformed,
      engine.refusal_class_malformed,
    ),
  )
}

/// Direction B's accused class must itself be in the family's domain.
test decoding_step_04_rejects_an_out_of_domain_accusation() fail {
  run_step_04(
    proven_state(
      engine.direction_wrongful_rejection,
      engine.source_kind_forced,
      3,
      engine.refusal_class_malformed,
    ),
  )
}

// ## Cancellation

fn run_cancel(signed_by_prover: Bool) -> Bool {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let asset_name = fixture.ct_token_asset_name(block)
  main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          proven_state(
            engine.direction_wrongful_acceptance,
            engine.source_kind_normal,
            engine.class_pending,
            engine.class_pending,
          ),
        ),
      },
    ),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(asset_name, asset_name, signed_by_prover),
  )
}

/// The prover may abandon the thread even at the final step.
test decoding_step_04_cancels_under_the_prover_signature() {
  run_cancel(True)
}

/// Nobody else may.
test decoding_step_04_rejects_an_unsigned_cancellation() fail {
  run_cancel(False)
}
