//// ## Mint-Authorization Fault Proof — Step 04 (W-C14)
////
//// Direction A's reference-input scan (self-loop). The machine's stage-nine
//// source surface has two halves: the inline field-6 witnesses (cleared by
//// step-03) and the reference scripts of the RESOLVED reference inputs —
//// each field-1 outpoint looked up in the transition step's
//// `pre_utxos_root`, contributing a source exactly when its descriptor's
//// `reference_script_language != -1`, matched by
//// `reference_script_hash` alone across every language.
////
//// Each `ResolveNext` reproduces one machine resolution: the committed
//// field-1 item at the cursor, its canonical outpoint key, the MPF
//// membership of the descriptor bytes under the thread-carried pre-state
//// root — the lookup is deterministic, so these ARE the bytes the machine
//// read — and the cleared-source predicate (`language == -1` or
//// `hash != policy_id`). `AdvanceComplete` closes the scan only when the
//// cursor equals the authenticated item count, so the absence claim covers
//// the complete surface with no gap for a prover to skip past.
////
//// The scan deliberately does NOT cross-check
//// `descriptor.output_index == outpoint.output_index`: absence must hold
//// over what the machine actually consulted, which is the trie's answer for
//// the key, consistent or not — and refusing an inconsistent committed
//// descriptor here would strand a genuine mint-authorization conviction on
//// a fault that belongs to a ledger-construction family. A descriptor whose
//// bytes fail canonical decode aborts the walk (refusal, never a wrongful
//// conviction): that is the decoding boundary, recorded in the plan.

use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, reference_inputs_field_index,
}
use midgard/fraud_proofs/mint_authorization/engine
use midgard/fraud_proofs/mint_authorization/step_04.{
  AdvanceComplete, Args, Datum, ResolveNext, SpendRedeemer, State,
}
use midgard/fraud_proofs/mint_authorization/step_05
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/ledger_output_commitment_v1
use midgard/mpf_proof_v1
use midgard/native_tx_field_access_v1.{field_item_count}
use midgard/native_tx_machine_walk_v1.{spend_input_at}

validator main(
  step_05_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
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
      ct.Continue(ResolveNext {
        input_index,
        output_index,
        reference_inputs_opening,
        descriptor_cbor,
        ledger_membership_proof,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. Generic thread-advancement validations must pass.
        let
            own_script_hash,
            _computation_thread_token_asset_name,
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

        expect
            Some(State { policy_id, bad_tx_id, prior_ledger_root, ref_cursor })
          = m_input_state_data

        // 2. The committed field-1 item at the cursor, through the §8.8
        //    door. §7.3 abort-never-clamp: an ordinal past the committed
        //    collection fails the read.
        let reference_inputs_view = opened_field_view(
          reference_inputs_opening,
          BodyAnchor { tx_id: bad_tx_id },
          reference_inputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        expect and {
            ref_cursor >= 0,
            ref_cursor < field_item_count(reference_inputs_view),
          }
        let outpoint = spend_input_at(reference_inputs_view, ref_cursor)
        let outpoint_key_bytes = encode_midgard_tx_input(outpoint)

        // 3. The descriptor is the pre-state ledger's own resolution of the
        //    outpoint — the deterministic trie answer the machine read.
        expect
          mpf_proof_v1.has(
            prior_ledger_root,
            outpoint_key_bytes,
            descriptor_cbor,
            ledger_membership_proof,
          )
        let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)

        // 4. The cleared-source predicate: this resolved output contributes
        //    no reference script of the claimed policy hash (any language).
        expect or {
            descriptor.reference_script_language == -1,
            descriptor.reference_script_hash != policy_id,
          }

        // 5. Self-loop with the cursor advanced.
        expect output_script_hash == own_script_hash
        let expected_output_state: Data = State {
          policy_id,
          bad_tx_id,
          prior_ledger_root,
          ref_cursor: ref_cursor + 1,
        }
        expect output_state_data == expected_output_state

        // Done.
        True
      }
      ct.Continue(AdvanceComplete {
        input_index,
        output_index,
        reference_inputs_opening,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. Generic thread-advancement validations must pass.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
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

        expect
            Some(State {
              policy_id,
              bad_tx_id,
              prior_ledger_root: _,
              ref_cursor,
            })
          = m_input_state_data

        // 2. The scan is complete exactly when the cursor equals the
        //    authenticated field-1 item count: every resolved reference
        //    script has been cleared, and with step-03's inline half the
        //    machine-consulted source surface is exhausted.
        let reference_inputs_view = opened_field_view(
          reference_inputs_opening,
          BodyAnchor { tx_id: bad_tx_id },
          reference_inputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        expect ref_cursor == field_item_count(reference_inputs_view)

        // 3. The absence claim is decided: close to the terminal step.
        expect output_script_hash == step_05_validator_script_hash
        let expected_output_state: Data = step_05.State {
          policy_id,
          direction: engine.direction_script_absent,
        }
        expect output_state_data == expected_output_state

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

// ## Family selectors (GOAL_SPEC.md §9.1 output 4)

/// A 28-byte policy id matching no fixture descriptor's reference-script
/// hash (the descriptors that carry one carry `tf.h28_y`).
const accused_policy_id: ByteArray =
  #"42424242424242424242424242424242424242424242424242424242"

fn challenged_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

/// The fixture transaction's single reference input.
fn fixture_outpoint_key(block: fixture.NativeBlockFixtureV1) -> ByteArray {
  expect Some(outpoint) = list.at(block.tx.body.reference_inputs, 0)
  encode_midgard_tx_input(outpoint)
}

fn field_1_items(block: fixture.NativeBlockFixtureV1) -> List<ByteArray> {
  block.tx.body.reference_inputs |> list.map(encode_midgard_tx_input)
}

/// A resolved-output descriptor: `language == -1` carries no reference
/// script; any other language carries hash `tf.h28_y`.
fn descriptor_cbor_of(language: Int) -> ByteArray {
  ledger_output_commitment_v1.encode(
    tf.scan_descriptor_v1(1, language, fixture.sample_native_script_bytes()),
  )
}

fn run_resolve(
  policy_id: ByteArray,
  descriptor_cbor: ByteArray,
  prior_ledger_root: ByteArray,
  ref_cursor: Int,
  carried_items: List<ByteArray>,
) -> Bool {
  let block = challenged_block()
  let state = State {
    policy_id,
    bad_tx_id: block.tx_id,
    prior_ledger_root,
    ref_cursor,
  }
  let expected_output_state: Data = State {
    policy_id,
    bad_tx_id: block.tx_id,
    prior_ledger_root,
    ref_cursor: ref_cursor + 1,
  }
  let asset_name = fixture.ct_token_asset_name(block)
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(
      ResolveNext {
        input_index: 0,
        output_index: 0,
        reference_inputs_opening: fixture.inline_body_opening(
          block.compact_cbor,
          carried_items,
        ),
        descriptor_cbor,
        ledger_membership_proof: [],
      },
    ),
    fixture.own_out_ref(),
    tf.self_loop_l1_tx_v1(asset_name, expected_output_state),
  )
}

fn run_resolve_language(policy_id: ByteArray, language: Int) -> Bool {
  let block = challenged_block()
  let descriptor_cbor = descriptor_cbor_of(language)
  run_resolve(
    policy_id,
    descriptor_cbor,
    tf.single_leaf_ledger_root_v1(fixture_outpoint_key(block), descriptor_cbor),
    0,
    field_1_items(block),
  )
}

/// A resolved output with no reference script at all clears: the machine
/// drew no source from it.
test mint_authorization_step_04_clears_a_scriptless_resolved_output() {
  run_resolve_language(accused_policy_id, -1)
}

/// A resolved output whose reference script hashes to a DIFFERENT policy
/// clears: hash matching is exact.
test mint_authorization_step_04_clears_a_foreign_reference_script() {
  run_resolve_language(accused_policy_id, 0)
}

/// A resolved output carrying a reference script of the claimed policy hash
/// refuses — whatever its language, the machine had the source, so the
/// absence accusation is false.
test mint_authorization_step_04_rejects_a_resolved_policy_source() fail {
  run_resolve_language(tf.h28_y, 3)
}

/// The cursor is bounded by the authenticated field-1 item count: an
/// ordinal past the committed collection fails the read.
test mint_authorization_step_04_rejects_an_out_of_range_cursor() fail {
  let block = challenged_block()
  let descriptor_cbor = descriptor_cbor_of(-1)
  run_resolve(
    accused_policy_id,
    descriptor_cbor,
    tf.single_leaf_ledger_root_v1(fixture_outpoint_key(block), descriptor_cbor),
    1,
    field_1_items(block),
  )
}

/// The descriptor must be the pre-state trie's answer for the outpoint: a
/// membership claim against a root that maps the key elsewhere fails.
test mint_authorization_step_04_rejects_a_non_member_descriptor() fail {
  let block = challenged_block()
  run_resolve(
    accused_policy_id,
    descriptor_cbor_of(-1),
    tf.single_leaf_ledger_root_v1(
      fixture_outpoint_key(block),
      descriptor_cbor_of(0),
    ),
    0,
    field_1_items(block),
  )
}

/// The scanned collection is only as good as the walked preimage: a
/// truncated field-1 carriage fails the door's authenticate-once check.
test mint_authorization_step_04_rejects_a_truncated_field_1_preimage() fail {
  let block = challenged_block()
  let descriptor_cbor = descriptor_cbor_of(-1)
  run_resolve(
    accused_policy_id,
    descriptor_cbor,
    tf.single_leaf_ledger_root_v1(fixture_outpoint_key(block), descriptor_cbor),
    0,
    [],
  )
}

// ### Completion

fn run_complete(ref_cursor: Int, carried_items: List<ByteArray>) -> Bool {
  let block = challenged_block()
  let state = State {
    policy_id: accused_policy_id,
    bad_tx_id: block.tx_id,
    prior_ledger_root: tf.h32_c,
    ref_cursor,
  }
  let expected_output_state: Data = step_05.State {
    policy_id: accused_policy_id,
    direction: engine.direction_script_absent,
  }
  let asset_name = fixture.ct_token_asset_name(block)
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(
      AdvanceComplete {
        input_index: 0,
        output_index: 0,
        reference_inputs_opening: fixture.inline_body_opening(
          block.compact_cbor,
          carried_items,
        ),
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
  )
}

/// Once the cursor has covered the authenticated item count the scan closes
/// to the terminal step with the direction-A verdict.
test mint_authorization_step_04_completes_at_the_item_count() {
  let block = challenged_block()
  run_complete(1, field_1_items(block))
}

/// The scan cannot close early: a cursor short of the item count leaves
/// unresolved reference inputs the policy could hide in.
test mint_authorization_step_04_rejects_an_early_completion() fail {
  let block = challenged_block()
  run_complete(0, field_1_items(block))
}

/// Nor can the count itself be shrunk: a truncated field-1 carriage fails
/// the door before the count is read.
test mint_authorization_step_04_rejects_completion_over_a_truncated_preimage() fail {
  run_complete(0, [])
}

// ## Cancellation

fn run_cancel(signed_by_prover: Bool) -> Bool {
  let block = challenged_block()
  let asset_name = fixture.ct_token_asset_name(block)
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          State {
            policy_id: accused_policy_id,
            bad_tx_id: block.tx_id,
            prior_ledger_root: tf.h32_c,
            ref_cursor: 0,
          },
        ),
      },
    ),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(asset_name, asset_name, signed_by_prover),
  )
}

/// The prover may abandon the thread.
test mint_authorization_step_04_cancels_under_the_prover_signature() {
  run_cancel(True)
}

/// Nobody else may.
test mint_authorization_step_04_rejects_an_unsigned_cancellation() fail {
  run_cancel(False)
}
