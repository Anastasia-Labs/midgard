//// ## Native-Script Decoding Fault Proof — Step 03 / Open Subject
////
//// Opens the accused transaction field through the authenticated §8.8 door.
//// An in-domain ordinal commits the exact canonical outpoint key for the
//// descriptor validator. An out-of-domain accusation closes only for
//// direction B with the class-0 contradiction marker.

use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, reference_inputs_field_index,
  spend_inputs_field_index,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/engine.{ScanThreadStateV1}
use midgard/fraud_proofs/native_script_decoding/step_03.{
  Datum, OpenSubjectArgs, OpenSubjectSpendRedeemer,
}
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/native_tx_machine_walk_v1.{spend_input_at, spend_input_count}

validator main(
  step_03_bind_descriptor_validator_script_hash: ScriptHash,
  step_04_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: OpenSubjectSpendRedeemer,
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
      ct.Continue(OpenSubjectArgs {
        input_index,
        output_index,
        subject_field_opening,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum

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
        expect Some(state) = m_input_state_data

        // The subject is opened once, directly from step-02's sentinel state.
        expect and {
            state.machine_state_hash == #"",
            state.refusal_class == engine.class_pending,
            state.outpoint_key_hash == #"",
            state.reference_script_language == engine.language_unbound,
            state.output_index == -1,
            state.total_length == -1,
            state.item_commitment == #"",
          }

        // Invalid field tags and negative ordinals are out of domain without
        // an opening. A named field with a non-negative ordinal always carries
        // its opening so the authenticated item count decides the branch.
        let subject_outpoint = if or {
          and {
            state.outpoint_source_kind != engine.outpoint_source_spend,
            state.outpoint_source_kind != engine.outpoint_source_reference,
          },
          state.outpoint_cursor < 0,
        } {
          expect None = subject_field_opening
          None
        } else {
          expect Some(opening) = subject_field_opening
          let subject_view = opened_field_view(
            opening,
            BodyAnchor { tx_id: state.verified_tx_id },
            if state.outpoint_source_kind == engine.outpoint_source_spend {
              spend_inputs_field_index
            } else {
              reference_inputs_field_index
            },
            reference_inputs,
            field_preimage_certificate_policy_id,
          )
          if state.outpoint_cursor >= spend_input_count(subject_view) {
            None
          } else {
            Some(spend_input_at(subject_view, state.outpoint_cursor))
          }
        }

        when subject_outpoint is {
          None -> {
            // Only a forced direction-B leaf can make an out-of-domain
            // accusation. Its impossibility closes directly to step-04.
            expect state.direction == engine.direction_wrongful_rejection
            expect output_script_hash == step_04_validator_script_hash
            let expected_output_state: Data = ScanThreadStateV1 {
              ..state,
              refusal_class: engine.refusal_class_malformed,
            }
            expect output_state_data == expected_output_state
            True
          }
          Some(outpoint) -> {
            let outpoint_key_cbor = encode_midgard_tx_input(outpoint)
            expect
              output_script_hash == step_03_bind_descriptor_validator_script_hash
            let expected_output_state: Data = engine.opened_subject_scan_state_v1(
              state,
              outpoint_key_cbor,
              outpoint.output_index,
            )
            expect output_state_data == expected_output_state
            True
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn open_subject_tx() -> native_types.MidgardTransaction {
  tf.subject_tx_v1(native_types.TxIsValid)
}

fn open_subject_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(open_subject_tx())
}

fn open_subject_state(direction: Int, cursor: Int) -> ScanThreadStateV1 {
  let block = open_subject_block()
  engine.pre_bind_scan_state_v1(
    direction,
    if direction == engine.direction_wrongful_rejection {
      engine.source_kind_forced
    } else {
      engine.source_kind_normal
    },
    block.tx_id,
    if direction == engine.direction_wrongful_rejection {
      tf.h32_d
    } else {
      #""
    },
    engine.class_pending,
    tf.h32_c,
    engine.outpoint_source_spend,
    cursor,
  )
}

fn run_open_subject(
  state: ScanThreadStateV1,
  args: OpenSubjectArgs,
  expected_output_state: Data,
) -> Bool {
  let asset_name = fixture.ct_token_asset_name(open_subject_block())
  main.spend(
    fixture.next_step_script_hash,
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(args),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
  )
}

fn spend_field_opening() {
  let block = open_subject_block()
  fixture.inline_body_opening(
    block.compact_cbor,
    tf.subject_spend_field_items_v1(open_subject_tx()),
  )
}

test open_subject_commits_the_authenticated_outpoint() {
  let tx = open_subject_tx()
  expect Some(outpoint) = list.at(tx.body.inputs, 0)
  let state = open_subject_state(engine.direction_wrongful_acceptance, 0)
  let expected: Data = engine.opened_subject_scan_state_v1(
    state,
    encode_midgard_tx_input(outpoint),
    outpoint.output_index,
  )
  run_open_subject(
    state,
    OpenSubjectArgs {
      input_index: 0,
      output_index: 0,
      subject_field_opening: Some(spend_field_opening()),
    },
    expected,
  )
}

test open_subject_closes_an_out_of_domain_direction_b_ordinal() {
  let state = open_subject_state(engine.direction_wrongful_rejection, 1)
  let expected: Data = ScanThreadStateV1 {
    ..state,
    refusal_class: engine.refusal_class_malformed,
  }
  run_open_subject(
    state,
    OpenSubjectArgs {
      input_index: 0,
      output_index: 0,
      subject_field_opening: Some(spend_field_opening()),
    },
    expected,
  )
}

test open_subject_rejects_an_out_of_domain_direction_a_ordinal() fail {
  let state = open_subject_state(engine.direction_wrongful_acceptance, 1)
  let expected: Data = ScanThreadStateV1 {
    ..state,
    refusal_class: engine.refusal_class_malformed,
  }
  run_open_subject(
    state,
    OpenSubjectArgs {
      input_index: 0,
      output_index: 0,
      subject_field_opening: Some(spend_field_opening()),
    },
    expected,
  )
}

test open_subject_rejects_a_second_open() fail {
  let tx = open_subject_tx()
  expect Some(outpoint) = list.at(tx.body.inputs, 0)
  let state = engine.opened_subject_scan_state_v1(
    open_subject_state(engine.direction_wrongful_acceptance, 0),
    encode_midgard_tx_input(outpoint),
    outpoint.output_index,
  )
  let expected: Data = state
  run_open_subject(
    state,
    OpenSubjectArgs {
      input_index: 0,
      output_index: 0,
      subject_field_opening: Some(spend_field_opening()),
    },
    expected,
  )
}
