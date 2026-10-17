//// ## Native-Script Decoding Fault Proof — Step 03 / Advance Or Close
////
//// Runs the frozen native-script scan under a prover-chosen primitive-step
//// budget. An advancing machine self-loops unless direction B reaches the
//// exact terminal. A direction-A refusal closes with its proven class.

use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/engine.{ScanThreadStateV1}
use midgard/fraud_proofs/native_script_decoding/step_03.{
  AdvanceOrCloseArgs, AdvanceOrCloseSpendRedeemer, Datum,
}
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/ledger_output_commitment_v1
use midgard/native_script_scan_v1

validator main(
  step_04_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: AdvanceOrCloseSpendRedeemer,
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
      ct.Continue(AdvanceOrCloseArgs {
        input_index,
        output_index,
        control_cbor,
        chunk_proof,
        next_chunk_proof,
        frames,
        step_budget,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum

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
        expect Some(state) = m_input_state_data

        expect and {
            state.refusal_class == engine.class_pending,
            state.outpoint_key_hash != #"",
            state.reference_script_language == 0,
            state.output_index >= 0,
            state.total_length > 0,
            state.item_commitment != #"",
            state.machine_state_hash != #"",
            engine.hash_machine_control_v1(control_cbor) == state.machine_state_hash,
          }
        let control = native_script_scan_v1.decode_structure_control_v1(
          control_cbor,
        )

        let Pair(expected_script_hash, next_state) = if and {
          state.direction == engine.direction_wrongful_rejection,
          native_script_scan_v1.structure_terminal_is_exact_v1(control),
        } {
          // Canonical close for a terminal committed by an earlier advance.
          expect None = chunk_proof
          expect None = next_chunk_proof
          expect [] = frames
          expect step_budget == 0
          Pair(
            step_04_validator_script_hash,
            ScanThreadStateV1 {
              ..state,
              refusal_class: engine.refusal_class_malformed,
            },
          )
        } else {
          let window = when chunk_proof is {
            None -> {
              expect None = next_chunk_proof
              None
            }
            Some(proof) ->
              Some(
                engine.authenticated_scan_window_v1(
                  state.output_index,
                  state.total_length,
                  state.item_commitment,
                  control.cursor,
                  proof,
                  next_chunk_proof,
                ),
              )
          }
          when
            engine.budgeted_scan_v1(control, window, frames, step_budget)
          is {
            engine.ScanAdvancedV1 { control: next_control } ->
              if and {
                state.direction == engine.direction_wrongful_rejection,
                native_script_scan_v1.structure_terminal_is_exact_v1(
                  next_control,
                ),
              } {
                Pair(
                  step_04_validator_script_hash,
                  ScanThreadStateV1 {
                    ..state,
                    refusal_class: engine.refusal_class_malformed,
                  },
                )
              } else {
                let next_control_cbor = native_script_scan_v1.encode_structure_control_v1(
                  next_control,
                )
                Pair(
                  own_script_hash,
                  ScanThreadStateV1 {
                    ..state,
                    machine_state_hash: engine.hash_machine_control_v1(
                      next_control_cbor,
                    ),
                  },
                )
              }
            engine.ScanRefusedV1 { refusal_class } -> {
              // A refusal proves wrongful acceptance. For direction B it
              // corroborates the operator and cannot close this fraud proof.
              expect state.direction == engine.direction_wrongful_acceptance
              Pair(
                step_04_validator_script_hash,
                ScanThreadStateV1 { ..state, refusal_class },
              )
            }
          }
        }

        expect output_script_hash == expected_script_hash
        let expected_output_state: Data = next_state
        expect output_state_data == expected_output_state
        True
      }
    }
  }

  else(_) {
    fail
  }
}

type AdvanceSetup {
  block: fixture.NativeBlockFixtureV1,
  descriptor: ledger_output_commitment_v1.LedgerOutputCommitmentV1,
  outpoint_key_cbor: ByteArray,
  item_bytes: ByteArray,
  initial_control: native_script_scan_v1.NativeScriptStructureControlV1,
}

fn advance_setup(item_bytes: ByteArray) -> AdvanceSetup {
  let tx = tf.subject_tx_v1(native_types.TxIsValid)
  let block = fixture.native_block_fixture_v1(tx)
  expect Some(outpoint) = list.at(tx.body.inputs, 0)
  let descriptor = tf.scan_descriptor_v1(outpoint.output_index, 0, item_bytes)
  expect engine.MachineBoundV1 { control } = engine.bind_machine_v1(
    item_bytes,
    descriptor.reference_script_total_length,
  )
  AdvanceSetup {
    block,
    descriptor,
    outpoint_key_cbor: encode_midgard_tx_input(outpoint),
    item_bytes,
    initial_control: control,
  }
}

fn bound_advance_state(
  setup: AdvanceSetup,
  direction: Int,
  control: native_script_scan_v1.NativeScriptStructureControlV1,
) -> ScanThreadStateV1 {
  let descriptor_cbor = ledger_output_commitment_v1.encode(setup.descriptor)
  let pre_bind = engine.pre_bind_scan_state_v1(
    direction,
    if direction == engine.direction_wrongful_rejection {
      engine.source_kind_forced
    } else {
      engine.source_kind_normal
    },
    setup.block.tx_id,
    if direction == engine.direction_wrongful_rejection {
      tf.h32_d
    } else {
      #""
    },
    engine.class_pending,
    tf.single_leaf_ledger_root_v1(setup.outpoint_key_cbor, descriptor_cbor),
    engine.outpoint_source_spend,
    0,
  )
  let opened = engine.opened_subject_scan_state_v1(
    pre_bind,
    setup.outpoint_key_cbor,
    setup.descriptor.output_index,
  )
  ScanThreadStateV1 {
    ..engine.bound_descriptor_scan_state_v1(opened, setup.descriptor),
    machine_state_hash: engine.hash_machine_control_v1(
      native_script_scan_v1.encode_structure_control_v1(control),
    ),
  }
}

fn advance_args(
  setup: AdvanceSetup,
  control: native_script_scan_v1.NativeScriptStructureControlV1,
  with_window: Bool,
  step_budget: Int,
) -> AdvanceOrCloseArgs {
  AdvanceOrCloseArgs {
    input_index: 0,
    output_index: 0,
    control_cbor: native_script_scan_v1.encode_structure_control_v1(control),
    chunk_proof: if with_window {
      Some(
        tf.single_chunk_proof_v1(
          setup.descriptor.output_index,
          setup.item_bytes,
        ),
      )
    } else {
      None
    },
    next_chunk_proof: None,
    frames: [],
    step_budget,
  }
}

fn advanced_control(
  setup: AdvanceSetup,
  control: native_script_scan_v1.NativeScriptStructureControlV1,
  step_budget: Int,
) -> native_script_scan_v1.NativeScriptStructureControlV1 {
  expect engine.ScanAdvancedV1 { control: next_control } = engine.budgeted_scan_v1(
    control,
    Some(engine.ScanWindowV1 { bytes: setup.item_bytes, start_offset: 0 }),
    [],
    step_budget,
  )
  next_control
}

fn run_advance(
  setup: AdvanceSetup,
  state: ScanThreadStateV1,
  args: AdvanceOrCloseArgs,
  expected_output_state: Data,
  closes: Bool,
) -> Bool {
  let asset_name = fixture.ct_token_asset_name(setup.block)
  let tx = if closes {
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state)
  } else {
    tf.self_loop_l1_tx_v1(asset_name, expected_output_state)
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(args),
    fixture.own_out_ref(),
    tx,
  )
}

test advance_or_close_commits_a_partial_advance() {
  let setup = advance_setup(tf.signature_script_item_v1())
  let state = bound_advance_state(
    setup,
    engine.direction_wrongful_rejection,
    setup.initial_control,
  )
  let next_control = advanced_control(setup, setup.initial_control, 1)
  let expected: Data = ScanThreadStateV1 {
    ..state,
    machine_state_hash: engine.hash_machine_control_v1(
      native_script_scan_v1.encode_structure_control_v1(next_control),
    ),
  }
  run_advance(
    setup,
    state,
    advance_args(setup, setup.initial_control, True, 1),
    expected,
    False,
  )
}

test advance_or_close_closes_direction_b_at_the_exact_terminal() {
  let setup = advance_setup(tf.signature_script_item_v1())
  let state = bound_advance_state(
    setup,
    engine.direction_wrongful_rejection,
    setup.initial_control,
  )
  let terminal = advanced_control(setup, setup.initial_control, 10)
  expect native_script_scan_v1.structure_terminal_is_exact_v1(terminal)
  let expected: Data = ScanThreadStateV1 {
    ..state,
    refusal_class: engine.refusal_class_malformed,
  }
  run_advance(
    setup,
    state,
    advance_args(setup, setup.initial_control, True, 10),
    expected,
    True,
  )
}

test advance_or_close_closes_a_direction_a_refusal() {
  let setup = advance_setup(tf.malformed_payload_item_v1())
  let state = bound_advance_state(
    setup,
    engine.direction_wrongful_acceptance,
    setup.initial_control,
  )
  expect engine.ScanRefusedV1 { refusal_class } = engine.budgeted_scan_v1(
    setup.initial_control,
    Some(engine.ScanWindowV1 { bytes: setup.item_bytes, start_offset: 0 }),
    [],
    10,
  )
  let expected: Data = ScanThreadStateV1 { ..state, refusal_class }
  run_advance(
    setup,
    state,
    advance_args(setup, setup.initial_control, True, 10),
    expected,
    True,
  )
}

test advance_or_close_rejects_a_replayed_control() fail {
  let setup = advance_setup(tf.signature_script_item_v1())
  let terminal = advanced_control(setup, setup.initial_control, 10)
  let state = bound_advance_state(
    setup,
    engine.direction_wrongful_rejection,
    terminal,
  )
  let expected: Data = state
  run_advance(
    setup,
    state,
    advance_args(setup, setup.initial_control, True, 10),
    expected,
    False,
  )
}

test advance_or_close_rejects_substituted_chunk_bytes() fail {
  let setup = advance_setup(tf.signature_script_item_v1())
  let state = bound_advance_state(
    setup,
    engine.direction_wrongful_rejection,
    setup.initial_control,
  )
  let args = advance_args(setup, setup.initial_control, True, 10)
  let expected: Data = state
  run_advance(
    setup,
    state,
    AdvanceOrCloseArgs {
      ..args,
      chunk_proof: Some(
        tf.single_chunk_proof_v1(
          setup.descriptor.output_index,
          tf.all_of_two_signatures_item_v1(),
        ),
      ),
    },
    expected,
    False,
  )
}

test advance_or_close_rejects_a_direction_b_refusal() fail {
  let setup = advance_setup(tf.malformed_payload_item_v1())
  let state = bound_advance_state(
    setup,
    engine.direction_wrongful_rejection,
    setup.initial_control,
  )
  let expected: Data = ScanThreadStateV1 {
    ..state,
    refusal_class: engine.refusal_class_malformed,
  }
  run_advance(
    setup,
    state,
    advance_args(setup, setup.initial_control, True, 10),
    expected,
    True,
  )
}
