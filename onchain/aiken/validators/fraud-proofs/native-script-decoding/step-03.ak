//// ## Native-Script Decoding Fault Proof — Step 03 (#635, #633)
////
//// The thread's working step, in three arms:
////
//// - `BindOutpoint` (once): opens the accused field at the accused ordinal
////   through the §8.8 door, authenticates the resolved output's descriptor
////   as the ledger-trie value under `prior_ledger_root`, and freezes the
////   scan anchor (`reference_script_item_commitment` /
////   `reference_script_total_length`, item-relative coordinates). Tag-0
////   descriptors initialise the frozen scan machine from the authenticated
////   first chunk (self-loop); a malformed or empty wrapper closes the bind
////   for direction A; a non-tag-0 descriptor closes it for direction B
////   (descriptor contradiction).
//// - `Scan` (self-loop): folds the frozen primitives under a prover-chosen
////   step budget over a chunk window authenticated once per L1 transaction
////   (cursor chunk plus, mandatorily, the adjacent following chunk). A
////   mid-fold refusal aborts — refusals are the verdict's to consume.
//// - `Verdict` (once): direction A exhibits exactly one refusing primitive
////   step; direction B exhibits the exact canonical terminal. Either hands
////   the classed state to step-04.

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
  Args, BindOutpoint, Datum, Scan, SpendRedeemer, Verdict,
}
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/ledger_output_commitment_v1
use midgard/mpf_proof_v1
use midgard/native_script_scan_v1
use midgard/native_tx_machine_walk_v1.{spend_input_at}

validator main(
  step_04_validator_script_hash: ScriptHash,
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
      ct.Continue(BindOutpoint {
        input_index,
        output_index,
        subject_field_opening,
        descriptor_cbor,
        ledger_membership_proof,
        first_chunk_proof,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx

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

        // 1. The bind happens exactly once, on a pre-bind state.
        expect and {
            state.machine_state_hash == #"",
            state.refusal_class == engine.class_pending,
            state.outpoint_key_hash == #"",
          }

        // 2. The accused field opens through the §8.8 door against the
        //    commitment the door extracts positionally from the compact
        //    structures `verified_tx_id` binds. The field index is a
        //    call-site literal per branch of the authenticated pair.
        let subject_view = if state.outpoint_source_kind == engine.outpoint_source_spend {
          opened_field_view(
            subject_field_opening,
            BodyAnchor { tx_id: state.verified_tx_id },
            spend_inputs_field_index,
            reference_inputs,
            field_preimage_certificate_policy_id,
          )
        } else {
          expect state.outpoint_source_kind == engine.outpoint_source_reference
          opened_field_view(
            subject_field_opening,
            BodyAnchor { tx_id: state.verified_tx_id },
            reference_inputs_field_index,
            reference_inputs,
            field_preimage_certificate_policy_id,
          )
        }

        // 3. The accused ordinal's outpoint — §7.3 abort-never-clamp: an
        //    ordinal past the committed collection fails the read.
        let subject_outpoint = spend_input_at(
          subject_view,
          state.outpoint_cursor,
        )
        let outpoint_key_bytes = encode_midgard_tx_input(subject_outpoint)

        // 4. The descriptor is the ledger's own resolution of that outpoint:
        //    canonical bytes, well-formed, and a member of the pre-state
        //    ledger trie under the outpoint's canonical key — the same
        //    opening the interactive machine's resolve step performs.
        let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
        expect subject_outpoint.output_index == descriptor.output_index
        expect descriptor.total_length > 0
        expect
          mpf_proof_v1.has(
            state.prior_ledger_root,
            outpoint_key_bytes,
            descriptor_cbor,
            ledger_membership_proof,
          )

        let bound = engine.bound_scan_state_v1(
          state,
          descriptor,
          outpoint_key_bytes,
        )

        // 5. Branch on the bound descriptor's language.
        if descriptor.reference_script_language == 0 {
          // Tag-0: the wrapper is read from the authenticated first chunk.
          expect Some(first_proof) = first_chunk_proof
          expect first_proof.chunk_index == 0
          expect
            ledger_output_commitment_v1.verify_reference_script_chunk(
              descriptor,
              first_proof,
            )
          when
            engine.bind_machine_v1(
              first_proof.chunk,
              descriptor.reference_script_total_length,
            )
          is {
            engine.MachineBoundV1 { control } -> {
              // The machine starts; the thread self-loops into Scan.
              let control_cbor = native_script_scan_v1.encode_structure_control_v1(
                control,
              )
              expect output_script_hash == own_script_hash
              let expected_output_state: Data = ScanThreadStateV1 {
                ..bound,
                machine_state_hash: engine.hash_machine_control_v1(control_cbor),
              }
              expect output_state_data == expected_output_state
              True
            }
            engine.MachineBindMalformedV1 -> {
              // The frozen machine's InvalidReferenceScript precedent: a
              // class-0 refusal provable at bind — direction A only. For
              // direction B the same fact corroborates the accusation.
              expect state.direction == engine.direction_wrongful_acceptance
              expect output_script_hash == step_04_validator_script_hash
              let expected_output_state: Data = ScanThreadStateV1 {
                ..bound,
                refusal_class: engine.refusal_class_malformed,
              }
              expect output_state_data == expected_output_state
              True
            }
            engine.MachineBindNonNativeV1 { .. } ->
              // Descriptor says tag-0, wrapper says otherwise: the frozen
              // machine never scans such an item, so neither direction can
              // close through this family.
              fail @"wrapper language contradicts the bound descriptor"
          }
        } else {
          // Non-tag-0 (or absent) reference script: the frozen machine
          // never emits a decoding arm for it, so a leaf accusing one is a
          // contradiction provable by the descriptor alone — direction B
          // closes at bind; direction A has nothing to prove here.
          expect state.direction == engine.direction_wrongful_rejection
          expect None = first_chunk_proof
          expect output_script_hash == step_04_validator_script_hash
          let expected_output_state: Data = ScanThreadStateV1 {
            ..bound,
            refusal_class: engine.refusal_class_malformed,
          }
          expect output_state_data == expected_output_state
          True
        }
      }
      ct.Continue(Scan {
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

        // 1. Only a bound, unclassed machine scans; the carried control must
        //    be the committed one, canonically encoded.
        expect and {
            state.refusal_class == engine.class_pending,
            state.machine_state_hash != #"",
            engine.hash_machine_control_v1(control_cbor) == state.machine_state_hash,
          }
        let control = native_script_scan_v1.decode_structure_control_v1(
          control_cbor,
        )

        // 2. The chunk window is authenticated once for this L1 transaction
        //    against the frozen item anchor. Frame/finalize-only stretches
        //    need no window at all.
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

        // 3. The budgeted fold must stop without a refusal — a prover who
        //    hits one mid-fold must re-run with a budget that stops just
        //    before it and consume it through Verdict.
        expect engine.ScanAdvancedV1 { control: next_control } = engine.budgeted_scan_v1(
          control,
          window,
          frames,
          step_budget,
        )
        let next_control_cbor = native_script_scan_v1.encode_structure_control_v1(
          next_control,
        )

        // 4. Self-loop with the advanced machine commitment.
        expect output_script_hash == own_script_hash
        let expected_output_state: Data = ScanThreadStateV1 {
          ..state,
          machine_state_hash: engine.hash_machine_control_v1(next_control_cbor),
        }
        expect output_state_data == expected_output_state

        True
      }
      ct.Continue(Verdict {
        input_index,
        output_index,
        control_cbor,
        chunk_proof,
        next_chunk_proof,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx

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

        expect and {
            state.refusal_class == engine.class_pending,
            state.machine_state_hash != #"",
            engine.hash_machine_control_v1(control_cbor) == state.machine_state_hash,
          }
        let control = native_script_scan_v1.decode_structure_control_v1(
          control_cbor,
        )

        let refusal_class = if state.direction == engine.direction_wrongful_rejection {
          // Direction B: the accused script scans to the exact canonical
          // terminal — contradicting every decoding arm. Class 0 is the
          // contradiction marker.
          expect native_script_scan_v1.structure_terminal_is_exact_v1(control)
          expect None = chunk_proof
          expect None = next_chunk_proof
          engine.refusal_class_malformed
        } else {
          expect state.direction == engine.direction_wrongful_acceptance
          // Direction A: exactly one primitive step must refuse. The
          // safe-read margin inside the fold guarantees the refusal is
          // never a window-truncation artifact; a step that advances —
          // terminal included — aborts the claim.
          let window = when chunk_proof is {
            None -> None
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
          expect engine.ScanRefusedV1 { refusal_class: proven_class } = engine.budgeted_scan_v1(
            control,
            window,
            [],
            1,
          )
          proven_class
        }

        // The classed state hands to step-04.
        expect output_script_hash == step_04_validator_script_hash
        let expected_output_state: Data = ScanThreadStateV1 {
          ..state,
          refusal_class,
        }
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
// Step-03 is where a challenge against an honest operator dies: the
// descriptor is the ledger's own resolution of the accused outpoint, the
// window is the committed item's own bytes, and the machine is the frozen
// scan — there is nothing left for the prover to substitute.

fn subject_tx() -> native_types.MidgardTransaction {
  tf.subject_tx_v1(native_types.TxIsValid)
}

fn subject_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(subject_tx())
}

/// Everything a bind-step test needs about one accused outpoint.
type BindSetup {
  block: fixture.NativeBlockFixtureV1,
  outpoint_source_kind: Int,
  field_items: List<ByteArray>,
  descriptor: ledger_output_commitment_v1.LedgerOutputCommitmentV1,
  descriptor_cbor: ByteArray,
  outpoint_key_bytes: ByteArray,
  ledger_root: ByteArray,
  item_bytes: ByteArray,
}

fn bind_setup(
  outpoint_source_kind: Int,
  language: Int,
  item_bytes: ByteArray,
) -> BindSetup {
  let tx = subject_tx()
  let block = subject_block()
  let outpoint = if outpoint_source_kind == engine.outpoint_source_spend {
    expect Some(input) = list.at(tx.body.inputs, 0)
    input
  } else {
    expect Some(input) = list.at(tx.body.reference_inputs, 0)
    input
  }
  let descriptor = tf.scan_descriptor_v1(
    outpoint.output_index,
    language,
    item_bytes,
  )
  let descriptor_cbor = ledger_output_commitment_v1.encode(descriptor)
  let outpoint_key_bytes = encode_midgard_tx_input(outpoint)
  BindSetup {
    block,
    outpoint_source_kind,
    field_items: if outpoint_source_kind == engine.outpoint_source_spend {
      tf.subject_spend_field_items_v1(tx)
    } else {
      tf.subject_reference_field_items_v1(tx)
    },
    descriptor,
    descriptor_cbor,
    outpoint_key_bytes,
    ledger_root: tf.single_leaf_ledger_root_v1(
      outpoint_key_bytes,
      descriptor_cbor,
    ),
    item_bytes,
  }
}

fn pre_bind_state(
  setup: BindSetup,
  direction: Int,
  scan_reason_class: Int,
  outpoint_cursor: Int,
) -> ScanThreadStateV1 {
  engine.pre_bind_scan_state_v1(
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
    scan_reason_class,
    setup.ledger_root,
    setup.outpoint_source_kind,
    outpoint_cursor,
  )
}

fn step_datum_with(state: ScanThreadStateV1) -> Datum {
  ct.StepDatum {
    fraud_prover: fixture.fraud_prover_key_hash,
    data: Some(state),
  }
}

fn run_step_03(
  input_state: ScanThreadStateV1,
  args: Args,
  expected_output_state: Data,
  advances_to_step_04: Bool,
) -> Bool {
  let asset_name = fixture.ct_token_asset_name(subject_block())
  let l1_tx = if advances_to_step_04 {
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state)
  } else {
    tf.self_loop_l1_tx_v1(asset_name, expected_output_state)
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(step_datum_with(input_state)),
    ct.Continue(args),
    fixture.own_out_ref(),
    l1_tx,
  )
}

fn bind_args(setup: BindSetup, with_first_chunk: Bool) -> Args {
  BindOutpoint {
    input_index: 0,
    output_index: 0,
    subject_field_opening: fixture.inline_body_opening(
      setup.block.compact_cbor,
      setup.field_items,
    ),
    descriptor_cbor: setup.descriptor_cbor,
    ledger_membership_proof: [],
    first_chunk_proof: if with_first_chunk {
      Some(
        tf.single_chunk_proof_v1(
          setup.descriptor.output_index,
          setup.item_bytes,
        ),
      )
    } else {
      None
    },
  }
}

fn initial_control_of(
  setup: BindSetup,
) -> native_script_scan_v1.NativeScriptStructureControlV1 {
  expect engine.MachineBoundV1 { control } = engine.bind_machine_v1(
    setup.item_bytes,
    setup.descriptor.reference_script_total_length,
  )
  control
}

fn machine_hash_of(
  control: native_script_scan_v1.NativeScriptStructureControlV1,
) -> ByteArray {
  engine.hash_machine_control_v1(
    native_script_scan_v1.encode_structure_control_v1(control),
  )
}

/// Positive selector: the accused spend outpoint's descriptor names a tag-0
/// script; the machine initialises from the authenticated first chunk and
/// the thread self-loops into Scan.
test decoding_step_03_binds_a_spend_outpoint_and_starts_the_machine() {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    0,
    tf.signature_script_item_v1(),
  )
  let state = pre_bind_state(
    setup,
    engine.direction_wrongful_acceptance,
    engine.class_pending,
    0,
  )
  let bound = engine.bound_scan_state_v1(
    state,
    setup.descriptor,
    setup.outpoint_key_bytes,
  )
  let expected: Data = ScanThreadStateV1 {
    ..bound,
    machine_state_hash: machine_hash_of(initial_control_of(setup)),
  }
  run_step_03(state, bind_args(setup, True), expected, False)
}

/// Positive selector: the reference-input field opens the same way (the
/// ruled pair's other branch).
test decoding_step_03_binds_a_reference_outpoint() {
  let setup = bind_setup(
    engine.outpoint_source_reference,
    0,
    tf.signature_script_item_v1(),
  )
  let state = pre_bind_state(
    setup,
    engine.direction_wrongful_rejection,
    engine.refusal_class_malformed,
    0,
  )
  let bound = engine.bound_scan_state_v1(
    state,
    setup.descriptor,
    setup.outpoint_key_bytes,
  )
  let expected: Data = ScanThreadStateV1 {
    ..bound,
    machine_state_hash: machine_hash_of(initial_control_of(setup)),
  }
  run_step_03(state, bind_args(setup, True), expected, False)
}

/// Positive selector, direction A: an undecodable wrapper is the frozen
/// machine's InvalidReferenceScript precedent — the bind itself proves
/// class 0 and hands to step-04.
test decoding_step_03_closes_a_malformed_wrapper_at_bind() {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    0,
    tf.malformed_wrapper_item_v1(),
  )
  let state = pre_bind_state(
    setup,
    engine.direction_wrongful_acceptance,
    engine.class_pending,
    0,
  )
  let bound = engine.bound_scan_state_v1(
    state,
    setup.descriptor,
    setup.outpoint_key_bytes,
  )
  let expected: Data = ScanThreadStateV1 {
    ..bound,
    refusal_class: engine.refusal_class_malformed,
  }
  run_step_03(state, bind_args(setup, True), expected, True)
}

/// Positive selector, direction A: a tag-0 wrapper with an empty payload is
/// the same precedent.
test decoding_step_03_closes_an_empty_payload_at_bind() {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    0,
    tf.empty_payload_item_v1(),
  )
  let state = pre_bind_state(
    setup,
    engine.direction_wrongful_acceptance,
    engine.class_pending,
    0,
  )
  let bound = engine.bound_scan_state_v1(
    state,
    setup.descriptor,
    setup.outpoint_key_bytes,
  )
  let expected: Data = ScanThreadStateV1 {
    ..bound,
    refusal_class: engine.refusal_class_malformed,
  }
  run_step_03(state, bind_args(setup, True), expected, True)
}

/// Positive selector, direction B: the descriptor names a non-native
/// reference script, so the accused decoding arm could never have fired —
/// the contradiction closes at bind.
test decoding_step_03_closes_a_descriptor_contradiction_for_direction_b() {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    3,
    tf.plutus_script_item_v1(),
  )
  let state = pre_bind_state(
    setup,
    engine.direction_wrongful_rejection,
    engine.refusal_class_node_limit,
    0,
  )
  let bound = engine.bound_scan_state_v1(
    state,
    setup.descriptor,
    setup.outpoint_key_bytes,
  )
  let expected: Data = ScanThreadStateV1 {
    ..bound,
    refusal_class: engine.refusal_class_malformed,
  }
  run_step_03(state, bind_args(setup, False), expected, True)
}

/// Direction A has nothing to prove on a non-native descriptor.
test decoding_step_03_rejects_direction_a_on_a_non_native_descriptor() fail {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    3,
    tf.plutus_script_item_v1(),
  )
  let state = pre_bind_state(
    setup,
    engine.direction_wrongful_acceptance,
    engine.class_pending,
    0,
  )
  let bound = engine.bound_scan_state_v1(
    state,
    setup.descriptor,
    setup.outpoint_key_bytes,
  )
  let expected: Data = ScanThreadStateV1 {
    ..bound,
    refusal_class: engine.refusal_class_malformed,
  }
  run_step_03(state, bind_args(setup, False), expected, True)
}

/// Direction B cannot close on a genuinely malformed wrapper: the accusation
/// it disputes is corroborated, not contradicted.
test decoding_step_03_rejects_direction_b_on_a_malformed_wrapper() fail {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    0,
    tf.malformed_wrapper_item_v1(),
  )
  let state = pre_bind_state(
    setup,
    engine.direction_wrongful_rejection,
    engine.refusal_class_malformed,
    0,
  )
  let bound = engine.bound_scan_state_v1(
    state,
    setup.descriptor,
    setup.outpoint_key_bytes,
  )
  let expected: Data = ScanThreadStateV1 {
    ..bound,
    refusal_class: engine.refusal_class_malformed,
  }
  run_step_03(state, bind_args(setup, True), expected, True)
}

/// A wrapper whose language contradicts the bound descriptor's serves
/// neither direction.
test decoding_step_03_rejects_a_wrapper_language_contradiction() fail {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    0,
    tf.plutus_script_item_v1(),
  )
  let state = pre_bind_state(
    setup,
    engine.direction_wrongful_acceptance,
    engine.class_pending,
    0,
  )
  let bound = engine.bound_scan_state_v1(
    state,
    setup.descriptor,
    setup.outpoint_key_bytes,
  )
  let expected: Data = ScanThreadStateV1 {
    ..bound,
    refusal_class: engine.refusal_class_malformed,
  }
  run_step_03(state, bind_args(setup, True), expected, True)
}

/// The descriptor must resolve the accused outpoint: an output-index
/// mismatch fails before the trie is consulted.
test decoding_step_03_rejects_a_descriptor_for_another_output_index() fail {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    0,
    tf.signature_script_item_v1(),
  )
  let wrong_descriptor = tf.scan_descriptor_v1(7, 0, setup.item_bytes)
  let wrong_cbor = ledger_output_commitment_v1.encode(wrong_descriptor)
  let state = ScanThreadStateV1 {
    ..pre_bind_state(
      setup,
      engine.direction_wrongful_acceptance,
      engine.class_pending,
      0,
    ),
    prior_ledger_root: tf.single_leaf_ledger_root_v1(
      setup.outpoint_key_bytes,
      wrong_cbor,
    ),
  }
  let bound = engine.bound_scan_state_v1(
    state,
    wrong_descriptor,
    setup.outpoint_key_bytes,
  )
  let expected: Data = ScanThreadStateV1 {
    ..bound,
    machine_state_hash: machine_hash_of(initial_control_of(setup)),
  }
  run_step_03(
    state,
    BindOutpoint {
      input_index: 0,
      output_index: 0,
      subject_field_opening: fixture.inline_body_opening(
        setup.block.compact_cbor,
        setup.field_items,
      ),
      descriptor_cbor: wrong_cbor,
      ledger_membership_proof: [],
      first_chunk_proof: Some(tf.single_chunk_proof_v1(7, setup.item_bytes)),
    },
    expected,
    False,
  )
}

/// The descriptor must be the pre-state ledger's own value for the accused
/// key: a substituted descriptor fails the trie membership.
test decoding_step_03_rejects_a_descriptor_the_ledger_never_held() fail {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    0,
    tf.signature_script_item_v1(),
  )
  let substituted = tf.scan_descriptor_v1(0, 0, tf.malformed_payload_item_v1())
  let substituted_cbor = ledger_output_commitment_v1.encode(substituted)
  let state = pre_bind_state(
    setup,
    engine.direction_wrongful_acceptance,
    engine.class_pending,
    0,
  )
  let bound = engine.bound_scan_state_v1(
    state,
    substituted,
    setup.outpoint_key_bytes,
  )
  let expected: Data = ScanThreadStateV1 {
    ..bound,
    refusal_class: engine.refusal_class_malformed,
  }
  run_step_03(
    state,
    BindOutpoint {
      input_index: 0,
      output_index: 0,
      subject_field_opening: fixture.inline_body_opening(
        setup.block.compact_cbor,
        setup.field_items,
      ),
      descriptor_cbor: substituted_cbor,
      ledger_membership_proof: [],
      first_chunk_proof: Some(
        tf.single_chunk_proof_v1(0, tf.malformed_payload_item_v1()),
      ),
    },
    expected,
    True,
  )
}

/// §7.3 abort-never-clamp: an accused ordinal past the committed field fails
/// the read.
test decoding_step_03_rejects_an_out_of_range_ordinal() fail {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    0,
    tf.signature_script_item_v1(),
  )
  let state = pre_bind_state(
    setup,
    engine.direction_wrongful_acceptance,
    engine.class_pending,
    9,
  )
  let bound = engine.bound_scan_state_v1(
    state,
    setup.descriptor,
    setup.outpoint_key_bytes,
  )
  let expected: Data = ScanThreadStateV1 {
    ..bound,
    machine_state_hash: machine_hash_of(initial_control_of(setup)),
  }
  run_step_03(state, bind_args(setup, True), expected, False)
}

/// The bind happens once: a bound state cannot be re-bound.
test decoding_step_03_rejects_a_second_bind() fail {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    0,
    tf.signature_script_item_v1(),
  )
  let state = ScanThreadStateV1 {
    ..engine.bound_scan_state_v1(
      pre_bind_state(
        setup,
        engine.direction_wrongful_acceptance,
        engine.class_pending,
        0,
      ),
      setup.descriptor,
      setup.outpoint_key_bytes,
    ),
    machine_state_hash: machine_hash_of(initial_control_of(setup)),
  }
  let expected: Data = state
  run_step_03(state, bind_args(setup, True), expected, False)
}

// ## Scan selectors

fn bound_state_over(
  setup: BindSetup,
  direction: Int,
  scan_reason_class: Int,
  control: native_script_scan_v1.NativeScriptStructureControlV1,
) -> ScanThreadStateV1 {
  ScanThreadStateV1 {
    ..engine.bound_scan_state_v1(
      pre_bind_state(setup, direction, scan_reason_class, 0),
      setup.descriptor,
      setup.outpoint_key_bytes,
    ),
    machine_state_hash: machine_hash_of(control),
  }
}

fn scan_args(
  setup: BindSetup,
  control: native_script_scan_v1.NativeScriptStructureControlV1,
  with_window: Bool,
  frames: List<native_script_scan_v1.NativeScriptFrameV1>,
  step_budget: Int,
) -> Args {
  Scan {
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
    frames,
    step_budget,
  }
}

fn advanced_control_of(
  setup: BindSetup,
  from: native_script_scan_v1.NativeScriptStructureControlV1,
  frames: List<native_script_scan_v1.NativeScriptFrameV1>,
  steps: Int,
) -> native_script_scan_v1.NativeScriptStructureControlV1 {
  expect engine.ScanAdvancedV1 { control } = engine.budgeted_scan_v1(
    from,
    Some(engine.ScanWindowV1 { bytes: setup.item_bytes, start_offset: 0 }),
    frames,
    steps,
  )
  control
}

/// Positive selector: the whole signature script scans to the exact terminal
/// in one budgeted transaction, and the machine commitment advances with it.
test decoding_step_03_scans_a_signature_script_to_terminal() {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    0,
    tf.signature_script_item_v1(),
  )
  let control = initial_control_of(setup)
  let state = bound_state_over(
    setup,
    engine.direction_wrongful_rejection,
    engine.refusal_class_malformed,
    control,
  )
  let terminal = advanced_control_of(setup, control, [], 10)
  let expected: Data = ScanThreadStateV1 {
    ..state,
    machine_state_hash: machine_hash_of(terminal),
  }
  expect native_script_scan_v1.structure_terminal_is_exact_v1(terminal)
  run_step_03(state, scan_args(setup, control, True, [], 10), expected, False)
}

/// Positive selector: a budget of one advances exactly one primitive step;
/// the thread carries the mid-scan commitment and later resumes from it.
test decoding_step_03_scans_under_a_partial_budget() {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    0,
    tf.signature_script_item_v1(),
  )
  let control = initial_control_of(setup)
  let state = bound_state_over(
    setup,
    engine.direction_wrongful_rejection,
    engine.refusal_class_malformed,
    control,
  )
  let mid = advanced_control_of(setup, control, [], 1)
  let expected: Data = ScanThreadStateV1 {
    ..state,
    machine_state_hash: machine_hash_of(mid),
  }
  run_step_03(state, scan_args(setup, control, True, [], 1), expected, False)
}

/// Cursor replay/rewind: a carried control that does not hash to the
/// committed machine state never runs.
test decoding_step_03_rejects_a_control_the_thread_never_committed() fail {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    0,
    tf.signature_script_item_v1(),
  )
  let control = initial_control_of(setup)
  let terminal = advanced_control_of(setup, control, [], 10)
  let state = bound_state_over(
    setup,
    engine.direction_wrongful_rejection,
    engine.refusal_class_malformed,
    terminal,
  )
  // The redeemer replays the initial control against the terminal
  // commitment.
  let expected: Data = state
  run_step_03(state, scan_args(setup, control, True, [], 10), expected, False)
}

/// Payload substitution: a chunk of some other item fails the window
/// authentication against the frozen item commitment.
test decoding_step_03_rejects_a_substituted_chunk() fail {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    0,
    tf.signature_script_item_v1(),
  )
  let control = initial_control_of(setup)
  let state = bound_state_over(
    setup,
    engine.direction_wrongful_rejection,
    engine.refusal_class_malformed,
    control,
  )
  let expected: Data = state
  run_step_03(
    state,
    Scan {
      input_index: 0,
      output_index: 0,
      control_cbor: native_script_scan_v1.encode_structure_control_v1(control),
      chunk_proof: Some(
        tf.single_chunk_proof_v1(
          setup.descriptor.output_index,
          tf.all_of_two_signatures_item_v1(),
        ),
      ),
      next_chunk_proof: None,
      frames: [],
      step_budget: 10,
    },
    expected,
    False,
  )
}

/// A refusal mid-fold aborts the Scan arm: refusals are consumed through
/// Verdict, never silently skipped past.
test decoding_step_03_scan_aborts_on_a_mid_fold_refusal() fail {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    0,
    tf.malformed_payload_item_v1(),
  )
  let control = initial_control_of(setup)
  let state = bound_state_over(
    setup,
    engine.direction_wrongful_acceptance,
    engine.class_pending,
    control,
  )
  let expected: Data = state
  run_step_03(state, scan_args(setup, control, True, [], 10), expected, False)
}

// ## Verdict selectors

fn verdict_args(
  setup: BindSetup,
  control: native_script_scan_v1.NativeScriptStructureControlV1,
  with_window: Bool,
) -> Args {
  Verdict {
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
  }
}

/// Positive selector, direction A: the single refusing step over the
/// malformed payload proves class 0 and hands to step-04.
test decoding_step_03_verdict_proves_a_malformed_payload() {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    0,
    tf.malformed_payload_item_v1(),
  )
  let control = initial_control_of(setup)
  let state = bound_state_over(
    setup,
    engine.direction_wrongful_acceptance,
    engine.class_pending,
    control,
  )
  let expected: Data = ScanThreadStateV1 {
    ..state,
    refusal_class: engine.refusal_class_malformed,
  }
  run_step_03(state, verdict_args(setup, control, True), expected, True)
}

/// Positive selector, direction B: the exact canonical terminal contradicts
/// the accusation and hands to step-04.
test decoding_step_03_verdict_proves_the_terminal_for_direction_b() {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    0,
    tf.signature_script_item_v1(),
  )
  let control = initial_control_of(setup)
  let terminal = advanced_control_of(setup, control, [], 10)
  let state = bound_state_over(
    setup,
    engine.direction_wrongful_rejection,
    engine.refusal_class_depth_limit,
    terminal,
  )
  let expected: Data = ScanThreadStateV1 {
    ..state,
    refusal_class: engine.refusal_class_malformed,
  }
  run_step_03(state, verdict_args(setup, terminal, False), expected, True)
}

/// Wrongful-acceptance edge: a scannable script yields an advancing step,
/// which is not a refusal — direction A cannot close on it.
test decoding_step_03_verdict_rejects_direction_a_on_a_scannable_script() fail {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    0,
    tf.signature_script_item_v1(),
  )
  let control = initial_control_of(setup)
  let state = bound_state_over(
    setup,
    engine.direction_wrongful_acceptance,
    engine.class_pending,
    control,
  )
  let expected: Data = ScanThreadStateV1 {
    ..state,
    refusal_class: engine.refusal_class_malformed,
  }
  run_step_03(state, verdict_args(setup, control, True), expected, True)
}

/// Direction A cannot close on the terminal either.
test decoding_step_03_verdict_rejects_direction_a_on_the_terminal() fail {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    0,
    tf.signature_script_item_v1(),
  )
  let control = initial_control_of(setup)
  let terminal = advanced_control_of(setup, control, [], 10)
  let state = bound_state_over(
    setup,
    engine.direction_wrongful_acceptance,
    engine.class_pending,
    terminal,
  )
  let expected: Data = ScanThreadStateV1 {
    ..state,
    refusal_class: engine.refusal_class_malformed,
  }
  run_step_03(state, verdict_args(setup, terminal, False), expected, True)
}

/// Wrongful-rejection edge: direction B closes only on the exact terminal —
/// a mid-scan control proves nothing.
test decoding_step_03_verdict_rejects_direction_b_before_the_terminal() fail {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    0,
    tf.signature_script_item_v1(),
  )
  let control = initial_control_of(setup)
  let state = bound_state_over(
    setup,
    engine.direction_wrongful_rejection,
    engine.refusal_class_malformed,
    control,
  )
  let expected: Data = ScanThreadStateV1 {
    ..state,
    refusal_class: engine.refusal_class_malformed,
  }
  run_step_03(state, verdict_args(setup, control, False), expected, True)
}

// ## Cancellation

fn run_cancel(signed_by_prover: Bool) -> Bool {
  let setup = bind_setup(
    engine.outpoint_source_spend,
    0,
    tf.signature_script_item_v1(),
  )
  let state = pre_bind_state(
    setup,
    engine.direction_wrongful_acceptance,
    engine.class_pending,
    0,
  )
  let asset_name = fixture.ct_token_asset_name(setup.block)
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(step_datum_with(state)),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(asset_name, asset_name, signed_by_prover),
  )
}

test decoding_step_03_cancels_under_the_prover_signature() {
  run_cancel(True)
}

test decoding_step_03_rejects_an_unsigned_cancellation() fail {
  run_cancel(False)
}
