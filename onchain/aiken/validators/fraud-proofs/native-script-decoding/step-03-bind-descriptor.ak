//// ## Native-Script Decoding Fault Proof — Step 03 / Bind Descriptor
////
//// Authenticates the ledger's exact descriptor for the subject opened by the
//// preceding validator, freezes its reference-script item anchor, and either
//// initializes the scan machine or closes a bind-time contradiction.

use aiken/collection/list
use aiken/crypto.{ScriptHash, blake2b_256}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/engine.{ScanThreadStateV1}
use midgard/fraud_proofs/native_script_decoding/step_03.{
  BindDescriptorArgs, BindDescriptorSpendRedeemer, Datum,
}
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/ledger_output_commitment_v1
use midgard/mpf_proof_v1
use midgard/native_script_scan_v1

validator main(
  step_03_advance_or_close_validator_script_hash: ScriptHash,
  step_04_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: BindDescriptorSpendRedeemer,
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
      ct.Continue(BindDescriptorArgs {
        input_index,
        output_index,
        outpoint_key_cbor,
        descriptor_cbor,
        ledger_membership_proof,
        first_chunk_proof,
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

        // OpenSubject fixed the exact ledger key and its output index; no
        // descriptor field or machine state may already be bound.
        expect and {
            state.machine_state_hash == #"",
            state.refusal_class == engine.class_pending,
            state.outpoint_key_hash != #"",
            state.reference_script_language == engine.language_unbound,
            state.output_index >= 0,
            state.total_length == -1,
            state.item_commitment == #"",
            blake2b_256(outpoint_key_cbor) == state.outpoint_key_hash,
          }

        let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
        expect descriptor.output_index == state.output_index
        expect descriptor.total_length > 0
        expect
          mpf_proof_v1.has(
            state.prior_ledger_root,
            outpoint_key_cbor,
            descriptor_cbor,
            ledger_membership_proof,
          )
        let bound = engine.bound_descriptor_scan_state_v1(state, descriptor)

        if descriptor.reference_script_language == 0 {
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
              let control_cbor = native_script_scan_v1.encode_structure_control_v1(
                control,
              )
              expect
                output_script_hash == step_03_advance_or_close_validator_script_hash
              let expected_output_state: Data = ScanThreadStateV1 {
                ..bound,
                machine_state_hash: engine.hash_machine_control_v1(control_cbor),
              }
              expect output_state_data == expected_output_state
              True
            }
            engine.MachineBindMalformedV1 -> {
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
              fail @"wrapper language contradicts the bound descriptor"
          }
        } else {
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
    }
  }

  else(_) {
    fail
  }
}

type BindDescriptorSetup {
  block: fixture.NativeBlockFixtureV1,
  descriptor: ledger_output_commitment_v1.LedgerOutputCommitmentV1,
  descriptor_cbor: ByteArray,
  outpoint_key_cbor: ByteArray,
  item_bytes: ByteArray,
}

fn bind_descriptor_setup(
  language: Int,
  item_bytes: ByteArray,
) -> BindDescriptorSetup {
  let tx = tf.subject_tx_v1(native_types.TxIsValid)
  let block = fixture.native_block_fixture_v1(tx)
  expect Some(outpoint) = list.at(tx.body.inputs, 0)
  let descriptor = tf.scan_descriptor_v1(
    outpoint.output_index,
    language,
    item_bytes,
  )
  BindDescriptorSetup {
    block,
    descriptor,
    descriptor_cbor: ledger_output_commitment_v1.encode(descriptor),
    outpoint_key_cbor: encode_midgard_tx_input(outpoint),
    item_bytes,
  }
}

fn opened_bind_state(
  setup: BindDescriptorSetup,
  direction: Int,
) -> ScanThreadStateV1 {
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
    tf.single_leaf_ledger_root_v1(
      setup.outpoint_key_cbor,
      setup.descriptor_cbor,
    ),
    engine.outpoint_source_spend,
    0,
  )
  engine.opened_subject_scan_state_v1(
    pre_bind,
    setup.outpoint_key_cbor,
    setup.descriptor.output_index,
  )
}

fn initial_bind_control(
  setup: BindDescriptorSetup,
) -> native_script_scan_v1.NativeScriptStructureControlV1 {
  expect engine.MachineBoundV1 { control } = engine.bind_machine_v1(
    setup.item_bytes,
    setup.descriptor.reference_script_total_length,
  )
  control
}

fn bind_descriptor_args(
  setup: BindDescriptorSetup,
  with_first_chunk: Bool,
) -> BindDescriptorArgs {
  BindDescriptorArgs {
    input_index: 0,
    output_index: 0,
    outpoint_key_cbor: setup.outpoint_key_cbor,
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

fn run_bind_descriptor(
  setup: BindDescriptorSetup,
  state: ScanThreadStateV1,
  args: BindDescriptorArgs,
  expected_output_state: Data,
) -> Bool {
  let asset_name = fixture.ct_token_asset_name(setup.block)
  main.spend(
    fixture.next_step_script_hash,
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
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
  )
}

test bind_descriptor_authenticates_and_starts_the_machine() {
  let setup = bind_descriptor_setup(0, tf.signature_script_item_v1())
  let state = opened_bind_state(setup, engine.direction_wrongful_acceptance)
  let control_cbor = native_script_scan_v1.encode_structure_control_v1(
    initial_bind_control(setup),
  )
  let expected: Data = ScanThreadStateV1 {
    ..engine.bound_descriptor_scan_state_v1(state, setup.descriptor),
    machine_state_hash: engine.hash_machine_control_v1(control_cbor),
  }
  run_bind_descriptor(setup, state, bind_descriptor_args(setup, True), expected)
}

test bind_descriptor_closes_a_non_native_direction_b_descriptor() {
  let setup = bind_descriptor_setup(3, tf.plutus_script_item_v1())
  let state = opened_bind_state(setup, engine.direction_wrongful_rejection)
  let expected: Data = ScanThreadStateV1 {
    ..engine.bound_descriptor_scan_state_v1(state, setup.descriptor),
    refusal_class: engine.refusal_class_malformed,
  }
  run_bind_descriptor(
    setup,
    state,
    bind_descriptor_args(setup, False),
    expected,
  )
}

test bind_descriptor_rejects_a_substituted_outpoint_key() fail {
  let setup = bind_descriptor_setup(0, tf.signature_script_item_v1())
  let state = opened_bind_state(setup, engine.direction_wrongful_acceptance)
  let args = bind_descriptor_args(setup, True)
  let expected: Data = state
  run_bind_descriptor(
    setup,
    state,
    BindDescriptorArgs { ..args, outpoint_key_cbor: #"00" },
    expected,
  )
}

test bind_descriptor_rejects_a_descriptor_the_ledger_never_held() fail {
  let setup = bind_descriptor_setup(0, tf.signature_script_item_v1())
  let state = opened_bind_state(setup, engine.direction_wrongful_acceptance)
  let substituted = tf.scan_descriptor_v1(
    setup.descriptor.output_index,
    0,
    tf.malformed_payload_item_v1(),
  )
  let args = bind_descriptor_args(setup, True)
  let expected: Data = state
  run_bind_descriptor(
    setup,
    state,
    BindDescriptorArgs {
      ..args,
      descriptor_cbor: ledger_output_commitment_v1.encode(substituted),
    },
    expected,
  )
}

test bind_descriptor_rejects_direction_a_for_a_non_native_descriptor() fail {
  let setup = bind_descriptor_setup(3, tf.plutus_script_item_v1())
  let state = opened_bind_state(setup, engine.direction_wrongful_acceptance)
  let expected: Data = ScanThreadStateV1 {
    ..engine.bound_descriptor_scan_state_v1(state, setup.descriptor),
    refusal_class: engine.refusal_class_malformed,
  }
  run_bind_descriptor(
    setup,
    state,
    bind_descriptor_args(setup, False),
    expected,
  )
}
