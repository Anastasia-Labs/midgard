use aiken/crypto.{ScriptHash, blake2b_256}
use aiken/primitive/bytearray
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue, finalize}
use midgard/fraud_proofs/execution_native_script_invalid/step_04.{
  Args, Datum, DirectFinalize, SpendRedeemer, StartSignerScan, State,
  direct_script_bytes_limit, direct_signer_limit,
  staged_signer_start_batch_limit,
}
use midgard/fraud_proofs/execution_native_script_invalid/step_05
use midgard/fraud_proofs/field_opening_v1.{
  WitnessAnchor, address_witnesses_field_index, fold_opened_field,
  opened_field_walk,
}
use midgard/fraud_proofs/mint_authorization/engine
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_script_decoding/thread_fixture_v1 as tf
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_address_witness_cbor, decode_midgard_versioned_script_at,
  encode_midgard_address_witness, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionWitnessSet, MidgardVersionedScript,
  NativeCardanoScript,
}
use midgard/native_tx_field_access_v1.{field_item_count}
use midgard/native_tx_machine_walk_v1 as machine_walk
use midgard/script_proof_v1
use midgard/validation_merkle_v1

type SignerAccumulatorV1 {
  previous_signer_hash: ByteArray,
  signer_count: Int,
  signer_peaks: List<validation_merkle_v1.FrontierPeak>,
}

validator main(
  step_05_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
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
      ct.Continue(action) -> {
        expect Some(step_datum) = datum
        when action is {
          DirectFinalize {
            input_index,
            output_index,
            fraud_proof_mint_redeemer_index,
            script_item_cbor,
            addr_tx_wits_opening,
          } -> {
            let Transaction { inputs, outputs, redeemers, reference_inputs, .. }
              = tx
            let
                _own_script_hash,
                _thread_asset_name,
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
            let script_bytes = native_script_bytes(state, script_item_cbor)
            expect bytearray.length(script_bytes) <= direct_script_bytes_limit
            let addr_tx_wits_walk = opened_address_witnesses(
              state,
              addr_tx_wits_opening,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            let Pair(view, _start) = addr_tx_wits_walk
            expect field_item_count(view) <= direct_signer_limit
            let signer_hashes = fold_opened_field(
              addr_tx_wits_walk,
              [],
              fn(acc, _index, item) { [signer_hash_of(item), ..acc] },
            )
            expect engine.ScriptEvaluatedV1 { satisfied } = engine.evaluate_native_script_v1(
              script_bytes,
              signer_hashes,
              state.validity_interval_start,
              state.validity_interval_end,
            )
            expect satisfied == ( state.direction == 1 )
            True
          }
          StartSignerScan {
            input_index,
            output_index,
            script_item_cbor,
            addr_tx_wits_opening,
            item_budget,
          } -> {
            expect valid_staged_budget(item_budget)
            let Transaction { inputs, outputs, reference_inputs, .. } = tx
            let
                _own_script_hash,
                _thread_asset_name,
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
            let script_bytes = native_script_bytes(state, script_item_cbor)
            let Pair(view, start) = opened_address_witnesses(
              state,
              addr_tx_wits_opening,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            expect or {
                field_item_count(view) > direct_signer_limit,
                bytearray.length(script_bytes) > direct_script_bytes_limit,
              }
            let Pair(accumulator, next) = machine_walk.walk_fold(
              view,
              start,
              item_budget,
              SignerAccumulatorV1 {
                previous_signer_hash: #"",
                signer_count: 0,
                signer_peaks: validation_merkle_v1.empty_frontier(),
              },
              fn(accumulator, _index, item) {
                append_signer(accumulator, signer_hash_of(item))
              },
            )
            expect output_script_hash == step_05_validator_script_hash
            let expected_output_state: Data = step_05.State {
              direction: state.direction,
              execution_index: state.execution_index,
              source_index: state.source_index,
              origin_kind: state.origin_kind,
              item_commitment: state.item_commitment,
              bad_tx_id: state.bad_tx_id,
              bad_tx_witness_set_hash: state.bad_tx_witness_set_hash,
              script_item_hash: state.script_item_hash,
              validity_interval_start: state.validity_interval_start,
              validity_interval_end: state.validity_interval_end,
              signer_checkpoint_hash: machine_walk.field_walk_checkpoint_hash(
                next,
              ),
              previous_signer_hash: accumulator.previous_signer_hash,
              signer_count: accumulator.signer_count,
              signer_peaks: accumulator.signer_peaks,
            }
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

fn opened_address_witnesses(
  state: State,
  opening,
  reference_inputs,
  certificate_policy_id,
) {
  opened_field_walk(
    opening,
    WitnessAnchor {
      tx_id: state.bad_tx_id,
      witness_set_hash: state.bad_tx_witness_set_hash,
    },
    address_witnesses_field_index,
    reference_inputs,
    certificate_policy_id,
  )
}

fn native_script_bytes(state: State, script_item_cbor: ByteArray) -> ByteArray {
  expect blake2b_256(script_item_cbor) == state.script_item_hash
  let Pair(offset, script) = decode_midgard_versioned_script_at(
    script_item_cbor,
    0,
  )
  expect offset == bytearray.length(script_item_cbor)
  expect encode_midgard_versioned_script(script) == script_item_cbor
  expect MidgardVersionedScript { language: NativeCardanoScript, script_bytes }
    = script
  script_bytes
}

fn signer_hash_of(item: ByteArray) -> ByteArray {
  let witness = decode_midgard_address_witness_cbor(item)
  expect encode_midgard_address_witness(witness) == item
  utils.get_verification_key_hash(witness.verification_key)
}

fn append_signer(
  accumulator: SignerAccumulatorV1,
  signer_hash: ByteArray,
) -> SignerAccumulatorV1 {
  expect or {
      accumulator.previous_signer_hash == #"",
      bytearray.compare(accumulator.previous_signer_hash, signer_hash) != Greater,
    }
  if accumulator.previous_signer_hash == signer_hash {
    accumulator
  } else {
    SignerAccumulatorV1 {
      previous_signer_hash: signer_hash,
      signer_count: accumulator.signer_count + 1,
      signer_peaks: validation_merkle_v1.append_leaf(
        accumulator.signer_count,
        accumulator.signer_peaks,
        script_proof_v1.signer_leaf_hash(signer_hash),
      ),
    }
  }
}

fn valid_staged_budget(item_budget: Int) -> Bool {
  expect item_budget > 0
  expect item_budget <= staged_signer_start_batch_limit
  True
}

fn sig_script(key_hash: ByteArray) -> ByteArray {
  #"8200581c" |> bytearray.concat(key_hash)
}

fn run_native_verdict(script_bytes: ByteArray) -> Bool {
  let base = fixture.valid_native_tx_v1()
  let script = MidgardVersionedScript {
    language: NativeCardanoScript,
    script_bytes,
  }
  let tx = MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [script],
    },
  }
  let block = fixture.native_block_fixture_v1(tx)
  let script_item_cbor = encode_midgard_versioned_script(script)
  let state = State {
    direction: 0,
    execution_index: 0,
    source_index: 0,
    origin_kind: 0,
    item_commitment: blake2b_256(script_item_cbor),
    bad_tx_id: block.tx_id,
    bad_tx_witness_set_hash: block.compact.witness_set_hash,
    script_item_hash: blake2b_256(script_item_cbor),
    validity_interval_start: block.compact.body.validity_interval_start,
    validity_interval_end: block.compact.body.validity_interval_end,
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(
      DirectFinalize {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        script_item_cbor,
        addr_tx_wits_opening: fixture.inline_witness_opening(
          block.compact_cbor,
          block.witness_set_compact,
          [encode_midgard_address_witness(fixture.sample_address_witness())],
        ),
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(fixture.ct_token_asset_name(block)),
  )
}

test execution_native_script_invalid_step_04_finalizes_an_unsatisfied_native_witness() {
  run_native_verdict(sig_script(tf.h28_y))
}

test execution_native_script_invalid_step_04_rejects_a_satisfied_native_witness() fail {
  run_native_verdict(
    sig_script(
      utils.get_verification_key_hash(fixture.sample_witness_verification_key()),
    ),
  )
}

test execution_native_script_invalid_step_04_rejects_malformed_native_bytes() fail {
  run_native_verdict(#"ff")
}
