use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue, finalize}
use midgard/fraud_proofs/field_opening_v1.{
  NativeTxAnchorV1, WitnessAnchor, fold_opened_field,
  opened_field_grammar_certification, opened_field_walk,
  script_witnesses_field_index,
}
use midgard/fraud_proofs/missing_native_script_utxo/step_05.{
  Datum, DirectFinalize, GrammarCertification, Ready, SpendRedeemer,
  StartGrammarCertification, State, direct_script_witness_limit,
  staged_batch_limit,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_versioned_script_at, encode_midgard_versioned_script,
}
use midgard/fraud_proofs/native_tx/types.{
  MidgardVersionedScript, NativeCardanoScript,
}
use midgard/native_tx_field_access_v1.{
  field_item_count, provisional_field_item_count_for_certification,
}
use midgard/native_tx_machine_walk_v1.{
  certify_field_grammar, field_grammar_checkpoint_hash,
  field_grammar_is_complete,
}
use midgard/script_proof_v1

validator main(
  step_06_validator_script_hash: ScriptHash,
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
        let Transaction { inputs, outputs, redeemers, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        when action is {
          DirectFinalize {
            input_index,
            output_index,
            fraud_proof_mint_redeemer_index,
            script_tx_wits_opening,
          } -> {
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
            expect Ready = state.phase
            let opened = opened_field_walk(
              script_tx_wits_opening,
              anchor_of(state),
              script_witnesses_field_index,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            let Pair(view, _start) = opened
            expect field_item_count(view) <= direct_script_witness_limit
            let required_script_is_present = fold_opened_field(
              opened,
              False,
              fn(found, _index, item) {
                scan_script_witness(
                  state.expected_missing_script_hash,
                  found,
                  item,
                )
              },
            )
            expect required_script_is_present == False
            True
          }
          StartGrammarCertification {
            input_index,
            output_index,
            script_tx_wits_opening,
            item_budget,
          } -> {
            expect expect_valid_staged_budget(item_budget)
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
            expect Ready = state.phase
            let Pair(view, start) = opened_field_grammar_certification(
              script_tx_wits_opening,
              anchor_of(state),
              script_witnesses_field_index,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            expect
              provisional_field_item_count_for_certification(view) > direct_script_witness_limit
            let next = certify_field_grammar(view, start, item_budget)
            expect !field_grammar_is_complete(next)
            expect output_script_hash == step_06_validator_script_hash
            expect_output_state(
              output_state_data,
              State {
                ..state,
                phase: GrammarCertification {
                  checkpoint_hash: field_grammar_checkpoint_hash(next),
                },
              },
            )
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}

fn anchor_of(state: State) -> NativeTxAnchorV1 {
  WitnessAnchor {
    tx_id: state.bad_tx_id,
    witness_set_hash: state.bad_tx_witness_set_hash,
  }
}

fn expect_valid_staged_budget(item_budget: Int) -> Bool {
  expect item_budget > 0
  expect item_budget <= staged_batch_limit
  True
}

fn expect_output_state(output_state_data: Data, expected_state: State) -> Bool {
  let expected: Data = expected_state
  expect output_state_data == expected
  True
}

fn scan_script_witness(
  expected_missing_script_hash: ByteArray,
  found: Bool,
  item: ByteArray,
) -> Bool {
  let Pair(_offset, script_wit) = decode_midgard_versioned_script_at(item, 0)
  expect encode_midgard_versioned_script(script_wit) == item
  or {
    found,
    script_proof_v1.versioned_script_hash(script_wit) == expected_missing_script_hash,
  }
}

fn run_finalize(expected_missing_script_hash: ByteArray) -> Bool {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let state = State {
    expected_missing_script_hash,
    bad_tx_id: block.tx_id,
    bad_tx_witness_set_hash: block.compact.witness_set_hash,
    phase: Ready,
  }
  let asset_name = fixture.ct_token_asset_name(block)
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
        script_tx_wits_opening: fixture.inline_witness_opening(
          block.compact_cbor,
          block.witness_set_compact,
          block.tx.witness_set.script_tx_wits
            |> list.map(encode_midgard_versioned_script),
        ),
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

test missing_native_script_utxo_step_05_finalizes_an_absent_script() {
  run_finalize(
    script_proof_v1.versioned_script_hash(
      MidgardVersionedScript {
        language: NativeCardanoScript,
        script_bytes: #"8201818200581c30303030303030303030303030303030303030303030",
      },
    ),
  )
}

test missing_native_script_utxo_step_05_rejects_a_present_script() fail {
  run_finalize(
    script_proof_v1.versioned_script_hash(
      fixture.sample_native_script_witness(),
    ),
  )
}
