use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, outputs_field_index,
}
use midgard/fraud_proofs/missing_native_script_tx/step_04.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/missing_native_script_tx/step_05
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{
  decode_midgard_tx_output_cbor, encode_midgard_tx_output,
}
use midgard/fraud_proofs/native_tx/types.{MidgardTxOutput, ScriptCredential}
use midgard/native_tx_field_access_v1.{field_item_at}

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
      ct.Continue(Args { input_index, output_index, outputs_opening }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced.
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
              producing_tx_id,
              bad_input_output_index,
              bad_tx_id,
              bad_tx_witness_set_hash,
            })
          = m_input_state_data

        // 2. The produced output is read out of the producing transaction's
        //    field-2 preimage through the §8.8 door. Field 2 is variable-width,
        //    so reaching item `n` walks `n` §5.1 heads — a byte jump each, and
        //    no offset table to trust (§7.2).
        let outputs_view = opened_field_view(
          outputs_opening,
          BodyAnchor { tx_id: producing_tx_id },
          outputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let output_with_missing_script = decode_midgard_tx_output_cbor(
          field_item_at(outputs_view, bad_input_output_index),
        )

        // 3. The spent output must be locked by a script credential. The next
        //    step proves this script hash corresponds to a native script.
        expect ScriptCredential(expected_missing_script_hash) = output_with_missing_script.address.payment_credential

        // 4. The UTxO must be produced at next step's script hash with a proper
        //    state.
        expect output_script_hash == step_05_validator_script_hash
        let expected_output_state: Data = step_05.State {
          expected_missing_script_hash,
          bad_tx_id,
          bad_tx_witness_set_hash,
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

const test_bad_tx_id: ByteArray =
  #"3838383838383838383838383838383838383838383838383838383838383838"

fn producing_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

fn producing_outputs() -> List<MidgardTxOutput> {
  producing_block().tx.body.outputs
}

fn run_step_04(
  carried_outputs: List<MidgardTxOutput>,
  bad_input_output_index: Int,
  expected_missing_script_hash: ByteArray,
) -> Bool {
  let block = producing_block()
  let expected_output_state: Data = step_05.State {
    expected_missing_script_hash,
    bad_tx_id: test_bad_tx_id,
    bad_tx_witness_set_hash: block.compact.witness_set_hash,
  }
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
            producing_tx_id: block.tx_id,
            bad_input_output_index,
            bad_tx_id: test_bad_tx_id,
            bad_tx_witness_set_hash: block.compact.witness_set_hash,
          },
        ),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        outputs_opening: fixture.inline_body_opening(
          block.compact_cbor,
          carried_outputs |> list.map(encode_midgard_tx_output),
        ),
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
  )
}

/// Valid-block negative selector: the fixture's output is locked by a key, not
/// a script, so there is no missing native script to prove and the step
/// refuses. It also exercises the positive half of the door — the preimage does
/// authenticate and the item is read — with the family rule doing the refusing.
test missing_native_script_tx_step_04_rejects_a_key_locked_output() fail {
  run_step_04(
    producing_outputs(),
    0,
    #"30303030303030303030303030303030303030303030303030303030",
  )
}

/// A preimage the producing transaction never committed fails the door's
/// authenticate-once check before any output is read.
test missing_native_script_tx_step_04_rejects_a_substituted_preimage() fail {
  run_step_04(
    [],
    0,
    #"30303030303030303030303030303030303030303030303030303030",
  )
}
