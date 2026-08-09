use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, spend_inputs_field_index,
}
use midgard/fraud_proofs/missing_native_script_tx/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/missing_native_script_tx/step_03
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/native_tx_machine_walk_v1.{spend_input_at}

validator main(
  step_03_validator_script_hash: ScriptHash,
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
      ct.Continue(Args {
        input_index,
        output_index,
        bad_input_index,
        spend_inputs_opening,
      }) -> {
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

        expect Some(State { bad_tx_id, bad_tx_witness_set_hash }) = m_input_state_data

        // 2. The spent input is read out of the bad transaction's field-0
        //    preimage through the §8.8 door — commitment extracted positionally
        //    inside the door, item located by §5.3 arithmetic.
        let spend_inputs_view = opened_field_view(
          spend_inputs_opening,
          BodyAnchor { tx_id: bad_tx_id },
          spend_inputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let input_with_missing_script = spend_input_at(
          spend_inputs_view,
          bad_input_index,
        )

        // 3. The produced UTxO must go to the next step with a proper state.
        expect output_script_hash == step_03_validator_script_hash
        let expected_output_state: Data = step_03.State {
          input_with_missing_script,
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

/// A transaction spending a script-locked output without carrying any script
/// witness.
fn challenged_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

fn challenged_inputs() -> List<MidgardTxInput> {
  challenged_block().tx.body.inputs
}

fn first_challenged_input() -> MidgardTxInput {
  expect Some(input) = list.at(challenged_inputs(), 0)
  input
}

fn run_step_02(
  block: fixture.NativeBlockFixtureV1,
  carried_inputs: List<MidgardTxInput>,
  bad_input_index: Int,
  expected_input_with_missing_script: MidgardTxInput,
) -> Bool {
  let expected_output_state: Data = step_03.State {
    input_with_missing_script: expected_input_with_missing_script,
    bad_tx_id: block.tx_id,
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
            bad_tx_id: block.tx_id,
            bad_tx_witness_set_hash: block.compact.witness_set_hash,
          },
        ),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        bad_input_index,
        spend_inputs_opening: fixture.inline_body_opening(
          block.compact_cbor,
          carried_inputs |> list.map(encode_midgard_tx_input),
        ),
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
  )
}

/// Positive selector: the carried preimage is what field 0 commits, so the
/// script-locked input reaches step-03 alongside the bad transaction id.
test missing_native_script_tx_step_02_forwards_the_spent_input() {
  run_step_02(
    challenged_block(),
    challenged_inputs(),
    0,
    first_challenged_input(),
  )
}

/// Valid-block negative selector: a preimage the transaction never committed
/// fails the door's authenticate-once check.
test missing_native_script_tx_step_02_rejects_a_substituted_preimage() fail {
  let injected = MidgardTxInput {
    tx_id: #"3535353535353535353535353535353535353535353535353535353535353535",
    output_index: 0,
  }
  run_step_02(challenged_block(), [injected], 0, injected)
}
