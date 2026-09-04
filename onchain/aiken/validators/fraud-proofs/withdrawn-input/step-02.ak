use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, spend_inputs_field_index,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/fraud_proofs/withdrawn_input/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/withdrawn_input/step_03
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
        spend_inputs_opening,
        bad_input_index,
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

        expect
            Some(State {
              bad_tx_id,
              blocks_withdrawals_root,
              blocks_withdrawal_count,
            })
          = m_input_state_data

        // 2. Target spend input is read out of the disputed transaction's
        //    field-0 preimage through the §8.8 door: the commitment is
        //    extracted positionally from the compact structures `bad_tx_id`
        //    authenticates, and the item is one arithmetic slice at §5.3's
        //    fixed stride. The field index is a literal here, never a redeemer
        //    argument, so this step cannot be steered onto field 1 — the
        //    reference-input mirror (`withdrawn-reference-input`) reads that
        //    field through its own literal.
        let spend_inputs_view = opened_field_view(
          spend_inputs_opening,
          BodyAnchor { tx_id: bad_tx_id },
          spend_inputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let withdrawn_input = spend_input_at(spend_inputs_view, bad_input_index)

        // 3. Target spend input, plus the counted withdrawals commitment,
        //    must be passed to the next step.
        expect output_script_hash == step_03_validator_script_hash
        let expected_output_state: Data = step_03.State {
          withdrawn_input,
          blocks_withdrawals_root,
          blocks_withdrawal_count,
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
//
// Step-02 opens the disputed transaction's field-0 preimage through the one
// door and selects the spend input the prover claims was withdrawn. The
// counted withdrawals commitment travels untouched to step-03.

const test_withdrawals_root: ByteArray =
  #"5151515151515151515151515151515151515151515151515151515151515151"

const test_withdrawal_count: Int = 3

fn challenged_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

fn challenged_spend_inputs() -> List<MidgardTxInput> {
  challenged_block().tx.body.inputs
}

fn first_challenged_spend_input() -> MidgardTxInput {
  expect Some(input) = list.at(challenged_spend_inputs(), 0)
  input
}

fn run_step_02(
  block: fixture.NativeBlockFixtureV1,
  carried_spend_inputs: List<MidgardTxInput>,
  bad_input_index: Int,
  expected_withdrawn_input: MidgardTxInput,
) -> Bool {
  let expected_output_state: Data = step_03.State {
    withdrawn_input: expected_withdrawn_input,
    blocks_withdrawals_root: test_withdrawals_root,
    blocks_withdrawal_count: test_withdrawal_count,
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
            blocks_withdrawals_root: test_withdrawals_root,
            blocks_withdrawal_count: test_withdrawal_count,
          },
        ),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        spend_inputs_opening: fixture.inline_body_opening(
          block.compact_cbor,
          carried_spend_inputs |> list.map(encode_midgard_tx_input),
        ),
        bad_input_index,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
  )
}

/// Positive selector: the carried preimage is the one the disputed
/// transaction's field-0 commitment authenticates, so the challenged spend
/// input reaches step-03 with the counted withdrawals commitment intact.
test withdrawn_input_step_02_forwards_the_challenged_spend_input() {
  let block = challenged_block()
  run_step_02(
    block,
    challenged_spend_inputs(),
    0,
    first_challenged_spend_input(),
  )
}

/// Valid-block negative selector: a preimage the transaction never committed
/// fails the door's authenticate-once check, so no spend input the
/// transaction did not use can be carried onward.
test withdrawn_input_step_02_rejects_a_substituted_preimage() fail {
  let block = challenged_block()
  let injected = MidgardTxInput {
    tx_id: #"3434343434343434343434343434343434343434343434343434343434343434",
    output_index: 2,
  }
  run_step_02(block, [injected], 0, injected)
}

/// §7.3 abort-never-clamp at the family's own call site.
test withdrawn_input_step_02_rejects_an_out_of_range_index() fail {
  let block = challenged_block()
  run_step_02(
    block,
    challenged_spend_inputs(),
    7,
    first_challenged_spend_input(),
  )
}
