use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, reference_inputs_field_index,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/fraud_proofs/no_reference_input/step_02.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/no_reference_input/step_03
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
        reference_inputs_opening,
        bad_reference_input_index,
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
              blocks_prev_utxos_root,
              blocks_transactions_root,
            })
          = m_input_state_data

        // 2. Target reference input is read out of the disputed transaction's
        //    field-1 preimage through the §8.8 door: the commitment is
        //    extracted positionally from the compact structures `bad_tx_id`
        //    authenticates, and the item is one arithmetic slice at §5.3's
        //    fixed stride rather than a reproduction of the whole collection.
        let reference_inputs_view = opened_field_view(
          reference_inputs_opening,
          BodyAnchor { tx_id: bad_tx_id },
          reference_inputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        // `spend_input_at` reads field **1** here, not field 0, and that is
        // deliberate: §5.3 gives spend inputs and reference inputs the same
        // fixed-stride item shape, so the same O(1) reader serves both (see its
        // own doc — "a spend- **or reference-input** item"). The field index is
        // the literal above and the reader's stride guard refuses any view whose
        // items are not that shape, so nothing about this can silently become a
        // field-0 read.
        let missing_reference_input = spend_input_at(
          reference_inputs_view,
          bad_reference_input_index,
        )

        // 3. Target reference input, plus the relevant block roots, must be
        //    passed to the next step.
        expect output_script_hash == step_03_validator_script_hash
        let expected_output_state: Data = step_03.State {
          missing_reference_input,
          blocks_prev_utxos_root,
          blocks_transactions_root,
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
// Step-02 opens the disputed transaction's field-1 preimage through the one
// door and selects the reference input the prover challenges. The expected
// commitment is derived positionally inside the door from the tx-id-verified
// compact structures, so a substituted preimage cannot be opened; and because
// §5.3 fixes the item width, the read is arithmetic at any index.

fn challenged_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

fn challenged_reference_inputs() -> List<MidgardTxInput> {
  challenged_block().tx.body.reference_inputs
}

fn first_challenged_reference_input() -> MidgardTxInput {
  expect Some(input) = list.at(challenged_reference_inputs(), 0)
  input
}

fn run_step_02(
  block: fixture.NativeBlockFixtureV1,
  carried_reference_inputs: List<MidgardTxInput>,
  bad_reference_input_index: Int,
  expected_missing_reference_input: MidgardTxInput,
) -> Bool {
  let expected_output_state: Data = step_03.State {
    missing_reference_input: expected_missing_reference_input,
    blocks_prev_utxos_root: block.header.prev_utxos_root,
    blocks_transactions_root: block.transactions_phas_root,
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
            blocks_prev_utxos_root: block.header.prev_utxos_root,
            blocks_transactions_root: block.transactions_phas_root,
          },
        ),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        reference_inputs_opening: fixture.inline_body_opening(
          block.compact_cbor,
          carried_reference_inputs |> list.map(encode_midgard_tx_input),
        ),
        bad_reference_input_index,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
  )
}

/// Positive selector: the carried preimage is the one the disputed
/// transaction's field-1 commitment authenticates, so the challenged reference
/// input reaches step-03 with both block roots.
test no_reference_input_step_02_forwards_the_challenged_reference_input() {
  let block = challenged_block()
  run_step_02(
    block,
    challenged_reference_inputs(),
    0,
    first_challenged_reference_input(),
  )
}

/// Valid-block negative selector: a valid block commits each transaction's real
/// reference-input collection, so a prover who carries a different preimage
/// fails the door's authenticate-once check.
test no_reference_input_step_02_rejects_a_substituted_preimage() fail {
  let block = challenged_block()
  let injected = MidgardTxInput {
    tx_id: #"3333333333333333333333333333333333333333333333333333333333333333",
    output_index: 0,
  }
  run_step_02(block, [injected], 0, injected)
}

/// §4 positional identity: field 0 and field 1 commit identically over the same
/// items, so the only thing that makes this the reference-input field is the
/// index the door is opened at. Carrying the transaction's *spend*-input
/// preimage here is rejected because it is not what field 1 commits.
test no_reference_input_step_02_rejects_the_spend_inputs_preimage() fail {
  let block = challenged_block()
  run_step_02(
    block,
    block.tx.body.inputs,
    0,
    first_challenged_reference_input(),
  )
}
