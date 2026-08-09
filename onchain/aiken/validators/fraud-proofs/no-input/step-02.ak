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
use midgard/fraud_proofs/no_input/step_02.{Args, Datum, SpendRedeemer, State}
use midgard/fraud_proofs/no_input/step_03
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
              blocks_prev_utxos_root,
              blocks_transactions_root,
            })
          = m_input_state_data

        // 2. Target input is read out of the disputed transaction's field-0
        //    preimage through the §8.8 door: the expected commitment is
        //    extracted positionally from the compact structures `bad_tx_id`
        //    authenticates, never supplied, and the item itself is one
        //    arithmetic slice at the §5.3 fixed stride (§10.5) rather than a
        //    reproduction of the whole collection.
        let spend_inputs_view = opened_field_view(
          spend_inputs_opening,
          BodyAnchor { tx_id: bad_tx_id },
          spend_inputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let missing_input = spend_input_at(spend_inputs_view, bad_input_index)

        // 3. Target input, plus the relevant block roots, must be passed to the
        //    next step.
        expect output_script_hash == step_03_validator_script_hash
        let expected_output_state: Data = step_03.State {
          missing_input,
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
// Step-02 opens the disputed transaction's field-0 preimage through the one
// door and selects the input the prover claims does not exist. Because the
// expected commitment is extracted positionally inside the door from the
// tx-id-verified compact structures, a substituted preimage cannot be opened at
// all — and because the read is arithmetic, selecting input 295 costs what
// selecting input 0 costs.

/// The block whose header authenticated the challenged transaction at step-01.
/// Its `prev_utxos_root` and authenticated raw transactions root are the exact
/// values step-01 forwards, so this step runs on real roots, not placeholders.
fn challenged_block() -> fixture.NativeBlockFixtureV1 {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
}

fn prev_utxos_root() -> ByteArray {
  challenged_block().header.prev_utxos_root
}

fn transactions_root() -> ByteArray {
  challenged_block().transactions_phas_root
}

fn run_step_02(
  block: fixture.NativeBlockFixtureV1,
  carried_inputs: List<MidgardTxInput>,
  bad_input_index: Int,
  expected_missing_input: MidgardTxInput,
) -> Bool {
  let expected_output_state: Data = step_03.State {
    missing_input: expected_missing_input,
    blocks_prev_utxos_root: prev_utxos_root(),
    blocks_transactions_root: transactions_root(),
  }
  let asset_name = fixture.ct_token_asset_name(challenged_block())
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
            blocks_prev_utxos_root: prev_utxos_root(),
            blocks_transactions_root: transactions_root(),
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
          carried_inputs |> list.map(encode_midgard_tx_input),
        ),
        bad_input_index,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
  )
}

fn sample_input(tx_id: ByteArray, output_index: Int) -> MidgardTxInput {
  MidgardTxInput { tx_id, output_index }
}

/// The spend inputs the fixture transaction actually commits — the only
/// preimage the door will open for it.
fn challenged_inputs() -> List<MidgardTxInput> {
  challenged_block().tx.body.inputs
}

fn first_challenged_input() -> MidgardTxInput {
  expect Some(input) = list.at(challenged_inputs(), 0)
  input
}

/// Positive selector: the carried preimage is the one the disputed
/// transaction's field-0 commitment authenticates, so the input at the
/// challenged index is carried to step-03 together with both block roots.
test no_input_step_02_forwards_the_challenged_spend_input() {
  run_step_02(
    challenged_block(),
    challenged_inputs(),
    0,
    first_challenged_input(),
  )
}

/// Valid-block negative selector: a valid block commits each transaction's real
/// spend-inputs collection, so a prover who carries a different preimage fails
/// the door's authenticate-once check and cannot select an input the
/// transaction never spent.
test no_input_step_02_rejects_a_substituted_inputs_preimage() fail {
  let injected = sample_input(
    #"3333333333333333333333333333333333333333333333333333333333333333",
    0,
  )
  run_step_02(
    challenged_block(),
    [injected, ..challenged_inputs()],
    0,
    injected,
  )
}

/// §7.3 abort-never-clamp, at the family's own call site: an index past the
/// committed collection fails the read instead of returning the nearest item.
test no_input_step_02_rejects_an_out_of_range_input_index() fail {
  run_step_02(
    challenged_block(),
    challenged_inputs(),
    64,
    first_challenged_input(),
  )
}
