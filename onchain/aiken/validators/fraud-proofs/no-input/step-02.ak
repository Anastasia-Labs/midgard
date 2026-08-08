use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}
use midgard/fraud_proofs/no_input/step_02.{Args, Datum, SpendRedeemer, State}
use midgard/fraud_proofs/no_input/step_03
use midgard/native_tx_field_access_v1

validator main(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
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
        inputs_preimage,
        bad_input_index,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx

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
              bad_tx_inputs_hash,
              blocks_prev_utxos_root,
              blocks_transactions_root,
            })
          = m_input_state_data

        // 2. Target input must be included in the verified transaction's native
        //    spend-inputs collection. Each item is the node's canonical
        //    `TransactionInput` CBOR, not `cbor.serialise` of the Aiken
        //    `OutputReference` list.
        expect
          native_tx_field_access_v1.field_commitment_from_items(list.map(inputs_preimage, encode_midgard_tx_input)) == bad_tx_inputs_hash
        let missing_input = inputs_preimage |> list.expect_at(bad_input_index)

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
// Step-02 opens the authenticated spend-inputs commitment forwarded by step-01
// and selects the input the prover claims does not exist. Only a preimage whose
// canonical bounded-collection commitment equals the forwarded hash may be
// opened, so a substituted input list cannot reach step-03.

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
  committed_inputs: List<MidgardTxInput>,
  inputs_preimage: List<MidgardTxInput>,
  bad_input_index: Int,
) -> Bool {
  let bad_tx_inputs_hash = native_tx_field_access_v1.field_commitment_from_items(list.map(committed_inputs, encode_midgard_tx_input))
  expect Some(missing_input) = list.at(inputs_preimage, bad_input_index)
  let expected_output_state: Data = step_03.State {
    missing_input,
    blocks_prev_utxos_root: prev_utxos_root(),
    blocks_transactions_root: transactions_root(),
  }
  let asset_name = fixture.ct_token_asset_name(challenged_block())
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          State {
            bad_tx_inputs_hash,
            blocks_prev_utxos_root: prev_utxos_root(),
            blocks_transactions_root: transactions_root(),
          },
        ),
      },
    ),
    ct.Continue(
      Args { input_index: 0, output_index: 0, inputs_preimage, bad_input_index },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
  )
}

fn sample_input(tx_id: ByteArray, output_index: Int) -> MidgardTxInput {
  MidgardTxInput { tx_id, output_index }
}

fn challenged_inputs() -> List<MidgardTxInput> {
  [
    sample_input(
      #"3131313131313131313131313131313131313131313131313131313131313131",
      0,
    ),
    sample_input(
      #"3232323232323232323232323232323232323232323232323232323232323232",
      7,
    ),
  ]
}

/// Positive selector: the supplied preimage reproduces the forwarded canonical
/// spend-inputs commitment exactly, so the input at the challenged index is
/// carried to step-03 together with both block roots.
test no_input_step_02_forwards_the_challenged_spend_input() {
  let inputs = challenged_inputs()
  run_step_02(inputs, inputs, 1)
}

/// Valid-block negative selector: a valid block commits each transaction's real
/// spend-inputs collection, so a prover who substitutes a different input list
/// for the authenticated commitment is rejected here and cannot select an input
/// the transaction never spent.
test no_input_step_02_rejects_a_substituted_inputs_preimage() fail {
  let inputs = challenged_inputs()
  let substituted = [
    sample_input(
      #"3333333333333333333333333333333333333333333333333333333333333333",
      0,
    ),
    ..inputs
  ]
  run_step_02(inputs, substituted, 0)
}
