use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/bounded_collection_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/double_spend/input_witness
use midgard/fraud_proofs/double_spend/step_03.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/double_spend/step_04
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}

validator main(
  step_04_validator_script_hash: ScriptHash,
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
        tx1_spend_inputs_ref_input_index,
        double_spent_input_index,
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
              verified_tx1_spend_inputs_hash,
              verified_tx2_spend_inputs_hash,
            })
          = m_input_state_data

        // 2. Referenced native spend-inputs preimage for tx1 must be valid.
        let tx1_spend_inputs = input_witness.verified_spend_inputs_at(
          reference_inputs,
          tx1_spend_inputs_ref_input_index,
          verified_tx1_spend_inputs_hash,
        )

        // 3. The provided index must correspond to an input in the given list.
        let tx1_double_spent_input = tx1_spend_inputs
          |> list.expect_at(double_spent_input_index)

        // 4. The fetched input, along with the unused second transaction inputs
        //    hash, must be carried over in the datum of the next step.
        expect output_script_hash == step_04_validator_script_hash
        let expected_output_state: Data = step_04.State {
          verified_tx2_spend_inputs_hash,
          double_spent_input: tx1_double_spent_input,
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
// Step-03 opens the first transaction's authenticated spend-inputs commitment
// from a reference input and selects the allegedly double-spent input. Only a
// preimage whose canonical bounded-collection commitment equals the forwarded
// hash may be opened.

/// The second conflicting transaction's canonical spend-inputs commitment. It is
/// opaque to this step — step-04 opens it — but it is a real native-V1
/// commitment, not a placeholder.
fn tx2_spend_inputs_hash() -> ByteArray {
  fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()).compact.body.spend_inputs_hash
}

fn spend_inputs_hash_of(inputs: List<MidgardTxInput>) -> ByteArray {
  bounded_collection_v1.from_items(0, list.map(inputs, encode_midgard_tx_input))
}

fn tx1_spend_inputs() -> List<MidgardTxInput> {
  [
    MidgardTxInput {
      tx_id: #"2020202020202020202020202020202020202020202020202020202020202020",
      output_index: 0,
    },
  ]
}

/// Reference input carrying a native spend-inputs preimage as an inline datum,
/// exactly as the prover publishes it for `input_witness`.
fn spend_inputs_reference_input(inputs: List<MidgardTxInput>) -> Input {
  let inputs_data: Data = inputs
  Input {
    output_reference: OutputReference {
      transaction_id: #"6060606060606060606060606060606060606060606060606060606060606060",
      output_index: 0,
    },
    output: Output {
      address: address.from_script(fixture.step_script_hash),
      value: assets.from_lovelace(2_000_000),
      datum: InlineDatum(inputs_data),
      reference_script: None,
    },
  }
}

fn run_step_03(
  committed_tx1_inputs: List<MidgardTxInput>,
  published_tx1_inputs: List<MidgardTxInput>,
  double_spent_input_index: Int,
) -> Bool {
  expect Some(double_spent_input) = list.at(
    published_tx1_inputs,
    double_spent_input_index,
  )
  let expected_output_state: Data = step_04.State {
    verified_tx2_spend_inputs_hash: tx2_spend_inputs_hash(),
    double_spent_input,
  }
  let asset_name = fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
  let l1_tx = Transaction {
    ..transaction.placeholder,
    inputs: [fixture.ct_thread_input(asset_name)],
    outputs: [fixture.next_step_output(asset_name, expected_output_state)],
    reference_inputs: [spend_inputs_reference_input(published_tx1_inputs)],
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          State {
            verified_tx1_spend_inputs_hash: spend_inputs_hash_of(
              committed_tx1_inputs,
            ),
            verified_tx2_spend_inputs_hash: tx2_spend_inputs_hash(),
          },
        ),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        tx1_spend_inputs_ref_input_index: 0,
        double_spent_input_index,
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}

/// Positive selector: the published preimage reproduces the authenticated
/// spend-inputs commitment of the first transaction exactly, so the selected
/// input is carried to step-04 alongside the untouched second commitment.
test double_spend_step_03_forwards_the_double_spent_input() {
  let inputs = tx1_spend_inputs()
  run_step_03(inputs, inputs, 0)
}

/// Valid-block negative selector: distinct transactions of a valid block spend
/// disjoint inputs, so a conflict can only be manufactured by publishing a
/// preimage that is not the bound transaction's. The authenticated
/// bounded-collection commitment rejects it.
test double_spend_step_03_rejects_an_unauthenticated_inputs_preimage() fail {
  let substituted = [
    MidgardTxInput {
      tx_id: #"3131313131313131313131313131313131313131313131313131313131313131",
      output_index: 1,
    },
  ]
  run_step_03(tx1_spend_inputs(), substituted, 0)
}
