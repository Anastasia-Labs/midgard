use aiken/collection/list
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/bounded_collection_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/double_spend/input_witness
use midgard/fraud_proofs/double_spend/step_04.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{MidgardTxInput}

validator main(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
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
        fraud_proof_mint_redeemer_index,
        tx2_spend_inputs_ref_input_index,
        double_spent_input_index,
      }) -> {
        let Transaction { inputs, outputs, redeemers, reference_inputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced at the
        //    fraud proof's spending address.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
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

        expect
            Some(State { verified_tx2_spend_inputs_hash, double_spent_input })
          = m_input_state_data

        // 2. Referenced native spend-inputs preimage for tx2 must be valid.
        let tx2_spend_inputs = input_witness.verified_spend_inputs_at(
          reference_inputs,
          tx2_spend_inputs_ref_input_index,
          verified_tx2_spend_inputs_hash,
        )

        // 3. The provided index must correspond to an input in the given list
        //    such that it matches the input carried over from previous step.
        let tx2_double_spent_input = tx2_spend_inputs
          |> list.expect_at(double_spent_input_index)
        expect double_spent_input == tx2_double_spent_input

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
// Step-04 closes the family: the second transaction's authenticated
// spend-inputs preimage must contain the very input carried from step-03. This
// is where a challenge against a valid block dies, because distinct
// transactions of a valid block spend disjoint inputs.

fn spend_inputs_hash_of(inputs: List<MidgardTxInput>) -> ByteArray {
  bounded_collection_v1.from_items(0, list.map(inputs, encode_midgard_tx_input))
}

fn double_spent_input() -> MidgardTxInput {
  MidgardTxInput {
    tx_id: #"2020202020202020202020202020202020202020202020202020202020202020",
    output_index: 0,
  }
}

fn disjoint_input() -> MidgardTxInput {
  MidgardTxInput {
    tx_id: #"3131313131313131313131313131313131313131313131313131313131313131",
    output_index: 1,
  }
}

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

fn run_finalize(tx2_spend_inputs: List<MidgardTxInput>) -> Bool {
  let asset_name = fixture.ct_token_asset_name(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
  )
  let base = fixture.finalize_step_l1_tx_v1(asset_name)
  let l1_tx = Transaction {
    ..base,
    reference_inputs: [spend_inputs_reference_input(tx2_spend_inputs)],
  }
  main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          State {
            verified_tx2_spend_inputs_hash: spend_inputs_hash_of(
              tx2_spend_inputs,
            ),
            double_spent_input: double_spent_input(),
          },
        ),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        tx2_spend_inputs_ref_input_index: 0,
        double_spent_input_index: 0,
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}

/// Positive selector: the second transaction's authenticated spend-inputs
/// preimage contains the same input carried from step-03, so the conflict is
/// proven and the proof finalizes into a permanent fraud-proof token.
test double_spend_step_04_accepts_a_shared_spend_input() {
  run_finalize([double_spent_input()])
}

/// Valid-block negative selector: in a valid block the two transactions spend
/// disjoint inputs, so the second transaction's authenticated preimage does not
/// contain the input carried from step-03 and the challenge cannot finalize.
test double_spend_step_04_rejects_valid_block_challenge() fail {
  run_finalize([disjoint_input()])
}
