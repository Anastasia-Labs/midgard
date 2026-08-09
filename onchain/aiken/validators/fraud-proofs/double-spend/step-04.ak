use aiken/collection/list
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/double_spend/step_04.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, spend_inputs_field_index,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTxInput,
}
use midgard/native_tx_machine_walk_v1.{spend_input_at}

validator main(
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
      ct.Continue(Args {
        input_index,
        output_index,
        fraud_proof_mint_redeemer_index,
        tx2_spend_inputs_opening,
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

        expect Some(State { verified_tx2_id, double_spent_input }) = m_input_state_data

        // 2. tx2's field-0 preimage is authenticated through the §8.8 door
        //    against the commitment extracted positionally from the compact
        //    structures `verified_tx2_id` binds.
        //
        //    This is the step the Q1X-F6 defect (#551) was measured on. Under
        //    the retired idiom it reproduced tx2's entire spend-input
        //    collection to re-hash it, which put the proof past the ledger's
        //    memory cap at the admissible 296-input cardinality. The door hashes
        //    the preimage once and the read below is arithmetic, so the cost no
        //    longer scales with how many inputs tx2 spends.
        let tx2_spend_inputs_view = opened_field_view(
          tx2_spend_inputs_opening,
          BodyAnchor { tx_id: verified_tx2_id },
          spend_inputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )

        // 3. The provided index must correspond to an input in the given
        //    collection such that it matches the input carried over from the
        //    previous step.
        let tx2_double_spent_input = spend_input_at(
          tx2_spend_inputs_view,
          double_spent_input_index,
        )
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
// Step-04 closes the family: the second transaction's authenticated field-0
// preimage must contain the very input carried from step-03. This is where a
// challenge against a valid block dies, because distinct transactions of a
// valid block spend disjoint inputs.

/// The second conflicting transaction — same spend inputs, different fee, hence
/// a different canonical id.
fn tx2_block() -> fixture.NativeBlockFixtureV1 {
  let base = fixture.valid_native_tx_v1()
  fixture.native_block_fixture_v1(
    MidgardTransaction {
      ..base,
      body: MidgardTransactionBody { ..base.body, fee: 2_000_000 },
    },
  )
}

fn tx2_spend_inputs() -> List<MidgardTxInput> {
  tx2_block().tx.body.inputs
}

fn double_spent_input() -> MidgardTxInput {
  expect Some(input) = list.at(tx2_spend_inputs(), 0)
  input
}

fn disjoint_input() -> MidgardTxInput {
  MidgardTxInput {
    tx_id: #"3131313131313131313131313131313131313131313131313131313131313131",
    output_index: 1,
  }
}

fn run_finalize(
  carried_double_spent_input: MidgardTxInput,
  carried_tx2_inputs: List<MidgardTxInput>,
) -> Bool {
  let block = tx2_block()
  let asset_name = fixture.ct_token_asset_name(block)
  main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          State {
            verified_tx2_id: block.tx_id,
            double_spent_input: carried_double_spent_input,
          },
        ),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        tx2_spend_inputs_opening: fixture.inline_body_opening(
          block.compact_cbor,
          carried_tx2_inputs |> list.map(encode_midgard_tx_input),
        ),
        double_spent_input_index: 0,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

/// Positive selector: the second transaction's authenticated field-0 preimage
/// contains the same input carried from step-03, so the conflict is proven and
/// the proof finalizes into a permanent fraud-proof token.
test double_spend_step_04_accepts_a_shared_spend_input() {
  run_finalize(double_spent_input(), tx2_spend_inputs())
}

/// Valid-block negative selector: in a valid block the two transactions spend
/// disjoint inputs, so the input carried from step-03 is not the one at the
/// named index of tx2's authenticated preimage and the challenge cannot
/// finalize.
test double_spend_step_04_rejects_valid_block_challenge() fail {
  run_finalize(disjoint_input(), tx2_spend_inputs())
}

/// The preimage must be tx2's own: substituting one that happens to contain the
/// carried input fails the door's authenticate-once hash check.
test double_spend_step_04_rejects_an_unauthenticated_inputs_preimage() fail {
  run_finalize(disjoint_input(), [disjoint_input()])
}
