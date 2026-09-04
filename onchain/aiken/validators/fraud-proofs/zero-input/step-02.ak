//// ## Zero Input Fraud Proof (Section 5.1.1)
////
//// Concludes the proof that the transaction carried through step-01 spends no
//// inputs.

use aiken/collection/list
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, spend_inputs_field_index,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_compact_cbor_v1}
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_input}
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/zero_input/rule.{
  StateV1, bind_state_v1, terminal_contradiction_v1,
}
use midgard/fraud_proofs/zero_input/step_02.{Args, Datum, SpendRedeemer}
use midgard/native_tx_field_access_v1.{field_item_count}

validator main(
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
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
        fraud_proof_mint_redeemer_index,
        spend_inputs_opening,
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

        expect Some(state) = m_input_state_data

        // 2. Field 0 of the disputed transaction must hold no items.
        //
        //    The count comes from the door, which for tiers 1–2 walks the whole
        //    preimage at view construction and so answers with an authenticated
        //    number (§5.2/§7.4). Reading it here rather than comparing a
        //    forwarded commitment against the pinned empty-field constant is
        //    what ties the verdict to *this* slot: §4 removed field-index domain
        //    separation, so all nine empty fields share one commitment and a
        //    hash equality proves only that some field is empty. Positional
        //    identity lives inside the door, and this is how the family gets it.
        let spend_inputs_view = opened_field_view(
          spend_inputs_opening,
          BodyAnchor { tx_id: state.subject.transaction_id },
          spend_inputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        // Done: the terminal rule applies the authenticated verdict polarity.
        terminal_contradiction_v1(state, field_item_count(spend_inputs_view))
      }
    }
  }

  else(_) {
    fail
  }
}

// ## Family selectors (GOAL_SPEC.md §9.1 output 4)

/// A transaction that spends nothing — the `zero-input` violation.
fn zero_input_bad_tx() -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, inputs: [] },
  }
}

fn run_finalize(block: fixture.NativeBlockFixtureV1) -> Bool {
  let asset_name = fixture.ct_token_asset_name(block)
  let subject = substrate.bind_accepted_subject_v1(
    verify_native_tx_compact_cbor_v1(block.tx_id, block.compact_cbor),
  )
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(bind_state_v1(subject)),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        spend_inputs_opening: fixture.inline_body_opening(
          block.compact_cbor,
          block.tx.body.inputs |> list.map(encode_midgard_tx_input),
        ),
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

/// Positive selector: the challenged transaction's authenticated field 0 holds
/// no items, so the proof finalizes.
test zero_input_step_02_accepts_an_empty_spend_inputs_field() {
  run_finalize(fixture.native_block_fixture_v1(zero_input_bad_tx()))
}

/// Valid-block negative selector: a transaction from a valid block spends at
/// least one input, so its authenticated field-0 count is non-zero and the
/// challenge cannot finalize.
test zero_input_step_02_rejects_valid_block_challenge() fail {
  run_finalize(fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()))
}

/// The empty preimage must be the one the *disputed* transaction commits: a
/// prover who carries `80` for a transaction that really spends inputs fails
/// the door's authenticate-once hash check, not merely the count assertion.
/// This is the substitution the pinned-constant comparison could not see, since
/// under §4 the empty commitment is the same 32 bytes for every field of every
/// transaction.
test zero_input_step_02_rejects_an_empty_preimage_for_a_spending_transaction() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let asset_name = fixture.ct_token_asset_name(block)
  main.spend(
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.computation_thread_policy_id,
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(
          bind_state_v1(
            substrate.bind_accepted_subject_v1(
              verify_native_tx_compact_cbor_v1(block.tx_id, block.compact_cbor),
            ),
          ),
        ),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        spend_inputs_opening: fixture.inline_body_opening(
          block.compact_cbor,
          [] |> list.map(encode_midgard_tx_input),
        ),
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}
