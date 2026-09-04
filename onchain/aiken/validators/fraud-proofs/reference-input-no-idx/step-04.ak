use aiken/collection/list
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, opened_field_view, outputs_field_index,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_output}
use midgard/fraud_proofs/native_tx/types.{MidgardTxOutput}
use midgard/fraud_proofs/reference_input_no_idx/step_04.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/native_tx_field_access_v1.{field_item_count}

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
        outputs_opening,
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

        expect Some(State { producing_tx_id, bad_reference_input_output_index })
          = m_input_state_data

        // 2. Field 2's preimage is authenticated through the §8.8 door against
        //    the commitment extracted positionally from the compact structures
        //    `producing_tx_id` binds.
        let outputs_view = opened_field_view(
          outputs_opening,
          BodyAnchor { tx_id: producing_tx_id },
          outputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )

        // 3. The challenged output index must be at or beyond the producing
        //    transaction's output count.
        //
        //    This rule consumes a *count*, which is why it reads it from the
        //    door rather than from a list it reproduced. `field_item_count`
        //    only answers where the count is authenticated — tiers 1–2 walk the
        //    whole preimage at view construction, and it refuses outright for a
        //    variable-width field under tier 3 (§5.2/§8.6). A non-existence
        //    verdict resting on an unauthenticated count would be exactly the
        //    fabricated evidence §7.4 exists to prevent, so the refusal is the
        //    right answer and this step does not work around it.
        expect
          bad_reference_input_output_index >= field_item_count(outputs_view)

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

fn run_finalize_with(
  bad_reference_input_output_index: Int,
  carried_outputs: List<MidgardTxOutput>,
) -> Bool {
  let producing_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let state = State {
    producing_tx_id: producing_block.tx_id,
    bad_reference_input_output_index,
  }
  let asset_name = fixture.ct_token_asset_name(producing_block)
  main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    fixture.test_certificate_policy_id,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        outputs_opening: fixture.inline_body_opening(
          producing_block.compact_cbor,
          carried_outputs |> list.map(encode_midgard_tx_output),
        ),
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

fn run_finalize(bad_reference_input_output_index: Int) -> Bool {
  let producing_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  run_finalize_with(
    bad_reference_input_output_index,
    producing_block.tx.body.outputs,
  )
}

/// The authenticated field-2 preimage of the producing transaction proves
/// index 7 does not exist (the producing transaction has one output).
test reference_input_no_idx_step_04_accepts_out_of_range_index() {
  run_finalize(7)
}

/// Valid-block negative selector: the referenced output index exists in the
/// producing transaction, so the challenge cannot finalize.
test reference_input_no_idx_step_04_rejects_valid_block_challenge() fail {
  run_finalize(0)
}

/// The non-existence verdict rests on the door's authenticated item count, so a
/// truncated preimage — the obvious way to make any index look out of range —
/// fails the authenticate-once hash check before the count is read.
test reference_input_no_idx_step_04_rejects_a_truncated_outputs_preimage() fail {
  run_finalize_with(0, [])
}
