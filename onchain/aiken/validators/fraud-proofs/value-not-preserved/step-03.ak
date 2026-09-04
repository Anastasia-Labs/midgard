//// ## Value-Not-Preserved Fraud Proof — step-03
////
//// Completes the conservation fold: the §3 anchor is paid once
//// (`anchored_native_tx`) and the challenged transaction's outputs — plus,
//// for a token claim, its mint field — are folded **whole** through the §8.8
//// door, subtracting outflow and adding mint. An ADA claim instead subtracts
//// the committed fee and must carry no mint carriage (ADA is structurally
//// unmintable, structural-na-q24). See
//// `midgard/fraud_proofs/value_not_preserved/step_03`.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, BodyTxOpening, anchored_field_walk, anchored_native_tx,
  fold_opened_field, mint_field_index, outputs_field_index,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/preimages.{
  encode_mint_preimage, encode_output_preimage,
}
use midgard/fraud_proofs/value_not_preserved/step_01.{
  AdaAsset, ClaimedAssetInflated, ClaimedAssetV1, TokenAsset,
}
use midgard/fraud_proofs/value_not_preserved/step_03.{
  Args, Datum, SpendRedeemer, State, mint_item_claimed_quantity_v1,
  output_claimed_quantity_v1,
}
use midgard/fraud_proofs/value_not_preserved/step_04
use midgard/fraud_proofs/value_not_preserved/thread_fixture_v1 as tf
use midgard/native_tx_field_access_v1 as field_access

validator main(
  step_04_validator_script_hash: ScriptHash,
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
        native_tx_compact_cbor,
        outputs_carriage,
        mint_carriage,
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

        expect Some(state) = m_input_state_data

        // 2. The §3 anchor is paid once; both fields open off the handle.
        let anchored = anchored_native_tx(
          BodyTxOpening { native_tx_compact_cbor },
          BodyAnchor { tx_id: state.bad_tx_id },
        )

        // 3. Stage three, restricted to the claimed unit: every output's
        //    claimed quantity is outflow. The fold walks the whole field, so
        //    a zero outflow is an established absence.
        let outputs_walk = anchored_field_walk(
          anchored,
          outputs_field_index,
          outputs_carriage,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let delta_after_outputs = fold_opened_field(
          outputs_walk,
          state.claimed_delta,
          fn(accumulated, _index, output_item_cbor) {
            accumulated - output_claimed_quantity_v1(
              output_item_cbor,
              state.claimed_asset,
            )
          },
        )

        // 4. The remaining terms of the claimed asset's conservation
        //    equation.
        let final_delta = when state.claimed_asset is {
          // ADA: subtract the root-committed fee. No mint term exists
          // (structural-na-q24), so no mint carriage may be supplied.
          AdaAsset -> {
            expect None = mint_carriage
            delta_after_outputs - state.committed_fee
          }
          // Token: stage four, restricted to the claimed unit — the whole
          // mint field is folded so a minted-and-fully-paid-out unit ends
          // balanced rather than looking inflated.
          TokenAsset { .. } -> {
            expect Some(carriage) = mint_carriage
            let mint_walk = anchored_field_walk(
              anchored,
              mint_field_index,
              carriage,
              reference_inputs,
              field_preimage_certificate_policy_id,
            )
            fold_opened_field(
              mint_walk,
              delta_after_outputs,
              fn(accumulated, _index, mint_item_cbor) {
                accumulated + mint_item_claimed_quantity_v1(
                  mint_item_cbor,
                  state.claimed_asset,
                )
              },
            )
          }
        }

        // 5. The completed fold moves to the decisive step.
        expect output_script_hash == step_04_validator_script_hash
        let expected_output_state: Data = step_04.State {
          bad_tx_id: state.bad_tx_id,
          claimed_asset: state.claimed_asset,
          claimed_direction: state.claimed_direction,
          final_delta,
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

// ## Family selectors

fn state_after_inputs(
  block: tf.ValueBlockV1,
  claimed_asset: ClaimedAssetV1,
  claimed_delta: Int,
) -> State {
  State {
    bad_tx_id: block.block.tx_id,
    claimed_asset,
    claimed_direction: ClaimedAssetInflated,
    committed_fee: block.block.compact.body.fee,
    claimed_delta,
  }
}

fn run_step_03(
  block: tf.ValueBlockV1,
  state: State,
  outputs_preimage: ByteArray,
  mint_carriage: Option<field_access.FieldCarriageV1>,
  expected_final_delta: Int,
) -> Bool {
  let expected_output_state: Data = step_04.State {
    bad_tx_id: state.bad_tx_id,
    claimed_asset: state.claimed_asset,
    claimed_direction: state.claimed_direction,
    final_delta: expected_final_delta,
  }
  let asset_name = tf.thread_asset_name_v1(block)
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
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
        native_tx_compact_cbor: block.block.compact_cbor,
        outputs_carriage: field_access.Inline { preimage: outputs_preimage },
        mint_carriage,
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
  )
}

fn genuine_outputs_preimage(block: tf.ValueBlockV1) -> ByteArray {
  encode_output_preimage(block.block.tx.body.outputs)
}

fn genuine_mint_carriage(
  block: tf.ValueBlockV1,
) -> Option<field_access.FieldCarriageV1> {
  Some(
    field_access.Inline {
      preimage: encode_mint_preimage(block.block.tx.body.mint),
    },
  )
}

/// Positive selector (token claim): 40 sourced, 50 paid out, nothing minted —
/// the completed fold is `40 − 50 = −10`.
test value_not_preserved_step_03_completes_the_token_fold() {
  let block = tf.value_block_v1(
    tf.token_inflated_tx_v1(),
    tf.spent_output_with_tokens_v1(),
  )
  run_step_03(
    block,
    state_after_inputs(block, tf.claimed_token_v1(), 40),
    genuine_outputs_preimage(block),
    genuine_mint_carriage(block),
    -10,
  )
}

/// Positive selector (ADA claim): 10 ADA sourced, 9.5 ADA paid out, 1 ADA
/// fee — the completed fold is `−500_000`, with no mint carriage.
test value_not_preserved_step_03_completes_the_ada_fold() {
  let block = tf.value_block_v1(
    tf.ada_inflated_tx_v1(),
    tf.spent_output_ada_only_v1(),
  )
  run_step_03(
    block,
    state_after_inputs(block, AdaAsset, 10_000_000),
    genuine_outputs_preimage(block),
    None,
    -500_000,
  )
}

/// The minted-and-fully-paid-out corner completes at zero: 0 sourced, 25
/// minted, 25 paid out. The zero is forwarded — refusing to convict on it is
/// step-04's decisive check.
test value_not_preserved_step_03_folds_mint_against_outputs() {
  let block = tf.value_block_v1(
    tf.minted_paid_out_tx_v1(),
    tf.spent_output_ada_only_v1(),
  )
  run_step_03(
    block,
    state_after_inputs(block, tf.claimed_token_v1(), 0),
    genuine_outputs_preimage(block),
    genuine_mint_carriage(block),
    0,
  )
}

/// Valid-block negative selector: an outputs preimage the transaction never
/// committed fails the door's authenticate-once check — outflow cannot be
/// understated by substituting outputs.
test value_not_preserved_step_03_rejects_a_substituted_outputs_preimage() fail {
  let block = tf.value_block_v1(
    tf.token_inflated_tx_v1(),
    tf.spent_output_with_tokens_v1(),
  )
  let balanced = tf.value_block_v1(
    tf.token_balanced_tx_v1(),
    tf.spent_output_with_tokens_v1(),
  )
  run_step_03(
    block,
    state_after_inputs(block, tf.claimed_token_v1(), 40),
    genuine_outputs_preimage(balanced),
    genuine_mint_carriage(block),
    0,
  )
}

/// A token claim must open the mint field: a thread that omits the carriage
/// is refused rather than allowed to present a minted unit as inflated.
test value_not_preserved_step_03_rejects_a_missing_mint_carriage() fail {
  let block = tf.value_block_v1(
    tf.minted_paid_out_tx_v1(),
    tf.spent_output_ada_only_v1(),
  )
  run_step_03(
    block,
    state_after_inputs(block, tf.claimed_token_v1(), 0),
    genuine_outputs_preimage(block),
    None,
    -25,
  )
}
