//// ## Value-Not-Preserved Fraud Proof — step-02
////
//// The spent-input accumulation self-loop. Each `FoldInput` iteration reads
//// the spend input at the state's cursor out of the challenged transaction's
//// field-0 preimage through the §8.8 door, authenticates that input's
//// pre-state ledger value through the single ledger-membership witness path,
//// and adds the claimed asset's quantity to the running delta;
//// `FinishInputs` compares the cursor against the field's authenticated item
//// count and forwards the accumulated inflow to step-03. See
//// `midgard/fraud_proofs/value_not_preserved/step_02` for why pre-state
//// values convict soundly.

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
use midgard/fraud_proofs/value_not_preserved/step_01.{
  AdaAsset, ClaimedAssetInflated, ClaimedAssetV1,
}
use midgard/fraud_proofs/value_not_preserved/step_02.{
  Args, Datum, FinishInputs, FoldArgs, FoldInput, SpendRedeemer,
  SpentInputValueWitnessV1, State, spent_input_claimed_quantity_v1,
}
use midgard/fraud_proofs/value_not_preserved/step_03
use midgard/fraud_proofs/value_not_preserved/thread_fixture_v1 as tf
use midgard/native_tx_machine_walk_v1.{spend_input_at, spend_input_count}

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
      ct.Continue(FoldInput(FoldArgs {
        input_index,
        output_index,
        spend_inputs_opening,
        value_witness,
      })) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced.
        let
            own_script_hash,
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

        // 2. The spend input at the cursor is read out of the challenged
        //    transaction's field-0 preimage through the §8.8 door.
        let spend_inputs_view = opened_field_view(
          spend_inputs_opening,
          BodyAnchor { tx_id: state.bad_tx_id },
          spend_inputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        let spend_input = spend_input_at(spend_inputs_view, state.input_cursor)

        // 3. The input's pre-state value is authenticated against the frozen
        //    `prev_utxos_root` and the claimed asset's quantity extracted —
        //    the machine's stage-two contribution restricted to one unit.
        let claimed_quantity = spent_input_claimed_quantity_v1(
          state.prev_utxos_root,
          spend_input,
          value_witness,
          state.claimed_asset,
        )

        // 4. The thread self-loops with the cursor advanced and the inflow
        //    accumulated; everything else is carried unchanged.
        expect output_script_hash == own_script_hash
        let expected_output_state: Data = State {
          ..state,
          input_cursor: state.input_cursor + 1,
          claimed_delta: state.claimed_delta + claimed_quantity,
        }
        expect output_state_data == expected_output_state

        // Done.
        True
      }
      ct.Continue(FinishInputs {
        input_index,
        output_index,
        spend_inputs_opening,
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

        // 2. Every spend input must have been folded: the cursor equals the
        //    field's **authenticated** item count, read through the door —
        //    not a prover assertion. A premature finish would hide inflow;
        //    an overshot cursor is unreachable because `spend_input_at`
        //    aborts past the count.
        let spend_inputs_view = opened_field_view(
          spend_inputs_opening,
          BodyAnchor { tx_id: state.bad_tx_id },
          spend_inputs_field_index,
          reference_inputs,
          field_preimage_certificate_policy_id,
        )
        expect state.input_cursor == spend_input_count(spend_inputs_view)

        // 3. The accumulated inflow moves to step-03.
        expect output_script_hash == step_03_validator_script_hash
        let expected_output_state: Data = step_03.State {
          bad_tx_id: state.bad_tx_id,
          claimed_asset: state.claimed_asset,
          claimed_direction: state.claimed_direction,
          committed_fee: state.committed_fee,
          claimed_delta: state.claimed_delta,
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

fn initial_state(block: tf.ValueBlockV1, claimed_asset: ClaimedAssetV1) -> State {
  State {
    bad_tx_id: block.block.tx_id,
    claimed_asset,
    claimed_direction: ClaimedAssetInflated,
    committed_fee: block.block.compact.body.fee,
    prev_utxos_root: block.block.header.prev_utxos_root,
    input_cursor: 0,
    claimed_delta: 0,
  }
}

fn spend_inputs_opening_of(block: tf.ValueBlockV1) {
  fixture.inline_body_opening(
    block.block.compact_cbor,
    list.map(block.block.tx.body.inputs, encode_midgard_tx_input),
  )
}

fn run_fold(
  block: tf.ValueBlockV1,
  claimed_asset: ClaimedAssetV1,
  value_witness: SpentInputValueWitnessV1,
  expected_quantity: Int,
) -> Bool {
  let state = initial_state(block, claimed_asset)
  let expected_output_state: Data = State {
    ..state,
    input_cursor: 1,
    claimed_delta: expected_quantity,
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
      FoldInput(
        FoldArgs {
          input_index: 0,
          output_index: 0,
          spend_inputs_opening: spend_inputs_opening_of(block),
          value_witness,
        },
      ),
    ),
    fixture.own_out_ref(),
    tf.self_loop_l1_tx_v1(asset_name, expected_output_state),
  )
}

fn run_finish(
  block: tf.ValueBlockV1,
  claimed_asset: ClaimedAssetV1,
  input_cursor: Int,
  claimed_delta: Int,
) -> Bool {
  let state = State {
    ..initial_state(block, claimed_asset),
    input_cursor,
    claimed_delta,
  }
  let expected_output_state: Data = step_03.State {
    bad_tx_id: state.bad_tx_id,
    claimed_asset: state.claimed_asset,
    claimed_direction: state.claimed_direction,
    committed_fee: state.committed_fee,
    claimed_delta: state.claimed_delta,
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
      FinishInputs {
        input_index: 0,
        output_index: 0,
        spend_inputs_opening: spend_inputs_opening_of(block),
      },
    ),
    fixture.own_out_ref(),
    fixture.continue_mid_step_l1_tx_v1(asset_name, expected_output_state),
  )
}

fn token_block() -> tf.ValueBlockV1 {
  tf.value_block_v1(tf.token_inflated_tx_v1(), tf.spent_output_with_tokens_v1())
}

fn ada_block() -> tf.ValueBlockV1 {
  tf.value_block_v1(tf.ada_inflated_tx_v1(), tf.spent_output_ada_only_v1())
}

/// Positive selector (token claim): the committed descriptor's full
/// authenticated asset-leaf walk yields the claimed unit's quantity — 40 —
/// and the thread self-loops with it accumulated.
test value_not_preserved_step_02_folds_a_token_spent_input() {
  let block = token_block()
  run_fold(
    block,
    tf.claimed_token_v1(),
    tf.spent_input_value_witness_v1(block, tf.claimed_token_v1()),
    40,
  )
}

/// Positive selector (ADA claim): the descriptor's own `lovelace` field is
/// the contribution; no leaf walk exists to run.
test value_not_preserved_step_02_folds_an_ada_spent_input() {
  let block = ada_block()
  run_fold(
    block,
    AdaAsset,
    tf.spent_input_value_witness_v1(block, AdaAsset),
    10_000_000,
  )
}

/// Valid-block negative selector: descriptor bytes the header's
/// `prev_utxos_root` never committed fail the ledger-membership check — a
/// prover cannot feed the fold a value of its own choosing.
test value_not_preserved_step_02_rejects_a_forged_descriptor() fail {
  let block = token_block()
  let foreign = tf.value_block_v1(
    tf.token_inflated_tx_v1(),
    tf.spent_output_ada_only_v1(),
  )
  run_fold(
    block,
    tf.claimed_token_v1(),
    SpentInputValueWitnessV1 {
      ..tf.spent_input_value_witness_v1(block, tf.claimed_token_v1()),
      descriptor_cbor: foreign.descriptor_cbor,
    },
    40,
  )
}

/// The witness must open **every** asset leaf the descriptor commits: a
/// truncated walk (1 opening against `asset_count == 2`) is refused, so a
/// zero contribution is always an established absence.
test value_not_preserved_step_02_rejects_a_truncated_asset_walk() fail {
  let block = token_block()
  let genuine = tf.spent_input_value_witness_v1(block, tf.claimed_token_v1())
  expect [first_opening, ..] = genuine.asset_openings
  run_fold(
    block,
    tf.claimed_token_v1(),
    SpentInputValueWitnessV1 { ..genuine, asset_openings: [first_opening] },
    40,
  )
}

/// A leaf opening restating the claimed unit's quantity fails the leaf-hash
/// authentication: quantities come from the committed frontier, never from
/// the prover.
test value_not_preserved_step_02_rejects_a_forged_leaf_quantity() fail {
  let block = token_block()
  let genuine = tf.spent_input_value_witness_v1(block, tf.claimed_token_v1())
  expect [first_opening, ..rest] = genuine.asset_openings
  run_fold(
    block,
    tf.claimed_token_v1(),
    SpentInputValueWitnessV1 {
      ..genuine,
      asset_openings: [
        step_02.AssetLeafOpeningV1 { ..first_opening, quantity: 400 },
        ..rest
      ],
    },
    400,
  )
}

/// Positive selector: after the single spend input is folded the cursor
/// equals the field's authenticated item count and the inflow moves to
/// step-03.
test value_not_preserved_step_02_finishes_after_the_last_input() {
  run_finish(token_block(), tf.claimed_token_v1(), 1, 40)
}

/// A premature finish (cursor 0 against an authenticated count of 1) would
/// hide inflow and is refused.
test value_not_preserved_step_02_rejects_a_premature_finish() fail {
  run_finish(token_block(), tf.claimed_token_v1(), 0, 0)
}
