//// ## Non-Existent Input UTxO Fraud Proof (Section 5.1.1)
////
//// Proves a given transaction spends an input that does not exist in the
//// initial ledger (i.e. `prev_utxos_root`) of its containing block, and that
//// it wasn't produced by any of the block's transactions.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/fraud_proofs/no_input/step_01.{Datum, SpendRedeemer}
use midgard/fraud_proofs/no_input/step_02
use midgard/ledger_state

validator main(
  step_02_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  hub_oracle: ScriptHash,
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
      ct.Continue(tx_inclusion_args) -> {
        // 1. Generic validations for passing the provided bad tx to the next
        //    step must pass.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            _m_input_state_data,
            output_script_hash,
            output_state_data,
            header,
            _bad_tx_id,
            bad_tx_view
          <- pass_native_tx_to_next_step(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          tx_inclusion_args,
          own_out_ref,
          tx,
        )

        let ledger_state.HeaderV1 { prev_utxos_root, .. } = header

        // 2. The UTxO must be produced at next step's script hash with the
        //    native spend-inputs hash of the bad transaction, and relevant
        //    block roots. The authenticated raw transactions phas root (verified
        //    against the header's counted `transactions_root` inside
        //    `pass_native_tx_to_next_step`) is threaded forward so step-04's
        //    non-membership check runs against a real MPF root.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State {
          bad_tx_inputs_hash: bad_tx_view.tx_compact.body.spend_inputs_hash,
          blocks_prev_utxos_root: prev_utxos_root,
          blocks_transactions_root: tx_inclusion_args.transactions_phas_root,
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

fn run_step_01(claimed: fixture.NativeBlockFixtureV1) -> Bool {
  let expected_output_state: Data = step_02.State {
    bad_tx_inputs_hash: claimed.compact.body.spend_inputs_hash,
    blocks_prev_utxos_root: claimed.header.prev_utxos_root,
    blocks_transactions_root: claimed.transactions_phas_root,
  }
  let l1_tx = fixture.continue_step_l1_tx_v1(
    claimed,
    expected_output_state,
    claimed.transactions_phas_root,
    claimed.tx_id,
    claimed.compact_cbor,
  )
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(fixture.native_inclusion_args_v1(claimed)),
    fixture.own_out_ref(),
    l1_tx,
  )
}

/// Positive selector: the challenged transaction is genuinely committed under
/// the block's counted `transactions_root`, so step-01 forwards its canonical
/// spend-inputs commitment together with the header's `prev_utxos_root` and the
/// authenticated raw transactions root the later non-membership checks need.
test no_input_step_01_binds_native_v1_block_fixture() {
  run_step_01(fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()))
}

/// Valid-block negative selector: a raw transactions root that is not the one
/// committed by the header's counted `transactions_root` cannot bind, so a
/// transaction fabricated against a valid block's header is rejected before any
/// non-existence claim can be made.
test no_input_step_01_rejects_forged_transactions_root() fail {
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let forged_block = fixture.native_block_fixture_v1(uncommitted_bad_tx())
  run_step_01(
    fixture.NativeBlockFixtureV1 {
      ..forged_block,
      header: valid_block.header,
      header_hash: valid_block.header_hash,
    },
  )
}

/// A native-V1 transaction that the fixture block does not commit, used as the
/// fabricated evidence of the valid-block negative.
fn uncommitted_bad_tx() -> native_types.MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  native_types.MidgardTransaction {
    ..base,
    body: native_types.MidgardTransactionBody { ..base.body, fee: 2_000_000 },
  }
}
