//// ## Double Spend Fraud Proof (Section 5.1.1)
////
//// Proves 2 unique transactions exist such that both spend the same UTxO.
////
//// First step's contract is responsible for verifying inclusion of the first
//// transaction in a Midgard block, and passing it to the second step's
//// contract. The second step verifies inclusion of the second transaction.
////
//// This is meant to take place in four transactions:
//// 1. First transaction spends the initialized fraud proof UTxO (which has no
////    `data`) and reproduces the UTxO at the next step's script address with
////    `data` containing the first verified bad transaction's ID and inputs
////    hash.
//// 2. Second transaction spends the UTxO produced in the first transaction and
////    produces the UTxO at the next step's script address with `data`
////    containing both verified bad transactions' inputs hashes.
//// 3. Third transaction provides the first bad transaction's inputs preimage and
////    carries the double-spent output reference to the next step.
//// 4. Fourth transaction provides the second bad transaction's inputs preimage
////    and finalizes the fraud proof.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step}
use midgard/fraud_proofs/double_spend/step_01.{Datum, SpendRedeemer}
use midgard/fraud_proofs/double_spend/step_02
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody,
}

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
            m_input_state_data,
            output_script_hash,
            output_state_data,
            _header,
            bad_tx_id,
            bad_tx_view
          <- pass_native_tx_to_next_step(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          tx_inclusion_args,
          own_out_ref,
          tx,
        )

        expect None = m_input_state_data

        // 2. The UTxO must be produced at next step's script hash with the
        //    verified first bad transaction ID and native spend-input hash in
        //    the `data` field of its `StepDatum`.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State {
          verified_tx1_id: bad_tx_id,
          verified_tx1_spend_inputs_hash: bad_tx_view.tx_compact.body.spend_inputs_hash,
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
// Step-01 binds the first of the two conflicting transactions to a committed
// block: only a transaction genuinely held by the header's counted
// `transactions_root` may forward its id and canonical spend-inputs commitment.

/// The second conflicting transaction: identical spend inputs, a different fee,
/// hence a different canonical transaction id. A block holding both is the
/// `double-spend` violation.
fn conflicting_native_tx_v1() -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, fee: 2_000_000 },
  }
}

fn first_step_datum() -> Datum {
  ct.StepDatum { fraud_prover: fixture.fraud_prover_key_hash, data: None }
}

fn run_step_01(claimed: fixture.NativeBlockFixtureV1) -> Bool {
  let expected_output_state: Data = step_02.State {
    verified_tx1_id: claimed.tx_id,
    verified_tx1_spend_inputs_hash: claimed.compact.body.spend_inputs_hash,
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
    Some(first_step_datum()),
    ct.Continue(fixture.native_inclusion_args_v1(claimed)),
    fixture.own_out_ref(),
    l1_tx,
  )
}

/// Positive selector: the first conflicting transaction is genuinely committed
/// under the block's counted `transactions_root`, so its id and canonical
/// spend-inputs commitment reach step-02.
test double_spend_step_01_binds_native_v1_block_fixture() {
  run_step_01(fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()))
}

/// Valid-block negative selector: a raw transactions root that is not the one
/// committed by the header's counted `transactions_root` cannot bind, so a
/// transaction fabricated against a valid block's header is rejected before any
/// conflict can be claimed.
test double_spend_step_01_rejects_forged_transactions_root() fail {
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let forged_block = fixture.native_block_fixture_v1(conflicting_native_tx_v1())
  run_step_01(
    fixture.NativeBlockFixtureV1 {
      ..forged_block,
      header: valid_block.header,
      header_hash: valid_block.header_hash,
    },
  )
}
