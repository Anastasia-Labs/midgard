use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step_carried}
use midgard/fraud_proofs/double_spend/step_02.{Datum, SpendRedeemer, State}
use midgard/fraud_proofs/double_spend/step_03
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody,
}

validator main(
  step_03_validator_script_hash: ScriptHash,
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
            tx2_id,
            tx2_view
          <- pass_native_tx_to_next_step_carried(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          tx_inclusion_args,
          own_out_ref,
          tx,
        )

        expect Some(State { verified_tx1_id, verified_tx1_spend_inputs_hash }) = m_input_state_data

        // 2. The two transactions must be different.
        expect verified_tx1_id != tx2_id

        // 3. The UTxO must be produced at next step's script hash with both
        //    verified bad transactions' native spend-input hashes in its datum.
        expect output_script_hash == step_03_validator_script_hash
        let expected_output_state: Data = step_03.State {
          verified_tx1_spend_inputs_hash,
          verified_tx2_spend_inputs_hash: tx2_view.tx_compact.body.spend_inputs_hash,
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
// Step-02 binds the second conflicting transaction to a committed block and
// enforces the distinctness requirement that makes the family sound: the two
// canonical transaction ids must differ.

/// The second conflicting transaction: identical spend inputs, a different fee,
/// hence a different canonical transaction id.
fn conflicting_native_tx_v1() -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, fee: 2_000_000 },
  }
}

fn run_step_02(
  tx2_block: fixture.NativeBlockFixtureV1,
  verified_tx1_id: ByteArray,
  verified_tx1_spend_inputs_hash: ByteArray,
) -> Bool {
  let expected_output_state: Data = step_03.State {
    verified_tx1_spend_inputs_hash,
    verified_tx2_spend_inputs_hash: tx2_block.compact.body.spend_inputs_hash,
  }
  let l1_tx = fixture.continue_step_l1_tx_v1(
    tx2_block,
    expected_output_state,
    tx2_block.transactions_phas_root,
    tx2_block.tx_id,
    tx2_block.compact_cbor,
  )
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(State { verified_tx1_id, verified_tx1_spend_inputs_hash }),
      },
    ),
    ct.Continue(fixture.native_inclusion_carriage_v1(tx2_block)),
    fixture.own_out_ref(),
    l1_tx,
  )
}

/// Positive selector: the second conflicting transaction is genuinely committed
/// under the block's counted `transactions_root` and is a different transaction
/// from the one bound at step-01, so both spend-inputs commitments reach
/// step-03.
test double_spend_step_02_forwards_both_spend_inputs_hashes() {
  let tx1_block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let tx2_block = fixture.native_block_fixture_v1(conflicting_native_tx_v1())
  run_step_02(
    tx2_block,
    tx1_block.tx_id,
    tx1_block.compact.body.spend_inputs_hash,
  )
}

// ## Published-chunk carriage (issue #545)

const adversarial_branch_levels: Int = 22

const adversarial_chunk_count: Int = 2

/// Positive selector: the second conflicting transaction is bound through a
/// 22-level membership proof carried by published chunks — deeper than the
/// direct route can carry — and the distinctness argument is unchanged.
test double_spend_step_02_binds_a_published_chunk_membership_proof() {
  let tx1_block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let base = fixture.native_block_fixture_v1(conflicting_native_tx_v1())
  let steps = fixture.adversarial_branch_steps(adversarial_branch_levels)
  let tx2_block = fixture.deep_transactions_root_block_fixture_v1(
    base,
    fixture.adversarial_membership_root(base.tx_id, base.compact_cbor, steps),
    1_000_000,
  )
  let expected_output_state: Data = step_03.State {
    verified_tx1_spend_inputs_hash: tx1_block.compact.body.spend_inputs_hash,
    verified_tx2_spend_inputs_hash: tx2_block.compact.body.spend_inputs_hash,
  }
  and {
    list.length(fixture.proof_chunk_reference_inputs(steps)) == adversarial_chunk_count,
    main.spend(
      fixture.next_step_script_hash,
      fixture.computation_thread_policy_id,
      fixture.hub_oracle_script_hash,
      Some(
        ct.StepDatum {
          fraud_prover: fixture.fraud_prover_key_hash,
          data: Some(
            State {
              verified_tx1_id: tx1_block.tx_id,
              verified_tx1_spend_inputs_hash: tx1_block.compact.body.spend_inputs_hash,
            },
          ),
        },
      ),
      ct.Continue(
        fixture.published_chunk_inclusion_carriage_v1(
          tx2_block,
          adversarial_chunk_count,
        ),
      ),
      fixture.own_out_ref(),
      fixture.continue_step_chunked_l1_tx_v1(
        tx2_block,
        expected_output_state,
        fixture.proof_chunk_reference_inputs(steps),
        fixture.chunked_membership_claim_v1(tx2_block, adversarial_chunk_count),
      ),
    ),
  }
}

/// Valid-block negative selector: a valid block never holds two distinct
/// transactions that spend the same UTxO, so the only conflict a prover can
/// assemble against one is the same committed transaction named twice. The
/// distinctness check rejects it.
test double_spend_step_02_rejects_the_same_transaction_presented_twice() fail {
  let tx_block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_step_02(tx_block, tx_block.tx_id, tx_block.compact.body.spend_inputs_hash)
}
