//// ## Zero Input Fraud Proof (Section 5.1.1)
////
//// Proves a given transaction spends no inputs.

use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/merkle_patricia_forestry as mpf
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step_carried}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody,
}
use midgard/fraud_proofs/zero_input/step_01.{Datum, SpendRedeemer}
use midgard/fraud_proofs/zero_input/step_02

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
            _header,
            bad_tx_id,
            _bad_tx_view
          <- pass_native_tx_to_next_step_carried(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          tx_inclusion_args,
          own_out_ref,
          tx,
        )

        // 2. The UTxO must be produced at next step's script hash with the
        //    native spend-inputs hash of the bad transaction. Step-02 concludes
        //    the proof by comparing it against the hash of an empty input list,
        //    so no block roots need to be threaded forward.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State { bad_tx_id }
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

fn zero_input_bad_tx() -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody { ..base.body, inputs: [] },
  }
}

/// Positive binding selector: a native-V1 transaction with no spend inputs,
/// genuinely committed under the block's counted `transactions_root`, passes
/// the first step and forwards its canonical spend-inputs hash.
test zero_input_step_01_binds_native_v1_block_fixture() {
  let block = fixture.native_block_fixture_v1(zero_input_bad_tx())
  let expected_output_state: Data = step_02.State { bad_tx_id: block.tx_id }
  let l1_tx = fixture.continue_step_l1_tx_v1(
    block,
    expected_output_state,
    block.transactions_phas_root,
    block.tx_id,
    block.compact_cbor,
  )
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(fixture.native_inclusion_carriage_v1(block)),
    fixture.own_out_ref(),
    l1_tx,
  )
}

// ## Published-chunk carriage (issue #545)
//
// The depth these selectors use is past the point at which the direct route
// exhausts the 16,384-byte L1 envelope (branch level 23 for this family, at a
// measured 276 signed bytes per level). On the published-chunk route the step
// transaction carries two small integers and two reference inputs instead.

const adversarial_branch_levels: Int = 22

const adversarial_chunk_count: Int = 2

fn deep_zero_input_block() -> (fixture.NativeBlockFixtureV1, mpf.Proof) {
  let base = fixture.native_block_fixture_v1(zero_input_bad_tx())
  let steps = fixture.adversarial_branch_steps(adversarial_branch_levels)
  (
    fixture.deep_transactions_root_block_fixture_v1(
      base,
      fixture.adversarial_membership_root(base.tx_id, base.compact_cbor, steps),
      1_000_000,
    ),
    steps,
  )
}

fn run_zero_input_step_01_published(
  block: fixture.NativeBlockFixtureV1,
  carriage_block: fixture.NativeBlockFixtureV1,
  chunk_count: Int,
  claim_chunk_count: Int,
  steps: mpf.Proof,
) -> Bool {
  let expected_output_state: Data = step_02.State { bad_tx_id: block.tx_id }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      fixture.published_chunk_inclusion_carriage_v1(carriage_block, chunk_count),
    ),
    fixture.own_out_ref(),
    fixture.continue_step_chunked_l1_tx_v1(
      block,
      expected_output_state,
      fixture.proof_chunk_reference_inputs(steps),
      fixture.chunked_membership_claim_v1(carriage_block, claim_chunk_count),
    ),
  )
}

/// Positive selector: a 22-level membership proof — deeper than the direct
/// route can carry — reaches this step through published chunks the transaction
/// merely references, and the step's own semantics are unchanged.
test zero_input_step_01_binds_a_published_chunk_membership_proof() {
  let (block, steps) = deep_zero_input_block()
  and {
    list.length(steps) == adversarial_branch_levels,
    list.length(fixture.proof_chunk_reference_inputs(steps)) == adversarial_chunk_count,
    run_zero_input_step_01_published(
      block,
      block,
      adversarial_chunk_count,
      adversarial_chunk_count,
      steps,
    ),
  }
}

/// The published chunks stay bound to the root the challenged header commits: a
/// carriage naming a different raw root is rejected by the counted-root
/// authentication, exactly as on the direct route.
test zero_input_step_01_rejects_published_chunks_under_a_forged_root() fail {
  let (block, steps) = deep_zero_input_block()
  let forged = fixture.NativeBlockFixtureV1 {
    ..block,
    transactions_phas_root: #"4040404040404040404040404040404040404040404040404040404040404040",
  }
  run_zero_input_step_01_published(
    block,
    forged,
    adversarial_chunk_count,
    adversarial_chunk_count,
    steps,
  )
}

/// The step is bound to the exact claim its redeemer named: a merkelized
/// verifier invoked on a different chunk order proves something this step did
/// not ask for. Whether that order reconstructs the root is the verifier's
/// business, and its own module's tests.
test zero_input_step_01_rejects_a_claim_the_redeemer_did_not_name() fail {
  let (block, steps) = deep_zero_input_block()
  run_zero_input_step_01_published(
    block,
    block,
    adversarial_chunk_count,
    1,
    steps,
  )
}

/// Valid-block negative selector: a raw transactions root that is not the one
/// committed by the header's counted `transactions_root` cannot bind, so
/// fabricated evidence against a valid block is rejected at step one.
test zero_input_step_01_rejects_forged_transactions_root() fail {
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let forged_block = fixture.native_block_fixture_v1(zero_input_bad_tx())
  let expected_output_state: Data = step_02.State {
    bad_tx_id: forged_block.tx_id,
  }
  // The prover claims the forged bad tx under the valid block's header by
  // supplying the forged raw root; the counted-root unwrap must reject it.
  let claimed_block = fixture.NativeBlockFixtureV1 {
    ..forged_block,
    header: valid_block.header,
    header_hash: valid_block.header_hash,
  }
  let l1_tx = fixture.continue_step_l1_tx_v1(
    claimed_block,
    expected_output_state,
    claimed_block.transactions_phas_root,
    claimed_block.tx_id,
    claimed_block.compact_cbor,
  )
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(fixture.native_inclusion_carriage_v1(claimed_block)),
    fixture.own_out_ref(),
    l1_tx,
  )
}
