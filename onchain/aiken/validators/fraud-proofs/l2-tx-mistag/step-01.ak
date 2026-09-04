//// ## L2 transaction mis-tag fraud proof — step 01
////
//// Binds a normal native-V1 leaf through the shared counted-root path and
//// forwards the validity scalar that the committed compact actually embeds.

use aiken/collection/list
use aiken/crypto.{ScriptHash}
use aiken/merkle_patricia_forestry as mpf
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step_carried}
use midgard/fraud_proofs/l2_tx_mistag/step_01.{Datum, SpendRedeemer}
use midgard/fraud_proofs/l2_tx_mistag/step_02
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/types as native_types

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
          <- pass_native_tx_to_next_step_carried(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          tx_inclusion_args,
          own_out_ref,
          tx,
        )

        expect None = m_input_state_data

        // A normal `transactions_root` leaf is an acceptance verdict. Its
        // canonical scalar domain is {0, 1}, so non-zero is exactly code 1.
        let committed_validity_code = bad_tx_view.tx_compact.validity_code
        expect committed_validity_code != 0

        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State {
          bad_tx_id,
          committed_validity_code,
        }
        expect output_state_data == expected_output_state

        True
      }
    }
  }

  else(_) {
    fail
  }
}

fn mistagged_tx() -> native_types.MidgardTransaction {
  native_types.MidgardTransaction {
    ..fixture.valid_native_tx_v1(),
    validity: native_types.TxIsInvalid,
  }
}

fn expected_state(block: fixture.NativeBlockFixtureV1) -> Data {
  let state: step_02.State = step_02.State {
    bad_tx_id: block.tx_id,
    committed_validity_code: block.compact.validity_code,
  }
  let state_data: Data = state
  state_data
}

fn run_step_01(block: fixture.NativeBlockFixtureV1, output_state: Data) -> Bool {
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(fixture.native_inclusion_carriage_v1(block)),
    fixture.own_out_ref(),
    fixture.continue_step_l1_tx_v1(
      block,
      output_state,
      block.transactions_phas_root,
      block.tx_id,
      block.source_cbor,
    ),
  )
}

/// A genuinely committed code-1 normal leaf binds and forwards its authenticated
/// transaction id and scalar.
test l2_tx_mistag_step_01_binds_a_committed_nonzero_validity_leaf() {
  let block = fixture.native_block_fixture_v1(mistagged_tx())
  run_step_01(block, expected_state(block))
}

/// Exact-check refusal: an honest code-0 normal leaf cannot advance the thread.
test l2_tx_mistag_step_01_rejects_an_honest_code_zero_leaf() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_step_01(block, expected_state(block))
}

/// Anti-framing refusal: flipping only the scalar preserves the body-derived id
/// but not membership of the compact value under the honest counted root.
test l2_tx_mistag_step_01_rejects_a_flipped_scalar_forgery() fail {
  let honest = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let forged = fixture.native_block_fixture_v1(mistagged_tx())
  let claimed = fixture.NativeBlockFixtureV1 {
    ..forged,
    transactions_phas_root: honest.transactions_phas_root,
    header: honest.header,
    header_hash: honest.header_hash,
  }
  // The production withdrawal validator executes this same `mpf.has` over
  // the redeemer payload that `run_step_01` binds. Execute it explicitly here
  // as well: a unit call to one validator does not automatically run the other
  // scripts in its synthetic transaction.
  expect
    mpf.has(
      mpf.from_root(claimed.transactions_phas_root),
      claimed.tx_id,
      claimed.compact_cbor,
      [],
    )
  run_step_01(claimed, expected_state(claimed))
}

/// A raw transactions root not committed by the header's counted root cannot
/// bind even when the leaf itself is a genuine code-1 compact.
test l2_tx_mistag_step_01_rejects_a_forged_transactions_root() fail {
  let block = fixture.native_block_fixture_v1(mistagged_tx())
  run_step_01(
    fixture.NativeBlockFixtureV1 {
      ..block,
      transactions_phas_root: #"4040404040404040404040404040404040404040404040404040404040404040",
    },
    expected_state(block),
  )
}

/// The forwarded scalar is recomputed from the authenticated compact; a prover
/// cannot replace it with code 0.
test l2_tx_mistag_step_01_rejects_a_lie_about_the_forwarded_code() fail {
  let block = fixture.native_block_fixture_v1(mistagged_tx())
  let lie: Data = step_02.State {
    bad_tx_id: block.tx_id,
    committed_validity_code: 0,
  }
  run_step_01(block, lie)
}

// ## Published-chunk carriage (#545)

const adversarial_branch_levels: Int = 22

const adversarial_chunk_count: Int = 2

fn deep_mistagged_block() -> (fixture.NativeBlockFixtureV1, mpf.Proof) {
  let base = fixture.native_block_fixture_v1(mistagged_tx())
  let steps = fixture.adversarial_branch_steps(adversarial_branch_levels)
  (
    fixture.deep_transactions_root_block_fixture_v1(
      base,
      fixture.adversarial_membership_root(base.tx_id, base.source_cbor, steps),
      1_000_000,
    ),
    steps,
  )
}

fn run_step_01_published(
  block: fixture.NativeBlockFixtureV1,
  carriage_block: fixture.NativeBlockFixtureV1,
  steps: mpf.Proof,
) -> Bool {
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      fixture.published_chunk_inclusion_carriage_v1(
        carriage_block,
        adversarial_chunk_count,
      ),
    ),
    fixture.own_out_ref(),
    fixture.continue_step_chunked_l1_tx_v1(
      block,
      expected_state(block),
      fixture.proof_chunk_reference_inputs(steps),
      fixture.chunked_membership_claim_v1(
        carriage_block,
        adversarial_chunk_count,
      ),
    ),
  )
}

/// A deep genuine code-1 membership proof can be referenced through published
/// chunks without changing this family's adjudication.
test l2_tx_mistag_step_01_binds_a_published_chunk_membership_proof() {
  let (block, steps) = deep_mistagged_block()
  and {
    list.length(steps) == adversarial_branch_levels,
    list.length(fixture.proof_chunk_reference_inputs(steps)) == adversarial_chunk_count,
    run_step_01_published(block, block, steps),
  }
}

/// The published proof remains bound to the header's authenticated raw root.
test l2_tx_mistag_step_01_rejects_published_chunks_under_a_forged_root() fail {
  let (block, steps) = deep_mistagged_block()
  let forged = fixture.NativeBlockFixtureV1 {
    ..block,
    transactions_phas_root: #"4040404040404040404040404040404040404040404040404040404040404040",
  }
  run_step_01_published(block, forged, steps)
}
