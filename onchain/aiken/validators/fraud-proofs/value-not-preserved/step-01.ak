//// ## Value-Not-Preserved Fraud Proof — step-01
////
//// Binds the challenged transaction out of the challenged block's counted
//// `transactions_root` through the one blessed binding path
//// (`common.pass_native_tx_to_next_step`), applies the **acceptance gate**,
//// checks the claim's domain, and freezes everything downstream steps consume
//// into thread state. See `midgard/fraud_proofs/value_not_preserved/step_01`
//// for the family doctrine and the D-C1 single-asset-claim resolution.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/value_not_preserved/step_01.{
  Args, ClaimedAssetInflated, Datum, SpendRedeemer, TokenAsset,
  claimed_asset_is_well_formed_v1,
}
use midgard/fraud_proofs/value_not_preserved/step_02
use midgard/fraud_proofs/value_not_preserved/thread_fixture_v1 as tf

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
      ct.Continue(Args { tx_inclusion, claimed_asset, claimed_direction }) -> {
        // 1. Generic validations for passing the provided challenged tx to
        //    the next step must pass — the one blessed binding path.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            _m_input_state_data,
            output_script_hash,
            output_state_data,
            header,
            bad_tx_id,
            bad_tx_view
          <- pass_native_tx_to_next_step(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          tx_inclusion,
          own_out_ref,
          tx,
        )

        // 2. The acceptance gate: only an operator-ACCEPTED transaction is in
        //    this family's domain. An invalid transaction honestly recorded
        //    as a no-op is correct block production — the machine applies no
        //    value transition for it — and must never reach the fold.
        //
        //    This `validity_code` read is committed by the **root**, not by
        //    the prover: `pass_native_tx_to_next_step` proved
        //    `tx_inclusion.l2_transaction_source_cbor` to be the exact source
        //    leaf value the block's counted `transactions_root` commits at
        //    `bad_tx_id`, verified the proof-source triple, and returned its
        //    compact bytes. `bad_tx_view` is re-derived from those authenticated
        //    compact bytes — the same
        //    argument the transition-trace family's
        //    `unanchored_validity_code_of(anchored) == 0` call site owes and
        //    documents (`midgard/fraud_proofs/field_opening_v1`).
        expect bad_tx_view.tx_compact.validity_code == 0

        // 3. The claim must be inside the domain committed leaves and mint
        //    entries are defined over.
        expect claimed_asset_is_well_formed_v1(claimed_asset)

        // 4. The produced UTxO must go to step-02 with the fold's initial
        //    state: the §3 anchor, the frozen claim, the root-committed fee,
        //    and the header's pre-state ledger root that every spent-input
        //    value witness will authenticate against.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State {
          bad_tx_id,
          claimed_asset,
          claimed_direction,
          committed_fee: bad_tx_view.tx_compact.body.fee,
          prev_utxos_root: header.prev_utxos_root,
          input_cursor: 0,
          claimed_delta: 0,
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

fn run_step_01(
  block: tf.ValueBlockV1,
  claimed_asset: step_01.ClaimedAssetV1,
) -> Bool {
  let expected_output_state: Data = step_02.State {
    bad_tx_id: block.block.tx_id,
    claimed_asset,
    claimed_direction: ClaimedAssetInflated,
    committed_fee: block.block.compact.body.fee,
    prev_utxos_root: block.block.header.prev_utxos_root,
    input_cursor: 0,
    claimed_delta: 0,
  }
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(
      Args {
        tx_inclusion: fixture.native_inclusion_args_v1(block.block),
        claimed_asset,
        claimed_direction: ClaimedAssetInflated,
      },
    ),
    fixture.own_out_ref(),
    tf.continue_step_01_l1_tx_v1(block, expected_output_state),
  )
}

/// Positive selector: an accepted transaction committed under the block's
/// counted `transactions_root` passes the acceptance gate and freezes the
/// claim, the committed fee and the header's `prev_utxos_root`.
test value_not_preserved_step_01_binds_and_freezes_the_claim() {
  run_step_01(
    tf.value_block_v1(
      tf.token_inflated_tx_v1(),
      tf.spent_output_with_tokens_v1(),
    ),
    tf.claimed_token_v1(),
  )
}

/// The critical soundness selector: the same unbalanced transaction honestly
/// recorded as a **no-op** (`TxIsInvalid`) is correct block production and
/// must refuse at the acceptance gate — never reach the fold, never convict.
test value_not_preserved_step_01_rejects_a_rejected_transaction() fail {
  run_step_01(
    tf.value_block_v1(
      tf.rejected_token_inflated_tx_v1(),
      tf.spent_output_with_tokens_v1(),
    ),
    tf.claimed_token_v1(),
  )
}

/// A claim outside the committed-leaf domain (27-byte policy id) can never
/// match a leaf or a mint entry and is refused at the door.
test value_not_preserved_step_01_rejects_a_malformed_claim() fail {
  run_step_01(
    tf.value_block_v1(
      tf.token_inflated_tx_v1(),
      tf.spent_output_with_tokens_v1(),
    ),
    TokenAsset {
      policy_id: #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
      asset_name: tf.claimed_asset_name,
    },
  )
}
