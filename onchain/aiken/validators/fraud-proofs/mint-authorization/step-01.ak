//// ## Mint-Authorization Fault Proof — Step 01 (W-C14)
////
//// Binds the disputed transaction through the block's counted
//// `transactions_root` and forwards the §2.5 anchors the later steps need:
//// the transaction id, the `witness_set_hash`, and the body's validity
//// interval. Only this step can supply the last two — §3's id preimage is
//// the body compact alone, so any later restatement would be the prover's
//// assertion rather than a reading of the block-committed structure.
////
//// **Acceptance binding (§2.4.3(d)).** The thread convicts only
//// operator-ACCEPTED transactions: the committed leaf's embedded validity
//// scalar must be 0. The read is sound here and only here — the compact
//// bytes this step sees are the `transactions_root` leaf's exact bytes,
//// re-checked by the counted-root membership proof, so `validity_code` is
//// committed by the root rather than by the id anchor (the id preimage does
//// not cover it; see `field_opening_v1.unanchored_validity_code_of`'s
//// warning). An invalid transaction honestly recorded as a no-op dies here
//// and can never convict.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step_carried}
use midgard/fraud_proofs/mint_authorization/step_01.{Args, Datum, SpendRedeemer}
use midgard/fraud_proofs/mint_authorization/step_02
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/types.{MidgardTransaction, TxIsInvalid}

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
      ct.Continue(Args { carriage }) -> {
        // 1. Generic validations for passing the disputed tx to the next
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
            bad_tx_view
          <- pass_native_tx_to_next_step_carried(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          carriage,
          own_out_ref,
          tx,
        )

        // 2. §2.4.3(d): only an operator-ACCEPTED transaction is in this
        //    family's domain. The scalar is the root-committed leaf's own
        //    (see the module header for why this read is sound at this step
        //    and at no later one).
        expect bad_tx_view.tx_compact.validity_code == 0

        // 3. The UTxO must be produced at next step's script hash carrying
        //    the §2.5 anchors and the committed validity interval.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State {
          bad_tx_id,
          bad_tx_witness_set_hash: bad_tx_view.tx_compact.witness_set_hash,
          validity_interval_start: bad_tx_view.tx_compact.body.validity_interval_start,
          validity_interval_end: bad_tx_view.tx_compact.body.validity_interval_end,
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

fn run_step_01(tx: MidgardTransaction) -> Bool {
  let block = fixture.native_block_fixture_v1(tx)
  let expected_output_state: Data = step_02.State {
    bad_tx_id: block.tx_id,
    bad_tx_witness_set_hash: block.compact.witness_set_hash,
    validity_interval_start: block.compact.body.validity_interval_start,
    validity_interval_end: block.compact.body.validity_interval_end,
  }
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
    ct.Continue(Args { carriage: fixture.native_inclusion_carriage_v1(block) }),
    fixture.own_out_ref(),
    l1_tx,
  )
}

/// Positive binding selector: an operator-accepted native-V1 transaction
/// committed under the block's counted `transactions_root` passes the first
/// step and forwards its verified anchors.
test mint_authorization_step_01_binds_an_accepted_transaction() {
  run_step_01(fixture.valid_native_tx_v1())
}

/// §2.4.3(d) guard: an invalid transaction honestly recorded as a no-op
/// (validity scalar 1) never enters the thread — an operator that correctly
/// recorded a rejection cannot be convicted of authorizing its mint.
test mint_authorization_step_01_rejects_an_honestly_rejected_tx() fail {
  run_step_01(
    MidgardTransaction { ..fixture.valid_native_tx_v1(), validity: TxIsInvalid },
  )
}

// ## Cancellation

fn run_cancel(signed_by_prover: Bool) -> Bool {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let asset_name = fixture.ct_token_asset_name(block)
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Cancel { input_index: 0, computation_thread_mint_redeemer_index: 0 },
    fixture.own_out_ref(),
    fixture.cancel_step_l1_tx_v1(asset_name, asset_name, signed_by_prover),
  )
}

/// The prover may abandon the thread.
test mint_authorization_step_01_cancels_under_the_prover_signature() {
  run_cancel(True)
}

/// Nobody else may.
test mint_authorization_step_01_rejects_an_unsigned_cancellation() fail {
  run_cancel(False)
}
