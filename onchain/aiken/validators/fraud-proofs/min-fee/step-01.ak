use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step}
use midgard/fraud_proofs/min_fee/step_01.{Datum, SpendRedeemer}
use midgard/fraud_proofs/min_fee/step_02
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture

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

        // 2. The UTxO must be produced at next step's script hash with the
        //    verified native compact transaction, its inline body fee, the
        //    verified transaction id, and the disputed header's own fee
        //    schedule.
        //
        //    Every one of these is read off evidence this step just
        //    authenticated — the compact structure and its id from the
        //    counted `transactions_root` leaf, `min_fee_a` / `min_fee_b`
        //    from the state-queue header the thread's asset name binds —
        //    which is what entitles step-02 to treat its state as anchored.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State {
          bad_tx: bad_tx_view.tx_compact,
          bad_tx_body_fee: bad_tx_view.tx_compact.body.fee,
          bad_tx_id,
          min_fee_a: header.min_fee_a,
          min_fee_b: header.min_fee_b,
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

fn expected_state_for(block: fixture.NativeBlockFixtureV1) -> Data {
  let state: Data = step_02.State {
    bad_tx: block.compact,
    bad_tx_body_fee: block.compact.body.fee,
    bad_tx_id: block.tx_id,
    min_fee_a: block.header.min_fee_a,
    min_fee_b: block.header.min_fee_b,
  }
  state
}

fn run_continue(block: fixture.NativeBlockFixtureV1, output_state: Data) -> Bool {
  let l1_tx = fixture.continue_step_l1_tx_v1(
    block,
    output_state,
    block.transactions_phas_root,
    block.tx_id,
    block.compact_cbor,
  )
  main.spend(
    fixture.next_step_script_hash,
    fixture.computation_thread_policy_id,
    fixture.hub_oracle_script_hash,
    Some(fixture.step_datum_without_state()),
    ct.Continue(fixture.native_inclusion_args_v1(block)),
    fixture.own_out_ref(),
    l1_tx,
  )
}

/// Positive binding selector: a native-V1 transaction genuinely committed
/// under the block's counted `transactions_root` passes the first step and
/// forwards its verified compact form, inline fee, transaction id, and the
/// header's fee schedule.
test min_fee_step_01_binds_native_v1_block_fixture() {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  run_continue(block, expected_state_for(block))
}

/// The forwarded schedule is the committed header's, not the redeemer's: a
/// non-zero `min_fee_a` / `min_fee_b` pair travels because the header hash the
/// thread's asset name binds re-derives over those very fields.
test min_fee_step_01_forwards_the_header_fee_schedule() {
  let block = fixture.min_fee_params_block_fixture_v1(
    fixture.native_block_fixture_v1(fixture.valid_native_tx_v1()),
    44,
    155_381,
  )
  run_continue(block, expected_state_for(block))
}

/// Valid-block negative selector: a raw transactions root that does not
/// re-commit to the valid header's counted `transactions_root` cannot bind
/// fabricated fee evidence.
test min_fee_step_01_rejects_forged_transactions_root() fail {
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let claimed_block = fixture.NativeBlockFixtureV1 {
    ..valid_block,
    transactions_phas_root: #"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef",
  }
  run_continue(claimed_block, expected_state_for(claimed_block))
}

/// The state equality is what stops a prover from writing a harsher fee
/// schedule than the header's into thread state: an output claiming
/// `min_fee_b` one lovelace above the committed header's is refused here, not
/// left for step-02 to trust.
test min_fee_step_01_rejects_a_forged_fee_schedule_in_state() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let forged_state: Data = step_02.State {
    bad_tx: block.compact,
    bad_tx_body_fee: block.compact.body.fee,
    bad_tx_id: block.tx_id,
    min_fee_a: block.header.min_fee_a,
    min_fee_b: block.header.min_fee_b + 1,
  }
  run_continue(block, forged_state)
}
