//// ## Withdrawn Reference Input Fraud Proof (Section 5.1.16)
////
//// Proves a given transaction refers to a reference input that was spent in a
//// withdrawal event.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/withdrawn_reference_input/step_01.{
  Datum, SpendRedeemer,
}
use midgard/fraud_proofs/withdrawn_reference_input/step_02

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

        // 2. The UTxO must be produced at next step's script hash with the
        //    native reference-inputs hash of the bad transaction, plus the
        //    header's counted withdrawals root and count (needed downstream to
        //    unwrap the counted root for the withdrawal membership check).
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State {
          bad_tx_reference_inputs_hash: bad_tx_view.tx_compact.body.reference_inputs_hash,
          blocks_withdrawals_root: header.withdrawals_root,
          blocks_withdrawal_count: header.withdrawal_count,
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

/// Positive binding selector: a native-V1 transaction referencing an input,
/// genuinely committed under the block's counted `transactions_root`, passes
/// the first step and forwards its canonical reference-inputs hash with the
/// counted withdrawals commitment.
test withdrawn_reference_input_step_01_binds_native_v1_block_fixture() {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let expected_output_state: Data = step_02.State {
    bad_tx_reference_inputs_hash: block.compact.body.reference_inputs_hash,
    blocks_withdrawals_root: block.header.withdrawals_root,
    blocks_withdrawal_count: block.header.withdrawal_count,
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
    ct.Continue(fixture.native_inclusion_args_v1(block)),
    fixture.own_out_ref(),
    l1_tx,
  )
}
