use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, pass_native_tx_to_next_step, verify_native_tx_witness_set,
}
use midgard/fraud_proofs/missing_native_script_tx/step_01.{
  Args, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/missing_native_script_tx/step_02
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionWitnessSet,
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
      ct.Continue(Args { tx_inclusion_args, bad_tx_witness_set_compact }) -> {
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

        // 2. The provided witness-set compact must be the preimage of the bad
        //    transaction's committed `witness_set_hash`.
        let bad_tx_witness_set = verify_native_tx_witness_set(
          bad_tx_view,
          bad_tx_witness_set_compact,
        )

        // 3. The UTxO must be produced at next step's script hash with the
        //    canonical native spend-inputs and script-witness collection
        //    hashes.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State {
          bad_tx_inputs_hash: bad_tx_view.tx_compact.body.spend_inputs_hash,
          bad_tx_script_wits_hash: bad_tx_witness_set.script_tx_wits_hash,
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

/// A transaction spending a script-locked output without carrying any script
/// witness.
fn missing_native_script_bad_tx() -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    witness_set: MidgardTransactionWitnessSet {
      ..base.witness_set,
      script_tx_wits: [],
    },
  }
}

/// Positive binding selector: the bad native-V1 transaction committed under
/// the block's counted `transactions_root` passes the first step with its
/// authenticated witness-set compact.
test missing_native_script_tx_step_01_binds_native_v1_block_fixture() {
  let block = fixture.native_block_fixture_v1(missing_native_script_bad_tx())
  let expected_output_state: Data = step_02.State {
    bad_tx_inputs_hash: block.compact.body.spend_inputs_hash,
    bad_tx_script_wits_hash: block.witness_set_compact.script_tx_wits_hash,
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
    ct.Continue(
      Args {
        tx_inclusion_args: fixture.native_inclusion_args_v1(block),
        bad_tx_witness_set_compact: block.witness_set_compact,
      },
    ),
    fixture.own_out_ref(),
    l1_tx,
  )
}
