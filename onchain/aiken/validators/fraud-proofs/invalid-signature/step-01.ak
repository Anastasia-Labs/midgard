use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step_carried}
use midgard/fraud_proofs/invalid_signature/step_01.{Datum, SpendRedeemer}
use midgard/fraud_proofs/invalid_signature/step_02
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

        expect bad_tx_view.tx_compact.validity_code == 0

        // 2. The UTxO must be produced at next step's script hash with the bad
        //    transaction's §2.5 anchor: its verified native id *and* the
        //    `witness_set_hash` of the compact structure the block's counted
        //    `transactions_root` commits. Step-02 opens field 7 through the
        //    §8.8 door, which re-derives the supplied witness set against that
        //    hash. The hash has to come from here because §3's id preimage is
        //    the body alone — re-deriving the id downstream re-derives nothing
        //    about the witness set, so this step is the last place the value is
        //    authenticated rather than asserted.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State {
          bad_tx_id,
          bad_tx_witness_set_hash: bad_tx_view.tx_compact.witness_set_hash,
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

/// Positive binding selector: a native-V1 transaction carrying an invalid
/// signature, genuinely committed under the block's counted
/// `transactions_root`, passes the first step and forwards its verified id.
test invalid_signature_step_01_binds_native_v1_block_fixture() {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let expected_output_state: Data = step_02.State {
    bad_tx_id: block.tx_id,
    bad_tx_witness_set_hash: block.compact.witness_set_hash,
  }
  let l1_tx = fixture.continue_step_l1_tx_v1(
    block,
    expected_output_state,
    block.transactions_phas_root,
    block.tx_id,
    block.source_cbor,
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

/// Valid-block negative selector: an id the header's counted
/// `transactions_root` does not commit cannot bind, so a valid block cannot be
/// challenged with a transaction it never held.
test invalid_signature_step_01_rejects_an_uncommitted_transaction() fail {
  let block = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let forged_tx_id = #"deadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeefdeadbeef"
  let expected_output_state: Data = step_02.State {
    bad_tx_id: forged_tx_id,
    bad_tx_witness_set_hash: block.compact.witness_set_hash,
  }
  let l1_tx = fixture.continue_step_l1_tx_v1(
    block,
    expected_output_state,
    block.transactions_phas_root,
    forged_tx_id,
    block.source_cbor,
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
