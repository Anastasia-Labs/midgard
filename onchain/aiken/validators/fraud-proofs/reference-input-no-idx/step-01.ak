//// ## Reference Input with No Index Fraud Proof (Section 5.1.1)
////
//// The reference-input mirror of `input-no-idx`. Proves a given transaction
//// *reads* a reference input, such that the preimage of its transaction ID
//// exists within the block (i.e. the UTxO was produced by another transaction
//// in the block), yet its output index is larger than the last output of its
//// producing transaction.
////
//// This first step forwards the bad transaction's verified id exactly as
//// `input-no-idx` does; the two families diverge only at step-02, which opens
//// consensus field 1 (**reference** inputs) through the §8.8 door rather than
//// field 0. Steps 03-04 differ from their `input-no-idx` counterparts only in
//// record field names, which PlutusData erases, so they compile to the same
//// UPLC and the two families share those two applied scripts — as the
//// `no-input`/`no-reference-input` pair already do.

use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, pass_native_tx_to_next_step}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/types.{
  MidgardTransaction, MidgardTransactionBody, MidgardTxInput,
}
use midgard/fraud_proofs/reference_input_no_idx/step_01.{Datum, SpendRedeemer}
use midgard/fraud_proofs/reference_input_no_idx/step_02

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
        //    step must pass. `pass_native_tx_to_next_step` authenticates the raw
        //    native tx against the header's counted `transactions_root`.
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
          <- pass_native_tx_to_next_step(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          tx_inclusion_args,
          own_out_ref,
          tx,
        )

        // 2. The UTxO must be produced at next step's script hash carrying the
        //    verified bad transaction's id, so step-02 can re-open its field-1
        //    (reference-inputs) preimage through the §8.8 door. The id anchors
        //    all body fields under §4's flat scheme; step-02 supplies the
        //    reference-inputs field index at its own call site.
        expect output_script_hash == step_02_validator_script_hash
        let expected_output_state: Data = step_02.State {
          verified_tx_id: bad_tx_id,
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

/// A transaction reading an out-of-range output index of another transaction.
fn reference_input_no_idx_bad_tx(
  producing_tx_id: ByteArray,
) -> MidgardTransaction {
  let base = fixture.valid_native_tx_v1()
  MidgardTransaction {
    ..base,
    body: MidgardTransactionBody {
      ..base.body,
      reference_inputs: [
        MidgardTxInput { tx_id: producing_tx_id, output_index: 7 },
      ],
    },
  }
}

/// Positive binding selector: the bad native-V1 transaction committed under
/// the block's counted `transactions_root` passes the first step and forwards
/// its verified id, which step-02 re-opens field 1 against through the door.
test reference_input_no_idx_step_01_binds_native_v1_block_fixture() {
  let producing = fixture.native_block_fixture_v1(fixture.valid_native_tx_v1())
  let block = fixture.native_block_fixture_v1(
    reference_input_no_idx_bad_tx(producing.tx_id),
  )
  let expected_output_state: Data = step_02.State {
    verified_tx_id: block.tx_id,
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

/// Valid-block negative selector at the binding: a raw transactions root that
/// does not re-commit to the valid header's counted `transactions_root` cannot
/// bind fabricated evidence.
test reference_input_no_idx_step_01_rejects_forged_transactions_root() fail {
  let valid_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let forged_block = fixture.native_block_fixture_v1(
    reference_input_no_idx_bad_tx(valid_block.tx_id),
  )
  let expected_output_state: Data = step_02.State {
    verified_tx_id: forged_block.tx_id,
  }
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
    ct.Continue(fixture.native_inclusion_args_v1(claimed_block)),
    fixture.own_out_ref(),
    l1_tx,
  )
}
