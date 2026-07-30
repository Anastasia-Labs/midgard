use aiken/collection/list
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/bounded_collection_v1
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/input_no_idx/step_04.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/native_binding_fixture_v1 as fixture
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_output}

validator main(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
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
      ct.Continue(Args {
        input_index,
        output_index,
        fraud_proof_mint_redeemer_index,
        outputs_preimage,
      }) -> {
        let Transaction { inputs, outputs, redeemers, .. } = tx

        expect Some(step_datum) = datum

        // 1. Specified input must be authentic and properly reproduced at the
        //    fraud proof's spending address.
        let
            _own_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            m_input_state_data
          <- finalize(
          computation_thread_token_policy_id,
          fraud_proof_token_policy_id,
          fraud_proof_token_address,
          step_datum,
          input_index,
          output_index,
          fraud_proof_mint_redeemer_index,
          own_out_ref,
          inputs,
          outputs,
          redeemers,
        )

        expect Some(State { producing_tx_outputs_hash, bad_input_output_index })
          = m_input_state_data

        // 2. Provided outputs preimage must match the producing transaction's
        //    native outputs hash. Each item is the node's canonical output
        //    CBOR under the consensus-ordered outputs field (2).
        expect
          bounded_collection_v1.from_items(
            2,
            list.map(outputs_preimage, encode_midgard_tx_output),
          ) == producing_tx_outputs_hash

        // 3. With preimage of outputs verified, it must be shown its length is
        //    smaller than the output index passed down from previous step.
        expect bad_input_output_index >= list.length(outputs_preimage)

        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

fn run_finalize(bad_input_output_index: Int) -> Bool {
  let producing_block = fixture.native_block_fixture_v1(
    fixture.valid_native_tx_v1(),
  )
  let state = State {
    producing_tx_outputs_hash: producing_block.compact.body.outputs_hash,
    bad_input_output_index,
  }
  let asset_name = fixture.ct_token_asset_name(producing_block)
  main.spend(
    fixture.computation_thread_policy_id,
    fixture.fraud_proof_policy_id,
    fixture.fraud_proof_token_address(),
    Some(
      ct.StepDatum {
        fraud_prover: fixture.fraud_prover_key_hash,
        data: Some(state),
      },
    ),
    ct.Continue(
      Args {
        input_index: 0,
        output_index: 0,
        fraud_proof_mint_redeemer_index: 0,
        outputs_preimage: producing_block.tx.body.outputs,
      },
    ),
    fixture.own_out_ref(),
    fixture.finalize_step_l1_tx_v1(asset_name),
  )
}

/// The canonical native outputs preimage of the producing transaction proves
/// index 7 does not exist (the producing transaction has one output).
test input_no_idx_step_04_accepts_out_of_range_index() {
  run_finalize(7)
}

/// Valid-block negative selector: the referenced output index exists in the
/// producing transaction, so the challenge cannot finalize.
test input_no_idx_step_04_rejects_valid_block_challenge() fail {
  run_finalize(0)
}
