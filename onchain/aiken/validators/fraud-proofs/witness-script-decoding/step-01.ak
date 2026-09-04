use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, continue, pass_native_tx_to_next_step_carried,
}
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/witness_script_decoding/rule.{bind_subject_v1}
use midgard/fraud_proofs/witness_script_decoding/step_01.{
  AcceptedSource, Args, Datum, ForcedSource, SpendRedeemer,
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
      ct.Continue(Args { source: AcceptedSource { inclusion }, script_index }) -> {
        let
            _own_script_hash,
            _thread_asset_name,
            _fraud_prover,
            _input_state,
            next_script_hash,
            next_state,
            _header,
            _tx_id,
            verified
          <- pass_native_tx_to_next_step_carried(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          inclusion,
          own_out_ref,
          tx,
        )
        let subject = substrate.bind_accepted_subject_v1(verified)
        let expected: Data = bind_subject_v1(
          subject,
          verified.tx_compact.witness_set_hash,
          script_index,
        )
        and {
          next_script_hash == step_02_validator_script_hash,
          next_state == expected,
        }
      }
      ct.Continue(Args {
        source: ForcedSource {
          input_index,
          output_index,
          header,
          membership,
          direction,
        },
        script_index,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let
            _own_script_hash,
            thread_asset_name,
            _fraud_prover,
            _input_state,
            next_script_hash,
            next_state
          <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        let subject = substrate.bind_forced_subject_to_thread_v1(
          thread_asset_name,
          header,
          membership,
          direction,
        )
        let leaf = membership.value
        let Pair(verified, _witness_set) = verify_native_tx_proof_source_v1(
          leaf.tx_id,
          leaf.source.compact_cbor,
          leaf.source.witness_set_compact_cbor,
          leaf.source.field_preimage_lengths_cbor,
        )
        let expected: Data = bind_subject_v1(
          subject,
          verified.tx_compact.witness_set_hash,
          script_index,
        )
        and {
          next_script_hash == step_02_validator_script_hash,
          next_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
