use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, continue, pass_native_tx_to_next_step_carried,
}
use midgard/fraud_proofs/direct_script_derivation/types.{SourceBoundV1}
use midgard/fraud_proofs/execution_native_script_invalid/step_01.{
  AcceptedSource, Args, Datum, ForcedSource, SpendRedeemer,
}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate
use midgard/fraud_proofs/source_carriage_v1

validator main(
  accepted_reconstruction_init_script_hash: ScriptHash,
  forced_step_02_validator_script_hash: ScriptHash,
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
      ct.Continue(Args {
        source: AcceptedSource { inclusion },
        execution_index: _,
      }) -> {
        let
            _,
            subject_thread_name,
            _,
            _,
            next_script_hash,
            next_state,
            header,
            _,
            verified
          <- pass_native_tx_to_next_step_carried(
          computation_thread_token_policy_id,
          hub_oracle,
          datum,
          inclusion,
          own_out_ref,
          tx,
        )
        // The inclusion helper authenticated these exact source bytes. Carry
        // its compact bytes forward instead of rebuilding the same envelope;
        // keeping the encoder out of this dispatcher preserves publication fit.
        let expected: Data = SourceBoundV1 {
          subject: substrate.open_subject_v1(
            subject_thread_name,
            substrate.AcceptedSource { header, tx_inclusion: verified },
          ),
          header,
          compact_cbor: source_carriage_v1.carriage_compact_cbor_v1(inclusion),
        }
        and {
          next_script_hash == accepted_reconstruction_init_script_hash,
          next_state == expected,
        }
      }
      ct.Continue(Args {
        source: ForcedSource { input_index, output_index, header, membership },
        execution_index: _,
      }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        let _, thread_asset_name, _, _, next_script_hash, next_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
        )
        let subject = substrate.open_subject_v1(
          thread_asset_name,
          substrate.ForcedSource { header, membership },
        )
        // bind_forced_subject_to_thread_v1 already verified this exact source.
        let expected: Data = SourceBoundV1 {
          subject,
          header,
          compact_cbor: membership.value.submitted_source.compact_cbor,
        }
        and {
          next_script_hash == forced_step_02_validator_script_hash,
          next_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
