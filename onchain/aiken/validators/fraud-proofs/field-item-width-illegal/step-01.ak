use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{
  cancel, continue, pass_native_tx_to_next_step_carried,
}
use midgard/fraud_proofs/field_item_width_illegal/rule.{
  BoundCoordinateV1, bind_coordinate_v1,
}
use midgard/fraud_proofs/field_item_width_illegal/step_01.{
  AcceptedSource, Args, Datum, ForcedSource, SpendRedeemer,
}
use midgard/fraud_proofs/native_tx/compact.{decode_forced_tx_compact_v1}
use midgard/fraud_proofs/native_tx/types.{TxCompactView}
use midgard/fraud_proofs/proof_thread_substrate_v1 as substrate

fn output_state(bound: BoundCoordinateV1) -> Data {
  let value: Data = bound
  value
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
      ct.Continue(Args {
        source: AcceptedSource { inclusion },
        field_index,
        item_index,
      }) -> {
        let
            _own_script_hash,
            subject_thread_name,
            _fraud_prover,
            _input_state,
            next_script_hash,
            next_state,
            subject_header,
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
        let subject = substrate.open_subject_v1(
          subject_thread_name,
          substrate.AcceptedSource {
            header: subject_header,
            tx_inclusion: verified,
          },
        )
        let bound = bind_coordinate_v1(
          subject,
          field_index,
          item_index,
          verified.tx_compact,
        )
        and {
          next_script_hash == step_02_validator_script_hash,
          next_state == output_state(bound),
        }
      }
      ct.Continue(Args {
        source: ForcedSource { input_index, output_index, header, membership },
        field_index,
        item_index,
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
        let subject = substrate.open_subject_v1(
          thread_asset_name,
          substrate.ForcedSource { header, membership },
        )
        // open_subject_v1 authenticated these exact compact bytes in the
        // forced leaf; this decode only projects its already-bound hash.
        let compact = decode_forced_tx_compact_v1(
          membership.value.submitted_source.compact_cbor,
        )
        let bound = bind_coordinate_v1(
          subject,
          field_index,
          item_index,
          TxCompactView {
            body: compact.body,
            witness_set_hash: compact.witness_set_hash,
            validity_code: None,
          },
        )
        and {
          next_script_hash == step_02_validator_script_hash,
          next_state == output_state(bound),
        }
      }
    }
  }

  else(_) {
    fail
  }
}
