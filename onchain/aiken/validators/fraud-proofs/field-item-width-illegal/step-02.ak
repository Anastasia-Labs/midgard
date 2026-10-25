use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_item_width_illegal/rule.{
  AuthenticatedItemV1, authenticate_item_legality_v1,
  authenticate_static_domain_v1, coordinate_is_supported_v1,
}
use midgard/fraud_proofs/field_item_width_illegal/step_02.{
  Args, Datum, SpendRedeemer,
}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, WitnessAnchor, opened_whole_field_view,
}

fn output_state(authenticated: AuthenticatedItemV1) -> Data {
  let value: Data = authenticated
  value
}

validator main(
  step_03_validator_script_hash: ScriptHash,
  computation_thread_token_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
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
      ct.Continue(Args { input_index, output_index, opening }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let
            _own_script_hash,
            _thread_asset_name,
            _fraud_prover,
            input_state,
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
          tx.extra_signatories,
        )
        expect Some(bound) = input_state
        let authenticated = if coordinate_is_supported_v1(
          bound.field_index,
          bound.item_index,
        ) {
          expect Some(field_opening) = opening
          let anchor = if bound.field_index == 7 {
            WitnessAnchor {
              tx_id: bound.subject.transaction_id,
              source_kind: bound.subject.source_kind,
              witness_set_hash: bound.compact.witness_set_hash,
            }
          } else {
            BodyAnchor {
              tx_id: bound.subject.transaction_id,
              source_kind: bound.subject.source_kind,
            }
          }
          let view = opened_whole_field_view(
            field_opening,
            anchor,
            bound.field_index,
            reference_inputs,
            field_preimage_certificate_policy_id,
          )
          authenticate_item_legality_v1(bound, view)
        } else {
          expect opening == None
          authenticate_static_domain_v1(bound)
        }
        and {
          next_script_hash == step_03_validator_script_hash,
          next_state == output_state(authenticated),
        }
      }
    }
  }

  else(_) {
    fail
  }
}
