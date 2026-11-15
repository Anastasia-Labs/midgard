use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  WitnessAnchor, opened_certified_field_item, opened_certified_field_item_count,
}
use midgard/fraud_proofs/redeemer_canonicity/rule.{
  ContinueData, FinishData, TerminalStateV1, claimed_coordinate_absent_v1,
  open_item_v1, redeemer_field_index,
}
use midgard/fraud_proofs/redeemer_canonicity/step_02.{
  Args, Datum, SpendRedeemer,
}

validator main(
  step_02_data_hash: ScriptHash,
  step_03_hash: ScriptHash,
  computation_thread_policy: PolicyId,
  certificate_policy: PolicyId,
  grammar_certificate_policy: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args {
        input_index,
        output_index,
        opening,
        coordinate_absent,
        grammar_ref_input_index,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, .. } = tx
        expect Some(step_datum) = datum
        let _, _, _, input_state, next_hash, next_state <- continue(
          computation_thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
          tx.extra_signatories,
        )
        expect Some(bound) = input_state
        let anchor = WitnessAnchor {
          tx_id: bound.subject.transaction_id,
          witness_set_hash: bound.witness_set_hash,
          source_kind: bound.subject.source_kind,
        }
        if coordinate_absent {
          // Canonical validation never reads a redeemer field 8 lacks, and
          // a transaction with no redeemers runs no Plutus script, so a
          // forced rejection citing either is answered by the canonical
          // verdict, which convicts only a wrongful rejection at the
          // terminal. The count is the authenticated one: the walk refuses
          // the provisional count of a certified variable-width view, and
          // the certified count is the field grammar certificate's.
          expect
            claimed_coordinate_absent_v1(
              bound,
              opened_certified_field_item_count(
                opening,
                anchor,
                redeemer_field_index,
                grammar_ref_input_index,
                reference_inputs,
                certificate_policy,
                grammar_certificate_policy,
              ),
            )
          let value: Data = TerminalStateV1 { bound, canonical: True }
          and {
            next_hash == step_03_hash,
            next_state == value,
          }
        } else {
          let item = opened_certified_field_item(
            opening,
            anchor,
            redeemer_field_index,
            bound.redeemer_index,
            grammar_ref_input_index,
            reference_inputs,
            certificate_policy,
            grammar_certificate_policy,
          )
          when open_item_v1(bound, item) is {
            ContinueData { scan } -> {
              let value: Data = scan
              and {
                next_hash == step_02_data_hash,
                next_state == value,
              }
            }
            FinishData { terminal } -> {
              let value: Data = terminal
              and {
                next_hash == step_03_hash,
                next_state == value,
              }
            }
          }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
