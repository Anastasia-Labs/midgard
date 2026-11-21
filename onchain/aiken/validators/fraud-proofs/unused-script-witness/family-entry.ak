use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/direct_script_derivation/all_purposes_scan as scan
use midgard/fraud_proofs/unused_script_witness/entry.{
  Args, Datum, SpendRedeemer,
}

/// Opens the all-purposes walk at the spend field with the prover's probe. An
/// empty spend field is complete at opening and goes straight to the mint
/// field's grammar door.
validator main(
  spend_scan_hash: ScriptHash,
  mint_grammar_hash: ScriptHash,
  thread_policy: PolicyId,
  certificate_policy: PolicyId,
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
          thread_policy,
          datum,
          input_index,
          own_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(Args { input_index, output_index, probe, opening }) -> {
        expect Some(step_datum) = datum
        let _, _, _, input_state, output_hash, output_state <- continue(
          thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          tx.inputs,
          tx.outputs,
          tx.extra_signatories,
        )
        expect Some(bound) = input_state
        let opened = scan.initial_v1(
          bound,
          Some(probe),
          opening,
          tx.reference_inputs,
          certificate_policy,
        )
        let expected: Data = opened
        and {
          output_hash == if opened.complete {
            mint_grammar_hash
          } else {
            spend_scan_hash
          },
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
