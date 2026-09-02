use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue, verify_membership_carried}
use midgard/fraud_proofs/resolved_output_non_canonical/step_03.{
  Args, Datum, ReconstructionV1, SpendRedeemer,
}
use midgard/fraud_proofs/transition_trace/proof.{ledger_outref_key}
use midgard/ledger_output_commitment_v1
use midgard/ledger_output_scan_v1

validator main(step_04_hash: ScriptHash, thread_policy: PolicyId) {
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
      ct.Continue(Args {
        input_index,
        output_index,
        descriptor_cbor,
        membership,
      }) -> {
        let Transaction { inputs, outputs, reference_inputs, redeemers, .. } = tx
        expect Some(step_datum) = datum
        let _, _, _, state, next_hash, next_state <- continue(
          thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          inputs,
          outputs,
        )
        expect Some(authenticated) = state
        expect
          verify_membership_carried(
            membership,
            authenticated.prior_root,
            ledger_outref_key(authenticated.out_ref),
            descriptor_cbor,
            reference_inputs,
            redeemers,
          )
        let descriptor = ledger_output_commitment_v1.decode(descriptor_cbor)
        expect descriptor.output_index == authenticated.out_ref.output_index
        expect descriptor.total_length > 0
        let output = ReconstructionV1 {
          subject: authenticated.subject,
          descriptor_cbor,
          control: ledger_output_scan_v1.initial_control_v1(),
        }
        let expected: Data = output
        and {
          next_hash == step_04_hash,
          next_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
