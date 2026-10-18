use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/missing_redeemer/claim_spend as claim

validator main(
  spend_scan_hash: ScriptHash,
  thread_policy: PolicyId,
  certificate_policy: PolicyId,
) {
  spend(
    datum: Option<claim.OpenDatum>,
    redeemer: claim.OpenRedeemer,
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
      ct.Continue(action) -> {
        let
            claim.OpenActionV1 {
              input_index,
              output_index,
              raw_item_index,
              opening,
              descriptor_cbor,
              membership,
              budget,
            }
          = action
        expect Some(step_datum) = datum
        let _, _, fraud_prover, input_state, output_hash, output_state <- continue(
          thread_policy,
          step_datum,
          input_index,
          output_index,
          own_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(context) = input_state
        // Raw position is a free choice until the completed rank scan.
        expect list.has(tx.extra_signatories, fraud_prover)
        let expected: Data = claim.open_v1(
          context,
          raw_item_index,
          opening,
          descriptor_cbor,
          membership,
          budget,
          tx.reference_inputs,
          tx.redeemers,
          certificate_policy,
        )
        and {
          output_hash == spend_scan_hash,
          output_state == expected,
        }
      }
    }
  }

  else(_) {
    fail
  }
}
