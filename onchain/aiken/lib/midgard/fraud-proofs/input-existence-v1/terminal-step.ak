use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, finalize}
use midgard/fraud_proofs/input_existence_v1/terminal.{verdict_contradiction_v1}
use midgard/fraud_proofs/input_existence_v1/types.{
  LedgerStateV1, TerminalArgsV1,
}

pub fn terminal_step_v1(
  fraud_policy: PolicyId,
  fraud_address: Address,
  policy: PolicyId,
  datum: Option<ct.StepDatum<LedgerStateV1>>,
  redeemer: ct.StepRedeemer<TerminalArgsV1>,
  own_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  when redeemer is {
    ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
      cancel(
        policy,
        datum,
        input_index,
        own_ref,
        computation_thread_mint_redeemer_index,
        tx,
      )
    ct.Continue(TerminalArgsV1 {
      input_index,
      output_index,
      fraud_proof_mint_redeemer_index,
      ledger_evidence,
    }) -> {
      expect Some(step_datum) = datum
      let _, _, _, input_state <- finalize(
        policy,
        fraud_policy,
        fraud_address,
        step_datum,
        input_index,
        output_index,
        fraud_proof_mint_redeemer_index,
        own_ref,
        tx.inputs,
        tx.outputs,
        tx.redeemers,
      )
      expect Some(state) = input_state
      verdict_contradiction_v1(
        state,
        ledger_evidence,
        tx.reference_inputs,
        tx.redeemers,
      )
    }
  }
}
