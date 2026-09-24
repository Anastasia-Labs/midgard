use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue, finalize}
use midgard/fraud_proofs/transition_trace/final_yield.{Datum, SpendRedeemer}
use midgard/fraud_proofs/transition_trace/final_yield as delegation
use midgard/fraud_proofs/transition_trace/proof
use midgard/fraud_proofs/transition_trace/proof_carriage
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/state_queue
use midgard/state_queue_yield
use midgard/transition_trace.{IndexedTraceProof, RootMembershipProof}

validator main(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
  reference_script_auth_policy_id: PolicyId,
  hub_oracle: PolicyId,
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
      ct.Continue(args) -> {
        expect Some(step_datum) = datum
        expect Some(state) = step_datum.data
        expect state.kind == 1
        expect
          if state.phase == 6 {
            expect args.yield_ref_input_indices == []
            delegation.advance(
              step_datum,
              args,
              tx.outputs,
              delegation.State { ..state, phase: 0 },
            )
          } else {
            expect [index] = args.yield_ref_input_indices
            let role = when state.phase is {
              0 -> delegation.deposit_projection_role
              10 -> delegation.deposit_projection_role
              2 -> delegation.deposit_scan_role
              7 -> delegation.deposit_value_role
              8 -> delegation.deposit_summaries_role
              3 -> delegation.deposit_assembly_role
              9 -> delegation.deposit_replay_role
              4 -> delegation.deposit_replay_role
              5 -> delegation.deposit_replay_role
              _ -> fail
            }
            expect _hash = state_queue_yield.require_authenticated_zero_yield(
              tx.reference_inputs,
              tx.withdrawals,
              tx.redeemers,
              reference_script_auth_policy_id,
              role,
              index,
            )
            True
          }
        if state.phase == 5 {
          let _, proof_asset, _, _ <- finalize(
            computation_thread_token_policy_id,
            fraud_proof_token_policy_id,
            fraud_proof_token_address,
            step_datum,
            args.input_index,
            args.output_index,
            args.fraud_proof_mint_redeemer_index,
            own_out_ref,
            tx.inputs,
            tx.outputs,
            tx.redeemers,
          )
          expect Some(raw_witness) = args.completed_fraud_witness
          expect witness: state_queue.CompletedFraudWitness = raw_witness
          let hub_datum = hub.get_datum(
            tx.reference_inputs,
            hub_oracle,
            args.hub_ref_input_index,
          )
          state_queue.require_completed_fraud_witness(
            tx,
            hub_datum.state_queue,
            proof_asset,
            witness,
          )
        } else {
          expect
            if state.phase == 6 {
              let data = proof_carriage.read(
                state.proof_commitment,
                args.proof_ref_indices,
                tx.reference_inputs,
              )
              let (header_data, fault) = proof_carriage.fields(data)
              expect header: ledger_state.HeaderV1 = header_data
              expect [trace_data,
                    event_data,
                    source_data, _] = delegation.one_step_witness(fault, 3)
              expect trace_proof: IndexedTraceProof = trace_data
              expect
                  event: RootMembershipProof<
                    ledger_state.EventKey,
                    ledger_state.EventToStepValue,
                  >
                = event_data
              expect
                  source: RootMembershipProof<
                    ledger_state.DepositId,
                    ledger_state.DepositInfo,
                  >
                = source_data
              proof.validate_deposit_one_step_binding(
                header,
                trace_proof,
                event,
                source,
              )
            } else {
              True
            }
          continue(
            computation_thread_token_policy_id,
            step_datum,
            args.input_index,
            args.output_index,
            own_out_ref,
            tx.inputs,
            tx.outputs,
            fn(input_hash, _, _, _, output_hash, _) {
              input_hash == output_hash
            },
          )
        }
      }
    }
  }

  else(_) {
    fail
  }
}
