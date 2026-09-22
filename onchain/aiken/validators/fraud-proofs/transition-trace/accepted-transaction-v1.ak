use aiken/builtin
use cardano/address.{Address}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue, finalize}
use midgard/fraud_proofs/transition_trace/final_yield.{
  Args, Datum, SpendRedeemer, State,
}
use midgard/fraud_proofs/transition_trace/final_yield as delegation
use midgard/fraud_proofs/transition_trace/proof
use midgard/fraud_proofs/transition_trace/proof_carriage
use midgard/ledger_state
use midgard/state_queue_yield
use midgard/transition_trace.{IndexedTraceProof, RootMembershipProof}
use midgard/validation_trace_v1

validator main(
  computation_thread_token_policy_id: PolicyId,
  fraud_proof_token_policy_id: PolicyId,
  fraud_proof_token_address: Address,
  reference_script_auth_policy_id: PolicyId,
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
        expect state.kind == 0 || state.kind == 2
        let terminal = if state.kind == 2 {
          state.phase == 3
        } else {
          state.phase == 5
        }
        if terminal {
          let _, _, _, _ <- finalize(
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
          if state.kind == 2 {
            expect args.yield_ref_input_indices == []
            let data = proof_carriage.read(
              state.proof_commitment,
              args.proof_ref_indices,
              tx.reference_inputs,
            )
            let (_, fault) = proof_carriage.fields(data)
            expect [witness_data] = builtin.unconstr_fields(fault)
            expect witness: proof.AcceptedTransactionTransitionMismatchWitness = witness_data
            let claim = witness.claim
            let terminal = claim.terminal_state
            and {
              claim.descriptor_membership.value.verdict == validation_trace_v1.Accepted,
              terminal.work_root == validation_trace_v1.hash_work_witness(
                validation_trace_v1.Terminal,
                terminal.program_counter,
                witness.terminal_acceptance_witness_cbor,
              ),
              proof.terminal_acceptance_post_root(
                witness.terminal_acceptance_witness_cbor,
              ) != claim.transition_step_membership.value.post_utxos_root,
            }
          } else {
            expect [index] = args.yield_ref_input_indices
            expect _hash = state_queue_yield.require_authenticated_zero_yield(
              tx.reference_inputs,
              tx.withdrawals,
              tx.redeemers,
              reference_script_auth_policy_id,
              delegation.l2_replay_role,
              index,
            )
            True
          }
        } else {
          expect
            if state.kind == 0 && state.phase == 6 {
              expect args.yield_ref_input_indices == []
              delegation.advance(
                step_datum,
                args,
                tx.outputs,
                State { ..state, phase: 0 },
              )
            } else {
              expect [index] = args.yield_ref_input_indices
              let role = if state.kind == 2 {
                when state.phase is {
                  0 -> delegation.claim_structure_role
                  1 -> delegation.claim_source_role
                  2 -> delegation.claim_endpoints_role
                  _ -> fail
                }
              } else {
                when state.phase is {
                  0 -> delegation.l2_open_role
                  1 -> delegation.l2_replay_role
                  2 -> delegation.l2_scan_role
                  7 -> delegation.l2_value_role
                  8 -> delegation.l2_summaries_role
                  3 -> delegation.l2_assembly_role
                  9 -> delegation.l2_replay_role
                  4 -> delegation.l2_replay_role
                  _ -> fail
                }
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
          expect
            if state.kind == 0 && state.phase == 6 {
              let data = proof_carriage.read(
                state.proof_commitment,
                args.proof_ref_indices,
                tx.reference_inputs,
              )
              let (header_data, fault) = proof_carriage.fields(data)
              expect header: ledger_state.HeaderV1 = header_data
              expect [trace_data,
                    event_data,
                    source_data,
                    _, _, _, _] = delegation.one_step_witness(fault, 4)
              expect trace_proof: IndexedTraceProof = trace_data
              expect
                  event: RootMembershipProof<
                    ledger_state.EventKey,
                    ledger_state.EventToStepValue,
                  >
                = event_data
              expect source: RootMembershipProof<ByteArray, ByteArray> = source_data
              proof.validate_l2_one_step_binding(
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
