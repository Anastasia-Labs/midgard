use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/transition_trace/proof.{
  AcceptedTransactionTransitionMismatch, CountFault, DuplicateTraceEvent,
  EventToStepMismatch, InvalidForcedTransactionNoOpTransition,
  InvalidOneStepTransition, InvalidWithdrawalNoOpTransition,
  L2TransactionTransition, OmittedDueL1Event, OutOfWindowSourceEvent,
  SourceMembershipMismatch, TraceBoundaryFault, TraceLinkFault,
  TransitionFaultProof, ValidDepositTransition, ValidWithdrawalTransition,
}

pub type Datum =
  ct.StepDatum<TransitionFaultProof>

pub type Args {
  input_index: Int,
  output_index: Int,
  proof: TransitionFaultProof,
}

pub type SpendRedeemer =
  ct.StepRedeemer<Args>

fn route_index(proof: TransitionFaultProof) -> Int {
  when proof.fault is {
    TraceBoundaryFault { .. } -> 0
    TraceLinkFault { .. } -> 0
    EventToStepMismatch { .. } -> 0
    CountFault { .. } -> 0
    SourceMembershipMismatch { .. } -> 1
    InvalidOneStepTransition { witness } ->
      when witness is {
        ValidWithdrawalTransition { .. } -> 2
        InvalidWithdrawalNoOpTransition { .. } -> 2
        InvalidForcedTransactionNoOpTransition { .. } -> 3
        L2TransactionTransition { .. } -> 4
        ValidDepositTransition { .. } -> 5
      }
    AcceptedTransactionTransitionMismatch { .. } -> 4
    OmittedDueL1Event { .. } -> 6
    OutOfWindowSourceEvent { .. } -> 6
    DuplicateTraceEvent { .. } -> 7
  }
}

/// Routes a unilateral transition fault proof to one narrowly compiled final
/// validator. Routing is a challenger-only transaction; it never creates an
/// operator response phase.
validator main(
  final_validator_script_hashes: List<ScriptHash>,
  computation_thread_token_policy_id: PolicyId,
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
      ct.Continue(Args { input_index, output_index, proof }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
        expect list.length(final_validator_script_hashes) == 8
        continue(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          inputs,
          outputs,
          fn(
            _input_script_hash,
            _computation_thread_token_asset_name,
            _fraud_prover,
            input_state,
            output_script_hash,
            output_state,
          ) {
            expect None = input_state
            let expected_script_hash = final_validator_script_hashes
              |> list.expect_at(route_index(proof))
            let expected_output_state: Data = proof
            and {
              output_script_hash == expected_script_hash,
              output_state == expected_output_state,
            }
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
