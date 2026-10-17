use aiken/builtin
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/transition_trace/final_yield
use midgard/fraud_proofs/transition_trace/proof as rules
use midgard/fraud_proofs/transition_trace/proof.{
  AcceptedTransactionTransitionMismatch, CountFault, DuplicateTraceEvent,
  EventToStepMismatch, InvalidForcedTransactionNoOpTransition,
  InvalidOneStepTransition, InvalidWithdrawalNoOpTransition,
  L2TransactionTransition, OmittedDueL1Event, OutOfWindowSourceEvent,
  SourceMembershipMismatch, TraceBoundaryFault, TraceLinkFault,
  TransitionFaultProof, ValidDepositTransition, ValidWithdrawalTransition,
}
use midgard/fraud_proofs/transition_trace/proof_carriage
use midgard/ledger_state

pub type Datum =
  ct.StepDatum<TransitionFaultProof>

pub type Args {
  input_index: Int,
  output_index: Int,
  proof: Option<TransitionFaultProof>,
  proof_ref_indices: List<Int>,
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
      ct.Continue(Args { input_index, output_index, proof, proof_ref_indices }) -> {
        let Transaction { inputs, outputs, .. } = tx
        expect Some(step_datum) = datum
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
            computation_thread_token_asset_name,
            _fraud_prover,
            input_state,
            output_script_hash,
            output_state,
          ) {
            expect None = input_state
            when proof is {
              Some(inline_proof) -> {
                expect proof_ref_indices == []
                let index = route_index(inline_proof)
                expect index != 4 && index != 5
                let expected_output_state: Data = inline_proof
                and {
                  output_script_hash == list.expect_at(
                    final_validator_script_hashes,
                    index,
                  ),
                  output_state == expected_output_state,
                }
              }
              None -> {
                let (commitment, data) = proof_carriage.open(
                  proof_ref_indices,
                  tx.reference_inputs,
                )
                expect builtin.unconstr_index(data) == 0
                expect [hash_data,
                      header_data, fault] = builtin.unconstr_fields(data)
                expect hash: ByteArray = hash_data
                expect header: ledger_state.HeaderV1 = header_data
                expect [witness] = builtin.unconstr_fields(fault)
                expect
                  rules.validate_transition_header_envelope(
                    hash,
                    header,
                    computation_thread_token_asset_name,
                  )
                let fault_tag = builtin.unconstr_index(fault)
                if fault_tag == 5 || fault_tag == 7 {
                  let event_kind = builtin.unconstr_index(witness)
                  expect event_kind >= 0 && event_kind <= 2
                  // Existing proof chunks transport the exact inline proof once.
                  // Event history and retention authority are checked by final-6.
                  and {
                    output_script_hash == list.expect_at(
                      final_validator_script_hashes,
                      6,
                    ),
                    output_state == data,
                  }
                } else {
                  let kind = if builtin.unconstr_index(fault) == 9 {
                    2
                  } else {
                    expect builtin.unconstr_index(fault) == 4
                    when builtin.unconstr_index(witness) is {
                      3 -> 1
                      4 -> 0
                      _ -> fail
                    }
                  }
                  let index = if kind == 1 {
                    5
                  } else {
                    4
                  }
                  // Each authenticated phase checks the exact narrow witness
                  // fields it consumes; routing never materializes MPF lists.
                  let expected_output_state: Data = final_yield.initial(
                    kind,
                    commitment,
                  )
                  and {
                    output_script_hash == list.expect_at(
                      final_validator_script_hashes,
                      index,
                    ),
                    output_state == expected_output_state,
                  }
                }
              }
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
