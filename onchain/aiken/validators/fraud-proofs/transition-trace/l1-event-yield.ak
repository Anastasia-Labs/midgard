use aiken/builtin
use aiken/collection/dict
use aiken/crypto.{ScriptHash}
use cardano/address.{Address, Credential}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/event_history/order_facts
use midgard/event_history/proof_evidence
use midgard/fraud_proofs/transition_trace/final_l1_event as delegation
use midgard/fraud_proofs/transition_trace/proof
use midgard/fraud_proofs/transition_trace/timed_history
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/rejection_reason_v1
use midgard/transition_trace

/// One zero withdrawal validates the exact Continue of one pinned dispatcher.
/// Custody/finalization and the queue marker are enforced by that spending script.
validator timing(
  dispatcher: ScriptHash,
  computation_thread_policy: PolicyId,
  hub_oracle: PolicyId,
  deposit_retention_address: Address,
  withdrawal_retention_address: Address,
  inline_limit: Int,
  max_payload_bytes: Int,
  max_payload_nodes: Int,
) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (input, datum, args) = delegation.dispatch(dispatcher, tx)
    expect Some(raw) = datum.data
    // Decode only the selected timing witness, avoiding every unrelated proof
    // variant in this script. The envelope remains fully authenticated below.
    expect builtin.unconstr_index(raw) == 0
    expect [hash_data, header_data, fault_data] = builtin.unconstr_fields(raw)
    expect header_hash: ByteArray = hash_data
    expect header: ledger_state.HeaderV1 = header_data
    expect [witness_data] = builtin.unconstr_fields(fault_data)
    expect [Pair(asset_name, 1)] = input.output.value
      |> assets.tokens(computation_thread_policy)
      |> dict.to_pairs
    expect
      proof.validate_transition_header_envelope(header_hash, header, asset_name)
    let reference_inputs = tx.reference_inputs
    let hub_datum = hub.get_datum(
      reference_inputs,
      hub_oracle,
      args.hub_ref_input_index,
    )
    let event_reference = args.event_reference
    let deposit_parameters = timed_history.Parameters {
      policy: hub_datum.deposit,
      list_address: hub_datum.deposit_addr,
      data: proof_evidence.Parameters {
        retention_address: deposit_retention_address,
        inline_limit,
        max_payload_bytes,
        max_payload_nodes,
      },
    }
    let withdrawal_parameters = timed_history.Parameters {
      policy: hub_datum.withdrawal,
      list_address: hub_datum.withdrawal_addr,
      data: proof_evidence.Parameters {
        retention_address: withdrawal_retention_address,
        inline_limit,
        max_payload_bytes,
        max_payload_nodes,
      },
    }
    when builtin.unconstr_index(fault_data) is {
      5 -> {
        expect [source_data] = builtin.unconstr_fields(witness_data)
        expect source: transition_trace.RootNonMembershipProof<OutputReference>
          = source_data
        when builtin.unconstr_index(witness_data) is {
          0 ->
            timed_history.omitted(
              tx,
              deposit_parameters,
              event_reference,
              order_facts.Deposit,
              header,
              source,
            )
          1 ->
            timed_history.omitted(
              tx,
              withdrawal_parameters,
              event_reference,
              order_facts.Withdrawal,
              header,
              source,
            )
          _ -> False
        }
      }
      7 ->
        when builtin.unconstr_index(witness_data) is {
          0 -> {
            expect [source_data] = builtin.unconstr_fields(witness_data)
            expect
                source: transition_trace.RootMembershipProof<
                  ledger_state.DepositId,
                  ledger_state.DepositInfo,
                >
              = source_data
            timed_history.out_of_window_deposit(
              tx,
              deposit_parameters,
              event_reference,
              header,
              source,
            )
          }
          1 -> {
            expect [source_data] = builtin.unconstr_fields(witness_data)
            expect
                source: transition_trace.RootMembershipProof<
                  ledger_state.WithdrawalId,
                  ledger_state.WithdrawalInfo,
                >
              = source_data
            timed_history.out_of_window_withdrawal(
              tx,
              withdrawal_parameters,
              event_reference,
              header,
              source,
            )
          }
          _ -> False
        }
      _ -> False
    }
  }

  else(_) {
    fail
  }
}

validator forced_timing(
  dispatcher: ScriptHash,
  computation_thread_policy: PolicyId,
  hub_oracle: PolicyId,
) {
  withdraw(_redeemer: Data, _credential: Credential, tx: Transaction) {
    let (input, datum, args) = delegation.dispatch(dispatcher, tx)
    expect Some(raw) = datum.data
    // Decode only the selected timing witness, avoiding every unrelated proof
    // variant in this script. The envelope remains fully authenticated below.
    expect builtin.unconstr_index(raw) == 0
    expect [hash_data, header_data, fault_data] = builtin.unconstr_fields(raw)
    expect header_hash: ByteArray = hash_data
    expect header: ledger_state.HeaderV1 = header_data
    expect [witness_data] = builtin.unconstr_fields(fault_data)
    expect [Pair(asset_name, 1)] = input.output.value
      |> assets.tokens(computation_thread_policy)
      |> dict.to_pairs
    expect
      proof.validate_transition_header_envelope(header_hash, header, asset_name)
    let reference_inputs = tx.reference_inputs
    let hub_datum = hub.get_datum(
      reference_inputs,
      hub_oracle,
      args.hub_ref_input_index,
    )
    let event_reference = args.event_reference
    expect event_reference.external_data_index == None
    expect builtin.unconstr_index(witness_data) == 2
    expect [_,
          asset_data,
          validity_data, source_data] = builtin.unconstr_fields(witness_data)
    expect event_asset_name: ByteArray = asset_data
    expect validity: rejection_reason_v1.OperatorVerdictV1 = validity_data
    when builtin.unconstr_index(fault_data) is {
      5 -> {
        expect
            source: transition_trace.RootNonMembershipProof<
              ledger_state.TxOrderId,
            >
          = source_data
        proof.validate_forced_l1_event_fault(
          header,
          proof.OmittedDueL1Event {
            witness: proof.OmittedDueForcedTransaction {
              event_ref_input_index: event_reference.order_index,
              event_asset_name,
              validity_override: validity,
              source_non_membership: source,
            },
          },
          hub_datum,
          reference_inputs,
          event_reference.order_index,
        )
      }
      7 -> {
        expect
            source: transition_trace.RootMembershipProof<
              ledger_state.TxOrderId,
              ledger_state.ForcedInclusionTxV1,
            >
          = source_data
        proof.validate_forced_l1_event_fault(
          header,
          proof.OutOfWindowSourceEvent {
            witness: proof.OutOfWindowForcedTransaction {
              event_ref_input_index: event_reference.order_index,
              event_asset_name,
              validity_override: validity,
              source_membership: source,
            },
          },
          hub_datum,
          reference_inputs,
          event_reference.order_index,
        )
      }
      _ -> False
    }
  }

  else(_) {
    fail
  }
}
