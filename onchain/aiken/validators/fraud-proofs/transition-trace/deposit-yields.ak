use aiken/cbor
use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Address, Credential}
use cardano/assets.{PolicyId}
use cardano/transaction.{Transaction}
use midgard/event_history/order_facts
use midgard/fraud_proofs/native_tx/components
use midgard/fraud_proofs/transition_trace/deposit_source
use midgard/fraud_proofs/transition_trace/final_yield.{
  OpenedOutputs, dispatched, one_step_witness,
}
use midgard/fraud_proofs/transition_trace/final_yield as delegation
use midgard/fraud_proofs/transition_trace/proof
use midgard/hub_oracle as hub
use midgard/ledger_state
use midgard/transition_trace.{IndexedTraceProof, RootMembershipProof}

validator projection(
  dispatcher: ScriptHash,
  hub_oracle: PolicyId,
  retention_address: Address,
  inline_limit: Int,
  max_payload_bytes: Int,
  max_payload_nodes: Int,
) {
  withdraw(opened: OpenedOutputs, _credential: Credential, tx: Transaction) {
    let (datum, args) = delegation.dispatch(dispatcher, tx.inputs, tx.redeemers)
    expect Some(state) = datum.data
    expect state.kind == 1
    if state.phase == 10 {
      let reference = deposit_source.open(state, args)
      let expected = proof.projected_original_deposit_output(
        reference.event.info,
        assets.zero,
      )
        |> components.encode_midgard_tx_output
      expect
        delegation.output_skeleton(
          state,
          delegation.output_bytes(state, args, tx.reference_inputs),
        ) == expected
      delegation.advance(
        datum,
        args,
        tx.outputs,
        delegation.State { ..state, phase: 8 },
      )
    } else {
      expect state.phase == 0
      let (header_data, fault, _) = dispatched(
        dispatcher,
        tx.inputs,
        tx.redeemers,
        tx.reference_inputs,
      )
      expect header: ledger_state.HeaderV1 = header_data
      expect args.deposit_opening == None
      expect [trace_data,
            _, source_data, projected_data] = one_step_witness(fault, 3)
      expect trace_proof: IndexedTraceProof = trace_data
      expect
          source: RootMembershipProof<
            ledger_state.DepositId,
            ledger_state.DepositInfo,
          >
        = source_data
      expect projected: proof.LedgerInsertWitness = projected_data
      let hub_datum = hub.get_datum(
        tx.reference_inputs,
        hub_oracle,
        args.hub_ref_input_index,
      )
      let (commitment, payload, original_assets) = order_facts.capture_open(
        tx,
        hub_datum.deposit,
        hub_datum.deposit_addr,
        retention_address,
        order_facts.Deposit,
        source.key,
        header.end_time,
        inline_limit,
        max_payload_bytes,
        max_payload_nodes,
        args.deposit_event_ref_index,
        args.deposit_external_ref_index,
      )
      expect order_facts.DepositPayload { event } = payload
      let count = original_assets
        |> assets.to_pairs
        |> list.foldl(
            0,
            fn(pair, total) {
              if pair.1st == assets.ada_policy_id {
                total
              } else {
                total + dict.size(pair.2nd)
              }
            },
          )
      and {
        event.id == source.key,
        event.info == source.value,
        projected.key == proof.ledger_outref_key(source.key),
        opened.spend_input_keys == [],
        list.length(opened.output_hashes) == 1,
        delegation.advance(
          datum,
          args,
          tx.outputs,
          delegation.State {
            ..state,
            opened,
            phase: 2,
            deposit_source_cbor: cbor.serialise(commitment),
            deposit_asset_count: count,
            deposit_index: source.key.output_index,
            current_root: trace_proof.value.pre_utxos_root,
          },
        ),
      }
    }
  }

  else(_) {
    fail
  }
}
