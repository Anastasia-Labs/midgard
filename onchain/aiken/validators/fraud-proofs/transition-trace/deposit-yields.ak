use aiken/builtin
use aiken/cbor
use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Credential}
use cardano/assets.{PolicyId}
use cardano/transaction.{Transaction}
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

validator projection(dispatcher: ScriptHash, hub_oracle: PolicyId) {
  withdraw(opened: OpenedOutputs, _credential: Credential, tx: Transaction) {
    let (datum, args) = delegation.dispatch(dispatcher, tx.inputs, tx.redeemers)
    expect Some(state) = datum.data
    expect state.kind == 1
    if state.phase == 10 {
      let (reference, policy, asset) = deposit_source.open(
        state,
        args,
        tx.reference_inputs,
      )
      let expected = proof.projected_deposit_output(
        reference.event.info,
        assets.from_asset(policy, asset, 1),
        policy,
        asset,
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
      let (_, fault, _) = dispatched(
        dispatcher,
        tx.inputs,
        tx.redeemers,
        tx.reference_inputs,
      )
      expect [trace_data,
            _,
            source_data,
            _, asset_data, projected_data] = one_step_witness(fault, 3)
      expect trace_proof: IndexedTraceProof = trace_data
      expect
          source: RootMembershipProof<
            ledger_state.DepositId,
            ledger_state.DepositInfo,
          >
        = source_data
      expect event_asset_name: ByteArray = asset_data
      expect projected: proof.LedgerInsertWitness = projected_data
      let hub_datum = hub.get_datum(
        tx.reference_inputs,
        hub_oracle,
        args.hub_ref_input_index,
      )
      let reference = proof.get_authenticated_deposit_reference(
        tx.reference_inputs,
        hub_datum.deposit,
        event_asset_name,
        args.deposit_event_ref_index,
      )
      let count = reference.value
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
      let out_ref: Data = reference.out_ref
      expect count > 0
      and {
        reference.event.id == source.key,
        reference.event.info == source.value,
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
            deposit_source_cbor: cbor.serialise(
              [
                out_ref,
                builtin.b_data(hub_datum.deposit),
                builtin.b_data(event_asset_name),
              ],
            ),
            deposit_asset_count: count - 1,
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
