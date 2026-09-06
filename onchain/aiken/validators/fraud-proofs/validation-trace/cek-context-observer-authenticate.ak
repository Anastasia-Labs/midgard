use aiken/builtin
use aiken/crypto.{ScriptHash}
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/cek_context_chain as chain
use midgard/cek_context_observer as observer
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/native_tx/compact.{verify_native_tx_proof_source_v1}
use midgard/native_tx_field_access_v1.{FieldCarriageV1, empty_field_commitment}
use midgard/validation_machine_field_door_v1 as door

validator main(
  fold_script_hash: ScriptHash,
  computation_thread_policy_id: PolicyId,
  field_preimage_certificate_policy_id: PolicyId,
) {
  spend(
    datum: Option<chain.StagedDatum>,
    redeemer: chain.ContextRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ct.Cancel { input_index, computation_thread_mint_redeemer_index } ->
        cancel(
          computation_thread_policy_id,
          datum,
          input_index,
          own_out_ref,
          computation_thread_mint_redeemer_index,
          tx,
        )
      ct.Continue(action) -> {
        expect Some(step) = datum
        expect Some(staged) = step.data
        expect staged.context.stage == 5
        expect chain.auxiliary_is_bound(staged.bound, action.auxiliary)
        let native = staged.native
        let context = staged.context
        let Pair(source, witness_set) = verify_native_tx_proof_source_v1(
          staged.bound.transaction_id,
          native.compact_cbor,
          native.witness_set_compact_cbor,
          native.field_preimage_lengths_cbor,
        )
        expect source.version == 1
        let opening = if source.tx_compact.body.required_observers_hash == empty_field_commitment {
          expect builtin.un_constr_data(action.auxiliary) == Pair(0, [])
          observer.EmptyObservers
        } else if context.observer_count > 0 && context.observer_items.length == context.observer_count {
          expect builtin.un_constr_data(action.auxiliary) == Pair(0, [])
          observer.FinishedObservers
        } else {
          expect Pair(1, [field_data,
                index_data,
                carriage_data]) = builtin.un_constr_data(action.auxiliary)
          expect field: Int = field_data
          expect index: Int = index_data
          expect carriage: FieldCarriageV1 = carriage_data
          let field_door = door.MachineFieldDoorV1 {
            reference_inputs: tx.reference_inputs,
            certificate_policy_id: field_preimage_certificate_policy_id,
          }
          let count = if context.observer_count == 0 {
            door.machine_field_count(
              field_door,
              source,
              witness_set,
              3,
              carriage,
            )
          } else {
            context.observer_count
          }
          let expected_index = count - context.observer_items.length - 1
          expect field == 3 && index == expected_index && expected_index >= 0
          let item = door.open_machine_field_item(
            field_door,
            source,
            witness_set,
            3,
            expected_index,
            carriage,
          )
          expect door.machine_field_item_count(item) == count
          expect door.machine_field_item_length(item) == 28
          observer.ObserverItem {
            count,
            hash: door.machine_field_item_bytes(item),
          }
        }
        let staged_data: Data = staged
        let next: Data = observer.ObserverPending {
          staged: staged_data,
          opening,
        }
        continue(
          computation_thread_policy_id,
          step,
          action.input_index,
          action.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
          fn(_, _, _, _, hash, state) {
            hash == fold_script_hash && state == next
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
