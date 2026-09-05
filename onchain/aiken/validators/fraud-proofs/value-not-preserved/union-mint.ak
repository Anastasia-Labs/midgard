use aiken/primitive/bytearray
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/computation_thread as ct
use midgard/fraud_proofs/common.{cancel, continue}
use midgard/fraud_proofs/field_opening_v1.{
  BodyAnchor, FieldOpeningV1, resume_opened_field_walk,
}
use midgard/fraud_proofs/value_not_preserved/union.{
  FoldState, PendingContribution,
}
use midgard/fraud_proofs/value_not_preserved/union_mint_scan
use midgard/fraud_proofs/value_not_preserved/union_state.{
  Complete, FieldCursor, MintCursor, MintPolicy,
}
use midgard/native_tx_field_access_v1.{field_read_range}
use midgard/native_tx_machine_walk_v1.{
  field_walk_checkpoint_hash, walk_is_complete, walk_next_extent,
}

pub type Args {
  input_index: Int,
  output_index: Int,
  opening: FieldOpeningV1,
  checkpoint_bytes: ByteArray,
}

validator main(
  union_update_hash: ByteArray,
  terminal_hash: ByteArray,
  field_certificate_policy_id: PolicyId,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<ct.StepDatum<FoldState>>,
    redeemer: ct.StepRedeemer<Args>,
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
        let own_hash, _thread_name, _, input_state, output_hash, output_state <- continue(
          computation_thread_token_policy_id,
          step_datum,
          args.input_index,
          args.output_index,
          own_out_ref,
          tx.inputs,
          tx.outputs,
        )
        expect Some(state) = input_state
        expect cursor: MintCursor = state.continuation
        expect cursor.field.field_index == 5
        let Pair(view, checkpoint) = resume_opened_field_walk(
          args.opening,
          BodyAnchor { tx_id: state.balance.transaction_id },
          5,
          cursor.field.checkpoint_hash,
          args.checkpoint_bytes,
          tx.reference_inputs,
          field_certificate_policy_id,
        )
        when cursor.active is {
          None ->
            if walk_is_complete(checkpoint) {
              let context: Data = Complete
              let expected: Data = FoldState { ..state, continuation: context }
              output_hash == terminal_hash && output_state == expected
            } else {
              let Pair(Pair(offset, length), advanced) = walk_next_extent(
                view,
                checkpoint,
              )
              let window = field_read_range(
                view,
                offset,
                if length < 132 {
                  length
                } else {
                  132
                },
              )
              let Pair(policy_id, Pair(consumed, count)) = union_mint_scan.policy_header(
                window,
                cursor.previous_policy,
              )
              expect consumed <= length
              let active = MintPolicy {
                offset,
                length,
                next_checkpoint_hash: field_walk_checkpoint_hash(advanced),
                policy_id,
                cursor: consumed,
                remaining: count,
                previous_asset: None,
              }
              let context: Data = MintCursor { ..cursor, active: Some(active) }
              let expected: Data = FoldState { ..state, continuation: context }
              output_hash == own_hash && output_state == expected
            }
          Some(policy) ->
            if policy.remaining == 0 {
              expect policy.cursor == policy.length
              let context: Data = MintCursor {
                field: FieldCursor {
                  ..cursor.field,
                  checkpoint_hash: policy.next_checkpoint_hash,
                },
                previous_policy: Some(policy.policy_id),
                active: None,
              }
              let expected: Data = FoldState { ..state, continuation: context }
              output_hash == own_hash && output_state == expected
            } else {
              expect policy.remaining > 0
              let remaining = policy.length - policy.cursor
              expect remaining > 0
              let window = field_read_range(
                view,
                policy.offset + policy.cursor,
                if remaining < 132 {
                  remaining
                } else {
                  132
                },
              )
              let Pair(name, Pair(quantity, consumed)) = union_mint_scan.asset(
                window,
                policy.previous_asset,
              )
              expect policy.cursor + consumed <= policy.length
              let advanced = MintPolicy {
                ..policy,
                cursor: policy.cursor + consumed,
                remaining: policy.remaining - 1,
                previous_asset: Some(name),
              }
              let next: Data = MintCursor { ..cursor, active: Some(advanced) }
              let pending: Data = PendingContribution {
                unit: bytearray.concat(policy.policy_id, name),
                quantity,
                return_script_hash: own_hash,
                next_continuation: next,
              }
              let expected: Data = FoldState { ..state, continuation: pending }
              output_hash == union_update_hash && output_state == expected
            }
        }
      }
    }
  }

  else(_) {
    fail
  }
}
