use aiken/builtin
use aiken/collection/list
use aiken/collection/dict.{Dict}
use aiken/interval.{Finite, Interval}
use midgard/linked_list/ordered.{Key} as ordered
use cardano/assets.{AssetName, PolicyId, Value, quantity_of}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction, ValidityRange,
}
use midgard/common/utils.{get_singleton_asset_with_policy}
use midgard/registered_operators.{Datum as RegisteredOperatorDatum}
use midgard/scheduler.{
  Advance, Datum, Deinit, Init, MintRedeemer, Rewind, SpendRedeemer,
  parse_scheduler_datum, scheduler_asset_name, shift_end_time,
}

const max_scheduler_transition_validity_window = 600_000

pub fn from_value(self: Value) -> Pairs<PolicyId, Dict<AssetName, Int>> {
  self |> assets.to_dict |> dict.to_pairs
}

/// Lookup the first asset policy from an output, or fails loudly if the output holds no tokens (beyond Ada).
pub fn peek_first(self: Output) -> PolicyId {
  builtin.head_list(builtin.tail_list(from_value(self.value))).1st
}

/// Lookup the first asset policy from an output
//  fails if: 
//   1. the output holds no tokens (beyond Ada).
//   2. the output contains more than exactly one non-ada asset.
pub fn peek_only(self: Output) -> PolicyId {
  let non_ada_pairs = builtin.tail_list(from_value(self.value))
  expect builtin.null_list(builtin.tail_list(non_ada_pairs))
  builtin.head_list(non_ada_pairs).1st
}

validator mint(
  hub_oracle_policy_id: PolicyId,
  hub_oracle_asset_name: ByteArray,
) {
  mint(redeemer: MintRedeemer, policy_id: PolicyId, self: Transaction) {
    when redeemer is {
      Init -> {
        expect
          Pair(hub_oracle_asset_name, 1) == get_singleton_asset_with_policy(
            self.mint,
            hub_oracle_policy_id,
          )
        expect
          Pair(scheduler_asset_name, 1) == get_singleton_asset_with_policy(
            self.mint,
            policy_id,
          )
        expect Some(scheduler_output) =
          list.find(
            self.outputs,
            fn(output) { quantity_of(output.value, policy_id, scheduler_asset_name) == 1 },
          )
        expect InlineDatum(scheduler_output_datum) = scheduler_output.datum
        let parsed_scheduler_output_datum =
          parse_scheduler_datum(scheduler_output_datum)
        expect parsed_scheduler_output_datum.operator == ""
        expect parsed_scheduler_output_datum.start_time == 0
        True
      }
      Deinit -> {
        expect
          Pair(hub_oracle_asset_name, -1) == get_singleton_asset_with_policy(
            self.mint,
            hub_oracle_policy_id,
          )
        expect
          Pair(scheduler_asset_name, -1) == get_singleton_asset_with_policy(
            self.mint,
            policy_id,
          )
        True
      }
    }
  }

  else(_) {
    fail
  }
}

validator spend(
  registered_operators_policy_id: PolicyId,
  active_operators_policy_id: PolicyId,
  scheduler_policy_id: PolicyId,
) {
  spend(
    opt_datum: Option<Datum>,
    redeemer: SpendRedeemer,
    inputOutRef: OutputReference,
    self: Transaction,
  ) {
    expect Some(datum) = opt_datum
    expect Some(input) =
      list.find(
        self.inputs,
        fn(a_input) { a_input.output_reference == inputOutRef },
      )
    when redeemer is {
      // Advance is the single-step scheduler transition inside the current
      // cycle. The witness node proves that the newly assigned operator can be
      // reached directly from the previously scheduled operator without
      // wrapping around the active-operators boundary.
      //
      // Intended authorization semantics:
      // 1. The currently scheduled operator (`datum.operator`) may authorize
      //    the hand-off by signing the transaction.
      // 2. Once the current shift has timed out, anyone may advance the
      //    scheduler if the validity interval starts at or after
      //    `datum.start_time + shift_duration`.
      // 3. The genesis empty scheduler may assign its initial operator without
      //    moving `start_time` away from zero.
      Advance { scheduler_output_index, active_node_ref_input_index } -> {
        expect Some(scheduler_output) =
          list.at(self.outputs, scheduler_output_index)
        expect InlineDatum(scheduler_output_datum) = scheduler_output.datum
        let parsed_scheduler_output_datum =
          parse_scheduler_datum(scheduler_output_datum)
        //1.
        //2.
        expect peek_only(input.output) == scheduler_policy_id
        expect peek_only(scheduler_output) == scheduler_policy_id
        //5.
        expect
          if advance_is_genesis_takeover(
            datum.operator,
            datum.start_time,
            parsed_scheduler_output_datum.start_time,
          ) {
            True
          } else {
            tx_interval_starts_new_shift(
              self.validity_range,
              datum.start_time,
              parsed_scheduler_output_datum.start_time,
            )
          }
        //6.
        expect
          if advance_is_genesis_takeover(
            datum.operator,
            datum.start_time,
            parsed_scheduler_output_datum.start_time,
          ) {
            True
          } else {
            // For normal in-cycle advances, authority comes from the currently
            // scheduled operator, not from the newly assigned operator.
            //
            // If the current operator signs, they can explicitly hand off the
            // next shift. Otherwise, once the current shift has expired,
            // anyone may advance the scheduler by starting the transaction
            // at or after the shift boundary.
            list.has(self.extra_signatories, datum.operator) ||
            tx_validity_starts_after_shift_end(
              self.validity_range,
              datum.start_time,
            )
          }
        //7.
        expect Some(active_node_ref_input) =
          list.at(self.reference_inputs, active_node_ref_input_index)
        expect InlineDatum(active_node_datum) =
          active_node_ref_input.output.datum
        expect parsed_active_node_datum: ordered.NodeDatum = active_node_datum
        expect
          ordered.prove_is_member(
            active_operators_policy_id,
            parsed_active_node_datum.key,
            active_node_ref_input.output,
          )
        //8.
        expect
          parsed_active_node_datum.key == Key(
            parsed_scheduler_output_datum.operator,
          )
        //9.
        expect parsed_active_node_datum.link == Key(datum.operator)
        // Done.
        True
      }
      // Rewind is the cycle wrap-around transition. It is used when the
      // scheduler can no longer prove the next assignee with a single
      // in-cycle witness node and must instead re-anchor itself at the cycle
      // boundary.
      //
      // This branch therefore inspects:
      // - the active-operators boundary witness for the wrapped assignee,
      // - the active-operators root witness that anchors the cycle boundary,
      // - and a registered-operators witness proving that no queued operator
      //   must activate before the wrap-around can occur.
      Rewind {
        scheduler_output_index,
        active_node_ref_input_index,
        active_root_node_ref_input_index,
        registered_node_ref_input_index,
      } -> {
        expect Some(scheduler_output) =
          list.at(self.outputs, scheduler_output_index)
        expect InlineDatum(scheduler_output_datum) = scheduler_output.datum
        let parsed_scheduler_output_datum =
          parse_scheduler_datum(scheduler_output_datum)
        //1.
        //2.
        expect peek_only(input.output) == scheduler_policy_id
        expect peek_only(scheduler_output) == scheduler_policy_id
        //5.
        // The wrap-around still advances exactly one shift forward in time.
        // Rewind changes which operator owns the next shift, but it does not
        // relax the scheduler's time progression rules.
        expect
          tx_interval_starts_new_shift(
            self.validity_range,
            datum.start_time,
            parsed_scheduler_output_datum.start_time,
          )
        //6.
        // Rewind uses the same authorization model as Advance:
        // - the currently scheduled operator may explicitly authorize the
        //   wrap-around by signing, or
        // - once that operator's shift has expired, anyone may perform the
        //   rewind so liveness does not depend on the outgoing operator.
        //
        // The signer is therefore `datum.operator` (the scheduler input
        // operator), not `parsed_scheduler_output_datum.operator` (the wrapped
        // assignee for the next shift).
        expect
          list.has(self.extra_signatories, datum.operator) ||
          tx_validity_starts_after_shift_end(
            self.validity_range,
            datum.start_time,
          )
        //7.
        // The wrapped assignee must be the last active-operators node. At a
        // high level, this proves that the scheduler is moving across the list
        // boundary instead of taking a normal in-cycle advance.
        expect Some(active_node_ref_input) =
          list.at(self.reference_inputs, active_node_ref_input_index)
        expect InlineDatum(active_node_datum) =
          active_node_ref_input.output.datum
        expect parsed_active_node_datum: ordered.NodeDatum = active_node_datum
        expect
          ordered.prove_is_last_node(
            active_operators_policy_id,
            active_node_ref_input.output,
          )
        //8.
        expect Some(active_root_node_ref_input) =
          list.at(self.reference_inputs, active_root_node_ref_input_index)
        expect InlineDatum(active_root_node_datum) =
          active_root_node_ref_input.output.datum
        expect parsed_active_root_node_datum: ordered.NodeDatum =
          active_root_node_datum
        expect
          ordered.prove_is_root_node(
            active_operators_policy_id,
            active_root_node_ref_input.output,
          )
        //9.
        expect
          parsed_active_node_datum.key == Key(
            parsed_scheduler_output_datum.operator,
          )
        //10.
        // This branch explains the special case for the canonical genesis
        // scheduler datum:
        // - `datum.operator == ""` means the scheduler has not yet assigned a
        //   real operator.
        // - `datum.start_time == 0` means we are still at the bootstrap state.
        //
        // In that situation there is no real "previous operator" to prove
        // against, so the code cannot require `root.link == Key(datum.operator)`
        // because `datum.operator` is the empty sentinel, not a member key.
        // Instead, the validator only checks that the active-operators root
        // points to some non-empty first operator, which proves the active list
        // is initialized and gives Rewind a legitimate operator to assign.
        //
        // Outside genesis, the current implementation uses the root link as the
        // cycle-boundary witness and requires it to point at the previously
        // scheduled operator before allowing the wrap-around.
        if datum.operator == "" {
          expect datum.start_time == 0
          expect parsed_active_root_node_datum.link != ordered.Empty
        } else {
          expect Key { key: root_node_link_as_bytearray } =
            parsed_active_root_node_datum.link
          expect
            builtin.less_than_equals_bytearray(
              datum.operator,
              root_node_link_as_bytearray,
            )
          expect parsed_active_root_node_datum.link == Key(datum.operator)
        }
        //11 and 12.
        // Rewind is only valid if no queued registered operator is already
        // eligible to activate at the cycle boundary.
        //
        // The witness is the last registered-operators node, because the queue
        // prepends new registrations at the front, so the last node is the
        // earliest registrant that would mature first.
        //
        // If the registered list is empty, there is nothing to block the
        // rewind. Otherwise this branch proves the chosen witness is that last
        // node and then checks that the refresh transaction finishes strictly
        // before the witness activation time.
        expect Some(registered_node_ref_input) =
          list.at(self.reference_inputs, registered_node_ref_input_index)
        expect InlineDatum(registered_node_datum) =
          registered_node_ref_input.output.datum
        expect Finite(valid_to) = self.validity_range.upper_bound.bound_type
        expect if ordered.prove_is_empty_list(
          registered_operators_policy_id,
          registered_node_ref_input.output,
        ) {
          True
        } else {
          expect parsed_registered_node_datum: ordered.NodeDatum =
            registered_node_datum
          expect
            ordered.prove_is_last_node(
              registered_operators_policy_id,
              registered_node_ref_input.output,
            )
          //13.
          expect registered_node_data: RegisteredOperatorDatum =
            parsed_registered_node_datum.data
          valid_to < registered_node_data.activation_time
        }
        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

fn tx_interval_starts_new_shift(
  validity_range: ValidityRange,
  previous_start_time: Int,
  next_start_time: Int,
) {
  expect Finite(valid_from) = validity_range.lower_bound.bound_type
  expect Finite(valid_to) = validity_range.upper_bound.bound_type
  expect valid_from >= shift_end_time(previous_start_time)
  expect valid_to >= valid_from
  expect valid_to <= valid_from + max_scheduler_transition_validity_window
  expect next_start_time == valid_from
  True
}

fn advance_is_genesis_takeover(
  previous_operator: ByteArray,
  previous_start_time: Int,
  next_start_time: Int,
) {
  previous_operator == "" && next_start_time == previous_start_time
}

fn tx_validity_starts_after_shift_end(
  validity_range: ValidityRange,
  previous_start_time: Int,
) {
  expect Finite(valid_from) = validity_range.lower_bound.bound_type
  valid_from >= shift_end_time(previous_start_time)
}
