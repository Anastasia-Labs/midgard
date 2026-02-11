use aiken/builtin
use aiken/collection/list
use aiken/interval.{Finite, is_entirely_after}
use aiken_design_patterns/linked_list
use aiken_design_patterns/singular_utxo_indexer
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction, ValidityRange,
}
use midgard/common/utils
use midgard/hub_oracle as hub
use midgard/operator_directory/registered_operators
use midgard/scheduler.{
  Advance, Datum, Deinit, Init, MintRedeemer, Rewind, SpendRedeemer, asset_name,
  shift_end_time,
}

validator mint(hub_oracle_script_hash: PolicyId) {
  mint(redeemer: MintRedeemer, own_policy_id: PolicyId, self: Transaction) {
    let Transaction { mint, .. } = self
    when redeemer is {
      Init -> and {
          Pair(hub.asset_name, 1) == utils.get_singleton_asset_with_policy(
            mint,
            hub_oracle_script_hash,
          ),
          Pair(asset_name, 1) == utils.get_singleton_asset_with_policy(
            mint,
            own_policy_id,
          ),
        }
      Deinit -> and {
          Pair(hub.asset_name, -1) == utils.get_singleton_asset_with_policy(
            mint,
            hub_oracle_script_hash,
          ),
          Pair(asset_name, -1) == utils.get_singleton_asset_with_policy(
            mint,
            own_policy_id,
          ),
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
    m_datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    self: Transaction,
  ) {
    when redeemer is {
      Advance {
        scheduler_input_index,
        scheduler_output_index,
        active_node_ref_input_index,
      } -> {
        let Transaction {
          inputs,
          reference_inputs,
          outputs,
          redeemers,
          extra_signatories,
          validity_range,
          ..
        } = self

        // 1. Proper datum must be attached to the scheduler input.
        expect Some(Datum {
          operator: input_operator,
          start_time: input_start_time,
        }) = m_datum

        // 2. Output and the correct input must exist at the specified indices.
        let
          input,
          output,
        <-
          singular_utxo_indexer.one_to_one(
            input_index: scheduler_input_index,
            output_index: scheduler_output_index,
            own_ref: own_out_ref,
            inputs: inputs,
            outputs: outputs,
            double_satisfaction_prevented: todo,
          )

        // 3. Both input and output must have the scheduler NFT, and the output
        //    datum must be properly formed.
        expect Datum {
          operator: output_operator,
          start_time: output_start_time,
        } =
          utils.authenticate_input_output_and_get_output_datum_data(
            input,
            output,
            scheduler_policy_id,
          )

        // 4. The start time of the new shift must be the end time of the
        //    previous shift.
        expect output_start_time == shift_end_time(input_start_time)

        // 5. Either the start time of the new shift is in the past, or it's
        //    contained within the transaction's validity range, in which case
        //    the previous operator (who is currently active) must consent to
        //    advancing the schedule.
        //
        //    TODO: See [#416](https://github.com/anastasia-shmidova/midgard/issues/416).
        expect or {
            validity_range |> interval.is_entirely_after(output_start_time),
            and {
              input_operator |> utils.has_signed(extra_signatories),
              {
                let (_inclusive_lower_bound, inclusive_upper_bound) =
                  utils.get_inclusive_bounds_of_a_short_validity_range(
                    validity_range,
                  )
                output_start_time < inclusive_upper_bound
              },
            },
          }

        expect Some(active_node_ref_input) =
          list.at(reference_inputs, active_node_ref_input_index)

        {
          let
            _active_node_lovelace,
            m_active_node_key,
            _active_node_data,
            active_node_link,
          <- linked_list.get_element_info(active_node_ref_input)

          expect Some(output_operator) == m_active_node_key

          expect Some(active_node_linked_operator) = active_node_link

          bytearray.compare(input_operator, active_node_linked_operator) != Greater
        }
          |> active_operators.finalize_linked_list(active_operators_policy_id)

        // Done.
        True
      }
      Rewind {
        scheduler_output_index,
        active_node_ref_input_index,
        active_root_node_ref_input_index,
        registered_node_ref_input_index,
      } -> {
        expect Some(scheduler_output) =
          list.at(self.outputs, scheduler_output_index)
        expect InlineDatum(scheduler_output_datum) = scheduler_output.datum
        expect parsed_scheduler_output_datum: Datum = scheduler_output_datum
        //1.
        expect
          Pair(scheduler_asset_name, 1) == get_singleton_asset_with_policy(
            input.output.value,
            scheduler_policy_id,
          )
        //2.
        expect
          Pair(scheduler_asset_name, 1) == get_singleton_asset_with_policy(
            scheduler_output.value,
            scheduler_policy_id,
          )
        //5.
        let datum_end_time = shift_end_time(datum.start_time)
        let parsed_scheduler_output_datum_end_time =
          shift_end_time(parsed_scheduler_output_datum.start_time)
        expect parsed_scheduler_output_datum.start_time == datum_end_time
        //6.
        expect or {
            and {
              list.has(
                self.extra_signatories,
                parsed_scheduler_output_datum.operator,
              ),
              tx_interval_is_contained(
                self.validity_range,
                parsed_scheduler_output_datum.start_time,
                parsed_scheduler_output_datum_end_time,
              ),
            },
            is_entirely_after(
              self.validity_range,
              parsed_scheduler_output_datum.start_time,
            ),
          }
        //7.
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
        expect Key { key: root_node_link_as_bytearray } =
          parsed_active_root_node_datum.link
        expect
          builtin.less_than_equals_bytearray(
            datum.operator,
            root_node_link_as_bytearray,
          )
        expect parsed_active_root_node_datum.link == Key(datum.operator)
        //11 and 12.
        expect Some(registered_node_ref_input) =
          list.at(self.reference_inputs, registered_node_ref_input_index)
        expect InlineDatum(registered_node_datum) =
          registered_node_ref_input.output.datum
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
        expect Finite(valid_to) = self.validity_range.upper_bound.bound_type
        expect valid_to < registered_node_data.activation_time
        // Done.
        True
      }
    }
  }

  else(_) {
    fail
  }
}

fn tx_interval_is_contained(
  validity_range: ValidityRange,
  start_time: Int,
  end_time: Int,
) {
  expect ClosedRange { lower, upper } = vrn.normalize_time_range(validity_range)
  and {
    lower <= start_time,
    upper > end_time,
  }
}
