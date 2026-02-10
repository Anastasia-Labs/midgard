use aiken/builtin
use aiken/collection/list
use aiken/interval.{Finite, Interval, is_entirely_after}
use aiken_design_patterns/linked_list
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction, ValidityRange,
}
use midgard/common/utils.{get_singleton_asset_with_policy}
use midgard/hub_oracle as hub
use midgard/operator_directory/registered_operators
use midgard/scheduler.{
  Advance, Datum, Deinit, Init, MintRedeemer, Rewind, SpendRedeemer, asset_name,
  scheduler_asset_name, shift_end_time,
}

validator mint(hub_oracle_script_hash: PolicyId) {
  mint(redeemer: MintRedeemer, own_policy_id: PolicyId, self: Transaction) {
    let Transaction { mint, .. } = self
    when redeemer is {
      Init -> and {
          Pair(hub.asset_name, 1) == get_singleton_asset_with_policy(
            mint,
            hub_oracle_script_hash,
          ),
          Pair(asset_name, 1) == get_singleton_asset_with_policy(
            mint,
            own_policy_id,
          ),
        }
      Deinit -> and {
          Pair(hub.asset_name, -1) == get_singleton_asset_with_policy(
            mint,
            hub_oracle_script_hash,
          ),
          Pair(asset_name, -1) == get_singleton_asset_with_policy(
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
      Advance { scheduler_output_index, active_node_ref_input_index } -> {
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
              tx_interval_contained(
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
              tx_interval_contained(
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

fn tx_interval_contained(
  validity_range: ValidityRange,
  start_time: Int,
  end_time: Int,
) {
  expect Finite(valid_from) = validity_range.lower_bound.bound_type
  expect Finite(valid_to) = validity_range.upper_bound.bound_type
  start_time <= valid_from && valid_to <= end_time
}
