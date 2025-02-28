use aiken/collection/list
use aiken/interval.{Finite, Interval, is_entirely_after}
use aiken_design_patterns/linked_list/ordered.{Key} as ordered
use cardano/assets.{PolicyId, quantity_of}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction, ValidityRange,
}
use midgard/common/utils.{quantity_of_policy_id}
use midgard/protocol_parameters.{registration_duration, shift_duration}
use midgard/registered_operators.{Datum as RegisteredOperatorDatum}
use midgard/scheduler.{
  Advance, Datum, Deinit, Init, MintRedeemer, Rewind, SpendRedeemer,
}

validator mint(
  hub_oracle_script_hash: PolicyId,
  hub_oracle_asset_name: ByteArray,
) {
  mint(redeemer: MintRedeemer, policy_id: PolicyId, self: Transaction) {
    when redeemer is {
      Init -> and {
          //TODO Should we check the destination of the minted NFT?
          quantity_of(self.mint, hub_oracle_script_hash, hub_oracle_asset_name) == 1,
          quantity_of(self.mint, policy_id, hub_oracle_asset_name) == 1,
          quantity_of_policy_id(self.mint, policy_id) == 1,
        }
      Deinit -> and {
          quantity_of(self.mint, hub_oracle_script_hash, hub_oracle_asset_name) == -1,
          quantity_of(self.mint, policy_id, hub_oracle_asset_name) == -1,
          quantity_of_policy_id(self.mint, policy_id) == 1,
        }
    }
  }

  else(_) {
    fail
  }
}

validator spend(
  registered_operators_script_hash: PolicyId,
  active_operators_script_hash: PolicyId,
  scheduler_policy_id: PolicyId,
  hub_oracle_asset_name: ByteArray,
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
          quantity_of(
            input.output.value,
            scheduler_policy_id,
            hub_oracle_asset_name,
          ) == 1
        //2.
        expect
          quantity_of(
            scheduler_output.value,
            scheduler_policy_id,
            hub_oracle_asset_name,
          ) == 1
        //5.
        expect
          parsed_scheduler_output_datum.start_time == datum.start_time + shift_duration
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
                parsed_scheduler_output_datum.start_time + shift_duration,
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
            active_operators_script_hash,
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
          quantity_of(
            input.output.value,
            scheduler_policy_id,
            hub_oracle_asset_name,
          ) == 1
        //2.
        expect
          quantity_of(
            scheduler_output.value,
            scheduler_policy_id,
            hub_oracle_asset_name,
          ) == 1
        //5.
        expect
          parsed_scheduler_output_datum.start_time == datum.start_time + shift_duration
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
                parsed_scheduler_output_datum.start_time + shift_duration,
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
            active_operators_script_hash,
            parsed_active_node_datum.key,
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
            active_operators_script_hash,
            active_root_node_ref_input.output,
          )
        //9.
        expect
          parsed_active_node_datum.key == Key(
            parsed_scheduler_output_datum.operator,
          )
        //10. TODO 
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
            registered_operators_script_hash,
            registered_node_ref_input.output,
          )
        //13.
        expect registered_node_data: RegisteredOperatorDatum =
          parsed_registered_node_datum.data
        expect Finite(valid_from) = self.validity_range.lower_bound.bound_type
        expect
          valid_from < registered_node_data.registration_time + registration_duration
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
  valid_from >= start_time && valid_to < end_time
}
