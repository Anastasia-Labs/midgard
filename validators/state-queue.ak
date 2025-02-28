use aiken/collection/list
use aiken/crypto
use aiken/interval.{Finite, Interval}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list/ordered
use aiken_design_patterns/linked_list/unordered.{
  Key, NodeDatum, get_node_inputs, get_node_outputs,
}
use cardano/assets.{PolicyId, quantity_of, tokens}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Spend, Transaction,
}
use midgard/active_operators
use midgard/ledger_state.{ConfirmedState, Header}
use midgard/protocol_parameters.{shift_duration}
use midgard/registered_operators
use midgard/retired_operators
use midgard/scheduler.{Datum as SchedulerDatum}
use midgard/state_queue.{
  CommitBlockHeader, Deinit, Init, MergeToConfirmedState, MintRedeemer,
  RemoveFraudulentBlockHeader,
}

validator spend(fraud_proof_catalog_mint_script_hash: ByteArray) {
  spend(
    _datum: Option<Data>,
    _redeemer: Data,
    _input: OutputReference,
    self: Transaction,
  ) {
    let tokens = tokens(self.mint, fraud_proof_catalog_mint_script_hash)

    unordered.list_state_transition(tokens)
  }

  else(_) {
    fail
  }
}

validator mint(
  hub_oracle_script_hash: PolicyId,
  hub_oracle_asset_name: ByteArray,
  active_operators_script_hash: PolicyId,
  retired_operators_script_hash: PolicyId,
  scheduler_script_hash: PolicyId,
  fraud_proof_script_hash: PolicyId,
  maturity_duration: Int,
) {
  mint(redeemer: MintRedeemer, policy_id: PolicyId, self: Transaction) {
    let node_in_outputs = get_node_inputs(self.inputs, policy_id)
    let node_outputs = get_node_outputs(self.outputs, policy_id)
    let node_mint = tokens(self.mint, policy_id)
    when redeemer is {
      Init -> and {
          quantity_of(self.mint, hub_oracle_script_hash, hub_oracle_asset_name) == 1,
          unordered.init(node_in_outputs, node_mint, policy_id),
        }
      Deinit -> and {
          quantity_of(self.mint, hub_oracle_script_hash, hub_oracle_asset_name) == -1,
          unordered.deinit(node_outputs, node_mint, policy_id),
        }
      CommitBlockHeader {
        operator,
        scheduler_asset_name,
        scheduler_state_ref_input_index,
        active_node_input_index,
        header_node_output_index,
        previous_header_node_output_index,
        active_operators_redeemer_index,
      } -> {
        // Commit the block header to the state queue, with the operator's consent:
        // (1) Let `operator` be a redeemer argument indicating the key of the
        //     operator committing the block header.
        // (2) The transaction must be signed by `operator`.
        expect list.has(self.extra_signatories, operator)

        // (3) The transaction must Append a node to the `state_queue`.
        //     Among the transaction outputs, let that node be `header_node` and
        //     its predecessor node be `previous_header_node`.
        expect Some(header_node_output) =
          list.at(self.outputs, header_node_output_index)
        expect InlineDatum(header_node_datum) = header_node_output.datum
        expect parsed_header_node_datum: NodeDatum = header_node_datum
        expect header_node_block_data: Header = parsed_header_node_datum.data
        expect parsed_header_node_block_data_as_bytearray: ByteArray =
          parsed_header_node_datum.data
        //
        expect Some(previous_header_node_output) =
          list.at(self.outputs, previous_header_node_output_index)
        expect InlineDatum(previous_header_node_datum) =
          previous_header_node_output.datum
        expect parsed_previous_header_node_datum: NodeDatum =
          previous_header_node_datum
        expect previous_header_node_block_data: Header =
          parsed_previous_header_node_datum.data
        //
        expect
          unordered.append_unsafe(
            parsed_header_node_datum.key,
            header_node_output_index,
            previous_header_node_output_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          )

        // (4) `operator` must be the operator of `header_node`.
        expect operator == header_node_block_data.operator_vkey

        // (5) The key field of header_node must be the hash of its data field.
        expect
          parsed_header_node_datum.key == Key(
            crypto.sha2_256(parsed_header_node_block_data_as_bytearray),
          )

        // Verify that it is the operator's turn to commit
        // according to the scheduler:
        // (6) The transaction must include a reference input with the scheduler NFT.
        //     Let that input be the `scheduler_state`.
        expect Some(scheduler_state_ref_input) =
          list.at(self.reference_inputs, scheduler_state_ref_input_index)
        expect InlineDatum(scheduler_state_datum) =
          scheduler_state_ref_input.output.datum
        expect parsed_scheduler_state_datum: SchedulerDatum =
          scheduler_state_datum
        expect
          quantity_of(
            scheduler_state_ref_input.output.value,
            scheduler_script_hash,
            scheduler_asset_name,
          ) == 1

        // (7) The `operator` field of `scheduler_state` must match `operator`.
        expect operator == parsed_scheduler_state_datum.operator

        // Verify block timestamps:
        // (8) The `start_time` of `header_node` must be equal to the
        //     `end_time` of the `previous_header_node`.
        expect
          header_node_block_data.start_time == previous_header_node_block_data.end_time

        // (9) The `end_time` of `header_node` must match the transaction's
        //      time-validity upper bound.
        expect Finite(valid_to) = self.validity_range.upper_bound.bound_type
        expect header_node_block_data.end_time == valid_to

        // (10) The `end_time` of `header_node` must be within the shift interval
        //      of `scheduler_state`.
        expect
          header_node_block_data.end_time >= parsed_scheduler_state_datum.start_time
        expect
          header_node_block_data.end_time <= parsed_scheduler_state_datum.start_time + shift_duration

        // Update the operator's timestamp in the active operators set:
        // (11) The transaction must include an input, spent via the Update
        //      Commitment Time redeemer, of an `active_operators` node with a key
        //      matching the `operator`.
        expect Some(active_operators_redeemer_pair) =
          list.at(self.redeemers, active_operators_redeemer_index)
        expect Spend(active_node_input_output_reference) =
          active_operators_redeemer_pair.1st
        expect active_operators.UpdateCommitmentTime { .. } =
          active_operators_redeemer_pair.2nd
        expect Some(active_node_input) =
          list.at(self.inputs, active_node_input_index)
        expect InlineDatum(active_node_datum) = active_node_input.output.datum
        expect parsed_active_node_datum: ordered.NodeDatum = active_node_datum
        expect
          active_node_input_output_reference == active_node_input.output_reference
        expect
          ordered.prove_is_member(
            active_operators_script_hash,
            parsed_active_node_datum.key,
            active_node_input.output,
          )
        expect ordered.Key(operator) == parsed_active_node_datum.key

        // Done.
        True
      }
      MergeToConfirmedState {
        header_node_key,
        header_node_input_index,
        confirmed_state_node_input_index,
        confirmed_state_node_output_index,
      } -> {
        // (1) The transaction must Remove a node from the `state_queue`.
        //     Let `header_node` be the removed node, `confirmed_state_node` be
        //     its predecessor node before removal,
        //     and `confirmed_state_node_output` be the remaining node after removal.
        expect Some(header_node_input) =
          list.at(self.outputs, header_node_input_index)
        expect InlineDatum(header_node_input_datum) = header_node_input.datum
        expect parsed_header_node_input_datum: NodeDatum =
          header_node_input_datum
        expect Header { start_time, end_time, protocol_version, .. } =
          parsed_header_node_input_datum.data
        //
        expect Some(confirmed_state_node_input) =
          list.at(self.inputs, confirmed_state_node_input_index)
        expect InlineDatum(confirmed_state_node_input_datum) =
          confirmed_state_node_input.output.datum
        expect parsed_confirmed_state_node_input_datum: NodeDatum =
          confirmed_state_node_input_datum
        expect ConfirmedState { header_hash, utxo_root, .. } =
          parsed_confirmed_state_node_input_datum.data
        //
        expect Some(confirmed_state_node_output) =
          list.at(self.outputs, confirmed_state_node_output_index)
        expect InlineDatum(confirmed_state_node_output_datum) =
          confirmed_state_node_output.datum
        expect parsed_confirmed_state_node_output_datum: NodeDatum =
          confirmed_state_node_output_datum
        expect parsed_confirmed_state_node_output_datum_data: ConfirmedState =
          parsed_confirmed_state_node_output_datum.data
        //
        expect
          unordered.remove(
            Key(header_node_key),
            header_node_input_index,
            confirmed_state_node_input_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          )

        // (2) `confirmed_state_node` and `confirmed_state_node_output` must both
        //     be root nodes of `state_queue`.
        expect
          unordered.prove_is_root_node(
            policy_id,
            confirmed_state_node_input.output,
          )
        expect
          unordered.prove_is_root_node(policy_id, confirmed_state_node_output)

        // (3) `header_node` must be mature --- the lower bound of the transaction
        //     validity interval meets or exceeds the sum of the `end_time` field
        //     of `header_node` and the Midgard `maturity_duration` protocol
        //     parameter.
        expect Finite(valid_from) = self.validity_range.lower_bound.bound_type
        expect valid_from >= end_time + maturity_duration

        // (4) `confirmed_state_node_output` must match:
        //    (a) `confirmed_state_node_input` on the `start_time`.
        //    (b) `confirmed_state_node_output` must match `header_node` on the
        //        `prev_header_hash`, `utxo_root`, `end_time`,
        //         and `protocol_version`.
        // (5) the `header_hash` of `confirmed_state_node_output` must match
        //     the `header node` key.
        let expected_new_confirmed_state =
          ConfirmedState {
            header_hash: header_node_key,
            prev_header_hash: header_hash,
            utxo_root,
            start_time,
            end_time,
            protocol_version,
          }
        expect
          parsed_confirmed_state_node_output_datum_data == expected_new_confirmed_state

        // TODO
        // (6) If either the `deposits_root` or `withdrawals_root` of `header_node`
        //     is _not_ the MPT root hash of the empty set, a `settlement_queue`
        //     node must be appended via the New Settlement redeemer.
        //     The redeemer must mention \code{header\_node} by input index.
        // 
        // Done.
        True
      }
      RemoveFraudulentBlockHeader {
        removed_node_input_index,
        predecessor_node_input_index,
        operators_redeemer_index,
        fraud_proof_ref_input_index,
        fraud_proof_asset_name,
        fraudulent_operator,
        operator_status,
      } -> {
        // Remove the fraudulent block header:
        // (1) Let `fraudulent_operator` be a redeemer argument indicating the
        //     operator who committed the fraudulent block header.
        // (2) The transaction must Remove a node from the `state_queue`.
        //     Let `removed_node` be the removed node and `predecessor_node` be its
        //     predecessor node before removal.
        // (3) `fraudulent_operator` must match the key of `removed_node`.
        expect Some(removed_node_input) =
          list.at(self.inputs, removed_node_input_index)
        expect Some(predecessor_node_input) =
          list.at(self.inputs, predecessor_node_input_index)
        let predecessor_node_key =
          unordered.get_key(
            unordered.get_raw_key_and_link(predecessor_node_input.output),
          )
        expect
          unordered.remove(
            Key(fraudulent_operator),
            removed_node_input_index,
            predecessor_node_input_index,
            node_in_outputs,
            node_outputs,
            node_mint,
          )

        // Slash the fraudulent operator:
        // (4) Let `operator_status` be a redeemer argument indicating whether
        //     the fraudulent operator is active or retired.
        expect
          when operator_status is {
            // (5) If `operator_status is active:
            //     The transaction must Remove a node from the `active_operators`
            //     set via the Remove Operator Slash Bond redeemer.
            //     The `slashed_operator` argument provided to that redeemer must
            //     match `fraudulent_operator`.
            registered_operators.Active -> {
              expect Some(active_operators_redeemer_pair) =
                list.at(self.redeemers, operators_redeemer_index)
              expect Mint(mint_script_hash) = active_operators_redeemer_pair.1st
              expect active_operators.RemoveOperatorSlashBond {
                slashed_active_operator_key,
                ..
              } = active_operators_redeemer_pair.2nd
              and {
                mint_script_hash == active_operators_script_hash,
                fraudulent_operator == slashed_active_operator_key,
              }
            }
            // (6) Otherwise:
            //     The transaction must Remove a node from the `retired_operators`
            //     set via the Remove Operator Slash Bond redeemer.
            //     The `slashed_operator` argument provided to that redeemer must
            //     match `fraudulent_operator`.
            registered_operators.Retired -> {
              expect Some(retired_operators_redeemer_pair) =
                list.at(self.redeemers, operators_redeemer_index)
              expect Mint(mint_script_hash) =
                retired_operators_redeemer_pair.1st
              expect retired_operators.RemoveOperatorSlashBond {
                slashed_retired_operator_key,
                ..
              } = retired_operators_redeemer_pair.2nd
              and {
                mint_script_hash == retired_operators_script_hash,
                fraudulent_operator == slashed_retired_operator_key,
              }
            }
            _ -> False
          }

        // Verify that fraud has been proved for the removed node or its predecessor:
        // (7) The transaction must include a reference input holding a
        //     `fraud_proof` token.
        expect Some(fraud_proof_ref_input) =
          list.at(self.reference_inputs, fraud_proof_ref_input_index)
        expect
          quantity_of(
            fraud_proof_ref_input.output.value,
            fraud_proof_script_hash,
            fraud_proof_asset_name,
          ) == 1

        // (8) Let `fraud_proof_block_hash` be the last 28 bytes of the 
        //     `fraud_proof` token's asset name.
        let fraud_proof_block_hash =
          bytearray.drop(
            fraud_proof_asset_name,
            bytearray.length(fraud_proof_asset_name) - 28,
          )

        // (9) One of the following must be true:
        or {
          // (a) `fraud_proof_block_hash` matches the `predecessor_node` key.
          Key(fraud_proof_block_hash) == predecessor_node_key,
          // (b) `fraud_proof_block_hash` matches the `removed_node` key, and
          //      the last node of `state_queue` is `removed_node`.
          //      This means that the fraudulent node is being removed
          //      and has no more children.
          and {
            fraud_proof_block_hash == fraudulent_operator,
            unordered.prove_is_last_node(policy_id, removed_node_input.output),
          },
        }
      }
    }
  }

  // TODO: Add redeemers for escape hatch blocks

  else(_) {
    fail
  }
}
