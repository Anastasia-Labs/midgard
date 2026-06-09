use aiken/builtin
use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list/nested as nll
use cardano/assets.{PolicyId}
use cardano/transaction.{OutputReference, Transaction}
use midgard/common/utils.{get_singleton_asset_with_policy, has_signed}
use midgard/fraud_proof_catalogue.{
  AddFraudProof, AddFraudProofStep, Datum, Init, ListStateTransition,
  LockCatalogue, Metadata, MintRedeemer, SpendRedeemer, finalize_linked_list,
  finalize_linked_list_root, first_step_id, fraud_proof_id_byte_count,
  fraud_proof_step_key,
}
use midgard/hub_oracle as hub

validator spend(
  genesis_admin: VerificationKeyHash,
  catalogue_mint_policy: PolicyId,
) {
  spend(
    _datum: Option<Datum>,
    redeemer: SpendRedeemer,
    own_outref: OutputReference,
    tx: Transaction,
  ) {
    when redeemer is {
      ListStateTransition ->
        // 1. Structural catalogue spends must be paired with a valid catalogue
        //    mint or burn under the catalogue policy.
        nll.spend_for_adding_or_removing_an_element(
          catalogue_mint_policy,
          tx.mint,
        )
      LockCatalogue { root_input_index, continued_root_output_index } -> {
        let Transaction { inputs, outputs, mint, extra_signatories, .. } = tx

        // 1. Catalogue lock must be authorized by genesis admin.
        expect genesis_admin |> has_signed(extra_signatories)

        // 2. Catalogue lock must spend and reproduce the root element only,
        //    changing root data from unlocked to locked.
        {
          let
            _root_address,
            _root_lovelace_change,
            _root_link,
            updated_element,
            _root_ref_script,
            _continued_root_ref_script,
          <-
            nll.spend_for_updating_elements_data(
              element_input_index: root_input_index,
              continued_element_output_index: continued_root_output_index,
              element_input_outref: own_outref,
              inputs: inputs,
              outputs: outputs,
              tx_mint: mint,
            )

          expect
            updated_element(
              fn(root_data, continued_root_data) {
                // 3. Root data must update from `False` to `True`.
                expect False = root_data
                expect continued_root_data

                // Done.
                True
              },
              fn(_, _, _, _) { False },
              fn(_, _, _) { False },
            )

          // Done.
          True
        }
          |> finalize_linked_list(catalogue_mint_policy)
      }
    }
  }

  else(_) {
    fail
  }
}

validator mint(
  genesis_admin: VerificationKeyHash,
  hub_oracle_script_hash: PolicyId,
) {
  mint(redeemer: MintRedeemer, own_policy_id: PolicyId, tx: Transaction) {
    when redeemer is {
      Init { output_index } -> {
        let Transaction { outputs, mint, extra_signatories, .. } = tx

        // 1. Catalogue operations must be authorized by genesis admin.
        expect genesis_admin |> has_signed(extra_signatories)

        // 2. Catalogue initialization must be authorized by minting the hub
        //    oracle NFT, and must authenticate the produced root UTxO.
        {
          let
            _root_address,
            _root_lovelace,
            root_data,
            root_ref_script,
          <-
            nll.init(
              nonce_validated: Pair(hub.asset_name, 1) == get_singleton_asset_with_policy(
                mint,
                hub_oracle_script_hash,
              ),
              produced_element_output: outputs |> list.expect_at(output_index),
              tx_mint: mint,
            )

          expect is_locked: Bool = root_data

          // 3. The root must be initialized as unlocked.
          expect False = is_locked

          // 4. Catalogue root must not carry a reference script.
          expect root_ref_script == None

          // Done.
          True
        }
          |> finalize_linked_list_root(own_policy_id)
      }
      AddFraudProof {
        fraud_proof_key,
        step_output_count,
        m_root_ref_input_index,
      } -> {
        let Transaction {
          inputs,
          outputs,
          reference_inputs,
          mint,
          extra_signatories,
          ..
        } = tx

        // 1. Catalogue operations must be authorized by genesis admin.
        expect genesis_admin |> has_signed(extra_signatories)

        // 2. Redeemer fraud proof key must match canonical key length.
        expect bytearray.length(fraud_proof_key) == fraud_proof_id_byte_count

        // 3. The first transaction output must be the continued outer-list
        //    anchor output. Composability is not a concern here.
        let continued_anchor_output = builtin.head_list(outputs)

        let outputs_after_anchor = builtin.tail_list(outputs)

        // 4. The first output after the continued anchor must be the new
        //    inner-root output.
        let fraud_proof_output = builtin.head_list(outputs_after_anchor)

        let step_outputs_tail = builtin.tail_list(outputs_after_anchor)

        let step_outputs =
          step_outputs_tail |> list.expect_take(step_output_count)

        let prepare_state =
          fn(
            _fraud_proof_lovelace,
            inner_root_fraud_proof_key,
            fraud_proof_data: Data,
            first_child_link,
            fraud_proof_ref_script,
          ) {
            // 5. Produced inner root key must match redeemer fraud proof key.
            expect inner_root_fraud_proof_key == fraud_proof_key

            // 6. Produced inner root data must declare the final step count.
            expect Metadata { step_count: total_step_count } = fraud_proof_data

            // 7. Initial step batch cannot exceed the declared final step
            //    count.
            expect step_output_count <= total_step_count

            // 8. AddFraudProof must include at least one produced step node.
            expect Some(first_child_key) = first_child_link

            let first_child_step_id =
              first_child_key
                |> bytearray.drop(fraud_proof_id_byte_count)
                |> bytearray.to_int_big_endian

            // 9. A partial initial batch must leave earlier steps to be
            //    prepended later.
            expect
              if total_step_count > step_output_count {
                first_child_step_id > first_step_id
              } else {
                first_child_step_id == first_step_id
              }

            // 10. Fraud proof inner roots must not carry reference scripts.
            expect fraud_proof_ref_script == None

            total_step_count
          }
        let node_validator =
          fn(
            total_step_count,
            _node_index,
            _node_address,
            _node_lovelace,
            node_key,
            _node_data,
            node_link,
            node_ref_script,
          ) {
            let current_step_id =
              node_key
                |> bytearray.drop(fraud_proof_id_byte_count)
                |> bytearray.to_int_big_endian

            // 11. Each produced step node key must be proof key followed by the
            //     encoded step ID.
            expect
              node_key == fraud_proof_step_key(fraud_proof_key, current_step_id)

            // 12. Step node links must increment by one and end at the declared
            //     final step.
            expect
              when node_link is {
                Some(next_node_key) ->
                  next_node_key == fraud_proof_step_key(
                    fraud_proof_key,
                    current_step_id + 1,
                  )
                None -> current_step_id == total_step_count - 1
              }

            // 13. Produced step nodes must carry their step reference script.
            expect Some(_node_ref_script_hash) = node_ref_script

            total_step_count
          }

        // 14. AddFraudProof must insert an inner list whose inner root is
        //     inserted into the outer list in ascending order.
        {
          let
            _anchor_input,
            _anchor_lovelace_change,
            m_anchor_key,
            anchor_data,
            continued_anchor_data,
            _old_outer_link,
            _total_step_count,
            other_list_assets,
          <-
            nll.insert_inner_list_ascending(
              ordering_of_inner_node_keys: nll.Ascending,
              continued_anchor_element_output: continued_anchor_output,
              new_inner_root_output: fraud_proof_output,
              inputs: inputs,
              inner_node_outputs: step_outputs,
              tx_mint: mint,
              prepare_state: prepare_state,
              node_validator: node_validator,
            )

          // 15. The continued outer-list anchor must preserve its data.
          expect anchor_data == continued_anchor_data

          // 16. No non-structural same-policy asset changes are allowed.
          expect dict.is_empty(other_list_assets)

          when m_anchor_key is {
            Some(_) ->
              // 17a. If the spent anchor is not root, the root UTxO must be
              //      provided in the reference inputs.
              {
                expect Some(root_ref_input_index) = m_root_ref_input_index
                let root_input =
                  reference_inputs |> list.expect_at(root_ref_input_index)
                let
                  _root_address,
                  _root_lovelace,
                  root_data,
                  _root_link,
                  _root_ref_script,
                <- nll.get_root_element_info(root_input.output)

                // 18a. The fraud proof list must not be locked.
                expect False = root_data

                // Done.
                True
              }
                |> finalize_linked_list_root(own_policy_id)
            None -> {
              // 17b. The fraud proof list must not be locked.
              expect False = anchor_data

              // Done.
              True
            }
          }
        }
          |> finalize_linked_list(own_policy_id)
      }
      AddFraudProofStep {
        fraud_proof_key,
        step_id,
        continued_anchor_output_index,
        step_output_index,
      } -> {
        let Transaction { inputs, outputs, mint, extra_signatories, .. } = tx

        // 1. Catalogue operations must be authorized by genesis admin.
        expect genesis_admin |> has_signed(extra_signatories)

        // 2. Redeemer fraud proof key must match canonical key length.
        expect bytearray.length(fraud_proof_key) == fraud_proof_id_byte_count

        // 3. Redeemer anchor output index must point to the continued inner
        //    list anchor output.
        let continued_anchor_output =
          outputs |> list.expect_at(continued_anchor_output_index)

        // 4. Redeemer step output index must point to the new step node output.
        let step_output = outputs |> list.expect_at(step_output_index)

        // 5. AddFraudProofStep must insert one step node into the requested
        //    fraud proof's inner list in ascending order.
        {
          let
            _anchor_input,
            _anchor_lovelace_change,
            anchor,
            _node_lovelace,
            node_key,
            _node_data,
            _node_link,
            node_ref_script,
            other_list_assets,
          <-
            nll.insert_into_inner_list(
              ordering_of_inner_node_keys: nll.Ascending,
              continued_anchor_element_output: continued_anchor_output,
              new_node_output: step_output,
              inputs: inputs,
              tx_mint: mint,
            )
          expect
            anchor(
              fn(
                anchor_key,
                anchor_data,
                continued_anchor_data,
                old_first_child_link,
              ) {
                // 6. An inner-root anchor must match the requested fraud proof
                //    key.
                expect anchor_key == fraud_proof_key

                // 7. A continued inner-root anchor must preserve its data.
                expect anchor_data == continued_anchor_data

                // 8. Step addition must prepend before an existing first
                //    child.
                expect Some(old_first_child_key) = old_first_child_link

                // 9. New step key must be exactly one less than the child it
                //     prepends to.
                expect
                  old_first_child_key == fraud_proof_step_key(
                    fraud_proof_key,
                    step_id + 1,
                  )

                // Done.
                True
              },
              fn(_, _, _) { False },
            )

          // 10. Produced step node must carry its step reference script.
          expect Some(_node_ref_script_hash) = node_ref_script

          // 11. Produced step node key must match proof key followed by the
          //     encoded step ID.
          expect node_key == fraud_proof_step_key(fraud_proof_key, step_id)

          // 12. No non-structural same-policy asset changes are allowed.
          expect dict.is_empty(other_list_assets)

          // Done.
          True
        }
          |> finalize_linked_list(own_policy_id)
      }
    }
  }

  else(_) {
    fail
  }
}
