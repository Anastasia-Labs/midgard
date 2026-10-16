use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{VerificationKeyHash}
use aiken/math
use aiken/primitive/bytearray
use aiken_design_patterns/linked_list
use cardano/address.{Script, from_script, from_verification_key}
use cardano/assets.{PolicyId, Value, lovelace_of}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use env
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/dispute_bond_v1.{DisputeBondV1}
use midgard/fraud_proof
use midgard/operator_directory/active_operators.{NodeData}
use midgard/validation_case_config_v1
use midgard/validation_case_v1.{
  BurnThreadToken, CaseClose, CaseRedeemerV1, CaseV1, Convicted, Defend,
  ThreadClose, VerifySource,
}
use midgard/validation_dispute_v1

/// The validation case (`midgard/validation_case_v1`), as a mint and a spend
/// purpose under one script hash. The mint purpose mints the case NFT and the
/// bond marker at VerifySource and the thread tokens at Defend, and burns a
/// thread token held by a configured thread script at BurnThreadToken; the
/// spend purpose runs Defend, ThreadClose and CaseClose on a case UTxO.
///
/// A case UTxO holds the case NFT, the case's computation-thread token and
/// lovelace. Every other mint or burn under this policy, outside VerifySource
/// and BurnThreadToken, happens in a transaction that spends a case UTxO, and
/// that spend fixes the transaction's mint exactly. A convicted close locks
/// the case NFT and the bond marker at the fraud-proof address, whose spend
/// always fails; every other close burns them.
validator main(
  hub_oracle: PolicyId,
  case_config_policy_id: PolicyId,
  computation_thread_token_policy_id: PolicyId,
  dispute_bond_lovelace: Int,
) {
  mint(redeemer: CaseRedeemerV1, policy_id: PolicyId, self: Transaction) {
    let Transaction { inputs, reference_inputs, mint, .. } = self
    when redeemer is {
      // The case NFT and its bond marker: two tokens, minted with an opener
      // input of source-v1, which checks the case NFT's name, the case output
      // and the bond output. The marker is named from the other token here,
      // so no marker is ever minted ahead of its case.
      VerifySource { opener_input_index, .. } -> {
        let config = validation_case_config_v1.get_config(
          reference_inputs,
          case_config_policy_id,
        )
        expect Some(opener) = list.at(inputs, opener_input_index)
        expect [Pair(first, 1),
              Pair(second, 1)] = dict.to_pairs(assets.tokens(mint, policy_id))
        and {
          opener.output.address.payment_credential == Script(config.source_v1),
          or {
            second == validation_case_v1.bond_marker_name(first),
            first == validation_case_v1.bond_marker_name(second),
          },
        }
      }
      // One thread token, burned with no case input: its holder is one of the
      // configured thread scripts, whose spend decides the timing and payee.
      BurnThreadToken { thread_input_index } -> {
        let config = validation_case_config_v1.get_config(
          reference_inputs,
          case_config_policy_id,
        )
        expect Some(thread_input) = list.at(inputs, thread_input_index)
        expect [Pair(thread_token_name, -1)] = dict.to_pairs(
          assets.tokens(mint, policy_id),
        )
        expect Script(holder) = thread_input.output.address.payment_credential
        and {
          assets.quantity_of(
            thread_input.output.value,
            policy_id,
            thread_token_name,
          ) == 1,
          or {
            holder == config.game_v1,
            holder == config.boundary_v1,
            holder == config.timeout_v1,
            holder == config.award_v1,
          },
        }
      }
      _ ->
        list.any(
          inputs,
          fn(input) {
            input.output.address.payment_credential == Script(policy_id)
          },
        )
    }
  }

  spend(
    datum: Option<CaseV1>,
    redeemer: CaseRedeemerV1,
    own_out_ref: OutputReference,
    self: Transaction,
  ) {
    expect Some(case) = datum
    let
        Transaction {
          inputs,
          reference_inputs,
          outputs,
          fee,
          mint,
          validity_range,
          extra_signatories,
          ..
        }
      = self
    expect
        Some(Input {
          output: Output { address: own_address, value: own_value, .. },
          ..
        })
      = transaction.find_input(inputs, own_out_ref)
    expect Script(own_policy) = own_address.payment_credential
    expect [Pair(case_id, 1)] = dict.to_pairs(
      assets.tokens(own_value, own_policy),
    )
    expect [Pair(computation_thread_token_name, 1)] = dict.to_pairs(
      assets.tokens(own_value, computation_thread_token_policy_id),
    )
    when redeemer is {
      CaseClose { bond_input_index, bond_output_index, remainder_output_index } -> {
        let config = validation_case_config_v1.get_config(
          reference_inputs,
          case_config_policy_id,
        )
        expect
          validation_case_v1.case_can_close(
            case,
            utils.get_inclusive_lower_bound_of_interval(validity_range),
          )
        expect Some(bond_input) = list.at(inputs, bond_input_index)
        let
            Input {
              output_reference: bond_out_ref,
              output: Output {
                address: bond_address,
                value: bond_value,
                datum: bond_datum,
                ..
              },
            }
          = bond_input
        expect InlineDatum(bond_data) = bond_datum
        expect bond: DisputeBondV1 = bond_data
        let bond_lovelace = lovelace_of(bond_value)
        let bond_marker = validation_case_v1.bond_marker_name(case_id)
        expect Some(bond_output) = list.at(outputs, bond_output_index)
        expect Some(remainder) = list.at(outputs, remainder_output_index)
        // The thread token burns at every close. The case NFT and the bond
        // marker burn too, except on a conviction: the fraud-proof token is
        // minted exactly against the thread token, so the case NFT and the
        // marker are locked for ever in their own output at the fraud-proof
        // address instead. Both outputs are funded from the case, each with
        // at most the fraud-proof output's lovelace (plus one move fee for the
        // record).
        let thread_burn = assets.from_asset(
          computation_thread_token_policy_id,
          computation_thread_token_name,
          -1,
        )
        let case_tokens = assets.from_asset(own_policy, case_id, 1)
          |> assets.add(own_policy, bond_marker, 1)
        let burn = assets.merge(thread_burn, assets.negate(case_tokens))
        let fraud_proof_lovelace = if validation_case_v1.case_outcome(case) == Convicted {
          let (fraud_proof_policy, fraud_proof_address) = validation_case_config_v1.hub_fraud_proof(
            reference_inputs,
            hub_oracle,
          )
          let fraud_proof_token = assets.from_asset(
            fraud_proof_policy,
            computation_thread_token_name,
            1,
          )
          expect Some(fraud_proof_output) = list.find(
            outputs,
            fn(output) {
              assets.quantity_of(
                output.value,
                fraud_proof_policy,
                computation_thread_token_name,
              ) == 1
            },
          )
          let record: Data = fraud_proof.Datum {
            fraud_prover: case.bounty_claimant,
          }
          let paid = lovelace_of(fraud_proof_output.value)
          expect Some(lock) = list.find(
            outputs,
            fn(output) {
              assets.quantity_of(output.value, own_policy, case_id) == 1
            },
          )
          let locked = lovelace_of(lock.value)
          expect and {
              lock.address == fraud_proof_address,
              locked <= env.fraud_proof_output_lovelace,
              assets.without_lovelace(lock.value) == case_tokens,
              mint == assets.merge(thread_burn, fraud_proof_token),
              fraud_proof_output.address == fraud_proof_address,
              assets.without_lovelace(fraud_proof_output.value) == fraud_proof_token,
              fraud_proof_output.datum == InlineDatum(record),
              paid >= env.fraud_proof_output_lovelace,
              paid <= env.fraud_proof_output_lovelace + env.max_dispute_move_fee_lovelace,
            }
          paid + locked
        } else {
          expect mint == burn
          0
        }
        // The case lovelace may pay the fee only when the case and its bond
        // are the only inputs.
        let fee_allowance = when inputs is {
          [_, _] -> math.min(fee, env.max_dispute_move_fee_lovelace)
          _ -> 0
        }
        let bond_tag: Data = bond_out_ref
        let case_tag: Data = own_out_ref
        and {
          bond_address.payment_credential == Script(config.dispute_bond_v1),
          assets.quantity_of(bond_value, own_policy, bond_marker) == 1,
          bond.case_id == case_id,
          bond_lovelace >= dispute_bond_lovelace,
          bond_output.address == validation_case_v1.challenger_bond_address(
            case,
          ),
          bond_output.datum == InlineDatum(bond_tag),
          lovelace_of(bond_output.value) >= bond_lovelace,
          remainder.address == from_verification_key(case.bounty_claimant),
          remainder.datum == InlineDatum(case_tag),
          lovelace_of(remainder.value) >= lovelace_of(own_value) - fraud_proof_lovelace - fee_allowance,
        }
      }
      // One defense: the case continues with the defender added and one
      // more thread open, the thread starts at game-v1 with its reserve, and
      // the defender's active node holds its bond until the case is over.
      Defend {
        case_output_index,
        thread_output_index,
        active_node_input_index,
        defender_vkh,
        defenders_proof,
        defender_root,
        defender_state,
        defender_state_proof,
      } -> {
        let config = validation_case_config_v1.get_config(
          reference_inputs,
          case_config_policy_id,
        )
        expect
          validation_case_v1.defense_is_valid(
            case,
            defender_root,
            defender_state,
            defender_state_proof,
          )
        expect Some(defended) = validation_case_v1.defend(
          case,
          defender_vkh,
          defenders_proof,
        )
        let (defense_time_lower, defense_time_upper) = utils.get_inclusive_bounds_of_a_bounded_validity_range(
          validity_range,
          validation_dispute_v1.dispute_max_validity_range_milliseconds,
        )
        // The thread's datum at game-v1: the challenger is its fraud prover,
        // the one key the resolution moves need.
        let thread: Data = ct.StepDatum {
          fraud_prover: case.bounty_claimant,
          data: Some(
            validation_case_v1.open_thread(
              case,
              case_id,
              defender_vkh,
              defender_root,
              defender_state_proof.state_hash,
              defense_time_lower,
              defense_time_upper,
            ),
          ),
        }
        let thread_token = assets.from_asset(
          own_policy,
          validation_case_v1.thread_token_name(case_id, defender_vkh),
          1,
        )
        expect Some(case_output) = list.at(outputs, case_output_index)
        expect Some(thread_output) = list.at(outputs, thread_output_index)
        let defended_data: Data = defended
        and {
          mint == thread_token,
          list.has(extra_signatories, defender_vkh),
          defender_bond_is_held(
            defended,
            defender_vkh,
            active_node_input_index,
            hub_oracle,
            inputs,
            reference_inputs,
            outputs,
          ),
          case_output.address == own_address,
          case_output.datum == InlineDatum(defended_data),
          assets.without_lovelace(case_output.value) == assets.without_lovelace(
            own_value,
          ),
          lovelace_of(case_output.value) >= lovelace_of(own_value),
          thread_output.address == from_script(config.game_v1),
          thread_output.datum == InlineDatum(thread),
          assets.without_lovelace(thread_output.value) == thread_token,
          lovelace_of(thread_output.value) >= validation_case_v1.thread_floor_lovelace,
        }
      }
      // One thread closes into the case. The thread's own script fixes the
      // outcome and binds it to this redeemer; this spend checks the rest.
      ThreadClose {
        case_output_index,
        thread_input_index,
        outcome,
        defender_vkh,
      } -> {
        let config = validation_case_config_v1.get_config(
          reference_inputs,
          case_config_policy_id,
        )
        expect
            Some(Input {
              output_reference: thread_out_ref,
              output: Output {
                address: thread_address,
                value: thread_value,
                ..
              },
            })
          = list.at(inputs, thread_input_index)
        expect Script(holder) = thread_address.payment_credential
        let thread_name = validation_case_v1.thread_token_name(
          case_id,
          defender_vkh,
        )
        let thread_burn = assets.from_asset(own_policy, thread_name, -1)
        // The receipt, when the defender lost: at the award validator, whose
        // hash is the receipt policy, funded from the thread within
        // `[fpol, fpol + cap]`.
        let receipt_lovelace = when
          validation_case_v1.lost_defense_receipt(
            case,
            case_id,
            defender_vkh,
            outcome,
          )
        is {
          None -> {
            expect mint == thread_burn
            0
          }
          Some(receipt) -> {
            let receipt_token = assets.from_asset(
              config.award_v1,
              validation_case_v1.receipt_token_name(case_id, defender_vkh),
              1,
            )
            expect Some(receipt_output) = list.find(
              outputs,
              fn(output) {
                assets.without_lovelace(output.value) == receipt_token
              },
            )
            let receipt_data: Data = receipt
            let paid = lovelace_of(receipt_output.value)
            expect and {
                mint == assets.merge(thread_burn, receipt_token),
                receipt_output.address == from_script(config.award_v1),
                receipt_output.datum == InlineDatum(receipt_data),
                paid >= env.fraud_proof_output_lovelace,
                paid <= env.fraud_proof_output_lovelace + env.max_dispute_move_fee_lovelace,
              }
            paid
          }
        }
        // The thread may pay the fee only when the case and the thread are
        // the only inputs.
        let fee_allowance = when inputs is {
          [_, _] -> math.min(fee, env.max_dispute_move_fee_lovelace)
          _ -> 0
        }
        let leftover_address = validation_case_v1.thread_leftover_address(
          case,
          defender_vkh,
          outcome,
        )
        let leftover = lovelace_of(thread_value) - receipt_lovelace - fee_allowance
        let thread_tag: Data = thread_out_ref
        expect Some(case_output) = list.at(outputs, case_output_index)
        let closed: Data = validation_case_v1.close_thread(case, outcome)
        and {
          or {
            holder == config.timeout_v1,
            holder == config.award_v1,
          },
          assets.quantity_of(thread_value, own_policy, thread_name) == 1,
          case_output.address == own_address,
          case_output.datum == InlineDatum(closed),
          assets.without_lovelace(case_output.value) == assets.without_lovelace(
            own_value,
          ),
          lovelace_of(case_output.value) >= lovelace_of(own_value),
          list.any(
            outputs,
            fn(output) {
              and {
                output.address == leftover_address,
                output.datum == InlineDatum(thread_tag),
                lovelace_of(output.value) >= leftover,
              }
            },
          ),
        }
      }
      VerifySource { .. } -> False
      BurnThreadToken { .. } -> False
    }
  }

  else(_) {
    fail
  }
}

/// The defender's active node, spent at `active_node_input_index` and
/// continued with its bond hold raised to `bond_hold_after_defense`. The node
/// is found by its active-set NFT, under the policy the hub names; the
/// active-operators validator checks the rest of the continuation.
fn defender_bond_is_held(
  case: CaseV1,
  defender_vkh: VerificationKeyHash,
  active_node_input_index: Int,
  hub_oracle: PolicyId,
  inputs: List<Input>,
  reference_inputs: List<Input>,
  outputs: List<Output>,
) -> Bool {
  let active_policy = validation_case_config_v1.hub_active_operators(
    reference_inputs,
    hub_oracle,
  )
  let node_name = bytearray.concat(
    active_operators.node_asset_name_prefix,
    defender_vkh,
  )
  let holds_node = fn(value: Value) {
    assets.quantity_of(value, active_policy, node_name) == 1
  }
  expect Some(Input { output: node_input, .. }) = list.at(
    inputs,
    active_node_input_index,
  )
  expect Some(node_output) = list.find(
    outputs,
    fn(output) { holds_node(output.value) },
  )
  let old_bond_unlock_time = node_bond_unlock_time(node_input)
  and {
    holds_node(node_input.value),
    node_bond_unlock_time(node_output) == Some(
      validation_case_v1.bond_hold_after_defense(old_bond_unlock_time, case),
    ),
  }
}

fn node_bond_unlock_time(node: Output) -> Option<Int> {
  expect InlineDatum(datum) = node.datum
  expect element: active_operators.Datum = datum
  expect linked_list.Node{data: NodeData { bond_unlock_time, .. }} = element.data
  bond_unlock_time
}
