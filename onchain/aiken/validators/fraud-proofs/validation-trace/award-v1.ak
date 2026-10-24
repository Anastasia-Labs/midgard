use aiken/builtin
use aiken/collection/dict
use aiken/collection/list
use aiken/collection/pairs
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use cardano/address.{Script}
use cardano/assets.{PolicyId}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, OutputReference, Redeemer, Transaction,
}
use midgard/hub_oracle as hub
use midgard/operator_directory.{
  ActiveOperatorsDirectory, SlashOperatorForLostDefense,
  operator_is_not_a_member,
}
use midgard/operator_directory/active_operators
use midgard/operator_directory/retired_operators
use midgard/validation_award_v1.{
  BurnReceipt, CloseAwardedThread, Datum, DefenderAlreadySlashed, MintReceipt,
  ReceiptRedeemerV1, SlashedForThisLoss, SpendReceipt, SpendRedeemer,
}
use midgard/validation_case_thread_v1
use midgard/validation_case_v1.{LostDefenseReceiptV1}
use midgard/validation_dispute_v1
use midgard/validation_resolution_v1
use midgard/validation_thread_expiry_v1

/// The slashing reason of the directory mint whose redeemer is
/// `directory_redeemer`. In both directories `SlashOperator` is constructor 4
/// with `slashing_arguments` first, and `slashing_reason` is the last of the
/// five fields of `SlashingArguments`. The directory policy decodes its own
/// redeemer in full, so only the reason is read here.
fn slashing_reason_of(directory_redeemer: Option<Redeemer>) -> Data {
  expect Some(redeemer) = directory_redeemer
  let Pair(tag, fields) = builtin.un_constr_data(redeemer)
  expect tag == 4
  expect [arguments, ..] = fields
  expect [_, _, _, _, reason] = builtin.un_constr_data(arguments).2nd
  reason
}

/// `defender` is in neither operator set: each reference input is an element
/// of its set whose key and link bracket `defender`.
fn in_neither_set(
  defender: VerificationKeyHash,
  hub_datum: hub.Datum,
  reference_inputs: List<Input>,
  active_index: Int,
  retired_index: Int,
) -> Bool {
  and {
    defender
      |> operator_is_not_a_member(reference_inputs, active_index)
      |> active_operators.finalize_linked_list(hub_datum.active_operators),
    defender
      |> operator_is_not_a_member(reference_inputs, retired_index)
      |> retired_operators.finalize_linked_list(hub_datum.retired_operators),
  }
}

/// A case thread whose one-step resolution certified the challenger's step,
/// and the policy of the lost-defense receipts a thread close mints. A receipt
/// stays in the output its thread close made until it is burned: by the
/// slash of its defender (`SlashOperatorForLostDefense`), or once its
/// defender is in neither operator set. An awarded thread its case can no
/// longer take is released to the thread expiry script; a receipt never is.
validator main(
  computation_thread_token_policy_id: PolicyId,
  hub_oracle_script_hash: PolicyId,
  thread_expiry_script_hash: ScriptHash,
) {
  mint(redeemer: ReceiptRedeemerV1, policy_id: PolicyId, tx: Transaction) {
    let Transaction { inputs, reference_inputs, mint, redeemers, .. } = tx
    when redeemer is {
      // The case spend fixes the whole mint, receipt included.
      MintReceipt { case_input_index } -> {
        expect Some(case_input) = list.at(inputs, case_input_index)
        case_input.output.address.payment_credential == Script(
          computation_thread_token_policy_id,
        )
      }
      BurnReceipt {
        receipt_input_index,
        hub_oracle_ref_input_index,
        permission,
      } -> {
        expect
            Some(Input {
              output: Output { value, datum: InlineDatum(receipt_data), .. },
              ..
            })
          = list.at(inputs, receipt_input_index)
        expect receipt: LostDefenseReceiptV1 = receipt_data
        let name = validation_case_v1.receipt_token_name(
          receipt.case_id,
          receipt.defender_vkh,
        )
        let hub_datum = hub.get_datum(
          reference_inputs,
          hub_oracle_script_hash,
          hub_oracle_ref_input_index,
        )
        and {
          assets.quantity_of(value, policy_id, name) == 1,
          dict.to_pairs(assets.tokens(mint, policy_id)) == [Pair(name, -1)],
          when permission is {
            // The directory checks that the receipt names the slashed
            // operator, and pays the reward.
            SlashedForThisLoss { directory } -> {
              let reason: Data = SlashOperatorForLostDefense {
                receipt_index: receipt_input_index,
              }
              slashing_reason_of(
                pairs.get_first(
                  redeemers,
                  Mint(
                    if directory == ActiveOperatorsDirectory {
                      hub_datum.active_operators
                    } else {
                      hub_datum.retired_operators
                    },
                  ),
                ),
              ) == reason
            }
            DefenderAlreadySlashed {
              active_operators_element_ref_input_index,
              retired_operators_element_ref_input_index,
            } ->
              in_neither_set(
                receipt.defender_vkh,
                hub_datum,
                reference_inputs,
                active_operators_element_ref_input_index,
                retired_operators_element_ref_input_index,
              )
          },
        }
      }
    }
  }

  spend(
    datum: Option<Data>,
    redeemer: SpendRedeemer,
    own_out_ref: OutputReference,
    tx: Transaction,
  ) {
    or {
      validation_thread_expiry_v1.holder_releases_to_expiry(
        computation_thread_token_policy_id,
        thread_expiry_script_hash,
        own_out_ref,
        tx,
      ),
      award_spend(
        computation_thread_token_policy_id,
        datum,
        redeemer,
        own_out_ref,
        tx,
      ),
    }
  }

  else(_) {
    fail
  }
}

fn award_spend(
  computation_thread_token_policy_id: PolicyId,
  datum: Option<Data>,
  redeemer: SpendRedeemer,
  own_out_ref: OutputReference,
  tx: Transaction,
) -> Bool {
  when redeemer is {
    CloseAwardedThread { input_index, case_input_index } -> {
      expect Some(data) = datum
      expect step_datum: Datum = data
      expect Some(state) = step_datum.data
      and {
        validation_resolution_v1.winning_resolution_is_well_formed(state),
        validation_case_thread_v1.closes_into_case(
          computation_thread_token_policy_id,
          input_index,
          case_input_index,
          own_out_ref,
          validation_dispute_v1.ChallengerWins,
          tx,
        ),
      }
    }
    // The receipt's own token is burned, so the receipt policy's
    // BurnReceipt runs.
    SpendReceipt -> {
      expect Some(Input { output: Output { address, value, .. }, .. }) = transaction.find_input(
        tx.inputs,
        own_out_ref,
      )
      expect Script(own_policy) = address.payment_credential
      expect [Pair(name, 1)] = dict.to_pairs(assets.tokens(value, own_policy))
      assets.quantity_of(tx.mint, own_policy, name) == -1
    }
  }
}
