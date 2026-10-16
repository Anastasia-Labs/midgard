use aiken/collection/dict
use aiken/collection/list
use aiken/math
use cardano/address.{Script, from_verification_key}
use cardano/assets.{PolicyId, lovelace_of}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use env
use midgard/common/utils
use midgard/dispute_bond_v1.{DisputeBondV1}
use midgard/fraud_proof
use midgard/validation_case_config_v1
use midgard/validation_case_v1.{
  BurnThreadToken, CaseClose, CaseRedeemerV1, CaseV1, Convicted, Defend,
  ThreadClose, VerifySource,
}

/// The validation case (`midgard/validation_case_v1`), as a mint and a spend
/// purpose under one script hash. The mint purpose mints the case NFT at
/// VerifySource, and the thread tokens at Defend, and burns a thread token
/// held by a configured thread script at BurnThreadToken; the spend purpose
/// runs Defend, ThreadClose and CaseClose on a case UTxO.
///
/// A case UTxO holds the case NFT, the case's computation-thread token and
/// lovelace. Every other mint or burn under this policy, outside VerifySource
/// and BurnThreadToken, happens in a transaction that spends a case UTxO, and
/// that spend fixes the transaction's mint exactly. A convicted close locks the case NFT at the
/// fraud-proof address, whose spend always fails; every other close burns it.
validator main(
  hub_oracle: PolicyId,
  case_config_policy_id: PolicyId,
  computation_thread_token_policy_id: PolicyId,
  dispute_bond_lovelace: Int,
) {
  mint(redeemer: CaseRedeemerV1, policy_id: PolicyId, self: Transaction) {
    let Transaction { inputs, reference_inputs, mint, .. } = self
    when redeemer is {
      // The case NFT: one token, minted with an opener input of source-v1,
      // which checks its name, the case output and the bond.
      VerifySource { opener_input_index, .. } -> {
        let config = validation_case_config_v1.get_config(
          reference_inputs,
          case_config_policy_id,
        )
        expect Some(opener) = list.at(inputs, opener_input_index)
        expect [Pair(_, 1)] = dict.to_pairs(assets.tokens(mint, policy_id))
        opener.output.address.payment_credential == Script(config.source_v1)
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
        expect Some(bond_output) = list.at(outputs, bond_output_index)
        expect Some(remainder) = list.at(outputs, remainder_output_index)
        // The thread token burns at every close. The case NFT burns too,
        // except on a conviction: the fraud-proof token is minted exactly
        // against the thread token, so the case NFT is locked for ever in its
        // own output at the fraud-proof address instead. Both outputs are
        // funded from the case, each with at most the fraud-proof output's
        // lovelace (plus one move fee for the record).
        let thread_burn = assets.from_asset(
          computation_thread_token_policy_id,
          computation_thread_token_name,
          -1,
        )
        let burn = thread_burn |> assets.add(own_policy, case_id, -1)
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
              assets.without_lovelace(lock.value) == assets.from_asset(
                own_policy,
                case_id,
                1,
              ),
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
      Defend { .. } -> False
      ThreadClose { .. } -> False
      VerifySource { .. } -> False
      BurnThreadToken { .. } -> False
    }
  }

  else(_) {
    fail
  }
}
