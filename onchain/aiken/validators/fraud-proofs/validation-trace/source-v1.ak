use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/assets.{PolicyId, lovelace_of}
use cardano/transaction.{InlineDatum, OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/dispute_bond_v1.{DisputeBondV1}
use midgard/fraud_proofs/common.{Anyone, cancel, continue_validation_trace}
use midgard/ledger_state
use midgard/validation_claim_v1
use midgard/validation_dispute_v1
use midgard/validation_game_v1
use midgard/validation_resolution_v1
use midgard/validation_trace_v1

pub type Datum =
  ct.StepDatum<validation_game_v1.PendingValidationClaimV1>

pub type VerifySourceActionV1 {
  VerifySource { input_index: Int, output_index: Int }
}

pub type SpendRedeemer =
  ct.StepRedeemer<VerifySourceActionV1>

/// SEAM (FW-R): the state both sides agree the trace starts from, or `None`
/// when the operator's committed initial state is not the one derivation
/// builds (the operator's own trace is then wrong at its first state). Until
/// the machine's initial state is derived on chain it is the operator
/// descriptor's committed initial state hash; afterwards it is always
/// `Some(hash_machine_state(derive_initial_state(..)))`.
fn agreed_low_hash(
  state: validation_game_v1.PendingValidationClaimV1,
) -> Option<ByteArray> {
  if validation_claim_v1.committed_initial_state_is_derived(
    state.challenged_header,
    state.claim,
  ) {
    Some(state.claim.descriptor_membership.value.initial_state_hash)
  } else {
    None
  }
}

/// VerifySource, the second half of the opener. In order:
/// 1. the committed source must be authenticated, or there is no successor;
/// 2. check 3: a descriptor whose verdict contradicts its source goes to award;
/// 3. Gap B: the terminal state is present exactly when both descriptors share
///    the terminal hash (Open stores it unchecked; this is its only reader), and
///    must be the operator's; incoherent goes to award, coherent has no
///    successor;
/// 4. R-DESC: only a normal source with redeemers is disputed further;
/// 5. the game opens: inside the maturity horizon, with the bond locked at
///    `dispute_bond_v1`, the fee reserve added to the thread, and no other
///    thread opening in the same transaction.
validator main(
  game_validator_script_hash: ScriptHash,
  award_validator_script_hash: ScriptHash,
  dispute_bond_validator_script_hash: ScriptHash,
  dispute_bond_lovelace: Int,
  computation_thread_token_policy_id: PolicyId,
) {
  spend(
    datum: Option<Datum>,
    redeemer: SpendRedeemer,
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
      ct.Continue(VerifySource { input_index, output_index }) -> {
        let Transaction { inputs, outputs, validity_range, .. } = tx
        let (_, source_time_upper) = utils.get_inclusive_bounds_of_a_bounded_validity_range(
          validity_range,
          validation_dispute_v1.dispute_max_validity_range_milliseconds,
        )
        expect Some(step_datum) = datum
        continue_validation_trace(
          computation_thread_token_policy_id,
          step_datum,
          input_index,
          output_index,
          own_out_ref,
          tx,
          Anyone,
          fn(
            input_script_hash,
            _computation_thread_token_asset_name,
            fraud_prover,
            input_state,
            output_script_hash,
            output_state,
          ) {
            expect Some(state) = input_state
            let descriptor = state.claim.descriptor_membership.value
            expect Some(witness_set) = validation_claim_v1.committed_claim_source_witness_set(
              state.challenged_header,
              state.claim,
            )
            let award_state: Data = validation_resolution_v1.winning_resolution()
            let goes_to_award = and {
              output_script_hash == award_validator_script_hash,
              output_state == award_state,
            }
            if !validation_claim_v1.descriptor_matches_source_verdict(
              descriptor,
              state.claim.source_membership,
            ) {
              goes_to_award
            } else {
              when state.terminal_state is {
                Some(terminal_state) -> {
                  expect
                    descriptor.terminal_state_hash == state.challenger_descriptor.terminal_state_hash
                  expect
                    validation_trace_v1.hash_machine_state(terminal_state) == descriptor.terminal_state_hash
                  expect
                    !validation_claim_v1.terminal_is_coherent(
                      descriptor,
                      terminal_state,
                    )
                  goes_to_award
                }
                None -> {
                  expect
                    descriptor.terminal_state_hash != state.challenger_descriptor.terminal_state_hash
                  expect
                    validation_claim_v1.source_requires_trace(
                      state.claim.source_membership,
                      witness_set,
                    )
                  when agreed_low_hash(state) is {
                    None -> goes_to_award
                    Some(low_hash) -> {
                      let expected_state: Data = validation_game_v1.ValidationGameStateV1 {
                        challenged_header_hash: state.challenged_header_hash,
                        operator_vkey: state.challenged_header.operator_vkey,
                        dispute: validation_dispute_v1.open_after_source_verification(
                          descriptor,
                          state.challenger_descriptor,
                          low_hash,
                          state.open_time_upper,
                          source_time_upper,
                          state.challenged_header.end_time,
                          ledger_state.block_maturity_duration_v1,
                        ),
                      }
                      let bond: Data = DisputeBondV1 {
                        challenged_header_hash: state.challenged_header_hash,
                        fraud_prover,
                        forfeit_after: dispute_bond_v1.forfeit_after(
                          state.challenged_header.end_time,
                        ),
                      }
                      // One opening per transaction, so each game is backed
                      // by its own bond output.
                      expect [_] = list.filter(
                        inputs,
                        fn(input) {
                          input.output.address.payment_credential == Script(
                            input_script_hash,
                          )
                        },
                      )
                      expect [bond_output] = list.filter(
                        outputs,
                        fn(output) {
                          output.address.payment_credential == Script(
                            dispute_bond_validator_script_hash,
                          )
                        },
                      )
                      expect Some(thread_input) = list.at(inputs, input_index)
                      expect Some(thread_output) = list.at(
                        outputs,
                        output_index,
                      )
                      and {
                        output_script_hash == game_validator_script_hash,
                        output_state == expected_state,
                        lovelace_of(bond_output.value) >= dispute_bond_lovelace,
                        bond_output.datum == InlineDatum(bond),
                        lovelace_of(thread_output.value) >= lovelace_of(
                          thread_input.output.value,
                        ) + validation_dispute_v1.thread_fee_reserve_lovelace,
                      }
                    }
                  }
                }
              }
            }
          },
        )
      }
    }
  }

  else(_) {
    fail
  }
}
