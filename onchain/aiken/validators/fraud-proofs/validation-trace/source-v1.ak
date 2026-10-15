use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Script}
use cardano/assets.{PolicyId, lovelace_of}
use cardano/transaction.{InlineDatum, OutputReference, Transaction}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/dispute_bond_v1.{DisputeBondV1}
use midgard/fraud_proofs/common.{Anyone, cancel, continue_validation_trace}
use midgard/validation_claim_v1
use midgard/validation_dispute_v1
use midgard/validation_game_v1
use midgard/validation_resolution_v1
use midgard/validation_trace_v1

pub type Datum =
  ct.StepDatum<validation_game_v1.PendingValidationClaimV1>

pub type VerifySourceActionV1 {
  VerifySource {
    input_index: Int,
    output_index: Int,
    /// The challenger's claimed state at its step count S.
    challenger_terminal: validation_trace_v1.ValidationMachineStateV1,
    /// Leaf S of the challenger's trace root, holding that state's hash.
    challenger_terminal_proof: validation_trace_v1.ValidationTraceProof,
  }
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

/// The challenger's claim is coherent: a Terminal Rejected state, proven as
/// leaf S of the challenger's trace root, whose hash is the challenger's
/// committed terminal hash.
fn claim_is_coherent(
  challenger: validation_trace_v1.ValidationTraceDescriptorV1,
  terminal: validation_trace_v1.ValidationMachineStateV1,
  proof: validation_trace_v1.ValidationTraceProof,
) -> Bool {
  let terminal_hash = validation_trace_v1.hash_machine_state(terminal)
  and {
    terminal.phase == validation_trace_v1.Terminal,
    terminal.verdict == validation_trace_v1.Rejected,
    proof.state_index == challenger.step_count,
    proof.state_hash == terminal_hash,
    challenger.terminal_state_hash == terminal_hash,
    validation_trace_v1.verify_trace_proof(challenger, proof),
  }
}

/// VerifySource, the second half of the opener. In order:
/// 1. the committed source must be authenticated, or there is no successor;
/// 2. R-DESC: only a normal source with redeemers is disputed further;
/// 3. the challenger's claim is coherent, and its terminal hash is not the
///    operator's;
/// 4. N5: the agreed initial state, or award when the operator's committed
///    initial state is not the derived one;
/// 5. the open guard: the validity upper bound is at most `block_end + M/2`;
/// 6. the game opens with the bond locked at `dispute_bond_v1`, and no other
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
      ct.Continue(VerifySource {
        input_index,
        output_index,
        challenger_terminal,
        challenger_terminal_proof,
      }) -> {
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
            let challenger = state.challenger_descriptor
            expect Some(witness_set) = validation_claim_v1.committed_claim_source_witness_set(
              state.challenged_header,
              state.claim,
            )
            expect
              validation_claim_v1.source_requires_trace(
                state.claim.source_membership,
                witness_set,
              )
            expect
              claim_is_coherent(
                challenger,
                challenger_terminal,
                challenger_terminal_proof,
              )
            expect
              descriptor.terminal_state_hash != challenger.terminal_state_hash
            when agreed_low_hash(state) is {
              None -> {
                let award_state: Data = validation_resolution_v1.winning_resolution()
                and {
                  output_script_hash == award_validator_script_hash,
                  output_state == award_state,
                }
              }
              Some(low_hash) -> {
                expect
                  validation_dispute_v1.case_can_open(
                    source_time_upper,
                    state.block_end,
                  )
                let expected_state: Data = validation_game_v1.ValidationGameStateV1 {
                  challenged_header_hash: state.challenged_header_hash,
                  dispute: validation_dispute_v1.open(
                    descriptor,
                    challenger,
                    low_hash,
                    source_time_upper,
                  ),
                }
                let bond: Data = DisputeBondV1 {
                  challenged_header_hash: state.challenged_header_hash,
                  fraud_prover,
                  forfeit_after: dispute_bond_v1.forfeit_after(
                    state.challenged_header.end_time,
                  ),
                }
                // One opening per transaction, so each game is backed by its
                // own bond output.
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
                and {
                  output_script_hash == game_validator_script_hash,
                  output_state == expected_state,
                  lovelace_of(bond_output.value) >= dispute_bond_lovelace,
                  bond_output.datum == InlineDatum(bond),
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
