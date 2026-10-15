use aiken/collection/list
use aiken/crypto.{ScriptHash}
use cardano/address.{Script, from_script}
use cardano/assets.{PolicyId, lovelace_of}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/common/utils
use midgard/computation_thread as ct
use midgard/dispute_bond_v1.{DisputeBondV1}
use midgard/fraud_proofs/common.{cancel}
use midgard/validation_case_v1
use midgard/validation_claim_v1
use midgard/validation_dispute_v1
use midgard/validation_game_v1
use midgard/validation_trace_v1

pub type Datum =
  ct.StepDatum<validation_game_v1.PendingValidationClaimV1>

pub type VerifySourceActionV1 {
  VerifySource {
    input_index: Int,
    /// The case output.
    output_index: Int,
    /// The challenger's claimed state at its step count S.
    challenger_terminal: validation_trace_v1.ValidationMachineStateV1,
    /// Leaf S of the challenger's trace root, holding that state's hash.
    challenger_terminal_proof: validation_trace_v1.ValidationTraceProof,
  }
}

pub type SpendRedeemer =
  ct.StepRedeemer<VerifySourceActionV1>

/// N5: the hash of the state every thread starts from.
///
/// SEAM (FW-R): until `validation_claim_v1.derive_initial_state` lands, the
/// claim's initial state must be exactly the one derivation builds
/// (`committed_initial_state_is_derived`), and its hash is the agreed one.
fn agreed_initial_hash(
  state: validation_game_v1.PendingValidationClaimV1,
) -> ByteArray {
  expect
    validation_claim_v1.committed_initial_state_is_derived(
      state.challenged_header,
      state.claim,
    )
  validation_trace_v1.hash_machine_state(state.claim.initial_state)
}

/// VerifySource, the second half of the opener. In order:
/// 1. the committed source is authenticated;
/// 2. R-DESC: only a normal source with redeemers opens a case;
/// 3. the challenger's claim is coherent: a Terminal Rejected state, proven as
///    leaf S of the challenger's trace root;
/// 4. N5: the agreed initial state;
/// 5. the open guard: the validity range is at most L_d long and its upper
///    bound is at most `block_end + M/2`;
/// 6. the bond: one output at `dispute_bond_v1` holding at least
///    `dispute_bond_lovelace`, bound to the case.
///
/// The opener's thread token moves into the case output, with the case NFT
/// minted under `case_v1` and named from the opener input.
validator main(
  case_v1_script_hash: ScriptHash,
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
        expect Some(ct.StepDatum { fraud_prover, data: Some(state) }) = datum
        expect
            Some(Input {
              output_reference: opener_out_ref,
              output: Output { value: opener_value, .. },
            })
          = list.at(inputs, input_index)
        expect opener_out_ref == own_out_ref
        let (opener_policy, thread_token_name, opener_quantity) = utils.get_single_asset_from_value_apart_from_ada(
          opener_value,
        )
        expect opener_policy == computation_thread_token_policy_id
        expect opener_quantity == 1
        // 1, 2.
        expect Some(witness_set) = validation_claim_v1.committed_claim_source_witness_set(
          state.challenged_header,
          state.claim,
        )
        expect
          validation_claim_v1.source_requires_trace(
            state.claim.source_membership,
            witness_set,
          )
        // 3.
        let terminal_hash = validation_trace_v1.hash_machine_state(
          challenger_terminal,
        )
        expect and {
            challenger_terminal.phase == validation_trace_v1.Terminal,
            challenger_terminal.verdict == validation_trace_v1.Rejected,
            challenger_terminal_proof.state_index == state.step_count,
            challenger_terminal_proof.state_hash == terminal_hash,
            validation_trace_v1.verify_trace_proof_against_root(
              state.challenger_root,
              state.step_count,
              challenger_terminal_proof,
            ),
          }
        // 4.
        let initial_hash = agreed_initial_hash(state)
        // 5.
        let (_, source_time_upper) = utils.get_inclusive_bounds_of_a_bounded_validity_range(
          validity_range,
          validation_dispute_v1.dispute_max_validity_range_milliseconds,
        )
        expect
          validation_dispute_v1.case_can_open(
            source_time_upper,
            state.block_end,
          )
        // 6.
        let event_key = state.claim.descriptor_membership.key
        let case_id = validation_case_v1.case_nft_name(
          state.challenged_header_hash,
          event_key,
          own_out_ref,
        )
        let bond: Data = DisputeBondV1 {
          challenged_header_hash: state.challenged_header_hash,
          fraud_prover,
          case_id,
        }
        expect [bond_output] = list.filter(
          outputs,
          fn(output) {
            output.address.payment_credential == Script(
              dispute_bond_validator_script_hash,
            )
          },
        )
        // The case.
        let case: Data = validation_case_v1.open_case(
          state.challenged_header_hash,
          event_key,
          state.challenger_root,
          state.step_count,
          terminal_hash,
          fraud_prover,
          initial_hash,
          source_time_upper,
        )
        expect
            Some(Output {
              address: case_address,
              value: case_value,
              datum: case_datum,
              reference_script: None,
            })
          = list.at(outputs, output_index)
        let case_lovelace = lovelace_of(case_value)
        and {
          lovelace_of(bond_output.value) >= dispute_bond_lovelace,
          bond_output.datum == InlineDatum(bond),
          case_address == from_script(case_v1_script_hash),
          case_datum == InlineDatum(case),
          assets.without_lovelace(case_value) == (
            assets.from_asset(
              computation_thread_token_policy_id,
              thread_token_name,
              1,
            )
              |> assets.add(case_v1_script_hash, case_id, 1)
          ),
          case_lovelace >= validation_case_v1.case_floor_lovelace,
          case_lovelace >= lovelace_of(opener_value),
        }
      }
    }
  }

  else(_) {
    fail
  }
}
