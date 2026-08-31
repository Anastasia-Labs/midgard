//// Deployment-bound computation-thread claim registry.
////
//// This module owns only the pure MPF transition. The singleton token,
//// transaction topology and computation-thread/fraud-token coupling live at
//// the validator boundary; keeping them out of this module lets the complete
//// 64-step mutation be measured before a policy ABI depends on it.

use aiken/collection/list
use aiken/merkle_patricia_forestry as mpf
use aiken/merkle_patricia_forestry/merkling.{null_hash}
use aiken/primitive/bytearray
use cardano/address.{Address}
use cardano/assets.{AssetName, PolicyId, quantity_of}
use cardano/transaction.{InlineDatum, Input, Output}
use env
use midgard/mpf_chunked_proof_v1
use midgard/mpf_proof_v1

/// Four-byte fraud-category id followed by the 28-byte header hash. The
/// registry singleton supplies the deployment-identity component of F04's
/// `(deployment, category, header)` identity.
pub const claim_id_byte_count: Int = 32

/// Third hub-policy singleton, held only at the dedicated claim-registry
/// spending validator.
pub const asset_name: AssetName = "MIDGARD_CLAIM_REGISTRY"

/// Domain-separated values committed below a claim key. They are deliberately
/// not constructor encodings: the exact bytes are stable across datum-schema
/// refactors and MPF hashes them before committing them into a leaf.
pub const live_claim_value: ByteArray = "MIDGARD_CLAIM_LIVE_V1"

pub const closed_claim_value: ByteArray = "MIDGARD_CLAIM_CLOSED_V1"

/// The canonical Midgard empty-MPF sentinel. `mpf_proof_v1.insert_root`
/// translates it to the forestry library's internal null root and every empty
/// registry is therefore represented identically to the other Midgard MPFs.
pub const empty_claims_root: ByteArray = env.empty_merkle_tree_root

/// Small proofs travel directly in the registry redeemer. A proof that would
/// exhaust the transaction envelope is published in the existing bounded MPF
/// proof-chunk UTxOs and named by reference-input index.
pub type MutationProof {
  RedeemerCarried(mpf.Proof)
  PublishedChunks { ordered_chunk_reference_input_indices: List<Int> }
}

pub type Mutation {
  /// Non-membership in the old root and exact insertion of `Live`.
  OpenClaim { claim_id: ByteArray, proof: MutationProof }
  /// Membership at `Live` and exact deletion, reopening the claim.
  CancelClaim { claim_id: ByteArray, proof: MutationProof }
  /// Membership at `Live` and exact update to the permanent `Closed` value.
  CloseClaim { claim_id: ByteArray, proof: MutationProof }
}

pub type Datum {
  claims_root: ByteArray,
  computation_thread_policy_id: PolicyId,
}

pub type Redeemer {
  Mutate { mutation: Mutation }
  Deinit { hub_oracle_input_index: Int }
}

pub fn mutation_claim_id(mutation: Mutation) -> ByteArray {
  when mutation is {
    OpenClaim { claim_id, .. } -> claim_id
    CancelClaim { claim_id, .. } -> claim_id
    CloseClaim { claim_id, .. } -> claim_id
  }
}

pub fn output_has_token(output: Output, hub_oracle_policy_id: PolicyId) -> Bool {
  quantity_of(output.value, hub_oracle_policy_id, asset_name) == 1
}

pub fn output_is_dedicated(
  output: Output,
  hub_oracle_policy_id: PolicyId,
  claim_registry_address: Address,
) -> Bool {
  let Output { address, value, reference_script, .. } = output
  and {
    address == claim_registry_address,
    output_has_token(output, hub_oracle_policy_id),
    assets.without_lovelace(value) == assets.from_asset(
      hub_oracle_policy_id,
      asset_name,
      1,
    ),
    reference_script == None,
  }
}

pub fn decode_datum(output: Output) -> Datum {
  expect InlineDatum(datum_data) = output.datum
  expect datum: Datum = datum_data
  datum
}

pub fn unique_input(
  inputs: List<Input>,
  hub_oracle_policy_id: PolicyId,
  claim_registry_address: Address,
) -> Input {
  expect [registry_input] = inputs
    |> list.filter(
        fn(Input { output, .. }) {
          output_has_token(output, hub_oracle_policy_id)
        },
      )
  expect
    output_is_dedicated(
      registry_input.output,
      hub_oracle_policy_id,
      claim_registry_address,
    )
  registry_input
}

pub fn unique_output(
  outputs: List<Output>,
  hub_oracle_policy_id: PolicyId,
  claim_registry_address: Address,
) -> Output {
  expect [registry_output] = outputs
    |> list.filter(
        fn(output) { output_has_token(output, hub_oracle_policy_id) },
      )
  expect
    output_is_dedicated(
      registry_output,
      hub_oracle_policy_id,
      claim_registry_address,
    )
  registry_output
}

pub fn has_no_output(
  outputs: List<Output>,
  hub_oracle_policy_id: PolicyId,
) -> Bool {
  list.length(
    outputs
      |> list.filter(
          fn(output) { output_has_token(output, hub_oracle_policy_id) },
        ),
  ) == 0
}

pub fn claim_id_is_well_formed(claim_id: ByteArray) -> Bool {
  bytearray.length(claim_id) == claim_id_byte_count
}

fn resolve_proof(
  proof: MutationProof,
  reference_inputs: List<Input>,
) -> Option<mpf.Proof> {
  when proof is {
    // The selected mutation helper performs the complete strict path/step
    // validation. Do not walk the same list just to count it here.
    RedeemerCarried(steps) -> Some(steps)
    PublishedChunks { ordered_chunk_reference_input_indices } ->
      if mpf_chunked_proof_v1.chunk_indices_are_well_formed(
        ordered_chunk_reference_input_indices,
        list.length(reference_inputs),
      ) {
        when
          mpf_chunked_proof_v1.concatenate_published_steps(
            reference_inputs,
            ordered_chunk_reference_input_indices,
          )
        is {
          None -> None
          // As on the direct route, the paired-fold mutation is the single
          // authority for the protocol-wide 64-step/path bound.
          Some(steps) -> Some(steps)
        }
      } else {
        None
      }
  }
}

/// Computes the one exact successor root or fails closed. The paired-fold
/// helpers validate every untrusted step once while deriving predecessor and
/// successor accumulators together; registry mutation therefore stays atomic
/// at maximum proof depth without a provisional lock state.
pub fn successor_root(
  previous_root: ByteArray,
  mutation: Mutation,
  reference_inputs: List<Input>,
) -> Option<ByteArray> {
  // Root width is enforced inside every paired-fold helper. Keeping a second
  // check here would add another guard without strengthening the transition.
  when mutation is {
    OpenClaim { claim_id, proof } ->
      when resolve_proof(proof, reference_inputs) is {
        None -> None
        Some(steps) ->
          if claim_id_is_well_formed(claim_id) {
            mpf_proof_v1.insert_root_paired_fold(
              previous_root,
              claim_id,
              live_claim_value,
              steps,
            )
          } else {
            None
          }
      }
    CancelClaim { claim_id, proof } ->
      when resolve_proof(proof, reference_inputs) is {
        None -> None
        Some(steps) ->
          if claim_id_is_well_formed(claim_id) {
            when
              mpf_proof_v1.delete_root_paired_fold(
                previous_root,
                claim_id,
                live_claim_value,
                steps,
              )
            is {
              None -> None
              Some(successor) ->
                // Forestry returns its internal all-zero root when the final
                // leaf is deleted. Commit the one canonical Midgard empty
                // sentinel instead, matching every initial registry datum.
                if successor == null_hash {
                  Some(empty_claims_root)
                } else {
                  Some(successor)
                }
            }
          } else {
            None
          }
      }
    CloseClaim { claim_id, proof } ->
      when resolve_proof(proof, reference_inputs) is {
        None -> None
        Some(steps) ->
          if claim_id_is_well_formed(claim_id) {
            mpf_proof_v1.update_root_paired_fold(
              previous_root,
              claim_id,
              live_claim_value,
              closed_claim_value,
              steps,
            )
          } else {
            None
          }
      }
  }
}
