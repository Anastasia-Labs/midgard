use aiken/builtin
use aiken/cbor
use aiken/collection/dict.{Dict}
use aiken/collection/list
use aiken/crypto.{
  ScriptHash, VerificationKey, VerificationKeyHash, blake2b_224, blake2b_256,
}
use aiken/interval.{Finite, Interval, NegativeInfinity, PositiveInfinity}
use aiken/merkle_patricia_forestry as mpf
use aiken_design_patterns/merkelized_validator
use cardano/address.{Address, PaymentCredential, Script, StakeCredential}
use cardano/assets.{AssetName, PolicyId, Value, tokens}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Redeemer, ScriptPurpose,
}
use env
use midgard/common/constants.{posix_time_none}
use midgard/common/types.{H28, H32, MerkleProofInputs, PosixTime, ValuePairs}

/// Check whether the interval starts on or after the point.
pub fn is_hereafter(self: Interval<Int>, point: Int) -> Bool {
  when self.lower_bound.bound_type is {
    Finite(lo) -> point <= lo
    NegativeInfinity -> False
    PositiveInfinity -> True
  }
}

/// Check whether the interval ends on or before the point.
pub fn is_herebefore(self: Interval<Int>, point: Int) -> Bool {
  when self.upper_bound.bound_type is {
    Finite(hi) -> hi <= point
    PositiveInfinity -> False
    NegativeInfinity -> True
  }
}

pub fn validate_mint(
  mints: Value,
  expected_minting_policy: ByteArray,
  expected_minting_name: ByteArray,
  expected_minting_amt: Int,
) -> Bool {
  let mints_policy = dict.to_pairs(tokens(mints, expected_minting_policy))
  mints_policy == [Pair(expected_minting_name, expected_minting_amt)]
}

pub fn safe_list_last(elems: List<a>) -> a {
  expect Some(last_elem) = list.last(elems)
  last_elem
}

pub fn safe_list_head(elems: List<a>) -> a {
  expect Some(last_elem) = list.head(elems)
  last_elem
}

pub fn safe_list_init(elems: List<a>) {
  expect Some(elems_except_last) = list.init(elems)
  elems_except_last
}

pub fn is_posix_time_none(time: PosixTime) {
  time == posix_time_none
}

/// Returns the quantity of all tokens with the policyId
pub fn quantity_of_policy_id(value: Value, policyId: PolicyId) {
  dict.foldl(tokens(value, policyId), 0, fn(_k, v, result) { v + result })
}

pub fn get_own_hash(output_reference: OutputReference, txInputs: List<Input>) {
  expect Some(input) =
    list.find(txInputs, fn(x) { x.output_reference == output_reference })

  expect Script(hash) = input.output.address.payment_credential
  hash
}

pub fn get_own_address(output_reference: OutputReference, txInputs: List<Input>) {
  expect Some(input) =
    list.find(txInputs, fn(x) { x.output_reference == output_reference })

  input.output.address
}

/// Helper function for accessing spending UTxO using its input index.
pub fn get_own_input_at(
  inputs: List<Input>,
  own_out_ref: OutputReference,
  input_index: Int,
) -> Output {
  expect Some(Input { output: own_utxo, output_reference: spent_own_out_ref }) =
    list.at(inputs, input_index)
  expect (own_out_ref == spent_own_out_ref)?
  own_utxo
}

pub fn is_output_to_sc(output: Output, scHash: ByteArray) -> Bool {
  when output.address.payment_credential is {
    Script(hash) -> hash == scHash
    _ -> False
  }
}

/// A more readable function for validating a pub key hash has provided
/// signature in a transaction.
pub fn has_signed(
  signer: VerificationKeyHash,
  extra_signatories: List<VerificationKeyHash>,
) -> Bool {
  extra_signatories |> list.has(signer)
}

/// Expects a single token to be present in the given `Value`, and returns it as
/// a triplet.
pub fn get_single_asset_from_value(v: Value) -> (PolicyId, AssetName, Int) {
  expect [Pair(sym, tn_qty_pairs)] =
    v
      |> assets.to_dict
      |> dict.to_pairs
  expect [Pair(tn, qty)] = tn_qty_pairs |> dict.to_pairs
  (sym, tn, qty)
}

/// Expects exactly 2 tokens in the given `Value`, one of which must be ADA, and
/// returns the other as a triplet.
pub fn get_single_asset_from_value_apart_from_ada(
  v: Value,
) -> (PolicyId, AssetName, Int) {
  expect [Pair(sym, tn_qty_pairs)] =
    v
      |> assets.to_dict
      |> dict.delete(assets.ada_policy_id)
      |> dict.to_pairs
  expect [Pair(tn, qty)] = tn_qty_pairs |> dict.to_pairs
  (sym, tn, qty)
}

/// For state UTxOs with specific policy IDs. Note that the address is not
/// checked.
pub fn get_authentic_input_with_policy_at(
  inputs: List<Input>,
  nft_policy_id: PolicyId,
  input_index: Int,
) -> Input {
  expect Some(
    Input { output: Output { value: input_value, .. }, .. } as found_input,
  ) = list.at(inputs, input_index)

  expect (input_nft_policy_id, _, 1) =
    get_single_asset_from_value_apart_from_ada(input_value)

  expect input_nft_policy_id == nft_policy_id

  found_input
}

/// For state UTxOs with specific NFTs. Note that the address is not checked.
pub fn get_authentic_input_with_nft_at(
  inputs: List<Input>,
  nft_policy_id: PolicyId,
  nft_asset_name: AssetName,
  input_index: Int,
) -> Input {
  expect Some(
    Input { output: Output { value: input_value, .. }, .. } as found_input,
  ) = list.at(inputs, input_index)

  let input_nft_triplet =
    get_single_asset_from_value_apart_from_ada(input_value)

  expect input_nft_triplet == (nft_policy_id, nft_asset_name, 1)

  found_input
}

/// For state UTxOs of validators which their NFTs have policy IDs equal to
/// their payment credentials.
pub fn get_authentic_input_of(
  inputs: List<Input>,
  address_script_hash: ScriptHash,
  nft_token_name: ByteArray,
  input_index: Int,
) -> Input {
  expect Some(
    Input {
      output: Output {
        address: Address { payment_credential: Script(refs_script_hash), .. },
        value: refs_value,
        ..
      },
      ..
    } as found_ref_input,
  ) = list.at(inputs, input_index)

  let refs_nft_triplet = get_single_asset_from_value_apart_from_ada(refs_value)

  expect and {
      refs_script_hash == address_script_hash,
      refs_nft_triplet == (address_script_hash, nft_token_name, 1),
    }

  found_ref_input
}

/// Helper useful for spending validators. Also ensures no reference scripts are
/// attached.
pub fn get_continued_output_datum_at(
  outputs: List<Output>,
  expected_address: Address,
  expected_value: Value,
  output_index: Int,
) -> Data {
  expect Some(Output {
    address: output_address,
    value: output_value,
    datum: InlineDatum(output_datum_data),
    reference_script: None,
  }) = list.at(outputs, output_index)

  expect output_address == expected_address
  expect output_value == expected_value

  output_datum_data
}

/// Helper useful for minting/withdrawal validators. Also ensures no reference
/// scripts are attached.
pub fn get_authentic_output_datum_at_address_with_nft_at(
  outputs: List<Output>,
  expected_address: Address,
  nft_policy_id: PolicyId,
  nft_asset_name: AssetName,
  output_index: Int,
) -> Data {
  expect Some(Output {
    address: output_address,
    value: output_value,
    datum: InlineDatum(output_datum_data),
    reference_script: None,
  }) = list.at(outputs, output_index)

  expect expected_address == output_address

  let output_nft_triplet =
    get_single_asset_from_value_apart_from_ada(output_value)

  expect output_nft_triplet == (nft_policy_id, nft_asset_name, 1)

  output_datum_data
}

// Used to retrieve asset name from a value with a specific policy id
pub fn get_asset_name_from_value_with_policy(
  value: Value,
  policy_id: PolicyId,
) -> AssetName {
  expect [Pair(asset_name, _)] =
    value
      |> assets.tokens(policy_id)
      |> dict.to_pairs

  asset_name
}

// Used to retrieve a singleton asset from a value with a specific policy id
pub fn get_singleton_asset_with_policy(
  value: Value,
  policy_id: PolicyId,
) -> Pair<AssetName, Int> {
  expect [p] =
    value
      |> assets.tokens(policy_id)
      |> dict.to_pairs

  p
}

// Verify a 32-byte hash (Blake2b-256)
pub fn verify_hash_32(data: Data, data_hash: H32<Data>) {
  expect blake2b_256(cbor.serialise(data)) == data_hash
}

// Verify a 28-byte hash (Blake2b-224)
pub fn verify_hash_28(data: Data, data_hash: H28<Data>) {
  expect blake2b_224(cbor.serialise(data)) == data_hash
}

/// Given a value, an NFT, and a `ValuePairs` (structured identical to `Value`,
/// but safe for storage in datums), this helper ensures that the given `Value`
/// is a correct representation of the given `ValuePairs`. Meaning it expects
/// `Value` to only have the `ValuePairs`, the NFT, and some ADA. It also
/// validates that 0 ADA is included in `ValuePairs` (this does NOT validate the
/// absence of ADA policy ID and asset name in `ValuePairs`, TODO?).
pub fn authentic_value_has_tokens(
  value: Value,
  nft_policy_id: PolicyId,
  nft_asset_name: AssetName,
  tokens: ValuePairs,
) -> Bool {
  let without_nft =
    value
      |> assets.add(nft_policy_id, nft_asset_name, -1)
  assets.match(without_nft, tokens, fn(_, tokens_ada) { tokens_ada == 0 })
}

/// Traverses two lists in parallel, accumulating a value using the provided
/// `with` function. The lists are traversed from left to right, stopping after
/// the smaller list's elements are exhausted. However, the `with` function
/// is applied in reverse order (i.e. right to left).
pub fn zip_foldr(
  self: List<a>,
  bs: List<b>,
  acc: c,
  with: fn(a, b, c) -> c,
) -> c {
  when self is {
    [] -> acc
    [x, ..xs] ->
      when bs is {
        [] -> acc
        [y, ..ys] -> {
          let c = zip_foldr(xs, ys, acc, with)
          with(x, y, c)
        }
      }
  }
}

/// Similar to [`zip_foldr`](#zip_foldr), but traverses the lists from left.
pub fn zip_foldl(
  self: List<a>,
  bs: List<b>,
  acc: c,
  with: fn(a, b, c) -> c,
) -> c {
  when self is {
    [] -> acc
    [x, ..xs] ->
      when bs is {
        [] -> acc
        [y, ..ys] -> {
          let c = with(x, y, acc)
          zip_foldl(xs, ys, c, with)
        }
      }
  }
}

/// Function for comparing a `Value` (presumably coming from script context),
/// agains a `ValuePairs` (presumably coming from a datum). This helper only
/// works for values that are identical, apart from the quantities.
///
/// As an example use-case, this allows checking the `Value` has still not
/// reached the target `ValuePairs`.
pub fn compare_authentic_value_with_target(
  value: Value,
  nft_policy_id: PolicyId,
  nft_asset_name: AssetName,
  target: ValuePairs,
  quantity_predicate: fn(Int, Int) -> Bool,
) -> Bool {
  let value_without_nft: Pairs<PolicyId, Dict<AssetName, Int>> =
    value
      |> assets.add(nft_policy_id, nft_asset_name, -1)
      |> assets.to_dict
      |> dict.to_pairs
  let
    value_policy_pair,
    target_policy_pair,
    _,
  <- zip_foldr(value_without_nft, target, True)
  expect value_policy_pair.1st == target_policy_pair.1st
  let value_token_pairs = value_policy_pair.2nd |> dict.to_pairs
  let
    value_token_pair,
    target_token_pair,
    _,
  <- zip_foldr(value_token_pairs, target_policy_pair.2nd, True)
  expect value_token_pair.1st == target_token_pair.1st
  quantity_predicate(value_token_pair.2nd, target_token_pair.2nd)
}

/// Fetch the redeemer data of a specific script purpose at an expected index.
pub fn get_redeemer_at(
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  expected_purpose: ScriptPurpose,
  redeemer_index: Int,
) -> Redeemer {
  expect Some(redeemer_pair) = redeemers |> list.at(redeemer_index)
  expect redeemer_pair.1st == expected_purpose
  redeemer_pair.2nd
}

/// Merkelized validator abstraction for `phas` function from Plutarch.
pub fn plutarch_phas(
  mpf_root: ByteArray,
  key: Data,
  value: Data,
  membership_proof: mpf.Proof,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Bool {
  merkelized_validator.generic_delegated_validation(
    staking_validator: env.plutarch_phas_validator_hash,
    withdraw_redeemer_validator: fn(phas_validator_redeemer: Data) -> Bool {
      expect [
        redeemer_root_data, redeemer_key_data, redeemer_value_data,
        redeemer_proof_data,
        ..
      ] = builtin.un_list_data(phas_validator_redeemer)

      let mpf_root_data: Data = mpf_root

      let proof_data: Data = membership_proof

      and {
        redeemer_root_data == mpf_root_data,
        redeemer_key_data == key,
        redeemer_value_data == value,
        redeemer_proof_data == proof_data,
      }
    },
    redeemers: redeemers,
  )
}

/// Merkelized validator abstraction for `pexcludes` function from Plutarch.
pub fn plutarch_pexcludes(
  mpf_root: ByteArray,
  key: Data,
  non_membership_proof: mpf.Proof,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Bool {
  merkelized_validator.generic_delegated_validation(
    staking_validator: env.plutarch_pexcludes_validator_hash,
    withdraw_redeemer_validator: fn(pexcludes_validator_redeemer: Data) -> Bool {
      expect [redeemer_root_data,
        redeemer_key_data,
        redeemer_proof_data, ..] =
        builtin.un_list_data(pexcludes_validator_redeemer)

      let mpf_root_data: Data = mpf_root

      let proof_data: Data = non_membership_proof

      and {
        redeemer_root_data == mpf_root_data,
        redeemer_key_data == key,
        redeemer_proof_data == proof_data,
      }
    },
    redeemers: redeemers,
  )
}

/// Merkelized validator abstraction for `pdelete` function from Plutarch.
pub fn plutarch_pdelete(
  mpf_old_root: ByteArray,
  key: Data,
  value: Data,
  deletion_proof: mpf.Proof,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> ByteArray {
  let inputsObject =
    MerkleProofInputs {
      mpf_root: mpf_old_root,
      key,
      value,
      proof: deletion_proof,
    }
  let mpf_proof_coercer =
    fn(proof_data: Data) -> mpf.Proof {
      expect proof_steps_as_data = builtin.un_list_data(proof_data)
      let data_to_mpf_neighbor_coercer =
        fn(neighbor_data: Data) -> mpf.Neighbor {
          expect [nibble_data,
            prefix_data, root_data] = builtin.unconstr_fields(neighbor_data)
          mpf.Neighbor {
            nibble: nibble_data |> builtin.un_i_data,
            prefix: prefix_data |> builtin.un_b_data,
            root: root_data |> builtin.un_b_data,
          }
        }
      list.foldr(
        proof_steps_as_data,
        [],
        fn(proof_step_as_data, result) -> List<mpf.ProofStep> {
          let new_elem =
            when builtin.unconstr_index(proof_step_as_data) is {
              0 ->
                mpf.Branch {
                  skip: proof_step_as_data
                    |> builtin.unconstr_fields
                    |> builtin.head_list
                    |> builtin.un_i_data,
                  neighbors: proof_step_as_data
                    |> builtin.unconstr_fields
                    |> builtin.tail_list
                    |> builtin.head_list
                    |> builtin.un_b_data,
                }
              1 ->
                mpf.Fork {
                  skip: proof_step_as_data
                    |> builtin.unconstr_fields
                    |> builtin.head_list
                    |> builtin.un_i_data,
                  neighbor: proof_step_as_data
                    |> builtin.unconstr_fields
                    |> builtin.tail_list
                    |> builtin.head_list
                    |> data_to_mpf_neighbor_coercer,
                }
              2 ->
                mpf.Leaf {
                  skip: proof_step_as_data
                    |> builtin.unconstr_fields
                    |> builtin.head_list
                    |> builtin.un_i_data,
                  key: proof_step_as_data
                    |> builtin.unconstr_fields
                    |> builtin.tail_list
                    |> builtin.head_list
                    |> builtin.un_b_data,
                  value: proof_step_as_data
                    |> builtin.unconstr_fields
                    |> builtin.tail_list
                    |> builtin.tail_list
                    |> builtin.head_list
                    |> builtin.un_b_data,
                }
              _ -> fail @"Unexpected proof step constructor"
            }

          list.push(result, new_elem)
        },
      )
    }
  merkelized_validator.delegated_compute(
    function_input: inputsObject,
    staking_validator: env.plutarch_pdelete_validator_hash,
    input_data_coercer: fn(input: Data) -> MerkleProofInputs {
      let fields = builtin.unconstr_fields(input)
      let mpf_root = fields |> builtin.head_list |> builtin.un_b_data
      let key = fields |> builtin.tail_list |> builtin.head_list
      let value =
        fields |> builtin.tail_list |> builtin.tail_list |> builtin.head_list
      let proof =
        fields
          |> builtin.tail_list
          |> builtin.tail_list
          |> builtin.tail_list
          |> builtin.head_list
          |> mpf_proof_coercer()
      MerkleProofInputs { mpf_root, key, value, proof }
    },
    output_data_coercer: fn(output) -> ByteArray { builtin.un_b_data(output) },
    redeemers: redeemers,
  )
}

pub fn get_verification_key_hash(key: VerificationKey) -> VerificationKeyHash {
  blake2b_224(key)
}

pub fn address_coercer(address_as_data: Data) -> Address {
  expect [payment_cred_data,
    stake_cred_data] = builtin.unconstr_fields(address_as_data)

  Address {
    payment_credential: payment_credential_coercer(payment_cred_data),
    stake_credential: optional_stake_credential_coercer(stake_cred_data),
  }
}

pub fn payment_credential_coercer(
  payment_cred_as_data: Data,
) -> PaymentCredential {
  expect result: PaymentCredential = payment_cred_as_data
  result
}

pub fn optional_stake_credential_coercer(
  opt_stake_cred_as_data: Data,
) -> Option<StakeCredential> {
  when builtin.unconstr_index(opt_stake_cred_as_data) is {
    0 -> None
    1 -> {
      expect [stake_cred_as_data] =
        builtin.unconstr_fields(opt_stake_cred_as_data)
      expect stake_cred: StakeCredential = stake_cred_as_data
      Some(stake_cred)
    }
    _ -> fail @"Unexpected optional stake credential constructor"
  }
}
