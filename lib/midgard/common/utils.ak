use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{ScriptHash, VerificationKeyHash}
use aiken_design_patterns/validity_range_normalization as vrn
use cardano/address.{Address, Script}
use cardano/assets.{AssetName, PolicyId, Value, tokens}
use cardano/transaction.{Input, Output, OutputReference, ValidityRange}
use midgard/common/constants.{
  max_validity_range_for_current_time, posix_time_none,
}
use midgard/common/types.{POSIXTime}

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

pub fn is_posix_time_none(time: POSIXTime) {
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

pub fn is_output_to_sc(output: Output, scHash: ByteArray) -> Bool {
  when output.address.payment_credential is {
    Script(hash) -> hash == scHash
    _ -> False
  }
}

pub fn get_current_time(validity_range: ValidityRange) -> POSIXTime {
  when vrn.normalize_time_range(validity_range) is {
    vrn.ClosedRange { lower, upper } -> {
      expect upper - lower <= max_validity_range_for_current_time
      upper
    }
    _ -> fail @"Unexpected validity range provided."
  }
}

pub fn has_signed(
  signer: VerificationKeyHash,
  extra_signatories: List<VerificationKeyHash>,
) -> Bool {
  extra_signatories |> list.has(signer)
}

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

/// For beacon UTxOs with specific policy IDs at a given address.
pub fn get_authentic_input_with_policy_at_address(
  inputs: List<Input>,
  address: Address,
  nft_policy_id: PolicyId,
  nft_token_name: ByteArray,
  input_index: Int,
) -> Input {
  expect Some(
    Input { output: Output { address: input_addr, value: input_value, .. }, .. } as found_input,
  ) = list.at(inputs, input_index)

  let input_nft_triplet =
    get_single_asset_from_value_apart_from_ada(input_value)

  expect and {
      input_addr == address,
      input_nft_triplet == (nft_policy_id, nft_token_name, 1),
    }

  found_input
}

/// For beacon UTxOs of validators which their NFTs have policy IDs equal to
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
