use aiken/builtin
use aiken/cbor
use aiken/collection/dict
use aiken/collection/list
use aiken/crypto.{
  ScriptHash, VerificationKey, VerificationKeyHash, blake2b_224, blake2b_256,
}
use aiken/interval.{Interval}
use aiken_design_patterns/validity_range_normalization as vrn
use cardano/address.{Address, Script}
use cardano/assets.{AssetName, PolicyId, Value, tokens}
use cardano/transaction.{
  InlineDatum, Input, Mint, Output, Redeemer, ScriptPurpose, Spend, Withdraw,
}
use env
use midgard/common/types.{H28, H32, MerkleRoot, PosixTime, Proof}

/// Validate datum value pairs without sorting or dropping entries, returning
/// a Value or failing. Both policy IDs and asset names must be strictly
/// lexicographically increasing; asset maps must be nonempty and quantities
/// nonzero. The empty value is valid, and signed quantities remain valid for
/// this general-purpose Value check. Check nonnegativity separately for funds.
/// This checks Value's structural invariants, not ledger key-length limits or
/// the special ADA namespace. Callers must enforce those when required.
pub fn is_canonical_value(xs: Pairs<PolicyId, Pairs<AssetName, Int>>) -> Value {
  xs
    |> list.map(
        fn(Pair(policy, entries)) {
          Pair(policy, dict.from_ascending_pairs(entries))
        },
      )
    |> assets.from_ascending_pairs
}

pub fn get_inclusive_bounds_of_a_short_validity_range(
  interval: Interval,
) -> (PosixTime, PosixTime) {
  when vrn.normalize_time_range(interval) is {
    vrn.ClosedRange { lower, upper } -> {
      expect upper - lower <= env.max_validity_range_length
      (lower, upper)
    }
    _ -> fail @"Validity range was expected to be a closed range"
  }
}

pub fn get_inclusive_upper_bound_of_interval(interval: Interval) -> Int {
  when vrn.normalize_time_range(interval) is {
    vrn.ClosedRange { upper, .. } | vrn.FromNegInf { upper } -> upper
    _ -> fail @"Validity range was expected to be bound at upper end"
  }
}

pub fn get_inclusive_lower_bound_of_interval(interval: Interval) -> Int {
  when vrn.normalize_time_range(interval) is {
    vrn.ClosedRange { lower, .. } | vrn.ToPosInf { lower } -> lower
    _ -> fail @"Validity range was expected to be bound at lower end"
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

/// Returns the quantity of all tokens with the policyId
pub fn quantity_of_policy_id(value: Value, policyId: PolicyId) {
  dict.foldr(tokens(value, policyId), 0, fn(_k, v, result) { v + result })
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
  expect [Pair(sym, tn_qty_pairs)] = v
    |> assets.to_dict
    |> dict.to_pairs
  expect [Pair(tn, qty)] = tn_qty_pairs |> dict.to_pairs
  (sym, tn, qty)
}

// Used to retrieve a singleton asset from a value with a specific policy id
pub fn get_singleton_asset_with_policy(
  value: Value,
  policy_id: PolicyId,
) -> Pair<AssetName, Int> {
  expect [p] = value
    |> assets.tokens(policy_id)
    |> dict.to_pairs

  p
}

/// Expects exactly 2 tokens in the given `Value`, one of which must be ADA, and
/// returns the other as a triplet.
pub fn get_single_asset_from_value_apart_from_ada(
  v: Value,
) -> (PolicyId, AssetName, Int) {
  expect [Pair(sym, tn_qty_pairs)] = v
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
  expect
      Some(
        Input { output: Output { value: input_value, .. }, .. } as found_input,
      )
    = list.at(inputs, input_index)

  expect (input_nft_policy_id, _, 1) = get_single_asset_from_value_apart_from_ada(
    input_value,
  )

  expect input_nft_policy_id == nft_policy_id

  found_input
}

pub fn get_authentic_input_and_asset_name_with_policy_at(
  inputs: List<Input>,
  nft_policy_id: PolicyId,
  input_index: Int,
  return: fn(Input, AssetName) -> result,
) -> result {
  expect
      Some(
        Input { output: Output { value: input_value, .. }, .. } as found_input,
      )
    = list.at(inputs, input_index)

  expect (input_nft_policy_id, input_nft_asset_name, 1) = get_single_asset_from_value_apart_from_ada(
    input_value,
  )

  expect input_nft_policy_id == nft_policy_id

  return(found_input, input_nft_asset_name)
}

/// Similar to [`get_authentic_input_with_policy_at`](#get_authentic_input_with_policy_at),
/// but returns only the datum data together with the NFT's asset name.
pub fn get_authentic_input_datum_and_asset_name_with_policy_at(
  inputs: List<Input>,
  nft_policy_id: PolicyId,
  input_index: Int,
  return: fn(AssetName, Data) -> result,
) -> result {
  expect
      Some(Input {
        output: Output {
          value: input_value,
          datum: InlineDatum(input_datum_data),
          ..
        },
        ..
      })
    = list.at(inputs, input_index)

  expect (input_nft_policy_id, input_nft_asset_name, 1) = get_single_asset_from_value_apart_from_ada(
    input_value,
  )

  expect input_nft_policy_id == nft_policy_id

  return(input_nft_asset_name, input_datum_data)
}

/// For state UTxOs with specific NFTs. Note that the address is not checked.
pub fn get_authentic_input_with_nft_at(
  inputs: List<Input>,
  nft_policy_id: PolicyId,
  nft_asset_name: AssetName,
  input_index: Int,
) -> Input {
  expect
      Some(
        Input { output: Output { value: input_value, .. }, .. } as found_input,
      )
    = list.at(inputs, input_index)

  let input_nft_triplet = get_single_asset_from_value_apart_from_ada(
    input_value,
  )

  expect input_nft_triplet == (nft_policy_id, nft_asset_name, 1)

  found_input
}

/// Similar to [`get_authentic_input_with_nft_at`](#get_authentic_input_with_nft_at),
/// but only returns the datum data. Useful for avoiding destructuring the
/// input an extra time when only the datum is needed.
pub fn get_authentic_input_datum_with_nft_at(
  inputs: List<Input>,
  nft_policy_id: PolicyId,
  nft_asset_name: AssetName,
  input_index: Int,
) -> Data {
  expect
      Some(Input {
        output: Output {
          value: input_value,
          datum: InlineDatum(input_datum_data),
          ..
        },
        ..
      })
    = list.at(inputs, input_index)

  let input_nft_triplet = get_single_asset_from_value_apart_from_ada(
    input_value,
  )

  expect input_nft_triplet == (nft_policy_id, nft_asset_name, 1)

  input_datum_data
}

/// For state UTxOs of validators which their NFTs have policy IDs equal to
/// their payment credentials.
pub fn get_authentic_input_of(
  inputs: List<Input>,
  address_script_hash: ScriptHash,
  nft_token_name: ByteArray,
  input_index: Int,
) -> Input {
  expect
      Some(
        Input {
          output: Output {
            address: Address {
              payment_credential: Script(refs_script_hash),
              ..
            },
            value: refs_value,
            ..
          },
          ..
        } as found_ref_input,
      )
    = list.at(inputs, input_index)

  let refs_nft_triplet = get_single_asset_from_value_apart_from_ada(refs_value)

  expect and {
      refs_script_hash == address_script_hash,
      refs_nft_triplet == (address_script_hash, nft_token_name, 1),
    }

  found_ref_input
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
  expect
      Some(Output {
        address: output_address,
        value: output_value,
        datum: InlineDatum(output_datum_data),
        reference_script: None,
      })
    = list.at(outputs, output_index)

  expect expected_address == output_address

  let output_nft_triplet = get_single_asset_from_value_apart_from_ada(
    output_value,
  )

  expect output_nft_triplet == (nft_policy_id, nft_asset_name, 1)

  output_datum_data
}

/// Helper useful for spending validators. Also ensures no reference scripts are
/// attached, and that the input and output have the same address and NFT.
///
/// > [!NOTE]
/// > This does NOT check the output's value contains the same amount of ADA
/// > as the input's, since change in data can fluctuate the min required ADA.
///
/// Useful for contracts with singular authentic UTxOs (which their data is the
/// only thing that matters), at endpoints for updating their datums.
pub fn authenticate_input_output_and_get_output_datum_data(
  input: Input,
  output: Output,
  authentication_policy_id: PolicyId,
) -> Data {
  let
      Input {
        output: Output { address: input_address, value: input_value, .. },
        ..
      }
    = input

  expect
      Output {
        address: output_address,
        value: output_value,
        datum: InlineDatum(output_datum_data),
        reference_script: None,
      }
    = output

  expect input_address == output_address

  expect (input_nft_symbol, _, 1) as input_nft_triplet = get_single_asset_from_value_apart_from_ada(
    input_value,
  )

  expect input_nft_symbol == authentication_policy_id

  let output_nft_triplet = get_single_asset_from_value_apart_from_ada(
    output_value,
  )

  expect input_nft_triplet == output_nft_triplet

  output_datum_data
}

pub fn serialise_and_hash_28(data: Data) -> H28<Data> {
  data |> cbor.serialise |> blake2b_224
}

pub fn serialise_and_hash_32(data: Data) -> H32<Data> {
  data |> cbor.serialise |> blake2b_256
}

// Verify a 28-byte hash (Blake2b-224)
pub fn verify_hash_28(data: Data, data_hash: H28<Data>) -> Bool {
  serialise_and_hash_28(data) == data_hash
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

/// > [!NOTE]
/// > No validations on state tokens. Only that the specified input comes from
/// > `script_address`'s payment credential.
pub fn get_spending_redeemer_data_at(
  script_address: Address,
  input_index: Int,
  redeemer_index: Int,
  inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> Data {
  expect
      Some(Input {
        output_reference: input_outref,
        output: Output { address: input_address, .. },
      })
    = inputs |> list.at(input_index)
  //
  expect input_address.payment_credential == script_address.payment_credential
  //
  redeemers |> get_redeemer_at(Spend(input_outref), redeemer_index)
}

/// Similar to [`get_spending_redeemer_data_at`](#get_spending_redeemer_data_at),
/// but also returns the inline datum's data.
pub fn get_inline_datum_and_spending_redeemer_data_at(
  script_address: Address,
  input_index: Int,
  redeemer_index: Int,
  inputs: List<Input>,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
) -> (Data, Data) {
  expect
      Some(Input {
        output_reference: input_outref,
        output: Output {
          address: input_address,
          datum: InlineDatum(datum_data),
          ..
        },
      })
    = inputs |> list.at(input_index)
  //
  expect input_address.payment_credential == script_address.payment_credential
  //
  (
    datum_data,
    redeemers |> get_redeemer_at(Spend(input_outref), redeemer_index),
  )
}

/// Merkelized validator abstraction for `phas` function from Plutarch.
///
/// > [!WARNING]
/// > This function can only be used **once within a given transaction**.
pub fn plutarch_phas(
  merkle_root: MerkleRoot<key, value>,
  key: Data<key>,
  value: Data<value>,
  membership_proof: Proof,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  withdraw_redeemer_index: Int,
) -> Bool {
  expect _withdraw_redeemer_index = withdraw_redeemer_index
  let phas_validator_redeemer = get_unique_withdraw_redeemer(
    redeemers,
    env.plutarch_phas_validator_hash,
  )
  let key_bytes = cbor.serialise(key)
  let value_bytes = cbor.serialise(value)
  expect
      [
        redeemer_root_data, redeemer_key_data, redeemer_value_data,
        redeemer_proof_data,
        ..
      ]
    = builtin.un_list_data(phas_validator_redeemer)

  let merkle_root_data: Data = merkle_root

  let proof_data: Data = membership_proof

  expect redeemer_root_data == merkle_root_data
  expect redeemer_key_data == builtin.b_data(key_bytes)
  expect redeemer_value_data == builtin.b_data(value_bytes)
  expect redeemer_proof_data == proof_data

  True
}

pub fn plutarch_phas_raw(
  merkle_root: ByteArray,
  key_bytes: ByteArray,
  value_bytes: ByteArray,
  membership_proof: Proof,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  withdraw_redeemer_index: Int,
) -> Bool {
  expect _withdraw_redeemer_index = withdraw_redeemer_index
  let phas_validator_redeemer = get_unique_withdraw_redeemer(
    redeemers,
    env.plutarch_phas_validator_hash,
  )
  expect
      [
        redeemer_root_data, redeemer_key_data, redeemer_value_data,
        redeemer_proof_data,
        ..
      ]
    = builtin.un_list_data(phas_validator_redeemer)

  let proof_data: Data = membership_proof

  expect redeemer_root_data == builtin.b_data(merkle_root)
  expect redeemer_key_data == builtin.b_data(key_bytes)
  expect redeemer_value_data == builtin.b_data(value_bytes)
  expect redeemer_proof_data == proof_data

  True
}

/// Merkelized validator abstraction for the `pexcludes` function from
/// Plutarch. It takes the merkle root and key as pre-computed bytes rather than
/// typed values, so the trie key encoding can match an external producer (e.g.
/// the node keys its ledger MPF by `CML.TransactionInput` CBOR, which does not
/// equal `cbor.serialise` of an aiken `OutputReference`).
///
/// > [!WARNING]
/// > This function can only be used **once within a given transaction**.
pub fn plutarch_pexcludes_raw(
  merkle_root: ByteArray,
  key_bytes: ByteArray,
  non_membership_proof: Proof,
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  withdraw_redeemer_index: Int,
) -> Bool {
  expect _withdraw_redeemer_index = withdraw_redeemer_index
  let pexcludes_validator_redeemer = get_unique_withdraw_redeemer(
    redeemers,
    env.plutarch_pexcludes_validator_hash,
  )
  expect [redeemer_root_data,
        redeemer_key_data,
        redeemer_proof_data, ..] = builtin.un_list_data(
    pexcludes_validator_redeemer,
  )

  let proof_data: Data = non_membership_proof

  expect redeemer_root_data == builtin.b_data(merkle_root)
  expect redeemer_key_data == builtin.b_data(key_bytes)
  expect redeemer_proof_data == proof_data

  True
}

/// The single withdraw redeemer of `withdraw_script_hash` in this transaction.
/// Exactly one is required, which is what makes a merkelized claim unambiguous.
pub fn get_unique_withdraw_redeemer(
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  withdraw_script_hash: ScriptHash,
) -> Redeemer {
  expect [Pair(_, redeemer)] = redeemers
    |> list.filter(
        fn(redeemer_pair) {
          when redeemer_pair.1st is {
            Withdraw(Script(script_hash)) -> script_hash == withdraw_script_hash
            _ -> False
          }
        },
      )
  redeemer
}

/// The single mint redeemer for `policy_id` in this transaction. Rewarding
/// validators use this to bind a yielded operation to the exact mint purpose
/// whose policy delegates the work.
pub fn get_unique_mint_redeemer(
  redeemers: Pairs<ScriptPurpose, Redeemer>,
  policy_id: PolicyId,
) -> Redeemer {
  expect [Pair(_, redeemer)] = redeemers
    |> list.filter(
        fn(redeemer_pair) {
          when redeemer_pair.1st is {
            Mint(candidate_policy_id) -> candidate_policy_id == policy_id
            _ -> False
          }
        },
      )
  redeemer
}

pub fn get_verification_key_hash(key: VerificationKey) -> VerificationKeyHash {
  blake2b_224(key)
}
