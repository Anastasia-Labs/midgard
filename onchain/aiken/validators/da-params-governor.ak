use aiken/collection/dict
use aiken/crypto.{VerificationKeyHash, blake2b_256}
use aiken/primitive/bytearray
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId, Value}
use cardano/transaction.{
  InlineDatum, Input, Output, OutputReference, Transaction,
}
use midgard/da_attestation_types.{
  DaParamsDatum, da_params_asset_name, max_indexed_signer_count,
  verification_key_byte_count,
}

const verification_key_hash_byte_count: Int = 28

fn bytearray_slice(bytes: ByteArray, offset: Int, byte_count: Int) -> ByteArray {
  bytearray.slice(bytes, offset, offset + byte_count - 1)
}

fn sorted_unique_packed_len_at_most(
  keys: ByteArray,
  max: Int,
  byte_count: Int,
) -> Int {
  expect max > 0
  let keys_len = bytearray.length(keys)
  expect keys_len >= byte_count
  let first = bytearray_slice(keys, 0, byte_count)
  sorted_unique_packed_len_from(
    keys,
    byte_count,
    first,
    1,
    max,
    byte_count,
    keys_len,
  )
}

fn sorted_unique_packed_len_from(
  keys: ByteArray,
  offset: Int,
  prev: ByteArray,
  len: Int,
  max: Int,
  byte_count: Int,
  keys_len: Int,
) -> Int {
  if offset == keys_len {
    len
  } else {
    expect offset + byte_count <= keys_len
    let key = bytearray_slice(keys, offset, byte_count)
    expect bytearray.compare(prev, key) == Less
    let next_len = len + 1
    expect next_len <= max
    sorted_unique_packed_len_from(
      keys,
      offset + byte_count,
      key,
      next_len,
      max,
      byte_count,
      keys_len,
    )
  }
}

fn sorted_unique_len_at_most(
  keys: List<ByteArray>,
  max: Int,
  byte_count: Int,
) -> Int {
  expect [first, ..rest] = keys
  expect bytearray.length(first) == byte_count
  sorted_unique_len_from(first, rest, 1, max, byte_count)
}

fn sorted_unique_len_from(
  prev: ByteArray,
  keys: List<ByteArray>,
  len: Int,
  max: Int,
  byte_count: Int,
) -> Int {
  when keys is {
    [] -> len
    [key, ..rest] -> {
      expect bytearray.length(key) == byte_count
      expect bytearray.compare(prev, key) == Less
      let next_len = len + 1
      expect next_len <= max
      sorted_unique_len_from(key, rest, next_len, max, byte_count)
    }
  }
}

/// Lower clamp of the F04 §4 governed threshold floor. This is what makes
/// single-key capture of `da_threshold` or `update_threshold` unrepresentable.
const min_governed_threshold: Int = 2

/// Smallest owner set the governor will represent — the owner-set drain
/// protection. It is deliberately a separate constant from
/// `min_governed_threshold` even though both are currently two: one bounds a
/// threshold, the other bounds a set size, and a future change to either must
/// not silently move the other. `da_params_governor_invariant_owner_set_drain_protection`
/// pins the relationship they must keep.
const min_owner_count: Int = 2

/// F04 §4 governed threshold floor: `max(2, ceil(2*set_len/3))`.
///
/// `ceil(2*set_len/3)` is evaluated as `(2*set_len + 2) / 3` under integer
/// division. The clamp is what makes single-key capture of either threshold
/// unrepresentable; the two-thirds term is what keeps a quorum from dropping
/// either threshold to a sub-two-thirds slice of its own set.
///
/// Source: `docs/midgard/decisions/0002-canonical-v1-goal-economics-and-margins.md`
/// §4 — `da_threshold >= max(2, ceil(2*committee_len/3))` and
/// `update_threshold >= max(2, ceil(2*owner_len/3))`.
fn governed_threshold_floor(set_len: Int) -> Int {
  let two_thirds_ceiling = ( 2 * set_len + 2 ) / 3
  if two_thirds_ceiling > min_governed_threshold {
    two_thirds_ceiling
  } else {
    min_governed_threshold
  }
}

fn valid_datum(
  datum: DaParamsDatum,
  max_committee_size: Int,
  max_owner_count: Int,
) -> Bool {
  expect max_committee_size > 0
  expect max_committee_size <= max_indexed_signer_count
  expect max_owner_count > 0

  let committee_len = sorted_unique_packed_len_at_most(
    datum.committee,
    max_committee_size,
    verification_key_byte_count,
  )
  expect datum.committee_signers_hash == blake2b_256(datum.committee)
  expect datum.da_threshold >= governed_threshold_floor(committee_len)
  expect datum.da_threshold <= committee_len

  let owner_len = sorted_unique_len_at_most(
    datum.owners,
    max_owner_count,
    verification_key_hash_byte_count,
  )
  expect owner_len >= min_owner_count
  expect datum.update_threshold >= governed_threshold_floor(owner_len)
  expect datum.update_threshold <= owner_len

  True
}

fn has_input_ref(inputs: List<Input>, out_ref: OutputReference) -> Bool {
  when inputs is {
    [] -> False
    [input, ..rest] ->
      if input.output_reference == out_ref {
        True
      } else {
        has_input_ref(rest, out_ref)
      }
  }
}

fn input_at_ref(inputs: List<Input>, out_ref: OutputReference) -> Input {
  when inputs is {
    [] -> fail
    [input, ..rest] ->
      if input.output_reference == out_ref {
        input
      } else {
        input_at_ref(rest, out_ref)
      }
  }
}

fn only_output_with_nft(
  outputs: List<Output>,
  policy_id: PolicyId,
  found: Option<Output>,
) -> Output {
  when outputs is {
    [] -> {
      expect Some(output) = found
      output
    }
    [output, ..rest] -> {
      let quantity = assets.quantity_of(
        output.value,
        policy_id,
        da_params_asset_name,
      )

      if quantity == 0 {
        only_output_with_nft(rest, policy_id, found)
      } else {
        expect quantity == 1
        expect found == None
        only_output_with_nft(rest, policy_id, Some(output))
      }
    }
  }
}

fn params_output_datum_at_script(
  output: Output,
  policy_id: PolicyId,
  max_committee_size: Int,
  max_owner_count: Int,
) -> DaParamsDatum {
  expect
      Output {
        address,
        datum: InlineDatum(datum_data),
        reference_script: None,
        ..
      }
    = output
  expect Script(output_script_hash) = address.payment_credential
  expect output_script_hash == policy_id
  expect datum: DaParamsDatum = datum_data
  expect valid_datum(datum, max_committee_size, max_owner_count)
  datum
}

fn params_output_datum_at_address(
  output: Output,
  address: Address,
  max_committee_size: Int,
  max_owner_count: Int,
) -> DaParamsDatum {
  expect
      Output {
        address: output_address,
        datum: InlineDatum(datum_data),
        reference_script: None,
        ..
      }
    = output
  expect output_address == address
  expect datum: DaParamsDatum = datum_data
  expect valid_datum(datum, max_committee_size, max_owner_count)
  datum
}

fn value_not_drained(input_value: Value, output_value: Value) -> Bool {
  let output_value_data: Data = output_value
  assets.expect_match(
    input_value,
    output_value_data,
    fn(input_lovelace, output_lovelace) { output_lovelace >= input_lovelace },
  )
}

fn has_key(keys: List<VerificationKeyHash>, key: VerificationKeyHash) -> Bool {
  when keys is {
    [] -> False
    [current, ..rest] ->
      if current == key {
        True
      } else {
        has_key(rest, key)
      }
  }
}

fn owner_quorum_met(
  owners: List<VerificationKeyHash>,
  signers: List<VerificationKeyHash>,
  threshold: Int,
  count: Int,
) -> Bool {
  if count >= threshold {
    True
  } else {
    when owners is {
      [] -> False
      [owner, ..rest] -> {
        let next_count = if has_key(signers, owner) {
          count + 1
        } else {
          count
        }
        owner_quorum_met(rest, signers, threshold, next_count)
      }
    }
  }
}

// ## Q63 governed-floor fixtures
//
// Committee keys and owner key hashes are generated with a strictly ascending
// leading byte so that every generated set satisfies the sorted-unique
// requirements of `valid_datum` and only the governed thresholds are ever the
// subject under test.

const test_max_committee_size: Int = 16

const test_max_owner_count: Int = 16

const test_committee_key_suffix: ByteArray =
  #"00000000000000000000000000000000000000000000000000000000000000"

const test_owner_hash_suffix: ByteArray =
  #"000000000000000000000000000000000000000000000000000000"

fn test_committee_key(index: Int) -> ByteArray {
  bytearray.push(test_committee_key_suffix, index)
}

fn test_owner_hash(index: Int) -> VerificationKeyHash {
  bytearray.push(test_owner_hash_suffix, index)
}

fn test_committee_from(size: Int, index: Int, packed: ByteArray) -> ByteArray {
  if index > size {
    packed
  } else {
    test_committee_from(
      size,
      index + 1,
      bytearray.concat(packed, test_committee_key(index)),
    )
  }
}

fn test_owners_from(
  index: Int,
  owners: List<VerificationKeyHash>,
) -> List<VerificationKeyHash> {
  if index == 0 {
    owners
  } else {
    test_owners_from(index - 1, [test_owner_hash(index), ..owners])
  }
}

fn test_params(
  committee_size: Int,
  da_threshold: Int,
  owner_count: Int,
  update_threshold: Int,
) -> DaParamsDatum {
  let committee = test_committee_from(committee_size, 1, #"")
  DaParamsDatum {
    committee,
    committee_signers_hash: blake2b_256(committee),
    da_threshold,
    owners: test_owners_from(owner_count, []),
    update_threshold,
  }
}

fn test_valid_datum(datum: DaParamsDatum) -> Bool {
  valid_datum(datum, test_max_committee_size, test_max_owner_count)
}

fn floor_is_satisfiable_from(set_len: Int, max_len: Int) -> Bool {
  if set_len > max_len {
    True
  } else if governed_threshold_floor(set_len) > set_len {
    False
  } else {
    floor_is_satisfiable_from(set_len + 1, max_len)
  }
}

// ## Q63 governed floor/drain invariants
//
// The three invariants the governor now enforces, pinned as absolute values
// against F04 §4 rather than against each other.

/// Invariant 1 — majority DA floor. `da_threshold` is floored at
/// `max(2, ceil(2*committee_len/3))`, so a bare majority of a committee is
/// never enough to move DA attestation power.
test da_params_governor_invariant_da_threshold_majority_floor() {
  and {
    governed_threshold_floor(2) == 2,
    governed_threshold_floor(3) == 2,
    governed_threshold_floor(4) == 3,
    governed_threshold_floor(5) == 4,
    governed_threshold_floor(6) == 4,
    governed_threshold_floor(7) == 5,
    governed_threshold_floor(9) == 6,
    governed_threshold_floor(16) == 11,
  }
}

/// Invariant 2 — 2-of-N update floor. The floor never drops below two for any
/// representable set size, so single-key capture of either threshold is
/// unrepresentable, and it never exceeds the set size, so it stays satisfiable.
test da_params_governor_invariant_update_threshold_two_of_n_floor() {
  and {
    min_governed_threshold == 2,
    governed_threshold_floor(1) == 2,
    governed_threshold_floor(2) == 2,
    governed_threshold_floor(3) == 2,
    governed_threshold_floor(4) == 3,
    floor_is_satisfiable_from(min_owner_count, max_indexed_signer_count),
  }
}

/// Invariant 3 — owner-set drain protection. The owner set floor is two, and a
/// lone owner cannot satisfy the threshold floor, so an owner quorum cannot
/// drain the owner set toward a single key or toward empty.
test da_params_governor_invariant_owner_set_drain_protection() {
  and {
    min_owner_count == 2,
    // An owner set must always be large enough to carry the threshold floor,
    // otherwise the governor would admit no representable datum at all.
    min_owner_count >= min_governed_threshold,
    governed_threshold_floor(1) > 1,
  }
}

// ## Q63 valid-boundary controls
//
// Both controls sit exactly on the F04 floor, so they prove the governed
// bound admits the smallest configuration it is meant to admit rather than
// rejecting everything.

/// Control: a 6-member committee with `da_threshold` exactly at the F04 floor
/// `max(2, ceil(2*6/3)) == 4` is accepted.
test da_params_governor_control_da_threshold_at_floor_accepted() {
  test_valid_datum(test_params(6, 4, 3, 2))
}

/// Control: a 4-owner set with `update_threshold` exactly at the F04 floor
/// `max(2, ceil(2*4/3)) == 3` is accepted.
test da_params_governor_control_update_threshold_at_floor_accepted() {
  test_valid_datum(test_params(3, 2, 4, 3))
}

// ## Q63 below-floor and drain rejection classes
//
// Each rejection pins exactly one disposition: every field other than the one
// under test is at or above its own governed floor, so the failure can only be
// the named violation.

/// Rejection: a bare majority `3` of a 6-member committee is below the F04
/// two-thirds floor of `4`, so DA capture by a sub-two-thirds committee slice
/// is unrepresentable.
test da_params_governor_rejects_da_threshold_below_floor() fail {
  test_valid_datum(test_params(6, 3, 3, 2))
}

/// Rejection: a single-key `update_threshold` over a 3-owner set is below the
/// F04 floor of `2`, so single-key governance capture is unrepresentable.
test da_params_governor_rejects_update_threshold_below_floor() fail {
  test_valid_datum(test_params(6, 4, 3, 1))
}

/// Rejection: draining the owner set to a single owner is unrepresentable —
/// the floor is `2`, and `update_threshold` can never exceed the owner count.
test da_params_governor_rejects_owner_set_drained_to_one() fail {
  test_valid_datum(test_params(6, 4, 1, 1))
}

validator da_params_governor(
  init_ref: OutputReference,
  max_committee_size: Int,
  max_owner_count: Int,
) {
  mint(_redeemer: Data, policy_id: PolicyId, tx: Transaction) {
    expect [Pair(asset_name, minted_quantity)] = tx.mint
      |> assets.tokens(policy_id)
      |> dict.to_pairs
    expect asset_name == da_params_asset_name
    expect minted_quantity == 1

    expect has_input_ref(tx.inputs, init_ref)

    expect _output_datum = tx.outputs
      |> only_output_with_nft(policy_id, None)
      |> params_output_datum_at_script(
          policy_id,
          max_committee_size,
          max_owner_count,
        )

    True
  }

  spend(
    datum: Option<DaParamsDatum>,
    _redeemer: Data,
    own_ref: OutputReference,
    tx: Transaction,
  ) {
    expect Some(input_datum) = datum
    expect valid_datum(input_datum, max_committee_size, max_owner_count)

    let own_input = input_at_ref(tx.inputs, own_ref)
    expect
        Output {
          address: input_address,
          value: input_value,
          datum: InlineDatum(input_datum_data),
          reference_script: None,
        }
      = own_input.output
    let expected_input_datum_data: Data = input_datum
    expect input_datum_data == expected_input_datum_data
    expect Script(policy_id) = input_address.payment_credential
    expect assets.quantity_of(input_value, policy_id, da_params_asset_name) == 1

    expect
      owner_quorum_met(
        input_datum.owners,
        tx.extra_signatories,
        input_datum.update_threshold,
        0,
      )

    let continued_output = tx.outputs |> only_output_with_nft(policy_id, None)

    expect _continued_datum = continued_output
      |> params_output_datum_at_address(
          input_address,
          max_committee_size,
          max_owner_count,
        )
    expect value_not_drained(input_value, continued_output.value)

    True
  }

  else(_) {
    fail
  }
}
