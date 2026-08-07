use aiken/collection/dict
use aiken/crypto.{VerificationKeyHash, blake2b_256}
use aiken/primitive/bytearray
use cardano/address.{Address, Script}
use cardano/assets.{PolicyId, Value}
use cardano/transaction.{
  InlineDatum, Input, NoDatum, Output, OutputReference, Transaction, placeholder,
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

/// Smallest owner set the governor will represent.
///
/// Under the current `governed_threshold_floor` the `expect owner_len >=
/// min_owner_count` in `valid_datum` is **redundant**, not load-bearing: a
/// one-owner set is already unrepresentable because `update_threshold` would
/// have to be both `>= governed_threshold_floor(1) == 2` and `<= owner_len ==
/// 1`. Removing the line would not change the set of datums the governor
/// admits today, so nothing here should be read as "this line is what stops the
/// drain".
///
/// It is kept as *declared defense in depth*, because the redundancy is an
/// artifact of the current arithmetic rather than a property of the governor.
/// F04 §4's two-thirds term is an economic parameter; if it were ever weakened
/// (say to `ceil(set_len/2)`, whose value at 1 is 1) a one-owner set carrying
/// `update_threshold == 1` would become representable again and single-key
/// governance capture would return. This constant blocks that independently of
/// whatever the floor expression becomes.
///
/// It is deliberately a separate constant from `min_governed_threshold` even
/// though both are currently two: one bounds a set size, the other bounds a
/// threshold, and a future change to either must not silently move the other.
/// `da_params_governor_invariant_owner_set_drain_protection` pins the
/// relationship they must keep.
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

/// True when no integer in `threshold..set_len` satisfies both
/// `>= governed_threshold_floor(set_len)` and `<= set_len`.
///
/// This is arithmetic over the floor function, not a statement about
/// `valid_datum`; it happens to mirror the shape of the two threshold bounds
/// `valid_datum` applies, which is why a `False` here corresponds to a set size
/// no datum could name a threshold for. Exhaustive rather than an appeal to
/// monotonicity, so "regardless of threshold" is measured over the whole
/// candidate range instead of asserted.
fn no_threshold_satisfies_floor(set_len: Int, threshold: Int) -> Bool {
  if threshold > set_len {
    True
  } else if threshold >= governed_threshold_floor(set_len) {
    False
  } else {
    no_threshold_satisfies_floor(set_len, threshold + 1)
  }
}

// ## Q63 spend-seam fixtures
//
// `valid_datum` is a private function; the property that actually matters is
// that the `spend` handler runs it over the *continuing* output's datum, so a
// quorum cannot rotate the params into a below-floor configuration. These
// fixtures build the smallest transaction that reaches that check.

const test_governor_script_hash: ByteArray =
  #"7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a7a"

const test_governor_own_ref: OutputReference =
  OutputReference {
    transaction_id: #"0101010101010101010101010101010101010101010101010101010101010101",
    output_index: 0,
  }

const test_governor_init_ref: OutputReference =
  OutputReference {
    transaction_id: #"0202020202020202020202020202020202020202020202020202020202020202",
    output_index: 1,
  }

const test_governor_lovelace: Int = 2_000_000

fn test_governor_params_value() -> Value {
  assets.from_lovelace(test_governor_lovelace)
    |> assets.add(test_governor_script_hash, da_params_asset_name, 1)
}

/// A DA-params update transaction: the params UTxO is spent from the governor
/// script, `signatories` witness it, and the NFT continues to the same address
/// carrying `continued_datum` and the same value.
fn test_governor_tx(
  input_datum: DaParamsDatum,
  continued_datum: DaParamsDatum,
  signatories: List<VerificationKeyHash>,
) -> Transaction {
  let script_address = address.from_script(test_governor_script_hash)
  let params_value = test_governor_params_value()
  let input_datum_data: Data = input_datum
  let continued_datum_data: Data = continued_datum
  Transaction {
    ..placeholder,
    inputs: [
      Input {
        output_reference: test_governor_own_ref,
        output: Output {
          address: script_address,
          value: params_value,
          datum: InlineDatum(input_datum_data),
          reference_script: None,
        },
      },
    ],
    outputs: [
      Output {
        address: script_address,
        value: params_value,
        datum: InlineDatum(continued_datum_data),
        reference_script: None,
      },
    ],
    extra_signatories: signatories,
  }
}

fn test_governor_spend(
  input_datum: DaParamsDatum,
  continued_datum: DaParamsDatum,
  signatories: List<VerificationKeyHash>,
) -> Bool {
  let redeemer: Data = 0
  da_params_governor.spend(
    test_governor_init_ref,
    test_max_committee_size,
    test_max_owner_count,
    Some(input_datum),
    redeemer,
    test_governor_own_ref,
    test_governor_tx(input_datum, continued_datum, signatories),
  )
}

/// The params the spend-seam tests start from: a 6-member committee at the F04
/// floor, and a 3-owner set whose `update_threshold` of two is also at its
/// floor.
fn test_governor_input_params() -> DaParamsDatum {
  test_params(6, 4, 3, 2)
}

/// Exactly `update_threshold` owners of `test_governor_input_params`, so the
/// owner quorum is met and the spend reaches the continuing-output checks.
fn test_governor_quorum() -> List<VerificationKeyHash> {
  [test_owner_hash(1), test_owner_hash(2)]
}

/// A DA-params initialisation transaction: the `init_ref` UTxO is consumed, one
/// params NFT is minted, and it lands at the governor script carrying
/// `minted_datum`.
fn test_governor_mint_tx(minted_datum: DaParamsDatum) -> Transaction {
  let script_address = address.from_script(test_governor_script_hash)
  let minted_datum_data: Data = minted_datum
  Transaction {
    ..placeholder,
    inputs: [
      Input {
        output_reference: test_governor_init_ref,
        output: Output {
          address: script_address,
          value: assets.from_lovelace(test_governor_lovelace),
          datum: NoDatum,
          reference_script: None,
        },
      },
    ],
    outputs: [
      Output {
        address: script_address,
        value: test_governor_params_value(),
        datum: InlineDatum(minted_datum_data),
        reference_script: None,
      },
    ],
    mint: assets.from_asset(test_governor_script_hash, da_params_asset_name, 1),
  }
}

fn test_governor_mint(minted_datum: DaParamsDatum) -> Bool {
  let redeemer: Data = 0
  da_params_governor.mint(
    test_governor_init_ref,
    test_max_committee_size,
    test_max_owner_count,
    redeemer,
    test_governor_script_hash,
    test_governor_mint_tx(minted_datum),
  )
}

// ## Q63 governed floor/drain invariants
//
// These three tests pin the *arithmetic* — `governed_threshold_floor` and the
// two constants — as absolute values against F04 §4 rather than against each
// other. They deliberately do not exercise a datum: that `valid_datum` applies
// the floor to `da_threshold` and `update_threshold` is pinned by the
// valid-boundary controls and the below-floor rejections below, and that the
// `spend` handler applies `valid_datum` to a continuing output is pinned by the
// spend-seam section.

/// Invariant 1 — majority DA floor. `governed_threshold_floor` reproduces
/// `max(2, ceil(2*set_len/3))` at a *sample* of set sizes, which is what makes
/// a bare majority of a committee insufficient once `valid_datum` floors
/// `da_threshold` with it.
///
/// Sample-pinned, not a full sweep: the sample deliberately straddles the
/// clamp boundary where the two-thirds ceiling overtakes the constant two
/// (`3 -> 2`, `4 -> 3`), and reaches the top of the representable range
/// (`100`, `255`, `256 == max_indexed_signer_count`) so a defect in the
/// large-`n` arithmetic cannot hide below the sample. The exhaustive
/// 0..max_indexed_signer_count agreement is pinned off-chain instead, by the
/// digest the `floor-arithmetic-provenance` check and the Q63 verifier both
/// recompute.
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
    governed_threshold_floor(100) == 67,
    governed_threshold_floor(255) == 170,
    governed_threshold_floor(max_indexed_signer_count) == 171,
  }
}

/// Invariant 2 — 2-of-N update floor. `governed_threshold_floor` never returns
/// less than two, and never exceeds its own argument for any set size from
/// `min_owner_count` to `max_indexed_signer_count`. The first half is what
/// leaves single-key capture of either threshold unrepresentable once
/// `valid_datum` applies the floor; the second is what keeps the floor
/// satisfiable rather than a bound that admits nothing.
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

/// Invariant 3 — owner-set drain protection. Pins the two constants and the
/// one arithmetic fact they rest on: `min_owner_count` is two, it is not below
/// `min_governed_threshold` (an owner set must be able to carry the threshold
/// floor, or the governor would admit no datum at all), and the floor at a
/// one-member set exceeds one. The last of those is what makes the drain
/// unrepresentable even with `min_owner_count` removed — see the constant's
/// own docstring for why the redundant check is kept anyway.
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
// The two threshold rejections each pin exactly one disposition: every field
// other than the one under test sits at or above its own governed floor, so the
// failure can only be the named violation.
//
// The drain rejection is deliberately the exception, and says so in its own
// name. No one-owner datum can violate exactly one bound, and the overlap is
// not removable by choosing a different `update_threshold`: at
// `update_threshold == 1` both `owner_len >= min_owner_count` and
// `update_threshold >= governed_threshold_floor(1) == 2` reject it (while
// `update_threshold <= owner_len` is satisfied), and at any
// `update_threshold >= 2` the floor is met but `update_threshold <= owner_len`
// fails instead — alongside `owner_len >= min_owner_count` either way. It
// therefore pins the class (a drained owner set is unrepresentable) rather
// than a single mechanism.

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

/// Rejection: draining the owner set to a single owner is unrepresentable.
///
/// Named for the overlap because the overlap is real: this datum
/// (`update_threshold == 1` over one owner) violates `owner_len >=
/// min_owner_count` *and* `update_threshold >= governed_threshold_floor(1) ==
/// 2` — two of the three owner bounds; `update_threshold <= owner_len` is
/// satisfied here. The two cannot be separated by picking a different
/// threshold, because any `update_threshold >= 2` trades the floor violation
/// for an `update_threshold <= owner_len` violation instead. The test
/// therefore pins that the class is rejected, not which bound did it.
/// `min_owner_count`'s docstring records which of them is redundant today and
/// why it is kept.
test da_params_governor_rejects_owner_set_drained_to_one_by_overlapping_bounds() fail {
  test_valid_datum(test_params(6, 4, 1, 1))
}

// ## Q63 single-member-committee consequence (review finding I2)
//
// A consequence of the F04 §4 floor that is worth pinning explicitly rather
// than leaving for an operator to rediscover: `governed_threshold_floor(1) ==
// 2 > 1`, so a one-member DA committee is unrepresentable — there is no
// `da_threshold` it could name. The same holds for a one-member owner set. Any
// deployment topology that assumed a single-signer DA committee is now invalid
// by construction, which is why the parent handoff has to move the node's
// single-key defaults.

/// Consequence, exhaustively: no `da_threshold` in `0..1` satisfies both
/// governed bounds over a one-member committee, so the committee size itself is
/// what is unrepresentable — not one unlucky threshold choice.
test da_params_governor_rejects_single_member_committee_thresholds() {
  no_threshold_satisfies_floor(1, 0)
}

/// Consequence, at the datum: a one-member committee naming the smallest
/// `da_threshold` the floor allows (`2`) is rejected because that threshold
/// exceeds the committee. Single disposition — the owner set is at its own
/// floor, and `da_threshold >= governed_threshold_floor(1) == 2` holds, so
/// `da_threshold <= committee_len` is the only bound left to fail.
test da_params_governor_rejects_single_member_committee_datum() fail {
  test_valid_datum(test_params(1, 2, 4, 3))
}

// ## Q63 mint-seam datum enforcement
//
// The mint path has its own `valid_datum` call, in
// `params_output_datum_at_script`, and it is the one that makes initialising
// the governor with single-key params impossible — the claim the parent
// handoff rests on when it says the node's current DA defaults are now
// unmintable. It needs its own differential pair: deleting that `expect` left
// every other selector green.

/// Mint-seam control: initialising a 6-member committee whose `da_threshold`
/// sits exactly on the F04 floor is accepted, so the negative below is not a
/// policy that refuses every initialisation.
test da_params_governor_mint_control_initial_params_at_floor_accepted() {
  test_governor_mint(test_params(6, 4, 3, 2))
}

/// Mint-seam rejection: the same initialisation transaction — same `init_ref`
/// input, same single minted NFT, same script output — differing only in that
/// the minted datum names a bare-majority `da_threshold` of `3` over the
/// 6-member committee. The control passing is what establishes that every
/// other check in the handler is satisfied, so the minted datum's governed
/// floor is the only bound left to fail.
test da_params_governor_mint_rejects_initial_datum_below_floor() fail {
  test_governor_mint(test_params(6, 3, 3, 2))
}

// ## Q63 spend-seam datum enforcement (review finding D1)
//
// Every test above reaches `valid_datum` directly. That leaves the seam that
// actually matters unpinned: dropping `expect valid_datum(...)` from
// `params_output_datum_at_address` would let an owner quorum rotate the params
// into a below-floor configuration while every direct-call test stayed green.
// These two exercise the real `spend` handler, so the negative fails closed
// only because the handler runs the check on the continuing output.

/// Spend-seam control: a quorum-signed update that raises `da_threshold` from
/// the floor (`4`) to `5` over the same 6-member committee is accepted, so the
/// negative below is not a handler that rejects every update.
test da_params_governor_spend_control_quorum_signed_update_accepted() {
  test_governor_spend(
    test_governor_input_params(),
    test_params(6, 5, 3, 2),
    test_governor_quorum(),
  )
}

/// Spend-seam rejection: the same quorum-signed update, differing only in that
/// the continuing datum drops `da_threshold` to a bare majority `3` of the
/// 6-member committee, is rejected. The input datum is valid, the quorum is
/// met, the NFT continues to the same address and the value is not drained, so
/// the continuing output's governed floor is the only bound that can fail.
test da_params_governor_spend_rejects_continued_datum_below_floor() fail {
  test_governor_spend(
    test_governor_input_params(),
    test_params(6, 3, 3, 2),
    test_governor_quorum(),
  )
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
