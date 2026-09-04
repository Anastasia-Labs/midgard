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

/// F04 §4 governed threshold floor: `ceil(2*set_len/3)`, defined for
/// `set_len >= 1`.
///
/// `ceil(2*set_len/3)` is evaluated as `(2*set_len + 2) / 3` under integer
/// division. The two-thirds term is what keeps a quorum from dropping either
/// threshold to a sub-two-thirds slice of its own set.
///
/// There is no lower clamp, and there is no set-size minimum beside it. Two
/// owner rulings brought it here:
///
/// - **2026-08-11 (ruling 4)** retired the floor's `max(2, …)` clamp —
///   "governor floors become `ceil(2n/3)` with n ≥ 1" — so a one-member DA
///   committee floors at one and the single-key attest loop is representable.
/// - **2026-08-13 (in-session, recorded on #602)** dropped the owner-set
///   minimum to one. A one-owner set carrying `update_threshold == 1` is now
///   representable, so single-key *governance* — one key rotating the committee
///   and both thresholds — is accepted behaviour by owner decision rather than
///   an unrepresentable state.
///
/// What still binds at every set size is this floor itself: it never returns
/// less than one, so no owner set and no committee, however small, can name a
/// threshold of zero. That is the whole of what survives of the drain
/// protection, and `da_params_governor_invariant_owner_set_drain_protection`
/// measures exactly it.
///
/// Source: `docs/midgard/decisions/0002-canonical-v1-goal-economics-and-margins.md`
/// §4 — `da_threshold >= ceil(2*committee_len/3)` and
/// `update_threshold >= ceil(2*owner_len/3)`, both for set sizes `>= 1`.
fn governed_threshold_floor(set_len: Int) -> Int {
  ( 2 * set_len + 2 ) / 3
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

  // No `expect owner_len >= min_owner_count` here. The 2026-08-13 owner ruling
  // dropped that minimum to one, and at one it would be a guard that cannot
  // fail: `sorted_unique_len_at_most` opens with `expect [first, ..rest] =
  // keys`, so an empty owner set already aborts before a count exists to
  // compare. The non-emptiness refusal is therefore structural rather than
  // restated, and it is measured directly by
  // `da_params_governor_rejects_empty_owner_set` so nothing here rests on the
  // reader trusting that argument.
  let owner_len = sorted_unique_len_at_most(
    datum.owners,
    max_owner_count,
    verification_key_hash_byte_count,
  )
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

/// True when `governed_threshold_floor` is at least `least` for every set size
/// in `set_len..max_len`.
///
/// The amended floor has no lower clamp and no set-size minimum beside it, so
/// "no set can name a threshold of zero" is the one lower bound left to
/// measure. This walker measures it over the whole representable range rather
/// than asserting it from the shape of the arithmetic.
fn floor_at_least_from(set_len: Int, max_len: Int, least: Int) -> Bool {
  if set_len > max_len {
    True
  } else if governed_threshold_floor(set_len) < least {
    False
  } else {
    floor_at_least_from(set_len + 1, max_len, least)
  }
}

/// How many integers in `threshold..set_len` satisfy both
/// `>= governed_threshold_floor(set_len)` and `<= set_len`.
///
/// This is arithmetic over the floor function, not a statement about
/// `valid_datum`; it happens to mirror the shape of the two threshold bounds
/// `valid_datum` applies, which is why a count of zero corresponds to a set
/// size no datum could name a threshold for, and a positive count to one that
/// can.
/// Exhaustive rather than an appeal to monotonicity, so "exactly one threshold
/// works" is measured over the whole candidate range instead of asserted.
fn thresholds_satisfying_floor(set_len: Int, threshold: Int, count: Int) -> Int {
  if threshold > set_len {
    count
  } else if threshold >= governed_threshold_floor(set_len) {
    thresholds_satisfying_floor(set_len, threshold + 1, count + 1)
  } else {
    thresholds_satisfying_floor(set_len, threshold + 1, count)
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
// These three tests pin the *arithmetic* — `governed_threshold_floor`, which
// after the two owner rulings is the whole of the governed bound — as absolute
// values against F04 §4 rather than against each other. There is no companion
// constant left to pin: `min_governed_threshold` and `min_owner_count` are both
// retired. They deliberately do not exercise a datum: that `valid_datum`
// applies the floor to `da_threshold` and `update_threshold` is pinned by the
// valid-boundary controls and the below-floor rejections below, and that the
// `spend` handler applies `valid_datum` to a continuing output is pinned by the
// spend-seam section.

/// Invariant 1 — majority DA floor. `governed_threshold_floor` reproduces
/// `ceil(2*set_len/3)` at a *sample* of set sizes, which is what makes a bare
/// majority of a committee insufficient once `valid_datum` floors
/// `da_threshold` with it.
///
/// The `1 -> 1` row is the amended floor itself, and is the row that reddens if
/// the lower clamp is ever restored: under the retired `max(2, ceil(2n/3))` the
/// floor at a one-member committee was two, which no such committee could name.
/// The `2 -> 2` and `3 -> 2` rows are what show the amendment moved nothing
/// else — they are the sizes where the retired clamp used to bind, and both
/// still floor at two because the two-thirds ceiling already does.
///
/// Sample-pinned, not a full sweep: the sample covers the whole lifted region
/// and the boundary where the ceiling overtakes two (`3 -> 2`, `4 -> 3`), then
/// reaches the top of the representable range (`100`, `255`,
/// `256 == max_indexed_signer_count`) so a defect in the large-`n` arithmetic
/// cannot hide below the sample. The exhaustive 0..max_indexed_signer_count
/// agreement is pinned off-chain instead, by the digest the
/// `floor-arithmetic-provenance` check and the Q63 verifier both recompute.
test da_params_governor_invariant_da_threshold_majority_floor() {
  and {
    governed_threshold_floor(1) == 1,
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

/// Invariant 2 — the update-threshold floor is the same two-thirds ceiling, and
/// it is satisfiable at every set size the governor represents.
///
/// Satisfiability is the load-bearing half: `governed_threshold_floor` never
/// exceeds its own argument for any `set_len` from one to
/// `max_indexed_signer_count`, so the floor is a bound that admits a threshold
/// rather than one that admits nothing. Exhaustive over that range rather than
/// sampled.
///
/// This test used to be named for a "2-of-N update floor" and asserted that the
/// floor never returns less than two over the owner-set sizes the old
/// `min_owner_count` admitted. Both halves of that claim are gone: the
/// 2026-08-13 owner ruling dropped the owner-set minimum to one, and at one the
/// floor is one. Renaming rather than weakening in place, so a reader cannot
/// mistake the surviving claim for the retired one.
test da_params_governor_invariant_update_threshold_floor() {
  and {
    governed_threshold_floor(1) == 1,
    governed_threshold_floor(2) == 2,
    governed_threshold_floor(3) == 2,
    governed_threshold_floor(4) == 3,
    floor_is_satisfiable_from(1, max_indexed_signer_count),
  }
}

/// Invariant 3 — what survives of owner-set drain protection.
///
/// Two owner rulings removed the rest of it. The 2026-08-11 ruling retired the
/// floor's lower clamp; the 2026-08-13 ruling dropped the owner-set minimum to
/// one, so a one-owner set at `update_threshold == 1` is representable and
/// single-key governance rotation is accepted behaviour. What is left is this:
/// the floor is at least one at *every* set size, so no owner set and no
/// committee — however small — can name a threshold of zero. A zero threshold
/// would be governance with no signature at all, which is the one drain neither
/// ruling licensed.
///
/// Exhaustive over `1..max_indexed_signer_count` rather than argued from the
/// shape of the arithmetic, and paired with the `1 -> 1` value so the bound is
/// pinned tight rather than merely "positive". The empty owner set is refused
/// structurally and is measured separately by
/// `da_params_governor_rejects_empty_owner_set`, because it is a datum
/// disposition rather than arithmetic.
test da_params_governor_invariant_owner_set_drain_protection() {
  and {
    governed_threshold_floor(1) == 1,
    floor_at_least_from(1, max_indexed_signer_count, 1),
  }
}

// ## Q63 valid-boundary controls
//
// Both controls sit exactly on the F04 floor, so they prove the governed
// bound admits the smallest configuration it is meant to admit rather than
// rejecting everything.

/// Control: a 6-member committee with `da_threshold` exactly at the F04 floor
/// `ceil(2*6/3) == 4` is accepted.
test da_params_governor_control_da_threshold_at_floor_accepted() {
  test_valid_datum(test_params(6, 4, 3, 2))
}

/// Control: a 4-owner set with `update_threshold` exactly at the F04 floor
/// `ceil(2*4/3) == 3` is accepted.
test da_params_governor_control_update_threshold_at_floor_accepted() {
  test_valid_datum(test_params(3, 2, 4, 3))
}

// ## Q63 below-floor and drain rejection classes
//
// All three rejections pin exactly one disposition: every field other than the
// one under test sits at or above its own governed bound, so the failure can
// only be the named violation.
//
// The third rejection has now been re-scoped twice, and the history matters
// because the class it names kept shrinking. Originally it was "an owner set
// drained to one key is unrepresentable", and it failed on two overlapping
// bounds at once. The 2026-08-11 ruling removed the overlap by lowering
// `governed_threshold_floor(1)` to one, leaving `owner_len >= min_owner_count`
// as the single rejecting bound. The 2026-08-13 ruling then removed that
// minimum outright, so a one-owner set is *accepted* — see the single-owner
// governance section below, which pins the acceptance positively.
//
// What survives as a rejection is the drain the rulings did not license: an
// owner set drained past one, to zero. That is refused structurally, by
// `sorted_unique_len_at_most`'s opening `expect [first, ..rest] = keys`, and
// the test below is the measurement of it.

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

/// Rejection: draining the owner set to *zero* owners is unrepresentable.
///
/// This is the drain neither owner ruling licensed, and the only one left. It
/// is refused before any threshold bound is reached: `sorted_unique_len_at_most`
/// opens with `expect [first, ..rest] = keys`, which aborts on the empty list,
/// so there is never an `owner_len` to compare a threshold against.
///
/// Measured rather than argued, because that refusal is structural — it is a
/// consequence of how the length walker is written, not a line anyone declared
/// as a bound, and a future rewrite of the walker could remove it silently.
/// This test is what would catch that.
test da_params_governor_rejects_empty_owner_set() fail {
  test_valid_datum(test_params(6, 4, 0, 1))
}

// ## Q63 single-member-committee consequence (review finding I2)
//
// This section used to pin the opposite claim, and the inversion is the first
// of the F04 §4 amendment's two observable effects.
// `governed_threshold_floor(1)` was two, so a one-member DA committee could
// name no `da_threshold` at all and every single-signer DA topology was invalid
// by construction. The 2026-08-11 owner ruling lifted that: the floor at one is
// now one, a one-member committee attesting 1-of-1 is representable, and the
// node's single-key configuration is a warned-about deployment shape rather
// than a refused one.
//
// The owner set followed two days later, under its own ruling — see the
// single-owner governance section below. Both sides of the datum now admit a
// set of one.

/// Consequence, exhaustively: over a one-member committee exactly one
/// `da_threshold` in `0..1` satisfies both governed bounds. Counting rather
/// than existence-checking is deliberate — it says the admissible threshold is
/// `1` and nothing else crept in, so the lift is the single intended value
/// rather than a bound that stopped constraining.
test da_params_governor_accepts_single_member_committee_thresholds() {
  thresholds_satisfying_floor(1, 0, 0) == 1
}

/// Consequence, at the datum: a one-member committee attesting 1-of-1 is
/// accepted. The owner set sits at its own floor, so this is the single-key
/// attest-loop configuration the ruling accepted, admitted by the real
/// `valid_datum` rather than by the arithmetic alone.
test da_params_governor_accepts_single_member_committee_datum() {
  test_valid_datum(test_params(1, 1, 4, 3))
}

/// Retained negative, uncited by the Q63 artifact: the lift did not disable the
/// upper bound. A one-member committee naming `da_threshold == 2` is still
/// rejected because that threshold exceeds the committee. Single disposition —
/// the owner set is at its own floor and `2 >= governed_threshold_floor(1) ==
/// 1` holds, so `da_threshold <= committee_len` is the only bound left to
/// fail.
test da_params_governor_rejects_single_member_committee_threshold_above_size() fail {
  test_valid_datum(test_params(1, 2, 4, 3))
}

// ## Q63 single-owner governance consequence (2026-08-13 owner ruling)
//
// The owner-side counterpart of the section above, and the newer of the two
// lifts. The 2026-08-13 in-session owner ruling recorded on #602 dropped the
// governor's owner-set minimum to one, so a single owner carrying
// `update_threshold == 1` is representable and can exercise the full governance
// power: rotating the committee and both thresholds on its own signature.
//
// That is *accepted behaviour by owner decision*, not an oversight, so it is
// pinned positively rather than left to be inferred from the absence of a
// rejection. The negative that survives is pinned beside it: one owner still
// cannot name a threshold larger than the set, and zero owners are still
// refused (`da_params_governor_rejects_empty_owner_set`).

/// Consequence, at the datum: a one-owner set governing at `update_threshold`
/// one is accepted. The committee sits at its own floor, so the owner bounds
/// are the only ones under test.
test da_params_governor_accepts_single_owner_datum() {
  test_valid_datum(test_params(6, 4, 1, 1))
}

/// Consequence, at the seam that matters: a single owner, signing alone,
/// rotates the committee from six members to three and moves `da_threshold`
/// from four to two with it — the continuing datum is accepted and the spend
/// succeeds.
///
/// `update_threshold` is deliberately not moved here, because over a set of one
/// it cannot be: `governed_threshold_floor(1) == 1` and `update_threshold <=
/// owner_len == 1` pin it to exactly one on both sides of the rotation. Holding
/// the owner set at one is the whole point of this test — it is what makes the
/// single signature a *quorum* — so the governed threshold that can move is the
/// one that does, and the owner-side bound is measured by the datum tests
/// beside this one.
///
/// This is the strongest positive statement of the ruling, and deliberately
/// stronger than the datum test above: it runs the real `spend` handler, so it
/// measures that one key satisfies `owner_quorum_met` at `update_threshold ==
/// 1` *and* that the rotated params pass `valid_datum` on the continuing
/// output. Single-key governance rotation is representable end to end.
test da_params_governor_accepts_single_owner_governance_rotation() {
  test_governor_spend(
    test_params(6, 4, 1, 1),
    test_params(3, 2, 1, 1),
    [test_owner_hash(1)],
  )
}

/// Retained negative, uncited by the Q63 artifact: dropping the owner-set
/// minimum did not disable the owner upper bound. A one-owner set naming
/// `update_threshold == 2` is still rejected because that threshold exceeds the
/// set. Single disposition — the committee is at its own floor and
/// `2 >= governed_threshold_floor(1) == 1` holds, so `update_threshold <=
/// owner_len` is the only bound left to fail.
test da_params_governor_rejects_single_owner_threshold_above_size() fail {
  test_valid_datum(test_params(6, 4, 1, 2))
}

// ## Q63 mint-seam datum enforcement
//
// The mint path has its own `valid_datum` call, in
// `params_output_datum_at_script`, and it is the one that binds the governed
// floors to the params a deployment is *initialised* with rather than only to
// the ones a later update continues. Since both owner rulings, single-key
// params are not what it refuses: a one-member committee and a one-owner set
// are mintable, and the node's single-key defaults initialise with warnings
// rather than being rejected here. What this seam still refuses is an initial
// datum below the governed floors. It needs its own differential pair:
// deleting that `expect` left every other selector green.

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
