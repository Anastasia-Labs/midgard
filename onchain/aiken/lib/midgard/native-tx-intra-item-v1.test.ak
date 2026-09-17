//// Seam vectors for intra-item access — `docs/spec/midgard-tx.md` §11.1
//// (the Value bookmark) and §11.2 (the Canonical-Data Acceptor).
////
//// Everything here is asserted at the module's public seam:
//// `open_value_bookmark` / `value_quantity_of` / `value_lovelace`, and
//// `datum_child_*` / `datum_integer_at_v1` / `datum_alternative_at_v1` /
//// `is_materialisable_plutus_data_v1`. No test names a private helper or pins
//// an intermediate offset, so the scan's internals stay free to change.
////
//// Three claims, and how each is established:
////
////   * **Both cases fit the budget** (§11.4). Cost claims, so they are
////     measured rather than asserted: `budget_*` below are the rows §11.4
////     quotes, and their assertions only pin that the cheap path and the
////     materialising path compute the same answer, so that the two numbers are
////     comparable at all.
////   * **A zero answer is evidence** (§11.1). `value_bookmark_*` walks a
////     64-unit value in canonical order and reads every quantity, reads past
////     absent units, and then shows that both halves of what makes absence
////     sound — the structural pass at open and the monotone floor — refuse when
////     removed: an unordered value cannot be opened, and a repeated or
////     backwards lookup aborts instead of answering `0`.
////   * **The #567 residual is closed** (§11.2). `nested_bignum_*` is the test
////     that could not be written before this ticket: a bignum nested inside an
////     otherwise ordinary canonical datum used to reach `cbor.deserialise` and
////     abort the machine (runner RC=101), so there was no verdict to assert.
////     The recursive acceptor turns it into a decline, and a decline is
////     assertable. The vectors put the bignum **first and in the middle** of its
////     container as well as last, because a flag fold that returned the last
////     child's answer instead of the conjunction would pass every last-position
////     vector ever written.
////
//// **How a forgery is spelled here.** `ValueBookmarkV1` is opaque, so no test
//// can write a bookmark literal — which is the point, since that capability is
//// exactly the one a prover must not have. Negative vectors therefore attack
//// the way a prover would: they hand `open_value_bookmark` a malformed value,
//// or drive a legitimately-obtained bookmark backwards.

use aiken/cbor
use aiken/collection/list
use aiken/primitive/bytearray
use midgard/canonical_plutus_data_v1.{
  is_canonical_plutus_data_v1, is_materialisable_plutus_data_v1,
}
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_output}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardTxOutput, MidgardValue, PubKeyCredential,
}
use midgard/ledger_output_v1
use midgard/native_tx_intra_item_v1.{
  datum_alternative_at_v1, datum_bytes_at_v1, datum_child_bytes_v1,
  datum_child_extent_v1, datum_integer_at_v1, max_asset_name_bytes,
  open_value_bookmark, value_lovelace, value_quantity_of,
}
use midgard/script_context_v1

// ---------------------------------------------------------------------------
// Fixtures
// ---------------------------------------------------------------------------

/// A 28-byte payment hash.
const payment_hash: ByteArray =
  #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

/// The 29-byte Midgard address payload the fixtures carry: `encode_midgard_address`'s
/// header for an unprotected key-only address on network 0 (`6·16 + 0`), then
/// the payment hash. Twenty-nine bytes is the shorter of the two widths §5.5
/// admits.
const address_payload: ByteArray =
  #"60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

/// Policy ids that are already in §5.5 canonical order for `p in 0..7`: same
/// length, so the comparison falls through to the byte compare.
fn policy_id(index: Int) -> ByteArray {
  bytearray.concat(
    bytearray.from_int_big_endian(index, 1),
    #"b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0b0",
  )
}

/// Asset names of a constant four bytes, so ordering inside a policy group is
/// also a plain byte compare.
fn asset_name(index: Int) -> ByteArray {
  bytearray.concat(#"c0c0c0", bytearray.from_int_big_endian(index, 1))
}

fn uint_bytes(value: Int) -> ByteArray {
  expect value >= 0
  if value <= 23 {
    bytearray.from_int_big_endian(value, 1)
  } else if value <= 255 {
    bytearray.concat(#"18", bytearray.from_int_big_endian(value, 1))
  } else if value <= 65535 {
    bytearray.concat(#"19", bytearray.from_int_big_endian(value, 2))
  } else {
    expect value <= 4294967295
    bytearray.concat(#"1a", bytearray.from_int_big_endian(value, 4))
  }
}

fn bytes_item(payload: ByteArray) -> ByteArray {
  let length = bytearray.length(payload)
  expect length <= 255
  if length <= 23 {
    bytearray.concat(bytearray.from_int_big_endian(64 + length, 1), payload)
  } else {
    #"58"
      |> bytearray.concat(bytearray.from_int_big_endian(length, 1))
      |> bytearray.concat(payload)
  }
}

fn map_header(entries: Int) -> ByteArray {
  expect entries >= 0
  expect entries <= 23
  bytearray.from_int_big_endian(160 + entries, 1)
}

/// Builds a §5.5 output item byte-for-byte, so a fixture can be perturbed in
/// exactly one place. The canonical builder (`encode_midgard_tx_output`) is
/// used as the cross-check in `hand_built_fixtures_match_the_canonical_encoder`
/// rather than as the source, because it would refuse every negative vector
/// below.
fn output_item(
  lovelace: Int,
  groups: List<Pair<ByteArray, List<Pair<ByteArray, Int>>>>,
) -> ByteArray {
  output_item_with_address(address_payload, lovelace, groups)
}

fn output_item_with_address(
  address: ByteArray,
  lovelace: Int,
  groups: List<Pair<ByteArray, List<Pair<ByteArray, Int>>>>,
) -> ByteArray {
  #"a2"
    |> bytearray.concat(#"00")
    |> bytearray.concat(bytes_item(address))
    |> bytearray.concat(#"01")
    |> bytearray.concat(#"82")
    |> bytearray.concat(uint_bytes(lovelace))
    |> bytearray.concat(map_header(list.length(groups)))
    |> bytearray.concat(
        list.foldr(
          groups,
          #"",
          fn(group, tail) {
            let Pair(policy, assets) = group
            bytes_item(policy)
              |> bytearray.concat(map_header(list.length(assets)))
              |> bytearray.concat(
                  list.foldr(
                    assets,
                    #"",
                    fn(asset, asset_tail) {
                      let Pair(name, quantity) = asset
                      bytes_item(name)
                        |> bytearray.concat(uint_bytes(quantity))
                        |> bytearray.concat(asset_tail)
                    },
                  ),
                )
              |> bytearray.concat(tail)
          },
        ),
      )
}

fn asset_group(
  policy_index: Int,
  asset_count: Int,
) -> Pair<ByteArray, List<Pair<ByteArray, Int>>> {
  Pair(
    policy_id(policy_index),
    list.map(
      list.range(0, asset_count - 1),
      fn(asset_index) {
        Pair(asset_name(asset_index), quantity_of(policy_index, asset_index))
      },
    ),
  )
}

/// Distinct per unit, and small enough to stay in the one-byte uint form so the
/// fixture's byte widths do not vary with the quantity.
fn quantity_of(policy_index: Int, asset_index: Int) -> Int {
  1 + policy_index * 8 + asset_index
}

/// Eight policies of eight assets each: 64 units, which is the same fixture
/// size §10.7's authenticate-once rows use, so the two budget tables read
/// against each other.
fn wide_value_item() -> ByteArray {
  output_item(
    5_000_000,
    list.map(
      list.range(0, 7),
      fn(policy_index) { asset_group(policy_index, 8) },
    ),
  )
}

fn canonical_output(
  groups: List<Pair<ByteArray, List<Pair<ByteArray, Int>>>>,
) -> MidgardTxOutput {
  MidgardTxOutput {
    address: MidgardAddress {
      protected: False,
      network_id: 0,
      payment_credential: PubKeyCredential(payment_hash),
      stake_credential: None,
    },
    value: MidgardValue {
      lovelace: 5_000_000,
      assets: list.flat_map(
        groups,
        fn(group) {
          let Pair(policy, assets) = group
          list.map(
            assets,
            fn(asset) {
              let Pair(name, quantity) = asset
              Pair(bytearray.concat(policy, name), quantity)
            },
          )
        },
      ),
    },
    datum_cbor: None,
    script_ref: None,
  }
}

fn wide_groups() -> List<Pair<ByteArray, List<Pair<ByteArray, Int>>>> {
  list.map(list.range(0, 7), fn(policy_index) { asset_group(policy_index, 8) })
}

/// Every unit of the wide fixture, in the canonical order the bookmark
/// requires.
fn wide_units() -> List<Pair<Int, Int>> {
  list.flat_map(
    list.range(0, 7),
    fn(policy_index) {
      list.map(
        list.range(0, 7),
        fn(asset_index) { Pair(policy_index, asset_index) },
      )
    },
  )
}

// ---------------------------------------------------------------------------
// §11.1 the fixture itself
// ---------------------------------------------------------------------------

/// The hand-built fixture is the canonical encoding, byte for byte. Without
/// this the negative vectors below would be attacking a shape the format never
/// produces, and the budget rows would be measuring the wrong bytes.
test hand_built_fixtures_match_the_canonical_encoder() {
  encode_midgard_tx_output(canonical_output(wide_groups())) == wide_value_item()
}

// ---------------------------------------------------------------------------
// §11.1 Case A — the Value bookmark
// ---------------------------------------------------------------------------

/// One sweep, every unit, in order. This is the access pattern a per-asset
/// conservation rule has, and the whole point of the bookmark is that the
/// sweep does not restart.
test value_bookmark_reads_every_unit_in_one_sweep() {
  let item = wide_value_item()
  let Pair(read, _final) = list.foldl(
    wide_units(),
    Pair(0, open_value_bookmark(item)),
    fn(unit, state) {
      let Pair(policy_index, asset_index) = unit
      let Pair(count, bookmark) = state
      let Pair(quantity, advanced) = value_quantity_of(
        bookmark,
        policy_id(policy_index),
        asset_name(asset_index),
      )
      expect quantity == quantity_of(policy_index, asset_index)
      Pair(count + 1, advanced)
    },
  )
  and {
    read == 64,
    value_lovelace(open_value_bookmark(item)) == 5_000_000,
  }
}

/// A zero answer, three ways: a policy the value does not carry, an asset name
/// the policy does not carry, and a unit past the last group. The sweep keeps
/// going afterwards, which is what makes the answer a step rather than a stop.
test value_bookmark_reports_absent_units_as_zero() {
  let item = wide_value_item()
  let bookmark = open_value_bookmark(item)
  // Policy 0 has assets 0..7; ask for one it does not carry.
  let Pair(missing_asset, after_asset) = value_quantity_of(
    bookmark,
    policy_id(0),
    asset_name(9),
  )
  // Policy 2 exists, so the sweep crosses policy 1 to reach it.
  let Pair(present, after_present) = value_quantity_of(
    after_asset,
    policy_id(2),
    asset_name(3),
  )
  // Past the last group entirely.
  let Pair(missing_policy, _done) = value_quantity_of(
    after_present,
    policy_id(9),
    asset_name(0),
  )
  and {
    missing_asset == 0,
    present == quantity_of(2, 3),
    missing_policy == 0,
  }
}

/// The cheap path and the materialising path agree on every unit. Without this
/// the budget comparison would be between two different questions.
test value_bookmark_agrees_with_the_materialising_decoder() {
  let item = wide_value_item()
  expect Some(decoded) = ledger_output_v1.decode_canonical_output(item)
  let Pair(agreed, _final) = list.foldl(
    wide_units(),
    Pair(True, open_value_bookmark(item)),
    fn(unit, state) {
      let Pair(policy_index, asset_index) = unit
      let Pair(ok, bookmark) = state
      let Pair(quantity, advanced) = value_quantity_of(
        bookmark,
        policy_id(policy_index),
        asset_name(asset_index),
      )
      let unit_key = bytearray.concat(
        policy_id(policy_index),
        asset_name(asset_index),
      )
      let materialised = when
        list.find(decoded.value.assets, fn(pair) { pair.1st == unit_key })
      is {
        None -> 0
        Some(pair) -> pair.2nd
      }
      Pair(and {
          ok,
          quantity == materialised,
        }, advanced)
    },
  )
  and {
    agreed,
    decoded.value.lovelace == value_lovelace(open_value_bookmark(item)),
  }
}

/// §11.1's monotone floor. Asking twice for the same unit aborts rather than
/// answering `0`: at that position the sweep cannot tell "absent" from
/// "already passed", and answering would fabricate absence evidence for an
/// asset the value holds.
test value_bookmark_refuses_a_repeated_lookup() fail {
  let bookmark = open_value_bookmark(wide_value_item())
  let Pair(_quantity, advanced) = value_quantity_of(
    bookmark,
    policy_id(2),
    asset_name(3),
  )
  expect _ = value_quantity_of(advanced, policy_id(2), asset_name(3))
  True
}

/// The same floor, backwards across policy groups.
test value_bookmark_refuses_a_backwards_lookup() fail {
  let bookmark = open_value_bookmark(wide_value_item())
  let Pair(_quantity, advanced) = value_quantity_of(
    bookmark,
    policy_id(4),
    asset_name(0),
  )
  expect _ = value_quantity_of(advanced, policy_id(1), asset_name(0))
  True
}

/// The structural pass at open, first half: policy groups strictly increase.
/// An unordered value cannot be opened at all, because a sweep over one could
/// pass a unit and then meet it.
test value_bookmark_refuses_unordered_policies() fail {
  let item = output_item(1, [asset_group(3, 2), asset_group(1, 2)])
  expect _ = open_value_bookmark(item)
  True
}

/// Same rule, applied to a duplicate rather than an inversion: `precedes` is
/// strict, so equal keys refuse too.
test value_bookmark_refuses_a_duplicate_policy() fail {
  let item = output_item(1, [asset_group(1, 2), asset_group(1, 2)])
  expect _ = open_value_bookmark(item)
  True
}

/// The structural pass at open, second half: asset names strictly increase
/// inside a policy group.
test value_bookmark_refuses_unordered_asset_names() fail {
  let item = output_item(
    1,
    [Pair(policy_id(0), [Pair(asset_name(5), 7), Pair(asset_name(2), 7)])],
  )
  expect _ = open_value_bookmark(item)
  True
}

/// §5.5: quantities are strictly positive, so a zero entry — a second spelling
/// of an absent asset — refuses.
test value_bookmark_refuses_a_zero_quantity() fail {
  let item = output_item(1, [Pair(policy_id(0), [Pair(asset_name(0), 0)])])
  expect _ = open_value_bookmark(item)
  True
}

/// §5.5: a policy with no assets has no canonical spelling — the policy would
/// simply be absent.
test value_bookmark_refuses_an_empty_policy_group() fail {
  let item = output_item(1, [Pair(policy_id(0), [])])
  expect _ = open_value_bookmark(item)
  True
}

/// §5.3's width assertion, inside the item: a policy id is 28 bytes.
test value_bookmark_refuses_a_short_policy_id() fail {
  let item = output_item(1, [Pair(#"b0b0b0b0", [Pair(asset_name(0), 7)])])
  expect _ = open_value_bookmark(item)
  True
}

/// §5.5: the address payload is 29 bytes (payment only) or 57 (with a stake
/// hash), and nothing else. A 28-byte one is well-formed CBOR whose value map
/// parses perfectly — the shift lands the `01` key one byte early and every
/// later read is consistent — so without this check the bookmark would happily
/// serve an output the encoder cannot produce.
test value_bookmark_refuses_a_non_canonical_address_width() fail {
  let item = output_item_with_address(
    #"60aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
    1,
    [asset_group(0, 1)],
  )
  expect _ = open_value_bookmark(item)
  True
}

// ---------------------------------------------------------------------------
// §11.1 the output envelope, one check at a time
// ---------------------------------------------------------------------------

/// The §5.5 output envelope assembled from the five things
/// `value_offset_in_output` reads before it will trust the value's offset, so a
/// vector can spoil exactly one of them and leave every other check satisfiable.
///
/// The envelope never looks at the address *payload*; what it checks is that the
/// **declared** width puts the value key where §5.5 says it is. So `declared`
/// doubles as the filler's length here, and the fixture stays one parameter per
/// check — which is what makes each vector below isolate one refusal instead of
/// tripping the next one on its way out.
fn raw_output(
  map_head: Int,
  address_key: Int,
  address_wrapper: Int,
  declared: Int,
  value_key: Int,
  value_bytes: ByteArray,
) -> ByteArray {
  bytearray.from_int_big_endian(map_head, 1)
    |> bytearray.concat(bytearray.from_int_big_endian(address_key, 1))
    |> bytearray.concat(bytearray.from_int_big_endian(address_wrapper, 1))
    |> bytearray.concat(bytearray.from_int_big_endian(declared, 1))
    |> bytearray.concat(filler(declared))
    |> bytearray.concat(bytearray.from_int_big_endian(value_key, 1))
    |> bytearray.concat(value_bytes)
}

fn filler(length: Int) -> ByteArray {
  list.foldr(
    list.range(1, length),
    #"",
    fn(_index, tail) { bytearray.concat(#"aa", tail) },
  )
}

/// `[1, {}]` — the shortest §5.5 value, so the vectors above it are about the
/// envelope and never about the value.
const ada_only_value: ByteArray = #"8201a0"

/// The control. Every negative vector below is this fixture with one byte
/// changed, so a refusal there is attributable to that byte and not to the
/// fixture being unopenable in the first place.
test the_raw_envelope_fixture_opens() {
  let bookmark = open_value_bookmark(
    raw_output(0xa2, 0x0, 0x58, 29, 0x1, ada_only_value),
  )
  let Pair(quantity, _advanced) = value_quantity_of(
    bookmark,
    policy_id(0),
    asset_name(0),
  )
  and {
    value_lovelace(bookmark) == 1,
    quantity == 0,
  }
}

/// §5.5: the output is a map of two, three or four entries by presence. `a5`
/// declares five, which no §5.3 output encoding can produce.
test value_bookmark_refuses_a_non_output_map_head() fail {
  expect _ = open_value_bookmark(
    raw_output(0xa5, 0x0, 0x58, 29, 0x1, ada_only_value),
  )
  True
}

/// §5.5: key `0` is the address, and it comes first. Nothing later in the
/// envelope re-reads this byte, so an output whose first key is `2` parses
/// perfectly from here on — the check is the only thing between it and a
/// bookmark.
test value_bookmark_refuses_a_first_key_that_is_not_the_address() fail {
  expect _ = open_value_bookmark(
    raw_output(0xa2, 0x2, 0x58, 29, 0x1, ada_only_value),
  )
  True
}

/// §5.5: the address payload is 29 or 57 bytes, so its canonical wrapper is
/// always the two-byte `58 LL` form. `57` is the one-byte form for a 23-byte
/// string — well-formed CBOR, and a shape the encoder cannot emit.
test value_bookmark_refuses_a_non_two_byte_address_wrapper() fail {
  expect _ = open_value_bookmark(
    raw_output(0xa2, 0x0, 0x57, 29, 0x1, ada_only_value),
  )
  True
}

/// §5.5: key `1` is the value, and the declared address width is what says
/// where it starts. A `2` there is the shape an output would have if the
/// address width and the map contents disagreed.
test value_bookmark_refuses_a_second_key_that_is_not_the_value() fail {
  expect _ = open_value_bookmark(
    raw_output(0xa2, 0x0, 0x58, 29, 0x2, ada_only_value),
  )
  True
}

/// §5.5: a value is the two-element `[coin, multiasset]`. `83` is a three-element
/// array whose first two elements read exactly as a well-formed value would, so
/// without this check the bookmark would serve a shape the encoder cannot emit.
test value_bookmark_refuses_a_value_that_is_not_a_pair() fail {
  expect _ = open_value_bookmark(
    raw_output(0xa2, 0x0, 0x58, 29, 0x1, #"8301a0"),
  )
  True
}

/// §6.1, inside the item: one logical number has one spelling. Each of these
/// puts the lovelace amount `1` in a wider head than it needs — well-formed
/// CBOR that every later read agrees with, and a second spelling of a number
/// that already has one.
///
/// `head_at` carries the bound at each of its four wide forms. They are four
/// instances of one rule, so all four are vectored: a bound that only ever runs
/// at the width the fixtures happen to reach is a bound nobody has checked.
test value_bookmark_refuses_a_non_minimal_interior_head() fail {
  expect _ = open_value_bookmark(
    raw_output(0xa2, 0x0, 0x58, 29, 0x1, #"821801a0"),
  )
  True
}

test value_bookmark_refuses_a_non_minimal_two_byte_head() fail {
  expect _ = open_value_bookmark(
    raw_output(0xa2, 0x0, 0x58, 29, 0x1, #"82190001a0"),
  )
  True
}

test value_bookmark_refuses_a_non_minimal_four_byte_head() fail {
  expect _ = open_value_bookmark(
    raw_output(0xa2, 0x0, 0x58, 29, 0x1, #"821a00000001a0"),
  )
  True
}

test value_bookmark_refuses_a_non_minimal_eight_byte_head() fail {
  expect _ = open_value_bookmark(
    raw_output(0xa2, 0x0, 0x58, 29, 0x1, #"821b0000000000000001a0"),
  )
  True
}

/// The head reader admits exactly the five definite widths. `1f` is the
/// indefinite form, which has no place in a §5.5 value — and the eight bytes
/// after it are chosen so that a reader which fell through to the widest
/// definite form would find a *minimal* eight-byte quantity there and open the
/// bookmark. Without those bytes the vector would be refused by the slice bound
/// instead, and would isolate nothing.
test value_bookmark_refuses_an_indefinite_interior_head() fail {
  expect _ = open_value_bookmark(
    raw_output(0xa2, 0x0, 0x58, 29, 0x1, #"821fffffffffffffffffa0"),
  )
  True
}

/// §6.1, the other half of the same reader: the call site names the major type,
/// so a byte string cannot be read where a number belongs. `41` where the
/// lovelace amount goes is a one-byte byte-string head whose low bits read as a
/// perfectly ordinary small integer if nothing checks the major.
test value_bookmark_refuses_an_interior_head_of_the_wrong_major_type() fail {
  expect _ = open_value_bookmark(
    raw_output(0xa2, 0x0, 0x58, 29, 0x1, #"8241a0"),
  )
  True
}

/// §5.5: an output value's asset names are capped at 32 bytes. Thirty-three is
/// well-formed CBOR and the rest of the group scans cleanly around it.
test value_bookmark_refuses_an_oversized_asset_name() fail {
  let item = output_item(
    1,
    [Pair(policy_id(0), [Pair(oversized_asset_name(), 7)])],
  )
  expect _ = open_value_bookmark(item)
  True
}

/// One byte past `max_asset_name_bytes`, and it says so rather than being a
/// literal whose length has to be counted by hand.
fn oversized_asset_name() -> ByteArray {
  let name = filler(max_asset_name_bytes + 1)
  expect bytearray.length(name) == max_asset_name_bytes + 1
  name
}

/// §5.5's width, on the *request* rather than on the value: a lookup names a
/// unit, and a 28-byte policy id is half of what a unit is. Without this a
/// short id would sweep, match nothing, and be answered `0` — an absence claim
/// about a policy that cannot exist.
test value_quantity_refuses_a_short_policy_id() fail {
  expect _ = value_quantity_of(
    open_value_bookmark(wide_value_item()),
    #"b0b0b0b0",
    asset_name(0),
  )
  True
}

/// §5.5's cap, on the request: the same argument, on the other half of the unit.
test value_quantity_refuses_an_oversized_asset_name() fail {
  expect _ = value_quantity_of(
    open_value_bookmark(wide_value_item()),
    policy_id(0),
    oversized_asset_name(),
  )
  True
}

/// A value with no assets at all is legal and readable — the ada-only output,
/// which is most of them.
test value_bookmark_opens_an_ada_only_value() {
  let item = output_item(1_500_000, [])
  let bookmark = open_value_bookmark(item)
  let Pair(quantity, _advanced) = value_quantity_of(
    bookmark,
    policy_id(0),
    asset_name(0),
  )
  and {
    value_lovelace(bookmark) == 1_500_000,
    quantity == 0,
  }
}

// ---------------------------------------------------------------------------
// §11.2 Case B — the Canonical-Data Acceptor
// ---------------------------------------------------------------------------

/// `d8799f 00 d8799f 4401020304 1901f4 ff ff` — a constructor whose second
/// argument is a constructor whose second argument is an integer. Written out
/// so the expected extents below are readable.
const nested_datum: ByteArray = #"d8799f00d8799f44010203041901f4ffff"

/// Interior access by path, at depth. The child's bytes come out of the datum
/// without the datum ever being materialised.
test acceptor_reaches_a_nested_child() {
  and {
    datum_child_bytes_v1(nested_datum, []) == Some(nested_datum),
    datum_child_bytes_v1(nested_datum, [0]) == Some(#"00"),
    datum_child_bytes_v1(nested_datum, [1]) == Some(#"d8799f44010203041901f4ff"),
    datum_child_bytes_v1(nested_datum, [1, 0]) == Some(#"4401020304"),
    datum_child_bytes_v1(nested_datum, [1, 1]) == Some(#"1901f4"),
    datum_integer_at_v1(nested_datum, 0) == None,
    datum_alternative_at_v1(nested_datum, 0) == Some(0),
  }
}

/// §7.3 abort-never-clamp, in `Option` form: an index past the end is `None`,
/// not the last child, and a path through a leaf is `None`, not the leaf.
test acceptor_refuses_out_of_range_paths() {
  and {
    datum_child_extent_v1(nested_datum, [2]) == None,
    datum_child_extent_v1(nested_datum, [1, 2]) == None,
    datum_child_extent_v1(nested_datum, [0, 0]) == None,
    datum_child_extent_v1(nested_datum, [-1]) == None,
    datum_child_extent_v1(#"d8799f00", [0]) == None,
  }
}

/// A map's children are its keys and values interleaved, so one path rule
/// serves all three containers.
test acceptor_walks_a_map_by_interleaved_children() {
  let datum = #"a2014401020304029f00ff"
  and {
    datum_child_bytes_v1(datum, [0]) == Some(#"01"),
    datum_child_bytes_v1(datum, [1]) == Some(#"4401020304"),
    datum_child_bytes_v1(datum, [2]) == Some(#"02"),
    datum_child_bytes_v1(datum, [3]) == Some(#"9f00ff"),
    datum_child_bytes_v1(datum, [3, 0]) == Some(#"00"),
    datum_child_extent_v1(datum, [4]) == None,
  }
}

/// The §6.2 re-pin's first re-pinned form, read rather than declined:
/// `c2 49 01 00…` is 2⁶⁴, and no `Data` decoder in the tree can produce it.
/// Reading it from the bytes is what makes a rule about such a datum possible
/// at all.
test acceptor_reads_a_big_bignum() {
  and {
    datum_integer_at_v1(#"c249010000000000000000", 0) == Some(
      18446744073709551616,
    ),
    datum_integer_at_v1(#"c349010000000000000000", 0) == Some(
      -18446744073709551617,
    ),
    // The ordinary integer forms still read, so the accessor is one accessor.
    datum_integer_at_v1(#"1901f4", 0) == Some(500),
    datum_integer_at_v1(#"20", 0) == Some(-1),
    datum_integer_at_v1(#"1bffffffffffffffff", 0) == Some(18446744073709551615),
    // A non-minimal magnitude is not canonical, so it does not read.
    datum_integer_at_v1(#"c2480100000000000000", 0) == None,
  }
}

/// The §6.2 re-pin's second re-pinned form: alternatives ≥ 128 wear tag 102,
/// which `cbor.deserialise` declines outright. The acceptor reads the
/// alternative and descends into the arguments regardless.
test acceptor_reads_a_high_constructor_alternative() {
  and {
    datum_alternative_at_v1(#"d86682188080", 0) == Some(128),
    datum_alternative_at_v1(#"d8668219ffff80", 0) == Some(65535),
    datum_alternative_at_v1(#"d8668218809f00ff", 0) == Some(128),
    datum_child_bytes_v1(#"d8668218809f00ff", [0]) == Some(#"00"),
    // The other two spellings, so one accessor covers all of §6.2.
    datum_alternative_at_v1(#"d87980", 0) == Some(0),
    datum_alternative_at_v1(#"d87f80", 0) == Some(6),
    datum_alternative_at_v1(#"d9050080", 0) == Some(7),
    // Alternative 127 is the last tag-1280 spelling; 128 in that spelling is
    // outside the range and refuses.
    datum_alternative_at_v1(#"d9057880", 0) == Some(127),
    datum_alternative_at_v1(#"d9057980", 0) == None,
    // Tag 102 below 128 has a shorter canonical spelling, so it is not one.
    datum_alternative_at_v1(#"d866820080", 0) == None,
  }
}

/// §11's contract on the three typed leaf readers, on the inputs that nearly
/// broke it: they **decline** an offset that does not begin an item of the kind
/// asked for. Declining is the whole promise — an abort takes the calling rule
/// down with it, and a rule cannot catch one.
///
/// The alternative reader is where the promise was false. It read the
/// constructor head straight through the shared minimal-width reader, which
/// *aborts* on a head §6.1 does not admit rather than declining, so these three
/// caller-reachable shapes were aborts:
///
///   * `d9 0079 80` — tag 121 spelled in the two-byte width;
///   * `d8 66` — tag 102 with nothing after it;
///   * `d8 66 82 19 0080 80` — tag 102 whose alternative, 128, is spelled two
///     bytes wide.
///
/// Deciding the item with the §6.2 scan before reading it is what the other two
/// readers already did, and the rows below hold all three to it on the same
/// bytes — so this is §11.2's contract rather than one function's habit.
test typed_leaf_readers_decline_a_head_6_1_does_not_admit() {
  let non_minimal_tag = #"d9007980"
  let truncated_tag_102 = #"d866"
  let non_minimal_alternative = #"d8668219008080"
  and {
    datum_alternative_at_v1(non_minimal_tag, 0) == None,
    datum_alternative_at_v1(truncated_tag_102, 0) == None,
    datum_alternative_at_v1(non_minimal_alternative, 0) == None,
    datum_integer_at_v1(non_minimal_tag, 0) == None,
    datum_integer_at_v1(truncated_tag_102, 0) == None,
    datum_integer_at_v1(non_minimal_alternative, 0) == None,
    datum_bytes_at_v1(non_minimal_tag, 0) == None,
    datum_bytes_at_v1(truncated_tag_102, 0) == None,
    datum_bytes_at_v1(non_minimal_alternative, 0) == None,
    // The canonical spellings of the same two constructors still read, so the
    // rows above are about the spelling and not about the tag.
    datum_alternative_at_v1(#"d87980", 0) == Some(0),
    datum_alternative_at_v1(#"d86682188080", 0) == Some(128),
  }
}

// ---------------------------------------------------------------------------
// §11.2 — the #567 residual
// ---------------------------------------------------------------------------

/// A bignum nested one level inside an otherwise ordinary canonical datum.
/// `d8799f ‖ c2 49 01 00×8 ‖ ff`.
const nested_bignum_datum: ByteArray = #"d8799fc249010000000000000000ff"

/// Deeper, and behind a map, so nothing about the verdict can be coming from a
/// head-byte peek.
const deep_bignum_datum: ByteArray = #"d8799f9fa10bc349010000000000000000ffff"

const nested_high_alternative_datum: ByteArray = #"d8799fd86682188080ff"

/// The same residual, moved off the end of its container.
///
/// Every fixture above puts the offending item **last** in whatever holds it,
/// and a flag fold that simply returned the last child's answer would pass all
/// of them. These four put it first and in the middle instead, with
/// materialisable siblings after it in every case, so the conjunction in the
/// list and map folds is load-bearing rather than incidentally right:
///
///   * `d8799f ‖ bignum ‖ 00 ‖ 01 ‖ ff` — first of three constructor arguments;
///   * `d8799f ‖ 00 ‖ bignum ‖ 01 ‖ ff` — the middle one;
///   * `a2 ‖ bignum ‖ 00 ‖ 01 ‖ 02` — a map's first **key**, which is a
///     position the key/value fold could drop on its own;
///   * `a2 ‖ 00 ‖ bignum ‖ 01 ‖ 02` — a map's first *value*, with a whole
///     ordinary entry after it.
const bignum_first_of_three: ByteArray = #"d8799fc2490100000000000000000001ff"

const bignum_middle_of_three: ByteArray = #"d8799f00c24901000000000000000001ff"

const bignum_as_first_map_key: ByteArray = #"a2c249010000000000000000000102"

const bignum_in_first_map_value: ByteArray = #"a200c2490100000000000000000102"

/// The other re-pinned form, also off the end of its container.
const high_alternative_first_of_two: ByteArray = #"d8799fd8668218808000ff"

fn output_with_datum(datum_cbor: ByteArray) -> ByteArray {
  encode_midgard_tx_output(
    MidgardTxOutput { ..canonical_output([]), datum_cbor: Some(datum_cbor) },
  )
}

/// **The residual #567 left open, closed.** Before the recursive acceptor this
/// test could not be written: `tx_out_data_v1` screened the head byte only, so
/// a bignum at depth reached `cbor.deserialise`, which reads `c2` as a
/// constructor tag and applies `constr_data(2 − 121, ..)`. At mainnet protocol
/// major version 11 that alternative is a `Word64`, so the machine fails —
/// the runner reports it as RC=101 from a Rust panic (aiken-lang/aiken#1359),
/// and an abort is not a verdict a test can assert.
///
/// The first conjunct of each row is the distinction the acceptor must not
/// blur: these bytes are still §6.2-canonical, exactly as #567's AC4 requires.
/// What changed is that the materialisation path now *declines* them instead of
/// aborting on them, at any depth.
fn declines(datum_cbor: ByteArray) -> Bool {
  let output = output_with_datum(datum_cbor)
  and {
    is_canonical_plutus_data_v1(datum_cbor),
    !is_materialisable_plutus_data_v1(datum_cbor),
    script_context_v1.tx_out_data_v1(output, False) == None,
    script_context_v1.tx_out_data_v1(output, True) == None,
    script_context_v1.spend_datum_summary_v1(output) == None,
  }
}

test nested_bignums_are_declined_rather_than_aborting() {
  and {
    declines(nested_bignum_datum),
    declines(deep_bignum_datum),
  }
}

/// The same residual, with the offending item somewhere other than last.
///
/// A flag fold that took the last child's answer instead of the conjunction of
/// all of them would satisfy every other vector in this file and fail every row
/// here — which is the point of the row: the screen's recursion has to *thread*
/// the answer, not merely reach the offending tag.
test bignums_are_declined_wherever_they_sit_in_their_container() {
  and {
    declines(bignum_first_of_three),
    declines(bignum_middle_of_three),
    declines(bignum_as_first_map_key),
    declines(bignum_in_first_map_value),
    declines(high_alternative_first_of_two),
    // …and the same containers without the bignum still materialise, so the
    // decline is about the item and not about the shape holding it.
    admits(#"d8799f000102ff"),
    admits(#"a200000102"),
    admits(#"d8799f8000ff"),
  }
}

/// The head-byte cases #567 already closed, still closed — the recursive screen
/// subsumes the screen it replaced rather than trading one gap for another.
test head_byte_bignums_are_still_declined() {
  and {
    declines(#"c249010000000000000000"),
    declines(#"c349010000000000000000"),
  }
}

/// The same treatment for the other §6.2 re-pinned form. A tag-102 datum was
/// already a decline rather than an abort — `cbor.deserialise` says `None` —
/// so this row is about the screen reaching the *right* verdict one step
/// earlier and at depth, not about closing an abort.
test nested_high_alternatives_are_declined() {
  declines(nested_high_alternative_datum)
}

fn admits(datum_cbor: ByteArray) -> Bool {
  let output = output_with_datum(datum_cbor)
  and {
    is_canonical_plutus_data_v1(datum_cbor),
    is_materialisable_plutus_data_v1(datum_cbor),
    script_context_v1.tx_out_data_v1(output, False) != None,
    script_context_v1.spend_datum_summary_v1(output) != None,
  }
}

/// The acceptor is still an *acceptor*: an ordinary datum, however nested, is
/// canonical, materialisable, and materialised. A screen that declined
/// everything would pass the tests above.
test ordinary_datums_stay_materialisable_at_depth() {
  and {
    admits(#"00"),
    admits(nested_datum),
    admits(#"a2014401020304029f00ff"),
  }
}

/// Non-canonical bytes are neither canonical nor materialisable, so the
/// stronger predicate is not being obtained by weakening the structural one.
test non_canonical_datums_are_not_materialisable() {
  and {
    !is_canonical_plutus_data_v1(#"c1"),
    !is_materialisable_plutus_data_v1(#"c1"),
    !is_canonical_plutus_data_v1(#"d8799f00"),
    !is_materialisable_plutus_data_v1(#"d8799f00"),
    !is_materialisable_plutus_data_v1(#""),
  }
}

/// The two predicates answer different questions, and the acceptor keeps them
/// apart. This is the line that goes red if a later change makes bignums
/// non-canonical to make them easy — which would cost the L1 parity §6.2 exists
/// to restore.
test canonicity_and_materialisability_stay_separate_questions() {
  and {
    is_canonical_plutus_data_v1(nested_bignum_datum),
    is_canonical_plutus_data_v1(deep_bignum_datum),
    is_canonical_plutus_data_v1(nested_high_alternative_datum),
    !is_materialisable_plutus_data_v1(nested_bignum_datum),
    !is_materialisable_plutus_data_v1(deep_bignum_datum),
    !is_materialisable_plutus_data_v1(nested_high_alternative_datum),
    // …and the output decoder still admits an output carrying one, because
    // canonicity is what it decides.
    ledger_output_v1.decode_canonical_output(
      output_with_datum(nested_bignum_datum),
    ) != None,
  }
}

/// Interior access works on the datums the materialisation path declines —
/// which is the point of having it. A rule about a bignum-bearing datum reads
/// the bignum; it does not need `Data`.
test interior_access_serves_unmaterialisable_datums() {
  and {
    datum_integer_at_v1(nested_bignum_datum, 3) == Some(18446744073709551616),
    datum_child_bytes_v1(nested_bignum_datum, [0]) == Some(
      #"c249010000000000000000",
    ),
    datum_child_bytes_v1(nested_high_alternative_datum, [0]) == Some(
      #"d86682188080",
    ),
    datum_alternative_at_v1(nested_high_alternative_datum, 3) == Some(128),
    cbor.deserialise(nested_high_alternative_datum) == None,
  }
}

// ---------------------------------------------------------------------------
// §11.4 budget rows
// ---------------------------------------------------------------------------

/// §11.4 row 1. Sixty-four ordered lookups over a 64-unit value through one
/// bookmark. The assertion is only that the answers are right; the runner's
/// memory column is the claim.
test budget_value_bookmark_sweeps_64_units() {
  let item = wide_value_item()
  let Pair(total, _final) = list.foldl(
    wide_units(),
    Pair(0, open_value_bookmark(item)),
    fn(unit, state) {
      let Pair(policy_index, asset_index) = unit
      let Pair(sum, bookmark) = state
      let Pair(quantity, advanced) = value_quantity_of(
        bookmark,
        policy_id(policy_index),
        asset_name(asset_index),
      )
      Pair(sum + quantity, advanced)
    },
  )
  total == 2080
}

/// §11.4 row 2, the control: the same sixty-four answers, obtained the way a
/// rule would have to obtain them without §11.1 — by materialising the output
/// and indexing the resulting list. Same fixture, same answers, so the two rows
/// are comparable.
test budget_materialised_value_reads_64_units() {
  let item = wide_value_item()
  expect Some(decoded) = ledger_output_v1.decode_canonical_output(item)
  let total = list.foldl(
    wide_units(),
    0,
    fn(unit, sum) {
      let Pair(policy_index, asset_index) = unit
      let unit_key = bytearray.concat(
        policy_id(policy_index),
        asset_name(asset_index),
      )
      when
        list.find(decoded.value.assets, fn(pair) { pair.1st == unit_key })
      is {
        None -> sum
        Some(pair) -> sum + pair.2nd
      }
    },
  )
  total == 2080
}

/// §11.4 row 11. The bookmark's fixed cost on its own: `open_value_bookmark`'s
/// structural pass over the 64-unit value, with no lookup after it. Subtracted
/// from row 1 this is what a sweep of 64 lookups costs at the margin, which is
/// the comparison §11.4's first reading actually rests on.
test budget_value_bookmark_open_only() {
  value_lovelace(open_value_bookmark(wide_value_item())) == 5_000_000
}

/// §11.4 row 12, the paired control: `decode_canonical_output`'s fixed cost on
/// the same item, with no lookup after it. Rows 11 and 12 are the same question
/// asked of the two paths — what does each pay before it has answered anything.
test budget_materialised_value_decode_only() {
  expect Some(decoded) = ledger_output_v1.decode_canonical_output(
    wide_value_item(),
  )
  decoded.value.lovelace == 5_000_000
}

/// §11.4 row 3. One lookup, same fixture — what a witness-minimal fault
/// statement about a single asset actually costs.
test budget_value_bookmark_single_unit() {
  let Pair(quantity, _advanced) = value_quantity_of(
    open_value_bookmark(wide_value_item()),
    policy_id(7),
    asset_name(7),
  )
  quantity == quantity_of(7, 7)
}

/// §11.4 row 4. Interior access into a datum: reach a child at depth two and
/// read it.
test budget_datum_interior_access() {
  and {
    datum_child_bytes_v1(nested_datum, [1, 1]) == Some(#"1901f4"),
    datum_integer_at_v1(nested_datum, 12) == Some(500),
  }
}

/// §11.4 row 5, the control: the same child, reached by materialising the datum
/// first. `cbor.deserialise` prices the whole datum before any index applies —
/// and for the §6.2 re-pinned forms it cannot be asked at all, which is the
/// part no measurement can express.
///
/// This row is **cheaper** than row 4, and stays cheaper at width (rows 6/7).
/// That is the honest shape of the trade and §11.2 states it: `deserialise` is
/// a builtin and the acceptor is interpreted Aiken, so on a datum the builtin
/// can take, the builtin wins. §11.2 is not a cost case — it is the only way to
/// reach inside the §6.2 forms the builtin declines or aborts on, and both rows
/// are far inside the basis, which is the whole of the budget claim on it.
test budget_datum_materialised_access() {
  expect Some(datum) = cbor.deserialise(nested_datum)
  expect materialised: MaterialisedDatumV1 = datum
  materialised.inner.number == 500
}

/// A 24-element datum of 32-byte leaves — the shape a real dispute reads one
/// field out of.
fn wide_datum() -> ByteArray {
  #"9f"
    |> bytearray.concat(
        list.foldr(
          list.range(0, 23),
          #"",
          fn(index, tail) {
            bytearray.concat(
              bytearray.concat(
                #"5820",
                bytearray.concat(
                  bytearray.from_int_big_endian(index, 1),
                  #"d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0",
                ),
              ),
              tail,
            )
          },
        ),
      )
    |> bytearray.concat(#"ff")
}

fn wide_datum_leaf(index: Int) -> ByteArray {
  bytearray.concat(
    bytearray.from_int_big_endian(index, 1),
    #"d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0d0",
  )
}

/// §11.4 row 6. The last leaf of the wide datum, reached by §11.2.
test budget_wide_datum_interior_access() {
  let datum = wide_datum()
  expect Some(Pair(offset, _length)) = datum_child_extent_v1(datum, [23])
  datum_bytes_at_v1(datum, offset) == Some(wide_datum_leaf(23))
}

/// §11.4 row 7, the control: the same leaf, materialised first. Same fixture,
/// same answer — and still cheaper, which is the measurement that stops §11.2
/// being sold as a cost win at width.
test budget_wide_datum_materialised_access() {
  expect Some(datum) = cbor.deserialise(wide_datum())
  expect leaves: List<ByteArray> = datum
  list.at(leaves, 23) == Some(wide_datum_leaf(23))
}

/// The shape of `nested_datum` as a Midgard type, so the control below can
/// index it the way a rule without §11.2 would have to.
type MaterialisedDatumV1 {
  flag: Int,
  inner: MaterialisedInnerV1,
}

type MaterialisedInnerV1 {
  payload: ByteArray,
  number: Int,
}
