//// Intra-item access — `docs/spec/midgard-tx.md` §11.1 and §11.2.
////
//// §10 walks a field down to **one item's bytes**. This module is what a rule
//// does once it is holding those bytes and the thing it needs is inside them:
//// a single asset quantity in an output's multiasset value (§11.1, the Value
//// bookmark), or one child of a datum (§11.2, the Canonical-Data Acceptor).
////
//// The two cases exist for **different** reasons, and §11.4's measurements say
//// so plainly rather than letting one story cover both.
////
////   * **§11.1 is a cost case.** The obvious implementation is
////     `decode_canonical_output`, which deserialises the whole output to `Data`
////     *and re-encodes it* to prove canonicity. Reading 64 units that way
////     measures 20.15 M against the 13.2 M basis; the bookmark's single
////     structural pass and forward sweep measure 12.22 M. The bookmark is how
////     a multiasset value fits.
////   * **§11.2 is a capability case, not a cost case.** `cbor.deserialise` is a
////     builtin and this module is interpreted Aiken, so on a datum the builtin
////     can take, the builtin wins — measured, 1.07 M against 4.87 M on the same
////     24-leaf fixture. What the acceptor buys is the datums the builtin
////     *cannot* take: §6.2's re-pinned bignums, where asking is an abort rather
////     than a decline, and its tag-102 constructors, which it declines. On
////     those there is no cheaper option, because there is no other option. Both
////     numbers are far inside the basis, which is what §11.4 claims for this
////     case and all it claims.
////
//// A reader tempted to route ordinary datum access through §11.2 for
//// uniformity should read that second bullet as the instruction it is.
////
//// **What authenticates these bytes.** Nothing here hashes anything. An item
//// reaches this module from `walk_next`/`field_item_at`, which read it through
//// the §8.8 door's `field_read_range` after the door's §7.1 hash check; §11 is
//// a pure function of bytes §7 already authenticated. That is also why this
//// module never takes a `FieldViewV1`: re-reading through the view would make
//// every interior byte pay tier-3's per-read chunk verification, and the point
//// of §11 is that interior access is cheap once the item is in hand.
////
//// **Why the bookmark holds bytes and §10's checkpoint does not.** §7.6 binds
//// *resumable* state — anything a thread carries between transactions — to
//// positions. `ValueBookmarkV1` is not that: it lives and dies inside one
//// transaction, so it carries the item it was opened on, which is what makes
//// it impossible to pair a position with bytes it did not come from. The
//// intra-item structure that *does* cross transactions is Case C's, and
//// `midgard/native_tx_script_pushdown_v1` pays §7.6 in full for it.
////
//// **Opaque for the §10.6 reason.** `ValueBookmarkV1`'s constructor is not
//// exported. A caller who could write one could place a position at a
//// byte offset of its choosing inside an item's payload and read from there
//// with nothing checked — the same capability #570 removed from the walk
//// checkpoint. Here a bookmark can only come from `open_value_bookmark`, which
//// derives it after validating the whole value, or from a lookup that advanced
//// one.

use aiken/primitive/bytearray
use midgard/canonical_plutus_data_v1.{
  canonical_data_end_at_v1, data_bytes_chunk, generic_constr_tag,
  is_canonical_plutus_data_v1, large_constr_tag_base,
  max_large_constr_alternative, max_small_constr_alternative,
  min_generic_constr_alternative, negative_bignum_tag, positive_bignum_tag,
  small_constr_tag_base,
}
use midgard/intra_item_bytes_v1.{byte_in, head_at, slice_exact}

// ---------------------------------------------------------------------------
// Interior-grammar readers
// ---------------------------------------------------------------------------
//
// `head_at`, `byte_in` and `slice_exact` are `midgard/intra_item_bytes_v1`'s.
// §5.5's value map and §11.2's `Data` are two grammars spelled in one CBOR, and
// §11.3's script is a third; §6.1 asks that one logical number have one
// spelling, so the three mechanisms share one reader rather than each carrying
// its own copy of the same minimality bounds.

/// §5.5/§5.6 canonical key order: length-first, then byte-lexicographic.
///
/// Strict — equal keys do not precede each other, which is what makes a
/// duplicate key a refusal rather than a silently shadowed entry.
pub fn canonical_key_precedes_v1(left: ByteArray, right: ByteArray) -> Bool {
  let left_length = bytearray.length(left)
  let right_length = bytearray.length(right)
  if left_length < right_length {
    True
  } else if left_length > right_length {
    False
  } else {
    bytearray.compare(left, right) == Less
  }
}

// ---------------------------------------------------------------------------
// §11.1 Case A — the Value bookmark
// ---------------------------------------------------------------------------

/// §5.5: a policy id is a 28-byte script hash.
pub const policy_id_bytes: Int = 28

/// §5.5: an output value's asset names are capped at 32 bytes — the same cap
/// `ledger_output_v1`'s materialising decoder applies to this field, not field
/// 5's.
pub const max_asset_name_bytes: Int = 32

/// A validated position inside one output item's multiasset value (§11.1).
///
/// The bookmark is **monotone**: each lookup must name a unit strictly after
/// the last one looked up, and the scan only ever moves forward. That is the
/// whole mechanism. A conservation rule over `k` units of an `n`-unit value
/// pays one structural pass plus one forward sweep — `O(n + k)` — instead of
/// `O(n·k)` restarts, and it never materialises the value as `Data`.
///
/// Monotonicity is also what makes **absence** provable. `open_value_bookmark`
/// establishes that the whole value is canonically ordered, so once the sweep
/// reaches a key that sorts after the requested unit, the requested unit is not
/// in the remainder; and the monotone floor establishes that it is not in the
/// prefix either, because the prefix is entirely below the previous request.
/// Drop either half and a zero answer stops being evidence.
pub opaque type ValueBookmarkV1 {
  /// The authenticated item the value lives in. Held rather than re-supplied
  /// per call so a position cannot be paired with foreign bytes; see this
  /// module's header on why that does not conflict with §7.6.
  bytes: ByteArray,
  /// The value's ada quantity, read once at open.
  lovelace: Int,
  /// Byte offset of the next key to read: an asset name while a policy group
  /// is open, a policy id otherwise.
  offset: Int,
  /// Policy groups not yet entered, excluding the open one.
  policies_remaining: Int,
  /// Assets left in the open group; `0` means no group is open.
  assets_remaining: Int,
  /// The open group's policy id, or `#""` when the position sits between
  /// groups. A real policy id is 28 bytes, so the empty string is unambiguous.
  policy_id: ByteArray,
  /// Lookups served. Zero means there is no ordering floor yet.
  queries: Int,
  /// The last requested unit — the ordering floor the next request must
  /// strictly follow.
  last_policy: ByteArray,
  last_name: ByteArray,
}

/// Open a bookmark over the value of an authenticated §5.3 field-2 output item.
///
/// The whole value is validated here, in one pass: canonical minimal heads,
/// the 28-byte policy width, the 32-byte asset-name cap, strictly increasing
/// policy and asset keys, non-empty policy groups, and strictly positive
/// quantities (§5.5). The pass is what every later zero answer rests on, so it
/// is not optional and it is not deferred to the reads.
///
/// It is still far below materialisation: `decode_canonical_output`
/// deserialises the output to `Data` and re-encodes the whole thing to decide
/// the same question.
pub fn open_value_bookmark(item: ByteArray) -> ValueBookmarkV1 {
  let value_offset = value_offset_in_output(item)
  // `82 ‖ uint(lovelace) ‖ policy-asset map` (§5.5).
  expect byte_in(item, value_offset) == 0x82
  let Pair(after_lovelace, lovelace) = head_at(item, value_offset + 1, 0)
  let Pair(after_map_head, policy_count) = head_at(item, after_lovelace, 5)
  let end_offset = scan_policy_groups(item, after_map_head, policy_count, #"")
  // This bound is a backstop — every read the scan makes is already
  // bounds-checked by `head_at`/`slice_exact`, so it has never been the thing
  // that refuses. It is not removable for that reason: `end_offset` has no
  // other consumer, and Aiken erases a `let` nothing reads. Delete the line and
  // the whole structural pass goes with it, silently, taking §11.1's evidence
  // for a zero answer with it. Anything that replaces it must still *consume*
  // `end_offset`.
  expect end_offset <= bytearray.length(item)
  ValueBookmarkV1 {
    bytes: item,
    lovelace,
    offset: after_map_head,
    policies_remaining: policy_count,
    assets_remaining: 0,
    policy_id: #"",
    queries: 0,
    last_policy: #"",
    last_name: #"",
  }
}

/// §5.5's output map is keyed `0..3` with `0 → bytes(address)` and
/// `1 → value`. Returns the offset of the value's own `82`.
fn value_offset_in_output(item: ByteArray) -> Int {
  let entries = byte_in(item, 0)
  // `a2`/`a3`/`a4` by presence — the only three heads §5.5 admits.
  expect or {
      entries == 0xa2,
      entries == 0xa3,
      entries == 0xa4,
    }
  expect byte_in(item, 1) == 0x0
  // The Midgard address payload is 29 bytes (payment only) or 57 (with a stake
  // hash), so its canonical wrapper is always the two-byte `58 LL` form.
  expect byte_in(item, 2) == 0x58
  let address_length = byte_in(item, 3)
  expect or {
      address_length == 29,
      address_length == 57,
    }
  let key_offset = 4 + address_length
  expect byte_in(item, key_offset) == 0x1
  key_offset + 1
}

fn scan_policy_groups(
  item: ByteArray,
  offset: Int,
  remaining: Int,
  previous_policy: ByteArray,
) -> Int {
  if remaining <= 0 {
    offset
  } else {
    let Pair(policy_offset, policy_length) = head_at(item, offset, 2)
    expect policy_length == policy_id_bytes
    let policy_id = slice_exact(item, policy_offset, policy_length)
    // Strictly increasing, so duplicates reject (§5.5). The first group has no
    // predecessor: a real policy id is 28 bytes, never empty.
    expect or {
        previous_policy == #"",
        canonical_key_precedes_v1(previous_policy, policy_id),
      }
    let Pair(after_map_head, asset_count) = head_at(
      item,
      policy_offset + policy_length,
      5,
    )
    // §5.5: an empty policy group has no canonical spelling — the policy would
    // simply be absent.
    expect asset_count > 0
    let after_assets = scan_assets(
      item,
      after_map_head,
      asset_count,
      #"",
      False,
    )
    scan_policy_groups(item, after_assets, remaining - 1, policy_id)
  }
}

/// §5.5's asset map, scanned as part of the structural pass over one output.
///
/// **Why §12.4's `scan_mint_assets` is a separate reader** and not this one
/// re-used: §5.5 and §5.6 spell asset maps alike but do not agree on what they
/// contain. A value quantity here is strictly *positive*; a mint quantity there
/// is a signed `int` whose only forbidden value is zero, because §5.6 admits
/// burns. And this scan runs over every group of the item as part of the
/// bookmark's one-pass validation, where that one is entered only for the single
/// group a §12.4 sweep matched. Folding the two together would mean one reader
/// parameterised by which sign rule to apply and by who owns the cursor — one
/// function holding two verdicts about two grammars, which is the shape §6.1
/// exists to keep out. The divergence is recorded at both sites so neither can
/// drift into the other by looking similar.
fn scan_assets(
  item: ByteArray,
  offset: Int,
  remaining: Int,
  previous_name: ByteArray,
  seen_any: Bool,
) -> Int {
  if remaining <= 0 {
    offset
  } else {
    let Pair(after_name, name_length) = head_at(item, offset, 2)
    // §5.3 — an asset name is at most 32 bytes.
    expect name_length <= max_asset_name_bytes
    let asset_name = slice_exact(item, after_name, name_length)
    // §5.5 — keys strictly increase inside a group. `seen_any` rather than an
    // empty `previous_name`, because §5.3 admits the empty asset name and it
    // would otherwise be indistinguishable from "no predecessor".
    expect or {
        !seen_any,
        canonical_key_precedes_v1(previous_name, asset_name),
      }
    let Pair(after_quantity, quantity) = head_at(
      item,
      after_name + name_length,
      0,
    )
    // §5.5: quantities are strictly positive; a zero entry would be a second
    // spelling of an absent asset.
    expect quantity > 0
    scan_assets(item, after_quantity, remaining - 1, asset_name, True)
  }
}

/// The value's ada quantity.
pub fn value_lovelace(bookmark: ValueBookmarkV1) -> Int {
  bookmark.lovelace
}

/// The quantity of `(policy_id, asset_name)`, and the advanced bookmark.
///
/// `0` means the unit is absent, and it is evidence: see `ValueBookmarkV1` on
/// what makes it so. The requested unit MUST strictly follow the previous
/// request — re-asking for a unit the sweep has passed aborts rather than
/// answering `0`, because at that position the two are indistinguishable.
pub fn value_quantity_of(
  bookmark: ValueBookmarkV1,
  policy_id: ByteArray,
  asset_name: ByteArray,
) -> Pair<Int, ValueBookmarkV1> {
  expect bytearray.length(policy_id) == policy_id_bytes
  expect bytearray.length(asset_name) <= max_asset_name_bytes
  // The monotone floor. Without it a repeat lookup would sweep forward from a
  // position already past the unit and report it absent — fabricating absence
  // evidence for an asset the value holds.
  expect or {
      bookmark.queries == 0,
      unit_precedes(
        bookmark.last_policy,
        bookmark.last_name,
        policy_id,
        asset_name,
      ),
    }
  let Pair(quantity, swept) = sweep_to_unit(bookmark, policy_id, asset_name)
  Pair(
    quantity,
    ValueBookmarkV1 {
      ..swept,
      queries: bookmark.queries + 1,
      last_policy: policy_id,
      last_name: asset_name,
    },
  )
}

fn unit_precedes(
  left_policy: ByteArray,
  left_name: ByteArray,
  right_policy: ByteArray,
  right_name: ByteArray,
) -> Bool {
  if left_policy == right_policy {
    canonical_key_precedes_v1(left_name, right_name)
  } else {
    canonical_key_precedes_v1(left_policy, right_policy)
  }
}

fn sweep_to_unit(
  bookmark: ValueBookmarkV1,
  policy_id: ByteArray,
  asset_name: ByteArray,
) -> Pair<Int, ValueBookmarkV1> {
  if bookmark.assets_remaining > 0 {
    if bookmark.policy_id == policy_id {
      sweep_within_group(bookmark, policy_id, asset_name)
    } else {
      // The requested policy is elsewhere. The floor already established that
      // it is not behind, so the rest of this group is behind it too.
      let after = skip_assets(
        bookmark.bytes,
        bookmark.offset,
        bookmark.assets_remaining,
      )
      sweep_to_unit(
        ValueBookmarkV1 {
          ..bookmark,
          offset: after,
          assets_remaining: 0,
          policy_id: #"",
        },
        policy_id,
        asset_name,
      )
    }
  } else if bookmark.policies_remaining <= 0 {
    // The sweep is past the last group, so the unit is not in the remainder.
    Pair(0, bookmark)
  } else {
    let Pair(policy_offset, policy_length) = head_at(
      bookmark.bytes,
      bookmark.offset,
      2,
    )
    let group_policy = slice_exact(bookmark.bytes, policy_offset, policy_length)
    let Pair(after_map_head, asset_count) = head_at(
      bookmark.bytes,
      policy_offset + policy_length,
      5,
    )
    if group_policy == policy_id {
      sweep_within_group(
        ValueBookmarkV1 {
          ..bookmark,
          offset: after_map_head,
          policies_remaining: bookmark.policies_remaining - 1,
          assets_remaining: asset_count,
          policy_id: group_policy,
        },
        policy_id,
        asset_name,
      )
    } else if canonical_key_precedes_v1(group_policy, policy_id) {
      let after = skip_assets(bookmark.bytes, after_map_head, asset_count)
      sweep_to_unit(
        ValueBookmarkV1 {
          ..bookmark,
          offset: after,
          policies_remaining: bookmark.policies_remaining - 1,
        },
        policy_id,
        asset_name,
      )
    } else {
      // The next group sorts after the requested policy, and the value is
      // ordered, so no later group carries it either.
      Pair(0, bookmark)
    }
  }
}

fn sweep_within_group(
  bookmark: ValueBookmarkV1,
  policy_id: ByteArray,
  asset_name: ByteArray,
) -> Pair<Int, ValueBookmarkV1> {
  if bookmark.assets_remaining <= 0 {
    // The group ran out; keep sweeping at the policy level, where the next
    // group's key decides.
    sweep_to_unit(
      ValueBookmarkV1 { ..bookmark, policy_id: #"" },
      policy_id,
      asset_name,
    )
  } else {
    let Pair(name_offset, name_length) = head_at(
      bookmark.bytes,
      bookmark.offset,
      2,
    )
    let name = slice_exact(bookmark.bytes, name_offset, name_length)
    let Pair(after_quantity, quantity) = head_at(
      bookmark.bytes,
      name_offset + name_length,
      0,
    )
    if name == asset_name {
      Pair(
        quantity,
        ValueBookmarkV1 {
          ..bookmark,
          offset: after_quantity,
          assets_remaining: bookmark.assets_remaining - 1,
        },
      )
    } else if canonical_key_precedes_v1(name, asset_name) {
      sweep_within_group(
        ValueBookmarkV1 {
          ..bookmark,
          offset: after_quantity,
          assets_remaining: bookmark.assets_remaining - 1,
        },
        policy_id,
        asset_name,
      )
    } else {
      Pair(0, bookmark)
    }
  }
}

fn skip_assets(item: ByteArray, offset: Int, remaining: Int) -> Int {
  if remaining <= 0 {
    offset
  } else {
    let Pair(after_name, name_length) = head_at(item, offset, 2)
    let Pair(after_quantity, _quantity) = head_at(
      item,
      after_name + name_length,
      0,
    )
    skip_assets(item, after_quantity, remaining - 1)
  }
}

// ---------------------------------------------------------------------------
// §11.2 Case B — the Canonical-Data Acceptor
// ---------------------------------------------------------------------------

/// `d8 66 82 ‖ uint(alternative) ‖ args` — the §6.2 tag-102 constructor's
/// two-element payload head.
const generic_constr_payload_head: Int = 0x82

const empty_list_byte: Int = 0x80

const indefinite_list_byte: Int = 0x9f

const break_byte: Int = 0xff

/// The `(offset, length)` of the child addressed by `path`, within a canonical
/// datum (§11.2).
///
/// A path step is an index into the item at the current position, counted in
/// **reading order**: a constructor's arguments, a list's elements, and — for a
/// map — its keys and values interleaved (`0` is entry 0's key, `1` its value,
/// `2` entry 1's key). One rule for all three containers means a path never
/// depends on knowing which of them it is walking through.
///
/// The empty path addresses the datum itself. `None` means the bytes are not
/// canonical, the position is not a container, or the index is out of range —
/// there is no clamping, and no partial answer.
///
/// **What this is for.** Not speed: §11.4 measures it slower than
/// `cbor.deserialise` on any datum the builtin can take, because the builtin is
/// a builtin. It is for the datums the builtin cannot take at all — the §6.2
/// re-pinned bignums and tag-102 constructors that
/// `is_materialisable_plutus_data_v1` screens out — where this is the only way
/// to reach a child. It stays well inside the §3.3 basis either way, which is
/// what the budget claim on it says.
pub fn datum_child_extent_v1(
  datum: ByteArray,
  path: List<Int>,
) -> Option<Pair<Int, Int>> {
  // The datum as a whole is decided first, for the same reason
  // `open_value_bookmark` validates the whole value before serving a lookup: a
  // path is only meaningful inside a structure that closes. Without this, the
  // truncated `d8 79 9f 00` would still hand back its first child, and the
  // caller would be reading a child of nothing.
  if !is_canonical_plutus_data_v1(datum) {
    None
  } else {
    when descend(datum, 0, path) is {
      None -> None
      Some(offset) ->
        when canonical_data_end_at_v1(datum, offset) is {
          None -> None
          Some(end) -> Some(Pair(offset, end - offset))
        }
    }
  }
}

/// The child's **canonical CBOR item**, sliced out of the datum — head
/// included, because that is what a rule re-hashes or re-compares. For the
/// payload of a byte-string child, `datum_bytes_at_v1` at the extent's offset.
pub fn datum_child_bytes_v1(
  datum: ByteArray,
  path: List<Int>,
) -> Option<ByteArray> {
  when datum_child_extent_v1(datum, path) is {
    None -> None
    Some(Pair(offset, length)) -> Some(slice_exact(datum, offset, length))
  }
}

/// The **payload** of the byte string at `offset`, definite or 64-byte chunked
/// (§6.2), with its head stripped and its chunks stitched.
///
/// The other typed leaf reader beside `datum_integer_at_v1`. Between them a
/// rule reads the two leaf kinds Plutus `Data` has without materialising
/// anything.
pub fn datum_bytes_at_v1(datum: ByteArray, offset: Int) -> Option<ByteArray> {
  when canonical_data_end_at_v1(datum, offset) is {
    None -> None
    Some(_end) ->
      if bytearray.at(datum, offset) / 32 != 2 {
        None
      } else {
        byte_string_payload(datum, offset)
      }
  }
}

fn descend(datum: ByteArray, offset: Int, path: List<Int>) -> Option<Int> {
  when path is {
    [] -> Some(offset)
    [index, ..rest] ->
      when child_offset(datum, offset, index) is {
        None -> None
        Some(child) -> descend(datum, child, rest)
      }
  }
}

/// The offset of the `index`-th child of the container at `offset`.
fn child_offset(datum: ByteArray, offset: Int, index: Int) -> Option<Int> {
  if index < 0 {
    None
  } else {
    when container_body(datum, offset) is {
      None -> None
      Some(Pair(body_offset, definite_children)) ->
        if definite_children >= 0 {
          if index >= definite_children {
            None
          } else {
            nth_sibling(datum, body_offset, index)
          }
        } else {
          nth_indefinite_sibling(datum, body_offset, index)
        }
    }
  }
}

/// Where a container's children begin, and how many there are — `-1` for the
/// indefinite list form, whose length is only known by walking to its break.
fn container_body(datum: ByteArray, offset: Int) -> Option<Pair<Int, Int>> {
  if or {
    offset < 0,
    offset >= bytearray.length(datum),
  } {
    None
  } else {
    let first = bytearray.at(datum, offset)
    let major = first / 32
    if major == 4 {
      if first == empty_list_byte {
        Some(Pair(offset + 1, 0))
      } else if first == indefinite_list_byte {
        Some(Pair(offset + 1, -1))
      } else {
        None
      }
    } else if major == 5 {
      // A map's children are its keys and values interleaved.
      when canonical_data_end_at_v1(datum, offset) is {
        None -> None
        Some(_end) -> {
          let Pair(body_offset, entries) = head_at(datum, offset, 5)
          Some(Pair(body_offset, entries * 2))
        }
      }
    } else if major == 6 {
      constr_body(datum, offset)
    } else {
      None
    }
  }
}

/// A constructor's argument list, whichever of §6.2's three tag spellings it
/// wears.
fn constr_body(datum: ByteArray, offset: Int) -> Option<Pair<Int, Int>> {
  when constr_head(datum, offset) is {
    None -> None
    Some(Pair(args_offset, _alternative)) -> container_body(datum, args_offset)
  }
}

/// `(offset of the argument list, alternative)` for the constructor at
/// `offset`. Covers all three §6.2 spellings, including tag 102 — which
/// `cbor.deserialise` declines outright, so this is the only way to read an
/// alternative ≥ 128 at all.
fn constr_head(datum: ByteArray, offset: Int) -> Option<Pair<Int, Int>> {
  if or {
    offset < 0,
    offset >= bytearray.length(datum),
  } {
    None
  } else if bytearray.at(datum, offset) / 32 != 6 {
    None
  } else {
    let Pair(after_tag, tag) = head_at(datum, offset, 6)
    if and {
      tag >= small_constr_tag_base,
      tag <= small_constr_tag_base + max_small_constr_alternative,
    } {
      Some(Pair(after_tag, tag - small_constr_tag_base))
    } else if and {
      tag >= large_constr_tag_base,
      tag <= large_constr_tag_base + max_large_constr_alternative - 7,
    } {
      Some(Pair(after_tag, tag - large_constr_tag_base + 7))
    } else if tag == generic_constr_tag {
      if byte_in(datum, after_tag) != generic_constr_payload_head {
        None
      } else {
        let Pair(after_alternative, alternative) = head_at(
          datum,
          after_tag + 1,
          0,
        )
        if alternative < min_generic_constr_alternative {
          None
        } else {
          Some(Pair(after_alternative, alternative))
        }
      }
    } else {
      None
    }
  }
}

/// The constructor alternative at `offset`, without materialising the
/// constructor — including the tag-102 alternatives ≥ 128 that §6.2 re-pinned
/// as canonical and that no `Data` decoder in the tree can produce.
///
/// **Decided before it is read**, like the other two typed leaf readers. §11
/// promises all three answer `None` for an offset that does not begin an item of
/// the kind asked for, and `constr_head` alone cannot keep that promise: it
/// reads heads with the shared minimal-width reader, which *aborts* on a
/// non-minimal or truncated one rather than declining. `d9 0079 80` — tag 121 in
/// the two-byte width — is canonical-looking enough to reach here and is exactly
/// such an abort. Deciding the item first turns every one of them into the
/// `None` the contract states.
pub fn datum_alternative_at_v1(datum: ByteArray, offset: Int) -> Option<Int> {
  when canonical_data_end_at_v1(datum, offset) is {
    None -> None
    Some(_end) ->
      when constr_head(datum, offset) is {
        None -> None
        Some(Pair(_args_offset, alternative)) -> Some(alternative)
      }
  }
}

fn nth_sibling(datum: ByteArray, offset: Int, index: Int) -> Option<Int> {
  if index <= 0 {
    Some(offset)
  } else {
    when canonical_data_end_at_v1(datum, offset) is {
      None -> None
      Some(next) -> nth_sibling(datum, next, index - 1)
    }
  }
}

fn nth_indefinite_sibling(
  datum: ByteArray,
  offset: Int,
  index: Int,
) -> Option<Int> {
  if or {
    offset < 0,
    offset >= bytearray.length(datum),
  } {
    None
  } else if bytearray.at(datum, offset) == break_byte {
    // Fewer children than the index asked for.
    None
  } else if index <= 0 {
    Some(offset)
  } else {
    when canonical_data_end_at_v1(datum, offset) is {
      None -> None
      Some(next) -> nth_indefinite_sibling(datum, next, index - 1)
    }
  }
}

/// The integer at `offset`, **including canonical tag-2/3 bignums** — the §6.2
/// forms `cbor.deserialise` aborts on rather than declines (§11.2).
///
/// This is the other half of what the acceptor buys. `is_materialisable_plutus_data_v1`
/// says a bignum-bearing datum cannot be turned into `Data`; this says a rule
/// does not need it to be, because the value is readable from the bytes. A
/// magnitude longer than 64 bytes is chunked (§6.2), and the chunks are
/// stitched here rather than rejected.
pub fn datum_integer_at_v1(datum: ByteArray, offset: Int) -> Option<Int> {
  if or {
    offset < 0,
    offset >= bytearray.length(datum),
  } {
    None
  } else {
    let first = bytearray.at(datum, offset)
    let major = first / 32
    if major == 0 {
      when canonical_data_end_at_v1(datum, offset) is {
        None -> None
        Some(_end) -> {
          let Pair(_next, value) = head_at(datum, offset, 0)
          Some(value)
        }
      }
    } else if major == 1 {
      when canonical_data_end_at_v1(datum, offset) is {
        None -> None
        Some(_end) -> {
          let Pair(_next, value) = head_at(datum, offset, 1)
          Some(-1 - value)
        }
      }
    } else if major == 6 {
      when canonical_data_end_at_v1(datum, offset) is {
        None -> None
        Some(_end) -> {
          let Pair(after_tag, tag) = head_at(datum, offset, 6)
          if tag == positive_bignum_tag {
            when byte_string_payload(datum, after_tag) is {
              None -> None
              Some(magnitude) -> Some(bytearray.to_int_big_endian(magnitude))
            }
          } else if tag == negative_bignum_tag {
            when byte_string_payload(datum, after_tag) is {
              None -> None
              Some(magnitude) ->
                Some(-1 - bytearray.to_int_big_endian(magnitude))
            }
          } else {
            None
          }
        }
      }
    } else {
      None
    }
  }
}

/// A byte string's payload, definite or 64-byte chunked (§6.2) — the bignum
/// magnitude reader and the leaf reader are the same function, because §6.2
/// spells both the same way. Canonicity of the surrounding item is decided by
/// the caller, so this only has to reassemble.
fn byte_string_payload(datum: ByteArray, offset: Int) -> Option<ByteArray> {
  if or {
    offset < 0,
    offset >= bytearray.length(datum),
  } {
    None
  } else if bytearray.at(datum, offset) == 0x5f {
    Some(concat_chunks(datum, offset + 1, #""))
  } else {
    let Pair(payload_offset, length) = head_at(datum, offset, 2)
    Some(slice_exact(datum, payload_offset, length))
  }
}

fn concat_chunks(
  datum: ByteArray,
  offset: Int,
  accumulated: ByteArray,
) -> ByteArray {
  if byte_in(datum, offset) == break_byte {
    accumulated
  } else {
    let Pair(payload_offset, length) = head_at(datum, offset, 2)
    expect length <= data_bytes_chunk
    concat_chunks(
      datum,
      payload_offset + length,
      bytearray.concat(accumulated, slice_exact(datum, payload_offset, length)),
    )
  }
}
