//// The datum/redeemer canonicity predicate — `docs/spec/midgard-tx.md` §6.2.
////
//// Canonicity is **membership in the image of the Plutus `serialiseData`
//// builtin**: exactly the byte forms `serialiseData` emits and
//// cardano-ledger's `decodeData` accepts. This module decides that membership
//// by walking the bytes directly.
////
//// It replaces a round-trip pin (`deserialise` then `serialise` and compare)
//// against Aiken stdlib v3.1.0's hand-written CBOR decoder. That decoder is
//// narrower than `serialiseData`'s image in two places, and the round-trip
//// inherited both gaps:
////
////   * **canonical tag-2/3 bignums** — integers with `|i| ≥ 2⁶⁴` — were
////     rejected structurally: the stdlib's major-6 arm has no tag-range guard,
////     so `c2`/`c3` fell through to the constructor branch, produced a
////     negative constructor index, and left the magnitude bytes unconsumed;
////   * **tag-102 constructor encodings** — alternatives ≥ 128 — were rejected
////     by an explicit `if tag == 102 { deserialise_failure }`.
////
//// Both are forms L1 accepts, so rejecting them cost Midgard L1 parity: a
//// datum Cardano would take could not be spent on Midgard. Accepting them
//// restores it.
////
//// Everything else the retired predicate enforced still holds: minimal
//// integer heads below 2⁶⁴; definite byte strings ≤ 64 bytes with indefinite
//// 64-byte chunking above; empty lists exactly `80` and non-empty lists
//// indefinite (`9f … ff`); definite-length maps with entry order preserved;
//// constructor tags `d8 79+alt` (0–6), `d9 0500+alt−7` (7–127) and tag 102
//// (≥ 128); exactly one item consuming exactly the declared bytes; and no
//// text, simple, float or other tags anywhere.
////
//// Every function here is total: malformed input returns `None`/`False`
//// rather than aborting, so the predicate can gate untrusted bytes.

use aiken/primitive/bytearray

/// `serialiseData` emits byte strings definite up to this length and switches
/// to indefinite chunking of exactly this size above it.
pub const data_bytes_chunk: Int = 64

/// Constructor alternatives 0–6 use tags 121–127.
pub const small_constr_tag_base: Int = 121

pub const max_small_constr_alternative: Int = 6

/// Alternatives 7–127 use tags 1280–1400.
pub const large_constr_tag_base: Int = 1280

pub const max_large_constr_alternative: Int = 127

/// Alternatives ≥ 128 use tag 102 with an explicit alternative field.
pub const generic_constr_tag: Int = 102

pub const min_generic_constr_alternative: Int = 128

/// Tag 2 is a positive bignum, tag 3 a negative one.
pub const positive_bignum_tag: Int = 2

pub const negative_bignum_tag: Int = 3

/// `|i| ≥ 2⁶⁴` needs at least nine magnitude bytes; anything smaller has a
/// shorter spelling as a plain major-0/1 integer, so a shorter magnitude is
/// non-minimal.
pub const min_bignum_magnitude_bytes: Int = 9

const break_byte: Int = 0xff

const indefinite_additional_info: Int = 31

/// True when `cbor_bytes` is exactly one canonical Plutus `Data` item — a byte
/// string `serialiseData` could have produced — with no trailing bytes.
pub fn is_canonical_plutus_data_v1(cbor_bytes: ByteArray) -> Bool {
  when scan_data(cbor_bytes, 0) is {
    None -> False
    Some(Pair(offset, _materialisable)) ->
      offset == bytearray.length(cbor_bytes)
  }
}

/// Canonical **and** materialisable: every construct inside it is one the
/// Aiken-stdlib `cbor.deserialise` path can turn into `Data` — spec §11.2.
///
/// Two canonical forms are not. Both are §6.2-acceptable and neither is
/// reachable through `deserialise`:
///
///   * **tag-2/3 bignums.** The stdlib's major-6 arm computes
///     `constr_data(tag − 121, ..)` with no tag-range guard, so `c2`/`c3`
///     produce a negative alternative. Under PlutusV3 builtin semantics variant
///     E — mainnet protocol major version 11 — that alternative is a `Word64`,
///     so a negative one fails the machine on real L1. This is an **abort**,
///     not a decline, which is why a caller has to screen *before* asking.
///   * **tag-102 constructors** (alternatives ≥ 128), which the stdlib declines
///     outright.
///
/// This is the recursive form the §6.2 re-pin left owing. The head-byte screen
/// it replaces — #567's, which this change removes from
/// `midgard/script_context_v1` — read byte zero only, so a bignum nested at any
/// depth inside an otherwise ordinary datum still reached `deserialise` and
/// still aborted. The flag threaded through `scan_data` is set at every tag site
/// the walk passes, so depth is not a way around it.
///
/// **Declining to materialise is not declaring non-canonical.**
/// `is_canonical_plutus_data_v1` still accepts every byte string this rejects;
/// the two predicates answer different questions and #567's AC4 requires that
/// they keep doing so.
pub fn is_materialisable_plutus_data_v1(cbor_bytes: ByteArray) -> Bool {
  when scan_data(cbor_bytes, 0) is {
    None -> False
    Some(Pair(offset, materialisable)) -> and {
        offset == bytearray.length(cbor_bytes),
        materialisable,
      }
  }
}

/// The offset just past the one canonical `Data` item starting at `offset`, or
/// `None` if the bytes there are not canonical.
///
/// The interior-access primitive of spec §11.2: navigating to a datum's `k`-th
/// child means skipping its earlier siblings, and skipping a sibling is exactly
/// this. Exposed rather than re-derived so that the acceptor and the predicate
/// keep one grammar and one verdict (§6.1).
pub fn canonical_data_end_at_v1(bytes: ByteArray, offset: Int) -> Option<Int> {
  when scan_data(bytes, offset) is {
    None -> None
    Some(Pair(next, _materialisable)) -> Some(next)
  }
}

fn byte_at(bytes: ByteArray, offset: Int) -> Option<Int> {
  if or {
    offset < 0,
    offset >= bytearray.length(bytes),
  } {
    None
  } else {
    Some(bytearray.at(bytes, offset))
  }
}

/// Reads one CBOR head at `offset` and returns `(next offset, argument)`.
/// Only minimal (shortest-form) widths are accepted, and additional-info
/// values 28–31 always fail here — indefinite forms are handled by their own
/// callers, which check for the `5f`/`9f` prefix byte before reaching this.
fn scan_head(bytes: ByteArray, offset: Int) -> Option<Pair<Int, Int>> {
  when byte_at(bytes, offset) is {
    None -> None
    Some(first) -> {
      let additional = first % 32
      if additional <= 23 {
        Some(Pair(offset + 1, additional))
      } else if additional == 24 {
        when byte_at(bytes, offset + 1) is {
          None -> None
          Some(value) ->
            if value >= 24 {
              Some(Pair(offset + 2, value))
            } else {
              None
            }
        }
      } else if additional == 25 {
        when read_big_endian(bytes, offset + 1, 2) is {
          None -> None
          Some(value) ->
            if value > 0xff {
              Some(Pair(offset + 3, value))
            } else {
              None
            }
        }
      } else if additional == 26 {
        when read_big_endian(bytes, offset + 1, 4) is {
          None -> None
          Some(value) ->
            if value > 0xffff {
              Some(Pair(offset + 5, value))
            } else {
              None
            }
        }
      } else if additional == 27 {
        when read_big_endian(bytes, offset + 1, 8) is {
          None -> None
          Some(value) ->
            if value > 0xffffffff {
              Some(Pair(offset + 9, value))
            } else {
              None
            }
        }
      } else {
        None
      }
    }
  }
}

fn read_big_endian(bytes: ByteArray, offset: Int, width: Int) -> Option<Int> {
  if width <= 0 {
    Some(0)
  } else {
    when byte_at(bytes, offset) is {
      None -> None
      Some(head) ->
        when read_big_endian(bytes, offset + 1, width - 1) is {
          None -> None
          Some(tail) -> Some(head * pow256(width - 1) + tail)
        }
    }
  }
}

fn pow256(exponent: Int) -> Int {
  if exponent <= 0 {
    1
  } else {
    256 * pow256(exponent - 1)
  }
}

fn major_of(first: Int) -> Int {
  first / 32
}

/// The scan returns `(offset just past the item, materialisable)`. The second
/// component is the §11.2 flag: `False` as soon as any construct the stdlib
/// `deserialise` path cannot take is passed, at any depth. Structure and
/// materialisability are decided in the same pass because they read the same
/// tags, and a caller that needs both should not pay for two walks.
fn scan_data(bytes: ByteArray, offset: Int) -> Option<Pair<Int, Bool>> {
  when byte_at(bytes, offset) is {
    None -> None
    Some(first) -> {
      let major = major_of(first)
      if or {
        major == 0,
        major == 1,
      } {
        // Integers below 2^64, minimal head. Bignums are major 6 and land in
        // `scan_tagged` instead.
        when scan_head(bytes, offset) is {
          None -> None
          Some(Pair(next, _value)) -> Some(Pair(next, True))
        }
      } else if major == 2 {
        lift(scan_byte_string(bytes, offset), True)
      } else if major == 4 {
        scan_list(bytes, offset)
      } else if major == 5 {
        scan_map(bytes, offset)
      } else if major == 6 {
        scan_tagged(bytes, offset)
      } else {
        // Major 3 (text) and major 7 (simple/float) are outside Plutus Data.
        None
      }
    }
  }
}

/// Lifts a leaf scan — one with no `Data` inside it, so nothing that could
/// carry the §11.2 flag down — into the flagged form, with the flag the leaf
/// deserves stated rather than set and then corrected.
fn lift(scanned: Option<Int>, materialisable: Bool) -> Option<Pair<Int, Bool>> {
  when scanned is {
    None -> None
    Some(next) -> Some(Pair(next, materialisable))
  }
}

fn scan_byte_string(bytes: ByteArray, offset: Int) -> Option<Int> {
  when byte_at(bytes, offset) is {
    None -> None
    Some(first) ->
      if first % 32 == indefinite_additional_info {
        scan_byte_chunks(bytes, offset + 1, 0)
      } else {
        when scan_definite_bytes(bytes, offset) is {
          None -> None
          Some(Pair(next, length)) ->
            if length <= data_bytes_chunk {
              Some(next)
            } else {
              // Above the chunk size `serialiseData` always chunks, so a long
              // definite byte string is not in its image.
              None
            }
        }
      }
  }
}

/// Reads a definite byte string head plus payload, returning
/// `(offset past the payload, payload length)`.
fn scan_definite_bytes(bytes: ByteArray, offset: Int) -> Option<Pair<Int, Int>> {
  when byte_at(bytes, offset) is {
    None -> None
    Some(first) ->
      if major_of(first) != 2 {
        None
      } else {
        when scan_head(bytes, offset) is {
          None -> None
          Some(Pair(next, length)) ->
            if next + length <= bytearray.length(bytes) {
              Some(Pair(next + length, length))
            } else {
              None
            }
        }
      }
  }
}

/// The indefinite byte-string form: every chunk is exactly `data_bytes_chunk`
/// bytes except the final one, which is 1–64, and there are at least two
/// chunks (one chunk would have been spelled definite).
fn scan_byte_chunks(bytes: ByteArray, offset: Int, seen: Int) -> Option<Int> {
  when byte_at(bytes, offset) is {
    None -> None
    Some(first) ->
      if first == break_byte {
        if seen >= 2 {
          Some(offset + 1)
        } else {
          None
        }
      } else {
        when scan_definite_bytes(bytes, offset) is {
          None -> None
          Some(Pair(next, length)) ->
            if length == data_bytes_chunk {
              scan_byte_chunks(bytes, next, seen + 1)
            } else if and {
              length > 0,
              length < data_bytes_chunk,
            } {
              // A short chunk can only be the last one.
              when byte_at(bytes, next) is {
                None -> None
                Some(terminator) ->
                  if and {
                    terminator == break_byte,
                    seen >= 1,
                  } {
                    Some(next + 1)
                  } else {
                    None
                  }
              }
            } else {
              None
            }
        }
      }
  }
}

/// Lists and constructor argument lists share one rule: empty is exactly `80`,
/// non-empty is indefinite `9f … ff`.
fn scan_list(bytes: ByteArray, offset: Int) -> Option<Pair<Int, Bool>> {
  when byte_at(bytes, offset) is {
    None -> None
    Some(first) ->
      if first == 0x80 {
        Some(Pair(offset + 1, True))
      } else if first == 0x9f {
        scan_list_items(bytes, offset + 1, 0, True)
      } else {
        None
      }
  }
}

fn scan_list_items(
  bytes: ByteArray,
  offset: Int,
  seen: Int,
  ok: Bool,
) -> Option<Pair<Int, Bool>> {
  when byte_at(bytes, offset) is {
    None -> None
    Some(first) ->
      if first == break_byte {
        if seen >= 1 {
          Some(Pair(offset + 1, ok))
        } else {
          // An empty indefinite list would be `9f ff`; `80` is the only
          // canonical spelling.
          None
        }
      } else {
        when scan_data(bytes, offset) is {
          None -> None
          Some(Pair(next, item_ok)) ->
            scan_list_items(bytes, next, seen + 1, and {
                ok,
                item_ok,
              })
        }
      }
  }
}

/// Maps are definite-length with entry order preserved as authored.
fn scan_map(bytes: ByteArray, offset: Int) -> Option<Pair<Int, Bool>> {
  when scan_head(bytes, offset) is {
    None -> None
    Some(Pair(next, entry_count)) ->
      scan_map_entries(bytes, next, entry_count, True)
  }
}

fn scan_map_entries(
  bytes: ByteArray,
  offset: Int,
  remaining: Int,
  ok: Bool,
) -> Option<Pair<Int, Bool>> {
  if remaining <= 0 {
    Some(Pair(offset, ok))
  } else {
    when scan_data(bytes, offset) is {
      None -> None
      Some(Pair(after_key, key_ok)) ->
        when scan_data(bytes, after_key) is {
          None -> None
          Some(Pair(after_value, value_ok)) ->
            scan_map_entries(bytes, after_value, remaining - 1, and {
                ok,
                key_ok,
                value_ok,
              })
        }
    }
  }
}

fn scan_tagged(bytes: ByteArray, offset: Int) -> Option<Pair<Int, Bool>> {
  when byte_at(bytes, offset) is {
    None -> None
    Some(first) ->
      if first % 32 == indefinite_additional_info {
        None
      } else {
        when scan_head(bytes, offset) is {
          None -> None
          Some(Pair(next, tag)) ->
            if and {
              tag >= small_constr_tag_base,
              tag <= small_constr_tag_base + max_small_constr_alternative,
            } {
              scan_list(bytes, next)
            } else if and {
              tag >= large_constr_tag_base,
              tag <= large_constr_tag_base + max_large_constr_alternative - 7,
            } {
              scan_list(bytes, next)
            } else if tag == generic_constr_tag {
              // §11.2: canonical, and declined by the stdlib decoder — so the
              // whole enclosing datum is unmaterialisable, however deep this
              // sits.
              unmaterialisable(scan_generic_constr(bytes, next))
            } else if or {
              tag == positive_bignum_tag,
              tag == negative_bignum_tag,
            } {
              // §11.2: canonical, and an *abort* rather than a decline on the
              // stdlib path. This is the site the #567 head-byte screen could
              // not reach at depth.
              lift(scan_bignum_magnitude(bytes, next), False)
            } else {
              None
            }
        }
      }
  }
}

/// Clears the §11.2 flag while preserving the structural verdict: these bytes
/// are canonical, and they are not materialisable.
fn unmaterialisable(scanned: Option<Pair<Int, Bool>>) -> Option<Pair<Int, Bool>> {
  when scanned is {
    None -> None
    Some(Pair(next, _)) -> Some(Pair(next, False))
  }
}

/// §6.2: `d8 66 82 ‖ uint(alternative) ‖ args-list`, with a minimal uint64
/// alternative that must be ≥ 128 — below that the tag-121/1280 spellings are
/// the canonical ones.
fn scan_generic_constr(bytes: ByteArray, offset: Int) -> Option<Pair<Int, Bool>> {
  when byte_at(bytes, offset) is {
    None -> None
    Some(first) ->
      if first != 0x82 {
        None
      } else {
        when byte_at(bytes, offset + 1) is {
          None -> None
          Some(alternative_head) ->
            if major_of(alternative_head) != 0 {
              None
            } else {
              when scan_head(bytes, offset + 1) is {
                None -> None
                Some(Pair(next, alternative)) ->
                  if alternative >= min_generic_constr_alternative {
                    scan_list(bytes, next)
                  } else {
                    None
                  }
              }
            }
        }
      }
  }
}

/// §6.2: a canonical bignum magnitude is a byte string of at least nine bytes
/// with no leading zero, chunked at 64 bytes like any other byte string.
fn scan_bignum_magnitude(bytes: ByteArray, offset: Int) -> Option<Int> {
  when byte_at(bytes, offset) is {
    None -> None
    Some(first) ->
      if major_of(first) != 2 {
        None
      } else if first % 32 == indefinite_additional_info {
        // Chunked: the magnitude is longer than 64 bytes, so the length floor
        // is met by construction; only the leading byte still needs checking.
        when scan_definite_bytes(bytes, offset + 1) is {
          None -> None
          Some(Pair(_next, _length)) ->
            when first_chunk_leading_byte(bytes, offset + 1) is {
              None -> None
              Some(leading) ->
                if leading == 0 {
                  None
                } else {
                  scan_byte_chunks(bytes, offset + 1, 0)
                }
            }
        }
      } else {
        when scan_head(bytes, offset) is {
          None -> None
          Some(Pair(next, length)) ->
            if or {
              length < min_bignum_magnitude_bytes,
              length > data_bytes_chunk,
              next + length > bytearray.length(bytes),
            } {
              None
            } else {
              when byte_at(bytes, next) is {
                None -> None
                Some(leading) ->
                  if leading == 0 {
                    None
                  } else {
                    Some(next + length)
                  }
              }
            }
        }
      }
  }
}

fn first_chunk_leading_byte(bytes: ByteArray, offset: Int) -> Option<Int> {
  when scan_head(bytes, offset) is {
    None -> None
    Some(Pair(next, _length)) -> byte_at(bytes, next)
  }
}
