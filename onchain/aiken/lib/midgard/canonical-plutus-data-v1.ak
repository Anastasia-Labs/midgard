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
    Some(offset) -> offset == bytearray.length(cbor_bytes)
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

fn scan_data(bytes: ByteArray, offset: Int) -> Option<Int> {
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
          Some(Pair(next, _value)) -> Some(next)
        }
      } else if major == 2 {
        scan_byte_string(bytes, offset)
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
fn scan_list(bytes: ByteArray, offset: Int) -> Option<Int> {
  when byte_at(bytes, offset) is {
    None -> None
    Some(first) ->
      if first == 0x80 {
        Some(offset + 1)
      } else if first == 0x9f {
        scan_list_items(bytes, offset + 1, 0)
      } else {
        None
      }
  }
}

fn scan_list_items(bytes: ByteArray, offset: Int, seen: Int) -> Option<Int> {
  when byte_at(bytes, offset) is {
    None -> None
    Some(first) ->
      if first == break_byte {
        if seen >= 1 {
          Some(offset + 1)
        } else {
          // An empty indefinite list would be `9f ff`; `80` is the only
          // canonical spelling.
          None
        }
      } else {
        when scan_data(bytes, offset) is {
          None -> None
          Some(next) -> scan_list_items(bytes, next, seen + 1)
        }
      }
  }
}

/// Maps are definite-length with entry order preserved as authored.
fn scan_map(bytes: ByteArray, offset: Int) -> Option<Int> {
  when scan_head(bytes, offset) is {
    None -> None
    Some(Pair(next, entry_count)) -> scan_map_entries(bytes, next, entry_count)
  }
}

fn scan_map_entries(
  bytes: ByteArray,
  offset: Int,
  remaining: Int,
) -> Option<Int> {
  if remaining <= 0 {
    Some(offset)
  } else {
    when scan_data(bytes, offset) is {
      None -> None
      Some(after_key) ->
        when scan_data(bytes, after_key) is {
          None -> None
          Some(after_value) ->
            scan_map_entries(bytes, after_value, remaining - 1)
        }
    }
  }
}

fn scan_tagged(bytes: ByteArray, offset: Int) -> Option<Int> {
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
              scan_generic_constr(bytes, next)
            } else if or {
              tag == positive_bignum_tag,
              tag == negative_bignum_tag,
            } {
              scan_bignum_magnitude(bytes, next)
            } else {
              None
            }
        }
      }
  }
}

/// §6.2: `d8 66 82 ‖ uint(alternative) ‖ args-list`, with a minimal uint64
/// alternative that must be ≥ 128 — below that the tag-121/1280 spellings are
/// the canonical ones.
fn scan_generic_constr(bytes: ByteArray, offset: Int) -> Option<Int> {
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
