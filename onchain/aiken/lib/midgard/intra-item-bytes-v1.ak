//// The byte primitives §11's intra-item mechanisms share —
//// `docs/spec/midgard-tx.md` §11.
////
//// §5.5's value map, §11.2's `Data` and §11.3's native script all sit *inside*
//// an item, below the §5.1 envelope the §8.8 door reads. They are three
//// grammars, but they are spelled in one CBOR: a definite head of a known major
//// type, minimal width, followed by bytes at an offset. §6.1 asks that one
//// logical number have one spelling, and two readers of the same head are two
//// chances for that to stop being true — so there is one reader here, and the
//// mechanisms differ only in what they read *with* it.
////
//// **Abort, never clamp** (§7.3). Every function here fails closed on an
//// out-of-range argument rather than returning a clamped or truncated answer:
//// two clamped reads of different ranges can be byte-equal, and equality
//// evidence fabricated that way is indistinguishable from the real thing.
////
//// The `Option`-returning counterpart is `midgard/canonical_cbor_scan_v1`,
//// which answers "are these bytes canonical" for a caller that still has to
//// **decide**. These are for a caller that has already decided and is now
//// reading, where a violation is not a verdict but a fault.

use aiken/primitive/bytearray

/// The one minimal-width definite-head reader for §11's interior grammars.
///
/// Returns `(offset just past the head, argument)`. `expected_major` is pinned
/// by the call site, so a map head cannot be read where a byte string belongs,
/// and every width carries its own minimality bound, so one logical number has
/// one spelling (§6.1). All five definite widths are admitted because the
/// numbers these grammars carry genuinely reach the eight-byte form — a
/// lovelace amount, an asset quantity, a timelock slot. The narrow things —
/// asset-name lengths, group counts, arities, child counts — are bounded by
/// their own call sites rather than by the head reader.
pub fn head_at(
  bytes: ByteArray,
  offset: Int,
  expected_major: Int,
) -> Pair<Int, Int> {
  expect offset >= 0
  expect offset < bytearray.length(bytes)
  let first = bytearray.at(bytes, offset)
  expect first / 32 == expected_major
  let additional = first % 32
  if additional <= 23 {
    Pair(offset + 1, additional)
  } else if additional == 24 {
    let value = byte_in(bytes, offset + 1)
    expect value >= 24
    Pair(offset + 2, value)
  } else if additional == 25 {
    let value = bytearray.to_int_big_endian(slice_exact(bytes, offset + 1, 2))
    expect value > 0xff
    Pair(offset + 3, value)
  } else if additional == 26 {
    let value = bytearray.to_int_big_endian(slice_exact(bytes, offset + 1, 4))
    expect value > 0xffff
    Pair(offset + 5, value)
  } else {
    expect additional == 27
    let value = bytearray.to_int_big_endian(slice_exact(bytes, offset + 1, 8))
    expect value > 0xffffffff
    Pair(offset + 9, value)
  }
}

/// `bytearray.at` is fail-closed on an out-of-range index in Aiken, but the
/// failure is a machine error rather than a stated bound. Stating it keeps the
/// refusal attributable.
pub fn byte_in(bytes: ByteArray, offset: Int) -> Int {
  expect offset >= 0
  expect offset < bytearray.length(bytes)
  bytearray.at(bytes, offset)
}

/// The only slice §11 takes. §7.3 abort-never-clamp: `bytearray.slice` clamps,
/// and two clamped reads of different ranges can be byte-equal.
pub fn slice_exact(bytes: ByteArray, offset: Int, length: Int) -> ByteArray {
  expect offset >= 0
  expect length >= 0
  expect offset + length <= bytearray.length(bytes)
  if length == 0 {
    #""
  } else {
    bytearray.slice(bytes, offset, offset + length - 1)
  }
}
