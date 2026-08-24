//// Measured ExUnits frontier for §5.3 descriptor derivation (#628).
////
//// The `#628` review left the basis crossing for `build_v1` as an
//// *interpolated* point (~1,680 output bytes) drawn between two golden
//// vectors of different shape. Interpolation across a shape change is not a
//// measurement, so this module brackets the crossing with adjacent measured
//// rungs of one fixed shape: a canonical output whose only varying part is an
//// inline datum.
////
//// Every rung comes in two flavours, and the pair is the point of the module:
////
////   * `..._derives` builds the output bytes and derives the descriptor;
////   * `..._fixture_only` builds the *same* output bytes and stops.
////
//// An Aiken `test` reading is a whole-test upper bound -- it charges fixture
//// construction to the row. Pinning the fixture-only twin makes the marginal
//// cost of `ledger_value_v1` a difference the reader takes over two pinned
//// rows rather than a claim this module asserts. The verifier
//// `scripts/verify-transition-trace-descriptor-exec-ledger-v1.mjs` recomputes
//// those differences from the measured readings.
////
//// Guardrail 3 (#617): no worst-case payload is written as a source literal.
//// `filler` builds every payload programmatically by doubling, so the source
//// stays small and the vector size is a number in one place.

use aiken/cbor
use aiken/primitive/bytearray
use midgard/fraud_proofs/native_tx/components as native_components
use midgard/fraud_proofs/native_tx/types as native_types
use midgard/ledger_output_descriptor_v1

const h28_a = #"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"

/// `n` bytes of `0xab`, built by doubling rather than pinned as a literal.
///
/// The recursion costs O(log n) concatenations, so the fixture-only rows stay
/// dominated by the CBOR encoder rather than by payload construction.
fn filler(n: Int) -> ByteArray {
  if n <= 0 {
    #""
  } else if n == 1 {
    #"ab"
  } else {
    let half = filler(n / 2)
    let doubled = bytearray.concat(half, half)
    if n % 2 == 0 {
      doubled
    } else {
      bytearray.concat(doubled, #"ab")
    }
  }
}

/// A canonical Midgard output whose only variable part is a `datum_bytes`-byte
/// inline datum.
///
/// Built through `encode_midgard_tx_output` rather than pinned, so the bytes
/// are canonical by construction and cannot drift away from what
/// `decode_canonical_output` accepts.
fn output_cbor_with_datum(datum_cbor: ByteArray) -> ByteArray {
  native_components.encode_midgard_tx_output(
    native_types.MidgardTxOutput {
      address: native_types.MidgardAddress {
        protected: False,
        network_id: 0,
        payment_credential: native_types.PubKeyCredential(h28_a),
        stake_credential: None,
      },
      value: native_types.MidgardValue { lovelace: 1_000_000, assets: [] },
      datum_cbor: Some(datum_cbor),
      script_ref: None,
    },
  )
}

fn sized_output_cbor(datum_bytes: Int) -> ByteArray {
  let datum: Data = filler(datum_bytes)
  output_cbor_with_datum(cbor.serialise(datum))
}

/// Derive the descriptor for a rung, asserting the rung's pinned output size.
///
/// The size assertion is carried by *both* flavours so it cancels out of the
/// difference; it is here so a rung silently changing width shows up as a
/// failing test rather than as a silently relabelled reading.
fn derives(datum_bytes: Int, output_bytes: Int) -> Bool {
  let output_cbor = sized_output_cbor(datum_bytes)
  expect bytearray.length(output_cbor) == output_bytes
  expect Some(value) = ledger_output_descriptor_v1.ledger_value_v1(
    0,
    output_cbor,
  )
  bytearray.length(value) > 0
}

/// The same fixture, without the derivation under measurement.
fn fixture_only(datum_bytes: Int, output_bytes: Int) -> Bool {
  let output_cbor = sized_output_cbor(datum_bytes)
  bytearray.length(output_cbor) == output_bytes
}

// ---------------------------------------------------------------------------
// Neutralisation selectors
//
// The rows below are only evidence if the thing they measure can fail. These
// three run in the same `aiken check` invocation as every reading (the exec
// ledger declares them and the verifier refuses a ledger whose selectors did
// not run), and between them they rule out the three ways a cost row here
// could be vacuous: a `filler` that quietly built the wrong width, a derivation
// that accepts anything, and a derivation that ignores the index it is salted
// with.
// ---------------------------------------------------------------------------

test filler_lengths_are_exact() {
  and {
    bytearray.length(filler(0)) == 0,
    bytearray.length(filler(1)) == 1,
    bytearray.length(filler(7)) == 7,
    bytearray.length(filler(64)) == 64,
    bytearray.length(filler(1024)) == 1024,
    bytearray.length(filler(2625)) == 2625,
  }
}

test derivation_refuses_truncated_output_bytes() {
  let output_cbor = sized_output_cbor(512)
  let truncated = bytearray.take(output_cbor, 574)
  ledger_output_descriptor_v1.ledger_value_v1(0, truncated) == None
}

/// An observation this lane measured rather than assumed, pinned so it cannot
/// change silently.
///
/// The `l2_datum` sweep in
/// `midgard/fraud_proofs/transition_trace/proof_exunits_frontier.test` has no
/// `Some(#"")` rung because that rung cannot be built: an otherwise-canonical
/// output whose inline datum is the empty byte string -- `#"40"`, which is
/// exactly what `cbor.serialise` produces for an empty `ByteArray` and which
/// `is_canonical_plutus_data_v1` accepts as canonical Plutus data -- has no
/// §5.3 descriptor. `ledger_value_v1` declines it, so the `expect Some(..)` in
/// every caller aborts. The one-byte datum immediately above it derives fine.
///
/// This test records the behaviour at the width where it bites; it does not
/// claim the behaviour is correct. Whether an L2 output carrying an empty
/// inline datum should be undeserviceable by `apply_l2_outputs` and by
/// `validate_valid_deposit_transition` is a §5.3/§6.2 question this lane raises
/// and does not settle -- see #628 and this lane's exec ledger.
test derivation_declines_an_empty_inline_datum() {
  and {
    ledger_output_descriptor_v1.ledger_value_v1(
      0,
      output_cbor_with_datum(#"40"),
    ) == None,
    ledger_output_descriptor_v1.ledger_value_v1(
      0,
      output_cbor_with_datum(#"41ab"),
    ) != None,
  }
}

test derivation_binds_the_output_index() {
  let output_cbor = sized_output_cbor(512)
  expect Some(at_zero) = ledger_output_descriptor_v1.ledger_value_v1(
    0,
    output_cbor,
  )
  expect Some(at_one) = ledger_output_descriptor_v1.ledger_value_v1(
    1,
    output_cbor,
  )
  at_zero != at_one
}

test descriptor_575_bytes_derives() {
  derives(512, 575)
}

test descriptor_575_bytes_fixture_only() {
  fixture_only(512, 575)
}

test descriptor_839_bytes_derives() {
  derives(768, 839)
}

test descriptor_839_bytes_fixture_only() {
  fixture_only(768, 839)
}

test descriptor_1103_bytes_derives() {
  derives(1024, 1103)
}

test descriptor_1103_bytes_fixture_only() {
  fixture_only(1024, 1103)
}

test descriptor_1367_bytes_derives() {
  derives(1280, 1367)
}

test descriptor_1367_bytes_fixture_only() {
  fixture_only(1280, 1367)
}

test descriptor_1499_bytes_derives() {
  derives(1408, 1499)
}

test descriptor_1499_bytes_fixture_only() {
  fixture_only(1408, 1499)
}

test descriptor_1631_bytes_derives() {
  derives(1536, 1631)
}

test descriptor_1631_bytes_fixture_only() {
  fixture_only(1536, 1631)
}

test descriptor_1763_bytes_derives() {
  derives(1664, 1763)
}

test descriptor_1763_bytes_fixture_only() {
  fixture_only(1664, 1763)
}

test descriptor_1895_bytes_derives() {
  derives(1792, 1895)
}

test descriptor_1895_bytes_fixture_only() {
  fixture_only(1792, 1895)
}

test descriptor_2159_bytes_derives() {
  derives(2048, 2159)
}

test descriptor_2159_bytes_fixture_only() {
  fixture_only(2048, 2159)
}

test descriptor_2423_bytes_derives() {
  derives(2304, 2423)
}

test descriptor_2423_bytes_fixture_only() {
  fixture_only(2304, 2423)
}

test descriptor_2687_bytes_derives() {
  derives(2560, 2687)
}

test descriptor_2687_bytes_fixture_only() {
  fixture_only(2560, 2687)
}

test descriptor_2753_bytes_derives() {
  derives(2624, 2753)
}

test descriptor_2753_bytes_fixture_only() {
  fixture_only(2624, 2753)
}

test descriptor_2819_bytes_derives() {
  derives(2688, 2819)
}

test descriptor_2819_bytes_fixture_only() {
  fixture_only(2688, 2819)
}

test descriptor_3083_bytes_derives() {
  derives(2944, 3083)
}

test descriptor_3083_bytes_fixture_only() {
  fixture_only(2944, 3083)
}
