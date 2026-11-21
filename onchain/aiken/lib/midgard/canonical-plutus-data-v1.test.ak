//// Conformance for the re-pinned datum/redeemer canonicity predicate
//// (`docs/spec/midgard-tx.md` §6.2).
////
//// The predicate's definition is "membership in the image of `serialiseData`",
//// so the strongest test available is the definition itself: Aiken's
//// `cbor.serialise` **is** that builtin, so anything it emits must be
//// accepted. The bignum and tag-102 cases below are the two classes the
//// retired stdlib round-trip pin rejected — datums L1 accepts that Midgard
//// used to refuse.

use aiken/builtin
use aiken/cbor
use aiken/primitive/bytearray
use midgard/canonical_plutus_data_v1.{is_canonical_plutus_data_v1}
use midgard/fraud_proofs/native_tx/components.{encode_midgard_tx_output}
use midgard/fraud_proofs/native_tx/types.{
  MidgardAddress, MidgardTxOutput, MidgardValue, PubKeyCredential,
}
use midgard/ledger_output_v1

/// 2^64 — the smallest integer that needs a bignum.
const two_pow_64: Int = 18446744073709551616

const long_65_bytes: ByteArray =
  #"000102030405060708090a0b0c0d0e0f101112131415161718191a1b1c1d1e1f202122232425262728292a2b2c2d2e2f303132333435363738393a3b3c3d3e3f40"

// ---------------------------------------------------------------------------
// The definition: everything serialiseData emits is canonical
// ---------------------------------------------------------------------------

fn round_trips(data: Data) -> Bool {
  is_canonical_plutus_data_v1(cbor.serialise(data))
}

test serialise_data_image_is_accepted_across_the_grammar() {
  let long_bytes = bytearray.concat(long_65_bytes, long_65_bytes)
  and {
    round_trips(builtin.i_data(0)),
    round_trips(builtin.i_data(23)),
    round_trips(builtin.i_data(24)),
    round_trips(builtin.i_data(255)),
    round_trips(builtin.i_data(65536)),
    round_trips(builtin.i_data(-1)),
    round_trips(builtin.i_data(-24)),
    round_trips(builtin.i_data(-4294967297)),
    round_trips(builtin.b_data(#"")),
    round_trips(builtin.b_data(#"00")),
    round_trips(builtin.b_data(bytearray.take(long_65_bytes, 64))),
    round_trips(builtin.b_data(long_65_bytes)),
    round_trips(builtin.b_data(long_bytes)),
    round_trips(builtin.list_data([])),
    round_trips(builtin.list_data([builtin.i_data(1), builtin.i_data(2)])),
    round_trips(builtin.map_data([])),
    round_trips(
      builtin.map_data([Pair(builtin.i_data(1), builtin.b_data(#"aa"))]),
    ),
    round_trips(builtin.constr_data(0, [])),
    round_trips(builtin.constr_data(6, [builtin.i_data(1)])),
    round_trips(builtin.constr_data(7, [])),
    round_trips(builtin.constr_data(127, [builtin.b_data(#"ff")])),
  }
}

/// The two newly-canonical classes, still expressed as "whatever the builtin
/// emits", so the test cannot drift from the definition.
test serialise_data_image_covers_bignums_and_high_alternatives() {
  and {
    round_trips(builtin.i_data(two_pow_64)),
    round_trips(builtin.i_data(-two_pow_64 - 1)),
    round_trips(builtin.i_data(two_pow_64 * two_pow_64)),
    round_trips(builtin.constr_data(128, [])),
    round_trips(builtin.constr_data(1000, [builtin.i_data(1)])),
    round_trips(builtin.constr_data(two_pow_64 - 1, [])),
  }
}

// ---------------------------------------------------------------------------
// The L1-parity re-pin, pinned on exact bytes
// ---------------------------------------------------------------------------

/// These are the byte forms the retired Aiken-stdlib-v3.1.0 round-trip pin
/// rejected. Every one is something cardano-ledger's `decodeData` accepts, so
/// every one must now be canonical.
test big_bignums_are_canonical() {
  and {
    // 2^64 exactly: nine magnitude bytes, no leading zero.
    is_canonical_plutus_data_v1(#"c249010000000000000000"),
    // -(2^64 + 1), the negative twin.
    is_canonical_plutus_data_v1(#"c349010000000000000000"),
    // A wider magnitude.
    is_canonical_plutus_data_v1(#"c24affffffffffffffffffff"),
    // The exact bytes the builtin emits for 2^64.
    cbor.serialise(builtin.i_data(two_pow_64)) == #"c249010000000000000000",
  }
}

test high_constructor_alternatives_are_canonical() {
  and {
    // Alternative 128 with an empty argument list.
    is_canonical_plutus_data_v1(#"d86682188080"),
    // Alternative 128 with one argument.
    is_canonical_plutus_data_v1(#"d8668218809f00ff"),
    // Alternative 65,535.
    is_canonical_plutus_data_v1(#"d8668219ffff80"),
    cbor.serialise(builtin.constr_data(128, [])) == #"d86682188080",
  }
}

/// Non-minimal and out-of-image bignum spellings still reject: a magnitude
/// shorter than nine bytes has a plain integer spelling, and a leading zero is
/// never minimal.
test non_canonical_bignums_still_reject() {
  and {
    !is_canonical_plutus_data_v1(#"c240"),
    !is_canonical_plutus_data_v1(#"c24101"),
    !is_canonical_plutus_data_v1(#"c248ffffffffffffffff"),
    !is_canonical_plutus_data_v1(#"c249000100000000000000"),
    // Indefinite magnitude below the 64-byte chunking threshold.
    !is_canonical_plutus_data_v1(#"c25f49010000000000000000ff"),
  }
}

/// Tag 102 is reserved for alternatives ≥ 128; below that the tag-121 and
/// tag-1280 spellings are the canonical ones, so tag 102 must reject.
test low_alternatives_under_tag_102_still_reject() {
  and {
    !is_canonical_plutus_data_v1(#"d866820080"),
    !is_canonical_plutus_data_v1(#"d86682187f80"),
    // A definite argument list is never canonical, whatever the tag.
    !is_canonical_plutus_data_v1(#"d8668218808100"),
  }
}

// ---------------------------------------------------------------------------
// Everything the retired predicate rejected still rejects
// ---------------------------------------------------------------------------

test structural_non_canonical_forms_still_reject() {
  and {
    // Non-minimal integer head.
    !is_canonical_plutus_data_v1(#"1817"),
    // Definite non-empty list.
    !is_canonical_plutus_data_v1(#"8100"),
    // Indefinite empty list; `80` is the only spelling.
    !is_canonical_plutus_data_v1(#"9fff"),
    // Empty indefinite byte string.
    !is_canonical_plutus_data_v1(#"5fff"),
    // Constructor with a definite argument list.
    !is_canonical_plutus_data_v1(#"d8798100"),
    // Indefinite map.
    !is_canonical_plutus_data_v1(#"bf0000ff"),
    // Text is outside Plutus Data.
    !is_canonical_plutus_data_v1(#"6161"),
    // Simple/float values are outside Plutus Data.
    !is_canonical_plutus_data_v1(#"f5"),
    // An unknown tag.
    !is_canonical_plutus_data_v1(#"c11a514b67b0"),
    // Trailing bytes after a complete item.
    !is_canonical_plutus_data_v1(#"0000"),
    // Truncated head.
    !is_canonical_plutus_data_v1(#"18"),
    // Empty input.
    !is_canonical_plutus_data_v1(#""),
  }
}

/// A single 65-byte definite byte string is not in the image: above 64 bytes
/// `serialiseData` always chunks.
test over_long_definite_byte_string_rejects() {
  let definite = bytearray.concat(#"5841", long_65_bytes)
  and {
    bytearray.length(long_65_bytes) == 65,
    !is_canonical_plutus_data_v1(definite),
    is_canonical_plutus_data_v1(cbor.serialise(builtin.b_data(long_65_bytes))),
  }
}

// ---------------------------------------------------------------------------
// End to end: the ledger-output datum gate
// ---------------------------------------------------------------------------

fn output_with_datum(datum_cbor: ByteArray) -> ByteArray {
  encode_midgard_tx_output(
    MidgardTxOutput {
      address: MidgardAddress {
        protected: False,
        network_id: 0,
        payment_credential: PubKeyCredential(
          #"0102030405060708090a0b0c0d0e0f101112131415161718191a1b1c",
        ),
        stake_credential: None,
      },
      value: MidgardValue { lovelace: 2000000, assets: [] },
      datum_cbor: Some(datum_cbor),
      script_ref: None,
    },
  )
}

fn output_is_decodable(datum_cbor: ByteArray) -> Bool {
  when
    ledger_output_v1.decode_canonical_output(output_with_datum(datum_cbor))
  is {
    Some(_output) -> True
    None -> False
  }
}

/// The user story the re-pin exists for: an output whose inline datum carries
/// a bignum or a high constructor alternative is spendable on Midgard, because
/// it is spendable on L1.
test outputs_carrying_l1_acceptable_datums_now_decode() {
  and {
    output_is_decodable(#"c249010000000000000000"),
    output_is_decodable(#"c349010000000000000000"),
    output_is_decodable(cbor.serialise(builtin.constr_data(128, []))),
    output_is_decodable(cbor.serialise(builtin.i_data(two_pow_64))),
    // The ordinary forms keep decoding.
    output_is_decodable(#"d87980"),
    output_is_decodable(#"00"),
  }
}

test outputs_carrying_non_canonical_datums_still_reject() {
  and {
    !output_is_decodable(#"1817"),
    !output_is_decodable(#"8100"),
    !output_is_decodable(#"c24101"),
    !output_is_decodable(#"d866820080"),
  }
}
