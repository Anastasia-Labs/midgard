/**
 * Raw Flat programs over Data constants, read by the Plutus V3 evaluator
 * (plutus-core 1.71, protocol version 11) through `deserialiseScript`, each
 * wrapped in one CBOR byte string. Cardano reads a Data constant's first CBOR
 * item and ignores the bytes after it. An accepted row records Cardano's result
 * and budget; the budget includes the 100/100 start-up charge, which the
 * Midgard machine charges too. A refused row records Cardano's decode error,
 * and the bit offset and detail of this decoder's `invalidData` refusal.
 */

/** Data constants under `serialiseData`, `equalsData` and `unIData`. */
// [label, raw Flat program, Cardano result, Cardano cpu/mem]
export const CARDANO_RUNS = [
  [
    "serialiseData of tag 102 index 0",
    "0101003766980105d8668200800001",
    "(con bytestring #d87980)",
    "1856854/408",
  ],
  [
    "serialiseData of tag 102 index 6",
    "0101003766980105d8668206800001",
    "(con bytestring #d87f80)",
    "1856854/408",
  ],
  [
    "serialiseData of tag 102 index 127",
    "0101003766980106d86682187f800001",
    "(con bytestring #d9057880)",
    "1856854/408",
  ],
  [
    "serialiseData of bignum 1",
    "0101003766980103c241010001",
    "(con bytestring #01)",
    "2070166/410",
  ],
  [
    "serialiseData of padded bignum 1",
    "0101003766980104c24200010001",
    "(con bytestring #01)",
    "2070166/410",
  ],
  [
    "serialiseData of empty bignum 0",
    "0101003766980102c2400001",
    "(con bytestring #00)",
    "2070166/410",
  ],
  [
    "serialiseData of negative bignum -1",
    "0101003766980103c341000001",
    "(con bytestring #20)",
    "2070166/410",
  ],
  [
    "serialiseData of empty negative bignum -1",
    "0101003766980102c3400001",
    "(con bytestring #20)",
    "2070166/410",
  ],
  [
    "serialiseData of integer 1",
    "0101003766980101010001",
    "(con bytestring #01)",
    "2070166/410",
  ],
  [
    "serialiseData of byte string 01",
    "010100376698010241010001",
    "(con bytestring #4101)",
    "2070166/410",
  ],
  [
    "serialiseData of an indefinite map",
    "0101003766980104bf0102ff0001",
    "(con bytestring #a10102)",
    "3989974/428",
  ],
  [
    "equalsData of bignum -1 and -1",
    "0101003375e98103c34100004c0101200001",
    "(con bool True)",
    "1114643/601",
  ],
  [
    "equalsData of empty bignum -1 and -1",
    "0101003375e98102c340004c0101200001",
    "(con bool True)",
    "1114643/601",
  ],
  [
    "equalsData of bignum -6 and -5",
    "0101003375e98103c34105004c0101240001",
    "(con bool False)",
    "1114643/601",
  ],
  [
    "equalsData of bignum -6 and -6",
    "0101003375e98103c34105004c0101250001",
    "(con bool True)",
    "1114643/601",
  ],
  [
    "equalsData of padded bignum 1 and 1",
    "0101003375e98104c2420001004c0101010001",
    "(con bool True)",
    "1114643/601",
  ],
  [
    "equalsData of tag 102 index 0 and tag 121",
    "0101003375e98105d866820080004c0103d879800001",
    "(con bool True)",
    "1087364/601",
  ],
  [
    "unIData of empty negative bignum -1",
    "010100375a980102c3400001",
    "(con integer -1)",
    "68844/432",
  ],
  [
    "serialiseData of bignum 2^64",
    "010100376698010bc2490100000000000000000001",
    "(con bytestring #c249010000000000000000)",
    "2283478/412",
  ],
  [
    "serialiseData of padded bignum 2^64",
    "010100376698010cc24a000100000000000000000001",
    "(con bytestring #c249010000000000000000)",
    "2283478/412",
  ],
  [
    "serialiseData of bignum 2^64-1",
    "010100376698010ac248ffffffffffffffff0001",
    "(con bytestring #1bffffffffffffffff)",
    "2070166/410",
  ],
] as const;

/**
 * Decoder boundaries. Each row applies `serialiseData` to a Data constant, to
 * `listData` of a `(list data)` constant, or to `fstPair` of a
 * `(pair data data)` constant.
 */
// [label, raw Flat, Cardano result, Cardano budget]
export const DATA_DECODER_ACCEPTED = [
  [
    "serialiseData of tag 102 index 2^64-1",
    "010100376698010dd866821bffffffffffffffff800001",
    "(con bytestring #d866821bffffffffffffffff80)",
    "1856854/408",
  ],
  [
    "serialiseData of tag 1400, the last compact index",
    "0101003766980104d90578800001",
    "(con bytestring #d9057880)",
    "1856854/408",
  ],
  [
    "serialiseData of tag 102 over an indefinite array",
    "0101003766980106d8669f0080ff0001",
    "(con bytestring #d87980)",
    "1856854/408",
  ],
  [
    "serialiseData of byte string 0102 in two chunks",
    "01010037669801065f41014102ff0001",
    "(con bytestring #420102)",
    "2070166/410",
  ],
  [
    "serialiseData of an empty chunked byte string",
    "01010037669801025fff0001",
    "(con bytestring #40)",
    "2070166/410",
  ],
  [
    "serialiseData of a definite list",
    "01010037669801038201020001",
    "(con bytestring #9f0102ff)",
    "3989974/428",
  ],
  [
    "serialiseData of an indefinite list",
    "01010037669801049f0102ff0001",
    "(con bytestring #9f0102ff)",
    "3989974/428",
  ],
  [
    "serialiseData of an empty indefinite list",
    "01010037669801029fff0001",
    "(con bytestring #80)",
    "1856854/408",
  ],
  [
    "serialiseData of an empty definite list",
    "0101003766980101800001",
    "(con bytestring #80)",
    "1856854/408",
  ],
  [
    "serialiseData of listData of [c2 bignum 1]",
    "01010037666e9d2f5c2103c241010001",
    "(con bytestring #9f01ff)",
    "2989266/650",
  ],
  [
    "serialiseData of listData of [1]",
    "01010037666e9d2f5c2101010001",
    "(con bytestring #9f01ff)",
    "2989266/650",
  ],
  [
    "serialiseData of listData of [c2 bignum 1, tag 102 constructor 0]",
    "01010037666e9d2f5c2103c24101008105d8668200800001",
    "(con bytestring #9f01d87980ff)",
    "3842514/658",
  ],
  [
    "serialiseData of listData of [1, constructor 0]",
    "01010037666e9d2f5c210101008103d879800001",
    "(con bytestring #9f01d87980ff)",
    "3842514/658",
  ],
  [
    "serialiseData of fstPair of (c2 bignum 1, 1)",
    "01010037666aae752f7b630103c24101000101010001",
    "(con bytestring #01)",
    "2276061/842",
  ],
  [
    "serialiseData of fstPair of (1, 1)",
    "01010037666aae752f7b63010101000101010001",
    "(con bytestring #01)",
    "2276061/842",
  ],
  [
    "serialiseData of 0 followed by a 00 byte",
    "010100376698010200000001",
    "(con bytestring #00)",
    "2070166/410",
  ],
  [
    "serialiseData of 0 followed by an ff byte",
    "010100376698010200ff0001",
    "(con bytestring #00)",
    "2070166/410",
  ],
  [
    "serialiseData of 0 followed by an incomplete item",
    "010100376698010200180001",
    "(con bytestring #00)",
    "2070166/410",
  ],
  [
    "serialiseData of 1 followed by a 00 byte",
    "010100376698010201000001",
    "(con bytestring #01)",
    "2070166/410",
  ],
  [
    "serialiseData of c2 bignum 1 followed by an ff byte",
    "0101003766980104c24101ff0001",
    "(con bytestring #01)",
    "2070166/410",
  ],
  [
    "serialiseData of constructor 0 followed by a text string",
    "0101003766980105d8798061610001",
    "(con bytestring #d87980)",
    "1856854/408",
  ],
] as const;

// [label, raw Flat, Cardano decode error, bit offset, refusal detail]
export const DATA_DECODER_REFUSED = [
  [
    "tag 102 with index -1",
    "0101003766980105d8668220800001",
    "expected word",
    49,
    "CBOR tag 102 Plutus Data index is not a word64",
  ],
  [
    "tag 102 with a bignum index 0",
    "0101003766980106d86682c240800001",
    "expected word",
    49,
    "CBOR tag 102 Plutus Data index is not a word64",
  ],
  [
    "tag 102 with a bignum index 1",
    "0101003766980107d86682c24101800001",
    "expected word",
    49,
    "CBOR tag 102 Plutus Data index is not a word64",
  ],
  [
    "tag 102 with a bignum index 2^64",
    "010100376698010fd86682c249010000000000000000800001",
    "expected word",
    49,
    "CBOR tag 102 Plutus Data index is not a word64",
  ],
  [
    "tag 1401, past the compact range",
    "0101003766980104d90579800001",
    "Unrecognized tag 1401",
    49,
    "unknown CBOR tag 1401 in Plutus Data",
  ],
  [
    "tag 102 over a three-element array",
    "0101003766980106d8668300800a0001",
    "Expected exactly two elements",
    49,
    "CBOR tag 102 Plutus Data is not a two-element array",
  ],
  [
    "tag 102 over an indefinite array with no break",
    "0101003766980105d8669f00800001",
    "end of input",
    49,
    "CBOR Plutus Data ends before its item does",
  ],
  [
    "a chunked byte string with a 65-byte chunk",
    "01010037669801455f5841" + "00".repeat(65) + "ff0001",
    "ByteString exceeds 64 bytes",
    49,
    "CBOR Plutus Data byte string exceeds 64 bytes",
  ],
  [
    "a list of Data holding a text string",
    "01010037666e9d2f5c210261610001",
    "Unrecognized value of type TypeString",
    75,
    "invalid CBOR major type for Plutus Data",
  ],
  [
    "a list of Data holding a three-element tag 102",
    "01010037666e9d2f5c210101008106d8668300800a0001",
    "Expected exactly two elements",
    105,
    "CBOR tag 102 Plutus Data is not a two-element array",
  ],
  [
    "a pair of Data holding tag 1401",
    "01010037666aae752f7b63010101000104d90579800001",
    "Unrecognized tag 1401",
    120,
    "unknown CBOR tag 1401 in Plutus Data",
  ],
  [
    "a pair of Data holding an unterminated tag 102",
    "01010037666aae752f7b63010101000105d8669f00800001",
    "end of input",
    120,
    "CBOR Plutus Data ends before its item does",
  ],
  [
    "an incomplete item",
    "0101003766980101180001",
    "end of input",
    49,
    "CBOR Plutus Data ends before its item does",
  ],
] as const;
