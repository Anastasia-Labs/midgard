import type { CardanoFlatRefusalClass } from "../src/cek-program.cardano-flat.js";

/**
 * Raw Flat programs read by the Plutus V3 evaluator (plutus-core 1.71,
 * protocol version 11) through `deserialiseScript`, each wrapped in one CBOR
 * byte string. An accepted row records Cardano's result and budget; its twin
 * is the canonical Flat of the program Cardano read, and Cardano gives the
 * twin the same result and budget. A refused row records Cardano's decode
 * error, and the class and bit offset this decoder reports.
 */

/** Hex of the Flat bits for program 1.1.0 and a term, with the final pad. */
export const flatOfTermBits = (term: string): string => {
  const bits = "000000010000000100000000" + term;
  const padded = bits + "0".repeat(7 - (bits.length % 8)) + "1";
  return Buffer.from(
    (padded.match(/.{8}/gu) ?? []).map((byte) => parseInt(byte, 2)),
  ).toString("hex");
};

/** `constr 0` over `count` unit constants. */
const constrOfUnits = (count: number): string =>
  flatOfTermBits(
    // tag 8, constr tag 0, then per field: cons, tag 4, type [unit], end.
    "1000" + "00000000" + "10100100110".repeat(count) + "0",
  );
export const CONSTR_1024_UNITS = constrOfUnits(1024);
export const CONSTR_1025_UNITS = constrOfUnits(1025);

// [label, raw Flat, Cardano result and budget, canonical twin]
export const CARDANO_ACCEPTED = [
  [
    "a Data filler running through a zero byte",
    "0101004c000101000001",
    "16100/200 (con data (I 0))",
    "0101004c0101000001",
  ],
  [
    "serialiseData over a Data filler running through a zero byte",
    "010100376698000101000001",
    "2070166/410 (con bytestring #00)",
    "0101003766980101000001",
  ],
  [
    "a byte-string filler running through a zero byte",
    "0101004880010200550001",
    "16100/200 (con bytestring #0055)",
    "01010048810200550001",
  ],
  [
    "a byte-string filler with an extra zero byte",
    "01010048800101aa0001",
    "16100/200 (con bytestring #aa)",
    "010100488101aa0001",
  ],
  [
    "a Data filler with an extra zero byte",
    "010100376698000103c241010001",
    "2070166/410 (con bytestring #01)",
    "0101003766980101010001",
  ],
  [
    "Data bytes split into chunks of 1 and 2",
    "0101003766980102c24101010001",
    "2070166/410 (con bytestring #01)",
    "0101003766980101010001",
  ],
  [
    "Data bytes split into chunks of 1, 1 and 1",
    "0101003766980101c2014101010001",
    "2070166/410 (con bytestring #01)",
    "0101003766980101010001",
  ],
  [
    "a final pad of 0001",
    "0101003766980103c24101000001",
    "2070166/410 (con bytestring #01)",
    "0101003766980101010001",
  ],
  [
    "a version number with a zero continuation group",
    "810001004981",
    "16100/200 (con unit ())",
    "0101004981",
  ],
  [
    "builtin tag 100",
    "0101007c81",
    "16100/200 (builtin scaleValue)",
    "0101007c81",
  ],
  [
    "builtin tag 33",
    "0101007421",
    "16100/200 (builtin headList)",
    "0101007421",
  ],
  ["constr in 1.1.0", "0101008001", "16100/200 (constr 0)", "0101008001"],
  [
    "constr tag 2^64 - 1",
    "0101008ffffffffffffffffff011",
    "16100/200 (constr 18446744073709551615)",
    "0101008ffffffffffffffffff011",
  ],
  [
    "constr with 1,024 fields",
    CONSTR_1024_UNITS,
    "16400100/102600 (constr 0 (con unit ()) ...)",
    CONSTR_1024_UNITS,
  ],
  [
    "a bound variable",
    "010100200101",
    "16100/200 (lam i!0 i!1)",
    "010100200101",
  ],
  [
    "an empty integer list",
    "0101004bd601",
    "16100/200 (con (list integer) [])",
    "0101004bd601",
  ],
  [
    "a constant type of 31 tags",
    "0101004bd6f5bd6f5bd6f5bd6f5bd6f5bd6f5bd6f5bd6001",
    "16100/200 (con (list ... (list integer)) [])",
    "0101004bd6f5bd6f5bd6f5bd6f5bd6f5bd6f5bd6f5bd6001",
  ],
  [
    "an empty list of BLS12-381 G1 elements",
    "0101004bd721",
    "16100/200 (con (list bls12_381_G1_element) [])",
    "0101004bd721",
  ],
  [
    "a string starting with a byte-order mark",
    "010100490104efbbbf610001",
    '16100/200 (con string "\\65279a")',
    "010100490104efbbbf610001",
  ],
  [
    "an integer and bool pair",
    "0101004bded0a017",
    "16100/200 (con (pair integer bool) (-3, True))",
    "0101004bded0a017",
  ],
] as const;

// [label, raw Flat, Cardano decode error, refusal class, bit offset]
export const CARDANO_REFUSED: readonly (readonly [
  string,
  string,
  string,
  CardanoFlatRefusalClass,
  number,
])[] = [
  [
    "a 00 byte after the final pad",
    "0101003766980103c24101000100",
    "TooMuchSpace",
    "tooMuchSpace",
    104,
  ],
  [
    "an ff byte after the final pad",
    "0101003766980103c241010001ff",
    "TooMuchSpace",
    "tooMuchSpace",
    104,
  ],
  [
    "a final pad of 03",
    "0101003766980103c241010003",
    "TooMuchSpace",
    "tooMuchSpace",
    103,
  ],
  [
    "a final pad of ff",
    "0101003766980103c2410100ff",
    "TooMuchSpace",
    "tooMuchSpace",
    97,
  ],
  [
    "a final pad of 80",
    "0101003766980103c241010080",
    "TooMuchSpace",
    "tooMuchSpace",
    97,
  ],
  [
    "a final pad ending inside a byte",
    "0101007430",
    "TooMuchSpace",
    "tooMuchSpace",
    36,
  ],
  [
    "no final pad",
    "0101003766980103c2410100",
    "NotEnoughSpace",
    "notEnoughSpace",
    96,
  ],
  [
    "a byte-string filler ending in an early 1 bit",
    "010100488401aa0001",
    'BadEncoding "usedBits /= 0"',
    "fillerNotByteAligned",
    34,
  ],
  [
    "term tag 10",
    "010100a1",
    "Unknown term constructor tag: 10",
    "unknownTermTag",
    24,
  ],
  [
    "an unknown term tag after an unbound variable",
    "01010032000b01",
    "Unknown term constructor tag: 11",
    "unknownTermTag",
    44,
  ],
  [
    "builtin tag 101",
    "0101007ca1",
    "MultiIndexArray is not available",
    "builtinNotAvailable",
    28,
  ],
  [
    "builtin tag 105",
    "0101007d21",
    "DropPolicies is not available",
    "builtinNotAvailable",
    28,
  ],
  [
    "builtin tag 106",
    "0101007d41",
    "Failed to decode builtin tag",
    "unknownBuiltin",
    28,
  ],
  [
    "constr in 1.0.0",
    "0100008001",
    "'constr' is not allowed before 1.1.0",
    "constrOrCaseBeforeV110",
    24,
  ],
  [
    "case in 1.0.0",
    "010000980001",
    "'case' is not allowed before 1.1.0",
    "constrOrCaseBeforeV110",
    24,
  ],
  [
    "constr with 1,025 fields",
    CONSTR_1025_UNITS,
    "constr with 1025 fields",
    "constrTooManyFields",
    24,
  ],
  [
    "a constr tag over 64 bits",
    "0101008ffffffffffffffffff021",
    "Unexpected extra byte in unsigned integer",
    "word64Overflow",
    28,
  ],
  [
    "a constr tag of 11 groups",
    "010100880808080808080808081001",
    "Unexpected extra byte in unsigned integer",
    "word64Overflow",
    28,
  ],
  [
    "a variable index over 64 bits",
    "01010020ffffffffffffffffff0201",
    "Unexpected extra byte in unsigned integer",
    "word64Overflow",
    32,
  ],
  [
    "a constant without type tags",
    "01010041",
    "Failed to decode a universe",
    "badConstantType",
    28,
  ],
  [
    "a protoList constant",
    "0101004a81",
    "A non-star type can't have a value",
    "badConstantType",
    28,
  ],
  [
    "a type tag left over",
    "010100484005",
    "Failed to decode a universe",
    "badConstantType",
    28,
  ],
  [
    "type tag 14",
    "0101004f01",
    "Failed to decode a universe",
    "badConstantType",
    28,
  ],
  [
    "list applied to list",
    "0101004bd6a1",
    "Failed to decode a universe",
    "badConstantType",
    28,
  ],
  [
    "a constant type of 33 tags",
    "0101004bd6f5bd6f5bd6f5bd6f5bd6f5bd6f5bd6f5bd6f5801",
    "Constant of type ... exceeds",
    "constantTypeTooLarge",
    28,
  ],
  [
    "a BLS12-381 G1 constant",
    "0101004c8130" + "00".repeat(48) + "0001",
    "Flat decoding is not supported for bls12_381_G1_element",
    "constantNotFlatDecodable",
    34,
  ],
  [
    "a string of ff",
    "010100490101ff0001",
    "Input contains invalid UTF-8 data",
    "invalidUtf8",
    34,
  ],
  [
    "a Data constant of ff",
    "0101004c0101ff0001",
    "DeserialiseFailure 0",
    "invalidData",
    34,
  ],
  [
    "a Value with a zero quantity",
    "0101004ea10081000001",
    "Value Flat decoder: zero quantity",
    "invalidValue",
    34,
  ],
  [
    "a Value with a 33-byte key",
    "0101004ea121" + "00".repeat(33) + "0001",
    "Invalid Value key",
    "invalidValue",
    35,
  ],
  [
    "an integer array before a trailing byte",
    "0101004bf20100",
    "TooMuchSpace",
    "tooMuchSpace",
    48,
  ],
];

// [label, raw Flat, Cardano verdict, bit offset of the item]
export const OUTSIDE_PROFILE = [
  [
    "variable index 0",
    "010100200001",
    "rejected DeBruijnError (FreeIndex 0)",
    32,
  ],
  [
    "version 256.0.0",
    "8002000061",
    "rejected PlutusCoreLanguageNotAvailableError",
    0,
  ],
  [
    "an empty integer array",
    "0101004bf201",
    "ok 16100/200 (con (array integer) [])",
    28,
  ],
  ["an empty Value", "0101004e81", "ok 16100/200 (con value [])", 28],
  [
    "a Value of one",
    "0101004ea10081000201",
    "ok 16100/200 (con value [(#, [(#, 1)])])",
    28,
  ],
] as const;
