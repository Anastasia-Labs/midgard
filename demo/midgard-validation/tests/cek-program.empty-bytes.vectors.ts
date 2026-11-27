/**
 * Raw Flat programs over Data that is, or ends in, an empty byte string, read
 * and run by the Plutus V3 evaluator (plutus-core 1.71, protocol version 11)
 * the same way as the rows in `cek-program.data-constant-spelling.vectors.ts`.
 * Each budget includes the 100/100 start-up charge. The empty byte string
 * reads no bytes, so it may end the payload; a byte string head that claims
 * one byte more than the payload holds is refused.
 */

/** One-constant programs: [label, raw Flat, canonical payload, cpu/mem]. */
export const EMPTY_BYTES_CONSTANTS = [
  ["an empty byte string", "0101004c0101400001", "40", "16100/200"],
  [
    "a definite list ending in an empty byte string",
    "0101004c010281400001",
    "9f40ff",
    "16100/200",
  ],
  [
    "a definite list of two empty byte strings",
    "0101004c01038240400001",
    "9f4040ff",
    "16100/200",
  ],
  [
    "constructor 1 over a definite list ending in an empty byte string",
    "0101004c0104d87a81400001",
    "d87a9f40ff",
    "16100/200",
  ],
  [
    "constructor 0 over an indefinite list ending in an empty byte string",
    "0101004c0105d8799f40ff0001",
    "d8799f40ff",
    "16100/200",
  ],
  [
    "a map whose last value is an empty byte string",
    "0101004c0103a100400001",
    "a10040",
    "16100/200",
  ],
  [
    "a map whose last key and value are empty byte strings",
    "0101004c0103a140400001",
    "a14040",
    "16100/200",
  ],
  [
    "a map ending in an empty byte string after a six-byte key",
    "0101004c0109a146c84797cfa721400001",
    "a146c84797cfa72140",
    "16100/200",
  ],
  [
    "a two-entry map ending in an empty byte string",
    "0101004c010da2448d3ed5d702448d3ed5d7400001",
    "a2448d3ed5d702448d3ed5d740",
    "16100/200",
  ],
  [
    "a list with an integer after an empty byte string",
    "0101004c01038240000001",
    "9f4000ff",
    "16100/200",
  ],
  [
    "a list with an empty chunk list after an empty byte string",
    "0101004c010482405fff0001",
    "9f4040ff",
    "16100/200",
  ],
  ["an empty chunk list", "0101004c01025fff0001", "40", "16100/200"],
  [
    "a list ending in an empty chunk list",
    "0101004c0103815fff0001",
    "9f40ff",
    "16100/200",
  ],
  [
    "a bignum over an empty byte string",
    "0101004c0102c2400001",
    "00",
    "16100/200",
  ],
  [
    "a negative bignum over an empty byte string",
    "0101004c0102c3400001",
    "20",
    "16100/200",
  ],
  [
    "a list ending in a bignum over an empty byte string",
    "0101004c010381c2400001",
    "9f00ff",
    "16100/200",
  ],
  [
    "a list ending in an empty list",
    "0101004c010281800001",
    "9f80ff",
    "16100/200",
  ],
  [
    "a list ending in an empty indefinite list",
    "0101004c0103819fff0001",
    "9f80ff",
    "16100/200",
  ],
  [
    "a list ending in an empty map",
    "0101004c010281a00001",
    "9fa0ff",
    "16100/200",
  ],
] as const;

/** Programs that apply one builtin: [label, raw Flat, Cardano result, cpu/mem]. */
export const EMPTY_BYTES_RUNS = [
  [
    "serialiseData of an empty byte string",
    "0101003766980101400001",
    "(con bytestring #40)",
    "2070166/410",
  ],
  [
    "serialiseData of a list ending in an empty byte string",
    "010100376698010281400001",
    "(con bytestring #9f40ff)",
    "2923414/418",
  ],
  [
    "serialiseData of a list of two empty byte strings",
    "01010037669801038240400001",
    "(con bytestring #9f4040ff)",
    "3989974/428",
  ],
  [
    "serialiseData of constructor 1 ending in an empty byte string",
    "0101003766980104d87a81400001",
    "(con bytestring #d87a9f40ff)",
    "2923414/418",
  ],
  [
    "serialiseData of a map whose last value is an empty byte string",
    "0101003766980103a100400001",
    "(con bytestring #a10040)",
    "3989974/428",
  ],
  [
    "serialiseData of a map whose last key and value are empty byte strings",
    "0101003766980103a140400001",
    "(con bytestring #a14040)",
    "3989974/428",
  ],
  [
    "serialiseData of a list with an integer after an empty byte string",
    "01010037669801038240000001",
    "(con bytestring #9f4000ff)",
    "3989974/428",
  ],
  [
    "serialiseData of an empty chunk list",
    "01010037669801025fff0001",
    "(con bytestring #40)",
    "2070166/410",
  ],
  [
    "unBData of an empty byte string",
    "010100375c980101400001",
    "(con bytestring #)",
    "68242/432",
  ],
  [
    "unBData of an empty chunk list",
    "010100375c9801025fff0001",
    "(con bytestring #)",
    "68242/432",
  ],
  [
    "equalsData of an empty byte string and an empty chunk list",
    "0101003375e9810140004c01025fff0001",
    "(con bool True)",
    "1114643/601",
  ],
  [
    "equalsData of two lists ending in an empty byte string",
    "0101003375e981028140004c0103815fff0001",
    "(con bool True)",
    "1223759/601",
  ],
  [
    "serialiseData of bData of an empty byte string",
    "01010037666ea522010001",
    "(con bytestring #40)",
    "2113349/642",
  ],
] as const;

/**
 * Programs whose builtin returns Data, a list or a pair: [label, raw Flat,
 * Cardano result, the result constant's type and payload CBOR as the
 * TypeScript builder writes them (null where the result is built over map
 * conversion steps), cpu/mem].
 */
export const EMPTY_BYTES_STRUCTURED_RUNS = [
  [
    "bData of an empty byte string",
    "010100375291010001",
    "(con data (B #))",
    "9f08ff:40",
    "59283/432",
  ],
  [
    "unListData of a list ending in an empty byte string",
    "010100375898010281400001",
    "(con (list data) [B #])",
    "9f0508ff:9f40ff",
    "74033/432",
  ],
  [
    "unConstrData of constructor 1 ending in an empty byte string",
    "0101003754980104d87a81400001",
    "(con (pair integer (list data)) (1, [B #]))",
    "9f06000508ff:d8799f019f40ffff",
    "72688/432",
  ],
  [
    "unMapData of a map whose last value is an empty byte string",
    "0101003756980103a100400001",
    "(con (list (pair data data)) [(I 0, B #)])",
    null,
    "72723/432",
  ],
] as const;

/**
 * One-constant programs Cardano refuses to deserialise: [label, raw Flat,
 * Cardano's CBOR decoder failure and the payload byte it names, Flat bit
 * offset, the TypeScript reader's refusal].
 */
export const EMPTY_BYTES_REFUSED = [
  [
    "a byte string head claiming one byte at the end",
    "0101004c0101410001",
    "end of input at byte 1",
    34,
    "CBOR Plutus Data ends before its item does",
  ],
  [
    "a list ending in a byte string head claiming one byte",
    "0101004c010281410001",
    "end of input at byte 2",
    34,
    "CBOR Plutus Data ends before its item does",
  ],
  [
    "a map ending in a byte string head claiming one byte",
    "0101004c0103a100410001",
    "end of input at byte 3",
    34,
    "CBOR Plutus Data ends before its item does",
  ],
  [
    "constructor 1 ending in a byte string head claiming one byte",
    "0101004c0104d87a81410001",
    "end of input at byte 4",
    34,
    "CBOR Plutus Data ends before its item does",
  ],
  [
    "an empty text string",
    "0101004c0101600001",
    "unrecognized value of type TypeString at byte 0",
    34,
    "invalid CBOR major type for Plutus Data",
  ],
  [
    "a list ending in an empty text string",
    "0101004c010281600001",
    "unrecognized value of type TypeString at byte 1",
    34,
    "invalid CBOR major type for Plutus Data",
  ],
] as const;
