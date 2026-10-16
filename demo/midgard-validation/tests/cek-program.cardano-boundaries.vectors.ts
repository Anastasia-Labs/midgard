/**
 * Raw Flat programs at the 64-bit integer and constructor-128 boundaries, read
 * and run by the Plutus V3 evaluator (plutus-core 1.71, protocol version 11)
 * the same way as the rows in `cek-program.data-constant-spelling.vectors.ts`.
 * Each budget includes the 100/100 start-up charge. Cardano sizes an integer
 * in 64-bit words, so from |x| = 2^64 on an integer is two words.
 */

/** Programs that apply one builtin: [label, raw Flat, Cardano result, cpu/mem]. */
export const CARDANO_BOUNDARY_RUNS = [
  [
    "addInteger 2^64-1 and 1",
    "01010033700907f7fffffffffffffff81a4005",
    "(con integer 18446744073709551616)",
    "181308/602",
  ],
  [
    "addInteger 2^64 and -1",
    "01010033700904040404040404040400224003",
    "(con integer 18446744073709551615)",
    "181728/603",
  ],
  [
    "addInteger -2^64 and -1",
    "01010033700907fffffffffffffffff81a4003",
    "(con integer -18446744073709551617)",
    "181728/603",
  ],
  [
    "addInteger -2^64-1 and 1",
    "010100337009040c0404040404040400224005",
    "(con integer -18446744073709551616)",
    "181728/603",
  ],
  [
    "addInteger 2^64-1 and 0",
    "01010033700907f7fffffffffffffff81a4001",
    "(con integer 18446744073709551615)",
    "181308/602",
  ],
  [
    "addInteger -2^64 and 0",
    "01010033700907fffffffffffffffff81a4001",
    "(con integer -18446744073709551616)",
    "181728/603",
  ],
  [
    "equalsData -2^64 and -2^64-1",
    "0101003375e981093bffffffffffffffff004c010bc3490100000000000000000001",
    "(con bool False)",
    "1141922/601",
  ],
  [
    "equalsData -2^64 and -2^64",
    "0101003375e981093bffffffffffffffff004c01093bffffffffffffffff0001",
    "(con bool True)",
    "1141922/601",
  ],
  [
    "equalsData 2^64-1 and 2^64",
    "0101003375e981091bffffffffffffffff004c010bc2490100000000000000000001",
    "(con bool False)",
    "1114643/601",
  ],
  [
    "equalsData 2^64 and 2^64",
    "0101003375e9810bc249010000000000000000004c010bc2490100000000000000000001",
    "(con bool True)",
    "1141922/601",
  ],
  [
    "equalsData constructor 127 and 128",
    "0101003375e98104d9057880004c0106d866821880800001",
    "(con bool False)",
    "1087364/601",
  ],
  [
    "serialiseData constructor 128",
    "0101003766980106d866821880800001",
    "(con bytestring #d86682188080)",
    "1856854/408",
  ],
  [
    "serialiseData constructor 128 over 2^64",
    "0101003766980112d8668218809fc249010000000000000000ff0001",
    "(con bytestring #d8668218809fc249010000000000000000ff)",
    "3136726/420",
  ],
  [
    "serialiseData constructor 127 over 2^64-1",
    "010100376698010ed905789f1bffffffffffffffffff0001",
    "(con bytestring #d905789f1bffffffffffffffffff)",
    "2923414/418",
  ],
  [
    "unIData 2^64-1",
    "010100375a9801091bffffffffffffffff0001",
    "(con integer 18446744073709551615)",
    "68844/432",
  ],
  [
    "unIData 2^64",
    "010100375a98010bc2490100000000000000000001",
    "(con integer 18446744073709551616)",
    "68844/432",
  ],
  [
    "unIData -2^64",
    "010100375a9801093bffffffffffffffff0001",
    "(con integer -18446744073709551616)",
    "68844/432",
  ],
  [
    "unIData -2^64-1",
    "010100375a98010bc3490100000000000000000001",
    "(con integer -18446744073709551617)",
    "68844/432",
  ],
  [
    "fstPair of unConstrData constructor 128",
    "01010035573a6ea9300106d866821880800001",
    "(con integer 128)",
    "278583/864",
  ],
  [
    "fstPair of unConstrData constructor 127",
    "01010035573a6ea9300104d90578800001",
    "(con integer 127)",
    "278583/864",
  ],
] as const;

/** Programs that are one constant: [label, raw Flat, payload CBOR, cpu/mem]. */
export const CARDANO_BOUNDARY_CONSTANTS = [
  [
    "integer 2^64-1",
    "010100483fbfffffffffffffffc0c1",
    "1bffffffffffffffff",
    "16100/200",
  ],
  [
    "integer 2^64",
    "010100482020202020202020200101",
    "c249010000000000000000",
    "16100/200",
  ],
  [
    "integer -2^64",
    "010100483fffffffffffffffffc0c1",
    "3bffffffffffffffff",
    "16100/200",
  ],
  [
    "integer -2^64-1",
    "010100482060202020202020200101",
    "c349010000000000000000",
    "16100/200",
  ],
  ["data constructor 127", "0101004c0104d90578800001", "d9057880", "16100/200"],
  [
    "data constructor 128",
    "0101004c0106d866821880800001",
    "d86682188080",
    "16100/200",
  ],
] as const;

// [label, raw Flat, Cardano decode error, bit offset, refusal detail]
export const CARDANO_BOUNDARY_REFUSED = [
  [
    "tag 2 under a two-byte head over 2^64",
    "010100376698010cd802490100000000000000000001",
    "Unrecognized tag 2",
    49,
    "unknown CBOR tag 2 in Plutus Data",
  ],
  [
    "tag 3 under a two-byte head over 2^64",
    "010100376698010cd803490100000000000000000001",
    "Unrecognized tag 3",
    49,
    "unknown CBOR tag 3 in Plutus Data",
  ],
] as const;
