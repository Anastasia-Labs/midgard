/**
 * The hand-spelled CBOR, script and output helpers the `forced-tx-admission-v1`
 * rows are written with, and the transaction field indices. Read by
 * `generate-forced-tx-admission-v1-goldens.rows.mjs`.
 */

import { blake2b } from "@noble/hashes/blake2.js";

export const fail = (message) => {
  throw new Error(`forced-tx-admission-v1: ${message}`);
};

// ## CBOR, spelled by hand

export const cat = (...parts) =>
  Buffer.concat(parts.map((part) => Buffer.from(part)));
export const filled = (length, byte) => Buffer.alloc(length, byte);
export const head = (major, value) => {
  const n = BigInt(value);
  const base = major << 5;
  if (n < 24n) return Buffer.from([base | Number(n)]);
  if (n < 0x100n) return Buffer.from([base | 24, Number(n)]);
  if (n < 0x10000n) return cat([base | 25], bigEndian(n, 2));
  if (n < 0x100000000n) return cat([base | 26], bigEndian(n, 4));
  return cat([base | 27], bigEndian(n, 8));
};
export const bigEndian = (value, width) => {
  const out = Buffer.alloc(width);
  let n = BigInt(value);
  for (let index = width - 1; index >= 0; index -= 1) {
    out[index] = Number(n & 0xffn);
    n >>= 8n;
  }
  return out;
};
export const uint = (value) => head(0, value);
export const bstr = (bytes) => cat(head(2, bytes.length), bytes);
export const mapHead = (count) => head(5, count);

/** §5.1: the array header, then every item as a definite byte string. */
export const field = (items) => cat(head(4, items.length), ...items.map(bstr));
export const EMPTY = field([]);

export const blake2b256 = (bytes) => Buffer.from(blake2b(bytes, { dkLen: 32 }));
export const EMPTY_NULL_HASH = blake2b256(Buffer.from([0xf6]));

// ## Items

export const spendInput = (byte, index) =>
  cat([0x82], bstr(filled(32, byte)), [0x19], bigEndian(index, 2));
export const hash28 = (byte) => filled(28, byte);
export const addressWitness = (byte) =>
  cat([0x82], bstr(filled(32, byte)), bstr(filled(64, byte)));

/** A versioned script: `82 ‖ tag ‖ bytes`. */
export const versionedScript = (tag, scriptBytes) =>
  cat([0x82], uint(tag), bstr(scriptBytes));
export const NATIVE = 0;
export const PLUTUS_V3 = 3;
export const MIDGARD_V1 = 128;
/** Native `[0, key hash]`: a signature script. */
export const nativeSignatureScript = cat([0x82, 0x00], bstr(hash28(0xcc)));
/** Native `[5, slot]`: InvalidHereafter, satisfiable only by an upper bound. */
export const nativeBeforeScript = cat([0x82, 0x05], uint(10_000_000));
export const plutusProgram = Buffer.from(
  "4e4d01000033222220051200120011",
  "hex",
);

export const address = cat([0x61], hash28(0x77));
/** `82 ‖ coin ‖ {}`: a value with no assets. */
export const adaValue = cat([0x82], uint(2_000_000), mapHead(0));

/** `{0: address, 1: value, ?2: datum, ?3: script_ref}`. */
export const output = ({ value = adaValue, datum, scriptRef }) => {
  const entries = [
    [0, bstr(address)],
    [1, value],
    ...(datum === undefined ? [] : [[2, bstr(datum)]]),
    ...(scriptRef === undefined ? [] : [[3, scriptRef]]),
  ];
  return cat(
    mapHead(entries.length),
    ...entries.map(([key, entry]) => cat(uint(key), entry)),
  );
};

/**
 * A value of exactly `size` bytes, `82 ‖ coin ‖ {policy => {name => 1}}`, with
 * every asset name the same length so the names are ascending however a
 * reader orders them.
 */
export const valueOfSize = (size) => {
  for (const coin of [1n, 24n, 256n, 65_536n, 4_294_967_296n]) {
    for (let nameLength = 32; nameLength >= 2; nameLength -= 1) {
      const fixed = 1 + uint(coin).length + 1 + bstr(hash28(0)).length;
      const perAsset = bstr(filled(nameLength, 0)).length + 1;
      for (let assets = 1; assets <= 255; assets += 1) {
        if (fixed + mapHead(assets).length + perAsset * assets !== size) {
          continue;
        }
        const names = Array.from({ length: assets }, (_, index) =>
          bigEndian(index + 1, nameLength),
        );
        return cat(
          [0x82],
          uint(coin),
          mapHead(1),
          bstr(hash28(0x99)),
          mapHead(assets),
          ...names.map((name) => cat(bstr(name), uint(1))),
        );
      }
    }
  }
  return fail(`no value spelling of ${size} bytes`);
};

// ## The admitted base and its neighbours

export const NONCANONICAL_DATUM = Buffer.from("19002a", "hex");
export const DATUM = Buffer.from("182a", "hex");
/** A 100-byte bytestring left unchunked: Plutus data chunks above 64 bytes. */
export const UNCHUNKED_BYTES_DATUM = cat([0x58, 100], filled(100, 0x5a));
/** Two policies, the second with two assets of different name lengths. */
export const MULTI_ASSET_VALUE = cat(
  [0x82],
  uint(5_000_000),
  mapHead(2),
  bstr(hash28(0x91)),
  mapHead(1),
  bstr(Buffer.alloc(0)),
  uint(7),
  bstr(hash28(0x92)),
  mapHead(2),
  bstr(Buffer.from("a")),
  uint(1),
  bstr(Buffer.from("bb")),
  uint(4_294_967_296n),
);

export const base = () => ({
  fields: [
    field([spendInput(0xaa, 3)]),
    EMPTY,
    field([output({})]),
    EMPTY,
    field([hash28(0xbb)]),
    EMPTY,
    EMPTY,
    field([addressWitness(0xdd)]),
    EMPTY,
  ],
  validityIntervalEnd: -1n,
  scriptIntegrityHash: EMPTY_NULL_HASH,
  auxiliaryDataHash: EMPTY_NULL_HASH,
  networkId: 0n,
});

export const SPEND_INPUTS = 0;
export const REFERENCE_INPUTS = 1;
export const OUTPUTS = 2;
export const REQUIRED_OBSERVERS = 3;
export const REQUIRED_SIGNERS = 4;
export const MINT = 5;
export const SCRIPT_WITNESSES = 6;
export const ADDRESS_WITNESSES = 7;
export const REDEEMERS = 8;

export const withFields = (row, entries) => {
  const fields = [...row.fields];
  for (const [index, preimage] of entries) fields[index] = preimage;
  return { ...row, fields };
};
