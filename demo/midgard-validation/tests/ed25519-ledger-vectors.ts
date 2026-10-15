import { createHash } from "node:crypto";

import { CML } from "@lucid-evolution/lucid";
import { ed25519, ED25519_TORSION_SUBGROUP } from "@noble/curves/ed25519.js";
import { sha512 } from "@noble/hashes/sha2.js";

import { evaluateCardanoExactBuiltin } from "../src/cek-builtin.cardano-exact.js";
import { TEST_PRIVATE_KEY } from "./validation-fixtures.make-min-ada-funded-exact-size-output-item.js";

/**
 * Ed25519 vectors for checking that an off-chain verifier answers exactly as
 * the ledger does. The reference is `evaluateCardanoExactBuiltin(21, ...)` in
 * src/cek-builtin.cardano-exact.ts, the verifier the cek-builtin-cardano-v1
 * goldens pin. Its checks, in order:
 *   - :109 a key that is not 32 bytes or a signature that is not 64 bytes is
 *     a failure;
 *   - :112 False when S is not below L, or when R is a small-order encoding:
 *     by :79-94, the y-coordinate with the sign bit ignored, reduced mod p, is
 *     that of one of the eight small-order points, so the two encodings at or
 *     above p count;
 *   - :113-115 False when the key's y is at or above p, or the key is a
 *     small-order encoding by the same rule;
 *   - :116-121 False when the key does not decode to a point;
 *   - :122-128 otherwise True exactly when [S]B - [h]A encodes to R, with
 *     h = SHA-512(R || A || M) mod L.
 */
export type Ed25519Vector = {
  readonly label: string;
  readonly vkey: Buffer;
  readonly message: Buffer;
  readonly signature: Buffer;
};

export const ED25519_P = ed25519.Point.Fp.ORDER;
export const ED25519_L = ed25519.Point.Fn.ORDER;

const littleEndian32 = (value: bigint): Buffer => {
  const out = Buffer.alloc(32);
  let rest = value;
  for (let index = 0; index < 32; index += 1) {
    out[index] = Number(rest & 0xffn);
    rest >>= 8n;
  }
  return out;
};

const littleEndianInteger = (bytes: Uint8Array): bigint =>
  BigInt(`0x${Buffer.from(bytes).reverse().toString("hex") || "0"}`);

const withSignBit = (encoding: Buffer, set: boolean): Buffer => {
  const out = Buffer.from(encoding);
  out[31] = set ? out[31]! | 0x80 : out[31]! & 0x7f;
  return out;
};

const uniqueEncodings = (encodings: readonly Buffer[]): readonly Buffer[] => [
  ...new Map(
    encodings.map((encoding) => [encoding.toString("hex"), encoding]),
  ).values(),
];

/** The verdict of the ledger's `verifyEd25519Signature` builtin. */
export const ledgerEd25519Verdict = (vector: Ed25519Vector): boolean => {
  const result = evaluateCardanoExactBuiltin(21, [
    { kind: "bytes", value: vector.vkey },
    { kind: "bytes", value: vector.message },
    { kind: "bytes", value: vector.signature },
  ]);
  if (result === "failure" || result.kind !== "bool") {
    throw new Error(`${vector.label}: the builtin has no verdict`);
  }
  return result.value;
};

/**
 * Every encoding of a small-order point: the eight canonical encodings with
 * either sign bit, and the encodings of y = p and y = p + 1 (y = 0 and 1)
 * with either sign bit.
 */
export const ED25519_SMALL_ORDER_ENCODINGS: readonly Buffer[] = uniqueEncodings(
  [
    ...ED25519_TORSION_SUBGROUP.map((hex) => Buffer.from(hex, "hex")),
    littleEndian32(ED25519_P),
    littleEndian32(ED25519_P + 1n),
  ].flatMap((encoding) => [
    withSignBit(encoding, false),
    withSignBit(encoding, true),
  ]),
);

const IDENTITY_ENCODING = littleEndian32(1n);

/** A deterministic 32-byte seed for signer `index`. */
export const signerSeed = (index: number): Buffer =>
  createHash("sha256").update(`ed25519-ledger-signer-${index}`).digest();

export const signerPublicKey = (seed: Uint8Array): Buffer =>
  Buffer.from(ed25519.getPublicKey(seed));

export const honestSignature = (
  seed: Uint8Array,
  message: Uint8Array,
): Buffer => Buffer.from(ed25519.sign(message, seed));

/**
 * A signature by the key's own secret whose R is the given encoding and
 * whose S is h·a mod L. For R the identity, [S]B - [h]A is the identity, so
 * the equation holds without the cofactor while R has small order.
 */
export const ownKeySignatureWithR = (
  seed: Uint8Array,
  message: Uint8Array,
  encodedR: Buffer,
): Buffer => {
  const { scalar, pointBytes } = ed25519.utils.getExtendedPublicKey(seed);
  const h =
    littleEndianInteger(
      sha512(Buffer.concat([encodedR, pointBytes, message])),
    ) % ED25519_L;
  return Buffer.concat([encodedR, littleEndian32((h * scalar) % ED25519_L)]);
};

export const ownKeyIdentityRSignature = (
  seed: Uint8Array,
  message: Uint8Array,
): Buffer => ownKeySignatureWithR(seed, message, IDENTITY_ENCODING);

const withS = (signature: Buffer, s: bigint): Buffer =>
  Buffer.concat([signature.subarray(0, 32), littleEndian32(s)]);

const sOf = (signature: Buffer): bigint =>
  littleEndianInteger(signature.subarray(32));

/** A signature by `seed` for key A + T, where T is a small-order point. */
const mixedOrderVector = (
  seed: Uint8Array,
  message: Buffer,
  torsionHex: string,
): Ed25519Vector => {
  const { prefix, scalar, point } = ed25519.utils.getExtendedPublicKey(seed);
  const vkey = Buffer.from(
    point.add(ed25519.Point.fromHex(torsionHex)).toBytes(),
  );
  const r =
    littleEndianInteger(sha512(Buffer.concat([prefix, message]))) % ED25519_L;
  const encodedR = Buffer.from(ed25519.Point.BASE.multiply(r).toBytes());
  const h =
    littleEndianInteger(sha512(Buffer.concat([encodedR, vkey, message]))) %
    ED25519_L;
  return {
    label: `mixed-order key A+${torsionHex}`,
    vkey,
    message,
    signature: Buffer.concat([
      encodedR,
      littleEndian32((r + h * scalar) % ED25519_L),
    ]),
  };
};

const hex = (bytes: Buffer): string => bytes.toString("hex");

/** The named vectors over `message`. */
export const namedEd25519Vectors = (message: Buffer): Ed25519Vector[] => {
  const seed = signerSeed(0);
  const vkey = signerPublicKey(seed);
  const honest = honestSignature(seed, message);
  const vectors: Ed25519Vector[] = [];
  for (let index = 0; index < 4; index += 1) {
    const signer = signerSeed(index);
    vectors.push({
      label: `honest signer ${index}`,
      vkey: signerPublicKey(signer),
      message,
      signature: honestSignature(signer, message),
    });
  }
  for (const encodedR of ED25519_SMALL_ORDER_ENCODINGS) {
    vectors.push({
      label: `own key, R ${hex(encodedR)}, S h·a`,
      vkey,
      message,
      signature: ownKeySignatureWithR(seed, message, encodedR),
    });
    vectors.push({
      label: `own key, R ${hex(encodedR)}, honest S`,
      vkey,
      message,
      signature: Buffer.concat([encodedR, honest.subarray(32)]),
    });
  }
  for (const key of ED25519_SMALL_ORDER_ENCODINGS) {
    for (const encodedR of ED25519_SMALL_ORDER_ENCODINGS) {
      for (const s of [0n, 1n]) {
        vectors.push({
          label: `key ${hex(key)}, R ${hex(encodedR)}, S ${s}`,
          vkey: key,
          message,
          signature: Buffer.concat([encodedR, littleEndian32(s)]),
        });
      }
    }
    vectors.push({
      label: `key ${hex(key)}, honest signature`,
      vkey: key,
      message,
      signature: honest,
    });
  }
  for (let offset = 0n; offset < 19n; offset += 1n) {
    for (const sign of [false, true]) {
      const key = withSignBit(littleEndian32(ED25519_P + offset), sign);
      vectors.push(
        {
          label: `key y = p+${offset}${sign ? ", sign set" : ""}, honest signature`,
          vkey: key,
          message,
          signature: honest,
        },
        {
          label: `key y = p+${offset}${sign ? ", sign set" : ""}, R identity, S 0`,
          vkey: key,
          message,
          signature: Buffer.concat([IDENTITY_ENCODING, littleEndian32(0n)]),
        },
      );
    }
  }
  for (let y = 2n, found = 0; found < 8; y += 1n) {
    const key = littleEndian32(y);
    try {
      ed25519.Point.fromBytes(key);
    } catch {
      found += 1;
      vectors.push({
        label: `off-curve key y = ${y}`,
        vkey: key,
        message,
        signature: honest,
      });
    }
  }
  const s = sOf(honest);
  for (const [label, value] of [
    ["S + L", s + ED25519_L],
    ["S + 2L", s + 2n * ED25519_L],
    ["S = L", ED25519_L],
    ["S = L - 1", ED25519_L - 1n],
    ["S = 2^256 - 1", (1n << 256n) - 1n],
  ] as const) {
    if (value < 1n << 256n) {
      vectors.push({
        label: `honest R, ${label}`,
        vkey,
        message,
        signature: withS(honest, value),
      });
    }
  }
  for (const torsionHex of ED25519_TORSION_SUBGROUP.slice(1)) {
    vectors.push(mixedOrderVector(seed, message, torsionHex));
  }
  vectors.push(
    {
      label: "all-zero key and signature",
      vkey: Buffer.alloc(32),
      message,
      signature: Buffer.alloc(64),
    },
    {
      label: "all-0xff key and signature",
      vkey: Buffer.alloc(32, 0xff),
      message,
      signature: Buffer.alloc(64, 0xff),
    },
  );
  return vectors;
};

/**
 * A seeded corpus: random keys and signatures, honest signatures, honest
 * signatures with one bit flipped, and random signatures with S below L on
 * honest keys, in rotation.
 */
export function* seededEd25519Corpus(
  corpusSeed: string,
  count: number,
): Generator<Ed25519Vector> {
  const block = (index: number, part: string): Buffer =>
    createHash("sha512").update(`${corpusSeed}:${index}:${part}`).digest();
  for (let index = 0; index < count; index += 1) {
    const message = block(index, "message").subarray(0, 32);
    const seed = block(index, "seed").subarray(0, 32);
    const random = block(index, "random");
    const label = `corpus ${corpusSeed} #${index}`;
    switch (index % 4) {
      case 0:
        yield {
          label,
          vkey: random.subarray(0, 32),
          message,
          signature: Buffer.concat([
            random.subarray(32, 64),
            block(index, "s").subarray(0, 32),
          ]),
        };
        break;
      case 1:
        yield {
          label,
          vkey: signerPublicKey(seed),
          message,
          signature: honestSignature(seed, message),
        };
        break;
      case 2: {
        const signature = honestSignature(seed, message);
        const bit = random[0]! % 512;
        signature[bit >> 3] ^= 1 << (bit & 7);
        yield { label, vkey: signerPublicKey(seed), message, signature };
        break;
      }
      default:
        yield {
          label,
          vkey: signerPublicKey(seed),
          message,
          signature: Buffer.concat([
            random.subarray(0, 32),
            littleEndian32(
              littleEndianInteger(random.subarray(32)) % ED25519_L,
            ),
          ]),
        };
    }
  }
}

/** The secret seed of the fixtures' default signer, `TEST_PRIVATE_KEY`. */
export const TEST_SIGNER_SEED = Buffer.from(TEST_PRIVATE_KEY.to_raw_bytes());

/** A `vkeywitness` CBOR item carrying exactly these key and signature bytes. */
export const vkeyWitnessCbor = (vkey: Buffer, signature: Buffer): Buffer =>
  Buffer.from(
    CML.Vkeywitness.new(
      CML.PublicKey.from_bytes(vkey),
      CML.Ed25519Signature.from_raw_bytes(signature),
    ).to_cbor_bytes(),
  );

/** An address witness by `seed` with an honest signature over the message. */
export const honestVkeyWitness =
  (seed: Uint8Array) =>
  (message: Buffer): Buffer =>
    vkeyWitnessCbor(signerPublicKey(seed), honestSignature(seed, message));

/**
 * An address witness by `seed` whose R is the identity encoding and whose S
 * is h·a mod L, as {@link ownKeyIdentityRSignature} builds it.
 */
export const ownKeyIdentityRVkeyWitness =
  (seed: Uint8Array) =>
  (message: Buffer): Buffer =>
    vkeyWitnessCbor(
      signerPublicKey(seed),
      ownKeyIdentityRSignature(seed, message),
    );
