/**
 * Argument builders shared by the `cek-builtin-cardano-v1` case modules: the
 * per-tag seeded generator, the JSON argument encoders, deterministic keys and
 * the BLS point pools. Every value is a pure function of its seed.
 */
import { createHash } from "node:crypto";

import { bls12_381 } from "@noble/curves/bls12-381.js";
import { ed25519 } from "@noble/curves/ed25519.js";
import { secp256k1 } from "@noble/curves/secp256k1.js";

export const CEK_BUILTIN_CARDANO_MAX_TAG = 86;

/** mulberry32, seeded per tag. */
export const makeRng = (seed) => {
  let state = seed >>> 0;
  const next = () => {
    state = (state + 0x6d2b79f5) >>> 0;
    let t = state;
    t = Math.imul(t ^ (t >>> 15), t | 1);
    t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
    return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
  };
  const int = (bound) => Math.floor(next() * bound);
  return {
    int,
    chance: (probability) => next() < probability,
    pick: (items) => items[int(items.length)],
  };
};

export const hex = (value) => Buffer.from(value).toString("hex");
export const fromHex = (value) => Uint8Array.from(Buffer.from(value, "hex"));
export const int = (value) => ({ ty: "int", v: BigInt(value).toString(10) });
export const bs = (value) => ({
  ty: "bs",
  v: typeof value === "string" ? value : hex(value),
});
export const boolArg = (value) => ({ ty: "bool", v: value });
export const strArg = (value) => ({ ty: "str", v: value });

export const INT64_MAX = (1n << 63n) - 1n;
export const INT64_MIN = -(1n << 63n);

export const EDGE_INTEGERS = [
  0n,
  1n,
  -1n,
  2n,
  -2n,
  7n,
  8n,
  23n,
  24n,
  -24n,
  -25n,
  255n,
  256n,
  65535n,
  65536n,
  (1n << 32n) - 1n,
  1n << 32n,
  INT64_MAX,
  INT64_MAX + 1n,
  INT64_MIN,
  INT64_MIN - 1n,
  (1n << 64n) - 1n,
  1n << 64n,
  -(1n << 64n),
  1n << 128n,
  -(1n << 128n),
  (1n << 520n) + 12345n,
];

export const BYTE_LENGTHS = [0, 1, 2, 3, 5, 8, 31, 32, 33, 63, 64, 65, 128];
export const STRINGS = ["", "a", "abc", "é", "日本語", "\u{1f600}"];
export const UTF8_CASES = [
  "",
  "61",
  "c3a9",
  "e697a5",
  "f09f9880",
  "ff",
  "c080",
  "eda080",
  "e697",
  "f4908080",
];

export const randomBytes = (
  rng,
  length = rng.chance(0.4) ? rng.pick(BYTE_LENGTHS) : rng.int(12),
) => Uint8Array.from({ length }, () => rng.int(256));

export const randomInteger = (rng) =>
  rng.chance(0.5)
    ? rng.pick(EDGE_INTEGERS)
    : BigInt(rng.int(2_000_001) - 1_000_000);

export const randomString = (rng) =>
  rng.chance(0.5)
    ? rng.pick(STRINGS)
    : Array.from({ length: rng.int(10) }, () =>
        String.fromCharCode(97 + rng.int(26)),
      ).join("");

export const CONSTR_INDEXES = [
  0n,
  1n,
  6n,
  7n,
  127n,
  128n,
  1000n,
  (1n << 64n) - 1n,
];

export const randomData = (rng, maxDepth = 3) => {
  const build = (depth) => {
    const leaf = depth >= maxDepth || rng.chance(0.35);
    const kind = leaf ? rng.int(2) : 2 + rng.int(3);
    if (kind === 0) return { i: randomInteger(rng).toString(10) };
    if (kind === 1) {
      const length = rng.chance(0.3)
        ? rng.pick([0, 1, 63, 64, 65, 128])
        : rng.int(8);
      return { b: hex(randomBytes(rng, length)) };
    }
    const width = rng.int(4);
    if (kind === 2) {
      return { l: Array.from({ length: width }, () => build(depth + 1)) };
    }
    if (kind === 3) {
      const entries = [];
      for (let index = 0; index < width; index++) {
        const key =
          entries.length > 0 && rng.chance(0.15)
            ? rng.pick(entries)[0]
            : build(depth + 1);
        entries.push([key, build(depth + 1)]);
      }
      return { m: entries };
    }
    return {
      c: rng.pick(CONSTR_INDEXES).toString(10),
      f: Array.from({ length: width }, () => build(depth + 1)),
    };
  };
  return build(0);
};

export const SIMPLE = ["int", "bs", "str", "unit", "bool", "data"];

export const valueOf = (rng, ty) => {
  if (typeof ty === "object") {
    if ("list" in ty) {
      return Array.from({ length: rng.int(4) }, () => valueOf(rng, ty.list));
    }
    return [valueOf(rng, ty.pair[0]), valueOf(rng, ty.pair[1])];
  }
  switch (ty) {
    case "int":
      return randomInteger(rng).toString(10);
    case "bs":
      return hex(randomBytes(rng));
    case "str":
      return randomString(rng);
    case "unit":
      return null;
    case "bool":
      return rng.chance(0.5);
    case "data":
      return randomData(rng);
    default:
      throw new Error(`no random value of type ${JSON.stringify(ty)}`);
  }
};

export const anyTy = (rng) => {
  const pick = rng.int(8);
  if (pick === 6) return { list: rng.pick(SIMPLE) };
  if (pick === 7) return { pair: [rng.pick(SIMPLE), rng.pick(SIMPLE)] };
  return SIMPLE[pick];
};

export const any = (rng, ty = anyTy(rng)) => ({ ty, v: valueOf(rng, ty) });

export const relatedBytes = (rng) => {
  const left = randomBytes(rng);
  switch (rng.int(5)) {
    case 0:
      return [left, Uint8Array.from(left)];
    case 1:
      return [left, Uint8Array.from([...left, rng.int(256)])];
    case 2:
      return [Uint8Array.from([...left, rng.int(256)]), left];
    case 3: {
      const right = Uint8Array.from(left.length === 0 ? [0] : left);
      right[rng.int(right.length)] = rng.int(256);
      return [left, right];
    }
    default:
      return [left, randomBytes(rng)];
  }
};

export const tamper = (rng, value) => {
  const copy = Uint8Array.from(value);
  if (copy.length > 0) copy[rng.int(copy.length)] ^= 1 << rng.int(8);
  return copy;
};

/* ---------- keys ---------- */

export const seedBytes = (label) =>
  Uint8Array.from(createHash("sha256").update(label).digest());

export const ED_SECRET = seedBytes("cek-builtin-cardano-v1/ed25519");
export const ED_KEY = ed25519.getPublicKey(ED_SECRET);
export const EC_SECRET = seedBytes("cek-builtin-cardano-v1/secp256k1");
export const EC_KEY = secp256k1.getPublicKey(EC_SECRET, true);
export const SECP256K1_N = secp256k1.Point.Fn.ORDER;
export const SECP256K1_P = secp256k1.Point.Fp.ORDER;
export const ED25519_L = ed25519.Point.Fn.ORDER;
export const ED25519_P = ed25519.Point.Fp.ORDER;

export const be32 = (value) => fromHex(value.toString(16).padStart(64, "0"));
export const le32 = (value) => be32(value).reverse();
export const leInt = (value) =>
  BigInt(`0x${hex(Uint8Array.from(value).reverse()) || "0"}`);

export const ed25519Case = (rng) => {
  const message = randomBytes(rng);
  const signature = ed25519.sign(message, ED_SECRET);
  const mode = rng.int(5);
  return [
    bs(mode === 3 ? ED_KEY.subarray(0, 31) : ED_KEY),
    bs(mode === 2 ? tamper(rng, message) : message),
    bs(
      mode === 1
        ? tamper(rng, signature)
        : mode === 4
          ? signature.subarray(0, 63)
          : signature,
    ),
  ];
};

export const sha256 = (value) =>
  Uint8Array.from(createHash("sha256").update(value).digest());

export const ecdsaSign = (digest) =>
  secp256k1.sign(digest, EC_SECRET, { prehash: false, lowS: true });

export const ecdsaCase = (rng) => {
  const digest = sha256(randomBytes(rng));
  const signature = ecdsaSign(digest);
  const mode = rng.int(6);
  return [
    bs(mode === 3 ? Uint8Array.from([4, ...EC_KEY.subarray(1)]) : EC_KEY),
    bs(
      mode === 2
        ? tamper(rng, digest)
        : mode === 5
          ? digest.subarray(0, 31)
          : digest,
    ),
    bs(
      mode === 1
        ? tamper(rng, signature)
        : mode === 4
          ? signature.subarray(0, 63)
          : signature,
    ),
  ];
};

/** BIP-340 test vector 0 (valid) and variants of it. */
export const SCHNORR_KEY = fromHex(
  "f9308a019258c31049344f85f89d5229b531c845836f99b08601f113bce036f9",
);
export const SCHNORR_MESSAGE = new Uint8Array(32);
export const SCHNORR_SIGNATURE = fromHex(
  "e907831f80848d1069a5371b402410364bdf1c5f8307b0084c55f1ce2dca821525f66a4a85ea8b71e482a74f382d2ce5ebeee8fdb2172f477df4900d310536c0",
);

export const schnorrCase = (rng) => {
  const mode = rng.int(5);
  return [
    bs(mode === 3 ? SCHNORR_KEY.subarray(0, 31) : SCHNORR_KEY),
    bs(mode === 2 ? tamper(rng, SCHNORR_MESSAGE) : SCHNORR_MESSAGE),
    bs(
      mode === 1
        ? tamper(rng, SCHNORR_SIGNATURE)
        : mode === 4
          ? SCHNORR_SIGNATURE.subarray(0, 63)
          : SCHNORR_SIGNATURE,
    ),
  ];
};

/* ---------- BLS points ---------- */

export const G1 = bls12_381.G1.Point;
export const G2 = bls12_381.G2.Point;
export const SCALARS = [1n, 2n, 3n, 7n, 0xdeadbeefn, (1n << 200n) + 17n];
export const G1_POOL = [
  G1.ZERO,
  ...SCALARS.map((k) => G1.BASE.multiply(k)),
].map((point) => hex(point.toBytes()));
export const G2_POOL = [
  G2.ZERO,
  ...SCALARS.map((k) => G2.BASE.multiply(k)),
].map((point) => hex(point.toBytes()));
export const g1Of = (k) => hex(G1.BASE.multiply(k).toBytes());
export const g2Of = (k) => hex(G2.BASE.multiply(k).toBytes());

export const compressedCase = (rng, pool, length) => {
  switch (rng.int(6)) {
    case 0:
      return hex(randomBytes(rng, length));
    case 1:
      return hex(randomBytes(rng, length - 1));
    case 2: {
      const flagged = new Uint8Array(length);
      flagged[0] = 0xc0;
      flagged[length - 1] = 1;
      return hex(flagged);
    }
    case 3:
      return hex(tamper(rng, fromHex(rng.pick(pool))));
    default:
      return rng.pick(pool);
  }
};

export const dst = (rng) =>
  hex(randomBytes(rng, rng.pick([0, 1, 16, 255, 256])));

export const loop = (g1, g2) => ({ loop: [g1, g2] });
export const mul = (left, right) => ({ mul: [left, right] });
export const ml = (value) => ({ ty: "ml", v: value });

export const randomMl = (rng, depth = 0) =>
  depth < 2 && rng.chance(0.35)
    ? mul(randomMl(rng, depth + 1), randomMl(rng, depth + 1))
    : loop(rng.pick(G1_POOL), rng.pick(G2_POOL));
