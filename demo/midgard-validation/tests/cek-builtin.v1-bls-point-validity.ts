import { bls12_381 } from "@noble/curves/bls12-381.js";
import { sha256 } from "@noble/hashes/sha2.js";
import { describe, expect, it } from "vitest";

import {
  BLS12_381_ENDOMORPHISM_BETA,
  BLS12_381_FIELD_PRIME,
  BLS12_381_G1_B,
  BLS12_381_G2_B,
  BLS12_381_GROUP_ORDER,
  BLS12_381_PSI_X_1,
  BLS12_381_PSI_Y,
  BLS12_381_Z,
  blsG1PointIsInvalid,
  blsG2PointIsInvalid,
} from "../src/cek-builtin.bls-point-validity.js";
import {
  evaluateMidgardCekDirectBuiltin,
  verifyMidgardCekDirectBuiltinFailure,
} from "../src/cek-builtin.js";
import { UPLCConst } from "../src/uplc-constant.js";
import {
  direct,
  directBuiltinRoot,
} from "./cek-builtin.v1-builtin-runtime-type-failures.js";

/**
 * G1 and G2 uncompress (59, 66) fail on an encoding off the curve or outside
 * the prime-order subgroup. The point check (the twin of
 * bls12-381-point-v1.ak) is cross-checked against @noble/curves, and the
 * direct failure verifier admits exactly the failures Cardano's builtin has.
 */
const { G1, G2, fields } = bls12_381;
const p = BLS12_381_FIELD_PRIME;
const r = BLS12_381_GROUP_ORDER;
const mod = (value: bigint, modulus = p): bigint =>
  ((value % modulus) + modulus) % modulus;

const bigEndian = (bytes: Uint8Array): bigint =>
  bytes.reduce((value, byte) => (value << 8n) | BigInt(byte), 0n);
const toBytes = (value: bigint, length: number): Uint8Array => {
  const bytes = new Uint8Array(length);
  let rest = value;
  for (let index = length - 1; index >= 0; index -= 1) {
    bytes[index] = Number(rest & 0xffn);
    rest >>= 8n;
  }
  return bytes;
};

/** Deterministic field elements: sha256 of a label and a counter. */
const fieldElement = (label: string, index: number): bigint => {
  const seed = new TextEncoder().encode(`${label}/${index}`);
  return mod(
    bigEndian(new Uint8Array([...sha256(seed), ...sha256(sha256(seed))])),
  );
};

/** A compressed encoding over x (G1) or x0 + x1 u (G2), sort bit clear. */
const g1Encoding = (x: bigint): Uint8Array => {
  const bytes = toBytes(x, 48);
  bytes[0] = bytes[0]! | 0x80;
  return bytes;
};
const g2Encoding = (x0: bigint, x1: bigint): Uint8Array => {
  const bytes = new Uint8Array([...toBytes(x1, 48), ...toBytes(x0, 48)]);
  bytes[0] = bytes[0]! | 0x80;
  return bytes;
};

/** noble's verdict: does the encoding fail to decode into G1 / G2? */
const nobleRejects = (tag: 59n | 66n, encoded: Uint8Array): boolean => {
  try {
    (tag === 59n ? G1 : G2).Point.fromBytes(encoded);
    return false;
  } catch {
    return true;
  }
};

const modPower = (base: bigint, exponent: bigint): bigint => {
  let result = 1n;
  let current = mod(base);
  for (let rest = exponent; rest > 0n; rest >>= 1n) {
    if (rest & 1n) result = mod(result * current);
    current = mod(current * current);
  }
  return result;
};

const g1X = (point: InstanceType<typeof G1.Point>): bigint =>
  point.toAffine().x;
const g2X = (
  point: InstanceType<typeof G2.Point>,
): readonly [bigint, bigint] => {
  const { x } = point.toAffine();
  return [x.c0, x.c1];
};

describe("V1 BLS12-381 point validity constants", () => {
  it("match @noble/curves", () => {
    expect(BLS12_381_FIELD_PRIME).toBe(fields.Fp.ORDER);
    expect(BLS12_381_GROUP_ORDER).toBe(fields.Fr.ORDER);
    expect(BLS12_381_Z).toBe(-bls12_381.params.ateLoopSize);
    expect(BLS12_381_G1_B).toBe(G1.Point.CURVE().b);
    const twistB = G2.Point.CURVE().b;
    expect([twistB.c0, twistB.c1]).toEqual([...BLS12_381_G2_B]);
  });

  it("both cofactors are odd, so neither group has a point of order 2", () => {
    expect(G1.Point.CURVE().h % 2n).toBe(1n);
    expect(G2.Point.CURVE().h % 2n).toBe(1n);
    expect(r % 2n).toBe(1n);
  });

  it("(beta x, y) is [-z^2] on G1", () => {
    expect(BLS12_381_ENDOMORPHISM_BETA).not.toBe(1n);
    expect(mod(BLS12_381_ENDOMORPHISM_BETA ** 3n)).toBe(1n);
    for (const scalar of [1n, 7n, fieldElement("g1-beta", 0) % r]) {
      const point = G1.Point.BASE.multiply(scalar);
      const image = point.multiply(mod(-(BLS12_381_Z ** 2n), r)).toAffine();
      const affine = point.toAffine();
      expect(image.x).toBe(mod(BLS12_381_ENDOMORPHISM_BETA * affine.x));
      expect(image.y).toBe(affine.y);
    }
  });

  it("psi(x, y) = (conj(x) psi_x, conj(y) psi_y) is [z] on G2", () => {
    const { Fp2 } = fields;
    const conj = (value: { c0: bigint; c1: bigint }) =>
      Fp2.create({ c0: value.c0, c1: mod(-value.c1) });
    const psiX = Fp2.create({ c0: 0n, c1: BLS12_381_PSI_X_1 });
    const psiY = Fp2.create({ c0: BLS12_381_PSI_Y[0], c1: BLS12_381_PSI_Y[1] });
    for (const scalar of [1n, 7n, fieldElement("g2-psi", 0) % r]) {
      const point = G2.Point.BASE.multiply(scalar);
      const { x, y } = point.toAffine();
      const image = point.multiply(mod(BLS12_381_Z, r)).toAffine();
      expect(Fp2.eql(image.x, Fp2.mul(conj(x), psiX))).toBe(true);
      expect(Fp2.eql(image.y, Fp2.mul(conj(y), psiY))).toBe(true);
    }
  });
});

describe("V1 BLS12-381 point validity against @noble/curves", () => {
  it("accepts points of G1, independently of order r", () => {
    for (let index = 0; index < 16; index += 1) {
      const point = G1.Point.BASE.multiply(
        1n + (fieldElement("g1-in", index) % (r - 1n)),
      );
      expect(
        point
          .multiplyUnsafe(r - 1n)
          .add(point)
          .is0(),
      ).toBe(true);
      expect(blsG1PointIsInvalid(g1X(point))).toBe(false);
    }
  });

  it("accepts points of G2, independently of order r", () => {
    for (let index = 0; index < 8; index += 1) {
      const point = G2.Point.BASE.multiply(
        1n + (fieldElement("g2-in", index) % (r - 1n)),
      );
      expect(
        point
          .multiplyUnsafe(r - 1n)
          .add(point)
          .is0(),
      ).toBe(true);
      const [x0, x1] = g2X(point);
      expect(blsG2PointIsInvalid(x0, x1)).toBe(false);
    }
  });

  it("agrees with noble on arbitrary G1 x coordinates", () => {
    for (let index = 0; index < 48; index += 1) {
      const x = fieldElement("g1-any", index);
      const rejected = nobleRejects(59n, g1Encoding(x));
      // Off the curve, or (with overwhelming probability) outside G1.
      expect(rejected).toBe(true);
      expect(blsG1PointIsInvalid(x)).toBe(rejected);
    }
    // Small x: (0, 2) has order 3; x = 4 is on the curve; x = 1 is not.
    for (const x of [0n, 1n, 2n, 3n, 4n, 5n, p - 1n]) {
      expect(blsG1PointIsInvalid(x)).toBe(nobleRejects(59n, g1Encoding(x)));
    }
  });

  it("agrees with noble on arbitrary G2 x coordinates", () => {
    for (let index = 0; index < 24; index += 1) {
      const x0 = fieldElement("g2-any-0", index);
      const x1 = fieldElement("g2-any-1", index);
      const rejected = nobleRejects(66n, g2Encoding(x0, x1));
      expect(rejected).toBe(true);
      expect(blsG2PointIsInvalid(x0, x1)).toBe(rejected);
    }
    for (const [x0, x1] of [
      [0n, 0n],
      [1n, 0n],
      [2n, 0n],
      [0n, 1n],
      [p - 1n, p - 1n],
    ] as const) {
      expect(blsG2PointIsInvalid(x0, x1)).toBe(
        nobleRejects(66n, g2Encoding(x0, x1)),
      );
    }
  });

  it("lifts y when y^2 has no u part, its root in Fp or in Fp u", () => {
    const { Fp2 } = fields;
    const isSquare = (value: bigint): boolean =>
      mod(value) === 0n || modPower(value, (p - 1n) / 2n) === 1n;
    const lifts = new Set<boolean>();
    for (const hex of [
      "8000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020e31aad2f4b199f7f87e6433692648312e55a89b142b798084e1ac133c07736855bf683690d5fa5f87e90a1b49384db0",
      "800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000013012ee46c892815c3ee133c0eb6ce1708f7aced12c82cb0a7404ad8ce28e77111a8fe9d10df4f22446c901e8f26165e6a",
    ]) {
      const encoded = Buffer.from(hex, "hex");
      const x1 = bigEndian(encoded.subarray(0, 48)) % 2n ** 381n;
      const x0 = bigEndian(encoded.subarray(48, 96));
      const x = Fp2.create({ c0: x0, c1: x1 });
      const ySquared = Fp2.add(
        Fp2.mul(Fp2.sqr(x), x),
        Fp2.create({ c0: 4n, c1: 4n }),
      );
      expect(ySquared.c1).toBe(0n);
      lifts.add(isSquare(ySquared.c0));
      expect(nobleRejects(66n, encoded)).toBe(true);
      expect(blsG2PointIsInvalid(x0, x1)).toBe(true);
    }
    expect(lifts).toEqual(new Set([true, false]));
  });
});

describe("V1 uncompress failures", () => {
  const charge = (tag: 59n | 66n) =>
    evaluateMidgardCekDirectBuiltin(tag, [
      direct(
        UPLCConst.byteString(
          tag === 59n
            ? g1Encoding(g1X(G1.Point.BASE))
            : g2Encoding(...g2X(G2.Point.BASE)),
        ),
      ),
    ]).budget;
  const verdict = (tag: 59n | 66n, encoded: Uint8Array) => {
    const arguments_ = [direct(UPLCConst.byteString(encoded))];
    return {
      evaluation: evaluateMidgardCekDirectBuiltin(tag, arguments_),
      proved: verifyMidgardCekDirectBuiltinFailure(
        tag,
        directBuiltinRoot(tag, arguments_),
        arguments_,
      ),
    };
  };
  const infinity = (length: number, first: number): Uint8Array => {
    const bytes = new Uint8Array(length);
    bytes[0] = first;
    return bytes;
  };
  const encodings: readonly (readonly [string, 59n | 66n, Uint8Array])[] = [
    ["G1 generator", 59n, g1Encoding(g1X(G1.Point.BASE))],
    ["G1 multiple", 59n, g1Encoding(g1X(G1.Point.BASE.multiply(5n)))],
    ["G1 off the curve", 59n, g1Encoding(1n)],
    ["G1 outside the subgroup", 59n, g1Encoding(4n)],
    ["G1 of order 3", 59n, g1Encoding(0n)],
    ["G1 infinity", 59n, infinity(48, 0xc0)],
    ["G1 infinity, sort bit", 59n, infinity(48, 0xe0)],
    ["G1 uncompressed flag", 59n, infinity(48, 0x40)],
    ["G2 generator", 66n, g2Encoding(...g2X(G2.Point.BASE))],
    ["G2 multiple", 66n, g2Encoding(...g2X(G2.Point.BASE.multiply(5n)))],
    ["G2 off the twist", 66n, g2Encoding(1n, 0n)],
    ["G2 outside the subgroup", 66n, g2Encoding(2n, 0n)],
    ["G2 infinity", 66n, infinity(96, 0xc0)],
    [
      "G2 infinity, nonzero tail",
      66n,
      (() => {
        const bytes = infinity(96, 0xc0);
        bytes[95] = 1;
        return bytes;
      })(),
    ],
  ];

  it.each(encodings)(
    "%s: proved exactly when it fails",
    (_name, tag, encoded) => {
      const { evaluation, proved } = verdict(tag, encoded);
      const fails = nobleRejects(tag, encoded);
      expect(evaluation.kind).toBe(fails ? "failure" : "success");
      expect(proved).toBe(fails);
      if (fails) expect(evaluation.budget).toEqual(charge(tag));
    },
  );

  it("proves arbitrary failing encodings and no valid one", () => {
    for (let index = 0; index < 8; index += 1) {
      const g1 = g1Encoding(fieldElement("verify-g1", index));
      const g2 = g2Encoding(
        fieldElement("verify-g2-0", index),
        fieldElement("verify-g2-1", index),
      );
      expect(verdict(59n, g1).proved).toBe(true);
      expect(verdict(66n, g2).proved).toBe(true);
      const scalar = 2n + (fieldElement("verify-in", index) % (r - 2n));
      expect(
        verdict(59n, G1.Point.BASE.multiply(scalar).toBytes()).proved,
      ).toBe(false);
      expect(
        verdict(66n, G2.Point.BASE.multiply(scalar).toBytes()).proved,
      ).toBe(false);
    }
  });
});
