/**
 * The TypeScript twin of `onchain/aiken/lib/midgard/bls12-381-point-v1.ak`:
 * BLS12-381 point validity for the uncompress failure arms (59, 66), in the
 * same integer arithmetic, step for step. The subgroup tests are blst's
 * (`POINTonE1_in_G1`, `POINTonE2_in_G2`), after M. Scott, ePrint 2021/1130;
 * their equivalence to subgroup membership is proved in ePrint 2022/352.
 */

/** The base field prime p. */
export const BLS12_381_FIELD_PRIME =
  0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaabn;
/** The prime order r of G1 and G2. */
export const BLS12_381_GROUP_ORDER =
  0x73eda753299d7d483339d80809a1d80553bda402fffe5bfeffffffff00000001n;
/** The curve parameter z, negative. */
export const BLS12_381_Z = -0xd201000000010000n;
/** G1: y^2 = x^3 + 4. G2 twist: y^2 = x^3 + 4(1 + u). */
export const BLS12_381_G1_B = 4n;
export const BLS12_381_G2_B: readonly [bigint, bigint] = [4n, 4n];
/** The cube root of unity of the G1 endomorphism (x, y) -> (beta x, y). */
export const BLS12_381_ENDOMORPHISM_BETA =
  0x5f19672fdf76ce51ba69c6076a0f77eaddb3a93be6f89688de17d813620a00022e01fffffffefffen;
/** psi(x, y) = (conj(x) psi_x, conj(y) psi_y), psi_x = psi_x_1 u. */
export const BLS12_381_PSI_X_1 =
  0x1a0111ea397fe699ec02408663d4de85aa0d857d89759ad4897d29650fb85f9b409427eb4f49fffd8bfd00000000aaadn;
export const BLS12_381_PSI_Y: readonly [bigint, bigint] = [
  0x135203e60180a68ee2e9c448d77a2cd91c3dedd930b1cf60ef396489f61eb45e304466cf3e67fa0af1ee7b04121bdea2n,
  0x06af0e0437ff400b6831e36d6bd17ffe48395dabc2d3435e77f76e17009241c5ee67992f72ec05f4c81084fbede3cc09n,
];

const P = BLS12_381_FIELD_PRIME;
const SQUARE_ROOT_EXPONENT = (P + 1n) / 4n;
const HALF = (P + 1n) / 2n;

/** Aiken's `%`: the floored remainder, in [0, p). */
const mod = (value: bigint): bigint => ((value % P) + P) % P;

const squareRootCandidate = (a: bigint): bigint => {
  let current = mod(a);
  let remaining = SQUARE_ROOT_EXPONENT;
  let accumulator = 1n;
  while (remaining !== 0n) {
    if (remaining % 2n === 1n) accumulator = (accumulator * current) % P;
    current = (current * current) % P;
    remaining /= 2n;
  }
  return accumulator;
};

/** blst's `times_minus_z` chain: (add, then double n times) after one doubling. */
const MINUS_Z_CHAIN = [2, 3, 9, 32, 16] as const;

type G1Jacobian = { x: bigint; y: bigint; z: bigint };

const g1Double = ({ x, y, z }: G1Jacobian): G1Jacobian => {
  const a = mod(x * x);
  const b = mod(y * y);
  const c = mod(b * b);
  const xb = x + b;
  const d = mod(2n * (xb * xb - a - c));
  const e = 3n * a;
  const nextX = mod(e * e - 2n * d);
  return { x: nextX, y: mod(e * (d - nextX) - 8n * c), z: mod(2n * y * z) };
};

const g1Add = (left: G1Jacobian, right: G1Jacobian): G1Jacobian => {
  if (mod(left.z) === 0n) return right;
  const z1z1 = mod(left.z * left.z);
  const z2z2 = mod(right.z * right.z);
  const u1 = mod(left.x * z2z2);
  const s1 = mod(mod(left.y * right.z) * z2z2);
  const h = mod(right.x * z1z1 - u1);
  const r = mod(mod(right.y * left.z) * z1z1 - s1);
  if (h === 0n) return r === 0n ? g1Double(left) : { x: 1n, y: 1n, z: 0n };
  const hh = mod(h * h);
  const hhh = mod(h * hh);
  const v = mod(u1 * hh);
  const x3 = mod(r * r - hhh - 2n * v);
  return {
    x: x3,
    y: mod(r * (v - x3) - s1 * hhh),
    z: mod(mod(left.z * right.z) * h),
  };
};

const g1TimesMinusZ = (base: G1Jacobian): G1Jacobian => {
  let accumulator = g1Double(base);
  for (const doublings of MINUS_Z_CHAIN) {
    accumulator = g1Add(accumulator, base);
    for (let index = 0; index < doublings; index += 1) {
      accumulator = g1Double(accumulator);
    }
  }
  return accumulator;
};

/**
 * `g1_point_is_invalid_v1`: the G1 point over x (in [0, p), not infinity) is
 * off the curve, or outside G1: (beta x, y) != [-z^2](x, y).
 */
export const blsG1PointIsInvalid = (x: bigint): boolean => {
  const rhs = mod(mod(x * x) * x + BLS12_381_G1_B);
  const y = squareRootCandidate(rhs);
  if (mod(y * y) !== rhs) return true;
  const result = g1TimesMinusZ(g1TimesMinusZ({ x, y, z: 1n }));
  const zz2 = mod(result.z * result.z);
  return !(
    result.z !== 0n &&
    mod(result.x - mod(BLS12_381_ENDOMORPHISM_BETA * x) * zz2) === 0n &&
    mod(result.y + mod(y * zz2) * result.z) === 0n
  );
};

type Fp2 = readonly [bigint, bigint];

const fp2Multiply = ([a0, a1]: Fp2, [b0, b1]: Fp2): Fp2 => [
  mod(a0 * b0 - a1 * b1),
  mod(a0 * b1 + a1 * b0),
];

type G2Jacobian = { x: Fp2; y: Fp2; z: Fp2 };

const g2Double = ({ x, y, z }: G2Jacobian): G2Jacobian => {
  const a = fp2Multiply(x, x);
  const b = fp2Multiply(y, y);
  const c = fp2Multiply(b, b);
  const xb: Fp2 = [x[0] + b[0], x[1] + b[1]];
  const xbxb = fp2Multiply(xb, xb);
  const d: Fp2 = [
    mod(2n * (xbxb[0] - a[0] - c[0])),
    mod(2n * (xbxb[1] - a[1] - c[1])),
  ];
  const e: Fp2 = [3n * a[0], 3n * a[1]];
  const ee = fp2Multiply(e, e);
  const nextX: Fp2 = [mod(ee[0] - 2n * d[0]), mod(ee[1] - 2n * d[1])];
  const edx = fp2Multiply(e, [d[0] - nextX[0], d[1] - nextX[1]]);
  const yz = fp2Multiply(y, z);
  return {
    x: nextX,
    y: [mod(edx[0] - 8n * c[0]), mod(edx[1] - 8n * c[1])],
    z: [mod(2n * yz[0]), mod(2n * yz[1])],
  };
};

const isZero = ([c0, c1]: Fp2): boolean => mod(c0) === 0n && mod(c1) === 0n;

const g2Add = (left: G2Jacobian, right: G2Jacobian): G2Jacobian => {
  if (isZero(left.z)) return right;
  const lzz = fp2Multiply(left.z, left.z);
  const rzz = fp2Multiply(right.z, right.z);
  const u1 = fp2Multiply(left.x, rzz);
  const u2 = fp2Multiply(right.x, lzz);
  const s1 = fp2Multiply(fp2Multiply(left.y, right.z), rzz);
  const s2 = fp2Multiply(fp2Multiply(right.y, left.z), lzz);
  const h: Fp2 = [mod(u2[0] - u1[0]), mod(u2[1] - u1[1])];
  const r: Fp2 = [mod(s2[0] - s1[0]), mod(s2[1] - s1[1])];
  if (isZero(h)) {
    return isZero(r)
      ? g2Double(left)
      : { x: [1n, 0n], y: [1n, 0n], z: [0n, 0n] };
  }
  const hh = fp2Multiply(h, h);
  const hhh = fp2Multiply(h, hh);
  const v = fp2Multiply(u1, hh);
  const rr = fp2Multiply(r, r);
  const x3: Fp2 = [
    mod(rr[0] - hhh[0] - 2n * v[0]),
    mod(rr[1] - hhh[1] - 2n * v[1]),
  ];
  const k = fp2Multiply(r, [v[0] - x3[0], v[1] - x3[1]]);
  const l = fp2Multiply(s1, hhh);
  return {
    x: x3,
    y: [mod(k[0] - l[0]), mod(k[1] - l[1])],
    z: fp2Multiply(fp2Multiply(left.z, right.z), h),
  };
};

const g2TimesMinusZ = (base: G2Jacobian): G2Jacobian => {
  let accumulator = g2Double(base);
  for (const doublings of MINUS_Z_CHAIN) {
    accumulator = g2Add(accumulator, base);
    for (let index = 0; index < doublings; index += 1) {
      accumulator = g2Double(accumulator);
    }
  }
  return accumulator;
};

/** Q = (x, y / w), w in Fp nonzero, is outside G2: [-z]Q != -psi(Q). */
const g2OutsideSubgroup = (x: Fp2, y: Fp2, w: bigint): boolean => {
  const ww = mod(w * w);
  const result = g2TimesMinusZ({
    x: [mod(x[0] * ww), mod(x[1] * ww)],
    y: [mod(y[0] * ww), mod(y[1] * ww)],
    z: [w, 0n],
  });
  const zz2 = fp2Multiply(result.z, result.z);
  const zz3 = fp2Multiply(zz2, result.z);
  const tx = fp2Multiply(
    [mod(BLS12_381_PSI_X_1 * x[1]), mod(BLS12_381_PSI_X_1 * x[0])],
    zz2,
  );
  const py = fp2Multiply([y[0], -y[1]], BLS12_381_PSI_Y);
  const ty = fp2Multiply([-py[0], -py[1]], zz3);
  return !(
    !isZero(result.z) &&
    mod(result.x[0] - tx[0]) === 0n &&
    mod(result.x[1] - tx[1]) === 0n &&
    mod(result.y[0] * w - ty[0]) === 0n &&
    mod(result.y[1] * w - ty[1]) === 0n
  );
};

/**
 * `g2_point_is_invalid_v1`: the G2 point over x0 + x1 u (both in [0, p), not
 * infinity) is off the twist, or outside G2.
 */
export const blsG2PointIsInvalid = (x0: bigint, x1: bigint): boolean => {
  const x: Fp2 = [x0, x1];
  const xxx = fp2Multiply(fp2Multiply(x, x), x);
  const a0 = mod(xxx[0] + BLS12_381_G2_B[0]);
  const a1 = mod(xxx[1] + BLS12_381_G2_B[1]);
  const norm = mod(a0 * a0 + a1 * a1);
  const root = squareRootCandidate(norm);
  if (mod(root * root) !== norm) return true;
  if (a1 === 0n) {
    const s = squareRootCandidate(a0);
    return mod(s * s) === a0
      ? g2OutsideSubgroup(x, [s, 0n], 1n)
      : g2OutsideSubgroup(x, [0n, s], 1n);
  }
  const t = mod((a0 + root) * HALF);
  const w = squareRootCandidate(t);
  return mod(w * w) === t
    ? g2OutsideSubgroup(x, [mod(2n * t), a1], mod(2n * w))
    : g2OutsideSubgroup(x, [a1, mod(-2n * t)], mod(2n * w));
};
