//// BLS12-381 point validity for the uncompress failure arms (59, 66), in
//// plain integer field arithmetic: no BLS builtin takes an invalid point.
////
//// Cardano uncompresses through blst, which rejects an x that is off the
//// curve and a point that is outside the prime-order subgroup. The subgroup
//// tests here are blst's own (`POINTonE1_in_G1`, `POINTonE2_in_G2`), after
//// M. Scott, "A note on group membership tests for G1, G2 and GT on BLS
//// pairing-friendly curves" (ePrint 2021/1130); their equivalence to
//// membership is proved by El Housni, Guillevic and Piellard, "Co-factor
//// clearing and subgroup membership testing on pairing-friendly curves"
//// (ePrint 2022/352). Both tests are sign-blind: P is in the subgroup exactly
//// when -P is, so either square root serves as y.
////
//// Points are added in Jacobian coordinates (x = X/Z^2, y = Y/Z^3): doubling
//// by dbl-2009-l and addition by add-1998-cmo-2 with its two exceptional
//// cases (equal points, opposite points) decided explicitly, so every sum is
//// the group sum. Neither curve group has a point of order two (both orders
//// are odd), so a doubling never meets y = 0.

/// The base field prime p. A function, not a constant: the compiler copies a
/// constant's literal into every use, and p has some eighty.
pub fn field_prime_v1() -> Int {
  0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab
}

/// (p + 1) / 4: as p = 3 mod 4, a^((p + 1) / 4) squares to a exactly when a
/// is a square.
const square_root_exponent =
  0x680447a8e5ff9a692c6e9ed90d2eb35d91dd2e13ce144afd9cc34a83dac3d8907aaffffac54ffffee7fbfffffffeaab

/// (p + 1) / 2, the inverse of 2.
const half =
  0xd0088f51cbff34d258dd3db21a5d66bb23ba5c279c2895fb39869507b587b120f55ffff58a9ffffdcff7fffffffd556

/// The primitive cube root of unity of the G1 endomorphism (x, y) -> (beta x,
/// y), which acts on G1 as [-z^2]: blst's `sigma` applied twice.
const endomorphism_beta =
  0x5f19672fdf76ce51ba69c6076a0f77eaddb3a93be6f89688de17d813620a00022e01fffffffefffe

/// psi(x, y) = (conj(x) psi_x, conj(y) psi_y) on the G2 twist, which acts on
/// G2 as [z]. psi_x = psi_x_1 u = 1 / (1 + u)^((p - 1) / 3) and
/// psi_y = 1 / (1 + u)^((p - 1) / 2), from blst `psi`.
const psi_x_1 =
  0x1a0111ea397fe699ec02408663d4de85aa0d857d89759ad4897d29650fb85f9b409427eb4f49fffd8bfd00000000aaad

const psi_y_0 =
  0x135203e60180a68ee2e9c448d77a2cd91c3dedd930b1cf60ef396489f61eb45e304466cf3e67fa0af1ee7b04121bdea2

const psi_y_1 =
  0x6af0e0437ff400b6831e36d6bd17ffe48395dabc2d3435e77f76e17009241c5ee67992f72ec05f4c81084fbede3cc09

fn power_go(current: Int, remaining: Int, accumulator: Int) -> Int {
  if remaining == 0 {
    accumulator
  } else {
    power_go(
      current * current % field_prime_v1(),
      remaining / 2,
      if remaining % 2 == 1 {
        accumulator * current % field_prime_v1()
      } else {
        accumulator
      },
    )
  }
}

/// a^((p + 1) / 4) for a in [0, p).
fn square_root_candidate(a: Int) -> Int {
  power_go(a, square_root_exponent, 1)
}

// ---------------------------------------------------------------- G1

type G1Jacobian {
  x: Int,
  y: Int,
  z: Int,
}

fn g1_double_n(x: Int, y: Int, z: Int, n: Int) -> G1Jacobian {
  if n == 0 {
    G1Jacobian { x, y, z }
  } else {
    let a = x * x % field_prime_v1()
    let b = y * y % field_prime_v1()
    let c = b * b % field_prime_v1()
    let xb = x + b
    let d = 2 * ( xb * xb - a - c ) % field_prime_v1()
    let e = 3 * a
    let next_x = ( e * e - 2 * d ) % field_prime_v1()
    g1_double_n(
      next_x,
      ( e * ( d - next_x ) - 8 * c ) % field_prime_v1(),
      2 * y * z % field_prime_v1(),
      n - 1,
    )
  }
}

fn g1_add(left: G1Jacobian, right: G1Jacobian) -> G1Jacobian {
  let G1Jacobian { x: x1, y: y1, z: z1 } = left
  let G1Jacobian { x: x2, y: y2, z: z2 } = right
  if z1 % field_prime_v1() == 0 {
    right
  } else {
    let z1z1 = z1 * z1 % field_prime_v1()
    let z2z2 = z2 * z2 % field_prime_v1()
    let u1 = x1 * z2z2 % field_prime_v1()
    let s1 = y1 * z2 % field_prime_v1() * z2z2 % field_prime_v1()
    let h = ( x2 * z1z1 - u1 ) % field_prime_v1()
    let r = ( y2 * z1 % field_prime_v1() * z1z1 - s1 ) % field_prime_v1()
    if h == 0 {
      if r == 0 {
        g1_double_n(x1, y1, z1, 1)
      } else {
        G1Jacobian { x: 1, y: 1, z: 0 }
      }
    } else {
      let hh = h * h % field_prime_v1()
      let hhh = h * hh % field_prime_v1()
      let v = u1 * hh % field_prime_v1()
      let x3 = ( r * r - hhh - 2 * v ) % field_prime_v1()
      G1Jacobian {
        x: x3,
        y: ( r * ( v - x3 ) - s1 * hhh ) % field_prime_v1(),
        z: z1 * z2 % field_prime_v1() * h % field_prime_v1(),
      }
    }
  }
}

fn g1_add_double_n(
  accumulator: G1Jacobian,
  base: G1Jacobian,
  n: Int,
) -> G1Jacobian {
  let G1Jacobian { x, y, z } = g1_add(accumulator, base)
  g1_double_n(x, y, z, n)
}

/// [-z]P = [0xd201000000010000]P, blst's addition chain
/// (`POINTonE1_times_minus_z`).
fn g1_times_minus_z(base: G1Jacobian) -> G1Jacobian {
  let G1Jacobian { x, y, z } = base
  g1_double_n(x, y, z, 1)
    |> g1_add_double_n(base, 2)
    |> g1_add_double_n(base, 3)
    |> g1_add_double_n(base, 9)
    |> g1_add_double_n(base, 32)
    |> g1_add_double_n(base, 16)
}

/// True when the G1 point with x coordinate `x` (in [0, p), not the point at
/// infinity) fails to uncompress: x is off the curve y^2 = x^3 + 4, or the
/// point is outside G1, that is (beta x, y) != [-z^2](x, y).
pub fn g1_point_is_invalid_v1(x: Int) -> Bool {
  let rhs = ( x * x % field_prime_v1() * x + 4 ) % field_prime_v1()
  let y = square_root_candidate(rhs)
  if y * y % field_prime_v1() != rhs {
    True
  } else {
    // [-z^2]P = -[-z]([-z]P), so the test is [-z]([-z]P) == (beta x, -y).
    let G1Jacobian { x: zx, y: zy, z: zz } = g1_times_minus_z(
      g1_times_minus_z(G1Jacobian { x, y, z: 1 }),
    )
    let zz2 = zz * zz % field_prime_v1()
    !and {
      zz != 0,
      ( zx - endomorphism_beta * x % field_prime_v1() * zz2 ) % field_prime_v1() == 0,
      ( zy + y * zz2 % field_prime_v1() * zz ) % field_prime_v1() == 0,
    }
  }
}

// ---------------------------------------------------------------- G2
// Fp2 = Fp[u] / (u^2 + 1); a value is the pair (c0, c1) = c0 + c1 u.

type Fp2 {
  c0: Int,
  c1: Int,
}

fn fp2_multiply(a0: Int, a1: Int, b0: Int, b1: Int) -> Fp2 {
  Fp2 {
    c0: ( a0 * b0 - a1 * b1 ) % field_prime_v1(),
    c1: ( a0 * b1 + a1 * b0 ) % field_prime_v1(),
  }
}

type G2Jacobian {
  x0: Int,
  x1: Int,
  y0: Int,
  y1: Int,
  z0: Int,
  z1: Int,
}

fn g2_double_n(
  x0: Int,
  x1: Int,
  y0: Int,
  y1: Int,
  z0: Int,
  z1: Int,
  n: Int,
) -> G2Jacobian {
  if n == 0 {
    G2Jacobian { x0, x1, y0, y1, z0, z1 }
  } else {
    // A = X^2, B = Y^2, C = B^2, D = 2((X + B)^2 - A - C), E = 3A
    let a0 = ( x0 + x1 ) * ( x0 - x1 ) % field_prime_v1()
    let a1 = 2 * x0 * x1 % field_prime_v1()
    let b0 = ( y0 + y1 ) * ( y0 - y1 ) % field_prime_v1()
    let b1 = 2 * y0 * y1 % field_prime_v1()
    let c0 = ( b0 + b1 ) * ( b0 - b1 ) % field_prime_v1()
    let c1 = 2 * b0 * b1 % field_prime_v1()
    let xb0 = x0 + b0
    let xb1 = x1 + b1
    let d0 = 2 * ( ( xb0 + xb1 ) * ( xb0 - xb1 ) - a0 - c0 ) % field_prime_v1()
    let d1 = 2 * ( 2 * xb0 * xb1 - a1 - c1 ) % field_prime_v1()
    let e0 = 3 * a0
    let e1 = 3 * a1
    // X3 = E^2 - 2D, Y3 = E(D - X3) - 8C, Z3 = 2YZ
    let next_x0 = ( ( e0 + e1 ) * ( e0 - e1 ) - 2 * d0 ) % field_prime_v1()
    let next_x1 = ( 2 * e0 * e1 - 2 * d1 ) % field_prime_v1()
    let m0 = d0 - next_x0
    let m1 = d1 - next_x1
    g2_double_n(
      next_x0,
      next_x1,
      ( e0 * m0 - e1 * m1 - 8 * c0 ) % field_prime_v1(),
      ( e0 * m1 + e1 * m0 - 8 * c1 ) % field_prime_v1(),
      2 * ( y0 * z0 - y1 * z1 ) % field_prime_v1(),
      2 * ( y0 * z1 + y1 * z0 ) % field_prime_v1(),
      n - 1,
    )
  }
}

fn g2_add(left: G2Jacobian, right: G2Jacobian) -> G2Jacobian {
  let G2Jacobian { x0: lx0, x1: lx1, y0: ly0, y1: ly1, z0: lz0, z1: lz1 } = left
  let G2Jacobian { x0: rx0, x1: rx1, y0: ry0, y1: ry1, z0: rz0, z1: rz1 } = right
  if and {
    lz0 % field_prime_v1() == 0,
    lz1 % field_prime_v1() == 0,
  } {
    right
  } else {
    let Fp2 { c0: lzz0, c1: lzz1 } = fp2_multiply(lz0, lz1, lz0, lz1)
    let Fp2 { c0: rzz0, c1: rzz1 } = fp2_multiply(rz0, rz1, rz0, rz1)
    let Fp2 { c0: u10, c1: u11 } = fp2_multiply(lx0, lx1, rzz0, rzz1)
    let Fp2 { c0: u20, c1: u21 } = fp2_multiply(rx0, rx1, lzz0, lzz1)
    let Fp2 { c0: lyz0, c1: lyz1 } = fp2_multiply(ly0, ly1, rz0, rz1)
    let Fp2 { c0: s10, c1: s11 } = fp2_multiply(lyz0, lyz1, rzz0, rzz1)
    let Fp2 { c0: ryz0, c1: ryz1 } = fp2_multiply(ry0, ry1, lz0, lz1)
    let Fp2 { c0: s20, c1: s21 } = fp2_multiply(ryz0, ryz1, lzz0, lzz1)
    let h0 = ( u20 - u10 ) % field_prime_v1()
    let h1 = ( u21 - u11 ) % field_prime_v1()
    let r0 = ( s20 - s10 ) % field_prime_v1()
    let r1 = ( s21 - s11 ) % field_prime_v1()
    if h0 == 0 && h1 == 0 {
      if r0 == 0 && r1 == 0 {
        g2_double_n(lx0, lx1, ly0, ly1, lz0, lz1, 1)
      } else {
        G2Jacobian { x0: 1, x1: 0, y0: 1, y1: 0, z0: 0, z1: 0 }
      }
    } else {
      let Fp2 { c0: hh0, c1: hh1 } = fp2_multiply(h0, h1, h0, h1)
      let Fp2 { c0: hhh0, c1: hhh1 } = fp2_multiply(h0, h1, hh0, hh1)
      let Fp2 { c0: v0, c1: v1 } = fp2_multiply(u10, u11, hh0, hh1)
      let Fp2 { c0: rr0, c1: rr1 } = fp2_multiply(r0, r1, r0, r1)
      let x30 = ( rr0 - hhh0 - 2 * v0 ) % field_prime_v1()
      let x31 = ( rr1 - hhh1 - 2 * v1 ) % field_prime_v1()
      let Fp2 { c0: k0, c1: k1 } = fp2_multiply(r0, r1, v0 - x30, v1 - x31)
      let Fp2 { c0: l0, c1: l1 } = fp2_multiply(s10, s11, hhh0, hhh1)
      let Fp2 { c0: zz0, c1: zz1 } = fp2_multiply(lz0, lz1, rz0, rz1)
      let Fp2 { c0: z30, c1: z31 } = fp2_multiply(zz0, zz1, h0, h1)
      G2Jacobian {
        x0: x30,
        x1: x31,
        y0: k0 - l0,
        y1: k1 - l1,
        z0: z30,
        z1: z31,
      }
    }
  }
}

fn g2_add_double_n(
  accumulator: G2Jacobian,
  base: G2Jacobian,
  n: Int,
) -> G2Jacobian {
  let G2Jacobian { x0, x1, y0, y1, z0, z1 } = g2_add(accumulator, base)
  g2_double_n(x0, x1, y0, y1, z0, z1, n)
}

/// [-z]Q, blst's addition chain (`POINTonE2_times_minus_z`).
fn g2_times_minus_z(base: G2Jacobian) -> G2Jacobian {
  let G2Jacobian { x0, x1, y0, y1, z0, z1 } = base
  g2_double_n(x0, x1, y0, y1, z0, z1, 1)
    |> g2_add_double_n(base, 2)
    |> g2_add_double_n(base, 3)
    |> g2_add_double_n(base, 9)
    |> g2_add_double_n(base, 32)
    |> g2_add_double_n(base, 16)
}

/// True when the point Q = (x, y), y = (y0 + y1 u) / w with w in Fp nonzero,
/// is outside G2, that is psi(Q) != [z]Q, tested as [-z]Q == -psi(Q).
fn g2_outside_subgroup(x0: Int, x1: Int, y0: Int, y1: Int, w: Int) -> Bool {
  let ww = w * w % field_prime_v1()
  // Jacobian (x w^2 : (y0 + y1 u) w^2 : w) is the affine (x, y).
  let base = G2Jacobian {
    x0: x0 * ww % field_prime_v1(),
    x1: x1 * ww % field_prime_v1(),
    y0: y0 * ww % field_prime_v1(),
    y1: y1 * ww % field_prime_v1(),
    z0: w,
    z1: 0,
  }
  let G2Jacobian { x0: zx0, x1: zx1, y0: zy0, y1: zy1, z0: zz0, z1: zz1 } = g2_times_minus_z(
    base,
  )
  let Fp2 { c0: zz20, c1: zz21 } = fp2_multiply(zz0, zz1, zz0, zz1)
  let Fp2 { c0: zz30, c1: zz31 } = fp2_multiply(zz20, zz21, zz0, zz1)
  // -psi(Q) = (conj(x) psi_x, -conj(y) psi_y); conj(x) psi_x_1 u is
  // (psi_x_1 x1, psi_x_1 x0), and its y is scaled by w so it stays integral.
  let Fp2 { c0: tx0, c1: tx1 } = fp2_multiply(
    psi_x_1 * x1 % field_prime_v1(),
    psi_x_1 * x0 % field_prime_v1(),
    zz20,
    zz21,
  )
  let Fp2 { c0: py0, c1: py1 } = fp2_multiply(y0, -y1, psi_y_0, psi_y_1)
  let Fp2 { c0: ty0, c1: ty1 } = fp2_multiply(-py0, -py1, zz30, zz31)
  !and {
    zz0 != 0 || zz1 != 0,
    ( zx0 - tx0 ) % field_prime_v1() == 0,
    ( zx1 - tx1 ) % field_prime_v1() == 0,
    ( zy0 * w - ty0 ) % field_prime_v1() == 0,
    ( zy1 * w - ty1 ) % field_prime_v1() == 0,
  }
}

/// True when the G2 point with x coordinate x0 + x1 u (both in [0, p), not
/// the point at infinity) fails to uncompress: x is off the twist
/// y^2 = x^3 + 4(1 + u), or the point is outside G2.
///
/// a = a0 + a1 u is a square in Fp2 exactly when its norm a0^2 + a1^2 is a
/// square in Fp: a^((p^2 - 1) / 2) = (a^(p + 1))^((p - 1) / 2) and
/// a^(p + 1) = a conj(a) is the norm. With n = norm, r = sqrt(n) and
/// t = (a0 + r) / 2, a root of a is w + (a1 / 2w) u when t = w^2, and
/// (a1 / 2w) + w u when t = -w^2 (t is not a square). Both scale by 2w to
/// (2t + a1 u) / 2w and (a1 - 2t u) / 2w. t is nonzero when a1 is; a1 = 0
/// takes the root of a0 or of -a0 directly.
pub fn g2_point_is_invalid_v1(x0: Int, x1: Int) -> Bool {
  let Fp2 { c0: xx0, c1: xx1 } = fp2_multiply(x0, x1, x0, x1)
  let Fp2 { c0: xxx0, c1: xxx1 } = fp2_multiply(xx0, xx1, x0, x1)
  let a0 = ( xxx0 + 4 ) % field_prime_v1()
  let a1 = ( xxx1 + 4 ) % field_prime_v1()
  let norm = ( a0 * a0 + a1 * a1 ) % field_prime_v1()
  let root = square_root_candidate(norm)
  if root * root % field_prime_v1() != norm {
    True
  } else if a1 == 0 {
    let s = square_root_candidate(a0)
    if s * s % field_prime_v1() == a0 {
      g2_outside_subgroup(x0, x1, s, 0, 1)
    } else {
      g2_outside_subgroup(x0, x1, 0, s, 1)
    }
  } else {
    let t = ( a0 + root ) * half % field_prime_v1()
    let w = square_root_candidate(t)
    if w * w % field_prime_v1() == t {
      g2_outside_subgroup(
        x0,
        x1,
        2 * t % field_prime_v1(),
        a1,
        2 * w % field_prime_v1(),
      )
    } else {
      g2_outside_subgroup(
        x0,
        x1,
        a1,
        -2 * t % field_prime_v1(),
        2 * w % field_prime_v1(),
      )
    }
  }
}
