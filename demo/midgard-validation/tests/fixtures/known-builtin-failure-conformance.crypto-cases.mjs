/**
 * The signature, UTF-8 and BLS12-381 cases of the known builtin failure
 * conformance corpus: tags 21, 52 and 53 (key, message and signature
 * lengths, secp256k1 key validity, ECDSA scalar range), 25 (strict UTF-8),
 * 59 and 66 (compressed point headers, coordinates, the curve and the
 * prime-order subgroup),
 * 60 and 67 (the hash-to-group domain length) and 56 and 63 (the scalarMul
 * scalar bound). See
 * `known-builtin-failure-conformance.cases.mjs`.
 */

import {
  BLS12_381_FIELD_PRIME,
  BLS12_381_G1_GENERATOR,
  BLS12_381_G2_GENERATOR,
  bigEndian,
  bytes,
  filled,
  g1Point,
  g2Point,
  int,
  millerLoop,
  opaque,
  SECP256K1_FIELD_PRIME,
  SECP256K1_GENERATOR_X,
  SECP256K1_GROUP_ORDER,
  semantic,
  str,
} from "./known-builtin-failure-conformance.arguments.mjs";

const semanticBytes = (memory) => semantic(bytes([1]), memory);

/** Ed25519 (21), ECDSA (52) and Schnorr (53) secp256k1 signature checks. */
const signatureCases = (add) => {
  for (const tag of [21, 52, 53]) {
    for (const keyLength of [31, 32, 33]) {
      for (const signatureLength of [63, 64, 65]) {
        add(tag, `key ${keyLength} bytes, signature ${signatureLength} bytes`, [
          filled(keyLength, 2),
          filled(32, 1),
          filled(signatureLength, 1),
        ]);
      }
    }
    add(tag, "empty key", [filled(0), filled(32), filled(64)]);
    add(tag, "empty message", [filled(32), filled(0), filled(64)]);
    add(tag, "empty signature", [filled(32), filled(32), filled(0)]);
    add(tag, "compacted message", [filled(32), semanticBytes(1), filled(64)]);
    add(tag, "compacted message, short key", [
      filled(31),
      semanticBytes(1),
      filled(64),
    ]);
    add(tag, "string message, short key", [filled(31), str("m"), filled(64)]);
    add(tag, "integer message, short key", [filled(31), int(1), filled(64)]);
    add(tag, "opaque message, short key", [filled(31), opaque, filled(64)]);
    add(tag, "string signature", [filled(31), filled(32), str("x")]);
    add(tag, "integer key", [int(1), filled(32), filled(64)]);
    add(tag, "compacted key", [semanticBytes(1), filled(32), filled(63)]);
    add(tag, "compacted signature", [filled(31), filled(32), semanticBytes(1)]);
    add(tag, "Miller-loop key", [millerLoop, filled(32), filled(64)]);
    add(tag, "message 31 bytes", [filled(32), filled(31), filled(64)]);
    add(tag, "message 33 bytes", [filled(32), filled(33), filled(64)]);
    add(tag, "two arguments", [filled(32), filled(32)]);
    add(tag, "four arguments", [filled(32), filled(32), filled(64), filled(1)]);
  }

  const compressedKey = (prefix, x) => bytes([prefix, ...bigEndian(x, 32)]);
  const ecdsaSignature = (r, s) =>
    bytes([...bigEndian(r, 32), ...bigEndian(s, 32)]);
  const n = SECP256K1_GROUP_ORDER;
  const p = SECP256K1_FIELD_PRIME;
  for (const [keyName, key] of [
    ["02 generator", compressedKey(2, SECP256K1_GENERATOR_X)],
    ["03 generator", compressedKey(3, SECP256K1_GENERATOR_X)],
    ["04 generator", compressedKey(4, SECP256K1_GENERATOR_X)],
    ["00 generator", compressedKey(0, SECP256K1_GENERATOR_X)],
    ["02 x=5 (off curve)", compressedKey(2, 5n)],
    ["02 x=p", compressedKey(2, p)],
    ["02 x=p-1", compressedKey(2, p - 1n)],
    ["02 x=0", compressedKey(2, 0n)],
  ]) {
    for (const [signatureName, signature] of [
      ["r=1 s=1", ecdsaSignature(1n, 1n)],
      ["r=n", ecdsaSignature(n, 1n)],
      ["r=n-1", ecdsaSignature(n - 1n, 1n)],
      ["s=n", ecdsaSignature(1n, n)],
      ["s=n-1", ecdsaSignature(1n, n - 1n)],
      ["s=2^256-1", ecdsaSignature(1n, 2n ** 256n - 1n)],
    ]) {
      add(52, `ECDSA key ${keyName}, ${signatureName}`, [
        key,
        filled(32, 3),
        signature,
      ]);
    }
    add(52, `ECDSA key ${keyName}, message 31 bytes`, [
      key,
      filled(31, 3),
      ecdsaSignature(n, 1n),
    ]);
  }
  for (const [keyName, key] of [
    ["generator", bytes(bigEndian(SECP256K1_GENERATOR_X, 32))],
    ["x=5 (off curve)", bytes(bigEndian(5n, 32))],
    ["x=p", bytes(bigEndian(p, 32))],
    ["x=p-1", bytes(bigEndian(p - 1n, 32))],
    ["x=0", bytes(bigEndian(0n, 32))],
    ["all 0xff", filled(32, 255)],
  ]) {
    add(53, `Schnorr key ${keyName}`, [key, filled(7, 3), filled(64, 1)]);
    add(53, `Schnorr key ${keyName}, signature 65 bytes`, [
      key,
      filled(7, 3),
      filled(65, 1),
    ]);
  }
};

/** decodeUtf8 (25): strict UTF-8 over the boundaries of each sequence length. */
const utf8Cases = (add) => {
  for (const [name, source] of [
    ["empty", []],
    ["ASCII", [0x41]],
    ["byte-order mark then ASCII", [0xef, 0xbb, 0xbf, 0x41]],
    ["overlong NUL c0 80", [0xc0, 0x80]],
    ["c1 bf", [0xc1, 0xbf]],
    ["c2 80", [0xc2, 0x80]],
    ["truncated c2", [0xc2]],
    ["df bf", [0xdf, 0xbf]],
    ["overlong e0 80 80", [0xe0, 0x80, 0x80]],
    ["e0 a0 80", [0xe0, 0xa0, 0x80]],
    ["truncated e0 a0", [0xe0, 0xa0]],
    ["surrogate ed a0 80", [0xed, 0xa0, 0x80]],
    ["ed 9f bf", [0xed, 0x9f, 0xbf]],
    ["ef bf bf", [0xef, 0xbf, 0xbf]],
    ["overlong f0 80 80 80", [0xf0, 0x80, 0x80, 0x80]],
    ["f0 90 80 80", [0xf0, 0x90, 0x80, 0x80]],
    ["f4 8f bf bf", [0xf4, 0x8f, 0xbf, 0xbf]],
    ["above U+10FFFF f4 90 80 80", [0xf4, 0x90, 0x80, 0x80]],
    ["f5 80 80 80", [0xf5, 0x80, 0x80, 0x80]],
    ["lone continuation 80", [0x80]],
    ["ff", [0xff]],
    ["truncated f0 90 80", [0xf0, 0x90, 0x80]],
    ["e1 80 then ASCII", [0xe1, 0x80, 0x41]],
    ["ASCII then truncated e1", [0x41, 0xe1]],
  ]) {
    add(25, `UTF-8 ${name}`, [bytes(source)]);
  }
  add(25, "compacted source", [semanticBytes(1)]);
  add(25, "string source", [str("x")]);
  add(25, "opaque source", [opaque]);
  add(25, "no arguments", []);
};

/** A G1 encoding: flags `first` over the 48-byte big-endian x. */
const g1 = (first, x) => {
  const encoded = bigEndian(x, 48);
  encoded[0] = encoded[0] % 32 | first;
  return bytes(encoded);
};

/** A G2 encoding: flags `first` over x1, then x0. */
const g2 = (first, x1, x0) => {
  const encoded = new Uint8Array(96);
  encoded.set(bigEndian(x1, 48), 0);
  encoded[0] = encoded[0] % 32 | first;
  encoded.set(bigEndian(x0, 48), 48);
  return bytes(encoded);
};

/** The encoding with its sort flag flipped: the negated point. */
const flipSort = (encoded) => {
  const flipped = Uint8Array.from(encoded);
  flipped[0] = flipped[0] ^ 0x20;
  return bytes(flipped);
};
const hex = (source) => bytes(Buffer.from(source, "hex"));

// Compressed points, verdicts per @noble/curves (cross-checked in
// tests/cek-builtin.v1-bls-point-validity.ts).
const G1_FIVE_GENERATOR =
  "af95b8218cbee2f4fa48e6b6f1df4e8ee46fee73c270dba395dad523d10c9b35295ccfc92cf0a9db8a065e16dafbfaad";
const G1_OUTSIDE_SUBGROUP =
  "8a2b9fbe12d0fd644c1e951c5983f40ca78f18e62b3b55a1d8e62cddba3d8a3736719efc37f870af7f436c2671752974";
// Off the curve: x^3 + 4 is not a square, and its candidate root
// (x^3 + 4)^((p + 1) / 4) squares to -(x^3 + 4).
const G1_ROOT_SQUARES_TO_NEGATED_RHS = [
  "95d698989e1fd767158b3be455ee17250ba1f0268a36a4a772132d47564c89399d3d20e54cc850d1e3be2cfd2588afd8",
  "85e205f2f5ffed0a82ab845e29d7c7c4d6b3558179f872cd6272aa4007115f21f12c491a9dc1f2db45e1e7e17e8832f2",
  "8a8ff69d381ffa7ddc911f5b34aafeb04f2720d4198b3e78e4ff5e08ff154d36e7a50d20af055b1bde8ecc0e0bee3ce5",
];
const G2_FIVE_GENERATOR =
  "b068ad1be382009ac2dce123ec62dca8337d6b93b909b3ee52e31cb9e4098d1b56d596bf3c08166c7b46cb3aa85c23381380055ab9f1a87786f2508f3e4ce5caa5abcdae0a80141ee8ccc3626311e0a53be5d873fa964fd85ad56771f2984579";
const G2_OFF_TWIST =
  "8b55de6ec1c1b9c65236f5282637f7e67bf524d7e4187af733ef92700eeb1086f4549c5049937408f31ba2e532c0bd501587ca03a248d9ad945e56a3e3ef50f225a92648ed8a647d159479e20ff13c8ed21188535039c8fec81090af5bef34c3";
const G2_OUTSIDE_SUBGROUP =
  "937f9ebde4497e13a660d4e184d974218b21d18c9d578ab770360ad3fe41a4217e4ac78c554e05bfd2fc02336b84d90505974a366247ac98e953e9afec7f10dd12080a46e6f7dede942d6265721ee3c807009433d1bc69bf82d69569713244a7";
const G2_ROOT_IN_FP =
  "8000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000020e31aad2f4b199f7f87e6433692648312e55a89b142b798084e1ac133c07736855bf683690d5fa5f87e90a1b49384db0";
const G2_ROOT_IN_FP_U =
  "800000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000013012ee46c892815c3ee133c0eb6ce1708f7aced12c82cb0a7404ad8ce28e77111a8fe9d10df4f22446c901e8f26165e6a";

/** G1/G2 uncompress (59, 66) and hash-to-group (60, 67). */
const blsCases = (add) => {
  const p = BLS12_381_FIELD_PRIME;
  for (const length of [0, 47, 49, 96]) {
    add(59, `G1 encoding ${length} bytes`, [filled(length, 0x80)]);
  }
  for (const [name, encoded] of [
    ["uncompressed", g1(0x00, 1n)],
    ["infinity", g1(0xc0, 0n)],
    ["infinity with sort flag", g1(0xe0, 0n)],
    ["infinity with nonzero x", g1(0xc0, 1n)],
    ["x=p", g1(0x80, p)],
    ["x=p-1", g1(0x80, p - 1n)],
    ["x=0", g1(0x80, 0n)],
    ["x=1", g1(0x80, 1n)],
    ["x=2", g1(0x80, 2n)],
    ["x=3", g1(0x80, 3n)],
    ["x=4", g1(0x80, 4n)],
    ["x=5", g1(0x80, 5n)],
    ["sort flag, x=1", g1(0xa0, 1n)],
    ["generator", bytes(BLS12_381_G1_GENERATOR)],
    ["generator, sort flag flipped", flipSort(BLS12_381_G1_GENERATOR)],
    ["[5] generator", hex(G1_FIVE_GENERATOR)],
    ["on the curve, outside G1", hex(G1_OUTSIDE_SUBGROUP)],
  ]) {
    add(59, `G1 ${name}`, [encoded]);
  }
  for (const [index, encoded] of G1_ROOT_SQUARES_TO_NEGATED_RHS.entries()) {
    add(
      59,
      `G1 off-curve x whose candidate root squares to the negated right-hand side ${index}`,
      [hex(encoded)],
    );
  }
  add(59, "compacted G1 encoding", [semanticBytes(1)]);
  add(59, "integer G1 encoding", [int(1)]);

  for (const length of [0, 48, 95, 97]) {
    add(66, `G2 encoding ${length} bytes`, [filled(length, 0x80)]);
  }
  for (const [name, encoded] of [
    ["uncompressed", g2(0x00, 1n, 1n)],
    ["infinity", g2(0xc0, 0n, 0n)],
    ["infinity with nonzero x0", g2(0xc0, 0n, 1n)],
    ["infinity with sort flag", g2(0xe0, 0n, 0n)],
    ["x0=p", g2(0x80, 1n, p)],
    ["x1=p", g2(0x80, p, 1n)],
    ["x0=p-1", g2(0x80, 1n, p - 1n)],
    ["x1=p-1", g2(0x80, p - 1n, 1n)],
    ["small coordinates", g2(0x80, 1n, 1n)],
    ["x=1, off the twist", g2(0x80, 0n, 1n)],
    ["x=2, on the twist, outside G2", g2(0x80, 0n, 2n)],
    ["generator", bytes(BLS12_381_G2_GENERATOR)],
    ["generator, sort flag flipped", flipSort(BLS12_381_G2_GENERATOR)],
    ["[5] generator", hex(G2_FIVE_GENERATOR)],
    ["off the twist", hex(G2_OFF_TWIST)],
    ["on the twist, outside G2", hex(G2_OUTSIDE_SUBGROUP)],
    ["y^2 in Fp, root in Fp", hex(G2_ROOT_IN_FP)],
    ["y^2 in Fp, root in Fp u", hex(G2_ROOT_IN_FP_U)],
  ]) {
    add(66, `G2 ${name}`, [encoded]);
  }
  add(66, "compacted G2 encoding", [semanticBytes(1)]);
  add(66, "Miller-loop G2 encoding", [millerLoop]);

  for (const tag of [60, 67]) {
    for (const length of [0, 255, 256]) {
      add(tag, `domain ${length} bytes`, [filled(3), filled(length)]);
      add(tag, `domain ${length} bytes, compacted message`, [
        semanticBytes(1),
        filled(length),
      ]);
      add(tag, `domain ${length} bytes, string message`, [
        str("m"),
        filled(length),
      ]);
    }
    add(tag, "compacted domain", [filled(3), semanticBytes(1)]);
    add(tag, "opaque message, long domain", [opaque, filled(256)]);
  }
};

/** G1 and G2 scalarMul (56, 63) over the scalar bound [-2^4095, 2^4095 - 1]. */
const scalarMulCases = (add) => {
  const bound = 2n ** 4095n;
  for (const [tag, point, other] of [
    [56, g1Point(BLS12_381_G1_GENERATOR), g2Point(BLS12_381_G2_GENERATOR)],
    [63, g2Point(BLS12_381_G2_GENERATOR), g1Point(BLS12_381_G1_GENERATOR)],
  ]) {
    for (const [name, scalar] of [
      ["2^4095 - 1", bound - 1n],
      ["2^4095", bound],
      ["-2^4095", -bound],
      ["-2^4095 - 1", -bound - 1n],
      ["0", 0n],
      ["7", 7n],
      ["2^5000", 2n ** 5000n],
    ]) {
      add(tag, `scalar ${name}`, [int(scalar), point]);
      add(tag, `scalar ${name}, compacted point`, [
        int(scalar),
        semantic(point, 18),
      ]);
    }
    add(tag, "scalar 2^4095, point of the other group", [int(bound), other]);
    add(tag, "scalar 2^4095, byte-string point", [
      int(bound),
      filled(tag === 56 ? 48 : 96),
    ]);
    add(tag, "scalar 2^4095, integer point", [int(bound), int(1)]);
    add(tag, "byte-string scalar", [filled(1), point]);
    add(tag, "compacted scalar", [semantic(int(bound), 65), point]);
    add(tag, "one argument", [int(bound)]);
    add(tag, "three arguments", [int(bound), point, point]);
  }
};

export const cryptoCases = (add) => {
  signatureCases(add);
  utf8Cases(add);
  blsCases(add);
  scalarMulCases(add);
};
