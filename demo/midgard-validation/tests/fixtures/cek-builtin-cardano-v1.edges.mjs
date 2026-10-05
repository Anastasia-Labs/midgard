/**
 * The fixed edge vectors of the `cek-builtin-cardano-v1` golden channel: the
 * size, range and encoding rules each builtin enforces, and the ledger's
 * outcome where it differs from the pinned Aiken evaluator's.
 */
import { ed25519, ED25519_TORSION_SUBGROUP } from "@noble/curves/ed25519.js";
import { secp256k1 } from "@noble/curves/secp256k1.js";
import { sha512 } from "@noble/hashes/sha2.js";

import {
  EC_KEY,
  EC_SECRET,
  ED25519_L,
  ED25519_P,
  ED_KEY,
  ED_SECRET,
  G1_POOL,
  G2_POOL,
  INT64_MAX,
  INT64_MIN,
  SECP256K1_N,
  SECP256K1_P,
  be32,
  boolArg,
  bs,
  ecdsaSign,
  fromHex,
  g1Of,
  g2Of,
  hex,
  int,
  le32,
  leInt,
  loop,
  ml,
  mul,
  sha256,
} from "./cek-builtin-cardano-v1.arguments.mjs";

const ed25519Edges = () => {
  const message = fromHex("010203");
  const signature = ed25519.sign(message, ED_SECRET);
  const r = signature.subarray(0, 32);
  const s = leInt(signature.subarray(32));
  const identity = ed25519.Point.ZERO.toBytes();
  const orderEight = fromHex(ED25519_TORSION_SUBGROUP[1]);
  // A key with a small-order component: A = aB + T for an order-8 T, signed
  // so that cofactorless and cofactored verification disagree or agree.
  const a = 12345n;
  const mixedKey = ed25519.Point.BASE.multiply(a)
    .add(ed25519.Point.fromBytes(orderEight))
    .toBytes();
  const mixedSignature = (wantMultipleOfEight) => {
    for (let nonce = 1n; ; nonce++) {
      const encodedR = ed25519.Point.BASE.multiply(nonce).toBytes();
      const h =
        leInt(sha512(Uint8Array.from([...encodedR, ...mixedKey, ...message]))) %
        ED25519_L;
      if ((h % 8n === 0n) === wantMultipleOfEight) {
        return Uint8Array.from([
          ...encodedR,
          ...le32((nonce + h * a) % ED25519_L),
        ]);
      }
    }
  };
  const nonCanonicalKey = le32(ED25519_P + 3n);
  return [
    { note: "valid signature", args: [bs(ED_KEY), bs(message), bs(signature)] },
    {
      note: "S + L in place of S",
      args: [
        bs(ED_KEY),
        bs(message),
        bs(Uint8Array.from([...r, ...le32(s + ED25519_L)])),
      ],
    },
    {
      note: "S = L",
      args: [
        bs(ED_KEY),
        bs(message),
        bs(Uint8Array.from([...r, ...le32(ED25519_L)])),
      ],
    },
    {
      note: "identity key, identity R, S = 0",
      args: [
        bs(identity),
        bs(message),
        bs(Uint8Array.from([...identity, ...new Uint8Array(32)])),
      ],
      // Cardano verifies through cardano-crypto-class Ed25519DSIGN, which
      // calls libsodium's crypto_sign_ed25519_verify_detached (IOHK's 1.0.18
      // build): ref10/open.c returns -1 when ge25519_has_small_order(pk).
      cardano: { kind: "value", type: "bool", value: false },
      reason:
        "libsodium crypto_sign_ed25519_verify_detached refuses a small-order public key",
    },
    {
      note: "order-8 key, order-8 R, S = 0",
      args: [
        bs(orderEight),
        bs(message),
        bs(Uint8Array.from([...orderEight, ...new Uint8Array(32)])),
      ],
    },
    {
      note: "valid key, identity R",
      args: [
        bs(ED_KEY),
        bs(message),
        bs(Uint8Array.from([...identity, ...signature.subarray(32)])),
      ],
    },
    {
      note: "mixed-order key, h not a multiple of 8",
      args: [bs(mixedKey), bs(message), bs(mixedSignature(false))],
    },
    {
      note: "mixed-order key, h a multiple of 8",
      args: [bs(mixedKey), bs(message), bs(mixedSignature(true))],
    },
    {
      note: "key encodes y = p + 3",
      args: [bs(nonCanonicalKey), bs(message), bs(signature)],
    },
    {
      note: "key all 0xff",
      args: [bs("ff".repeat(32)), bs(message), bs(signature)],
    },
    {
      note: "R all 0xff",
      args: [
        bs(ED_KEY),
        bs(message),
        bs(
          Uint8Array.from([
            ...new Uint8Array(32).fill(0xff),
            ...signature.subarray(32),
          ]),
        ),
      ],
    },
    {
      note: "key 31 bytes",
      args: [bs(ED_KEY.subarray(0, 31)), bs(message), bs(signature)],
    },
    {
      note: "key 33 bytes",
      args: [bs(Uint8Array.from([...ED_KEY, 0])), bs(message), bs(signature)],
    },
    {
      note: "signature 63 bytes",
      args: [bs(ED_KEY), bs(message), bs(signature.subarray(0, 63))],
    },
    {
      note: "signature 65 bytes",
      args: [bs(ED_KEY), bs(message), bs(Uint8Array.from([...signature, 0]))],
    },
  ];
};

const ecdsaEdges = () => {
  const digest = sha256(fromHex("0102"));
  const signature = ecdsaSign(digest);
  const r = signature.subarray(0, 32);
  const s = BigInt(`0x${hex(signature.subarray(32))}`);
  const withRS = (rValue, sValue) =>
    Uint8Array.from([...rValue, ...be32(sValue)]);
  const key = (prefix, x) => Uint8Array.from([prefix, ...be32(x)]);
  let offCurveX = 5n;
  for (;;) {
    try {
      secp256k1.Point.fromBytes(key(2, offCurveX));
      offCurveX++;
    } catch {
      break;
    }
  }
  return [
    { note: "valid low-s", args: [bs(EC_KEY), bs(digest), bs(signature)] },
    {
      note: "high-s",
      args: [bs(EC_KEY), bs(digest), bs(withRS(r, SECP256K1_N - s))],
    },
    {
      note: "r = 0",
      args: [bs(EC_KEY), bs(digest), bs(withRS(new Uint8Array(32), s))],
    },
    { note: "s = 0", args: [bs(EC_KEY), bs(digest), bs(withRS(r, 0n))] },
    {
      note: "r = n",
      args: [bs(EC_KEY), bs(digest), bs(withRS(be32(SECP256K1_N), s))],
    },
    {
      note: "s = n",
      args: [bs(EC_KEY), bs(digest), bs(withRS(r, SECP256K1_N))],
    },
    {
      note: "r all 0xff",
      args: [
        bs(EC_KEY),
        bs(digest),
        bs(withRS(new Uint8Array(32).fill(0xff), s)),
      ],
    },
    {
      note: "wrong message",
      args: [bs(EC_KEY), bs(sha256(fromHex("03"))), bs(signature)],
    },
    {
      note: "key prefix 04",
      args: [
        bs(Uint8Array.from([4, ...EC_KEY.subarray(1)])),
        bs(digest),
        bs(signature),
      ],
    },
    {
      note: "key prefix 00",
      args: [
        bs(Uint8Array.from([0, ...EC_KEY.subarray(1)])),
        bs(digest),
        bs(signature),
      ],
    },
    {
      note: "key x off the curve",
      args: [bs(key(2, offCurveX)), bs(digest), bs(signature)],
    },
    {
      note: "key x = p",
      args: [bs(key(2, SECP256K1_P)), bs(digest), bs(signature)],
    },
    {
      note: "uncompressed 65-byte key",
      args: [
        bs(secp256k1.getPublicKey(EC_SECRET, false)),
        bs(digest),
        bs(signature),
      ],
      // CIP-49; plutus-core PlutusCore/Crypto/Secp256k1.hs
      // verifyEcdsaSecp256k1Signature fails with "Invalid verification key."
      // when rawDeserialiseVerKeyDSIGN @EcdsaSecp256k1DSIGN refuses the key,
      // and that accepts only the 33-byte compressed form.
      cardano: { kind: "error" },
      reason:
        "Cardano accepts only a 33-byte compressed secp256k1 verification key (CIP-49)",
    },
    {
      note: "message 31 bytes",
      args: [bs(EC_KEY), bs(digest.subarray(0, 31)), bs(signature)],
    },
    {
      note: "message 33 bytes",
      args: [bs(EC_KEY), bs(Uint8Array.from([...digest, 0])), bs(signature)],
    },
    {
      note: "signature 63 bytes",
      args: [bs(EC_KEY), bs(digest), bs(signature.subarray(0, 63))],
    },
  ];
};

const blsFinalEdges = () => {
  const [infinity1] = G1_POOL;
  const [infinity2] = G2_POOL;
  return [
    {
      note: "loops over infinity on each side",
      args: [ml(loop(infinity1, g2Of(1n))), ml(loop(g1Of(1n), infinity2))],
    },
    {
      note: "infinity loop against a generator loop",
      args: [ml(loop(infinity1, g2Of(1n))), ml(loop(g1Of(1n), g2Of(1n)))],
    },
    {
      note: "product of infinity loops against an infinity loop",
      args: [
        ml(loop(infinity1, infinity2)),
        ml(mul(loop(infinity1, g2Of(1n)), loop(g1Of(1n), infinity2))),
      ],
    },
    {
      note: "bilinearity e(3P, Q) = e(P, 3Q)",
      args: [ml(loop(g1Of(3n), g2Of(1n))), ml(loop(g1Of(1n), g2Of(3n)))],
    },
    {
      note: "e(2P, Q) e(P, Q) = e(P, 3Q)",
      args: [
        ml(mul(loop(g1Of(2n), g2Of(1n)), loop(g1Of(1n), g2Of(1n)))),
        ml(loop(g1Of(1n), g2Of(3n))),
      ],
    },
    {
      note: "e(2P, Q) against e(P, 3Q)",
      args: [ml(loop(g1Of(2n), g2Of(1n))), ml(loop(g1Of(1n), g2Of(3n)))],
    },
  ];
};

const BYTES_0102 = "0102";
const INT64_EDGES = [
  INT64_MAX,
  INT64_MAX + 1n,
  INT64_MIN,
  INT64_MIN - 1n,
  (1n << 64n) + 1n,
];

export const EDGES = {
  12: () => [
    ...[
      [INT64_MAX, 1n],
      [INT64_MAX + 1n, 0n],
      [INT64_MIN, 2n],
      [INT64_MIN - 1n, 1n],
      [0n, INT64_MAX],
      [0n, INT64_MAX + 1n],
      [1n, INT64_MIN],
      [-4n, 1n << 128n],
      [3n, -1n],
      [100n, 5n],
      [-3n, 4n],
    ].map(([start, count]) => ({
      note: `slice ${start} ${count}`,
      args: [int(start), int(count), bs("0011223344556677")],
    })),
  ],
  16: () =>
    [
      ["01", "0000"],
      ["00", "0000"],
      ["0000", "00"],
      ["0102", "0102"],
      ["", ""],
      ["", "00"],
      ["ff", "00ff"],
    ].map(([left, right]) => ({
      note: `${left || "empty"} vs ${right || "empty"}`,
      args: [bs(left), bs(right)],
    })),
  21: ed25519Edges,
  52: ecdsaEdges,
  70: blsFinalEdges,
  77: () =>
    [
      [true, "ac12", "02b4bfbe20"],
      [false, "ac12", "02b4bfbe20"],
      [true, "02b4bfbe20", "ac12"],
      [true, "", "ff00"],
      [false, "", "ff00"],
      [true, "", ""],
    ].map(([padding, left, right]) => ({
      note: `xor padding=${padding} ${left || "empty"} ${right || "empty"}`,
      args: [boolArg(padding), bs(left), bs(right)],
    })),
  82: () => [
    ...INT64_EDGES.map((amount) => ({
      note: `shift by ${amount}`,
      args: [bs(BYTES_0102), int(amount)],
    })),
    { note: "shift empty by 3", args: [bs(""), int(3n)] },
    { note: "shift by -3", args: [bs("80ff01"), int(-3n)] },
    { note: "shift by 9", args: [bs("80ff01"), int(9n)] },
  ],
  83: () => [
    ...INT64_EDGES.map((amount) => ({
      note: `rotate by ${amount}`,
      args: [bs(BYTES_0102), int(amount)],
    })),
    { note: "rotate by -3", args: [bs(BYTES_0102), int(-3n)] },
    { note: "rotate by 19", args: [bs(BYTES_0102), int(19n)] },
    { note: "rotate empty by -5", args: [bs(""), int(-5n)] },
  ],
  85: () =>
    ["8000", "e7", "", "0000", "0100", "0001", "80"].map((value) => ({
      note: `findFirstSetBit ${value || "empty"}`,
      args: [bs(value)],
    })),
};
EDGES[17] = EDGES[16];
