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

/**
 * BIP-340 signs a message of any length: vectors 15 to 18 of
 * bitcoin/bips bip-0340/test-vectors.csv (messages of 0, 1, 17 and 100
 * bytes) and signatures under the same key over 31- and 33-byte messages
 * (BIP-340 signing with that key and an all-zero aux_rand). Only the key (32
 * bytes) and the signature (64 bytes) have fixed lengths.
 */
const schnorrEdges = () => {
  const key = fromHex(
    "778caa53b4393ac467774d09497a87224bf9fab6f6e68b23086497324d6fd117",
  );
  const signed = (message, signature) => ({
    message,
    signature: fromHex(signature),
  });
  const counting = (length) => Uint8Array.from({ length }, (_, i) => i);
  const empty = signed(
    new Uint8Array(0),
    "71535db165ecd9fbbc046e5ffaea61186bb6ad436732fccc25291a55895464cf6069ce26bf03466228f19a3a62db8a649f2d560fac652827d1af0574e427ab63",
  );
  const short = signed(
    counting(31),
    "6279da0776f05a84e74bd0400f577f22de28eab33c6cd0a69319b9097e912d4360c3c32b1252eb9b55186684198886abfceaf124f57b2088e809133ddd24d34a",
  );
  const long = signed(
    counting(33),
    "fe02a88f7b6ad46d2a02f678741b1e03ff93ce930235a2c63f644e0d4ee9129d6fdb9aef937e53a826791b584e2392c6135386cca3009b31c7cab9cd3475e7d7",
  );
  const hundred = signed(
    new Uint8Array(100).fill(0x99),
    "403b12b0d8555a344175ea7ec746566303321e5dbfa8be6f091635163eca79a8585ed3e3170807e7c03b720fc54c7b23897fcba0e9d0b4a06894cfd249f22367",
  );
  const valid = [
    empty,
    signed(
      fromHex("11"),
      "08a20a0afef64124649232e0693c583ab1b9934ae63b4c3511f3ae1134c6a303ea3173bfea6683bd101fa5aa5dbc1996fe7cacfc5a577d33ec14564cec2bacbf",
    ),
    signed(
      fromHex("0102030405060708090a0b0c0d0e0f1011"),
      "5130f39a4059b43bc7cac09a19ece52b5d8699d1a71e3c52da9afdb6b50ac370c4a482b77bf960f8681540e25b6771ece1e5a37fd80e5a51897c5566a97ea5a5",
    ),
    short,
    long,
    hundred,
  ];
  return [
    ...valid.map(({ message, signature }) => ({
      note: `valid signature, message ${message.length} bytes`,
      args: [bs(key), bs(message), bs(signature)],
    })),
    {
      note: "signature over another message, message 33 bytes",
      args: [bs(key), bs(long.message), bs(short.signature)],
    },
    {
      note: "key 31 bytes, message 100 bytes",
      args: [
        bs(key.subarray(0, 31)),
        bs(hundred.message),
        bs(hundred.signature),
      ],
    },
    {
      note: "key 33 bytes, message 0 bytes",
      args: [
        bs(Uint8Array.from([...key, 0])),
        bs(empty.message),
        bs(empty.signature),
      ],
    },
    {
      note: "signature 63 bytes, message 31 bytes",
      args: [bs(key), bs(short.message), bs(short.signature.subarray(0, 63))],
    },
    {
      note: "signature 65 bytes, message 100 bytes",
      args: [
        bs(key),
        bs(hundred.message),
        bs(Uint8Array.from([...hundred.signature, 0])),
      ],
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
  53: schnorrEdges,
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
