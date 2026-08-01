import { computeHash32 } from "@al-ft/midgard-core";
import { CML } from "@lucid-evolution/lucid";

/**
 * Raw (CML-free) builder for the canonical V1 unary-depth Cardano candidate.
 *
 * Extracted from `plutus-data-unary-depth-boundary-v1.test.ts` so the boundary
 * suite and the out-of-process maximum-depth admission share one builder. The
 * transaction is assembled byte by byte rather than through CML precisely so
 * that constructing the candidate never depends on CML's recursive decoder —
 * that dependency is what made the maximum unreachable in the first place, and
 * the boundary suite pins the depth-one output against CML byte for byte.
 */

const unaryConstructorPrefixHex = "d8799f";
const unaryConstructorSuffixHex = "ff";
const unaryLeafHex = "00";

export const cardanoUnaryConstructorDataCborV1 = (depth: number): string => {
  if (!Number.isSafeInteger(depth) || depth <= 0) {
    throw new Error("Cardano unary Data depth must be positive");
  }
  return (
    unaryConstructorPrefixHex.repeat(depth) +
    unaryLeafHex +
    unaryConstructorSuffixHex.repeat(depth)
  );
};

export const measureExactUnaryConstructorDataV1 = (
  datumCborHex: string,
): {
  readonly depth: number;
  readonly nodeCount: number;
  readonly scalarCount: number;
} => {
  const bytes = Buffer.from(datumCborHex, "hex");
  let cursor = 0;
  let depth = 0;
  while (
    cursor + 3 <= bytes.length &&
    bytes[cursor] === 0xd8 &&
    bytes[cursor + 1] === 0x79 &&
    bytes[cursor + 2] === 0x9f
  ) {
    depth += 1;
    cursor += 3;
  }
  if (depth === 0 || bytes[cursor] !== 0x00) {
    throw new Error("Unary Data must terminate in the exact integer-zero leaf");
  }
  cursor += 1;
  for (let index = 0; index < depth; index += 1) {
    if (bytes[cursor + index] !== 0xff) {
      throw new Error("Unary Data constructor must have exactly one child");
    }
  }
  if (cursor + depth !== bytes.length) {
    throw new Error("Unary Data contains trailing bytes");
  }
  return {
    depth,
    nodeCount: depth + 1,
    scalarCount: 1,
  };
};

const encodeCborHead = (major: number, value: bigint): Buffer => {
  if (
    !Number.isSafeInteger(major) ||
    major < 0 ||
    major > 7 ||
    value < 0n ||
    value > 0xffff_ffff_ffff_ffffn
  ) {
    throw new Error("CBOR head is outside the supported uint64 range");
  }
  if (value < 24n) {
    return Buffer.from([(major << 5) | Number(value)]);
  }
  const widths = [
    { limit: 0xffn, additional: 24, bytes: 1 },
    { limit: 0xffffn, additional: 25, bytes: 2 },
    { limit: 0xffff_ffffn, additional: 26, bytes: 4 },
    {
      limit: 0xffff_ffff_ffff_ffffn,
      additional: 27,
      bytes: 8,
    },
  ] as const;
  const width = widths.find(({ limit }) => value <= limit)!;
  const encoded = Buffer.alloc(1 + width.bytes);
  encoded[0] = (major << 5) | width.additional;
  let remaining = value;
  for (let index = width.bytes; index > 0; index -= 1) {
    encoded[index] = Number(remaining & 0xffn);
    remaining >>= 8n;
  }
  return encoded;
};

const encodeCborUint = (value: bigint): Buffer => encodeCborHead(0, value);

const encodeCborBytes = (bytes: Uint8Array): Buffer =>
  Buffer.concat([encodeCborHead(2, BigInt(bytes.length)), Buffer.from(bytes)]);

const encodeCborArrayRaw = (items: readonly Uint8Array[]): Buffer =>
  Buffer.concat([
    encodeCborHead(4, BigInt(items.length)),
    ...items.map((item) => Buffer.from(item)),
  ]);

const encodeCborMapRaw = (
  entries: readonly (readonly [Uint8Array, Uint8Array])[],
): Buffer =>
  Buffer.concat([
    encodeCborHead(5, BigInt(entries.length)),
    ...entries.flatMap(([key, value]) => [
      Buffer.from(key),
      Buffer.from(value),
    ]),
  ]);

const encodeCborTagRaw = (tag: bigint, item: Uint8Array): Buffer =>
  Buffer.concat([encodeCborHead(6, tag), Buffer.from(item)]);

export type RawSignedCardanoUnaryCandidateV1 = {
  readonly requestedItemCount: number;
  readonly cborHex: string;
  readonly signedBytes: number;
  readonly fee: bigint;
  readonly bodyHash: Buffer;
  readonly signature: Buffer;
  readonly datumCbor: Buffer;
};

/**
 * Builds the same simple Alonzo-map transaction shape as CML, but keeps the
 * tag-24 inline datum bytes opaque. This avoids treating the CML/WASM call
 * stack as a Cardano protocol depth limit. The depth-one result is compared
 * byte-for-byte with CML in the boundary suite, including the deterministic
 * vkey witness.
 */
export const buildRawSignedCardanoUnaryCandidateV1 = ({
  privateKey,
  inputTransactionId,
  inputLovelace,
  recipientAddress,
  requestedDepth,
  minFeeA,
  minFeeB,
}: {
  readonly privateKey: CML.PrivateKey;
  readonly inputTransactionId: string;
  readonly inputLovelace: bigint;
  readonly recipientAddress: string;
  readonly requestedDepth: number;
  readonly minFeeA: number;
  readonly minFeeB: number;
}): RawSignedCardanoUnaryCandidateV1 => {
  const datumCbor = Buffer.from(
    cardanoUnaryConstructorDataCborV1(requestedDepth),
    "hex",
  );
  const input = encodeCborArrayRaw([
    encodeCborBytes(Buffer.from(inputTransactionId, "hex")),
    encodeCborUint(0n),
  ]);
  const inputs = encodeCborTagRaw(258n, encodeCborArrayRaw([input]));
  const address = Buffer.from(
    CML.Address.from_bech32(recipientAddress).to_raw_bytes(),
  );
  const datumOption = encodeCborArrayRaw([
    encodeCborUint(1n),
    encodeCborTagRaw(24n, encodeCborBytes(datumCbor)),
  ]);
  const publicKey = Buffer.from(privateKey.to_public().to_raw_bytes());

  let fee = BigInt(minFeeB);
  for (let attempt = 0; attempt < 10; attempt += 1) {
    const outputLovelace = inputLovelace - fee;
    if (outputLovelace <= 0n) {
      throw new Error("Unary-depth candidate exhausts its funding input");
    }
    const output = encodeCborMapRaw([
      [encodeCborUint(0n), encodeCborBytes(address)],
      [encodeCborUint(1n), encodeCborUint(outputLovelace)],
      [encodeCborUint(2n), datumOption],
    ]);
    const body = encodeCborMapRaw([
      [encodeCborUint(0n), inputs],
      [encodeCborUint(1n), encodeCborArrayRaw([output])],
      [encodeCborUint(2n), encodeCborUint(fee)],
    ]);
    const bodyHash = computeHash32(body);
    const signature = Buffer.from(privateKey.sign(bodyHash).to_raw_bytes());
    const vkeyWitness = encodeCborArrayRaw([
      encodeCborBytes(publicKey),
      encodeCborBytes(signature),
    ]);
    const witnessSet = encodeCborMapRaw([
      [
        encodeCborUint(0n),
        encodeCborTagRaw(258n, encodeCborArrayRaw([vkeyWitness])),
      ],
    ]);
    const signed = encodeCborArrayRaw([
      body,
      witnessSet,
      Buffer.from([0xf5]),
      Buffer.from([0xf6]),
    ]);
    const nextFee = BigInt(minFeeA) * BigInt(signed.length) + BigInt(minFeeB);
    if (nextFee === fee) {
      if (
        !privateKey
          .to_public()
          .verify(bodyHash, CML.Ed25519Signature.from_raw_bytes(signature))
      ) {
        throw new Error("Unary-depth Cardano vkey witness did not verify");
      }
      return {
        requestedItemCount: requestedDepth,
        cborHex: signed.toString("hex"),
        signedBytes: signed.length,
        fee,
        bodyHash,
        signature,
        datumCbor,
      };
    }
    fee = nextFee;
  }
  throw new Error("Unary-depth Cardano fee did not converge");
};
