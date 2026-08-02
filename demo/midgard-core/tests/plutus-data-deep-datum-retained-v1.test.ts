import { createHash } from "node:crypto";

import { CML } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  cardanoTxBytesToMidgardNativeTxCanonicalCborV1,
  computeHash32,
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardTxOutput,
  deriveMidgardNativeTxProofSourceV1,
  deriveMidgardV1TxFieldChunks,
  encodeCbor,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardTxOutput,
  materializeMidgardNativeTxFromCanonicalV1,
  reconstructMidgardTransactionV1FromChunks,
  validateMidgardConsensusV1Tx,
  verifyMidgardV1TxFieldChunk,
} from "../src/index.js";

/**
 * C26 depth regression: the exact Cardano-derived maximum unary datum
 * (depth 4,043 inside a 16,384-byte signed transaction) must pass full
 * canonical decode, consensus validation, and the complete retained
 * reconstruction fold with stock, unpatched CML.
 *
 * The pinned sha256 digests were captured while `decodeMidgardDatum` still
 * probed decodability through the recursive CML/lucid `Data.from` path, so
 * they also prove the recursion-free gate produces byte-identical canonical
 * transactions at the depth that path could still reach.
 */
const unaryConstructorDataCborHex = (depth: number): string =>
  "d8799f".repeat(depth) + "00" + "ff".repeat(depth);

const encodeCborHead = (major: number, value: bigint): Buffer => {
  if (value < 24n) {
    return Buffer.from([(major << 5) | Number(value)]);
  }
  const widths = [
    { limit: 0xffn, additional: 24, bytes: 1 },
    { limit: 0xffffn, additional: 25, bytes: 2 },
    { limit: 0xffff_ffffn, additional: 26, bytes: 4 },
    { limit: 0xffff_ffff_ffff_ffffn, additional: 27, bytes: 8 },
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
const uintCbor = (value: bigint): Buffer => encodeCborHead(0, value);
const bytesCbor = (bytes: Uint8Array): Buffer =>
  Buffer.concat([encodeCborHead(2, BigInt(bytes.length)), Buffer.from(bytes)]);
const arrayCbor = (items: readonly Uint8Array[]): Buffer =>
  Buffer.concat([
    encodeCborHead(4, BigInt(items.length)),
    ...items.map((item) => Buffer.from(item)),
  ]);
const mapCbor = (
  entries: readonly (readonly [Uint8Array, Uint8Array])[],
): Buffer =>
  Buffer.concat([
    encodeCborHead(5, BigInt(entries.length)),
    ...entries.flatMap(([key, value]) => [
      Buffer.from(key),
      Buffer.from(value),
    ]),
  ]);
const tagCbor = (tag: bigint, item: Uint8Array): Buffer =>
  Buffer.concat([encodeCborHead(6, tag), Buffer.from(item)]);

const boundaryPrivateKey = (): CML.PrivateKey =>
  CML.PrivateKey.from_normal_bytes(
    computeHash32(
      Buffer.concat([
        Buffer.from("CardanoBoundarySignerKeyV1", "utf8"),
        Buffer.alloc(4),
      ]),
    ),
  );

/**
 * The same raw Alonzo-map transaction shape the unary-depth boundary suite
 * signs: one genesis input, one inline-datum output, converged linear fee,
 * one deterministic vkey witness.
 */
const buildSignedUnaryCandidate = (datumDepth: number): Buffer => {
  const privateKey = boundaryPrivateKey();
  const address = CML.EnterpriseAddress.new(
    0,
    CML.Credential.new_pub_key(privateKey.to_public().hash()),
  )
    .to_address()
    .to_raw_bytes();
  const datumCbor = Buffer.from(unaryConstructorDataCborHex(datumDepth), "hex");
  const inputs = tagCbor(
    258n,
    arrayCbor([arrayCbor([bytesCbor(Buffer.alloc(32)), uintCbor(0n)])]),
  );
  const datumOption = arrayCbor([
    uintCbor(1n),
    tagCbor(24n, bytesCbor(datumCbor)),
  ]);
  const publicKey = privateKey.to_public().to_raw_bytes();
  let fee = 155_381n;
  for (let attempt = 0; attempt < 10; attempt += 1) {
    const output = mapCbor([
      [uintCbor(0n), bytesCbor(address)],
      [uintCbor(1n), uintCbor(40_000_000_000n - fee)],
      [uintCbor(2n), datumOption],
    ]);
    const body = mapCbor([
      [uintCbor(0n), inputs],
      [uintCbor(1n), arrayCbor([output])],
      [uintCbor(2n), uintCbor(fee)],
    ]);
    const signature = privateKey.sign(computeHash32(body)).to_raw_bytes();
    const witnessSet = mapCbor([
      [
        uintCbor(0n),
        tagCbor(
          258n,
          arrayCbor([arrayCbor([bytesCbor(publicKey), bytesCbor(signature)])]),
        ),
      ],
    ]);
    const signed = arrayCbor([
      body,
      witnessSet,
      Buffer.from([0xf5]),
      Buffer.from([0xf6]),
    ]);
    const nextFee = 44n * BigInt(signed.length) + 155_381n;
    if (nextFee === fee) {
      return signed;
    }
    fee = nextFee;
  }
  throw new Error("unary candidate fee did not converge");
};

/**
 * Builds the canonical Midgard transaction that carries a depth-`datumDepth`
 * unary datum without asking CML to parse the deep bytes: the shallow
 * depth-one candidate goes through the production Cardano conversion, then
 * the output datum is replaced through the repo's own codecs.
 */
const canonicalWithSubstitutedDatum = (datumDepth: number): Buffer => {
  const shallowCanonical = cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
    buildSignedUnaryCandidate(1),
  );
  const native = decodeMidgardNativeTxFullV1FromCanonicalCbor(shallowCanonical);
  const outputsBytes = decodeMidgardNativeByteListPreimage(
    native.body.outputsPreimageCbor,
    "native.outputs",
  );
  expect(outputsBytes).toHaveLength(1);
  const decodedOutput = decodeMidgardTxOutput(outputsBytes[0]!);
  expect(decodedOutput.datum?.cbor.toString("hex")).toBe(
    unaryConstructorDataCborHex(1),
  );
  const deepOutput = encodeMidgardTxOutput({
    ...decodedOutput,
    datum: {
      kind: "inline",
      cbor: Buffer.from(unaryConstructorDataCborHex(datumDepth), "hex"),
    },
  });
  const deepNative = materializeMidgardNativeTxFromCanonicalV1({
    version: native.version,
    validity: native.validity,
    body: {
      ...native.body,
      outputsPreimageCbor: encodeCbor([deepOutput]),
    },
    witnessSet: native.witnessSet,
  });
  return encodeMidgardNativeTxCanonicalV1(deepNative);
};

const exerciseRetainedReconstruction = (
  canonical: Buffer,
  datumDepth: number,
): { readonly revealStepCount: number } => {
  const native = decodeMidgardNativeTxFullV1FromCanonicalCbor(canonical);
  expect(validateMidgardConsensusV1Tx(native, canonical.length)).toBeNull();
  const outputs = decodeMidgardNativeByteListPreimage(
    native.body.outputsPreimageCbor,
    "native.outputs",
  );
  expect(outputs).toHaveLength(1);
  expect(decodeMidgardTxOutput(outputs[0]!).datum?.cbor.toString("hex")).toBe(
    unaryConstructorDataCborHex(datumDepth),
  );
  const transactionId = computeMidgardNativeTxIdV1(native);
  const source = deriveMidgardNativeTxProofSourceV1(native);
  const transactionCommitment = computeMidgardNativeTxProofCommitmentV1(source);
  const chunkProofs = deriveMidgardV1TxFieldChunks(canonical);
  for (const chunk of chunkProofs) {
    verifyMidgardV1TxFieldChunk({
      transactionId,
      transactionCommitment,
      source,
      collectionProof: chunk.collectionProof,
      proof: chunk.proof,
    });
  }
  const reconstructed = reconstructMidgardTransactionV1FromChunks({
    transactionId,
    transactionCommitment,
    source,
    chunkProofs,
  });
  expect(reconstructed.equals(canonical)).toBe(true);
  return { revealStepCount: chunkProofs.length };
};

const sha256Hex = (bytes: Uint8Array): string =>
  createHash("sha256").update(bytes).digest("hex");

describe("canonical V1 deep unary datum with stock CML", () => {
  it("converts the production depth-1,024 candidate byte-identically to the recursive-probe era", () => {
    const canonical = cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
      buildSignedUnaryCandidate(1_024),
    );
    expect(canonical.length).toBe(4_394);
    expect(sha256Hex(canonical)).toBe(
      "006323b169693e4ee90e58656a0bd4e6dba7590a1a4de65462e1ae43ae1c9d6f",
    );
    exerciseRetainedReconstruction(canonical, 1_024);
  });

  it("carries the substituted depth-1,024 datum byte-identically to the recursive-probe era", () => {
    const canonical = canonicalWithSubstitutedDatum(1_024);
    expect(canonical.length).toBe(4_394);
    expect(sha256Hex(canonical)).toBe(
      "ae9f29c7b2adbc36daca28b22243b49a7eb1882dc4f011ea6559c54baf294b73",
    );
    exerciseRetainedReconstruction(canonical, 1_024);
  });

  it("reaches the exact depth-4,043 maximum through decode, validation, and the retained fold", () => {
    const canonical = canonicalWithSubstitutedDatum(4_043);
    expect(canonical.length).toBe(16_470);
    expect(sha256Hex(canonical)).toBe(
      "81a330db0b6355a1dbd5d7f46e2cb9198cab407a94848567627d8071b1f2afd1",
    );
    const { revealStepCount } = exerciseRetainedReconstruction(
      canonical,
      4_043,
    );
    expect(revealStepCount).toBe(6);
  }, 60_000);

  it("also handles the adjacent depth-4,044 datum as canonical Midgard content", () => {
    // Depth 4,044 exceeds the signed 16,384-byte Cardano capacity as a
    // transaction, but the Midgard codecs must still treat the value itself
    // as ordinary canonical Data rather than tripping a recursion limit.
    const canonical = canonicalWithSubstitutedDatum(4_044);
    exerciseRetainedReconstruction(canonical, 4_044);
  }, 60_000);
});
