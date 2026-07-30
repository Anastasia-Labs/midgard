import {
  buildMidgardCekDataTraverseTraceV1,
  cardanoTxBytesToMidgardNativeTxCanonicalCborV1,
  computeHash32,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardTxOutput,
  encodeMidgardCekDataFrameV1,
  encodeMidgardCekDataTraverseControlV1,
  finalizeMidgardCekDataTraverseV1,
  MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN_V1,
  nextMidgardCekDataTraverseSpanV1,
  validateMidgardConsensusV1Tx,
} from "@al-ft/midgard-core";
import { CML, Emulator } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildSignedCardanoNestedDatumCandidateV1,
  CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
  deterministicCardanoBoundaryPrivateKeyV1,
  exerciseMidgardOrderedCollectionBoundaryV1,
  findSignedCardanoCollectionBoundaryV1,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
} from "./helpers/ordered-collection-boundary-v1.js";
import { exerciseMidgardRetainedDaBoundaryV1 } from "./helpers/retained-da-boundary-v1.js";

const unaryConstructorPrefixHex = "d8799f";
const unaryConstructorSuffixHex = "ff";
const unaryLeafHex = "00";
const productionRuntimeUnaryDepthWitnessV1 = 1_024;

const cardanoUnaryConstructorDataCborV1 = (depth: number): string => {
  if (!Number.isSafeInteger(depth) || depth <= 0) {
    throw new Error("Cardano unary Data depth must be positive");
  }
  return (
    unaryConstructorPrefixHex.repeat(depth) +
    unaryLeafHex +
    unaryConstructorSuffixHex.repeat(depth)
  );
};

const measureExactUnaryConstructorDataV1 = (
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

type RawSignedCardanoUnaryCandidateV1 = {
  readonly requestedItemCount: number;
  readonly cborHex: string;
  readonly signedBytes: number;
  readonly fee: bigint;
  readonly bodyHash: Buffer;
  readonly signature: Buffer;
  readonly datumCbor: Buffer;
};

const maximumUnaryDepthTerminalVectorV1 = {
  maxTxSize: 16_384,
  cardanoSignedCapacityCandidate: {
    acceptedDepth: 4_043,
    acceptedDatumCborBytes: 16_173,
    acceptedSignedCardanoBytes: 16_384,
    signedCardanoByteMargin: 0,
    adjacentDepth: 4_044,
    adjacentDatumCborBytes: 16_177,
    adjacentSignedCardanoBytes: 16_388,
  },
  midgardProjection: {
    dataNodeCount: 4_044,
    traverseSteps: 16_191,
    maximumSourceSpan: 14,
    terminalPreControlCborHex:
      "8a010600193f2d193f2d58204b1583076e081511c0c79da0bd361e87a6da46e2bbea22cf93f153a3dbb28203d87a80d87a80d87a80d87a80",
    terminalFrameCborHex:
      "8b000040000040010181820058200349c700d41147fa43955b7c1ee2578d2ef8f08599dd99307121859dd2ee8e860184582087f3ecadbcf7a9f6aacd8fb875358df0898dafbc3e02ac97b94590971260e71201193f29193f2d",
    terminalPostControlCborHex:
      "8a010700193f2d193f2d40d87a80d87a80d87a80d8799f835820db84befa89735cb7e184bc06890e5b922bcb7e2550caffdff82dcec934fdd723193f2d193f31ff",
    terminalSummary: {
      rootHex:
        "db84befa89735cb7e184bc06890e5b922bcb7e2550caffdff82dcec934fdd723",
      cborLength: "16173",
      memory: "16177",
    },
  },
} as const;

/**
 * Builds the same simple Alonzo-map transaction shape as CML, but keeps the
 * tag-24 inline datum bytes opaque. This avoids treating the CML/WASM call
 * stack as a Cardano protocol depth limit. The depth-one result is compared
 * byte-for-byte with CML below, including the deterministic vkey witness.
 */
const buildRawSignedCardanoUnaryCandidateV1 = ({
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

describe("canonical V1 Plutus Data unary-depth boundary", () => {
  it("derives the signed capacity boundary and exhaustively traverses its unary datum", async () => {
    const privateKey = deterministicCardanoBoundaryPrivateKeyV1(0);
    const funder = {
      seedPhrase: "",
      privateKey: privateKey.to_bech32(),
      address: CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_pub_key(privateKey.to_public().hash()),
      )
        .to_address()
        .to_bech32(),
      assets: { lovelace: 40_000_000_000n },
    };
    const emulator = new Emulator(
      [funder],
      PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
    );
    const buildCandidate = async (requestedDepth: number) =>
      buildRawSignedCardanoUnaryCandidateV1({
        privateKey,
        inputTransactionId: "00".repeat(32),
        inputLovelace: funder.assets.lovelace,
        recipientAddress: funder.address,
        requestedDepth,
        minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
        minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
      });

    const cmlDepthOne = await buildSignedCardanoNestedDatumCandidateV1({
      privateKeyBech32: funder.privateKey,
      inputTransactionId: "00".repeat(32),
      inputOutputIndex: 0n,
      inputLovelace: funder.assets.lovelace,
      recipientAddress: funder.address,
      requestedNestedLeafCount: 1,
      nestedDatumCborHex: cardanoUnaryConstructorDataCborV1(1),
      minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
      minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
      minFeeRefScriptCostPerByte:
        PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeRefScriptCostPerByte,
    });
    const rawDepthOne = await buildCandidate(1);
    expect(rawDepthOne.cborHex).toBe(cmlDepthOne.cborHex);
    expect(rawDepthOne.fee).toBe(cmlDepthOne.fee);

    const boundary = await findSignedCardanoCollectionBoundaryV1({
      maxTxSize: emulator.protocolParameters.maxTxSize,
      buildSignedCandidate: buildCandidate,
    });
    const accepted = boundary.accepted as RawSignedCardanoUnaryCandidateV1;
    const adjacent = boundary.adjacent as RawSignedCardanoUnaryCandidateV1;
    const acceptedShape = measureExactUnaryConstructorDataV1(
      accepted.datumCbor.toString("hex"),
    );
    const adjacentShape = measureExactUnaryConstructorDataV1(
      adjacent.datumCbor.toString("hex"),
    );

    expect(accepted.signedBytes).toBeLessThanOrEqual(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect(adjacent.signedBytes).toBeGreaterThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect(adjacentShape.depth).toBe(acceptedShape.depth + 1);
    expect(accepted.requestedItemCount).toBe(acceptedShape.depth);
    expect(adjacent.requestedItemCount).toBe(adjacentShape.depth);
    expect(acceptedShape.nodeCount).toBe(acceptedShape.depth + 1);
    expect(acceptedShape.scalarCount).toBe(1);
    expect(accepted.datumCbor.length).toBe(acceptedShape.depth * 4 + 1);
    expect(adjacent.datumCbor.length).toBe(accepted.datumCbor.length + 4);

    const trace = buildMidgardCekDataTraverseTraceV1({
      sourceStart: 0,
      source: accepted.datumCbor,
    });
    const terminalSummary = finalizeMidgardCekDataTraverseV1(trace.terminal);
    expect(terminalSummary).not.toBeNull();
    expect(terminalSummary!.cborLength).toBe(BigInt(accepted.datumCbor.length));
    expect(
      trace.steps.filter(({ action }) => action?.kind === "headSequence"),
    ).toHaveLength(acceptedShape.depth);
    expect(
      trace.steps.filter(({ action }) => action?.kind === "foldList"),
    ).toHaveLength(acceptedShape.depth);
    expect(
      trace.steps.filter(({ action }) => action?.kind === "finalizeFrame"),
    ).toHaveLength(acceptedShape.depth);
    expect(
      trace.steps.filter(({ action }) => action?.kind === "headScalar"),
    ).toHaveLength(1);
    expect(
      trace.steps.filter(({ action }) => action?.kind === "attachScalar"),
    ).toHaveLength(1);
    const maximumSourceSpan = trace.steps.reduce(
      (maximum, { control, sourceBytes }) => {
        const span = nextMidgardCekDataTraverseSpanV1(control);
        if (span === null) {
          expect(sourceBytes).toBeNull();
        } else {
          expect(sourceBytes).not.toBeNull();
          expect(sourceBytes!.length).toBe(span.length);
        }
        return Math.max(maximum, span?.length ?? 0);
      },
      0,
    );
    expect(maximumSourceSpan).toBeLessThanOrEqual(
      MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN_V1,
    );
    const finalStep = trace.steps.at(-1)!;
    expect(finalStep.action?.kind).toBe("finalizeFrame");
    if (finalStep.action?.kind !== "finalizeFrame") {
      throw new Error("Maximum unary datum lost its terminal frame");
    }
    expect(finalStep.action.parent).toBeNull();
    const terminalVector = {
      maxTxSize: CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
      cardanoSignedCapacityCandidate: {
        acceptedDepth: acceptedShape.depth,
        acceptedDatumCborBytes: accepted.datumCbor.length,
        acceptedSignedCardanoBytes: accepted.signedBytes,
        signedCardanoByteMargin:
          CARDANO_BOUNDARY_MAX_TX_SIZE_V1 - accepted.signedBytes,
        adjacentDepth: adjacentShape.depth,
        adjacentDatumCborBytes: adjacent.datumCbor.length,
        adjacentSignedCardanoBytes: adjacent.signedBytes,
      },
      midgardProjection: {
        dataNodeCount: acceptedShape.nodeCount,
        traverseSteps: trace.steps.length,
        maximumSourceSpan,
        terminalPreControlCborHex: encodeMidgardCekDataTraverseControlV1(
          finalStep.control,
        ).toString("hex"),
        terminalFrameCborHex: encodeMidgardCekDataFrameV1(
          finalStep.action.frame,
        ).toString("hex"),
        terminalPostControlCborHex: encodeMidgardCekDataTraverseControlV1(
          finalStep.next,
        ).toString("hex"),
        terminalSummary: {
          rootHex: Buffer.from(terminalSummary!.root).toString("hex"),
          cborLength: terminalSummary!.cborLength.toString(),
          memory: terminalSummary!.memory.toString(),
        },
      },
    };
    expect(terminalVector).toEqual(maximumUnaryDepthTerminalVectorV1);
    if (process.env.MIDGARD_PRINT_AIKEN_VECTOR === "1") {
      console.info(
        JSON.stringify({
          unaryDepthBoundaryV1: terminalVector,
        }),
      );
    }
  }, 300_000);

  it("retains a 1,024-deep production-runtime witness through normal and forced paths", async () => {
    const privateKey = deterministicCardanoBoundaryPrivateKeyV1(0);
    const funder = {
      seedPhrase: "",
      privateKey: privateKey.to_bech32(),
      address: CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_pub_key(privateKey.to_public().hash()),
      )
        .to_address()
        .to_bech32(),
      assets: { lovelace: 40_000_000_000n },
    };
    const emulator = new Emulator(
      [funder],
      PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
    );
    const datumCborHex = cardanoUnaryConstructorDataCborV1(
      productionRuntimeUnaryDepthWitnessV1,
    );
    const candidate = await buildSignedCardanoNestedDatumCandidateV1({
      privateKeyBech32: funder.privateKey,
      inputTransactionId: "00".repeat(32),
      inputOutputIndex: 0n,
      inputLovelace: funder.assets.lovelace,
      recipientAddress: funder.address,
      requestedNestedLeafCount: productionRuntimeUnaryDepthWitnessV1,
      nestedDatumCborHex: datumCborHex,
      minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
      minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
      minFeeRefScriptCostPerByte:
        PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeRefScriptCostPerByte,
    });
    expect(candidate.signedBytes).toBeLessThan(CARDANO_BOUNDARY_MAX_TX_SIZE_V1);
    const canonical = cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
      Buffer.from(candidate.cborHex, "hex"),
    );
    const native = decodeMidgardNativeTxFullV1FromCanonicalCbor(canonical);
    expect(validateMidgardConsensusV1Tx(native, canonical.length)).toBeNull();
    const outputs = decodeMidgardNativeByteListPreimage(
      native.body.outputsPreimageCbor,
      "native.outputs",
    );
    expect(outputs).toHaveLength(1);
    expect(decodeMidgardTxOutput(outputs[0]!).datum?.cbor.toString("hex")).toBe(
      datumCborHex,
    );

    const outputField = exerciseMidgardOrderedCollectionBoundaryV1({
      signedCardanoCborHex: candidate.cborHex,
      fieldIndex: 2,
    });
    expect(outputField.itemCount).toBe(1);
    const retained = await exerciseMidgardRetainedDaBoundaryV1({
      signedCardanoCborHex: candidate.cborHex,
    });
    expect(retained.normal.sourceKind).toBe("normal");
    expect(retained.forced.sourceKind).toBe("forced");
    expect(retained.normal.reconstructedCanonicalBytes).toBe(canonical.length);
    expect(retained.forced.reconstructedCanonicalBytes).toBe(canonical.length);
    expect(retained.normal.revealStepCount).toBe(
      outputField.completeFoldStepCount,
    );
    expect(retained.forced.revealStepCount).toBe(
      outputField.completeFoldStepCount,
    );

    const txHash = await emulator.submitTx(candidate.cborHex);
    await expect(emulator.awaitTx(txHash)).resolves.toBe(true);
  }, 300_000);
});
