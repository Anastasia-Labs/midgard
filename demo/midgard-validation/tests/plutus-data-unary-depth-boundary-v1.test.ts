import {
  buildMidgardCekDataTraverseTraceV1,
  cardanoTxBytesToMidgardNativeTxCanonicalCborV1,
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

import { runMaxDepthCmlOperationV1 } from "./helpers/cml-max-depth-runner-v1.js";
import {
  buildSignedCardanoNestedDatumCandidateV1,
  CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
  deterministicCardanoBoundaryPrivateKeyV1,
  exerciseMidgardOrderedCollectionBoundaryV1,
  findSignedCardanoCollectionBoundaryV1,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
} from "./helpers/ordered-collection-boundary-v1.js";
import { exerciseMidgardRetainedDaBoundaryV1 } from "./helpers/retained-da-boundary-v1.js";
import {
  buildRawSignedCardanoUnaryCandidateV1,
  cardanoUnaryConstructorDataCborV1,
  measureExactUnaryConstructorDataV1,
  type RawSignedCardanoUnaryCandidateV1,
} from "./helpers/unary-depth-candidate-v1.js";

const productionRuntimeUnaryDepthWitnessV1 = 1_024;

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

  /**
   * Genuine emulator admission at the exact derived maximum.
   *
   * This is the case C26 could not reach before: `Emulator.submitTx` parses the
   * candidate through `CML.Transaction.from_cbor_hex`, whose stock 1 MiB wasm
   * shadow stack traps at depth ~1,503. It runs in a child process with
   * `--stack-size=2000` because the patched shadow stack exposes V8's separate
   * machine-stack limit — see `helpers/cml-max-depth-runner-v1.ts`.
   */
  it("admits the exact maximum-depth candidate through the real emulator", () => {
    const privateKey = deterministicCardanoBoundaryPrivateKeyV1(0);
    const address = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(privateKey.to_public().hash()),
    )
      .to_address()
      .to_bech32();
    const inputLovelace = 40_000_000_000n;
    const buildCandidate = (requestedDepth: number) =>
      buildRawSignedCardanoUnaryCandidateV1({
        privateKey,
        inputTransactionId: "00".repeat(32),
        inputLovelace,
        recipientAddress: address,
        requestedDepth,
        minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
        minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
      });

    const { cardanoSignedCapacityCandidate } =
      maximumUnaryDepthTerminalVectorV1;
    const accepted = buildCandidate(
      cardanoSignedCapacityCandidate.acceptedDepth,
    );
    const adjacent = buildCandidate(
      cardanoSignedCapacityCandidate.adjacentDepth,
    );
    expect(accepted.signedBytes).toBe(
      cardanoSignedCapacityCandidate.acceptedSignedCardanoBytes,
    );
    expect(accepted.signedBytes).toBe(CARDANO_BOUNDARY_MAX_TX_SIZE_V1);
    expect(
      measureExactUnaryConstructorDataV1(accepted.datumCbor.toString("hex"))
        .depth,
    ).toBe(cardanoSignedCapacityCandidate.acceptedDepth);
    // Adjacent rejection keeps deriving from the signed byte count, not from
    // the emulator: the emulator does not enforce `maxTxSize`, so depth 4,044
    // would be admitted by it even though a real node rejects the 16,388-byte
    // transaction. The boundary is the byte count, asserted here.
    expect(adjacent.signedBytes).toBe(
      cardanoSignedCapacityCandidate.adjacentSignedCardanoBytes,
    );
    expect(adjacent.signedBytes).toBeGreaterThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );

    const admission = runMaxDepthCmlOperationV1({
      operation: "emulatorAdmission",
      signedTxHex: accepted.cborHex,
      expectedDatumHex: accepted.datumCbor.toString("hex"),
      account: {
        privateKey: privateKey.to_bech32(),
        address,
        assets: { lovelace: inputLovelace },
      },
      protocolParameters: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
    });

    expect(admission).toMatchObject({
      ok: true,
      operation: "emulatorAdmission",
      signedBytes: CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
      maxTxSize: CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
      withinMaxTxSize: true,
      confirmed: true,
      admittedOutputCount: 1,
      emulatorReturnedExactDatum: true,
      returnedDatumBytes: cardanoSignedCapacityCandidate.acceptedDatumCborBytes,
    });
    if (process.env.MIDGARD_PRINT_C26_ADMISSION === "1") {
      console.info(
        JSON.stringify({ maximumDepthEmulatorAdmissionV1: admission }),
      );
    }
  }, 300_000);
});
