import {
  advanceMidgardCekDataTraverse,
  buildMidgardLedgerOutputProofTrace,
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardTxOutput,
  encodeMidgardCekDataFrame,
  encodeMidgardCekDataTraverseControl,
  finalizeMidgardCekDataTraverse,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN,
  midgardNativeTxFullToCardanoTxEncoding,
  nextMidgardCekDataTraverseSpan,
  validateMidgardConsensusTx,
} from "@al-ft/midgard-core";
import { CML, Emulator } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildSignedCardanoNestedDatumCandidate,
  CARDANO_BOUNDARY_MAX_TX_SIZE,
  cardanoBoundaryNestedDataCbor,
  deterministicCardanoBoundaryPrivateKey,
  exerciseMidgardOrderedCollectionBoundary,
  findSignedCardanoCollectionBoundary,
  measureSignedCardanoNestedDatum,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
} from "./helpers/ordered-collection-boundary.js";
import { exerciseMidgardRetainedDaBoundary } from "./helpers/retained-da-boundary.js";

const hex = (bytes: Uint8Array): string => Buffer.from(bytes).toString("hex");

const jsonDataSummary = (summary: {
  readonly root: Uint8Array;
  readonly cborLength: bigint;
  readonly memory: bigint;
}) => ({
  rootHex: hex(summary.root),
  cborLength: summary.cborLength.toString(),
  memory: summary.memory.toString(),
});

const jsonDataFrame = (
  frame: Parameters<typeof encodeMidgardCekDataFrame>[0],
) => ({
  cborHex: encodeMidgardCekDataFrame(frame).toString("hex"),
  kind: frame.kind,
  ...(frame.kind === "constrSmall"
    ? { constructor: frame.constructor.toString() }
    : frame.kind === "constrLarge"
      ? {
          constructorCborRootHex: hex(frame.constructorCborRoot),
          constructorCborLength: frame.constructorCborLength.toString(),
          constructorMemory: frame.constructorMemory.toString(),
        }
      : {}),
  tailHex: hex(frame.tail),
  expectedChildren: frame.expectedChildren,
  childCount: frame.childCount,
  childPeaks: frame.childFrontier.peaks.map((peak) => ({
    height: peak.height,
    hashHex: hex(peak.hash),
  })),
  foldCursor: frame.foldCursor,
  sequence: {
    rootHex: hex(frame.sequence.root),
    length: frame.sequence.length.toString(),
    payloadCborLength: frame.sequence.payloadCborLength.toString(),
    memory: frame.sequence.memory.toString(),
  },
});

// The exact genuine signed-Cardano nested-datum boundary. The terminal vector
// below carries the same numbers, but its comparison is skipped while an Aiken
// vector is being regenerated; these four pins are unconditional, so a silently
// shrunk datum can no longer satisfy the relative bounds alone.
const MAXIMUM_NESTED_DATA_ACCEPTED_LEAF_COUNT = 5_387;
const MAXIMUM_NESTED_DATA_ACCEPTED_SIGNED_BYTES = 16_382;
const MAXIMUM_NESTED_DATA_ADJACENT_LEAF_COUNT = 5_388;
const MAXIMUM_NESTED_DATA_ADJACENT_SIGNED_BYTES = 16_385;
const MAXIMUM_NESTED_DATA_DATUM_TRAVERSE_STEP_COUNT = 129_311;

const maximumNestedDataTerminalVector = {
  maxTxSize: 16_384,
  nestedLeafCount: 5_387,
  dataNodeCount: 10_776,
  datumCborBytes: 16_171,
  signedCardanoBytes: 16_382,
  signedCardanoByteMargin: 2,
  adjacentLeafCount: 5_388,
  adjacentDatumCborBytes: 16_174,
  adjacentSignedCardanoBytes: 16_385,
  nativeCanonicalBytes: 16_470,
  outputItemBytes: 16_220,
  outputProofSteps: 129_324,
  datumTraverseSteps: 129_311,
  maximumSourceSpan: 14,
  terminalPreControlCborHex:
    "8a01061831193f2b193f2b582064c916e4a790b0d133bb36d04d9e9ecca7dc2f3c679690d2d005f84b311e9d94d87a80d87a80d87a80d87a80",
  terminalFrameCborHex:
    "8b010058203ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf020640010181820058209f62b20d5db17ead31389c3864fb7ea3a2f68726b21ef48452c670dab47a8a6601845820f64559d8fa739e5dec6e218602ff2ebd0d24b477421f38b1872a2274454f84c701193f2419bd67",
  terminalPostControlCborHex:
    "8a01071831193f2b193f2b40d87a80d87a80d87a80d8799f83582077156535ea7ff621233f808b4995b94294f504a0dd78455593440e3d03ad2b6f193f2b19bd6bff",
  terminalSummary: {
    rootHex: "77156535ea7ff621233f808b4995b94294f504a0dd78455593440e3d03ad2b6f",
    cborLength: "16171",
    memory: "48491",
  },
} as const;

describe("canonical V1 nested Cardano Data boundary", () => {
  it("retains and traverses the maximum balanced constructor/list/map datum", async () => {
    const privateKey = deterministicCardanoBoundaryPrivateKey(0);
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
      PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
    );
    const buildCandidate = (requestedNestedLeafCount: number) => {
      const nestedDatumCborHex = cardanoBoundaryNestedDataCbor(
        requestedNestedLeafCount,
      );
      return buildSignedCardanoNestedDatumCandidate({
        privateKeyBech32: funder.privateKey,
        inputTransactionId: "00".repeat(32),
        inputOutputIndex: 0n,
        inputLovelace: funder.assets.lovelace,
        recipientAddress: funder.address,
        requestedNestedLeafCount,
        nestedDatumCborHex,
        minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeA,
        minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeB,
        minFeeRefScriptCostPerByte:
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeRefScriptCostPerByte,
      });
    };
    const boundary = await findSignedCardanoCollectionBoundary({
      maxTxSize: emulator.protocolParameters.maxTxSize,
      buildSignedCandidate: buildCandidate,
    });
    const accepted = measureSignedCardanoNestedDatum(boundary.accepted.cborHex);
    const adjacent = measureSignedCardanoNestedDatum(boundary.adjacent.cborHex);
    const acceptedDatumCborHex = cardanoBoundaryNestedDataCbor(
      boundary.accepted.requestedItemCount,
    );
    const adjacentDatumCborHex = cardanoBoundaryNestedDataCbor(
      boundary.adjacent.requestedItemCount,
    );

    expect(boundary.accepted.signedBytes).toBeLessThanOrEqual(
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );
    expect(boundary.adjacent.signedBytes).toBeGreaterThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );
    expect(boundary.adjacent.requestedItemCount).toBe(
      boundary.accepted.requestedItemCount + 1,
    );

    // The genuine maximum and its immediately adjacent control are exact, not
    // merely "whatever the search returned".
    expect(boundary.accepted.requestedItemCount).toBe(
      MAXIMUM_NESTED_DATA_ACCEPTED_LEAF_COUNT,
    );
    expect(boundary.accepted.signedBytes).toBe(
      MAXIMUM_NESTED_DATA_ACCEPTED_SIGNED_BYTES,
    );
    expect(boundary.adjacent.requestedItemCount).toBe(
      MAXIMUM_NESTED_DATA_ADJACENT_LEAF_COUNT,
    );
    expect(boundary.adjacent.signedBytes).toBe(
      MAXIMUM_NESTED_DATA_ADJACENT_SIGNED_BYTES,
    );
    expect(accepted.datumCborHex).toBe(acceptedDatumCborHex);
    expect(adjacent.datumCborHex).toBe(adjacentDatumCborHex);
    expect(accepted.datumCborBytes).toBe(
      boundary.accepted.requestedItemCount * 3 + 10,
    );
    expect(adjacent.datumCborBytes).toBe(accepted.datumCborBytes + 3);
    expect({
      outputCount: accepted.outputCount,
      vkeyWitnessCount: accepted.vkeyWitnessCount,
      outputAddress: accepted.outputAddress,
      hasWithdrawals: accepted.hasWithdrawals,
      hasMint: accepted.hasMint,
      hasPlutusScripts: accepted.hasPlutusScripts,
      hasRedeemers: accepted.hasRedeemers,
      collateralInputCount: accepted.collateralInputCount,
    }).toEqual({
      outputCount: 1,
      vkeyWitnessCount: 1,
      outputAddress: funder.address,
      hasWithdrawals: false,
      hasMint: false,
      hasPlutusScripts: false,
      hasRedeemers: false,
      collateralInputCount: 0,
    });

    const canonical = cardanoTxBytesToMidgardNativeTxCanonicalCbor(
      Buffer.from(boundary.accepted.cborHex, "hex"),
    );
    const native = decodeMidgardNativeTxFullFromCanonicalCbor(canonical);
    expect(validateMidgardConsensusTx(native, canonical.length)).toBeNull();
    const outputCbors = decodeMidgardNativeByteListPreimage(
      native.body.outputsPreimageCbor,
      "native.outputs",
    );
    expect(outputCbors).toHaveLength(1);
    const output = decodeMidgardTxOutput(outputCbors[0]!);
    expect(output.datum?.cbor.toString("hex")).toBe(acceptedDatumCborHex);

    const outputProof = buildMidgardLedgerOutputProofTrace({
      outputIndex: 0,
      outputCbor: outputCbors[0]!,
    });
    const datumSteps = outputProof.steps.filter(
      ({ control, witness, next }) =>
        control.datum !== null &&
        witness?.kind === "datum" &&
        next.datum !== null,
    );
    expect(datumSteps.length).toBeGreaterThan(
      boundary.accepted.requestedItemCount,
    );
    expect(datumSteps.length).toBe(
      MAXIMUM_NESTED_DATA_DATUM_TRAVERSE_STEP_COUNT,
    );
    expect(
      datumSteps.map(({ witness }) =>
        witness!.kind === "datum" ? witness!.action?.kind : undefined,
      ),
    ).toEqual(
      expect.arrayContaining([
        "headLargeConstructor",
        "headSequence",
        "headMap",
        "headScalar",
        "foldList",
        "foldMap",
        "finalizeFrame",
      ]),
    );
    const maximumSourceSpan = datumSteps.reduce((maximum, { control }) => {
      const span = nextMidgardCekDataTraverseSpan(control.datum!);
      return Math.max(maximum, span?.length ?? 0);
    }, 0);
    expect(maximumSourceSpan).toBeLessThanOrEqual(
      MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN,
    );
    const maximumChunkBytes = datumSteps.reduce(
      (maximum, { witness }) =>
        witness!.kind === "datum"
          ? Math.max(
              maximum,
              witness!.chunkProof?.chunk.length ?? 0,
              witness!.nextChunkProof?.chunk.length ?? 0,
            )
          : maximum,
      0,
    );
    expect(maximumChunkBytes).toBeLessThanOrEqual(
      MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
    );
    const finalDatumStep = datumSteps.at(-1)!;
    const finalAction =
      finalDatumStep.witness!.kind === "datum"
        ? finalDatumStep.witness!.action
        : null;
    expect(finalAction?.kind).toBe("finalizeFrame");
    if (finalAction?.kind !== "finalizeFrame") {
      throw new Error("Maximum nested datum lost its terminal frame");
    }
    const terminalSummary = finalizeMidgardCekDataTraverse(
      finalDatumStep.next.datum!,
    );
    expect(terminalSummary).not.toBeNull();
    expect(terminalSummary!.cborLength).toBe(BigInt(accepted.datumCborBytes));
    const terminalVector = {
      maxTxSize: CARDANO_BOUNDARY_MAX_TX_SIZE,
      nestedLeafCount: boundary.accepted.requestedItemCount,
      dataNodeCount: boundary.accepted.requestedItemCount * 2 + 2,
      datumCborBytes: accepted.datumCborBytes,
      signedCardanoBytes: boundary.accepted.signedBytes,
      signedCardanoByteMargin:
        CARDANO_BOUNDARY_MAX_TX_SIZE - boundary.accepted.signedBytes,
      adjacentLeafCount: boundary.adjacent.requestedItemCount,
      adjacentDatumCborBytes: adjacent.datumCborBytes,
      adjacentSignedCardanoBytes: boundary.adjacent.signedBytes,
      nativeCanonicalBytes: canonical.length,
      outputItemBytes: outputCbors[0]!.length,
      outputProofSteps: outputProof.steps.length,
      datumTraverseSteps: datumSteps.length,
      maximumSourceSpan,
      terminalPreControlCborHex: encodeMidgardCekDataTraverseControl(
        finalDatumStep.control.datum!,
      ).toString("hex"),
      terminalFrameCborHex: encodeMidgardCekDataFrame(
        finalAction.frame,
      ).toString("hex"),
      terminalPostControlCborHex: encodeMidgardCekDataTraverseControl(
        finalDatumStep.next.datum!,
      ).toString("hex"),
      terminalSummary: {
        rootHex: Buffer.from(terminalSummary!.root).toString("hex"),
        cborLength: terminalSummary!.cborLength.toString(),
        memory: terminalSummary!.memory.toString(),
      },
    };
    const representativeKinds = [
      "headLargeConstructor",
      "headSequence",
      "headMap",
      "headScalar",
      "foldList",
      "foldMap",
      "finalizeFrame",
    ] as const;
    const appliedActionVectors = representativeKinds.map((kind) => {
      const step = datumSteps.find(({ witness }) =>
        witness?.kind === "datum" ? witness.action?.kind === kind : false,
      );
      if (
        step === undefined ||
        step.witness?.kind !== "datum" ||
        step.witness.action === null ||
        step.control.datum === null ||
        step.next.datum === null
      ) {
        throw new Error(`Maximum nested datum lost applied ${kind} evidence`);
      }
      const { action } = step.witness;
      const span = nextMidgardCekDataTraverseSpan(step.control.datum);
      const sourceBytes =
        span === null
          ? null
          : Buffer.from(acceptedDatumCborHex, "hex").subarray(
              span.absoluteStart - step.control.datum.sourceStart,
              span.absoluteStart - step.control.datum.sourceStart + span.length,
            );
      expect(
        advanceMidgardCekDataTraverse({
          control: step.control.datum,
          sourceBytes,
          action,
        }),
      ).toEqual(step.next.datum);
      return {
        kind,
        preControlCborHex: encodeMidgardCekDataTraverseControl(
          step.control.datum,
        ).toString("hex"),
        sourceBytesHex:
          sourceBytes === null ? null : sourceBytes.toString("hex"),
        postControlCborHex: encodeMidgardCekDataTraverseControl(
          step.next.datum,
        ).toString("hex"),
        action:
          action.kind === "headLargeConstructor"
            ? {
                constructorCborLength: action.constructorCborLength,
                expectedChildren: action.expectedChildren,
              }
            : action.kind === "headSequence"
              ? {
                  expectedChildren: action.expectedChildren,
                }
              : action.kind === "headScalar"
                ? { itemLength: action.itemLength }
                : action.kind === "foldList"
                  ? {
                      frame: jsonDataFrame(action.frame),
                      childIndex: action.childIndex,
                      child: jsonDataSummary(action.child),
                      siblingHexes: action.siblings.map(hex),
                    }
                  : action.kind === "foldMap"
                    ? {
                        frame: jsonDataFrame(action.frame),
                        pairIndex: action.pairIndex,
                        key: jsonDataSummary(action.key),
                        value: jsonDataSummary(action.value),
                        keySiblingHexes: action.keySiblings.map(hex),
                        valueSiblingHexes: action.valueSiblings.map(hex),
                      }
                    : action.kind === "finalizeFrame"
                      ? {
                          frame: jsonDataFrame(action.frame),
                          parent:
                            action.parent === null
                              ? null
                              : jsonDataFrame(action.parent),
                        }
                      : {},
      };
    });
    if (process.env.MIDGARD_PRINT_AIKEN_VECTOR !== "1") {
      expect(terminalVector).toEqual(maximumNestedDataTerminalVector);
    }

    const field = exerciseMidgardOrderedCollectionBoundary({
      signedCardanoCborHex: boundary.accepted.cborHex,
      fieldIndex: 2,
    });
    expect(field.itemCount).toBe(1);
    const retained = await exerciseMidgardRetainedDaBoundary({
      signedCardanoCborHex: boundary.accepted.cborHex,
      corpusLabel: "balanced-nested-datum",
    });
    expect(retained.normal.reconstructedCanonicalBytes).toBe(canonical.length);
    expect(retained.forced.reconstructedCanonicalBytes).toBe(canonical.length);
    expect(retained.normal.revealStepCount).toBe(field.completeFoldStepCount);
    expect(retained.forced.revealStepCount).toBe(field.completeFoldStepCount);

    const roundTrip = measureSignedCardanoNestedDatum(
      Buffer.from(midgardNativeTxFullToCardanoTxEncoding(native)).toString(
        "hex",
      ),
    );
    expect({
      outputCount: roundTrip.outputCount,
      outputAddress: roundTrip.outputAddress,
      outputLovelace: roundTrip.outputLovelace,
      datumCborHex: roundTrip.datumCborHex,
    }).toEqual({
      outputCount: accepted.outputCount,
      outputAddress: accepted.outputAddress,
      outputLovelace: accepted.outputLovelace,
      datumCborHex: accepted.datumCborHex,
    });

    const txHash = await emulator.submitTx(boundary.accepted.cborHex);
    await expect(emulator.awaitTx(txHash)).resolves.toBe(true);

    if (process.env.MIDGARD_PRINT_AIKEN_VECTOR === "1") {
      console.info(
        JSON.stringify({
          nestedDataBoundaryV1: terminalVector,
          nestedDataAppliedActionVectorsV1: appliedActionVectors,
        }),
      );
    }
  }, 300_000);
});
