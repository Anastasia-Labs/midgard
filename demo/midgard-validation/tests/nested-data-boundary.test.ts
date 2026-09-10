import {
  advanceMidgardCekDataTraverse,
  buildMidgardCekDataTraverseTrace,
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

import { publishAikenVector } from "./helpers/aiken-vector-channel.js";
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

// The exact genuine signed-Cardano nested-datum boundary. These four pins are
// the cardinality and byte count the search must land on, so a silently shrunk
// datum can no longer satisfy the relative bounds alone.
const MAXIMUM_NESTED_DATA_ACCEPTED_LEAF_COUNT = 5_387;
const MAXIMUM_NESTED_DATA_ACCEPTED_SIGNED_BYTES = 16_382;
const MAXIMUM_NESTED_DATA_ADJACENT_LEAF_COUNT = 5_388;
const MAXIMUM_NESTED_DATA_ADJACENT_SIGNED_BYTES = 16_385;

/**
 * A closed-form reference model for the number of traverse steps a balanced
 * `cardanoBoundaryNestedDataCbor` datum owes.
 *
 * The builder emits one balanced binary tree of indefinite lists over
 * `leafCount` leaves inside a fixed outer frame, so the traversal's step count
 * is a linear function of the leaf count rather than a measurement: every leaf
 * costs the same fixed run of head/fold/finalize transitions, and the outer
 * `d866 82 1880 9f a1 d87980 … ff` framing costs a constant. The model is
 * checked below against the real producer at small leaf counts, and the
 * boundary datum is then required to satisfy it — which is what a traversal
 * that stopped early, or that grew a step per byte, would fail.
 */
const NESTED_DATA_TRAVERSE_STEPS_PER_LEAF = 24;
const NESTED_DATA_TRAVERSE_FRAMING_STEPS = 23;
const balancedNestedDataTraverseSteps = (leafCount: number): number =>
  NESTED_DATA_TRAVERSE_STEPS_PER_LEAF * leafCount +
  NESTED_DATA_TRAVERSE_FRAMING_STEPS;

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
    // The reference model is checked against the real traversal at leaf counts
    // small enough to read, and only then applied to the boundary datum.
    for (const smallLeafCount of [1, 2, 5, 50]) {
      const smallTrace = buildMidgardCekDataTraverseTrace({
        sourceStart: 0,
        source: Buffer.from(
          cardanoBoundaryNestedDataCbor(smallLeafCount),
          "hex",
        ),
      });
      expect(smallTrace.steps).toHaveLength(
        balancedNestedDataTraverseSteps(smallLeafCount),
      );
    }
    expect(datumSteps.length).toBe(
      balancedNestedDataTraverseSteps(boundary.accepted.requestedItemCount),
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
          ? Math.max(maximum, witness!.window?.length ?? 0)
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
    // The Aiken twin's `maximum_cardano_nested_data_*` constants are rebound
    // from this vector by
    // `scripts/generate-nested-boundary-aiken-goldens.mjs`, whose `--check` run
    // is a required CI job. Publishing happens after every assertion above, so
    // the generator can only ever see a vector this suite has already accepted,
    // and no environment variable can remove an assertion.
    publishAikenVector("nested-data-boundary-v1", {
      ...terminalVector,
      // The frame's own sequence root, which the Aiken twin needs to rebuild
      // the terminal frame it steps over.
      terminalFrameSequenceRootHex: hex(finalAction.frame.sequence.root),
      appliedActions: appliedActionVectors,
    });

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
  }, 300_000);
});
