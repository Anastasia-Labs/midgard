import {
  assertMidgardPlutusDataWellFormedV1,
  buildMidgardCekDataTraverseTraceV1,
  cardanoTxBytesToMidgardNativeTxCanonicalCborV1,
  computeHash32,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardTxOutput,
  deriveMidgardV1TxFieldChunks,
  encodeCbor,
  encodeMidgardCekDataFrameV1,
  encodeMidgardCekDataTraverseControlV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardTxOutput,
  finalizeMidgardCekDataTraverseV1,
  materializeMidgardNativeTxFromCanonicalV1,
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
import {
  exerciseMidgardRetainedDaBoundaryV1,
  exerciseMidgardRetainedDaCanonicalBoundaryV1,
} from "./helpers/retained-da-boundary-v1.js";
import {
  buildRawSignedCardanoUnaryCandidateV1,
  cardanoUnaryConstructorDataCborV1,
  measureExactUnaryConstructorDataV1,
  type RawSignedCardanoUnaryCandidateV1,
} from "./helpers/unary-depth-candidate-v1.js";

const productionRuntimeUnaryDepthWitnessV1 = 1_024;

// The exact genuine signed-Cardano unary-depth boundary. The terminal vector
// below carries the same numbers inside one large object comparison; these four
// pins state the cardinality and byte count directly at the search site, so a
// silently shrunk depth can no longer satisfy the relative bounds alone.
const MAXIMUM_UNARY_DEPTH_ACCEPTED_COUNT_V1 = 4_043;
const MAXIMUM_UNARY_DEPTH_ACCEPTED_SIGNED_BYTES_V1 = 16_384;
const MAXIMUM_UNARY_DEPTH_ADJACENT_COUNT_V1 = 4_044;
const MAXIMUM_UNARY_DEPTH_ADJACENT_SIGNED_BYTES_V1 = 16_388;

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

    // The genuine maximum and its immediately adjacent control are exact, not
    // merely "whatever the search returned".
    expect(acceptedShape.depth).toBe(MAXIMUM_UNARY_DEPTH_ACCEPTED_COUNT_V1);
    expect(accepted.signedBytes).toBe(
      MAXIMUM_UNARY_DEPTH_ACCEPTED_SIGNED_BYTES_V1,
    );
    expect(adjacentShape.depth).toBe(MAXIMUM_UNARY_DEPTH_ADJACENT_COUNT_V1);
    expect(adjacent.signedBytes).toBe(
      MAXIMUM_UNARY_DEPTH_ADJACENT_SIGNED_BYTES_V1,
    );

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
   * Retained DA reconstruction at the exact derived maximum, not just at the
   * 1,024 production-runtime witness above.
   *
   * Two constraints shape this test, both third-party and both measured:
   *
   *  1. CML cannot *build* a 4,043-deep `PlutusData` in-process, so the signed
   *     candidate comes from the raw signer, exactly as the emulator-admission
   *     test below does.
   *  2. Maximum-depth `CML.Transaction.from_cbor_bytes` is effectively one-shot
   *     per process even with the patched shadow stack: the first call in a
   *     fresh worker succeeds, and a second one — or a first one in a worker
   *     where any earlier test already drove CML through a deep datum — fails
   *     with `RangeError: Maximum call stack size exceeded`. That is the same
   *     property `helpers/cml-max-depth-runner-v1.ts` exists for, so the
   *     signed-Cardano-hex entry `exerciseMidgardRetainedDaBoundaryV1` cannot
   *     be used here without making the suite order-dependent.
   *
   * The canonical maximum-shape transaction is therefore projected through the
   * repo's own recursion-free codecs — the shallow candidate goes through the
   * production Cardano bridge and the maximum-depth datum is substituted into
   * its single output — and handed to the canonical retained-DA entry point.
   * That is the same exercise `exerciseMidgardRetainedDaBoundaryV1` delegates
   * to (envelope, SDK decode, both DA classifications, every bounded reveal and
   * the terminal reconstruction fold); only the CML-bound conversion in front
   * of it is replaced. It matches the depth-4,043 measurement in
   * `docs/exec-plans/evidence/c26-cml-investigation.md`, which was likewise
   * taken with stock CML through the repo codecs.
   *
   * Emulator admission stays in the child-process test below.
   */
  it("retains the exact maximum-depth witness through normal and forced paths", async () => {
    const privateKey = deterministicCardanoBoundaryPrivateKeyV1(0);
    const { acceptedDepth, acceptedDatumCborBytes } =
      maximumUnaryDepthTerminalVectorV1.cardanoSignedCapacityCandidate;
    const address = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(privateKey.to_public().hash()),
    )
      .to_address()
      .to_bech32();
    const buildCandidate = (requestedDepth: number) =>
      buildRawSignedCardanoUnaryCandidateV1({
        privateKey,
        inputTransactionId: "00".repeat(32),
        inputLovelace: 40_000_000_000n,
        recipientAddress: address,
        requestedDepth,
        minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
        minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
      });

    // The genuine signed candidate at the derived maximum: exactly on the
    // signed capacity, unlike the 1,024 witness above which is strictly inside
    // it. Built and measured without asking CML to read the deep datum.
    const maximumCandidate = buildCandidate(acceptedDepth);
    const maximumDatumCborHex =
      cardanoUnaryConstructorDataCborV1(acceptedDepth);
    expect(maximumCandidate.datumCbor.toString("hex")).toBe(
      maximumDatumCborHex,
    );
    expect(maximumCandidate.datumCbor.length).toBe(acceptedDatumCborBytes);
    expect(maximumCandidate.signedBytes).toBe(CARDANO_BOUNDARY_MAX_TX_SIZE_V1);
    expect(measureExactUnaryConstructorDataV1(maximumDatumCborHex).depth).toBe(
      acceptedDepth,
    );

    const shallowCanonical = cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
      Buffer.from(buildCandidate(1).cborHex, "hex"),
    );
    const shallowNative =
      decodeMidgardNativeTxFullV1FromCanonicalCbor(shallowCanonical);
    const shallowOutputs = decodeMidgardNativeByteListPreimage(
      shallowNative.body.outputsPreimageCbor,
      "native.outputs",
    );
    expect(shallowOutputs).toHaveLength(1);
    const shallowOutput = decodeMidgardTxOutput(shallowOutputs[0]!);
    expect(shallowOutput.datum?.cbor.toString("hex")).toBe(
      cardanoUnaryConstructorDataCborV1(1),
    );
    const canonical = encodeMidgardNativeTxCanonicalV1(
      materializeMidgardNativeTxFromCanonicalV1({
        version: shallowNative.version,
        validity: shallowNative.validity,
        body: {
          ...shallowNative.body,
          outputsPreimageCbor: encodeCbor([
            encodeMidgardTxOutput({
              ...shallowOutput,
              datum: { kind: "inline", cbor: maximumCandidate.datumCbor },
            }),
          ]),
        },
        witnessSet: shallowNative.witnessSet,
      }),
    );

    const native = decodeMidgardNativeTxFullV1FromCanonicalCbor(canonical);
    expect(validateMidgardConsensusV1Tx(native, canonical.length)).toBeNull();
    const outputs = decodeMidgardNativeByteListPreimage(
      native.body.outputsPreimageCbor,
      "native.outputs",
    );
    expect(outputs).toHaveLength(1);
    expect(decodeMidgardTxOutput(outputs[0]!).datum?.cbor.toString("hex")).toBe(
      maximumDatumCborHex,
    );
    // Canonical maximum-shape byte count pinned by the C26 investigation
    // record's depth-4,043 row.
    expect(canonical.length).toBe(16_470);
    const completeFoldStepCount =
      deriveMidgardV1TxFieldChunks(canonical).length;
    expect(completeFoldStepCount).toBe(6);

    const retained = await exerciseMidgardRetainedDaCanonicalBoundaryV1({
      canonicalTransactionCbor: canonical,
    });
    expect(retained.normal.sourceKind).toBe("normal");
    expect(retained.forced.sourceKind).toBe("forced");
    expect(retained.normal.retainedPreimageBytes).toBe(canonical.length);
    expect(retained.forced.retainedPreimageBytes).toBe(canonical.length);
    expect(retained.normal.reconstructedCanonicalBytes).toBe(canonical.length);
    expect(retained.forced.reconstructedCanonicalBytes).toBe(canonical.length);
    expect(retained.normal.revealStepCount).toBe(completeFoldStepCount);
    expect(retained.forced.revealStepCount).toBe(completeFoldStepCount);
    // Canonical maximum signed-byte and digest identity, not only byte counts:
    // both retained classifications must store and rebuild the exact same
    // canonical bytes, with the same transaction identity and commitment.
    const canonicalDigestHex = computeHash32(canonical).toString("hex");
    expect({
      normalRetained: retained.normal.retainedPreimageDigestHex,
      normalReconstructed: retained.normal.reconstructedCanonicalDigestHex,
      forcedRetained: retained.forced.retainedPreimageDigestHex,
      forcedReconstructed: retained.forced.reconstructedCanonicalDigestHex,
    }).toEqual({
      normalRetained: canonicalDigestHex,
      normalReconstructed: canonicalDigestHex,
      forcedRetained: canonicalDigestHex,
      forcedReconstructed: canonicalDigestHex,
    });
    expect(retained.normal.transactionIdHex).toBe(
      retained.forced.transactionIdHex,
    );
    expect(retained.normal.transactionCommitmentHex).toBe(
      retained.forced.transactionCommitmentHex,
    );
    expect(retained.normal.transactionIdHex).toBe(retained.transactionIdHex);
  }, 300_000);

  /**
   * Focused malformed and noncanonical controls at the exact maximum depth.
   *
   * The boundary above is only meaningful if the accepted shape is the *only*
   * accepted shape at that size. Every mutation here keeps the depth-4,043
   * envelope and breaks exactly one structural property, and each must be
   * refused by both the production iterative well-formedness gate and the
   * exact unary measurement — no CML recursion involved on either side.
   */
  it("rejects malformed and noncanonical unary Data at the exact maximum depth", () => {
    const { acceptedDepth, acceptedDatumCborBytes } =
      maximumUnaryDepthTerminalVectorV1.cardanoSignedCapacityCandidate;
    const acceptedHex = cardanoUnaryConstructorDataCborV1(acceptedDepth);
    const accepted = Buffer.from(acceptedHex, "hex");
    expect(accepted.length).toBe(acceptedDatumCborBytes);
    // Control: the accepted maximum passes both gates.
    expect(() => {
      assertMidgardPlutusDataWellFormedV1(accepted);
    }).not.toThrow();
    expect(measureExactUnaryConstructorDataV1(acceptedHex)).toEqual({
      depth: acceptedDepth,
      nodeCount: acceptedDepth + 1,
      scalarCount: 1,
    });

    const controls = [
      {
        label: "truncated break markers",
        hex: acceptedHex.slice(0, acceptedHex.length - 2),
        wellFormed: false,
      },
      {
        label: "missing innermost leaf",
        hex: "d8799f".repeat(acceptedDepth) + "ff".repeat(acceptedDepth),
        wellFormed: true,
      },
      {
        label: "trailing byte after the closing sequence",
        hex: `${acceptedHex}00`,
        wellFormed: false,
      },
      {
        label: "extra break marker",
        hex: `${acceptedHex}ff`,
        wellFormed: false,
      },
      {
        label: "two children in the innermost constructor",
        hex:
          "d8799f".repeat(acceptedDepth) + "0000" + "ff".repeat(acceptedDepth),
        wellFormed: true,
      },
      {
        label: "noncanonical definite-length constructor body",
        hex:
          "d8799f".repeat(acceptedDepth - 1) +
          "d87981" +
          "00" +
          "ff".repeat(acceptedDepth - 1),
        wellFormed: true,
      },
      {
        label: "noncanonical bytestring leaf in place of integer zero",
        hex:
          "d8799f".repeat(acceptedDepth) + "4100" + "ff".repeat(acceptedDepth),
        wellFormed: true,
      },
    ] as const;

    for (const control of controls) {
      expect(control.hex).not.toBe(acceptedHex);
      // The exact unary measurement must refuse every one of them: it is the
      // predicate the boundary claim is stated in.
      expect(
        () => measureExactUnaryConstructorDataV1(control.hex),
        control.label,
      ).toThrow();
      if (!control.wellFormed) {
        expect(() => {
          assertMidgardPlutusDataWellFormedV1(Buffer.from(control.hex, "hex"));
        }, control.label).toThrow();
      }
    }

    // The adjacent depth is well formed and exactly unary — it is rejected by
    // the signed byte count, never by structure. Recorded so the malformed
    // controls above are not confused with the capacity boundary.
    const adjacentHex = cardanoUnaryConstructorDataCborV1(acceptedDepth + 1);
    expect(() => {
      assertMidgardPlutusDataWellFormedV1(Buffer.from(adjacentHex, "hex"));
    }).not.toThrow();
    expect(measureExactUnaryConstructorDataV1(adjacentHex).depth).toBe(
      acceptedDepth + 1,
    );
  }, 120_000);

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
