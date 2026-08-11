import { readFileSync } from "node:fs";

import {
  assertMidgardPlutusDataWellFormedV1,
  buildMidgardCekDataTraverseTraceV1,
  cardanoTxBytesToMidgardNativeTxCanonicalCborV1,
  computeHash32,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardTxOutput,
  deriveMidgardV1TxFieldPreimages,
  encodeCbor,
  encodeMidgardCekDataFrameV1,
  encodeMidgardCekDataTraverseControlV1,
  encodeMidgardFieldPreimageForFieldV1,
  encodeMidgardNativeTxCanonicalV1,
  encodeMidgardTxOutput,
  finalizeMidgardCekDataTraverseV1,
  materializeMidgardNativeTxFromCanonicalV1,
  MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN_V1,
  nextMidgardCekDataTraverseSpanV1,
  validateMidgardConsensusV1Tx,
} from "@al-ft/midgard-core";
import {
  applyDoubleCborEncoding,
  CML,
  Data,
  Emulator,
  Lucid,
  type SpendingValidator,
  validatorToAddress,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { countedMachineTransactionChunkStepsV1 } from "../src/validation-machine.js";
import { runMaxDepthCmlOperationV1 } from "./helpers/cml-max-depth-runner-v1.js";
import {
  buildCollateralFreeMidgardSchemaParallelCandidateV1,
  buildSignedCardanoNestedDatumCandidateV1,
  buildSignedCardanoSpendRedeemersCandidateV1,
  CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
  CARDANO_BOUNDARY_TOTAL_COLLATERAL_V1,
  deterministicCardanoBoundaryPrivateKeyV1,
  exerciseMidgardOrderedCollectionBoundaryV1,
  findSignedCardanoCollectionBoundaryV1,
  measureCollateralizedPlutusFeasibilityCandidateV1,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
} from "./helpers/ordered-collection-boundary-v1.js";
import {
  buildMidgardRetainedDaCanonicalScriptProjectionV1,
  exerciseMidgardRetainedDaBoundaryV1,
  exerciseMidgardRetainedDaCanonicalBoundaryV1,
} from "./helpers/retained-da-boundary-v1.js";
import {
  buildRawSignedCardanoUnaryCandidateV1,
  buildRawSignedCardanoUnaryRedeemersCandidateV1,
  cardanoUnaryConstructorDataCborV1,
  measureExactUnaryConstructorDataV1,
  type RawSignedCardanoUnaryCandidateV1,
  type RawSignedCardanoUnaryRedeemerCandidateV1,
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

// The exact genuine signed-Cardano *redeemer* unary-depth boundary. Field 8
// carries strictly less unary depth than the inline datum above because the
// spend-redeemer envelope (script witness, redeemer pointer and execution
// units, collateral input/return/total, and the script-data hash) is larger
// than an output's datum option.
const MAXIMUM_UNARY_REDEEMER_DEPTH_ACCEPTED_COUNT_V1 = 3_995;
const MAXIMUM_UNARY_REDEEMER_DEPTH_ACCEPTED_SIGNED_BYTES_V1 = 16_381;
const MAXIMUM_UNARY_REDEEMER_DEPTH_ADJACENT_COUNT_V1 = 3_996;
const MAXIMUM_UNARY_REDEEMER_DEPTH_ADJACENT_SIGNED_BYTES_V1 = 16_385;

const maximumUnaryRedeemerDepthTerminalVectorV1 = {
  maxTxSize: 16384,
  cardanoSignedCapacityCandidate: {
    acceptedDepth: 3995,
    acceptedRedeemerDataCborBytes: 15981,
    acceptedSignedCardanoBytes: 16381,
    signedCardanoByteMargin: 3,
    adjacentDepth: 3996,
    adjacentRedeemerDataCborBytes: 15985,
    adjacentSignedCardanoBytes: 16385,
  },
  midgardProjection: {
    dataNodeCount: 3996,
    traverseSteps: 15999,
    maximumSourceSpan: 14,
    sourceCanonicalTransactionBytes: 16356,
    canonicalTransactionBytes: 16282,
    redeemerFieldBytes: 16000,
    redeemerFieldChunkCount: 4,
    completeFoldStepCount: 8,
    terminalPreControlCborHex:
      "8a010600193e6d193e6d582023fabfae62d51cba8147f76aef6c613f4f3525e8257361873c678e77aa750929d87a80d87a80d87a80d87a80",
    terminalFrameCborHex:
      "8b000040000040010181820058202e255919cf99b2743582ee389fb462ccb7562cf0a614647a01d9ce9fb14000bc0184582003a6eed9be7ce3bf1e01f104842a3b8fc33bfb24f5941e2fc84019a4bdce9c2001193e69193e6d",
    terminalPostControlCborHex:
      "8a010700193e6d193e6d40d87a80d87a80d87a80d8799f8358207102b7fc9525a54adf2b32de87dfc544c46f2cf275bd45bfa794dbb518f4fa1e193e6d193e71ff",
    terminalSummary: {
      rootHex:
        "7102b7fc9525a54adf2b32de87dfc544c46f2cf275bd45bfa794dbb518f4fa1e",
      cborLength: "15981",
      memory: "15985",
    },
  },
} as const;

type AlwaysSucceedsBlueprintV1 = {
  readonly validators: readonly {
    readonly title: string;
    readonly compiledCode: string;
  }[];
};

const alwaysSucceedsCompiledCodeV1 = (
  JSON.parse(
    readFileSync(
      new URL(
        "../../midgard-node/blueprints/always-succeeds/plutus.json",
        import.meta.url,
      ),
      "utf8",
    ),
  ) as AlwaysSucceedsBlueprintV1
).validators.find(
  (validator) => validator.title === "midgard.deposit_spend.else",
)?.compiledCode;
if (alwaysSucceedsCompiledCodeV1 === undefined) {
  throw new Error(
    "Missing always-succeeds blueprint entry midgard.deposit_spend.else",
  );
}
const unaryRedeemerSpendingScriptV1: SpendingValidator = {
  type: "PlutusV3",
  script: applyDoubleCborEncoding(alwaysSucceedsCompiledCodeV1),
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
    // record's depth-4,043 row, re-measured for §5.3's fixed 38-byte
    // spend-input item (one input, so two bytes wider than the depth-4,043 row
    // measured against CML's minimal-index out-ref CBOR).
    expect(canonical.length).toBe(16_472);
    const completeFoldStepCount =
      countedMachineTransactionChunkStepsV1(canonical).length;
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

  /**
   * The genuine field-8 unary *redeemer* maximum.
   *
   * The inline-datum maximum above says nothing about the redeemer carrier: a
   * spend redeemer pays for a Plutus script witness, a redeemer pointer with
   * execution units, collateral input/return/total, and a script-data hash, so
   * its unary capacity has to be derived separately.
   *
   * `buildSignedCardanoSpendRedeemersCandidateV1` cannot reach that maximum. It
   * materializes the redeemer through `CML.PlutusData.from_cbor_hex` and
   * derives the script-data hash through `CML.calc_script_data_hash`, and both
   * recurse over the Data tree. `buildRawSignedCardanoUnaryRedeemersCandidateV1`
   * assembles the identical Conway transaction byte by byte — including the
   * `blake2b-256(redeemers || language_views)` script-data hash — and keeps the
   * redeemer Data opaque. Depth one is pinned byte-for-byte against the CML
   * builder here, so "the same transaction, only deeper" is measured rather
   * than assumed, and the only remaining limit is the signed byte count.
   *
   * The Midgard projection uses the same recursion-free substitution the
   * maximum-depth datum test uses, for the same measured reason: the production
   * bridge parses through CML. The depth-one collateral-free parallel candidate
   * goes through the real bridge, its field-8 redeemer preimage is rebuilt with
   * the maximum-depth Data — with the depth-one rebuild pinned byte-identical
   * to the bridge's own output first — and the canonical transaction is handed
   * to the canonical retained-DA entry point unchanged.
   */
  it("derives the genuine field-8 unary redeemer maximum and retains it through normal and forced paths", async () => {
    const privateKey = deterministicCardanoBoundaryPrivateKeyV1(0);
    const walletAddress = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(privateKey.to_public().hash()),
    )
      .to_address()
      .to_bech32();
    const scriptAddress = validatorToAddress(
      "Custom",
      unaryRedeemerSpendingScriptV1,
    );
    const walletLovelace = 1_000_000_000_000n;
    const emulator = new Emulator(
      [
        {
          seedPhrase: "",
          privateKey: privateKey.to_bech32(),
          address: walletAddress,
          assets: { lovelace: walletLovelace },
        },
        {
          seedPhrase: "",
          privateKey: privateKey.to_bech32(),
          address: walletAddress,
          assets: { lovelace: walletLovelace },
        },
        {
          seedPhrase: "",
          privateKey: "",
          address: scriptAddress,
          assets: { lovelace: 10_000_000n },
          outputData: { inline: Data.void() },
        },
      ],
      PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
    );
    const walletInputs = (await emulator.getUtxos(walletAddress)).sort(
      (left, right) => left.outputIndex - right.outputIndex,
    );
    const scriptInputs = await emulator.getUtxos(scriptAddress);
    expect(walletInputs).toHaveLength(2);
    expect(scriptInputs).toHaveLength(1);

    // A real on-chain script execution supplies the genuine execution units and
    // the exact script witness the candidates below carry.
    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromPrivateKey(privateKey.to_bech32());
    const completedSeed = await lucid
      .newTx()
      .collectFrom([walletInputs[0]!])
      .collectFrom([scriptInputs[0]!], Data.void())
      .pay.ToAddress(walletAddress, { lovelace: 10_000_000n })
      .attach.SpendingValidator(unaryRedeemerSpendingScriptV1)
      .complete({ localUPLCEval: true });
    const signedSeed = await completedSeed.sign.withWallet().complete();
    const seed = measureCollateralizedPlutusFeasibilityCandidateV1(
      signedSeed.toCBOR(),
    );
    const seedScripts = CML.Transaction.from_cbor_hex(signedSeed.toCBOR())
      .witness_set()
      .plutus_v3_scripts();
    expect(seedScripts?.len()).toBe(1);
    expect(seed.executionMemory).toBeGreaterThan(0n);
    expect(seed.executionSteps).toBeGreaterThan(0n);
    const plutusV3ScriptCborHex = seedScripts!.get(0).to_cbor_hex();

    // The single script input sorts after the key funding input, so it is
    // redeemer pointer index 1.
    const scriptInputIndex = 1;
    const spendInputs = [walletInputs[0]!, scriptInputs[0]!].map((utxo) => ({
      txHash: utxo.txHash,
      outputIndex: utxo.outputIndex,
      lovelace: utxo.assets.lovelace!,
    }));
    const buildRawCandidate = (requestedDepth: number) =>
      buildRawSignedCardanoUnaryRedeemersCandidateV1({
        privateKey,
        spendInputs,
        scriptInputIndex,
        collateralInput: {
          txHash: walletInputs[1]!.txHash,
          outputIndex: walletInputs[1]!.outputIndex,
          lovelace: walletInputs[1]!.assets.lovelace!,
        },
        recipientAddress: walletAddress,
        plutusV3ScriptCborHex,
        requestedDepth,
        executionMemory: seed.executionMemory,
        executionSteps: seed.executionSteps,
        totalCollateral: CARDANO_BOUNDARY_TOTAL_COLLATERAL_V1,
        minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
        minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
        priceMem: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.priceMem,
        priceStep: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.priceStep,
        plutusV3CostModel:
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.costModels.PlutusV3,
      });

    // Faithfulness of the raw builder, including its raw script-data hash:
    // depth one is byte-identical to the CML-built candidate, fee included.
    const cmlDepthOne = await buildSignedCardanoSpendRedeemersCandidateV1({
      privateKeyBech32: privateKey.to_bech32(),
      feeFundingInput: walletInputs[0]!,
      collateralInput: walletInputs[1]!,
      availableScriptInputs: scriptInputs,
      recipientAddress: walletAddress,
      plutusV3ScriptCborHex,
      redeemerDataCborHex: cardanoUnaryConstructorDataCborV1(1),
      executionMemory: seed.executionMemory,
      executionSteps: seed.executionSteps,
      requestedRedeemerCount: 1,
      minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
      minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
      minFeeRefScriptCostPerByte:
        PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeRefScriptCostPerByte,
      priceMem: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.priceMem,
      priceStep: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.priceStep,
      collateralPercentage:
        PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.collateralPercentage,
      costModels: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.costModels,
    });
    const rawDepthOne = buildRawCandidate(1);
    expect(rawDepthOne.cborHex).toBe(cmlDepthOne.cborHex);
    expect(rawDepthOne.fee).toBe(cmlDepthOne.fee);

    const boundary = await findSignedCardanoCollectionBoundaryV1({
      maxTxSize: emulator.protocolParameters.maxTxSize,
      buildSignedCandidate: async (requestedDepth: number) =>
        buildRawCandidate(requestedDepth),
    });
    const accepted =
      boundary.accepted as RawSignedCardanoUnaryRedeemerCandidateV1;
    const adjacent =
      boundary.adjacent as RawSignedCardanoUnaryRedeemerCandidateV1;
    const acceptedShape = measureExactUnaryConstructorDataV1(
      accepted.redeemerDataCbor.toString("hex"),
    );
    const adjacentShape = measureExactUnaryConstructorDataV1(
      adjacent.redeemerDataCbor.toString("hex"),
    );

    expect(accepted.signedBytes).toBeLessThanOrEqual(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect(adjacent.signedBytes).toBeGreaterThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect(adjacentShape.depth).toBe(acceptedShape.depth + 1);
    expect(acceptedShape.depth).toBe(
      MAXIMUM_UNARY_REDEEMER_DEPTH_ACCEPTED_COUNT_V1,
    );
    expect(accepted.signedBytes).toBe(
      MAXIMUM_UNARY_REDEEMER_DEPTH_ACCEPTED_SIGNED_BYTES_V1,
    );
    expect(adjacentShape.depth).toBe(
      MAXIMUM_UNARY_REDEEMER_DEPTH_ADJACENT_COUNT_V1,
    );
    expect(adjacent.signedBytes).toBe(
      MAXIMUM_UNARY_REDEEMER_DEPTH_ADJACENT_SIGNED_BYTES_V1,
    );
    // The redeemer maximum is strictly shallower than the inline-datum maximum,
    // and it is bound by the signed byte count rather than by any recursion cap.
    expect(acceptedShape.depth).toBeLessThan(
      MAXIMUM_UNARY_DEPTH_ACCEPTED_COUNT_V1,
    );
    expect(accepted.scriptDataHash).toHaveLength(32);
    expect(accepted.redeemerDataCbor.length).toBe(acceptedShape.depth * 4 + 1);
    expect(adjacent.redeemerDataCbor.length).toBe(
      accepted.redeemerDataCbor.length + 4,
    );

    // Field-8 Midgard projection. The depth-one collateral-free parallel
    // candidate is produced by the same CML helper the nested-redeemer boundary
    // uses, so nothing about the Midgard schema shape is invented here.
    const parallelDepthOne =
      buildCollateralFreeMidgardSchemaParallelCandidateV1({
        collateralizedCardanoCborHex: rawDepthOne.cborHex,
        privateKeyBech32: privateKey.to_bech32(),
      });
    const shallowCanonical = cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
      Buffer.from(parallelDepthOne.cborHex, "hex"),
    );
    const shallowNative =
      decodeMidgardNativeTxFullV1FromCanonicalCbor(shallowCanonical);
    // §5.1/§5.3: one enveloped `enc_8` item, the same form the production bridge
    // emits — the retired counted scheme spelled this as a bare array of arrays.
    const redeemerPreimageFor = (redeemerDataCbor: Buffer): Buffer =>
      encodeMidgardFieldPreimageForFieldV1({
        fieldIndex: 8,
        items: [
          {
            purpose: "Spend",
            index: BigInt(scriptInputIndex),
            redeemerCbor: redeemerDataCbor,
            executionUnits: {
              memory: seed.executionMemory,
              steps: seed.executionSteps,
            },
          },
        ],
      });
    // The rebuild is byte-identical to the production bridge's own field-8
    // preimage at depth one, which is what licenses substituting the maximum.
    expect(
      redeemerPreimageFor(
        Buffer.from(cardanoUnaryConstructorDataCborV1(1), "hex"),
      ).toString("hex"),
    ).toBe(
      Buffer.from(shallowNative.witnessSet.redeemerTxWitsPreimageCbor).toString(
        "hex",
      ),
    );

    const canonical = encodeMidgardNativeTxCanonicalV1(
      materializeMidgardNativeTxFromCanonicalV1({
        version: shallowNative.version,
        validity: shallowNative.validity,
        body: shallowNative.body,
        witnessSet: {
          ...shallowNative.witnessSet,
          redeemerTxWitsPreimageCbor: redeemerPreimageFor(
            accepted.redeemerDataCbor,
          ),
        },
      }),
    );
    // The Cardano script witness is a raw Flat program, not a canonical Midgard
    // CEK envelope, so the same single-script projection the nested-redeemer
    // boundary uses supplies the canonical script identity. It rewrites only
    // the script witness and the script-integrity commitment; the field-8
    // redeemer preimage carrying the maximum unary Data is untouched.
    const projection = buildMidgardRetainedDaCanonicalScriptProjectionV1({
      canonicalTransactionCbor: canonical,
    });
    const projected = Buffer.from(projection.canonicalTransactionCbor);
    const native = decodeMidgardNativeTxFullV1FromCanonicalCbor(projected);
    expect(validateMidgardConsensusV1Tx(native, projected.length)).toBeNull();
    expect(
      Buffer.from(native.witnessSet.redeemerTxWitsPreimageCbor).toString("hex"),
    ).toBe(redeemerPreimageFor(accepted.redeemerDataCbor).toString("hex"));
    const redeemerField = deriveMidgardV1TxFieldPreimages(projected).find(
      (field) => field.fieldIndex === 8,
    );
    expect(redeemerField?.fieldName).toBe("redeemers");
    expect(
      redeemerField!.preimageCbor
        .toString("hex")
        .includes(accepted.redeemerDataCbor.toString("hex")),
    ).toBe(true);
    const completeChunks = countedMachineTransactionChunkStepsV1(projected);
    const redeemerChunks = completeChunks.filter(
      (chunk) => chunk.fieldIndex === 8,
    );

    const trace = buildMidgardCekDataTraverseTraceV1({
      sourceStart: 0,
      source: accepted.redeemerDataCbor,
    });
    const terminalSummary = finalizeMidgardCekDataTraverseV1(trace.terminal);
    expect(terminalSummary).not.toBeNull();
    expect(terminalSummary!.cborLength).toBe(
      BigInt(accepted.redeemerDataCbor.length),
    );
    expect(
      trace.steps.filter(({ action }) => action?.kind === "headSequence"),
    ).toHaveLength(acceptedShape.depth);
    expect(
      trace.steps.filter(({ action }) => action?.kind === "headScalar"),
    ).toHaveLength(1);
    const maximumSourceSpan = trace.steps.reduce(
      (maximum, { control }) =>
        Math.max(
          maximum,
          nextMidgardCekDataTraverseSpanV1(control)?.length ?? 0,
        ),
      0,
    );
    expect(maximumSourceSpan).toBeLessThanOrEqual(
      MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN_V1,
    );
    const finalStep = trace.steps.at(-1)!;
    expect(finalStep.action?.kind).toBe("finalizeFrame");
    if (finalStep.action?.kind !== "finalizeFrame") {
      throw new Error("Maximum unary redeemer lost its terminal frame");
    }
    expect(finalStep.action.parent).toBeNull();

    const retained = await exerciseMidgardRetainedDaCanonicalBoundaryV1({
      canonicalTransactionCbor: projected,
      canonicalMaterialSidecarCbor: projection.canonicalMaterialSidecarCbor,
      sourceRawScriptAuditHash: projection.sourceRawScriptAuditHash,
    });
    expect(retained.normal.sourceKind).toBe("normal");
    expect(retained.forced.sourceKind).toBe("forced");
    expect(retained.normal.retainedPreimageBytes).toBe(projected.length);
    expect(retained.forced.retainedPreimageBytes).toBe(projected.length);
    expect(retained.normal.reconstructedCanonicalBytes).toBe(projected.length);
    expect(retained.forced.reconstructedCanonicalBytes).toBe(projected.length);
    expect(retained.normal.revealStepCount).toBe(completeChunks.length);
    expect(retained.forced.revealStepCount).toBe(completeChunks.length);
    const canonicalDigestHex = computeHash32(projected).toString("hex");
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

    const terminalVector = {
      maxTxSize: CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
      cardanoSignedCapacityCandidate: {
        acceptedDepth: acceptedShape.depth,
        acceptedRedeemerDataCborBytes: accepted.redeemerDataCbor.length,
        acceptedSignedCardanoBytes: accepted.signedBytes,
        signedCardanoByteMargin:
          CARDANO_BOUNDARY_MAX_TX_SIZE_V1 - accepted.signedBytes,
        adjacentDepth: adjacentShape.depth,
        adjacentRedeemerDataCborBytes: adjacent.redeemerDataCbor.length,
        adjacentSignedCardanoBytes: adjacent.signedBytes,
      },
      midgardProjection: {
        dataNodeCount: acceptedShape.nodeCount,
        traverseSteps: trace.steps.length,
        maximumSourceSpan,
        sourceCanonicalTransactionBytes: canonical.length,
        canonicalTransactionBytes: projected.length,
        redeemerFieldBytes: redeemerField!.preimageCbor.length,
        redeemerFieldChunkCount: redeemerChunks.length,
        completeFoldStepCount: completeChunks.length,
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
    expect(terminalVector).toEqual(maximumUnaryRedeemerDepthTerminalVectorV1);
    if (process.env.MIDGARD_PRINT_AIKEN_VECTOR === "1") {
      console.info(
        JSON.stringify({ unaryRedeemerDepthBoundaryV1: terminalVector }),
      );
    }
  }, 300_000);
});
