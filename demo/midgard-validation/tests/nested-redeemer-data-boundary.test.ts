import { readFileSync } from "node:fs";

import {
  buildMidgardCekDataTraverseTrace,
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardCekDataFrame,
  encodeMidgardCekDataTraverseControl,
  finalizeMidgardCekDataTraverse,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN,
  midgardNativeTxFullToCardanoTxEncoding,
  nextMidgardCekDataTraverseSpan,
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

import { decodeMidgardRedeemers } from "../src/midgard-redeemers.js";
import { publishAikenVector } from "./helpers/aiken-vector-channel.js";
import {
  buildCollateralFreeMidgardSchemaParallelCandidate,
  buildSignedCardanoSpendRedeemersCandidate,
  CARDANO_BOUNDARY_MAX_TX_SIZE,
  CARDANO_BOUNDARY_TOTAL_COLLATERAL,
  cardanoBoundaryNestedDataCbor,
  deterministicCardanoBoundaryPrivateKey,
  exerciseMidgardOrderedCollectionBoundary,
  findSignedCardanoCollectionBoundary,
  measureCollateralizedPlutusFeasibilityCandidate,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
} from "./helpers/ordered-collection-boundary.js";
import {
  buildMidgardRetainedDaCanonicalScriptProjection,
  exerciseMidgardRetainedDaBoundary,
  exerciseMidgardRetainedDaCanonicalBoundary,
} from "./helpers/retained-da-boundary.js";

type BlueprintValidator = {
  readonly title: string;
  readonly compiledCode: string;
};

const alwaysSucceedsBlueprint = JSON.parse(
  readFileSync(
    new URL(
      "../../midgard-node/blueprints/always-succeeds/plutus.json",
      import.meta.url,
    ),
    "utf8",
  ),
) as {
  readonly validators: readonly BlueprintValidator[];
};

const alwaysSucceedsCompiledCode = alwaysSucceedsBlueprint.validators.find(
  (validator) => validator.title === "midgard.deposit_spend.else",
)?.compiledCode;
if (alwaysSucceedsCompiledCode === undefined) {
  throw new Error(
    "Missing always-succeeds blueprint entry midgard.deposit_spend.else",
  );
}

const spendingScript: SpendingValidator = {
  type: "PlutusV3",
  script: applyDoubleCborEncoding(alwaysSucceedsCompiledCode),
};

// The exact genuine signed-Cardano nested-redeemer boundary. These four pins
// are the cardinality and byte count the search must land on, so a silently
// shrunk redeemer datum can no longer satisfy the relative bounds alone.
const MAXIMUM_NESTED_REDEEMER_DATA_ACCEPTED_LEAF_COUNT = 5_324;
const MAXIMUM_NESTED_REDEEMER_DATA_ACCEPTED_SIGNED_BYTES = 16_382;
const MAXIMUM_NESTED_REDEEMER_DATA_ADJACENT_LEAF_COUNT = 5_325;
const MAXIMUM_NESTED_REDEEMER_DATA_ADJACENT_SIGNED_BYTES = 16_385;

/**
 * The same closed-form reference model the nested-datum boundary uses: the
 * redeemer carries a `cardanoBoundaryNestedDataCbor` tree, so its traversal
 * costs a fixed run of transitions per leaf plus a constant for the outer
 * frame. It is checked against the real producer at small leaf counts before
 * being applied to the boundary redeemer, so an early-stopping traversal
 * cannot satisfy it.
 */
const NESTED_DATA_TRAVERSE_STEPS_PER_LEAF = 24;
const NESTED_DATA_TRAVERSE_FRAMING_STEPS = 23;
const balancedNestedDataTraverseSteps = (leafCount: number): number =>
  NESTED_DATA_TRAVERSE_STEPS_PER_LEAF * leafCount +
  NESTED_DATA_TRAVERSE_FRAMING_STEPS;

describe("canonical V1 nested Cardano redeemer Data boundary", () => {
  it("normalizes, retains, and traverses one maximum nested redeemer without weakening collateral rejection", async () => {
    const privateKey = deterministicCardanoBoundaryPrivateKey(0);
    const walletAddress = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(privateKey.to_public().hash()),
    )
      .to_address()
      .to_bech32();
    const scriptAddress = validatorToAddress("Custom", spendingScript);
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
      PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
    );
    const walletInputs = (await emulator.getUtxos(walletAddress)).sort(
      (left, right) => left.outputIndex - right.outputIndex,
    );
    const scriptInputs = await emulator.getUtxos(scriptAddress);
    expect(walletInputs).toHaveLength(2);
    expect(scriptInputs).toHaveLength(1);

    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromPrivateKey(privateKey.to_bech32());
    const completedSeed = await lucid
      .newTx()
      .collectFrom([walletInputs[0]!])
      .collectFrom([scriptInputs[0]!], Data.void())
      .pay.ToAddress(walletAddress, { lovelace: 10_000_000n })
      .attach.SpendingValidator(spendingScript)
      .complete({ localUPLCEval: true });
    const signedSeed = await completedSeed.sign.withWallet().complete();
    const seed = measureCollateralizedPlutusFeasibilityCandidate(
      signedSeed.toCBOR(),
    );
    const seedTransaction = CML.Transaction.from_cbor_hex(signedSeed.toCBOR());
    const seedScripts = seedTransaction.witness_set().plutus_v3_scripts();
    expect(seedScripts?.len()).toBe(1);
    expect(seed.executionMemory).toBeGreaterThan(0n);
    expect(seed.executionSteps).toBeGreaterThan(0n);

    const buildCandidate = async (requestedNestedLeafCount: number) => {
      const candidate = await buildSignedCardanoSpendRedeemersCandidate({
        privateKeyBech32: privateKey.to_bech32(),
        feeFundingInput: walletInputs[0]!,
        collateralInput: walletInputs[1]!,
        availableScriptInputs: scriptInputs,
        recipientAddress: walletAddress,
        plutusV3ScriptCborHex: seedScripts!.get(0).to_cbor_hex(),
        redeemerDataCborHex: cardanoBoundaryNestedDataCbor(
          requestedNestedLeafCount,
        ),
        executionMemory: seed.executionMemory,
        executionSteps: seed.executionSteps,
        requestedRedeemerCount: 1,
        minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeA,
        minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeB,
        minFeeRefScriptCostPerByte:
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeRefScriptCostPerByte,
        priceMem: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.priceMem,
        priceStep: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.priceStep,
        collateralPercentage:
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.collateralPercentage,
        costModels: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.costModels,
      });
      return {
        ...candidate,
        requestedItemCount: requestedNestedLeafCount,
      };
    };
    const boundary = await findSignedCardanoCollectionBoundary({
      maxTxSize: emulator.protocolParameters.maxTxSize,
      buildSignedCandidate: buildCandidate,
    });
    const accepted = measureCollateralizedPlutusFeasibilityCandidate(
      boundary.accepted.cborHex,
    );
    const adjacent = measureCollateralizedPlutusFeasibilityCandidate(
      boundary.adjacent.cborHex,
    );
    const acceptedDataCborHex = cardanoBoundaryNestedDataCbor(
      boundary.accepted.requestedItemCount,
    );
    const adjacentDataCborHex = cardanoBoundaryNestedDataCbor(
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
      MAXIMUM_NESTED_REDEEMER_DATA_ACCEPTED_LEAF_COUNT,
    );
    expect(boundary.accepted.signedBytes).toBe(
      MAXIMUM_NESTED_REDEEMER_DATA_ACCEPTED_SIGNED_BYTES,
    );
    expect(boundary.adjacent.requestedItemCount).toBe(
      MAXIMUM_NESTED_REDEEMER_DATA_ADJACENT_LEAF_COUNT,
    );
    expect(boundary.adjacent.signedBytes).toBe(
      MAXIMUM_NESTED_REDEEMER_DATA_ADJACENT_SIGNED_BYTES,
    );
    expect(accepted.redeemerCount).toBe(1);
    expect(adjacent.redeemerCount).toBe(1);
    expect(accepted.redeemerTags).toEqual([CML.RedeemerTag.Spend]);
    expect(accepted.redeemerIndexes).toEqual([1n]);
    expect(accepted.redeemerDataCborHexes).toEqual([acceptedDataCborHex]);
    expect(adjacent.redeemerDataCborHexes).toEqual([adjacentDataCborHex]);
    expect(accepted.executionMemory).toBe(seed.executionMemory);
    expect(accepted.executionSteps).toBe(seed.executionSteps);
    expect(accepted.totalCollateral).toBe(CARDANO_BOUNDARY_TOTAL_COLLATERAL);
    const acceptedTransaction = CML.Transaction.from_cbor_hex(
      boundary.accepted.cborHex,
    );
    expect(acceptedTransaction.body().withdrawals()).toBeUndefined();
    expect(acceptedTransaction.body().mint()).toBeUndefined();
    expect(acceptedTransaction.body().required_signers()).toBeUndefined();

    let collateralRejection:
      | {
          readonly message: string;
          readonly code: string | null;
          readonly detail: string | null;
        }
      | undefined;
    try {
      cardanoTxBytesToMidgardNativeTxCanonicalCbor(
        Buffer.from(boundary.accepted.cborHex, "hex"),
      );
    } catch (error) {
      const structured = error as {
        readonly code?: unknown;
        readonly detail?: unknown;
      };
      collateralRejection = {
        message: error instanceof Error ? error.message : String(error),
        code: typeof structured.code === "string" ? structured.code : null,
        detail:
          typeof structured.detail === "string" ? structured.detail : null,
      };
    }
    expect(collateralRejection).toEqual({
      message:
        "Cardano tx cannot be converted to Midgard native format without dropping fields",
      code: "E_CONVERSION_UNSUPPORTED_FEATURE",
      detail: "collateral_inputs",
    });

    const parallel = buildCollateralFreeMidgardSchemaParallelCandidate({
      collateralizedCardanoCborHex: boundary.accepted.cborHex,
      privateKeyBech32: privateKey.to_bech32(),
    });
    const parallelTransaction = CML.Transaction.from_cbor_hex(parallel.cborHex);
    expect(parallelTransaction.body().collateral_inputs()).toBeUndefined();
    expect(parallelTransaction.body().collateral_return()).toBeUndefined();
    expect(parallelTransaction.body().total_collateral()).toBeUndefined();
    expect(parallelTransaction.body().withdrawals()).toBeUndefined();
    expect(parallelTransaction.body().mint()).toBeUndefined();
    expect(
      Array.from(
        {
          length: parallelTransaction.body().inputs().len(),
        },
        (_, index) =>
          parallelTransaction.body().inputs().get(index).to_cbor_hex(),
      ),
    ).toEqual(
      Array.from(
        {
          length: acceptedTransaction.body().inputs().len(),
        },
        (_, index) =>
          acceptedTransaction.body().inputs().get(index).to_cbor_hex(),
      ),
    );
    expect(
      Array.from(
        {
          length: parallelTransaction.body().outputs().len(),
        },
        (_, index) =>
          parallelTransaction.body().outputs().get(index).to_cbor_hex(),
      ),
    ).toEqual(
      Array.from(
        {
          length: acceptedTransaction.body().outputs().len(),
        },
        (_, index) =>
          acceptedTransaction.body().outputs().get(index).to_cbor_hex(),
      ),
    );
    expect(parallelTransaction.body().fee()).toBe(
      acceptedTransaction.body().fee(),
    );
    expect(parallelTransaction.body().script_data_hash()?.to_hex()).toBe(
      acceptedTransaction.body().script_data_hash()?.to_hex(),
    );

    const nativeCanonical = cardanoTxBytesToMidgardNativeTxCanonicalCbor(
      Buffer.from(parallel.cborHex, "hex"),
    );
    const native = decodeMidgardNativeTxFullFromCanonicalCbor(nativeCanonical);
    const decodedRedeemers = decodeMidgardRedeemers(
      native.witnessSet.redeemerTxWitsPreimageCbor,
    );
    expect(decodedRedeemers).toHaveLength(1);
    expect(decodedRedeemers[0]).toMatchObject({
      tag: CML.RedeemerTag.Spend,
      index: 1n,
      exUnits: {
        memory: seed.executionMemory,
        steps: seed.executionSteps,
      },
    });
    expect(
      Buffer.from(decodedRedeemers[0]!.dataCborHex, "hex").equals(
        Buffer.from(acceptedDataCborHex, "hex"),
      ),
    ).toBe(true);

    const redeemerField = exerciseMidgardOrderedCollectionBoundary({
      signedCardanoCborHex: parallel.cborHex,
      fieldIndex: 8,
    });
    expect(redeemerField.itemCount).toBe(1);
    expect(redeemerField.maxChunkBytes).toBe(MIDGARD_BOUNDED_ITEM_CHUNK_BYTES);
    expect(redeemerField.maxRevealBytes).toBeLessThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );
    const trace = buildMidgardCekDataTraverseTrace({
      sourceStart: 0,
      source: Buffer.from(acceptedDataCborHex, "hex"),
    });
    const terminalSummary = finalizeMidgardCekDataTraverse(trace.terminal);
    expect(terminalSummary).not.toBeNull();
    expect(terminalSummary!.cborLength).toBe(
      BigInt(acceptedDataCborHex.length / 2),
    );
    const maximumSourceSpan = trace.steps.reduce(
      (maximum, { control }) =>
        Math.max(maximum, nextMidgardCekDataTraverseSpan(control)?.length ?? 0),
      0,
    );
    expect(maximumSourceSpan).toBeLessThanOrEqual(
      MIDGARD_CEK_DATA_TRAVERSE_MAX_SOURCE_SPAN,
    );
    // The traverse step count is decided by the reference model above, checked
    // first against the real producer at leaf counts small enough to read.
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
    expect(trace.steps).toHaveLength(
      balancedNestedDataTraverseSteps(boundary.accepted.requestedItemCount),
    );

    const terminalStep = trace.steps.at(-1)!;
    expect(terminalStep.action?.kind).toBe("finalizeFrame");
    if (terminalStep.action?.kind !== "finalizeFrame") {
      throw new Error("Maximum nested redeemer lost its terminal frame");
    }
    const terminalVector = {
      maxTxSize: CARDANO_BOUNDARY_MAX_TX_SIZE,
      nestedLeafCount: boundary.accepted.requestedItemCount,
      dataNodeCount: boundary.accepted.requestedItemCount * 2 + 2,
      dataCborBytes: acceptedDataCborHex.length / 2,
      signedCardanoBytes: boundary.accepted.signedBytes,
      signedCardanoByteMargin:
        CARDANO_BOUNDARY_MAX_TX_SIZE - boundary.accepted.signedBytes,
      adjacentLeafCount: boundary.adjacent.requestedItemCount,
      adjacentDataCborBytes: adjacentDataCborHex.length / 2,
      adjacentSignedCardanoBytes: boundary.adjacent.signedBytes,
      parallelSignedCardanoBytes: parallel.cborHex.length / 2,
      nativeCanonicalBytes: nativeCanonical.length,
      redeemerFieldBytes: redeemerField.fieldBytes,
      redeemerTraverseSteps: trace.steps.length,
      maximumSourceSpan,
      terminalPreControlCborHex: encodeMidgardCekDataTraverseControl(
        terminalStep.control,
      ).toString("hex"),
      terminalFrameCborHex: encodeMidgardCekDataFrame(
        terminalStep.action.frame,
      ).toString("hex"),
      terminalPostControlCborHex: encodeMidgardCekDataTraverseControl(
        terminalStep.next,
      ).toString("hex"),
      terminalSummary: {
        rootHex: Buffer.from(terminalSummary!.root).toString("hex"),
        cborLength: terminalSummary!.cborLength.toString(),
        memory: terminalSummary!.memory.toString(),
      },
      productionCollateralRejection: collateralRejection,
    };
    // The Aiken twin's `maximum_cardano_nested_redeemer_*` constants are
    // rebound from this vector by
    // `scripts/generate-nested-boundary-aiken-goldens.mjs`, whose `--check` run
    // is a required CI job. Publishing happens after every assertion above, and
    // no environment variable can remove an assertion.
    publishAikenVector("nested-redeemer-data-boundary-v1", {
      ...terminalVector,
      // The frame's own sequence root, which the Aiken twin needs to rebuild
      // the terminal frame it steps over.
      terminalFrameSequenceRootHex: Buffer.from(
        terminalStep.action.frame.sequence.root,
      ).toString("hex"),
    });

    const retained = await exerciseMidgardRetainedDaBoundary({
      signedCardanoCborHex: parallel.cborHex,
    });
    expect(retained.normal.reconstructedCanonicalBytes).toBe(
      nativeCanonical.length,
    );
    expect(retained.forced.reconstructedCanonicalBytes).toBe(
      nativeCanonical.length - 1,
    );
    expect(retained.normal.revealStepCount).toBe(
      redeemerField.completeFoldStepCount,
    );
    expect(retained.forced.revealStepCount).toBe(
      redeemerField.completeFoldStepCount,
    );

    const retainedProjection = buildMidgardRetainedDaCanonicalScriptProjection({
      canonicalTransactionCbor: nativeCanonical,
    });
    const productionRetained = await exerciseMidgardRetainedDaCanonicalBoundary(
      {
        canonicalTransactionCbor: retainedProjection.canonicalTransactionCbor,
        corpusLabel: "balanced-nested-redeemer",
        canonicalMaterialSidecarCbor:
          retainedProjection.canonicalMaterialSidecarCbor,
        sourceRawScriptAuditHash: retainedProjection.sourceRawScriptAuditHash,
      },
    );
    expect(productionRetained.normal.reconstructedCanonicalBytes).toBe(
      retainedProjection.canonicalTransactionCbor.length,
    );
    expect(productionRetained.forced.reconstructedCanonicalBytes).toBe(
      retainedProjection.canonicalTransactionCbor.length - 1,
    );

    const reconstructed = measureCollateralizedPlutusFeasibilityCandidate(
      Buffer.from(midgardNativeTxFullToCardanoTxEncoding(native)).toString(
        "hex",
      ),
    );
    expect({
      tags: reconstructed.redeemerTags,
      indexes: reconstructed.redeemerIndexes,
      data: reconstructed.redeemerDataCborHexes,
      memory: reconstructed.executionMemory,
      steps: reconstructed.executionSteps,
    }).toEqual({
      tags: accepted.redeemerTags,
      indexes: accepted.redeemerIndexes,
      data: accepted.redeemerDataCborHexes,
      memory: accepted.executionMemory,
      steps: accepted.executionSteps,
    });

    const txHash = await emulator.submitTx(boundary.accepted.cborHex);
    await expect(emulator.awaitTx(txHash)).resolves.toBe(true);
  }, 300_000);
});
