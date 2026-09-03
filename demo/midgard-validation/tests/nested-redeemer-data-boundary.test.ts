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

// The exact genuine signed-Cardano nested-redeemer boundary. The terminal
// vector below carries the same numbers, but its comparison is skipped while an
// Aiken vector is being regenerated; these four pins are unconditional, so a
// silently shrunk redeemer datum can no longer satisfy the relative bounds
// alone.
const MAXIMUM_NESTED_REDEEMER_DATA_ACCEPTED_LEAF_COUNT = 5_324;
const MAXIMUM_NESTED_REDEEMER_DATA_ACCEPTED_SIGNED_BYTES = 16_382;
const MAXIMUM_NESTED_REDEEMER_DATA_ADJACENT_LEAF_COUNT = 5_325;
const MAXIMUM_NESTED_REDEEMER_DATA_ADJACENT_SIGNED_BYTES = 16_385;

const maximumNestedRedeemerDataTerminalVector = {
  maxTxSize: 16384,
  nestedLeafCount: 5324,
  dataNodeCount: 10650,
  dataCborBytes: 15982,
  signedCardanoBytes: 16382,
  signedCardanoByteMargin: 2,
  adjacentLeafCount: 5325,
  adjacentDataCborBytes: 15985,
  adjacentSignedCardanoBytes: 16385,
  parallelSignedCardanoBytes: 16293,
  nativeCanonicalBytes: 16357,
  redeemerFieldBytes: 16001,
  redeemerTraverseSteps: 127799,
  maximumSourceSpan: 14,
  terminalPreControlCborHex:
    "8a010600193e6e193e6e582008f6a2dc24df8fbc23b2d4255dda3ca30a2fd28eb361e9ab31bf01732c764eead87a80d87a80d87a80d87a80",
  terminalFrameCborHex:
    "8b010058203ba6e86f178af94b2662ab108e98320a100ccd6b2c517f0eee2ab72a2c562fcf0206400101818200582020de66bc0f1322c9c61884ce582d6698c9075e35c183a4264c6c7c27fbf1401b018458200f7bb776751d400f727bf81b02cd7ed66457e144209eb5f9f90e2c6500fe149601193e6719bb30",
  terminalPostControlCborHex:
    "8a010700193e6e193e6e40d87a80d87a80d87a80d8799f83582026ef420c9e803ba9d74f048b521bff6c99e6a6b4d8aefd077c300a8e31a4dc20193e6e19bb34ff",
  terminalSummary: {
    rootHex: "26ef420c9e803ba9d74f048b521bff6c99e6a6b4d8aefd077c300a8e31a4dc20",
    cborLength: "15982",
    memory: "47924",
  },
  productionCollateralRejection: {
    message:
      "Cardano tx cannot be converted to Midgard native format without dropping fields",
    code: "E_CONVERSION_UNSUPPORTED_FEATURE",
    detail: "collateral_inputs",
  },
} as const;

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
    if (process.env.MIDGARD_PRINT_AIKEN_VECTOR !== "1") {
      expect(terminalVector).toEqual(maximumNestedRedeemerDataTerminalVector);
    }

    const retained = await exerciseMidgardRetainedDaBoundary({
      signedCardanoCborHex: parallel.cborHex,
    });
    expect(retained.normal.reconstructedCanonicalBytes).toBe(
      nativeCanonical.length,
    );
    expect(retained.forced.reconstructedCanonicalBytes).toBe(
      nativeCanonical.length,
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
      retainedProjection.canonicalTransactionCbor.length,
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

    if (process.env.MIDGARD_PRINT_AIKEN_VECTOR === "1") {
      console.info(
        JSON.stringify({
          nestedRedeemerDataBoundaryV1: terminalVector,
        }),
      );
    }
  }, 300_000);
});
