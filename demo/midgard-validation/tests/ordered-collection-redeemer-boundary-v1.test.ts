import { readFileSync } from "node:fs";

import {
  cardanoTxBytesToMidgardNativeTxCanonicalCborV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  midgardNativeTxFullToCardanoTxEncoding,
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

import {
  buildCollateralFreeMidgardSchemaParallelCandidateV1,
  buildSignedCardanoSpendRedeemersCandidateV1,
  CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
  CARDANO_BOUNDARY_TOTAL_COLLATERAL_V1,
  deriveCardanoGenesisInputSupplyV1,
  deterministicCardanoBoundaryPrivateKeyV1,
  exerciseMidgardOrderedCollectionBoundaryV1,
  findSignedCardanoCollectionBoundaryV1,
  measureCollateralizedPlutusFeasibilityCandidateV1,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
} from "./helpers/ordered-collection-boundary-v1.js";

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

const alwaysSucceedsCompiledCode =
  alwaysSucceedsBlueprint.validators.find(
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

describe("canonical V1 spend-redeemer Cardano boundary", () => {
  it("derives the exact field-8 cardinality from Cardano bytes and execution limits", async () => {
    const spendingKey = deterministicCardanoBoundaryPrivateKeyV1(0);
    const walletAddress = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(
        spendingKey.to_public().hash(),
      ),
    )
      .to_address()
      .to_bech32();
    const scriptAddress = validatorToAddress("Custom", spendingScript);
    const scriptInputSupply = deriveCardanoGenesisInputSupplyV1(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    const walletInputLovelace = 1_000_000_000_000n;
    const scriptInputLovelace = 10_000_000n;
    const emulator = new Emulator(
      [
        {
          seedPhrase: "",
          privateKey: spendingKey.to_bech32(),
          address: walletAddress,
          assets: { lovelace: walletInputLovelace },
        },
        {
          seedPhrase: "",
          privateKey: spendingKey.to_bech32(),
          address: walletAddress,
          assets: { lovelace: walletInputLovelace },
        },
        ...Array.from({ length: scriptInputSupply }, () => ({
          seedPhrase: "",
          privateKey: "",
          address: scriptAddress,
          assets: { lovelace: scriptInputLovelace },
          outputData: { inline: Data.void() },
        })),
      ],
      PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
    );
    const walletInputs = (
      await emulator.getUtxos(walletAddress)
    ).sort((left, right) => left.outputIndex - right.outputIndex);
    const scriptInputs = (
      await emulator.getUtxos(scriptAddress)
    ).sort((left, right) => left.outputIndex - right.outputIndex);
    expect(walletInputs).toHaveLength(2);
    expect(scriptInputs).toHaveLength(scriptInputSupply);
    expect(walletInputs.map((input) => input.outputIndex)).toEqual([
      0, 1,
    ]);
    expect(scriptInputs[0]?.outputIndex).toBe(2);
    expect(scriptInputs.at(-1)?.outputIndex).toBe(
      scriptInputSupply + 1,
    );
    for (const input of [...walletInputs, ...scriptInputs]) {
      expect(input.txHash).toBe("00".repeat(32));
    }

    const lucid = await Lucid(emulator, "Custom");
    lucid.selectWallet.fromPrivateKey(spendingKey.to_bech32());
    const completedSeed = await lucid
      .newTx()
      .collectFrom([walletInputs[0]!])
      .collectFrom([scriptInputs[0]!], Data.void())
      .pay.ToAddress(walletAddress, { lovelace: 10_000_000n })
      .attach.SpendingValidator(spendingScript)
      .complete({ localUPLCEval: true });
    const signedSeed = await completedSeed.sign.withWallet().complete();
    const seedMeasurement =
      measureCollateralizedPlutusFeasibilityCandidateV1(
        signedSeed.toCBOR(),
      );
    expect(seedMeasurement.redeemerCount).toBe(1);
    expect(seedMeasurement.redeemerTags).toEqual([
      CML.RedeemerTag.Spend,
    ]);
    expect(seedMeasurement.redeemerDataCborHexes).toEqual([
      Data.void(),
    ]);
    expect(seedMeasurement.executionMemory).toBeGreaterThan(0n);
    expect(seedMeasurement.executionSteps).toBeGreaterThan(0n);

    const seedTransaction = CML.Transaction.from_cbor_hex(
      signedSeed.toCBOR(),
    );
    const seedPlutusV3Scripts =
      seedTransaction.witness_set().plutus_v3_scripts();
    expect(seedPlutusV3Scripts?.len()).toBe(1);
    const plutusV3ScriptCborHex =
      seedPlutusV3Scripts!.get(0).to_cbor_hex();
    const [feeFundingInput, collateralInput] = walletInputs;
    const buildCandidate = (requestedRedeemerCount: number) =>
      buildSignedCardanoSpendRedeemersCandidateV1({
        privateKeyBech32: spendingKey.to_bech32(),
        feeFundingInput: feeFundingInput!,
        collateralInput: collateralInput!,
        availableScriptInputs: scriptInputs,
        recipientAddress: walletAddress,
        plutusV3ScriptCborHex,
        redeemerDataCborHex: seedMeasurement.redeemerDataCborHexes[0]!,
        executionMemory: seedMeasurement.executionMemory,
        executionSteps: seedMeasurement.executionSteps,
        requestedRedeemerCount,
        minFeeA:
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
        minFeeB:
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
        minFeeRefScriptCostPerByte:
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeRefScriptCostPerByte,
        priceMem:
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.priceMem,
        priceStep:
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.priceStep,
        collateralPercentage:
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.collateralPercentage,
        costModels:
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.costModels,
      });

    const firstCandidate = await buildCandidate(1);
    const firstMeasurement =
      measureCollateralizedPlutusFeasibilityCandidateV1(
        firstCandidate.cborHex,
      );
    expect(firstMeasurement.redeemerCount).toBe(1);
    expect(firstMeasurement.redeemerDataCborHexes).toEqual(
      seedMeasurement.redeemerDataCborHexes,
    );
    expect(firstMeasurement.executionMemory).toBe(
      seedMeasurement.executionMemory,
    );
    expect(firstMeasurement.executionSteps).toBe(
      seedMeasurement.executionSteps,
    );

    const boundary = await findSignedCardanoCollectionBoundaryV1({
      maxTxSize: emulator.protocolParameters.maxTxSize,
      buildSignedCandidate: buildCandidate,
    });
    const accepted =
      measureCollateralizedPlutusFeasibilityCandidateV1(
        boundary.accepted.cborHex,
      );
    const adjacent =
      measureCollateralizedPlutusFeasibilityCandidateV1(
        boundary.adjacent.cborHex,
      );
    const acceptedCount = boundary.accepted.requestedItemCount;
    const adjacentCount = boundary.adjacent.requestedItemCount;
    const maxByMemory = Number(
      emulator.protocolParameters.maxTxExMem /
        seedMeasurement.executionMemory,
    );
    const maxBySteps = Number(
      emulator.protocolParameters.maxTxExSteps /
        seedMeasurement.executionSteps,
    );

    expect(boundary.accepted.signedBytes).toBeLessThanOrEqual(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect(boundary.adjacent.signedBytes).toBeGreaterThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect(adjacentCount).toBe(acceptedCount + 1);
    expect(adjacentCount).toBeLessThanOrEqual(maxByMemory);
    expect(adjacentCount).toBeLessThanOrEqual(maxBySteps);
    expect(accepted.redeemerCount).toBe(acceptedCount);
    expect(adjacent.redeemerCount).toBe(adjacentCount);
    expect(accepted.inputCount).toBe(acceptedCount + 1);
    expect(adjacent.inputCount).toBe(adjacentCount + 1);
    expect(accepted.outputCount).toBe(1);
    expect(adjacent.outputCount).toBe(1);
    expect(accepted.collateralInputOutRefs).toEqual([
      `${collateralInput!.txHash}#${collateralInput!.outputIndex.toString()}`,
    ]);
    expect(adjacent.collateralInputOutRefs).toEqual(
      accepted.collateralInputOutRefs,
    );
    expect(accepted.totalCollateral).toBe(
      CARDANO_BOUNDARY_TOTAL_COLLATERAL_V1,
    );
    expect(adjacent.totalCollateral).toBe(
      CARDANO_BOUNDARY_TOTAL_COLLATERAL_V1,
    );
    expect(accepted.vkeyWitnessCount).toBe(1);
    expect(adjacent.vkeyWitnessCount).toBe(1);
    expect(accepted.plutusV3ScriptCount).toBe(1);
    expect(adjacent.plutusV3ScriptCount).toBe(1);
    expect(accepted.redeemerTags).toEqual(
      Array.from(
        { length: acceptedCount },
        () => CML.RedeemerTag.Spend,
      ),
    );
    expect(adjacent.redeemerTags).toEqual(
      Array.from(
        { length: adjacentCount },
        () => CML.RedeemerTag.Spend,
      ),
    );
    expect(accepted.redeemerDataCborHexes).toEqual(
      Array.from(
        { length: acceptedCount },
        () => seedMeasurement.redeemerDataCborHexes[0],
      ),
    );
    expect(adjacent.redeemerDataCborHexes).toEqual(
      Array.from(
        { length: adjacentCount },
        () => seedMeasurement.redeemerDataCborHexes[0],
      ),
    );
    expect(accepted.executionMemory).toBe(
      BigInt(acceptedCount) * seedMeasurement.executionMemory,
    );
    expect(adjacent.executionMemory).toBe(
      BigInt(adjacentCount) * seedMeasurement.executionMemory,
    );
    expect(accepted.executionSteps).toBe(
      BigInt(acceptedCount) * seedMeasurement.executionSteps,
    );
    expect(adjacent.executionSteps).toBe(
      BigInt(adjacentCount) * seedMeasurement.executionSteps,
    );
    expect(accepted.executionMemory).toBeLessThanOrEqual(
      emulator.protocolParameters.maxTxExMem,
    );
    expect(adjacent.executionMemory).toBeLessThanOrEqual(
      emulator.protocolParameters.maxTxExMem,
    );
    expect(accepted.executionSteps).toBeLessThanOrEqual(
      emulator.protocolParameters.maxTxExSteps,
    );
    expect(adjacent.executionSteps).toBeLessThanOrEqual(
      emulator.protocolParameters.maxTxExSteps,
    );
    expect(new Set(accepted.redeemerIndexes).size).toBe(
      acceptedCount,
    );
    expect(new Set(adjacent.redeemerIndexes).size).toBe(
      adjacentCount,
    );
    expect(accepted.redeemerIndexes).toEqual(
      Array.from(
        { length: acceptedCount },
        (_, index) => BigInt(index + 1),
      ),
    );
    expect(adjacent.redeemerIndexes).toEqual(
      Array.from(
        { length: adjacentCount },
        (_, index) => BigInt(index + 1),
      ),
    );
    const acceptedTransaction = CML.Transaction.from_cbor_hex(
      boundary.accepted.cborHex,
    );
    const adjacentTransaction = CML.Transaction.from_cbor_hex(
      boundary.adjacent.cborHex,
    );
    expect(acceptedTransaction.body().withdrawals()).toBeUndefined();
    expect(adjacentTransaction.body().withdrawals()).toBeUndefined();
    expect(acceptedTransaction.body().mint()).toBeUndefined();
    expect(adjacentTransaction.body().mint()).toBeUndefined();

    let collateralRejection:
      | {
          readonly message: string;
          readonly code: string | null;
          readonly detail: string | null;
        }
      | undefined;
    try {
      exerciseMidgardOrderedCollectionBoundaryV1({
        signedCardanoCborHex: boundary.accepted.cborHex,
        fieldIndex: 8,
      });
    } catch (error) {
      const structured = error as {
        readonly code?: unknown;
        readonly detail?: unknown;
      };
      collateralRejection = {
        message: error instanceof Error ? error.message : String(error),
        code:
          typeof structured.code === "string"
            ? structured.code
            : null,
        detail:
          typeof structured.detail === "string"
            ? structured.detail
            : null,
      };
    }
    expect(collateralRejection).toEqual({
      message:
        "Cardano tx cannot be converted to Midgard native format without dropping fields",
      code: "E_CONVERSION_UNSUPPORTED_FEATURE",
      detail: "collateral_inputs",
    });

    const parallel =
      buildCollateralFreeMidgardSchemaParallelCandidateV1({
        collateralizedCardanoCborHex: boundary.accepted.cborHex,
        privateKeyBech32: spendingKey.to_bech32(),
      });
    expect(parallel.parallelRedeemersCborHex).toBe(
      parallel.collateralizedRedeemersCborHex,
    );
    const redeemerField =
      exerciseMidgardOrderedCollectionBoundaryV1({
        signedCardanoCborHex: parallel.cborHex,
        fieldIndex: 8,
      });
    expect(redeemerField.itemCount).toBe(acceptedCount);
    expect(redeemerField.revealStepCount).toBe(acceptedCount);
    expect(redeemerField.maxRevealBytes).toBeLessThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    const parallelNative =
      decodeMidgardNativeTxFullV1FromCanonicalCbor(
        cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
          Buffer.from(parallel.cborHex, "hex"),
        ),
      );
    expect(
      parallelNative.witnessSet.redeemerTxWitsPreimageCbor.toString(
        "hex",
      ),
    ).toBe(redeemerField.fieldPreimageCborHex);
    const reconstructed = measureCollateralizedPlutusFeasibilityCandidateV1(
      Buffer.from(
        midgardNativeTxFullToCardanoTxEncoding(parallelNative),
      ).toString("hex"),
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

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            redeemerBoundaryV1: {
              fieldIndex: 8,
              fieldName: "redeemer_tx_wits",
              fixtureGenerationBasis:
                "real script UTxOs; genuine N=1 local evaluation; on-demand exact signed-byte search",
              maxTxSize: emulator.protocolParameters.maxTxSize,
              maxTxExMem:
                emulator.protocolParameters.maxTxExMem.toString(),
              maxTxExSteps:
                emulator.protocolParameters.maxTxExSteps.toString(),
              perRedeemerMemory:
                seedMeasurement.executionMemory.toString(),
              perRedeemerSteps:
                seedMeasurement.executionSteps.toString(),
              maxRedeemersByMemory: maxByMemory,
              maxRedeemersBySteps: maxBySteps,
              requestedRedeemerCount: acceptedCount,
              actualInputCount: accepted.inputCount,
              actualRedeemerCount: accepted.redeemerCount,
              actualPlutusV3ScriptCount:
                accepted.plutusV3ScriptCount,
              actualVkeyWitnessCount:
                accepted.vkeyWitnessCount,
              signedCardanoBytes: boundary.accepted.signedBytes,
              byteMargin:
                emulator.protocolParameters.maxTxSize -
                boundary.accepted.signedBytes,
              executionMemory: accepted.executionMemory.toString(),
              executionMemoryMargin:
                (
                  emulator.protocolParameters.maxTxExMem -
                  accepted.executionMemory
                ).toString(),
              executionSteps: accepted.executionSteps.toString(),
              executionStepsMargin:
                (
                  emulator.protocolParameters.maxTxExSteps -
                  accepted.executionSteps
                ).toString(),
              fee: boundary.accepted.fee.toString(),
              totalCollateral:
                accepted.totalCollateral?.toString() ?? null,
              nativeCanonicalBytes:
                redeemerField.nativeCanonicalBytes,
              redeemerFieldBytes: redeemerField.fieldBytes,
              redeemerFieldCommitmentHex:
                redeemerField.fieldCommitmentHex,
              redeemerFieldPreimageHashHex:
                redeemerField.fieldPreimageHashHex,
              redeemerItems: redeemerField.itemCount,
              redeemerRevealSteps:
                redeemerField.revealStepCount,
              maxChunkBytes: redeemerField.maxChunkBytes,
              maxRevealBytes: redeemerField.maxRevealBytes,
              completeFoldSteps:
                redeemerField.completeFoldStepCount,
              productionCollateralRejection:
                collateralRejection,
              adjacentRequestedRedeemerCount: adjacentCount,
              adjacentActualInputCount: adjacent.inputCount,
              adjacentActualRedeemerCount: adjacent.redeemerCount,
              adjacentSignedCardanoBytes:
                boundary.adjacent.signedBytes,
              adjacentByteMargin:
                emulator.protocolParameters.maxTxSize -
                boundary.adjacent.signedBytes,
              adjacentExecutionMemory:
                adjacent.executionMemory.toString(),
              adjacentExecutionSteps:
                adjacent.executionSteps.toString(),
              adjacentFee: boundary.adjacent.fee.toString(),
              adjacentFailure: boundary.adjacentFailure,
              emulatorResult: "PASS",
            },
          },
          null,
          2,
        ),
      );
    }
    if (
      process.env.MIDGARD_PRINT_AIKEN_VECTOR === "1"
    ) {
      console.info(
        JSON.stringify({
          redeemerFieldPreimageCborHex:
            redeemerField.fieldPreimageCborHex,
          redeemerFieldCommitmentHex:
            redeemerField.fieldCommitmentHex,
          redeemerFieldPreimageHashHex:
            redeemerField.fieldPreimageHashHex,
        }),
      );
    }
  }, 300_000);
});
