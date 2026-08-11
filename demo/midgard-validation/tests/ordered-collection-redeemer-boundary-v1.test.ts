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

import { publishAikenVectorV1 } from "./helpers/aiken-vector-channel.js";
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
import {
  buildMidgardRetainedDaCanonicalScriptProjectionV1,
  exerciseMidgardRetainedDaBoundaryV1,
  exerciseMidgardRetainedDaCanonicalBoundaryV1,
} from "./helpers/retained-da-boundary-v1.js";

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

// The exact genuine signed-Cardano field-8 boundary. The terminal fold vector
// below is the Aiken-replayed half; these four numbers pin the redeemer
// cardinality and byte count the search must land on, so a silently shrunk
// redeemer collection can no longer satisfy the relative bounds alone.
const MAXIMUM_REDEEMER_ACCEPTED_COUNT_V1 = 296;
const MAXIMUM_REDEEMER_ACCEPTED_SIGNED_BYTES_V1 = 16_377;
const MAXIMUM_REDEEMER_ADJACENT_COUNT_V1 = 297;
const MAXIMUM_REDEEMER_ADJACENT_SIGNED_BYTES_V1 = 16_433;

const maximumRedeemerTerminalFoldVectorV1 = {
  fieldCommitmentHex:
    "07da3c8aea4dd252510b18f872268ea7b7d752fe9d6874f3321286ec6d8c4133",
  fieldPreimageHashHex:
    "680079f9aebb6ab20240bf0a4b46a9b607181843413e0cdfbb293942aebe3d0a",
  transactionIdHex:
    "bb9eefcba3b233b08c3969d7e72dd6911353f5ae4fb0f78dd4fa94bec151e203",
  transactionCommitmentHex:
    "c4670fb733c631fe5d11bfce0c8b70eeca201c074ed5aa3cba55f81c29755d7b",
  preWorkRootHex:
    "c54966df530e3156a5d0312b2a4d441490dbd1996c3778cebc94f2e35ee22c62",
  postWorkRootHex:
    "1e375126f821f3794f40887267a40a9d9b4cf1bc4b77a9e08b60eb46fa1e5051",
  encodedLengthBeforeItem: 5_035,
  collectionProof: {
    fieldIndex: 8,
    itemCount: 296,
    itemIndex: 295,
    itemLength: 18,
    itemCommitmentHex:
      "0b7517c996b4be98c145b61a84789c337d9a394529322f0e4ff2b00825a13fe5",
    frontier: [
      {
        height: 3,
        hashHex:
          "599fb8883e9753ebff787e9ba693c9d266a94e2e4c2412f5cc8def17a37efc4b",
      },
      {
        height: 5,
        hashHex:
          "d3ee1a26b14495f4b4e5196a4035453be00e5947a29d7a106e78df0ffb840942",
      },
      {
        height: 8,
        hashHex:
          "baff8cd322326841f8dbf9a8fa0464a67cdf9c4161e429fbb569e0346adffde1",
      },
    ],
    siblingHexes: [
      "b2a18a9249e13b7f2c75032bf73d6d447b196d970097d8276f450ddcbd45ff21",
      "8277569ed239c02bb472113a2804537ce7e9977f6fc3e8e01104f1c38c696c18",
      "07c8bf5bbf9c9e2b477a1c8ccfe1a7d4ce62846164c5fbebc6481c4da4fc6f22",
    ],
  },
  chunkProof: {
    fieldIndex: 8,
    itemIndex: 295,
    totalLength: 18,
    chunkIndex: 0,
    chunkHex: "840019012843d87980821906411a0004d2f5",
    frontier: [
      {
        height: 0,
        hashHex:
          "bd5765879c3e766f6cbc89ea728e263b73af278ed4091e26cabaf5b7fb04d91e",
      },
    ],
    siblingHexes: [],
  },
} as const;

describe("canonical V1 spend-redeemer Cardano boundary", () => {
  it("derives the exact field-8 cardinality from Cardano bytes and execution limits", async () => {
    const spendingKey = deterministicCardanoBoundaryPrivateKeyV1(0);
    const walletAddress = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(spendingKey.to_public().hash()),
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
    const walletInputs = (await emulator.getUtxos(walletAddress)).sort(
      (left, right) => left.outputIndex - right.outputIndex,
    );
    const scriptInputs = (await emulator.getUtxos(scriptAddress)).sort(
      (left, right) => left.outputIndex - right.outputIndex,
    );
    expect(walletInputs).toHaveLength(2);
    expect(scriptInputs).toHaveLength(scriptInputSupply);
    expect(walletInputs.map((input) => input.outputIndex)).toEqual([0, 1]);
    expect(scriptInputs[0]?.outputIndex).toBe(2);
    expect(scriptInputs.at(-1)?.outputIndex).toBe(scriptInputSupply + 1);
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
    const seedMeasurement = measureCollateralizedPlutusFeasibilityCandidateV1(
      signedSeed.toCBOR(),
    );
    expect(seedMeasurement.redeemerCount).toBe(1);
    expect(seedMeasurement.redeemerTags).toEqual([CML.RedeemerTag.Spend]);
    expect(seedMeasurement.redeemerDataCborHexes).toEqual([Data.void()]);
    expect(seedMeasurement.executionMemory).toBeGreaterThan(0n);
    expect(seedMeasurement.executionSteps).toBeGreaterThan(0n);

    const seedTransaction = CML.Transaction.from_cbor_hex(signedSeed.toCBOR());
    const seedPlutusV3Scripts = seedTransaction
      .witness_set()
      .plutus_v3_scripts();
    expect(seedPlutusV3Scripts?.len()).toBe(1);
    const plutusV3ScriptCborHex = seedPlutusV3Scripts!.get(0).to_cbor_hex();
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

    const firstCandidate = await buildCandidate(1);
    const firstMeasurement = measureCollateralizedPlutusFeasibilityCandidateV1(
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
    const accepted = measureCollateralizedPlutusFeasibilityCandidateV1(
      boundary.accepted.cborHex,
    );
    const adjacent = measureCollateralizedPlutusFeasibilityCandidateV1(
      boundary.adjacent.cborHex,
    );
    const acceptedCount = boundary.accepted.requestedItemCount;
    const adjacentCount = boundary.adjacent.requestedItemCount;
    const maxByMemory = Number(
      emulator.protocolParameters.maxTxExMem / seedMeasurement.executionMemory,
    );
    const maxBySteps = Number(
      emulator.protocolParameters.maxTxExSteps / seedMeasurement.executionSteps,
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

    // The genuine maximum and its immediately adjacent control are exact, not
    // merely "whatever the search returned".
    expect(acceptedCount).toBe(MAXIMUM_REDEEMER_ACCEPTED_COUNT_V1);
    expect(boundary.accepted.signedBytes).toBe(
      MAXIMUM_REDEEMER_ACCEPTED_SIGNED_BYTES_V1,
    );
    expect(adjacentCount).toBe(MAXIMUM_REDEEMER_ADJACENT_COUNT_V1);
    expect(boundary.adjacent.signedBytes).toBe(
      MAXIMUM_REDEEMER_ADJACENT_SIGNED_BYTES_V1,
    );
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
    expect(accepted.totalCollateral).toBe(CARDANO_BOUNDARY_TOTAL_COLLATERAL_V1);
    expect(adjacent.totalCollateral).toBe(CARDANO_BOUNDARY_TOTAL_COLLATERAL_V1);
    expect(accepted.vkeyWitnessCount).toBe(1);
    expect(adjacent.vkeyWitnessCount).toBe(1);
    expect(accepted.plutusV3ScriptCount).toBe(1);
    expect(adjacent.plutusV3ScriptCount).toBe(1);
    expect(accepted.redeemerTags).toEqual(
      Array.from({ length: acceptedCount }, () => CML.RedeemerTag.Spend),
    );
    expect(adjacent.redeemerTags).toEqual(
      Array.from({ length: adjacentCount }, () => CML.RedeemerTag.Spend),
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
    expect(new Set(accepted.redeemerIndexes).size).toBe(acceptedCount);
    expect(new Set(adjacent.redeemerIndexes).size).toBe(adjacentCount);
    expect(accepted.redeemerIndexes).toEqual(
      Array.from({ length: acceptedCount }, (_, index) => BigInt(index + 1)),
    );
    expect(adjacent.redeemerIndexes).toEqual(
      Array.from({ length: adjacentCount }, (_, index) => BigInt(index + 1)),
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

    const parallel = buildCollateralFreeMidgardSchemaParallelCandidateV1({
      collateralizedCardanoCborHex: boundary.accepted.cborHex,
      privateKeyBech32: spendingKey.to_bech32(),
    });
    expect(parallel.parallelRedeemersCborHex).toBe(
      parallel.collateralizedRedeemersCborHex,
    );
    const redeemerField = exerciseMidgardOrderedCollectionBoundaryV1({
      signedCardanoCborHex: parallel.cborHex,
      fieldIndex: 8,
    });
    expect(redeemerField.itemCount).toBe(acceptedCount);
    expect(redeemerField.revealStepCount).toBe(acceptedCount);
    expect(redeemerField.maxRevealBytes).toBeLessThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    const retainedDa = await exerciseMidgardRetainedDaBoundaryV1({
      signedCardanoCborHex: parallel.cborHex,
    });
    expect(retainedDa.normal.reconstructedCanonicalBytes).toBe(
      redeemerField.nativeCanonicalBytes,
    );
    expect(retainedDa.forced.reconstructedCanonicalBytes).toBe(
      redeemerField.nativeCanonicalBytes,
    );
    expect(retainedDa.normal.revealStepCount).toBe(
      redeemerField.completeFoldStepCount,
    );
    expect(retainedDa.forced.revealStepCount).toBe(
      redeemerField.completeFoldStepCount,
    );
    const retainedProjection =
      buildMidgardRetainedDaCanonicalScriptProjectionV1({
        canonicalTransactionCbor:
          cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
            Buffer.from(parallel.cborHex, "hex"),
          ),
      });
    const productionRetainedDa =
      await exerciseMidgardRetainedDaCanonicalBoundaryV1({
        canonicalTransactionCbor: retainedProjection.canonicalTransactionCbor,
        corpusLabel: "maximum-redeemers",
        canonicalMaterialSidecarCbor:
          retainedProjection.canonicalMaterialSidecarCbor,
        sourceRawScriptAuditHash: retainedProjection.sourceRawScriptAuditHash,
      });
    expect(productionRetainedDa.normal.reconstructedCanonicalBytes).toBe(
      retainedProjection.canonicalTransactionCbor.length,
    );
    expect(productionRetainedDa.forced.reconstructedCanonicalBytes).toBe(
      retainedProjection.canonicalTransactionCbor.length,
    );
    expect({
      fieldCommitmentHex: redeemerField.fieldCommitmentHex,
      fieldPreimageHashHex: redeemerField.fieldPreimageHashHex,
      transactionIdHex: redeemerField.terminalFoldVector.transactionIdHex,
      transactionCommitmentHex:
        redeemerField.terminalFoldVector.transactionCommitmentHex,
      preWorkRootHex: redeemerField.terminalFoldVector.preWorkRootHex,
      postWorkRootHex: redeemerField.terminalFoldVector.postWorkRootHex,
      encodedLengthBeforeItem:
        redeemerField.terminalFoldVector.encodedLengthBeforeItem,
      collectionProof: redeemerField.terminalFoldVector.collectionProof,
      chunkProof: redeemerField.terminalFoldVector.chunkProof,
    }).toEqual(maximumRedeemerTerminalFoldVectorV1);
    // This suite is the producer for the whole constant block of
    // `onchain/aiken/lib/midgard/fraud-proofs/native-tx.max-redeemers.test.ak`
    // (and for the two `validators/fraud-proofs/da-hash-preimage` steps' compact
    // forms). Publishing the vector after the assertions above is what lets
    // `generate-ordered-collection-boundary-aiken-goldens.mjs` rebind those
    // constants instead of a human retyping them (#588).
    publishAikenVectorV1("spend-redeemer-boundary-v1", {
      redeemerCount: redeemerField.itemCount,
      redeemerFieldBytes: redeemerField.fieldBytes,
      redeemerFieldPreimageCborHex: redeemerField.fieldPreimageCborHex,
      redeemerFieldPreimageHashHex: redeemerField.fieldPreimageHashHex,
      redeemerFieldCommitmentHex: redeemerField.fieldCommitmentHex,
      transactionIdHex: redeemerField.terminalFoldVector.transactionIdHex,
      transactionCommitmentHex:
        redeemerField.terminalFoldVector.transactionCommitmentHex,
      compactCborHex: redeemerField.terminalFoldVector.compactCborHex,
      witnessSetCompactCborHex:
        redeemerField.terminalFoldVector.witnessSetCompactCborHex,
      fieldPreimageLengthsCborHex:
        redeemerField.terminalFoldVector.fieldPreimageLengthsCborHex,
      validationContextCborHex:
        redeemerField.terminalFoldVector.validationContextCborHex,
      preWorkRootHex: redeemerField.terminalFoldVector.preWorkRootHex,
      postWorkRootHex: redeemerField.terminalFoldVector.postWorkRootHex,
      // The whole terminal fold, beyond the flat fields the generator binds
      // today. `native-tx.max-redeemers.test.ak`'s second test and the
      // `maximum_*_field_terminal_fixture_v1` family in
      // `validation-machine-v1.test.ak` still spell these proof structures out by
      // hand — they are struct literals inside functions rather than named
      // constants, so the name-keyed rebinder cannot reach them. Publishing them
      // here means the follow-up that gives them a producer has nothing left to
      // find out.
      terminalFoldVector: redeemerField.terminalFoldVector,
    });
    const parallelNative = decodeMidgardNativeTxFullV1FromCanonicalCbor(
      cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
        Buffer.from(parallel.cborHex, "hex"),
      ),
    );
    expect(
      parallelNative.witnessSet.redeemerTxWitsPreimageCbor.toString("hex"),
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
              maxTxExMem: emulator.protocolParameters.maxTxExMem.toString(),
              maxTxExSteps: emulator.protocolParameters.maxTxExSteps.toString(),
              perRedeemerMemory: seedMeasurement.executionMemory.toString(),
              perRedeemerSteps: seedMeasurement.executionSteps.toString(),
              maxRedeemersByMemory: maxByMemory,
              maxRedeemersBySteps: maxBySteps,
              requestedRedeemerCount: acceptedCount,
              actualInputCount: accepted.inputCount,
              actualRedeemerCount: accepted.redeemerCount,
              actualPlutusV3ScriptCount: accepted.plutusV3ScriptCount,
              actualVkeyWitnessCount: accepted.vkeyWitnessCount,
              signedCardanoBytes: boundary.accepted.signedBytes,
              byteMargin:
                emulator.protocolParameters.maxTxSize -
                boundary.accepted.signedBytes,
              executionMemory: accepted.executionMemory.toString(),
              executionMemoryMargin: (
                emulator.protocolParameters.maxTxExMem -
                accepted.executionMemory
              ).toString(),
              executionSteps: accepted.executionSteps.toString(),
              executionStepsMargin: (
                emulator.protocolParameters.maxTxExSteps -
                accepted.executionSteps
              ).toString(),
              fee: boundary.accepted.fee.toString(),
              totalCollateral: accepted.totalCollateral?.toString() ?? null,
              nativeCanonicalBytes: redeemerField.nativeCanonicalBytes,
              redeemerFieldBytes: redeemerField.fieldBytes,
              redeemerFieldCommitmentHex: redeemerField.fieldCommitmentHex,
              redeemerFieldPreimageHashHex: redeemerField.fieldPreimageHashHex,
              redeemerItems: redeemerField.itemCount,
              redeemerRevealSteps: redeemerField.revealStepCount,
              maxChunkBytes: redeemerField.maxChunkBytes,
              maxRevealBytes: redeemerField.maxRevealBytes,
              completeFoldSteps: redeemerField.completeFoldStepCount,
              productionCollateralRejection: collateralRejection,
              adjacentRequestedRedeemerCount: adjacentCount,
              adjacentActualInputCount: adjacent.inputCount,
              adjacentActualRedeemerCount: adjacent.redeemerCount,
              adjacentSignedCardanoBytes: boundary.adjacent.signedBytes,
              adjacentByteMargin:
                emulator.protocolParameters.maxTxSize -
                boundary.adjacent.signedBytes,
              adjacentExecutionMemory: adjacent.executionMemory.toString(),
              adjacentExecutionSteps: adjacent.executionSteps.toString(),
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
  }, 300_000);
});
