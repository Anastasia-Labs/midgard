import {
  buildMidgardLedgerOutputProofTraceV1,
  buildMidgardLedgerOutputValueTraceV1,
  cardanoTxBytesToMidgardNativeTxCanonicalCborV1,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  decodeMidgardTxOutput,
  encodeMidgardLedgerOutputValueControlV1,
  finalizeMidgardLedgerOutputValueV1,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  type MidgardLedgerOutputAssetV1,
  midgardNativeTxFullToCardanoTxEncoding,
  midgardValueToCmlValue,
  validateMidgardConsensusV1Tx,
} from "@al-ft/midgard-core";
import { CML, Emulator } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildSignedCardanoNestedValueCandidateV1,
  CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
  CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1,
  CARDANO_BOUNDARY_NESTED_VALUE_ASSET_COUNT_V1,
  CARDANO_BOUNDARY_NESTED_VALUE_LOVELACE_V1,
  CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES_V1,
  CARDANO_BOUNDARY_PROTOCOL_MAJOR_V1,
  cardanoBoundaryNestedValueAssetsV1,
  deterministicCardanoBoundaryPrivateKeyV1,
  exerciseMidgardOrderedCollectionBoundaryV1,
  measureMidgardCompleteItemCarriageFitV1,
  measureSignedCardanoNestedValueV1,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
} from "./helpers/ordered-collection-boundary-v1.js";
import { exerciseMidgardRetainedDaBoundaryV1 } from "./helpers/retained-da-boundary-v1.js";

const maximumNestedValueTerminalVectorV1 = {
  protocolMajor: 11,
  maxTxSize: 16_384,
  maxValueSize: 5_000,
  signedCardanoBytes: 5_233,
  adjacentSignedCardanoBytes: 5_234,
  cardanoValueBytes: 5_000,
  adjacentCardanoValueBytes: 5_001,
  nativeCanonicalBytes: 5_329,
  outputsFieldBytes: 5_085,
  outputItemBytes: 5_034,
  outputProofSteps: 3_198,
  valueAssetCount: 1_592,
  valueProofSteps: 1_594,
  maxValueWitnessBytes: 358,
  valueFrontier: [
    {
      height: 3,
      hashHex:
        "d7186f5f4ba03f35771f5fe9a2bb1d98c1b6647fdb307a3a990674bdef9f44eb",
    },
    {
      height: 4,
      hashHex:
        "3b64e0dee5d4fb8dea89b2e43cec29e9d042e70855235cdf5ee5dfba032370c8",
    },
    {
      height: 5,
      hashHex:
        "99005b0619518750da865e276edd6ded5162e42e975b84ca5a0b39e2106d0c04",
    },
    {
      height: 9,
      hashHex:
        "e140a7bcc4ae85c668a78b45e94afac7712d80519d9d4e1ad42c1e09afd56c71",
    },
    {
      height: 10,
      hashHex:
        "d0f89d5bf0b4028db4ba39470ca2ff296bc5733936ddfef6beb77278f1003833",
    },
  ],
  preTerminalControlCborHex:
    "87010100581c1111111111111111111111111111111111111111111111111111111184582018a9e6706a9c0f115695a7d384af88baa135d31fa409e78ee3ab09ca6fe4cf8f18e41902ab1908e8845820ae48ba80db2f915cc4653c8bfd3a3394d4fe56f340dbe7cf66225478a2a93a65061910b6193620d87a80",
  terminalControlCborHex:
    "8701020040845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000845820bbcb3bff6f87a2005a336b6cb5fe5fbea093815716945279140f31aec8cbaba2000000d8799f83582035df7dc7ebdd5dba96f45ab79dbf23ba6a6325fc3b51f99b7c0cf62c8a31efb419138a193f46ff",
  terminalResult: {
    rootHex: "35df7dc7ebdd5dba96f45ab79dbf23ba6a6325fc3b51f99b7c0cf62c8a31efb4",
    cborLength: "5002",
    memory: "16198",
  },
  policyTransition: {
    controlCborHex:
      "87010018e4581c1212121212121212121212121212121212121212121212121212121284582018a9e6706a9c0f115695a7d384af88baa135d31fa409e78ee3ab09ca6fe4cf8f18e41902ab1908e8845820e6b1158ed70eadba4dd3edea999fbc8ce3f345de8a1c442c11d25f67245209b905190deb192d14d87a80",
    nextControlCborHex:
      "87010018e3581c11111111111111111111111111111111111111111111111111111111845820f3a575175904810deba1e2e614bb689c08dfd8797bfb1913783c8dd98ed55af501030a845820ae48ba80db2f915cc4653c8bfd3a3394d4fe56f340dbe7cf66225478a2a93a65061910b6193620d87a80",
    policyIdHex: "11111111111111111111111111111111111111111111111111111111",
    assetNameHex: "e2",
    quantity: "1",
    siblingHexes: [
      "c8ff3191003d91361e84195924aba18210e02790385d36debdcbd8f70bbe30ce",
      "e57595be0911f47b6ad9a4fc577e4dd80633ab1708ea007f1e10f2c046023271",
      "9bab8e0b298f6ba239d749199354e3b5077454fdc4c46e9be6ee181b57620a36",
      "ad95bca1382117e5c4150798dd59d0e50cb61f0ce24ae2cead5bb278d129863b",
      "b741a1bc87ac41225567abf47022d28aff8bef9010b3f555dd95cc37606f19cc",
      "4326fd84d7f7c1cb1c22a7d9be5cd7dd1e4e0cac784c895fe34a22252757e90f",
      "e3aac35704e9bbab8073bd8db99057348328982a5d454a7af9be0891419510ef",
      "16233275d44a0986f77f8f15153b441404e7248a0c844166968868f7d7bd996e",
      "9f1efec8e1306f877d11aa735c34e3ff4491ee38d5b428bc5d3b03c0bd33b490",
      "4ebf5ad0166b0ae660e33b8b4037a666ea07a55c72ffd3ec3b51e9cff53bdd1c",
    ],
  },
} as const;

const makeBoundaryEmulator = (
  requestedValueCborBytes: number,
): {
  readonly emulator: Emulator;
  readonly funder: {
    readonly seedPhrase: "";
    readonly privateKey: string;
    readonly address: string;
    readonly assets: Readonly<Record<string, bigint>>;
  };
} => {
  const privateKey = deterministicCardanoBoundaryPrivateKeyV1(0);
  const valueAssets = cardanoBoundaryNestedValueAssetsV1(
    requestedValueCborBytes,
  );
  const funder = {
    seedPhrase: "" as const,
    privateKey: privateKey.to_bech32(),
    address: CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(privateKey.to_public().hash()),
    )
      .to_address()
      .to_bech32(),
    assets: {
      lovelace: 40_000_000_000n,
      ...Object.fromEntries(
        valueAssets.map((asset) => [
          `${asset.policyIdHex}${asset.assetNameHex}`,
          asset.quantity,
        ]),
      ),
    },
  };
  return {
    emulator: new Emulator([funder], PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1),
    funder,
  };
};

const buildCandidate = async ({
  requestedValueCborBytes,
  funder,
}: {
  readonly requestedValueCborBytes: number;
  readonly funder: ReturnType<typeof makeBoundaryEmulator>["funder"];
}) =>
  buildSignedCardanoNestedValueCandidateV1({
    privateKeyBech32: funder.privateKey,
    inputTransactionId: "00".repeat(32),
    inputOutputIndex: 0n,
    inputLovelace: funder.assets.lovelace!,
    recipientAddress: funder.address,
    requestedValueCborBytes,
    minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
    minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
    minFeeRefScriptCostPerByte:
      PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeRefScriptCostPerByte,
  });

const exactAssetSemantics = (
  measurement: ReturnType<typeof measureSignedCardanoNestedValueV1>,
): readonly (readonly [string, string, bigint])[] =>
  measurement.assetNameHexes
    .map(
      (assetNameHex, index) =>
        [
          measurement.assetPolicyHashHexes[index]!,
          assetNameHex,
          measurement.assetQuantities[index]!,
        ] as const,
    )
    .sort(
      ([leftPolicy, leftName], [rightPolicy, rightName]) =>
        leftPolicy.localeCompare(rightPolicy) ||
        leftName.localeCompare(rightName),
    );

const expectedAssetSemantics = (
  requestedValueCborBytes: number,
): readonly (readonly [string, string, bigint])[] =>
  cardanoBoundaryNestedValueAssetsV1(requestedValueCborBytes).map(
    ({ policyIdHex, assetNameHex, quantity }) =>
      [policyIdHex, assetNameHex, quantity] as const,
  );

// Mirrors the official validateOutputTooBigUTxO size predicate after CML has
// produced the canonical Value bytes for the pinned protocol-major-11 shape.
const violatesCardanoOutputTooBigUtxoSnapshotV1 = (
  canonicalValueCborBytes: number,
): boolean => canonicalValueCborBytes > CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1;

describe("canonical V1 nested Cardano Value boundary", () => {
  it("retains and folds an exact 5,000-byte Value while rejecting 5,001 bytes", async () => {
    const acceptedEnvironment = makeBoundaryEmulator(
      CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1,
    );
    const adjacentEnvironment = makeBoundaryEmulator(
      CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1 + 1,
    );
    const acceptedCandidate = await buildCandidate({
      requestedValueCborBytes: CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1,
      funder: acceptedEnvironment.funder,
    });
    const adjacentCandidate = await buildCandidate({
      requestedValueCborBytes: CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1 + 1,
      funder: adjacentEnvironment.funder,
    });
    const accepted = measureSignedCardanoNestedValueV1(
      acceptedCandidate.cborHex,
    );
    const adjacent = measureSignedCardanoNestedValueV1(
      adjacentCandidate.cborHex,
    );

    expect(accepted.valueCborBytes).toBe(CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1);
    expect(adjacent.valueCborBytes).toBe(
      CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1 + 1,
    );
    expect(acceptedCandidate.signedBytes).toBeLessThanOrEqual(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect(adjacentCandidate.signedBytes).toBeLessThanOrEqual(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect({
      protocolMajor: CARDANO_BOUNDARY_PROTOCOL_MAJOR_V1,
      acceptedOutputTooBig: violatesCardanoOutputTooBigUtxoSnapshotV1(
        accepted.valueCborBytes,
      ),
      adjacentOutputTooBig: violatesCardanoOutputTooBigUtxoSnapshotV1(
        adjacent.valueCborBytes,
      ),
    }).toEqual({
      protocolMajor: 11,
      acceptedOutputTooBig: false,
      adjacentOutputTooBig: true,
    });
    expect(exactAssetSemantics(accepted)).toEqual(
      expectedAssetSemantics(CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1),
    );
    expect(exactAssetSemantics(adjacent)).toEqual(
      expectedAssetSemantics(CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1 + 1),
    );
    expect(accepted.assetQuantities.every((quantity) => quantity === 1n)).toBe(
      true,
    );
    expect(
      adjacent.assetQuantities.filter((quantity) => quantity === 24n),
    ).toHaveLength(1);
    const policyCount = CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES_V1.length;
    const acceptedValueLowerBound =
      7 +
      policyCount * 32 +
      CARDANO_BOUNDARY_NESTED_VALUE_ASSET_COUNT_V1 * 3 -
      policyCount;
    const sevenPolicyAdjacentCardinalityLowerBound =
      7 +
      policyCount * 32 +
      (CARDANO_BOUNDARY_NESTED_VALUE_ASSET_COUNT_V1 + 1) * 3 -
      policyCount;
    const atMostSixPolicyAdjacentCardinalityLowerBound = 7 + 6 * 803 + 51 * 4;
    const atLeastEightPolicyAdjacentCardinalityLowerBound =
      7 + 8 * 30 + (CARDANO_BOUNDARY_NESTED_VALUE_ASSET_COUNT_V1 + 1) * 3;
    expect(acceptedValueLowerBound).toBe(CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1);
    expect(sevenPolicyAdjacentCardinalityLowerBound).toBe(5_003);
    expect(atMostSixPolicyAdjacentCardinalityLowerBound).toBe(5_029);
    expect(atLeastEightPolicyAdjacentCardinalityLowerBound).toBe(5_026);
    expect({
      outputCount: accepted.outputCount,
      vkeyWitnessCount: accepted.vkeyWitnessCount,
      outputAddress: accepted.outputAddress,
      outputLovelace: accepted.outputLovelace,
      policyHashHexes: accepted.policyHashHexes,
      assetCount: accepted.assetNameHexes.length,
      hasWithdrawals: accepted.hasWithdrawals,
      hasMint: accepted.hasMint,
      hasPlutusScripts: accepted.hasPlutusScripts,
      hasRedeemers: accepted.hasRedeemers,
      hasDatums: accepted.hasDatums,
      collateralInputCount: accepted.collateralInputCount,
    }).toEqual({
      outputCount: 2,
      vkeyWitnessCount: 1,
      outputAddress: acceptedEnvironment.funder.address,
      outputLovelace: CARDANO_BOUNDARY_NESTED_VALUE_LOVELACE_V1,
      policyHashHexes: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES_V1,
      assetCount: CARDANO_BOUNDARY_NESTED_VALUE_ASSET_COUNT_V1,
      hasWithdrawals: false,
      hasMint: false,
      hasPlutusScripts: false,
      hasRedeemers: false,
      hasDatums: false,
      collateralInputCount: 0,
    });

    const acceptedCanonical = cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
      Buffer.from(acceptedCandidate.cborHex, "hex"),
    );
    const adjacentCanonical = cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
      Buffer.from(adjacentCandidate.cborHex, "hex"),
    );
    const acceptedNative =
      decodeMidgardNativeTxFullV1FromCanonicalCbor(acceptedCanonical);
    const adjacentNative =
      decodeMidgardNativeTxFullV1FromCanonicalCbor(adjacentCanonical);
    expect(
      validateMidgardConsensusV1Tx(acceptedNative, acceptedCanonical.length),
    ).toBeNull();
    expect(
      validateMidgardConsensusV1Tx(adjacentNative, adjacentCanonical.length),
    ).toMatchObject({
      code: "E_VALUE_SIZE",
      featureId: "output_value",
    });

    const outputCbors = decodeMidgardNativeByteListPreimage(
      acceptedNative.body.outputsPreimageCbor,
      "native.outputs",
    );
    expect(outputCbors).toHaveLength(2);
    const targetOutput = decodeMidgardTxOutput(outputCbors[0]!);
    expect(
      midgardValueToCmlValue(targetOutput.value).to_cbor_bytes().length,
    ).toBe(CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1);
    const valueAssets: MidgardLedgerOutputAssetV1[] = [
      ...targetOutput.value.assets.entries(),
    ].flatMap(([policyIdHex, policyAssets]) =>
      [...policyAssets.entries()].map(([assetNameHex, quantity]) => ({
        policyId: Buffer.from(policyIdHex, "hex"),
        assetName: Buffer.from(assetNameHex, "hex"),
        quantity,
      })),
    );
    expect(valueAssets).toHaveLength(
      CARDANO_BOUNDARY_NESTED_VALUE_ASSET_COUNT_V1,
    );

    const outputProof = buildMidgardLedgerOutputProofTraceV1({
      outputIndex: 0,
      outputCbor: outputCbors[0]!,
    });
    const chunkWitnesses = outputProof.steps.flatMap(({ witness }) =>
      witness?.kind === "chunks" ? [witness] : [],
    );
    const valueWitnesses = outputProof.steps.flatMap(({ witness }) =>
      witness?.kind === "value" ? [witness] : [],
    );
    expect(outputProof.terminal.outputScan.cardanoValueSize).toBe(
      CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1,
    );
    expect(outputProof.terminal.outputScan.assetFrontier.count).toBe(
      CARDANO_BOUNDARY_NESTED_VALUE_ASSET_COUNT_V1,
    );
    expect(valueWitnesses).toHaveLength(
      CARDANO_BOUNDARY_NESTED_VALUE_ASSET_COUNT_V1,
    );
    expect(chunkWitnesses.length).toBeGreaterThan(1);
    expect(
      Math.max(
        ...chunkWitnesses.flatMap((witness) => [
          witness.chunkProof.chunk.length,
          witness.nextChunkProof?.chunk.length ?? 0,
        ]),
      ),
    ).toBeLessThanOrEqual(MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1);
    const maxValueWitnessBytes = Math.max(
      ...valueWitnesses.map(
        (witness) =>
          witness.policyId.length +
          witness.assetName.length +
          9 +
          witness.siblings.length * 32,
      ),
    );
    expect(maxValueWitnessBytes).toBeLessThan(CARDANO_BOUNDARY_MAX_TX_SIZE_V1);

    const valueTrace = buildMidgardLedgerOutputValueTraceV1({
      assets: valueAssets,
      lovelace: targetOutput.value.lovelace,
    });
    expect(valueTrace.steps).toHaveLength(
      CARDANO_BOUNDARY_NESTED_VALUE_ASSET_COUNT_V1 + 2,
    );
    const policyTransitionSteps = valueTrace.steps.filter(
      ({ control, witness }) =>
        witness !== null &&
        control.currentPolicy.length > 0 &&
        !Buffer.from(witness.policyId).equals(control.currentPolicy),
    );
    expect(
      policyTransitionSteps.map(({ control, witness, next }) => ({
        fromPolicyHex: control.currentPolicy.toString("hex"),
        toPolicyHex: witness!.policyId.toString("hex"),
        remainingBefore: control.assetRemaining,
        remainingAfter: next.assetRemaining,
      })),
    ).toEqual([
      {
        fromPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES_V1[6],
        toPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES_V1[5],
        remainingBefore: 1_365,
        remainingAfter: 1_364,
      },
      {
        fromPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES_V1[5],
        toPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES_V1[4],
        remainingBefore: 1_138,
        remainingAfter: 1_137,
      },
      {
        fromPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES_V1[4],
        toPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES_V1[3],
        remainingBefore: 911,
        remainingAfter: 910,
      },
      {
        fromPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES_V1[3],
        toPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES_V1[2],
        remainingBefore: 684,
        remainingAfter: 683,
      },
      {
        fromPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES_V1[2],
        toPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES_V1[1],
        remainingBefore: 456,
        remainingAfter: 455,
      },
      {
        fromPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES_V1[1],
        toPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES_V1[0],
        remainingBefore: 228,
        remainingAfter: 227,
      },
    ]);
    const policyTransitionStep = policyTransitionSteps.at(-1)!;
    expect(
      encodeMidgardLedgerOutputValueControlV1(outputProof.terminal.value!),
    ).toEqual(encodeMidgardLedgerOutputValueControlV1(valueTrace.terminal));
    expect(
      finalizeMidgardLedgerOutputValueV1(valueTrace.terminal),
    ).not.toBeNull();
    const finalizeStep = valueTrace.steps.at(-1)!;
    const terminalResult = finalizeMidgardLedgerOutputValueV1(
      valueTrace.terminal,
    )!;
    const terminalVector = {
      protocolMajor: CARDANO_BOUNDARY_PROTOCOL_MAJOR_V1,
      maxTxSize: CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
      maxValueSize: CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1,
      signedCardanoBytes: acceptedCandidate.signedBytes,
      adjacentSignedCardanoBytes: adjacentCandidate.signedBytes,
      cardanoValueBytes: accepted.valueCborBytes,
      adjacentCardanoValueBytes: adjacent.valueCborBytes,
      nativeCanonicalBytes: acceptedCanonical.length,
      outputsFieldBytes: acceptedNative.body.outputsPreimageCbor.length,
      outputItemBytes: outputCbors[0]!.length,
      outputProofSteps: outputProof.steps.length,
      valueAssetCount: valueTrace.assets.length,
      valueProofSteps: valueTrace.steps.length,
      maxValueWitnessBytes,
      valueFrontier: valueTrace.frontier.peaks.map((peak) => ({
        height: peak.height,
        hashHex: peak.hash.toString("hex"),
      })),
      preTerminalControlCborHex: encodeMidgardLedgerOutputValueControlV1(
        finalizeStep.control,
      ).toString("hex"),
      terminalControlCborHex: encodeMidgardLedgerOutputValueControlV1(
        finalizeStep.next,
      ).toString("hex"),
      terminalResult: {
        rootHex: Buffer.from(terminalResult.root).toString("hex"),
        cborLength: terminalResult.cborLength.toString(),
        memory: terminalResult.memory.toString(),
      },
      policyTransition: {
        controlCborHex: encodeMidgardLedgerOutputValueControlV1(
          policyTransitionStep.control,
        ).toString("hex"),
        nextControlCborHex: encodeMidgardLedgerOutputValueControlV1(
          policyTransitionStep.next,
        ).toString("hex"),
        policyIdHex: policyTransitionStep.witness!.policyId.toString("hex"),
        assetNameHex: policyTransitionStep.witness!.assetName.toString("hex"),
        quantity: policyTransitionStep.witness!.quantity.toString(),
        siblingHexes: policyTransitionStep.witness!.siblings.map((sibling) =>
          Buffer.from(sibling).toString("hex"),
        ),
      },
    };
    if (process.env.MIDGARD_PRINT_AIKEN_VECTOR !== "1") {
      expect(terminalVector).toEqual(maximumNestedValueTerminalVectorV1);
    }

    const midgard = exerciseMidgardOrderedCollectionBoundaryV1({
      signedCardanoCborHex: acceptedCandidate.cborHex,
      fieldIndex: 2,
    });
    expect(midgard.itemCount).toBe(2);
    expect(midgard.maxChunkBytes).toBe(MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1);
    const retained = await exerciseMidgardRetainedDaBoundaryV1({
      signedCardanoCborHex: acceptedCandidate.cborHex,
      corpusLabel: "maximum-nested-value",
    });
    expect(retained.normal.reconstructedCanonicalBytes).toBe(
      acceptedCanonical.length,
    );
    expect(retained.forced.reconstructedCanonicalBytes).toBe(
      acceptedCanonical.length,
    );
    expect(retained.normal.revealStepCount).toBe(midgard.completeFoldStepCount);
    expect(retained.forced.revealStepCount).toBe(midgard.completeFoldStepCount);

    const roundtrip = measureSignedCardanoNestedValueV1(
      Buffer.from(
        midgardNativeTxFullToCardanoTxEncoding(acceptedNative),
      ).toString("hex"),
    );
    expect({
      outputAddress: roundtrip.outputAddress,
      outputLovelace: roundtrip.outputLovelace,
      valueCborBytes: roundtrip.valueCborBytes,
      policyHashHexes: roundtrip.policyHashHexes,
      assets: exactAssetSemantics(roundtrip),
    }).toEqual({
      outputAddress: accepted.outputAddress,
      outputLovelace: accepted.outputLovelace,
      valueCborBytes: accepted.valueCborBytes,
      policyHashHexes: accepted.policyHashHexes,
      assets: exactAssetSemantics(accepted),
    });

    const txHash = await acceptedEnvironment.emulator.submitTx(
      acceptedCandidate.cborHex,
    );
    await expect(acceptedEnvironment.emulator.awaitTx(txHash)).resolves.toBe(
      true,
    );

    if (process.env.MIDGARD_PRINT_AIKEN_VECTOR === "1") {
      console.info(
        JSON.stringify({
          nestedValueBoundaryV1: terminalVector,
        }),
      );
    }
  }, 300_000);

  // §3.2 complete-item-first ordering for C22. The maximum nested Value is a
  // whole output item, so its proof carriage must be measured complete —
  // direct and single-publication reference — before any incremental Value
  // fallback is even considered. The incremental Value fold above stays a
  // capability, not a necessity: this case proves both complete routes admit
  // the exact 5,000-byte Value, so no §3.2 necessity artifact is owed for it.
  it("fits the complete maximum-Value output item into direct and reference carriage before any fallback", async () => {
    const environment = makeBoundaryEmulator(
      CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1,
    );
    const candidate = await buildCandidate({
      requestedValueCborBytes: CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1,
      funder: environment.funder,
    });
    const canonical = cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
      Buffer.from(candidate.cborHex, "hex"),
    );
    const native = decodeMidgardNativeTxFullV1FromCanonicalCbor(canonical);
    expect(validateMidgardConsensusV1Tx(native, canonical.length)).toBeNull();
    const outputCbors = decodeMidgardNativeByteListPreimage(
      native.body.outputsPreimageCbor,
      "native.outputs",
    );
    const outputItem = outputCbors[0]!;
    // The complete item is the whole output, Value included, at the exact
    // 5,000-byte Value maximum pinned by the boundary case above.
    expect(
      midgardValueToCmlValue(
        decodeMidgardTxOutput(outputItem).value,
      ).to_cbor_bytes().length,
    ).toBe(CARDANO_BOUNDARY_MAX_VALUE_SIZE_V1);
    expect(outputItem.length).toBe(
      maximumNestedValueTerminalVectorV1.outputItemBytes,
    );

    const fit = measureMidgardCompleteItemCarriageFitV1({
      fieldIndex: 2,
      itemIndex: 0,
      itemCbor: outputItem,
    });
    expect(fit).toMatchObject({
      fieldIndex: 2,
      itemIndex: 0,
      itemBytes: maximumNestedValueTerminalVectorV1.outputItemBytes,
      carriage: "direct",
      fitsDirectCarriage: true,
      fitsSinglePublicationCarriage: true,
      requiresBoundedFallback: false,
    });
    expect(fit.itemBytes).toBeLessThanOrEqual(
      fit.maxReliableDirectCompleteItemBytes,
    );
    expect(fit.itemBytes).toBeLessThanOrEqual(
      fit.maxSinglePublicationCompleteItemBytes,
    );
    expect(fit.publicationTransactionBytes).toBeLessThanOrEqual(
      fit.maxL1TransactionBytes,
    );
    expect(fit.publicationDatumBytes).toBeGreaterThan(fit.itemBytes);
    // A bounded fallback would have to split the same item; it is available
    // but unnecessary, which is exactly what §3.2 requires us to measure.
    expect(fit.boundedFallbackChunkCount).toBeGreaterThan(1);
    expect(fit.commitmentHex).toMatch(/^[0-9a-f]{64}$/u);

    // Control: the item bound is not vacuous. An item one byte above the
    // measured single-publication envelope has no complete route at all.
    const oversized = measureMidgardCompleteItemCarriageFitV1({
      fieldIndex: 2,
      itemIndex: 0,
      itemCbor: Buffer.alloc(
        fit.maxSinglePublicationCompleteItemBytes + 1,
        0xa5,
      ),
    });
    expect(oversized).toMatchObject({
      fitsDirectCarriage: false,
      fitsSinglePublicationCarriage: false,
      requiresBoundedFallback: true,
    });

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify({ maximumNestedValueCompleteItemFitV1: fit }),
      );
    }
  }, 300_000);
});
