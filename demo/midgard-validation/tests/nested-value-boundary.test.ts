import {
  buildMidgardLedgerOutputProofTrace,
  buildMidgardLedgerOutputValueTrace,
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  decodeMidgardNativeByteListPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  decodeMidgardTxOutput,
  encodeMidgardLedgerOutputValueControl,
  finalizeMidgardLedgerOutputValue,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  type MidgardLedgerOutputAsset,
  midgardNativeTxFullToCardanoTxEncoding,
  midgardValueToCmlValue,
  validateMidgardConsensusTx,
} from "@al-ft/midgard-core";
import { CML, Emulator } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { publishAikenVector } from "./helpers/aiken-vector-channel.js";
import {
  buildSignedCardanoNestedValueCandidate,
  CARDANO_BOUNDARY_MAX_TX_SIZE,
  CARDANO_BOUNDARY_MAX_VALUE_SIZE,
  CARDANO_BOUNDARY_NESTED_VALUE_ASSET_COUNT,
  CARDANO_BOUNDARY_NESTED_VALUE_LOVELACE,
  CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES,
  CARDANO_BOUNDARY_PROTOCOL_MAJOR,
  cardanoBoundaryNestedValueAssets,
  deterministicCardanoBoundaryPrivateKey,
  exerciseMidgardOrderedCollectionBoundary,
  measureMidgardCompleteItemCarriageFit,
  measureSignedCardanoNestedValue,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
} from "./helpers/ordered-collection-boundary.js";
import { exerciseMidgardRetainedDaBoundary } from "./helpers/retained-da-boundary.js";

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
  const privateKey = deterministicCardanoBoundaryPrivateKey(0);
  const valueAssets = cardanoBoundaryNestedValueAssets(requestedValueCborBytes);
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
    emulator: new Emulator([funder], PREPROD_EPOCH_303_BOUNDARY_PARAMETERS),
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
  buildSignedCardanoNestedValueCandidate({
    privateKeyBech32: funder.privateKey,
    inputTransactionId: "00".repeat(32),
    inputOutputIndex: 0n,
    inputLovelace: funder.assets.lovelace!,
    recipientAddress: funder.address,
    requestedValueCborBytes,
    minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeA,
    minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeB,
    minFeeRefScriptCostPerByte:
      PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeRefScriptCostPerByte,
  });

const exactAssetSemantics = (
  measurement: ReturnType<typeof measureSignedCardanoNestedValue>,
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
  cardanoBoundaryNestedValueAssets(requestedValueCborBytes).map(
    ({ policyIdHex, assetNameHex, quantity }) =>
      [policyIdHex, assetNameHex, quantity] as const,
  );

// Mirrors the official validateOutputTooBigUTxO size predicate after CML has
// produced the canonical Value bytes for the pinned protocol-major-11 shape.
const violatesCardanoOutputTooBigUtxoSnapshot = (
  canonicalValueCborBytes: number,
): boolean => canonicalValueCborBytes > CARDANO_BOUNDARY_MAX_VALUE_SIZE;

describe("canonical V1 nested Cardano Value boundary", () => {
  it("retains and folds an exact 5,000-byte Value while rejecting 5,001 bytes", async () => {
    const acceptedEnvironment = makeBoundaryEmulator(
      CARDANO_BOUNDARY_MAX_VALUE_SIZE,
    );
    const adjacentEnvironment = makeBoundaryEmulator(
      CARDANO_BOUNDARY_MAX_VALUE_SIZE + 1,
    );
    const acceptedCandidate = await buildCandidate({
      requestedValueCborBytes: CARDANO_BOUNDARY_MAX_VALUE_SIZE,
      funder: acceptedEnvironment.funder,
    });
    const adjacentCandidate = await buildCandidate({
      requestedValueCborBytes: CARDANO_BOUNDARY_MAX_VALUE_SIZE + 1,
      funder: adjacentEnvironment.funder,
    });
    const accepted = measureSignedCardanoNestedValue(acceptedCandidate.cborHex);
    const adjacent = measureSignedCardanoNestedValue(adjacentCandidate.cborHex);

    expect(accepted.valueCborBytes).toBe(CARDANO_BOUNDARY_MAX_VALUE_SIZE);
    expect(adjacent.valueCborBytes).toBe(CARDANO_BOUNDARY_MAX_VALUE_SIZE + 1);
    expect(acceptedCandidate.signedBytes).toBeLessThanOrEqual(
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );
    expect(adjacentCandidate.signedBytes).toBeLessThanOrEqual(
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );
    expect({
      protocolMajor: CARDANO_BOUNDARY_PROTOCOL_MAJOR,
      acceptedOutputTooBig: violatesCardanoOutputTooBigUtxoSnapshot(
        accepted.valueCborBytes,
      ),
      adjacentOutputTooBig: violatesCardanoOutputTooBigUtxoSnapshot(
        adjacent.valueCborBytes,
      ),
    }).toEqual({
      protocolMajor: 11,
      acceptedOutputTooBig: false,
      adjacentOutputTooBig: true,
    });
    expect(exactAssetSemantics(accepted)).toEqual(
      expectedAssetSemantics(CARDANO_BOUNDARY_MAX_VALUE_SIZE),
    );
    expect(exactAssetSemantics(adjacent)).toEqual(
      expectedAssetSemantics(CARDANO_BOUNDARY_MAX_VALUE_SIZE + 1),
    );
    expect(accepted.assetQuantities.every((quantity) => quantity === 1n)).toBe(
      true,
    );
    expect(
      adjacent.assetQuantities.filter((quantity) => quantity === 24n),
    ).toHaveLength(1);
    const policyCount = CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES.length;
    const acceptedValueLowerBound =
      7 +
      policyCount * 32 +
      CARDANO_BOUNDARY_NESTED_VALUE_ASSET_COUNT * 3 -
      policyCount;
    const sevenPolicyAdjacentCardinalityLowerBound =
      7 +
      policyCount * 32 +
      (CARDANO_BOUNDARY_NESTED_VALUE_ASSET_COUNT + 1) * 3 -
      policyCount;
    const atMostSixPolicyAdjacentCardinalityLowerBound = 7 + 6 * 803 + 51 * 4;
    const atLeastEightPolicyAdjacentCardinalityLowerBound =
      7 + 8 * 30 + (CARDANO_BOUNDARY_NESTED_VALUE_ASSET_COUNT + 1) * 3;
    expect(acceptedValueLowerBound).toBe(CARDANO_BOUNDARY_MAX_VALUE_SIZE);
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
      outputLovelace: CARDANO_BOUNDARY_NESTED_VALUE_LOVELACE,
      policyHashHexes: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES,
      assetCount: CARDANO_BOUNDARY_NESTED_VALUE_ASSET_COUNT,
      hasWithdrawals: false,
      hasMint: false,
      hasPlutusScripts: false,
      hasRedeemers: false,
      hasDatums: false,
      collateralInputCount: 0,
    });

    const acceptedCanonical = cardanoTxBytesToMidgardNativeTxCanonicalCbor(
      Buffer.from(acceptedCandidate.cborHex, "hex"),
    );
    const adjacentCanonical = cardanoTxBytesToMidgardNativeTxCanonicalCbor(
      Buffer.from(adjacentCandidate.cborHex, "hex"),
    );
    const acceptedNative =
      decodeMidgardNativeTxFullFromCanonicalCbor(acceptedCanonical);
    const adjacentNative =
      decodeMidgardNativeTxFullFromCanonicalCbor(adjacentCanonical);
    expect(
      validateMidgardConsensusTx(acceptedNative, acceptedCanonical.length),
    ).toBeNull();
    expect(
      validateMidgardConsensusTx(adjacentNative, adjacentCanonical.length),
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
    ).toBe(CARDANO_BOUNDARY_MAX_VALUE_SIZE);
    const valueAssets: MidgardLedgerOutputAsset[] = [
      ...targetOutput.value.assets.entries(),
    ].flatMap(([policyIdHex, policyAssets]) =>
      [...policyAssets.entries()].map(([assetNameHex, quantity]) => ({
        policyId: Buffer.from(policyIdHex, "hex"),
        assetName: Buffer.from(assetNameHex, "hex"),
        quantity,
      })),
    );
    expect(valueAssets).toHaveLength(CARDANO_BOUNDARY_NESTED_VALUE_ASSET_COUNT);

    const outputProof = buildMidgardLedgerOutputProofTrace({
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
      CARDANO_BOUNDARY_MAX_VALUE_SIZE,
    );
    expect(outputProof.terminal.outputScan.assetFrontier.count).toBe(
      CARDANO_BOUNDARY_NESTED_VALUE_ASSET_COUNT,
    );
    expect(valueWitnesses).toHaveLength(
      CARDANO_BOUNDARY_NESTED_VALUE_ASSET_COUNT,
    );
    expect(chunkWitnesses.length).toBeGreaterThan(1);
    expect(
      Math.max(
        ...chunkWitnesses.flatMap((witness) => [
          witness.chunkProof.chunk.length,
          witness.nextChunkProof?.chunk.length ?? 0,
        ]),
      ),
    ).toBeLessThanOrEqual(MIDGARD_BOUNDED_ITEM_CHUNK_BYTES);
    const maxValueWitnessBytes = Math.max(
      ...valueWitnesses.map(
        (witness) =>
          witness.policyId.length +
          witness.assetName.length +
          9 +
          witness.siblings.length * 32,
      ),
    );
    expect(maxValueWitnessBytes).toBeLessThan(CARDANO_BOUNDARY_MAX_TX_SIZE);

    const valueTrace = buildMidgardLedgerOutputValueTrace({
      assets: valueAssets,
      lovelace: targetOutput.value.lovelace,
    });
    expect(valueTrace.steps).toHaveLength(
      CARDANO_BOUNDARY_NESTED_VALUE_ASSET_COUNT + 2,
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
        fromPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES[6],
        toPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES[5],
        remainingBefore: 1_365,
        remainingAfter: 1_364,
      },
      {
        fromPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES[5],
        toPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES[4],
        remainingBefore: 1_138,
        remainingAfter: 1_137,
      },
      {
        fromPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES[4],
        toPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES[3],
        remainingBefore: 911,
        remainingAfter: 910,
      },
      {
        fromPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES[3],
        toPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES[2],
        remainingBefore: 684,
        remainingAfter: 683,
      },
      {
        fromPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES[2],
        toPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES[1],
        remainingBefore: 456,
        remainingAfter: 455,
      },
      {
        fromPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES[1],
        toPolicyHex: CARDANO_BOUNDARY_NESTED_VALUE_POLICY_ID_HEXES[0],
        remainingBefore: 228,
        remainingAfter: 227,
      },
    ]);
    const policyTransitionStep = policyTransitionSteps.at(-1)!;
    expect(
      encodeMidgardLedgerOutputValueControl(outputProof.terminal.value!),
    ).toEqual(encodeMidgardLedgerOutputValueControl(valueTrace.terminal));
    expect(
      finalizeMidgardLedgerOutputValue(valueTrace.terminal),
    ).not.toBeNull();
    const finalizeStep = valueTrace.steps.at(-1)!;
    const terminalResult = finalizeMidgardLedgerOutputValue(
      valueTrace.terminal,
    )!;
    const terminalVector = {
      protocolMajor: CARDANO_BOUNDARY_PROTOCOL_MAJOR,
      maxTxSize: CARDANO_BOUNDARY_MAX_TX_SIZE,
      maxValueSize: CARDANO_BOUNDARY_MAX_VALUE_SIZE,
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
      preTerminalControlCborHex: encodeMidgardLedgerOutputValueControl(
        finalizeStep.control,
      ).toString("hex"),
      terminalControlCborHex: encodeMidgardLedgerOutputValueControl(
        finalizeStep.next,
      ).toString("hex"),
      terminalResult: {
        rootHex: Buffer.from(terminalResult.root).toString("hex"),
        cborLength: terminalResult.cborLength.toString(),
        memory: terminalResult.memory.toString(),
      },
      policyTransition: {
        controlCborHex: encodeMidgardLedgerOutputValueControl(
          policyTransitionStep.control,
        ).toString("hex"),
        nextControlCborHex: encodeMidgardLedgerOutputValueControl(
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
    // The Aiken twin's `typescript_maximum_value_*` constants are rebound from
    // this vector by `scripts/generate-nested-boundary-aiken-goldens.mjs`,
    // whose `--check` run is a required CI job. Publishing happens after every
    // assertion above, so the generator can only ever see a vector this suite
    // has already accepted, and no environment variable can remove an
    // assertion.
    publishAikenVector("nested-value-boundary-v1", {
      ...terminalVector,
      lovelace: CARDANO_BOUNDARY_NESTED_VALUE_LOVELACE.toString(),
    });

    const midgard = exerciseMidgardOrderedCollectionBoundary({
      signedCardanoCborHex: acceptedCandidate.cborHex,
      fieldIndex: 2,
    });
    expect(midgard.itemCount).toBe(2);
    expect(midgard.maxChunkBytes).toBe(MIDGARD_BOUNDED_ITEM_CHUNK_BYTES);
    const retained = await exerciseMidgardRetainedDaBoundary({
      signedCardanoCborHex: acceptedCandidate.cborHex,
      corpusLabel: "maximum-nested-value",
    });
    expect(retained.normal.reconstructedCanonicalBytes).toBe(
      acceptedCanonical.length,
    );
    expect(retained.forced.reconstructedCanonicalBytes).toBe(
      acceptedCanonical.length - 1,
    );
    expect(retained.normal.revealStepCount).toBe(midgard.completeFoldStepCount);
    expect(retained.forced.revealStepCount).toBe(midgard.completeFoldStepCount);

    const roundtrip = measureSignedCardanoNestedValue(
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
  }, 300_000);

  // §3.2 complete-item-first ordering for C22. The maximum nested Value is a
  // whole output item, so its proof carriage must be measured complete —
  // direct and single-publication reference — before any incremental Value
  // fallback is even considered. The incremental Value fold above stays a
  // capability, not a necessity: this case proves both complete routes admit
  // the exact 5,000-byte Value, so no §3.2 necessity artifact is owed for it.
  it("fits the complete maximum-Value output item into direct and reference carriage before any fallback", async () => {
    const environment = makeBoundaryEmulator(CARDANO_BOUNDARY_MAX_VALUE_SIZE);
    const candidate = await buildCandidate({
      requestedValueCborBytes: CARDANO_BOUNDARY_MAX_VALUE_SIZE,
      funder: environment.funder,
    });
    const canonical = cardanoTxBytesToMidgardNativeTxCanonicalCbor(
      Buffer.from(candidate.cborHex, "hex"),
    );
    const native = decodeMidgardNativeTxFullFromCanonicalCbor(canonical);
    expect(validateMidgardConsensusTx(native, canonical.length)).toBeNull();
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
    ).toBe(CARDANO_BOUNDARY_MAX_VALUE_SIZE);
    // The item's size is fixed by the exact Value maximum asserted above plus
    // the output's own framing, so it is stated as that relation rather than as
    // a transcribed byte count.
    expect(outputItem.length).toBeGreaterThan(CARDANO_BOUNDARY_MAX_VALUE_SIZE);
    expect(outputItem.length).toBeLessThan(
      CARDANO_BOUNDARY_MAX_VALUE_SIZE + 128,
    );

    const fit = measureMidgardCompleteItemCarriageFit({
      fieldIndex: 2,
      itemIndex: 0,
      itemCbor: outputItem,
    });
    expect(fit).toMatchObject({
      fieldIndex: 2,
      itemIndex: 0,
      itemBytes: outputItem.length,
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
    const oversized = measureMidgardCompleteItemCarriageFit({
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
