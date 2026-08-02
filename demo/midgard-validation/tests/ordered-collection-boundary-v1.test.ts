import { CML, Emulator } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildSignedCardanoOutputsCandidateV1,
  CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
  exerciseMidgardOrderedCollectionBoundaryV1,
  findSignedCardanoCollectionBoundaryV1,
  measureSignedCardanoOutputsV1,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
} from "./helpers/ordered-collection-boundary-v1.js";
import { exerciseMidgardRetainedDaBoundaryV1 } from "./helpers/retained-da-boundary-v1.js";

describe("canonical V1 ordered-collection Cardano boundaries", () => {
  it("derives and reveals the exact signed outputs boundary without a Midgard count cap", async () => {
    const privateKey = CML.PrivateKey.from_normal_bytes(
      Buffer.from("11".repeat(32), "hex"),
    );
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

    const boundary = await findSignedCardanoCollectionBoundaryV1({
      maxTxSize: emulator.protocolParameters.maxTxSize,
      buildSignedCandidate: (requestedOutputCount) =>
        buildSignedCardanoOutputsCandidateV1({
          privateKeyBech32: funder.privateKey,
          inputTransactionId: "00".repeat(32),
          inputOutputIndex: 0n,
          inputLovelace: funder.assets.lovelace,
          recipientAddress: funder.address,
          requestedOutputCount,
          lovelacePerOutput: 2_000_000n,
          minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
          minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
          minFeeRefScriptCostPerByte:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeRefScriptCostPerByte,
        }),
    });
    const cardanoMeasurement = measureSignedCardanoOutputsV1(
      boundary.accepted.cborHex,
    );
    const adjacentCardanoMeasurement = measureSignedCardanoOutputsV1(
      boundary.adjacent.cborHex,
    );
    const midgardMeasurement = exerciseMidgardOrderedCollectionBoundaryV1({
      signedCardanoCborHex: boundary.accepted.cborHex,
      fieldIndex: 2,
    });
    const retainedDa = await exerciseMidgardRetainedDaBoundaryV1({
      signedCardanoCborHex: boundary.accepted.cborHex,
      corpusLabel: "maximum-outputs",
    });
    expect(retainedDa.normal.reconstructedCanonicalBytes).toBe(
      midgardMeasurement.nativeCanonicalBytes,
    );
    expect(retainedDa.forced.reconstructedCanonicalBytes).toBe(
      midgardMeasurement.nativeCanonicalBytes,
    );
    expect(retainedDa.normal.revealStepCount).toBe(
      midgardMeasurement.completeFoldStepCount,
    );
    expect(retainedDa.forced.revealStepCount).toBe(
      midgardMeasurement.completeFoldStepCount,
    );

    expect(boundary.accepted.signedBytes).toBeLessThanOrEqual(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect(boundary.adjacent.requestedItemCount).toBe(
      boundary.accepted.requestedItemCount + 1,
    );
    expect(boundary.adjacent.signedBytes).toBeGreaterThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect(boundary.accepted.fee).toBe(
      BigInt(
        PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA *
          boundary.accepted.signedBytes +
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
      ),
    );
    expect(boundary.adjacent.fee).toBe(
      BigInt(
        PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA *
          boundary.adjacent.signedBytes +
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
      ),
    );
    expect(cardanoMeasurement.vkeyWitnessCount).toBe(1);
    expect(adjacentCardanoMeasurement.vkeyWitnessCount).toBe(1);
    expect(adjacentCardanoMeasurement.outputCount).toBe(
      cardanoMeasurement.outputCount + 1,
    );
    expect(midgardMeasurement.itemCount).toBe(cardanoMeasurement.outputCount);
    expect(midgardMeasurement.revealStepCount).toBe(
      cardanoMeasurement.outputCount,
    );
    expect(midgardMeasurement.maxRevealBytes).toBeLessThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );

    const txHash = await emulator.submitTx(boundary.accepted.cborHex);
    await expect(emulator.awaitTx(txHash)).resolves.toBe(true);

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            orderedCollectionBoundaryV1: {
              fieldIndex: 2,
              fieldName: "outputs",
              maxTxSize: emulator.protocolParameters.maxTxSize,
              maxValueSize: emulator.protocolParameters.maxValSize,
              requestedOutputCount: boundary.accepted.requestedItemCount,
              signedCardanoBytes: boundary.accepted.signedBytes,
              fee: boundary.accepted.fee.toString(),
              cardanoOutputCount: cardanoMeasurement.outputCount,
              nativeCanonicalBytes: midgardMeasurement.nativeCanonicalBytes,
              outputsFieldBytes: midgardMeasurement.fieldBytes,
              maxRevealBytes: midgardMeasurement.maxRevealBytes,
              maxChunkBytes: midgardMeasurement.maxChunkBytes,
              outputRevealSteps: midgardMeasurement.revealStepCount,
              completeFoldSteps: midgardMeasurement.completeFoldStepCount,
              adjacentRequestedOutputCount:
                boundary.adjacent.requestedItemCount,
              adjacentSignedCardanoBytes: boundary.adjacent.signedBytes,
              adjacentFee: boundary.adjacent.fee.toString(),
              adjacentCardanoOutputCount:
                adjacentCardanoMeasurement.outputCount,
              adjacentFailure: boundary.adjacentFailure,
              result: "PASS",
            },
          },
          null,
          2,
        ),
      );
    }
  }, 300_000);
});
