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

const maximumOutputTerminalFoldVectorV1 = {
  transactionIdHex:
    "851486b3f437bcae3712e1a2f0dbfab86062e4f0e8a3ed207607e1c0581c29e3",
  transactionCommitmentHex:
    "23ad248d5c89787009031bf83f08588bf60d221f57370f8e3bb99b35e3d5fa2a",
  preWorkRootHex:
    "dbff2136c653b5f01f9cc44095f2d9e72e93bbf5f87791c978c7424e2cad6030",
  postWorkRootHex:
    "3e54da6a779af01fccaba5ffe794ec0f678f22c415bf5a7c3309b3726ab5ff5c",
  encodedLengthBeforeItem: 18_794,
  collectionProof: {
    fieldIndex: 2,
    itemCount: 438,
    itemIndex: 437,
    itemLength: 45,
    itemCommitmentHex:
      "0597306fb7c06665c796780ac8c2c3dff11acb9d8081451b0b6841225b66502e",
  },
  chunkProof: {
    fieldIndex: 2,
    itemIndex: 437,
    totalLength: 45,
    chunkIndex: 0,
    chunkHex:
      "a200581d605ae193abe694a607531e20f85d8358ade9a474a4f45ac4e15e962da101821b000000091c0a049ba0",
  },
} as const;

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
    expect({
      transactionIdHex: midgardMeasurement.terminalFoldVector.transactionIdHex,
      transactionCommitmentHex:
        midgardMeasurement.terminalFoldVector.transactionCommitmentHex,
      preWorkRootHex: midgardMeasurement.terminalFoldVector.preWorkRootHex,
      postWorkRootHex: midgardMeasurement.terminalFoldVector.postWorkRootHex,
      encodedLengthBeforeItem:
        midgardMeasurement.terminalFoldVector.encodedLengthBeforeItem,
      collectionProof: {
        fieldIndex:
          midgardMeasurement.terminalFoldVector.collectionProof.fieldIndex,
        itemCount:
          midgardMeasurement.terminalFoldVector.collectionProof.itemCount,
        itemIndex:
          midgardMeasurement.terminalFoldVector.collectionProof.itemIndex,
        itemLength:
          midgardMeasurement.terminalFoldVector.collectionProof.itemLength,
        itemCommitmentHex:
          midgardMeasurement.terminalFoldVector.collectionProof
            .itemCommitmentHex,
      },
      chunkProof: {
        fieldIndex: midgardMeasurement.terminalFoldVector.chunkProof.fieldIndex,
        itemIndex: midgardMeasurement.terminalFoldVector.chunkProof.itemIndex,
        totalLength:
          midgardMeasurement.terminalFoldVector.chunkProof.totalLength,
        chunkIndex: midgardMeasurement.terminalFoldVector.chunkProof.chunkIndex,
        chunkHex: midgardMeasurement.terminalFoldVector.chunkProof.chunkHex,
      },
    }).toEqual(maximumOutputTerminalFoldVectorV1);

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
              terminalFoldVector: midgardMeasurement.terminalFoldVector,
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
