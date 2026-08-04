import { CML, Emulator } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildSignedCardanoSignersCandidateV1,
  CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
  deterministicCardanoBoundaryPrivateKeyV1,
  exerciseMidgardOrderedCollectionBoundaryV1,
  findSignedCardanoCollectionBoundaryV1,
  measureSignedCardanoSignersV1,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
} from "./helpers/ordered-collection-boundary-v1.js";
import { exerciseMidgardRetainedDaBoundaryV1 } from "./helpers/retained-da-boundary-v1.js";

const maximumRequiredSignerTerminalFoldVectorV1 = {
  transactionIdHex:
    "0297901027ac0e7df5aeefe14961ca4fcebcdf27d69bc9d9ab2638ee2c86b71e",
  transactionCommitmentHex:
    "c0ef3ffbbef5147e9ebc16ace943689df4f1ba1ef4521fc62af33fc8bb0d4b67",
  preWorkRootHex:
    "a9daba44283f35f0f8bd4a463aab8de3a8004e0c25518ed8fae31f0c8459a61d",
  postWorkRootHex:
    "2645969168012ec73dacba2a2fad3932d0af7eb0ee6721a5050d2eb21783e160",
  encodedLengthBeforeItem: 3_692,
  collectionProof: {
    fieldIndex: 4,
    itemCount: 124,
    itemIndex: 123,
    itemLength: 28,
    itemCommitmentHex:
      "feb8f7de321dc04604f1f371dbb874ae0937cb89cbc9b3725f65b394f66ba84c",
  },
  chunkProof: {
    fieldIndex: 4,
    itemIndex: 123,
    totalLength: 28,
    chunkIndex: 0,
    chunkHex: "38abf94805d076d7253d8386794096ec3d48fe233bc45d5edf97ab19",
  },
} as const;

describe("canonical V1 coupled signer/witness Cardano boundary", () => {
  it("derives and reveals fields 4 and 7 from one exact signed transaction", async () => {
    const spendingKey = deterministicCardanoBoundaryPrivateKeyV1(0);
    const funder = {
      seedPhrase: "",
      privateKey: spendingKey.to_bech32(),
      address: CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_pub_key(spendingKey.to_public().hash()),
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
      buildSignedCandidate: (requestedSignerCount) =>
        buildSignedCardanoSignersCandidateV1({
          inputTransactionId: "00".repeat(32),
          inputOutputIndex: 0n,
          inputLovelace: funder.assets.lovelace,
          recipientAddress: funder.address,
          requestedSignerCount,
          minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
          minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
          minFeeRefScriptCostPerByte:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeRefScriptCostPerByte,
        }),
    });
    const acceptedCardano = measureSignedCardanoSignersV1(
      boundary.accepted.cborHex,
    );
    const adjacentCardano = measureSignedCardanoSignersV1(
      boundary.adjacent.cborHex,
    );
    const signerField = exerciseMidgardOrderedCollectionBoundaryV1({
      signedCardanoCborHex: boundary.accepted.cborHex,
      fieldIndex: 4,
    });
    const witnessField = exerciseMidgardOrderedCollectionBoundaryV1({
      signedCardanoCborHex: boundary.accepted.cborHex,
      fieldIndex: 7,
    });
    const retainedDa = await exerciseMidgardRetainedDaBoundaryV1({
      signedCardanoCborHex: boundary.accepted.cborHex,
      corpusLabel: "maximum-signers-and-witnesses",
    });
    expect(retainedDa.normal.reconstructedCanonicalBytes).toBe(
      signerField.nativeCanonicalBytes,
    );
    expect(retainedDa.forced.reconstructedCanonicalBytes).toBe(
      signerField.nativeCanonicalBytes,
    );
    expect(retainedDa.normal.revealStepCount).toBe(
      signerField.completeFoldStepCount,
    );
    expect(retainedDa.forced.revealStepCount).toBe(
      signerField.completeFoldStepCount,
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
    expect(acceptedCardano.requiredSignerCount).toBe(
      boundary.accepted.requestedItemCount,
    );
    expect(acceptedCardano.vkeyWitnessCount).toBe(
      boundary.accepted.requestedItemCount,
    );
    expect(acceptedCardano.outputCount).toBe(1);
    expect(adjacentCardano.requiredSignerCount).toBe(
      boundary.adjacent.requestedItemCount,
    );
    expect(adjacentCardano.vkeyWitnessCount).toBe(
      boundary.adjacent.requestedItemCount,
    );
    expect(adjacentCardano.outputCount).toBe(1);
    expect(signerField.itemCount).toBe(acceptedCardano.requiredSignerCount);
    expect(signerField.revealStepCount).toBe(
      acceptedCardano.requiredSignerCount,
    );
    expect(witnessField.itemCount).toBe(acceptedCardano.vkeyWitnessCount);
    expect(witnessField.revealStepCount).toBe(acceptedCardano.vkeyWitnessCount);
    expect(signerField.completeFoldStepCount).toBe(
      witnessField.completeFoldStepCount,
    );
    expect(signerField.maxRevealBytes).toBeLessThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect(witnessField.maxRevealBytes).toBeLessThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect({
      transactionIdHex: signerField.terminalFoldVector.transactionIdHex,
      transactionCommitmentHex:
        signerField.terminalFoldVector.transactionCommitmentHex,
      preWorkRootHex: signerField.terminalFoldVector.preWorkRootHex,
      postWorkRootHex: signerField.terminalFoldVector.postWorkRootHex,
      encodedLengthBeforeItem:
        signerField.terminalFoldVector.encodedLengthBeforeItem,
      collectionProof: {
        fieldIndex: signerField.terminalFoldVector.collectionProof.fieldIndex,
        itemCount: signerField.terminalFoldVector.collectionProof.itemCount,
        itemIndex: signerField.terminalFoldVector.collectionProof.itemIndex,
        itemLength: signerField.terminalFoldVector.collectionProof.itemLength,
        itemCommitmentHex:
          signerField.terminalFoldVector.collectionProof.itemCommitmentHex,
      },
      chunkProof: {
        fieldIndex: signerField.terminalFoldVector.chunkProof.fieldIndex,
        itemIndex: signerField.terminalFoldVector.chunkProof.itemIndex,
        totalLength: signerField.terminalFoldVector.chunkProof.totalLength,
        chunkIndex: signerField.terminalFoldVector.chunkProof.chunkIndex,
        chunkHex: signerField.terminalFoldVector.chunkProof.chunkHex,
      },
    }).toEqual(maximumRequiredSignerTerminalFoldVectorV1);

    const txHash = await emulator.submitTx(boundary.accepted.cborHex);
    await expect(emulator.awaitTx(txHash)).resolves.toBe(true);

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            coupledSignerWitnessBoundaryV1: {
              fieldIndexes: [4, 7],
              fieldNames: ["required_signers", "address_witnesses"],
              maxTxSize: emulator.protocolParameters.maxTxSize,
              maxValueSize: emulator.protocolParameters.maxValSize,
              requestedSignerCount: boundary.accepted.requestedItemCount,
              actualRequiredSignerCount: acceptedCardano.requiredSignerCount,
              actualVkeyWitnessCount: acceptedCardano.vkeyWitnessCount,
              signedCardanoBytes: boundary.accepted.signedBytes,
              byteMargin:
                emulator.protocolParameters.maxTxSize -
                boundary.accepted.signedBytes,
              fee: boundary.accepted.fee.toString(),
              nativeCanonicalBytes: signerField.nativeCanonicalBytes,
              requiredSignersField: {
                bytes: signerField.fieldBytes,
                revealSteps: signerField.revealStepCount,
                maxChunkBytes: signerField.maxChunkBytes,
                maxRevealBytes: signerField.maxRevealBytes,
              },
              addressWitnessesField: {
                bytes: witnessField.fieldBytes,
                revealSteps: witnessField.revealStepCount,
                maxChunkBytes: witnessField.maxChunkBytes,
                maxRevealBytes: witnessField.maxRevealBytes,
              },
              completeFoldSteps: signerField.completeFoldStepCount,
              terminalFoldVector: signerField.terminalFoldVector,
              adjacentRequestedSignerCount:
                boundary.adjacent.requestedItemCount,
              adjacentRequiredSignerCount: adjacentCardano.requiredSignerCount,
              adjacentVkeyWitnessCount: adjacentCardano.vkeyWitnessCount,
              adjacentSignedCardanoBytes: boundary.adjacent.signedBytes,
              adjacentByteMargin:
                emulator.protocolParameters.maxTxSize -
                boundary.adjacent.signedBytes,
              adjacentFee: boundary.adjacent.fee.toString(),
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
