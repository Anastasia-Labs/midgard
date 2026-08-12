import { CML, Emulator } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { publishAikenVectorV1 } from "./helpers/aiken-vector-channel.js";
import {
  buildSignedCardanoOutputsCandidateV1,
  CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
  exerciseMidgardOrderedCollectionBoundaryV1,
  findSignedCardanoCollectionBoundaryV1,
  measureSignedCardanoOutputsV1,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
} from "./helpers/ordered-collection-boundary-v1.js";
import { exerciseMidgardRetainedDaBoundaryV1 } from "./helpers/retained-da-boundary-v1.js";

// The exact genuine signed-Cardano field-2 boundary. The terminal fold vector
// below is the Aiken-replayed half; these four numbers pin the cardinality and
// byte count the search must land on, so a silently shrunk outputs collection
// can no longer satisfy the relative bounds alone.
const MAXIMUM_OUTPUT_ACCEPTED_COUNT_V1 = 437;
const MAXIMUM_OUTPUT_ACCEPTED_SIGNED_BYTES_V1 = 16_372;
const MAXIMUM_OUTPUT_ADJACENT_COUNT_V1 = 438;
const MAXIMUM_OUTPUT_ADJACENT_SIGNED_BYTES_V1 = 16_409;

const maximumOutputTerminalFoldVectorV1 = {
  transactionIdHex:
    "1faba3bc592fac6c30165c2789e08a18357deb9b449f5d0ceea1b3e6aa40ee1e",
  transactionCommitmentHex:
    "89de7a79482046596d7a217e20889350a26e5cb168c9cb69fdca382b4a402c7a",
  preWorkRootHex:
    "c355c350925b94503664350b6b6d7f44c0776ef44caef0e40ebdf5d6936b29c2",
  postWorkRootHex:
    "b39febf97e07ab5bd85330521e7205f074b12e8e20e0dd2e39a40e5868fdfce1",
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

    // The genuine maximum and its immediately adjacent control are exact, not
    // merely "whatever the search returned".
    expect(boundary.accepted.requestedItemCount).toBe(
      MAXIMUM_OUTPUT_ACCEPTED_COUNT_V1,
    );
    expect(boundary.accepted.signedBytes).toBe(
      MAXIMUM_OUTPUT_ACCEPTED_SIGNED_BYTES_V1,
    );
    expect(boundary.adjacent.requestedItemCount).toBe(
      MAXIMUM_OUTPUT_ADJACENT_COUNT_V1,
    );
    expect(boundary.adjacent.signedBytes).toBe(
      MAXIMUM_OUTPUT_ADJACENT_SIGNED_BYTES_V1,
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
    // #590 scope item 0: the write channel this suite did not have.
    //
    // The `output-boundary-v1` fixture in
    // `onchain/aiken/lib/midgard/validation-machine-v1.test.ak` mirrors this
    // boundary's terminal fold, and until now nothing carried the bytes across —
    // so that fixture still pinned the *counted* field roots this package stopped
    // emitting at #585, and stayed green only because the id it pinned was the id
    // of the compact it pinned beside it. #592's rebind puts §8's carriage where
    // the counted `(ItemProofV1, ChunkProofV1)` pair used to be, and a carriage is
    // the field's whole §5.1 preimage, which no human is going to retype.
    //
    // Published after the assertions above, so the generator can only ever see a
    // vector this suite has already accepted.
    publishAikenVectorV1("output-boundary-v1", {
      fieldIndex: midgardMeasurement.terminalFoldVector.collectionProof.fieldIndex,
      itemCount: midgardMeasurement.terminalFoldVector.collectionProof.itemCount,
      itemIndex: midgardMeasurement.terminalFoldVector.collectionProof.itemIndex,
      terminalChunkIndex: midgardMeasurement.terminalFoldVector.chunkProof.chunkIndex,
      encodedLengthBeforeItem:
        midgardMeasurement.terminalFoldVector.encodedLengthBeforeItem,
      // §8.1's tier-1 carriage: the field's whole §5.1 preimage, which the door
      // hashes once against the flat commitment below.
      fieldPreimageCborHex: midgardMeasurement.fieldPreimageCborHex,
      fieldCommitmentHex: midgardMeasurement.fieldCommitmentHex,
      transactionIdHex: midgardMeasurement.terminalFoldVector.transactionIdHex,
      transactionCommitmentHex:
        midgardMeasurement.terminalFoldVector.transactionCommitmentHex,
      compactCborHex: midgardMeasurement.terminalFoldVector.compactCborHex,
      witnessSetCompactCborHex:
        midgardMeasurement.terminalFoldVector.witnessSetCompactCborHex,
      fieldPreimageLengthsCborHex:
        midgardMeasurement.terminalFoldVector.fieldPreimageLengthsCborHex,
      validationContextCborHex:
        midgardMeasurement.terminalFoldVector.validationContextCborHex,
      preWorkRootHex: midgardMeasurement.terminalFoldVector.preWorkRootHex,
      postWorkRootHex: midgardMeasurement.terminalFoldVector.postWorkRootHex,
    });


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
