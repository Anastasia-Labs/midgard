import { CML, Emulator } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { publishAikenVector } from "./helpers/aiken-vector-channel.js";
import {
  buildSignedCardanoSignersCandidate,
  CARDANO_BOUNDARY_MAX_TX_SIZE,
  deterministicCardanoBoundaryPrivateKey,
  exerciseMidgardOrderedCollectionBoundary,
  findSignedCardanoCollectionBoundary,
  measureSignedCardanoSigners,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
} from "./helpers/ordered-collection-boundary.js";
import { exerciseMidgardRetainedDaBoundary } from "./helpers/retained-da-boundary.js";

// The exact genuine signed-Cardano field-4/field-7 boundary. The terminal fold
// vector below is the Aiken-replayed half; these four numbers pin the coupled
// signer/witness cardinality and byte count the search must land on, so a
// silently shrunk collection can no longer satisfy the relative bounds alone.
const MAXIMUM_SIGNER_WITNESS_ACCEPTED_COUNT = 124;
const MAXIMUM_SIGNER_WITNESS_ACCEPTED_SIGNED_BYTES = 16_351;
const MAXIMUM_SIGNER_WITNESS_ADJACENT_COUNT = 125;
const MAXIMUM_SIGNER_WITNESS_ADJACENT_SIGNED_BYTES = 16_482;

const maximumRequiredSignerTerminalFoldVector = {
  transactionIdHex:
    "7b4e4657e0083544359f4398fb092c482766220cd53ad99b598239297d1e9813",
  transactionCommitmentHex:
    "56ba4f868c6e37a2eb67542110b3cf1b87007e6f88493b97faa1e276cd1943ea",
  preWorkRootHex:
    "c39fcbe1b9077c6a2242f2cec28b84ad23fadb5b4065248f6cd6ecaab49a2c03",
  postWorkRootHex:
    "f34d2c3452c0168fc935a7a0c9de927311c8bfd372a53d83d7df8e0c5effec37",
  encodedLengthBeforeItem: 3692,
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
    const spendingKey = deterministicCardanoBoundaryPrivateKey(0);
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
      PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
    );

    const boundary = await findSignedCardanoCollectionBoundary({
      maxTxSize: emulator.protocolParameters.maxTxSize,
      buildSignedCandidate: (requestedSignerCount) =>
        buildSignedCardanoSignersCandidate({
          inputTransactionId: "00".repeat(32),
          inputOutputIndex: 0n,
          inputLovelace: funder.assets.lovelace,
          recipientAddress: funder.address,
          requestedSignerCount,
          minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeA,
          minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeB,
          minFeeRefScriptCostPerByte:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeRefScriptCostPerByte,
        }),
    });
    const acceptedCardano = measureSignedCardanoSigners(
      boundary.accepted.cborHex,
    );
    const adjacentCardano = measureSignedCardanoSigners(
      boundary.adjacent.cborHex,
    );
    const signerField = exerciseMidgardOrderedCollectionBoundary({
      signedCardanoCborHex: boundary.accepted.cborHex,
      fieldIndex: 4,
    });
    const witnessField = exerciseMidgardOrderedCollectionBoundary({
      signedCardanoCborHex: boundary.accepted.cborHex,
      fieldIndex: 7,
    });
    const retainedDa = await exerciseMidgardRetainedDaBoundary({
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
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );
    expect(boundary.adjacent.requestedItemCount).toBe(
      boundary.accepted.requestedItemCount + 1,
    );
    expect(boundary.adjacent.signedBytes).toBeGreaterThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );
    expect(boundary.accepted.fee).toBe(
      BigInt(
        PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeA *
          boundary.accepted.signedBytes +
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeB,
      ),
    );
    expect(boundary.adjacent.fee).toBe(
      BigInt(
        PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeA *
          boundary.adjacent.signedBytes +
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeB,
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
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );
    expect(witnessField.maxRevealBytes).toBeLessThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );

    // The genuine maximum and its immediately adjacent control are exact, not
    // merely "whatever the search returned".
    expect(boundary.accepted.requestedItemCount).toBe(
      MAXIMUM_SIGNER_WITNESS_ACCEPTED_COUNT,
    );
    expect(boundary.accepted.signedBytes).toBe(
      MAXIMUM_SIGNER_WITNESS_ACCEPTED_SIGNED_BYTES,
    );
    expect(boundary.adjacent.requestedItemCount).toBe(
      MAXIMUM_SIGNER_WITNESS_ADJACENT_COUNT,
    );
    expect(boundary.adjacent.signedBytes).toBe(
      MAXIMUM_SIGNER_WITNESS_ADJACENT_SIGNED_BYTES,
    );
    expect(signerField.itemCount).toBe(MAXIMUM_SIGNER_WITNESS_ACCEPTED_COUNT);
    expect(witnessField.itemCount).toBe(MAXIMUM_SIGNER_WITNESS_ACCEPTED_COUNT);
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
    }).toEqual(maximumRequiredSignerTerminalFoldVector);
    // This suite is the producer for the C20-7 constant family in
    // `onchain/aiken/lib/midgard/fraud-proofs/native-tx-v1.test.ak` (and for the
    // signer-field terminal fixture in `validation-machine-v1.test.ak`). Publishing
    // the vector here — after every assertion above has already checked it — is
    // what lets `generate-ordered-collection-boundary-aiken-goldens.mjs` rebind
    // those constants instead of a human retyping them (#588).
    publishAikenVector("coupled-signer-witness-boundary-v1", {
      vkeyWitnessCount: boundary.accepted.requestedItemCount,
      acceptedSignedCardanoBytes: boundary.accepted.signedBytes,
      adjacentSignedCardanoBytes: boundary.adjacent.signedBytes,
      cardanoMaxTransactionBytes: CARDANO_BOUNDARY_MAX_TX_SIZE,
      addressWitnessFieldBytes: witnessField.fieldBytes,
      nativeCanonicalBytes: witnessField.nativeCanonicalBytes,
      addressWitnessFieldPreimageCborHex: witnessField.fieldPreimageCborHex,
      addressWitnessFieldPreimageHashHex: witnessField.fieldPreimageHashHex,
      addressWitnessFieldCommitmentHex: witnessField.fieldCommitmentHex,
      transactionIdHex: witnessField.terminalFoldVector.transactionIdHex,
      transactionCommitmentHex:
        witnessField.terminalFoldVector.transactionCommitmentHex,
      compactCborHex: witnessField.terminalFoldVector.compactCborHex,
      witnessSetCompactCborHex:
        witnessField.terminalFoldVector.witnessSetCompactCborHex,
      fieldPreimageLengthsCborHex:
        witnessField.terminalFoldVector.fieldPreimageLengthsCborHex,
      signerFieldTerminalFoldVector: signerField.terminalFoldVector,
      // #592: the field-4 terminal fixture in `validation-machine-v1.test.ak`
      // carries §8's tier-1 carriage now, which is the field's whole §5.1
      // preimage rather than a per-item opening. Published from the same
      // measurement the pinned expectations above already checked.
      signerFieldPreimageCborHex: signerField.fieldPreimageCborHex,
      signerFieldCommitmentHex: signerField.fieldCommitmentHex,
    });

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
