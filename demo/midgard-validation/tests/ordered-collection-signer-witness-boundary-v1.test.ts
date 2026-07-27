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

describe("canonical V1 coupled signer/witness Cardano boundary", () => {
  it("derives and reveals fields 4 and 6 from one exact signed transaction", async () => {
    const spendingKey = deterministicCardanoBoundaryPrivateKeyV1(0);
    const funder = {
      seedPhrase: "",
      privateKey: spendingKey.to_bech32(),
      address: CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_pub_key(
          spendingKey.to_public().hash(),
        ),
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
          minFeeA:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
          minFeeB:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
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
      fieldIndex: 6,
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
    expect(signerField.itemCount).toBe(
      acceptedCardano.requiredSignerCount,
    );
    expect(signerField.revealStepCount).toBe(
      acceptedCardano.requiredSignerCount,
    );
    expect(witnessField.itemCount).toBe(
      acceptedCardano.vkeyWitnessCount,
    );
    expect(witnessField.revealStepCount).toBe(
      acceptedCardano.vkeyWitnessCount,
    );
    expect(signerField.completeFoldStepCount).toBe(
      witnessField.completeFoldStepCount,
    );
    expect(signerField.maxRevealBytes).toBeLessThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect(witnessField.maxRevealBytes).toBeLessThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );

    const txHash = await emulator.submitTx(boundary.accepted.cborHex);
    await expect(emulator.awaitTx(txHash)).resolves.toBe(true);

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            coupledSignerWitnessBoundaryV1: {
              fieldIndexes: [4, 6],
              fieldNames: [
                "required_signers",
                "address_witnesses",
              ],
              maxTxSize:
                emulator.protocolParameters.maxTxSize,
              maxValueSize:
                emulator.protocolParameters.maxValSize,
              requestedSignerCount:
                boundary.accepted.requestedItemCount,
              actualRequiredSignerCount:
                acceptedCardano.requiredSignerCount,
              actualVkeyWitnessCount:
                acceptedCardano.vkeyWitnessCount,
              signedCardanoBytes: boundary.accepted.signedBytes,
              byteMargin:
                emulator.protocolParameters.maxTxSize -
                boundary.accepted.signedBytes,
              fee: boundary.accepted.fee.toString(),
              nativeCanonicalBytes:
                signerField.nativeCanonicalBytes,
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
              completeFoldSteps:
                signerField.completeFoldStepCount,
              adjacentRequestedSignerCount:
                boundary.adjacent.requestedItemCount,
              adjacentRequiredSignerCount:
                adjacentCardano.requiredSignerCount,
              adjacentVkeyWitnessCount:
                adjacentCardano.vkeyWitnessCount,
              adjacentSignedCardanoBytes:
                boundary.adjacent.signedBytes,
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
