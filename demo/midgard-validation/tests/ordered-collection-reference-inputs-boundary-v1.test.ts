import { CML, Emulator } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildSignedCardanoReferenceInputsCandidateV1,
  CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
  deriveCardanoGenesisInputSupplyV1,
  deterministicCardanoBoundaryPrivateKeyV1,
  exerciseMidgardOrderedCollectionBoundaryV1,
  findSignedCardanoCollectionBoundaryV1,
  measureSignedCardanoReferenceInputsV1,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
} from "./helpers/ordered-collection-boundary-v1.js";
import { exerciseMidgardRetainedDaBoundaryV1 } from "./helpers/retained-da-boundary-v1.js";

describe("canonical V1 reference-inputs Cardano boundary", () => {
  it("derives and reveals field 1 using only distinct real emulator UTxOs", async () => {
    const spendingKey = deterministicCardanoBoundaryPrivateKeyV1(0);
    const address = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(
        spendingKey.to_public().hash(),
      ),
    )
      .to_address()
      .to_bech32();
    const inputSupply = deriveCardanoGenesisInputSupplyV1(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    const lovelacePerInput = 10_000_000n;
    const emulator = new Emulator(
      Array.from({ length: inputSupply }, () => ({
        seedPhrase: "",
        privateKey: spendingKey.to_bech32(),
        address,
        assets: { lovelace: lovelacePerInput },
      })),
      PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
    );
    const availableInputs = (
      await emulator.getUtxos(address)
    ).sort((left, right) => left.outputIndex - right.outputIndex);
    expect(availableInputs).toHaveLength(inputSupply);
    for (const [index, input] of availableInputs.entries()) {
      expect(input.txHash).toBe("00".repeat(32));
      expect(input.outputIndex).toBe(index);
    }

    const boundary = await findSignedCardanoCollectionBoundaryV1({
      maxTxSize: emulator.protocolParameters.maxTxSize,
      buildSignedCandidate: (requestedReferenceInputCount) =>
        buildSignedCardanoReferenceInputsCandidateV1({
          privateKeyBech32: spendingKey.to_bech32(),
          availableInputs,
          recipientAddress: address,
          requestedReferenceInputCount,
          minFeeA:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
          minFeeB:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
          minFeeRefScriptCostPerByte:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeRefScriptCostPerByte,
        }),
    });
    const acceptedCardano = measureSignedCardanoReferenceInputsV1(
      boundary.accepted.cborHex,
    );
    const adjacentCardano = measureSignedCardanoReferenceInputsV1(
      boundary.adjacent.cborHex,
    );
    const referenceInputField =
      exerciseMidgardOrderedCollectionBoundaryV1({
        signedCardanoCborHex: boundary.accepted.cborHex,
        fieldIndex: 1,
      });
    const retainedDa = await exerciseMidgardRetainedDaBoundaryV1({
      signedCardanoCborHex: boundary.accepted.cborHex,
    });
    expect(retainedDa.normal.reconstructedCanonicalBytes).toBe(
      referenceInputField.nativeCanonicalBytes,
    );
    expect(retainedDa.forced.reconstructedCanonicalBytes).toBe(
      referenceInputField.nativeCanonicalBytes,
    );
    expect(retainedDa.normal.revealStepCount).toBe(
      referenceInputField.completeFoldStepCount,
    );
    expect(retainedDa.forced.revealStepCount).toBe(
      referenceInputField.completeFoldStepCount,
    );

    const assertConcreteInputIndexes = (
      candidateCborHex: string,
      expectedReferenceInputCount: number,
    ): void => {
      const body = CML.Transaction.from_cbor_hex(
        candidateCborHex,
      ).body();
      expect(body.inputs().len()).toBe(1);
      expect(body.inputs().get(0).transaction_id().to_hex()).toBe(
        "00".repeat(32),
      );
      expect(body.inputs().get(0).index()).toBe(0n);
      const referenceInputs = body.reference_inputs();
      expect(referenceInputs?.len()).toBe(
        expectedReferenceInputCount,
      );
      for (
        let referenceIndex = 0;
        referenceIndex < expectedReferenceInputCount;
        referenceIndex += 1
      ) {
        const referenceInput = referenceInputs!.get(referenceIndex);
        expect(referenceInput.transaction_id().to_hex()).toBe(
          "00".repeat(32),
        );
        expect(referenceInput.index()).toBe(
          BigInt(referenceIndex + 1),
        );
      }
    };
    assertConcreteInputIndexes(
      boundary.accepted.cborHex,
      boundary.accepted.requestedItemCount,
    );
    assertConcreteInputIndexes(
      boundary.adjacent.cborHex,
      boundary.adjacent.requestedItemCount,
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
    expect(boundary.adjacent.requestedItemCount + 1).toBeLessThanOrEqual(
      inputSupply,
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
    expect(acceptedCardano.inputCount).toBe(1);
    expect(acceptedCardano.referenceInputCount).toBe(
      boundary.accepted.requestedItemCount,
    );
    expect(acceptedCardano.outputCount).toBe(1);
    expect(acceptedCardano.vkeyWitnessCount).toBe(1);
    expect(adjacentCardano.inputCount).toBe(1);
    expect(adjacentCardano.referenceInputCount).toBe(
      boundary.adjacent.requestedItemCount,
    );
    expect(adjacentCardano.outputCount).toBe(1);
    expect(adjacentCardano.vkeyWitnessCount).toBe(1);
    expect(referenceInputField.itemCount).toBe(
      acceptedCardano.referenceInputCount,
    );
    expect(referenceInputField.revealStepCount).toBe(
      acceptedCardano.referenceInputCount,
    );
    expect(referenceInputField.maxRevealBytes).toBeLessThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );

    const txHash = await emulator.submitTx(boundary.accepted.cborHex);
    await expect(emulator.awaitTx(txHash)).resolves.toBe(true);

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            referenceInputsBoundaryV1: {
              fieldIndex: 1,
              fieldName: "reference_inputs",
              maxTxSize:
                emulator.protocolParameters.maxTxSize,
              maxValueSize:
                emulator.protocolParameters.maxValSize,
              realGenesisInputSupply: inputSupply,
              fundingSpendIndex: 0,
              acceptedReferenceIndexes:
                `1..${boundary.accepted.requestedItemCount.toString()}`,
              adjacentReferenceIndexes:
                `1..${boundary.adjacent.requestedItemCount.toString()}`,
              requestedReferenceInputCount:
                boundary.accepted.requestedItemCount,
              actualSpendInputCount:
                acceptedCardano.inputCount,
              actualReferenceInputCount:
                acceptedCardano.referenceInputCount,
              actualOutputCount: acceptedCardano.outputCount,
              actualVkeyWitnessCount:
                acceptedCardano.vkeyWitnessCount,
              signedCardanoBytes: boundary.accepted.signedBytes,
              byteMargin:
                emulator.protocolParameters.maxTxSize -
                boundary.accepted.signedBytes,
              fee: boundary.accepted.fee.toString(),
              nativeCanonicalBytes:
                referenceInputField.nativeCanonicalBytes,
              referenceInputsFieldBytes:
                referenceInputField.fieldBytes,
              referenceInputItemCount:
                referenceInputField.itemCount,
              referenceInputRevealSteps:
                referenceInputField.revealStepCount,
              maxChunkBytes:
                referenceInputField.maxChunkBytes,
              maxRevealBytes:
                referenceInputField.maxRevealBytes,
              completeFoldSteps:
                referenceInputField.completeFoldStepCount,
              adjacentRequestedReferenceInputCount:
                boundary.adjacent.requestedItemCount,
              adjacentActualSpendInputCount:
                adjacentCardano.inputCount,
              adjacentActualReferenceInputCount:
                adjacentCardano.referenceInputCount,
              adjacentOutputCount:
                adjacentCardano.outputCount,
              adjacentVkeyWitnessCount:
                adjacentCardano.vkeyWitnessCount,
              adjacentSignedCardanoBytes:
                boundary.adjacent.signedBytes,
              adjacentByteMargin:
                emulator.protocolParameters.maxTxSize -
                boundary.adjacent.signedBytes,
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
