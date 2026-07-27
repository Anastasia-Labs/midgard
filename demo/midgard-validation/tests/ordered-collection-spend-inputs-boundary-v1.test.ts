import { CML, Emulator } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildSignedCardanoSpendInputsCandidateV1,
  CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
  deriveCardanoGenesisInputSupplyV1,
  deterministicCardanoBoundaryPrivateKeyV1,
  exerciseMidgardOrderedCollectionBoundaryV1,
  findSignedCardanoCollectionBoundaryV1,
  measureSignedCardanoSpendInputsV1,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
} from "./helpers/ordered-collection-boundary-v1.js";
import { exerciseMidgardRetainedDaBoundaryV1 } from "./helpers/retained-da-boundary-v1.js";

describe("canonical V1 spend-inputs Cardano boundary", () => {
  it("derives and reveals field 0 using only real emulator UTxOs", async () => {
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
      buildSignedCandidate: (requestedInputCount) =>
        buildSignedCardanoSpendInputsCandidateV1({
          privateKeyBech32: spendingKey.to_bech32(),
          availableInputs,
          recipientAddress: address,
          requestedInputCount,
          minFeeA:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
          minFeeB:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
          minFeeRefScriptCostPerByte:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeRefScriptCostPerByte,
        }),
    });
    const acceptedCardano = measureSignedCardanoSpendInputsV1(
      boundary.accepted.cborHex,
    );
    const adjacentCardano = measureSignedCardanoSpendInputsV1(
      boundary.adjacent.cborHex,
    );
    const inputField = exerciseMidgardOrderedCollectionBoundaryV1({
      signedCardanoCborHex: boundary.accepted.cborHex,
      fieldIndex: 0,
    });
    const retainedDa = await exerciseMidgardRetainedDaBoundaryV1({
      signedCardanoCborHex: boundary.accepted.cborHex,
      corpusLabel: "maximum-spend-inputs",
    });
    expect(retainedDa.normal.reconstructedCanonicalBytes).toBe(
      inputField.nativeCanonicalBytes,
    );
    expect(retainedDa.forced.reconstructedCanonicalBytes).toBe(
      inputField.nativeCanonicalBytes,
    );
    expect(retainedDa.normal.revealStepCount).toBe(
      inputField.completeFoldStepCount,
    );
    expect(retainedDa.forced.revealStepCount).toBe(
      inputField.completeFoldStepCount,
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
    expect(boundary.adjacent.requestedItemCount).toBeLessThanOrEqual(
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
    expect(acceptedCardano.inputCount).toBe(
      boundary.accepted.requestedItemCount,
    );
    expect(acceptedCardano.vkeyWitnessCount).toBe(1);
    expect(acceptedCardano.outputCount).toBe(1);
    expect(adjacentCardano.inputCount).toBe(
      boundary.adjacent.requestedItemCount,
    );
    expect(adjacentCardano.vkeyWitnessCount).toBe(1);
    expect(adjacentCardano.outputCount).toBe(1);
    expect(inputField.itemCount).toBe(acceptedCardano.inputCount);
    expect(inputField.revealStepCount).toBe(
      acceptedCardano.inputCount,
    );
    expect(inputField.maxRevealBytes).toBeLessThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );

    const txHash = await emulator.submitTx(boundary.accepted.cborHex);
    await expect(emulator.awaitTx(txHash)).resolves.toBe(true);

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            spendInputsBoundaryV1: {
              fieldIndex: 0,
              fieldName: "spend_inputs",
              maxTxSize:
                emulator.protocolParameters.maxTxSize,
              maxValueSize:
                emulator.protocolParameters.maxValSize,
              inputSupplyDerivation:
                "floor(maxTxSize / 32-byte transaction id) + 2 adjacent reserve",
              realGenesisInputSupply: inputSupply,
              lovelacePerInput: lovelacePerInput.toString(),
              requestedInputCount:
                boundary.accepted.requestedItemCount,
              actualInputCount: acceptedCardano.inputCount,
              actualVkeyWitnessCount:
                acceptedCardano.vkeyWitnessCount,
              signedCardanoBytes: boundary.accepted.signedBytes,
              byteMargin:
                emulator.protocolParameters.maxTxSize -
                boundary.accepted.signedBytes,
              fee: boundary.accepted.fee.toString(),
              nativeCanonicalBytes:
                inputField.nativeCanonicalBytes,
              spendInputsFieldBytes: inputField.fieldBytes,
              inputItemCount: inputField.itemCount,
              inputRevealSteps: inputField.revealStepCount,
              maxChunkBytes: inputField.maxChunkBytes,
              maxRevealBytes: inputField.maxRevealBytes,
              completeFoldSteps:
                inputField.completeFoldStepCount,
              adjacentRequestedInputCount:
                boundary.adjacent.requestedItemCount,
              adjacentActualInputCount:
                adjacentCardano.inputCount,
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
