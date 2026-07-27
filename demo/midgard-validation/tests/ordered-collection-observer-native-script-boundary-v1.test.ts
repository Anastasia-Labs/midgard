import { CML, Emulator } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildSignedCardanoObserverNativeScriptsCandidateV1,
  CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
  CARDANO_BOUNDARY_OBSERVER_EXPIRY_BASE_V1,
  CARDANO_BOUNDARY_OBSERVER_TTL_V1,
  deterministicCardanoBoundaryPrivateKeyV1,
  exerciseMidgardOrderedCollectionBoundaryV1,
  findSignedCardanoCollectionBoundaryV1,
  measureSignedCardanoObserverNativeScriptsV1,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
} from "./helpers/ordered-collection-boundary-v1.js";
import { exerciseMidgardRetainedDaBoundaryV1 } from "./helpers/retained-da-boundary-v1.js";

describe("canonical V1 observer/native-script Cardano boundary", () => {
  it("couples every field-3 observer to one real field-7 native-script witness", async () => {
    const spendingKey = deterministicCardanoBoundaryPrivateKeyV1(0);
    const spendingKeyHash = spendingKey.to_public().hash();
    const address = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(spendingKeyHash),
    )
      .to_address()
      .to_bech32();
    const emulator = new Emulator(
      [
        {
          seedPhrase: "",
          privateKey: spendingKey.to_bech32(),
          address,
          assets: { lovelace: 100_000_000n },
        },
      ],
      PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
    );
    const [fundingInput] = await emulator.getUtxos(address);
    expect(fundingInput).toBeDefined();
    expect(fundingInput!.txHash).toBe("00".repeat(32));
    expect(fundingInput!.outputIndex).toBe(0);

    const buildCandidate = (requestedObserverCount: number) =>
      buildSignedCardanoObserverNativeScriptsCandidateV1({
        privateKeyBech32: spendingKey.to_bech32(),
        fundingInput: fundingInput!,
        recipientAddress: address,
        requestedObserverCount,
        minFeeA:
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
        minFeeB:
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
        minFeeRefScriptCostPerByte:
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeRefScriptCostPerByte,
      });

    const firstCandidate = await buildCandidate(1);
    const firstMeasurement =
      measureSignedCardanoObserverNativeScriptsV1(
        firstCandidate.cborHex,
      );
    const firstObserverField =
      exerciseMidgardOrderedCollectionBoundaryV1({
        signedCardanoCborHex: firstCandidate.cborHex,
        fieldIndex: 3,
      });
    const firstScriptField =
      exerciseMidgardOrderedCollectionBoundaryV1({
        signedCardanoCborHex: firstCandidate.cborHex,
        fieldIndex: 7,
      });
    expect(firstCandidate.signedBytes).toBeLessThanOrEqual(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect(firstMeasurement.withdrawalCount).toBe(1);
    expect(firstMeasurement.nativeScriptWitnessCount).toBe(1);
    expect(firstObserverField.itemCount).toBe(1);
    expect(firstScriptField.itemCount).toBe(1);

    const boundary = await findSignedCardanoCollectionBoundaryV1({
      maxTxSize: emulator.protocolParameters.maxTxSize,
      buildSignedCandidate: buildCandidate,
    });
    const acceptedCardano =
      measureSignedCardanoObserverNativeScriptsV1(
        boundary.accepted.cborHex,
      );
    const adjacentCardano =
      measureSignedCardanoObserverNativeScriptsV1(
        boundary.adjacent.cborHex,
      );
    const observerField =
      exerciseMidgardOrderedCollectionBoundaryV1({
        signedCardanoCborHex: boundary.accepted.cborHex,
        fieldIndex: 3,
      });
    const scriptField =
      exerciseMidgardOrderedCollectionBoundaryV1({
        signedCardanoCborHex: boundary.accepted.cborHex,
        fieldIndex: 7,
      });
    const retainedDa = await exerciseMidgardRetainedDaBoundaryV1({
      signedCardanoCborHex: boundary.accepted.cborHex,
      corpusLabel: "maximum-observers-and-native-scripts",
    });
    expect(retainedDa.normal.reconstructedCanonicalBytes).toBe(
      observerField.nativeCanonicalBytes,
    );
    expect(retainedDa.forced.reconstructedCanonicalBytes).toBe(
      observerField.nativeCanonicalBytes,
    );
    expect(retainedDa.normal.revealStepCount).toBe(
      observerField.completeFoldStepCount,
    );
    expect(retainedDa.forced.revealStepCount).toBe(
      observerField.completeFoldStepCount,
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

    const assertExactCoupling = (
      candidateCborHex: string,
      measurement: ReturnType<
        typeof measureSignedCardanoObserverNativeScriptsV1
      >,
      expectedObserverCount: number,
    ): void => {
      expect(measurement.inputCount).toBe(1);
      expect(measurement.withdrawalCount).toBe(
        expectedObserverCount,
      );
      expect(measurement.nativeScriptWitnessCount).toBe(
        expectedObserverCount,
      );
      expect(measurement.outputCount).toBe(1);
      expect(measurement.vkeyWitnessCount).toBe(1);
      expect(measurement.validityStart).toBeUndefined();
      expect(measurement.ttl).toBe(
        CARDANO_BOUNDARY_OBSERVER_TTL_V1,
      );
      expect(measurement.withdrawalAmounts).toEqual(
        Array.from({ length: expectedObserverCount }, () => 0n),
      );
      expect(
        [...measurement.observerScriptHashHexes].sort(),
      ).toEqual([...measurement.nativeScriptHashHexes].sort());
      expect(
        new Set(measurement.observerScriptHashHexes).size,
      ).toBe(expectedObserverCount);
      expect(measurement.hasPlutusScripts).toBe(false);
      expect(measurement.hasRedeemers).toBe(false);
      expect(measurement.hasDatums).toBe(false);
      expect(measurement.collateralInputCount).toBe(0);

      const transaction = CML.Transaction.from_cbor_hex(
        candidateCborHex,
      );
      const nativeScripts =
        transaction.witness_set().native_scripts();
      const signerHashes = CML.Ed25519KeyHashList.new();
      signerHashes.add(spendingKeyHash);
      expect(nativeScripts?.len()).toBe(expectedObserverCount);
      for (
        let scriptIndex = 0;
        scriptIndex < expectedObserverCount;
        scriptIndex += 1
      ) {
        expect(
          nativeScripts!.get(scriptIndex).verify(
            undefined,
            CARDANO_BOUNDARY_OBSERVER_TTL_V1,
            signerHashes,
          ),
        ).toBe(true);
      }
    };
    assertExactCoupling(
      boundary.accepted.cborHex,
      acceptedCardano,
      boundary.accepted.requestedItemCount,
    );
    assertExactCoupling(
      boundary.adjacent.cborHex,
      adjacentCardano,
      boundary.adjacent.requestedItemCount,
    );

    expect(observerField.itemCount).toBe(
      acceptedCardano.withdrawalCount,
    );
    expect(observerField.revealStepCount).toBe(
      acceptedCardano.withdrawalCount,
    );
    expect(scriptField.itemCount).toBe(
      acceptedCardano.nativeScriptWitnessCount,
    );
    expect(scriptField.revealStepCount).toBe(
      acceptedCardano.nativeScriptWitnessCount,
    );
    expect(observerField.completeFoldStepCount).toBe(
      scriptField.completeFoldStepCount,
    );
    expect(observerField.maxRevealBytes).toBeLessThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect(scriptField.maxRevealBytes).toBeLessThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );

    for (const rewardAddress of acceptedCardano.rewardAddressBech32s) {
      emulator.chain[rewardAddress] = {
        registeredStake: true,
        delegation: { poolId: null, rewards: 0n },
      };
    }
    const txHash = await emulator.submitTx(boundary.accepted.cborHex);
    await expect(emulator.awaitTx(txHash)).resolves.toBe(true);

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            observerNativeScriptBoundaryV1: {
              observerFieldIndex: 3,
              scriptWitnessFieldIndex: 7,
              maxTxSize:
                emulator.protocolParameters.maxTxSize,
              maxValueSize:
                emulator.protocolParameters.maxValSize,
              fixtureGenerationBasis:
                "on-demand until exact signed bytes exceed maxTxSize",
              actualSpendInputCount: acceptedCardano.inputCount,
              actualObserverWithdrawalCount:
                acceptedCardano.withdrawalCount,
              actualNativeScriptWitnessCount:
                acceptedCardano.nativeScriptWitnessCount,
              actualOutputCount: acceptedCardano.outputCount,
              actualVkeyWitnessCount:
                acceptedCardano.vkeyWitnessCount,
              validityStart: "unset",
              validityEnd:
                CARDANO_BOUNDARY_OBSERVER_TTL_V1.toString(),
              distinctExpiryStart:
                CARDANO_BOUNDARY_OBSERVER_EXPIRY_BASE_V1.toString(),
              distinctExpiryEnd:
                (
                  CARDANO_BOUNDARY_OBSERVER_EXPIRY_BASE_V1 +
                  BigInt(
                    boundary.accepted.requestedItemCount - 1,
                  )
                ).toString(),
              signedCardanoBytes: boundary.accepted.signedBytes,
              byteMargin:
                emulator.protocolParameters.maxTxSize -
                boundary.accepted.signedBytes,
              fee: boundary.accepted.fee.toString(),
              nativeCanonicalBytes:
                observerField.nativeCanonicalBytes,
              observerFieldBytes: observerField.fieldBytes,
              observerItems: observerField.itemCount,
              observerRevealSteps: observerField.revealStepCount,
              observerMaxChunkBytes:
                observerField.maxChunkBytes,
              observerMaxRevealBytes:
                observerField.maxRevealBytes,
              scriptWitnessFieldBytes: scriptField.fieldBytes,
              scriptWitnessItems: scriptField.itemCount,
              scriptWitnessRevealSteps:
                scriptField.revealStepCount,
              scriptWitnessMaxChunkBytes:
                scriptField.maxChunkBytes,
              scriptWitnessMaxRevealBytes:
                scriptField.maxRevealBytes,
              completeFoldSteps:
                observerField.completeFoldStepCount,
              adjacentRequestedObserverCount:
                boundary.adjacent.requestedItemCount,
              adjacentObserverWithdrawalCount:
                adjacentCardano.withdrawalCount,
              adjacentNativeScriptWitnessCount:
                adjacentCardano.nativeScriptWitnessCount,
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
