import {
  decodeMidgardMintFieldPreimage,
  encodeMidgardMintPolicyItem,
} from "@al-ft/midgard-core";
import { CML, Emulator } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { publishAikenVector } from "./helpers/aiken-vector-channel.js";
import {
  buildSignedCardanoMintNativePoliciesCandidate,
  CARDANO_BOUNDARY_MAX_TX_SIZE,
  CARDANO_BOUNDARY_MINT_ASSET_NAME,
  CARDANO_BOUNDARY_OBSERVER_EXPIRY_BASE,
  CARDANO_BOUNDARY_OBSERVER_TTL,
  deterministicCardanoBoundaryPrivateKey,
  exerciseMidgardOrderedCollectionBoundary,
  findSignedCardanoCollectionBoundary,
  measureSignedCardanoMintNativePolicies,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
} from "./helpers/ordered-collection-boundary.js";
import { exerciseMidgardRetainedDaBoundary } from "./helpers/retained-da-boundary.js";
import {
  MAXIMUM_MINT_POLICY_ACCEPTED_COUNT,
  MAXIMUM_MINT_POLICY_ACCEPTED_SIGNED_BYTES,
  MAXIMUM_MINT_POLICY_ADJACENT_COUNT,
  MAXIMUM_MINT_POLICY_ADJACENT_SIGNED_BYTES,
  maximumMintTerminalFoldVector,
} from "./ordered-collection-mint-boundary.maximum-mint-terminal-fold-vector.js";

describe("canonical V1 mint Cardano boundary", () => {
  it("packs field-5 assets under maxValueSize and authorizes every policy with a field-6 native script", async () => {
    const spendingKey = deterministicCardanoBoundaryPrivateKey(0);
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
          assets: { lovelace: 1_000_000_000_000n },
        },
      ],
      PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
    );
    const [fundingInput] = await emulator.getUtxos(address);
    expect(fundingInput).toBeDefined();
    expect(fundingInput!.txHash).toBe("00".repeat(32));
    expect(fundingInput!.outputIndex).toBe(0);

    const buildCandidate = (requestedPolicyCount: number) =>
      buildSignedCardanoMintNativePoliciesCandidate({
        privateKeyBech32: spendingKey.to_bech32(),
        fundingInput: fundingInput!,
        recipientAddress: address,
        requestedPolicyCount,
        maxValueSize: emulator.protocolParameters.maxValSize,
        minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeA,
        minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeB,
        minFeeRefScriptCostPerByte:
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeRefScriptCostPerByte,
      });

    const firstCandidate = await buildCandidate(1);
    const firstMeasurement = measureSignedCardanoMintNativePolicies(
      firstCandidate.cborHex,
    );
    const firstMintField = exerciseMidgardOrderedCollectionBoundary({
      signedCardanoCborHex: firstCandidate.cborHex,
      fieldIndex: 5,
    });
    const firstScriptField = exerciseMidgardOrderedCollectionBoundary({
      signedCardanoCborHex: firstCandidate.cborHex,
      fieldIndex: 6,
    });
    expect(firstCandidate.signedBytes).toBeLessThanOrEqual(
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );
    expect(firstMeasurement.mintPolicyCount).toBe(1);
    expect(firstMeasurement.mintAssetCount).toBe(1);
    expect(firstMeasurement.nativeScriptWitnessCount).toBe(1);
    expect(firstMintField.itemCount).toBe(1);
    expect(firstScriptField.itemCount).toBe(1);

    const boundary = await findSignedCardanoCollectionBoundary({
      maxTxSize: emulator.protocolParameters.maxTxSize,
      buildSignedCandidate: buildCandidate,
    });
    const acceptedCardano = measureSignedCardanoMintNativePolicies(
      boundary.accepted.cborHex,
    );
    const adjacentCardano = measureSignedCardanoMintNativePolicies(
      boundary.adjacent.cborHex,
    );
    const mintField = exerciseMidgardOrderedCollectionBoundary({
      signedCardanoCborHex: boundary.accepted.cborHex,
      fieldIndex: 5,
    });
    const scriptField = exerciseMidgardOrderedCollectionBoundary({
      signedCardanoCborHex: boundary.accepted.cborHex,
      fieldIndex: 6,
    });
    const retainedDa = await exerciseMidgardRetainedDaBoundary({
      signedCardanoCborHex: boundary.accepted.cborHex,
      corpusLabel: "maximum-mint-and-native-policies",
    });
    expect(retainedDa.normal.reconstructedCanonicalBytes).toBe(
      mintField.nativeCanonicalBytes,
    );
    expect(retainedDa.forced.reconstructedCanonicalBytes).toBe(
      mintField.nativeCanonicalBytes - 1,
    );
    expect(retainedDa.normal.revealStepCount).toBe(
      mintField.completeFoldStepCount,
    );
    expect(retainedDa.forced.revealStepCount).toBe(
      mintField.completeFoldStepCount,
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

    const assertExactPolicyCoupling = (
      measurement: ReturnType<typeof measureSignedCardanoMintNativePolicies>,
      expectedPolicyCount: number,
    ): void => {
      expect(measurement.inputCount).toBe(1);
      expect(measurement.mintPolicyCount).toBe(expectedPolicyCount);
      expect(measurement.mintAssetCount).toBe(expectedPolicyCount);
      expect(measurement.nativeScriptWitnessCount).toBe(expectedPolicyCount);
      expect(measurement.vkeyWitnessCount).toBe(1);
      expect(measurement.outputCount).toBeGreaterThan(0);
      expect(measurement.validityStart).toBeUndefined();
      expect(measurement.ttl).toBe(CARDANO_BOUNDARY_OBSERVER_TTL);
      expect(measurement.policyAssetCounts).toEqual(
        Array.from({ length: expectedPolicyCount }, () => 1),
      );
      expect(measurement.mintQuantities).toEqual(
        Array.from({ length: expectedPolicyCount }, () => 1n),
      );
      expect(measurement.outputAssetCount).toBe(expectedPolicyCount);
      expect(measurement.outputAssetNameHexes).toEqual(
        Array.from({ length: expectedPolicyCount }, () =>
          CARDANO_BOUNDARY_MINT_ASSET_NAME.toString("hex"),
        ),
      );
      expect(measurement.outputAssetQuantities).toEqual(
        Array.from({ length: expectedPolicyCount }, () => 1n),
      );
      expect([...measurement.mintPolicyHashHexes].sort()).toEqual(
        [...measurement.nativeScriptHashHexes].sort(),
      );
      expect([...measurement.mintPolicyHashHexes].sort()).toEqual(
        [...measurement.outputPolicyHashHexes].sort(),
      );
      expect(new Set(measurement.mintPolicyHashHexes).size).toBe(
        expectedPolicyCount,
      );
      for (const valueBytes of measurement.outputValueByteLengths) {
        expect(valueBytes).toBeLessThanOrEqual(
          emulator.protocolParameters.maxValSize,
        );
      }
      expect(measurement.hasWithdrawals).toBe(false);
      expect(measurement.hasPlutusScripts).toBe(false);
      expect(measurement.hasRedeemers).toBe(false);
      expect(measurement.hasDatums).toBe(false);
      expect(measurement.collateralInputCount).toBe(0);
    };
    assertExactPolicyCoupling(
      acceptedCardano,
      boundary.accepted.requestedItemCount,
    );
    assertExactPolicyCoupling(
      adjacentCardano,
      boundary.adjacent.requestedItemCount,
    );

    expect(mintField.itemCount).toBe(acceptedCardano.mintPolicyCount);
    expect(mintField.revealStepCount).toBe(acceptedCardano.mintPolicyCount);
    expect(scriptField.itemCount).toBe(
      acceptedCardano.nativeScriptWitnessCount,
    );
    expect(scriptField.revealStepCount).toBe(
      acceptedCardano.nativeScriptWitnessCount,
    );
    expect(mintField.completeFoldStepCount).toBe(
      scriptField.completeFoldStepCount,
    );
    expect(mintField.maxRevealBytes).toBeLessThan(CARDANO_BOUNDARY_MAX_TX_SIZE);

    // The genuine maximum and its immediately adjacent control are exact, not
    // merely "whatever the search returned".
    expect(boundary.accepted.requestedItemCount).toBe(
      MAXIMUM_MINT_POLICY_ACCEPTED_COUNT,
    );
    expect(boundary.accepted.signedBytes).toBe(
      MAXIMUM_MINT_POLICY_ACCEPTED_SIGNED_BYTES,
    );
    expect(boundary.adjacent.requestedItemCount).toBe(
      MAXIMUM_MINT_POLICY_ADJACENT_COUNT,
    );
    expect(boundary.adjacent.signedBytes).toBe(
      MAXIMUM_MINT_POLICY_ADJACENT_SIGNED_BYTES,
    );
    expect(mintField.itemCount).toBe(MAXIMUM_MINT_POLICY_ACCEPTED_COUNT);
    expect(scriptField.itemCount).toBe(MAXIMUM_MINT_POLICY_ACCEPTED_COUNT);
    // §5.6: field 5 is the enveloped per-policy item list, and its decoder is
    // where the one-policy/one-asset shape, the 28-byte policy id, canonical key
    // order and non-zero quantities are enforced. The hand-rolled CBOR walk this
    // replaced re-stated those rules against the retired raw-map form.
    const nativeMintPolicyItems = decodeMidgardMintFieldPreimage(
      Buffer.from(mintField.fieldPreimageCborHex, "hex"),
    );
    const nativeMintEntries = nativeMintPolicyItems.map((item, itemIndex) => {
      if (item.assets.length !== 1) {
        throw new Error(
          `Canonical native mint item ${itemIndex.toString()} is not one exact policy/asset pair`,
        );
      }
      const asset = item.assets[0]!;
      return {
        policyIdHex: Buffer.from(item.policyId).toString("hex"),
        assetNameHex: Buffer.from(asset.assetName).toString("hex"),
        quantity: asset.quantity,
      };
    });
    expect(nativeMintEntries.map(({ policyIdHex }) => policyIdHex)).toEqual(
      acceptedCardano.mintPolicyHashHexes,
    );
    expect(nativeMintEntries.map(({ assetNameHex }) => assetNameHex)).toEqual(
      Array.from({ length: acceptedCardano.mintPolicyCount }, () =>
        CARDANO_BOUNDARY_MINT_ASSET_NAME.toString("hex"),
      ),
    );
    expect(nativeMintEntries.map(({ quantity }) => quantity)).toEqual(
      acceptedCardano.mintQuantities,
    );
    expect({
      fieldCommitmentHex: mintField.fieldCommitmentHex,
      transactionIdHex: mintField.terminalFoldVector.transactionIdHex,
      transactionCommitmentHex:
        mintField.terminalFoldVector.transactionCommitmentHex,
      preWorkRootHex: mintField.terminalFoldVector.preWorkRootHex,
      postWorkRootHex: mintField.terminalFoldVector.postWorkRootHex,
      encodedLengthBeforeItem:
        mintField.terminalFoldVector.encodedLengthBeforeItem,
      collectionProof: mintField.terminalFoldVector.collectionProof,
      chunkProof: mintField.terminalFoldVector.chunkProof,
    }).toEqual(maximumMintTerminalFoldVector);
    // #590 scope item 0: the write channel this suite did not have.
    //
    // The `mint-boundary-v1` fixture in
    // `onchain/aiken/lib/midgard/validation-machine-tests/` mirrors this
    // boundary's terminal fold, and until now nothing carried the bytes across —
    // so that fixture still pinned the *counted* field roots this package stopped
    // emitting at #585, and stayed green only because the id it pinned was the id
    // of the compact it pinned beside it. #592's rebind puts §8's carriage where
    // the counted `(ItemProofV1, ChunkProofV1)` pair used to be, and a carriage is
    // the field's whole §5.1 preimage, which no human is going to retype.
    //
    // Published after the assertions above, so the generator can only ever see a
    // vector this suite has already accepted.
    publishAikenVector("mint-boundary-v1", {
      fieldIndex: mintField.terminalFoldVector.collectionProof.fieldIndex,
      itemCount: mintField.terminalFoldVector.collectionProof.itemCount,
      itemIndex: mintField.terminalFoldVector.collectionProof.itemIndex,
      terminalChunkIndex: mintField.terminalFoldVector.chunkProof.chunkIndex,
      encodedLengthBeforeItem:
        mintField.terminalFoldVector.encodedLengthBeforeItem,
      // §8.1's tier-1 carriage: the field's whole §5.1 preimage, which the door
      // hashes once against the flat commitment below.
      fieldPreimageCborHex: mintField.fieldPreimageCborHex,
      fieldCommitmentHex: mintField.fieldCommitmentHex,
      transactionIdHex: mintField.terminalFoldVector.transactionIdHex,
      transactionCommitmentHex:
        mintField.terminalFoldVector.transactionCommitmentHex,
      compactCborHex: mintField.terminalFoldVector.compactCborHex,
      witnessSetCompactCborHex:
        mintField.terminalFoldVector.witnessSetCompactCborHex,
      fieldPreimageLengthsCborHex:
        mintField.terminalFoldVector.fieldPreimageLengthsCborHex,
      validationContextCborHex:
        mintField.terminalFoldVector.validationContextCborHex,
      preWorkRootHex: mintField.terminalFoldVector.preWorkRootHex,
      postWorkRootHex: mintField.terminalFoldVector.postWorkRootHex,
    });

    const txHash = await emulator.submitTx(boundary.accepted.cborHex);
    await expect(emulator.awaitTx(txHash)).resolves.toBe(true);

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            mintBoundaryV1: {
              mintFieldIndex: 5,
              scriptWitnessControlFieldIndex: 6,
              maxTxSize: emulator.protocolParameters.maxTxSize,
              maxValueSize: emulator.protocolParameters.maxValSize,
              fixtureGenerationBasis:
                "on-demand until exact signed bytes exceed maxTxSize",
              assetNameHex: CARDANO_BOUNDARY_MINT_ASSET_NAME.toString("hex"),
              actualSpendInputCount: acceptedCardano.inputCount,
              actualMintPolicyCount: acceptedCardano.mintPolicyCount,
              actualMintAssetCount: acceptedCardano.mintAssetCount,
              actualNativeScriptWitnessCount:
                acceptedCardano.nativeScriptWitnessCount,
              actualOutputCount: acceptedCardano.outputCount,
              actualVkeyWitnessCount: acceptedCardano.vkeyWitnessCount,
              validityStart: "unset",
              validityEnd: CARDANO_BOUNDARY_OBSERVER_TTL.toString(),
              distinctExpiryStart:
                CARDANO_BOUNDARY_OBSERVER_EXPIRY_BASE.toString(),
              distinctExpiryEnd: (
                CARDANO_BOUNDARY_OBSERVER_EXPIRY_BASE +
                BigInt(boundary.accepted.requestedItemCount - 1)
              ).toString(),
              signedCardanoBytes: boundary.accepted.signedBytes,
              byteMargin:
                emulator.protocolParameters.maxTxSize -
                boundary.accepted.signedBytes,
              fee: boundary.accepted.fee.toString(),
              outputValueByteLengths: acceptedCardano.outputValueByteLengths,
              outputPolicyCounts: acceptedCardano.outputPolicyCounts,
              outputValueMargins: acceptedCardano.outputValueByteLengths.map(
                (valueBytes) =>
                  emulator.protocolParameters.maxValSize - valueBytes,
              ),
              nativeCanonicalBytes: mintField.nativeCanonicalBytes,
              mintFieldBytes: mintField.fieldBytes,
              mintItems: mintField.itemCount,
              mintRevealSteps: mintField.revealStepCount,
              mintMaxChunkBytes: mintField.maxChunkBytes,
              mintMaxRevealBytes: mintField.maxRevealBytes,
              scriptWitnessItems: scriptField.itemCount,
              completeFoldSteps: mintField.completeFoldStepCount,
              // The §5.6 `enc_5` bytes of the penultimate policy item, re-encoded
              // from the decoded item so the artifact records the canonical form
              // rather than a slice of the preimage.
              penultimateMintItemHex: encodeMidgardMintPolicyItem(
                nativeMintPolicyItems.at(-2) ?? {
                  policyId: Buffer.alloc(28),
                  assets: [{ assetName: Buffer.alloc(0), quantity: 1n }],
                },
              ).toString("hex"),
              terminalFoldVector: mintField.terminalFoldVector,
              adjacentRequestedPolicyCount:
                boundary.adjacent.requestedItemCount,
              adjacentMintPolicyCount: adjacentCardano.mintPolicyCount,
              adjacentMintAssetCount: adjacentCardano.mintAssetCount,
              adjacentNativeScriptWitnessCount:
                adjacentCardano.nativeScriptWitnessCount,
              adjacentOutputCount: adjacentCardano.outputCount,
              adjacentVkeyWitnessCount: adjacentCardano.vkeyWitnessCount,
              adjacentOutputValueByteLengths:
                adjacentCardano.outputValueByteLengths,
              adjacentOutputPolicyCounts: adjacentCardano.outputPolicyCounts,
              adjacentOutputValueMargins:
                adjacentCardano.outputValueByteLengths.map(
                  (valueBytes) =>
                    emulator.protocolParameters.maxValSize - valueBytes,
                ),
              adjacentSignedCardanoBytes: boundary.adjacent.signedBytes,
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
