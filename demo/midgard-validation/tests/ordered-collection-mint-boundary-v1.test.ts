import {
  decodeMidgardMintFieldPreimageV1,
  encodeMidgardMintPolicyItemV1,
} from "@al-ft/midgard-core";
import { CML, Emulator } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { publishAikenVectorV1 } from "./helpers/aiken-vector-channel.js";
import {
  buildSignedCardanoMintNativePoliciesCandidateV1,
  CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
  CARDANO_BOUNDARY_MINT_ASSET_NAME_V1,
  CARDANO_BOUNDARY_OBSERVER_EXPIRY_BASE_V1,
  CARDANO_BOUNDARY_OBSERVER_TTL_V1,
  deterministicCardanoBoundaryPrivateKeyV1,
  exerciseMidgardOrderedCollectionBoundaryV1,
  findSignedCardanoCollectionBoundaryV1,
  measureSignedCardanoMintNativePoliciesV1,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
} from "./helpers/ordered-collection-boundary-v1.js";
import { exerciseMidgardRetainedDaBoundaryV1 } from "./helpers/retained-da-boundary-v1.js";

// The exact genuine signed-Cardano field-5/field-6 boundary. The terminal fold
// vector below is the Aiken-replayed half; these four numbers pin the policy
// cardinality and byte count the search must land on, so a silently shrunk mint
// collection can no longer satisfy the relative bounds alone.
const MAXIMUM_MINT_POLICY_ACCEPTED_COUNT_V1 = 130;
const MAXIMUM_MINT_POLICY_ACCEPTED_SIGNED_BYTES_V1 = 16_376;
const MAXIMUM_MINT_POLICY_ADJACENT_COUNT_V1 = 131;
const MAXIMUM_MINT_POLICY_ADJACENT_SIGNED_BYTES_V1 = 16_500;

const maximumMintTerminalFoldVectorV1 = {
  fieldCommitmentHex:
    "7ba153c420ecc2fc34570ff76ea54d8b892b9b2f21baff8ae9bbaf95b7e1eab7",
  transactionIdHex:
    "7d05811c269235d01486fb1b9f5d9f08ee27dfd67f5b2f5553e98f5f63d2904c",
  transactionCommitmentHex:
    "53fab1e7ca307ed09192048fc8c4fe82b33e4cd9de40bd2e723207039d85b281",
  preWorkRootHex:
    "4a04c1b0d3ab0307fcbad5cda4985636ac30cbd52d11f229967bb21d6e027868",
  postWorkRootHex:
    "6b18ca10e35cdec5bf29736c0a43aa76b4fa32600949fce9208dde1c9fdbd798",
  encodedLengthBeforeItem: 5807,
  collectionProof: {
    fieldIndex: 5,
    itemCount: 130,
    itemIndex: 129,
    itemLength: 43,
    itemCommitmentHex:
      "534ff6685dd10a576be7dec4ecb1cf2f239a5fdcb751db98ccc1b93ebd4e5c04",
    frontier: [
      {
        height: 1,
        hashHex:
          "ce7d37cda58da9e9e61128e546de8b86657a6ba8b3412ff6b0ac9768220facc1",
      },
      {
        height: 7,
        hashHex:
          "a4e70fd3e6e67a34ff688b3ee548e87d066c538c85340620668e3b53e09d7110",
      },
    ],
    siblingHexes: [
      "7dee9561c439c28a1058042f1cccd273783fe91f56440e1b9c44e306d0aee12b",
    ],
  },
  chunkProof: {
    fieldIndex: 5,
    itemIndex: 129,
    totalLength: 43,
    chunkIndex: 0,
    chunkHex:
      "82581cffab1dd64f82b6991818c1ecc5047d52ce5d00f6fdbc5023e2980167a1494d696467617264563101",
    frontier: [
      {
        height: 0,
        hashHex:
          "ae892cdb843a795de543205f99a43ea2d0f946bcab042d2c405bd786dbad75da",
      },
    ],
    siblingHexes: [],
  },
} as const;

describe("canonical V1 mint Cardano boundary", () => {
  it("packs field-5 assets under maxValueSize and authorizes every policy with a field-6 native script", async () => {
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
          assets: { lovelace: 1_000_000_000_000n },
        },
      ],
      PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
    );
    const [fundingInput] = await emulator.getUtxos(address);
    expect(fundingInput).toBeDefined();
    expect(fundingInput!.txHash).toBe("00".repeat(32));
    expect(fundingInput!.outputIndex).toBe(0);

    const buildCandidate = (requestedPolicyCount: number) =>
      buildSignedCardanoMintNativePoliciesCandidateV1({
        privateKeyBech32: spendingKey.to_bech32(),
        fundingInput: fundingInput!,
        recipientAddress: address,
        requestedPolicyCount,
        maxValueSize: emulator.protocolParameters.maxValSize,
        minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
        minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
        minFeeRefScriptCostPerByte:
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeRefScriptCostPerByte,
      });

    const firstCandidate = await buildCandidate(1);
    const firstMeasurement = measureSignedCardanoMintNativePoliciesV1(
      firstCandidate.cborHex,
    );
    const firstMintField = exerciseMidgardOrderedCollectionBoundaryV1({
      signedCardanoCborHex: firstCandidate.cborHex,
      fieldIndex: 5,
    });
    const firstScriptField = exerciseMidgardOrderedCollectionBoundaryV1({
      signedCardanoCborHex: firstCandidate.cborHex,
      fieldIndex: 6,
    });
    expect(firstCandidate.signedBytes).toBeLessThanOrEqual(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect(firstMeasurement.mintPolicyCount).toBe(1);
    expect(firstMeasurement.mintAssetCount).toBe(1);
    expect(firstMeasurement.nativeScriptWitnessCount).toBe(1);
    expect(firstMintField.itemCount).toBe(1);
    expect(firstScriptField.itemCount).toBe(1);

    const boundary = await findSignedCardanoCollectionBoundaryV1({
      maxTxSize: emulator.protocolParameters.maxTxSize,
      buildSignedCandidate: buildCandidate,
    });
    const acceptedCardano = measureSignedCardanoMintNativePoliciesV1(
      boundary.accepted.cborHex,
    );
    const adjacentCardano = measureSignedCardanoMintNativePoliciesV1(
      boundary.adjacent.cborHex,
    );
    const mintField = exerciseMidgardOrderedCollectionBoundaryV1({
      signedCardanoCborHex: boundary.accepted.cborHex,
      fieldIndex: 5,
    });
    const scriptField = exerciseMidgardOrderedCollectionBoundaryV1({
      signedCardanoCborHex: boundary.accepted.cborHex,
      fieldIndex: 6,
    });
    const retainedDa = await exerciseMidgardRetainedDaBoundaryV1({
      signedCardanoCborHex: boundary.accepted.cborHex,
      corpusLabel: "maximum-mint-and-native-policies",
    });
    expect(retainedDa.normal.reconstructedCanonicalBytes).toBe(
      mintField.nativeCanonicalBytes,
    );
    expect(retainedDa.forced.reconstructedCanonicalBytes).toBe(
      mintField.nativeCanonicalBytes,
    );
    expect(retainedDa.normal.revealStepCount).toBe(
      mintField.completeFoldStepCount,
    );
    expect(retainedDa.forced.revealStepCount).toBe(
      mintField.completeFoldStepCount,
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

    const assertExactPolicyCoupling = (
      measurement: ReturnType<typeof measureSignedCardanoMintNativePoliciesV1>,
      expectedPolicyCount: number,
    ): void => {
      expect(measurement.inputCount).toBe(1);
      expect(measurement.mintPolicyCount).toBe(expectedPolicyCount);
      expect(measurement.mintAssetCount).toBe(expectedPolicyCount);
      expect(measurement.nativeScriptWitnessCount).toBe(expectedPolicyCount);
      expect(measurement.vkeyWitnessCount).toBe(1);
      expect(measurement.outputCount).toBeGreaterThan(0);
      expect(measurement.validityStart).toBeUndefined();
      expect(measurement.ttl).toBe(CARDANO_BOUNDARY_OBSERVER_TTL_V1);
      expect(measurement.policyAssetCounts).toEqual(
        Array.from({ length: expectedPolicyCount }, () => 1),
      );
      expect(measurement.mintQuantities).toEqual(
        Array.from({ length: expectedPolicyCount }, () => 1n),
      );
      expect(measurement.outputAssetCount).toBe(expectedPolicyCount);
      expect(measurement.outputAssetNameHexes).toEqual(
        Array.from({ length: expectedPolicyCount }, () =>
          CARDANO_BOUNDARY_MINT_ASSET_NAME_V1.toString("hex"),
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
    expect(mintField.maxRevealBytes).toBeLessThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );

    // The genuine maximum and its immediately adjacent control are exact, not
    // merely "whatever the search returned".
    expect(boundary.accepted.requestedItemCount).toBe(
      MAXIMUM_MINT_POLICY_ACCEPTED_COUNT_V1,
    );
    expect(boundary.accepted.signedBytes).toBe(
      MAXIMUM_MINT_POLICY_ACCEPTED_SIGNED_BYTES_V1,
    );
    expect(boundary.adjacent.requestedItemCount).toBe(
      MAXIMUM_MINT_POLICY_ADJACENT_COUNT_V1,
    );
    expect(boundary.adjacent.signedBytes).toBe(
      MAXIMUM_MINT_POLICY_ADJACENT_SIGNED_BYTES_V1,
    );
    expect(mintField.itemCount).toBe(MAXIMUM_MINT_POLICY_ACCEPTED_COUNT_V1);
    expect(scriptField.itemCount).toBe(MAXIMUM_MINT_POLICY_ACCEPTED_COUNT_V1);
    // §5.6: field 5 is the enveloped per-policy item list, and its decoder is
    // where the one-policy/one-asset shape, the 28-byte policy id, canonical key
    // order and non-zero quantities are enforced. The hand-rolled CBOR walk this
    // replaced re-stated those rules against the retired raw-map form.
    const nativeMintPolicyItems = decodeMidgardMintFieldPreimageV1(
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
        CARDANO_BOUNDARY_MINT_ASSET_NAME_V1.toString("hex"),
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
    }).toEqual(maximumMintTerminalFoldVectorV1);
    // #590 scope item 0: the write channel this suite did not have.
    //
    // The `mint-boundary-v1` fixture in
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
    publishAikenVectorV1("mint-boundary-v1", {
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
              assetNameHex: CARDANO_BOUNDARY_MINT_ASSET_NAME_V1.toString("hex"),
              actualSpendInputCount: acceptedCardano.inputCount,
              actualMintPolicyCount: acceptedCardano.mintPolicyCount,
              actualMintAssetCount: acceptedCardano.mintAssetCount,
              actualNativeScriptWitnessCount:
                acceptedCardano.nativeScriptWitnessCount,
              actualOutputCount: acceptedCardano.outputCount,
              actualVkeyWitnessCount: acceptedCardano.vkeyWitnessCount,
              validityStart: "unset",
              validityEnd: CARDANO_BOUNDARY_OBSERVER_TTL_V1.toString(),
              distinctExpiryStart:
                CARDANO_BOUNDARY_OBSERVER_EXPIRY_BASE_V1.toString(),
              distinctExpiryEnd: (
                CARDANO_BOUNDARY_OBSERVER_EXPIRY_BASE_V1 +
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
              penultimateMintItemHex: encodeMidgardMintPolicyItemV1(
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
