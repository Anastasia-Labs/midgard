import { encodeMidgardTxOutput } from "@al-ft/midgard-core";
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

// The exact genuine signed-Cardano field-1 boundary. Every value below is also
// pinned byte-for-byte by
// `onchain/aiken/lib/midgard/validation-machine-v1.test.ak`
// (`maximum_reference_input_field_terminal_fixture_v1`), so this object is the
// TypeScript half of the cross-language agreement for C20-1.
const MAXIMUM_REFERENCE_INPUT_ACCEPTED_COUNT_V1 = 433;
const MAXIMUM_REFERENCE_INPUT_ACCEPTED_SIGNED_BYTES_V1 = 16_380;
const MAXIMUM_REFERENCE_INPUT_ADJACENT_COUNT_V1 = 434;
const MAXIMUM_REFERENCE_INPUT_ADJACENT_SIGNED_BYTES_V1 = 16_418;

const maximumReferenceInputTerminalFoldVectorV1 = {
  transactionIdHex:
    "cee18ffae3c1e118db1b046c5cc2da1e06cc8c611fe1afd2e6355149e869e3dc",
  transactionCommitmentHex:
    "81de50f4c6b825a90ce4d70bdc89e7062494af859cfeda854dbe335a61c329f0",
  compactCborHex:
    "84018c5820114094118138473ad4d828ed3aa3b5767604cf846235863510ded7f7fb5d36655820e8d8722d2b57d87875a3aead6c1b8ea4aa999d1ad7d8340a712ae2dee01a228458204ddc79a7ae5ce6f67c3282833863d15ffe34a3dcad707bb6b921a31bf9c77b3b1a000d5e4520205820e5ccfcd8e326be04d73634d1ef2cb659e5dd6c49b5ce3e511d57081b54f6e1095820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820d79bc2560eef235bd2a538c7f6110513f9bca34ff66948b14aab16b2c21f5ec600",
  witnessSetCompactCborHex:
    "83582058a2b8a985737738bebe056e227d4b84b4a97c9534a63afd2b10925d2e28b8935820ae7b18490f716b798eb0871325c96023e7e8ba472b7aa0cedcd75cd05f66f76c5820196ccfc47d922bafc8abf3a727aa1afba83b8583e2063c5d281f5d2b60b62ef3",
  fieldPreimageLengthsCborHex: "891827194295182c01010101186801",
  fieldCommitmentHex:
    "e8d8722d2b57d87875a3aead6c1b8ea4aa999d1ad7d8340a712ae2dee01a2284",
  preWorkRootHex:
    "b7ccf4d203ce485dc61512b6680f6a7710f0fc9a57a5732fb838e1ada6b60d3b",
  postWorkRootHex:
    "63b56d7b9c2368b667349fa33f722915691adba711645dce39a556a280a965af",
  encodedLengthBeforeItem: 17_005,
  collectionProof: {
    fieldIndex: 1,
    itemCount: 433,
    itemIndex: 432,
    itemLength: 38,
    itemCommitmentHex:
      "36b4dd3bf311b81d7f82428358cefdd48f2a8a896b105454ac98d24120580a51",
    frontier: [
      {
        height: 0,
        hashHex:
          "f25dce7b75dc95fac3a66ae5014c50b4e3aa3ae235aa70f5cf03d59888aa490d",
      },
      {
        height: 4,
        hashHex:
          "b9dfbe97f48da08a03316da8df54bc031cbbc6acfaf038ee04cf0e33d8ec12f6",
      },
      {
        height: 5,
        hashHex:
          "9328ea0ba44c359f4a243f6573f819a44ed4d70b086a61d0f5aebdf7261bd857",
      },
      {
        height: 7,
        hashHex:
          "266cda5f3304f6975845924b5ceb99aeb2f7379d619bac20f87ea4dd5049d6a5",
      },
      {
        height: 8,
        hashHex:
          "8606d2748f4812aa20f053dfc7660fb46b7f80a2fe4fb75309dc26cf8b0bd9b7",
      },
    ],
    siblingHexes: [],
  },
  chunkProof: {
    fieldIndex: 1,
    itemIndex: 432,
    totalLength: 38,
    chunkIndex: 0,
    chunkHex:
      "82582000000000000000000000000000000000000000000000000000000000000000001901b1",
    frontier: [
      {
        height: 0,
        hashHex:
          "4a6e597090be615a74af8ce771e0d4db02d004da070c991b1dfea5f355a87d78",
      },
    ],
    siblingHexes: [],
  },
} as const;

describe("canonical V1 reference-inputs Cardano boundary", () => {
  it("derives and reveals field 1 using only distinct real emulator UTxOs", async () => {
    const spendingKey = deterministicCardanoBoundaryPrivateKeyV1(0);
    const address = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(spendingKey.to_public().hash()),
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
    const availableInputs = (await emulator.getUtxos(address)).sort(
      (left, right) => left.outputIndex - right.outputIndex,
    );
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
          minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
          minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
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
    const referenceInputField = exerciseMidgardOrderedCollectionBoundaryV1({
      signedCardanoCborHex: boundary.accepted.cborHex,
      fieldIndex: 1,
    });
    const resolvedReferenceUtxos = availableInputs
      .slice(1, boundary.accepted.requestedItemCount + 1)
      .map((input): [string, string] => {
        expect(Object.keys(input.assets)).toEqual(["lovelace"]);
        const lovelace = input.assets.lovelace;
        expect(lovelace).toBeDefined();
        return [
          Buffer.from(
            CML.TransactionInput.new(
              CML.TransactionHash.from_hex(input.txHash),
              BigInt(input.outputIndex),
            ).to_cbor_bytes(),
          ).toString("hex"),
          encodeMidgardTxOutput({
            address: Buffer.from(
              CML.Address.from_bech32(input.address).to_raw_bytes(),
            ),
            value: {
              lovelace: lovelace!,
              assets: new Map(),
            },
          }).toString("hex"),
        ];
      })
      .sort(([left], [right]) => left.localeCompare(right));
    const retainedDa = await exerciseMidgardRetainedDaBoundaryV1({
      signedCardanoCborHex: boundary.accepted.cborHex,
      corpusLabel: "maximum-reference-inputs",
      resolvedReferenceUtxos,
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
      const body = CML.Transaction.from_cbor_hex(candidateCborHex).body();
      expect(body.inputs().len()).toBe(1);
      expect(body.inputs().get(0).transaction_id().to_hex()).toBe(
        "00".repeat(32),
      );
      expect(body.inputs().get(0).index()).toBe(0n);
      const referenceInputs = body.reference_inputs();
      expect(referenceInputs?.len()).toBe(expectedReferenceInputCount);
      for (
        let referenceIndex = 0;
        referenceIndex < expectedReferenceInputCount;
        referenceIndex += 1
      ) {
        const referenceInput = referenceInputs!.get(referenceIndex);
        expect(referenceInput.transaction_id().to_hex()).toBe("00".repeat(32));
        expect(referenceInput.index()).toBe(BigInt(referenceIndex + 1));
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

    // The genuine maximum and its immediately adjacent control are exact, not
    // merely "whatever the search returned".
    expect(boundary.accepted.requestedItemCount).toBe(
      MAXIMUM_REFERENCE_INPUT_ACCEPTED_COUNT_V1,
    );
    expect(boundary.accepted.signedBytes).toBe(
      MAXIMUM_REFERENCE_INPUT_ACCEPTED_SIGNED_BYTES_V1,
    );
    expect(boundary.adjacent.requestedItemCount).toBe(
      MAXIMUM_REFERENCE_INPUT_ADJACENT_COUNT_V1,
    );
    expect(boundary.adjacent.signedBytes).toBe(
      MAXIMUM_REFERENCE_INPUT_ADJACENT_SIGNED_BYTES_V1,
    );
    expect({
      transactionIdHex: referenceInputField.terminalFoldVector.transactionIdHex,
      transactionCommitmentHex:
        referenceInputField.terminalFoldVector.transactionCommitmentHex,
      compactCborHex: referenceInputField.terminalFoldVector.compactCborHex,
      witnessSetCompactCborHex:
        referenceInputField.terminalFoldVector.witnessSetCompactCborHex,
      fieldPreimageLengthsCborHex:
        referenceInputField.terminalFoldVector.fieldPreimageLengthsCborHex,
      fieldCommitmentHex: referenceInputField.fieldCommitmentHex,
      preWorkRootHex: referenceInputField.terminalFoldVector.preWorkRootHex,
      postWorkRootHex: referenceInputField.terminalFoldVector.postWorkRootHex,
      encodedLengthBeforeItem:
        referenceInputField.terminalFoldVector.encodedLengthBeforeItem,
      collectionProof: referenceInputField.terminalFoldVector.collectionProof,
      chunkProof: referenceInputField.terminalFoldVector.chunkProof,
    }).toEqual(maximumReferenceInputTerminalFoldVectorV1);

    const txHash = await emulator.submitTx(boundary.accepted.cborHex);
    await expect(emulator.awaitTx(txHash)).resolves.toBe(true);

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            referenceInputsBoundaryV1: {
              fieldIndex: 1,
              fieldName: "reference_inputs",
              maxTxSize: emulator.protocolParameters.maxTxSize,
              maxValueSize: emulator.protocolParameters.maxValSize,
              realGenesisInputSupply: inputSupply,
              fundingSpendIndex: 0,
              acceptedReferenceIndexes: `1..${boundary.accepted.requestedItemCount.toString()}`,
              adjacentReferenceIndexes: `1..${boundary.adjacent.requestedItemCount.toString()}`,
              requestedReferenceInputCount:
                boundary.accepted.requestedItemCount,
              actualSpendInputCount: acceptedCardano.inputCount,
              actualReferenceInputCount: acceptedCardano.referenceInputCount,
              actualOutputCount: acceptedCardano.outputCount,
              actualVkeyWitnessCount: acceptedCardano.vkeyWitnessCount,
              signedCardanoBytes: boundary.accepted.signedBytes,
              byteMargin:
                emulator.protocolParameters.maxTxSize -
                boundary.accepted.signedBytes,
              fee: boundary.accepted.fee.toString(),
              nativeCanonicalBytes: referenceInputField.nativeCanonicalBytes,
              referenceInputsFieldBytes: referenceInputField.fieldBytes,
              referenceInputItemCount: referenceInputField.itemCount,
              referenceInputRevealSteps: referenceInputField.revealStepCount,
              maxChunkBytes: referenceInputField.maxChunkBytes,
              maxRevealBytes: referenceInputField.maxRevealBytes,
              completeFoldSteps: referenceInputField.completeFoldStepCount,
              adjacentRequestedReferenceInputCount:
                boundary.adjacent.requestedItemCount,
              adjacentActualSpendInputCount: adjacentCardano.inputCount,
              adjacentActualReferenceInputCount:
                adjacentCardano.referenceInputCount,
              adjacentOutputCount: adjacentCardano.outputCount,
              adjacentVkeyWitnessCount: adjacentCardano.vkeyWitnessCount,
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
