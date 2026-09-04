import {
  encodeMidgardSpendInputItem,
  encodeMidgardTxOutput,
} from "@al-ft/midgard-core";
import { CML, Emulator } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { publishAikenVector } from "./helpers/aiken-vector-channel.js";
import {
  buildSignedCardanoReferenceInputsCandidate,
  CARDANO_BOUNDARY_MAX_TX_SIZE,
  deriveCardanoGenesisInputSupply,
  deterministicCardanoBoundaryPrivateKey,
  exerciseMidgardOrderedCollectionBoundary,
  findSignedCardanoCollectionBoundary,
  measureSignedCardanoReferenceInputs,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
} from "./helpers/ordered-collection-boundary.js";
import { exerciseMidgardRetainedDaBoundary } from "./helpers/retained-da-boundary.js";

// The exact genuine signed-Cardano field-1 boundary. Every value below is also
// pinned byte-for-byte by
// `onchain/aiken/lib/midgard/validation-machine-v1.test.ak`
// (`maximum_reference_input_field_terminal_fixture_v1`), so this object is the
// TypeScript half of the cross-language agreement for C20-1.
const MAXIMUM_REFERENCE_INPUT_ACCEPTED_COUNT = 433;
const MAXIMUM_REFERENCE_INPUT_ACCEPTED_SIGNED_BYTES = 16_380;
const MAXIMUM_REFERENCE_INPUT_ADJACENT_COUNT = 434;
const MAXIMUM_REFERENCE_INPUT_ADJACENT_SIGNED_BYTES = 16_418;

const maximumReferenceInputTerminalFoldVector = {
  transactionIdHex:
    "a6bd2cb922f5d1052a1e1efd83bcdd11dd7721160080df149682cf417306b952",
  transactionCommitmentHex:
    "40574dd42f376802bade6c4f6cd872b8a42ef2604bb0369eacde6945336fd387",
  compactCborHex:
    "84018c58202d56d604247c43792618a75b77864f8a6c6d35b9b5a66d25b944476d6930588e5820b6f6734af4f93c51274316e8954c613c86a0ecc5402b4b828ce31325900c817c58209ea027fa938e2c590b0b0352bd6f547b2ccd29b3938947830b3576943d7c0fb71a000d5e452020582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff582073631c952fa8c19edcca913f23c451480f5be10695478f07a691ae19c7b1ccab00",
  witnessSetCompactCborHex:
    "835820f675cfc328f48bfd88c4bc87b4a1e7ae69116c9c98d31b5d5166904637cc7c87582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0",
  fieldPreimageLengthsCborHex: "8918291943ab182c01010101186801",
  fieldCommitmentHex:
    "b6f6734af4f93c51274316e8954c613c86a0ecc5402b4b828ce31325900c817c",
  preWorkRootHex:
    "20c14233d70ec13b8bfdf301e0eb9c670c55e71fe37a27a3a93421ce00b393eb",
  postWorkRootHex:
    "bdc0a9fd3ee84b6e458a9f3c61552e89c41ae213a78893a73679a0336c476305",
  encodedLengthBeforeItem: 17_283,
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
          "bd691df9f8528fc55076ff58c1bd281b20d046dd77d43887c1d53d4d87bd9319",
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
    const spendingKey = deterministicCardanoBoundaryPrivateKey(0);
    const address = CML.EnterpriseAddress.new(
      0,
      CML.Credential.new_pub_key(spendingKey.to_public().hash()),
    )
      .to_address()
      .to_bech32();
    const inputSupply = deriveCardanoGenesisInputSupply(
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );
    const lovelacePerInput = 10_000_000n;
    const emulator = new Emulator(
      Array.from({ length: inputSupply }, () => ({
        seedPhrase: "",
        privateKey: spendingKey.to_bech32(),
        address,
        assets: { lovelace: lovelacePerInput },
      })),
      PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
    );
    const availableInputs = (await emulator.getUtxos(address)).sort(
      (left, right) => left.outputIndex - right.outputIndex,
    );
    expect(availableInputs).toHaveLength(inputSupply);
    for (const [index, input] of availableInputs.entries()) {
      expect(input.txHash).toBe("00".repeat(32));
      expect(input.outputIndex).toBe(index);
    }

    const boundary = await findSignedCardanoCollectionBoundary({
      maxTxSize: emulator.protocolParameters.maxTxSize,
      buildSignedCandidate: (requestedReferenceInputCount) =>
        buildSignedCardanoReferenceInputsCandidate({
          privateKeyBech32: spendingKey.to_bech32(),
          availableInputs,
          recipientAddress: address,
          requestedReferenceInputCount,
          minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeA,
          minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeB,
          minFeeRefScriptCostPerByte:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeRefScriptCostPerByte,
        }),
    });
    const acceptedCardano = measureSignedCardanoReferenceInputs(
      boundary.accepted.cborHex,
    );
    const adjacentCardano = measureSignedCardanoReferenceInputs(
      boundary.adjacent.cborHex,
    );
    const referenceInputField = exerciseMidgardOrderedCollectionBoundary({
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
          // A DA payload UTxO key is a ledger out-ref, so it is §5.3's
          // fixed-index item (`82 ‖ 58 20 tx_id ‖ 19 index_be16`, 38 bytes) and
          // not CML's minimal-index `TransactionInput` CBOR. The strict DA
          // decoder matches these keys against the transaction's field-1
          // reference-input items, which carry exactly these bytes.
          encodeMidgardSpendInputItem({
            txId: CML.TransactionHash.from_hex(input.txHash).to_raw_bytes(),
            outputIndex: input.outputIndex,
          }).toString("hex"),
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
    const retainedDa = await exerciseMidgardRetainedDaBoundary({
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
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );
    expect(boundary.adjacent.requestedItemCount).toBe(
      boundary.accepted.requestedItemCount + 1,
    );
    expect(boundary.adjacent.signedBytes).toBeGreaterThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );
    expect(boundary.adjacent.requestedItemCount + 1).toBeLessThanOrEqual(
      inputSupply,
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
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );

    // The genuine maximum and its immediately adjacent control are exact, not
    // merely "whatever the search returned".
    expect(boundary.accepted.requestedItemCount).toBe(
      MAXIMUM_REFERENCE_INPUT_ACCEPTED_COUNT,
    );
    expect(boundary.accepted.signedBytes).toBe(
      MAXIMUM_REFERENCE_INPUT_ACCEPTED_SIGNED_BYTES,
    );
    expect(boundary.adjacent.requestedItemCount).toBe(
      MAXIMUM_REFERENCE_INPUT_ADJACENT_COUNT,
    );
    expect(boundary.adjacent.signedBytes).toBe(
      MAXIMUM_REFERENCE_INPUT_ADJACENT_SIGNED_BYTES,
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
    }).toEqual(maximumReferenceInputTerminalFoldVector);
    // #590 scope item 0: the write channel this suite did not have.
    //
    // The `reference-inputs-boundary-v1` fixture in
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
    publishAikenVector("reference-inputs-boundary-v1", {
      fieldIndex:
        referenceInputField.terminalFoldVector.collectionProof.fieldIndex,
      itemCount:
        referenceInputField.terminalFoldVector.collectionProof.itemCount,
      itemIndex:
        referenceInputField.terminalFoldVector.collectionProof.itemIndex,
      terminalChunkIndex:
        referenceInputField.terminalFoldVector.chunkProof.chunkIndex,
      encodedLengthBeforeItem:
        referenceInputField.terminalFoldVector.encodedLengthBeforeItem,
      // §8.1's tier-1 carriage: the field's whole §5.1 preimage, which the door
      // hashes once against the flat commitment below.
      fieldPreimageCborHex: referenceInputField.fieldPreimageCborHex,
      fieldCommitmentHex: referenceInputField.fieldCommitmentHex,
      transactionIdHex: referenceInputField.terminalFoldVector.transactionIdHex,
      transactionCommitmentHex:
        referenceInputField.terminalFoldVector.transactionCommitmentHex,
      compactCborHex: referenceInputField.terminalFoldVector.compactCborHex,
      witnessSetCompactCborHex:
        referenceInputField.terminalFoldVector.witnessSetCompactCborHex,
      fieldPreimageLengthsCborHex:
        referenceInputField.terminalFoldVector.fieldPreimageLengthsCborHex,
      validationContextCborHex:
        referenceInputField.terminalFoldVector.validationContextCborHex,
      preWorkRootHex: referenceInputField.terminalFoldVector.preWorkRootHex,
      postWorkRootHex: referenceInputField.terminalFoldVector.postWorkRootHex,
    });

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
