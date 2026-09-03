import { CML, Emulator } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { publishAikenVector } from "./helpers/aiken-vector-channel.js";
import {
  buildSignedCardanoSpendInputsCandidate,
  CARDANO_BOUNDARY_MAX_TX_SIZE,
  deriveCardanoGenesisInputSupply,
  deterministicCardanoBoundaryPrivateKey,
  exerciseMidgardOrderedCollectionBoundary,
  findSignedCardanoCollectionBoundary,
  measureSignedCardanoSpendInputs,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
} from "./helpers/ordered-collection-boundary-v1.js";
import { exerciseMidgardRetainedDaBoundary } from "./helpers/retained-da-boundary-v1.js";

// The exact genuine signed-Cardano field-0 boundary. Every value below is also
// pinned byte-for-byte by
// `onchain/aiken/lib/midgard/validation-machine-v1.test.ak`
// (`maximum_spend_input_field_terminal_fixture_v1`), so this object is the
// TypeScript half of the cross-language agreement for C20-0.
const MAXIMUM_SPEND_INPUT_ACCEPTED_COUNT = 434;
const MAXIMUM_SPEND_INPUT_ACCEPTED_SIGNED_BYTES = 16_379;
const MAXIMUM_SPEND_INPUT_ADJACENT_COUNT = 435;
const MAXIMUM_SPEND_INPUT_ADJACENT_SIGNED_BYTES = 16_417;

const maximumSpendInputTerminalFoldVector = {
  transactionIdHex:
    "1a24788b66a39ec3a980f70999a30cb532f8dbdd9e86d427666f1c710c821da2",
  transactionCommitmentHex:
    "438e8bf2fa1a91166d32545aa48b202bfd6db6d518021dd441add0943e6a3b64",
  compactCborHex:
    "84018c5820c711b14605dab88657696940a8f08edb424dbf6d655398d0634b6ae04277a6aa582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820861703c5de65d91b0eb30c2749b083d9bb97a20e6dd52daebce7d43b9fc5ddeb1a000d5e192020582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820b0fee96667c5707ce45fd0f9b4b06bbda781069e678ab53683e47cfa5e7299be00",
  witnessSetCompactCborHex:
    "83582095902e309b4dab96b5f91c564b2e393cf88e259b5d0a8cd02a30acab22c02466582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0",
  fieldPreimageLengthsCborHex: "891943d301183001010101186801",
  fieldCommitmentHex:
    "c711b14605dab88657696940a8f08edb424dbf6d655398d0634b6ae04277a6aa",
  preWorkRootHex:
    "476250f69a5619d336a674f519abc6322f35504ad1b37de6e602086d3825fd42",
  postWorkRootHex:
    "09dae6637ffa736e9bad178b60ad59a1572e6817d27d92b74a66602b423be606",
  encodedLengthBeforeItem: 17_323,
  collectionProof: {
    fieldIndex: 0,
    itemCount: 434,
    itemIndex: 433,
    itemLength: 38,
    itemCommitmentHex:
      "dba9086489131c8444128fddbf2a864d16f628c13cac5b157d35b2bffeb8f12c",
    frontier: [
      {
        height: 1,
        hashHex:
          "dd12647adc66bb3ef04f3e994637f7057ca5c4b96b7a58cde6e4f925a34c4d83",
      },
      {
        height: 4,
        hashHex:
          "8539f11555b31aa517cb051cdf1c6849258e396576dc70474c16bd92b53efa01",
      },
      {
        height: 5,
        hashHex:
          "e0c3b95882dc7bdf380e0aa80c42b4a02cb52e77e4627db5e9a0399102039c25",
      },
      {
        height: 7,
        hashHex:
          "548fabcfbe39b4adee5ef017869a04015edfbc9fe2a2c8489a86a8059e956515",
      },
      {
        height: 8,
        hashHex:
          "e9af1286950598c5c2fc6ce275c39ce11a5dada1c59051ea51d6a417f4b2cf82",
      },
    ],
    siblingHexes: [
      "7857404d8ac012dc3b41cc157772693f62616b0fecb0714d7526254f4850a8f0",
    ],
  },
  chunkProof: {
    fieldIndex: 0,
    itemIndex: 433,
    totalLength: 38,
    chunkIndex: 0,
    chunkHex:
      "82582000000000000000000000000000000000000000000000000000000000000000001901b1",
    frontier: [
      {
        height: 0,
        hashHex:
          "cb3168562b9c7affbe10efaa7474882ba25cc037186ac9075b2106d9f68f8d2e",
      },
    ],
    siblingHexes: [],
  },
} as const;

describe("canonical V1 spend-inputs Cardano boundary", () => {
  it("derives and reveals field 0 using only real emulator UTxOs", async () => {
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
      buildSignedCandidate: (requestedInputCount) =>
        buildSignedCardanoSpendInputsCandidate({
          privateKeyBech32: spendingKey.to_bech32(),
          availableInputs,
          recipientAddress: address,
          requestedInputCount,
          minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeA,
          minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeB,
          minFeeRefScriptCostPerByte:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeRefScriptCostPerByte,
        }),
    });
    const acceptedCardano = measureSignedCardanoSpendInputs(
      boundary.accepted.cborHex,
    );
    const adjacentCardano = measureSignedCardanoSpendInputs(
      boundary.adjacent.cborHex,
    );
    const inputField = exerciseMidgardOrderedCollectionBoundary({
      signedCardanoCborHex: boundary.accepted.cborHex,
      fieldIndex: 0,
    });
    const retainedDa = await exerciseMidgardRetainedDaBoundary({
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
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );
    expect(boundary.adjacent.requestedItemCount).toBe(
      boundary.accepted.requestedItemCount + 1,
    );
    expect(boundary.adjacent.signedBytes).toBeGreaterThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );
    expect(boundary.adjacent.requestedItemCount).toBeLessThanOrEqual(
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
    expect(inputField.revealStepCount).toBe(acceptedCardano.inputCount);
    expect(inputField.maxRevealBytes).toBeLessThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );

    // The genuine maximum and its immediately adjacent control are exact, not
    // merely "whatever the search returned".
    expect(boundary.accepted.requestedItemCount).toBe(
      MAXIMUM_SPEND_INPUT_ACCEPTED_COUNT,
    );
    expect(boundary.accepted.signedBytes).toBe(
      MAXIMUM_SPEND_INPUT_ACCEPTED_SIGNED_BYTES,
    );
    expect(boundary.adjacent.requestedItemCount).toBe(
      MAXIMUM_SPEND_INPUT_ADJACENT_COUNT,
    );
    expect(boundary.adjacent.signedBytes).toBe(
      MAXIMUM_SPEND_INPUT_ADJACENT_SIGNED_BYTES,
    );
    expect({
      transactionIdHex: inputField.terminalFoldVector.transactionIdHex,
      transactionCommitmentHex:
        inputField.terminalFoldVector.transactionCommitmentHex,
      compactCborHex: inputField.terminalFoldVector.compactCborHex,
      witnessSetCompactCborHex:
        inputField.terminalFoldVector.witnessSetCompactCborHex,
      fieldPreimageLengthsCborHex:
        inputField.terminalFoldVector.fieldPreimageLengthsCborHex,
      fieldCommitmentHex: inputField.fieldCommitmentHex,
      preWorkRootHex: inputField.terminalFoldVector.preWorkRootHex,
      postWorkRootHex: inputField.terminalFoldVector.postWorkRootHex,
      encodedLengthBeforeItem:
        inputField.terminalFoldVector.encodedLengthBeforeItem,
      collectionProof: inputField.terminalFoldVector.collectionProof,
      chunkProof: inputField.terminalFoldVector.chunkProof,
    }).toEqual(maximumSpendInputTerminalFoldVector);
    // #590 scope item 0: the write channel this suite did not have.
    //
    // The `spend-inputs-boundary-v1` fixture in
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
    publishAikenVector("spend-inputs-boundary-v1", {
      fieldIndex: inputField.terminalFoldVector.collectionProof.fieldIndex,
      itemCount: inputField.terminalFoldVector.collectionProof.itemCount,
      itemIndex: inputField.terminalFoldVector.collectionProof.itemIndex,
      terminalChunkIndex: inputField.terminalFoldVector.chunkProof.chunkIndex,
      encodedLengthBeforeItem:
        inputField.terminalFoldVector.encodedLengthBeforeItem,
      // §8.1's tier-1 carriage: the field's whole §5.1 preimage, which the door
      // hashes once against the flat commitment below.
      fieldPreimageCborHex: inputField.fieldPreimageCborHex,
      fieldCommitmentHex: inputField.fieldCommitmentHex,
      transactionIdHex: inputField.terminalFoldVector.transactionIdHex,
      transactionCommitmentHex:
        inputField.terminalFoldVector.transactionCommitmentHex,
      compactCborHex: inputField.terminalFoldVector.compactCborHex,
      witnessSetCompactCborHex:
        inputField.terminalFoldVector.witnessSetCompactCborHex,
      fieldPreimageLengthsCborHex:
        inputField.terminalFoldVector.fieldPreimageLengthsCborHex,
      validationContextCborHex:
        inputField.terminalFoldVector.validationContextCborHex,
      preWorkRootHex: inputField.terminalFoldVector.preWorkRootHex,
      postWorkRootHex: inputField.terminalFoldVector.postWorkRootHex,
    });

    const txHash = await emulator.submitTx(boundary.accepted.cborHex);
    await expect(emulator.awaitTx(txHash)).resolves.toBe(true);

    if (process.env.MIDGARD_PRINT_PROOF_FIT === "1") {
      console.info(
        JSON.stringify(
          {
            spendInputsBoundaryV1: {
              fieldIndex: 0,
              fieldName: "spend_inputs",
              maxTxSize: emulator.protocolParameters.maxTxSize,
              maxValueSize: emulator.protocolParameters.maxValSize,
              inputSupplyDerivation:
                "floor(maxTxSize / 32-byte transaction id) + 2 adjacent reserve",
              realGenesisInputSupply: inputSupply,
              lovelacePerInput: lovelacePerInput.toString(),
              requestedInputCount: boundary.accepted.requestedItemCount,
              actualInputCount: acceptedCardano.inputCount,
              actualVkeyWitnessCount: acceptedCardano.vkeyWitnessCount,
              signedCardanoBytes: boundary.accepted.signedBytes,
              byteMargin:
                emulator.protocolParameters.maxTxSize -
                boundary.accepted.signedBytes,
              fee: boundary.accepted.fee.toString(),
              nativeCanonicalBytes: inputField.nativeCanonicalBytes,
              spendInputsFieldBytes: inputField.fieldBytes,
              inputItemCount: inputField.itemCount,
              inputRevealSteps: inputField.revealStepCount,
              maxChunkBytes: inputField.maxChunkBytes,
              maxRevealBytes: inputField.maxRevealBytes,
              completeFoldSteps: inputField.completeFoldStepCount,
              adjacentRequestedInputCount: boundary.adjacent.requestedItemCount,
              adjacentActualInputCount: adjacentCardano.inputCount,
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
