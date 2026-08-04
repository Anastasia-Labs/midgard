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

// The exact genuine signed-Cardano field-0 boundary. Every value below is also
// pinned byte-for-byte by
// `onchain/aiken/lib/midgard/validation-machine-v1.test.ak`
// (`maximum_spend_input_field_terminal_fixture_v1`), so this object is the
// TypeScript half of the cross-language agreement for C20-0.
const MAXIMUM_SPEND_INPUT_ACCEPTED_COUNT_V1 = 434;
const MAXIMUM_SPEND_INPUT_ACCEPTED_SIGNED_BYTES_V1 = 16_379;
const MAXIMUM_SPEND_INPUT_ADJACENT_COUNT_V1 = 435;
const MAXIMUM_SPEND_INPUT_ADJACENT_SIGNED_BYTES_V1 = 16_417;

const maximumSpendInputTerminalFoldVectorV1 = {
  transactionIdHex:
    "5616fc26bfef26893b4d4413a9b75af4275feed78dd7e13000d52af2b221402b",
  transactionCommitmentHex:
    "c91b1c78baee450b8138a85aa4febfd6d9b5e6e208633fef29d4cec4e2c63d0b",
  compactCborHex:
    "84018c5820fa49d0bce0fd16e8a8f20703162027ce5d24e4de5872def31c5e89f46d27cdb25820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f58204b9a64944d619f32cd8c14e88f53c67857b717dd0686b1a20838c8e815308dce1a000d5e1920205820e5ccfcd8e326be04d73634d1ef2cb659e5dd6c49b5ce3e511d57081b54f6e1095820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff5820ecaa2a4ca3d5110e0d124e3b3da2012a20c326fd6bdc25c1b71e67e9b9a04a9e00",
  witnessSetCompactCborHex:
    "835820bdb04d390efaca1a689981865803b28a488f2c518ce905626962c7a7cebb7be25820ae7b18490f716b798eb0871325c96023e7e8ba472b7aa0cedcd75cd05f66f76c5820196ccfc47d922bafc8abf3a727aa1afba83b8583e2063c5d281f5d2b60b62ef3",
  fieldPreimageLengthsCborHex: "891942bb01183001010101186801",
  fieldCommitmentHex:
    "fa49d0bce0fd16e8a8f20703162027ce5d24e4de5872def31c5e89f46d27cdb2",
  preWorkRootHex:
    "99ce6f53c84ad64e689f582577913b117d7f86257bffb96308554678d1506973",
  postWorkRootHex:
    "d0354c24ccee8206b9926ca2662ea50639e0a114a5b60f4173eb986a94792cef",
  encodedLengthBeforeItem: 17_043,
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
          "2ec5b0bf539013547a4e08389d40946112938257232a275eabda35f47a512ef8",
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
      buildSignedCandidate: (requestedInputCount) =>
        buildSignedCardanoSpendInputsCandidateV1({
          privateKeyBech32: spendingKey.to_bech32(),
          availableInputs,
          recipientAddress: address,
          requestedInputCount,
          minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
          minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
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
    expect(inputField.revealStepCount).toBe(acceptedCardano.inputCount);
    expect(inputField.maxRevealBytes).toBeLessThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );

    // The genuine maximum and its immediately adjacent control are exact, not
    // merely "whatever the search returned".
    expect(boundary.accepted.requestedItemCount).toBe(
      MAXIMUM_SPEND_INPUT_ACCEPTED_COUNT_V1,
    );
    expect(boundary.accepted.signedBytes).toBe(
      MAXIMUM_SPEND_INPUT_ACCEPTED_SIGNED_BYTES_V1,
    );
    expect(boundary.adjacent.requestedItemCount).toBe(
      MAXIMUM_SPEND_INPUT_ADJACENT_COUNT_V1,
    );
    expect(boundary.adjacent.signedBytes).toBe(
      MAXIMUM_SPEND_INPUT_ADJACENT_SIGNED_BYTES_V1,
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
    }).toEqual(maximumSpendInputTerminalFoldVectorV1);

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
