import {
  cardanoTxBytesToMidgardNativeTxCanonicalCborV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
  MIDGARD_CONSENSUS_LIMITS_V1,
  midgardNativeTxFullToCardanoTxEncoding,
} from "@al-ft/midgard-core";
import { CML, Emulator } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import {
  buildSignedCardanoInlineDatumCandidateV1,
  CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
  deterministicCardanoBoundaryPrivateKeyV1,
  exerciseMidgardOrderedCollectionBoundaryV1,
  findSignedCardanoCollectionBoundaryV1,
  measureSignedCardanoInlineDatumV1,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1,
} from "./helpers/ordered-collection-boundary-v1.js";
import { exerciseMidgardRetainedDaBoundaryV1 } from "./helpers/retained-da-boundary-v1.js";

// The exact genuine signed-Cardano inline-datum blob boundary. The terminal
// vector below is the Aiken-replayed half; these four numbers pin the payload
// size and byte count the search must land on, so a silently shrunk blob can no
// longer satisfy the relative bounds alone.
const MAXIMUM_INLINE_DATUM_ACCEPTED_PAYLOAD_BYTES_V1 = 15_680;
const MAXIMUM_INLINE_DATUM_ACCEPTED_SIGNED_BYTES_V1 = 16_383;
const MAXIMUM_INLINE_DATUM_ADJACENT_PAYLOAD_BYTES_V1 = 15_681;
const MAXIMUM_INLINE_DATUM_ADJACENT_SIGNED_BYTES_V1 = 16_385;
const MAXIMUM_INLINE_DATUM_ACCEPTED_DATUM_CBOR_BYTES_V1 = 16_172;
const MAXIMUM_INLINE_DATUM_ADJACENT_DATUM_CBOR_BYTES_V1 = 16_174;

const maximumInlineDatumBlobTerminalVectorV1 = {
  transactionIdHex:
    "e6a945b4b120666ac555e9ca42e714179943da5ebbc3edba81200787d8c7250b",
  transactionCommitmentHex:
    "c6c216b46181a13fa8ad40f5b652842677feb6222ff2b82fbd49df75ded123bc",
  compactCborHex:
    "84018c58204dc5462fa75f970091526b50e11ff2161e020020f791756d77ed6cd8c45d111c5820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f5820d40dc24540734968ab6b8212814ab280ab51e0102c18294b1abf2c9e21db5a871a000d5ec920205820e5ccfcd8e326be04d73634d1ef2cb659e5dd6c49b5ce3e511d57081b54f6e1095820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff58206295d6e5a837fa5a95389ebbd7ad38ffa316e09cd29d28a9e71639cae906aa2c00",
  witnessSetCompactCborHex:
    "835820650b4c39edb0d2b447c9d9f25b892ef1b1e272201ddae9519989ed3ee927f4815820ae7b18490f716b798eb0871325c96023e7e8ba472b7aa0cedcd75cd05f66f76c5820196ccfc47d922bafc8abf3a727aa1afba83b8583e2063c5d281f5d2b60b62ef3",
  fieldPreimageLengthsCborHex: "89182901193f6101010101186801",
  preWorkRootHex:
    "7deabb98099010b3e4bdd504ee11ff1e801104d07c834ab5314a0915a4e9da93",
  postWorkRootHex:
    "444630db7213867459910732a2014ef541e9df14c899cdc119607838f96880b0",
  outputsFieldBytes: 16_225,
  itemLength: 16_221,
  itemCommitmentHex:
    "0b98a091df9a97ac8b6c858f8ac3a50125986f551df3011575575dc58ae422e9",
  collectionFrontierHashHex:
    "a68306b1789aa1bfe5ea824fa0eafe7b7a7f35a9b5b8bad7897d694383c5d99d",
  chunkFrontierHashHex:
    "e28dcceb8c768fd8cfc2c05cb0f713bad5b2e47fb5791ded037f9fe5cf59338b",
  chunkSiblingHexes: [
    "5d5a9d6259d53c1663a3e797b3ba87e4b738b2231ded6225a47b2fb68a3f9837",
    "a427ef63d23f12f91b7c02e0febef3a98754273827f46a36e5545c92e693edf6",
  ],
} as const;

describe("canonical V1 byte-blob Cardano boundary", () => {
  it("reveals one maximum inline-datum output through bounded chunks in both DA classifications", async () => {
    const privateKey = deterministicCardanoBoundaryPrivateKeyV1(0);
    const funder = {
      seedPhrase: "",
      privateKey: privateKey.to_bech32(),
      address: CML.EnterpriseAddress.new(
        0,
        CML.Credential.new_pub_key(privateKey.to_public().hash()),
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
      buildSignedCandidate: (requestedDatumPayloadBytes) =>
        buildSignedCardanoInlineDatumCandidateV1({
          privateKeyBech32: funder.privateKey,
          inputTransactionId: "00".repeat(32),
          inputOutputIndex: 0n,
          inputLovelace: funder.assets.lovelace,
          recipientAddress: funder.address,
          requestedDatumPayloadBytes,
          minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
          minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
          minFeeRefScriptCostPerByte:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeRefScriptCostPerByte,
        }),
    });
    const accepted = measureSignedCardanoInlineDatumV1(
      boundary.accepted.cborHex,
    );
    const adjacent = measureSignedCardanoInlineDatumV1(
      boundary.adjacent.cborHex,
    );

    expect(boundary.accepted.signedBytes).toBeLessThanOrEqual(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect(boundary.adjacent.signedBytes).toBeGreaterThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    expect(boundary.adjacent.requestedItemCount).toBe(
      boundary.accepted.requestedItemCount + 1,
    );
    expect(accepted.datumPayloadBytes).toBe(
      boundary.accepted.requestedItemCount,
    );
    expect(adjacent.datumPayloadBytes).toBe(
      boundary.adjacent.requestedItemCount,
    );
    expect(accepted.outputCount).toBe(1);
    expect(adjacent.outputCount).toBe(1);
    expect(accepted.vkeyWitnessCount).toBe(1);
    expect(adjacent.vkeyWitnessCount).toBe(1);
    expect(accepted.outputAddress).toBe(funder.address);
    expect(adjacent.outputAddress).toBe(funder.address);
    expect(accepted.datumCborBytes).toBeGreaterThan(accepted.datumPayloadBytes);
    expect(adjacent.datumCborBytes).toBeGreaterThan(adjacent.datumPayloadBytes);
    expect(adjacent.datumCborBytes).toBeGreaterThan(accepted.datumCborBytes);

    // The genuine maximum and its immediately adjacent control are exact, not
    // merely "whatever the search returned".
    expect(boundary.accepted.requestedItemCount).toBe(
      MAXIMUM_INLINE_DATUM_ACCEPTED_PAYLOAD_BYTES_V1,
    );
    expect(boundary.accepted.signedBytes).toBe(
      MAXIMUM_INLINE_DATUM_ACCEPTED_SIGNED_BYTES_V1,
    );
    expect(boundary.adjacent.requestedItemCount).toBe(
      MAXIMUM_INLINE_DATUM_ADJACENT_PAYLOAD_BYTES_V1,
    );
    expect(boundary.adjacent.signedBytes).toBe(
      MAXIMUM_INLINE_DATUM_ADJACENT_SIGNED_BYTES_V1,
    );
    expect(accepted.datumCborBytes).toBe(
      MAXIMUM_INLINE_DATUM_ACCEPTED_DATUM_CBOR_BYTES_V1,
    );
    expect(adjacent.datumCborBytes).toBe(
      MAXIMUM_INLINE_DATUM_ADJACENT_DATUM_CBOR_BYTES_V1,
    );

    const midgard = exerciseMidgardOrderedCollectionBoundaryV1({
      signedCardanoCborHex: boundary.accepted.cborHex,
      fieldIndex: 2,
    });
    expect(midgard.itemCount).toBe(1);
    expect(midgard.fieldBytes).toBeLessThanOrEqual(
      MIDGARD_CONSENSUS_LIMITS_V1.maxOutputsPreimageBytes,
    );
    expect(
      midgard.terminalFoldVector.collectionProof.itemLength,
    ).toBeGreaterThan(MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1);
    expect(midgard.maxChunkBytes).toBe(MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1);
    expect(midgard.revealStepCount).toBe(
      Math.ceil(
        midgard.terminalFoldVector.collectionProof.itemLength /
          MIDGARD_BOUNDED_ITEM_CHUNK_BYTES_V1,
      ),
    );
    expect(midgard.maxRevealBytes).toBeLessThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
    );
    const terminal = midgard.terminalFoldVector;
    expect({
      transactionIdHex: terminal.transactionIdHex,
      transactionCommitmentHex: terminal.transactionCommitmentHex,
      compactCborHex: terminal.compactCborHex,
      witnessSetCompactCborHex: terminal.witnessSetCompactCborHex,
      fieldPreimageLengthsCborHex: terminal.fieldPreimageLengthsCborHex,
      preWorkRootHex: terminal.preWorkRootHex,
      postWorkRootHex: terminal.postWorkRootHex,
      outputsFieldBytes: midgard.fieldBytes,
      itemLength: terminal.collectionProof.itemLength,
      itemCommitmentHex: terminal.collectionProof.itemCommitmentHex,
      collectionFrontierHashHex: terminal.collectionProof.frontier[0]?.hashHex,
      chunkFrontierHashHex: terminal.chunkProof.frontier[0]?.hashHex,
      chunkSiblingHexes: terminal.chunkProof.siblingHexes,
    }).toEqual(maximumInlineDatumBlobTerminalVectorV1);
    expect(terminal.collectionProof).toMatchObject({
      fieldIndex: 2,
      itemCount: 1,
      itemIndex: 0,
      frontier: [{ height: 0 }],
      siblingHexes: [],
    });
    expect(terminal.successorPhase).toBe("canonicalDecode");
    expect(terminal.chunkProof).toMatchObject({
      fieldIndex: 2,
      itemIndex: 0,
      totalLength: maximumInlineDatumBlobTerminalVectorV1.itemLength,
      chunkIndex: 3,
      frontier: [{ height: 2 }],
    });
    const terminalChunk = Buffer.from(terminal.chunkProof.chunkHex, "hex");
    expect(terminalChunk).toEqual(
      Buffer.concat([
        Buffer.alloc(41, 0x5a),
        ...Array.from({ length: 59 }, () =>
          Buffer.concat([Buffer.from([0x58, 0x40]), Buffer.alloc(64, 0x5a)]),
        ),
        Buffer.from([0xff]),
      ]),
    );

    const retained = await exerciseMidgardRetainedDaBoundaryV1({
      signedCardanoCborHex: boundary.accepted.cborHex,
      corpusLabel: "maximum-inline-datum-blob",
    });
    expect(retained.normal.reconstructedCanonicalBytes).toBe(
      midgard.nativeCanonicalBytes,
    );
    expect(retained.forced.reconstructedCanonicalBytes).toBe(
      midgard.nativeCanonicalBytes,
    );
    expect(retained.normal.revealStepCount).toBe(midgard.completeFoldStepCount);
    expect(retained.forced.revealStepCount).toBe(midgard.completeFoldStepCount);

    const native = decodeMidgardNativeTxFullV1FromCanonicalCbor(
      cardanoTxBytesToMidgardNativeTxCanonicalCborV1(
        Buffer.from(boundary.accepted.cborHex, "hex"),
      ),
    );
    const roundtrip = measureSignedCardanoInlineDatumV1(
      Buffer.from(midgardNativeTxFullToCardanoTxEncoding(native)).toString(
        "hex",
      ),
    );
    expect({
      outputCount: roundtrip.outputCount,
      address: roundtrip.outputAddress,
      lovelace: roundtrip.outputLovelace,
      datumCborHex: roundtrip.datumCborHex,
    }).toEqual({
      outputCount: accepted.outputCount,
      address: accepted.outputAddress,
      lovelace: accepted.outputLovelace,
      datumCborHex: accepted.datumCborHex,
    });

    const acceptedTransaction = CML.Transaction.from_cbor_hex(
      boundary.accepted.cborHex,
    );
    expect(acceptedTransaction.body().withdrawals()).toBeUndefined();
    expect(acceptedTransaction.body().mint()).toBeUndefined();

    const txHash = await emulator.submitTx(boundary.accepted.cborHex);
    await expect(emulator.awaitTx(txHash)).resolves.toBe(true);

    if (process.env.MIDGARD_PRINT_AIKEN_VECTOR === "1") {
      console.info(
        JSON.stringify({
          blobChunkBoundaryV1: {
            maxTxSize: emulator.protocolParameters.maxTxSize,
            requestedDatumPayloadBytes: boundary.accepted.requestedItemCount,
            datumCborBytes: accepted.datumCborBytes,
            signedCardanoBytes: boundary.accepted.signedBytes,
            signedCardanoByteMargin:
              emulator.protocolParameters.maxTxSize -
              boundary.accepted.signedBytes,
            adjacentDatumPayloadBytes: boundary.adjacent.requestedItemCount,
            adjacentSignedCardanoBytes: boundary.adjacent.signedBytes,
            nativeCanonicalBytes: midgard.nativeCanonicalBytes,
            outputsFieldBytes: midgard.fieldBytes,
            revealSteps: midgard.revealStepCount,
            completeFoldSteps: midgard.completeFoldStepCount,
            terminalFoldVector: midgard.terminalFoldVector,
          },
        }),
      );
    }
  }, 300_000);
});
