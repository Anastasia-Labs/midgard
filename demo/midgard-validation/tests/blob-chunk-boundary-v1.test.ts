import {
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
  MIDGARD_CONSENSUS_LIMITS,
  midgardNativeTxFullToCardanoTxEncoding,
} from "@al-ft/midgard-core";
import { CML, Emulator } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { publishAikenVector } from "./helpers/aiken-vector-channel.js";
import {
  buildSignedCardanoInlineDatumCandidate,
  CARDANO_BOUNDARY_MAX_TX_SIZE,
  deterministicCardanoBoundaryPrivateKey,
  exerciseMidgardOrderedCollectionBoundary,
  findSignedCardanoCollectionBoundary,
  measureSignedCardanoInlineDatum,
  PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
} from "./helpers/ordered-collection-boundary-v1.js";
import { exerciseMidgardRetainedDaBoundary } from "./helpers/retained-da-boundary-v1.js";

// The exact genuine signed-Cardano inline-datum blob boundary. The terminal
// vector below is the Aiken-replayed half; these four numbers pin the payload
// size and byte count the search must land on, so a silently shrunk blob can no
// longer satisfy the relative bounds alone.
const MAXIMUM_INLINE_DATUM_ACCEPTED_PAYLOAD_BYTES = 15_680;
const MAXIMUM_INLINE_DATUM_ACCEPTED_SIGNED_BYTES = 16_383;
const MAXIMUM_INLINE_DATUM_ADJACENT_PAYLOAD_BYTES = 15_681;
const MAXIMUM_INLINE_DATUM_ADJACENT_SIGNED_BYTES = 16_385;
const MAXIMUM_INLINE_DATUM_ACCEPTED_DATUM_CBOR_BYTES = 16_172;
const MAXIMUM_INLINE_DATUM_ADJACENT_DATUM_CBOR_BYTES = 16_174;

const maximumInlineDatumBlobTerminalVector = {
  transactionIdHex:
    "46001b1343578d758a9b543c9d673e65a517038007b8943d223fb795f58844ac",
  transactionCommitmentHex:
    "7eee0e0bdb2a5da4bf0d593800014ec57b6919e9f80008504968f4574a21295a",
  compactCborHex:
    "84018c58202d56d604247c43792618a75b77864f8a6c6d35b9b5a66d25b944476d6930588e582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c058201e56c960b230680d781f77f08341cb9936b0f94813dfc441d8ccfc21ab1f04151a000d5ec92020582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff58203f3ac9a4ac6b4ea0ec0a27b54d7c19126e56c5601b3dacbb8bafdc6a85e75b2c00",
  witnessSetCompactCborHex:
    "835820b404be1942a40954073b837534858a12148fcc2a6b1d27f5b859056a2d683605582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0",
  fieldPreimageLengthsCborHex: "89182901193f6101010101186801",
  preWorkRootHex:
    "f35a2d6843514959acd6bae4307ae79288b282ea911daeacfeb1b70e6b7b2437",
  postWorkRootHex:
    "bb748d14a67b2932d004a45d7bed13b9a619c3ea0ecbf022e367f46bd8cc8303",
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
    const privateKey = deterministicCardanoBoundaryPrivateKey(0);
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
      PREPROD_EPOCH_303_BOUNDARY_PARAMETERS,
    );

    const boundary = await findSignedCardanoCollectionBoundary({
      maxTxSize: emulator.protocolParameters.maxTxSize,
      buildSignedCandidate: (requestedDatumPayloadBytes) =>
        buildSignedCardanoInlineDatumCandidate({
          privateKeyBech32: funder.privateKey,
          inputTransactionId: "00".repeat(32),
          inputOutputIndex: 0n,
          inputLovelace: funder.assets.lovelace,
          recipientAddress: funder.address,
          requestedDatumPayloadBytes,
          minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeA,
          minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeB,
          minFeeRefScriptCostPerByte:
            PREPROD_EPOCH_303_BOUNDARY_PARAMETERS.minFeeRefScriptCostPerByte,
        }),
    });
    const accepted = measureSignedCardanoInlineDatum(boundary.accepted.cborHex);
    const adjacent = measureSignedCardanoInlineDatum(boundary.adjacent.cborHex);

    expect(boundary.accepted.signedBytes).toBeLessThanOrEqual(
      CARDANO_BOUNDARY_MAX_TX_SIZE,
    );
    expect(boundary.adjacent.signedBytes).toBeGreaterThan(
      CARDANO_BOUNDARY_MAX_TX_SIZE,
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
      MAXIMUM_INLINE_DATUM_ACCEPTED_PAYLOAD_BYTES,
    );
    expect(boundary.accepted.signedBytes).toBe(
      MAXIMUM_INLINE_DATUM_ACCEPTED_SIGNED_BYTES,
    );
    expect(boundary.adjacent.requestedItemCount).toBe(
      MAXIMUM_INLINE_DATUM_ADJACENT_PAYLOAD_BYTES,
    );
    expect(boundary.adjacent.signedBytes).toBe(
      MAXIMUM_INLINE_DATUM_ADJACENT_SIGNED_BYTES,
    );
    expect(accepted.datumCborBytes).toBe(
      MAXIMUM_INLINE_DATUM_ACCEPTED_DATUM_CBOR_BYTES,
    );
    expect(adjacent.datumCborBytes).toBe(
      MAXIMUM_INLINE_DATUM_ADJACENT_DATUM_CBOR_BYTES,
    );

    const midgard = exerciseMidgardOrderedCollectionBoundary({
      signedCardanoCborHex: boundary.accepted.cborHex,
      fieldIndex: 2,
    });
    expect(midgard.itemCount).toBe(1);
    expect(midgard.fieldBytes).toBeLessThanOrEqual(
      MIDGARD_CONSENSUS_LIMITS.maxOutputsPreimageBytes,
    );
    expect(
      midgard.terminalFoldVector.collectionProof.itemLength,
    ).toBeGreaterThan(MIDGARD_BOUNDED_ITEM_CHUNK_BYTES);
    expect(midgard.maxChunkBytes).toBe(MIDGARD_BOUNDED_ITEM_CHUNK_BYTES);
    expect(midgard.revealStepCount).toBe(
      Math.ceil(
        midgard.terminalFoldVector.collectionProof.itemLength /
          MIDGARD_BOUNDED_ITEM_CHUNK_BYTES,
      ),
    );
    expect(midgard.maxRevealBytes).toBeLessThan(CARDANO_BOUNDARY_MAX_TX_SIZE);
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
    }).toEqual(maximumInlineDatumBlobTerminalVector);
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
      totalLength: maximumInlineDatumBlobTerminalVector.itemLength,
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

    const retained = await exerciseMidgardRetainedDaBoundary({
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

    const native = decodeMidgardNativeTxFullFromCanonicalCbor(
      cardanoTxBytesToMidgardNativeTxCanonicalCbor(
        Buffer.from(boundary.accepted.cborHex, "hex"),
      ),
    );
    const roundtrip = measureSignedCardanoInlineDatum(
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

    // This suite is the producer for the generated constant block of
    // `onchain/aiken/lib/midgard/fraud-proofs/native-tx.max-inline-datum.test.ak`.
    // Publishing the vector *after* the assertions above is what lets
    // `generate-ordered-collection-boundary-aiken-goldens.mjs` rebind those
    // constants instead of a human retyping them out of a terminal (#588) — which
    // is how that module came to still pin retired counted field commitments
    // after this suite's own expectations had already moved.
    publishAikenVector("blob-chunk-boundary-v1", {
      maxTxSize: emulator.protocolParameters.maxTxSize,
      requestedDatumPayloadBytes: boundary.accepted.requestedItemCount,
      datumCborBytes: accepted.datumCborBytes,
      signedCardanoBytes: boundary.accepted.signedBytes,
      signedCardanoByteMargin:
        emulator.protocolParameters.maxTxSize - boundary.accepted.signedBytes,
      adjacentDatumPayloadBytes: boundary.adjacent.requestedItemCount,
      adjacentSignedCardanoBytes: boundary.adjacent.signedBytes,
      nativeCanonicalBytes: midgard.nativeCanonicalBytes,
      outputsFieldBytes: midgard.fieldBytes,
      revealSteps: midgard.revealStepCount,
      completeFoldSteps: midgard.completeFoldStepCount,
      transactionIdHex: terminal.transactionIdHex,
      transactionCommitmentHex: terminal.transactionCommitmentHex,
      compactCborHex: terminal.compactCborHex,
      witnessSetCompactCborHex: terminal.witnessSetCompactCborHex,
      fieldPreimageLengthsCborHex: terminal.fieldPreimageLengthsCborHex,
      // Not among the pinned expectations above because the machine derives it
      // from the consensus profile rather than from the boundary search, but the
      // Aiken twin has to carry the identical bytes or its work roots cannot
      // agree.
      validationContextCborHex: terminal.validationContextCborHex,
      preWorkRootHex: terminal.preWorkRootHex,
      postWorkRootHex: terminal.postWorkRootHex,
      // The whole terminal fold, beyond the flat constants the generator binds.
      // The Aiken module's proof literals live inside a test function rather than
      // in named constants, so the name-keyed rebinder cannot reach them;
      // publishing them here means the follow-up that gives them a producer has
      // nothing left to find out.
      terminalFoldVector: midgard.terminalFoldVector,
      // #592: `native-tx.max-inline-datum.test.ak` is the one Aiken row whose
      // §8 carriage is not derivable in-module — its field-2 preimage is a single
      // 16,221-byte output item and that module only ever built the terminal
      // 3,936-byte chunk. This is the value it needs. Published as the *field
      // preimage*, one value, with §5.1's splitter applied on the generator side
      // so the Aiken constant is the bare item and the §5.1 envelope is still
      // derived in Aiken by `encode_field_preimage` — one published value that
      // cannot disagree with itself, rather than an item and an envelope that
      // could.
      outputsFieldPreimageCborHex: midgard.fieldPreimageCborHex,
      outputsFieldCommitmentHex: midgard.fieldCommitmentHex,
      terminalChunkIndex: midgard.terminalFoldVector.chunkProof.chunkIndex,
      terminalItemIndex: midgard.terminalFoldVector.collectionProof.itemIndex,
      terminalItemCount: midgard.terminalFoldVector.collectionProof.itemCount,
    });
  }, 300_000);
});
