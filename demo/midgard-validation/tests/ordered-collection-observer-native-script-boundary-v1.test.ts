import { midgardFieldHeaderLengthForCountV1 } from "@al-ft/midgard-core";
import { CML, Emulator } from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { publishAikenVectorV1 } from "./helpers/aiken-vector-channel.js";
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

// The exact genuine signed-Cardano field-3 boundary. Every value below is also
// pinned byte-for-byte by
// `onchain/aiken/lib/midgard/validation-machine-v1.test.ak`
// (`cek_context_observer_cardano_maximum_224_first_item_and_terminal_agree`
// and `maximum_observer_field_terminal_fixture_v1`), so this object is the
// TypeScript half of the cross-language agreement for C20-3.
const MAXIMUM_OBSERVER_ACCEPTED_COUNT_V1 = 224;
const MAXIMUM_OBSERVER_ACCEPTED_SIGNED_BYTES_V1 = 16_338;
const MAXIMUM_OBSERVER_ADJACENT_COUNT_V1 = 225;
const MAXIMUM_OBSERVER_ADJACENT_SIGNED_BYTES_V1 = 16_410;

const maximumObserverTerminalFoldVectorV1 = {
  transactionIdHex:
    "b16b2f9ffdef875e489d13c518def5771131b15832e88cd0f79d6d3956fe168b",
  transactionCommitmentHex:
    "5b79d540d0a51bb5e2ab678c63f5b8b88646d13e337c1d33f1e1ba6ca2db4d95",
  compactCborHex:
    "84018c58202d56d604247c43792618a75b77864f8a6c6d35b9b5a66d25b944476d6930588e582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c05820b2b468552a6382aa0a02ece7767ee914a08a85a2527b117e048ca891ba560f351a000d570d2019271058206ba17d1becf6846cbc137424e22d01732cdefc5a2b14105f47babd83e6550668582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff582059778271799fb2f636923a2cb70dac0426ba8aa42ab730db4166b1968f01b29000",
  witnessSetCompactCborHex:
    "83582091bef5c8f29aa474731c2ad6b30f1872403c692814a9bfd55e70687bfdc74810582039728233c559f2fdc7876f58aad839f313ea4af375a4e91d60e63e1a55adbe2e582045b0cfc220ceec5b7c1c62c4d4193d38e4eba48e8815729ce75f9c0ab0e4c1c0",
  fieldPreimageLengthsCborHex: "89182901182c191a420101192842186801",
  fieldCommitmentHex:
    "6ba17d1becf6846cbc137424e22d01732cdefc5a2b14105f47babd83e6550668",
  preWorkRootHex:
    "1078262a33daff30359c3e2b00ebc601b2058fd77e7587bdae0d67baf5478e19",
  postWorkRootHex:
    "5e98b6a8a98432b486d068899b3dad627f8c41dd6ae8afef51976b88fa0d84d4",
  encodedLengthBeforeItem: 6692,
  collectionProof: {
    fieldIndex: 3,
    itemCount: 224,
    itemIndex: 223,
    itemLength: 28,
    itemCommitmentHex:
      "eea077178a4c1efc0237062dd79d0e88e25f226a75c50e2941386c551c8dbdf1",
    frontier: [
      {
        height: 5,
        hashHex:
          "b3d0869ae6f779e2a3bd2657550bad972479f7bd2bf30525d7d6ea1f6e273219",
      },
      {
        height: 6,
        hashHex:
          "8b8872929417945fc60a184c86b36e6cb8b3f79f063a2b1bd74276e3e6dd4561",
      },
      {
        height: 7,
        hashHex:
          "4f23682ce5aac57bdd356897e085c19da0956f3c43a4d0a590e06931693ec7aa",
      },
    ],
    siblingHexes: [
      "37a247add2ab2bb8dd1ca90bc7340ccbd288795241423781485f83f1d2404441",
      "12cbd11747b0ee2b75df2dbf849eb190bfa2ad508af765c0ef769438b6b697e4",
      "761dcb1c5ea56450765f089f956ac800528d0d03b602a5f2ebdbcfdc391aeb92",
      "38c8a30b4b417afa1434d182e99e1c5f595d57dfd0643c0e49e8c1975d60884c",
      "637359bdbf37e81101c411bd6f8f72efd3bf2983764dfe73da65ebc09e999582",
    ],
  },
  chunkProof: {
    fieldIndex: 3,
    itemIndex: 223,
    totalLength: 28,
    chunkIndex: 0,
    chunkHex: "ffab1dd64f82b6991818c1ecc5047d52ce5d00f6fdbc5023e2980167",
    frontier: [
      {
        height: 0,
        hashHex:
          "770f825142ee9790250392a5b0310c8e2a8e833eaa6c62ca6f7223b5ed9e2288",
      },
    ],
    siblingHexes: [],
  },
} as const;

describe("canonical V1 observer/native-script Cardano boundary", () => {
  it("couples every field-3 observer to one real field-6 native-script witness", async () => {
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
        minFeeA: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeA,
        minFeeB: PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeB,
        minFeeRefScriptCostPerByte:
          PREPROD_EPOCH_303_BOUNDARY_PARAMETERS_V1.minFeeRefScriptCostPerByte,
      });

    const firstCandidate = await buildCandidate(1);
    const firstMeasurement = measureSignedCardanoObserverNativeScriptsV1(
      firstCandidate.cborHex,
    );
    const firstObserverField = exerciseMidgardOrderedCollectionBoundaryV1({
      signedCardanoCborHex: firstCandidate.cborHex,
      fieldIndex: 3,
    });
    const firstScriptField = exerciseMidgardOrderedCollectionBoundaryV1({
      signedCardanoCborHex: firstCandidate.cborHex,
      fieldIndex: 6,
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
    const acceptedCardano = measureSignedCardanoObserverNativeScriptsV1(
      boundary.accepted.cborHex,
    );
    const adjacentCardano = measureSignedCardanoObserverNativeScriptsV1(
      boundary.adjacent.cborHex,
    );
    const observerField = exerciseMidgardOrderedCollectionBoundaryV1({
      signedCardanoCborHex: boundary.accepted.cborHex,
      fieldIndex: 3,
    });
    const scriptField = exerciseMidgardOrderedCollectionBoundaryV1({
      signedCardanoCborHex: boundary.accepted.cborHex,
      fieldIndex: 6,
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
      expect(measurement.withdrawalCount).toBe(expectedObserverCount);
      expect(measurement.nativeScriptWitnessCount).toBe(expectedObserverCount);
      expect(measurement.outputCount).toBe(1);
      expect(measurement.vkeyWitnessCount).toBe(1);
      expect(measurement.validityStart).toBeUndefined();
      expect(measurement.ttl).toBe(CARDANO_BOUNDARY_OBSERVER_TTL_V1);
      expect(measurement.withdrawalAmounts).toEqual(
        Array.from({ length: expectedObserverCount }, () => 0n),
      );
      expect([...measurement.observerScriptHashHexes].sort()).toEqual(
        [...measurement.nativeScriptHashHexes].sort(),
      );
      expect(new Set(measurement.observerScriptHashHexes).size).toBe(
        expectedObserverCount,
      );
      expect(measurement.hasPlutusScripts).toBe(false);
      expect(measurement.hasRedeemers).toBe(false);
      expect(measurement.hasDatums).toBe(false);
      expect(measurement.collateralInputCount).toBe(0);

      const transaction = CML.Transaction.from_cbor_hex(candidateCborHex);
      const nativeScripts = transaction.witness_set().native_scripts();
      const signerHashes = CML.Ed25519KeyHashList.new();
      signerHashes.add(spendingKeyHash);
      expect(nativeScripts?.len()).toBe(expectedObserverCount);
      for (
        let scriptIndex = 0;
        scriptIndex < expectedObserverCount;
        scriptIndex += 1
      ) {
        expect(
          nativeScripts!
            .get(scriptIndex)
            .verify(undefined, CARDANO_BOUNDARY_OBSERVER_TTL_V1, signerHashes),
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

    expect(observerField.itemCount).toBe(acceptedCardano.withdrawalCount);
    expect(observerField.revealStepCount).toBe(acceptedCardano.withdrawalCount);
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

    // The genuine 224-observer maximum and its immediately adjacent
    // 225-observer rejection are exact, not merely "whatever the search
    // returned", and the field-3 terminal is the same one Aiken replays.
    expect(boundary.accepted.requestedItemCount).toBe(
      MAXIMUM_OBSERVER_ACCEPTED_COUNT_V1,
    );
    expect(boundary.accepted.signedBytes).toBe(
      MAXIMUM_OBSERVER_ACCEPTED_SIGNED_BYTES_V1,
    );
    expect(boundary.adjacent.requestedItemCount).toBe(
      MAXIMUM_OBSERVER_ADJACENT_COUNT_V1,
    );
    expect(boundary.adjacent.signedBytes).toBe(
      MAXIMUM_OBSERVER_ADJACENT_SIGNED_BYTES_V1,
    );
    expect({
      transactionIdHex: observerField.terminalFoldVector.transactionIdHex,
      transactionCommitmentHex:
        observerField.terminalFoldVector.transactionCommitmentHex,
      compactCborHex: observerField.terminalFoldVector.compactCborHex,
      witnessSetCompactCborHex:
        observerField.terminalFoldVector.witnessSetCompactCborHex,
      fieldPreimageLengthsCborHex:
        observerField.terminalFoldVector.fieldPreimageLengthsCborHex,
      fieldCommitmentHex: observerField.fieldCommitmentHex,
      preWorkRootHex: observerField.terminalFoldVector.preWorkRootHex,
      postWorkRootHex: observerField.terminalFoldVector.postWorkRootHex,
      encodedLengthBeforeItem:
        observerField.terminalFoldVector.encodedLengthBeforeItem,
      collectionProof: observerField.terminalFoldVector.collectionProof,
      chunkProof: observerField.terminalFoldVector.chunkProof,
    }).toEqual(maximumObserverTerminalFoldVectorV1);
    // This suite is the producer for the C20-6 constant family in
    // `onchain/aiken/lib/midgard/fraud-proofs/native-tx-v1.test.ak`: field 6's
    // 224 native-script witnesses, their preimage, and the compact forms that
    // bind them. Publishing the vector after the assertions above is what lets
    // `generate-ordered-collection-boundary-aiken-goldens.mjs` rebind those
    // constants instead of a human retyping ~10 kB of hex (#588).
    // §5.1's wrapped width of one field-6 item at this boundary — what the field
    // grows by when one identical native script is added. Every item here is the
    // same synthetic signer/expiry script, so the field is exactly its array
    // header plus `count` strides, and the stride follows from that rather than
    // from a literal. Published because the envelope moved it: under the retired
    // counted grammar field 6 concatenated raw item CBOR, so the delta was the
    // item alone, with no wrapper.
    const scriptWitnessItemStrideBytesV1 =
      (scriptField.fieldBytes -
        midgardFieldHeaderLengthForCountV1(scriptField.itemCount)) /
      scriptField.itemCount;
    expect(Number.isInteger(scriptWitnessItemStrideBytesV1)).toBe(true);
    publishAikenVectorV1("observer-native-script-boundary-v1", {
      nativeScriptWitnessCount: acceptedCardano.nativeScriptWitnessCount,
      acceptedSignedCardanoBytes: boundary.accepted.signedBytes,
      adjacentSignedCardanoBytes: boundary.adjacent.signedBytes,
      cardanoMaxTransactionBytes: CARDANO_BOUNDARY_MAX_TX_SIZE_V1,
      observerExpiryBase: Number(CARDANO_BOUNDARY_OBSERVER_EXPIRY_BASE_V1),
      scriptWitnessFieldBytes: scriptField.fieldBytes,
      scriptWitnessItemStrideBytes: scriptWitnessItemStrideBytesV1,
      nativeCanonicalBytes: scriptField.nativeCanonicalBytes,
      scriptWitnessFieldPreimageCborHex: scriptField.fieldPreimageCborHex,
      scriptWitnessFieldPreimageHashHex: scriptField.fieldPreimageHashHex,
      scriptWitnessFieldCommitmentHex: scriptField.fieldCommitmentHex,
      transactionIdHex: scriptField.terminalFoldVector.transactionIdHex,
      transactionCommitmentHex:
        scriptField.terminalFoldVector.transactionCommitmentHex,
      compactCborHex: scriptField.terminalFoldVector.compactCborHex,
      witnessSetCompactCborHex:
        scriptField.terminalFoldVector.witnessSetCompactCborHex,
      fieldPreimageLengthsCborHex:
        scriptField.terminalFoldVector.fieldPreimageLengthsCborHex,
      observerFieldTerminalFoldVector: observerField.terminalFoldVector,
    });

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
              scriptWitnessFieldIndex: 6,
              maxTxSize: emulator.protocolParameters.maxTxSize,
              maxValueSize: emulator.protocolParameters.maxValSize,
              fixtureGenerationBasis:
                "on-demand until exact signed bytes exceed maxTxSize",
              actualSpendInputCount: acceptedCardano.inputCount,
              actualObserverWithdrawalCount: acceptedCardano.withdrawalCount,
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
              nativeCanonicalBytes: observerField.nativeCanonicalBytes,
              observerFieldBytes: observerField.fieldBytes,
              observerItems: observerField.itemCount,
              observerRevealSteps: observerField.revealStepCount,
              observerMaxChunkBytes: observerField.maxChunkBytes,
              observerMaxRevealBytes: observerField.maxRevealBytes,
              scriptWitnessFieldBytes: scriptField.fieldBytes,
              scriptWitnessItemStrideBytes: scriptWitnessItemStrideBytesV1,
              scriptWitnessItems: scriptField.itemCount,
              scriptWitnessRevealSteps: scriptField.revealStepCount,
              scriptWitnessMaxChunkBytes: scriptField.maxChunkBytes,
              scriptWitnessMaxRevealBytes: scriptField.maxRevealBytes,
              completeFoldSteps: observerField.completeFoldStepCount,
              scriptWitnessTerminalFoldVector: scriptField.terminalFoldVector,
              adjacentRequestedObserverCount:
                boundary.adjacent.requestedItemCount,
              adjacentObserverWithdrawalCount: adjacentCardano.withdrawalCount,
              adjacentNativeScriptWitnessCount:
                adjacentCardano.nativeScriptWitnessCount,
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
