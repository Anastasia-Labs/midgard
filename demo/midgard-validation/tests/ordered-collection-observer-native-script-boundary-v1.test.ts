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
    "dea55d4e6a14e025bdce718e1c21ebeeea77723058cabd65d9bbfed76af516af",
  transactionCommitmentHex:
    "8da6f4d07d4cc3728c53bfe50977d38163a24b7f9e03738ec36bc1375bb21a1f",
  compactCborHex:
    "84018c58204dc5462fa75f970091526b50e11ff2161e020020f791756d77ed6cd8c45d111c5820971b52c16ad426099e34913c7b4adc0059f82f4b1025d866f7abcf0df2f00b9f5820598bbaa08e9cc6dc4d9634b23089ead14091f45c5e1165dcf8a6288be95a1b001a000d570d201927105820e127f848e4bda8c1e9b42ddf4c89dfbd1479301dd90551baeff900fdfcec2e975820491655fbd9fd82df78078e397b6785aa4fc65e32b9786bb5e0deda42b351ea745820b6c7c8c1905cda580cf99b528418df3b62a7182102d089fefa4323fbd18ac47d582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab53582001f4b788593d4f70de2a45c2e1e87088bfbdfa29577ae1b62aba60e095e3ab5318ff58205c66a9cb2310e13ca74d861c7d29f5b996030ad755920f1dfc2c325ec3cb015d00",
  witnessSetCompactCborHex:
    "8358209c3c9f949b41759fc4d9ea024e36e2aa7659f3d5dbe41611256f4dfc80a9a62d5820ad4dcd868783831d5bd321d25528c3295f55b1ea6c8d61d85c3216be9a73ea3d5820196ccfc47d922bafc8abf3a727aa1afba83b8583e2063c5d281f5d2b60b62ef3",
  fieldPreimageLengthsCborHex: "89182901182c191a420101192682186801",
  fieldCommitmentHex:
    "e127f848e4bda8c1e9b42ddf4c89dfbd1479301dd90551baeff900fdfcec2e97",
  preWorkRootHex:
    "fe92e3fb9e857d492cdf9a5540313e388c71a0b17af5190bf988ed5bcefb0b02",
  postWorkRootHex:
    "2f4cd74cdbb1cf7c719f50fd4f2be0c49c82fcf05757651e09b9134f4ad9cd0a",
  encodedLengthBeforeItem: 6_692,
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
