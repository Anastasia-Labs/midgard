import {
  computeFraudProofRawL1PointId,
  type FraudProofRawL1Transaction,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import type { WatcherLocalKupmiosNativeObservation } from "../../src/l1/local-kupmios-native-observation.js";
import type { WatcherNativeBlockAdmission } from "../../src/l1/native-block-admission.js";
import { h32 } from "../support/deployment-authority-fixture.js";
import { appendFixture } from "./authenticated-state-queue-observation.append-fixture.js";
import {
  fixture,
  RELEASE_DEPTH,
  RELEASE_DEPTH_TEXT,
  value,
} from "./authenticated-state-queue-observation.fixture.js";

/**
 * A RemoveUnavailableBlockAfterTimeout(RemoveTimedOutHead) transaction that
 * removes the appended head: spends confirmed state, the node, and the Idle
 * CorrectionLock; burns the node token and (unless told otherwise) the DACH
 * the redeemer names.
 */
export const availabilityTimeoutFixture = ({
  initial,
  append,
  challengeAssetName,
  burnChallenge = true,
}: {
  initial: ReturnType<typeof fixture>;
  append: ReturnType<typeof appendFixture>;
  challengeAssetName: string;
  burnChallenge?: boolean;
}) => {
  const hashes = initial.authority.protocolScriptHashes;
  const stateQueueAddress = credentialToAddress(
    initial.authority.network,
    scriptHashToCredential(hashes.stateQueueSpend),
  );
  const correctionLockAddress = credentialToAddress(
    initial.authority.network,
    scriptHashToCredential(hashes.correctionLockSpend),
  );
  const rootDatumCborHex = Data.to(
    SDK.nodeViewToLinkedListDatum({
      key: "Empty",
      next: "Empty",
      data: Data.to([]),
    }),
    SDK.LinkedListDatum,
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(stateQueueAddress),
      value(hashes.stateQueueMint, SDK.STATE_QUEUE_ROOT_ASSET_NAME),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(rootDatumCborHex)),
      undefined,
    ),
  );
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(correctionLockAddress),
      value(hashes.hubOracleMint, SDK.CORRECTION_LOCK_ASSET_NAME),
      CML.DatumOption.new_datum(
        CML.PlutusData.from_cbor_hex(Data.to("Idle", SDK.CorrectionLockDatum)),
      ),
      undefined,
    ),
  );
  const spent = [
    { txHash: append.raw.txHash, index: 0, body: append.raw.bodyCbor },
    { txHash: append.raw.txHash, index: 1, body: append.raw.bodyCbor },
    { txHash: initial.raw.txHash, index: 1, body: initial.raw.bodyCbor },
  ];
  const inputs = CML.TransactionInputList.new();
  for (const input of spent)
    inputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(input.txHash),
        BigInt(input.index),
      ),
    );
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(hashes.stateQueueMint),
    CML.AssetName.from_hex(
      `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${append.headerHash}`,
    ),
    -1n,
  );
  mint.set(
    CML.ScriptHash.from_hex(hashes.availabilityChallengeMint),
    CML.AssetName.from_hex(burnChallenge ? challengeAssetName : h32("dd")),
    -1n,
  );
  body.set_mint(mint);
  const policies = [
    hashes.stateQueueMint,
    hashes.availabilityChallengeMint,
  ].sort();
  const stateQueuePolicyIndex = policies.indexOf(hashes.stateQueueMint);
  const redeemerCbor = Data.to(
    {
      RemoveUnavailableBlockAfterTimeout: {
        yield_to_ref_input_index: 0n,
        unavailable_header_hash: append.headerHash,
        challenge_asset_name: challengeAssetName,
        removal_approach: {
          RemoveTimedOutHead: {
            confirmed_state_input_outref: {
              transactionId: append.raw.txHash,
              outputIndex: 0n,
            },
            confirmed_state_output_index: 0n,
          },
        },
      },
    },
    SDK.StateQueueRedeemer,
  );
  const canonicalRedeemerCbor =
    CML.PlutusData.from_cbor_hex(redeemerCbor).to_canonical_cbor_hex();
  const witnessSet = CML.TransactionWitnessSet.new();
  const witnessRedeemers = CML.LegacyRedeemerList.new();
  witnessRedeemers.add(
    CML.LegacyRedeemer.new(
      CML.RedeemerTag.Mint,
      BigInt(stateQueuePolicyIndex),
      CML.PlutusData.from_cbor_hex(redeemerCbor),
      CML.ExUnits.new(0n, 0n),
    ),
  );
  witnessSet.set_redeemers(
    CML.Redeemers.new_arr_legacy_redeemer(witnessRedeemers),
  );
  const bodyCbor = body.to_canonical_cbor_hex();
  const witnessSetCbor = witnessSet.to_canonical_cbor_hex();
  const txHash = CML.hash_transaction(body).to_hex();
  const transaction = CML.Transaction.new(body, witnessSet, true, undefined);
  const blockHash = h32("a3");
  const removalPoint = Object.freeze({
    blockHash,
    blockNo: "102",
    slot: "1002",
    pointId: computeFraudProofRawL1PointId({
      blockHash,
      blockNo: "102",
      slot: "1002",
    }),
  });
  const nativeBlock = Object.freeze({
    blockHash,
    blockNo: removalPoint.blockNo,
    blockType: "7",
    prevHash: append.nativeBlock.blockHash,
    protocolMajor: "9",
    rawBlockCbor: "80",
    rawHeaderCbor: "80",
    schemaVersion: "midgard-watcher-native-block-admission-v1",
    slot: removalPoint.slot,
    transactionIds: Object.freeze([txHash]),
    transactionCbors: Object.freeze([transaction.to_canonical_cbor_hex()]),
  }) as WatcherNativeBlockAdmission;
  const localObservation = {
    block: {
      chainPoint: {
        blockHash,
        blockNo: removalPoint.blockNo,
        chainPointId: h32("b5"),
        depth: RELEASE_DEPTH_TEXT,
        parentBlockHash: nativeBlock.prevHash,
        pointDigest: h32("b6"),
        slot: removalPoint.slot,
      },
      transactions: [
        {
          txHash,
          body: { bytesHex: bodyCbor },
          witnessSet: { bytesHex: witnessSetCbor },
          redeemers: [
            {
              purpose: "mint",
              index: stateQueuePolicyIndex.toString(),
              bytes: { bytesHex: canonicalRedeemerCbor },
            },
          ],
        },
      ],
    },
  } as unknown as WatcherLocalKupmiosNativeObservation;
  const raw = Object.freeze({
    txHash,
    bodyCbor,
    witnessSetCbor,
    redeemersCbor: witnessSet.redeemers()!.to_canonical_cbor_hex(),
    isValid: true,
    inclusionPoint: removalPoint,
    confirmationDepth: RELEASE_DEPTH,
    resolvedInputs: Object.freeze(
      spent.map((input) => {
        const output = CML.TransactionBody.from_cbor_hex(input.body)
          .outputs()
          .get(input.index);
        return {
          outRef: `${input.txHash}#${input.index.toString()}`,
          outputCbor: output.to_canonical_cbor_hex(),
          datumCbor: output.datum()!.as_datum()!.to_canonical_cbor_hex(),
          referenceScriptCbor: null,
        };
      }),
    ),
    resolvedReferenceInputs: Object.freeze([]),
  }) satisfies FraudProofRawL1Transaction;
  return { nativeBlock, localObservation, raw };
};
