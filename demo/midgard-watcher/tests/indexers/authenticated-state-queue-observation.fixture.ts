import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
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

import { type WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import type { WatcherLocalKupmiosNativeObservation } from "../../src/l1/local-kupmios-native-observation.js";
import type { WatcherNativeBlockAdmission } from "../../src/l1/native-block-admission.js";
import { watcherDeploymentProtocolScriptAuthority } from "../../src/runtime/deployment-identity.js";
import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.js";
import {
  h28,
  h32,
  makeDeploymentAuthority,
} from "../support/deployment-authority-fixture.js";

/** The compiled deployment profile's release depth (3 testing, 30 public). */
export const RELEASE_DEPTH = DEPLOYMENT_MANIFEST_L1_FINALITY.confirmationDepth;

export const RELEASE_DEPTH_TEXT = RELEASE_DEPTH.toString();

const point = Object.freeze({
  blockHash: h32("a1"),
  blockNo: "100",
  slot: "1000",
  pointId: computeFraudProofRawL1PointId({
    blockHash: h32("a1"),
    blockNo: "100",
    slot: "1000",
  }),
});

const deploymentFixture = makeDeploymentAuthority();

export const protocolAuthority = watcherDeploymentProtocolScriptAuthority(
  deploymentFixture.result,
);

export const rehashObservation = (
  observation: WatcherAuthenticatedStateQueueObservation,
): WatcherAuthenticatedStateQueueObservation => {
  const { observationDigest: _ignored, ...body } = observation;
  return {
    ...body,
    observationDigest: watcherSha256CanonicalJson(body),
  };
};

export const value = (policyId: string, assetName: string): CML.Value => {
  const multiasset = CML.MultiAsset.new();
  multiasset.set(
    CML.ScriptHash.from_hex(policyId),
    CML.AssetName.from_hex(assetName),
    1n,
  );
  return CML.Value.new(2_000_000n, multiasset);
};

export const fixture = (omitLock = false) => {
  const deployment = deploymentFixture;
  const authority = protocolAuthority;
  const stateQueueAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.stateQueueSpend),
  );
  const correctionLockAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.correctionLockSpend),
  );
  const rootDatum = Data.to(
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
      value(
        authority.protocolScriptHashes.stateQueueMint,
        SDK.STATE_QUEUE_ROOT_ASSET_NAME,
      ),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(rootDatum)),
      undefined,
    ),
  );
  if (!omitLock) {
    outputs.add(
      CML.TransactionOutput.new(
        CML.Address.from_bech32(correctionLockAddress),
        value(
          authority.protocolScriptHashes.hubOracleMint,
          SDK.CORRECTION_LOCK_ASSET_NAME,
        ),
        CML.DatumOption.new_datum(
          CML.PlutusData.from_cbor_hex(
            Data.to("Idle", SDK.CorrectionLockDatum),
          ),
        ),
        undefined,
      ),
    );
  }
  const body = CML.TransactionBody.new(
    CML.TransactionInputList.new(),
    outputs,
    170_000n,
  );
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(authority.protocolScriptHashes.stateQueueMint),
    CML.AssetName.from_hex(SDK.STATE_QUEUE_ROOT_ASSET_NAME),
    1n,
  );
  mint.set(
    CML.ScriptHash.from_hex(authority.protocolScriptHashes.hubOracleMint),
    CML.AssetName.from_hex(SDK.CORRECTION_LOCK_ASSET_NAME),
    1n,
  );
  body.set_mint(mint);
  const policies = [
    authority.protocolScriptHashes.stateQueueMint,
    authority.protocolScriptHashes.hubOracleMint,
  ].sort();
  const stateQueuePolicyIndex = policies.indexOf(
    authority.protocolScriptHashes.stateQueueMint,
  );
  const redeemerCbor = Data.to(
    { InitV1: { output_index: 0n } },
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
  const nativeBlock = Object.freeze({
    blockHash: point.blockHash,
    blockNo: point.blockNo,
    blockType: "7",
    prevHash: h32("a0"),
    protocolMajor: "9",
    rawBlockCbor: "80",
    rawHeaderCbor: "80",
    schemaVersion: "midgard-watcher-native-block-admission-v1",
    slot: point.slot,
    transactionIds: Object.freeze([txHash]),
    transactionCbors: Object.freeze([transaction.to_canonical_cbor_hex()]),
  }) as WatcherNativeBlockAdmission;
  const localObservation = {
    block: {
      chainPoint: {
        blockHash: point.blockHash,
        blockNo: point.blockNo,
        chainPointId: h32("b1"),
        depth: RELEASE_DEPTH_TEXT,
        parentBlockHash: nativeBlock.prevHash,
        pointDigest: h32("b2"),
        slot: point.slot,
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
    inclusionPoint: point,
    confirmationDepth: RELEASE_DEPTH,
    resolvedInputs: Object.freeze([]),
    resolvedReferenceInputs: Object.freeze([]),
  }) satisfies FraudProofRawL1Transaction;
  return { deployment, authority, nativeBlock, localObservation, raw };
};

export const headerFixture = (): SDK.Header => ({
  prevUtxosRoot: h32("01"),
  utxosRoot: h32("02"),
  withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transactionsRoot: h32("03"),
  depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
  transitionTraceRoot: h32("04"),
  eventToStepRoot: h32("05"),
  validationTracesRoot: h32("08"),
  withdrawalCount: 0n,
  forcedTransactionCount: 0n,
  l2TransactionCount: 1n,
  depositCount: 0n,
  totalEventCount: 1n,
  transitionStepCount: 1n,
  validationTraceCount: 1n,
  startTime: 1n,
  endTime: 2n,
  blockSlot: 0n,
  expectedNetworkId: 0n,
  minFeeA: 0n,
  minFeeB: 0n,
  prevHeaderHash: h28("06"),
  operatorVkey: h28("07"),
  protocolVersion: 1n,
});
