import { computeHash28 } from "@al-ft/midgard-core/codec/hash";
import {
  computeFraudProofRawL1PointIdV1,
  computeFraudProofReleaseFinalityPolicyDigestV1,
  createLocalKupmiosHttpOgmiosRawSourceV1,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_V1_SCHEMA_VERSION,
  type FraudProofRawL1PointV1,
  type FraudProofRawL1TransactionV1,
  LocalKupmiosExactPointNotCanonicalV1Error,
  validateVerifiedFraudProofReleaseFinalityPolicyV1,
} from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  Data,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";
import { describe, expect, it } from "vitest";

import { watcherDeploymentProtocolScriptAuthorityV1 } from "../src/deployment-identity.js";
import { watcherSha256CanonicalJsonV1 } from "../src/durable-store.js";
import type { WatcherLocalKupmiosNativeObservationV1 } from "../src/local-kupmios-native-observation-v1.js";
import type { WatcherNativeBlockAdmissionV1 } from "../src/native-block-admission-v1.js";
import {
  assertWatcherProductionStateQueueHeaderObservationV1,
  assertWatcherProductionStateQueueObservationV1,
  createWatcherProductionStateQueueObservationSourceV1,
  unsafeDeriveFraudProofCorrectionIdentityForTest,
  unsafeDeriveWatcherProductionStateQueueObservationForTest,
  unsafeResolveRetainedWatcherStateQueueHeaderForTest,
  unsafeRestoreLongestWatcherProductionStateQueuePrefixForTest,
  unsafeRestorePersistedWatcherProductionStateQueueObservationForTest,
  unsafeSelectWatcherStateQueueRawCandidatesForTest,
  unsafeSnapshotWatcherProductionStateQueueAtBoundaryForTest,
  type WatcherProductionStateQueueObservationV1,
} from "../src/production-state-queue-observation-v1.js";
import {
  h28,
  h32,
  makeDeploymentAuthority,
} from "./support/deployment-authority-fixture.js";

const point = Object.freeze({
  blockHash: h32("a1"),
  blockNo: "100",
  slot: "1000",
  pointId: computeFraudProofRawL1PointIdV1({
    blockHash: h32("a1"),
    blockNo: "100",
    slot: "1000",
  }),
});

const deploymentFixture = makeDeploymentAuthority();
const protocolAuthority = watcherDeploymentProtocolScriptAuthorityV1(
  deploymentFixture.result,
);

const rehashObservation = (
  observation: WatcherProductionStateQueueObservationV1,
): WatcherProductionStateQueueObservationV1 => {
  const { observationDigest: _ignored, ...body } = observation;
  return {
    ...body,
    observationDigest: watcherSha256CanonicalJsonV1(body),
  };
};

const value = (policyId: string, assetName: string): CML.Value => {
  const multiasset = CML.MultiAsset.new();
  multiasset.set(
    CML.ScriptHash.from_hex(policyId),
    CML.AssetName.from_hex(assetName),
    1n,
  );
  return CML.Value.new(2_000_000n, multiasset);
};

const fixture = (omitLock = false) => {
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
  }) as WatcherNativeBlockAdmissionV1;
  const localObservation = {
    block: {
      chainPoint: {
        blockHash: point.blockHash,
        blockNo: point.blockNo,
        chainPointId: h32("b1"),
        depth: "30",
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
  } as unknown as WatcherLocalKupmiosNativeObservationV1;
  const raw = Object.freeze({
    txHash,
    bodyCbor,
    witnessSetCbor,
    redeemersCbor: witnessSet.redeemers()!.to_canonical_cbor_hex(),
    isValid: true,
    inclusionPoint: point,
    confirmationDepth: 30,
    resolvedInputs: Object.freeze([]),
    resolvedReferenceInputs: Object.freeze([]),
  }) satisfies FraudProofRawL1TransactionV1;
  return { deployment, authority, nativeBlock, localObservation, raw };
};

const headerFixture = (): SDK.HeaderV1 => ({
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

const appendFixture = ({
  initial,
  previous,
  nodeHeader = headerFixture(),
  assetHeaderHash,
  daAvailability = "Unattested",
}: {
  initial: ReturnType<typeof fixture>;
  previous: WatcherProductionStateQueueObservationV1;
  nodeHeader?: SDK.HeaderV1;
  assetHeaderHash?: string;
  daAvailability?: SDK.DaAvailabilityStateQueueStatusV1;
}) => {
  const stateQueueAddress = credentialToAddress(
    initial.authority.network,
    scriptHashToCredential(
      initial.authority.protocolScriptHashes.stateQueueSpend,
    ),
  );
  const headerCborHex = Data.to(nodeHeader, SDK.HeaderV1);
  const computedHeaderHash = computeHash28(
    Buffer.from(headerCborHex, "hex"),
  ).toString("hex");
  const nodeAssetHeaderHash = assetHeaderHash ?? computedHeaderHash;
  const stateQueueNode: SDK.StateQueueNodeV1 = {
    header: nodeHeader,
    da_attestation: daAvailability,
  };
  const rootDatumCborHex = Data.to(
    SDK.nodeViewToLinkedListDatum({
      key: "Empty",
      next: { Key: { key: nodeAssetHeaderHash } },
      data: Data.to([]),
    }),
    SDK.LinkedListDatum,
  );
  const nodeDatumCborHex = Data.to(
    SDK.nodeViewToLinkedListDatum({
      key: { Key: { key: nodeAssetHeaderHash } },
      next: "Empty",
      data: Data.castTo(stateQueueNode, SDK.StateQueueNodeV1),
    }),
    SDK.LinkedListDatum,
  );
  const outputs = CML.TransactionOutputList.new();
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(stateQueueAddress),
      value(
        initial.authority.protocolScriptHashes.stateQueueMint,
        SDK.STATE_QUEUE_ROOT_ASSET_NAME,
      ),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(rootDatumCborHex)),
      undefined,
    ),
  );
  outputs.add(
    CML.TransactionOutput.new(
      CML.Address.from_bech32(stateQueueAddress),
      value(
        initial.authority.protocolScriptHashes.stateQueueMint,
        `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${nodeAssetHeaderHash}`,
      ),
      CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex(nodeDatumCborHex)),
      undefined,
    ),
  );
  const inputs = CML.TransactionInputList.new();
  inputs.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(initial.raw.txHash),
      0n,
    ),
  );
  const references = CML.TransactionInputList.new();
  references.add(
    CML.TransactionInput.new(
      CML.TransactionHash.from_hex(initial.raw.txHash),
      1n,
    ),
  );
  const body = CML.TransactionBody.new(inputs, outputs, 170_000n);
  body.set_reference_inputs(references);
  const mint = CML.Mint.new();
  mint.set(
    CML.ScriptHash.from_hex(
      initial.authority.protocolScriptHashes.stateQueueMint,
    ),
    CML.AssetName.from_hex(
      `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${nodeAssetHeaderHash}`,
    ),
    1n,
  );
  body.set_mint(mint);
  const redeemerCbor = Data.to(
    {
      CommitBlockHeader: {
        new_block_output_index: 1n,
        continued_latest_block_output_index: 0n,
        operator: nodeHeader.operatorVkey,
        scheduler_ref_input_index: 0n,
        active_operators_input_index: 0n,
        active_operators_redeemer_index: 0n,
        m_confirmed_state_ref_input_index: null,
        m_head_state_queue_node_ref_input_index: null,
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
      0n,
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
  const blockHash = h32("a2");
  const appendPoint = Object.freeze({
    blockHash,
    blockNo: "101",
    slot: "1001",
    pointId: computeFraudProofRawL1PointIdV1({
      blockHash,
      blockNo: "101",
      slot: "1001",
    }),
  });
  const nativeBlock = Object.freeze({
    blockHash,
    blockNo: appendPoint.blockNo,
    blockType: "7",
    prevHash: initial.nativeBlock.blockHash,
    protocolMajor: "9",
    rawBlockCbor: "80",
    rawHeaderCbor: "80",
    schemaVersion: "midgard-watcher-native-block-admission-v1",
    slot: appendPoint.slot,
    transactionIds: Object.freeze([txHash]),
    transactionCbors: Object.freeze([transaction.to_canonical_cbor_hex()]),
  }) as WatcherNativeBlockAdmissionV1;
  const localObservation = {
    block: {
      chainPoint: {
        blockHash,
        blockNo: appendPoint.blockNo,
        chainPointId: h32("b3"),
        depth: "30",
        parentBlockHash: nativeBlock.prevHash,
        pointDigest: h32("b4"),
        slot: appendPoint.slot,
      },
      transactions: [
        {
          txHash,
          body: { bytesHex: bodyCbor },
          witnessSet: { bytesHex: witnessSetCbor },
          redeemers: [
            {
              purpose: "mint",
              index: "0",
              bytes: { bytesHex: canonicalRedeemerCbor },
            },
          ],
        },
      ],
    },
  } as unknown as WatcherLocalKupmiosNativeObservationV1;
  const initialBody = CML.TransactionBody.from_cbor_hex(initial.raw.bodyCbor);
  const raw = Object.freeze({
    txHash,
    bodyCbor,
    witnessSetCbor,
    redeemersCbor: witnessSet.redeemers()!.to_canonical_cbor_hex(),
    isValid: true,
    inclusionPoint: appendPoint,
    confirmationDepth: 30,
    resolvedInputs: Object.freeze([
      {
        outRef: `${initial.raw.txHash}#0`,
        outputCbor: initialBody.outputs().get(0).to_canonical_cbor_hex(),
        datumCbor: initialBody
          .outputs()
          .get(0)
          .datum()!
          .as_datum()!
          .to_canonical_cbor_hex(),
        referenceScriptCbor: null,
      },
    ]),
    resolvedReferenceInputs: Object.freeze([
      {
        outRef: `${initial.raw.txHash}#1`,
        outputCbor: initialBody.outputs().get(1).to_canonical_cbor_hex(),
        datumCbor: initialBody
          .outputs()
          .get(1)
          .datum()!
          .as_datum()!
          .to_canonical_cbor_hex(),
        referenceScriptCbor: null,
      },
    ]),
  }) satisfies FraudProofRawL1TransactionV1;
  return {
    nativeBlock,
    localObservation,
    raw,
    previous,
    stateQueueNodeCborHex: Data.to(stateQueueNode, SDK.StateQueueNodeV1),
    linkedListDatumCborHex: body
      .outputs()
      .get(1)
      .datum()!
      .as_datum()!
      .to_canonical_cbor_hex(),
    headerCborHex,
    headerHash: computedHeaderHash,
  };
};

describe("production state-queue observation source", () => {
  it("prefilters raw blocks to state-queue mint/output or exact cursor touches", () => {
    const initial = fixture();
    const emptyBody = CML.TransactionBody.new(
      CML.TransactionInputList.new(),
      CML.TransactionOutputList.new(),
      170_000n,
    );
    const emptyTransaction = CML.Transaction.new(
      emptyBody,
      CML.TransactionWitnessSet.new(),
      true,
    );
    const spendInputs = CML.TransactionInputList.new();
    spendInputs.add(
      CML.TransactionInput.new(
        CML.TransactionHash.from_hex(initial.raw.txHash),
        0n,
      ),
    );
    const spendBody = CML.TransactionBody.new(
      spendInputs,
      CML.TransactionOutputList.new(),
      170_000n,
    );
    const spendTransaction = CML.Transaction.new(
      spendBody,
      CML.TransactionWitnessSet.new(),
      true,
    );
    const rawBlock = {
      schemaVersion: "midgard-local-kupmios-raw-block-at-point-v1",
      sourceId: "test-source",
      point: initial.raw.inclusionPoint,
      parentBlockHash: initial.nativeBlock.prevHash,
      kupoCheckpoint: {
        slot: Number(initial.raw.inclusionPoint.slot),
        blockHash: initial.raw.inclusionPoint.blockHash,
      },
      transactions: [
        {
          txHash: CML.hash_transaction(emptyBody).to_hex(),
          transactionCbor: emptyTransaction.to_canonical_cbor_hex(),
        },
        {
          txHash: initial.raw.txHash,
          transactionCbor: initial.nativeBlock.transactionCbors[0]!,
        },
        {
          txHash: CML.hash_transaction(spendBody).to_hex(),
          transactionCbor: spendTransaction.to_canonical_cbor_hex(),
        },
      ],
    } as const;
    expect(
      unsafeSelectWatcherStateQueueRawCandidatesForTest({
        rawBlock,
        queue: [],
        currentLock: null,
        stateQueuePolicyId:
          initial.authority.protocolScriptHashes.stateQueueMint,
        hubOraclePolicyId: initial.authority.protocolScriptHashes.hubOracleMint,
      }).map(({ transactionIndex }) => transactionIndex),
    ).toEqual([1]);
    expect(
      unsafeSelectWatcherStateQueueRawCandidatesForTest({
        rawBlock,
        queue: [{ headerHash: null, outRef: `${initial.raw.txHash}#0` }],
        currentLock: null,
        stateQueuePolicyId:
          initial.authority.protocolScriptHashes.stateQueueMint,
        hubOraclePolicyId: initial.authority.protocolScriptHashes.hubOracleMint,
      }).map(({ transactionIndex }) => transactionIndex),
    ).toEqual([1, 2]);
  });

  it("authenticates permanent proof policy at the distinct fraud-proof spend address", () => {
    const current = fixture();
    const targetHeaderHash = h28("ab");
    const assetName = `01020304${targetHeaderHash}`;
    const proof = (address: string) => {
      const output = CML.TransactionOutput.new(
        CML.Address.from_bech32(address),
        value(current.authority.protocolScriptHashes.fraudProofMint, assetName),
        CML.DatumOption.new_datum(CML.PlutusData.from_cbor_hex("80")),
        undefined,
      );
      return {
        outRef: `${h32("91")}#0`,
        outputCbor: output.to_canonical_cbor_hex(),
        datumCbor: "80",
        referenceScriptCbor: null,
      };
    };
    const spendingAddress = credentialToAddress(
      current.authority.network,
      scriptHashToCredential(
        current.authority.protocolScriptHashes.fraudProofSpend,
      ),
    );
    const mintPolicyAddress = credentialToAddress(
      current.authority.network,
      scriptHashToCredential(
        current.authority.protocolScriptHashes.fraudProofMint,
      ),
    );
    expect(
      unsafeDeriveFraudProofCorrectionIdentityForTest({
        proof: proof(spendingAddress),
        fraudProofPolicyId:
          current.authority.protocolScriptHashes.fraudProofMint,
        fraudProofAddress: spendingAddress,
        targetHeaderHash,
      }),
    ).toEqual({ FraudProof: { fraud_proof_asset_name: assetName } });
    expect(() =>
      unsafeDeriveFraudProofCorrectionIdentityForTest({
        proof: proof(mintPolicyAddress),
        fraudProofPolicyId:
          current.authority.protocolScriptHashes.fraudProofMint,
        fraudProofAddress: spendingAddress,
        targetHeaderHash,
      }),
    ).toThrow("exact permanent proof");
  });

  it("derives the exact finalized Init queue and CorrectionLock genesis witness", () => {
    const current = fixture();
    const result = unsafeDeriveWatcherProductionStateQueueObservationForTest({
      nativeBlock: current.nativeBlock,
      localObservation: current.localObservation,
      authority: current.authority,
      sourceId: "test-source",
      previous: null,
      rawTransactions: [current.raw],
    });
    expect(result).toMatchObject({
      deploymentIdentityDigest: current.deployment.result.manifestId,
      stateQueuePolicyId: current.authority.protocolScriptHashes.stateQueueMint,
      checkpoints: [
        {
          checkpointKind: "init",
          correctionLockWitness: { kind: "genesis", nextDatum: "Idle" },
        },
      ],
      finalizedQueue: [{ headerHash: null, outRef: `${current.raw.txHash}#0` }],
      finalizedCorrectionLock: {
        outRef: `${current.raw.txHash}#1`,
        datum: "Idle",
        observedTransactionHash: current.raw.txHash,
        observedBlockHash: current.nativeBlock.blockHash,
        observedSlot: current.nativeBlock.slot,
        observedBlockNo: current.nativeBlock.blockNo,
        observedChainPointId: current.raw.inclusionPoint.pointId,
        finalityDepth: "30",
      },
    });
    expect(() =>
      assertWatcherProductionStateQueueObservationV1(result),
    ).toThrow("was not admitted");
  });

  it("bootstraps an empty durable store from the exact live queue and CorrectionLock snapshot", async () => {
    const current = fixture();
    const body = CML.TransactionBody.from_cbor_hex(current.raw.bodyCbor);
    const stateQueueAddress = credentialToAddress(
      current.authority.network,
      scriptHashToCredential(
        current.authority.protocolScriptHashes.stateQueueSpend,
      ),
    );
    const correctionLockAddress = credentialToAddress(
      current.authority.network,
      scriptHashToCredential(
        current.authority.protocolScriptHashes.correctionLockSpend,
      ),
    );
    const bootstrapped =
      await unsafeSnapshotWatcherProductionStateQueueAtBoundaryForTest({
        intersection: {
          blockHash: current.raw.inclusionPoint.blockHash,
          blockNo: current.raw.inclusionPoint.blockNo,
          slot: current.raw.inclusionPoint.slot,
        },
        authority: current.authority,
        sourceId: "test-source",
        readers: {
          readBlock: async () => ({
            schemaVersion: "midgard-local-kupmios-raw-block-at-point-v1",
            sourceId: "test-source",
            point: current.raw.inclusionPoint,
            parentBlockHash: current.nativeBlock.prevHash,
            kupoCheckpoint: {
              slot: Number(current.raw.inclusionPoint.slot),
              blockHash: current.raw.inclusionPoint.blockHash,
            },
            transactions: [
              {
                txHash: current.raw.txHash,
                transactionCbor: current.nativeBlock.transactionCbors[0]!,
              },
            ],
          }),
          readTransaction: async () => current.raw,
          readUnitHistory: async () => ({
            checkpoint: current.raw.inclusionPoint,
            transactions: [
              {
                txHash: current.raw.txHash,
                inclusionPoint: current.raw.inclusionPoint,
              },
            ],
          }),
          readAddress: async (address) => {
            const outputIndex =
              address === stateQueueAddress
                ? 0
                : address === correctionLockAddress
                  ? 1
                  : -1;
            if (outputIndex < 0)
              throw new Error("unexpected bootstrap address");
            const output = body.outputs().get(outputIndex);
            return [
              {
                outRef: `${current.raw.txHash}#${outputIndex.toString()}`,
                outputCbor: output.to_canonical_cbor_hex(),
                datumCbor: output.datum()!.as_datum()!.to_canonical_cbor_hex(),
                referenceScriptCbor: null,
              },
            ];
          },
        },
      });
    expect(bootstrapped).toMatchObject({
      previousObservationDigest: null,
      checkpoints: [],
      finalizedQueue: [{ headerHash: null, outRef: `${current.raw.txHash}#0` }],
      finalizedCorrectionLock: {
        outRef: `${current.raw.txHash}#1`,
        datum: "Idle",
      },
    });
    expect(() =>
      assertWatcherProductionStateQueueObservationV1(bootstrapped),
    ).toThrow("was not admitted");

    const restoreReaders = {
      readBlock: async () => ({
        schemaVersion: "midgard-local-kupmios-raw-block-at-point-v1" as const,
        sourceId: "test-source",
        point: current.raw.inclusionPoint,
        parentBlockHash: current.nativeBlock.prevHash,
        kupoCheckpoint: {
          slot: Number(current.raw.inclusionPoint.slot),
          blockHash: current.raw.inclusionPoint.blockHash,
        },
        transactions: [
          {
            txHash: current.raw.txHash,
            transactionCbor: current.nativeBlock.transactionCbors[0]!,
          },
        ],
      }),
      readTransaction: async () => current.raw,
      readAddress: async (): Promise<never> => {
        throw new Error("bootstrap re-admission must use unit history");
      },
      readUnitHistory: async () => ({
        checkpoint: current.raw.inclusionPoint,
        transactions: [
          {
            txHash: current.raw.txHash,
            inclusionPoint: current.raw.inclusionPoint,
          },
        ],
      }),
    };
    await expect(
      unsafeRestorePersistedWatcherProductionStateQueueObservationForTest({
        persistedObservations: [JSON.parse(JSON.stringify(bootstrapped))],
        intersection: {
          blockHash: current.raw.inclusionPoint.blockHash,
          blockNo: current.raw.inclusionPoint.blockNo,
          slot: current.raw.inclusionPoint.slot,
        },
        ogmiosTipBlockNo: current.raw.inclusionPoint.blockNo,
        authority: current.authority,
        sourceId: "test-source",
        maximumObservations: 1,
        readers: restoreReaders,
      }),
    ).resolves.toMatchObject({ previous: bootstrapped });

    const forgedCanonical = {
      ...bootstrapped,
      finalizedQueue: [{ headerHash: null, outRef: `${h32("ff")}#0` }],
    };
    const { observationDigest: _digest, ...forgedBody } = forgedCanonical;
    const forged = {
      ...forgedBody,
      observationDigest: watcherSha256CanonicalJsonV1(forgedBody),
    };
    await expect(
      unsafeRestorePersistedWatcherProductionStateQueueObservationForTest({
        persistedObservations: [forged],
        intersection: {
          blockHash: current.raw.inclusionPoint.blockHash,
          blockNo: current.raw.inclusionPoint.blockNo,
          slot: current.raw.inclusionPoint.slot,
        },
        ogmiosTipBlockNo: current.raw.inclusionPoint.blockNo,
        authority: current.authority,
        sourceId: "test-source",
        maximumObservations: 1,
        readers: restoreReaders,
      }),
    ).rejects.toThrow("absent from unit history");

    const forgedNativePoint = rehashObservation({
      ...bootstrapped,
      nativePoint: {
        ...bootstrapped.nativePoint,
        parentBlockHash: h32("cd"),
      },
    });
    await expect(
      unsafeRestorePersistedWatcherProductionStateQueueObservationForTest({
        persistedObservations: [forgedNativePoint],
        intersection: {
          blockHash: current.raw.inclusionPoint.blockHash,
          blockNo: current.raw.inclusionPoint.blockNo,
          slot: current.raw.inclusionPoint.slot,
        },
        ogmiosTipBlockNo: current.raw.inclusionPoint.blockNo,
        authority: current.authority,
        sourceId: "test-source",
        maximumObservations: 1,
        readers: restoreReaders,
      }),
    ).rejects.toThrow("block metadata was substituted");

    const forgedLock = rehashObservation({
      ...bootstrapped,
      finalizedCorrectionLock: {
        ...bootstrapped.finalizedCorrectionLock!,
        observedChainPointId: h32("ce"),
      },
    });
    await expect(
      unsafeRestorePersistedWatcherProductionStateQueueObservationForTest({
        persistedObservations: [forgedLock],
        intersection: {
          blockHash: current.raw.inclusionPoint.blockHash,
          blockNo: current.raw.inclusionPoint.blockNo,
          slot: current.raw.inclusionPoint.slot,
        },
        ogmiosTipBlockNo: current.raw.inclusionPoint.blockNo,
        authority: current.authority,
        sourceId: "test-source",
        maximumObservations: 1,
        readers: restoreReaders,
      }),
    ).rejects.toThrow("CorrectionLock was substituted");
  });

  it("preserves the exact finalized CommitBlockHeader datum, HeaderV1, and lock provenance", () => {
    const initial = fixture();
    const previous = unsafeDeriveWatcherProductionStateQueueObservationForTest({
      nativeBlock: initial.nativeBlock,
      localObservation: initial.localObservation,
      authority: initial.authority,
      sourceId: "test-source",
      previous: null,
      rawTransactions: [initial.raw],
    });
    const append = appendFixture({ initial, previous });
    const result = unsafeDeriveWatcherProductionStateQueueObservationForTest({
      nativeBlock: append.nativeBlock,
      localObservation: append.localObservation,
      authority: initial.authority,
      sourceId: "test-source",
      previous,
      rawTransactions: [append.raw],
    });

    expect(result.checkpoints).toMatchObject([
      {
        checkpointKind: "append",
        correctionLockWitness: {
          kind: "idle_reference",
          referenceOutRef: `${initial.raw.txHash}#1`,
          datum: "Idle",
        },
      },
    ]);
    expect(result.finalizedHeaders).toEqual([
      {
        headerHash: append.headerHash,
        headerCborHex: append.headerCborHex,
        stateQueueNodeCborHex: append.stateQueueNodeCborHex,
        linkedListDatumCborHex: append.linkedListDatumCborHex,
        daAvailability: "Unattested",
        queueOutRef: `${append.raw.txHash}#1`,
        nextHeaderHash: null,
        observedTransactionHash: append.raw.txHash,
        observedBlockHash: append.nativeBlock.blockHash,
        observedSlot: append.nativeBlock.slot,
        observedBlockNo: append.nativeBlock.blockNo,
        observedChainPointId: append.raw.inclusionPoint.pointId,
        finalityDepth: "30",
      },
    ]);
    expect(result.finalizedCorrectionLock).toEqual(
      previous.finalizedCorrectionLock,
    );
  });

  it("re-authenticates a merged predecessor HeaderV1 only from retained public-DA unit history", async () => {
    const initial = fixture();
    const previous = unsafeDeriveWatcherProductionStateQueueObservationForTest({
      nativeBlock: initial.nativeBlock,
      localObservation: initial.localObservation,
      authority: initial.authority,
      sourceId: "test-source",
      previous: null,
      rawTransactions: [initial.raw],
    });
    const attached = appendFixture({
      initial,
      previous,
      daAvailability: {
        Attested: { da_bond_asset_name: h32("da") },
      },
    });
    const expectedUnit = `${initial.authority.protocolScriptHashes.stateQueueMint}${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${attached.headerHash}`;
    const readers = {
      readBoundary: async () => ({
        kupoCheckpoint: attached.raw.inclusionPoint,
        ogmiosTip: {
          ...attached.raw.inclusionPoint,
          blockNo: "130",
        },
        confirmationDepth: 30,
      }),
      readHistory: async (unit: string) => {
        expect(unit).toBe(expectedUnit);
        return {
          checkpoint: attached.raw.inclusionPoint,
          transactions: [
            {
              txHash: attached.raw.txHash,
              inclusionPoint: attached.raw.inclusionPoint,
            },
          ],
        };
      },
      readTransaction: async () => attached.raw,
    };
    const retained = await unsafeResolveRetainedWatcherStateQueueHeaderForTest({
      headerHash: attached.headerHash,
      authority: initial.authority,
      readers,
    });
    expect(retained).toEqual({
      headerHash: attached.headerHash,
      headerCborHex: attached.headerCborHex,
      stateQueueNodeCborHex: attached.stateQueueNodeCborHex,
      linkedListDatumCborHex: attached.linkedListDatumCborHex,
      daAvailability: {
        Attested: { da_bond_asset_name: h32("da") },
      },
      queueOutRef: `${attached.raw.txHash}#1`,
      nextHeaderHash: null,
      observedTransactionHash: attached.raw.txHash,
      observedBlockHash: attached.raw.inclusionPoint.blockHash,
      observedSlot: attached.raw.inclusionPoint.slot,
      observedBlockNo: attached.raw.inclusionPoint.blockNo,
      observedChainPointId: attached.raw.inclusionPoint.pointId,
      finalityDepth: "30",
    });
    expect(() =>
      assertWatcherProductionStateQueueHeaderObservationV1(retained),
    ).toThrow("was not admitted");

    const unattested = appendFixture({ initial, previous });
    await expect(
      unsafeResolveRetainedWatcherStateQueueHeaderForTest({
        headerHash: unattested.headerHash,
        authority: initial.authority,
        readers: {
          ...readers,
          readHistory: async () => ({
            checkpoint: unattested.raw.inclusionPoint,
            transactions: [
              {
                txHash: unattested.raw.txHash,
                inclusionPoint: unattested.raw.inclusionPoint,
              },
            ],
          }),
          readTransaction: async () => unattested.raw,
        },
      }),
    ).rejects.toThrow("public DA attachment");
  });

  it("rejects a CommitBlockHeader whose node asset does not commit its exact HeaderV1", () => {
    const initial = fixture();
    const previous = unsafeDeriveWatcherProductionStateQueueObservationForTest({
      nativeBlock: initial.nativeBlock,
      localObservation: initial.localObservation,
      authority: initial.authority,
      sourceId: "test-source",
      previous: null,
      rawTransactions: [initial.raw],
    });
    const assetMismatch = appendFixture({
      initial,
      previous,
      assetHeaderHash: h28("fe"),
    });
    expect(() =>
      unsafeDeriveWatcherProductionStateQueueObservationForTest({
        nativeBlock: assetMismatch.nativeBlock,
        localObservation: assetMismatch.localObservation,
        authority: initial.authority,
        sourceId: "test-source",
        previous,
        rawTransactions: [assetMismatch.raw],
      }),
    ).toThrow("header bytes or DA-attestation identity differ");

    const altered = headerFixture();
    altered.utxosRoot = h32("ef");
    const datumMismatch = appendFixture({
      initial,
      previous,
      nodeHeader: altered,
      assetHeaderHash: appendFixture({ initial, previous }).headerHash,
    });
    expect(() =>
      unsafeDeriveWatcherProductionStateQueueObservationForTest({
        nativeBlock: datumMismatch.nativeBlock,
        localObservation: datumMismatch.localObservation,
        authority: initial.authority,
        sourceId: "test-source",
        previous,
        rawTransactions: [datumMismatch.raw],
      }),
    ).toThrow("header bytes or DA-attestation identity differ");
  });

  it("re-admits the cached intersection and replays an offline queue mutation before the catch-up boundary", async () => {
    const initial = fixture();
    const initialResult =
      unsafeDeriveWatcherProductionStateQueueObservationForTest({
        nativeBlock: initial.nativeBlock,
        localObservation: initial.localObservation,
        authority: initial.authority,
        sourceId: "test-source",
        previous: null,
        rawTransactions: [initial.raw],
      });
    const append = appendFixture({ initial, previous: initialResult });
    const appendResult =
      unsafeDeriveWatcherProductionStateQueueObservationForTest({
        nativeBlock: append.nativeBlock,
        localObservation: append.localObservation,
        authority: initial.authority,
        sourceId: "test-source",
        previous: initialResult,
        rawTransactions: [append.raw],
      });
    const intersection = {
      blockHash: h32("a3"),
      blockNo: "105",
      slot: "1005",
    };
    const pointId = computeFraudProofRawL1PointIdV1(intersection);
    let transactionReads = 0;
    const readBlock = async (requested: FraudProofRawL1PointV1) => {
      const entry =
        requested.pointId === initial.raw.inclusionPoint.pointId
          ? {
              point: initial.raw.inclusionPoint,
              txHash: initial.raw.txHash,
              transactionCbor: initial.nativeBlock.transactionCbors[0]!,
            }
          : requested.pointId === append.raw.inclusionPoint.pointId
            ? {
                point: append.raw.inclusionPoint,
                txHash: append.raw.txHash,
                transactionCbor: append.nativeBlock.transactionCbors[0]!,
              }
            : requested.pointId === pointId
              ? {
                  point: requested,
                  txHash: null,
                  transactionCbor: null,
                }
              : null;
      if (entry === null) throw new Error("unexpected raw block");
      return Object.freeze({
        schemaVersion: "midgard-local-kupmios-raw-block-at-point-v1" as const,
        sourceId: "test-source",
        point: entry.point,
        parentBlockHash:
          entry.point.pointId === initial.raw.inclusionPoint.pointId
            ? initial.nativeBlock.prevHash
            : entry.point.pointId === append.raw.inclusionPoint.pointId
              ? append.nativeBlock.prevHash
              : append.nativeBlock.blockHash,
        kupoCheckpoint: {
          slot: Number(entry.point.slot),
          blockHash: entry.point.blockHash,
        },
        transactions:
          entry.txHash === null
            ? Object.freeze([])
            : Object.freeze([
                {
                  txHash: entry.txHash,
                  transactionCbor: entry.transactionCbor!,
                },
              ]),
      });
    };
    const readTransaction = async (txHash: string) => {
      transactionReads += 1;
      if (txHash === initial.raw.txHash) return initial.raw;
      if (txHash === append.raw.txHash) return append.raw;
      throw new Error("unexpected raw transaction");
    };
    const restored =
      await unsafeRestorePersistedWatcherProductionStateQueueObservationForTest(
        {
          persistedObservations: [JSON.parse(JSON.stringify(initialResult))],
          intersection,
          ogmiosTipBlockNo: "130",
          authority: initial.authority,
          sourceId: "test-source",
          maximumObservations: 64,
          readers: {
            readBlock,
            readTransaction,
            readUnitHistory: async () => ({
              checkpoint: initial.raw.inclusionPoint,
              transactions: [
                {
                  txHash: initial.raw.txHash,
                  inclusionPoint: initial.raw.inclusionPoint,
                },
              ],
            }),
            readAddress: async (): Promise<never> => {
              throw new Error(
                "offline catch-up must not require latest topology equality",
              );
            },
          },
        },
      );

    expect(transactionReads).toBe(2);
    expect(restored.replayIntersection).toEqual({
      blockHash: initialResult.nativePoint.blockHash,
      blockNo: initialResult.nativePoint.blockNo,
      slot: initialResult.nativePoint.slot,
      chainPointId: initialResult.nativePoint.chainPointId,
    });
    expect(restored.catchupBoundary).toEqual({
      blockHash: intersection.blockHash,
      blockNo: intersection.blockNo,
      slot: intersection.slot,
      chainPointId: pointId,
      finalityDepth: "30",
      ogmiosTipBlockNo: "130",
    });
    expect(restored.previous).toEqual(initialResult);
    expect(() =>
      assertWatcherProductionStateQueueObservationV1(restored.previous),
    ).toThrow("was not admitted");

    const caughtUp = unsafeDeriveWatcherProductionStateQueueObservationForTest({
      nativeBlock: append.nativeBlock,
      localObservation: append.localObservation,
      authority: initial.authority,
      sourceId: "test-source",
      previous: restored.previous,
      rawTransactions: [append.raw],
    });
    expect(caughtUp.finalizedQueue).toEqual(appendResult.finalizedQueue);
    expect(caughtUp.finalizedHeaders).toEqual(appendResult.finalizedHeaders);

    const compacted =
      await unsafeRestorePersistedWatcherProductionStateQueueObservationForTest(
        {
          persistedObservations: [JSON.parse(JSON.stringify(appendResult))],
          intersection,
          ogmiosTipBlockNo: "130",
          authority: initial.authority,
          sourceId: "test-source",
          maximumObservations: 64,
          readers: {
            readBlock,
            readTransaction,
            readUnitHistory: async () => ({
              checkpoint: append.raw.inclusionPoint,
              transactions: [
                {
                  txHash: initial.raw.txHash,
                  inclusionPoint: initial.raw.inclusionPoint,
                },
                {
                  txHash: append.raw.txHash,
                  inclusionPoint: append.raw.inclusionPoint,
                },
              ],
            }),
            readAddress: async (): Promise<never> => {
              throw new Error("compacted restore must use unit history");
            },
          },
        },
      );
    expect(compacted.previous).toEqual(appendResult);
    expect(compacted.previous.previousObservationDigest).toBe(
      initialResult.observationDigest,
    );

    const compactedHeader = appendResult.finalizedHeaders[0]!;
    const forgedCompactedHeader = rehashObservation({
      ...appendResult,
      finalizedHeaders: [
        {
          ...compactedHeader,
          observedBlockHash: h32("cf"),
        },
      ],
    });
    await expect(
      unsafeRestorePersistedWatcherProductionStateQueueObservationForTest({
        persistedObservations: [forgedCompactedHeader],
        intersection,
        ogmiosTipBlockNo: "130",
        authority: initial.authority,
        sourceId: "test-source",
        maximumObservations: 64,
        readers: {
          readBlock,
          readTransaction,
          readUnitHistory: async () => ({
            checkpoint: append.raw.inclusionPoint,
            transactions: [
              {
                txHash: initial.raw.txHash,
                inclusionPoint: initial.raw.inclusionPoint,
              },
              {
                txHash: append.raw.txHash,
                inclusionPoint: append.raw.inclusionPoint,
              },
            ],
          }),
          readAddress: async (): Promise<never> => {
            throw new Error("compacted restore must use unit history");
          },
        },
      }),
    ).rejects.toThrow("HeaderV1 bytes were substituted");

    const rolledBack =
      await unsafeRestoreLongestWatcherProductionStateQueuePrefixForTest({
        persistedObservations: [
          JSON.parse(JSON.stringify(initialResult)),
          JSON.parse(JSON.stringify(appendResult)),
        ],
        intersection,
        ogmiosTipBlockNo: "130",
        authority: initial.authority,
        sourceId: "test-source",
        maximumObservations: 64,
        readers: {
          readBlock: async (requested) => {
            if (requested.pointId === append.raw.inclusionPoint.pointId) {
              throw new LocalKupmiosExactPointNotCanonicalV1Error(
                "Kupo exact checkpoint rolled back",
              );
            }
            return await readBlock(requested);
          },
          readTransaction,
          readUnitHistory: async () => ({
            checkpoint: initial.raw.inclusionPoint,
            transactions: [
              {
                txHash: initial.raw.txHash,
                inclusionPoint: initial.raw.inclusionPoint,
              },
            ],
          }),
          readAddress: async (): Promise<never> => {
            throw new Error("rollback prefix must use unit history");
          },
        },
      });
    expect(rolledBack.previous).toEqual(initialResult);
    expect(rolledBack.discardedObservationCount).toBe(1);
    expect(rolledBack.replayIntersection.chainPointId).toBe(
      initialResult.nativePoint.chainPointId,
    );
    await expect(
      unsafeRestoreLongestWatcherProductionStateQueuePrefixForTest({
        persistedObservations: [
          JSON.parse(JSON.stringify(initialResult)),
          JSON.parse(JSON.stringify(appendResult)),
        ],
        intersection,
        ogmiosTipBlockNo: "130",
        authority: initial.authority,
        sourceId: "test-source",
        maximumObservations: 64,
        readers: {
          readBlock: async (requested) => {
            if (requested.pointId === append.raw.inclusionPoint.pointId) {
              throw new Error("temporary Ogmios transport failure");
            }
            return await readBlock(requested);
          },
          readTransaction,
          readUnitHistory: async () => ({
            checkpoint: initial.raw.inclusionPoint,
            transactions: [
              {
                txHash: initial.raw.txHash,
                inclusionPoint: initial.raw.inclusionPoint,
              },
            ],
          }),
          readAddress: async (): Promise<never> => {
            throw new Error("ambiguous failure must not reach topology scan");
          },
        },
      }),
    ).rejects.toThrow("temporary Ogmios transport failure");
  });

  it("rejects forged rehashed durable cursors and caps restore before raw reads", async () => {
    const initial = fixture();
    const result = unsafeDeriveWatcherProductionStateQueueObservationForTest({
      nativeBlock: initial.nativeBlock,
      localObservation: initial.localObservation,
      authority: initial.authority,
      sourceId: "test-source",
      previous: null,
      rawTransactions: [initial.raw],
    });
    const forgedCanonical = {
      ...result,
      finalizedQueue: [{ headerHash: null, outRef: `${h32("ff")}#0` }] as const,
    };
    const { observationDigest: _ignored, ...forgedBody } = forgedCanonical;
    const forged = {
      ...forgedBody,
      observationDigest: watcherSha256CanonicalJsonV1(forgedBody),
    };
    let reads = 0;
    const neverReaders = {
      readBlock: async (): Promise<never> => {
        reads += 1;
        throw new Error("must not read");
      },
      readTransaction: async (): Promise<never> => {
        reads += 1;
        throw new Error("must not read");
      },
      readAddress: async (): Promise<never> => {
        reads += 1;
        throw new Error("must not read");
      },
    };
    await expect(
      unsafeRestorePersistedWatcherProductionStateQueueObservationForTest({
        persistedObservations: [forged, forged],
        intersection: {
          blockHash: initial.nativeBlock.blockHash,
          blockNo: initial.nativeBlock.blockNo,
          slot: initial.nativeBlock.slot,
        },
        ogmiosTipBlockNo: initial.nativeBlock.blockNo,
        authority: initial.authority,
        sourceId: "test-source",
        maximumObservations: 1,
        readers: neverReaders,
      }),
    ).rejects.toThrow("release bound");
    expect(reads).toBe(0);
    await expect(
      unsafeRestorePersistedWatcherProductionStateQueueObservationForTest({
        persistedObservations: [JSON.parse(JSON.stringify(result))],
        intersection: {
          blockHash: initial.nativeBlock.blockHash,
          blockNo: initial.nativeBlock.blockNo,
          slot: initial.nativeBlock.slot,
        },
        ogmiosTipBlockNo: "2260",
        authority: initial.authority,
        sourceId: "test-source",
        maximumObservations: 2160,
        readers: {
          readBlock: async () => ({
            schemaVersion: "midgard-local-kupmios-raw-block-at-point-v1",
            sourceId: "test-source",
            point: initial.raw.inclusionPoint,
            parentBlockHash: initial.nativeBlock.prevHash,
            kupoCheckpoint: {
              slot: Number(initial.raw.inclusionPoint.slot),
              blockHash: initial.raw.inclusionPoint.blockHash,
            },
            transactions: [
              {
                txHash: initial.raw.txHash,
                transactionCbor: initial.nativeBlock.transactionCbors[0]!,
              },
            ],
          }),
          readTransaction: async () => initial.raw,
          readUnitHistory: async () => ({
            checkpoint: initial.raw.inclusionPoint,
            transactions: [
              {
                txHash: initial.raw.txHash,
                inclusionPoint: initial.raw.inclusionPoint,
              },
            ],
          }),
          readAddress: async (): Promise<never> => {
            throw new Error("exact recovery bound must use unit history");
          },
        },
      }),
    ).resolves.toMatchObject({
      replayIntersection: { blockNo: "100" },
      catchupBoundary: { ogmiosTipBlockNo: "2260" },
    });
    await expect(
      unsafeRestorePersistedWatcherProductionStateQueueObservationForTest({
        persistedObservations: [JSON.parse(JSON.stringify(result))],
        intersection: {
          blockHash: initial.nativeBlock.blockHash,
          blockNo: initial.nativeBlock.blockNo,
          slot: initial.nativeBlock.slot,
        },
        ogmiosTipBlockNo: "2261",
        authority: initial.authority,
        sourceId: "test-source",
        maximumObservations: 2160,
        readers: neverReaders,
      }),
    ).rejects.toThrow("catch-up block distance exceeds its release bound");
    expect(reads).toBe(0);
    await expect(
      unsafeRestorePersistedWatcherProductionStateQueueObservationForTest({
        persistedObservations: [forged],
        intersection: {
          blockHash: initial.nativeBlock.blockHash,
          blockNo: initial.nativeBlock.blockNo,
          slot: initial.nativeBlock.slot,
        },
        ogmiosTipBlockNo: initial.nativeBlock.blockNo,
        authority: initial.authority,
        sourceId: "test-source",
        maximumObservations: 1,
        readers: {
          readBlock: async () => ({
            schemaVersion: "midgard-local-kupmios-raw-block-at-point-v1",
            sourceId: "test-source",
            point: initial.raw.inclusionPoint,
            parentBlockHash: initial.nativeBlock.prevHash,
            kupoCheckpoint: {
              slot: Number(initial.raw.inclusionPoint.slot),
              blockHash: initial.raw.inclusionPoint.blockHash,
            },
            transactions: [
              {
                txHash: initial.raw.txHash,
                transactionCbor: initial.nativeBlock.transactionCbors[0]!,
              },
            ],
          }),
          readTransaction: async () => initial.raw,
          readUnitHistory: async () => ({
            checkpoint: initial.raw.inclusionPoint,
            transactions: [
              {
                txHash: initial.raw.txHash,
                inclusionPoint: initial.raw.inclusionPoint,
              },
            ],
          }),
          readAddress: async (): Promise<never> => {
            throw new Error("forged cursor reached address admission");
          },
        },
      }),
    ).rejects.toThrow("absent from unit history");
  });

  it("rejects missing lock, substituted tx identity, and mismatched chain point", () => {
    const missing = fixture(true);
    expect(() =>
      unsafeDeriveWatcherProductionStateQueueObservationForTest({
        nativeBlock: missing.nativeBlock,
        localObservation: missing.localObservation,
        authority: missing.authority,
        sourceId: "test-source",
        previous: null,
        rawTransactions: [missing.raw],
      }),
    ).toThrow("Init has invalid CorrectionLock topology");

    const current = fixture();
    expect(() =>
      unsafeDeriveWatcherProductionStateQueueObservationForTest({
        nativeBlock: current.nativeBlock,
        localObservation: current.localObservation,
        authority: current.authority,
        sourceId: "test-source",
        previous: null,
        rawTransactions: [{ ...current.raw, txHash: h32("ff") }],
      }),
    ).toThrow("substituted across the native chain point");
    expect(() =>
      unsafeDeriveWatcherProductionStateQueueObservationForTest({
        nativeBlock: current.nativeBlock,
        localObservation: {
          ...current.localObservation,
          block: {
            ...current.localObservation.block,
            chainPoint: {
              ...current.localObservation.block.chainPoint,
              blockHash: h32("ee"),
            },
          },
        },
        authority: current.authority,
        sourceId: "test-source",
        previous: null,
        rawTransactions: [current.raw],
      }),
    ).toThrow("chain point/finality differs");
  });

  it("rejects structural deployment/local-observation authority and release mismatch", async () => {
    const current = fixture();
    expect(() =>
      createWatcherProductionStateQueueObservationSourceV1({
        deploymentIdentity: { ...current.deployment.result },
        rawSource: {} as never,
      }),
    ).toThrow("verifiedDeploymentIdentity");

    const policy = Object.freeze({
      confirmationDepth: 30 as const,
      automaticRecoveryMaxDepth: 2160 as const,
      deepRollbackPolicy: "automated_rewind_replay_incident-v1" as const,
    });
    const releaseFinality = validateVerifiedFraudProofReleaseFinalityPolicyV1({
      schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_V1_SCHEMA_VERSION,
      deploymentIdentityDigest: current.deployment.result.manifestId,
      releaseIdentityDigest: current.deployment.result.releaseEvidenceDigest,
      policyDigest: computeFraudProofReleaseFinalityPolicyDigestV1(policy),
      policy,
    });
    const rawSource = createLocalKupmiosHttpOgmiosRawSourceV1({
      sourceId: "watcher-production-state-queue-test",
      kupoHttpUrl: "http://127.0.0.1:1442",
      ogmiosUrl: "ws://127.0.0.1:1337",
      releaseFinality,
    });
    const source = createWatcherProductionStateQueueObservationSourceV1({
      deploymentIdentity: current.deployment.result,
      rawSource,
    });
    await expect(
      source.observe({
        nativeBlock: current.nativeBlock,
        localObservation: current.localObservation,
        previous: null,
      }),
    ).rejects.toThrow("not admitted for the native block");

    const foreignFinality = validateVerifiedFraudProofReleaseFinalityPolicyV1({
      schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_V1_SCHEMA_VERSION,
      deploymentIdentityDigest: h32("cd"),
      releaseIdentityDigest: current.deployment.result.releaseEvidenceDigest,
      policyDigest: computeFraudProofReleaseFinalityPolicyDigestV1(policy),
      policy,
    });
    const foreignSource = createLocalKupmiosHttpOgmiosRawSourceV1({
      sourceId: "watcher-production-state-queue-foreign",
      kupoHttpUrl: "http://127.0.0.1:1442",
      ogmiosUrl: "ws://127.0.0.1:1337",
      releaseFinality: foreignFinality,
    });
    expect(() =>
      createWatcherProductionStateQueueObservationSourceV1({
        deploymentIdentity: current.deployment.result,
        rawSource: foreignSource,
      }),
    ).toThrow("not bound to the verified deployment");
  });

  it("rejects structural observations at the durable admission boundary", () => {
    expect(() =>
      assertWatcherProductionStateQueueObservationV1(
        Object.freeze({}) as WatcherProductionStateQueueObservationV1,
      ),
    ).toThrow("was not admitted");
  });
});
