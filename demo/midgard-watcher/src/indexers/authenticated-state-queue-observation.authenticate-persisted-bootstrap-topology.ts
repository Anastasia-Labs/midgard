import { type FraudProofRawL1Transaction } from "@al-ft/midgard-fault-proofs";
import * as SDK from "@al-ft/midgard-sdk";
import {
  CML,
  credentialToAddress,
  scriptHashToCredential,
} from "@lucid-evolution/lucid";

import { watcherDeploymentProtocolScriptAuthority } from "../runtime/deployment-identity.js";
import { watcherSameCanonicalJson } from "../storage/durable-store.js";
import { sameLockDatum } from "./authenticated-state-queue-observation.correction-lock-witness.js";
import {
  OUT_REF,
  type QueueOutput,
  RELEASE_FINALITY_DEPTH,
  type WatcherAuthenticatedStateQueueObservation,
} from "./authenticated-state-queue-observation.parse-persisted-header.js";
import {
  lockOutput,
  queueOutput,
} from "./authenticated-state-queue-observation.queue-output.js";
import { type PersistedRestoreReaders } from "./authenticated-state-queue-observation.snapshot-observation-at-boundary.js";

export const rawRedeemers = (
  witnessSetCbor: string,
): readonly SDK.StateQueueTransitionRedeemer[] => {
  const witnessSet = CML.TransactionWitnessSet.from_cbor_hex(witnessSetCbor);
  const flat = witnessSet.redeemers()?.to_flat_format();
  if (flat === undefined) return Object.freeze([]);
  const result: SDK.StateQueueTransitionRedeemer[] = [];
  for (let index = 0; index < flat.len(); index += 1) {
    const redeemer = flat.get(index);
    const purpose = (() => {
      switch (redeemer.tag()) {
        case CML.RedeemerTag.Spend:
          return "spend";
        case CML.RedeemerTag.Mint:
          return "mint";
        case CML.RedeemerTag.Cert:
          return "certificate";
        case CML.RedeemerTag.Reward:
          return "withdrawal";
        case CML.RedeemerTag.Voting:
          return "vote";
        case CML.RedeemerTag.Proposing:
          return "propose";
        default:
          throw new Error(
            "persisted queue transaction has unknown redeemer tag",
          );
      }
    })();
    result.push(
      Object.freeze({
        purpose,
        index: redeemer.index().toString(),
        cborHex: redeemer.data().to_canonical_cbor_hex(),
      }),
    );
  }
  return Object.freeze(result);
};

const pointAtOrBefore = (
  left: Readonly<{ blockNo: string; slot: string }>,
  right: Readonly<{ blockNo: string; slot: string }>,
): boolean =>
  BigInt(left.blockNo) < BigInt(right.blockNo) ||
  (left.blockNo === right.blockNo && BigInt(left.slot) <= BigInt(right.slot));

export const authenticatePersistedBootstrapTopology = async ({
  persisted,
  throughPoint,
  historyPoint,
  authority,
  readers,
}: {
  persisted: WatcherAuthenticatedStateQueueObservation;
  throughPoint: Readonly<{
    blockHash: string;
    blockNo: string;
    slot: string;
    pointId: string;
  }>;
  // Unit history is read through the pinned catch-up boundary, which the raw
  // source admits regardless of how far the persisted base lies behind it,
  // and is then filtered to the persisted point. A base deeper than the
  // release recovery window therefore restores instead of failing closed.
  historyPoint: Readonly<{
    blockHash: string;
    blockNo: string;
    slot: string;
    pointId: string;
  }>;
  authority: ReturnType<typeof watcherDeploymentProtocolScriptAuthority>;
  readers: PersistedRestoreReaders;
}): Promise<void> => {
  if (!pointAtOrBefore(throughPoint, historyPoint)) {
    throw new Error(
      "persisted bootstrap history point precedes the persisted point",
    );
  }
  const stateQueuePolicyId = authority.protocolScriptHashes.stateQueueMint;
  const stateQueueAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.stateQueueSpend),
  );
  const correctionLockAddress = credentialToAddress(
    authority.network,
    scriptHashToCredential(authority.protocolScriptHashes.correctionLockSpend),
  );
  const transactionCache = new Map<
    string,
    Promise<FraudProofRawL1Transaction>
  >();
  if (readers.readUnitHistory === undefined) {
    throw new Error(
      "persisted bootstrap re-admission requires exact unit history",
    );
  }
  const readUnitHistory = readers.readUnitHistory;
  const readHistoryTransaction = (
    txHash: string,
    point: Readonly<{
      blockHash: string;
      blockNo: string;
      slot: string;
      pointId: string;
    }>,
  ): Promise<FraudProofRawL1Transaction> => {
    const cached = transactionCache.get(txHash);
    if (cached !== undefined) return cached;
    const read = readers.readTransaction(txHash, point);
    transactionCache.set(txHash, read);
    return read;
  };
  const authenticateOutRef = async ({
    unit,
    outRef,
  }: {
    unit: string;
    outRef: string;
  }): Promise<
    Readonly<{
      output: CML.TransactionOutput;
      creation: FraudProofRawL1Transaction;
    }>
  > => {
    if (!OUT_REF.test(outRef)) {
      throw new Error("persisted bootstrap contains a non-canonical outref");
    }
    const [creationHash, outputIndexText] = outRef.split("#") as [
      string,
      string,
    ];
    const history = await readUnitHistory(unit, historyPoint);
    const creation = history.transactions.find(
      ({ txHash }) => txHash === creationHash,
    );
    if (creation === undefined) {
      throw new Error("persisted bootstrap outref is absent from unit history");
    }
    if (!pointAtOrBefore(creation.inclusionPoint, persisted.nativePoint)) {
      throw new Error(
        "persisted bootstrap outref was created after its claimed point",
      );
    }
    const creationTransaction = await readHistoryTransaction(
      creation.txHash,
      creation.inclusionPoint,
    );
    const outputIndex = Number(outputIndexText);
    const outputs = CML.TransactionBody.from_cbor_hex(
      creationTransaction.bodyCbor,
    ).outputs();
    if (!Number.isSafeInteger(outputIndex) || outputIndex >= outputs.len()) {
      throw new Error("persisted bootstrap outref output is absent");
    }
    for (const entry of history.transactions) {
      if (
        entry.txHash === creationHash ||
        !pointAtOrBefore(entry.inclusionPoint, persisted.nativePoint)
      ) {
        continue;
      }
      const transaction = await readHistoryTransaction(
        entry.txHash,
        entry.inclusionPoint,
      );
      if (transaction.resolvedInputs.some((input) => input.outRef === outRef)) {
        throw new Error(
          "persisted bootstrap outref was already spent at its claimed point",
        );
      }
    }
    return Object.freeze({
      output: outputs.get(outputIndex),
      creation: creationTransaction,
    });
  };

  const headers = new Map(
    persisted.finalizedHeaders.map((header) => [header.headerHash, header]),
  );
  const queueOutputs: QueueOutput[] = [];
  for (const node of persisted.finalizedQueue) {
    const assetName =
      node.headerHash === null
        ? SDK.STATE_QUEUE_ROOT_ASSET_NAME
        : `${SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX}${node.headerHash}`;
    const authenticated = await authenticateOutRef({
      unit: `${stateQueuePolicyId}${assetName}`,
      outRef: node.outRef,
    });
    const decoded = queueOutput({
      output: authenticated.output,
      outRef: node.outRef,
      stateQueueAddress,
      stateQueuePolicyId,
    });
    if (decoded === null || decoded.node.headerHash !== node.headerHash) {
      throw new Error("persisted bootstrap queue output was substituted");
    }
    if (decoded.header !== null) {
      const header = headers.get(decoded.header.headerHash);
      if (
        header === undefined ||
        header.queueOutRef !== node.outRef ||
        header.nextHeaderHash !== decoded.nextHeaderHash ||
        header.headerCborHex !== decoded.header.headerCborHex ||
        header.stateQueueNodeCborHex !== decoded.header.stateQueueNodeCborHex ||
        header.linkedListDatumCborHex !==
          decoded.header.linkedListDatumCborHex ||
        !watcherSameCanonicalJson(
          header.daAvailability,
          decoded.header.daAvailability,
        ) ||
        header.observedTransactionHash !== authenticated.creation.txHash ||
        header.observedBlockHash !==
          authenticated.creation.inclusionPoint.blockHash ||
        header.observedSlot !== authenticated.creation.inclusionPoint.slot ||
        header.observedBlockNo !==
          authenticated.creation.inclusionPoint.blockNo ||
        header.observedChainPointId !==
          authenticated.creation.inclusionPoint.pointId ||
        header.finalityDepth !== RELEASE_FINALITY_DEPTH.toString()
      ) {
        throw new Error("persisted bootstrap HeaderV1 bytes were substituted");
      }
      headers.delete(decoded.header.headerHash);
    }
    queueOutputs.push(decoded);
  }
  if (
    headers.size !== 0 ||
    queueOutputs.some(
      (output, index) =>
        output.nextHeaderHash !==
        (queueOutputs[index + 1]?.node.headerHash ?? null),
    )
  ) {
    throw new Error("persisted bootstrap queue topology was substituted");
  }
  const persistedLock = persisted.finalizedCorrectionLock;
  if (persistedLock === null) {
    throw new Error("persisted bootstrap omitted CorrectionLock");
  }
  const lockUnit = SDK.correctionLockUnit(
    authority.protocolScriptHashes.hubOracleMint,
  );
  const authenticatedLock = await authenticateOutRef({
    unit: lockUnit,
    outRef: persistedLock.outRef,
  });
  const lock = lockOutput({
    output: authenticatedLock.output,
    outRef: persistedLock.outRef,
    correctionLockAddress,
    hubOraclePolicyId: authority.protocolScriptHashes.hubOracleMint,
  });
  if (
    lock === null ||
    lock.outRef !== persistedLock.outRef ||
    !sameLockDatum(lock.datum, persistedLock.datum) ||
    persistedLock.observedTransactionHash !==
      authenticatedLock.creation.txHash ||
    persistedLock.observedBlockHash !==
      authenticatedLock.creation.inclusionPoint.blockHash ||
    persistedLock.observedSlot !==
      authenticatedLock.creation.inclusionPoint.slot ||
    persistedLock.observedBlockNo !==
      authenticatedLock.creation.inclusionPoint.blockNo ||
    persistedLock.observedChainPointId !==
      authenticatedLock.creation.inclusionPoint.pointId ||
    persistedLock.finalityDepth !== RELEASE_FINALITY_DEPTH.toString()
  ) {
    throw new Error("persisted bootstrap CorrectionLock was substituted");
  }
};
