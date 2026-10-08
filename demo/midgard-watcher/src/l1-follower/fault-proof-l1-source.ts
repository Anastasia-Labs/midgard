import type { L1NodeTransport } from "@al-ft/l1-node-transport";
import {
  admitFraudProofRawL1Snapshot,
  computeFraudProofRawL1RollbackCursor,
  FRAUD_PROOF_L1_SOURCE,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
  FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
  type FraudProofL1ObservationDepth,
  type FraudProofL1Source,
  type FraudProofRawL1Point,
  type FraudProofRawL1SnapshotAuthority,
  type FraudProofRawL1SnapshotRequest,
  type FraudProofRawL1Transaction,
  type VerifiedFraudProofReleaseFinalityPolicy,
} from "@al-ft/midgard-fault-proofs";
import type { FactStore } from "@al-ft/midgard-l1-follower";

import {
  blockAtDepth,
  chainMoved,
  currentView,
  required,
  tipPointOf,
  withCheckpointRetries,
} from "./fault-proof-l1-source.chain.js";
import { createSignedTransactionRecovery } from "./fault-proof-l1-source.signed.js";
import type { WatcherProofRetention } from "./proof-retention.js";
import {
  type FollowerRawReads,
  rawPointOf,
  requireResolvedInputs,
} from "./raw-reads.types.js";

export { WatcherFaultProofL1RefusedError } from "./fault-proof-l1-source.chain.js";

/**
 * The fault-proof families' L1 source over the watcher's chain follower
 * (ticket W1): raw snapshots pinned at a depth under the release policy,
 * and signed-intent recovery against the follower's canonical chain.
 *
 * Depth is counted from the follower's cursor; a lagging cursor only
 * understates it. Every refusal of the raw reads becomes a named error,
 * never a stand-in value: a point that left the stored chain is
 * `FraudProofL1CheckpointChangedError` (the capture retries), a store with
 * no cursor is `FraudProofL1UnavailableError`, and every other refusal is
 * `WatcherFaultProofL1RefusedError` with its reason.
 */

/** The sourceId prefix the earlier local source wrote. Persisted: snapshot digests bind it. */
export const WATCHER_FAULT_PROOF_SOURCE_ID_PREFIX =
  "midgard-local-kupo-http-ogmios-ws-source-v1:";

/** The depth a snapshot's boundary sits at for each observation depth. */
export const observationMinimumDepth = (
  releaseFinality: VerifiedFraudProofReleaseFinalityPolicy,
  observationDepth: FraudProofL1ObservationDepth,
): number =>
  observationDepth === "inclusion"
    ? 1
    : observationDepth === "recovery_finality"
      ? releaseFinality.policy.automaticRecoveryMaxDepth + 2
      : releaseFinality.policy.confirmationDepth;

const samePoint = (
  left: FraudProofRawL1Point,
  right: FraudProofRawL1Point,
): boolean =>
  left.slot === right.slot &&
  left.blockHash === right.blockHash &&
  left.blockNo === right.blockNo &&
  left.pointId === right.pointId;

type SnapshotSource = Readonly<{
  store: FactStore;
  rawReads: FollowerRawReads;
  sourceId: string;
  proofRetention?: WatcherProofRetention;
}>;

/** One capture at one view; throws `FraudProofL1CheckpointChangedError` when the chain moved under it. */
const captureOnce = async (
  { store, rawReads, sourceId, proofRetention }: SnapshotSource,
  request: FraudProofRawL1SnapshotRequest,
  releaseFinality: VerifiedFraudProofReleaseFinalityPolicy,
  observationDepth: FraudProofL1ObservationDepth,
): Promise<unknown> => {
  const view = await currentView(store);
  const boundary = await blockAtDepth(
    store,
    view,
    observationMinimumDepth(releaseFinality, observationDepth),
  );
  const point = rawPointOf(boundary);
  const tip = tipPointOf(view);
  const scopes = [];
  for (const scope of request.scopes)
    scopes.push({
      ...scope,
      utxos: required(await rawReads.addressUtxosAtPoint(scope.address, point)),
    });
  // A pinned objective's followed units stay readable past k (E1 ruling).
  await proofRetention?.holdUnits(request.headerHash, request.historyUnits);
  const histories = [];
  for (const unit of request.historyUnits)
    histories.push({
      unit,
      transactions: required(await rawReads.unitHistoryAtPoint(unit, point))
        .transactions,
    });
  const inclusionByHash = new Map<string, FraudProofRawL1Point>();
  for (const history of histories)
    for (const { txHash, inclusionPoint } of history.transactions) {
      const previous = inclusionByHash.get(txHash);
      if (previous !== undefined && !samePoint(previous, inclusionPoint))
        throw new Error(`unit histories disagree about transaction ${txHash}`);
      inclusionByHash.set(txHash, inclusionPoint);
    }
  const transactions: FraudProofRawL1Transaction[] = [];
  for (const [txHash, inclusionPoint] of [...inclusionByHash.entries()].sort(
    // Lowercase hex: code-unit order.
    ([left], [right]) => (left < right ? -1 : left > right ? 1 : 0),
  )) {
    const transaction = required(
      requireResolvedInputs(
        await rawReads.rawTransaction(txHash, inclusionPoint),
      ),
    );
    // Depth against this capture's tip, as the snapshot cursor counts it.
    transactions.push({
      ...transaction,
      confirmationDepth: view.height - Number(inclusionPoint.blockNo) + 1,
    });
  }
  // The view's point still stored keeps every block under it, the boundary included.
  if (!(await store.viewValid(view)))
    throw chainMoved("the follower rolled back during the snapshot capture");
  const value = {
    schemaVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_SCHEMA_VERSION,
    deploymentIdentityDigest: request.deploymentIdentityDigest,
    blueprintHash: request.blueprintHash,
    finalityPolicyDigest: request.finalityPolicyDigest,
    headerHash: request.headerHash,
    provenance: {
      // Persisted provenance values: evidence and decision digests bind them.
      trustClass: "authenticated_cardano_l1",
      sourceId,
      grade: "security",
      // Persisted value.
      sourceMode: "local_kupo_ogmios",
      // Persisted field name: the boundary point.
      kupoCheckpoint: point,
      // Persisted field name: the tip point.
      ogmiosTip: tip,
    },
    cursor: {
      point,
      tip,
      confirmationDepth: view.height - boundary.height + 1,
      rollbackCursor: computeFraudProofRawL1RollbackCursor({
        deploymentIdentityDigest: request.deploymentIdentityDigest,
        blueprintHash: request.blueprintHash,
        finalityPolicyDigest: request.finalityPolicyDigest,
        sourceId,
        pointId: point.pointId,
      }),
    },
    scopes,
    historyUnits: [...request.historyUnits],
    history: histories.map((history) => ({
      unit: history.unit,
      fromGenesis: true as const,
      completeThroughPointId: point.pointId,
      transactionHashes: history.transactions.map(({ txHash }) => txHash),
    })),
    transactions,
  };
  return admitFraudProofRawL1Snapshot({
    value,
    request,
    releaseFinality,
    observationDepth,
  });
};

const assertSourceId = (sourceId: string): void => {
  const suffix = sourceId.slice(WATCHER_FAULT_PROOF_SOURCE_ID_PREFIX.length);
  if (
    !sourceId.startsWith(WATCHER_FAULT_PROOF_SOURCE_ID_PREFIX) ||
    suffix.length === 0 ||
    suffix.trim() !== suffix
  )
    throw new Error(
      `the fault-proof L1 sourceId must be ${WATCHER_FAULT_PROOF_SOURCE_ID_PREFIX} followed by a canonical non-empty id`,
    );
};

export const createWatcherFaultProofL1Source = (
  input: Readonly<{
    /** The watcher's follower store. */
    store: FactStore;
    /** `createFollowerRawReads` over the same store. */
    rawReads: FollowerRawReads;
    node: Pick<L1NodeTransport, "submit" | "hasTx">;
    /** The complete provenance sourceId, byte-identical to the earlier one. */
    sourceId: string;
    /** Holds the units a pinned objective's captures read. */
    proofRetention?: WatcherProofRetention;
  }>,
): FraudProofL1Source => {
  assertSourceId(input.sourceId);
  const snapshotSource: SnapshotSource = {
    store: input.store,
    rawReads: input.rawReads,
    sourceId: input.sourceId,
    ...(input.proofRetention === undefined
      ? {}
      : { proofRetention: input.proofRetention }),
  };
  const source: FraudProofL1Source = {
    sourceVersion: FRAUD_PROOF_L1_SOURCE,
    snapshotAuthority: ({
      releaseFinality,
      observationDepth,
    }): FraudProofRawL1SnapshotAuthority =>
      Object.freeze({
        authorityVersion: FRAUD_PROOF_RAW_L1_SNAPSHOT_AUTHORITY,
        capture: (request: FraudProofRawL1SnapshotRequest) =>
          withCheckpointRetries(() =>
            captureOnce(
              snapshotSource,
              request,
              releaseFinality,
              observationDepth,
            ),
          ),
      }),
    signedTransactions: ({ releaseFinality }) =>
      createSignedTransactionRecovery({
        store: input.store,
        rawReads: input.rawReads,
        node: input.node,
        recoveryDepth: observationMinimumDepth(
          releaseFinality,
          "recovery_finality",
        ),
      }),
  };
  return Object.freeze(source);
};
