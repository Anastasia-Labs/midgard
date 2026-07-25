import * as SDK from "@al-ft/midgard-sdk";
import { LucidEvolution, toUnit } from "@lucid-evolution/lucid";
import { Effect, Ref } from "effect";

import type { Globals } from "@/services/globals.js";
import {
  type SerializedStateQueueUTxO,
  serializeStateQueueUTxO,
} from "@/workers/utils/commit-block-header.js";

/**
 * Summary of the current on-chain state-queue topology.
 */
export type StateQueueTopology = {
  readonly policyUtxoCount: number;
  readonly parsedNodeCount: number;
  readonly invalidNodeCount: number;
  readonly rootCount: number;
  readonly tailCount: number;
  readonly initialized: boolean;
  readonly healthy: boolean;
  readonly reason: string | undefined;
};

export type StateQueueSnapshotReason =
  | "startup"
  | "post_merge"
  | "commit_preflight"
  | "commit_revalidation"
  | "readiness"
  | "manual_status"
  | "recovery";

export type StateQueueSnapshot = {
  readonly snapshotId: string;
  readonly reason: StateQueueSnapshotReason;
  readonly observedAtMs: number;
  readonly topology: StateQueueTopology;
  readonly root: {
    readonly outRef: string;
    readonly headerHash: string | null;
    readonly utxo: SerializedStateQueueUTxO;
  };
  readonly tailCommitBase: {
    readonly outRef: string;
    readonly headerHash: string | null;
    readonly utxo: SerializedStateQueueUTxO;
    readonly blockEndTimeMs: number;
    readonly roots: {
      readonly utxosRoot: string;
      readonly transactionsRoot: string;
      readonly depositsRoot: string;
      readonly withdrawalsRoot: string;
    };
  };
};

/**
 * Derives a human-readable unhealthy reason for the observed topology.
 */
const deriveReason = (
  policyUtxoCount: number,
  invalidNodeCount: number,
  rootCount: number,
  tailCount: number,
): string | undefined => {
  if (policyUtxoCount === 0) {
    return undefined;
  }
  if (invalidNodeCount > 0) {
    return `Found ${invalidNodeCount} non-decodable state_queue UTxO(s) under the configured policy`;
  }
  if (rootCount !== 1) {
    return `Expected exactly 1 state_queue root node, found ${rootCount}`;
  }
  if (tailCount !== 1) {
    return `Expected exactly 1 state_queue tail node, found ${tailCount}`;
  }
  return undefined;
};

/**
 * Summarizes the topology health of the authenticated state-queue nodes.
 */
export const summarizeStateQueueTopology = (
  policyUtxoCount: number,
  nodes: readonly SDK.StateQueueUTxO[],
): StateQueueTopology => {
  const parsedNodeCount = nodes.length;
  const invalidNodeCount = Math.max(0, policyUtxoCount - parsedNodeCount);
  const rootCount = nodes.filter((node) => node.datum.key === "Empty").length;
  const tailCount = nodes.filter((node) => node.datum.next === "Empty").length;
  const initialized = policyUtxoCount > 0;
  const reason = deriveReason(
    policyUtxoCount,
    invalidNodeCount,
    rootCount,
    tailCount,
  );
  return {
    policyUtxoCount,
    parsedNodeCount,
    invalidNodeCount,
    rootCount,
    tailCount,
    initialized,
    healthy: initialized && reason === undefined,
    reason,
  };
};

/**
 * Formats a topology summary into a compact log/metric string.
 */
export const formatStateQueueTopology = (
  topology: StateQueueTopology,
): string =>
  `policy_utxos=${topology.policyUtxoCount},parsed_nodes=${topology.parsedNodeCount},invalid_nodes=${topology.invalidNodeCount},roots=${topology.rootCount},tails=${topology.tailCount},healthy=${topology.healthy}`;

/**
 * Fetches live state-queue UTxOs and derives a topology summary from them.
 */
export const fetchStateQueueTopologyProgram = (
  lucid: LucidEvolution,
  stateQueue: SDK.AuthenticatedValidator,
): Effect.Effect<StateQueueTopology, SDK.LucidError> =>
  Effect.gen(function* () {
    const policyUtxos = yield* SDK.utxosAtByNFTPolicyId(
      lucid,
      stateQueue.spendingScriptAddress,
      stateQueue.policyId,
    );
    const parsed = yield* SDK.utxosToStateQueueUTxOs(
      policyUtxos.map(({ utxo }) => utxo),
      stateQueue.policyId,
    );
    return summarizeStateQueueTopology(policyUtxos.length, parsed);
  });

const MAX_OPERATIONAL_STATE_QUEUE_NODES = 10_000;

/**
 * Follows the authenticated state-queue list from its fixed root NFT using
 * exact-unit provider queries. Operational commit/merge paths need the live
 * canonical chain, not an address-wide superset, and must remain available
 * when unrelated L1 queries are busy. Full topology/status gates retain the
 * address-wide scan so orphan or malformed policy UTxOs are still surfaced.
 */
export const fetchCanonicalStateQueueNodesProgram = (
  lucid: LucidEvolution,
  stateQueue: SDK.AuthenticatedValidator,
): Effect.Effect<
  readonly SDK.StateQueueUTxO[],
  SDK.LucidError | SDK.StateQueueError
> =>
  Effect.gen(function* () {
    const nodes: SDK.StateQueueUTxO[] = [];
    const seenAssetNames = new Set<string>();
    let assetName = SDK.STATE_QUEUE_ROOT_ASSET_NAME;

    while (nodes.length < MAX_OPERATIONAL_STATE_QUEUE_NODES) {
      if (seenAssetNames.has(assetName)) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message: "Cannot derive state-queue snapshot from cyclic topology",
            cause: `asset_name=${assetName},visited=${nodes.length.toString()}`,
          }),
        );
      }
      seenAssetNames.add(assetName);
      const unit = toUnit(stateQueue.policyId, assetName);
      const matches = yield* Effect.tryPromise({
        try: () =>
          lucid.utxosAtWithUnit(stateQueue.spendingScriptAddress, unit),
        catch: (cause) =>
          new SDK.LucidError({
            message: `Failed to fetch state-queue linked-list unit at: ${stateQueue.spendingScriptAddress}`,
            cause,
          }),
      });
      if (matches.length !== 1) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message: "State-queue linked-list unit is missing or not unique",
            cause: `unit=${unit},matches=${matches.length.toString()}`,
          }),
        );
      }
      const node = yield* SDK.utxoToStateQueueUTxO(
        matches[0],
        stateQueue.policyId,
      ).pipe(
        Effect.mapError(
          (cause) =>
            new SDK.StateQueueError({
              message: "Failed to authenticate state-queue linked-list node",
              cause,
            }),
        ),
      );
      if (node.assetName !== assetName) {
        return yield* Effect.fail(
          new SDK.StateQueueError({
            message: "State-queue linked-list unit returned the wrong node",
            cause: `expected_asset_name=${assetName},actual_asset_name=${node.assetName}`,
          }),
        );
      }
      nodes.push(node);
      if (node.datum.next === "Empty") {
        return nodes;
      }
      assetName =
        SDK.STATE_QUEUE_NODE_ASSET_NAME_PREFIX + node.datum.next.Key.key;
    }

    return yield* Effect.fail(
      new SDK.StateQueueError({
        message: "State-queue linked-list traversal exceeded its safety bound",
        cause: `max_nodes=${MAX_OPERATIONAL_STATE_QUEUE_NODES.toString()}`,
      }),
    );
  });

const usesExactOperationalTraversal = (
  reason: StateQueueSnapshotReason,
): boolean =>
  reason === "commit_preflight" ||
  reason === "commit_revalidation" ||
  reason === "post_merge";

const outRef = (node: SDK.StateQueueUTxO): string =>
  `${node.utxo.txHash}#${node.utxo.outputIndex.toString()}`;

const nodeHeaderHash = (
  node: SDK.StateQueueUTxO,
): Effect.Effect<string | null, SDK.DataCoercionError | SDK.HashingError> =>
  node.datum.key === "Empty"
    ? Effect.gen(function* () {
        const { data } = yield* SDK.getConfirmedStateFromStateQueueDatum(
          node.datum,
        );
        return data.headerHash;
      })
    : Effect.gen(function* () {
        const header = yield* SDK.getHeaderV1FromStateQueueDatum(node.datum);
        return yield* SDK.hashBlockHeaderV1(header);
      });

const nodeEndTimeAndRoots = (
  node: SDK.StateQueueUTxO,
): Effect.Effect<
  StateQueueSnapshot["tailCommitBase"]["roots"] & {
    readonly blockEndTimeMs: number;
  },
  SDK.DataCoercionError
> =>
  Effect.gen(function* () {
    if (node.datum.key === "Empty") {
      const { data } = yield* SDK.getConfirmedStateFromStateQueueDatum(
        node.datum,
      );
      return {
        blockEndTimeMs: Number(data.endTime),
        utxosRoot: data.utxoRoot,
        transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      };
    }
    const header = yield* SDK.getHeaderV1FromStateQueueDatum(node.datum);
    return {
      blockEndTimeMs: Number(header.endTime),
      utxosRoot: header.utxosRoot,
      transactionsRoot: header.transactionsRoot,
      depositsRoot: header.depositsRoot,
      withdrawalsRoot: header.withdrawalsRoot,
    };
  });

export const fetchStateQueueSnapshotProgram = (
  lucid: LucidEvolution,
  stateQueue: SDK.AuthenticatedValidator,
  reason: StateQueueSnapshotReason,
): Effect.Effect<
  StateQueueSnapshot,
  | SDK.LucidError
  | SDK.StateQueueError
  | SDK.DataCoercionError
  | SDK.HashingError
  | SDK.CmlUnexpectedError
  | SDK.CborSerializationError
> =>
  Effect.gen(function* () {
    const operationalNodes = usesExactOperationalTraversal(reason)
      ? yield* fetchCanonicalStateQueueNodesProgram(lucid, stateQueue)
      : undefined;
    const policyUtxos =
      operationalNodes === undefined
        ? yield* SDK.utxosAtByNFTPolicyId(
            lucid,
            stateQueue.spendingScriptAddress,
            stateQueue.policyId,
          )
        : operationalNodes.map(({ utxo, assetName }) => ({
            utxo,
            policyId: stateQueue.policyId,
            assetName,
          }));
    const nodes =
      operationalNodes ??
      (yield* SDK.utxosToStateQueueUTxOs(
        policyUtxos.map(({ utxo }) => utxo),
        stateQueue.policyId,
      ));
    const topology = summarizeStateQueueTopology(
      operationalNodes === undefined ? policyUtxos.length : nodes.length,
      nodes,
    );
    if (!topology.healthy) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Cannot derive state-queue snapshot from unhealthy topology",
          cause: `${formatStateQueueTopology(topology)}; reason=${topology.reason ?? "unknown"}`,
        }),
      );
    }
    const root = nodes.find((node) => node.datum.key === "Empty");
    const tail = nodes.find((node) => node.datum.next === "Empty");
    if (root === undefined || tail === undefined) {
      return yield* Effect.fail(
        new SDK.StateQueueError({
          message: "Cannot derive state-queue snapshot without root and tail",
          cause: formatStateQueueTopology(topology),
        }),
      );
    }
    const [rootSerialized, tailSerialized, rootHeaderHash, tailHeaderHash] =
      yield* Effect.all(
        [
          serializeStateQueueUTxO(root),
          serializeStateQueueUTxO(tail),
          nodeHeaderHash(root),
          nodeHeaderHash(tail),
        ],
        { concurrency: "unbounded" },
      );
    const { blockEndTimeMs, ...roots } = yield* nodeEndTimeAndRoots(tail);
    const observedAtMs = Date.now();
    const rootOutRef = outRef(root);
    const tailOutRef = outRef(tail);
    const snapshotId = [reason, rootOutRef, tailOutRef, observedAtMs].join(":");
    return {
      snapshotId,
      reason,
      observedAtMs,
      topology,
      root: {
        outRef: rootOutRef,
        headerHash: rootHeaderHash,
        utxo: rootSerialized,
      },
      tailCommitBase: {
        outRef: tailOutRef,
        headerHash: tailHeaderHash,
        utxo: tailSerialized,
        blockEndTimeMs,
        roots,
      },
    };
  });

export const refreshStateQueueGlobalsFromSnapshot = (
  globals: Globals,
  snapshot: StateQueueSnapshot,
): Effect.Effect<void> =>
  Effect.gen(function* () {
    yield* Ref.set(
      globals.AVAILABLE_CONFIRMED_BLOCK,
      snapshot.tailCommitBase.utxo,
    );
    yield* Ref.set(
      globals.LATEST_LOCAL_BLOCK_END_TIME_MS,
      snapshot.tailCommitBase.blockEndTimeMs,
    );
    yield* Ref.set(
      globals.BLOCKS_IN_QUEUE,
      Math.max(0, snapshot.topology.parsedNodeCount - 1),
    );
    yield* Ref.set(
      globals.LATEST_SYNC_TIME_OF_STATE_QUEUE_LENGTH,
      snapshot.observedAtMs,
    );
  });
