import {
  makeWatcherDurableStore,
  watcherCanonicalJson,
  type WatcherDurableStore,
} from "../../storage/durable-store.js";
import { type WatcherUserEventCheckpoint } from "../../storage/user-event-checkpoint.js";
import { type WatcherUserEventArchiveIndexRead } from ".././user-event-history-archive.js";
import { localArchiveField } from "./local-history.local-entry-at-block.js";
import {
  localArchiveBudgets,
  type LocalArchiveObject,
  type LocalPair,
  localRefuse,
  type LocalRetainedEvidence,
  type WatcherLocalUserEventAnchor,
  type WatcherLocalUserEventEntry,
  type WatcherLocalUserEventHistory,
} from "./local-history.local-history-owner.js";
import {
  exactRecord,
  immutableWireValue,
  isHex28,
  isHex32,
  isHexBytes,
  sha256Canonical,
} from "./policy.js";
import {
  WATCHER_USER_EVENT_INDEXER_BOUNDS,
  WATCHER_USER_EVENT_SNAPSHOT_SCHEMA_VERSION,
} from "./types.js";

/** Compare only replay-stable semantics. The three original acquisition-derived
 * point commitments remain checked/retained as archived values, and are never
 * relabelled as commitments produced by the fresh W12 observations.
 */
export const localStableSnapshot = (value: unknown): unknown => {
  const snapshot = exactRecord(value, [
    "schemaVersion",
    "activeEvents",
    "terminalEvents",
    "quarantined",
    "snapshotDigest",
  ]);
  if (
    snapshot === null ||
    snapshot.schemaVersion !== WATCHER_USER_EVENT_SNAPSHOT_SCHEMA_VERSION ||
    snapshot.quarantined !== false ||
    !Array.isArray(snapshot.activeEvents) ||
    !Array.isArray(snapshot.terminalEvents) ||
    snapshot.activeEvents.length >
      WATCHER_USER_EVENT_INDEXER_BOUNDS.activeEvents ||
    snapshot.terminalEvents.length >
      WATCHER_USER_EVENT_INDEXER_BOUNDS.terminalEvents ||
    !isHex32(snapshot.snapshotDigest)
  )
    return localRefuse("archive snapshot framing differs");
  const { snapshotDigest, ...fields } = snapshot;
  if (sha256Canonical(fields) !== snapshotDigest)
    return localRefuse("archive snapshot digest differs");
  const stableEvent = (value: unknown, terminal: boolean) => {
    const baseKeys = [
      "kind",
      "eventId",
      "outRef",
      "transactionHash",
      "outputIndex",
      "nonceOutRef",
      "policyId",
      "spendScriptHash",
      "addressHex",
      "assetNameHex",
      ...(typeof value === "object" &&
      value !== null &&
      Object.hasOwn(value, "witnessScriptHash")
        ? ["witnessScriptHash"]
        : []),
      ...(typeof value === "object" &&
      value !== null &&
      Object.hasOwn(value, "historyPayloadCborHex")
        ? ["historyPayloadCborHex"]
        : []),
      "inclusionTime",
      "eventCborHex",
      "datumCborHex",
      "outputCborHex",
      "eventContentDigest",
      "datumDigest",
      "outputDigest",
      "originPointDigest",
      "originChainPointId",
      "originBlockHash",
      "originSlot",
      "originBlockNo",
      "finalityStatus",
    ];
    const classification =
      typeof value === "object" &&
      value !== null &&
      Object.hasOwn(value, "terminalClassification");
    const event = exactRecord(value, [
      ...baseKeys,
      ...(terminal
        ? [
            "terminalStatus",
            "terminalTransactionHash",
            "terminalPointDigest",
            "terminalBlockHash",
            "terminalSlot",
            "terminalBlockNo",
            "terminalFinalityStatus",
            ...(classification ? ["terminalClassification"] : []),
          ]
        : []),
    ]);
    if (
      event === null ||
      (event.kind === "forced_order"
        ? !isHex28(event.witnessScriptHash) ||
          Object.hasOwn(event, "historyPayloadCborHex")
        : (event.kind !== "deposit" && event.kind !== "withdrawal") ||
          !isHexBytes(event.historyPayloadCborHex) ||
          Object.hasOwn(event, "witnessScriptHash")) ||
      !isHex32(event.originPointDigest) ||
      !isHex32(event.originChainPointId) ||
      event.finalityStatus !== "final" ||
      (terminal &&
        (!isHex32(event.terminalPointDigest) ||
          event.terminalFinalityStatus !== "final"))
    )
      return localRefuse("archive event framing differs");
    const {
      originPointDigest: _originPointDigest,
      originChainPointId: _originChainPointId,
      terminalPointDigest: _terminalPointDigest,
      terminalClassification,
      ...stable
    } = event;
    if (classification) {
      const decoded = exactRecord(terminalClassification, [
        "schemaVersion",
        "operatorValidity",
        "terminalTransactionHash",
        "terminalPointDigest",
      ]);
      if (
        decoded === null ||
        decoded.terminalPointDigest !== event.terminalPointDigest
      )
        return localRefuse("archive terminal classification differs");
      const {
        terminalPointDigest: _classificationPoint,
        ...stableClassification
      } = decoded;
      return { ...stable, terminalClassification: stableClassification };
    }
    return stable;
  };
  return {
    schemaVersion: snapshot.schemaVersion,
    quarantined: false,
    activeEvents: snapshot.activeEvents.map((event) =>
      stableEvent(event, false),
    ),
    terminalEvents: snapshot.terminalEvents.map((event) =>
      stableEvent(event, true),
    ),
  };
};

export const localStableEventStore = (store: WatcherDurableStore): unknown => {
  const points = new Map(
    store.chainPoints.map((point) => [point.chainPointId, point]),
  );
  const stablePoint = (id: string) => {
    const point =
      points.get(id) ?? localRefuse("event store point dependency is absent");
    return {
      providerId: point.providerId,
      blockHash: point.blockHash,
      slot: point.slot,
      blockNo: point.blockNo,
    };
  };
  const {
    chainPoints,
    l1Observations,
    protocolUtxos,
    spentProtocolUtxos,
    caches: _caches,
    ...rest
  } = store;
  const order = (values: readonly unknown[]) =>
    [...values].sort((a, b) =>
      watcherCanonicalJson(a).localeCompare(watcherCanonicalJson(b)),
    );
  return {
    ...rest,
    chainPoints: order(
      chainPoints.map((point) => stablePoint(point.chainPointId)),
    ),
    l1Observations: order(
      l1Observations.map((row) => ({
        providerId: row.providerId,
        point: stablePoint(row.chainPointId),
      })),
    ),
    protocolUtxos: protocolUtxos.map(({ chainPointId, ...utxo }) => ({
      ...utxo,
      point: stablePoint(chainPointId),
    })),
    spentProtocolUtxos: spentProtocolUtxos.map(
      ({ chainPointId, spentAtChainPointId, ...utxo }) => ({
        ...utxo,
        point: stablePoint(chainPointId),
        spentAt: stablePoint(spentAtChainPointId),
      }),
    ),
  };
};

export const localStableOrigin = (value: unknown): unknown => ({
  schemaVersion: localArchiveField(value, ["schemaVersion"]),
  deploymentFingerprint: localArchiveField(value, ["deploymentFingerprint"]),
  blueprintHash: localArchiveField(value, ["blueprintHash"]),
  blueprintSha256: localArchiveField(value, ["blueprintSha256"]),
  network: localArchiveField(value, ["network"]),
  canonicalOneShotOutRef: localArchiveField(value, ["canonicalOneShotOutRef"]),
  scripts: localArchiveField(value, ["scripts"]),
  parentPoint: localArchiveField(value, ["parentPoint"]),
  activation: localArchiveField(value, ["activation"]),
});

type LocalAnchorValue = Readonly<{
  sourceStore: WatcherDurableStore;
  nextStore: WatcherDurableStore;
  archiveIndex: WatcherUserEventArchiveIndexRead;
  archiveObjects: readonly LocalArchiveObject[];
  retainedEntries: readonly WatcherLocalUserEventEntry[];
  retainedEvidence: readonly LocalRetainedEvidence[];
  pinnedEvidence: readonly LocalRetainedEvidence[];
  nextCheckpoint: WatcherUserEventCheckpoint;
  expectedCheckpointDigest: string;
  expectedCheckpointSequence: string;
}>;

type LocalAnchorOwner = {
  readonly history: WatcherLocalUserEventHistory;
  readonly pair: LocalPair;
  readonly generation: number;
  readonly value: LocalAnchorValue;
  accepted: boolean;
};

export const localAnchors: WeakMap<
  WatcherLocalUserEventAnchor,
  LocalAnchorOwner
> = new WeakMap();

export const localMaterializedStore = (
  source: WatcherDurableStore,
  retainedEvidence: readonly LocalRetainedEvidence[],
): WatcherDurableStore => {
  return localMaterializedStoreFromPoints(
    source,
    new Set(retainedEvidence.map(({ chainPointId }) => chainPointId)),
  );
};

export const localMaterializedStoreFromPoints = (
  source: WatcherDurableStore,
  requiredPoints: Set<string>,
): WatcherDurableStore => {
  for (const utxo of source.protocolUtxos)
    requiredPoints.add(utxo.chainPointId);
  for (const utxo of source.spentProtocolUtxos) {
    requiredPoints.add(utxo.chainPointId);
    requiredPoints.add(utxo.spentAtChainPointId);
  }
  const chainPoints = source.chainPoints.filter(({ chainPointId }) =>
    requiredPoints.has(chainPointId),
  );
  if (chainPoints.length !== requiredPoints.size)
    return localRefuse("materialization point dependency is absent");
  return immutableWireValue(
    makeWatcherDurableStore({
      deploymentMarker: source.deploymentMarker,
      revision: (BigInt(source.revision) + 1n).toString(),
      records: {
        ...source,
        chainPoints,
        l1Observations: source.l1Observations.filter(({ chainPointId }) =>
          requiredPoints.has(chainPointId),
        ),
      },
    }),
  );
};

export const localArchiveClosure = (
  objects: readonly LocalArchiveObject[],
): readonly LocalArchiveObject[] => {
  const unique = Object.freeze([
    ...new Map(objects.map((object) => [object.digest, object])).values(),
  ]);
  if (
    unique.length >
      WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceContainerEntries ||
    unique.reduce((total, object) => total + object.bytesHex.length / 2, 0) >
      WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceBytes ||
    unique.reduce(
      (total, object) => total + localArchiveBudgets.get(object)!.nodes,
      0,
    ) > WATCHER_USER_EVENT_INDEXER_BOUNDS.cumulativeEvidenceNodes
  )
    return localRefuse("materialized archive bound exceeded");
  return unique;
};
