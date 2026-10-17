import {
  exactArray,
  exactRecord,
  isHex32,
  isNullableHex28,
  sha256Canonical,
  STATE_QUEUE_SNAPSHOT_BOUNDS,
  WATCHER_STATE_QUEUE_SNAPSHOT_SCHEMA_VERSION,
  type WatcherIndexedActiveOperator,
  type WatcherIndexedRetiredOperator,
  type WatcherStateQueueHeader,
  type WatcherStateQueueSnapshot,
} from "./state-queue-snapshot.header-view.js";
import {
  linkedKeys,
  parseActive,
  parseConfirmed,
  parseRetired,
  parseScheduler,
  parseWatcherStateQueueHeader,
  snapshotWithoutDigest,
} from "./state-queue-snapshot.parse-watcher-state-queue-header.js";

export const makeWatcherStateQueueSnapshot = (
  value: Omit<WatcherStateQueueSnapshot, "schemaVersion" | "snapshotDigest">,
): WatcherStateQueueSnapshot | null => {
  const canonical = {
    schemaVersion: WATCHER_STATE_QUEUE_SNAPSHOT_SCHEMA_VERSION,
    ...value,
  };
  return parseWatcherStateQueueSnapshot({
    ...canonical,
    snapshotDigest: sha256Canonical(canonical),
  });
};

/**
 * Parses the indexer's derived projection for durable restart/replay.
 * Snapshots are outputs of node-derived topology reconstruction, never an
 * accepted observation or security-boundary input.
 */
export const parseWatcherStateQueueSnapshot = (
  value: unknown,
): WatcherStateQueueSnapshot | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "confirmedState",
    "queue",
    "scheduler",
    "activeOperators",
    "retiredOperators",
    "quarantinedFromHeaderHash",
    "snapshotDigest",
  ]);
  const confirmed =
    record === null ? null : parseConfirmed(record.confirmedState);
  const scheduler = record === null ? null : parseScheduler(record.scheduler);
  const queueValues =
    record === null
      ? null
      : exactArray(record.queue, STATE_QUEUE_SNAPSHOT_BOUNDS.queueNodes);
  const activeValues =
    record === null
      ? null
      : exactArray(
          record.activeOperators,
          STATE_QUEUE_SNAPSHOT_BOUNDS.activeOperators,
        );
  const retiredValues =
    record === null
      ? null
      : exactArray(
          record.retiredOperators,
          STATE_QUEUE_SNAPSHOT_BOUNDS.activeOperators,
        );
  const queue = queueValues?.map(parseWatcherStateQueueHeader) ?? null;
  const active = activeValues?.map(parseActive) ?? null;
  const retired = retiredValues?.map(parseRetired) ?? null;
  if (
    record === null ||
    confirmed === null ||
    scheduler === null ||
    queue === null ||
    active === null ||
    retired === null ||
    queue.some((entry) => entry === null) ||
    active.some((entry) => entry === null) ||
    retired.some((entry) => entry === null) ||
    !isNullableHex28(record.quarantinedFromHeaderHash) ||
    !isHex32(record.snapshotDigest)
  ) {
    return null;
  }
  const canonical = Object.freeze({
    schemaVersion: WATCHER_STATE_QUEUE_SNAPSHOT_SCHEMA_VERSION,
    confirmedState: confirmed,
    queue: Object.freeze(queue as WatcherStateQueueHeader[]),
    scheduler,
    activeOperators: Object.freeze(active as WatcherIndexedActiveOperator[]),
    retiredOperators: Object.freeze(retired as WatcherIndexedRetiredOperator[]),
    quarantinedFromHeaderHash: record.quarantinedFromHeaderHash,
  });
  const allOperators = [
    ...canonical.activeOperators.map(({ operatorVkey }) => operatorVkey),
    ...canonical.retiredOperators.map(({ operatorVkey }) => operatorVkey),
  ];
  const queueLinks = canonical.queue.every(
    (header, index) =>
      header.nextHeaderHash ===
      (canonical.queue[index + 1]?.headerHash ?? null),
  );
  const chainBreaks = canonical.queue
    .map((header, index) => {
      const previous = canonical.queue[index - 1];
      return index > 0 &&
        (header.prevHeaderHash !== previous?.headerHash ||
          header.prevUtxosRoot !== previous.utxosRoot ||
          BigInt(header.startTime) !== BigInt(previous.endTime))
        ? (previous?.headerHash ?? null)
        : null;
    })
    .filter((entry): entry is string => entry !== null);
  const queueHead = canonical.queue[0];
  if (
    sha256Canonical(snapshotWithoutDigest(canonical)) !==
      record.snapshotDigest ||
    !queueLinks ||
    chainBreaks.length > 1 ||
    (chainBreaks[0] ?? null) !== canonical.quarantinedFromHeaderHash ||
    (queueHead !== undefined &&
      (queueHead.prevHeaderHash !== canonical.confirmedState.headerHash ||
        queueHead.prevUtxosRoot !== canonical.confirmedState.utxosRoot ||
        BigInt(queueHead.startTime) !==
          BigInt(canonical.confirmedState.endTime))) ||
    new Set(canonical.queue.map(({ headerHash }) => headerHash)).size !==
      canonical.queue.length ||
    new Set(allOperators).size !== allOperators.length ||
    !linkedKeys(
      canonical.activeOperators.map((entry) => [
        entry.operatorVkey,
        entry.nextOperatorVkey,
      ]),
    ) ||
    !linkedKeys(
      canonical.retiredOperators.map((entry) => [
        entry.operatorVkey,
        entry.nextOperatorVkey,
      ]),
    ) ||
    (canonical.scheduler.operatorVkey !== null &&
      !canonical.activeOperators.some(
        ({ operatorVkey }) => operatorVkey === canonical.scheduler.operatorVkey,
      ))
  ) {
    return null;
  }
  return Object.freeze({
    ...canonical,
    snapshotDigest: record.snapshotDigest,
  });
};
