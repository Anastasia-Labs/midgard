import { type WatcherNormalizedL1Block } from "../../l1/l1-adapter.js";
import { type WatcherDeploymentIdentityPolicy } from "../../runtime/deployment-identity.js";
import {
  encodeWatcherDurableStore,
  journalWatcherProtocolUtxoTransition,
  type WatcherDurableStore,
  watcherDurableStoreBytesSha256,
} from "../../storage/durable-store.js";
import { type WatcherUserEventReferenceEvidence } from ".././user-event-reference-authority.js";
import {
  scanConsumedTransactionEvents,
  scanCreatedTransactionEvents,
} from "./decode.js";
import {
  cloneMarker,
  exactRecord,
  isHex32,
  isNatural,
  isNetwork,
  same,
  sha256Canonical,
  snapshotTerminalClassificationsAreExact,
} from "./policy.js";
import {
  WATCHER_USER_EVENT_INDEXER_BOUNDS,
  WATCHER_USER_EVENT_OBSERVATION_SCHEMA_VERSION,
  WATCHER_USER_EVENT_SNAPSHOT_SCHEMA_VERSION,
  type WatcherIndexedUserEvent,
  type WatcherTerminalUserEvent,
  type WatcherUserEventIndexerPolicy,
  type WatcherUserEventKind,
  type WatcherUserEventObservation,
  type WatcherUserEventSnapshot,
} from "./types.js";

const snapshotWithoutDigest = (
  value: Omit<WatcherUserEventSnapshot, "snapshotDigest">,
) => ({
  schemaVersion: WATCHER_USER_EVENT_SNAPSHOT_SCHEMA_VERSION,
  activeEvents: value.activeEvents,
  terminalEvents: value.terminalEvents,
  quarantined: value.quarantined,
});

export const makeSnapshot = (
  activeEvents: readonly WatcherIndexedUserEvent[],
  terminalEvents: readonly WatcherTerminalUserEvent[],
  quarantined = false,
): WatcherUserEventSnapshot | null => {
  if (
    activeEvents.length > WATCHER_USER_EVENT_INDEXER_BOUNDS.activeEvents ||
    terminalEvents.length > WATCHER_USER_EVENT_INDEXER_BOUNDS.terminalEvents
  ) {
    return null;
  }
  const canonical = snapshotWithoutDigest({
    schemaVersion: WATCHER_USER_EVENT_SNAPSHOT_SCHEMA_VERSION,
    activeEvents: Object.freeze([...activeEvents]),
    terminalEvents: Object.freeze([...terminalEvents]),
    quarantined,
  });
  return Object.freeze({
    ...canonical,
    snapshotDigest: sha256Canonical(canonical),
  });
};

export const protocolRole = (
  kind: WatcherUserEventKind,
): "deposit" | "withdrawal" | "forced_transaction" =>
  kind === "forced_order" ? "forced_transaction" : kind;

export const topologyMatches = (
  store: WatcherDurableStore,
  snapshot: WatcherUserEventSnapshot,
): boolean => {
  const durable = store.protocolUtxos
    .filter(({ role }) =>
      ["deposit", "withdrawal", "forced_transaction"].includes(role),
    )
    .sort((left, right) => left.outRef.localeCompare(right.outRef));
  const active = [...snapshot.activeEvents].sort((left, right) =>
    left.outRef.localeCompare(right.outRef),
  );
  return (
    durable.length === active.length &&
    durable.every((utxo, index) => {
      const event = active[index];
      return (
        event !== undefined &&
        utxo.outRef === event.outRef &&
        utxo.role === protocolRole(event.kind) &&
        store.chainPoints.some(
          ({ chainPointId }) => chainPointId === utxo.chainPointId,
        ) &&
        utxo.output.cborHex === event.outputCborHex &&
        utxo.output.sha256 === event.outputDigest
      );
    })
  );
};

export const storeDigest = (store: WatcherDurableStore): string =>
  watcherDurableStoreBytesSha256(encodeWatcherDurableStore(store));

const sameRecordSet = <T>(left: readonly T[], right: readonly T[]): boolean =>
  same(left, right);

export const storeTransitionMatches = (
  source: WatcherDurableStore,
  next: WatcherDurableStore,
  block: WatcherNormalizedL1Block,
  snapshot: WatcherUserEventSnapshot,
): boolean => {
  if (
    BigInt(next.revision) !== BigInt(source.revision) + 1n ||
    !same(source.deploymentMarker, next.deploymentMarker) ||
    !sameRecordSet(source.daProofInputs, next.daProofInputs) ||
    !sameRecordSet(source.reconstructedStates, next.reconstructedStates) ||
    !sameRecordSet(source.decisions, next.decisions) ||
    !sameRecordSet(source.faults, next.faults) ||
    !sameRecordSet(source.submissions, next.submissions) ||
    !sameRecordSet(source.confirmations, next.confirmations) ||
    !sameRecordSet(source.retries, next.retries) ||
    !sameRecordSet(source.deadlines, next.deadlines) ||
    !sameRecordSet(source.correctionResults, next.correctionResults)
  ) {
    return false;
  }
  const nextObservations = new Map(
    next.l1Observations.map((entry) => [entry.observationId, entry]),
  );
  const alreadyObserved = source.l1Observations.some(
    ({ observationId }) => observationId === block.observationDigest,
  );
  if (
    !source.l1Observations.every((entry) =>
      same(nextObservations.get(entry.observationId), entry),
    ) ||
    next.l1Observations.length !==
      source.l1Observations.length + (alreadyObserved ? 0 : 1)
  ) {
    return false;
  }
  const nextPoints = new Map(
    next.chainPoints.map((entry) => [entry.chainPointId, entry]),
  );
  for (const entry of source.chainPoints) {
    const candidate = nextPoints.get(entry.chainPointId);
    if (
      candidate === undefined ||
      (!same(candidate, entry) &&
        entry.chainPointId !== block.chainPoint.chainPointId)
    ) {
      return false;
    }
  }
  if (
    next.chainPoints.length !==
    source.chainPoints.length +
      (source.chainPoints.some(
        ({ chainPointId }) => chainPointId === block.chainPoint.chainPointId,
      )
        ? 0
        : 1)
  ) {
    return false;
  }
  const eventRoles = new Set(["deposit", "withdrawal", "forced_transaction"]);
  const sourceUnrelated = source.protocolUtxos.filter(
    ({ role }) => !eventRoles.has(role),
  );
  const nextUnrelated = next.protocolUtxos.filter(
    ({ role }) => !eventRoles.has(role),
  );
  try {
    const journal = journalWatcherProtocolUtxoTransition({
      sourceStore: source,
      nextChainPoints: next.chainPoints,
      nextProtocolUtxos: next.protocolUtxos,
      spentAtChainPointId: block.chainPoint.chainPointId,
    });
    return (
      same(sourceUnrelated, nextUnrelated) &&
      same(journal.spentProtocolUtxos, next.spentProtocolUtxos) &&
      topologyMatches(next, snapshot)
    );
  } catch {
    return false;
  }
};

const withCurrentTerminalFinality = (
  currentPointDigest: string,
  finalityGranted: boolean,
  events: readonly WatcherTerminalUserEvent[],
): readonly WatcherTerminalUserEvent[] =>
  Object.freeze(
    events.map((event) => ({
      ...event,
      terminalFinalityStatus:
        finalityGranted && event.terminalPointDigest === currentPointDigest
          ? "final"
          : event.terminalFinalityStatus,
    })),
  );

const observationWithoutDigest = (
  value: Omit<WatcherUserEventObservation, "observationDigest">,
) => ({
  schemaVersion: WATCHER_USER_EVENT_OBSERVATION_SCHEMA_VERSION,
  policyDigest: value.policyDigest,
  network: value.network,
  blueprintHash: value.blueprintHash,
  deploymentMarker: value.deploymentMarker,
  transitionKind: value.transitionKind,
  pointDigest: value.pointDigest,
  blockHash: value.blockHash,
  slot: value.slot,
  blockNo: value.blockNo,
  sourceObservationDigest: value.sourceObservationDigest,
  chainPointId: value.chainPointId,
  sourceDurableStoreDigest: value.sourceDurableStoreDigest,
  sourceDurableStoreRevision: value.sourceDurableStoreRevision,
  durableStoreDigest: value.durableStoreDigest,
  durableStoreRevision: value.durableStoreRevision,
  rollbackTargetEntryDigest: value.rollbackTargetEntryDigest,
  snapshot: value.snapshot,
});

export const makeObservation = (
  value: Omit<WatcherUserEventObservation, "observationDigest">,
): WatcherUserEventObservation => {
  const canonical = observationWithoutDigest(value);
  return Object.freeze({
    ...canonical,
    observationDigest: sha256Canonical(canonical),
  });
};

/** Fold an admitted whole block in native order; no partial block is constructed. */
export const deriveLocalBlockEventSnapshot = (
  policy: WatcherUserEventIndexerPolicy,
  previous: WatcherUserEventSnapshot,
  block: WatcherNormalizedL1Block,
  referenceEvidence: WatcherUserEventReferenceEvidence,
  deployment: Pick<WatcherDeploymentIdentityPolicy, "appliedScriptHashes">,
): WatcherUserEventSnapshot | null => {
  if (previous.quarantined) return null;
  const active = new Map(
    previous.activeEvents.map((event) => [event.outRef, event]),
  );
  const terminal = [...previous.terminalEvents];
  const eventIds = new Set(
    [...previous.activeEvents, ...previous.terminalEvents].map(
      (event) => event.eventId,
    ),
  );
  for (const transaction of block.transactions) {
    if (
      scanConsumedTransactionEvents(
        block,
        transaction,
        referenceEvidence,
        active,
        terminal,
        deployment,
      ) === null
    )
      return null;
    const created: WatcherIndexedUserEvent[] = [];
    if (
      scanCreatedTransactionEvents(
        policy,
        block,
        transaction,
        referenceEvidence,
        deployment,
        created,
      ) === null
    )
      return null;
    for (const event of created) {
      if (active.has(event.outRef) || eventIds.has(event.eventId)) return null;
      active.set(
        event.outRef,
        Object.freeze({ ...event, finalityStatus: "final" }),
      );
      eventIds.add(event.eventId);
    }
    if (
      active.size > WATCHER_USER_EVENT_INDEXER_BOUNDS.activeEvents ||
      terminal.length > WATCHER_USER_EVENT_INDEXER_BOUNDS.terminalEvents
    )
      return null;
  }
  return makeSnapshot(
    [...active.values()].sort((left, right) =>
      left.outRef.localeCompare(right.outRef),
    ),
    withCurrentTerminalFinality(
      block.chainPoint.pointDigest,
      true,
      terminal.sort((left, right) =>
        `${left.terminalPointDigest}:${left.outRef}`.localeCompare(
          `${right.terminalPointDigest}:${right.outRef}`,
        ),
      ),
    ),
  );
};

export const parseObservationStructural = (
  value: unknown,
): WatcherUserEventObservation | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "policyDigest",
    "network",
    "blueprintHash",
    "deploymentMarker",
    "transitionKind",
    "pointDigest",
    "blockHash",
    "slot",
    "blockNo",
    "sourceObservationDigest",
    "chainPointId",
    "sourceDurableStoreDigest",
    "sourceDurableStoreRevision",
    "durableStoreDigest",
    "durableStoreRevision",
    "rollbackTargetEntryDigest",
    "snapshot",
    "observationDigest",
  ]);
  if (
    record === null ||
    record.schemaVersion !== WATCHER_USER_EVENT_OBSERVATION_SCHEMA_VERSION ||
    !isHex32(record.policyDigest) ||
    !isNetwork(record.network) ||
    !isHex32(record.blueprintHash) ||
    cloneMarker(record.deploymentMarker) === null ||
    !["apply_block", "rollback"].includes(String(record.transitionKind)) ||
    !isHex32(record.sourceDurableStoreDigest) ||
    !isNatural(record.sourceDurableStoreRevision) ||
    !isHex32(record.durableStoreDigest) ||
    !isNatural(record.durableStoreRevision) ||
    !isHex32(record.observationDigest) ||
    !snapshotTerminalClassificationsAreExact(record.snapshot)
  ) {
    return null;
  }
  const candidate = value as WatcherUserEventObservation;
  const canonical = observationWithoutDigest(candidate);
  return sha256Canonical(canonical) === candidate.observationDigest
    ? candidate
    : null;
};
