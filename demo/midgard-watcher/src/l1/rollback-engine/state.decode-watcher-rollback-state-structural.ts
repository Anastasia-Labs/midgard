import {
  parseWatcherDurableStore,
  type WatcherDurableStore,
} from "../../storage/durable-store.js";
import { parseWatcherFinalityState } from ".././finality-engine.js";
import { type WatcherNormalizedL1Block } from ".././l1-adapter.js";
import { type WatcherMultiProviderConsistency } from ".././multi-provider-consistency.js";
import {
  exactArray,
  exactPlainRecord,
  exactUnrestrictedStringArray,
  marker,
  sameMarker,
  sameStrings,
} from "./records.js";
import {
  parseEpochCheckpoint,
  parseRollbackIncident,
  parseTransitionRecord,
} from "./state.parse-epoch-checkpoint.js";
import { storeDigest } from "./state.parse-finality-transition.js";
import {
  CANONICAL_NATURAL,
  HEX_32,
  NETWORKS,
  sha256Canonical,
  WATCHER_ROLLBACK_BOUNDS,
  WATCHER_ROLLBACK_STATE_SCHEMA_VERSION,
  type WatcherRollbackRemovedRecords,
  type WatcherRollbackState,
  type WatcherRollbackTransition,
} from "./types.js";

export const decodeWatcherRollbackStateStructural = (
  value: unknown,
  parseStore = parseWatcherDurableStore,
): WatcherRollbackState | null => {
  try {
    const record = exactPlainRecord(value, [
      "schemaVersion",
      "policyDigest",
      "network",
      "blueprintHash",
      "deploymentMarker",
      "bootstrapStore",
      "bootstrapFinalityState",
      "epoch",
      "epochCheckpoint",
      "transitions",
      "storeDigest",
      "transitionCount",
      "currentFinalityStateDigest",
      "lastPreviousFinalityStateDigest",
      "lastConsistencyDigest",
      "lastFinalityResultDigest",
      "lastInstructionDigest",
      "transitionLineageDigest",
      "incident",
      "stateDigest",
    ]);
    if (
      record === null ||
      record.schemaVersion !== WATCHER_ROLLBACK_STATE_SCHEMA_VERSION ||
      typeof record.policyDigest !== "string" ||
      !HEX_32.test(record.policyDigest) ||
      !NETWORKS.includes(record.network as (typeof NETWORKS)[number]) ||
      typeof record.blueprintHash !== "string" ||
      !HEX_32.test(record.blueprintHash) ||
      typeof record.storeDigest !== "string" ||
      !HEX_32.test(record.storeDigest) ||
      typeof record.epoch !== "string" ||
      !CANONICAL_NATURAL.test(record.epoch) ||
      typeof record.transitionCount !== "string" ||
      !CANONICAL_NATURAL.test(record.transitionCount) ||
      ![
        record.currentFinalityStateDigest,
        record.lastPreviousFinalityStateDigest,
        record.lastConsistencyDigest,
        record.lastFinalityResultDigest,
        record.lastInstructionDigest,
      ].every(
        (member) =>
          member === null ||
          (typeof member === "string" && HEX_32.test(member)),
      ) ||
      typeof record.transitionLineageDigest !== "string" ||
      !HEX_32.test(record.transitionLineageDigest) ||
      typeof record.stateDigest !== "string" ||
      !HEX_32.test(record.stateDigest)
    ) {
      return null;
    }
    const deploymentMarker = marker(record.deploymentMarker);
    let bootstrapStore: WatcherDurableStore;
    try {
      bootstrapStore = parseStore(record.bootstrapStore);
    } catch {
      return null;
    }
    const bootstrapFinalityState = parseWatcherFinalityState(
      record.bootstrapFinalityState,
    );
    const epochCheckpoint =
      record.epochCheckpoint === null
        ? null
        : parseEpochCheckpoint(record.epochCheckpoint);
    const transitionInputs = exactArray(record.transitions);
    if (
      bootstrapFinalityState === null ||
      transitionInputs === null ||
      transitionInputs.length > WATCHER_ROLLBACK_BOUNDS.transitionHistory
    ) {
      return null;
    }
    const transitions = transitionInputs.map(parseTransitionRecord);
    const parsedIncident =
      record.incident === null ? null : parseRollbackIncident(record.incident);
    if (
      deploymentMarker === null ||
      (record.epochCheckpoint !== null && epochCheckpoint === null) ||
      transitions.some((transition) => transition === null) ||
      !sameMarker(bootstrapStore.deploymentMarker, deploymentMarker) ||
      (record.incident !== null && parsedIncident === null)
    ) {
      return null;
    }
    const canonical = {
      schemaVersion: WATCHER_ROLLBACK_STATE_SCHEMA_VERSION,
      policyDigest: record.policyDigest,
      network: record.network as (typeof NETWORKS)[number],
      blueprintHash: record.blueprintHash,
      deploymentMarker,
      bootstrapStore,
      bootstrapFinalityState,
      epoch: record.epoch,
      epochCheckpoint,
      transitions: Object.freeze(
        transitions as readonly WatcherRollbackTransition[],
      ),
      storeDigest: record.storeDigest,
      transitionCount: record.transitionCount,
      currentFinalityStateDigest: record.currentFinalityStateDigest as
        | string
        | null,
      lastPreviousFinalityStateDigest:
        record.lastPreviousFinalityStateDigest as string | null,
      lastConsistencyDigest: record.lastConsistencyDigest as string | null,
      lastFinalityResultDigest: record.lastFinalityResultDigest as
        | string
        | null,
      lastInstructionDigest: record.lastInstructionDigest as string | null,
      transitionLineageDigest: record.transitionLineageDigest,
      incident: parsedIncident,
    };
    const count = BigInt(canonical.transitionCount);
    const epochTransitionCount = BigInt(canonical.transitions.length);
    const priorTransitionCount = BigInt(
      canonical.epochCheckpoint?.priorTerminalTransitionCount ?? "0",
    );
    if (
      count !== priorTransitionCount + epochTransitionCount ||
      BigInt(canonical.epoch) !==
        BigInt(canonical.epochCheckpoint?.epoch ?? "0") ||
      (canonical.epochCheckpoint === null) !== (canonical.epoch === "0") ||
      (canonical.epochCheckpoint !== null &&
        (canonical.epochCheckpoint.checkpointStoreDigest !==
          storeDigest(canonical.bootstrapStore) ||
          canonical.epochCheckpoint.checkpointFinalityStateDigest !==
            canonical.bootstrapFinalityState.stateDigest))
    ) {
      return null;
    }
    const initialShape =
      count === 0n &&
      canonical.epoch === "0" &&
      canonical.currentFinalityStateDigest === null &&
      canonical.lastPreviousFinalityStateDigest === null &&
      canonical.lastConsistencyDigest === null &&
      canonical.lastFinalityResultDigest === null &&
      canonical.lastInstructionDigest === null &&
      canonical.incident === null &&
      canonical.transitionLineageDigest ===
        sha256Canonical({
          schemaVersion: WATCHER_ROLLBACK_STATE_SCHEMA_VERSION,
          kind: "genesis",
          policyDigest: canonical.policyDigest,
          network: canonical.network,
          blueprintHash: canonical.blueprintHash,
          deploymentMarker: canonical.deploymentMarker,
        });
    const checkpointShape =
      canonical.epochCheckpoint !== null &&
      epochTransitionCount === 0n &&
      canonical.currentFinalityStateDigest ===
        canonical.epochCheckpoint.checkpointFinalityStateDigest &&
      canonical.lastPreviousFinalityStateDigest === null &&
      canonical.lastConsistencyDigest === null &&
      canonical.lastFinalityResultDigest === null &&
      canonical.lastInstructionDigest === null &&
      canonical.incident === null &&
      canonical.transitionLineageDigest ===
        canonical.epochCheckpoint.priorTerminalTransitionLineageDigest;
    const transitionedShape =
      epochTransitionCount > 0n &&
      canonical.currentFinalityStateDigest !== null &&
      canonical.lastPreviousFinalityStateDigest !== null &&
      canonical.lastConsistencyDigest !== null &&
      canonical.lastFinalityResultDigest !== null &&
      ((canonical.incident === null &&
        canonical.lastInstructionDigest !== null) ||
        (canonical.incident !== null &&
          canonical.lastInstructionDigest === null));
    if (!initialShape && !checkpointShape && !transitionedShape) {
      return null;
    }
    if (
      parsedIncident !== null &&
      (parsedIncident.policyDigest !== canonical.policyDigest ||
        parsedIncident.blueprintHash !== canonical.blueprintHash ||
        !sameMarker(
          parsedIncident.deploymentMarker,
          canonical.deploymentMarker,
        ) ||
        parsedIncident.nextStoreDigest !== canonical.storeDigest ||
        parsedIncident.transitionCount !== canonical.transitionCount ||
        parsedIncident.previousFinalityStateDigest !==
          canonical.lastPreviousFinalityStateDigest ||
        parsedIncident.consistencyDigest !== canonical.lastConsistencyDigest ||
        parsedIncident.finalityResultDigest !==
          canonical.lastFinalityResultDigest ||
        parsedIncident.finalityStateDigest !==
          canonical.currentFinalityStateDigest)
    ) {
      return null;
    }
    if (sha256Canonical(canonical) !== record.stateDigest) {
      return null;
    }
    const state = Object.freeze({
      ...canonical,
      stateDigest: record.stateDigest,
    }) as WatcherRollbackState;
    return state;
  } catch {
    return null;
  }
};

export const parseRemovedRecords = (
  value: unknown,
): WatcherRollbackRemovedRecords | null => {
  const keys = [
    "l1ObservationIds",
    "chainPointIds",
    "protocolUtxoOutRefs",
    "daProofInputIds",
    "reconstructedBlockHashes",
    "decisionBlockHashes",
    "faultIds",
    "submissionIds",
    "confirmationIds",
    "retryIds",
    "deadlineIds",
    "correctionResultIds",
  ] as const;
  const record = exactPlainRecord(value, keys);
  if (record === null) {
    return null;
  }
  const parsed = {} as Record<(typeof keys)[number], readonly string[]>;
  for (const key of keys) {
    const array = exactUnrestrictedStringArray(record[key]);
    if (
      array === null ||
      array.some(
        (member) =>
          typeof member !== "string" ||
          (key === "protocolUtxoOutRefs"
            ? !/^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u.test(member)
            : !HEX_32.test(member)),
      )
    ) {
      return null;
    }
    const strings = array;
    if (
      new Set(strings).size !== strings.length ||
      !sameStrings(strings, [...strings].sort())
    ) {
      return null;
    }
    parsed[key] = Object.freeze([...strings]);
  }
  return Object.freeze(parsed) as WatcherRollbackRemovedRecords;
};

export const removedRecordCount = (
  removed: WatcherRollbackRemovedRecords,
): number =>
  Object.values(removed).reduce((total, values) => total + values.length, 0);

export const sorted = (values: Iterable<string>): readonly string[] =>
  Object.freeze([...values].sort());

export type PersistedConsistencyEvidence = Readonly<{
  observationIds: ReadonlySet<string>;
  chainPointIds: ReadonlySet<string>;
  observations: readonly WatcherNormalizedL1Block[];
  consistency: WatcherMultiProviderConsistency;
}>;

export type PersistedObservationIndexEntry = Readonly<{
  durable: WatcherDurableStore["l1Observations"][number];
  point: WatcherDurableStore["chainPoints"][number] | null;
  observation: WatcherNormalizedL1Block;
}>;

export type PersistedObservationIndex = ReadonlyMap<
  string,
  PersistedObservationIndexEntry | null
>;
