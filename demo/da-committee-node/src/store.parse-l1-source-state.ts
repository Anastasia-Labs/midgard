import type {
  L1ObservedDecision,
  L1SourceState,
} from "./store.committee-store.js";
import { isCanonicalIsoTimestamp } from "./store.parse-decision-outbox-record.js";

const STATE_QUEUE_STATUSES: ReadonlySet<unknown> = new Set([
  "unattested",
  "attesting",
  "attested",
  "merged",
  "removed",
  "conflicted",
]);

export const parseL1SourceState = (value: unknown): L1SourceState => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error("committee node L1 source state must be an object");
  }
  const state = value as Partial<L1SourceState>;
  const stateKeys = new Set([
    "schemaVersion",
    "sourceMode",
    "network",
    "authoritySha256",
    "status",
    "observations",
    "observedAt",
  ]);
  if (
    Object.keys(state).some((key) => !stateKeys.has(key)) ||
    state.schemaVersion !== 1 ||
    state.sourceMode !== "local_node" ||
    typeof state.network !== "string" ||
    state.network.trim() !== state.network ||
    state.network.length === 0 ||
    typeof state.authoritySha256 !== "string" ||
    !/^[0-9a-f]{64}$/u.test(state.authoritySha256) ||
    state.status !== "healthy" ||
    typeof state.observedAt !== "string" ||
    !isCanonicalIsoTimestamp(state.observedAt) ||
    !Array.isArray(state.observations)
  ) {
    throw new Error("committee node L1 source state is malformed");
  }
  const observations = state.observations.map(
    (entry: unknown): L1ObservedDecision => {
      if (typeof entry !== "object" || entry === null || Array.isArray(entry)) {
        throw new Error("committee node L1 source observation is malformed");
      }
      const record = entry as Partial<L1ObservedDecision>;
      const observationKeys = new Set([
        "headerHash",
        "stateQueueOutRef",
        "stateQueueStatus",
        "slot",
        "blockHash",
        "finalized",
        "hasPersistedDecision",
      ]);
      if (
        Object.keys(record).some((key) => !observationKeys.has(key)) ||
        typeof record.headerHash !== "string" ||
        !/^[0-9a-f]{56}$/u.test(record.headerHash) ||
        typeof record.stateQueueOutRef !== "string" ||
        !/^[0-9a-f]{64}#[0-9]+$/u.test(record.stateQueueOutRef) ||
        !STATE_QUEUE_STATUSES.has(record.stateQueueStatus) ||
        typeof record.finalized !== "boolean" ||
        typeof record.hasPersistedDecision !== "boolean" ||
        (record.slot !== undefined &&
          (!Number.isSafeInteger(record.slot) || record.slot < 0)) ||
        (record.blockHash !== undefined &&
          (typeof record.blockHash !== "string" ||
            !/^[0-9a-f]{64}$/u.test(record.blockHash)))
      ) {
        throw new Error("committee node L1 source observation is malformed");
      }
      return {
        headerHash: record.headerHash,
        stateQueueOutRef: record.stateQueueOutRef,
        stateQueueStatus: record.stateQueueStatus!,
        ...(record.slot === undefined ? {} : { slot: record.slot }),
        ...(record.blockHash === undefined
          ? {}
          : { blockHash: record.blockHash }),
        finalized: record.finalized,
        hasPersistedDecision: record.hasPersistedDecision,
      };
    },
  );
  observations.sort((left, right) =>
    left.headerHash.localeCompare(right.headerHash, "en"),
  );
  if (
    new Set(observations.map(({ headerHash }) => headerHash)).size !==
    observations.length
  ) {
    throw new Error(
      "committee node L1 source observations contain duplicate headers",
    );
  }
  return {
    schemaVersion: 1,
    sourceMode: state.sourceMode,
    network: state.network,
    authoritySha256: state.authoritySha256,
    status: state.status,
    observations,
    observedAt: state.observedAt,
  };
};
