import type { StateQueueOutputStep } from "./l1/terminal-retention-observation.js";
import {
  type L1ObservedDecision,
  type L1SourceState,
  UNKNOWN_STATE_QUEUE_STATUS,
} from "./store.committee-store.js";
import { isCanonicalIsoTimestamp } from "./store.parse-decision-outbox-record.js";
import { parseStateQueueReplayAnchor } from "./store.persisted-decision-transition.js";

const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;

/** A non-empty list of well-formed steps, or undefined. */
const parseStateQueueOutputSteps = (
  value: unknown,
): readonly StateQueueOutputStep[] | undefined => {
  if (!Array.isArray(value) || value.length === 0) return undefined;
  const steps: StateQueueOutputStep[] = [];
  for (const entry of value as unknown[]) {
    if (typeof entry !== "object" || entry === null || Array.isArray(entry)) {
      return undefined;
    }
    const step = entry as Partial<StateQueueOutputStep>;
    if (
      Object.keys(step).some(
        (key) => !["fromOutRef", "toOutRef", "slot", "blockHash"].includes(key),
      ) ||
      typeof step.fromOutRef !== "string" ||
      !OUT_REF.test(step.fromOutRef) ||
      (step.toOutRef !== undefined &&
        (typeof step.toOutRef !== "string" || !OUT_REF.test(step.toOutRef))) ||
      typeof step.slot !== "number" ||
      !Number.isSafeInteger(step.slot) ||
      step.slot < 0 ||
      typeof step.blockHash !== "string" ||
      !/^[0-9a-f]{64}$/u.test(step.blockHash)
    ) {
      return undefined;
    }
    steps.push({
      fromOutRef: step.fromOutRef,
      ...(step.toOutRef === undefined ? {} : { toOutRef: step.toOutRef }),
      slot: step.slot,
      blockHash: step.blockHash,
    });
  }
  return steps;
};

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
    "stateQueueReplayAnchor",
    "quarantineReason",
    "quarantinedAt",
  ]);
  if (
    Object.keys(state).some((key) => !stateKeys.has(key)) ||
    state.schemaVersion !== 1 ||
    (state.sourceMode !== "local_node" &&
      state.sourceMode !== "external_providers") ||
    typeof state.network !== "string" ||
    state.network.trim() !== state.network ||
    state.network.length === 0 ||
    typeof state.authoritySha256 !== "string" ||
    !/^[0-9a-f]{64}$/u.test(state.authoritySha256) ||
    (state.status !== "healthy" && state.status !== "quarantined") ||
    typeof state.observedAt !== "string" ||
    !isCanonicalIsoTimestamp(state.observedAt) ||
    !Array.isArray(state.observations)
  ) {
    throw new Error("committee node L1 source state is malformed");
  }
  if (
    state.status === "quarantined" &&
    (typeof state.quarantineReason !== "string" ||
      state.quarantineReason.length === 0 ||
      typeof state.quarantinedAt !== "string" ||
      !isCanonicalIsoTimestamp(state.quarantinedAt))
  ) {
    throw new Error(
      "quarantined committee node L1 source state lacks evidence",
    );
  }
  if (
    state.status === "healthy" &&
    (state.quarantineReason !== undefined || state.quarantinedAt !== undefined)
  ) {
    throw new Error(
      "healthy committee node L1 source state contains quarantine fields",
    );
  }
  const observations = state.observations.map((entry) => {
    if (typeof entry !== "object" || entry === null || Array.isArray(entry)) {
      throw new Error("committee node L1 source observation is malformed");
    }
    const record = entry as Partial<L1ObservedDecision>;
    const observationKeys = new Set([
      "headerHash",
      "stateQueueOutRef",
      "stateQueueStatus",
      "lastKnownStatus",
      "slot",
      "blockHash",
      "finalized",
      "hasPersistedDecision",
      "authenticatedSteps",
    ]);
    const authenticatedSteps = parseStateQueueOutputSteps(
      record.authenticatedSteps,
    );
    if (
      Object.keys(record).some((key) => !observationKeys.has(key)) ||
      typeof record.headerHash !== "string" ||
      !/^[0-9a-f]{56}$/u.test(record.headerHash) ||
      typeof record.stateQueueOutRef !== "string" ||
      !/^[0-9a-f]{64}#[0-9]+$/u.test(record.stateQueueOutRef) ||
      (record.stateQueueStatus !== "unattested" &&
        record.stateQueueStatus !== "attesting" &&
        record.stateQueueStatus !== "attested" &&
        record.stateQueueStatus !== "merged" &&
        record.stateQueueStatus !== "removed" &&
        record.stateQueueStatus !== "conflicted" &&
        record.stateQueueStatus !== UNKNOWN_STATE_QUEUE_STATUS) ||
      (record.stateQueueStatus === UNKNOWN_STATE_QUEUE_STATUS
        ? record.lastKnownStatus !== "unattested" &&
          record.lastKnownStatus !== "attesting" &&
          record.lastKnownStatus !== "attested" &&
          record.lastKnownStatus !== "merged" &&
          record.lastKnownStatus !== "removed" &&
          record.lastKnownStatus !== "conflicted"
        : record.lastKnownStatus !== undefined) ||
      typeof record.finalized !== "boolean" ||
      typeof record.hasPersistedDecision !== "boolean" ||
      (record.slot !== undefined &&
        (!Number.isSafeInteger(record.slot) || record.slot < 0)) ||
      (record.blockHash !== undefined &&
        (typeof record.blockHash !== "string" ||
          !/^[0-9a-f]{64}$/u.test(record.blockHash))) ||
      (record.authenticatedSteps !== undefined &&
        authenticatedSteps === undefined)
    ) {
      throw new Error("committee node L1 source observation is malformed");
    }
    return {
      headerHash: record.headerHash,
      stateQueueOutRef: record.stateQueueOutRef,
      stateQueueStatus: record.stateQueueStatus,
      ...(record.lastKnownStatus === undefined
        ? {}
        : { lastKnownStatus: record.lastKnownStatus }),
      ...(record.slot === undefined ? {} : { slot: record.slot }),
      ...(record.blockHash === undefined
        ? {}
        : { blockHash: record.blockHash }),
      finalized: record.finalized,
      hasPersistedDecision: record.hasPersistedDecision,
      ...(authenticatedSteps === undefined ? {} : { authenticatedSteps }),
    };
  });
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
  const stateQueueReplayAnchor = parseStateQueueReplayAnchor(
    state.stateQueueReplayAnchor,
  );
  if (
    state.stateQueueReplayAnchor !== undefined &&
    stateQueueReplayAnchor === undefined
  ) {
    throw new Error("committee node L1 source replay anchor is malformed");
  }
  return {
    schemaVersion: 1,
    sourceMode: state.sourceMode,
    network: state.network,
    authoritySha256: state.authoritySha256,
    status: state.status,
    observations,
    observedAt: state.observedAt,
    ...(stateQueueReplayAnchor === undefined ? {} : { stateQueueReplayAnchor }),
    ...(state.quarantineReason === undefined
      ? {}
      : { quarantineReason: state.quarantineReason }),
    ...(state.quarantinedAt === undefined
      ? {}
      : { quarantinedAt: state.quarantinedAt }),
  };
};
