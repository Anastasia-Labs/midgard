import { isProxy } from "node:util/types";

import {
  type DeploymentMarker,
  parseDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity";

import { watcherSameCanonicalJson } from "../../storage/durable-store.js";
import {
  WATCHER_FINALITY_REWIND_INSTRUCTION_SCHEMA_VERSION,
  type WatcherFinalityBoundObservation,
  type WatcherFinalityPolicy,
  type WatcherFinalityRewindInstruction,
  type WatcherFinalityState,
} from ".././finality-engine.js";
import {
  CANONICAL_NATURAL,
  HEX_32,
  type PlainRecord,
  sha256Canonical,
  WATCHER_ROLLBACK_RESULT_SCHEMA_VERSION,
  type WatcherRollbackAlertCode,
  type WatcherRollbackReasonCode,
  type WatcherRollbackRemovedRecords,
  type WatcherRollbackResult,
} from "./types.js";

export const exactPlainRecord = (
  value: unknown,
  keys: readonly string[],
): PlainRecord | null => {
  if (
    typeof value !== "object" ||
    value === null ||
    isProxy(value) ||
    Array.isArray(value)
  ) {
    return null;
  }
  const candidate = value as object;
  if (
    Object.getPrototypeOf(candidate) !== Object.prototype ||
    Reflect.ownKeys(candidate).length !== keys.length
  ) {
    return null;
  }
  const expected = new Set(keys);
  for (const key of Reflect.ownKeys(candidate)) {
    if (typeof key !== "string" || !expected.has(key)) {
      return null;
    }
    const descriptor = Object.getOwnPropertyDescriptor(candidate, key);
    if (
      descriptor === undefined ||
      !descriptor.enumerable ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined
    ) {
      return null;
    }
  }
  return value as PlainRecord;
};

export const exactStringArray = (
  value: unknown,
  allowed: readonly string[],
): readonly string[] | null => {
  if (
    !Array.isArray(value) ||
    isProxy(value) ||
    Object.getPrototypeOf(value) !== Array.prototype ||
    Reflect.ownKeys(value).length !== value.length + 1
  ) {
    return null;
  }
  const strings: string[] = [];
  for (let index = 0; index < value.length; index += 1) {
    const descriptor = Object.getOwnPropertyDescriptor(value, index.toString());
    const member = value[index] as unknown;
    if (
      descriptor === undefined ||
      !descriptor.enumerable ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined ||
      typeof member !== "string" ||
      !allowed.includes(member)
    ) {
      return null;
    }
    strings.push(member);
  }
  return Object.freeze(strings);
};

export const exactUnrestrictedStringArray = (
  value: unknown,
): readonly string[] | null => {
  if (
    !Array.isArray(value) ||
    isProxy(value) ||
    Object.getPrototypeOf(value) !== Array.prototype ||
    Reflect.ownKeys(value).length !== value.length + 1
  ) {
    return null;
  }
  const strings: string[] = [];
  for (let index = 0; index < value.length; index += 1) {
    const descriptor = Object.getOwnPropertyDescriptor(value, index.toString());
    const member = value[index] as unknown;
    if (
      descriptor === undefined ||
      !descriptor.enumerable ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined ||
      typeof member !== "string"
    ) {
      return null;
    }
    strings.push(member);
  }
  return Object.freeze(strings);
};

export const exactArray = (value: unknown): readonly unknown[] | null => {
  if (
    !Array.isArray(value) ||
    isProxy(value) ||
    Object.getPrototypeOf(value) !== Array.prototype ||
    Reflect.ownKeys(value).length !== value.length + 1
  ) {
    return null;
  }
  for (let index = 0; index < value.length; index += 1) {
    const descriptor = Object.getOwnPropertyDescriptor(value, index.toString());
    if (
      descriptor === undefined ||
      !descriptor.enumerable ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined
    ) {
      return null;
    }
  }
  return value as readonly unknown[];
};

export const sameStrings = (
  left: readonly string[],
  right: readonly string[],
): boolean =>
  left.length === right.length &&
  left.every((member, index) => member === right[index]);

export const marker = (value: unknown): DeploymentMarker | null => {
  try {
    return parseDeploymentMarker(value);
  } catch {
    return null;
  }
};

export const sameMarker = (
  left: DeploymentMarker,
  right: DeploymentMarker,
): boolean =>
  left.schemaVersion === right.schemaVersion &&
  left.manifestId === right.manifestId;

export const sameBinding = (
  left: WatcherFinalityBoundObservation,
  right: WatcherFinalityBoundObservation,
): boolean => watcherSameCanonicalJson(left, right);

export const emptyRemovedRecords = (): WatcherRollbackRemovedRecords =>
  Object.freeze({
    l1ObservationIds: Object.freeze([]),
    chainPointIds: Object.freeze([]),
    protocolUtxoOutRefs: Object.freeze([]),
    daProofInputIds: Object.freeze([]),
    reconstructedBlockHashes: Object.freeze([]),
    decisionBlockHashes: Object.freeze([]),
    faultIds: Object.freeze([]),
    submissionIds: Object.freeze([]),
    confirmationIds: Object.freeze([]),
    retryIds: Object.freeze([]),
    deadlineIds: Object.freeze([]),
    correctionResultIds: Object.freeze([]),
  });

export const makeResult = (
  value: Omit<WatcherRollbackResult, "schemaVersion" | "resultDigest">,
): WatcherRollbackResult => {
  const canonical = {
    schemaVersion: WATCHER_ROLLBACK_RESULT_SCHEMA_VERSION,
    action: value.action,
    protocolDecision: value.protocolDecision,
    reasonCodes: Object.freeze([...value.reasonCodes]),
    alertCodes: Object.freeze([...value.alertCodes]),
    sourceRevision: value.sourceRevision,
    nextRevision: value.nextRevision,
    instructionDigest: value.instructionDigest,
    sourceStoreDigest: value.sourceStoreDigest,
    nextStoreDigest: value.nextStoreDigest,
    removedRecords: value.removedRecords,
    nextStore: value.nextStore,
    rollbackState: value.rollbackState,
    rollbackBootstrapState: value.rollbackBootstrapState,
    trustedCheckpointStateDigest: value.trustedCheckpointStateDigest,
  };
  return Object.freeze({
    ...canonical,
    resultDigest: sha256Canonical(canonical),
  });
};

export const reject = (
  reason: WatcherRollbackReasonCode,
  alert: WatcherRollbackAlertCode = "watcher_rollback_input_rejected",
): WatcherRollbackResult =>
  makeResult({
    action: "reject",
    protocolDecision: "quarantined",
    reasonCodes: [reason],
    alertCodes: [alert],
    sourceRevision: null,
    nextRevision: null,
    instructionDigest: null,
    sourceStoreDigest: null,
    nextStoreDigest: null,
    removedRecords: emptyRemovedRecords(),
    nextStore: null,
    rollbackState: null,
    rollbackBootstrapState: null,
    trustedCheckpointStateDigest: null,
  });

export const parseInstruction = (
  value: unknown,
): WatcherFinalityRewindInstruction | null => {
  const instruction = exactPlainRecord(value, [
    "schemaVersion",
    "kind",
    "discardedStateDigest",
    "replacementPointDigest",
    "replacementContentDigest",
    "replacementDepth",
    "instructionDigest",
  ]);
  if (
    instruction === null ||
    instruction.schemaVersion !==
      WATCHER_FINALITY_REWIND_INSTRUCTION_SCHEMA_VERSION ||
    ![
      "pending_depth_regression",
      "pending_point_changed",
      "pending_content_changed",
    ].includes(instruction.kind as string) ||
    typeof instruction.discardedStateDigest !== "string" ||
    !HEX_32.test(instruction.discardedStateDigest) ||
    typeof instruction.replacementPointDigest !== "string" ||
    !HEX_32.test(instruction.replacementPointDigest) ||
    typeof instruction.replacementContentDigest !== "string" ||
    !HEX_32.test(instruction.replacementContentDigest) ||
    typeof instruction.replacementDepth !== "string" ||
    !CANONICAL_NATURAL.test(instruction.replacementDepth) ||
    typeof instruction.instructionDigest !== "string" ||
    !HEX_32.test(instruction.instructionDigest)
  ) {
    return null;
  }
  const canonical = {
    schemaVersion: WATCHER_FINALITY_REWIND_INSTRUCTION_SCHEMA_VERSION,
    kind: instruction.kind as WatcherFinalityRewindInstruction["kind"],
    discardedStateDigest: instruction.discardedStateDigest,
    replacementPointDigest: instruction.replacementPointDigest,
    replacementContentDigest: instruction.replacementContentDigest,
    replacementDepth: instruction.replacementDepth,
  };
  if (sha256Canonical(canonical) !== instruction.instructionDigest) {
    return null;
  }
  return Object.freeze({
    ...canonical,
    instructionDigest: instruction.instructionDigest,
  });
};

export const stateBindingFailure = (
  policy: WatcherFinalityPolicy,
  state: WatcherFinalityState,
): WatcherRollbackReasonCode | null => {
  if (state.network !== policy.network) {
    return "network_mismatch";
  }
  if (state.blueprintHash !== policy.blueprintHash) {
    return "blueprint_mismatch";
  }
  if (!sameMarker(state.deploymentMarker, policy.deploymentMarker)) {
    return "deployment_mismatch";
  }
  return state.policyDigest === policy.policyDigest ? null : "policy_mismatch";
};
