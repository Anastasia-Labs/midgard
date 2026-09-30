import { createHash } from "node:crypto";

import {
  rejectionCodeOf,
  type RejectionReason,
  rejectionReasonArmOf,
} from "@al-ft/midgard-sdk";

import {
  watcherSameCanonicalJson,
  watcherSha256CanonicalJson,
} from "../../storage/durable-store.js";
import {
  type EvidenceGraphBudget,
  HEX_28,
  HEX_32,
  HEX_BYTES,
  NATURAL,
  type PlainRecord,
  WATCHER_FORCED_TERMINAL_CLASSIFICATION_SCHEMA_VERSION,
  WATCHER_USER_EVENT_INDEXER_BOUNDS,
  type WatcherForcedOperatorVerdict,
  type WatcherForcedTerminalClassification,
  type WatcherUserEventNetwork,
} from "./types.js";

const NETWORKS = ["Mainnet", "Preprod", "Preview", "Custom"] as const;

export const immutableWireValue = <T>(value: T): T => {
  const clone = JSON.parse(JSON.stringify(value)) as T;
  const pending: object[] =
    typeof clone === "object" && clone !== null ? [clone] : [];
  while (pending.length > 0) {
    const candidate = pending.pop()!;
    for (const member of Object.values(candidate)) {
      if (typeof member === "object" && member !== null) {
        pending.push(member);
      }
    }
    Object.freeze(candidate);
  }
  return clone;
};

export const evidenceWithinBounds = (
  value: unknown,
  budget: EvidenceGraphBudget = { nodes: 0, bytes: 0 },
): boolean => {
  const seen = new WeakSet<object>();
  const path = new WeakSet<object>();
  const visit = (candidate: unknown): boolean => {
    budget.nodes += 1;
    if (budget.nodes > WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceGraphNodes) {
      return false;
    }
    if (typeof candidate === "string") {
      budget.bytes += Buffer.byteLength(candidate, "utf8");
    } else if (typeof candidate === "number") {
      if (!Number.isSafeInteger(candidate)) {
        return false;
      }
      budget.bytes += 8;
    } else if (
      typeof candidate === "bigint" ||
      typeof candidate === "symbol" ||
      typeof candidate === "function" ||
      typeof candidate === "undefined"
    ) {
      return false;
    } else if (typeof candidate === "boolean") {
      budget.bytes += 8;
    }
    if (budget.bytes > WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceGraphBytes) {
      return false;
    }
    if (typeof candidate !== "object" || candidate === null) {
      return true;
    }
    if (path.has(candidate)) {
      // A node reachable from itself is a true cycle; recursive parsers must
      // never see one.
      return false;
    }
    if (seen.has(candidate)) {
      // Shared acyclic evidence is walked and budgeted once.
      return true;
    }
    seen.add(candidate);
    path.add(candidate);
    const array = Array.isArray(candidate);
    if (
      Object.getPrototypeOf(candidate) !==
      (array ? Array.prototype : Object.prototype)
    ) {
      return false;
    }
    if (
      array &&
      candidate.length >
        WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceContainerEntries
    ) {
      return false;
    }
    const keys = Reflect.ownKeys(candidate);
    if (
      keys.some((key) => typeof key !== "string") ||
      (array &&
        (keys.length !== candidate.length + 1 ||
          keys.some(
            (key) =>
              key !== "length" &&
              (!NATURAL.test(key as string) ||
                BigInt(key as string) >= BigInt(candidate.length)),
          )))
    ) {
      return false;
    }
    if (
      !array &&
      keys.length > WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceContainerEntries
    ) {
      return false;
    }
    for (const key of keys) {
      const descriptor = Object.getOwnPropertyDescriptor(candidate, key);
      if (
        descriptor === undefined ||
        descriptor.get !== undefined ||
        descriptor.set !== undefined ||
        (key !== "length" && !descriptor.enumerable)
      ) {
        return false;
      }
      if (key === "length") {
        continue;
      }
      budget.bytes += Buffer.byteLength(key as string, "utf8");
      if (budget.bytes > WATCHER_USER_EVENT_INDEXER_BOUNDS.evidenceGraphBytes) {
        return false;
      }
      if (!visit(descriptor.value)) {
        return false;
      }
    }
    path.delete(candidate);
    return true;
  };
  try {
    return visit(value);
  } catch {
    return false;
  }
};

export const sha256Bytes = (value: Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");

export const sha256Canonical = watcherSha256CanonicalJson;

export const same = watcherSameCanonicalJson;

export const exactRecord = (
  value: unknown,
  keys: readonly string[],
): PlainRecord | null => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== keys.length
  ) {
    return null;
  }
  const expected = new Set(keys);
  for (const key of Reflect.ownKeys(value)) {
    if (typeof key !== "string" || !expected.has(key)) {
      return null;
    }
    const descriptor = Object.getOwnPropertyDescriptor(value, key);
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

export const isHex28 = (value: unknown): value is string =>
  typeof value === "string" && HEX_28.test(value);

export const isHex32 = (value: unknown): value is string =>
  typeof value === "string" && HEX_32.test(value);

export const isHexBytes = (value: unknown): value is string =>
  typeof value === "string" && HEX_BYTES.test(value);

export const isNatural = (value: unknown): value is string =>
  typeof value === "string" && NATURAL.test(value);

export const isNetwork = (value: unknown): value is WatcherUserEventNetwork =>
  typeof value === "string" &&
  NETWORKS.includes(value as WatcherUserEventNetwork);

/**
 * `ForcedTxValid` — the watcher spelling of an accepting operator verdict.
 */
export const WATCHER_FORCED_TX_VALID = "ForcedTxValid" as const;

/**
 * Membership test for {@link WatcherForcedOperatorVerdict}: the accepting
 * literal, or any constructor tag the canonical 47-arm `RejectionReason`
 * bridge knows. Delegating to `rejectionCodeOf` (rather than restating the
 * arm list here) keeps the watcher vocabulary in lockstep with the SDK twin.
 */
export const isWatcherForcedOperatorVerdict = (
  value: unknown,
): value is WatcherForcedOperatorVerdict => {
  if (typeof value !== "string") {
    return false;
  }
  if (value === WATCHER_FORCED_TX_VALID) {
    return true;
  }
  try {
    rejectionCodeOf(value as RejectionReason);
    return true;
  } catch {
    return false;
  }
};

/**
 * Projects a decoded `OperatorVerdictV1` (`ForcedInclusionTxV1.verdict`) onto
 * its watcher spelling, or `null` when the value is not a verdict at all.
 */
export const watcherForcedOperatorVerdict = (
  verdict: unknown,
): WatcherForcedOperatorVerdict | null => {
  if (verdict === WATCHER_FORCED_TX_VALID) {
    return WATCHER_FORCED_TX_VALID;
  }
  if (typeof verdict !== "object" || verdict === null) {
    return null;
  }
  const invalid = (verdict as { ForcedTxInvalid?: unknown }).ForcedTxInvalid;
  if (typeof invalid !== "object" || invalid === null) {
    return null;
  }
  const reason = (invalid as { reason?: unknown }).reason;
  if (
    typeof reason !== "string" &&
    (typeof reason !== "object" || reason === null)
  ) {
    return null;
  }
  let arm: string;
  try {
    arm = rejectionReasonArmOf(reason as RejectionReason);
  } catch {
    return null;
  }
  return isWatcherForcedOperatorVerdict(arm) ? arm : null;
};

export const parseForcedTerminalClassification = (
  value: unknown,
): WatcherForcedTerminalClassification | null => {
  const record = exactRecord(value, [
    "schemaVersion",
    "operatorValidity",
    "terminalTransactionHash",
    "terminalPointDigest",
  ]);
  if (
    record === null ||
    record.schemaVersion !==
      WATCHER_FORCED_TERMINAL_CLASSIFICATION_SCHEMA_VERSION ||
    !isWatcherForcedOperatorVerdict(record.operatorValidity) ||
    !isHex32(record.terminalTransactionHash) ||
    !isHex32(record.terminalPointDigest)
  ) {
    return null;
  }
  return Object.freeze({
    schemaVersion: WATCHER_FORCED_TERMINAL_CLASSIFICATION_SCHEMA_VERSION,
    operatorValidity: record.operatorValidity,
    terminalTransactionHash: record.terminalTransactionHash,
    terminalPointDigest: record.terminalPointDigest,
  });
};
