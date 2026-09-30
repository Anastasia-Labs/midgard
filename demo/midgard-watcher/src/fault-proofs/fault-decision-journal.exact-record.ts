import { createHash } from "node:crypto";
import { isAbsolute, normalize } from "node:path";

import { type HeaderDecision } from "@al-ft/midgard-fault-proofs";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import type { WatcherInstalledWorkflowCategory } from "./fault-proof-application.js";

export const WATCHER_FAULT_DECISION_JOURNAL_SCHEMA_VERSION =
  "midgard-watcher-production-fault-decision-journal-v1" as const;

export const WATCHER_FAULT_DECISION_RECORD_SCHEMA_VERSION =
  "midgard-watcher-production-fault-decision-record-v1" as const;

export const DIGEST = /^[0-9a-f]{64}$/u;

export const HEADER_HASH = /^[0-9a-f]{56}$/u;

export const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

export const IDENTIFIER = /^[A-Za-z0-9][A-Za-z0-9:._/-]{0,511}$/u;

// Production detection identities include canonical transaction output references.
export const DETECTION_IDENTIFIER = /^[A-Za-z0-9][A-Za-z0-9:._/#-]{0,511}$/u;

export const RECORD_FILE = /^([0-9]{20})\.json$/u;

export const MAX_RECORD_BYTES = 64 * 1024;

export const MAX_RECORDS = 65_536;

export type WatcherPersistedFaultDecisionRecord = Readonly<{
  schemaVersion: typeof WATCHER_FAULT_DECISION_RECORD_SCHEMA_VERSION;
  revision: string;
  priorRecordSha256: string | null;
  decision: HeaderDecision;
}>;

export type WatcherFaultDecisionJournal = Readonly<{
  schemaVersion: typeof WATCHER_FAULT_DECISION_JOURNAL_SCHEMA_VERSION;
  readAll(): Promise<readonly WatcherPersistedFaultDecisionRecord[]>;
  /** Explicit bounded disk-chain audit; ordinary reads use the admitted cache. */
  audit(): Promise<readonly WatcherPersistedFaultDecisionRecord[]>;
  appendLiveDecision(
    decision: HeaderDecision,
  ): Promise<WatcherPersistedFaultDecisionRecord>;
}>;

export type UnsafeWatcherFaultDecisionJournalForTest =
  WatcherFaultDecisionJournal &
    Readonly<{
      unsafeAppendDecisionEnvelopeForTest(
        decision: unknown,
      ): Promise<WatcherPersistedFaultDecisionRecord>;
    }>;

export type UnsafeWatcherFaultDecisionJournalStorage = Readonly<{
  prepare(parent: string, directory: string): Promise<void>;
  list(
    directory: string,
  ): Promise<readonly Readonly<{ name: string; isFile: boolean }>[]>;
  read(path: string): Promise<Uint8Array>;
  writeExclusive(path: string, bytes: Uint8Array): Promise<void>;
  syncDirectory(directory: string): Promise<void>;
}>;

export const exactRecord = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error(`${label} must be a plain string-keyed object`);
  }
  const record = value as Readonly<Record<string, unknown>>;
  const actual = Object.keys(record).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has unknown or missing fields`);
  }
  for (const key of actual) {
    const descriptor = Object.getOwnPropertyDescriptor(record, key);
    if (
      descriptor === undefined ||
      descriptor.get !== undefined ||
      descriptor.set !== undefined
    ) {
      throw new Error(`${label} must not contain accessors`);
    }
  }
  return record;
};

export const exactString = (
  value: unknown,
  pattern: RegExp,
  label: string,
): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${label} is invalid`);
  }
  return value;
};

export const sha256 = (value: Uint8Array | string): string =>
  createHash("sha256").update(value).digest("hex");

export const canonicalDigest = (value: unknown): string =>
  sha256(watcherCanonicalJson(value));

export const canonicalDirectory = (value: unknown): string => {
  if (
    typeof value !== "string" ||
    value.trim() !== value ||
    !isAbsolute(value) ||
    normalize(value) !== value ||
    value === "/" ||
    value === "/tmp" ||
    value.startsWith("/tmp/")
  ) {
    throw new Error(
      "watcher fault decision journal requires a canonical durable directory",
    );
  }
  return value;
};

export const exactLaunchScope = (
  value: unknown,
  expected: readonly WatcherInstalledWorkflowCategory[],
): readonly WatcherInstalledWorkflowCategory[] => {
  if (
    !Array.isArray(value) ||
    value.length !== expected.length ||
    value.some((category, index) => category !== expected[index])
  ) {
    throw new Error(
      "persisted production decision launch scope differs from the installed application",
    );
  }
  return Object.freeze([...expected]);
};
