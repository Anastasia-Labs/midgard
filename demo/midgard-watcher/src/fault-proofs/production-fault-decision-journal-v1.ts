import { createHash } from "node:crypto";
import {
  type FileHandle,
  mkdir,
  open,
  readdir,
  readFile,
  realpath,
} from "node:fs/promises";
import { isAbsolute, join, normalize } from "node:path";

import {
  COMPLETE_CANONICAL_REPLAY,
  HEADER_CLASSIFIER,
  HEADER_DECISION,
  type HeaderDecision,
  headerDecisionEnvelope,
} from "@al-ft/midgard-fault-proofs";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import type { WatcherInstalledWorkflowCategory } from "./production-fault-proof-application-v1.js";

export const WATCHER_FAULT_DECISION_JOURNAL_SCHEMA_VERSION =
  "midgard-watcher-production-fault-decision-journal-v1" as const;
export const WATCHER_FAULT_DECISION_RECORD_SCHEMA_VERSION =
  "midgard-watcher-production-fault-decision-record-v1" as const;

const DIGEST = /^[0-9a-f]{64}$/u;
const HEADER_HASH = /^[0-9a-f]{56}$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const IDENTIFIER = /^[A-Za-z0-9][A-Za-z0-9:._/-]{0,511}$/u;
const RECORD_FILE = /^([0-9]{20})\.json$/u;
const MAX_RECORD_BYTES = 64 * 1024;
const MAX_RECORDS = 65_536;

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

const exactRecord = (
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

const exactString = (
  value: unknown,
  pattern: RegExp,
  label: string,
): string => {
  if (typeof value !== "string" || !pattern.test(value)) {
    throw new Error(`${label} is invalid`);
  }
  return value;
};

const sha256 = (value: Uint8Array | string): string =>
  createHash("sha256").update(value).digest("hex");

const canonicalDigest = (value: unknown): string =>
  sha256(watcherCanonicalJson(value));

const canonicalDirectory = (value: unknown): string => {
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

const exactLaunchScope = (
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

const parseDecision = (
  value: unknown,
  deploymentFingerprint: string,
  launchScope: readonly WatcherInstalledWorkflowCategory[],
): HeaderDecision => {
  const common = [
    "schemaVersion",
    "classifierVersion",
    "deploymentFingerprint",
    "headerHash",
    "authenticatedObservationDigest",
    "payloadEnvelopeSha256",
    "payloadSha256",
    "replayVersion",
    "replayDigest",
    "launchScope",
    "launchScopeDigest",
    "classificationDigest",
    "decisionDigest",
    "decision",
  ] as const;
  const candidate = exactRecord(
    value,
    (() => {
      const decision = (value as { readonly decision?: unknown } | null)
        ?.decision;
      if (decision === "fault_detected") {
        return [
          ...common,
          "category",
          "violationId",
          "detectionId",
          "position",
        ];
      }
      if (decision === "unprovable") {
        return [...common, "reason", "violationId", "detectionId", "position"];
      }
      return common;
    })(),
    "persisted production decision",
  );
  if (
    candidate.schemaVersion !== HEADER_DECISION ||
    candidate.classifierVersion !== HEADER_CLASSIFIER ||
    candidate.replayVersion !== COMPLETE_CANONICAL_REPLAY ||
    candidate.deploymentFingerprint !== deploymentFingerprint
  ) {
    throw new Error("persisted production decision identity is invalid");
  }
  const parsedScope = exactLaunchScope(candidate.launchScope, launchScope);
  if (
    exactString(
      candidate.launchScopeDigest,
      DIGEST,
      "persisted production decision launch-scope digest",
    ) !== canonicalDigest(parsedScope)
  ) {
    throw new Error(
      "persisted production decision launch-scope digest mismatch",
    );
  }
  const base = {
    schemaVersion: HEADER_DECISION,
    classifierVersion: HEADER_CLASSIFIER,
    deploymentFingerprint,
    headerHash: exactString(
      candidate.headerHash,
      HEADER_HASH,
      "persisted production decision header hash",
    ),
    authenticatedObservationDigest: exactString(
      candidate.authenticatedObservationDigest,
      DIGEST,
      "persisted production decision observation digest",
    ),
    payloadEnvelopeSha256: exactString(
      candidate.payloadEnvelopeSha256,
      DIGEST,
      "persisted production decision envelope digest",
    ),
    payloadSha256: exactString(
      candidate.payloadSha256,
      DIGEST,
      "persisted production decision payload digest",
    ),
    replayVersion: COMPLETE_CANONICAL_REPLAY,
    replayDigest: exactString(
      candidate.replayDigest,
      DIGEST,
      "persisted production decision replay digest",
    ),
    launchScope: parsedScope,
    launchScopeDigest: candidate.launchScopeDigest as string,
    classificationDigest: exactString(
      candidate.classificationDigest,
      DIGEST,
      "persisted production decision classification digest",
    ),
  } as const;
  const decision = (() => {
    if (candidate.decision === "healthy") {
      return Object.freeze({ ...base, decision: "healthy" as const });
    }
    const violationId = exactString(
      candidate.violationId,
      IDENTIFIER,
      "persisted production decision violation id",
    );
    const detectionId = exactString(
      candidate.detectionId,
      IDENTIFIER,
      "persisted production decision detection id",
    );
    const position = exactString(
      candidate.position,
      NATURAL,
      "persisted production decision position",
    );
    if (candidate.decision === "unprovable") {
      if (
        candidate.reason !== "unregistered_violation" &&
        candidate.reason !== "category_not_installed" &&
        candidate.reason !== "predecessor_context_unavailable"
      ) {
        throw new Error("persisted unprovable decision reason is invalid");
      }
      return Object.freeze({
        ...base,
        decision: "unprovable" as const,
        reason: candidate.reason,
        violationId,
        detectionId,
        position,
      });
    }
    if (
      candidate.decision !== "fault_detected" ||
      !launchScope.includes(
        candidate.category as WatcherInstalledWorkflowCategory,
      )
    ) {
      throw new Error(
        "persisted production decision kind or category is invalid",
      );
    }
    return Object.freeze({
      ...base,
      decision: "fault_detected" as const,
      category: candidate.category as WatcherInstalledWorkflowCategory,
      violationId,
      detectionId,
      position,
    });
  })();
  const decisionDigest = exactString(
    candidate.decisionDigest,
    DIGEST,
    "persisted production decision digest",
  );
  if (decisionDigest !== canonicalDigest(decision)) {
    throw new Error("persisted production decision digest mismatch");
  }
  return Object.freeze({ ...decision, decisionDigest });
};

const readBounded = async (
  storage: UnsafeWatcherFaultDecisionJournalStorage,
  path: string,
): Promise<Uint8Array> => {
  const bytes = await storage.read(path);
  if (bytes.byteLength === 0 || bytes.byteLength > MAX_RECORD_BYTES) {
    throw new Error("watcher fault decision journal record size is invalid");
  }
  return Uint8Array.from(bytes);
};

const syncDirectory = async (directory: string): Promise<void> => {
  let handle: FileHandle | undefined;
  try {
    handle = await open(directory, "r");
    await handle.sync();
  } finally {
    await handle?.close();
  }
};

const productionStorage: UnsafeWatcherFaultDecisionJournalStorage =
  Object.freeze({
    prepare: async (parent, directory) => {
      await mkdir(parent, { recursive: true, mode: 0o700 });
      if ((await realpath(parent)) !== parent) {
        throw new Error(
          "watcher fault decision journal parent traverses a symlink",
        );
      }
      await mkdir(directory, { recursive: true, mode: 0o700 });
      if ((await realpath(directory)) !== directory) {
        throw new Error("watcher fault decision journal traverses a symlink");
      }
    },
    list: async (directory) =>
      (await readdir(directory, { withFileTypes: true })).map((entry) =>
        Object.freeze({ name: entry.name, isFile: entry.isFile() }),
      ),
    read: async (path) => Uint8Array.from(await readFile(path)),
    writeExclusive: async (path, bytes) => {
      let handle: FileHandle | undefined;
      try {
        handle = await open(path, "wx", 0o600);
        await handle.writeFile(bytes);
        await handle.sync();
      } finally {
        await handle?.close();
      }
    },
    syncDirectory,
  });

const createJournal = async (input: {
  readonly directory: string;
  readonly deploymentFingerprint: string;
  readonly launchScope: readonly WatcherInstalledWorkflowCategory[];
  readonly exposeUnsafeAppendForTest: boolean;
  readonly storage: UnsafeWatcherFaultDecisionJournalStorage;
}): Promise<
  WatcherFaultDecisionJournal | UnsafeWatcherFaultDecisionJournalForTest
> => {
  const parent = canonicalDirectory(input.directory);
  const deploymentFingerprint = exactString(
    input.deploymentFingerprint,
    DIGEST,
    "watcher fault decision deployment fingerprint",
  );
  const launchScope = exactLaunchScope(input.launchScope, input.launchScope);
  const directory = join(parent, "fault-decisions");
  await input.storage.prepare(parent, directory);

  const parseRecordBytes = (
    bytes: Uint8Array,
    index: number,
    priorSha256: string | null,
  ): WatcherPersistedFaultDecisionRecord => {
    let value: unknown;
    try {
      value = JSON.parse(
        new TextDecoder("utf-8", { fatal: true }).decode(bytes),
      );
    } catch {
      throw new Error("watcher fault decision journal record is malformed");
    }
    const record = exactRecord(
      value,
      ["schemaVersion", "revision", "priorRecordSha256", "decision"],
      "watcher fault decision record",
    );
    if (
      record.schemaVersion !== WATCHER_FAULT_DECISION_RECORD_SCHEMA_VERSION ||
      record.revision !== index.toString() ||
      record.priorRecordSha256 !== priorSha256
    ) {
      throw new Error("watcher fault decision journal chain is invalid");
    }
    const parsed = Object.freeze({
      schemaVersion: WATCHER_FAULT_DECISION_RECORD_SCHEMA_VERSION,
      revision: index.toString(),
      priorRecordSha256: priorSha256,
      decision: parseDecision(
        record.decision,
        deploymentFingerprint,
        launchScope,
      ),
    });
    const canonicalBytes = Buffer.from(
      `${watcherCanonicalJson(parsed)}\n`,
      "utf8",
    );
    if (!Buffer.from(bytes).equals(canonicalBytes)) {
      throw new Error("watcher fault decision journal bytes are noncanonical");
    }
    return parsed;
  };

  const scan = async (): Promise<
    readonly WatcherPersistedFaultDecisionRecord[]
  > => {
    const entries = await input.storage.list(directory);
    const names = entries
      .map((entry) => {
        if (!entry.isFile || !RECORD_FILE.test(entry.name)) {
          throw new Error(
            `watcher fault decision journal contains invalid entry ${entry.name}`,
          );
        }
        return entry.name;
      })
      .sort();
    if (names.length > MAX_RECORDS) {
      throw new Error(
        "watcher fault decision journal exceeds its record bound",
      );
    }
    const records: WatcherPersistedFaultDecisionRecord[] = [];
    let priorSha256: string | null = null;
    for (let index = 0; index < names.length; index += 1) {
      const name = names[index]!;
      const expectedName = `${index.toString().padStart(20, "0")}.json`;
      if (name !== expectedName) {
        throw new Error("watcher fault decision journal has a revision gap");
      }
      const bytes = await readBounded(input.storage, join(directory, name));
      const parsed = parseRecordBytes(bytes, index, priorSha256);
      priorSha256 = sha256(bytes);
      records.push(parsed);
    }
    return Object.freeze(records);
  };

  const cachedRecords = [...(await scan())];
  const decisionByDigest = new Map(
    cachedRecords.map((record) => [record.decision.decisionDigest, record]),
  );
  if (decisionByDigest.size !== cachedRecords.length) {
    throw new Error("watcher fault decision journal repeats a decision digest");
  }
  let lastRecordSha256 = (() => {
    const prior = cachedRecords.at(-1);
    return prior === undefined
      ? null
      : sha256(`${watcherCanonicalJson(prior)}\n`);
  })();

  let serial = Promise.resolve();
  const serialized = async <Result>(operation: () => Promise<Result>) => {
    const previous = serial;
    let release!: () => void;
    serial = new Promise<void>((resolve) => {
      release = resolve;
    });
    await previous;
    try {
      return await operation();
    } finally {
      release();
    }
  };

  const appendEnvelope = async (
    value: unknown,
  ): Promise<WatcherPersistedFaultDecisionRecord> =>
    await serialized(async () => {
      const decision = parseDecision(value, deploymentFingerprint, launchScope);
      const existing = decisionByDigest.get(decision.decisionDigest);
      if (existing !== undefined) return existing;
      if (cachedRecords.length >= MAX_RECORDS) {
        throw new Error("watcher fault decision journal is full");
      }
      const record = Object.freeze({
        schemaVersion: WATCHER_FAULT_DECISION_RECORD_SCHEMA_VERSION,
        revision: cachedRecords.length.toString(),
        priorRecordSha256: lastRecordSha256,
        decision,
      });
      const bytes = Buffer.from(`${watcherCanonicalJson(record)}\n`, "utf8");
      const path = join(
        directory,
        `${cachedRecords.length.toString().padStart(20, "0")}.json`,
      );
      await input.storage.writeExclusive(path, bytes);
      await input.storage.syncDirectory(directory);
      const readBack = await readBounded(input.storage, path);
      const appended = parseRecordBytes(
        readBack,
        cachedRecords.length,
        lastRecordSha256,
      );
      if (appended.decision.decisionDigest !== decision.decisionDigest) {
        throw new Error(
          "watcher fault decision journal failed append read-back",
        );
      }
      cachedRecords.push(appended);
      decisionByDigest.set(decision.decisionDigest, appended);
      lastRecordSha256 = sha256(readBack);
      return appended;
    });

  const journal: WatcherFaultDecisionJournal = Object.freeze({
    schemaVersion: WATCHER_FAULT_DECISION_JOURNAL_SCHEMA_VERSION,
    readAll: async () => Object.freeze([...cachedRecords]),
    audit: async () =>
      await serialized(async () => {
        const audited = await scan();
        if (
          watcherCanonicalJson(audited) !== watcherCanonicalJson(cachedRecords)
        ) {
          throw new Error(
            "watcher fault decision journal changed outside the admitted writer",
          );
        }
        return audited;
      }),
    appendLiveDecision: async (decision) =>
      await appendEnvelope(headerDecisionEnvelope(decision)),
  });
  return input.exposeUnsafeAppendForTest
    ? Object.freeze({
        ...journal,
        unsafeAppendDecisionEnvelopeForTest: appendEnvelope,
      })
    : journal;
};

export const openWatcherFaultDecisionJournal = async (input: {
  readonly directory: string;
  readonly deploymentFingerprint: string;
  readonly launchScope: readonly WatcherInstalledWorkflowCategory[];
}): Promise<WatcherFaultDecisionJournal> =>
  (await createJournal({
    ...input,
    exposeUnsafeAppendForTest: false,
    storage: productionStorage,
  })) as WatcherFaultDecisionJournal;

/** Test-only structural seeding seam; production append still requires admission. */
export const unsafeOpenWatcherFaultDecisionJournalForTest = async (
  input: {
    readonly directory: string;
    readonly deploymentFingerprint: string;
    readonly launchScope: readonly WatcherInstalledWorkflowCategory[];
  },
  storage: UnsafeWatcherFaultDecisionJournalStorage = productionStorage,
): Promise<UnsafeWatcherFaultDecisionJournalForTest> =>
  (await createJournal({
    ...input,
    exposeUnsafeAppendForTest: true,
    storage,
  })) as UnsafeWatcherFaultDecisionJournalForTest;
