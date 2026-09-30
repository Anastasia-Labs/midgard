import { join } from "node:path";

import { headerDecisionEnvelope } from "@al-ft/midgard-fault-proofs";

import { watcherCanonicalJson } from "../storage/durable-store.js";
import {
  canonicalDirectory,
  DIGEST,
  exactLaunchScope,
  exactRecord,
  exactString,
  MAX_RECORDS,
  RECORD_FILE,
  sha256,
  type UnsafeWatcherFaultDecisionJournalForTest,
  type UnsafeWatcherFaultDecisionJournalStorage,
  WATCHER_FAULT_DECISION_JOURNAL_SCHEMA_VERSION,
  WATCHER_FAULT_DECISION_RECORD_SCHEMA_VERSION,
  type WatcherFaultDecisionJournal,
  type WatcherPersistedFaultDecisionRecord,
} from "./fault-decision-journal.exact-record.js";
import {
  parseDecision,
  productionStorage,
  readBounded,
} from "./fault-decision-journal.parse-decision.js";
import type { WatcherInstalledWorkflowCategory } from "./fault-proof-application.js";

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
