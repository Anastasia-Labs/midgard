import * as fs from "node:fs/promises";
import { mkdtemp, readdir, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { afterEach, describe, expect, it, vi } from "vitest";

import {
  openWatcherFaultDecisionJournal,
  unsafeOpenWatcherFaultDecisionJournalForTest,
} from "../../src/fault-proofs/fault-decision-journal.js";
import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "../../src/fault-proofs/fault-proof-application.js";
import { unsafeCreateWatcherFaultProofSupervisorForTest } from "../../src/fault-proofs/fault-proof-supervisor.js";
import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";

vi.mock("node:fs/promises", async (importOriginal) => ({
  ...(await importOriginal<typeof import("node:fs/promises")>()),
}));

const directories: string[] = [];
const DEPLOYMENT = "dd".repeat(32);
const HEADER = "aa".repeat(28);
const DIGEST = "bb".repeat(32);

const directory = async (): Promise<string> => {
  const path = await mkdtemp("/var/tmp/midgard-fault-decisions-");
  directories.push(path);
  return path;
};

const faultDecision = (
  overrides: Readonly<Record<string, unknown>> = {},
): Readonly<Record<string, unknown>> => {
  const launchScope = [...WATCHER_INSTALLED_WORKFLOW_CATEGORIES];
  const content = {
    schemaVersion: "midgard-production-header-decision-v1",
    classifierVersion: "midgard-production-header-classifier-v1",
    deploymentFingerprint: DEPLOYMENT,
    headerHash: HEADER,
    authenticatedObservationDigest: "11".repeat(32),
    payloadEnvelopeSha256: "22".repeat(32),
    payloadSha256: "33".repeat(32),
    replayVersion: "midgard-complete-canonical-replay-v1",
    replayDigest: "44".repeat(32),
    launchScope,
    launchScopeDigest: watcherSha256CanonicalJson(launchScope),
    classificationDigest: "55".repeat(32),
    decision: "fault_detected",
    category: "doubleSpend",
    violationId: "double_spend_v1",
    detectionId: `double_spend_v1:0:${DIGEST}`,
    position: "0",
    ...overrides,
  };
  return Object.freeze({
    ...content,
    decisionDigest: watcherSha256CanonicalJson(content),
  });
};

const healthyDecision = (): Readonly<Record<string, unknown>> => {
  const fault = faultDecision();
  const {
    category: _category,
    violationId: _violationId,
    detectionId: _detectionId,
    position: _position,
    decisionDigest: _decisionDigest,
    ...common
  } = fault;
  const content = { ...common, decision: "healthy" };
  return Object.freeze({
    ...content,
    decisionDigest: watcherSha256CanonicalJson(content),
  });
};

afterEach(async () => {
  vi.restoreAllMocks();
  await Promise.all(
    directories
      .splice(0)
      .map(async (path) => await rm(path, { force: true, recursive: true })),
  );
});

describe("production fault decision journal", () => {
  it("exposes only complete records to a fresh reader during a delayed write", async () => {
    const root = await directory();
    const input = {
      directory: root,
      deploymentFingerprint: DEPLOYMENT,
      launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    };
    const journal = await unsafeOpenWatcherFaultDecisionJournalForTest(input);
    const first =
      await journal.unsafeAppendDecisionEnvelopeForTest(faultDecision());
    let markWriteStarted!: () => void;
    let releaseWrite!: () => void;
    const writeStarted = new Promise<void>((resolve) => {
      markWriteStarted = resolve;
    });
    const writeGate = new Promise<void>((resolve) => {
      releaseWrite = resolve;
    });
    const originalOpen = fs.open;
    vi.spyOn(fs, "open").mockImplementation(async (...args) => {
      const handle = await originalOpen(...args);
      if (args[1] === "wx") {
        const originalWrite = handle.writeFile.bind(handle);
        vi.spyOn(handle, "writeFile").mockImplementationOnce(async (bytes) => {
          if (!(bytes instanceof Uint8Array))
            throw new Error("fixture expects bytes");
          const middle = Math.floor(bytes.length / 2);
          await originalWrite(bytes.subarray(0, middle));
          markWriteStarted();
          await writeGate;
          await originalWrite(bytes.subarray(middle));
        });
      }
      return handle;
    });
    const append =
      journal.unsafeAppendDecisionEnvelopeForTest(healthyDecision());
    try {
      await writeStarted;
      const reader = await openWatcherFaultDecisionJournal(input);
      expect(await reader.readAll()).toEqual([first]);
    } finally {
      releaseWrite();
      await append;
    }
    const reader = await openWatcherFaultDecisionJournal(input);
    expect((await reader.readAll()).map(({ revision }) => revision)).toEqual([
      "0",
      "1",
    ]);
    expect(await readdir(root)).toEqual(["fault-decisions"]);
  });

  it("never overwrites an existing revision when independent writers collide", async () => {
    const root = await directory();
    const input = {
      directory: root,
      deploymentFingerprint: DEPLOYMENT,
      launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    };
    const first = await unsafeOpenWatcherFaultDecisionJournalForTest(input);
    const second = await unsafeOpenWatcherFaultDecisionJournalForTest(input);
    await first.unsafeAppendDecisionEnvelopeForTest(faultDecision());
    const path = join(root, "fault-decisions", "00000000000000000000.json");
    const original = await readFile(path);
    await expect(
      second.unsafeAppendDecisionEnvelopeForTest(healthyDecision()),
    ).rejects.toMatchObject({ code: "EEXIST" });
    expect(await readFile(path)).toEqual(original);
    expect(await first.audit()).toEqual(await first.readAll());
    expect(await readdir(root)).toEqual(["fault-decisions"]);
  });

  it("admits out-ref detection identifiers without relaxing violation identifiers", async () => {
    const journal = await unsafeOpenWatcherFaultDecisionJournalForTest({
      directory: await directory(),
      deploymentFingerprint: DEPLOYMENT,
      launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    });
    const envelope = faultDecision({
      detectionId: `double-spend:0:1:0:${DIGEST}#0`,
    });
    expect(
      (await journal.unsafeAppendDecisionEnvelopeForTest(envelope)).decision,
    ).toEqual(envelope);
    await expect(
      journal.unsafeAppendDecisionEnvelopeForTest(
        faultDecision({ violationId: "double-spend#0" }),
      ),
    ).rejects.toThrow("violation id is invalid");
    await expect(
      journal.unsafeAppendDecisionEnvelopeForTest(
        faultDecision({ detectionId: "double-spend:bad input#0" }),
      ),
    ).rejects.toThrow("detection id is invalid");
  });
  it("persists exact envelopes but never recreates runnable authority", async () => {
    const root = await directory();
    const journal = await unsafeOpenWatcherFaultDecisionJournalForTest({
      directory: root,
      deploymentFingerprint: DEPLOYMENT,
      launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    });
    const first =
      await journal.unsafeAppendDecisionEnvelopeForTest(faultDecision());
    const duplicate =
      await journal.unsafeAppendDecisionEnvelopeForTest(faultDecision());
    expect(first.revision).toBe("0");
    expect(duplicate).toEqual(first);

    const reopened = await openWatcherFaultDecisionJournal({
      directory: root,
      deploymentFingerprint: DEPLOYMENT,
      launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    });
    const [persisted] = await reopened.readAll();
    expect(persisted?.decision.decision).toBe("fault_detected");
    if (persisted?.decision.decision !== "fault_detected")
      throw new Error("fixture must retain a fault");
    await expect(
      reopened.appendLiveDecision(persisted!.decision),
    ).rejects.toThrow("was not module-admitted");

    let calls = 0;
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: root,
      deploymentFingerprint: DEPLOYMENT,
      run: async () => {
        calls += 1;
      },
    });
    await supervisor.recoverExisting(null);
    await expect(
      supervisor.requestProgress({
        observation: progressObservation({ deploymentFingerprint: DEPLOYMENT }),
        rollbackGeneration: "0",
        fault: {
          decision: persisted!.decision,
          actuationPermit: Object.freeze({
            permitVersion: "midgard-production-workflow-actuation-permit-v1",
          }),
          deadline: Object.freeze({
            headerHash: persisted!.decision.headerHash,
            headerEndTimeMs: "0",
            maturityAtMs: "604800000",
            latestSafeStartAtMs: "302400000",
          }),
        },
      }),
    ).rejects.toThrow("was not module-admitted");
    expect(calls).toBe(0);
    await supervisor.close();
  });

  it("serializes concurrent decisions into one contiguous hash chain", async () => {
    const root = await directory();
    const journal = await unsafeOpenWatcherFaultDecisionJournalForTest({
      directory: root,
      deploymentFingerprint: DEPLOYMENT,
      launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    });
    await Promise.all([
      journal.unsafeAppendDecisionEnvelopeForTest(faultDecision()),
      journal.unsafeAppendDecisionEnvelopeForTest(healthyDecision()),
    ]);
    const records = await journal.readAll();
    expect(records.map(({ revision }) => revision)).toEqual(["0", "1"]);
    expect(records[1]!.priorRecordSha256).toMatch(/^[0-9a-f]{64}$/u);

    const secondPath = join(
      root,
      "fault-decisions",
      "00000000000000000001.json",
    );
    await writeFile(
      secondPath,
      `${JSON.stringify({ ...records[1], priorRecordSha256: "00".repeat(32) })}\n`,
      "utf8",
    );
    await expect(journal.audit()).rejects.toThrow("chain is invalid");
  });

  it("rejects scope, category, digest, and record-layout substitutions", async () => {
    const root = await directory();
    const journal = await unsafeOpenWatcherFaultDecisionJournalForTest({
      directory: root,
      deploymentFingerprint: DEPLOYMENT,
      launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
    });
    const swappedScope = [...WATCHER_INSTALLED_WORKFLOW_CATEGORIES];
    [swappedScope[0], swappedScope[1]] = [swappedScope[1]!, swappedScope[0]!];
    await expect(
      journal.unsafeAppendDecisionEnvelopeForTest(
        faultDecision({
          launchScope: swappedScope,
          launchScopeDigest: watcherSha256CanonicalJson(swappedScope),
        }),
      ),
    ).rejects.toThrow("launch scope differs");
    await expect(
      journal.unsafeAppendDecisionEnvelopeForTest(
        faultDecision({ category: "unregisteredCategory" }),
      ),
    ).rejects.toThrow("kind or category is invalid");
    await expect(
      journal.unsafeAppendDecisionEnvelopeForTest({
        ...faultDecision(),
        decisionDigest: "00".repeat(32),
      }),
    ).rejects.toThrow("decision digest mismatch");

    await writeFile(
      join(root, "fault-decisions", "unexpected.json"),
      "{}\n",
      "utf8",
    );
    await expect(journal.audit()).rejects.toThrow(
      "contains invalid entry unexpected.json",
    );
  });

  it("appends 10,000 decisions with one opening scan and one exact read-back each", async () => {
    const files = new Map<string, Uint8Array>();
    let listCalls = 0;
    let readCalls = 0;
    let writeCalls = 0;
    let syncCalls = 0;
    const root = "/var/lib/midgard/test-fault-decision-scale";
    const journal = await unsafeOpenWatcherFaultDecisionJournalForTest(
      {
        directory: root,
        deploymentFingerprint: DEPLOYMENT,
        launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
      },
      Object.freeze({
        prepare: async () => undefined,
        list: async (directory) => {
          listCalls += 1;
          const prefix = `${directory}/`;
          return [...files.keys()]
            .filter((path) => path.startsWith(prefix))
            .map((path) =>
              Object.freeze({
                name: path.slice(prefix.length),
                isFile: true,
              }),
            );
        },
        read: async (path) => {
          readCalls += 1;
          const bytes = files.get(path);
          if (bytes === undefined) throw new Error("missing test record");
          return Uint8Array.from(bytes);
        },
        writeExclusive: async (path, bytes) => {
          writeCalls += 1;
          if (files.has(path)) throw new Error("exclusive create conflict");
          files.set(path, Uint8Array.from(bytes));
        },
        syncDirectory: async () => {
          syncCalls += 1;
        },
      }),
    );
    for (let index = 0; index < 10_000; index += 1) {
      await journal.unsafeAppendDecisionEnvelopeForTest(
        faultDecision({
          detectionId: `double_spend_v1:${index.toString()}:${DIGEST}`,
          position: index.toString(),
        }),
      );
    }
    expect((await journal.readAll()).length).toBe(10_000);
    expect({ listCalls, readCalls, writeCalls, syncCalls }).toEqual({
      listCalls: 1,
      readCalls: 10_000,
      writeCalls: 10_000,
      syncCalls: 10_000,
    });
  }, 30_000);

  it("serializes a full audit behind an in-flight exclusive append", async () => {
    const files = new Map<string, Uint8Array>();
    let releaseWrite!: () => void;
    let markWriteStarted!: () => void;
    const writeStarted = new Promise<void>((resolve) => {
      markWriteStarted = resolve;
    });
    const writeGate = new Promise<void>((resolve) => {
      releaseWrite = resolve;
    });
    const root = "/var/lib/midgard/test-fault-decision-concurrency";
    const journal = await unsafeOpenWatcherFaultDecisionJournalForTest(
      {
        directory: root,
        deploymentFingerprint: DEPLOYMENT,
        launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
      },
      Object.freeze({
        prepare: async () => undefined,
        list: async (directory) => {
          const prefix = `${directory}/`;
          return [...files.keys()]
            .filter((path) => path.startsWith(prefix))
            .map((path) => ({
              name: path.slice(prefix.length),
              isFile: true,
            }));
        },
        read: async (path) => {
          const bytes = files.get(path);
          if (bytes === undefined) throw new Error("missing test record");
          return bytes;
        },
        writeExclusive: async (path, bytes) => {
          files.set(path, Uint8Array.from(bytes));
          markWriteStarted();
          await writeGate;
        },
        syncDirectory: async () => undefined,
      }),
    );
    const appending =
      journal.unsafeAppendDecisionEnvelopeForTest(faultDecision());
    await writeStarted;
    const auditing = journal.audit();
    releaseWrite();
    await expect(appending).resolves.toMatchObject({ revision: "0" });
    await expect(auditing).resolves.toHaveLength(1);
  });
});
