import { mkdtemp, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import { watcherSha256CanonicalJsonV1 } from "../src/durable-store.js";
import {
  openWatcherProductionFaultDecisionJournalV1,
  unsafeOpenWatcherProductionFaultDecisionJournalForTestV1,
} from "../src/production-fault-decision-journal-v1.js";
import { WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1 } from "../src/production-fault-proof-application-v1.js";
import {
  enqueueWatcherProductionFaultDecisionV1,
  unsafeCreateWatcherProductionFaultProofSupervisorForTestV1,
} from "../src/production-fault-proof-supervisor-v1.js";

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
  const launchScope = [...WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1];
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
    launchScopeDigest: watcherSha256CanonicalJsonV1(launchScope),
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
    decisionDigest: watcherSha256CanonicalJsonV1(content),
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
    decisionDigest: watcherSha256CanonicalJsonV1(content),
  });
};

afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map(async (path) => await rm(path, { force: true, recursive: true })),
  );
});

describe("production fault decision journal", () => {
  it("persists exact envelopes but never recreates runnable authority", async () => {
    const root = await directory();
    const journal =
      await unsafeOpenWatcherProductionFaultDecisionJournalForTestV1({
        directory: root,
        deploymentFingerprint: DEPLOYMENT,
        launchScope: WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1,
      });
    const first =
      await journal.unsafeAppendDecisionEnvelopeForTest(faultDecision());
    const duplicate =
      await journal.unsafeAppendDecisionEnvelopeForTest(faultDecision());
    expect(first.revision).toBe("0");
    expect(duplicate).toEqual(first);

    const reopened = await openWatcherProductionFaultDecisionJournalV1({
      directory: root,
      deploymentFingerprint: DEPLOYMENT,
      launchScope: WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1,
    });
    const [persisted] = await reopened.readAll();
    expect(persisted?.decision.decision).toBe("fault_detected");
    await expect(
      reopened.appendLiveDecision(persisted!.decision),
    ).rejects.toThrow("was not module-admitted");

    let calls = 0;
    const supervisor =
      unsafeCreateWatcherProductionFaultProofSupervisorForTestV1({
        journalRoot: root,
        deploymentFingerprint: DEPLOYMENT,
        run: async () => {
          calls += 1;
        },
      });
    await supervisor.recoverExisting(null);
    await expect(
      enqueueWatcherProductionFaultDecisionV1({
        supervisor,
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
        rollbackGeneration: "0",
      }),
    ).rejects.toThrow("was not module-admitted");
    expect(calls).toBe(0);
    await supervisor.close();
  });

  it("serializes concurrent decisions into one contiguous hash chain", async () => {
    const root = await directory();
    const journal =
      await unsafeOpenWatcherProductionFaultDecisionJournalForTestV1({
        directory: root,
        deploymentFingerprint: DEPLOYMENT,
        launchScope: WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1,
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
    const journal =
      await unsafeOpenWatcherProductionFaultDecisionJournalForTestV1({
        directory: root,
        deploymentFingerprint: DEPLOYMENT,
        launchScope: WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1,
      });
    const swappedScope = [
      ...WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1,
    ];
    [swappedScope[0], swappedScope[1]] = [swappedScope[1]!, swappedScope[0]!];
    await expect(
      journal.unsafeAppendDecisionEnvelopeForTest(
        faultDecision({
          launchScope: swappedScope,
          launchScopeDigest: watcherSha256CanonicalJsonV1(swappedScope),
        }),
      ),
    ).rejects.toThrow("launch scope differs");
    await expect(
      journal.unsafeAppendDecisionEnvelopeForTest(
        faultDecision({ category: "fabricatedDeposit" }),
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
    const journal =
      await unsafeOpenWatcherProductionFaultDecisionJournalForTestV1(
        {
          directory: root,
          deploymentFingerprint: DEPLOYMENT,
          launchScope: WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1,
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
    const journal =
      await unsafeOpenWatcherProductionFaultDecisionJournalForTestV1(
        {
          directory: root,
          deploymentFingerprint: DEPLOYMENT,
          launchScope: WATCHER_INSTALLED_PRODUCTION_WORKFLOW_CATEGORIES_V1,
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
