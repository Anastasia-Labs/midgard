import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { afterEach, describe, expect, it } from "vitest";

import {
  openWatcherFaultDecisionJournal,
  readWatcherFaultDecisionEvidence,
  unsafeOpenWatcherFaultDecisionJournalForTest,
} from "../../src/fault-proofs/fault-decision-journal.js";
import { WATCHER_INSTALLED_WORKFLOW_CATEGORIES } from "../../src/fault-proofs/fault-proof-application.js";
import { unsafeCreateWatcherFaultProofSupervisorForTest } from "../../src/fault-proofs/fault-proof-supervisor.js";
import {
  closeWatcherJournalDatabase,
  isWatcherJournalIntegrityError,
  openWatcherJournalDatabase,
  WATCHER_JOURNAL_DATABASE_FILE,
  watcherJournalIntegrityFailure,
} from "../../src/fault-proofs/watcher-journal-database.js";
import { watcherObjectiveScope } from "../../src/fault-proofs/watcher-journal-schema.js";
import { watcherSha256CanonicalJson } from "../../src/storage/durable-store.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import {
  journalDirectory,
  removeJournalDirectories,
  TEST_JOURNAL_KEY,
} from "../support/watcher-journal-fixture.js";

const DEPLOYMENT = "dd".repeat(32);
const HEADER = "aa".repeat(28);
const DIGEST = "bb".repeat(32);

const directory = (): Promise<string> =>
  journalDirectory("midgard-fault-decisions");

const journalInput = (root: string) => ({
  directory: root,
  deploymentFingerprint: DEPLOYMENT,
  launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
  authenticationKey: TEST_JOURNAL_KEY,
});

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

afterEach(removeJournalDirectories);

describe("production fault decision journal", () => {
  it("shows a second handle the rows another handle committed", async () => {
    const root = await directory();
    const writer = await unsafeOpenWatcherFaultDecisionJournalForTest(
      journalInput(root),
    );
    const reader = await openWatcherFaultDecisionJournal(journalInput(root));
    expect(await reader.readAll()).toEqual([]);
    const first =
      await writer.unsafeAppendDecisionEnvelopeForTest(faultDecision());
    expect(await reader.read(first.decision.decisionDigest)).toEqual(first);
    const second =
      await writer.unsafeAppendDecisionEnvelopeForTest(healthyDecision());
    expect(await reader.readAll()).toEqual([first, second]);
    expect(first.revision).toBe("1");
    expect(second.revision).toBe("2");
  });

  it("keeps every row across a restart and serves them as evidence", async () => {
    const root = await directory();
    const journal = await unsafeOpenWatcherFaultDecisionJournalForTest(
      journalInput(root),
    );
    const first =
      await journal.unsafeAppendDecisionEnvelopeForTest(faultDecision());
    closeWatcherJournalDatabase(root);
    const reopened = await openWatcherFaultDecisionJournal(journalInput(root));
    expect(await reopened.readAll()).toEqual([first]);
    expect(
      readWatcherFaultDecisionEvidence({
        directory: root,
        deploymentFingerprint: DEPLOYMENT,
        launchScope: WATCHER_INSTALLED_WORKFLOW_CATEGORIES,
      }),
    ).toEqual([first.decision]);
  });

  it("admits out-ref detection identifiers without relaxing violation identifiers", async () => {
    const journal = await unsafeOpenWatcherFaultDecisionJournalForTest(
      journalInput(await directory()),
    );
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
    const journal = await unsafeOpenWatcherFaultDecisionJournalForTest(
      journalInput(root),
    );
    const first =
      await journal.unsafeAppendDecisionEnvelopeForTest(faultDecision());
    const duplicate =
      await journal.unsafeAppendDecisionEnvelopeForTest(faultDecision());
    expect(first.revision).toBe("1");
    expect(duplicate).toEqual(first);

    closeWatcherJournalDatabase(root);
    const reopened = await openWatcherFaultDecisionJournal(journalInput(root));
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
            maturityAtMs: MIDGARD_RETENTION_WINDOW.maturityMs.toString(),
            latestSafeStartAtMs: (
              MIDGARD_RETENTION_WINDOW.maturityMs -
              MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs
            ).toString(),
          }),
        },
      }),
    ).rejects.toThrow("was not module-admitted");
    expect(calls).toBe(0);
    await supervisor.close();
  });

  it("commits concurrent decisions as consecutive revisions", async () => {
    const root = await directory();
    const journal = await unsafeOpenWatcherFaultDecisionJournalForTest(
      journalInput(root),
    );
    await Promise.all([
      journal.unsafeAppendDecisionEnvelopeForTest(faultDecision()),
      journal.unsafeAppendDecisionEnvelopeForTest(healthyDecision()),
    ]);
    const records = await journal.readAll();
    expect(records.map(({ revision }) => revision)).toEqual(["1", "2"]);
  });

  it("rejects scope, category and digest substitutions", async () => {
    const journal = await unsafeOpenWatcherFaultDecisionJournalForTest(
      journalInput(await directory()),
    );
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
    expect(await journal.readAll()).toEqual([]);
  });

  it("refuses a row whose authenticated body was swapped for another decision", async () => {
    const root = await directory();
    const journal = await unsafeOpenWatcherFaultDecisionJournalForTest(
      journalInput(root),
    );
    const fault =
      await journal.unsafeAppendDecisionEnvelopeForTest(faultDecision());
    await journal.unsafeAppendDecisionEnvelopeForTest(healthyDecision());
    closeWatcherJournalDatabase(root);
    // Swap the two rows' keys: each row keeps its own MAC, which binds its
    // key, so the edit must be refused at the next start.
    const database = new DatabaseSync(
      join(root, WATCHER_JOURNAL_DATABASE_FILE),
    );
    database
      .prepare(
        "UPDATE watcher_fault_decisions SET row_key = 'swap' WHERE row_key = ?",
      )
      .run(fault.decision.decisionDigest);
    database.close();
    await expect(
      openWatcherFaultDecisionJournal(journalInput(root)),
    ).rejects.toThrow("row swap MAC differs");
  });

  // Rows authenticated under the journal key, as a bug in an earlier writer
  // could have left them: the refusal must latch, not fail the process.
  const expectLatchedRefusal = async (
    row: Readonly<{ key: string; body: unknown }>,
    detail: string,
  ): Promise<void> => {
    const root = await directory();
    openWatcherJournalDatabase({
      journalRoot: root,
      authenticationKey: TEST_JOURNAL_KEY,
    }).transaction((tx) =>
      tx.put("fault_decisions", {
        key: row.key,
        scope: watcherObjectiveScope("doubleSpend", HEADER),
        state: "fault_detected",
        body: row.body,
      }),
    );
    closeWatcherJournalDatabase(root);
    const failure = await openWatcherFaultDecisionJournal(
      journalInput(root),
    ).catch((error: unknown) => error);
    expect(isWatcherJournalIntegrityError(failure)).toBe(true);
    expect(String(failure)).toContain(detail);
    expect(watcherJournalIntegrityFailure(root)).toContain(detail);
  };

  it("latches a refusal of an authenticated row that differs from its decision", async () => {
    await expectLatchedRefusal(
      { key: "cc".repeat(32), body: faultDecision() },
      `row ${"cc".repeat(32)} differs from its decision`,
    );
  });

  it("latches a refusal of an authenticated row whose body is not a decision", async () => {
    await expectLatchedRefusal(
      { key: "cc".repeat(32), body: { decision: "fault_detected" } },
      `row ${"cc".repeat(32)} body is not a decision`,
    );
  });

  it("appends 2,000 decisions at one row each", async () => {
    const root = await directory();
    const journal = await unsafeOpenWatcherFaultDecisionJournalForTest(
      journalInput(root),
    );
    for (let index = 0; index < 2_000; index += 1)
      await journal.unsafeAppendDecisionEnvelopeForTest(
        faultDecision({
          detectionId: `double_spend_v1:${index.toString()}:${DIGEST}`,
          position: index.toString(),
        }),
      );
    expect((await journal.readAll()).length).toBe(2_000);
    const database = new DatabaseSync(
      join(root, WATCHER_JOURNAL_DATABASE_FILE),
      {
        readOnly: true,
      },
    );
    const count = (sql: string) =>
      Number((database.prepare(sql).get() as { n: number }).n);
    expect(count("SELECT count(*) AS n FROM watcher_fault_decisions")).toBe(
      2_000,
    );
    expect(
      count(
        "SELECT count(*) AS n FROM watcher_journal_revisions WHERE journal = 'fault_decisions'",
      ),
    ).toBe(64);
    database.close();
  }, 120_000);
});
