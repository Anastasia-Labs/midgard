import { setTimeout as pause } from "node:timers/promises";

import { afterEach, describe, expect, it, vi } from "vitest";

import { MAX_OPEN_OBJECTIVES } from "../../src/fault-proofs/fault-proof-objective-table.js";
import { unsafeCreateWatcherFaultProofSupervisorForTest } from "../../src/fault-proofs/fault-proof-supervisor.js";
import {
  closeWatcherJournalDatabase,
  openWatcherJournalDatabase,
} from "../../src/fault-proofs/watcher-journal-database.js";
import { watcherObjectiveScope } from "../../src/fault-proofs/watcher-journal-schema.js";
import { createWatcherOperationsObservability } from "../../src/runtime/operations-observability.js";
import {
  journalDirectory,
  removeJournalDirectories,
  TEST_JOURNAL_KEY,
} from "../support/watcher-journal-fixture.js";

afterEach(removeJournalDirectories);

const DEPLOYMENT = "dd".repeat(32);
const header = (index: number): string => index.toString(16).padStart(56, "0");
const job = (headerHash: string) => ({
  mode: "run" as const,
  category: "doubleSpend" as const,
  headerHash,
  decisionDigest: "cd".repeat(32),
  rollbackGeneration: "0",
  observationRevision: "first",
});

/** A journal holding its cap of open objectives, as a restart finds it. */
const atCapacity = async (): Promise<string> => {
  const journalRoot = await journalDirectory("midgard-journal-capacity");
  openWatcherJournalDatabase({
    journalRoot,
    authenticationKey: TEST_JOURNAL_KEY,
  }).transaction((tx) => {
    for (let index = 0; index < MAX_OPEN_OBJECTIVES; index += 1) {
      const scope = watcherObjectiveScope("doubleSpend", header(index));
      tx.put("fault_proof_objectives", {
        key: scope,
        scope,
        state: "open",
        body: { category: "doubleSpend", headerHash: header(index) },
      });
    }
  });
  closeWatcherJournalDatabase(journalRoot);
  return journalRoot;
};

const observe = (
  supervisor: ReturnType<typeof unsafeCreateWatcherFaultProofSupervisorForTest>,
) =>
  createWatcherOperationsObservability({
    deploymentFingerprint: DEPLOYMENT,
    supervisor,
    launchScopeStatus: () => ({
      installedCategoryCount: 54,
      requiredCategoryCount: 54,
    }),
    retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
    durableProofQueueStatus: () => supervisor.durableQueueStatus(),
    nowMs: () => 100_000n,
    monotonicNowMs: () => 1_000,
    l1FreshnessMaximumAgeMs: 10_000,
  });

describe("watcher journal capacity (L2)", () => {
  it("reports journal_capacity for a cap reached by live rows, refuses only new objectives and stays live", async () => {
    const journalRoot = await atCapacity();
    const run = vi.fn(async () => undefined);
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot,
      deploymentFingerprint: DEPLOYMENT,
      run,
    });
    let failure: unknown;
    void supervisor.done.catch((error: unknown) => {
      failure = error;
    });
    // A new objective past the cap resolves without running or throwing.
    await expect(
      supervisor.unsafeScheduleForTest(job(header(MAX_OPEN_OBJECTIVES))),
    ).resolves.toBeUndefined();
    expect(run).not.toHaveBeenCalled();
    const status = supervisor.status();
    expect(status.phase).toBe("accepting");
    expect(status.journalCapacity).toBe(true);
    const operations = observe(supervisor).api.status();
    expect(operations.readinessReasons).toContain("journal_capacity");
    expect(operations.readiness).toBe("not_ready");
    expect(operations.liveness).toBe("live");

    // An objective the table already holds still runs.
    await supervisor.unsafeScheduleForTest(job(header(0)));
    for (
      let attempt = 0;
      attempt < 400 && run.mock.calls.length === 0;
      attempt++
    )
      await pause(5);
    expect(run).toHaveBeenCalledOnce();
    expect(failure).toBeUndefined();
    await supervisor.close();
  });

  it("reports no capacity condition below the cap", async () => {
    const journalRoot = await journalDirectory("midgard-journal-capacity");
    const run = vi.fn(async () => undefined);
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot,
      deploymentFingerprint: DEPLOYMENT,
      run,
    });
    await supervisor.unsafeScheduleForTest(job(header(0)));
    expect(supervisor.status().journalCapacity).toBe(false);
    expect(observe(supervisor).api.status().readinessReasons).not.toContain(
      "journal_capacity",
    );
    await supervisor.close();
  });
});
