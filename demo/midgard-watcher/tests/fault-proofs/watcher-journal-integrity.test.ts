import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";
import { setTimeout as pause } from "node:timers/promises";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { h28 } from "@al-ft/midgard-test-support/hex";
import { afterEach, describe, expect, it } from "vitest";

import { createWatcherFaultProofSupervisor } from "../../src/fault-proofs/fault-proof-supervisor.js";
import {
  closeWatcherJournalDatabase,
  WATCHER_JOURNAL_DATABASE_FILE,
} from "../../src/fault-proofs/watcher-journal-database.js";
import { watcherObjectiveScope } from "../../src/fault-proofs/watcher-journal-schema.js";
import { createWatcherOperationsObservability } from "../../src/runtime/operations-observability.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { storelessProofRetention } from "../support/proof-retention.js";
import {
  journalDirectory,
  recordObjectives,
  recordRawObjectiveRow,
  removeJournalDirectories,
  TEST_JOURNAL_KEY,
} from "../support/watcher-journal-fixture.js";

afterEach(removeJournalDirectories);

const DEPLOYMENT = "dd".repeat(32);

/**
 * Starts the production supervisor over a corrupted journal and drives two
 * progress passes, as the decision driver's retries would. Each pass must
 * fail on the refused journal, and the watcher must stay live and report
 * `journal_integrity` with the failure named, never block or fail `done`.
 */
const expectHeldUnready = async (
  journalRoot: string,
  detail: string,
): Promise<void> => {
  const supervisor = createWatcherFaultProofSupervisor({
    journalRoot,
    deploymentFingerprint: DEPLOYMENT,
    deadlineAlertHeadroomMs: MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs,
    queueAuthenticationKey: TEST_JOURNAL_KEY,
    proofRetention: storelessProofRetention,
    execution: {
      verifyCompleted: async () => {
        throw new Error("unexpected completed execution");
      },
      execute: async () => {
        throw new Error("unexpected execution");
      },
    },
  });
  let failure: unknown;
  void supervisor.done.catch((error: unknown) => {
    failure = error;
  });
  const operations = createWatcherOperationsObservability({
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
  for (let pass = 0; pass < 2; pass += 1)
    await expect(
      supervisor.requestProgress({
        observation: progressObservation({ deploymentFingerprint: DEPLOYMENT }),
        rollbackGeneration: "0",
      }),
    ).rejects.toThrow(detail);
  await pause(10);

  const status = supervisor.status();
  expect(status.phase).toBe("accepting");
  expect(status.journalIntegrity).toContain(detail);
  const reported = operations.api.status();
  expect(reported.readinessReasons).toContain("journal_integrity");
  expect(reported.readiness).toBe("not_ready");
  expect(reported.liveness).toBe("live");
  expect(failure).toBeUndefined();
  await supervisor.close();
};

describe("watcher journal integrity (W2-E2)", () => {
  it("holds the watcher live and unready on an objective row whose MAC differs", async () => {
    const journalRoot = await journalDirectory("midgard-journal-integrity");
    const objective = { category: "doubleSpend" as const, headerHash: h28(1) };
    recordObjectives(journalRoot, [objective]);
    closeWatcherJournalDatabase(journalRoot);
    const key = watcherObjectiveScope(objective.category, objective.headerHash);
    const raw = new DatabaseSync(
      join(journalRoot, WATCHER_JOURNAL_DATABASE_FILE),
    );
    const { mac } = raw
      .prepare(
        "SELECT mac FROM watcher_fault_proof_objectives WHERE row_key = ?",
      )
      .get(key) as { mac: string };
    raw
      .prepare(
        "UPDATE watcher_fault_proof_objectives SET mac = ? WHERE row_key = ?",
      )
      .run(`${mac[0] === "0" ? "1" : "0"}${mac.slice(1)}`, key);
    raw.close();

    await expectHeldUnready(journalRoot, `row ${key} MAC differs`);
  });

  it("holds the watcher live and unready on an authenticated objective body that does not parse", async () => {
    const journalRoot = await journalDirectory("midgard-journal-integrity");
    recordRawObjectiveRow(journalRoot, {
      category: "doubleSpend",
      headerHash: "not-hex",
    });

    await expectHeldUnready(journalRoot, "proof objective row is malformed");
  });

  it("holds the watcher live and unready on an objective row that differs from its key", async () => {
    const journalRoot = await journalDirectory("midgard-journal-integrity");
    recordRawObjectiveRow(
      journalRoot,
      { category: "doubleSpend", headerHash: h28(1) },
      watcherObjectiveScope("doubleSpend", h28(2)),
    );

    await expectHeldUnready(
      journalRoot,
      "proof objective row differs from its key",
    );
  });
});
