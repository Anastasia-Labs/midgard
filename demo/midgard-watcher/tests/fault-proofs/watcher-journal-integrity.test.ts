import { mkdirSync, rmdirSync, rmSync, writeFileSync } from "node:fs";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";
import { setTimeout as pause } from "node:timers/promises";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { h28 } from "@al-ft/midgard-test-support/hex";
import { afterEach, describe, expect, it } from "vitest";

import { createWatcherFaultProofSupervisor } from "../../src/fault-proofs/fault-proof-supervisor.js";
import {
  closeWatcherJournalDatabase,
  openWatcherJournalDatabase,
  WATCHER_JOURNAL_DATABASE_FILE,
} from "../../src/fault-proofs/watcher-journal-database.js";
import { watcherObjectiveScope } from "../../src/fault-proofs/watcher-journal-schema.js";
import { handleWatcherOperationsHttpRequest } from "../../src/runtime/operations-observability.handle-http-request.js";
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

/** The fields of an operations response these probes read. */
type ProbeBody = {
  ready?: boolean;
  reasons?: string[];
  error?: string;
  supervisor?: { journalIntegrity: string | null };
};

afterEach(removeJournalDirectories);

const DEPLOYMENT = "dd".repeat(32);

const startSupervisor = (journalRoot: string) => {
  const supervisor = createWatcherFaultProofSupervisor({
    reservationDecisionHolds: () => [],
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
  const get = async (path: string) => {
    const response = await handleWatcherOperationsHttpRequest(
      new Request(`http://127.0.0.1${path}`),
      operations.api,
    );
    return {
      status: response.status,
      body: (await response.json()) as ProbeBody,
    };
  };
  return { supervisor, operations, get, failure: () => failure };
};

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
  const { supervisor, operations, get, failure } = startSupervisor(journalRoot);
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
  expect(failure()).toBeUndefined();
  // Probes see the refusal as the watcher's state, never a bad request.
  expect(await get("/readyz")).toMatchObject({
    status: 503,
    body: {
      ready: false,
      reasons: expect.arrayContaining(["journal_integrity"]),
    },
  });
  expect(await get("/v1/metrics")).toEqual({
    status: 503,
    body: { error: "journal_integrity" },
  });
  const live = await get("/v1/status");
  expect(live.status).toBe(200);
  expect(live.body.supervisor?.journalIntegrity).toContain(detail);
  await supervisor.close();
};

describe("watcher journal integrity (W2-E2)", () => {
  it("holds the watcher live and unready on a journal file that is not a database", async () => {
    const journalRoot = await journalDirectory("midgard-journal-integrity");
    writeFileSync(
      join(journalRoot, WATCHER_JOURNAL_DATABASE_FILE),
      Buffer.alloc(8_192, 0x5a),
    );
    await expectHeldUnready(journalRoot, "file is not a database");
  });

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

describe("watcher journal open failures (R6)", () => {
  it("holds the watcher unready by name on a migration that changed after it was applied", async () => {
    const journalRoot = await journalDirectory("midgard-journal-migration");
    openWatcherJournalDatabase({
      journalRoot,
      authenticationKey: TEST_JOURNAL_KEY,
    });
    closeWatcherJournalDatabase(journalRoot);
    const database = new DatabaseSync(
      join(journalRoot, WATCHER_JOURNAL_DATABASE_FILE),
    );
    database.exec(
      "UPDATE watcher_journal_migrations SET checksum = 'stale' WHERE rowid = (SELECT MIN(rowid) FROM watcher_journal_migrations)",
    );
    database.close();
    await expectHeldUnready(journalRoot, "changed after it was applied");
  });

  it("reports journal_unavailable while the journals cannot be opened and recovers on the next use, without a restart", async () => {
    const journalRoot = await journalDirectory("midgard-journal-unavailable");
    // A directory where the database file belongs: SQLite cannot open it.
    const path = join(journalRoot, WATCHER_JOURNAL_DATABASE_FILE);
    mkdirSync(path);
    const { supervisor, operations, get, failure } =
      startSupervisor(journalRoot);
    await expect(
      supervisor.requestProgress({
        observation: progressObservation({ deploymentFingerprint: DEPLOYMENT }),
        rollbackGeneration: "0",
      }),
    ).rejects.toThrow("watcher journals are unavailable");
    await pause(10);
    expect(supervisor.status()).toMatchObject({
      phase: "accepting",
      journalIntegrity: null,
      journalUnavailable: expect.stringContaining(
        "unable to open database file",
      ),
    });
    expect(operations.api.status().readinessReasons).toContain(
      "journal_unavailable",
    );
    expect(await get("/v1/metrics")).toEqual({
      status: 503,
      body: { error: "journal_unavailable" },
    });

    // The cause clears. SQLite could not open the file, which no wait
    // fixes, so no timer reopens it (the first would run after 1 s); the
    // next use does, and recovers the journals in this same process.
    rmdirSync(path);
    await pause(1_500);
    expect(supervisor.status().journalUnavailable).toEqual(
      expect.stringContaining("unable to open database file"),
    );
    // A status read is a use: it opens the queue journal again.
    const opened = () => {
      try {
        supervisor.durableQueueStatus();
        return true;
      } catch {
        return false;
      }
    };
    for (let waited = 0; !opened() && waited < 5_000; waited += 50)
      await pause(50);
    expect(supervisor.status()).toMatchObject({
      phase: "accepting",
      journalIntegrity: null,
      journalUnavailable: null,
    });
    expect(operations.api.status().readinessReasons).not.toContain(
      "journal_unavailable",
    );
    expect(supervisor.durableQueueStatus()).toEqual({
      queuedJobCount: 0,
      oldestQueuedAtMs: null,
    });
    expect(failure()).toBeUndefined();
    await supervisor.close();
  });

  it("keeps an integrity failure latched: no retry clears it", async () => {
    const journalRoot = await journalDirectory("midgard-journal-integrity");
    const path = join(journalRoot, WATCHER_JOURNAL_DATABASE_FILE);
    writeFileSync(path, Buffer.alloc(8_192, 0x5a));
    const { supervisor, failure } = startSupervisor(journalRoot);
    await pause(10);
    expect(supervisor.status().journalIntegrity).toContain(
      "file is not a database",
    );
    // Even a repaired file stays refused until the process restarts.
    rmSync(path);
    await expect(
      supervisor.requestProgress({
        observation: progressObservation({ deploymentFingerprint: DEPLOYMENT }),
        rollbackGeneration: "0",
      }),
    ).rejects.toThrow("file is not a database");
    expect(supervisor.status()).toMatchObject({
      phase: "accepting",
      journalIntegrity: expect.stringContaining("file is not a database"),
      journalUnavailable: null,
    });
    expect(failure()).toBeUndefined();
    await supervisor.close();
  });
});
