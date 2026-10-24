import { mkdir, mkdtemp, readFile, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";
import { DatabaseSync } from "node:sqlite";

import { afterEach, describe, expect, it, vi } from "vitest";

import { unsafeCreateWatcherFaultProofSupervisorForTest } from "../../src/fault-proofs/fault-proof-supervisor.js";
import {
  warnWatcherLegacyJournalDirectories,
  type WatcherLegacyJournalIgnored,
} from "../../src/runtime/legacy-journal-directories.js";
import { createWatcherOperationsObservability } from "../../src/runtime/operations-observability.js";
import { createWatcherRuntime } from "../../src/runtime/watcher-runtime.js";
import { freeOperationsEndpoint } from "../support/free-port.js";
import {
  journalDirectory,
  removeJournalDirectories,
} from "../support/watcher-journal-fixture.js";
import { writeWatcherRuntimeProcessConfig } from "../support/watcher-runtime-process-config.js";

afterEach(removeJournalDirectories);

const DEPLOYMENT = "dd".repeat(32);

describe("legacy file journals (W2-E6)", () => {
  it("warns once for each non-empty legacy directory, ignores them and still reaches readiness", async () => {
    const journalRoot = await journalDirectory("midgard-legacy-journals");
    const legacy = join(journalRoot, "fault-decisions");
    await mkdir(legacy);
    await writeFile(join(legacy, "000001.json"), '{"decision":"legacy"}');
    // An empty legacy directory holds nothing and warns nothing.
    await mkdir(join(journalRoot, "fault-proof-queue-v1"));
    // The per-proof workflow journals of an earlier layout: in-flight
    // proofs, which the error alert names instead.
    const workflows = join(journalRoot, "fault-proofs");
    const execution = join(
      workflows,
      "mintItemNonCanonical",
      "ab".repeat(32),
      "cd".repeat(32),
    );
    await mkdir(execution, { recursive: true });
    await writeFile(join(execution, "00000000.json"), "{}");

    const warnings: WatcherLegacyJournalIgnored[] = [];
    await warnWatcherLegacyJournalDirectories(journalRoot, (warning) =>
      warnings.push(warning),
    );
    expect(warnings).toEqual([
      {
        event: "legacy_journal_ignored",
        path: legacy,
        detail: expect.stringContaining("ignored and never imported"),
      },
    ]);

    // The journals start fresh beside it and the watcher becomes ready.
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot,
      deploymentFingerprint: DEPLOYMENT,
      run: async () => undefined,
    });
    await expect(supervisor.recoverExisting(null)).resolves.toBe(0);
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
    });
    operations.sink.recordL1Source({
      sourceIdentityDigest: "22".repeat(32),
      sourceMode: "local_node",
      status: "consistent",
      blockHash: "33".repeat(32),
      blockNo: "50",
      slot: "500",
      observedAtMs: "100000",
    });
    expect(operations.api.status()).toMatchObject({
      readiness: "ready",
      readinessReasons: [],
      liveness: "live",
    });
    // Ignored, not removed.
    expect(await readFile(join(legacy, "000001.json"), "utf8")).toBe(
      '{"decision":"legacy"}',
    );
    expect(await readFile(join(execution, "00000000.json"), "utf8")).toBe("{}");
    await supervisor.close();
  });

  it("logs one error alert at startup naming the earlier layout's in-flight proof state, and leaves it in place", async () => {
    const directory = await mkdtemp("/var/tmp/midgard-watcher-legacy-proofs-");
    try {
      const config = {
        ...(await writeWatcherRuntimeProcessConfig(directory)),
        operationsEndpoint: await freeOperationsEndpoint(),
      };
      const storagePath = config.watcherConfig.storage.path;
      const old = new DatabaseSync(storagePath);
      old.exec(`
        PRAGMA journal_mode = WAL;
        CREATE TABLE watcher_prover_funding_handoff_v1 (reservation_id TEXT);
        CREATE TABLE watcher_prover_funding_abandonment_v1 (reservation_id TEXT);
        INSERT INTO watcher_prover_funding_handoff_v1 VALUES ('a');
      `);
      old.close();
      const workflows = join(config.workflowJournalDirectory, "fault-proofs");
      const execution = join(workflows, "doubleSpend", "ab".repeat(32));
      await mkdir(execution, { recursive: true, mode: 0o700 });
      await writeFile(join(execution, "00000000.json"), "{}");
      // Startup stops later, at the deployment authority not written yet.
      await rm(config.deploymentAuthorityPath);
      const stderr = vi.spyOn(process.stderr, "write");
      await expect(createWatcherRuntime({ config })).rejects.toThrow();
      const alerts = stderr.mock.calls
        .map(([chunk]) => String(chunk))
        .filter((line) => line.includes('"legacy_proofs_not_carried_over"'))
        .map((line) => JSON.parse(line) as unknown);
      stderr.mockRestore();
      expect(alerts).toEqual([
        expect.objectContaining({
          level: "error",
          // The empty abandonment table holds nothing in flight.
          present: [
            workflows,
            `${storagePath} table watcher_prover_funding_handoff_v1`,
          ],
          detail: expect.stringContaining(
            "in-flight proofs from the old layout are not carried over",
          ),
        }),
      ]);
      expect(await readFile(join(execution, "00000000.json"), "utf8")).toBe(
        "{}",
      );
    } finally {
      await rm(directory, { recursive: true, force: true });
    }
  });
});
