import { mkdir, readFile, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { afterEach, describe, expect, it } from "vitest";

import { unsafeCreateWatcherFaultProofSupervisorForTest } from "../../src/fault-proofs/fault-proof-supervisor.js";
import {
  warnWatcherLegacyJournalDirectories,
  type WatcherLegacyJournalIgnored,
} from "../../src/runtime/legacy-journal-directories.js";
import { createWatcherOperationsObservability } from "../../src/runtime/operations-observability.js";
import {
  journalDirectory,
  removeJournalDirectories,
} from "../support/watcher-journal-fixture.js";

afterEach(removeJournalDirectories);

const DEPLOYMENT = "dd".repeat(32);

describe("legacy file journals (W2-E6)", () => {
  it("warns once for a non-empty fault-decisions directory, ignores it and still reaches readiness", async () => {
    const journalRoot = await journalDirectory("midgard-legacy-journals");
    const legacy = join(journalRoot, "fault-decisions");
    await mkdir(legacy);
    await writeFile(join(legacy, "000001.json"), '{"decision":"legacy"}');
    // An empty legacy directory holds nothing and warns nothing.
    await mkdir(join(journalRoot, "fault-proof-queue-v1"));

    const warnings: WatcherLegacyJournalIgnored[] = [];
    await warnWatcherLegacyJournalDirectories(journalRoot, (warning) =>
      warnings.push(warning),
    );
    expect(warnings).toEqual([
      {
        event: "legacy_journal_ignored",
        path: legacy,
        detail: expect.stringContaining("ignored, never imported"),
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
    await supervisor.close();
  });
});
