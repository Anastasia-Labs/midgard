import { DatabaseSync } from "node:sqlite";

import type { WatcherFaultProofSupervisor } from "../../src/fault-proofs/fault-proof-supervisor.js";
import { openWatcherJournalDatabase } from "../../src/fault-proofs/watcher-journal-database.js";
import { handleWatcherOperationsHttpRequest } from "../../src/runtime/operations-observability.handle-http-request.js";
import { createWatcherOperationsObservability } from "../../src/runtime/operations-observability.js";
import { deploymentIdentity } from "./fault-proof-funding-fixture.js";
import { TEST_JOURNAL_KEY } from "./watcher-journal-fixture.js";

/**
 * Holds the journals' write lock from a second connection, as another
 * process would: every write the watcher begins waits out its busy timeout
 * and fails with SQLITE_BUSY. Returns the release, which is idempotent.
 */
export const lockWatcherJournals = (journalRoot: string): (() => void) => {
  const { path } = openWatcherJournalDatabase({
    journalRoot,
    authenticationKey: TEST_JOURNAL_KEY,
  });
  const holder = new DatabaseSync(path);
  holder.exec("BEGIN IMMEDIATE");
  let held = true;
  return () => {
    if (!held) return;
    held = false;
    holder.exec("ROLLBACK");
    holder.close();
  };
};

/** `/readyz` of an operations server over `supervisor` alone. */
export const watcherSupervisorReadyz = (
  supervisor: WatcherFaultProofSupervisor,
) => {
  const operations = createWatcherOperationsObservability({
    deploymentFingerprint: deploymentIdentity.manifestId,
    supervisor,
    launchScopeStatus: () => ({
      installedCategoryCount: 54,
      requiredCategoryCount: 54,
    }),
    retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
    durableProofQueueStatus: () => supervisor.durableQueueStatus(),
    nowMs: () => 100_000n,
  });
  return async () => {
    const response = await handleWatcherOperationsHttpRequest(
      new Request("http://127.0.0.1/readyz"),
      operations.api,
    );
    return {
      status: response.status,
      reasons: ((await response.json()) as { reasons: readonly string[] })
        .reasons,
    };
  };
};
