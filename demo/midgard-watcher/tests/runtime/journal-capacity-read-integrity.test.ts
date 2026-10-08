import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { afterEach, describe, expect, it, vi } from "vitest";

import { createWatcherFaultProofSupervisor } from "../../src/fault-proofs/fault-proof-supervisor.js";
import { handleWatcherOperationsHttpRequest } from "../../src/runtime/operations-observability.handle-http-request.js";
import { createWatcherOperationsObservability } from "../../src/runtime/operations-observability.js";
import { storelessProofRetention } from "../support/proof-retention.js";
import {
  journalDirectory,
  removeJournalDirectories,
  TEST_JOURNAL_KEY,
} from "../support/watcher-journal-fixture.js";

// The capacity read is the first to meet a damaged journal: it refuses the
// journals, as a SQLite corruption code met by its count would.
const capacityRead = vi.hoisted(() => ({ damaged: false }));
vi.mock(
  "../../src/fault-proofs/fault-proof-objective-table.js",
  async (importOriginal) => {
    const original =
      await importOriginal<
        typeof import("../../src/fault-proofs/fault-proof-objective-table.js")
      >();
    return {
      ...original,
      watcherJournalCapacityReached: (
        database: Parameters<typeof original.watcherJournalCapacityReached>[0],
      ) =>
        capacityRead.damaged
          ? database.refuse(
              "fault_proof_objectives",
              "database disk image is malformed",
            )
          : original.watcherJournalCapacityReached(database),
    };
  },
);

afterEach(removeJournalDirectories);

const DEPLOYMENT = "dd".repeat(32);

describe("readiness over a capacity read that meets corruption (R7)", () => {
  it("answers /readyz 503 journal_integrity, never 400", async () => {
    const journalRoot = await journalDirectory("midgard-journal-capacity");
    const supervisor = createWatcherFaultProofSupervisor({
      journalRoot,
      deploymentFingerprint: DEPLOYMENT,
      deadlineAlertHeadroomMs:
        MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs,
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
    const readyz = async () => {
      const response = await handleWatcherOperationsHttpRequest(
        new Request("http://127.0.0.1/readyz"),
        operations.api,
      );
      return { status: response.status, body: await response.json() };
    };
    // The queue journal opens at construction; the capacity read needs it.
    await vi.waitFor(() => supervisor.durableQueueStatus());
    expect(await readyz()).toMatchObject({ status: 503 }); // no L1 source yet
    expect(supervisor.status().journalIntegrity).toBeNull();

    capacityRead.damaged = true;
    expect(await readyz()).toMatchObject({
      status: 503,
      body: {
        ready: false,
        reasons: expect.arrayContaining(["journal_integrity"]),
      },
    });
    expect(supervisor.status()).toMatchObject({
      phase: "accepting",
      journalIntegrity: expect.stringContaining(
        "database disk image is malformed",
      ),
      journalCapacity: false,
    });
    await supervisor.close();
  });
});
