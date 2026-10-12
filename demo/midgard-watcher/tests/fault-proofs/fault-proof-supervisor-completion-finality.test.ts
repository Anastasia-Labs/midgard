import { mkdtemp, rm } from "node:fs/promises";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { createWorkflowActuationPermitController } from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import { listWatcherProofObjectives } from "../../src/fault-proofs/fault-proof-objective-table.js";
import { unsafeCreateWatcherFaultProofSupervisorForTest } from "../../src/fault-proofs/fault-proof-supervisor.js";
import { openWatcherJournalDatabase } from "../../src/fault-proofs/watcher-journal-database.js";
import {
  classifyDoubleSpend,
  deploymentIdentity,
  writeExecution,
} from "../support/completed-proof-journal-fixture.js";
import { storelessProofRetention } from "../support/proof-retention.js";
import { TEST_JOURNAL_KEY } from "../support/watcher-journal-fixture.js";

// The double-spend classifier fixture launches one family, so the installed
// scope is narrowed to that family for this file only.
vi.mock("../../src/fault-proofs/fault-proof-application.js", async (load) => {
  const actual =
    await load<
      typeof import("../../src/fault-proofs/fault-proof-application.js")
    >();
  return {
    ...actual,
    WATCHER_INSTALLED_WORKFLOW_CATEGORIES: Object.freeze(["doubleSpend"]),
  };
});

// A marker write that fails once, as a database error leaves it: the next
// job for the same execution then reaches the cached validation.
const marking = vi.hoisted(() => ({ failures: 0 }));
vi.mock(
  "../../src/fault-proofs/fault-proof-objective-table.js",
  async (load) => {
    const actual =
      await load<
        typeof import("../../src/fault-proofs/fault-proof-objective-table.js")
      >();
    return {
      ...actual,
      completeWatcherProofObjective: (
        ...args: Parameters<typeof actual.completeWatcherProofObjective>
      ) => {
        if (marking.failures > 0) {
          marking.failures -= 1;
          throw new Error("database is unavailable");
        }
        return actual.completeWatcherProofObjective(...args);
      },
    };
  },
);

const directories: string[] = [];
afterEach(async () => {
  marking.failures = 0;
  await Promise.all(
    directories
      .splice(0)
      .map(async (path) => rm(path, { recursive: true, force: true })),
  );
});

const K = storelessProofRetention.securityParameter;

/** A supervisor over a completed execution whose completion check answers
 * applicable at `confirmationDepth`, run for `rounds` observations. */
const complete = async (confirmationDepth: number, rounds: number) => {
  const root = await mkdtemp("/var/tmp/midgard-fault-supervisor-finality-");
  directories.push(root);
  const decision = await classifyDoubleSpend();
  await writeExecution(root, decision, true);
  const released: string[] = [];
  const verifyCompleted = vi.fn(
    async () => ({ kind: "applicable", confirmationDepth }) as const,
  );
  const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
    journalRoot: root,
    deploymentFingerprint: deploymentIdentity.manifestId,
    run: async () => {
      throw new Error("a completed execution never runs");
    },
    unsafeVerifyCompletedForTest: verifyCompleted,
    unsafeProofRetentionForTest: {
      ...storelessProofRetention,
      release: async ({ headerHash }) => void released.push(headerHash),
    },
  });
  const controller = createWorkflowActuationPermitController({
    decision,
    rollbackGeneration: "3",
  });
  const rows = () =>
    listWatcherProofObjectives(
      openWatcherJournalDatabase({
        journalRoot: root,
        authenticationKey: TEST_JOURNAL_KEY,
      }),
      ["doubleSpend"],
    ).map(({ state, marker }) => ({
      state,
      depth: marker?.confirmationDepth ?? null,
    }));
  const states: ReturnType<typeof rows>[] = [];
  for (let round = 0; round < rounds; round += 1) {
    if (round === 0)
      await supervisor.recoverExisting(
        decision,
        controller.permit,
        Object.freeze({
          headerHash: decision.headerHash,
          headerEndTimeMs: "0",
          maturityAtMs: MIDGARD_RETENTION_WINDOW.maturityMs.toString(),
          latestSafeStartAtMs: (
            MIDGARD_RETENTION_WINDOW.maturityMs -
            MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs
          ).toString(),
        }),
        "3",
      );
    else
      await supervisor.unsafeScheduleForTest({
        mode: "resume",
        category: "doubleSpend",
        headerHash: decision.headerHash,
        decisionDigest: decision.decisionDigest,
        rollbackGeneration: "3",
        observationRevision: String(round),
      });
    await vi.waitFor(() => {
      expect(supervisor.status().activeJob).toBeNull();
      expect(supervisor.status().queuedJobCount).toBe(0);
    });
    states.push(rows());
  }
  const status = supervisor.status();
  await supervisor.close();
  return { states, status, released, verifyCompleted };
};

// Depth > k is final (plan section 9), for the completion check and the
// marker alike: one verified at exactly k is not yet applicable.
describe("fault-proof supervisor completion finality", () => {
  it("keeps a completion verified at exactly k open and verifies it again", async () => {
    const outcome = await complete(K, 2);
    expect(outcome.states).toEqual([
      [{ state: "open", depth: null }],
      [{ state: "open", depth: null }],
    ]);
    expect(outcome.verifyCompleted).toHaveBeenCalledTimes(2);
    expect(outcome.released).toEqual([]);
    expect(outcome.status.phase).toBe("accepting");
  }, 60_000);

  it("marks a completion verified at k + 1 and releases its history", async () => {
    const outcome = await complete(K + 1, 2);
    expect(outcome.states).toEqual([
      [{ state: "marked", depth: K + 1 }],
      [{ state: "marked", depth: K + 1 }],
    ]);
    // The second job takes the marker as final without verifying again.
    expect(outcome.verifyCompleted).toHaveBeenCalledOnce();
    expect(outcome.released.length).toBeGreaterThan(0);
  }, 60_000);

  it("marks from the cached validation with its verified depth after a failed marker write", async () => {
    marking.failures = 1;
    const outcome = await complete(K + 1, 2);
    expect(outcome.states).toEqual([
      [{ state: "open", depth: null }],
      [{ state: "marked", depth: K + 1 }],
    ]);
    expect(outcome.verifyCompleted).toHaveBeenCalledOnce();
    expect(outcome.released.length).toBeGreaterThan(0);
  }, 60_000);
});
