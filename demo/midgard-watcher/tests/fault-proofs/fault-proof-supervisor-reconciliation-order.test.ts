import { mkdtemp, rm } from "node:fs/promises";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { h28 } from "@al-ft/midgard-test-support/hex";
import { afterEach, describe, expect, it, vi } from "vitest";

import { openWatcherFaultDecisionJournal } from "../../src/fault-proofs/fault-decision-journal.js";
import { unsafeCreateWatcherFaultProofSupervisorForTest } from "../../src/fault-proofs/fault-proof-supervisor.js";
import {
  classifyDoubleSpend,
  deploymentIdentity,
  writeExecution,
} from "../support/completed-proof-journal-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { waitForFaultProofSupervisorIdle } from "../support/fault-proof-supervisor-idle.js";
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

const latestSafeStartOffsetMs =
  MIDGARD_RETENTION_WINDOW.maturityMs -
  MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs;
const deadline = (headerHash: string, headerEndTimeMs: number) =>
  Object.freeze({
    headerHash,
    headerEndTimeMs: headerEndTimeMs.toString(),
    maturityAtMs: (
      headerEndTimeMs + MIDGARD_RETENTION_WINDOW.maturityMs
    ).toString(),
    latestSafeStartAtMs: (headerEndTimeMs + latestSafeStartOffsetMs).toString(),
  });

const roots: string[] = [];
afterEach(async () => {
  await Promise.all(
    roots.splice(0).map((root) => rm(root, { recursive: true, force: true })),
  );
});

describe("fault-proof supervisor queue order", () => {
  it("runs deadline work before deadline-free reconciliation and reports its deadline", async () => {
    const root = await mkdtemp("/var/tmp/midgard-fault-supervisor-order-");
    roots.push(root);
    const reconciled = await classifyDoubleSpend(140);
    const journal = await openWatcherFaultDecisionJournal({
      directory: root,
      deploymentFingerprint: deploymentIdentity.manifestId,
      launchScope: reconciled.launchScope,
      authenticationKey: TEST_JOURNAL_KEY,
    });
    await journal.appendLiveDecision(reconciled);
    await writeExecution(root, reconciled, true);
    const order: string[] = [];
    let releaseActive!: () => void;
    const active = new Promise<void>((resolve) => (releaseActive = resolve));
    let nowMs = 1_000;
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: root,
      deploymentFingerprint: deploymentIdentity.manifestId,
      deadlineAlertHeadroomMs: 1_000,
      unsafeNowMsForTest: () => nowMs,
      run: async (job) => {
        order.push(job.headerHash);
        if (job.headerHash === h28(0x91)) await active;
      },
      unsafeVerifyCompletedForTest: async ({ job }) => {
        order.push(`verify:${job.headerHash}`);
        return { kind: "applicable", confirmationDepth: 1 };
      },
    });
    try {
      const running = supervisor.unsafeRunOrResumeForTest({
        mode: "run",
        category: "doubleSpend",
        headerHash: h28(0x91),
        decisionDigest: "91".repeat(32),
        rollbackGeneration: "0",
        deadline: deadline(h28(0x91), 50_000),
      });
      await vi.waitFor(() =>
        expect(supervisor.status().activeJob?.headerHash).toBe(h28(0x91)),
      );
      // Restart reconciliation of a signed journal arrives first, without a
      // deadline; live deadline work for another header arrives behind it.
      await supervisor.requestProgress({
        observation: progressObservation({
          deploymentFingerprint: deploymentIdentity.manifestId,
        }),
        rollbackGeneration: "0",
      });
      const queued = supervisor.unsafeRunOrResumeForTest({
        mode: "run",
        category: "doubleSpend",
        headerHash: h28(0x92),
        decisionDigest: "92".repeat(32),
        rollbackGeneration: "0",
        deadline: deadline(h28(0x92), 20_000),
      });
      await vi.waitFor(() =>
        expect(supervisor.status().queuedJobCount).toBe(2),
      );
      nowMs = latestSafeStartOffsetMs + 19_500;
      expect(supervisor.status()).toMatchObject({
        deadlineHealth: "at_risk",
        earliestDeadlineJob: { headerHash: h28(0x92) },
        remainingSafeStartMs: "500",
      });
      nowMs = 1_000;
      releaseActive();
      await Promise.all([running, queued]);
      await waitForFaultProofSupervisorIdle(supervisor);
      expect(order).toEqual([
        h28(0x91),
        h28(0x92),
        `verify:${reconciled.headerHash}`,
      ]);
    } finally {
      releaseActive();
      await supervisor.close();
    }
  }, 60_000);
});
