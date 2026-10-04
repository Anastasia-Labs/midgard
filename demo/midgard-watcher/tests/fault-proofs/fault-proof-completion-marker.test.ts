import { mkdtemp, readdir, rm, writeFile } from "node:fs/promises";
import { join } from "node:path";

import { afterEach, describe, expect, it, vi } from "vitest";

import { openWatcherFaultDecisionJournal } from "../../src/fault-proofs/fault-decision-journal.js";
import { unsafeCreateWatcherFaultProofSupervisorForTest } from "../../src/fault-proofs/fault-proof-supervisor.js";
import { WATCHER_ROLLBACK_BOUNDS } from "../../src/l1/rollback-engine/types.js";
import {
  classifyDoubleSpend,
  deploymentIdentity,
  type DoubleSpendDecision,
  writeExecution,
} from "../support/completed-proof-journal-fixture.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";
import { waitForFaultProofSupervisorIdle } from "../support/fault-proof-supervisor-idle.js";

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

const RECOVERY_DEPTH = Number(
  WATCHER_ROLLBACK_BOUNDS.postFinalityRecoveryDepth,
);
const RELEASE_FINALITY_DEPTH = 10;

type Verification =
  | Readonly<{ kind: "applicable"; confirmationDepth: number }>
  | Readonly<{ kind: "pending"; reason: string }>;

const roots: string[] = [];
afterEach(async () => {
  await Promise.all(
    roots.splice(0).map((root) => rm(root, { recursive: true, force: true })),
  );
});

/** A journal root holding `count` completed, signed double-spend proofs. */
const completedProofs = async (count: number) => {
  const root = await mkdtemp("/var/tmp/midgard-fault-completion-marker-");
  roots.push(root);
  const decisions: DoubleSpendDecision[] = [];
  for (let seed = 0; seed < count; seed += 1)
    decisions.push(await classifyDoubleSpend(120 + seed));
  const journal = await openWatcherFaultDecisionJournal({
    directory: root,
    deploymentFingerprint: deploymentIdentity.manifestId,
    launchScope: decisions[0]!.launchScope,
  });
  for (const decision of decisions) {
    await journal.appendLiveDecision(decision);
    await writeExecution(root, decision, true);
  }
  return { root, decisions };
};

const queueRecordCount = async (root: string): Promise<number> =>
  (
    await readdir(join(root, "fault-proof-queue-v1")).catch(
      (error: NodeJS.ErrnoException) => {
        if (error.code === "ENOENT") return [];
        throw error;
      },
    )
  ).length;

const markers = async (root: string): Promise<string[]> =>
  await readdir(join(root, "fault-proof-completions-v1", "doubleSpend")).catch(
    (error: NodeJS.ErrnoException) => {
      if (error.code === "ENOENT") return [];
      throw error;
    },
  );

/** One watcher process lifetime: start, observe once, drain, stop. */
const restart = async (
  root: string,
  verifyCompleted: () => Promise<Verification>,
) => {
  const verify = vi.fn(verifyCompleted);
  const run = vi.fn(async () => {
    throw new Error("a completed proof must never run again");
  });
  const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
    journalRoot: root,
    deploymentFingerprint: deploymentIdentity.manifestId,
    run,
    unsafeVerifyCompletedForTest: verify,
  });
  try {
    await supervisor.requestProgress({
      observation: progressObservation({
        deploymentFingerprint: deploymentIdentity.manifestId,
      }),
      rollbackGeneration: "0",
    });
    await waitForFaultProofSupervisorIdle(supervisor);
    return {
      verifications: verify.mock.calls.length,
      unfinished: supervisor.status().unfinishedObjectiveCount,
    };
  } finally {
    await supervisor.close();
    expect(run).not.toHaveBeenCalled();
  }
};

describe("durable proof completion marker", () => {
  it("keeps the queue journal flat across hundreds of restarts over marked completions", async () => {
    const proofs = 12;
    const { root } = await completedProofs(proofs);
    const beyondRecovery = async () =>
      ({ kind: "applicable", confirmationDepth: RECOVERY_DEPTH + 1 }) as const;
    // The first start verifies every completion once and marks each one.
    await expect(restart(root, beyondRecovery)).resolves.toEqual({
      verifications: proofs,
      unfinished: 0,
    });
    expect(await markers(root)).toHaveLength(proofs);
    const settled = await queueRecordCount(root);
    expect(settled).toBeGreaterThan(0);
    for (let round = 0; round < 300; round += 1)
      await expect(restart(root, beyondRecovery)).resolves.toEqual({
        verifications: 0,
        unfinished: 0,
      });
    expect(await queueRecordCount(root)).toBe(settled);
  }, 300_000);

  it("verifies an unmarked completion again on every restart", async () => {
    const { root } = await completedProofs(2);
    const atRecoveryReach = async () =>
      ({ kind: "applicable", confirmationDepth: RECOVERY_DEPTH }) as const;
    const counts: number[] = [];
    for (let round = 0; round < 3; round += 1) {
      await expect(restart(root, atRecoveryReach)).resolves.toEqual({
        verifications: 2,
        unfinished: 0,
      });
      counts.push(await queueRecordCount(root));
    }
    expect(await markers(root)).toEqual([]);
    // Each re-verification costs journal records, which the marker prevents.
    expect(counts[2]).toBeGreaterThan(counts[1]!);
    expect(counts[1]).toBeGreaterThan(counts[0]!);
  }, 120_000);

  it("leaves a completion inside rollback reach unmarked, so a rollback reopens it", async () => {
    const { root } = await completedProofs(1);
    await expect(
      restart(
        root,
        async () =>
          ({
            kind: "applicable",
            confirmationDepth: RELEASE_FINALITY_DEPTH,
          }) as const,
      ),
    ).resolves.toEqual({ verifications: 1, unfinished: 0 });
    expect(await markers(root)).toEqual([]);
    // A rollback removed the removal transaction: the target is live again.
    await expect(
      restart(
        root,
        async () => ({ kind: "pending", reason: "target_live" }) as const,
      ),
    ).resolves.toEqual({ verifications: 1, unfinished: 1 });
  }, 120_000);

  it("verifies again when the marker no longer binds the completed journal", async () => {
    const { root, decisions } = await completedProofs(1);
    const beyondRecovery = async () =>
      ({ kind: "applicable", confirmationDepth: RECOVERY_DEPTH + 1 }) as const;
    await restart(root, beyondRecovery);
    const [marker] = await markers(root);
    expect(marker).toBe(`${decisions[0]!.headerHash}.json`);
    const path = join(root, "fault-proof-completions-v1", "doubleSpend");
    await writeFile(join(path, marker!), "{ torn");
    await expect(restart(root, beyondRecovery)).resolves.toEqual({
      verifications: 1,
      unfinished: 0,
    });
    // Re-verification rewrote it, so the next start skips the proof again.
    await expect(restart(root, beyondRecovery)).resolves.toEqual({
      verifications: 0,
      unfinished: 0,
    });
  }, 120_000);
});
