import { mkdtemp, rm } from "node:fs/promises";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { createWorkflowActuationPermitController } from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import { unsafeCreateWatcherFaultProofSupervisorForTest } from "../../src/fault-proofs/fault-proof-supervisor.js";
import {
  classifyDoubleSpend,
  deploymentIdentity,
  writeExecution,
} from "../support/completed-proof-journal-fixture.js";

// The production supervisor admits exactly the installed launch scope. The
// double-spend classifier fixture launches one family, so the installed
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

const directories: string[] = [];
afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map(async (path) => rm(path, { recursive: true, force: true })),
  );
});

const recover = async (completed: boolean, rounds = 1) => {
  const root = await mkdtemp("/var/tmp/midgard-fault-supervisor-completed-");
  directories.push(root);
  const decision = await classifyDoubleSpend();
  await writeExecution(root, decision, completed);
  let ran = 0;
  const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
    journalRoot: root,
    deploymentFingerprint: deploymentIdentity.manifestId,
    run: async (job) => {
      ran += 1;
      return completed ? job.mode : { kind: "terminal_included" };
    },
  });
  // A live re-classification of the same header carries a fresh decision
  // digest: the authenticated observation digest differs at every point.
  const controller = createWorkflowActuationPermitController({
    decision,
    rollbackGeneration: "3",
  });
  let recovered = 0;
  for (let round = 0; round < rounds; round += 1) {
    recovered =
      round === 0
        ? await supervisor.recoverExisting(
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
          )
        : (await supervisor.unsafeScheduleForTest({
            mode: "resume",
            category: "doubleSpend",
            headerHash: decision.headerHash,
            decisionDigest: decision.decisionDigest,
            rollbackGeneration: "3",
            observationRevision: String(round),
          }),
          1);
    await vi.waitFor(() => {
      expect(supervisor.status().activeJob).toBeNull();
      expect(supervisor.status().queuedJobCount).toBe(0);
    });
  }
  await supervisor.close();
  return { recovered, ran, status: supervisor.status() };
};

describe("fault-proof supervisor over a completed execution", () => {
  it("does not start a new execution for a header whose journal already completed", async () => {
    const outcome = await recover(true);
    expect(outcome.recovered).toBe(1);
    expect(outcome.ran).toBe(0);
    expect(outcome.status).toMatchObject({
      phase: "closed",
      queuedJobCount: 0,
    });
  }, 60_000);

  it("reconciles a provisionally finished execution again on later observations", async () => {
    const outcome = await recover(false, 2);
    expect(outcome.ran).toBe(2);
    expect(outcome.status.queuedJobCount).toBe(0);
  }, 60_000);

  it("reopens a yielded prepared objective only when a later observation schedules it", async () => {
    const root = await mkdtemp("/var/tmp/midgard-fault-supervisor-yielded-");
    directories.push(root);
    const decision = await classifyDoubleSpend();
    await writeExecution(root, decision, false, true);
    const run = vi.fn(async (_job: { mode: "run" | "resume" }) => ({
      kind: "pending",
      resumeOnObservation: true,
    }));
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: root,
      deploymentFingerprint: deploymentIdentity.manifestId,
      run,
    });
    const controller = createWorkflowActuationPermitController({
      decision,
      rollbackGeneration: "3",
    });
    const observe = async () =>
      await supervisor.recoverExisting(
        decision,
        controller.permit,
        {
          headerHash: decision.headerHash,
          headerEndTimeMs: "0",
          maturityAtMs: MIDGARD_RETENTION_WINDOW.maturityMs.toString(),
          latestSafeStartAtMs: (
            MIDGARD_RETENTION_WINDOW.maturityMs -
            MIDGARD_RETENTION_WINDOW.worstCaseProofTimeBoundMs
          ).toString(),
        },
        "3",
      );
    try {
      await observe();
      await vi.waitFor(() => {
        expect(run).toHaveBeenCalledTimes(1);
        expect(supervisor.status().activeJob).toBeNull();
        expect(supervisor.status().queuedJobCount).toBe(0);
      });
      // Draining runnable work does not schedule another invocation by itself.
      await new Promise<void>((resolve) => setImmediate(resolve));
      expect(run).toHaveBeenCalledTimes(1);
      await observe();
      // Repeating the same canonical observation cannot create a busy loop.
      expect(run).toHaveBeenCalledTimes(1);
      await supervisor.unsafeScheduleForTest({
        mode: "resume",
        category: "doubleSpend",
        headerHash: decision.headerHash,
        decisionDigest: decision.decisionDigest,
        rollbackGeneration: "3",
        observationRevision: "next-canonical-observation",
      });
      await vi.waitFor(() => {
        expect(run).toHaveBeenCalledTimes(2);
        expect(supervisor.status().activeJob).toBeNull();
        expect(supervisor.status().queuedJobCount).toBe(0);
      });
      expect(supervisor.status().phase).toBe("accepting");
      expect(run.mock.calls.map(([job]) => job.mode)).toEqual([
        "resume",
        "resume",
      ]);
    } finally {
      await supervisor.close();
    }
  }, 60_000);

  it("still resumes an execution that has not completed", async () => {
    const outcome = await recover(false);
    expect(outcome.recovered).toBe(1);
    expect(outcome.ran).toBe(1);
  }, 60_000);
});
