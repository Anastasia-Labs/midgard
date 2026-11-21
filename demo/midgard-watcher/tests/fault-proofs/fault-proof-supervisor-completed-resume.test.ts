import { mkdtemp, rm } from "node:fs/promises";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { createWorkflowActuationPermitController } from "@al-ft/midgard-fault-proofs/workflow/actuation-permit";
import { afterEach, describe, expect, it, vi } from "vitest";

import { unsafeCreateWatcherFaultProofSupervisorForTest } from "../../src/fault-proofs/fault-proof-supervisor.create-watcher-fault-proof-supervisor.js";
import {
  classifiedDoubleSpend,
  deploymentIdentity,
} from "../support/classified-fault-fixture.js";
import { recordObjectives } from "../support/watcher-journal-fixture.js";

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

describe("fault-proof supervisor over a yielded objective", () => {
  it("reopens a yielded objective only when a later observation schedules it", async () => {
    const root = await mkdtemp("/var/tmp/midgard-fault-supervisor-yielded-");
    directories.push(root);
    const { old: decision } = await classifiedDoubleSpend();
    recordObjectives(root, [
      { category: "doubleSpend", headerHash: decision.headerHash },
    ]);
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
          headerBlockTimeMs: "0",
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
      // Nothing of it is in flight in this process, so each invocation
      // starts from what the chain shows.
      expect(run.mock.calls.map(([job]) => job.mode)).toEqual(["run", "run"]);
    } finally {
      await supervisor.close();
    }
  }, 60_000);
});
