/**
 * The supervisor's pump runs one own-thread sweep pass for the admitted
 * observations since its last pass, only after the jobs they queued, and a
 * sweep failure never blocks the supervisor.
 */
import { mkdtemp, rm } from "node:fs/promises";
import { setImmediate } from "node:timers/promises";

import { afterEach, describe, expect, it } from "vitest";

import { unsafeCreateWatcherFaultProofSupervisorForTest } from "../../src/fault-proofs/fault-proof-supervisor.create-watcher-fault-proof-supervisor.js";
import { progressObservation } from "../support/fault-proof-progress-observation.js";

const DEPLOYMENT_FINGERPRINT = "dd".repeat(32);
const directories: string[] = [];

const deferred = () => {
  let resolve!: () => void;
  const promise = new Promise<void>((resolvePromise) => {
    resolve = resolvePromise;
  });
  return { promise, resolve };
};

const settle = async (): Promise<void> => {
  for (let i = 0; i < 20; i += 1) await setImmediate();
};

const supervisorWith = async (
  input: Readonly<{
    sweep: () => Promise<void>;
    run?: () => Promise<unknown>;
  }>,
) => {
  const journalRoot = await mkdtemp("/var/tmp/watcher-supervisor-sweep-");
  directories.push(journalRoot);
  const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
    journalRoot,
    deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
    run: input.run ?? (async () => "completed workflow"),
    unsafeOwnThreadSweepForTest: input.sweep,
  });
  await supervisor.recoverExisting(null);
  return supervisor;
};

const observe = (revision: number) => ({
  observation: progressObservation({
    deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
    revision,
  }),
  rollbackGeneration: "0",
});

afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map((directory) => rm(directory, { recursive: true, force: true })),
  );
});

describe("fault-proof supervisor: the own-thread sweep", () => {
  it("runs one pass after each admitted observation", async () => {
    let passes = 0;
    const supervisor = await supervisorWith({
      sweep: async () => {
        passes += 1;
      },
    });
    try {
      await supervisor.requestProgress(observe(1));
      await settle();
      expect(passes).toBe(1);
      await supervisor.requestProgress(observe(2));
      await settle();
      expect(passes).toBe(2);
    } finally {
      await supervisor.close();
    }
  });

  it("runs only after the queued jobs, and one pass for observations met meanwhile", async () => {
    const order: string[] = [];
    const release = deferred();
    const supervisor = await supervisorWith({
      sweep: async () => {
        order.push("sweep");
      },
      run: async () => {
        order.push("job");
        await release.promise;
        order.push("job done");
        return "completed workflow";
      },
    });
    try {
      const completion = supervisor.unsafeRunOrResumeForTest({
        mode: "run",
        category: "doubleSpend",
        headerHash: "11".repeat(28),
        decisionDigest: "22".repeat(32),
        rollbackGeneration: "0",
      });
      await settle();
      await supervisor.requestProgress(observe(1));
      await supervisor.requestProgress(observe(2));
      await settle();
      expect(order).toEqual(["job"]);
      release.resolve();
      await completion;
      await settle();
      expect(order).toEqual(["job", "job done", "sweep"]);
    } finally {
      release.resolve();
      await supervisor.close();
    }
  });

  it("keeps accepting when a pass fails", async () => {
    let passes = 0;
    const supervisor = await supervisorWith({
      sweep: async () => {
        passes += 1;
        throw new Error("controlled sweep failure");
      },
    });
    try {
      await supervisor.requestProgress(observe(1));
      await settle();
      await supervisor.requestProgress(observe(2));
      await settle();
      expect(passes).toBe(2);
      expect(supervisor.status().phase).toBe("accepting");
    } finally {
      await supervisor.close();
    }
  });
});
