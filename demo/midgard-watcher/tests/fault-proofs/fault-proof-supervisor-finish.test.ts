import { mkdtemp, rm } from "node:fs/promises";
import { setImmediate } from "node:timers/promises";

import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { unsafeCreateWatcherFaultProofSupervisorForTest } from "../../src/fault-proofs/fault-proof-supervisor.js";

const journalControl = vi.hoisted(() => ({
  beforeFinish: async (): Promise<void> => undefined,
  registerCalls: 0,
}));

vi.mock("../../src/fault-proofs/fault-proof-queue-journal.js", async (load) => {
  const actual =
    await load<
      typeof import("../../src/fault-proofs/fault-proof-queue-journal.js")
    >();
  return {
    ...actual,
    openWatcherFaultProofQueueJournal: async (
      input: Parameters<typeof actual.openWatcherFaultProofQueueJournal>[0],
    ) => {
      const journal = await actual.openWatcherFaultProofQueueJournal(input);
      return {
        ...journal,
        register: async (...args: Parameters<typeof journal.register>) => {
          journalControl.registerCalls += 1;
          return await journal.register(...args);
        },
        markFinished: async (
          ...args: Parameters<typeof journal.markFinished>
        ) => {
          await journalControl.beforeFinish();
          await journal.markFinished(...args);
        },
      };
    },
  };
});

const directories: string[] = [];
const job = Object.freeze({
  mode: "run" as const,
  category: "doubleSpend" as const,
  headerHash: "11".repeat(28),
  decisionDigest: "22".repeat(32),
  rollbackGeneration: "0",
});

const deferred = () => {
  let resolve!: () => void;
  let reject!: (error: Error) => void;
  const promise = new Promise<void>((resolvePromise, rejectPromise) => {
    resolve = resolvePromise;
    reject = rejectPromise;
  });
  return { promise, resolve, reject };
};

const fixture = async () => {
  const journalRoot = await mkdtemp("/var/tmp/watcher-supervisor-finish-");
  directories.push(journalRoot);
  const entered = deferred();
  const release = deferred();
  journalControl.beforeFinish = async () => {
    entered.resolve();
    await release.promise;
  };
  const run = vi.fn(async () => "completed workflow");
  const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
    journalRoot,
    deploymentFingerprint: "dd".repeat(32),
    run,
  });
  await supervisor.recoverExisting(null);
  return { supervisor, run, entered, release };
};

beforeEach(() => {
  journalControl.beforeFinish = async () => undefined;
  journalControl.registerCalls = 0;
});

afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map((directory) => rm(directory, { recursive: true, force: true })),
  );
});

describe("fault-proof supervisor durable completion", () => {
  it("keeps completion pending until the finished record is durable", async () => {
    const { supervisor, entered, release } = await fixture();
    let settled = false;
    const completion = supervisor.unsafeRunOrResumeForTest(job);
    void completion.then(
      () => {
        settled = true;
      },
      () => {
        settled = true;
      },
    );
    try {
      await entered.promise;
      // Drain promise callbacks after entering the controlled finish write.
      await setImmediate();
      expect(settled).toBe(false);
      release.resolve();
      await expect(completion).resolves.toBe("completed workflow");
    } finally {
      release.resolve();
      await supervisor.close();
    }
  });

  it("deduplicates the same job while its finished record is pending", async () => {
    const { supervisor, run, entered, release } = await fixture();
    const completion = supervisor.unsafeRunOrResumeForTest(job);
    try {
      await entered.promise;
      await setImmediate();
      await supervisor.unsafeScheduleForTest({ ...job, mode: "resume" });
      const registrationsWhileFinishing = journalControl.registerCalls;
      release.resolve();
      await completion;
      await supervisor.close();
      expect({
        registrationsWhileFinishing,
        executions: run.mock.calls.length,
      }).toEqual({ registrationsWhileFinishing: 1, executions: 1 });
    } finally {
      release.resolve();
      await supervisor.close();
    }
  });

  it("rejects completion when the finished record cannot be persisted", async () => {
    const { supervisor, entered, release } = await fixture();
    const failure = new Error("controlled finished-record write failure");
    const completion = supervisor.unsafeRunOrResumeForTest(job).then(
      (value) => ({ kind: "resolved", value }),
      (error: unknown) => ({ kind: "rejected", error }),
    );
    try {
      await entered.promise;
      release.reject(failure);
      await expect(supervisor.done).rejects.toBe(failure);
      expect(await completion).toEqual({ kind: "rejected", error: failure });
      expect(supervisor.status().phase).toBe("blocked");
    } finally {
      release.resolve();
      await supervisor.close();
    }
  });
});
