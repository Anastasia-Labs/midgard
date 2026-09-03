import { mkdir, mkdtemp, rm, symlink } from "node:fs/promises";
import { join } from "node:path";

import type { WorkflowActuationRevokedError } from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it } from "vitest";

import {
  createWatcherFaultProofSupervisor,
  unsafeCreateWatcherFaultProofSupervisorForTest,
  type WatcherFaultProofJob,
} from "../../src/fault-proofs/production-fault-proof-supervisor-v1.js";

const directories: string[] = [];
const h28 = (byte: string): string => byte.repeat(28);
const DEPLOYMENT_FINGERPRINT = "dd".repeat(32);
const deadline = (headerHash: string, headerEndTimeMs: number) =>
  Object.freeze({
    headerHash,
    headerEndTimeMs: headerEndTimeMs.toString(),
    maturityAtMs: (headerEndTimeMs + 604_800_000).toString(),
    latestSafeStartAtMs: (headerEndTimeMs + 302_400_000).toString(),
  });

const directory = async (): Promise<string> => {
  const path = await mkdtemp("/var/tmp/midgard-fault-supervisor-");
  directories.push(path);
  return path;
};

const deferred = <T>() => {
  let resolve!: (value: T) => void;
  let reject!: (reason: Error) => void;
  const promise = new Promise<T>((resolvePromise, rejectPromise) => {
    resolve = resolvePromise;
    reject = rejectPromise;
  });
  return { promise, resolve, reject };
};

const waitUntil = async (predicate: () => boolean): Promise<void> => {
  for (let attempt = 0; attempt < 200; attempt += 1) {
    if (predicate()) return;
    await new Promise((resolve) => setTimeout(resolve, 5));
  }
  throw new Error("test condition was not reached");
};

afterEach(async () => {
  await Promise.all(
    directories
      .splice(0)
      .map(async (path) => rm(path, { force: true, recursive: true })),
  );
});

describe("production fault-proof supervisor", () => {
  it("does not expose caller-selected category dispatch in production", async () => {
    const root = await directory();
    const supervisor = createWatcherFaultProofSupervisor({
      journalRoot: root,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      deadlineAlertHeadroomMs: 3_600_000,
      queueAuthenticationKey: Uint8Array.from({ length: 32 }, () => 0xa5),
      run: async () => undefined,
    });
    expect(Object.keys(supervisor).sort()).toEqual([
      "close",
      "done",
      "durableQueueStatus",
      "recoverExisting",
      "schemaVersion",
      "status",
    ]);
    await supervisor.recoverExisting(null);
    await supervisor.close();
  });

  it("recovers canonical journals in installed category and header order", async () => {
    const root = await directory();
    await Promise.all([
      mkdir(join(root, "fault-proofs", "networkId", h28("bb")), {
        recursive: true,
      }),
      mkdir(join(root, "fault-proofs", "doubleSpend", h28("cc")), {
        recursive: true,
      }),
      mkdir(join(root, "fault-proofs", "doubleSpend", h28("aa")), {
        recursive: true,
      }),
    ]);
    const observed: string[] = [];
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: root,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      run: async (job) => {
        observed.push(`${job.mode}:${job.category}:${job.headerHash}`);
        return job.headerHash;
      },
    });

    await expect(supervisor.recoverExisting(null)).resolves.toBe(3);
    await supervisor.close();

    expect(observed).toEqual([
      `resume:doubleSpend:${h28("aa")}`,
      `resume:doubleSpend:${h28("cc")}`,
      `resume:networkId:${h28("bb")}`,
    ]);
    expect(supervisor.status()).toMatchObject({
      phase: "closed",
      recovered: true,
      queuedJobCount: 0,
    });
  });

  it("rejects unknown, malformed, and symlinked journal targets", async () => {
    const unknownRoot = await directory();
    await mkdir(join(unknownRoot, "fault-proofs", "forgedFamily"), {
      recursive: true,
    });
    const unknown = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: unknownRoot,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      run: async () => undefined,
    });
    await expect(unknown.recoverExisting(null)).rejects.toThrow(
      "unknown category forgedFamily",
    );

    const malformedRoot = await directory();
    await mkdir(join(malformedRoot, "fault-proofs", "doubleSpend", h28("aa")), {
      recursive: true,
    });
    await mkdir(join(malformedRoot, "fault-proofs", "doubleSpend", "not-hex"));
    const malformed = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: malformedRoot,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      run: async () => undefined,
    });
    await expect(malformed.recoverExisting(null)).rejects.toThrow(
      "invalid doubleSpend target not-hex",
    );

    const symlinkRoot = await directory();
    const outside = await directory();
    await mkdir(join(symlinkRoot, "fault-proofs", "doubleSpend"), {
      recursive: true,
    });
    await symlink(
      outside,
      join(symlinkRoot, "fault-proofs", "doubleSpend", h28("bb")),
    );
    const linked = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: symlinkRoot,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      run: async () => undefined,
    });
    await expect(linked.recoverExisting(null)).rejects.toThrow(
      `invalid doubleSpend target ${h28("bb")}`,
    );
  });

  it("deduplicates only an exact decision generation and serializes replacements", async () => {
    const root = await directory();
    const first = deferred<string>();
    const starts: WatcherFaultProofJob[] = [];
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: root,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      run: async (job) => {
        starts.push(job);
        return starts.length === 1 ? await first.promise : job.headerHash;
      },
    });
    await supervisor.recoverExisting(null);
    const firstJob = {
      mode: "run" as const,
      category: "doubleSpend" as const,
      headerHash: h28("11"),
      decisionDigest: "11".repeat(32),
      rollbackGeneration: "0",
    };
    const firstRun = supervisor.unsafeRunOrResumeForTest(firstJob);
    const duplicate = supervisor.unsafeRunOrResumeForTest({
      ...firstJob,
      mode: "resume",
    });
    const replacement = supervisor.unsafeRunOrResumeForTest({
      ...firstJob,
      decisionDigest: "12".repeat(32),
    });
    const generationReplacement = supervisor.unsafeRunOrResumeForTest({
      ...firstJob,
      rollbackGeneration: "1",
    });
    const secondRun = supervisor.unsafeRunOrResumeForTest({
      mode: "run",
      category: "networkId",
      headerHash: h28("22"),
      decisionDigest: "22".repeat(32),
      rollbackGeneration: "0",
    });
    await waitUntil(
      () => starts.length === 1 && supervisor.status().queuedJobCount === 3,
    );
    expect(starts).toHaveLength(1);
    expect(supervisor.status().queuedJobCount).toBe(3);

    first.resolve("first");
    await expect(firstRun).resolves.toBe("first");
    await expect(duplicate).resolves.toBe("first");
    await expect(replacement).resolves.toBe(h28("11"));
    await expect(generationReplacement).resolves.toBe(h28("11"));
    await expect(secondRun).resolves.toBe(h28("22"));
    expect(starts.map(({ category }) => category)).toEqual([
      "doubleSpend",
      "doubleSpend",
      "doubleSpend",
      "networkId",
    ]);
    await supervisor.close();
  });

  it("stops intake and drains an accepted workflow before graceful close", async () => {
    const root = await directory();
    const gate = deferred<string>();
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: root,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      run: async () => await gate.promise,
    });
    await supervisor.recoverExisting(null);
    const running = supervisor.unsafeRunOrResumeForTest({
      mode: "run",
      category: "doubleSpend",
      headerHash: h28("55"),
      decisionDigest: "55".repeat(32),
      rollbackGeneration: "0",
    });
    await waitUntil(() => supervisor.status().activeJob !== null);
    const closing = supervisor.close();
    expect(supervisor.status().phase).toBe("closing");
    await expect(
      supervisor.unsafeRunOrResumeForTest({
        mode: "run",
        category: "networkId",
        headerHash: h28("66"),
        decisionDigest: "66".repeat(32),
        rollbackGeneration: "0",
      }),
    ).rejects.toThrow("supervisor is closing");
    gate.resolve("completed");
    await expect(running).resolves.toBe("completed");
    await expect(closing).resolves.toBeUndefined();
    await expect(supervisor.done).resolves.toBeUndefined();
    expect(supervisor.status().phase).toBe("closed");
  });

  it("acknowledges durable scheduling without awaiting the active workflow", async () => {
    const root = await directory();
    const active = deferred<string>();
    const starts: WatcherFaultProofJob[] = [];
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: root,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      run: async (job) => {
        starts.push(job);
        return starts.length === 1 ? await active.promise : "second";
      },
    });
    await supervisor.recoverExisting(null);
    const first = supervisor.unsafeScheduleForTest({
      mode: "run",
      category: "doubleSpend",
      headerHash: h28("71"),
      decisionDigest: "71".repeat(32),
      rollbackGeneration: "0",
    });
    await expect(first).resolves.toBeUndefined();
    await waitUntil(() => starts.length === 1);

    await expect(
      supervisor.unsafeScheduleForTest({
        mode: "run",
        category: "networkId",
        headerHash: h28("72"),
        decisionDigest: "72".repeat(32),
        rollbackGeneration: "0",
      }),
    ).resolves.toBeUndefined();
    expect(supervisor.status().queuedJobCount).toBe(1);
    active.resolve("first");
    await supervisor.close();
    expect(starts.map(({ decisionDigest }) => decisionDigest)).toEqual([
      "71".repeat(32),
      "72".repeat(32),
    ]);
  });

  it("treats an admitted rollback revocation as cancellation and accepts a fresh generation", async () => {
    const root = await directory();
    const revoked = Object.freeze({
      decisionDigest: "73".repeat(32),
      rollbackGeneration: "0",
      checkpoint: "before_submit" as const,
    }) as WorkflowActuationRevokedError;
    let calls = 0;
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: root,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      unsafeIsActuationRevokedErrorForTest: (
        error,
      ): error is WorkflowActuationRevokedError => error === revoked,
      run: async () => {
        calls += 1;
        if (calls === 1) throw revoked;
        return "fresh-generation-completed";
      },
    });
    await supervisor.recoverExisting(null);
    const first = supervisor.unsafeRunOrResumeForTest({
      mode: "run",
      category: "doubleSpend",
      headerHash: h28("73"),
      decisionDigest: "73".repeat(32),
      rollbackGeneration: "0",
    });
    await expect(first).resolves.toMatchObject({
      kind: "actuation_revoked",
      rollbackGeneration: "0",
      checkpoint: "before_submit",
    });
    expect(supervisor.status().phase).toBe("accepting");

    await expect(
      supervisor.unsafeRunOrResumeForTest({
        mode: "resume",
        category: "doubleSpend",
        headerHash: h28("73"),
        decisionDigest: "73".repeat(32),
        rollbackGeneration: "1",
      }),
    ).resolves.toBe("fresh-generation-completed");
    expect(supervisor.status().phase).toBe("accepting");
    await supervisor.close();
  });

  it("blocks every later target after a runner failure and drains accepted work on close", async () => {
    const root = await directory();
    const gate = deferred<void>();
    let calls = 0;
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: root,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      run: async () => {
        calls += 1;
        await gate.promise;
        throw new Error("authenticated workflow stalled");
      },
    });
    await supervisor.recoverExisting(null);
    const failed = supervisor.unsafeRunOrResumeForTest({
      mode: "run",
      category: "doubleSpend",
      headerHash: h28("33"),
      decisionDigest: "33".repeat(32),
      rollbackGeneration: "0",
    });
    const queued = supervisor.unsafeRunOrResumeForTest({
      mode: "run",
      category: "networkId",
      headerHash: h28("44"),
      decisionDigest: "44".repeat(32),
      rollbackGeneration: "0",
    });
    await waitUntil(
      () => calls === 1 && supervisor.status().queuedJobCount === 1,
    );
    const closing = supervisor.close();
    expect(supervisor.status().phase).toBe("closing");
    gate.resolve();
    await expect(failed).rejects.toThrow("authenticated workflow stalled");
    await expect(queued).rejects.toThrow("supervisor is blocked");
    await expect(supervisor.done).rejects.toThrow(
      "authenticated workflow stalled",
    );
    await expect(closing).resolves.toBeUndefined();
    expect(calls).toBe(1);
    expect(supervisor.status()).toMatchObject({
      phase: "blocked",
      blockedJob: { category: "doubleSpend", headerHash: h28("33") },
    });
    await expect(
      supervisor.unsafeRunOrResumeForTest({
        mode: "resume",
        category: "doubleSpend",
        headerHash: h28("33"),
        decisionDigest: "33".repeat(32),
        rollbackGeneration: "0",
      }),
    ).rejects.toThrow("supervisor is blocked");
  });

  it("runs queued jobs by earliest authenticated safe-start deadline", async () => {
    const root = await directory();
    const active = deferred<void>();
    const starts: string[] = [];
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: root,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      unsafeNowMsForTest: () => 1_000,
      run: async (job) => {
        starts.push(job.headerHash);
        if (starts.length === 1) await active.promise;
      },
    });
    await supervisor.recoverExisting(null);
    const first = supervisor.unsafeRunOrResumeForTest({
      mode: "run",
      category: "doubleSpend",
      headerHash: h28("81"),
      decisionDigest: "81".repeat(32),
      rollbackGeneration: "0",
      deadline: deadline(h28("81"), 10_000),
    });
    await Promise.resolve();
    const later = supervisor.unsafeRunOrResumeForTest({
      mode: "run",
      category: "networkId",
      headerHash: h28("82"),
      decisionDigest: "82".repeat(32),
      rollbackGeneration: "0",
      deadline: deadline(h28("82"), 30_000),
    });
    const earlier = supervisor.unsafeRunOrResumeForTest({
      mode: "run",
      category: "invalidRange",
      headerHash: h28("83"),
      decisionDigest: "83".repeat(32),
      rollbackGeneration: "0",
      deadline: deadline(h28("83"), 20_000),
    });
    await waitUntil(() => supervisor.status().queuedJobCount === 2);
    active.resolve();
    await Promise.all([first, later, earlier]);
    expect(starts).toEqual([h28("81"), h28("83"), h28("82")]);
    await supervisor.close();
  });

  it("reports an earlier queued deadline while a later-deadline job is active", async () => {
    const root = await directory();
    const active = deferred<void>();
    let nowMs = 1_000;
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: root,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      deadlineAlertHeadroomMs: 1_000,
      unsafeNowMsForTest: () => nowMs,
      run: async (job) => {
        if (job.headerHash === h28("84")) await active.promise;
      },
    });
    await supervisor.recoverExisting(null);
    const running = supervisor.unsafeRunOrResumeForTest({
      mode: "run",
      category: "doubleSpend",
      headerHash: h28("84"),
      decisionDigest: "84".repeat(32),
      rollbackGeneration: "0",
      deadline: deadline(h28("84"), 30_000),
    });
    await waitUntil(
      () => supervisor.status().activeJob?.headerHash === h28("84"),
    );
    const queued = supervisor.unsafeRunOrResumeForTest({
      mode: "run",
      category: "invalidRange",
      headerHash: h28("85"),
      decisionDigest: "85".repeat(32),
      rollbackGeneration: "0",
      deadline: deadline(h28("85"), 20_000),
    });
    await waitUntil(() => supervisor.status().queuedJobCount === 1);

    nowMs = 302_419_500;
    expect(supervisor.status()).toMatchObject({
      deadlineHealth: "at_risk",
      earliestDeadlineJob: { headerHash: h28("85") },
      remainingSafeStartMs: "500",
    });

    active.resolve();
    await expect(running).resolves.toBeUndefined();
    await expect(queued).resolves.toBeUndefined();
    await supervisor.close();
  });

  it("reports at-risk headroom and fails closed before an unsafe job starts", async () => {
    const root = await directory();
    const gate = deferred<void>();
    let nowMs = 302_399_500;
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: root,
      deploymentFingerprint: DEPLOYMENT_FINGERPRINT,
      deadlineAlertHeadroomMs: 1_000,
      unsafeNowMsForTest: () => nowMs,
      run: async () => await gate.promise,
    });
    await supervisor.recoverExisting(null);
    const run = supervisor.unsafeRunOrResumeForTest({
      mode: "run",
      category: "doubleSpend",
      headerHash: h28("91"),
      decisionDigest: "91".repeat(32),
      rollbackGeneration: "0",
      deadline: deadline(h28("91"), 0),
    });
    await waitUntil(() => supervisor.status().earliestDeadlineJob !== null);
    expect(supervisor.status()).toMatchObject({
      deadlineHealth: "at_risk",
      remainingSafeStartMs: "500",
    });
    nowMs = 302_400_000;
    const unsafe = supervisor.unsafeRunOrResumeForTest({
      mode: "run",
      category: "networkId",
      headerHash: h28("92"),
      decisionDigest: "92".repeat(32),
      rollbackGeneration: "0",
      deadline: deadline(h28("92"), 0),
    });
    gate.resolve();
    await expect(run).resolves.toBeUndefined();
    await expect(unsafe).rejects.toThrow("deadline is unsafe");
    await expect(supervisor.done).rejects.toThrow("deadline is unsafe");
    expect(supervisor.status()).toMatchObject({
      phase: "blocked",
      deadlineHealth: "unsafe",
    });
    await supervisor.close();
  });
});
