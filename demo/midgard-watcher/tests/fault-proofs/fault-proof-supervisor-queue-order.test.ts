import { mkdtemp, rm } from "node:fs/promises";

import { MIDGARD_RETENTION_WINDOW } from "@al-ft/midgard-core";
import { h28 } from "@al-ft/midgard-test-support/hex";
import { afterEach, describe, expect, it, vi } from "vitest";

import { unsafeCreateWatcherFaultProofSupervisorForTest } from "../../src/fault-proofs/fault-proof-supervisor.create-watcher-fault-proof-supervisor.js";
import { waitForFaultProofSupervisorIdle } from "../support/fault-proof-supervisor-idle.js";

// The installed scope is narrowed to one family for this file only.
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
const deadline = (headerHash: string, headerBlockTimeMs: number) =>
  Object.freeze({
    headerHash,
    headerBlockTimeMs: headerBlockTimeMs.toString(),
    maturityAtMs: (
      headerBlockTimeMs + MIDGARD_RETENTION_WINDOW.maturityMs
    ).toString(),
    latestSafeStartAtMs: (
      headerBlockTimeMs + latestSafeStartOffsetMs
    ).toString(),
  });

const roots: string[] = [];
afterEach(async () => {
  await Promise.all(
    roots.splice(0).map((root) => rm(root, { recursive: true, force: true })),
  );
});

describe("fault-proof supervisor queue order", () => {
  it("runs the earliest latest safe start first, whatever its arrival, and reports its deadline", async () => {
    const root = await mkdtemp("/var/tmp/midgard-fault-supervisor-order-");
    roots.push(root);
    const order: string[] = [];
    let releaseActive!: () => void;
    const active = new Promise<void>((resolve) => (releaseActive = resolve));
    let nowMs = 1_000;
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: root,
      hubOraclePolicyId: "dd".repeat(28),
      deadlineAlertHeadroomMs: 1_000,
      unsafeNowMsForTest: () => nowMs,
      run: async ({ headerHash }) => {
        order.push(headerHash);
        if (headerHash === h28(0x91)) await active;
      },
    });
    const job = (byte: number, headerBlockTimeMs: number) => ({
      mode: "run" as const,
      category: "doubleSpend" as const,
      headerHash: h28(byte),
      decisionDigest: byte.toString(16).repeat(32),
      rollbackGeneration: "0",
      deadline: deadline(h28(byte), headerBlockTimeMs),
    });
    try {
      const running = supervisor.unsafeRunOrResumeForTest(job(0x91, 50_000));
      await vi.waitFor(() =>
        expect(supervisor.status().activeJob?.headerHash).toBe(h28(0x91)),
      );
      // Work with a later deadline arrives first; work with an earlier one
      // arrives behind it.
      const later = supervisor.unsafeRunOrResumeForTest(job(0x93, 40_000));
      const earlier = supervisor.unsafeRunOrResumeForTest(job(0x92, 20_000));
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
      await Promise.all([running, later, earlier]);
      await waitForFaultProofSupervisorIdle(supervisor);
      expect(order).toEqual([h28(0x91), h28(0x92), h28(0x93)]);
    } finally {
      releaseActive();
      await supervisor.close();
    }
  }, 60_000);
});
