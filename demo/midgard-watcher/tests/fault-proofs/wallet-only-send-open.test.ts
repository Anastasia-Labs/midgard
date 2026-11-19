/** The restart wait and slot clock: open once the clock and the last
 * applied block are both one wallet-only validity window past the instant
 * the supervisor was built, on the deployment's slot mapping, and the same
 * for every admission of the process. */
import { mkdtemp, rm } from "node:fs/promises";
import { setTimeout as waitForDisk } from "node:timers/promises";

import { WALLET_ONLY_VALIDITY_MS } from "@al-ft/midgard-fault-proofs/wallet-only-validity";
import { afterEach, describe, expect, it, vi } from "vitest";

import type { WatcherFaultProofExecutionAdmission } from "../../src/fault-proofs/fault-proof-execution.js";
import { unsafeCreateWatcherFaultProofSupervisorForTest } from "../../src/fault-proofs/fault-proof-supervisor.create-watcher-fault-proof-supervisor.js";
import {
  createWalletOnlySend,
  isWalletOnlySendOpen,
} from "../../src/fault-proofs/wallet-only-send-open.js";

/** One-second slots from slot 50 at 1,000,000 ms. */
const slotConfig = { zeroTime: 1_000_000, zeroSlot: 50, slotLength: 1_000 };
const timeOfSlot = (slot: number) =>
  slotConfig.zeroTime + (slot - slotConfig.zeroSlot) * slotConfig.slotLength;

const paths: string[] = [];
afterEach(async () => {
  vi.useRealTimers();
  await Promise.all(
    paths.splice(0).map((path) => rm(path, { recursive: true, force: true })),
  );
});

describe("isWalletOnlySendOpen", () => {
  const notBeforeMs = 1_000_000;
  it.each([
    { nowMs: notBeforeMs, appliedL1TimeMs: notBeforeMs, open: true },
    { nowMs: notBeforeMs + 5, appliedL1TimeMs: notBeforeMs + 9, open: true },
    { nowMs: notBeforeMs - 1, appliedL1TimeMs: notBeforeMs, open: false },
    { nowMs: notBeforeMs, appliedL1TimeMs: notBeforeMs - 1, open: false },
    { nowMs: notBeforeMs + 60_000, appliedL1TimeMs: null, open: false },
  ])(
    "is $open at clock $nowMs and applied block time $appliedL1TimeMs",
    ({ nowMs, appliedL1TimeMs, open }) => {
      expect(
        isWalletOnlySendOpen({ notBeforeMs, nowMs, appliedL1TimeMs }),
      ).toBe(open);
    },
  );
});

describe("createWalletOnlySend", () => {
  it("fixes the instant when it is built, one window past the clock, and reads the applied slot on the deployment's mapping", async () => {
    let now = timeOfSlot(60);
    let applied: number | null = null;
    const { open } = createWalletOnlySend({
      nowMs: () => now,
      slotConfig,
      appliedSlot: () => applied,
    });
    const instant = timeOfSlot(60) + WALLET_ONLY_VALIDITY_MS;
    const instantSlot = 60 + WALLET_ONLY_VALIDITY_MS / slotConfig.slotLength;
    now = instant;
    expect(await open()).toBe(false);
    applied = instantSlot - 1;
    expect(await open()).toBe(false);
    applied = instantSlot;
    expect(await open()).toBe(true);
    now = instant - 1;
    expect(await open()).toBe(false);
  });

  it("reads a failed read of the applied slot as no applied block", async () => {
    let now = timeOfSlot(50);
    const instantSlot = 50 + WALLET_ONLY_VALIDITY_MS / slotConfig.slotLength;
    let read: () => number | Promise<number | null> = () => {
      throw new Error("follower cursor unreadable");
    };
    const { open } = createWalletOnlySend({
      nowMs: () => now,
      slotConfig,
      appliedSlot: () => read(),
    });
    now = timeOfSlot(instantSlot);
    expect(await open()).toBe(false);
    read = async () => {
      throw new Error("follower cursor unreadable");
    };
    expect(await open()).toBe(false);
    read = async () => instantSlot;
    expect(await open()).toBe(true);
  });

  it("gives the deployment slot one window past its clock at the check, floored as the ledger does", () => {
    let now = timeOfSlot(50) - WALLET_ONLY_VALIDITY_MS;
    const { latestValidToSlot } = createWalletOnlySend({
      nowMs: () => now,
      slotConfig,
      appliedSlot: () => null,
    });
    expect(latestValidToSlot()).toBe(50);
    now = timeOfSlot(230) + 999 - WALLET_ONLY_VALIDITY_MS;
    expect(latestValidToSlot()).toBe(230);
    now += 1;
    expect(latestValidToSlot()).toBe(231);
  });
});

const waitFor = async (predicate: () => boolean) => {
  for (let attempt = 0; attempt < 1_000; attempt++) {
    if (predicate()) return;
    await waitForDisk(5);
  }
  throw new Error("supervisor did not reach its expected state");
};

describe("the supervisor's restart wait", () => {
  it("carries one reader, fixed at construction, on every admission and a requeue, and admits other pending objectives on every observation", async () => {
    vi.useFakeTimers({ toFake: ["setTimeout", "clearTimeout"] });
    const root = await mkdtemp("/var/tmp/midgard-restart-wait-");
    paths.push(root);
    const constructedAtMs = 1_000;
    let now = constructedAtMs;
    let applied: number | null = null;
    const admissions: WatcherFaultProofExecutionAdmission[] = [];
    const outcomes = [
      // A transport loss: the supervisor requeues the job after its backoff.
      {
        kind: "retryable",
        resume: "backoff",
        reason: "controlled transport loss",
        retryAfterMs: 1_000,
      },
      { kind: "pending", resume: "await_observation", reason: "in flight" },
      { kind: "pending", resume: "await_observation", reason: "in flight" },
    ];
    const run = vi.fn(
      async (_job: unknown, admission: WatcherFaultProofExecutionAdmission) => {
        admissions.push(admission);
        return outcomes[admissions.length - 1];
      },
    );
    const supervisor = unsafeCreateWatcherFaultProofSupervisorForTest({
      journalRoot: root,
      deploymentFingerprint: "dd".repeat(32),
      run,
      unsafeNowMsForTest: () => now,
      unsafeL1ClockForTest: {
        // One-second slots from 0: slot n starts at n seconds.
        slotConfig: { zeroTime: 0, zeroSlot: 0, slotLength: 1_000 },
        appliedSlot: () => applied,
      },
    });
    let failure: unknown;
    void supervisor.done.catch((error: unknown) => {
      failure = error;
    });
    const job = {
      mode: "run" as const,
      category: "doubleSpend" as const,
      headerHash: "ab".repeat(28),
      decisionDigest: "cd".repeat(32),
      rollbackGeneration: "0",
      observationRevision: "first",
    };
    await supervisor.recoverExisting(null);
    // Each admission comes later on the clock: none of them moves the instant.
    now += 40_000;
    await supervisor.unsafeScheduleForTest(job);
    await waitFor(
      () =>
        run.mock.calls.length === 1 && supervisor.status().activeJob === null,
    );
    now += 40_000;
    await vi.advanceTimersByTimeAsync(1_000);
    await waitFor(
      () =>
        failure !== undefined ||
        (run.mock.calls.length === 2 && supervisor.status().activeJob === null),
    );
    now += 40_000;
    await supervisor.unsafeScheduleForTest({
      ...job,
      observationRevision: "second",
    });
    await waitFor(
      () =>
        failure !== undefined ||
        (run.mock.calls.length === 3 && supervisor.status().activeJob === null),
    );
    if (failure !== undefined) throw failure;
    expect(admissions).toHaveLength(3);
    const [first, ...rest] = admissions.map(
      ({ walletOnlySend }) => walletOnlySend,
    );
    for (const reader of rest) expect(reader).toBe(first);

    const instant = constructedAtMs + WALLET_ONLY_VALIDITY_MS;
    const instantSlot = instant / 1_000;
    now = instant - 1;
    applied = instantSlot;
    expect(await first!.open()).toBe(false);
    now = instant;
    applied = instantSlot - 1;
    expect(await first!.open()).toBe(false);
    applied = null;
    expect(await first!.open()).toBe(false);
    applied = instantSlot;
    expect(await first!.open()).toBe(true);
    now = instant + 999;
    expect(first!.latestValidToSlot()).toBe(
      instantSlot + WALLET_ONLY_VALIDITY_MS / 1_000,
    );
    await supervisor.close();
  });
});
