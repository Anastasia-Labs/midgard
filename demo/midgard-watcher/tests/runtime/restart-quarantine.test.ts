import { describe, expect, it, vi } from "vitest";

import { startWatcherNativeChainSyncWithRetry } from "../../src/l1/native-chain-sync.js";
import {
  createWatcherStartupProgress,
  type WatcherStartupProgress,
} from "../../src/runtime/startup-progress.js";
import { attemptWatcherRestartQuarantineRecovery } from "../../src/runtime/watcher-runtime.restart-quarantine.js";
import type { WatcherDurableRuntime } from "../../src/storage/durable-runtime.js";
import {
  config,
  INTERSECTION,
  readIdentityFixture,
  spawnFixture,
} from "../l1/native-chain-sync.config.js";

const HELD =
  "Watcher restart remains quarantined: authenticated recovery evidence is incomplete";

/** Starts the honest helper for each attempt and counts open streams. */
const nativeHelper = () => {
  const counts = { started: 0, closed: 0 };
  const start: typeof startWatcherNativeChainSyncWithRetry = async (input) => {
    const runtime = await startWatcherNativeChainSyncWithRetry({
      ...input,
      unsafeSpawnForTest: spawnFixture("honest"),
      unsafeReadIdentityFileForTest: readIdentityFixture,
    });
    counts.started += 1;
    return {
      ...runtime,
      close: async () => {
        counts.closed += 1;
        await runtime.close();
      },
    };
  };
  return { counts, start };
};

const recoverAt = (
  stateQueueBlockNo: string,
  recover: NonNullable<
    Parameters<typeof attemptWatcherRestartQuarantineRecovery>[0]["recover"]
  >,
) => {
  const events: WatcherStartupProgress[] = [];
  const startup = createWatcherStartupProgress(
    (event) => events.push(event),
    () => 1,
  );
  const helper = nativeHelper();
  const outcome = startup("post_finality_recovery", () =>
    attemptWatcherRestartQuarantineRecovery({
      durable: {
        readFinality: () => ({ finalized: null }),
      } as unknown as WatcherDurableRuntime,
      blockProgress: { readHead: () => null, readCandidates: () => [] },
      stateQueueCursor: {
        blockHash: INTERSECTION.blockHash,
        blockNo: stateQueueBlockNo,
        slot: INTERSECTION.slot,
      },
      binaryPath: "/test/native-chain-sync",
      watcherConfig: config(),
      start: helper.start,
      recover,
    }),
  );
  return { events, helper, outcome };
};

const shape = (events: readonly WatcherStartupProgress[]) =>
  events.map(({ stage, outcome, error, retryAfterMs }) => ({
    stage,
    outcome,
    ...(error === undefined ? {} : { error }),
    ...(retryAfterMs === undefined ? {} : { retryAfterMs }),
  }));

describe("restart into a post-finality quarantine", () => {
  it("holds startup while recovery evidence is incomplete and resumes exactly once", async () => {
    const recover = vi
      .fn()
      .mockResolvedValueOnce(true)
      .mockResolvedValueOnce(true)
      .mockResolvedValue(false);
    const { events, helper, outcome } = recoverAt("10", recover);
    await expect(outcome).resolves.toBeUndefined();
    // Each attempt asks the node afresh and closes its stream.
    expect(recover).toHaveBeenCalledTimes(3);
    for (const [call] of recover.mock.calls)
      expect(call.restartIntersection).toEqual(INTERSECTION);
    expect(helper.counts).toEqual({ started: 3, closed: 3 });
    expect(shape(events)).toEqual([
      { stage: "post_finality_recovery", outcome: "started" },
      {
        stage: "post_finality_recovery",
        outcome: "pending",
        error: HELD,
        retryAfterMs: 1,
      },
      {
        stage: "post_finality_recovery",
        outcome: "pending",
        error: HELD,
        retryAfterMs: 1,
      },
      { stage: "post_finality_recovery", outcome: "completed" },
    ]);
  });

  it("still fails startup on a recovery persistence conflict", async () => {
    const conflict = new Error(
      "watcher restart recovery persistence conflicted",
    );
    const recover = vi.fn().mockRejectedValue(conflict);
    const { events, helper, outcome } = recoverAt("10", recover);
    await expect(outcome).rejects.toBe(conflict);
    expect(recover).toHaveBeenCalledOnce();
    expect(helper.counts).toEqual({ started: 1, closed: 1 });
    expect(shape(events)).toEqual([
      { stage: "post_finality_recovery", outcome: "started" },
      {
        stage: "post_finality_recovery",
        outcome: "failed",
        error: conflict.message,
      },
    ]);
  });

  it("still fails startup when the node's tip is behind the recorded resume point", async () => {
    // The fixture node's tip is block 12; the recorded point claims block 50.
    const recover = vi.fn().mockResolvedValue(false);
    const { helper, outcome } = recoverAt("50", recover);
    await expect(outcome).rejects.toThrow(
      "native chain-sync tip is behind the selected resume point",
    );
    expect(recover).not.toHaveBeenCalled();
    expect(helper.counts).toEqual({ started: 1, closed: 1 });
  });

  it("gives each native start the process-start bound, not the request timeout", async () => {
    const helper = nativeHelper();
    const timeouts: number[] = [];
    await attemptWatcherRestartQuarantineRecovery({
      durable: {
        readFinality: () => ({ finalized: null }),
      } as unknown as WatcherDurableRuntime,
      blockProgress: { readHead: () => null, readCandidates: () => [] },
      stateQueueCursor: {
        blockHash: INTERSECTION.blockHash,
        blockNo: "10",
        slot: INTERSECTION.slot,
      },
      binaryPath: "/test/native-chain-sync",
      watcherConfig: config(),
      start: async (input) => {
        timeouts.push(input.startupTimeoutMs);
        return await helper.start(input);
      },
      recover: vi.fn().mockResolvedValue(false),
    });
    expect(config().l1.requestTimeoutMs).toBe(10_000);
    expect(timeouts).toEqual([120_000]);
  });
});
