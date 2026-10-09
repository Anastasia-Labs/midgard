/**
 * The follower names what it cannot read or run: a failed refusals or
 * tx-inputs read is a degradation of its own rather than nothing to report,
 * and a follow loop that throws is named in readiness and restarted after a
 * backoff rather than left stopped.
 */
import type { L1NodeTransport } from "@al-ft/l1-node-transport";
import type { FollowStatus } from "@al-ft/midgard-l1-follower";
import { SIM_ORIGIN } from "@al-ft/midgard-l1-follower/testing";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  L1_FOLLOWER_LOOP_FAILED,
  openWatcherFollowerRuntime,
} from "../../src/l1-follower/follower-runtime.js";
import { SIM_HUB_ORACLE_ONE_SHOT } from "../support/l1-follower-state-queue-traffic.js";
import { D, RECOVERY_DEPTH } from "../support/l1-follower-store-reset.js";

const faults = vi.hoisted(() => ({
  refusals: false,
  txInputs: false,
  /** Follow runs that throw before one runs until aborted. */
  loopThrows: 0,
  loopRuns: 0,
}));

vi.mock("@al-ft/midgard-l1-follower", async (load) => {
  const actual = await load<typeof import("@al-ft/midgard-l1-follower")>();
  return {
    ...actual,
    followChain: async (
      options: Parameters<typeof actual.followChain>[0],
    ): Promise<FollowStatus | null> => {
      faults.loopRuns += 1;
      if (faults.loopThrows > 0) {
        faults.loopThrows -= 1;
        throw new Error("follow loop defect");
      }
      const status = {
        cursor: null,
        readiness: [],
      } as unknown as FollowStatus;
      await options.onStatus?.(status);
      await new Promise((resolve) =>
        options.signal?.addEventListener("abort", resolve, { once: true }),
      );
      return status;
    },
  };
});
vi.mock("../../src/l1-follower/event-refusals.js", async (load) => {
  const actual =
    await load<typeof import("../../src/l1-follower/event-refusals.js")>();
  return {
    ...actual,
    eventRefusalDegradationsIn: async (
      ...args: Parameters<typeof actual.eventRefusalDegradationsIn>
    ) => {
      if (faults.refusals) throw new Error("refusals table is locked");
      return await actual.eventRefusalDegradationsIn(...args);
    },
  };
});
vi.mock("../../src/l1-follower/tx-inputs.js", async (load) => {
  const actual =
    await load<typeof import("../../src/l1-follower/tx-inputs.js")>();
  return {
    ...actual,
    createTxInputsResolver: (
      ...args: Parameters<typeof actual.createTxInputsResolver>
    ) => {
      const resolver = actual.createTxInputsResolver(...args);
      return {
        ...resolver,
        assess: async () => {
          if (faults.txInputs) throw new Error("tx inputs table is locked");
          return await resolver.assess();
        },
      };
    },
  };
});

afterEach(() => {
  Object.assign(faults, {
    refusals: false,
    txInputs: false,
    loopThrows: 0,
    loopRuns: 0,
  });
});

const open = (origin: boolean) =>
  openWatcherFollowerRuntime({
    deployment: D,
    storePath: ":memory:",
    automaticRecoveryMaxDepth: RECOVERY_DEPTH,
    origin: origin
      ? { origin: SIM_ORIGIN.point, hubOracleOneShot: SIM_HUB_ORACLE_ONE_SHOT }
      : null,
    node: {
      binaryPath: "/nonexistent/midgard-l1-node-transport",
      socketPath: "/nonexistent/node.socket",
      networkMagic: 42,
      requestTimeoutMs: 1_000,
    },
    walletAddresses: [],
    eventProjection: { networkId: 0, lists: [] },
    unsafeTransportForTest: {
      close: async () => undefined,
    } as unknown as L1NodeTransport,
  });

const reasons = async (follower: ReturnType<typeof open>) =>
  (await follower.degradations()).map(({ reason, count, detail }) => ({
    reason,
    count,
    detail,
  }));

describe("follower runtime liveness", () => {
  it("names a failed refusals read and a failed tx-inputs read as degradations, and clears them", async () => {
    const follower = open(false);
    try {
      // With no origin nothing starts the store; the test does.
      expect((await follower.store.start()).kind).toBe("ready");
      expect(await reasons(follower)).toEqual([]);
      faults.refusals = true;
      expect(await reasons(follower)).toEqual([
        {
          reason: "l1_event_refusals_unreadable",
          count: 1,
          detail: "refusals table is locked",
        },
      ]);
      faults.txInputs = true;
      expect(await reasons(follower)).toEqual([
        {
          reason: "l1_tx_inputs_unreadable",
          count: 1,
          detail: "tx inputs table is locked",
        },
        {
          reason: "l1_event_refusals_unreadable",
          count: 1,
          detail: "refusals table is locked",
        },
      ]);
      faults.refusals = false;
      faults.txInputs = false;
      expect(await reasons(follower)).toEqual([]);
    } finally {
      await follower.close();
    }
  });

  it("names a follow loop that throws in readiness and restarts it after a backoff", async () => {
    faults.loopThrows = 2;
    const follower = open(true);
    try {
      await vi.waitFor(async () => {
        expect(faults.loopRuns).toBe(1);
        expect(await follower.readiness()).toContainEqual({
          reason: L1_FOLLOWER_LOOP_FAILED,
          detail: "follow loop defect; restarting in 250 ms",
        });
      });
      // The second throw backs off longer; the third run reports a status.
      await vi.waitFor(async () => {
        expect(faults.loopRuns).toBe(2);
        expect(await follower.readiness()).toContainEqual({
          reason: L1_FOLLOWER_LOOP_FAILED,
          detail: "follow loop defect; restarting in 500 ms",
        });
      });
      await vi.waitFor(
        async () => {
          expect(faults.loopRuns).toBe(3);
          expect(
            (await follower.readiness()).map(({ reason }) => reason),
          ).not.toContain(L1_FOLLOWER_LOOP_FAILED);
        },
        { timeout: 5_000 },
      );
    } finally {
      await follower.close();
    }
    // Closing aborts the running loop, which settles `done`.
    await expect(follower.done).resolves.not.toBeUndefined();
  });
});
