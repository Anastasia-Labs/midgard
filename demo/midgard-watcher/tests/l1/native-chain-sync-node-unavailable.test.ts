import { describe, expect, it, vi } from "vitest";

import {
  startWatcherNativeChainSyncWithRetry,
  watcherNativeChainSyncAuthorityDetails,
  watcherNativeChainSyncStartupTimeoutMs,
} from "../../src/l1/native-chain-sync.js";
import {
  config,
  INTERSECTION,
  readIdentityFixture,
} from "./native-chain-sync.config.js";
import { fakeNodeTransport } from "./native-chain-sync.fake-transport.js";

/**
 * Starts against a node that takes each scripted step once, in order, then
 * follows honestly. "hello:<code>" refuses a session's handshake; every other
 * step answers one chain-sync open.
 */
const startWith = async (
  steps: readonly string[],
  extra: Partial<Parameters<typeof startWatcherNativeChainSyncWithRetry>[0]>,
  mode = "honest",
) => {
  const transport = await fakeNodeTransport(mode, { steps });
  const warn = vi.fn();
  const runtime = startWatcherNativeChainSyncWithRetry({
    binaryPath: transport.binaryPath,
    watcherConfig: config(),
    intersectionCandidates: [INTERSECTION],
    startupTimeoutMs: 2_000,
    onEvent: async () => undefined,
    unsafeReadIdentityFileForTest: readIdentityFixture,
    warn,
    retryDelayMs: () => 1,
    ...extra,
  });
  const taken = () =>
    transport.journal().filter((line) => line.startsWith("step "));
  return { taken, warn, runtime };
};

describe("native chain-sync startup while the node does not answer", () => {
  it("waits for a restarting node and starts exactly one stream once it answers", async () => {
    const { taken, warn, runtime } = await startWith(
      [
        "hello:node_handshake_failed",
        "hello:node_handshake_failed",
        "fail:node_unavailable",
      ],
      {},
    );
    const started = await runtime;
    try {
      // The transport rides out the refused handshakes within the start
      // bound; the refused open is retried as a new start.
      expect(taken()).toEqual([
        "step hello:node_handshake_failed",
        "step hello:node_handshake_failed",
        "step fail:node_unavailable",
        "step honest",
      ]);
      expect(
        watcherNativeChainSyncAuthorityDetails(started.authority)
          ?.selectedIntersection,
      ).toEqual(INTERSECTION);
      // One warning for the outage, not one per attempt.
      expect(warn).toHaveBeenCalledOnce();
      expect(warn).toHaveBeenCalledWith({
        event: "native_node_unavailable",
        code: "node_unavailable",
        retryAfterMs: 1,
      });
    } finally {
      await started.close();
    }
  });

  it("stays unready past the start bound while the node refuses its handshake", async () => {
    const { taken, warn, runtime } = await startWith(
      ["hello:node_unreachable", "hello:node_unreachable"],
      { startupTimeoutMs: 200 },
    );
    const started = await runtime;
    try {
      expect(taken()).toEqual([
        "step hello:node_unreachable",
        "step hello:node_unreachable",
        "step honest",
      ]);
      expect(warn).toHaveBeenCalledOnce();
      expect(warn).toHaveBeenCalledWith({
        event: "native_node_unavailable",
        code: "node_unreachable",
        retryAfterMs: 1,
      });
    } finally {
      await started.close();
    }
  });

  it("waits out a node that does not answer the intersection in time", async () => {
    const { taken, runtime } = await startWith(["no_ready"], {
      startupTimeoutMs: 200,
    });
    const started = await runtime;
    try {
      expect(taken()).toEqual(["step no_ready", "step honest"]);
    } finally {
      await started.close();
    }
  });

  it.each([
    ["invalid_points", ["fail:invalid_points"], "honest"],
    ["intersection_failed", [], "retry_intersection"],
  ] as const)(
    "still refuses startup on %s, at once",
    async (code, steps, mode) => {
      const { taken, warn, runtime } = await startWith(steps, {}, mode);
      await expect(runtime).rejects.toMatchObject({
        name: "NativeChainSyncStartupFailure",
        code,
      });
      expect(taken()).toHaveLength(1);
      expect(warn).not.toHaveBeenCalled();
    },
  );

  it("bounds a native process start by its own two-minute floor, never the per-request timeout", () => {
    const base = config();
    const withRequestTimeout = (requestTimeoutMs: number) => ({
      ...base,
      l1: { ...base.l1, requestTimeoutMs },
    });
    expect(watcherNativeChainSyncStartupTimeoutMs(base)).toBe(120_000);
    expect(
      watcherNativeChainSyncStartupTimeoutMs(withRequestTimeout(30_000)),
    ).toBe(120_000);
    expect(
      watcherNativeChainSyncStartupTimeoutMs(withRequestTimeout(150_000)),
    ).toBe(150_000);
  });
});
