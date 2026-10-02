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
  spawnFixture,
} from "./native-chain-sync.config.js";

/** Spawns each scripted helper mode once, in order, then an honest helper. */
const scripted = (modes: readonly string[]) => {
  const spawned: string[] = [];
  return {
    spawned,
    spawn: () => {
      const mode = modes[spawned.length] ?? "honest";
      spawned.push(mode);
      return spawnFixture(mode)();
    },
  };
};

const startWith = (
  modes: readonly string[],
  extra: Partial<Parameters<typeof startWatcherNativeChainSyncWithRetry>[0]>,
) => {
  const helper = scripted(modes);
  const warn = vi.fn();
  const runtime = startWatcherNativeChainSyncWithRetry({
    binaryPath: "/test/native-chain-sync",
    watcherConfig: config(),
    intersectionCandidates: [INTERSECTION],
    startupTimeoutMs: 2_000,
    onEvent: async () => undefined,
    unsafeSpawnForTest: helper.spawn,
    unsafeReadIdentityFileForTest: readIdentityFixture,
    warn,
    retryDelayMs: () => 1,
    ...extra,
  });
  return { helper, warn, runtime };
};

describe("native chain-sync startup while the node does not answer", () => {
  it("waits for a restarting node and starts exactly one stream once it answers", async () => {
    const { helper, warn, runtime } = startWith(
      [
        "fail:node_handshake_failed",
        "fail:node_handshake_failed",
        "fail:tip_query_failed",
      ],
      {},
    );
    const started = await runtime;
    try {
      expect(helper.spawned).toEqual([
        "fail:node_handshake_failed",
        "fail:node_handshake_failed",
        "fail:tip_query_failed",
        "honest",
      ]);
      expect(
        watcherNativeChainSyncAuthorityDetails(started.authority)
          ?.selectedIntersection,
      ).toEqual(INTERSECTION);
      // One warning for the outage, not one per attempt.
      expect(warn).toHaveBeenCalledOnce();
      expect(warn).toHaveBeenCalledWith({
        event: "native_node_unavailable",
        code: "node_handshake_failed",
        retryAfterMs: 1,
      });
    } finally {
      await started.close();
    }
  });

  it("waits out a node that does not become ready in time", async () => {
    const { helper, runtime } = startWith(["no_ready"], {
      startupTimeoutMs: 200,
    });
    const started = await runtime;
    try {
      expect(helper.spawned).toEqual(["no_ready", "honest"]);
    } finally {
      await started.close();
    }
  });

  it.each([
    "invalid_startup",
    "connection_setup_failed",
    "intersection_failed",
  ])("still refuses startup on %s, at once", async (code) => {
    const { helper, warn, runtime } = startWith([`fail:${code}`], {});
    await expect(runtime).rejects.toMatchObject({
      name: "NativeChainSyncStartupFailure",
      code,
    });
    expect(helper.spawned).toEqual([`fail:${code}`]);
    expect(warn).not.toHaveBeenCalled();
  });

  it("still refuses a helper whose readiness contradicts its startup", async () => {
    const { helper, runtime } = startWith(["forged_ready"], {});
    await expect(runtime).rejects.toThrow(
      "native chain-sync ready identity differs from startup authority",
    );
    expect(helper.spawned).toEqual(["forged_ready"]);
  });

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
