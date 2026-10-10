/**
 * The follower's node transport is bounded by the watcher's own
 * `l1.requestTimeoutMs`, not the transport's default: a bound the transport
 * refuses (below 2 ms) refuses the follower at construction.
 */
import { SIM_ORIGIN } from "@al-ft/midgard-l1-follower/testing";
import { describe, expect, it } from "vitest";

import { openWatcherFollowerRuntime } from "../../src/l1-follower/follower-runtime.js";
import { SIM_HUB_ORACLE_ONE_SHOT } from "../support/l1-follower-state-queue-traffic.js";
import { D, RECOVERY_DEPTH } from "../support/l1-follower-store-reset.js";

describe("the follower's node transport options", () => {
  it("hands the configured request timeout to the transport", () => {
    expect(() =>
      openWatcherFollowerRuntime({
        deployment: D,
        storePath: ":memory:",
        automaticRecoveryMaxDepth: RECOVERY_DEPTH,
        origin: {
          origin: SIM_ORIGIN.point,
          hubOracleOneShot: SIM_HUB_ORACLE_ONE_SHOT,
        },
        node: {
          binaryPath: "/nonexistent/midgard-l1-node-transport",
          socketPath: "/nonexistent/node.socket",
          networkMagic: 42,
          requestTimeoutMs: 1,
        },
        walletAddresses: [],
      }),
    ).toThrow("requestTimeoutMs must be an integer of at least 2");
  });
});
