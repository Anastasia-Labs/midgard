/**
 * The follower's L1 degradations reach status and metrics without failing
 * readiness (ruling C on permanently unresolvable tx inputs), while the same
 * name as a follower readiness reason makes the watcher not ready; the
 * decision driver's reasons reach readiness the same way.
 */
import {
  FOLLOWER_NODE_BEHIND,
  FOLLOWER_NODE_UNAVAILABLE,
} from "@al-ft/midgard-l1-follower";
import { describe, expect, it } from "vitest";

import type { WatcherFollowerReadiness } from "../../src/l1-follower/follower-runtime.js";
import {
  L1_TX_INPUTS_UNRESOLVABLE,
  type WatcherL1Degradation,
} from "../../src/l1-follower/tx-inputs.js";
import { createWatcherOperationsObservability } from "../../src/runtime/operations-observability.js";
import {
  WATCHER_RETIREMENT_RESET_FAILED,
  type WatcherDecisionReadiness,
} from "../../src/runtime/watcher-runtime.decision-driver.js";
import { createWatcherL1Readiness } from "../../src/runtime/watcher-runtime.l1-readiness.js";
import { supervisor } from "./operations-observability.supervisor.js";

const MANIFEST = "11".repeat(32);

const watcher = (
  follower: Readonly<{
    readiness: readonly WatcherFollowerReadiness[];
    degradations: readonly WatcherL1Degradation[];
  }>,
  driver: readonly WatcherDecisionReadiness[] = [],
) => {
  const l1 = createWatcherL1Readiness({
    follower: {
      readiness: () => Promise.resolve(follower.readiness),
      degradations: () => Promise.resolve(follower.degradations),
    },
    driver: () => ({ readiness: () => driver }),
  });
  const observability = createWatcherOperationsObservability({
    deploymentFingerprint: MANIFEST,
    supervisor: supervisor().runtime,
    launchScopeStatus: () => ({
      installedCategoryCount: 32,
      requiredCategoryCount: 32,
    }),
    durableProofQueueStatus: () => ({
      queuedJobCount: 1,
      oldestQueuedAtMs: "99000",
    }),
    retainedDaTransportStatus: () => ({ state: "idle", failure: null }),
    l1Readiness: l1.read,
    l1Degradations: l1.degradations,
    nowMs: () => 100_000n,
  });
  observability.sink.recordL1Source({
    sourceIdentityDigest: "22".repeat(32),
    sourceMode: "local_node",
    status: "consistent",
    blockHash: "33".repeat(32),
    blockNo: "50",
    slot: "500",
    observedAtMs: "100000",
  });
  return { l1, api: observability.api };
};

describe("L1 degradations in operations observability", () => {
  it("reports an unpinned unresolvable input in status and metrics and stays ready", async () => {
    const w = watcher({
      readiness: [],
      degradations: [
        { reason: L1_TX_INPUTS_UNRESOLVABLE, count: 3, detail: "no proof" },
      ],
    });
    await w.l1.refresh();
    expect(w.api.status()).toMatchObject({
      readiness: "ready",
      readinessReasons: [],
      l1Degradations: [
        { reason: L1_TX_INPUTS_UNRESOLVABLE, count: "3", detail: "no proof" },
      ],
    });
    expect(w.api.metrics().l1Degradations).toEqual({
      [L1_TX_INPUTS_UNRESOLVABLE]: "3",
    });
  });

  it("fails readiness by name when the follower reports a pinned unresolvable input", async () => {
    const w = watcher({
      readiness: [{ reason: L1_TX_INPUTS_UNRESOLVABLE, detail: "pinned" }],
      degradations: [],
    });
    await w.l1.refresh();
    expect(w.api.status()).toMatchObject({
      readiness: "not_ready",
      readinessReasons: [L1_TX_INPUTS_UNRESOLVABLE],
      l1Degradations: [],
    });
    expect(w.api.metrics().l1Degradations).toEqual({});
  });

  it("fails readiness by name while the follower reports the L1 node unavailable", async () => {
    const lost = {
      reason: FOLLOWER_NODE_UNAVAILABLE,
      detail: "node_connection_lost: node socket closed",
    };
    const w = watcher({ readiness: [lost], degradations: [] });
    await w.l1.refresh();
    expect(w.api.status()).toMatchObject({
      readiness: "not_ready",
      readinessReasons: [FOLLOWER_NODE_UNAVAILABLE],
      l1Readiness: [lost],
    });
  });

  it("fails readiness by name, with the lag, while the follower reports the L1 node behind", async () => {
    const behind = {
      reason: FOLLOWER_NODE_BEHIND,
      detail:
        "node tip slot 1000 is 600 s behind wall-clock time (bound 300 s)",
    };
    const w = watcher({ readiness: [behind], degradations: [] });
    await w.l1.refresh();
    expect(w.api.status()).toMatchObject({
      readiness: "not_ready",
      readinessReasons: [FOLLOWER_NODE_BEHIND],
      l1Readiness: [behind],
    });
  });

  it("fails readiness by name while the decision driver reports a failed retirement reset", async () => {
    const failed = {
      reason: WATCHER_RETIREMENT_RESET_FAILED,
      detail: "the transcript store is locked",
    };
    const w = watcher({ readiness: [], degradations: [] }, [failed]);
    await w.l1.refresh();
    expect(w.api.status()).toMatchObject({
      readiness: "not_ready",
      readinessReasons: [WATCHER_RETIREMENT_RESET_FAILED],
      l1Readiness: [failed],
    });
  });
});
