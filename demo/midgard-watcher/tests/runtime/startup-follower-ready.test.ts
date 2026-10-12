import type { FollowStatus } from "@al-ft/midgard-l1-follower";
import { describe, expect, it } from "vitest";

import {
  createWatcherStartupProgress,
  type WatcherStartupProgress,
} from "../../src/runtime/startup-progress.js";
import { untilFollowerReady } from "../../src/runtime/watcher-runtime.prepare-services.js";

const atTip = { readiness: [], cursor: null } as unknown as FollowStatus;
const behind = {
  readiness: [{ reason: "l1_follower_catching_up", detail: "100 blocks" }],
  cursor: null,
} as unknown as FollowStatus;

/** A follower whose status is the next of `statuses` on each read, then the last. */
const scripted = (
  reasons: readonly { reason: string; detail: string }[],
  ...statuses: (FollowStatus | null)[]
) => {
  let reads = 0;
  return {
    reads: () => reads,
    status: () => statuses[Math.min(reads++, statuses.length - 1)]!,
    readiness: async () => reasons,
  };
};

const progress = () => {
  const seen: WatcherStartupProgress[] = [];
  return {
    seen,
    startup: createWatcherStartupProgress(
      (entry) => seen.push(entry),
      () => 1,
    ),
  };
};

describe("the watcher's L1 follower startup stage", () => {
  it("passes at once when the follower reported a status at the tip", async () => {
    const follower = scripted([], atTip);
    const { seen, startup } = progress();
    await untilFollowerReady(follower, startup);
    expect(follower.reads()).toBe(1);
    expect(seen.map(({ outcome }) => outcome)).toEqual([
      "started",
      "completed",
    ]);
  });

  it("holds, naming the follower's reasons, until it is at the tip", async () => {
    const follower = scripted(
      [{ reason: "l1_follower_catching_up", detail: "100 blocks" }],
      null,
      behind,
      atTip,
    );
    const { seen, startup } = progress();
    await untilFollowerReady(follower, startup);
    expect(follower.reads()).toBe(3);
    const pending = seen.filter(({ outcome }) => outcome === "pending");
    expect(pending).toHaveLength(2);
    for (const entry of pending)
      expect(entry.error).toBe(
        "the L1 follower is not ready: l1_follower_catching_up: 100 blocks",
      );
    expect(seen.at(-1)?.outcome).toBe("completed");
  });

  it("holds by name, never failing, on a reason only an operator clears", async () => {
    let status: FollowStatus | null = null;
    let reads = 0;
    const follower = {
      status: () => {
        reads += 1;
        return status;
      },
      readiness: async () => [
        { reason: "l1_origin_not_configured", detail: "l1.origin is not set" },
      ],
    };
    const { seen, startup } = progress();
    let settled = false;
    const stage = untilFollowerReady(follower, startup).finally(() => {
      settled = true;
    });
    while (reads < 20) await new Promise((r) => setTimeout(r, 1));
    expect(settled).toBe(false);
    expect(seen.some(({ outcome }) => outcome === "failed")).toBe(false);
    expect(seen.at(-1)?.error).toBe(
      "the L1 follower is not ready: l1_origin_not_configured: l1.origin is not set",
    );
    // An operator set the origin and restarted the follower: startup goes on.
    status = atTip;
    await stage;
    expect(seen.at(-1)?.outcome).toBe("completed");
  });
});
