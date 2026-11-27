import { describe, expect, it, vi } from "vitest";

import { deferredAvailabilityResponder } from "../src/availability/deferred-responder.js";
import type { AvailabilityResponderReport } from "../src/availability/responder.js";
import type { CommitteeL1Readiness } from "../src/l1/follower/l1-follower.js";

const idle: AvailabilityResponderReport = { challenges: 0, status: "idle" };

/** A follower whose readiness reasons the test sets. */
const follower = (initial: readonly CommitteeL1Readiness[]) => {
  let reasons = initial;
  return {
    source: { readiness: () => reasons },
    set: (next: readonly CommitteeL1Readiness[]) => {
      reasons = next;
    },
  };
};

const built = () => {
  const drain = vi.fn(async () => idle);
  const close = vi.fn();
  return { built: { responder: { drain }, close }, drain, close };
};

describe("the availability responder built on the follower's first ready drain", () => {
  it("reports awaiting_scan with the follower's reasons and builds nothing while the follower holds the committee", async () => {
    const l1 = follower([
      { reason: "rollback_beyond_k", detail: "depth 7 > k 6" },
      { reason: "follower_waiting", detail: "node down" },
    ]);
    const build = vi.fn(async () => built().built);
    const deferred = deferredAvailabilityResponder(l1.source, build);
    for (let i = 0; i < 3; i += 1) {
      const report = await deferred.responder.drain();
      expect(report).toMatchObject({ challenges: 0, status: "awaiting_scan" });
      expect(report.detail).toContain(
        "rollback_beyond_k: depth 7 > k 6; follower_waiting: node down",
      );
    }
    expect(build).not.toHaveBeenCalled();
  });

  it("builds once the follower is ready, then drains the built responder on every later drain without building again", async () => {
    const l1 = follower([{ reason: "follower_catching_up", detail: "behind" }]);
    const inner = built();
    const build = vi.fn(async () => inner.built);
    const deferred = deferredAvailabilityResponder(l1.source, build);
    await expect(deferred.responder.drain()).resolves.toMatchObject({
      status: "awaiting_scan",
    });
    l1.set([]);
    await expect(deferred.responder.drain()).resolves.toEqual(idle);
    await expect(deferred.responder.drain()).resolves.toEqual(idle);
    expect(build).toHaveBeenCalledTimes(1);
    expect(inner.drain).toHaveBeenCalledTimes(2);
    // Once built, the responder's own steps read the follower's boundary
    // and report its holds themselves; the wrapper does not gate them again.
    l1.set([{ reason: "rollback_beyond_k", detail: "deep" }]);
    await deferred.responder.drain();
    expect(inner.drain).toHaveBeenCalledTimes(3);
  });

  it("reports a failed construction as a failed drain and retries it on the next drain, the process up", async () => {
    const l1 = follower([]);
    const inner = built();
    const build = vi
      .fn<() => Promise<typeof inner.built>>()
      .mockRejectedValueOnce(new Error("no reference scripts at the tip yet"))
      .mockResolvedValueOnce(inner.built);
    const deferred = deferredAvailabilityResponder(l1.source, build);
    await expect(deferred.responder.drain()).resolves.toEqual({
      challenges: 0,
      status: "failed",
      detail:
        "Availability responder construction failed: no reference scripts at the tip yet",
    });
    await expect(deferred.responder.drain()).resolves.toEqual(idle);
    expect(build).toHaveBeenCalledTimes(2);
    expect(inner.drain).toHaveBeenCalledTimes(1);
  });

  it("closes the built responder, and one whose construction finishes after close, never draining it", async () => {
    const l1 = follower([]);
    const first = built();
    const deferred = deferredAvailabilityResponder(
      l1.source,
      async () => first.built,
    );
    await deferred.responder.drain();
    deferred.close();
    expect(first.close).toHaveBeenCalledTimes(1);

    const late = built();
    let finish: (value: typeof late.built) => void = () => undefined;
    const pending = deferredAvailabilityResponder(
      l1.source,
      () =>
        new Promise((resolve) => {
          finish = resolve;
        }),
    );
    const drained = pending.responder.drain();
    pending.close();
    finish(late.built);
    await expect(drained).resolves.toEqual(idle);
    expect(late.close).toHaveBeenCalledTimes(1);
    expect(late.drain).not.toHaveBeenCalled();
  });
});
