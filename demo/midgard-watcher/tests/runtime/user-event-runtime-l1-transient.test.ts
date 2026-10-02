import { LocalKupmiosTransportUnavailableError } from "@al-ft/midgard-fault-proofs";
import { afterEach, describe, expect, it, vi } from "vitest";

import {
  createWatcherUserEventRuntime,
  type WatcherUserEventRuntime,
} from "../../src/runtime/user-event-runtime.js";
import { setup } from "./user-event-runtime.setup.js";

// Fails the next native captures with scripted errors, before any request.
const faults = vi.hoisted((): (() => Error)[] => []);
vi.mock("../../src/l1/local-historical-capture.js", async (original) => {
  const actual =
    await original<typeof import("../../src/l1/local-historical-capture.js")>();
  return {
    ...actual,
    openWatcherLocalHistoricalCapture: async (
      ...args: Parameters<typeof actual.openWatcherLocalHistoricalCapture>
    ) => {
      const fault = faults.shift();
      if (fault !== undefined) throw fault();
      return await actual.openWatcherLocalHistoricalCapture(...args);
    },
  };
});

afterEach(() => {
  faults.length = 0;
});

const settled = (runtime: WatcherUserEventRuntime) => {
  let outcome: "pending" | "resolved" | "rejected" = "pending";
  runtime.done.then(
    () => (outcome = "resolved"),
    () => (outcome = "rejected"),
  );
  return () => outcome;
};

describe("user-event runtime under L1 transients", () => {
  it("waits out an unavailable source and publishes the touched block exactly once", async () => {
    const context = await setup();
    const { fixture, capturesOf } = context;
    const warn = vi.fn();
    let runtime: WatcherUserEventRuntime | null = null;
    try {
      runtime = await createWatcherUserEventRuntime({
        ...context.input,
        warn,
      });
      const done = settled(runtime);
      const touched = await context.touchedBlock();
      const before = (await fixture.readNativeQueries()).length;
      const casBefore = context.durable.casCount();
      faults.push(
        () => new LocalKupmiosTransportUnavailableError("Ogmios socket closed"),
        () => new LocalKupmiosTransportUnavailableError("Ogmios socket closed"),
      );
      await runtime.advanceThrough(touched.point);
      expect(faults).toHaveLength(0);
      expect(runtime.read()).toMatchObject({
        status: "ready",
        currentPoint: touched.point,
        headCursor: touched.point,
      });
      expect(done()).toBe("pending");
      // One warning for the outage, not one per attempt.
      expect(warn).toHaveBeenCalledOnce();
      expect(warn).toHaveBeenCalledWith({
        event: "user_event_l1_wait",
        error: "Ogmios socket closed",
        retryAfterMs: 250,
      });
      // The block is captured as if nothing failed, and published once.
      expect(
        capturesOf(
          (await fixture.readNativeQueries()).slice(before),
          touched.point.blockHash,
        ),
      ).toHaveLength(4);
      expect(context.durable.casCount() - casBefore).toBe(1);
    } finally {
      await runtime?.close();
      await context.close();
    }
  }, 120_000);

  it("still fails the runtime on an error that is not an L1 transient", async () => {
    const context = await setup();
    const warn = vi.fn();
    let runtime: WatcherUserEventRuntime | null = null;
    try {
      runtime = await createWatcherUserEventRuntime({
        ...context.input,
        warn,
      });
      const failed = expect(runtime.done).rejects.toThrow(
        "native capture parsed a non-canonical value",
      );
      const touched = await context.touchedBlock();
      // A name alone does not make an error the typed transient.
      faults.push(() =>
        Object.assign(
          new Error("native capture parsed a non-canonical value"),
          {
            name: "LocalKupmiosTransportUnavailableError",
          },
        ),
      );
      await expect(runtime.advanceThrough(touched.point)).rejects.toThrow(
        "native capture parsed a non-canonical value",
      );
      await failed;
      // A failed runtime closes its history: nothing reads from it again.
      expect(() => runtime!.read()).toThrow(
        "Local user-event history refused: history is closed",
      );
      expect(warn).not.toHaveBeenCalled();
    } finally {
      await runtime?.close();
      await context.close();
    }
  }, 120_000);
});
