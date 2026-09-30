import { setTimeout as delay } from "node:timers/promises";

import { describe, expect, it, vi } from "vitest";

import * as historicalCapture from "../../src/l1/local-historical-capture.js";
import { startControlledRuntime } from "./user-event-runtime.user-event-runtime-coverage-and-capture.js";
import { waitForNativeQueries } from "./user-event-runtime.wait-for-native-queries.js";

describe("user-event pending native growth", () => {
  it("keeps one-pair reacquisition pending past 120 seconds and remains cancellable", async () => {
    const { context, runtime } = await startControlledRuntime();
    const realNow = performance.now.bind(performance);
    let elapsed = 0;
    const clock = vi
      .spyOn(performance, "now")
      .mockImplementation(() => realNow() + elapsed);
    try {
      const point = runtime.read().headCursor;
      const before = context.capturesOf(
        await context.fixture.readNativeQueries(),
        point.blockHash,
      ).length;
      let settled = false;
      const pending = runtime.handleRollback({
        kind: "point",
        blockHash: point.blockHash,
        slot: point.slot,
      });
      void pending.then(
        () => {
          settled = true;
        },
        () => {
          settled = true;
        },
      );
      await waitForNativeQueries(context, point.blockHash, before + 4);
      elapsed += 130_000;
      await delay(1_200);
      expect(settled).toBe(false);
      expect(runtime.read().status).toBe("suspended");
      await context.fixture.growNativeTip();
      await pending;
      expect(runtime.read()).toMatchObject({ status: "ready", generation: 1 });
      expect(runtime.read().headCursor).toEqual(point);

      const queries = context.capturesOf(
        await context.fixture.readNativeQueries(),
        point.blockHash,
      ).length;
      const cancelled = runtime.handleRollback({
        kind: "point",
        blockHash: point.blockHash,
        slot: point.slot,
      });
      const rejected = expect(cancelled).rejects.toThrow(
        /aborted|closed|cancelled/u,
      );
      await waitForNativeQueries(context, point.blockHash, queries + 4);
      await runtime.close();
      await rejected;
      expect(runtime.read().status).toBe("closed");
    } finally {
      await runtime.close();
      clock.mockRestore();
      await context.close();
    }
  }, 120_000);

  it("publishes the fresh batch prefix before a later long gap and retains unused first facts", async () => {
    const { context, runtime } = await startControlledRuntime();
    const realNow = performance.now.bind(performance);
    let elapsed = 0;
    const clock = vi
      .spyOn(performance, "now")
      .mockImplementation(() => realNow() + elapsed);
    const open = historicalCapture.openWatcherLocalHistoricalCapture;
    let growBeforeSecondFirst = true;
    let activeCaptures = 0;
    let gapProbeClosed = false;
    let crossedLongGap = false;
    const first = await context.touchedBlock();
    const second = await context.touchedBlock();
    await context.fixture.setNativeTip(second.point);
    await context.fixture.growNativeTip(35);
    const capture = vi
      .spyOn(historicalCapture, "openWatcherLocalHistoricalCapture")
      .mockImplementation(async (input) => {
        if (
          input.point.blockHash === second.point.blockHash &&
          growBeforeSecondFirst
        ) {
          growBeforeSecondFirst = false;
          await context.fixture.growNativeTip();
        }
        // Advance time between owned captures. Counting native query starts
        // and sleeping can otherwise jump time during a still-live receipt.
        if (
          !crossedLongGap &&
          input.point.blockHash === second.point.blockHash &&
          runtime.read().headCursor?.blockHash === first.point.blockHash
        ) {
          elapsed += 130_000;
          crossedLongGap = true;
        }
        const afterGap = crossedLongGap;
        const acquired = await open(input);
        activeCaptures += 1;
        let closed = false;
        return {
          ...acquired,
          close: async () => {
            await acquired.close();
            if (!closed) {
              closed = true;
              activeCaptures -= 1;
              if (afterGap) gapProbeClosed = true;
            }
          },
        };
      });
    try {
      let settled = false;
      const pending = runtime.advanceThrough(second.point);
      void pending.then(
        () => {
          settled = true;
        },
        () => {
          settled = true;
        },
      );
      await vi.waitFor(
        () => expect(runtime.read().headCursor).toEqual(first.point),
        { timeout: 20_000 },
      );
      await vi.waitFor(
        () => {
          expect(gapProbeClosed).toBe(true);
          expect(activeCaptures).toBe(0);
        },
        { timeout: 20_000 },
      );
      expect(settled).toBe(false);
      expect(runtime.read().status).toBe("ready");
      expect(runtime.read().headCursor).toEqual(first.point);
      await context.fixture.growNativeTip();
      await pending;
      expect(runtime.read()).toMatchObject({
        currentPoint: second.point,
        headCursor: second.point,
      });
      expect(activeCaptures).toBe(0);
      // Returning A's fresh prefix preserves B's closed first facts across
      // the later block-production gap.
      expect(
        capture.mock.calls.filter(
          ([input]) => input.point.blockHash === first.point.blockHash,
        ),
      ).toHaveLength(2);
    } finally {
      await runtime.close();
      capture.mockRestore();
      clock.mockRestore();
      await context.close();
    }
  }, 120_000);

  it("renews consumed first facts after a partial pair expires and requires new tip growth", async () => {
    const { context, runtime } = await startControlledRuntime();
    const realNow = performance.now.bind(performance);
    let elapsed = 0;
    const clock = vi
      .spyOn(performance, "now")
      .mockImplementation(() => realNow() + elapsed);
    const open = historicalCapture.openWatcherLocalHistoricalCapture;
    const initialHead = runtime.read().headCursor;
    const first = await context.touchedBlock();
    const second = await context.touchedBlock();
    await context.fixture.setNativeTip(second.point);
    await context.fixture.growNativeTip(35);
    let firstCaptures = 0;
    let secondCaptures = 0;
    let activeCaptures = 0;
    const capture = vi
      .spyOn(historicalCapture, "openWatcherLocalHistoricalCapture")
      .mockImplementation(async (input) => {
        if (input.point.blockHash === first.point.blockHash) firstCaptures += 1;
        if (input.point.blockHash === second.point.blockHash) {
          secondCaptures += 1;
          if (secondCaptures === 1) await context.fixture.growNativeTip();
          if (secondCaptures === 2) {
            // A has an admitted, live second observation. Acquiring B after
            // that lifetime cannot renew A's authority or reuse its predecessor.
            expect(activeCaptures).toBe(1);
            elapsed += 130_000;
          }
        }
        const acquired = await open(input);
        activeCaptures += 1;
        let closed = false;
        return {
          ...acquired,
          close: async () => {
            await acquired.close();
            if (!closed) {
              closed = true;
              activeCaptures -= 1;
            }
          },
        };
      });
    try {
      let settled = false;
      let failure: unknown;
      const pending = runtime.advanceThrough(second.point);
      void pending.then(
        () => {
          settled = true;
        },
        (error: unknown) => {
          settled = true;
          failure = error;
        },
      );
      await vi.waitFor(
        () => {
          if (failure !== undefined) throw failure;
          expect(firstCaptures).toBeGreaterThanOrEqual(4);
          expect(activeCaptures).toBe(0);
        },
        { timeout: 20_000 },
      );
      await waitForNativeQueries(context, first.point.blockHash, 8);
      expect(activeCaptures).toBe(0);
      expect(settled).toBe(false);
      expect(runtime.read().headCursor).toEqual(initialHead);
      // Renewed A's first observation consumed the current tip. A real newer
      // native tip is required before either point can be published.
      await context.fixture.growNativeTip();
      await pending;
      expect(runtime.read()).toMatchObject({
        status: "ready",
        currentPoint: second.point,
        headCursor: second.point,
      });
      expect(firstCaptures).toBeGreaterThanOrEqual(5);
      expect(secondCaptures).toBe(3);
      expect(activeCaptures).toBe(0);
    } finally {
      await runtime.close();
      capture.mockRestore();
      clock.mockRestore();
      await context.close();
    }
  }, 120_000);

  it("rejects changed block bytes after a pending gap instead of retrying admission failure", async () => {
    const { context, runtime } = await startControlledRuntime();
    const realNow = performance.now.bind(performance);
    let elapsed = 0;
    const clock = vi
      .spyOn(performance, "now")
      .mockImplementation(() => realNow() + elapsed);
    const read = historicalCapture.readWatcherLocalHistoricalCapture;
    let changed = false;
    const target = await context.touchedBlock();
    await context.fixture.setNativeTip(target.point);
    await context.fixture.growNativeTip(35);
    const capture = vi
      .spyOn(historicalCapture, "readWatcherLocalHistoricalCapture")
      .mockImplementation((receipt) => {
        const value = read(receipt);
        return changed && value.point.blockHash === target.point.blockHash
          ? {
              ...value,
              nativeBlock: { ...value.nativeBlock, rawBlockCbor: "ff" },
            }
          : value;
      });
    try {
      const pending = runtime.advanceThrough(target.point);
      const rejected = expect(pending).rejects.toThrow(
        "native block changed between observations",
      );
      await waitForNativeQueries(context, target.point.blockHash, 4);
      elapsed += 130_000;
      await delay(1_200);
      changed = true;
      await context.fixture.growNativeTip();
      await rejected;
      expect(() => runtime.read()).toThrow("history is closed");
      await expect(runtime.done).rejects.toThrow(
        "native block changed between observations",
      );
    } finally {
      await runtime.close();
      capture.mockRestore();
      clock.mockRestore();
      await context.close();
    }
  }, 120_000);
});
