import { PassThrough, Writable } from "node:stream";
import { pipeline } from "node:stream/promises";

import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { createDaAvailabilityReadScope } from "../src/availability-challenge-operation.read-scope.js";

beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(1000);
});
afterEach(() => {
  vi.useRealTimers();
});

describe("availability read attempt boundaries", () => {
  it("keeps protocol remainder finite across wall clock rollback", async () => {
    let wall = 1000;
    let mono = 0;
    const scope = createDaAvailabilityReadScope({
      deadlineEpochMs: 1100,
      attemptTimeoutMs: 200,
      nowMs: () => wall,
      monotonicMs: () => mono,
    });
    try {
      wall = 500;
      mono = 99;
      expect(scope.remainingMs()).toBe(1);
      mono = 100;
      await expect(scope.read(async () => "late")).rejects.toThrow(
        /deadline 1100 reached/,
      );
    } finally {
      scope.close();
    }
  });

  it("uses the remaining parent budget across nested reads and retries", async () => {
    const scope = createDaAvailabilityReadScope({
      monotonicMs: Date.now,
      attemptTimeoutMs: 100,
    });
    let secondStarted = false;
    try {
      const first = scope.read(async () => {
        await new Promise((resolve) => setTimeout(resolve, 70));
      });
      await vi.advanceTimersByTimeAsync(70);
      await first;
      const next = scope.read(async () =>
        scope.read(async () => {
          secondStarted = true;
          await new Promise((resolve) => setTimeout(resolve, 40));
          return "late";
        }),
      );
      const rejected = expect(next).rejects.toThrow(/attempt expired/);
      await vi.advanceTimersByTimeAsync(30);
      await rejected;
      expect(secondStarted).toBe(true);
      expect(scope.remainingMs()).toBe(0);
      await vi.advanceTimersByTimeAsync(10);
    } finally {
      scope.close();
    }
  });

  it("enforces a request cap without resetting or cancelling the remaining parent budget", async () => {
    const scope = createDaAvailabilityReadScope({
      monotonicMs: Date.now,
      attemptTimeoutMs: 100,
    });
    let child: AbortSignal | undefined;
    try {
      const request = scope.read(
        (signal) => {
          child = signal;
          return new Promise<string>(() => {});
        },
        { timeoutMs: 20 },
      );
      const rejected = expect(request).rejects.toThrow(/attempt expired/);
      await vi.advanceTimersByTimeAsync(20);
      await rejected;
      expect(child?.aborted).toBe(true);
      expect(scope.signal.aborted).toBe(false);
      expect(await scope.read(async () => "fresh read")).toBe("fresh read");
      expect(scope.remainingMs()).toBe(80);
    } finally {
      scope.close();
    }
  });

  it("cancels a real Node stream pipeline through the admitted AbortSignal", async () => {
    const scope = createDaAvailabilityReadScope({
      monotonicMs: Date.now,
      attemptTimeoutMs: 30,
    });
    const source = new PassThrough();
    const sink = new Writable({
      write(_chunk, _encoding, callback) {
        callback();
      },
    });
    try {
      const io = scope.read((signal) => pipeline(source, sink, { signal }));
      const rejected = expect(io).rejects.toThrow(/attempt expired/);
      await vi.advanceTimersByTimeAsync(30);
      await rejected;
      expect(source.destroyed).toBe(true);
      expect(sink.destroyed).toBe(true);
    } finally {
      scope.close();
      source.destroy();
      sink.destroy();
    }
  });

  it("fences a noncooperative late read before a following effect", async () => {
    const scope = createDaAvailabilityReadScope({
      monotonicMs: Date.now,
      attemptTimeoutMs: 20,
    });
    let resolveLate!: (value: string) => void;
    const effect = vi.fn();
    try {
      const consumer = async () => {
        const value = await scope.read(
          () =>
            new Promise<string>((resolve) => {
              resolveLate = resolve;
            }),
        );
        effect(value);
      };
      const read = consumer();
      const rejected = expect(read).rejects.toThrow(/attempt expired/);
      await vi.advanceTimersByTimeAsync(20);
      await rejected;
      resolveLate("stale");
      await vi.advanceTimersByTimeAsync(0);
      expect(effect).not.toHaveBeenCalled();
    } finally {
      scope.close();
    }
  });

  it("propagates explicit cancellation and prevents queued work after close", async () => {
    const controller = new AbortController();
    const scope = createDaAvailabilityReadScope({
      monotonicMs: Date.now,
      attemptTimeoutMs: 100,
      signal: controller.signal,
    });
    const reason = new Error("owner changed");
    try {
      const read = scope.read(() => new Promise<void>(() => {}));
      const rejected = expect(read).rejects.toBe(reason);
      controller.abort(reason);
      await rejected;
      expect(scope.signal.reason).toBe(reason);
    } finally {
      scope.close();
    }
    const closed = createDaAvailabilityReadScope({
      monotonicMs: Date.now,
      attemptTimeoutMs: 100,
    });
    const callback = vi.fn(async () => undefined);
    const queued = closed.read(callback);
    const rejected = expect(queued).rejects.toThrow(/scope closed/);
    closed.close();
    await rejected;
    expect(callback).not.toHaveBeenCalled();
  });

  it("rejects unbounded or invalid attempt/request policies before starting work", async () => {
    for (const attemptTimeoutMs of [0, -1, Infinity, NaN, 1.5])
      expect(() => createDaAvailabilityReadScope({ attemptTimeoutMs })).toThrow(
        /deadline\/cap/,
      );
    const scope = createDaAvailabilityReadScope({
      monotonicMs: Date.now,
      attemptTimeoutMs: 100,
    });
    const callback = vi.fn(async () => undefined);
    try {
      await expect(scope.read(callback, { timeoutMs: 0 })).rejects.toThrow(
        /request cap/,
      );
      expect(callback).not.toHaveBeenCalled();
    } finally {
      scope.close();
    }
  });
});
