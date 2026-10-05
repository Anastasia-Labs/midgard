import { createDaAvailabilityReadScope } from "@al-ft/midgard-sdk";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { LocalKupmiosTransportUnavailableError } from "../src/workflow/local-kupmios-http-ogmios-source.read-admitted-local-kupmios-signed-transaction-recovery.js";
import {
  type LocalKupmiosFraudProofRawSource,
  withLocalKupmiosSourceCapture,
} from "../src/workflow/local-kupmios-raw-l1-authority.js";
import {
  type LocalKupmiosReadAttempt,
  LocalKupmiosReadGenerationExpiredError,
  withLocalKupmiosReadOperation,
} from "../src/workflow/local-kupmios-read-operation.js";

// Queue/retry tests need only identity; concrete transport tests are separate.
const source = () => ({}) as LocalKupmiosFraudProofRawSource;
const scope = (attemptTimeoutMs = 100) =>
  createDaAvailabilityReadScope({ attemptTimeoutMs, monotonicMs: Date.now });
beforeEach(() => {
  vi.useFakeTimers();
  vi.setSystemTime(1000);
});
afterEach(() => {
  vi.useRealTimers();
});

describe("owning local Kupmios read operations", () => {
  it("restarts the whole read-only operation and invalidates the prior generation", async () => {
    const owner = source();
    const budget = scope();
    let old: LocalKupmiosReadAttempt | undefined;
    const steps: string[] = [];
    try {
      const result = await withLocalKupmiosReadOperation(
        owner,
        async (attempt) => {
          steps.push(`boundary-${attempt.generation}`);
          if (attempt.generation === 0) {
            old = attempt;
            steps.push("partial-old-evidence");
            throw new LocalKupmiosTransportUnavailableError("peer closed");
          }
          expect(() => old!.assertCurrent()).toThrow(
            LocalKupmiosReadGenerationExpiredError,
          );
          steps.push("new-evidence");
          return { generation: attempt.generation, evidence: "new-evidence" };
        },
        { scope: budget },
      );
      expect(result).toEqual({ generation: 1, evidence: "new-evidence" });
      expect(steps).toEqual([
        "boundary-0",
        "partial-old-evidence",
        "boundary-1",
        "new-evidence",
      ]);
      expect(() => old!.assertCurrent()).toThrow(
        LocalKupmiosReadGenerationExpiredError,
      );
    } finally {
      budget.close();
    }
  });

  it("ends after three typed transport failures", async () => {
    const budget = scope();
    const errors = [0, 1, 2].map(
      (n) => new LocalKupmiosTransportUnavailableError(`failure-${n}`),
    );
    const read = vi.fn(async () => {
      throw errors[read.mock.calls.length - 1];
    });
    try {
      await expect(
        withLocalKupmiosReadOperation(source(), read, { scope: budget }),
      ).rejects.toBe(errors[2]);
      expect(read).toHaveBeenCalledTimes(3);
    } finally {
      budget.close();
    }
  });

  it.each([
    new Error("MAC mismatch"),
    new Error("topology mismatch"),
    new Error("protocol response refused"),
    Object.assign(new Error("lookalike"), {
      name: "LocalKupmiosTransportUnavailableError",
    }),
  ])("never retries integrity or name-only errors: %s", async (error) => {
    const budget = scope();
    const read = vi.fn(async () => {
      throw error;
    });
    try {
      await expect(
        withLocalKupmiosReadOperation(source(), read, { scope: budget }),
      ).rejects.toBe(error);
      expect(read).toHaveBeenCalledTimes(1);
    } finally {
      budget.close();
    }
  });

  it("keeps one absolute remainder across retries and fences the late second result", async () => {
    const budget = scope(100);
    let resolveSecond!: (value: string) => void;
    const read = vi.fn(async (attempt) => {
      if (attempt.generation === 0) {
        await new Promise((resolve) => setTimeout(resolve, 60));
        throw new LocalKupmiosTransportUnavailableError("transient");
      }
      return new Promise<string>((resolve) => {
        resolveSecond = resolve;
      });
    });
    try {
      const run = withLocalKupmiosReadOperation(source(), read, {
        scope: budget,
      });
      const rejected = expect(run).rejects.toThrow(/attempt expired/);
      await vi.advanceTimersByTimeAsync(100);
      await rejected;
      expect(read).toHaveBeenCalledTimes(2);
      expect(budget.remainingMs()).toBe(0);
      resolveSecond("late");
      await vi.advanceTimersByTimeAsync(0);
      expect(read).toHaveBeenCalledTimes(2);
    } finally {
      budget.close();
    }
  });

  it("does not release active source ownership when cancellation wins the outer race", async () => {
    const owner = source();
    const budget = scope(20);
    let finish!: (value: string) => void;
    const next = vi.fn(async () => "next");
    try {
      const first = withLocalKupmiosSourceCapture(
        owner,
        () =>
          new Promise<string>((resolve) => {
            finish = resolve;
          }),
        budget,
      );
      let expiredAtBoundary = false;
      void first.catch(() => {
        expiredAtBoundary = true;
      });
      const rejected = expect(first).rejects.toThrow(/attempt expired/);
      await vi.advanceTimersByTimeAsync(0);
      const following = withLocalKupmiosSourceCapture(owner, next);
      await vi.advanceTimersByTimeAsync(20);
      expect(expiredAtBoundary).toBe(true);
      await rejected;
      expect(next).not.toHaveBeenCalled();
      finish("stale");
      expect(await following).toBe("next");
      expect(next).toHaveBeenCalledTimes(1);
    } finally {
      budget.close();
    }
  });

  it("keeps queued cancellation behind its predecessor and skips the cancelled callback", async () => {
    const owner = source();
    let finish!: (value: string) => void;
    const pending = withLocalKupmiosSourceCapture(
      owner,
      () =>
        new Promise<string>((resolve) => {
          finish = resolve;
        }),
    );
    await vi.advanceTimersByTimeAsync(0);
    const budget = scope(20);
    const cancelled = vi.fn(async () => "cancelled");
    const next = vi.fn(async () => "next");
    try {
      const queued = withLocalKupmiosSourceCapture(owner, cancelled, budget);
      const rejected = expect(queued).rejects.toThrow(/attempt expired/);
      const following = withLocalKupmiosSourceCapture(owner, next);
      await vi.advanceTimersByTimeAsync(20);
      await rejected;
      expect(cancelled).not.toHaveBeenCalled();
      expect(next).not.toHaveBeenCalled();
      finish("predecessor");
      await pending;
      expect(await following).toBe("next");
      expect(cancelled).not.toHaveBeenCalled();
    } finally {
      budget.close();
    }
  });

  it("rejects already-cancelled operations without starting a capture", async () => {
    const owner = source();
    const budget = scope();
    budget.close();
    const read = vi.fn(async () => "unexpected");
    await expect(
      withLocalKupmiosReadOperation(owner, read, { scope: budget }),
    ).rejects.toThrow(/scope closed/);
    await vi.advanceTimersByTimeAsync(0);
    expect(read).not.toHaveBeenCalled();
    expect(await withLocalKupmiosSourceCapture(owner, async () => "next")).toBe(
      "next",
    );
  });
});
