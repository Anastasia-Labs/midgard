import { describe, expect, it, vi } from "vitest";

import {
  DaRequestTimeoutError,
  withDaRequestDeadline,
} from "../src/da-request-deadline.js";

describe("DA request absolute deadline", () => {
  it("covers open and the entire stream exchange and aborts a stalled stream", async () => {
    const abort = vi.fn();
    const stream = {};
    const startedAt = Date.now();
    await expect(
      withDaRequestDeadline({
        timeoutMs: 25,
        open: async () => stream,
        run: async () => new Promise<never>(() => undefined),
        abort,
      }),
    ).rejects.toBeInstanceOf(DaRequestTimeoutError);
    expect(Date.now() - startedAt).toBeLessThan(250);
    expect(abort).toHaveBeenCalledOnce();
  });

  it("passes one signal through open and exchange", async () => {
    let openedSignal: AbortSignal | undefined;
    let runSignal: AbortSignal | undefined;
    await expect(
      withDaRequestDeadline({
        timeoutMs: 1_000,
        open: async (signal) => {
          openedSignal = signal;
          return "stream";
        },
        run: async (_stream, signal) => {
          runSignal = signal;
          return "ok";
        },
        abort: () => undefined,
      }),
    ).resolves.toBe("ok");
    expect(openedSignal).toBe(runSignal);
    expect(openedSignal?.aborted).toBe(false);
  });

  it("aborts a non-cooperative late-opened stream without starting exchange work", async () => {
    let resolveOpen!: (stream: object) => void;
    const open = new Promise<object>((resolve) => {
      resolveOpen = resolve;
    });
    const run = vi.fn(async () => "unexpected");
    const abort = vi.fn();
    const request = withDaRequestDeadline({
      timeoutMs: 20,
      open: async () => open,
      run,
      abort,
    });

    await expect(request).rejects.toBeInstanceOf(DaRequestTimeoutError);
    const lateStream = {};
    resolveOpen(lateStream);
    await new Promise((resolve) => setTimeout(resolve, 0));

    expect(run).not.toHaveBeenCalled();
    expect(abort).toHaveBeenCalledOnce();
    expect(abort).toHaveBeenCalledWith(
      lateStream,
      expect.any(DaRequestTimeoutError),
    );
  });
});
