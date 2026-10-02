import { LocalKupmiosTransportUnavailableError } from "@al-ft/midgard-fault-proofs";
import { describe, expect, it, vi } from "vitest";

import {
  retryWatcherL1Transient,
  watcherL1TransientRetryDelayMs,
} from "../../src/l1/transient-retry.js";

const transient = () =>
  new LocalKupmiosTransportUnavailableError("Ogmios socket closed");

describe("L1 transient retry", () => {
  it("repeats the attempt through a transient and returns its value exactly once", async () => {
    const attempt = vi
      .fn<() => Promise<string>>()
      .mockRejectedValueOnce(transient())
      .mockRejectedValueOnce(transient())
      .mockResolvedValueOnce("read");
    const onRetry = vi.fn();
    await expect(
      retryWatcherL1Transient(attempt, { onRetry, delayMs: () => 1 }),
    ).resolves.toBe("read");
    expect(attempt).toHaveBeenCalledTimes(3);
    expect(onRetry.mock.calls.map(([, retry]) => retry)).toEqual([1, 2]);
  });

  it.each([
    ["a genuine refusal", new Error("Kupo value JSON is not canonical")],
    [
      "a forged transient name",
      Object.assign(new Error("Ogmios socket closed"), {
        name: "LocalKupmiosTransportUnavailableError",
      }),
    ],
  ])("rethrows %s at once without retrying", async (_label, failure) => {
    const attempt = vi.fn(async () => {
      throw failure;
    });
    const onRetry = vi.fn();
    await expect(retryWatcherL1Transient(attempt, { onRetry })).rejects.toBe(
      failure,
    );
    expect(attempt).toHaveBeenCalledOnce();
    expect(onRetry).not.toHaveBeenCalled();
  });

  it("stops waiting when its signal aborts and rethrows the transient", async () => {
    const controller = new AbortController();
    const failure = transient();
    const attempt = vi.fn(async () => {
      throw failure;
    });
    const outcome = retryWatcherL1Transient(attempt, {
      signal: controller.signal,
      delayMs: () => 60_000,
    });
    await vi.waitFor(() => expect(attempt).toHaveBeenCalledOnce());
    controller.abort();
    await expect(outcome).rejects.toBe(failure);
    // No attempt starts after the abort.
    expect(attempt).toHaveBeenCalledOnce();
  });

  it("spaces attempts from 250 ms doubling to 30 s", () => {
    expect([1, 2, 3, 7, 8, 9, 100].map(watcherL1TransientRetryDelayMs)).toEqual(
      [250, 500, 1_000, 16_000, 30_000, 30_000, 30_000],
    );
  });
});
