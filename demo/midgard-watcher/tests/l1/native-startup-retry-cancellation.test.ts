import { expect, it, vi } from "vitest";

import { NativeChainSyncStartupFailure } from "../../src/l1/native-chain-sync.exact-record.js";
import { startWatcherNativeChainSyncWithRetry } from "../../src/l1/native-chain-sync.start-watcher-native-chain-sync-with-retry.js";
import { config, INTERSECTION } from "./native-chain-sync.config.js";

const { start } = vi.hoisted(() => ({ start: vi.fn() }));
vi.mock(
  "../../src/l1/native-chain-sync.open-watcher-native-exact-point-query.js",
  () => ({
    startWatcherNativeChainSync: start,
  }),
);

const input = (signal: AbortSignal) => ({
  binaryPath: "/test/native",
  watcherConfig: config(),
  intersectionCandidates: [INTERSECTION, { kind: "origin" as const }],
  startupTimeoutMs: 2000,
  onEvent: async () => {},
  signal,
});

it("cancels a retry wait with its owned reason and never starts another candidate", async () => {
  const controller = new AbortController();
  const reason = new Error("owned retry stopped");
  start
    .mockReset()
    .mockRejectedValueOnce(
      new NativeChainSyncStartupFailure("node_handshake_failed"),
    )
    .mockRejectedValue(
      new Error("unexpected attempt after owner cancellation"),
    );
  const pending = startWatcherNativeChainSyncWithRetry({
    ...input(controller.signal),
    warn: () => queueMicrotask(() => controller.abort(reason)),
    retryDelayMs: () => 100,
  });
  await expect(pending).rejects.toBe(reason);
  expect(start).toHaveBeenCalledTimes(1);
});

it("does not reinterpret an intrinsic failure when termination arrives concurrently", async () => {
  const controller = new AbortController();
  const failure = new NativeChainSyncStartupFailure("genesis_mismatch");
  start.mockReset().mockImplementation(async () => {
    controller.abort(new Error("owned shutdown"));
    throw failure;
  });
  await expect(
    startWatcherNativeChainSyncWithRetry(input(controller.signal)),
  ).rejects.toBe(failure);
  expect(start).toHaveBeenCalledTimes(1);
});

it("does not reinterpret a warning failure as cancellation of a wait that never began", async () => {
  const controller = new AbortController();
  const failure = new NativeChainSyncStartupFailure("node_handshake_failed");
  start.mockReset().mockRejectedValue(failure);
  await expect(
    startWatcherNativeChainSyncWithRetry({
      ...input(controller.signal),
      warn: () => {
        controller.abort(new Error("owned shutdown"));
        throw failure;
      },
    }),
  ).rejects.toBe(failure);
  expect(start).toHaveBeenCalledTimes(1);
});

it.each(["generic", "intersection_failed"])(
  "joins a ready runtime and preserves its %s close failure over cancellation",
  async (kind) => {
    const controller = new AbortController();
    const failure =
      kind === "generic"
        ? new Error("owned native drain failed")
        : new NativeChainSyncStartupFailure("intersection_failed");
    const close = vi.fn(async () => {
      throw failure;
    });
    start.mockReset().mockImplementation(async () => {
      controller.abort(new Error("owned shutdown"));
      return { close };
    });
    await expect(
      startWatcherNativeChainSyncWithRetry(input(controller.signal)),
    ).rejects.toBe(failure);
    expect(close).toHaveBeenCalledTimes(1);
    expect(start).toHaveBeenCalledTimes(1);
  },
);

it("refuses a retired owner before any native attempt", async () => {
  const controller = new AbortController();
  controller.abort(new Error("already stopped"));
  start.mockReset().mockRejectedValue(new Error("unexpected spawn"));
  await expect(
    startWatcherNativeChainSyncWithRetry(input(controller.signal)),
  ).rejects.toBe(controller.signal.reason);
  expect(start).not.toHaveBeenCalled();
});

it("joins an owner retired between the candidate walk and public retry return", async () => {
  const controller = new AbortController();
  const reason = new Error("owned public return retired");
  const close = vi.fn(async () => {});
  const runtime = { close };
  start.mockReset().mockImplementation(() =>
    Promise.resolve().then(() => {
      queueMicrotask(() => queueMicrotask(() => controller.abort(reason)));
      return runtime;
    }),
  );
  await expect(
    startWatcherNativeChainSyncWithRetry(input(controller.signal)),
  ).rejects.toBe(reason);
  expect(close).toHaveBeenCalledTimes(1);
  expect(start).toHaveBeenCalledTimes(1);
});
