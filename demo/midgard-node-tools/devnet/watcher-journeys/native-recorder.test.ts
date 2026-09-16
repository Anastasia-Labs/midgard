import { setTimeout as pause } from "node:timers/promises";

import type {
  startWatcherNativeChainSync,
  WatcherConfig,
  WatcherNativeChainSyncEvent,
} from "midgard-watcher";
import { NativeTransactionNotIncludedError } from "midgard-watcher/tests/support/published-da-target-consumption";
import { afterEach, beforeEach, expect, it, vi } from "vitest";

import { startJourneyNativeRecorder } from "./native-recorder.js";

const io = vi.hoisted(() => ({
  start: vi.fn(),
  admit: vi.fn(),
  authority: vi.fn(() => ({})),
  append: vi.fn(async (_path: string, _bytes: string) => undefined),
}));
vi.mock("node:fs/promises", () => ({ appendFile: io.append }));
vi.mock("midgard-watcher", () => ({
  startWatcherNativeChainSync: io.start,
  admitWatcherNativeRollForwardBlock: io.admit,
  watcherNativeChainSyncAuthorityDetails: io.authority,
}));

beforeEach(() => {
  vi.clearAllMocks();
  io.authority.mockReturnValue({});
});
afterEach(() => vi.restoreAllMocks());

it("revokes the old fork height before rollback I/O and restores it only from an admitted forward", async () => {
  let onEvent!: Parameters<typeof startWatcherNativeChainSync>[0]["onEvent"];
  io.start.mockImplementation(async (input) => {
    onEvent = input.onEvent;
    return { done: Promise.resolve(), close: async () => undefined };
  });
  let releaseRollback!: () => void;
  const rollbackPending = new Promise<void>((resolve) => {
    releaseRollback = resolve;
  });
  let enteredRollback!: () => void;
  const rollbackEntered = new Promise<void>((resolve) => {
    enteredRollback = resolve;
  });
  const recorder = await startJourneyNativeRecorder({
    directory: "/unused",
    watcherConfig: {} as WatcherConfig,
    binaryPath: "/unused",
    onRollback: async () => {
      enteredRollback();
      await rollbackPending;
    },
  });
  const forward = async (blockNo: number) => {
    const point = {
      blockNo: String(blockNo),
      slot: String(blockNo),
      blockHash: "aa".repeat(32),
    };
    io.admit.mockReturnValueOnce({
      ...point,
      transactionIds: [],
      transactionCbors: [],
    });
    await onEvent({
      kind: "roll_forward",
      tip: { kind: "point", ...point },
    } as WatcherNativeChainSyncEvent);
  };
  await forward(35);
  expect(recorder.observedBlockNo()).toBe(35n);
  // An inclusion at block 6 looked depth 30 on the discarded branch.
  expect(recorder.observedBlockNo()! - 6n + 1n).toBe(30n);
  const rollingBack = onEvent({
    schemaVersion: "midgard-watcher-native-chain-sync-v1",
    kind: "roll_backward",
    point: { kind: "point", slot: "33", blockHash: "bb".repeat(32) },
    tip: {
      kind: "point",
      blockNo: "34",
      slot: "34",
      blockHash: "cc".repeat(32),
    },
  });
  // The event handler has yielded at its first append, before the rollback callback.
  expect(recorder.observedBlockNo()).toBeUndefined();
  await rollbackEntered;
  expect(recorder.observedBlockNo()).toBeUndefined();
  releaseRollback();
  await rollingBack;
  expect(recorder.observedBlockNo()).toBeUndefined();
  await forward(34);
  expect(recorder.observedBlockNo()).toBe(34n);
  expect(recorder.observedBlockNo()! - 6n + 1n).toBe(29n);
  await recorder.close();
});

it("retains shared family transactions and records a rollback before re-inclusion without restarting", async () => {
  let onEvent!: Parameters<typeof startWatcherNativeChainSync>[0]["onEvent"];
  const close = vi.fn(async () => undefined);
  io.start.mockImplementation(async (input) => {
    onEvent = input.onEvent;
    return { done: Promise.resolve(), close };
  });
  const onBlock = vi.fn(async () => undefined);
  let releaseRollback!: () => void;
  const rollbackPending = new Promise<void>((resolve) => {
    releaseRollback = resolve;
  });
  const onRollback = vi.fn(async () => await rollbackPending);
  const recorder = await startJourneyNativeRecorder({
    directory: "/batch/session-1",
    watcherConfig: {} as WatcherConfig,
    binaryPath: "/unused",
    onBlock,
    onRollback,
  });
  const forward = async (blockNo: number, hash: string, txHash: string) => {
    const point = {
      blockNo: String(blockNo),
      slot: String(blockNo),
      blockHash: hash.repeat(32),
    };
    io.admit.mockReturnValueOnce({
      ...point,
      transactionIds: [txHash],
      transactionCbors: ["original-transaction-bytes"],
    });
    await onEvent({
      kind: "roll_forward",
      tip: { kind: "point", ...point },
    } as WatcherNativeChainSyncEvent);
    return point;
  };
  const retainedId = "11".repeat(32);
  const replacedId = "22".repeat(32);
  try {
    const firstFamilyPoint = await forward(30, "aa", retainedId);
    expect((await recorder.transaction(retainedId)).point).toEqual(
      firstFamilyPoint,
    );
    await forward(31, "bb", replacedId);
    // The next family keeps the same recorder and its complete transaction history.
    expect((await recorder.transaction(retainedId)).point).toEqual(
      firstFamilyPoint,
    );
    const rollingBack = onEvent({
      schemaVersion: "midgard-watcher-native-chain-sync-v1",
      kind: "roll_backward",
      point: { kind: "point", slot: "30", blockHash: "aa".repeat(32) },
      tip: {
        kind: "point",
        blockNo: "31",
        slot: "31",
        blockHash: "cc".repeat(32),
      },
    });
    expect(recorder.observedBlockNo()).toBeUndefined();
    expect((await recorder.transaction(retainedId)).point).toEqual(
      firstFamilyPoint,
    );
    let staleResolved = false;
    const pending = recorder.transaction(replacedId).then((transaction) => {
      staleResolved = true;
      return transaction;
    });
    await pause(10);
    expect(staleResolved).toBe(false);
    expect(onRollback).toHaveBeenCalledOnce();
    releaseRollback();
    await rollingBack;
    const replacementPoint = await forward(31, "cc", replacedId);
    expect((await pending).point).toEqual(replacementPoint);
    expect(recorder.nativeEvidencePath).toBe(
      "/batch/session-1/native-chain.ndjson",
    );
    expect(io.append.mock.calls.map(([path]) => path)).toEqual(
      Array(4).fill(recorder.nativeEvidencePath),
    );
    expect(onBlock).toHaveBeenCalledTimes(3);
    expect(onRollback).toHaveBeenCalledOnce();
    expect(io.start).toHaveBeenCalledOnce();
    expect(close).not.toHaveBeenCalled();
  } finally {
    releaseRollback();
    await recorder.close();
  }
  expect(close).toHaveBeenCalledOnce();
});

it("does not report caught-up coverage before the block archive and transactions are ready", async () => {
  let onEvent!: Parameters<typeof startWatcherNativeChainSync>[0]["onEvent"];
  io.start.mockImplementation(async (input) => {
    onEvent = input.onEvent;
    return { done: Promise.resolve(), close: async () => undefined };
  });
  let finish!: () => void;
  const held = new Promise<void>((resolve) => {
    finish = resolve;
  });
  const recorder = await startJourneyNativeRecorder({
    directory: "/unused",
    watcherConfig: {} as WatcherConfig,
    binaryPath: "/unused",
    onBlock: async () => held,
  });
  const point = { blockNo: "10", slot: "10", blockHash: "aa".repeat(32) };
  const hash = "bb".repeat(32);
  io.admit.mockReturnValue({
    ...point,
    transactionIds: [hash],
    transactionCbors: ["bytes"],
  });
  const advancing = onEvent({
    kind: "roll_forward",
    tip: { kind: "point", ...point },
  } as WatcherNativeChainSyncEvent);
  await Promise.resolve();
  expect(recorder.observedBlockNo()).toBeUndefined();
  finish();
  await advancing;
  expect(recorder.observedBlockNo()).toBe(10n);
  await expect(recorder.transaction(hash)).resolves.toMatchObject({
    cbor: "bytes",
  });
  io.authority.mockReturnValueOnce(null as never);
  await expect(recorder.transaction(hash)).rejects.toThrow(
    "authority is inactive",
  );
});

it("resets native absence waiting when coverage falls behind or rolls back", async () => {
  let onEvent!: Parameters<typeof startWatcherNativeChainSync>[0]["onEvent"];
  io.start.mockImplementation(async (input) => {
    onEvent = input.onEvent;
    return { done: Promise.resolve(), close: async () => undefined };
  });
  const recorder = await startJourneyNativeRecorder({
    directory: "/unused",
    watcherConfig: {} as WatcherConfig,
    binaryPath: "/unused",
  });
  const forward = async (height: number, tipHeight = height) => {
    io.admit.mockReturnValue({
      blockNo: String(height),
      slot: String(height),
      blockHash: "aa".repeat(32),
      transactionIds: [],
      transactionCbors: [],
    });
    await onEvent({
      kind: "roll_forward",
      tip: {
        kind: "point",
        blockNo: String(tipHeight),
        slot: String(tipHeight),
        blockHash: "aa".repeat(32),
      },
    } as WatcherNativeChainSyncEvent);
  };
  let time = 0;
  vi.spyOn(Date, "now").mockImplementation(() => time);
  await forward(10);
  let failure: unknown;
  const missingHash = "cc".repeat(32);
  const pending = recorder.transaction(missingHash).catch((error: unknown) => {
    failure = error;
  });
  time = 100_000;
  await forward(11, 20);
  await pause(300);
  expect(failure).toBeUndefined();
  await forward(20);
  await pause(300);
  // Rollback and recovery happen between polls, so generation must reset too.
  await onEvent({
    kind: "roll_backward",
    point: { kind: "point", slot: "19", blockHash: "aa".repeat(32) },
    tip: {
      kind: "point",
      blockNo: "20",
      slot: "20",
      blockHash: "aa".repeat(32),
    },
  } as WatcherNativeChainSyncEvent);
  await forward(20);
  time = 200_000;
  await pause(300);
  expect(failure).toBeUndefined();
  time = 300_000;
  await pending;
  expect(failure).toBeInstanceOf(NativeTransactionNotIncludedError);
  expect(failure).toMatchObject({ txHash: missingHash });
});
