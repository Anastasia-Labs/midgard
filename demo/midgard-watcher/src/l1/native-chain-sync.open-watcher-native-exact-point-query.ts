import { performance } from "node:perf_hooks";

import { parseWatcherConfig } from "../runtime/config.js";
import { type NativeStreamInput } from "./native-chain-sync.derive-watcher-native-genesis-identity.js";
import {
  exactRecord,
  HEX_32,
  MAX_UINT64,
  type NativeBlockPoint,
  NATURAL,
  readWatcherNativeChainSyncEventReceipt,
  string,
  type WatcherNativeChainSyncAuthority,
  type WatcherNativeChainSyncEventReceipt,
  watcherNativeChainSyncEventReceipt,
  type WatcherNativeChainSyncRollForward,
  type WatcherNativeChainSyncRuntime,
} from "./native-chain-sync.exact-record.js";
import { startNativeSupervisor } from "./native-chain-sync.start-native-supervisor.js";

export const startWatcherNativeChainSync = async (
  input: NativeStreamInput,
): Promise<WatcherNativeChainSyncRuntime> => {
  const { intersection, ...rest } = input;
  return await startNativeSupervisor({
    ...rest,
    intersections: [intersection],
    operation: Object.freeze({ kind: "stream" }),
  });
};

const queryReceiptBrand = Symbol("native-exact-point-query-receipt");

export type WatcherNativeExactPointQueryReceipt = Readonly<{
  [queryReceiptBrand]: true;
}>;

type NativeExactPointQueryRead = Readonly<{
  authority: WatcherNativeChainSyncAuthority;
  eventReceipt: WatcherNativeChainSyncEventReceipt;
  startupDigest: string;
  eventDigest: string;
  event: WatcherNativeChainSyncRollForward;
  observedTip: NativeBlockPoint;
  depthAtObservedTip: string;
  observedAt: string;
  expiresAt: string;
}>;

const queryReceipts = new WeakMap<
  WatcherNativeExactPointQueryReceipt,
  Readonly<{ value: NativeExactPointQueryRead; assertLive(): void }>
>();

/**
 * A bounded exact-query snapshot, not a passive rollback monitor. A new query
 * is required for fresh canonical corroboration; this read only checks the
 * original acquisition's process-local liveness and monotonic deadline.
 */
export const readWatcherNativeExactPointQuery = (
  receipt: WatcherNativeExactPointQueryReceipt,
): NativeExactPointQueryRead => {
  const state = queryReceipts.get(receipt);
  if (state === undefined) {
    throw new Error("native exact-point query receipt is absent or stale");
  }
  state.assertLive();
  readWatcherNativeChainSyncEventReceipt(state.value.eventReceipt);
  return state.value;
};

const uint64 = (value: string, label: string): bigint => {
  if (!NATURAL.test(value) || value.length > 20 || BigInt(value) > MAX_UINT64) {
    throw new Error(`${label} is not a canonical UInt64`);
  }
  return BigInt(value);
};

const parseBlockPoint = (value: unknown, label: string): NativeBlockPoint => {
  const parsed = exactRecord(value, ["blockHash", "blockNo", "slot"], label);
  const result = Object.freeze({
    blockHash: string(parsed.blockHash, HEX_32, `${label} hash`),
    blockNo: string(parsed.blockNo, NATURAL, `${label} block number`),
    slot: string(parsed.slot, NATURAL, `${label} slot`),
  });
  uint64(result.blockNo, `${label} block number`);
  uint64(result.slot, `${label} slot`);
  return result;
};

/** Owns one configured exact-point read; callers cannot supply acquired events. */
export const openWatcherNativeExactPointQuery = async (input: {
  readonly binaryPath: string;
  readonly watcherConfig: unknown;
  readonly predecessor: NativeBlockPoint;
  readonly target: NativeBlockPoint;
  readonly timeoutMs: number;
  readonly signal?: AbortSignal;
}): Promise<
  Readonly<{
    receipt: WatcherNativeExactPointQueryReceipt;
    close(): Promise<void>;
  }>
> => {
  const timeoutMs = input.timeoutMs;
  const signal = input.signal;
  if (signal !== undefined) {
    try {
      Object.getOwnPropertyDescriptor(
        AbortSignal.prototype,
        "aborted",
      )!.get!.call(signal);
    } catch {
      throw new Error("native exact-point query signal is not an AbortSignal");
    }
  }
  const started = performance.now();
  const startedUtc = Date.now();
  if (
    !Number.isSafeInteger(timeoutMs) ||
    timeoutMs < 100 ||
    timeoutMs > 120_000
  ) {
    throw new Error("native exact-point query timeout is invalid");
  }
  signal?.throwIfAborted();
  const watcherConfig = parseWatcherConfig(input.watcherConfig);
  const predecessor = parseBlockPoint(
    input.predecessor,
    "native query predecessor",
  );
  const target = parseBlockPoint(input.target, "native query target");
  if (
    BigInt(predecessor.blockNo) + 1n !== BigInt(target.blockNo) ||
    BigInt(predecessor.slot) >= BigInt(target.slot)
  ) {
    throw new Error(
      "native exact-point query target is not the direct successor",
    );
  }
  const deadline = started + timeoutMs;
  const controller = new AbortController();
  let active = true;
  let runtime: WatcherNativeChainSyncRuntime | undefined;
  type TargetCapture = Readonly<{
    receipt: WatcherNativeChainSyncEventReceipt;
    observedAt: string;
  }>;
  let rejectTarget!: (error: Error) => void;
  let resolveTarget!: (capture: TargetCapture) => void;
  const targetReady = new Promise<TargetCapture>((resolve, reject) => {
    resolveTarget = resolve;
    rejectTarget = reject;
  });
  void targetReady.catch(() => undefined);
  const invalidate = (error: Error): void => {
    active = false;
    controller.abort(error);
    rejectTarget(error);
  };
  const abort = (): void =>
    invalidate(new Error("native exact-point query was cancelled"));
  signal?.addEventListener("abort", abort, { once: true });
  const timer = setTimeout(
    () => invalidate(new Error("native exact-point query expired")),
    Math.max(0, deadline - performance.now()),
  );
  const assertLive = (): void => {
    if (!active || controller.signal.aborted || performance.now() >= deadline) {
      throw new Error("native exact-point query receipt is absent or stale");
    }
  };
  const cleanup = (): void => {
    clearTimeout(timer);
    signal?.removeEventListener("abort", abort);
  };
  let startup: Promise<WatcherNativeChainSyncRuntime> | undefined;
  const close = async (): Promise<void> => {
    invalidate(new Error("native exact-point query was closed"));
    cleanup();
    const owned = runtime ?? (await startup?.catch(() => undefined));
    await owned?.close();
  };
  try {
    assertLive();
    startup = startNativeSupervisor({
      binaryPath: input.binaryPath,
      watcherConfig,
      intersections: [
        Object.freeze({
          kind: "point",
          blockHash: predecessor.blockHash,
          slot: predecessor.slot,
        }),
      ],
      startupTimeoutMs: timeoutMs,
      operation: Object.freeze({
        kind: "exact_point",
        predecessorBlockNo: predecessor.blockNo,
        target,
        timeoutMs: timeoutMs,
      }),
      signal: controller.signal,
      onEvent: async (event) => {
        assertLive();
        if (event.kind !== "roll_forward") return;
        const receipt = watcherNativeChainSyncEventReceipt(event);
        if (receipt === null)
          throw new Error(
            "native exact-point target has no acquisition receipt",
          );
        resolveTarget({ receipt, observedAt: new Date().toISOString() });
      },
    });
    const cancellation = new Promise<never>((_, reject) => {
      const rejectCancelled = () =>
        reject(new Error("native exact-point query was cancelled or expired"));
      controller.signal.addEventListener("abort", rejectCancelled, {
        once: true,
      });
      void startup!
        .finally(() =>
          controller.signal.removeEventListener("abort", rejectCancelled),
        )
        .catch(() => undefined);
    });
    runtime = await Promise.race([startup, cancellation]);
    void runtime.done.then(
      () => {
        invalidate(new Error("native exact-point read ended"));
        cleanup();
      },
      (error: unknown) => {
        invalidate(
          error instanceof Error
            ? error
            : new Error("native exact-point read failed"),
        );
        cleanup();
      },
    );
    const targetCapture = await targetReady;
    const eventReceipt = targetCapture.receipt;
    assertLive();
    const observed = readWatcherNativeChainSyncEventReceipt(eventReceipt);
    if (
      observed.event.kind !== "roll_forward" ||
      observed.authority !== runtime.authority ||
      observed.event.tip.kind !== "point"
    ) {
      throw new Error("native exact-point query omitted a current target tip");
    }
    const tip = parseBlockPoint(
      {
        blockHash: observed.event.tip.blockHash,
        blockNo: observed.event.tip.blockNo,
        slot: observed.event.tip.slot,
      },
      "native query observed tip",
    );
    const depth = BigInt(tip.blockNo) - BigInt(target.blockNo) + 1n;
    if (
      depth <= 0n ||
      depth > MAX_UINT64 ||
      BigInt(tip.slot) < BigInt(target.slot) ||
      (depth === 1n &&
        (tip.blockHash !== target.blockHash || tip.slot !== target.slot)) ||
      (depth > 1n && BigInt(tip.slot) <= BigInt(target.slot))
    ) {
      throw new Error("native exact-point query tip cannot contain the target");
    }
    const receipt = Object.freeze({ [queryReceiptBrand]: true as const });
    queryReceipts.set(
      receipt,
      Object.freeze({
        assertLive,
        value: Object.freeze({
          authority: observed.authority,
          eventReceipt,
          startupDigest: observed.startupDigest,
          eventDigest: observed.eventDigest,
          event: observed.event,
          observedTip: tip,
          depthAtObservedTip: depth.toString(),
          observedAt: targetCapture.observedAt,
          expiresAt: new Date(startedUtc + timeoutMs).toISOString(),
        }),
      }),
    );
    return Object.freeze({ receipt, close });
  } catch (error) {
    invalidate(new Error("native exact-point query failed"));
    cleanup();
    // Identity reads may still be pending. They recheck cancellation before
    // opening; a read that was already opened is closed by the supervisor.
    if (runtime !== undefined) await runtime.close();
    else if (startup !== undefined)
      void startup.then((owned) => owned.close()).catch(() => undefined);
    throw error;
  }
};
