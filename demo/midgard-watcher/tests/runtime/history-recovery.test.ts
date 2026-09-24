import { afterEach, describe, expect, it, vi } from "vitest";

import { WatcherFaultDecisionRetired } from "../../src/fault-proofs/fault-decision-bridge.js";
import type { WatcherNativeBlockAdmission } from "../../src/l1/native-block-admission.js";
import type { WatcherNativeChainSyncEvent } from "../../src/l1/native-chain-sync.js";
import { unsafeCreateWatcherChainCoordinatorForTest } from "../../src/runtime/chain-coordinator.js";
import { createWatcherHistoryRecovery } from "../../src/runtime/history-recovery.js";
import { WatcherUserEventOperationRetired } from "../../src/runtime/user-event-runtime.js";

const point = {
  blockHash: "11".repeat(32),
  blockNo: "10",
  slot: "100",
  pointId: "22".repeat(32),
};
const block = {
  ...point,
  schemaVersion: "midgard-watcher-native-block-admission-v1",
  blockType: "7",
  protocolMajor: "10",
  prevHash: "00".repeat(32),
  rawBlockCbor: "80",
  rawHeaderCbor: "80",
  transactionIds: [],
  transactionCbors: [],
} satisfies WatcherNativeBlockAdmission;
const forward: WatcherNativeChainSyncEvent = {
  ...block,
  schemaVersion: "midgard-watcher-native-chain-sync-v1",
  kind: "roll_forward",
  tip: { kind: "point", ...point, blockNo: "40" },
};
const deferred = () => {
  let resolve!: () => void;
  let reject!: (error: unknown) => void;
  const promise = new Promise<void>((yes, no) => {
    resolve = yes;
    reject = no;
  });
  return { promise, resolve, reject };
};
const cleanup: (() => void)[] = [];
afterEach(() => {
  cleanup.splice(0).forEach((close) => close());
  vi.useRealTimers();
});

// This exercises the actual production hook composition and coordinator.
// The owner and durable observer are controlled seams, not L1 authority evidence.
const harness = (finalize = true) => {
  let status: "ready" | "suspended" | "failed" = "ready";
  let generation = 0;
  let quarantined = false;
  const attempts: ReturnType<typeof deferred>[] = [];
  const finalized = vi.fn(async (): Promise<void> => undefined);
  const included = vi.fn(async (): Promise<void> => undefined);
  const revoked = vi.fn();
  const queueRollback = vi.fn(async (): Promise<void> => undefined);
  const advance = vi.fn(async (): Promise<void> => undefined);
  const history = {
    read: () => ({
      status,
      generation,
      currentPoint: point,
      headCursor: point,
    }),
    handleRollback: vi.fn(() => {
      status = "suspended";
      generation += 1;
      const next = deferred();
      attempts.push(next);
      return next.promise.then(() => {
        status = "ready";
      });
    }),
    classify: () => "touched" as const,
    advanceThrough: advance,
    coverQuiet: advance,
  };
  const recovery = createWatcherHistoryRecovery({
    history,
    queue: {
      onRollback: queueRollback,
      onIncluded: included,
      onFinalized: finalized,
    },
    bridge: {
      invalidateForRollback: revoked,
      beforeHistoryAdvance: () => undefined,
    },
    availability: { invalidateForRollback: () => undefined },
    quarantined: () => quarantined,
    resume: () => coordinator.resume(),
    retryDelayMs: 10,
    onPending: () => undefined,
  });
  cleanup.push(recovery.close);
  let state: Record<string, unknown> = {
    phase: "unobserved",
    pending: null,
    finalized: null,
  };
  const observe = vi.fn(async () => ({
    block: {},
    consistency: {},
    transportAttestations: [],
  }));
  const coordinator = unsafeCreateWatcherChainCoordinatorForTest(
    {
      policy: { confirmationDepth: "30" } as never,
      durable: {
        readFinality: () => state,
        read: () => ({ currentFinalityState: state, currentStore: {} }),
        persistCanonicalProgress: async () => {
          state = {
            phase: finalize ? "finalized" : "pending",
            pending: finalize ? null : point,
            finalized: finalize ? point : null,
          };
          return {
            persistence: "committed",
            finalityResult: {
              action: finalize ? "finalize" : "observe_pending",
            },
          };
        },
      } as never,
      observation: { observe } as never,
      hooks: recovery.hooks,
    },
    {
      admitRollForward: () => block,
      relevance: (value) => recovery.classify(value, []),
    },
  );
  return {
    coordinator,
    recovery,
    attempts,
    finalized,
    included,
    revoked,
    queueRollback,
    advance,
    observe,
    history,
    suspend: () => {
      status = "suspended";
      generation += 1;
    },
    quarantine: () => {
      quarantined = true;
    },
    setReady: () => {
      status = "ready";
    },
    setFailed: () => {
      status = "failed";
    },
  };
};

const beginRollback = async (h: ReturnType<typeof harness>) => {
  const rollbackPoint = {
    kind: "point" as const,
    blockHash: "00".repeat(32),
    slot: "99",
  };
  h.recovery.hooks.onRollbackArrived!(rollbackPoint);
  await h.recovery.hooks.onRollback(rollbackPoint);
};

describe("production history recovery coordination", () => {
  it("retains finalized delivery and automatically resumes at a stable tip", async () => {
    const h = harness();
    await beginRollback(h);
    expect(h.revoked).toHaveBeenCalledOnce();
    expect(h.queueRollback).toHaveBeenCalledOnce();
    await h.coordinator.handle(forward);
    expect(h.coordinator.status()).toMatchObject({
      deliveryHeld: true,
      bufferedBlockCount: 1,
      processedThrough: null,
    });
    expect(h.finalized).not.toHaveBeenCalled();
    h.attempts[0]!.resolve();
    await vi.waitFor(() => expect(h.finalized).toHaveBeenCalledOnce());
    expect(h.coordinator.status()).toMatchObject({
      deliveryHeld: false,
      bufferedBlockCount: 0,
      processedThrough: expect.objectContaining({ blockHash: block.blockHash }),
    });
    await h.coordinator.resume();
    expect(h.finalized).toHaveBeenCalledOnce();
  });

  it("holds included consumers too, retries failed acquisition and retains the failure", async () => {
    const h = harness(false);
    await beginRollback(h);
    await h.coordinator.handle({
      ...forward,
      tip: { kind: "point", ...point },
    });
    expect(h.included).not.toHaveBeenCalled();
    h.attempts[0]!.reject(new Error("archive unavailable"));
    await vi.waitFor(() =>
      expect(h.recovery.status().lastError).toBe("archive unavailable"),
    );
    await vi.waitFor(() => expect(h.attempts).toHaveLength(2));
    expect(h.coordinator.status().deliveryHeld).toBe(true);
    h.attempts[1]!.resolve();
    await vi.waitFor(() => expect(h.included).toHaveBeenCalledOnce());
    expect(h.finalized).not.toHaveBeenCalled();
    expect(h.recovery.status()).toEqual({ pending: false, lastError: null });
  });

  it("holds a generation-retired history operation without dropping its block", async () => {
    const h = harness();
    const work = deferred();
    h.advance.mockImplementationOnce(() => work.promise);
    const processing = h.coordinator.handle(forward);
    await vi.waitFor(() => expect(h.advance).toHaveBeenCalledOnce());
    h.suspend();
    work.reject(
      new WatcherUserEventOperationRetired(
        "operation retired by native rollback",
      ),
    );
    await processing;
    expect(h.coordinator.status()).toMatchObject({
      deliveryHeld: true,
      bufferedBlockCount: 1,
      processedThrough: null,
    });
    expect(h.finalized).not.toHaveBeenCalled();
  });

  it("revokes immediately when rollback arrives during a resumed queue consumer", async () => {
    const h = harness();
    await beginRollback(h);
    await h.coordinator.handle(forward);
    const consumer = deferred();
    h.finalized.mockImplementationOnce(() => consumer.promise);
    h.attempts[0]!.resolve();
    await vi.waitFor(() => expect(h.finalized).toHaveBeenCalledOnce());
    const rollback = h.coordinator.handle({
      schemaVersion: "midgard-watcher-native-chain-sync-v1",
      kind: "roll_backward",
      point: { kind: "point", blockHash: block.blockHash, slot: block.slot },
      tip: { kind: "point", ...point },
    });
    expect(h.revoked).toHaveBeenCalledTimes(2);
    expect(h.history.read().status).toBe("suspended");
    expect(h.coordinator.status().processedThrough).toBeNull();
    consumer.resolve();
    await rollback;
    expect(h.coordinator.status().processedThrough).toBeNull();
    expect(h.queueRollback).toHaveBeenCalledTimes(2);
    h.attempts[1]!.resolve();
    await vi.waitFor(() => expect(h.finalized).toHaveBeenCalledTimes(2));
    expect(h.coordinator.status()).toMatchObject({
      deliveryHeld: false,
      processedThrough: expect.objectContaining({ blockHash: block.blockHash }),
    });
  });

  it("preserves unrelated history errors and quarantine refusal", async () => {
    const h = harness();
    h.advance.mockRejectedValueOnce(new Error("invalid immutable evidence"));
    await expect(h.coordinator.handle(forward)).rejects.toThrow(
      "invalid immutable evidence",
    );
    expect(h.coordinator.status().deliveryHeld).toBe(false);
    h.quarantine();
    await expect(
      h.recovery.hooks.onIncluded!({
        nativeBlock: block,
        localObservation: null,
        relevance: "quiet",
      }),
    ).rejects.toThrow("quarantine");
    expect(h.included).not.toHaveBeenCalled();
  });

  it("keeps catch-up waiting until stable-tip consumer delivery finishes", async () => {
    const h = harness();
    await beginRollback(h);
    await h.coordinator.handle(forward);
    const consumer = deferred();
    h.finalized.mockImplementationOnce(() => consumer.promise);
    let caughtUp = false;
    const catchUp = (async () => {
      await h.recovery.waitForRecovery();
      await h.coordinator.waitForDelivery();
      caughtUp = true;
    })();
    expect(caughtUp).toBe(false);
    h.attempts[0]!.resolve();
    await vi.waitFor(() => expect(h.finalized).toHaveBeenCalledOnce());
    expect(caughtUp).toBe(false);
    consumer.resolve();
    await catchUp;
    expect(caughtUp).toBe(true);
    expect(h.coordinator.status().processedThrough?.blockHash).toBe(
      block.blockHash,
    );
  });

  it("holds explicit retired work after rapid owner recovery", async () => {
    const h = harness();
    const work = deferred();
    h.advance.mockImplementationOnce(() => work.promise);
    const processing = h.coordinator.handle(forward);
    await vi.waitFor(() => expect(h.advance).toHaveBeenCalledOnce());
    h.suspend();
    h.setReady();
    work.reject(new WatcherUserEventOperationRetired("retired operation"));
    await processing;
    expect(h.coordinator.status().deliveryHeld).toBe(true);
    await vi.waitFor(() => expect(h.finalized).toHaveBeenCalledOnce());
    expect(h.coordinator.status().deliveryHeld).toBe(false);
  });

  it("holds revoked queue decisions and preserves unrelated queue failures", async () => {
    const h = harness();
    const work = deferred();
    h.finalized.mockImplementationOnce(() => work.promise);
    const processing = h.coordinator.handle(forward);
    await vi.waitFor(() => expect(h.finalized).toHaveBeenCalledOnce());
    await beginRollback(h);
    work.reject(
      new WatcherFaultDecisionRetired(
        "authority changed during fault classification",
      ),
    );
    await processing;
    expect(h.coordinator.status().deliveryHeld).toBe(true);
    h.attempts[0]!.resolve();
    await vi.waitFor(() => expect(h.finalized).toHaveBeenCalledTimes(2));
    const other = harness();
    other.finalized.mockRejectedValueOnce(new Error("corrupt queue evidence"));
    await expect(other.coordinator.handle(forward)).rejects.toThrow(
      "corrupt queue evidence",
    );
    await expect(other.coordinator.waitForDelivery()).rejects.toThrow(
      "corrupt queue evidence",
    );
  });

  it("waits for active consumers at shutdown and never records their completion", async () => {
    const h = harness();
    const consumer = deferred();
    h.finalized.mockImplementationOnce(() => consumer.promise);
    const processing = h.coordinator.handle(forward);
    await vi.waitFor(() => expect(h.finalized).toHaveBeenCalledOnce());
    h.recovery.close();
    let stopped = false;
    const stopping = h.coordinator.stop().then(() => {
      stopped = true;
    });
    await Promise.resolve();
    expect(stopped).toBe(false);
    consumer.resolve();
    await processing;
    await stopping;
    expect(h.coordinator.status().processedThrough).toBeNull();
    await expect(h.coordinator.waitForDelivery()).rejects.toThrow("stopped");
    await expect(h.recovery.waitForRecovery()).rejects.toThrow("closed");
  });

  it("does not wake consumers from an older recovery or after close", async () => {
    const h = harness();
    await beginRollback(h);
    await h.coordinator.handle(forward);
    await beginRollback(h);
    h.attempts[0]!.resolve();
    await Promise.resolve();
    await Promise.resolve();
    expect(h.finalized).not.toHaveBeenCalled();
    h.recovery.close();
    h.attempts[1]!.resolve();
    await Promise.resolve();
    await Promise.resolve();
    expect(h.finalized).not.toHaveBeenCalled();
  });
});
