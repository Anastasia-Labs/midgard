import { describe, expect, it, vi } from "vitest";

import type { WatcherFaultDecisionBridge } from "../../src/fault-proofs/fault-decision-bridge.js";
import type { WatcherAuthenticatedStateQueueObservation } from "../../src/indexers/authenticated-state-queue-observation.js";
import type { WatcherNativeBlockAdmission } from "../../src/l1/native-block-admission.js";
import { createWatcherStateQueueRuntime } from "../../src/runtime/state-queue-runtime.js";

const point = (blockNo: number, byte: string) =>
  Object.freeze({
    blockHash: byte.repeat(32),
    blockNo: blockNo.toString(),
    slot: (blockNo * 10).toString(),
    chainPointId: "ff".repeat(32),
  });

const cursor = Object.freeze({
  observationDigest: "44".repeat(32),
  previousObservationDigest: null,
  nativePoint: Object.freeze({
    ...point(100, "44"),
    parentBlockHash: "00".repeat(32),
    finalityDepth: "30",
  }),
}) as WatcherAuthenticatedStateQueueObservation;

const nativeBlock = (blockNo: number, byte: string) =>
  Object.freeze({
    blockHash: byte.repeat(32),
    blockNo: blockNo.toString(),
    slot: (blockNo * 10).toString(),
  }) as WatcherNativeBlockAdmission;

const quietWake = async (input: {
  readonly hasInclusion: boolean;
  readonly catchupBoundary: ReturnType<typeof point>;
}) => {
  const runtime = await createWatcherStateQueueRuntime({
    source: {
      restore: async () => ({
        previous: cursor,
        discardedObservationCount: 0,
        replayIntersection: point(100, "44"),
        catchupBoundary: {
          ...input.catchupBoundary,
          finalityDepth: "30",
          ogmiosTipBlockNo: "130",
        },
      }),
      bootstrap: async () => {
        throw new Error("not used");
      },
      observe: async () => {
        throw new Error("quiet progress must not query the queue");
      },
      ...(input.hasInclusion
        ? {
            observeIncluded: async () => {
              throw new Error("quiet progress must not query the queue");
            },
          }
        : {}),
      resolveRetainedHeader: async () => {
        throw new Error("not used");
      },
    },
    store: {
      readAll: async () => [cursor],
      append: async () => "appended" as const,
      rollbackTo: async () => undefined,
    },
  });
  const calls: string[] = [];
  const retryDeferredClassification = vi.fn(async () => {
    calls.push("retry");
  });
  const recoverExisting = vi.fn(async () => {
    calls.push("recover");
    return 0;
  });
  const hooks = runtime.bindFaultDecisionBridge(
    Object.freeze({
      retryDeferredClassification,
      recoverExisting,
      reconcileAndDispatch: vi.fn(),
      prepareForRecovery: vi.fn(),
      invalidateForRollback: vi.fn(),
    }) as unknown as WatcherFaultDecisionBridge,
  );
  await hooks.onFinalized({
    nativeBlock: nativeBlock(101, "45"),
    localObservation: null,
    relevance: "quiet",
  });
  return { calls, retryDeferredClassification };
};

describe("state-queue runtime deferred classification wakes", () => {
  it("retries deferred classification on quiet finalized progress without inclusion wakes", async () => {
    const wake = await quietWake({
      hasInclusion: false,
      catchupBoundary: point(100, "44"),
    });
    expect(wake.retryDeferredClassification).toHaveBeenCalledExactlyOnceWith(
      cursor,
    );
    expect(wake.calls).toEqual(["retry", "recover"]);
  });

  it("leaves quiet finalized progress to the inclusion wake when the source has one", async () => {
    const wake = await quietWake({
      hasInclusion: true,
      catchupBoundary: point(100, "44"),
    });
    expect(wake.calls).toEqual([]);
  });

  it("does not retry before replay reaches the catch-up boundary", async () => {
    const wake = await quietWake({
      hasInclusion: false,
      catchupBoundary: point(102, "46"),
    });
    expect(wake.calls).toEqual([]);
  });
});
