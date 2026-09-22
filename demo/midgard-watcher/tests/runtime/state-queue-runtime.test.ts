import { describe, expect, it, vi } from "vitest";

import type { WatcherFaultDecisionBridge } from "../../src/fault-proofs/fault-decision-bridge.js";
import type {
  WatcherAuthenticatedStateQueueObservation,
  WatcherStateQueueObservationSource,
} from "../../src/indexers/authenticated-state-queue-observation.js";
import type { WatcherLocalKupmiosNativeObservation } from "../../src/l1/local-kupmios-native-observation.js";
import type { WatcherNativeBlockAdmission } from "../../src/l1/native-block-admission.js";
import { createWatcherStateQueueRuntime } from "../../src/runtime/state-queue-runtime.js";
import type { WatcherSqliteStateQueueObservationStore } from "../../src/storage/sqlite-durable-backend.js";

const point = (blockNo: number, byte: string) =>
  Object.freeze({
    blockHash: byte.repeat(32),
    blockNo: blockNo.toString(),
    slot: (blockNo * 10).toString(),
    chainPointId: `${byte === "ff" ? "ee" : "ff"}`.repeat(32),
  });

const observation = (
  blockNo: number,
  byte: string,
  previousObservationDigest: string | null,
): WatcherAuthenticatedStateQueueObservation =>
  Object.freeze({
    observationDigest: byte.repeat(32),
    previousObservationDigest,
    nativePoint: Object.freeze({
      ...point(blockNo, byte),
      parentBlockHash: "00".repeat(32),
      finalityDepth: "30",
    }),
  }) as WatcherAuthenticatedStateQueueObservation;

const nativeBlock = (
  blockNo: number,
  byte: string,
): WatcherNativeBlockAdmission =>
  Object.freeze({
    blockHash: byte.repeat(32),
    blockNo: blockNo.toString(),
    slot: (blockNo * 10).toString(),
  }) as WatcherNativeBlockAdmission;

const localObservation = Object.freeze(
  {},
) as WatcherLocalKupmiosNativeObservation;

const bridge = () => {
  const recoverExisting = vi.fn(async () => 0);
  const retryDeferredClassification = vi.fn(async () => undefined);
  const invalidateForRollback = vi.fn();
  const prepareForRecovery = vi.fn(async () => ({
    observationDigest: "00".repeat(32),
    decisionDigests: Object.freeze([]),
    target: null,
  }));
  const reconcileAndDispatch = vi.fn(async () => ({
    observationDigest: "00".repeat(32),
    decisionDigests: Object.freeze([]),
    target: null,
  }));
  return {
    recoverExisting,
    retryDeferredClassification,
    invalidateForRollback,
    prepareForRecovery,
    reconcileAndDispatch,
    value: Object.freeze({
      recoverExisting,
      retryDeferredClassification,
      invalidateForRollback,
      prepareForRecovery,
      reconcileAndDispatch,
    }) as unknown as WatcherFaultDecisionBridge,
  };
};

describe("production state-queue runtime V1", () => {
  it.each([
    { relevance: "quiet", failOnce: false, rollback: false },
    { relevance: "touched", failOnce: false, rollback: false },
    { relevance: "quiet", failOnce: true, rollback: false },
    { relevance: "touched", failOnce: true, rollback: false },
    { relevance: "quiet", failOnce: false, rollback: true },
  ] as const)(
    "coalesces finalized classification: next=$relevance failure=$failOnce rollback=$rollback",
    async ({ relevance, failOnce, rollback }) => {
      const stable = observation(100, "61", null);
      const included = observation(130, "62", stable.observationDigest);
      const newest = observation(131, "63", included.observationDigest);
      const firstFinalized = observation(101, "64", stable.observationDigest);
      const secondFinalized = observation(
        102,
        "65",
        firstFinalized.observationDigest,
      );
      const recovery = {
        previous: stable,
        discardedObservationCount: 0,
        replayIntersection: point(100, "61"),
        catchupBoundary: {
          ...point(100, "61"),
          finalityDepth: "30",
          ogmiosTipBlockNo: "129",
        },
      };
      const append = vi.fn(async () => "appended" as const);
      const observe = vi.fn(async ({ nativeBlock: block }) =>
        block.blockNo === "101" ? firstFinalized : secondFinalized,
      );
      const observeIncluded = vi.fn(async ({ nativeBlock: block }) =>
        block.blockNo === "130" ? included : newest,
      );
      const runtime = await createWatcherStateQueueRuntime({
        source: {
          restore: async () => recovery,
          bootstrap: async () => recovery,
          observe,
          observeIncluded,
          resolveRetainedHeader: async () => {
            throw new Error("unused");
          },
        },
        store: {
          readAll: async () => [stable],
          append,
          rollbackTo: async () => undefined,
        },
      });
      const decisionBridge = bridge();
      const availability = {
        reconcile: vi.fn(async () => undefined),
        invalidateForRollback: vi.fn(),
      };
      const hooks = runtime.bindFaultDecisionBridge(
        decisionBridge.value,
        availability,
      );
      await hooks.onIncluded!({
        nativeBlock: nativeBlock(130, "62"),
        localObservation,
        relevance: "touched",
      });
      decisionBridge.reconcileAndDispatch.mockClear();
      for (const [height, byte] of [
        [101, "64"],
        [102, "65"],
      ] as const)
        await hooks.onFinalized({
          nativeBlock: nativeBlock(height, byte),
          localObservation,
          relevance: "touched",
        });
      expect(observe).toHaveBeenCalledTimes(2);
      expect(append).toHaveBeenCalledTimes(2);
      expect(append).toHaveBeenNthCalledWith(1, firstFinalized);
      expect(append).toHaveBeenNthCalledWith(2, secondFinalized);
      expect(availability.reconcile).toHaveBeenNthCalledWith(
        1,
        firstFinalized,
        true,
      );
      expect(availability.reconcile).toHaveBeenNthCalledWith(
        2,
        secondFinalized,
        true,
      );
      expect(decisionBridge.recoverExisting).toHaveBeenCalledTimes(3);
      expect(decisionBridge.reconcileAndDispatch).not.toHaveBeenCalled();
      // Already traversed callbacks cannot consume the pending context refresh.
      await hooks.onIncluded!({
        nativeBlock: nativeBlock(130, "62"),
        localObservation,
        relevance: "touched",
      });
      expect(decisionBridge.reconcileAndDispatch).not.toHaveBeenCalled();
      if (rollback) {
        await hooks.onRollback({
          kind: "point",
          blockHash: stable.nativePoint.blockHash,
          slot: stable.nativePoint.slot,
        });
        await hooks.onIncluded!({
          nativeBlock: nativeBlock(131, "63"),
          localObservation: null,
          relevance: "quiet",
        });
        expect(
          decisionBridge.prepareForRecovery,
        ).toHaveBeenCalledExactlyOnceWith(stable);
        expect(decisionBridge.reconcileAndDispatch).not.toHaveBeenCalled();
        expect(
          decisionBridge.retryDeferredClassification,
        ).toHaveBeenCalledExactlyOnceWith(stable);
        return;
      }
      const fresh = () =>
        hooks.onIncluded!({
          nativeBlock: nativeBlock(131, "63"),
          localObservation: relevance === "touched" ? localObservation : null,
          relevance,
        });
      if (failOnce) {
        decisionBridge.reconcileAndDispatch.mockRejectedValueOnce(
          new Error("classifier unavailable"),
        );
        await expect(fresh()).rejects.toThrow("classifier unavailable");
        // A failed classification survives until the next eligible callback,
        // even after a touched callback has already advanced the queue view.
        await hooks.onIncluded!({
          nativeBlock: nativeBlock(132, "66"),
          localObservation: null,
          relevance: "quiet",
        });
      } else await fresh();
      expect(decisionBridge.reconcileAndDispatch).toHaveBeenCalledTimes(
        failOnce ? 2 : 1,
      );
      expect(decisionBridge.reconcileAndDispatch).toHaveBeenLastCalledWith(
        relevance === "touched" ? newest : included,
      );
      expect(decisionBridge.retryDeferredClassification).not.toHaveBeenCalled();
      await hooks.onIncluded!({
        nativeBlock: nativeBlock(133, "67"),
        localObservation: null,
        relevance: "quiet",
      });
      expect(decisionBridge.reconcileAndDispatch).toHaveBeenCalledTimes(
        failOnce ? 2 : 1,
      );
      expect(
        decisionBridge.retryDeferredClassification,
      ).toHaveBeenCalledExactlyOnceWith(
        relevance === "touched" ? newest : included,
      );
    },
  );

  it.each([true, false])(
    "coalesces historical finalized snapshots before the first quiet inclusion (inclusion=%s)",
    async (hasInclusion) => {
      const stable = observation(100, "61", null);
      const first = observation(101, "62", stable.observationDigest);
      const second = observation(102, "63", first.observationDigest);
      const latest = observation(103, "64", second.observationDigest);
      const snapshots = [first, second, latest];
      let nextSnapshot = 0;
      const observe = vi.fn(async () => snapshots[nextSnapshot++]!);
      const observeIncluded = vi.fn(async () => {
        throw new Error("quiet inclusion must not query queue");
      });
      const append = vi.fn(async () => "appended" as const);
      const runtime = await createWatcherStateQueueRuntime({
        source: {
          restore: async () => ({
            previous: stable,
            discardedObservationCount: 0,
            replayIntersection: point(100, "61"),
            catchupBoundary: {
              ...point(100, "61"),
              finalityDepth: "30",
              ogmiosTipBlockNo: "100",
            },
          }),
          bootstrap: async () => {
            throw new Error("not used");
          },
          observe,
          ...(hasInclusion ? { observeIncluded } : {}),
          resolveRetainedHeader: async () => {
            throw new Error("not used");
          },
        },
        store: {
          readAll: async () => [stable],
          append,
          rollbackTo: async () => undefined,
        },
      });
      // Trusted restore is already caught up to its persisted cursor even
      // though the native stream still has a historical backlog to deliver.
      await expect(runtime.caughtUp).resolves.toBeUndefined();
      const decisionBridge = bridge();
      const availability = {
        reconcile: vi.fn(async () => undefined),
        invalidateForRollback: vi.fn(),
      };
      const hooks = runtime.bindFaultDecisionBridge(
        decisionBridge.value,
        availability,
      );
      for (const [index, byte] of ["62", "63", "64"].entries()) {
        await hooks.onFinalized({
          nativeBlock: nativeBlock(101 + index, byte),
          localObservation,
          relevance: "touched",
        });
        expect(append).toHaveBeenNthCalledWith(index + 1, snapshots[index]);
        expect(availability.reconcile).toHaveBeenNthCalledWith(
          index + 1,
          snapshots[index],
          true,
        );
      }
      expect(observe).toHaveBeenCalledTimes(3);
      expect(runtime.current()).toBe(latest);
      expect(decisionBridge.reconcileAndDispatch).toHaveBeenCalledTimes(
        hasInclusion ? 0 : 3,
      );
      if (!hasInclusion) {
        expect(decisionBridge.reconcileAndDispatch).toHaveBeenLastCalledWith(
          latest,
        );
        return;
      }
      const firstInclusion = nativeBlock(131, "65");
      await hooks.onIncluded!({
        nativeBlock: firstInclusion,
        localObservation: null,
        relevance: "quiet",
      });
      expect(
        decisionBridge.reconcileAndDispatch,
      ).toHaveBeenCalledExactlyOnceWith(latest);
      expect(decisionBridge.recoverExisting).toHaveBeenLastCalledWith({
        nativeProgress: firstInclusion,
      });
      await hooks.onIncluded!({
        nativeBlock: nativeBlock(132, "66"),
        localObservation: null,
        relevance: "quiet",
      });
      expect(decisionBridge.reconcileAndDispatch).toHaveBeenCalledOnce();
      expect(
        decisionBridge.retryDeferredClassification,
      ).toHaveBeenCalledExactlyOnceWith(latest);
      expect(observeIncluded).not.toHaveBeenCalled();
      expect(append).toHaveBeenCalledTimes(3);
    },
  );

  it("advances the volatile view on inclusion and discards it before rollback recovery", async () => {
    const stable = observation(100, "61", null);
    const provisional = observation(101, "62", stable.observationDigest);
    const replacement = observation(101, "63", stable.observationDigest);
    const recovery = {
      previous: stable,
      discardedObservationCount: 0,
      replayIntersection: point(100, "61"),
      catchupBoundary: {
        ...point(100, "61"),
        finalityDepth: "30",
        ogmiosTipBlockNo: "129",
      },
    };
    const append = vi.fn(async () => "appended" as const);
    const observeIncluded = vi.fn(async ({ nativeBlock: block }) =>
      block.blockHash === provisional.nativePoint.blockHash
        ? provisional
        : replacement,
    );
    const source: WatcherStateQueueObservationSource = {
      restore: async () => recovery,
      bootstrap: async () => recovery,
      observe: async ({ previous }) => previous,
      observeIncluded,
      resolveRetainedHeader: async () => {
        throw new Error("unused");
      },
    };
    const runtime = await createWatcherStateQueueRuntime({
      source,
      store: {
        readAll: async () => [stable],
        append,
        rollbackTo: async () => undefined,
      },
    });
    const decisionBridge = bridge();
    const availability = {
      reconcile: vi.fn(async () => undefined),
      invalidateForRollback: vi.fn(),
    };
    const hooks = runtime.bindFaultDecisionBridge(
      decisionBridge.value,
      availability,
    );
    await hooks.onIncluded!({
      nativeBlock: nativeBlock(101, "62"),
      localObservation,
      relevance: "touched",
    });
    expect(runtime.current()).toBe(provisional);
    expect(decisionBridge.reconcileAndDispatch).toHaveBeenLastCalledWith(
      provisional,
    );
    expect(append).not.toHaveBeenCalled();
    expect(availability.reconcile).not.toHaveBeenCalled();
    // Public DA arriving after the touched inclusion is retried on the next
    // quiet block, using the same authenticated queue before depth 30.
    await hooks.onIncluded!({
      nativeBlock: nativeBlock(102, "64"),
      localObservation: null,
      relevance: "quiet",
    });
    expect(
      decisionBridge.retryDeferredClassification,
    ).toHaveBeenCalledExactlyOnceWith(provisional);
    expect(observeIncluded).toHaveBeenCalledTimes(1);
    expect(availability.reconcile).not.toHaveBeenCalled();
    await hooks.onFinalized({
      nativeBlock: nativeBlock(100, "61"),
      localObservation,
      relevance: "touched",
    });
    expect(runtime.current()).toBe(provisional);
    expect(decisionBridge.reconcileAndDispatch).toHaveBeenLastCalledWith(
      provisional,
    );
    expect(availability.reconcile).toHaveBeenLastCalledWith(stable, true);
    await hooks.onRollback({
      kind: "point",
      blockHash: stable.nativePoint.blockHash,
      slot: stable.nativePoint.slot,
    });
    expect(runtime.current()).toBe(stable);
    await hooks.onIncluded!({
      nativeBlock: nativeBlock(101, "63"),
      localObservation,
      relevance: "touched",
    });
    expect(observeIncluded).toHaveBeenLastCalledWith({
      nativeBlock: nativeBlock(101, "63"),
      localObservation,
      previous: stable,
    });
    expect(runtime.current()).toBe(replacement);
    expect(decisionBridge.recoverExisting).toHaveBeenCalledTimes(4);
  });

  it("durably revokes a raw-L1-rejected cache suffix before native replay", async () => {
    const before = observation(100, "51", null);
    const rejected = observation(102, "52", before.observationDigest);
    let persisted: readonly unknown[] = Object.freeze([before, rejected]);
    const rollbackTo = vi.fn(async () => {
      persisted = Object.freeze([before]);
    });
    const store: WatcherSqliteStateQueueObservationStore = {
      readAll: async () => persisted,
      append: async () => "appended",
      rollbackTo,
    };
    const restore = vi.fn(async ({ persistedObservations }) => ({
      previous: before,
      discardedObservationCount: persistedObservations.length === 2 ? 1 : 0,
      replayIntersection: point(100, "51"),
      catchupBoundary: Object.freeze({
        ...point(103, "53"),
        finalityDepth: "30",
        ogmiosTipBlockNo: "103",
      }),
    }));
    const source: WatcherStateQueueObservationSource = {
      restore,
      bootstrap: async () => {
        throw new Error("nonempty cache must restore");
      },
      observe: async ({ previous }) => previous,
      resolveRetainedHeader: async () => {
        throw new Error("not used");
      },
    };

    const runtime = await createWatcherStateQueueRuntime({
      store,
      source,
    });
    expect(rollbackTo).toHaveBeenCalledWith({
      kind: "point",
      blockHash: "51".repeat(32),
      slot: "1000",
    });
    expect(restore).toHaveBeenCalledTimes(2);
    expect(runtime.current()).toBe(before);
    expect(runtime.replayIntersection).toEqual(point(100, "51"));
  });

  it("starts at the exact reauthenticated cursor and catches an offline queue mutation", async () => {
    const before = observation(100, "11", null);
    const after = observation(102, "22", before.observationDigest);
    const appended: WatcherAuthenticatedStateQueueObservation[] = [];
    const store: WatcherSqliteStateQueueObservationStore = {
      readAll: async () => Object.freeze([before]),
      append: async (value) => {
        appended.push(value);
        return "appended";
      },
      rollbackTo: async () => undefined,
    };
    const source: WatcherStateQueueObservationSource = {
      restore: async () => ({
        previous: before,
        discardedObservationCount: 0,
        replayIntersection: point(100, "11"),
        catchupBoundary: Object.freeze({
          ...point(102, "22"),
          finalityDepth: "30",
          ogmiosTipBlockNo: "102",
        }),
      }),
      bootstrap: async () => {
        throw new Error("nonempty cache must restore");
      },
      observe: async ({ nativeBlock: block, previous }) =>
        block.blockNo === "102" ? after : previous,
      resolveRetainedHeader: async () => {
        throw new Error("not used");
      },
    };
    const decisionBridge = bridge();
    const runtime = await createWatcherStateQueueRuntime({
      store,
      source,
    });
    expect(runtime.replayIntersection).toEqual(point(100, "11"));
    const hooks = runtime.bindFaultDecisionBridge(decisionBridge.value);

    await hooks.onFinalized({
      nativeBlock: nativeBlock(101, "33"),
      localObservation,
      relevance: "touched",
    });
    expect(appended).toEqual([]);
    expect(runtime.current()).toBe(before);

    await hooks.onFinalized({
      nativeBlock: nativeBlock(102, "22"),
      localObservation,
      relevance: "touched",
    });
    await expect(runtime.caughtUp).resolves.toBeUndefined();
    expect(appended).toEqual([after]);
    expect(runtime.current()).toBe(after);
    expect(decisionBridge.reconcileAndDispatch).toHaveBeenLastCalledWith(after);
  });

  it("revokes synchronously before rollback persistence and restores only retained authority", async () => {
    const before = observation(100, "44", null);
    let releaseRollback!: () => void;
    const rollbackGate = new Promise<void>((resolve) => {
      releaseRollback = resolve;
    });
    const events: string[] = [];
    const store: WatcherSqliteStateQueueObservationStore = {
      readAll: async () => Object.freeze([before]),
      append: async () => "appended",
      rollbackTo: async () => {
        events.push("rollback_started");
        await rollbackGate;
        events.push("rollback_finished");
      },
    };
    const source: WatcherStateQueueObservationSource = {
      restore: async () => ({
        previous: before,
        discardedObservationCount: 0,
        replayIntersection: point(100, "44"),
        catchupBoundary: Object.freeze({
          ...point(100, "44"),
          finalityDepth: "30",
          ogmiosTipBlockNo: "100",
        }),
      }),
      bootstrap: async () => {
        throw new Error("not used");
      },
      observe: async ({ previous }) => previous,
      resolveRetainedHeader: async () => {
        throw new Error("not used");
      },
    };
    const decisionBridge = bridge();
    decisionBridge.invalidateForRollback.mockImplementation(() => {
      events.push("revoked");
    });
    const runtime = await createWatcherStateQueueRuntime({
      store,
      source,
    });
    const availability = {
      invalidateForRollback: () => {
        events.push("availability_revoked");
      },
      reconcile: async () => {
        events.push("availability_recovered");
      },
    };
    const hooks = runtime.bindFaultDecisionBridge(
      decisionBridge.value,
      availability,
    );
    const rollingBack = hooks.onRollback({
      kind: "point",
      blockHash: "44".repeat(32),
      slot: "1000",
    });
    expect(events).toEqual([
      "revoked",
      "availability_revoked",
      "rollback_started",
    ]);
    expect(decisionBridge.prepareForRecovery).not.toHaveBeenCalled();

    releaseRollback();
    await rollingBack;
    expect(events).toEqual([
      "revoked",
      "availability_revoked",
      "rollback_started",
      "rollback_finished",
      "availability_recovered",
    ]);
    expect(decisionBridge.prepareForRecovery).toHaveBeenCalledWith(before);
  });

  it("dispatches availability at fresh finalized points even when the queue cursor is unchanged", async () => {
    const before = observation(100, "44", null);
    const current = observation(101, "45", before.observationDigest);
    const append = vi.fn(async () => "appended" as const);
    const source: WatcherStateQueueObservationSource = {
      restore: async () => ({
        previous: before,
        discardedObservationCount: 0,
        replayIntersection: point(100, "44"),
        catchupBoundary: {
          ...point(100, "44"),
          finalityDepth: "30",
          ogmiosTipBlockNo: "130",
        },
      }),
      bootstrap: async () => {
        throw new Error("not used");
      },
      observe: async () => before,
      latestFinalizedObservation: () => current,
      resolveRetainedHeader: async () => {
        throw new Error("not used");
      },
    };
    const runtime = await createWatcherStateQueueRuntime({
      source,
      store: {
        readAll: async () => [before],
        append,
        rollbackTo: async () => undefined,
      },
    });
    const decisionBridge = bridge();
    const reconcile = vi.fn(async () => undefined);
    const hooks = runtime.bindFaultDecisionBridge(decisionBridge.value, {
      reconcile,
      invalidateForRollback: () => undefined,
    });
    await hooks.onFinalized({
      nativeBlock: nativeBlock(101, "45"),
      localObservation,
      relevance: "touched",
    });
    expect(append).not.toHaveBeenCalled();
    expect(runtime.current()).toBe(current);
    expect(reconcile).toHaveBeenCalledWith(current, true);
    expect(decisionBridge.reconcileAndDispatch).toHaveBeenCalledWith(current);
    expect(reconcile.mock.invocationCallOrder[0]).toBeLessThan(
      decisionBridge.reconcileAndDispatch.mock.invocationCallOrder[0]!,
    );
  });
  it("replays the finalized block already recorded as the durable cursor without re-observing it", async () => {
    // A crash between appending the observation and recording block progress
    // makes the coordinator deliver the same finalized block again on restart.
    const before = observation(100, "44", null);
    const current = observation(101, "45", before.observationDigest);
    const observe = vi.fn(async () => {
      throw new Error(
        "state-queue observation predecessor is foreign or non-monotone",
      );
    });
    const append = vi.fn(async () => "appended" as const);
    const source: WatcherStateQueueObservationSource = {
      restore: async () => ({
        previous: current,
        discardedObservationCount: 0,
        replayIntersection: point(101, "45"),
        catchupBoundary: {
          ...point(101, "45"),
          finalityDepth: "30",
          ogmiosTipBlockNo: "131",
        },
      }),
      bootstrap: async () => {
        throw new Error("not used");
      },
      observe,
      resolveRetainedHeader: async () => {
        throw new Error("not used");
      },
    };
    const runtime = await createWatcherStateQueueRuntime({
      source,
      store: {
        readAll: async () => [before, current],
        append,
        rollbackTo: async () => undefined,
      },
    });
    const decisionBridge = bridge();
    const reconcile = vi.fn(async () => undefined);
    const hooks = runtime.bindFaultDecisionBridge(decisionBridge.value, {
      reconcile,
      invalidateForRollback: () => undefined,
    });
    await hooks.onFinalized({
      nativeBlock: nativeBlock(101, "45"),
      localObservation,
      relevance: "touched",
    });
    expect(observe).not.toHaveBeenCalled();
    expect(append).not.toHaveBeenCalled();
    expect(runtime.current()).toBe(current);
    expect(reconcile).toHaveBeenCalledWith(current, true);
    expect(decisionBridge.reconcileAndDispatch).toHaveBeenCalledWith(current);
    await expect(
      hooks.onFinalized({
        nativeBlock: nativeBlock(101, "46"),
        localObservation,
        relevance: "touched",
      }),
    ).rejects.toThrow("non-monotone");
  });

  it.each([
    { blockNo: 102, byte: "33", reason: "foreign" },
    { blockNo: 103, byte: "22", reason: "skipped" },
  ])(
    "refuses a $reason historical catch-up point with the Ogmios tip ahead",
    async ({ blockNo, byte, reason }) => {
      const before = observation(100, "11", null);
      const source: WatcherStateQueueObservationSource = {
        restore: async () => ({
          previous: before,
          discardedObservationCount: 0,
          replayIntersection: point(100, "11"),
          catchupBoundary: {
            ...point(102, "22"),
            finalityDepth: "30",
            ogmiosTipBlockNo: "132",
          },
        }),
        bootstrap: async () => {
          throw new Error("nonempty cache must restore");
        },
        observe: async ({ previous }) => previous,
        resolveRetainedHeader: async () => {
          throw new Error("not used");
        },
      };
      const store: WatcherSqliteStateQueueObservationStore = {
        readAll: async () => [before],
        append: async () => "appended",
        rollbackTo: async () => undefined,
      };
      const runtime = await createWatcherStateQueueRuntime({ store, source });
      const decisionBridge = bridge();
      const hooks = runtime.bindFaultDecisionBridge(decisionBridge.value);
      await expect(
        hooks.onFinalized({
          nativeBlock: nativeBlock(blockNo, byte),
          localObservation,
          relevance: "touched",
        }),
      ).rejects.toThrow(reason);
      await expect(runtime.caughtUp).rejects.toThrow(reason);
      expect(decisionBridge.reconcileAndDispatch).not.toHaveBeenCalled();
    },
  );

  it.each([true, false])(
    "forwards quiet canonical progress to wake yielded proofs without queue queries (inclusion=%s)",
    async (hasInclusion) => {
      const before = observation(100, "44", null);
      const observe = vi.fn(async () => before);
      const observeIncluded = vi.fn(async () => before);
      const append = vi.fn(async () => "appended" as const);
      const source: WatcherStateQueueObservationSource = {
        restore: async () => ({
          previous: before,
          discardedObservationCount: 0,
          replayIntersection: point(100, "44"),
          catchupBoundary: {
            ...point(100, "44"),
            finalityDepth: "30",
            ogmiosTipBlockNo: "130",
          },
        }),
        bootstrap: async () => {
          throw new Error("not used");
        },
        observe,
        ...(hasInclusion ? { observeIncluded } : {}),
        resolveRetainedHeader: async () => {
          throw new Error("not used");
        },
      };
      const runtime = await createWatcherStateQueueRuntime({
        source,
        store: {
          readAll: async () => [before],
          append,
          rollbackTo: async () => undefined,
        },
      });
      const decisionBridge = bridge();
      const hooks = runtime.bindFaultDecisionBridge(decisionBridge.value);
      const progress = nativeBlock(131, "45");
      const input = {
        nativeBlock: progress,
        localObservation: null,
        relevance: "quiet" as const,
      };
      if (hasInclusion) await hooks.onIncluded!(input);
      else await hooks.onFinalized(input);
      expect(decisionBridge.recoverExisting).toHaveBeenCalledWith({
        nativeProgress: progress,
      });
      expect(observe).not.toHaveBeenCalled();
      expect(observeIncluded).not.toHaveBeenCalled();
      expect(append).not.toHaveBeenCalled();
      expect(decisionBridge.reconcileAndDispatch).not.toHaveBeenCalled();
      expect(runtime.current()).toBe(before);
    },
  );

  it("advances a quiet block through the catch-up boundary without touching the queue", async () => {
    const before = observation(100, "44", null);
    const observe = vi.fn(async () => before);
    const append = vi.fn(async () => "appended" as const);
    const source: WatcherStateQueueObservationSource = {
      restore: async () => ({
        previous: before,
        discardedObservationCount: 0,
        replayIntersection: point(100, "44"),
        catchupBoundary: {
          ...point(101, "45"),
          finalityDepth: "30",
          ogmiosTipBlockNo: "131",
        },
      }),
      bootstrap: async () => {
        throw new Error("not used");
      },
      observe,
      resolveRetainedHeader: async () => {
        throw new Error("not used");
      },
    };
    const runtime = await createWatcherStateQueueRuntime({
      source,
      store: {
        readAll: async () => [before],
        append,
        rollbackTo: async () => undefined,
      },
    });
    const decisionBridge = bridge();
    const reconcile = vi.fn(async () => undefined);
    const hooks = runtime.bindFaultDecisionBridge(decisionBridge.value, {
      reconcile,
      invalidateForRollback: () => undefined,
    });
    await hooks.onFinalized({
      nativeBlock: nativeBlock(101, "45"),
      localObservation: null,
      relevance: "quiet",
    });
    await expect(runtime.caughtUp).resolves.toBeUndefined();
    await hooks.onFinalized({
      nativeBlock: nativeBlock(102, "46"),
      localObservation: null,
      relevance: "quiet",
    });
    expect(observe).not.toHaveBeenCalled();
    expect(append).not.toHaveBeenCalled();
    expect(reconcile).not.toHaveBeenCalled();
    expect(decisionBridge.reconcileAndDispatch).not.toHaveBeenCalled();
    expect(runtime.current()).toBe(before);

    await expect(
      hooks.onFinalized({
        nativeBlock: nativeBlock(103, "47"),
        localObservation: null,
        relevance: "touched",
      }),
    ).rejects.toThrow("touched block finalized without a local observation");
  });
});
