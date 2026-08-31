import { describe, expect, it, vi } from "vitest";

import type { WatcherLocalKupmiosNativeObservationV1 } from "../src/local-kupmios-native-observation-v1.js";
import type { WatcherNativeBlockAdmissionV1 } from "../src/native-block-admission-v1.js";
import type { WatcherProductionFaultDecisionBridgeV1 } from "../src/production-fault-decision-bridge-v1.js";
import type {
  WatcherProductionStateQueueObservationSourceV1,
  WatcherProductionStateQueueObservationV1,
} from "../src/production-state-queue-observation-v1.js";
import { createWatcherProductionStateQueueRuntimeV1 } from "../src/production-state-queue-runtime-v1.js";
import type { WatcherSqliteStateQueueObservationStoreV1 } from "../src/sqlite-durable-backend-v1.js";

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
): WatcherProductionStateQueueObservationV1 =>
  Object.freeze({
    observationDigest: byte.repeat(32),
    previousObservationDigest,
    nativePoint: Object.freeze({
      ...point(blockNo, byte),
      parentBlockHash: "00".repeat(32),
      finalityDepth: "30",
    }),
  }) as WatcherProductionStateQueueObservationV1;

const nativeBlock = (
  blockNo: number,
  byte: string,
): WatcherNativeBlockAdmissionV1 =>
  Object.freeze({
    blockHash: byte.repeat(32),
    blockNo: blockNo.toString(),
    slot: (blockNo * 10).toString(),
  }) as WatcherNativeBlockAdmissionV1;

const localObservation = Object.freeze(
  {},
) as WatcherLocalKupmiosNativeObservationV1;

const bridge = () => {
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
    invalidateForRollback,
    prepareForRecovery,
    reconcileAndDispatch,
    value: Object.freeze({
      invalidateForRollback,
      prepareForRecovery,
      reconcileAndDispatch,
    }) as unknown as WatcherProductionFaultDecisionBridgeV1,
  };
};

describe("production state-queue runtime V1", () => {
  it("durably revokes a raw-L1-rejected cache suffix before native replay", async () => {
    const before = observation(100, "51", null);
    const rejected = observation(102, "52", before.observationDigest);
    let persisted: readonly unknown[] = Object.freeze([before, rejected]);
    const rollbackTo = vi.fn(async () => {
      persisted = Object.freeze([before]);
    });
    const store: WatcherSqliteStateQueueObservationStoreV1 = {
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
    const source: WatcherProductionStateQueueObservationSourceV1 = {
      restore,
      bootstrap: async () => {
        throw new Error("nonempty cache must restore");
      },
      observe: async ({ previous }) => previous,
      resolveRetainedHeader: async () => {
        throw new Error("not used");
      },
    };

    const runtime = await createWatcherProductionStateQueueRuntimeV1({
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
    const appended: WatcherProductionStateQueueObservationV1[] = [];
    const store: WatcherSqliteStateQueueObservationStoreV1 = {
      readAll: async () => Object.freeze([before]),
      append: async (value) => {
        appended.push(value);
        return "appended";
      },
      rollbackTo: async () => undefined,
    };
    const source: WatcherProductionStateQueueObservationSourceV1 = {
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
    const runtime = await createWatcherProductionStateQueueRuntimeV1({
      store,
      source,
    });
    expect(runtime.replayIntersection).toEqual(point(100, "11"));
    const hooks = runtime.bindFaultDecisionBridge(decisionBridge.value);

    await hooks.onFinalized({
      nativeBlock: nativeBlock(101, "33"),
      localObservation,
    });
    expect(appended).toEqual([]);
    expect(runtime.current()).toBe(before);

    await hooks.onFinalized({
      nativeBlock: nativeBlock(102, "22"),
      localObservation,
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
    const store: WatcherSqliteStateQueueObservationStoreV1 = {
      readAll: async () => Object.freeze([before]),
      append: async () => "appended",
      rollbackTo: async () => {
        events.push("rollback_started");
        await rollbackGate;
        events.push("rollback_finished");
      },
    };
    const source: WatcherProductionStateQueueObservationSourceV1 = {
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
    const runtime = await createWatcherProductionStateQueueRuntimeV1({
      store,
      source,
    });
    const hooks = runtime.bindFaultDecisionBridge(decisionBridge.value);
    const rollingBack = hooks.onRollback({
      kind: "point",
      blockHash: "44".repeat(32),
      slot: "1000",
    });
    expect(events).toEqual(["revoked", "rollback_started"]);
    expect(decisionBridge.prepareForRecovery).not.toHaveBeenCalled();

    releaseRollback();
    await rollingBack;
    expect(events).toEqual([
      "revoked",
      "rollback_started",
      "rollback_finished",
    ]);
    expect(decisionBridge.prepareForRecovery).toHaveBeenCalledWith(before);
  });
});
