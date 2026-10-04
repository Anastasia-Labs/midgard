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

describe("oldest retained queue rollback", () => {
  it("rebuilds a canonical snapshot when rollback orphans the oldest retained row", async () => {
    const old = observation(100, "71", null);
    const replacement = observation(101, "72", null);
    let rows = [old];
    const recovery = (previous: WatcherAuthenticatedStateQueueObservation) => ({
      previous,
      discardedObservationCount: 0,
      replayIntersection: previous.nativePoint,
      catchupBoundary: { ...previous.nativePoint, ogmiosTipBlockNo: "130" },
    });
    const bootstrap = vi.fn(async () => recovery(replacement));
    const runtime = await createWatcherStateQueueRuntime({
      source: {
        restore: async () => recovery(old),
        bootstrap,
        observe: async () => {
          throw new Error(
            "old prefix cannot be replayed as a new queue successor",
          );
        },
      } as unknown as WatcherStateQueueObservationSource,
      store: {
        readAll: async () => rows,
        rollbackTo: async () => {
          rows = [];
        },
        append: async (next) => {
          rows.push(next);
          return "appended";
        },
      } as WatcherSqliteStateQueueObservationStore,
    });
    const decisions = bridge();
    const hooks = runtime.bindFaultDecisionBridge(decisions.value);
    await hooks.onRollback({
      kind: "point",
      blockHash: "70".repeat(32),
      slot: "990",
    });
    expect(bootstrap).toHaveBeenCalledOnce();
    expect(rows).toEqual([replacement]);
    expect(runtime.current()).toBe(replacement);
    expect(decisions.invalidateForRollback).toHaveBeenCalledOnce();
    await hooks.onFinalized({
      nativeBlock: nativeBlock(100, "73"),
      localObservation,
      relevance: "touched",
    });
    expect(decisions.prepareForRecovery).not.toHaveBeenCalled();
    await hooks.onFinalized({
      nativeBlock: nativeBlock(101, "72"),
      localObservation,
      relevance: "touched",
    });
    expect(decisions.prepareForRecovery).toHaveBeenCalledWith(replacement);
    expect(decisions.reconcileAndDispatch).toHaveBeenCalledWith(replacement);
  });
});
