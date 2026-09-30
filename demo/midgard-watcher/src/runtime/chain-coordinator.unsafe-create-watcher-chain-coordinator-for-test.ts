import { type WatcherFinalityPolicy } from "../l1/finality-engine.js";
import type { WatcherLocalKupmiosNativeObservationRuntime } from "../l1/local-kupmios-native-observation.js";
import type { WatcherNativeChainSyncPoint } from "../l1/native-chain-sync.js";
import type { WatcherDurableRuntime } from "../storage/durable-runtime.js";
import {
  type AdmitRollForward,
  productionDependencies,
  type WatcherChainCoordinator,
  type WatcherChainCoordinatorDependencies,
  type WatcherChainCoordinatorHooks,
} from "./chain-coordinator.canonical-path-from-history.js";
import { createCoordinator } from "./chain-coordinator.create-coordinator.js";

export const createWatcherChainCoordinator = (input: {
  readonly policy: WatcherFinalityPolicy;
  readonly durable: WatcherDurableRuntime;
  readonly observation: WatcherLocalKupmiosNativeObservationRuntime;
  readonly restartIntersection?: WatcherNativeChainSyncPoint;
  readonly hooks: WatcherChainCoordinatorHooks;
  readonly relevance?: WatcherChainCoordinatorDependencies["relevance"];
  readonly progress?: WatcherChainCoordinatorDependencies["progress"];
}): WatcherChainCoordinator =>
  createCoordinator({
    ...input,
    dependencies: {
      ...productionDependencies,
      relevance: input.relevance,
      progress: input.progress,
    },
  });

/** Test-only seam for independently exercising ordering and rollback states. */
export const unsafeCreateWatcherChainCoordinatorForTest = (
  input: {
    readonly policy: WatcherFinalityPolicy;
    readonly durable: WatcherDurableRuntime;
    readonly observation: WatcherLocalKupmiosNativeObservationRuntime;
    readonly restartIntersection?: WatcherNativeChainSyncPoint;
    readonly hooks?: WatcherChainCoordinatorHooks;
  },
  dependencies: Readonly<{ admitRollForward: AdmitRollForward }> &
    WatcherChainCoordinatorDependencies,
): WatcherChainCoordinator =>
  createCoordinator({
    ...input,
    hooks:
      input.hooks ??
      Object.freeze({
        onRollback: async () => undefined,
        onFinalized: async () => undefined,
      }),
    dependencies,
  });
