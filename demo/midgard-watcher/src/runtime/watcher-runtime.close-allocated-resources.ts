import { type WatcherAvailabilityRuntime } from "../availability/runtime.js";
import { type WatcherFaultDecisionBridge } from "../fault-proofs/fault-decision-bridge.js";
import { type WatcherFaultProofApplication } from "../fault-proofs/fault-proof-application.js";
import { type WatcherFaultProofSupervisor } from "../fault-proofs/fault-proof-supervisor.js";
import { type WatcherSqliteProverFundingReservationStoreRuntime } from "../funding/sqlite-prover-funding-reservation-store.js";
import type { WatcherStateQueueReadScopes } from "../indexers/authenticated-state-queue-observation.read-scopes.js";
import { createWatcherLocalKupmiosNativeObservationRuntime } from "../l1/local-kupmios-native-observation.js";
import { type WatcherNativeChainSyncRuntime } from "../l1/native-chain-sync.js";
import { type WatcherRetainedDaOperationsBinding } from "../storage/retained-da-runtime.js";
import { openWatcherSqliteDurableBackend } from "../storage/sqlite-durable-backend.js";
import { type WatcherChainCoordinator } from "./chain-coordinator.js";
import { createWatcherHistoryRecovery } from "./history-recovery.js";
import { type WatcherOperationsHttpServer } from "./operations-http.js";
import { type WatcherUserEventRuntime } from "./user-event-runtime.js";

export const closeWatcherAllocatedResources = async (
  allocated: Readonly<{
    readScopes?: () => WatcherStateQueueReadScopes | undefined;
    historyRecovery: () =>
      | ReturnType<typeof createWatcherHistoryRecovery>
      | undefined;
    activeCoordinator: () => WatcherChainCoordinator | undefined;
    faultDecisionBridge: () => WatcherFaultDecisionBridge | undefined;
    availability: () => WatcherAvailabilityRuntime | undefined;
    retainedDaOperationsBinding: () =>
      | WatcherRetainedDaOperationsBinding
      | undefined;
    operationsHttp: () => WatcherOperationsHttpServer | undefined;
    native: () => WatcherNativeChainSyncRuntime | undefined;
    faultProofSupervisor: () => WatcherFaultProofSupervisor | undefined;
    allocatedFaultProofApplication: () =>
      | WatcherFaultProofApplication
      | undefined;
    observation: () =>
      | Awaited<
          ReturnType<typeof createWatcherLocalKupmiosNativeObservationRuntime>
        >
      | undefined;
    proverFundingStore: () =>
      | WatcherSqliteProverFundingReservationStoreRuntime
      | undefined;
    userEventRuntime: () => WatcherUserEventRuntime | undefined;
    sqlite: () => Awaited<ReturnType<typeof openWatcherSqliteDurableBackend>>;
  }>,
): Promise<void> => {
  allocated.readScopes?.()?.close();
  allocated.historyRecovery()?.close();
  const coordinatorStopped = allocated.activeCoordinator()?.stop();
  const failures: unknown[] = [];
  allocated.faultDecisionBridge()?.invalidateForShutdown();
  allocated.availability()?.invalidateForShutdown();
  try {
    allocated.retainedDaOperationsBinding()?.close();
  } catch (error) {
    failures.push(error);
  }
  const operationsHttp = allocated.operationsHttp();
  if (operationsHttp !== undefined) {
    try {
      await operationsHttp.close();
    } catch (error) {
      failures.push(error);
    }
  }
  const native = allocated.native();
  if (native !== undefined) {
    try {
      await native.close();
    } catch (error) {
      failures.push(error);
    }
  }
  try {
    await coordinatorStopped;
  } catch (error) {
    failures.push(error);
  }
  const faultProofSupervisor = allocated.faultProofSupervisor();
  if (faultProofSupervisor !== undefined) {
    try {
      await faultProofSupervisor.close();
    } catch (error) {
      failures.push(error);
    }
  }
  const allocatedFaultProofApplication =
    allocated.allocatedFaultProofApplication();
  if (allocatedFaultProofApplication !== undefined) {
    try {
      await allocatedFaultProofApplication.close();
    } catch (error) {
      failures.push(error);
    }
  }
  const availability = allocated.availability();
  if (availability !== undefined) {
    try {
      await availability.close();
    } catch (error) {
      failures.push(error);
    }
  }
  try {
    allocated.observation()?.close();
  } catch (error) {
    failures.push(error);
  }
  try {
    allocated.proverFundingStore()?.close();
  } catch (error) {
    failures.push(error);
  }
  const userEventRuntime = allocated.userEventRuntime();
  if (userEventRuntime !== undefined) {
    try {
      await userEventRuntime.close();
    } catch (error) {
      failures.push(error);
    }
  }
  try {
    allocated.sqlite().close();
  } catch (error) {
    failures.push(error);
  }
  if (failures.length > 0) {
    throw new AggregateError(
      failures,
      "watcher production runtime shutdown failed",
    );
  }
};
