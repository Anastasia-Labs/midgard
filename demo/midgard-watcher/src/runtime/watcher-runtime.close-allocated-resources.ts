import { type WatcherAvailabilityRuntime } from "../availability/runtime.js";
import { type WatcherFaultDecisionBridge } from "../fault-proofs/fault-decision-bridge.js";
import { type WatcherFaultProofApplication } from "../fault-proofs/fault-proof-application.js";
import { type WatcherFaultProofSupervisor } from "../fault-proofs/fault-proof-supervisor.js";
import { type WatcherSqliteProverFundingReservationStoreRuntime } from "../funding/sqlite-prover-funding-reservation-store.js";
import { type WatcherFollowerRuntime } from "../l1-follower/follower-runtime.js";
import { type WatcherRetainedDaOperationsBinding } from "../storage/retained-da-runtime.js";
import { openWatcherSqliteDurableBackend } from "../storage/sqlite-durable-backend.js";
import { type WatcherOperationsHttpServer } from "./operations-http.js";
import { type WatcherUserEventRuntime } from "./user-event-runtime.js";
import { type WatcherDecisionDriver } from "./watcher-runtime.decision-driver.js";

/**
 * Closes whatever startup allocated, newest authority first: decisions and
 * availability lose their authority synchronously before any await, then
 * the driver, the HTTP surface, the supervisor and the stores close, and the
 * follower (with its node transport) last.
 */
export const closeWatcherAllocatedResources = async (
  allocated: Readonly<{
    decisionDriver: () => WatcherDecisionDriver | undefined;
    follower: () => WatcherFollowerRuntime | undefined;
    faultDecisionBridge: () => WatcherFaultDecisionBridge | undefined;
    availability: () => WatcherAvailabilityRuntime | undefined;
    retainedDaOperationsBinding: () =>
      | WatcherRetainedDaOperationsBinding
      | undefined;
    operationsHttp: () => WatcherOperationsHttpServer | undefined;
    faultProofSupervisor: () => WatcherFaultProofSupervisor | undefined;
    allocatedFaultProofApplication: () =>
      | WatcherFaultProofApplication
      | undefined;
    proverFundingStore: () =>
      | WatcherSqliteProverFundingReservationStoreRuntime
      | undefined;
    userEventRuntime: () => WatcherUserEventRuntime | undefined;
    sqlite: () => Awaited<ReturnType<typeof openWatcherSqliteDurableBackend>>;
  }>,
): Promise<void> => {
  const failures: unknown[] = [];
  allocated.faultDecisionBridge()?.invalidateForShutdown();
  allocated.availability()?.invalidateForShutdown();
  const attempt = async (close: () => unknown): Promise<void> => {
    try {
      await close();
    } catch (error) {
      failures.push(error);
    }
  };
  await attempt(() => allocated.decisionDriver()?.close());
  await attempt(() => allocated.retainedDaOperationsBinding()?.close());
  await attempt(() => allocated.operationsHttp()?.close());
  await attempt(() => allocated.faultProofSupervisor()?.close());
  await attempt(() => allocated.allocatedFaultProofApplication()?.close());
  await attempt(() => allocated.availability()?.close());
  await attempt(() => allocated.proverFundingStore()?.close());
  await attempt(() => allocated.userEventRuntime()?.close());
  await attempt(() => allocated.follower()?.close());
  await attempt(() => allocated.sqlite().close());
  if (failures.length > 0) {
    throw new AggregateError(
      failures,
      "watcher production runtime shutdown failed",
    );
  }
};
