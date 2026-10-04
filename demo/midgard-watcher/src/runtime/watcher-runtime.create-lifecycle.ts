import { type WatcherAvailabilityRuntime } from "../availability/runtime.js";
import {
  type WatcherFaultProofApplication,
  type WatcherFaultProofStartupReadiness,
} from "../fault-proofs/fault-proof-application.js";
import { type WatcherFaultProofSupervisor } from "../fault-proofs/fault-proof-supervisor.js";
import { type WatcherNativeChainSyncRuntime } from "../l1/native-chain-sync.js";
import { type WatcherChainCoordinator } from "./chain-coordinator.js";
import { createWatcherHistoryRecovery } from "./history-recovery.js";
import { type WatcherOperationsHttpServer } from "./operations-http.js";
import { createWatcherStateQueueRuntime } from "./state-queue-runtime.js";
import { type WatcherUserEventRuntime } from "./user-event-runtime.js";
import {
  WATCHER_RUNTIME_SCHEMA_VERSION,
  type WatcherRuntime,
} from "./watcher-runtime.create-watcher-native-event-handler.js";

export const createWatcherRuntimeLifecycle = (
  input: Readonly<{
    deploymentAuthority: WatcherRuntime["deploymentAuthority"];
    policy: WatcherRuntime["policy"];
    coordinator: WatcherChainCoordinator;
    faultProofApplication: WatcherFaultProofApplication;
    faultProofReadiness: readonly WatcherFaultProofStartupReadiness[];
    faultProofSupervisor: WatcherFaultProofSupervisor;
    operations: WatcherRuntime["operations"];
    operationsHttp: WatcherOperationsHttpServer;
    recoveredFaultProofWorkflowCount: number;
    availability: WatcherAvailabilityRuntime;
    recovery: ReturnType<typeof createWatcherHistoryRecovery>;
    native: WatcherNativeChainSyncRuntime;
    activeUserEventRuntime: WatcherUserEventRuntime;
    nativeCaughtUp: Promise<void>;
    stateQueueRuntime: Awaited<
      ReturnType<typeof createWatcherStateQueueRuntime>
    >;
    closeAllocatedResources: () => Promise<void>;
  }>,
): WatcherRuntime => {
  const {
    deploymentAuthority,
    policy,
    coordinator,
    faultProofApplication,
    faultProofReadiness,
    faultProofSupervisor,
    operations,
    operationsHttp,
    recoveredFaultProofWorkflowCount,
    availability,
    recovery,
    native,
    activeUserEventRuntime,
    nativeCaughtUp,
    stateQueueRuntime,
    closeAllocatedResources,
  } = input;
  let phase: "live" | "closing" | "closed" | "failed" = "live";
  let caughtUp = false;
  let closePromise: Promise<void> | undefined;
  const activeFaultProofSupervisor = faultProofSupervisor;
  const runtimeDone = Promise.race([
    recovery.done,
    native.done,
    activeUserEventRuntime.done,
    activeFaultProofSupervisor.done,
    operationsHttp.done,
  ]);
  const caughtUpPromise = Promise.race([
    Promise.all([nativeCaughtUp, stateQueueRuntime.caughtUp]).then(async () => {
      do {
        await recovery.waitForRecovery();
        await coordinator.waitForDelivery();
      } while (
        recovery.status().pending ||
        coordinator.status().deliveryHeld ||
        coordinator.status().rollbackPoint !== null
      );
      caughtUp = true;
    }),
    runtimeDone.then(() => {
      throw new Error(
        "watcher production liveness ended before durable catch-up",
      );
    }),
  ]);
  void caughtUpPromise.catch(() => undefined);
  void runtimeDone.then(
    () => {
      if (phase === "live") phase = "failed";
    },
    () => {
      if (phase === "live") phase = "failed";
    },
  );
  const runtime: WatcherRuntime = Object.freeze({
    schemaVersion: WATCHER_RUNTIME_SCHEMA_VERSION,
    deploymentAuthority,
    policy,
    coordinator,
    faultProofApplication,
    faultProofReadiness: Object.freeze(faultProofReadiness),
    faultProofSupervisor: activeFaultProofSupervisor,
    operations,
    operationsEndpoint: operationsHttp.endpoint,
    recoveredFaultProofWorkflowCount,
    availability,
    done: runtimeDone,
    caughtUp: caughtUpPromise,
    status: () => {
      const proofSupervisor = activeFaultProofSupervisor.status();
      const operationsStatus = operations.api.status();
      const availabilityStatus = availability!.status();
      const liveness = phase === "live";
      return Object.freeze({
        phase,
        liveness,
        readiness:
          liveness &&
          caughtUp &&
          !recovery.status().pending &&
          !coordinator.status().deliveryHeld &&
          !coordinator.status().quarantined &&
          proofSupervisor.phase === "accepting" &&
          proofSupervisor.recovered &&
          proofSupervisor.deadlineHealth === "safe" &&
          availabilityStatus.phase !== "blocked" &&
          operationsStatus.readiness === "ready",
        caughtUp,
        historyRecovery: recovery.status(),
        proofSupervisor,
        availability: availabilityStatus,
      });
    },
    close: () => {
      if (closePromise !== undefined) return closePromise;
      phase = "closing";
      closePromise = closeAllocatedResources().then(
        () => {
          phase = "closed";
        },
        (error: unknown) => {
          phase = "failed";
          throw error;
        },
      );
      return closePromise;
    },
  });
  return runtime;
};

export const createWatcherRuntimeSignals = () => {
  let resolveCoordinator!: (value: WatcherChainCoordinator) => void;
  let rejectCoordinator!: (reason: Error) => void;
  const coordinatorReady = new Promise<WatcherChainCoordinator>(
    (resolve, reject) => {
      resolveCoordinator = resolve;
      rejectCoordinator = reject;
    },
  );
  // Startup can fail before the event handler awaits this promise. Observe
  // rejection immediately while leaving the original promise rejecting.
  void coordinatorReady.catch(() => undefined);
  let resolveCaughtUp!: () => void;
  let rejectCaughtUp!: (reason: Error) => void;
  const nativeCaughtUp = new Promise<void>((resolve, reject) => {
    resolveCaughtUp = resolve;
    rejectCaughtUp = reject;
  });
  void nativeCaughtUp.catch(() => undefined);
  return {
    coordinatorReady,
    resolveCoordinator,
    rejectCoordinator,
    nativeCaughtUp,
    resolveCaughtUp,
    rejectCaughtUp,
  };
};
