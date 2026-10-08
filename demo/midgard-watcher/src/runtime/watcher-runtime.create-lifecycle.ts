import { type WatcherAvailabilityRuntime } from "../availability/runtime.js";
import {
  type WatcherFaultProofApplication,
  type WatcherFaultProofStartupReadiness,
} from "../fault-proofs/fault-proof-application.js";
import { type WatcherFaultProofSupervisor } from "../fault-proofs/fault-proof-supervisor.js";
import { type WatcherFollowerRuntime } from "../l1-follower/follower-runtime.js";
import { type WatcherOperationsHttpServer } from "./operations-http.js";
import { type WatcherDecisionDriver } from "./watcher-runtime.decision-driver.js";
import {
  WATCHER_RUNTIME_SCHEMA_VERSION,
  type WatcherRuntime,
} from "./watcher-runtime.launch-checks.js";

/**
 * The running watcher. Liveness ends only when the proof supervisor or the
 * operations server stops; an L1 condition (the follower behind, waiting or
 * stopped on an intervention, a failed decision pass, a failed user-event
 * history) is a readiness reason and never ends the process.
 */
export const createWatcherRuntimeLifecycle = (
  input: Readonly<{
    deploymentAuthority: WatcherRuntime["deploymentAuthority"];
    faultProofApplication: WatcherFaultProofApplication;
    faultProofReadiness: readonly WatcherFaultProofStartupReadiness[];
    faultProofSupervisor: WatcherFaultProofSupervisor;
    operations: WatcherRuntime["operations"];
    operationsHttp: WatcherOperationsHttpServer;
    recoveredFaultProofWorkflowCount: number;
    availability: WatcherAvailabilityRuntime;
    follower: WatcherFollowerRuntime;
    decisionDriver: WatcherDecisionDriver;
    closeAllocatedResources: () => Promise<void>;
  }>,
): WatcherRuntime => {
  const {
    deploymentAuthority,
    faultProofApplication,
    faultProofReadiness,
    faultProofSupervisor,
    operations,
    operationsHttp,
    recoveredFaultProofWorkflowCount,
    availability,
    follower,
    decisionDriver,
    closeAllocatedResources,
  } = input;
  let phase: "live" | "closing" | "closed" | "failed" = "live";
  let caughtUp = false;
  let closePromise: Promise<void> | undefined;
  const runtimeDone = Promise.race([
    faultProofSupervisor.done,
    operationsHttp.done,
  ]);
  const caughtUpPromise = Promise.race([
    decisionDriver.caughtUp.then(() => {
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
    faultProofApplication,
    faultProofReadiness: Object.freeze(faultProofReadiness),
    faultProofSupervisor,
    operations,
    operationsEndpoint: operationsHttp.endpoint,
    recoveredFaultProofWorkflowCount,
    availability,
    follower,
    decisionDriver,
    done: runtimeDone,
    caughtUp: caughtUpPromise,
    status: () => {
      const proofSupervisor = faultProofSupervisor.status();
      const operationsStatus = operations.api.status();
      const availabilityStatus = availability.status();
      const liveness = phase === "live";
      return Object.freeze({
        phase,
        liveness,
        readiness:
          liveness &&
          caughtUp &&
          proofSupervisor.phase === "accepting" &&
          proofSupervisor.recovered &&
          proofSupervisor.deadlineHealth === "safe" &&
          availabilityStatus.phase !== "blocked" &&
          operationsStatus.readiness === "ready",
        caughtUp,
        l1Readiness: operationsStatus.l1Readiness,
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
