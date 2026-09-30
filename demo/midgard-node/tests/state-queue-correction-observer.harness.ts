import {
  type StateQueueAuthenticatedTransition,
  type StateQueueTransitionNode,
} from "@al-ft/midgard-sdk";
import { vi } from "vitest";

import {
  reconcileStateQueueCorrectionObserver,
  type StateQueueCorrectionObserverSource,
  type StateQueueCorrectionObserverStore,
} from "../src/services/state-queue-correction-observer.js";
import {
  before,
  checkpointFromTerminal,
  deployment,
  policy,
} from "./state-queue-correction-observer.authenticated-fraud-transition.js";

export const memoryStore = (): StateQueueCorrectionObserverStore & {
  current: () => unknown | null;
} => {
  let value: unknown | null = null;
  return {
    load: async () => structuredClone(value),
    save: async (next) => {
      value = structuredClone(next);
    },
    current: () => structuredClone(value),
  };
};

export const harness = () => {
  let queue = before;
  type Observation = Awaited<
    ReturnType<StateQueueCorrectionObserverSource["observeTransitions"]>
  >[number];
  let observations: readonly Observation[] | "gap" = [checkpointFromTerminal()];
  let depth: bigint | null = 1n;
  const depthByTransaction = new Map<string, bigint | null>();
  const observeTransitions = vi.fn(async () => {
    if (observations === "gap") {
      throw new Error("ordered-transition gap; durable cursor retained");
    }
    return observations;
  });
  const source: StateQueueCorrectionObserverSource = {
    readQueue: async () => queue,
    observeTransitions,
    canonicalDepth: async (transition) =>
      depthByTransaction.has(transition.transactionHash)
        ? depthByTransaction.get(transition.transactionHash)!
        : depth,
  };
  return {
    source,
    setQueue: (next: readonly StateQueueTransitionNode[]) => {
      queue = next;
    },
    setDepth: (next: bigint | null) => {
      depth = next;
    },
    setTransactionDepth: (txHash: string, next: bigint | null) => {
      depthByTransaction.set(txHash, next);
    },
    setObservation: (next: Observation | "gap") => {
      observations = next === "gap" ? "gap" : [next];
    },
    setObservations: (next: readonly Observation[]) => {
      observations = next;
    },
    observeTransitions,
  };
};

export const run = async ({
  source,
  store,
  reinclude,
  restore,
  persistTerminal,
  revokeTerminal,
  assertRollbackPermitted,
}: {
  source: StateQueueCorrectionObserverSource;
  store: StateQueueCorrectionObserverStore;
  reinclude: (transition: StateQueueAuthenticatedTransition) => Promise<void>;
  restore: (transition: StateQueueAuthenticatedTransition) => Promise<void>;
  persistTerminal?: Parameters<
    typeof reconcileStateQueueCorrectionObserver
  >[0]["persistTerminal"];
  revokeTerminal?: Parameters<
    typeof reconcileStateQueueCorrectionObserver
  >[0]["revokeTerminal"];
  assertRollbackPermitted?: Parameters<
    typeof reconcileStateQueueCorrectionObserver
  >[0]["assertRollbackPermitted"];
}) =>
  await reconcileStateQueueCorrectionObserver({
    deploymentIdentityDigest: deployment,
    stateQueuePolicyId: policy,
    requiredFinalityDepth: 30n,
    source,
    store,
    reinclude,
    restoreAfterRollback: restore,
    persistTerminal,
    revokeTerminal,
    assertRollbackPermitted,
  });
