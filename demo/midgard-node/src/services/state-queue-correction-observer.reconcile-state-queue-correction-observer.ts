import {
  replayStateQueueAuthenticatedCheckpoints,
  type StateQueueAuthenticatedTransition,
  type StateQueueTransitionNode,
  withStateQueueAuthenticatedTransitionFinalityDepth,
} from "@al-ft/midgard-sdk";

import { type FetchLike } from "../l1-tx-order-carriage.js";
import { normalizeOgmiosHttpUrl } from "../local-ledger-slot.js";
import { sameQueue } from "./state-queue-correction-observer.create-database-state-queue-correction-observer-store.js";
import {
  HEX_28,
  HEX_32,
  makeState,
  OUT_REF,
  parseQueue,
  parseStateQueueCorrectionObserverState,
  STATE_QUEUE_CORRECTION_OBSERVER_SCHEMA_VERSION,
  type StateQueueCorrectionObserverResult,
  type StateQueueCorrectionObserverSource,
  type StateQueueCorrectionObserverStore,
} from "./state-queue-correction-observer.parse-state-queue-correction-observer-state.js";

/**
 * Reconciles the durable cursor before admitting any action. `reinclude` and
 * `restoreAfterRollback` are idempotent database transactions; persisting after
 * them is therefore crash-safe (a retry can repeat the exact mutation).
 */
export const reconcileStateQueueCorrectionObserver = async ({
  deploymentIdentityDigest,
  stateQueuePolicyId,
  requiredFinalityDepth,
  source,
  store,
  reinclude,
  restoreAfterRollback,
  persistTerminal,
  revokeTerminal,
  assertRollbackPermitted,
}: {
  readonly deploymentIdentityDigest: string;
  readonly stateQueuePolicyId: string;
  readonly requiredFinalityDepth: bigint;
  readonly source: StateQueueCorrectionObserverSource;
  readonly store: StateQueueCorrectionObserverStore;
  readonly reinclude: (
    transition: StateQueueAuthenticatedTransition,
  ) => Promise<void>;
  readonly restoreAfterRollback: (
    transition: StateQueueAuthenticatedTransition,
  ) => Promise<void>;
  readonly persistTerminal?: (
    transition: StateQueueAuthenticatedTransition,
  ) => Promise<void>;
  readonly revokeTerminal?: (
    transition: StateQueueAuthenticatedTransition,
  ) => Promise<void>;
  /** Refuses a timeout or fraud removal's rollback before any of its local
   * effects is revoked: a refusal leaves the terminal outcome, and every other
   * record of the removal, exactly as it was. */
  readonly assertRollbackPermitted?: (
    transition: StateQueueAuthenticatedTransition,
  ) => Promise<void>;
}): Promise<StateQueueCorrectionObserverResult> => {
  if (
    !HEX_32.test(deploymentIdentityDigest) ||
    !HEX_28.test(stateQueuePolicyId) ||
    requiredFinalityDepth <= 0n
  ) {
    throw new Error("Invalid state-queue correction observer authority");
  }
  const queue = await source.readQueue();
  if (parseQueue(queue) === null) {
    throw new Error(
      "State-queue correction observer refused a structurally invalid queue snapshot",
    );
  }
  const loaded = await store.load();
  if (loaded === null) {
    await store.save(
      makeState({
        schemaVersion: STATE_QUEUE_CORRECTION_OBSERVER_SCHEMA_VERSION,
        deploymentIdentityDigest,
        stateQueuePolicyId,
        cursorQueue: queue,
        pending: [],
        admitted: [],
        retractedTransactionHashes: [],
        postFinalityRollbackIncidents: [],
      }),
    );
    return {
      status: "bootstrapped",
      admittedTransactionHashes: [],
      retractedTransactionHashes: [],
      postFinalityRollbackTransactionHashes: [],
    };
  }
  const state = parseStateQueueCorrectionObserverState(loaded);
  if (
    state === null ||
    state.deploymentIdentityDigest !== deploymentIdentityDigest ||
    state.stateQueuePolicyId !== stateQueuePolicyId
  ) {
    throw new Error(
      "State-queue correction observer store is non-canonical or belongs to another deployment",
    );
  }

  let pending = [...state.pending];
  let admitted = [...state.admitted];
  const retracted = new Set(state.retractedTransactionHashes);
  const incidents = [...state.postFinalityRollbackIncidents];
  const admittedNow: string[] = [];
  const retractedNow: string[] = [];
  const incidentsNow: string[] = [];
  const persistTerminalTransition = persistTerminal ?? (async () => undefined);
  const revokeTerminalTransition = revokeTerminal ?? (async () => undefined);

  const depthByTransaction = new Map<string, bigint | null>();
  const depthOf = async (
    transition: StateQueueAuthenticatedTransition,
  ): Promise<bigint | null> => {
    if (depthByTransaction.has(transition.transactionHash)) {
      return depthByTransaction.get(transition.transactionHash)!;
    }
    const depth = await source.canonicalDepth(transition);
    depthByTransaction.set(transition.transactionHash, depth);
    return depth;
  };
  let rollbackAnchor: readonly StateQueueTransitionNode[] | null = null;
  const bindRollbackAnchor = (
    transition: StateQueueAuthenticatedTransition,
  ): void => {
    if (
      rollbackAnchor !== null &&
      !sameQueue(rollbackAnchor, transition.previousQueue)
    ) {
      throw new Error(
        "State-queue correction observer found competing rollback anchors",
      );
    }
    rollbackAnchor = transition.previousQueue;
  };

  const pendingAfterRollback: StateQueueAuthenticatedTransition[] = [];
  for (const transition of pending) {
    const depth = await depthOf(transition);
    if (depth === null) {
      if (sameQueue(queue, transition.nextQueue)) {
        throw new Error(
          "State-queue provider reports a terminal transaction absent while its exact post-state remains current",
        );
      }
      bindRollbackAnchor(transition);
      retracted.add(transition.transactionHash);
      retractedNow.push(transition.transactionHash);
    } else {
      pendingAfterRollback.push(transition);
    }
  }
  pending = pendingAfterRollback;

  const admittedAfterRollback: StateQueueAuthenticatedTransition[] = [];
  for (const transition of admitted) {
    const depth = await depthOf(transition);
    if (depth === null) {
      if (sameQueue(queue, transition.nextQueue)) {
        throw new Error(
          "State-queue provider reports a finalized terminal transaction absent while its exact post-state remains current",
        );
      }
      bindRollbackAnchor(transition);
      const removal =
        transition.transitionKind === "timeout_correction" ||
        transition.transitionKind === "fraud_removal";
      if (removal) await assertRollbackPermitted?.(transition);
      await revokeTerminalTransition(transition);
      if (removal) await restoreAfterRollback(transition);
      retracted.add(transition.transactionHash);
      retractedNow.push(transition.transactionHash);
      if (
        !incidents.some(
          ({ transactionHash }) =>
            transactionHash === transition.transactionHash,
        )
      ) {
        incidents.push({
          transactionHash: transition.transactionHash,
          transitionDigest: transition.transitionDigest,
        });
        incidentsNow.push(transition.transactionHash);
      }
    } else {
      admittedAfterRollback.push(transition);
    }
  }
  admitted = admittedAfterRollback;

  const replayAnchor = rollbackAnchor ?? state.cursorQueue;
  if (!sameQueue(replayAnchor, queue)) {
    const observed = await source.observeTransitions(replayAnchor, queue);
    const replay = replayStateQueueAuthenticatedCheckpoints({
      deploymentIdentityDigest,
      stateQueuePolicyId,
      minimumFinalityDepth: 1n,
      anchor: {
        queue: replayAnchor,
        blockNo: "0",
        transactionIndex: "0",
      },
      checkpoints: observed,
    });
    if (replay === null || !sameQueue(replay.queue, queue)) {
      throw new Error(
        "State-queue authenticated checkpoint replay does not reach the current queue",
      );
    }
    for (const candidate of replay.terminals) {
      if (
        !pending.some(
          ({ transactionHash }) =>
            transactionHash === candidate.transactionHash,
        ) &&
        !admitted.some(
          ({ transactionHash }) =>
            transactionHash === candidate.transactionHash,
        )
      ) {
        retracted.delete(candidate.transactionHash);
        pending.push(candidate);
      }
    }
  }

  const stillPending: StateQueueAuthenticatedTransition[] = [];
  for (const transition of pending) {
    const depth = await depthOf(transition);
    if (depth === null) {
      stillPending.push(transition);
      continue;
    }
    if (depth < requiredFinalityDepth) {
      stillPending.push(transition);
      continue;
    }
    const finalized = withStateQueueAuthenticatedTransitionFinalityDepth(
      transition,
      depth.toString(),
    );
    if (finalized === null) {
      throw new Error("Failed to bind authenticated correction finality depth");
    }
    if (
      finalized.transitionKind === "timeout_correction" ||
      finalized.transitionKind === "fraud_removal"
    ) {
      await reinclude(finalized);
    }
    await persistTerminalTransition(finalized);
    admitted.push(finalized);
    admittedNow.push(finalized.transactionHash);
  }
  pending = stillPending;

  await store.save(
    makeState({
      schemaVersion: STATE_QUEUE_CORRECTION_OBSERVER_SCHEMA_VERSION,
      deploymentIdentityDigest,
      stateQueuePolicyId,
      cursorQueue: queue,
      pending,
      admitted,
      retractedTransactionHashes: [...retracted].sort(),
      postFinalityRollbackIncidents: incidents,
    }),
  );
  return {
    status: "reconciled",
    admittedTransactionHashes: admittedNow,
    retractedTransactionHashes: [...new Set(retractedNow)],
    postFinalityRollbackTransactionHashes: incidentsNow,
  };
};

export const outRef = (
  label: string,
): { txHash: string; outputIndex: number } => {
  const match = OUT_REF.exec(label);
  if (match === null) throw new Error(`Invalid output reference ${label}`);
  const [txHash, index] = label.split("#") as [string, string];
  return { txHash, outputIndex: Number(index) };
};

export const TIP_READ_ATTEMPTS = 5;

export const queryOgmios = async (
  ogmiosUrl: string,
  fetchImpl: FetchLike,
  method: string,
): Promise<unknown> => {
  const response = await fetchImpl(normalizeOgmiosHttpUrl(ogmiosUrl), {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify({
      jsonrpc: "2.0",
      method,
      params: {},
      id: "midgard-state-queue-correction-tip-v1",
    }),
  });
  const body = (await response.json()) as { result?: unknown };
  if (!response.ok) throw new Error(`Ogmios ${method} query failed`);
  return body.result;
};
