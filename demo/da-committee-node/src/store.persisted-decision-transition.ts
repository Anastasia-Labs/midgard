import type { DaSignatureRecordV1 } from "./domain.js";
import type { StateQueueReplayAnchor } from "./l1/state-queue-scanner.js";
import {
  type DecisionOutboxRecord,
  type L1ObservedDecision,
  type L1SourceState,
  UNKNOWN_STATE_QUEUE_STATUS,
} from "./store.committee-store.js";
import { knownStatus } from "./store.parse-decision-outbox-record.js";

export const mergeL1SourceState = (
  current: L1SourceState | undefined,
  proposed: L1SourceState,
): L1SourceState => {
  if (current === undefined) {
    return proposed;
  }
  if (
    current.sourceMode !== proposed.sourceMode ||
    current.network !== proposed.network
  ) {
    throw new Error(
      "committee node L1 source authority changed without a reset",
    );
  }
  if (current.status === "quarantined") {
    if (proposed.status === "healthy") {
      throw new Error("quarantined committee node L1 source state is terminal");
    }
    return current;
  }
  if (
    proposed.status === "quarantined" &&
    current.observations.some(
      ({ hasPersistedDecision }) => hasPersistedDecision,
    )
  ) {
    throw new Error(
      "persisted L1 decisions must be quarantined atomically with their artifacts",
    );
  }
  if (proposed.status === "quarantined") {
    return proposed;
  }
  // The replay anchor only ever advances; a healthy write that carries none
  // (such as a decision effect) must not drop the recorded one.
  const stateQueueReplayAnchor =
    proposed.stateQueueReplayAnchor ?? current.stateQueueReplayAnchor;
  return {
    ...proposed,
    observations: mergePersistedDecisionObservations(
      current.observations,
      proposed.observations,
    ),
    ...(stateQueueReplayAnchor === undefined ? {} : { stateQueueReplayAnchor }),
  };
};

/**
 * How a persisted decision's L1 observation changed from `prior` to `next`:
 * - `same`: the same output, status and chain point; or the same output and
 *   chain point where `prior`'s status was unknown and `next` observed it
 *   (an output's datum never changes, so this only fills it in).
 * - `explained`: final authenticated replay explains the change. `next`
 *   carries the final steps of its scan; walked from the prior output they
 *   must lead, unbroken, to the next output (or take the header out of the
 *   queue, for a merged or removed header), and the last of them must be
 *   where `next` was observed. A status may only advance to `attested`, or
 *   become terminal with the last step; a move may also take a known status
 *   to unknown (a catch-up that did not see the new output's datum).
 * - `unexplained`: anything else, which is a fork of a decided observation;
 *   in particular a known status contradicted at an unchanged output.
 * An unknown status stands in for its `lastKnownStatus` in these rules, so
 * filling it in may only advance that status as a direct change could: a
 * status contradicted across an unknown one is a fork too.
 * Both the committee's tick and the store's merge judge changes by this alone.
 */
export const persistedDecisionTransition = (
  prior: L1ObservedDecision,
  next: L1ObservedDecision,
): "same" | "explained" | "unexplained" => {
  const priorKnown = knownStatus(prior);
  const nextKnown = knownStatus(next);
  const advances =
    priorKnown !== undefined &&
    nextKnown !== undefined &&
    (nextKnown === priorKnown ||
      ((priorKnown === "unattested" || priorKnown === "attesting") &&
        nextKnown === "attested"));
  if (
    next.stateQueueOutRef === prior.stateQueueOutRef &&
    (next.stateQueueStatus === prior.stateQueueStatus ||
      prior.stateQueueStatus === UNKNOWN_STATE_QUEUE_STATUS) &&
    advances &&
    next.slot === prior.slot &&
    next.blockHash === prior.blockHash
  ) {
    return "same";
  }
  const steps = next.authenticatedSteps ?? [];
  const first = steps.findIndex(
    ({ fromOutRef }) => fromOutRef === prior.stateQueueOutRef,
  );
  const last = steps.at(-1);
  const terminal = (status: L1ObservedDecision["stateQueueStatus"]) =>
    status === "merged" || status === "removed";
  const statusExplained =
    last?.toOutRef === undefined
      ? terminal(next.stateQueueStatus)
      : !terminal(next.stateQueueStatus) && advances;
  return next.finalized &&
    first >= 0 &&
    last !== undefined &&
    priorKnown !== undefined &&
    !terminal(priorKnown) &&
    priorKnown !== "conflicted" &&
    statusExplained &&
    steps
      .slice(first + 1)
      .every(
        ({ fromOutRef }, index) =>
          steps[first + index]!.toOutRef === fromOutRef,
      ) &&
    (last.toOutRef ?? last.fromOutRef) === next.stateQueueOutRef &&
    next.slot === last.slot &&
    next.blockHash === last.blockHash &&
    prior.slot !== undefined &&
    last.slot >= prior.slot
    ? "explained"
    : "unexplained";
};

const mergePersistedDecisionObservations = (
  current: readonly L1ObservedDecision[],
  proposed: readonly L1ObservedDecision[],
): readonly L1ObservedDecision[] => {
  const observations = new Map(
    proposed.map((entry) => [entry.headerHash, entry] as const),
  );
  for (const prior of current) {
    if (!prior.hasPersistedDecision) {
      continue;
    }
    const next = observations.get(prior.headerHash);
    if (next === undefined) {
      observations.set(prior.headerHash, prior);
      continue;
    }
    if (persistedDecisionTransition(prior, next) === "unexplained") {
      throw new Error(
        "persisted L1 decision changed canonical output or chain point",
      );
    }
    observations.set(prior.headerHash, {
      ...next,
      hasPersistedDecision: true,
    });
  }
  return [...observations.values()].sort((left, right) =>
    left.headerHash.localeCompare(right.headerHash),
  );
};

export const mergeQuarantinedL1SourceState = (
  current: L1SourceState | undefined,
  proposed: L1SourceState,
): L1SourceState => {
  if (proposed.status !== "quarantined") {
    throw new Error("L1 source quarantine state is required");
  }
  if (current === undefined) {
    return proposed;
  }
  if (
    current.sourceMode !== proposed.sourceMode ||
    current.network !== proposed.network
  ) {
    throw new Error(
      "committee node L1 source authority changed without a reset",
    );
  }
  if (current.status === "quarantined") {
    return current;
  }
  return {
    ...proposed,
    observations: mergePersistedDecisionObservations(
      current.observations,
      proposed.observations,
    ),
  };
};

export const assertDecisionSourceState = (
  effect: DecisionOutboxRecord,
  sourceState: L1SourceState,
): void => {
  const observation = sourceState.observations.find(
    ({ headerHash }) => headerHash === effect.headerHash,
  );
  if (
    sourceState.status !== "healthy" ||
    sourceState.sourceMode !== effect.sourceMode ||
    sourceState.network !== effect.network ||
    observation?.stateQueueOutRef !== effect.stateQueueOutRef ||
    observation.stateQueueStatus === UNKNOWN_STATE_QUEUE_STATUS ||
    observation.finalized !== true ||
    observation.hasPersistedDecision !== true ||
    observation.slot !== effect.slot ||
    observation.blockHash !== effect.blockHash
  ) {
    throw new Error("decision outbox lacks matching durable L1 observation");
  }
};

export const assertDecisionSignature = (
  effect: DecisionOutboxRecord,
  signature: DaSignatureRecordV1 | undefined,
): void => {
  if (
    (effect.effectKind === "signature_publish" &&
      (signature === undefined ||
        signature.deploymentFingerprint !== effect.deploymentFingerprint ||
        signature.headerHash !== effect.headerHash ||
        signature.signerIndex !== effect.signerIndex ||
        signature.validation.stateQueueOutRef !== effect.stateQueueOutRef ||
        signature.l1ChainPoint.slot !== effect.slot ||
        signature.l1ChainPoint.blockHash !== effect.blockHash)) ||
    (effect.effectKind === "l1_reconcile" && signature !== undefined)
  ) {
    throw new Error("decision outbox signature does not match effect identity");
  }
};

export const parseStateQueueReplayAnchor = (
  value: unknown,
): StateQueueReplayAnchor | undefined => {
  if (value === undefined) return undefined;
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    return undefined;
  }
  const anchor = value as Partial<StateQueueReplayAnchor>;
  const keys = [
    "deploymentIdentityDigest",
    "stateQueuePolicyId",
    "queue",
    "blockNo",
    "transactionIndex",
  ];
  if (
    Object.keys(anchor).length !== keys.length ||
    !Object.keys(anchor).every((key) => keys.includes(key)) ||
    typeof anchor.deploymentIdentityDigest !== "string" ||
    !/^[0-9a-f]{64}$/u.test(anchor.deploymentIdentityDigest) ||
    typeof anchor.stateQueuePolicyId !== "string" ||
    !/^[0-9a-f]{56}$/u.test(anchor.stateQueuePolicyId) ||
    typeof anchor.blockNo !== "string" ||
    !/^(?:0|[1-9][0-9]*)$/u.test(anchor.blockNo) ||
    typeof anchor.transactionIndex !== "string" ||
    !/^(?:0|[1-9][0-9]*)$/u.test(anchor.transactionIndex) ||
    !Array.isArray(anchor.queue) ||
    anchor.queue.length === 0
  ) {
    return undefined;
  }
  const queue = anchor.queue.map((value) => {
    if (typeof value !== "object" || value === null || Array.isArray(value)) {
      return null;
    }
    const node = value as { headerHash?: unknown; outRef?: unknown };
    return Object.keys(node).length === 2 &&
      (node.headerHash === null ||
        (typeof node.headerHash === "string" &&
          /^[0-9a-f]{56}$/u.test(node.headerHash))) &&
      typeof node.outRef === "string" &&
      /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u.test(node.outRef)
      ? { headerHash: node.headerHash as string | null, outRef: node.outRef }
      : null;
  });
  if (
    queue.some((node) => node === null) ||
    queue[0]?.headerHash !== null ||
    new Set(queue.map((node) => node!.headerHash)).size !== queue.length ||
    new Set(queue.map((node) => node!.outRef)).size !== queue.length
  ) {
    return undefined;
  }
  return {
    deploymentIdentityDigest: anchor.deploymentIdentityDigest,
    stateQueuePolicyId: anchor.stateQueuePolicyId,
    queue: queue as StateQueueReplayAnchor["queue"],
    blockNo: anchor.blockNo,
    transactionIndex: anchor.transactionIndex,
  };
};
