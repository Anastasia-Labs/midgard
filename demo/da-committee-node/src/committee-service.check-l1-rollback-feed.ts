import {
  type CommitteePayloadFetchObservation,
  type CommitteeTickResult,
  ingestDaConflictEvidence,
} from "./committee-service.ingest-da-conflict-evidence.js";
import type { DaGossipMessageHandler } from "./da/libp2p/DaGossip.js";
import type { DaPeerRegistry } from "./da/libp2p/DaPeerRegistry.js";
import type { DaPayloadFetchFailure } from "./da/source.js";
import type {
  ChainSyncCursor,
  ChainSyncReplayProvider,
} from "./l1/provider.js";
import { L1SourceIntegrityError } from "./l1/source-integrity.js";
import { type StateQueueProvider } from "./l1/state-queue-scanner.js";
import {
  type CommitteeStore,
  type L1ObservedDecision,
  type L1SourceState,
  persistedDecisionTransition,
} from "./store.js";

export const createDaConflictEvidenceGossipHandler = (args: {
  readonly deploymentFingerprint: string;
  readonly registry: DaPeerRegistry;
  readonly store: Pick<CommitteeStore, "saveDaConflictEvidence">;
  readonly now?: () => Date;
}): DaGossipMessageHandler => {
  const now = args.now ?? (() => new Date());
  return async (context) => {
    await ingestDaConflictEvidence({
      ...args,
      context,
      receivedAt: now(),
    });
  };
};

/**
 * Checks every persisted decision against this tick's observation of its
 * header. `deferred` holds headers an authenticated but not-yet-final
 * checkpoint moved or took out of the queue: they are neither forked nor
 * disappeared yet, and are checked again once that checkpoint is final. Any
 * other change must be explained by final authenticated replay.
 */
export const l1ObservationTransitionFailure = (
  previous: L1SourceState | undefined,
  current: ReadonlyMap<string, L1ObservedDecision>,
  deferred: ReadonlySet<string>,
): string | undefined => {
  if (previous === undefined) {
    return undefined;
  }
  for (const prior of previous.observations) {
    if (!prior.hasPersistedDecision || deferred.has(prior.headerHash)) {
      continue;
    }
    const observed = current.get(prior.headerHash);
    if (observed === undefined) {
      return `l1_source_decision_disappeared:${prior.headerHash}`;
    }
    const transition = persistedDecisionTransition(prior, observed);
    if (transition === "unexplained") {
      return `l1_source_decision_forked:${prior.headerHash}`;
    }
    if (transition === "same" && !observed.finalized) {
      return `l1_source_decision_lost_finality:${prior.headerHash}`;
    }
  }
  return undefined;
};

export type L1RollbackFeedCheck = {
  /** Where the next tick's rollback replay starts: the snapshot's cursor. */
  readonly cursor?: ChainSyncCursor;
  readonly failure?: string;
};

type DurableChainSyncReplayProvider = StateQueueProvider &
  ChainSyncReplayProvider;

/**
 * Replays the rollback feed since the durable consumer cursor against the
 * persisted decisions. This tick decides on a snapshot read at
 * `snapshotCursor`, and a rollback after it may undo what that snapshot
 * showed, so the tick acknowledges `snapshotCursor`, not the authority's
 * current cursor: the next tick replays every later event against the
 * decisions this tick persists.
 */
export const checkL1RollbackFeed = async (
  previous: L1SourceState | undefined,
  provider: StateQueueProvider,
  snapshotCursor: ChainSyncCursor | undefined,
  retirementStore?: Pick<
    CommitteeStore,
    "getRetirementFloor" | "recordRetirementBreach"
  >,
): Promise<L1RollbackFeedCheck> => {
  const floor = await retirementStore?.getRetirementFloor();
  if (floor?.breach)
    throw new L1SourceIntegrityError("committee retirement floor was breached");
  const replayProvider = durableChainSyncReplayProvider(provider);
  if (replayProvider === undefined) {
    return {};
  }
  if (snapshotCursor === undefined) {
    throw new Error(
      "local-node state-queue snapshot carries no chain-sync cursor to acknowledge",
    );
  }
  const current = await replayProvider.currentChainSyncCursor();
  const consumed = await replayProvider.loadConsumedChainSyncCursor();
  const decisions =
    previous?.observations.filter(
      ({ hasPersistedDecision }) => hasPersistedDecision,
    ) ?? [];
  if (
    (previous === undefined || decisions.length === 0) &&
    floor?.point === undefined
  ) {
    return { cursor: snapshotCursor };
  }
  if (consumed === undefined) {
    throw new L1SourceIntegrityError(
      "persisted L1 decisions lack a durable chain-sync consumer cursor",
    );
  }
  if (
    consumed.sequence > current.sequence ||
    consumed.rollbackGeneration > current.rollbackGeneration ||
    (consumed.sequence === current.sequence &&
      !sameChainSyncCursor(consumed, current))
  ) {
    throw new L1SourceIntegrityError(
      "durable chain-sync consumer cursor is ahead of or conflicts with the authority cursor",
    );
  }
  const events = await replayProvider.replayChainSyncEvents(consumed.sequence);
  if (events.length !== current.sequence - consumed.sequence) {
    throw new L1SourceIntegrityError(
      "chain-sync rollback replay is not contiguous with its durable consumer cursor",
    );
  }
  const replayedRollbacks = events.filter(
    ({ direction }) => direction === "roll_backward",
  );
  if (
    consumed.rollbackGeneration + replayedRollbacks.length !==
    current.rollbackGeneration
  ) {
    throw new L1SourceIntegrityError(
      "chain-sync rollback replay does not match the durable rollback generation",
    );
  }
  // A rollback undoes a decision only if it reaches below the chain point the
  // decision recorded. A decision with no recorded point (one persisted from a
  // peer's signature on an observation that carried none) cannot be placed
  // against a rollback point; the tick's observation check judges it instead,
  // and fails closed if its output is gone or forked.
  const placed = decisions.filter(
    (
      decision,
    ): decision is L1ObservedDecision & {
      readonly slot: number;
      readonly blockHash: string;
    } => decision.slot !== undefined && decision.blockHash !== undefined,
  );
  for (const rollback of replayedRollbacks) {
    if (
      floor?.point &&
      (rollback.point.slot < floor.point.slot ||
        (rollback.point.slot === floor.point.slot &&
          rollback.point.blockHash !== floor.point.blockHash))
    ) {
      const failure = `l1_source_retirement_floor_crossed:${rollback.point.slot}:${rollback.point.blockHash}`;
      await retirementStore!.recordRetirementBreach(failure, {
        slot: rollback.point.slot,
        blockHash: rollback.point.blockHash,
      });
      return { cursor: snapshotCursor, failure };
    }
    for (const decision of placed) {
      if (
        rollback.point.slot < decision.slot ||
        (rollback.point.slot === decision.slot &&
          rollback.point.blockHash !== decision.blockHash)
      ) {
        return {
          cursor: snapshotCursor,
          failure: `l1_source_chain_sync_rollback:${decision.headerHash}:${rollback.point.slot.toString()}:${rollback.point.blockHash}`,
        };
      }
    }
  }
  return { cursor: snapshotCursor };
};

/**
 * Acknowledges the cursor the tick's decisions were made at. The authority
 * may have synchronized past it during the tick, rollbacks included; the next
 * tick replays those events from this cursor.
 */
export const acknowledgeL1RollbackFeed = async (
  provider: StateQueueProvider,
  cursor: ChainSyncCursor,
  writeEvent: (event: Readonly<Record<string, unknown>>) => void,
): Promise<void> => {
  const replayProvider = durableChainSyncReplayProvider(provider);
  if (replayProvider === undefined) {
    throw new Error(
      "chain-sync rollback replay capabilities disappeared before acknowledgement",
    );
  }
  const acknowledgement =
    await replayProvider.acknowledgeChainSyncCursor(cursor);
  if (acknowledgement.rollbackSinceCapture) {
    writeEvent({
      event: "l1_chain_sync_rollback_after_acknowledged_cursor",
      sequence: cursor.sequence,
      rollbackGeneration: cursor.rollbackGeneration,
    });
  }
};

const durableChainSyncReplayProvider = (
  provider: StateQueueProvider,
): DurableChainSyncReplayProvider | undefined => {
  const candidate = provider as Partial<ChainSyncReplayProvider>;
  const capabilities = [
    candidate.currentChainSyncCursor,
    candidate.replayChainSyncEvents,
    candidate.loadConsumedChainSyncCursor,
    candidate.acknowledgeChainSyncCursor,
  ];
  if (capabilities.every((capability) => capability === undefined)) {
    return undefined;
  }
  if (capabilities.some((capability) => typeof capability !== "function")) {
    throw new Error(
      "local-node provider exposes incomplete durable rollback replay capabilities",
    );
  }
  return provider as DurableChainSyncReplayProvider;
};

const sameChainSyncCursor = (
  left: ChainSyncCursor,
  right: ChainSyncCursor,
): boolean =>
  left.sequence === right.sequence &&
  left.rollbackGeneration === right.rollbackGeneration &&
  left.point.network === right.point.network &&
  left.point.slot === right.point.slot &&
  left.point.blockHash === right.point.blockHash &&
  left.point.providerSource === right.point.providerSource &&
  left.point.observedAt === right.point.observedAt;

export const quarantinedTickResult = (
  state: L1SourceState,
): CommitteeTickResult => ({
  scannedHeaders: 0,
  signedHeaders: 0,
  reconciledHeaders: 0,
  skippedHeaders: 0,
  payloadFetches: [],
  errors: [
    `L1 source quarantined: ${state.quarantineReason ?? "unknown reason"}`,
  ],
});

export const payloadFetchObservation = (
  headerHash: string,
  attempts: DaPayloadFetchFailure["attempts"],
): CommitteePayloadFetchObservation => {
  const status = attempts.every((attempt) => attempt.status === "not_found")
    ? "missing_da"
    : "fetch_failed";
  const detail = attempts
    .map((attempt) => `${attempt.sourcePeerId}:${attempt.status}`)
    .join(",");
  return {
    headerHash,
    status,
    sourcePeerIds: attempts.map((attempt) => attempt.sourcePeerId),
    ...(detail.length === 0 ? {} : { detail }),
  };
};
