import { Effect } from "effect";

import {
  DepositsDB,
  ForcedTransactionsDB,
  ForeignTipReconciliationsDB,
  WithdrawalsDB,
} from "../database/index.js";
import { computeChallengeableCutoff } from "../database/retention-policy.js";
import { DatabaseError } from "../database/utils/common.js";
import { ContractDeploymentIdentity, Database } from "../services/index.js";
import {
  emptyIds,
  eventCommitmentsAreConsistent,
  type T2ForeignEventResolution,
} from "./t2-foreign-event-reconciliation.resolve-t2-foreign-event-evidence.js";

export type ForeignWindow = {
  readonly foreignHeaderHash: string;
  readonly blockStartTime: Date;
  readonly blockEndTime: Date;
};

export type AwaitingForeignDa = Extract<
  T2ForeignEventResolution,
  { readonly type: "AwaitingForeignDa" }
>;

export type Unresolved = {
  readonly window: ForeignWindow;
  readonly resolution: AwaitingForeignDa;
  /** Refuses every commit, whatever its window holds. */
  readonly unconditional: boolean;
};

/**
 * Verdicts no window can lift: the foreign block is malformed or its payload
 * failed verification (`invalid`), or verified DA proves it already carries
 * one of this node's events. Each refuses every commit, as before the window
 * gate existed, and a row holding one is never pruned by time.
 */
const UNCONDITIONAL_REASONS: readonly AwaitingForeignDa["reason"][] = [
  "invalid",
  "foreign_event_present_requires_finalization",
];

/** Whether a fresh verdict's reason, or a stored `reason:detail`, is one no
 * window can lift. */
export const refusesUnconditionally = (reason: string | null): boolean =>
  reason !== null &&
  UNCONDITIONAL_REASONS.some(
    (kind) => reason === kind || reason.startsWith(`${kind}:`),
  );

/** The same, for a row that could not be replayed this pass: its stored
 * verdict, or a header its own commitment columns show is malformed. */
export const storedVerdictRefusesUnconditionally = (
  verdict: ForeignTipReconciliationsDB.StoredVerdict,
): boolean =>
  refusesUnconditionally(verdict.blockingReason) ||
  !eventCommitmentsAreConsistent(verdict.commitments);

export const entryVerdict = (
  entry: ForeignTipReconciliationsDB.Entry,
): ForeignTipReconciliationsDB.StoredVerdict => {
  const { Columns } = ForeignTipReconciliationsDB;
  return {
    blockingReason: entry[Columns.BLOCKING_REASON],
    commitments: {
      depositsRoot: entry[Columns.DEPOSITS_ROOT],
      depositCount: entry[Columns.DEPOSIT_COUNT],
      forcedTransactionsRoot: entry[Columns.FORCED_TRANSACTIONS_ROOT],
      forcedTransactionCount: entry[Columns.FORCED_TRANSACTION_COUNT],
      withdrawalsRoot: entry[Columns.WITHDRAWALS_ROOT],
      withdrawalCount: entry[Columns.WITHDRAWAL_COUNT],
    },
  };
};

export const activeEvidenceScope: Effect.Effect<
  ForeignTipReconciliationsDB.EvidenceScope,
  never,
  ContractDeploymentIdentity
> = Effect.map(ContractDeploymentIdentity, (identity) => ({
  manifestId: identity.deploymentMarker?.manifestId,
  consensusProfileId: identity.consensusProfile.profileId,
}));

export const entryWindow = (
  entry: ForeignTipReconciliationsDB.Entry,
): ForeignWindow => ({
  foreignHeaderHash:
    entry[ForeignTipReconciliationsDB.Columns.FOREIGN_HEADER_HASH].toString(
      "hex",
    ),
  blockStartTime: entry[ForeignTipReconciliationsDB.Columns.BLOCK_START_TIME],
  blockEndTime: entry[ForeignTipReconciliationsDB.Columns.BLOCK_END_TIME],
});

export const awaiting = (
  window: ForeignWindow,
  reason: AwaitingForeignDa["reason"],
  detail: string,
): AwaitingForeignDa => ({
  type: "AwaitingForeignDa",
  foreignHeaderHash: window.foreignHeaderHash,
  reason,
  detail,
  present: emptyIds(),
});

/**
 * Inclusion times of every event the next block could carry up to `through`.
 * It reads the same pending-header sets the block build reads, so an event
 * counted here is exactly one the build would carry.
 */
const pendingInclusionTimesThrough = (
  through: Date,
): Effect.Effect<readonly number[], DatabaseError, Database> =>
  Effect.map(
    Effect.all(
      [
        DepositsDB.retrievePendingHeaderEntriesUpTo(through),
        ForcedTransactionsDB.retrievePendingHeaderEntriesUpTo(through),
        WithdrawalsDB.retrievePendingHeaderEntriesUpTo(through),
      ],
      { concurrency: 1 },
    ),
    ([deposits, forcedTransactions, withdrawals]) => [
      ...deposits.map((event) =>
        event[DepositsDB.Columns.INCLUSION_TIME].getTime(),
      ),
      ...forcedTransactions.map((event) =>
        event[ForcedTransactionsDB.Columns.INCLUSION_TIME].getTime(),
      ),
      ...withdrawals.map((event) =>
        event[WithdrawalsDB.Columns.INCLUSION_TIME].getTime(),
      ),
    ],
  );

/**
 * For each window, why an unverified foreign block over it still blocks the
 * commit, or undefined when it cannot matter to it. Its events can only
 * collide with events inside its own window (start, end]. Once every event
 * table is ingested past `end` and no event the build would carry lies in
 * that window, nothing the build includes can already be in the foreign
 * block, and the commit proceeds over (end, now]. An event indexed into that
 * window later makes it block again. One read of the pending sets, up to the
 * latest ingested window end, serves every window.
 */
const windowBlockingCauses = (
  windows: readonly ForeignWindow[],
  eventsIngestedThrough: Date,
): Effect.Effect<readonly (string | undefined)[], DatabaseError, Database> =>
  Effect.gen(function* () {
    const ingestedMs = eventsIngestedThrough.getTime();
    const ingestedEnds = windows
      .map((window) => window.blockEndTime.getTime())
      .filter((endMs) => endMs <= ingestedMs);
    const pending =
      ingestedEnds.length === 0
        ? []
        : yield* pendingInclusionTimesThrough(
            new Date(Math.max(...ingestedEnds)),
          );
    return windows.map((window) => {
      const startMs = window.blockStartTime.getTime();
      const endMs = window.blockEndTime.getTime();
      if (endMs > ingestedMs) return "window_not_yet_ingested";
      return pending.some((timeMs) => timeMs > startMs && timeMs <= endMs)
        ? "pending_event_in_window"
        : undefined;
    });
  });

export const firstBlocking = (
  unresolved: readonly Unresolved[],
  eventsIngestedThrough: Date,
): Effect.Effect<AwaitingForeignDa | undefined, DatabaseError, Database> =>
  Effect.gen(function* () {
    const unconditional = unresolved.find((row) => row.unconditional);
    if (unconditional !== undefined) return unconditional.resolution;
    const causes = yield* windowBlockingCauses(
      unresolved.map((row) => row.window),
      eventsIngestedThrough,
    );
    const index = causes.findIndex((cause) => cause !== undefined);
    if (index === -1) return undefined;
    const { resolution } = unresolved[index]!;
    return {
      ...resolution,
      detail: `${resolution.detail};gate=${causes[index]!}`,
    };
  });

export const undecodableUnresolved = (
  rows: readonly ForeignTipReconciliationsDB.UndecodableEvidence[],
): Effect.Effect<readonly Unresolved[]> =>
  Effect.forEach(rows, (row) => {
    const window: ForeignWindow = {
      foreignHeaderHash: row.foreignHeaderHash.toString("hex"),
      blockStartTime: row.blockStartTime,
      blockEndTime: row.blockEndTime,
    };
    return Effect.logWarning(
      `🔹 Skipping undecodable foreign-tip reconciliation header_hash=${window.foreignHeaderHash}; its window still gates the commit: ${row.cause.message}`,
      row.cause,
    ).pipe(
      Effect.as({
        window,
        resolution: awaiting(window, "replay_failed", row.cause.message),
        unconditional: storedVerdictRefusesUnconditionally(row.verdict),
      }),
    );
  });

/**
 * Deletes rows that can no longer affect any block: their foreign block ended
 * before the challengeability horizon, measured from both now and the
 * ingestion barrier, and they either belong to another deployment or hold a
 * verdict the window can lift with no event the build would carry remaining
 * inside their window. Run after the replay pass, so every replayable row of
 * the active deployment carries this pass's verdict.
 */
export const pruneSettledForeignTipReconciliations = ({
  now,
  eventsIngestedThrough,
}: {
  readonly now: Date;
  readonly eventsIngestedThrough: Date;
}): Effect.Effect<
  number,
  DatabaseError,
  Database | ContractDeploymentIdentity
> =>
  Effect.gen(function* () {
    const scope = yield* activeEvidenceScope;
    // Measured from the earlier of now and the ingestion barrier, so a row
    // leaves only once its window has also been ingested for the whole
    // challengeability horizon.
    const cutoff = computeChallengeableCutoff(
      new Date(Math.min(now.getTime(), eventsIngestedThrough.getTime())),
    );
    // Unbatched: a batch of kept rows could starve the settled ones behind
    // it, and only rows whose window still holds an event, or whose verdict
    // no window lifts, survive a pass.
    const candidates =
      yield* ForeignTipReconciliationsDB.retrievePruneCandidates(cutoff);
    const inScope = (candidate: ForeignTipReconciliationsDB.PruneCandidate) =>
      candidate.consensusProfileId === scope.consensusProfileId &&
      (scope.manifestId === undefined ||
        candidate.manifestId === scope.manifestId);
    const windowed = candidates.filter(
      (candidate) =>
        inScope(candidate) &&
        !storedVerdictRefusesUnconditionally(candidate.verdict),
    );
    const causes = yield* windowBlockingCauses(
      windowed.map((candidate) => ({
        foreignHeaderHash: candidate.foreignHeaderHash.toString("hex"),
        blockStartTime: candidate.blockStartTime,
        blockEndTime: candidate.blockEndTime,
      })),
      eventsIngestedThrough,
    );
    const settled = [
      ...candidates.filter((candidate) => !inScope(candidate)),
      ...windowed.filter((_, index) => causes[index] === undefined),
    ].map((candidate) => candidate.foreignHeaderHash);
    const deleted = yield* ForeignTipReconciliationsDB.deleteSettled({
      foreignHeaderHashes: settled,
      cutoff,
    });
    if (deleted > 0) {
      yield* Effect.logInfo(
        `🔹 Pruned ${deleted.toString()} settled foreign-tip reconciliation(s) past the challengeability horizon (cutoff=${cutoff.toISOString()}).`,
      );
    }
    return deleted;
  });
