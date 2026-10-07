import { Data } from "effect";

import * as Journal from "../database/eventHistoryJournal.js";
import type { HistoryProvenanceChange } from "../l1-event-history-provenance.js";
import { type LedgerSnapshotPoint } from "../l1-ledger-snapshot.js";

export class HistoryOwnerUnavailable extends Data.TaggedError(
  "HistoryOwnerUnavailable",
)<{ readonly cause: unknown }> {}

/** A forward block whose dependent reconciliation is not a pure extension of
 * the producers' prefix. Its Ready append rolls back and recovery takes it. */
export class HistoryAppendNeedsRecovery extends Data.TaggedError(
  "HistoryAppendNeedsRecovery",
)<{ readonly reason: string }> {}

/** Readiness opens when the follower first converges to the source tip and
 * stays open while it keeps up: forward blocks and tip advances are journaled
 * at the head without closing it. Further than this many blocks behind the
 * tip, new producers are refused and readiness names the lag until the
 * follower catches up; producers already running keep their journaled prefix.
 * Only a rollback, a rewinding intersection, source failure or close supersede
 * them. */
export const HISTORY_READY_MAXIMUM_LAG_BLOCKS = 5;

/** Retry delay of a pending reconciliation that stayed pending with the same
 * reason: doubles from the initial value up to the cap. */
export const PENDING_RECONCILIATION_BACKOFF_INITIAL_MS = 500;

export const PENDING_RECONCILIATION_BACKOFF_MAX_MS = 30_000;

/** A reconciliation pending with the same reason for this long is reported
 * as a warning, and again at most once per this interval while it stays. */
export const PENDING_RECONCILIATION_BLOCKED_WARN_INTERVAL_MS = 60_000;

export type HistoryOwnerFrontier = Readonly<{
  ready: boolean;
  headHeight: number | null;
  tipHeight: number | null;
  lagBlocks: number;
  maximumLagBlocks: number;
}>;

/** A forward block carries the provenance changes it staged, so dependent
 * materialization walks only those; every other change is reconciled whole. */
export type HistoryOwnerChange =
  | Readonly<{
      kind: "seed" | "rollback" | "resume";
      before: Journal.Checkpoint | null;
      after: Journal.Checkpoint;
    }>
  | Readonly<{
      kind: "forward";
      before: Journal.Checkpoint;
      after: Journal.Checkpoint;
      changes: readonly HistoryProvenanceChange[];
    }>;

/** The source can continue collecting canonical evidence while producers stay
 * fenced. Pending reconciliation must perform no dependent ledger mutations.
 */
export type HistoryReconciliationPending = Readonly<{
  status: "pending";
  reason: string;
}>;

/** Settlement or foreign-adoption evidence is keeping the journal anchor more
 * than the rollback horizon behind where the horizon alone would put it. */
export type HistoryRetentionHold = Readonly<{
  holdSlot: number;
  anchorHeight: number;
  unheldAnchorHeight: number;
  heldBlocks: number;
  rollbackHorizon: number;
}>;

export type HistoryOwnerCoverage = Readonly<{
  bindingDigest: string;
  checkpointRevision: string;
  point: LedgerSnapshotPoint;
  snapshotDigest: string;
  includedThroughMs: number;
  /** The complete canonical prefix strictly before this retained rollback
   * anchor can no longer be rewound automatically. Absent on model permits
   * that do not establish retention authority. */
  retention?: Readonly<{
    anchor: LedgerSnapshotPoint;
    includedThroughMs: number;
  }>;
}>;

export const samePoint = (a: LedgerSnapshotPoint, b: LedgerSnapshotPoint) =>
  a.id === b.id && a.slot === b.slot;

export class MissingBody extends Error {
  constructor(readonly txHash: string) {
    super(`Missing history creating body ${txHash}`);
  }
}
