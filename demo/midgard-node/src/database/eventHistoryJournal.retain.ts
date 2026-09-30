import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { type HistoryProvenanceChange } from "../l1-event-history-provenance.js";
import {
  decodeBoundEventHistoryLedgerSnapshot,
  type EventHistorySourceBinding,
} from "../l1-event-history-source.js";
import type { LedgerSnapshotOutput } from "../l1-ledger-snapshot.js";
import {
  lockCheckpoint,
  prepareAppend,
  type Prepared,
  putIncarnation,
  putOutput,
  requireExpected,
  type Retention,
  RETENTION_BATCH,
  type RetentionHold,
} from "./eventHistoryJournal.prepare-append.js";
import {
  type ApplicationRow,
  bytes,
  checked,
  type Checkpoint,
  decodeUndo,
  digest,
  fail,
  natural,
  type Point,
  point,
  samePoint,
  serialise,
  table,
  type Undo,
  validateLiveCoverage,
} from "./eventHistoryJournal.validate-live-coverage.js";
import { decodeJournalOutput } from "./eventHistoryJournalCodec.js";
import {
  originReplayHead,
  prune as pruneReplayReceipts,
} from "./eventHistoryReplayReceipts.js";
import { DatabaseError } from "./utils/common.js";

/** Advance the anchor to the deepest journaled block that is past the rollback
 * horizon (height <= tipHeight - horizon), strictly behind head (so the retained
 * canonical range is never empty) and never so far that the first retained
 * block starts after holdSlot, by at most RETENTION_BATCH blocks. The anchor
 * moves with its snapshot digest in this transaction, and the applications at
 * or behind it (plus orphan branches rooted there) are deleted. Once the anchor
 * has left the seed point, replay receipts are deleted in bounded batches too.
 * The origin receipt is never changed. */
export const retain = (
  binding: EventHistorySourceBinding,
  current: Readonly<{
    anchor: Point;
    anchorSnapshotDigest: string;
    head: Point;
    originReceipt: string;
  }>,
  retention: Retention,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const key = bytes(binding.digest);
    const limit = yield* checked(() => {
      natural(retention.tipHeight);
      natural(retention.horizon);
      if (retention.holdSlot !== undefined) natural(retention.holdSlot);
      return Math.min(
        retention.tipHeight - retention.horizon,
        current.head.height - 1,
        current.anchor.height + RETENTION_BATCH,
      );
    });
    let target = limit;
    let holding = false;
    if (retention.holdSlot !== undefined && target > current.anchor.height) {
      // The deepest block whose successor still starts at or before holdSlot.
      const [held] = yield* sql<{
        height: string | null;
      }>`SELECT max(block_height)::text AS height FROM event_history_block_applications
        WHERE binding_digest = ${key} AND canonical AND block_slot <= ${retention.holdSlot}`;
      const cap =
        held?.height === null || held?.height === undefined
          ? current.anchor.height
          : natural(held.height) - 1;
      if (cap < target) {
        target = Math.max(cap, current.anchor.height);
        holding = true;
      }
    }
    let anchor = current.anchor;
    let anchorSnapshotDigest = current.anchorSnapshotDigest;
    if (target > current.anchor.height) {
      const rows =
        yield* sql<ApplicationRow>`SELECT * FROM event_history_block_applications
        WHERE binding_digest = ${key} AND canonical AND block_height IN (${target}, ${target + 1})
        ORDER BY block_height`;
      const [next, child] = yield* checked(() => {
        const next = rows[0];
        if (next === undefined || natural(next.block_height) !== target)
          fail("Retention target is not a canonical journal application");
        const child = rows[1];
        if (child === undefined)
          fail("Retention target has no canonical successor");
        return [next, child] as const;
      });
      // The first retained application now descends from the anchor itself.
      yield* sql`UPDATE event_history_block_applications SET parent_application_revision = NULL
          WHERE binding_digest = ${key} AND block_hash = ${child.block_hash}
            AND application_revision = ${child.application_revision}::bigint`;
      // One statement per batch: every deleted row's descendants are deleted
      // with it, so no RESTRICT parent link is left dangling.
      const deleted = yield* sql<{
        canonical: boolean;
      }>`WITH RECURSIVE doomed AS (
          SELECT block_hash, application_revision FROM event_history_block_applications
            WHERE binding_digest = ${key} AND block_height <= ${target}
          UNION
          SELECT c.block_hash, c.application_revision FROM event_history_block_applications c
            JOIN doomed d ON c.parent_hash = d.block_hash AND c.parent_application_revision = d.application_revision
            WHERE c.binding_digest = ${key}
        )
        DELETE FROM event_history_block_applications a USING doomed d
        WHERE a.binding_digest = ${key} AND a.block_hash = d.block_hash
          AND a.application_revision = d.application_revision
        RETURNING a.canonical`;
      yield* checked(() => {
        if (
          deleted.filter((row) => row.canonical).length !==
          target - current.anchor.height
        )
          fail("Retention deleted a different canonical range");
      });
      anchor = point(next.block_hash, next.block_slot, next.block_height);
      anchorSnapshotDigest = next.after_snapshot_digest.toString("hex");
      yield* sql`UPDATE event_history_cursor SET anchor_hash = ${next.block_hash},
        anchor_slot = ${anchor.slot}, anchor_height = ${anchor.height},
        anchor_snapshot_digest = ${next.after_snapshot_digest}
        WHERE binding_digest = ${key}`;
    }
    const seed = yield* checked(() => originReplayHead(current.originReceipt));
    if (seed === undefined || !samePoint(seed, anchor))
      yield* pruneReplayReceipts(binding.digest, RETENTION_BATCH);
    const hold: RetentionHold | undefined = holding
      ? Object.freeze({
          holdSlot: retention.holdSlot!,
          anchorHeight: anchor.height,
          unheldAnchorHeight: Math.min(
            retention.tipHeight - retention.horizon,
            current.head.height - 1,
          ),
        })
      : undefined;
    return { anchor, anchorSnapshotDigest, hold };
  });

/** What an applied append hands its callback, in the same transaction: the
 * checkpoint it wrote (after retention) and the block's staged provenance
 * changes, so dependent materialization need walk only those. */
export type Appended = Readonly<{
  after: Checkpoint;
  changes: readonly HistoryProvenanceChange[];
}>;

export const historicalApplications = (
  binding: EventHistorySourceBinding,
  prepared: Prepared,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const historical =
      yield* sql<ApplicationRow>`SELECT * FROM event_history_block_applications WHERE binding_digest = ${bytes(binding.digest)} AND block_hash = ${bytes(prepared.block.point.id)} ORDER BY application_revision`;
    yield* checked(() => {
      for (const application of historical) {
        decodeUndo(application, binding.digest);
        if (application.ledger_receipt !== prepared.receipt)
          fail("Same block has a conflicting immutable ledger receipt");
      }
    });
    return historical;
  });

/** One journaled application: its row, the live-output and incarnation
 * changes it records, and the cursor moved onto it with a new revision. */
export const writeApplication = (
  binding: EventHistorySourceBinding,
  parent: Readonly<{
    head: Point;
    headApplicationRevision: string | null;
    snapshotDigest: string;
  }>,
  prepared: Prepared,
  write: Readonly<{
    revision: string;
    snapshotDigest: string;
    undo: Undo;
    changes: readonly HistoryProvenanceChange[];
  }>,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const undoRecord = JSON.stringify(write.undo);
    yield* sql`INSERT INTO event_history_block_applications (binding_digest, block_hash, application_revision, parent_hash, parent_application_revision, block_slot, block_height, before_snapshot_digest, after_snapshot_digest, ledger_receipt, ledger_receipt_digest, undo_record, undo_digest, canonical)
    VALUES (${bytes(binding.digest)}, ${bytes(prepared.block.point.id)}, ${write.revision}::bigint, ${bytes(parent.head.id)}, ${parent.headApplicationRevision}::bigint, ${prepared.block.point.slot}, ${prepared.block.point.height}, ${bytes(parent.snapshotDigest)}, ${bytes(write.snapshotDigest)}, ${prepared.receipt}, ${bytes(digest(prepared.receipt))}, ${undoRecord}, ${bytes(digest(undoRecord))}, true)`;
    for (const change of write.undo.outputs) {
      if (change.after !== null)
        yield* putOutput(binding.digest, decodeJournalOutput(change.after));
      else if (change.before !== null) {
        const previous = decodeJournalOutput(change.before);
        yield* sql`DELETE FROM event_history_live_outputs WHERE binding_digest = ${bytes(binding.digest)} AND tx_hash = ${bytes(previous.txHash)} AND output_index = ${previous.outputIndex}`;
      }
    }
    for (const change of write.changes) yield* putIncarnation(change.after);
    yield* sql`UPDATE event_history_cursor SET head_hash = ${bytes(prepared.block.point.id)}, head_slot = ${prepared.block.point.slot}, head_height = ${prepared.block.point.height}, head_application_revision = ${write.revision}::bigint, snapshot_digest = ${bytes(write.snapshotDigest)}, revision = ${write.revision}::bigint WHERE binding_digest = ${bytes(binding.digest)}`;
  });

/** Recovery: re-load the locked checkpoint, re-stage the block against it and
 * re-load the written checkpoint for the callback. */
export const appendRecovering = (
  binding: EventHistorySourceBinding,
  prepared: Prepared,
  retention: Retention,
) =>
  Effect.gen(function* () {
    const actual = yield* lockCheckpoint(binding, "source");
    const historical = yield* historicalApplications(binding, prepared);
    if (
      samePoint(actual.head, prepared.block.point) &&
      historical.some(
        (application) =>
          application.canonical &&
          application.application_revision === actual.headApplicationRevision,
      )
    )
      return { applied: false as const, revision: actual.revision };
    const capture = yield* decodeBoundEventHistoryLedgerSnapshot(
      prepared.projection.capture.history.ledger,
      binding,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseError({
            table,
            message: "Invalid appended capture",
            cause,
          }),
      ),
    );
    const staged = yield* checked(() => {
      requireExpected(actual, prepared.expected);
      const fresh = prepareAppend(actual, prepared.block, {
        capture,
        transitions: prepared.projection.transitions,
      });
      if (
        fresh.receipt !== prepared.receipt ||
        serialise(fresh.undo) !== serialise(prepared.undo)
      )
        fail("Prepared block images changed");
      return {
        revision: (BigInt(actual.revision) + 1n).toString(),
        fresh,
      };
    });
    yield* checked(() => {
      if (capture.snapshotDigest !== prepared.projection.capture.snapshotDigest)
        fail("Appended capture digest disagrees");
      const after = new Map(
        actual.incarnations.map((value) => [value.id, value]),
      );
      for (const change of staged.fresh.changes)
        after.set(change.after.id, change.after);
      validateLiveCoverage(capture, [...after.values()], prepared.block.point);
    });
    yield* writeApplication(
      binding,
      {
        head: actual.head,
        headApplicationRevision: actual.headApplicationRevision,
        snapshotDigest: actual.capture.snapshotDigest,
      },
      prepared,
      {
        revision: staged.revision,
        snapshotDigest: capture.snapshotDigest,
        undo: staged.fresh.undo,
        changes: staged.fresh.changes,
      },
    );
    const { hold } = yield* retain(
      binding,
      {
        anchor: actual.anchor,
        anchorSnapshotDigest: actual.anchorSnapshotDigest,
        head: prepared.block.point,
        originReceipt: actual.originReceipt,
      },
      retention,
    );
    const after = yield* lockCheckpoint(binding, "source");
    return {
      applied: true as const,
      after,
      changes: staged.fresh.changes,
      hold,
    };
  });

// Stored order: loadLocked reads outputs by (tx_hash, output_index) and
// incarnations by incarnation_id. Lowercase fixed-width hex compares as bytes.
export const byOutRef = (a: LedgerSnapshotOutput, b: LedgerSnapshotOutput) =>
  a.txHash < b.txHash
    ? -1
    : a.txHash > b.txHash
      ? 1
      : a.outputIndex - b.outputIndex;
