import { SqlClient } from "@effect/sql";
import { Effect, Schema } from "effect";

import { historyIncarnationDigest } from "../l1-event-history-provenance.js";
import {
  decodeBoundEventHistoryLedgerSnapshot,
  type EventHistorySourceBinding,
} from "../l1-event-history-source.js";
import {
  type ApplicationRow,
  bytes,
  checked,
  type Checkpoint,
  type CursorRow,
  decodeUndo,
  digest,
  fail,
  issue,
  point,
  samePoint,
  table,
  validateLiveCoverage,
  type Verification,
} from "./eventHistoryJournal.validate-live-coverage.js";
import {
  decodeJournalIncarnation,
  decodeJournalOutput,
} from "./eventHistoryJournalCodec.js";
import { requireOriginCoverage } from "./eventHistoryReplayReceipts.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

/** The cursor's own fields, its canonical application chain ("chain") or
 * exact head application ("head"), and its captured address scope. */
export const verifyCursor = (
  binding: EventHistorySourceBinding,
  row: CursorRow,
  applications: readonly ApplicationRow[],
  verification: Verification,
) => {
  if (
    row.binding_digest.toString("hex") !== binding.digest ||
    row.manifest_id.toString("hex") !== binding.manifestId
  )
    fail("Stored cursor belongs to another source or manifest");
  if (
    row.origin_receipt.length === 0 ||
    digest(row.origin_receipt) !== row.origin_receipt_digest.toString("hex")
  )
    fail("Stored origin receipt digest disagrees");
  const anchor = point(row.anchor_hash, row.anchor_slot, row.anchor_height);
  const head = point(row.head_hash, row.head_slot, row.head_height);
  let previous = anchor;
  let previousRevision: string | null = null;
  let previousDigest = row.anchor_snapshot_digest.toString("hex");
  if (verification === "head" && applications.length === 1) {
    const application = applications[0]!;
    decodeUndo(application, binding.digest);
    const next = point(
      application.block_hash,
      application.block_slot,
      application.block_height,
    );
    const parentRevision = application.parent_application_revision;
    if (
      (parentRevision === null
        ? application.parent_hash.toString("hex") !== anchor.id ||
          application.before_snapshot_digest.toString("hex") !==
            previousDigest ||
          next.height !== anchor.height + 1 ||
          next.slot <= anchor.slot
        : next.height < anchor.height + 2 ||
          BigInt(application.application_revision) <= BigInt(parentRevision)) ||
      BigInt(application.application_revision) > BigInt(row.revision)
    )
      fail("Canonical application ancestry disagrees");
    previous = next;
    previousRevision = application.application_revision;
    previousDigest = application.after_snapshot_digest.toString("hex");
  }
  if (verification === "chain")
    for (const application of applications) {
      decodeUndo(application, binding.digest);
      const next = point(
        application.block_hash,
        application.block_slot,
        application.block_height,
      );
      if (
        application.parent_hash.toString("hex") !== previous.id ||
        application.parent_application_revision !== previousRevision ||
        application.before_snapshot_digest.toString("hex") !== previousDigest ||
        next.height !== previous.height + 1 ||
        next.slot <= previous.slot ||
        BigInt(application.application_revision) <=
          BigInt(previousRevision ?? "0") ||
        BigInt(application.application_revision) > BigInt(row.revision)
      )
        fail("Canonical application ancestry disagrees");
      previous = next;
      previousRevision = application.application_revision;
      previousDigest = application.after_snapshot_digest.toString("hex");
    }
  if (
    !samePoint(previous, head) ||
    previousRevision !== row.head_application_revision ||
    previousDigest !== row.snapshot_digest.toString("hex")
  )
    fail("Cursor does not identify its exact canonical head application");
  const addresses = Schema.decodeUnknownSync(
    Schema.Array(Schema.NonEmptyString),
  )(row.addresses);
  if (new Set(addresses).size !== addresses.length)
    fail("Repeated captured address");
  return { anchor, head, addresses };
};

export const loadLocked = (
  binding: EventHistorySourceBinding,
  row: CursorRow,
  verification: Verification,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const bindingBytes = yield* checked(() => bytes(binding.digest));
    const outputs = yield* sql<{
      tx_hash: Buffer;
      output_index: number;
      output_record: string;
      output_digest: Buffer;
    }>`SELECT * FROM event_history_live_outputs WHERE binding_digest = ${bindingBytes} ORDER BY tx_hash, output_index`;
    const origins = yield* sql<{
      incarnation_id: Buffer;
      kind: string;
      event_id: Buffer;
      event_key: Buffer;
      origin_canonical: boolean;
      incarnation_record: string;
      incarnation_digest: Buffer;
    }>`SELECT * FROM event_history_incarnations WHERE binding_digest = ${bindingBytes} ORDER BY incarnation_id`;
    const applications =
      verification === "chain"
        ? yield* sql<ApplicationRow>`SELECT * FROM event_history_block_applications WHERE binding_digest = ${bindingBytes} AND canonical ORDER BY block_height`
        : row.head_application_revision === null
          ? []
          : yield* sql<ApplicationRow>`SELECT * FROM event_history_block_applications WHERE binding_digest = ${bindingBytes} AND block_hash = ${row.head_hash} AND application_revision = ${row.head_application_revision}::bigint AND canonical`;
    const decoded = yield* checked(() => {
      const { anchor, head, addresses } = verifyCursor(
        binding,
        row,
        applications,
        verification,
      );
      const ledgerOutputs = outputs.map((stored) => {
        const output = decodeJournalOutput(stored.output_record);
        if (
          digest(stored.output_record) !==
            stored.output_digest.toString("hex") ||
          output.txHash !== stored.tx_hash.toString("hex") ||
          output.outputIndex !== stored.output_index ||
          !addresses.includes(output.address)
        )
          fail("Stored output record, index or digest disagrees");
        return output;
      });
      const incarnations = Object.freeze(
        origins.map((stored) => {
          const value = decodeJournalIncarnation(stored.incarnation_record);
          if (
            historyIncarnationDigest(value) !==
              stored.incarnation_digest.toString("hex") ||
            value.id !== stored.incarnation_id.toString("hex") ||
            value.kind !== stored.kind ||
            value.event.idCbor !== stored.event_id.toString("hex") ||
            value.event.key !== stored.event_key.toString("hex") ||
            (value.placement !== null) !== stored.origin_canonical
          )
            fail("Stored incarnation record, index or digest disagrees");
          return value;
        }),
      );
      return {
        anchor,
        head,
        incarnations,
        ledger: Object.freeze({
          point: Object.freeze({ id: head.id, slot: head.slot }),
          addresses: Object.freeze([...addresses]),
          outputs: Object.freeze(ledgerOutputs),
        }),
      };
    });
    yield* requireOriginCoverage({
      binding,
      originReceipt: row.origin_receipt,
      anchor: decoded.anchor,
      anchorSnapshotDigest: row.anchor_snapshot_digest.toString("hex"),
    });
    const capture = yield* decodeBoundEventHistoryLedgerSnapshot(
      decoded.ledger,
      binding,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseError({
            table,
            message: "Stored history capture is invalid",
            cause,
          }),
      ),
    );
    return yield* checked(() => {
      if (capture.snapshotDigest !== row.snapshot_digest.toString("hex"))
        fail("Stored snapshot digest disagrees");
      validateLiveCoverage(capture, decoded.incarnations, decoded.head);
      return issue({
        bindingDigest: binding.digest,
        manifestId: binding.manifestId,
        originReceipt: row.origin_receipt,
        originReceiptDigest: row.origin_receipt_digest.toString("hex"),
        anchor: decoded.anchor,
        anchorSnapshotDigest: row.anchor_snapshot_digest.toString("hex"),
        head: decoded.head,
        headApplicationRevision: row.head_application_revision,
        revision: row.revision,
        capture,
        incarnations: decoded.incarnations,
      });
    });
  });

const read = (binding: EventHistorySourceBinding, verification: Verification) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const bindingBytes = yield* checked(() => bytes(binding.digest));
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const rows =
          yield* sql<CursorRow>`SELECT * FROM event_history_cursor WHERE binding_digest = ${bindingBytes} FOR SHARE`;
        return rows[0] === undefined
          ? null
          : yield* loadLocked(binding, rows[0], verification);
      }),
    );
  }).pipe(sqlErrorToDatabaseError(table, "Failed to load history journal"));

/** Coherent local recovery material only, with the whole retained chain
 * re-verified from the anchor. The source owner must re-admit the origin
 * receipt, branch and coverage before publishing readiness. All journal
 * writers lock this same cursor after the authority lock. */
export const load = (binding: EventHistorySourceBinding) =>
  read(binding, "chain");

/** The current checkpoint for a caller that already holds a fully verified
 * one and has since advanced it only through append/undoHead: verifies the
 * cursor, its head application and the live image, not the retained chain. */
export const loadCurrent = (binding: EventHistorySourceBinding) =>
  read(binding, "head");

/** Restart intersection candidates, newest first: the head, then retained
 * canonical blocks exponentially further back (head-1, head-2, head-4, ...),
 * then the anchor. A short fork while offline rewinds about as far as the
 * fork is deep, never the whole retained range. */
export const intersections = (
  binding: EventHistorySourceBinding,
  checkpoint: Checkpoint,
) =>
  Effect.gen(function* () {
    const heights: number[] = [];
    for (
      let depth = 1;
      checkpoint.head.height - depth > checkpoint.anchor.height;
      depth *= 2
    )
      heights.push(checkpoint.head.height - depth);
    const sql = yield* SqlClient.SqlClient;
    const rows =
      heights.length === 0
        ? []
        : yield* sql<
            Pick<ApplicationRow, "block_hash" | "block_slot" | "block_height">
          >`SELECT block_hash, block_slot, block_height FROM event_history_block_applications
          WHERE binding_digest = ${bytes(binding.digest)} AND canonical AND block_height IN ${sql.in(heights)}
          ORDER BY block_height DESC`;
    return yield* checked(() => {
      const points = rows.map((row) =>
        point(row.block_hash, row.block_slot, row.block_height),
      );
      if (
        points.length !== heights.length ||
        points.some((at, index) => at.height !== heights[index])
      )
        fail("Retained canonical intersections have a gap");
      return Object.freeze(
        samePoint(checkpoint.head, checkpoint.anchor)
          ? [checkpoint.head]
          : [checkpoint.head, ...points, checkpoint.anchor],
      );
    });
  }).pipe(
    sqlErrorToDatabaseError(table, "Failed to read retained intersections"),
  );

/** Whether a rollback target is this checkpoint's anchor or one of its
 * retained canonical blocks: the only points undoHead can rewind to. */
export const retains = (
  binding: EventHistorySourceBinding,
  checkpoint: Checkpoint,
  target: Readonly<{ id: string; slot: number }>,
) =>
  Effect.gen(function* () {
    if (
      target.id === checkpoint.anchor.id &&
      target.slot === checkpoint.anchor.slot
    )
      return true;
    if (
      target.slot <= checkpoint.anchor.slot ||
      target.slot > checkpoint.head.slot
    )
      return false;
    const sql = yield* SqlClient.SqlClient;
    const rows =
      yield* sql`SELECT 1 FROM event_history_block_applications WHERE binding_digest = ${bytes(binding.digest)}
        AND canonical AND block_hash = ${bytes(target.id)} AND block_slot = ${target.slot}`;
    return rows.length === 1;
  }).pipe(sqlErrorToDatabaseError(table, "Failed to read retained ancestry"));
