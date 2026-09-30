import { SqlClient } from "@effect/sql";
import { Effect, Option } from "effect";

import {
  type HistoryIncarnation,
  historyIncarnationDigest,
  type HistoryProvenanceChange,
} from "../l1-event-history-provenance.js";
import {
  decodeBoundEventHistoryLedgerSnapshot,
  type EventHistorySourceBinding,
} from "../l1-event-history-source.js";
import { currentOwnedTransaction } from "./eventHistoryAuthority.js";
import { verifyCursor } from "./eventHistoryJournal.load-locked.js";
import {
  lockCursor,
  type Prepared,
  type Retention,
} from "./eventHistoryJournal.prepare-append.js";
import {
  type Appended,
  appendRecovering,
  byOutRef,
  historicalApplications,
  retain,
  writeApplication,
} from "./eventHistoryJournal.retain.js";
import {
  type ApplicationRow,
  bytes,
  checked,
  digest,
  fail,
  issue,
  issued,
  key,
  point,
  preparations,
  samePoint,
  serialise,
  table,
} from "./eventHistoryJournal.validate-live-coverage.js";
import {
  decodeJournalIncarnation,
  decodeJournalOutput,
  encodeJournalIncarnation,
  encodeJournalOutput,
} from "./eventHistoryJournalCodec.js";
import { requireOriginCoverage } from "./eventHistoryReplayReceipts.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

/** The expected incarnations with the block's staged changes applied, as a
 * load would read them back: canonical records in identity order. */
const appliedIncarnations = (
  expected: readonly HistoryIncarnation[],
  changes: readonly HistoryProvenanceChange[],
) => {
  const staged = changes
    .map((change) => ({
      before: change.before,
      value: decodeJournalIncarnation(encodeJournalIncarnation(change.after)),
    }))
    .sort((a, b) =>
      a.value.id < b.value.id ? -1 : a.value.id > b.value.id ? 1 : 0,
    );
  const result: HistoryIncarnation[] = [];
  let next = 0;
  const push = (value: HistoryIncarnation) => {
    const last = result[result.length - 1];
    if (last !== undefined && last.id >= value.id)
      fail("Checkpoint incarnations are not in identity order");
    result.push(value);
  };
  const take = (existing: HistoryIncarnation | undefined) => {
    const change = staged[next++]!;
    if (
      change.before === null
        ? existing !== undefined
        : existing === undefined ||
          historyIncarnationDigest(existing) !==
            historyIncarnationDigest(change.before)
    )
      fail("Staged incarnation before-image disagrees with its checkpoint");
    push(change.value);
  };
  for (const value of expected) {
    while (next < staged.length && staged[next]!.value.id < value.id)
      take(undefined);
    if (next < staged.length && staged[next]!.value.id === value.id)
      take(value);
    else push(value);
  }
  while (next < staged.length) take(undefined);
  return Object.freeze(result);
};

/** Ready head: the owner's checkpoint was fully verified when loaded and has
 * since changed only through this journal. Every writer of the cursor, live
 * outputs and incarnations is in this module, runs under the authority lock
 * and this cursor lock, and moves the revision (append, undoHead) or the head
 * and anchor with it (retain inside append); seed writes the cursor once. So a
 * cursor equal to the expected one field by field, with its exact head
 * application, proves the stored images are the expected ones. Only the
 * staged rows are read back, and the written checkpoint is built from the
 * expected one and the block's changes rather than reloaded: the cost of a
 * Ready append does not grow with the journal's incarnations. */
const appendHead = (
  binding: EventHistorySourceBinding,
  prepared: Prepared,
  retention: Retention,
) =>
  Effect.gen(function* () {
    const expected = prepared.expected;
    yield* checked(() => {
      if (!preparations.has(prepared) || !issued.has(expected))
        fail(
          "Ready append requires a preparation staged from a loaded checkpoint",
        );
    });
    const row = yield* lockCursor(binding, "source");
    const historical = yield* historicalApplications(binding, prepared);
    const current = yield* checked(() =>
      point(row.head_hash, row.head_slot, row.head_height),
    );
    if (
      samePoint(current, prepared.block.point) &&
      historical.some(
        (application) =>
          application.canonical &&
          application.application_revision === row.head_application_revision,
      )
    )
      return { applied: false as const, revision: row.revision };
    const sql = yield* SqlClient.SqlClient;
    const bindingKey = yield* checked(() => bytes(binding.digest));
    const heads =
      row.head_application_revision === null
        ? []
        : yield* sql<ApplicationRow>`SELECT * FROM event_history_block_applications WHERE binding_digest = ${bindingKey} AND block_hash = ${row.head_hash} AND application_revision = ${row.head_application_revision}::bigint AND canonical`;
    yield* checked(() => {
      const { anchor, head, addresses } = verifyCursor(
        binding,
        row,
        heads,
        "head",
      );
      if (
        expected.bindingDigest !== binding.digest ||
        expected.manifestId !== binding.manifestId ||
        row.origin_receipt !== expected.originReceipt ||
        row.origin_receipt_digest.toString("hex") !==
          expected.originReceiptDigest ||
        row.revision !== expected.revision ||
        row.head_application_revision !== expected.headApplicationRevision ||
        !samePoint(head, expected.head) ||
        row.snapshot_digest.toString("hex") !==
          expected.capture.snapshotDigest ||
        !samePoint(anchor, expected.anchor) ||
        row.anchor_snapshot_digest.toString("hex") !==
          expected.anchorSnapshotDigest ||
        serialise(addresses) !==
          serialise(expected.capture.history.ledger.addresses)
      )
        fail("History cursor revision or head changed");
    });
    // The staged before-images, read back under the same lock: stored
    // incarnations and live outputs the block changes must be exactly the
    // images it was prepared from (absent when it creates them).
    const ids = yield* checked(() =>
      prepared.changes.map((change) => bytes(change.after.id)),
    );
    const incarnations =
      ids.length === 0
        ? []
        : yield* sql<{
            incarnation_id: Buffer;
            incarnation_digest: Buffer;
          }>`SELECT incarnation_id, incarnation_digest FROM event_history_incarnations
          WHERE binding_digest = ${bindingKey} AND ${sql.in("incarnation_id", ids)} FOR UPDATE`;
    const refs = yield* checked(() =>
      prepared.undo.outputs.map((change) =>
        decodeJournalOutput((change.before ?? change.after)!),
      ),
    );
    const hashes = yield* checked(() =>
      [...new Set(refs.map((ref) => ref.txHash))].map((hash) => bytes(hash)),
    );
    const outputs =
      hashes.length === 0
        ? []
        : yield* sql<{
            tx_hash: Buffer;
            output_index: number;
            output_digest: Buffer;
          }>`SELECT tx_hash, output_index, output_digest FROM event_history_live_outputs
          WHERE binding_digest = ${bindingKey} AND ${sql.in("tx_hash", hashes)} FOR UPDATE`;
    yield* checked(() => {
      const storedIncarnations = new Map(
        incarnations.map((stored) => [
          stored.incarnation_id.toString("hex"),
          stored.incarnation_digest.toString("hex"),
        ]),
      );
      const storedOutputs = new Map(
        outputs.map((stored) => [
          key({
            txHash: stored.tx_hash.toString("hex"),
            outputIndex: stored.output_index,
          }),
          stored.output_digest.toString("hex"),
        ]),
      );
      if (
        prepared.changes.some(
          (change) =>
            storedIncarnations.get(change.after.id) !==
            (change.before === null
              ? undefined
              : historyIncarnationDigest(change.before)),
        ) ||
        prepared.undo.outputs.some(
          (change, index) =>
            storedOutputs.get(key(refs[index]!)) !==
            (change.before === null ? undefined : digest(change.before)),
        )
      )
        fail("Prepared block images changed");
    });
    const block = prepared.block.point;
    const capture = yield* decodeBoundEventHistoryLedgerSnapshot(
      Object.freeze({
        point: Object.freeze({ id: block.id, slot: block.slot }),
        addresses: Object.freeze([
          ...expected.capture.history.ledger.addresses,
        ]),
        outputs: Object.freeze(
          prepared.projection.capture.history.ledger.outputs
            .map((output) => decodeJournalOutput(encodeJournalOutput(output)))
            .sort(byOutRef),
        ),
      }),
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
    yield* checked(() => {
      if (capture.snapshotDigest !== prepared.projection.capture.snapshotDigest)
        fail("Appended capture digest disagrees");
    });
    const revision = (BigInt(row.revision) + 1n).toString();
    yield* writeApplication(
      binding,
      {
        head: expected.head,
        headApplicationRevision: expected.headApplicationRevision,
        snapshotDigest: expected.capture.snapshotDigest,
      },
      prepared,
      {
        revision,
        snapshotDigest: capture.snapshotDigest,
        undo: prepared.undo,
        changes: prepared.changes,
      },
    );
    const kept = yield* retain(
      binding,
      {
        anchor: expected.anchor,
        anchorSnapshotDigest: expected.anchorSnapshotDigest,
        head: block,
        originReceipt: expected.originReceipt,
      },
      retention,
    );
    yield* requireOriginCoverage({
      binding,
      originReceipt: expected.originReceipt,
      anchor: kept.anchor,
      anchorSnapshotDigest: kept.anchorSnapshotDigest,
    });
    const after = yield* checked(() =>
      issue({
        bindingDigest: expected.bindingDigest,
        manifestId: expected.manifestId,
        originReceipt: expected.originReceipt,
        originReceiptDigest: expected.originReceiptDigest,
        anchor: kept.anchor,
        anchorSnapshotDigest: kept.anchorSnapshotDigest,
        head: Object.freeze({
          id: block.id,
          slot: block.slot,
          height: block.height,
        }),
        headApplicationRevision: revision,
        revision,
        capture,
        incarnations: appliedIncarnations(
          expected.incarnations,
          prepared.changes,
        ),
      }),
    );
    return {
      applied: true as const,
      after,
      changes: prepared.changes,
      hold: kept.hold,
    };
  });

/** Recovery or Ready-head append. All changes and the callback share the
 * already-owned outer transaction. Duplicate current-head delivery skips the
 * callback; a new application after rollback receives a fresh monotone
 * revision. Bounded retention (see retain) runs in the same transaction,
 * before the callback observes the new checkpoint. Recovery re-verifies the
 * whole live image and reloads the result; a Ready head append verifies the
 * cursor and the staged rows only (see appendHead). */
export const append = <A, E, R>(
  binding: EventHistorySourceBinding,
  prepared: Prepared,
  materialize: (appended: Appended) => Effect.Effect<A, E, R>,
  retention: Retention,
) =>
  Effect.gen(function* () {
    const owner = yield* currentOwnedTransaction;
    const appended =
      Option.isSome(owner) && owner.value.state === "ready"
        ? yield* appendHead(binding, prepared, retention)
        : yield* appendRecovering(binding, prepared, retention);
    if (!appended.applied)
      return { applied: false as const, revision: appended.revision };
    const result = yield* materialize({
      after: appended.after,
      changes: appended.changes,
    });
    return {
      applied: true as const,
      revision: appended.after.revision,
      result,
      hold: appended.hold,
    };
  }).pipe(sqlErrorToDatabaseError(table, "Failed to append history journal"));
