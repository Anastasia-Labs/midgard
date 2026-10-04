import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import * as ForeignCensus from "./eventHistoryForeignCensus.js";

import { reverseHistoryProvenance } from "../l1-event-history-provenance.js";
import {
  decodeBoundEventHistoryLedgerSnapshot,
  type EventHistorySourceBinding,
} from "../l1-event-history-source.js";
import {
  lockCheckpoint,
  putIncarnation,
  putOutput,
  requireExpected,
} from "./eventHistoryJournal.prepare-append.js";
import {
  type ApplicationRow,
  bytes,
  checked,
  type Checkpoint,
  decodeUndo,
  fail,
  key,
  point,
  table,
  validateLiveCoverage,
} from "./eventHistoryJournal.validate-live-coverage.js";
import {
  decodeJournalIncarnation,
  decodeJournalOutput,
  encodeJournalOutput,
} from "./eventHistoryJournalCodec.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

/** Reverse exactly one current head while producers remain fenced. Retains
 * orphan admissions, never restores D/W L2 classifications from L1 images.
 * The callback must repair affected L2 descendants in this same transaction. */
export const undoHead = <A, E, R>(
  binding: EventHistorySourceBinding,
  expected: Checkpoint,
  repair: Effect.Effect<A, E, R>,
) =>
  Effect.gen(function* () {
    const actual = yield* lockCheckpoint(binding, "recovery");
    yield* checked(() => {
      requireExpected(actual, expected);
      if (actual.headApplicationRevision === null)
        fail("Rollback exceeds retained replay anchor");
    });
    const sql = yield* SqlClient.SqlClient;
    const rows =
      yield* sql<ApplicationRow>`SELECT * FROM event_history_block_applications WHERE binding_digest = ${bytes(binding.digest)} AND block_hash = ${bytes(actual.head.id)} AND application_revision = ${actual.headApplicationRevision}::bigint AND canonical`;
    const restored = yield* checked(() => {
      const application = rows[0];
      if (application === undefined)
        return fail("Missing canonical head application");
      const undo = decodeUndo(application, binding.digest);
      const current = new Map(
        actual.capture.history.ledger.outputs.map((value) => [
          key(value),
          value,
        ]),
      );
      const seen = new Set<string>();
      for (const change of undo.outputs) {
        const before =
          change.before === null ? null : decodeJournalOutput(change.before);
        const after =
          change.after === null ? null : decodeJournalOutput(change.after);
        const ref = before ?? after;
        if (
          ref === null ||
          (before !== null && after !== null && key(before) !== key(after)) ||
          seen.has(key(ref))
        )
          fail("Malformed output undo image");
        const label = key(ref!);
        seen.add(label);
        const output = current.get(label);
        if (
          (output === undefined ? null : encodeJournalOutput(output)) !==
          change.after
        )
          fail("Rollback output poststate disagrees");
        if (before === null) current.delete(label);
        else current.set(label, before);
      }
      const changes = undo.incarnations.map((change) => ({
        before:
          change.before === null
            ? null
            : decodeJournalIncarnation(change.before),
        after: decodeJournalIncarnation(change.after),
      }));
      const replacements = reverseHistoryProvenance(
        changes,
        actual.incarnations,
      );
      return {
        application,
        undo,
        replacements,
        outputs: Object.freeze([...current.values()]),
      };
    });
    let parent = actual.anchor;
    if (restored.application.parent_application_revision !== null) {
      const parents =
        yield* sql<ApplicationRow>`SELECT * FROM event_history_block_applications WHERE binding_digest = ${bytes(binding.digest)} AND block_hash = ${restored.application.parent_hash} AND application_revision = ${restored.application.parent_application_revision}::bigint AND canonical`;
      parent = yield* checked(() => {
        const row = parents[0];
        if (row === undefined)
          return fail("Missing canonical parent application");
        const at = point(row.block_hash, row.block_slot, row.block_height);
        // The one link this step exposes: the new head must be the exact
        // image the undone head was applied to.
        if (
          at.height !== actual.head.height - 1 ||
          at.slot >= actual.head.slot ||
          row.after_snapshot_digest.toString("hex") !==
            restored.application.before_snapshot_digest.toString("hex")
        )
          fail("Canonical application ancestry disagrees");
        return at;
      });
    }
    const capture = yield* decodeBoundEventHistoryLedgerSnapshot(
      Object.freeze({
        point: Object.freeze({ id: parent.id, slot: parent.slot }),
        addresses: actual.capture.history.ledger.addresses,
        outputs: restored.outputs,
      }),
      binding,
    ).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseError({
            table,
            message: "Invalid restored history capture",
            cause,
          }),
      ),
    );
    yield* checked(() => {
      if (
        capture.snapshotDigest !==
        restored.application.before_snapshot_digest.toString("hex")
      )
        fail("Restored snapshot digest disagrees");
      const incarnations = new Map(
        actual.incarnations.map((value) => [value.id, value]),
      );
      for (const value of restored.replacements)
        incarnations.set(value.id, value);
      validateLiveCoverage(capture, [...incarnations.values()], parent);
    });
    for (const change of restored.undo.outputs) {
      if (change.before !== null)
        yield* putOutput(binding.digest, decodeJournalOutput(change.before));
      else if (change.after !== null) {
        const output = decodeJournalOutput(change.after);
        yield* sql`DELETE FROM event_history_live_outputs WHERE binding_digest = ${bytes(binding.digest)} AND tx_hash = ${bytes(output.txHash)} AND output_index = ${output.outputIndex}`;
      }
    }
    for (const value of restored.replacements) yield* putIncarnation(value);
    yield* sql`UPDATE event_history_block_applications SET canonical = false WHERE binding_digest = ${bytes(binding.digest)} AND block_hash = ${bytes(actual.head.id)} AND application_revision = ${actual.headApplicationRevision}::bigint`;
    const revision = (BigInt(actual.revision) + 1n).toString();
    yield* sql`UPDATE event_history_cursor SET head_hash = ${bytes(parent.id)}, head_slot = ${parent.slot}, head_height = ${parent.height}, head_application_revision = ${restored.application.parent_application_revision}::bigint, snapshot_digest = ${bytes(capture.snapshotDigest)}, revision = ${revision}::bigint WHERE binding_digest = ${bytes(binding.digest)}`;
    if (yield* ForeignCensus.exists(binding))
      yield* ForeignCensus.undo(binding, actual.head, parent);
    const result = yield* repair;
    return { revision, result };
  }).pipe(
    sqlErrorToDatabaseError(table, "Failed to undo history journal head"),
  );
