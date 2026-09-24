import { SqlClient } from "@effect/sql";
import type { Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { historyIncarnationEntry } from "../l1-event-history-entries.js";
import { historyIncarnationDigest } from "../l1-event-history-provenance.js";
import type { HistoryOwnerChange } from "../services/event-history-owner.js";
import * as Deposits from "./deposits.js";
import { requireRecoveryTransaction } from "./eventHistoryAuthority.js";
import { repairUnpublishedHistoryLedger } from "./eventHistoryLedgerRepair.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";
import * as Withdrawals from "./withdrawals.js";

const table = "event_history_incarnations";
type Association = {
  event_id: Buffer;
  history_binding_digest: Buffer | null;
  history_incarnation_id: Buffer | null;
};
const fail = (message: string, cause?: unknown) =>
  Effect.fail(new DatabaseError({ table, message, cause }));

/** Bounded SQL materialization inside the source owner's recovery transaction.
 * A new live admission starts unclassified. Continuations and retirement retain
 * the exact existing row's L2 state. Unassociated rows and orphan-dependent L2
 * state require explicit repair; neither is silently adopted or discarded.
 * This callback performs no network/native/cache work and never opens Ready.
 */
export const materializeCanonicalHistory = (
  change: HistoryOwnerChange,
  network: Network,
) =>
  Effect.gen(function* () {
    const token = yield* requireRecoveryTransaction;
    const checkpoint = change.after;
    if (token.deploymentIdentity !== checkpoint.manifestId)
      return yield* fail("History materialization deployment changed");
    const sql = yield* SqlClient.SqlClient;
    const cursor = yield* sql<{
      revision: string;
      head_hash: Buffer;
      snapshot_digest: Buffer;
    }>`
    SELECT revision::text, head_hash, snapshot_digest FROM event_history_cursor
    WHERE binding_digest = ${Buffer.from(checkpoint.bindingDigest, "hex")} FOR UPDATE`;
    if (
      cursor.length !== 1 ||
      cursor[0]!.revision !== checkpoint.revision ||
      cursor[0]!.head_hash.toString("hex") !== checkpoint.head.id ||
      cursor[0]!.snapshot_digest.toString("hex") !==
        checkpoint.capture.snapshotDigest
    )
      return yield* fail("History materialization checkpoint changed");
    yield* repairUnpublishedHistoryLedger(change);
    for (const incarnation of checkpoint.incarnations) {
      const binding = Buffer.from(incarnation.bindingDigest, "hex");
      const id = Buffer.from(incarnation.id, "hex");
      const eventId = Buffer.from(incarnation.event.idCbor, "hex");
      const stored = yield* sql<{
        incarnation_digest: Buffer;
        origin_canonical: boolean;
        kind: string;
        event_id: Buffer;
      }>`
      SELECT incarnation_digest, origin_canonical, kind, event_id FROM event_history_incarnations
      WHERE binding_digest = ${binding} AND incarnation_id = ${id} FOR UPDATE`;
      if (
        incarnation.bindingDigest !== checkpoint.bindingDigest ||
        stored.length !== 1 ||
        stored[0]!.incarnation_digest.toString("hex") !==
          historyIncarnationDigest(incarnation) ||
        stored[0]!.origin_canonical !== (incarnation.placement !== null) ||
        stored[0]!.kind !== incarnation.kind ||
        !stored[0]!.event_id.equals(eventId)
      )
        return yield* fail(
          "History materialization incarnation changed",
          incarnation.id,
        );
      const eventTable =
        incarnation.kind === "deposit"
          ? Deposits.tableName
          : Withdrawals.tableName;
      const rows =
        yield* sql<Association>`SELECT event_id, history_binding_digest, history_incarnation_id
      FROM ${sql(eventTable)} WHERE event_id = ${eventId} FOR UPDATE`;
      const previous = rows[0];
      const sameAssociation =
        previous?.history_binding_digest?.equals(binding) === true &&
        previous.history_incarnation_id?.equals(id) === true;
      if (incarnation.placement === null) {
        if (sameAssociation)
          return yield* fail(
            "Orphaned history admission requires dependent L2 repair before readiness",
            incarnation.id,
          );
        continue;
      }
      if (previous !== undefined && !sameAssociation)
        return yield* fail(
          "Refusing to adopt a local event row by public ID without its exact history incarnation",
          incarnation.id,
        );
      if (previous === undefined && incarnation.placement.current === null)
        return yield* fail(
          "Retired history admission requires retained L2 association evidence",
          incarnation.id,
        );
      const materialized = yield* historyIncarnationEntry(
        incarnation,
        network,
      ).pipe(
        Effect.mapError(
          (cause) =>
            new DatabaseError({
              table,
              message: "Failed to materialize authenticated history bytes",
              cause,
            }),
        ),
      );
      if (previous === undefined) {
        const payload =
          materialized.kind === "deposit"
            ? materialized.entry
            : {
                ...materialized.entry,
                [Withdrawals.Columns.VALIDITY_DETAIL]:
                  sql`CAST(${JSON.stringify(materialized.entry[Withdrawals.Columns.VALIDITY_DETAIL])} AS TEXT)::JSONB`,
              };
        const entry = {
          ...payload,
          history_binding_digest: binding,
          history_incarnation_id: id,
        };
        // Render values explicitly so the withdrawal JSONB cast survives SQL
        // parameter construction, as in the ordinary withdrawal writer.
        const inserted = yield* sql<Association>`INSERT INTO ${sql(eventTable)}
          (${sql.csv(Object.keys(entry).map((column) => sql`${sql(column)}`))})
          VALUES (${sql.csv(Object.values(entry).map((value) => sql`${value}`))})
          ON CONFLICT (event_id) DO NOTHING RETURNING event_id, history_binding_digest, history_incarnation_id`;
        if (inserted.length !== 1)
          return yield* fail(
            "History admission row changed before materialization",
            incarnation.id,
          );
      } else if (materialized.kind === "deposit") {
        yield* Deposits.insertEntries([materialized.entry]);
      } else {
        yield* Withdrawals.insertEntries([materialized.entry]);
      }
    }
    // Every existing event row must remain associated with its own canonical
    // admission, including retired origins. This also rejects incomplete startup
    // materialization when local rows predate the authenticated replay anchor.
    for (const [kind, eventTable] of [
      ["deposit", Deposits.tableName],
      ["withdrawal", Withdrawals.tableName],
    ] as const) {
      const invalid = yield* sql<{
        event_id: Buffer;
      }>`SELECT e.event_id FROM ${sql(eventTable)} e
      LEFT JOIN event_history_incarnations i
        ON i.binding_digest = e.history_binding_digest AND i.incarnation_id = e.history_incarnation_id
      WHERE i.binding_digest IS DISTINCT FROM ${Buffer.from(checkpoint.bindingDigest, "hex")}
        OR i.origin_canonical IS DISTINCT FROM true OR i.kind IS DISTINCT FROM ${kind}
        OR i.event_id IS DISTINCT FROM e.event_id LIMIT 1`;
      if (invalid.length !== 0)
        return yield* fail(
          "Local event eligibility is not backed by this canonical history",
          invalid[0]!.event_id.toString("hex"),
        );
    }
  }).pipe(
    sqlErrorToDatabaseError(table, "Failed canonical history materialization"),
  );
