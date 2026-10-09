/**
 * The build-time depth check (plan §8.1): a commit may include only events
 * the follower admitted at least d blocks (`HISTORY_COMMIT_HORIZON_LAG_BLOCKS`)
 * below its cursor. It runs inside the journal-preparation transaction, so a
 * refused commit journals nothing and is never signed; the next tick builds
 * again from the events then selectable. The end-time cap (U3) already keeps
 * shallow events out of the window; this check refuses one that reaches the
 * build anyway.
 *
 * At d = 0 it refuses nothing. At d > 0 it also refuses a commit that
 * includes events whose depth it cannot measure: when the follower's event
 * table is missing, when the follower has no cursor, and when an included
 * deposit or withdrawal has no admission row in that table. Each refusal
 * names its reason.
 */
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect } from "effect";

import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

export const COMMIT_EVENT_NOT_DEEP_MESSAGE =
  "Refusing to journal a commit that includes an event admitted fewer than d blocks below the follower's view";

export const COMMIT_EVENT_DEPTH_NO_EVENT_TABLE_MESSAGE =
  "Refusing to journal a commit that includes events while the follower's event table is missing";

export const COMMIT_EVENT_DEPTH_NO_CURSOR_MESSAGE =
  "Refusing to journal a commit that includes events while the follower has no cursor to measure their depth from";

export const COMMIT_EVENT_DEPTH_UNADMITTED_MESSAGE =
  "Refusing to journal a commit that includes a deposit or withdrawal with no follower admission row";

const refuse = (message: string, cause: string) =>
  Effect.fail(
    new DatabaseError({
      table: "pending_block_finalizations",
      message,
      cause,
    }),
  );

const bytea = (values: readonly Buffer[]) =>
  values.map((value) => `\\x${value.toString("hex")}`);

export type CommitEventDepthInput = Readonly<{
  lagBlocks: number;
  depositIds: readonly Buffer[];
  forcedIds: readonly Buffer[];
  withdrawalIds: readonly Buffer[];
}>;

/**
 * Fails `COMMIT_EVENT_NOT_DEEP_MESSAGE` when an included event is shallower
 * than d, and one of the `COMMIT_EVENT_DEPTH_*` messages when an included
 * event's depth cannot be measured.
 */
export const assertIncludedEventsDeep = (input: CommitEventDepthInput) =>
  Effect.gen(function* () {
    if (input.lagBlocks === 0) return;
    if (
      input.depositIds.length === 0 &&
      input.forcedIds.length === 0 &&
      input.withdrawalIds.length === 0
    )
      return;
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const [tables] = yield* sql<{ events: boolean; orders: boolean }>`SELECT
      to_regclass('node_l1_events') IS NOT NULL AS events,
      to_regclass('node_l1_forced_order_fields') IS NOT NULL AS orders`;
    const included = `deposits=${input.depositIds.length.toString()},forced=${input.forcedIds.length.toString()},withdrawals=${input.withdrawalIds.length.toString()},d=${input.lagBlocks.toString()}`;
    if (tables === undefined || !tables.events)
      return yield* refuse(COMMIT_EVENT_DEPTH_NO_EVENT_TABLE_MESSAGE, included);
    const [cursor] = yield* sql<{ height: string }>`
      SELECT height::text AS height FROM l1_follower_cursor`;
    if (cursor === undefined)
      return yield* refuse(COMMIT_EVENT_DEPTH_NO_CURSOR_MESSAGE, included);
    const highest = Number(cursor.height) - input.lagBlocks;
    // `admitted` counts the included ids that join an admission row; one
    // that joins none has no depth to measure.
    const [deposits] = yield* sql<{ admitted: string; height: string | null }>`
      SELECT count(DISTINCT d.event_id)::text AS admitted,
        max(e.admitted_height)::text AS height FROM deposits_utxos d
      JOIN node_l1_events e ON e.kind = 'deposit'
        AND e.event_key = d.l1_event_key
      WHERE d.event_id = ANY(${pg.array(bytea(input.depositIds))}::bytea[])`;
    const [withdrawals] = yield* sql<{
      admitted: string;
      height: string | null;
    }>`
      SELECT count(DISTINCT w.event_id)::text AS admitted,
        max(e.admitted_height)::text AS height FROM withdrawal_utxos w
      JOIN node_l1_events e ON e.kind = 'withdrawal'
        AND e.event_key = w.l1_event_key
      WHERE w.event_id = ANY(${pg.array(bytea(input.withdrawalIds))}::bytea[])`;
    const unadmitted = [
      ["deposit", input.depositIds, deposits] as const,
      ["withdrawal", input.withdrawalIds, withdrawals] as const,
    ].find(
      ([, ids, row]) =>
        Number(row?.admitted ?? 0) !==
        new Set(ids.map((id) => id.toString("hex"))).size,
    );
    if (unadmitted !== undefined) {
      const [kind, ids, row] = unadmitted;
      return yield* refuse(
        COMMIT_EVENT_DEPTH_UNADMITTED_MESSAGE,
        `kind=${kind},included=${ids.length.toString()},admitted=${row?.admitted ?? "0"},d=${input.lagBlocks.toString()}`,
      );
    }
    const forced = tables.orders
      ? yield* sql<{ height: string | null }>`
          SELECT max(o.height)::text AS height
          FROM forced_transaction_utxos f
          JOIN node_l1_forced_order_fields o
            ON o.order_tx_hash = f.tx_order_l1_tx_hash
           AND o.order_output_index = f.tx_order_l1_output_index
          WHERE f.tx_order_id = ANY(${pg.array(bytea(input.forcedIds))}::bytea[])`
      : [];
    const shallowest = [deposits, withdrawals, ...forced]
      .map((row) =>
        row === undefined || row.height === null ? null : Number(row.height),
      )
      .filter((height): height is number => height !== null)
      .reduce<number | null>(
        (top, height) => (top === null || height > top ? height : top),
        null,
      );
    if (shallowest !== null && shallowest > highest)
      return yield* refuse(
        COMMIT_EVENT_NOT_DEEP_MESSAGE,
        `admitted_height=${shallowest.toString()},view_height=${cursor.height},d=${input.lagBlocks.toString()}`,
      );
  }).pipe(
    sqlErrorToDatabaseError(
      "pending_block_finalizations",
      "Failed to check the depth of a commit's included events",
    ),
  );
