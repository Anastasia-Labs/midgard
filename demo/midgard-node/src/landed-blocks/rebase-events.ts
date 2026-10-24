/**
 * Event statuses on the rebase target (plan §7.3, N3, ruling E-N3-1):
 *
 * - an event a removed block or a disposed-of own journal held, or one
 *   projected to a header that is neither a processed landed block nor one
 *   of this node's unabandoned journals, goes back to `awaiting` (a
 *   withdrawal also loses its classification);
 * - every event a processed foreign block, or a revived own block, holds is
 *   projected to it, a withdrawal with the classification its replay (or
 *   its journal) gave; a deposit is `consumed` once its output left the
 *   working ledger.
 *
 * This node's other own blocks' events are its commit and finalization
 * paths'.
 */
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect } from "effect";

import { DatabaseError } from "../database/utils/common.js";
import type { RebaseTarget } from "./rebase-target.js";
import type { LandedBlockRow } from "./store.js";

const failure = (message: string, cause?: unknown) =>
  new DatabaseError({ table: "node_landed_blocks", message, cause });

const bytea = (values: readonly Buffer[]) =>
  values.map((value) => `\\x${value.toString("hex")}`);

const TABLES = [
  { table: "deposits_utxos", id: "event_id" },
  { table: "forced_transaction_utxos", id: "tx_order_id" },
  { table: "withdrawal_utxos", id: "event_id" },
] as const;

/**
 * Every event of the blocks `released` (removed or disposed of), or of an
 * unknown header, back to awaiting.
 */
export const resetUnheldEvents = (
  target: RebaseTarget,
  released: readonly string[],
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const held = bytea(
      target.rows
        .filter((row) => row.state === "processed")
        .map((row) => Buffer.from(row.headerHash, "hex")),
    );
    const removedHeaders = bytea(
      released.map((headerHash) => Buffer.from(headerHash, "hex")),
    );
    const unheld = (alias: string) => sql`(
      ${sql(alias)}.projected_header_hash = ANY(${pg.array(removedHeaders)}::bytea[])
      OR (${sql(alias)}.status = 'projected'
        AND ${sql(alias)}.projected_header_hash IS NOT NULL
        AND NOT ${sql(alias)}.projected_header_hash = ANY(${pg.array(held)}::bytea[])
        AND NOT EXISTS (SELECT 1 FROM pending_block_finalizations j
          WHERE j.header_hash = ${sql(alias)}.projected_header_hash
            AND j.status <> 'abandoned')))`;
    yield* sql`UPDATE deposits_utxos d SET status = 'awaiting',
      projected_header_hash = NULL WHERE ${unheld("d")}`;
    yield* sql`UPDATE forced_transaction_utxos d SET status = 'awaiting',
      projected_header_hash = NULL, updated_at = NOW() WHERE ${unheld("d")}`;
    yield* sql`UPDATE withdrawal_utxos d SET status = 'awaiting',
      reopened_from_header_hash = d.projected_header_hash,
      projected_header_hash = NULL, validity = NULL,
      validity_detail = '{}'::jsonb, settlement_event_info = NULL,
      classification_revision = d.classification_revision + 1,
      updated_at = NOW() WHERE ${unheld("d")}`;
  });

const requireAll = (table: (typeof TABLES)[number], ids: readonly Buffer[]) =>
  Effect.gen(function* () {
    if (ids.length === 0) return;
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const found = yield* sql<{ id: Buffer }>`
      SELECT ${sql(table.id)} AS id FROM ${sql(table.table)}
      WHERE ${sql(table.id)} = ANY(${pg.array(bytea(ids))}::bytea[])`;
    if (found.length !== new Set(ids.map((id) => id.toString("hex"))).size)
      return yield* Effect.fail(
        failure(`A processed landed block names an event ${table.table} lacks`),
      );
  });

/** Projects every event a processed foreign or revived own block holds to it. */
export const assignChainEvents = (foreign: readonly LandedBlockRow[]) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    for (const row of foreign) {
      const header = Buffer.from(row.headerHash, "hex");
      const withdrawalIds = row.withdrawals.map((member) =>
        Buffer.from(member.id, "hex"),
      );
      yield* requireAll(TABLES[0], row.depositIds);
      yield* requireAll(TABLES[1], row.forcedIds);
      yield* requireAll(TABLES[2], withdrawalIds);
      if (row.depositIds.length > 0)
        yield* sql`UPDATE deposits_utxos SET status = 'projected',
          projected_header_hash = ${header}
          WHERE event_id = ANY(${pg.array(bytea(row.depositIds))}::bytea[])
            AND projected_header_hash IS DISTINCT FROM ${header}`;
      if (row.forcedIds.length > 0)
        yield* sql`UPDATE forced_transaction_utxos SET status = 'projected',
          projected_header_hash = ${header}, updated_at = NOW()
          WHERE tx_order_id = ANY(${pg.array(bytea(row.forcedIds))}::bytea[])
            AND (status <> 'projected'
              OR projected_header_hash IS DISTINCT FROM ${header})`;
      for (const member of row.withdrawals)
        yield* sql`UPDATE withdrawal_utxos d SET status = 'projected',
          projected_header_hash = ${header}, validity = ${member.validity},
          validity_detail = CAST(${JSON.stringify(member.detail ?? {})} AS TEXT)::jsonb,
          settlement_event_info = ${Buffer.from(member.settlement, "hex")},
          classification_revision = d.classification_revision + 1,
          updated_at = NOW()
          WHERE d.event_id = ${Buffer.from(member.id, "hex")}
            AND (d.status <> 'projected'
              OR d.projected_header_hash IS DISTINCT FROM ${header}
              OR d.validity IS DISTINCT FROM ${member.validity}
              OR d.settlement_event_info IS DISTINCT FROM ${Buffer.from(member.settlement, "hex")})`;
    }
  });

/**
 * A processed foreign block's deposit is `consumed` once its output left
 * the working ledger `ledger`, `projected` while it is there.
 */
export const settleChainDeposits = (
  foreign: readonly LandedBlockRow[],
  ledger: ReadonlyMap<string, Buffer>,
  outputs: ReadonlyMap<string, { readonly eventId: Buffer }>,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const ids = new Set(
      foreign.flatMap((row) => row.depositIds.map((id) => id.toString("hex"))),
    );
    const present: Buffer[] = [];
    const spent: Buffer[] = [];
    for (const [outRef, { eventId }] of outputs)
      if (ids.has(eventId.toString("hex")))
        (ledger.has(outRef) ? present : spent).push(eventId);
    if (present.length > 0)
      yield* sql`UPDATE deposits_utxos SET status = 'projected'
        WHERE event_id = ANY(${pg.array(bytea(present))}::bytea[])`;
    if (spent.length > 0)
      yield* sql`UPDATE deposits_utxos SET status = 'consumed'
        WHERE event_id = ANY(${pg.array(bytea(spent))}::bytea[])`;
  });
