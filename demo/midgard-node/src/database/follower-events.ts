/**
 * The follower-change driver's Postgres sink (plan §7.3, N1): it writes the
 * node's event rows (`deposits_utxos`, `withdrawal_utxos`) from the event
 * projection at a follower view, in the caller's node transaction.
 *
 * - The view is checked valid under `FOR SHARE` on the follower cursor, so a
 *   rewind cannot commit between the check and the write; a moved view
 *   writes nothing (`stale`).
 * - A projected event without a row is inserted with its follower admission
 *   identity (event key, admission outref). One with a row of the same
 *   identity keeps the row's L2 state; only a withdrawal's location moves.
 * - One bad event never fails the run: an event that does not decode into a
 *   node row (`l1_event_undecodable`), or whose public id has a row under
 *   another live identity or none (`l1_event_identity_conflict`), is refused
 *   by name and left out; the rest of the plan is ingested. The refusals are
 *   a function of the plan and the node rows, so every run derives them
 *   again. A row with no identity (one from before migration 0008) still
 *   fails the run below, by the eligibility check every event table gets.
 * - Rows whose admission the follower no longer holds (orphans) are never
 *   adopted; those the recovery must settle are counted
 *   (`countOrphansAwaitingRecovery`): their dependents are rejected by the
 *   owner's recovery. An own landed block's orphan is held by its own
 *   narrow hold instead and is not counted.
 * - Due deposits are projected into the mempool ledger (hidden until a
 *   header is assigned), up to the caller's cutoff.
 * - The ingestion point is recorded for the commit horizon.
 *
 * Decoding (`userEventEntry`) runs only for events the node has no row for.
 */
import type { ProjectedEvent } from "@al-ft/midgard-l1-follower/events";
import { SqlClient, type Statement } from "@effect/sql";
import type { Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  EVENT_IDENTITY_CONFLICT,
  EVENT_UNDECODABLE,
  type EventRefusal,
  type IngestionPlan,
} from "../l1-events/driver.js";
import * as Deposits from "./deposits.js";
import { identityOf, rowOf } from "./follower-events.row-of.js";
import { countOrphansAwaitingRecovery } from "./follower-orphan-repair.js";
import { followerViewValid } from "./follower-schema.js";
import {
  type AdmissionKind,
  canonicalAdmission,
} from "./l1-admission-identity.js";
import * as MempoolLedgerDB from "./mempoolLedger.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";
import * as Withdrawals from "./withdrawals.js";

const table = "follower_event_ingestion";
const fail = (eventTable: string, message: string, cause?: unknown) =>
  Effect.fail(new DatabaseError({ table: eventTable, message, cause }));

/** Event rows read, checked and written per statement. */
export const FOLLOWER_INGESTION_CHUNK = 500;

const chunks = <A>(values: readonly A[]): A[][] => {
  const result: A[][] = [];
  for (let at = 0; at < values.length; at += FOLLOWER_INGESTION_CHUNK)
    result.push(values.slice(at, at + FOLLOWER_INGESTION_CHUNK));
  return result;
};

type ExistingRow = {
  event_id: Buffer;
  l1_event_key: Buffer | null;
  l1_origin_outref: Buffer | null;
  canonical: boolean;
  withdrawal_l1_tx_hash?: Buffer;
  withdrawal_l1_output_index?: number;
};

const sameBytes = (a: Buffer | null, b: Buffer): boolean =>
  a !== null && a.equals(b);

const EVENT_TABLE: Readonly<Record<AdmissionKind, string>> = {
  deposit: Deposits.tableName,
  withdrawal: Withdrawals.tableName,
};

export type FollowerIngestion = Readonly<{
  inserted: number;
  locationsMoved: number;
  /**
   * Orphans the recovery must settle: rows whose admission the follower no
   * longer holds, own landed blocks' rows excluded.
   */
  orphans: number;
  /** Events retired at the view that the node never ingested (skipped). */
  retiredUnseen: number;
  /** Deposits newly projected into the mempool ledger (hidden). */
  projected: number;
  /** Header-assigned deposits whose mempool row was restored: the cache must reload. */
  spendableUpserts: readonly MempoolLedgerDB.DepositEntry[];
  /** Events refused by name and left out of this run. */
  refused: readonly EventRefusal[];
}>;

export type FollowerIngestionOutcome =
  | Readonly<{ kind: "stale" }>
  | Readonly<{ kind: "applied"; ingestion: FollowerIngestion }>;

/** One kind's walk over the plan, in chunks, under row locks. */
const ingestKind = (
  kind: AdmissionKind,
  events: readonly ProjectedEvent[],
  network: Network,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const eventTable = EVENT_TABLE[kind];
    let inserted = 0;
    let locationsMoved = 0;
    let retiredUnseen = 0;
    const refused: EventRefusal[] = [];
    const refuse = (
      event: ProjectedEvent,
      reason: EventRefusal["reason"],
      detail: string,
    ) =>
      refused.push({
        kind,
        key: event.key,
        idCbor: event.idCbor,
        reason,
        detail,
      });
    for (const chunk of chunks(events)) {
      const ids = chunk.map((event) => Buffer.from(event.idCbor, "hex"));
      const existing = new Map(
        (yield* sql<ExistingRow>`SELECT e.event_id, e.l1_event_key, e.l1_origin_outref,
            ${canonicalAdmission(sql, "e", kind)} AS canonical
            ${kind === "withdrawal" ? sql`, e.withdrawal_l1_tx_hash, e.withdrawal_l1_output_index` : sql``}
          FROM ${sql(eventTable)} e WHERE ${sql.in("e.event_id", ids)} FOR UPDATE OF e`).map(
          (row) => [row.event_id.toString("hex"), row] as const,
        ),
      );
      const inserts: Readonly<Record<string, Statement.Argument>>[] = [];
      for (const event of chunk) {
        const row = existing.get(event.idCbor);
        const identity = identityOf(event);
        if (row === undefined) {
          if (event.retirement !== null) {
            // Retired before the node saw it: the event left the list in an
            // L1 tx, so no block of this node can include it, and there is no
            // L2 association to carry. Nothing to ingest.
            retiredUnseen += 1;
            continue;
          }
          let payload: Readonly<Record<string, Statement.Argument>>;
          try {
            payload = rowOf(event, network);
          } catch (cause) {
            refuse(
              event,
              EVENT_UNDECODABLE,
              cause instanceof Error ? cause.message : String(cause),
            );
            continue;
          }
          inserts.push(
            kind === "withdrawal"
              ? {
                  ...payload,
                  [Withdrawals.Columns.VALIDITY_DETAIL]:
                    sql`CAST('{}' AS TEXT)::JSONB`,
                }
              : payload,
          );
          continue;
        }
        const same =
          sameBytes(row.l1_event_key, identity.l1_event_key) &&
          sameBytes(row.l1_origin_outref, identity.l1_origin_outref);
        if (!same) {
          // An orphaned row of the same public id waits for recovery to
          // reject its dependents and remove it; the id is readmitted after.
          if (row.l1_event_key !== null && !row.canonical) continue;
          refuse(
            event,
            EVENT_IDENTITY_CONFLICT,
            row.l1_event_key === null
              ? "a local row of its public id has no admission identity"
              : "a local row of its public id holds another live admission",
          );
          continue;
        }
        // Same admission: the row keeps its L2 state. A deposit's L1 tx hash
        // is its admission tx (ruling 2); only a withdrawal's location moves.
        if (kind === "withdrawal") {
          const location = event.location;
          if (
            row.withdrawal_l1_tx_hash === undefined ||
            !row.withdrawal_l1_tx_hash.equals(location.txHash) ||
            row.withdrawal_l1_output_index !== location.index
          ) {
            const updated =
              yield* sql`UPDATE ${sql(Withdrawals.tableName)} SET ${sql(Withdrawals.Columns.WITHDRAWAL_L1_TX_HASH)} = ${Buffer.from(location.txHash)},
                ${sql(Withdrawals.Columns.WITHDRAWAL_L1_OUTPUT_INDEX)} = ${location.index}, updated_at = NOW()
              WHERE event_id = ${Buffer.from(event.idCbor, "hex")} RETURNING event_id`;
            if (updated.length !== 1)
              return yield* fail(
                eventTable,
                "Event row changed before follower ingestion",
                event.idCbor,
              );
            locationsMoved += 1;
          }
        }
      }
      if (inserts.length !== 0) {
        const columns = Object.keys(inserts[0]!);
        const written = yield* sql<{
          event_id: Buffer;
        }>`INSERT INTO ${sql(eventTable)}
          (${sql.csv(columns.map((column) => sql`${sql(column)}`))})
          VALUES ${sql.csv(inserts.map((entry) => sql`(${sql.csv(columns.map((column) => sql`${entry[column]}`))})`))}
          ON CONFLICT (event_id) DO NOTHING RETURNING event_id`;
        if (written.length !== inserts.length)
          return yield* fail(
            eventTable,
            "Event row changed before follower ingestion",
          );
        inserted += written.length;
      }
    }
    return { inserted, locationsMoved, retiredUnseen, refused };
  });

const sameProjectedDepositEntry = (
  expected: MempoolLedgerDB.DepositEntry,
  actual: MempoolLedgerDB.EntryWithTimeStamp,
): boolean =>
  expected[MempoolLedgerDB.Columns.TX_ID].equals(
    actual[MempoolLedgerDB.Columns.TX_ID],
  ) &&
  expected[MempoolLedgerDB.Columns.OUTREF].equals(
    actual[MempoolLedgerDB.Columns.OUTREF],
  ) &&
  expected[MempoolLedgerDB.Columns.OUTPUT].equals(
    actual[MempoolLedgerDB.Columns.OUTPUT],
  ) &&
  expected[MempoolLedgerDB.Columns.ADDRESS] ===
    actual[MempoolLedgerDB.Columns.ADDRESS] &&
  actual[MempoolLedgerDB.Columns.SOURCE_EVENT_ID] !== null &&
  expected[MempoolLedgerDB.Columns.SOURCE_EVENT_ID].equals(
    actual[MempoolLedgerDB.Columns.SOURCE_EVENT_ID],
  );

/** Projected deposits keep their mempool rows; a missing one is restored. */
const reconcileAlreadyProjectedDeposits = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const projectedEntries =
    yield* sql<Deposits.Entry>`SELECT d.* FROM ${sql(Deposits.tableName)} d
    WHERE d.${sql(Deposits.Columns.STATUS)} = ${Deposits.Status.Projected}
      AND ${canonicalAdmission(sql, "d", "deposit")}
    ORDER BY d.${sql(Deposits.Columns.INCLUSION_TIME)} ASC, d.${sql(Deposits.Columns.ID)} ASC`;
  if (projectedEntries.length === 0)
    return [] as readonly MempoolLedgerDB.DepositEntry[];
  const mempoolEntries = yield* Effect.forEach(
    projectedEntries,
    Deposits.toMempoolLedgerEntry,
  );
  const existing = yield* MempoolLedgerDB.retrieveBySourceEventIds(
    projectedEntries.map((entry) => entry[Deposits.Columns.ID]),
  );
  const existingBySource = new Map(
    existing.flatMap((entry) => {
      const source = entry[MempoolLedgerDB.Columns.SOURCE_EVENT_ID];
      return source === null ? [] : [[source.toString("hex"), entry] as const];
    }),
  );
  const missing: MempoolLedgerDB.DepositEntry[] = [];
  for (const entry of mempoolEntries) {
    const source =
      entry[MempoolLedgerDB.Columns.SOURCE_EVENT_ID].toString("hex");
    const current = existingBySource.get(source);
    if (current === undefined) {
      missing.push(entry);
      continue;
    }
    if (!sameProjectedDepositEntry(entry, current))
      return yield* fail(
        MempoolLedgerDB.tableName,
        "Projected deposit reconciliation found an existing mempool_ledger row with mismatched payload",
        `source_event_id=${source}`,
      );
  }
  yield* MempoolLedgerDB.insertDepositEntriesStrict(missing);
  const headerAssigned = new Set(
    projectedEntries
      .filter((entry) => entry[Deposits.Columns.PROJECTED_HEADER_HASH] !== null)
      .map((entry) => entry[Deposits.Columns.ID].toString("hex")),
  );
  return missing.filter((entry) =>
    headerAssigned.has(
      entry[MempoolLedgerDB.Columns.SOURCE_EVENT_ID].toString("hex"),
    ),
  );
});

/** Awaiting deposits due by `cutoff` move into the mempool ledger, hidden. */
const projectAwaitingDeposits = (cutoff: Date) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const due =
      yield* sql<Deposits.Entry>`SELECT d.* FROM ${sql(Deposits.tableName)} d
      WHERE d.${sql(Deposits.Columns.STATUS)} = ${Deposits.Status.Awaiting}
        AND d.${sql(Deposits.Columns.INCLUSION_TIME)} <= ${cutoff}
        AND ${canonicalAdmission(sql, "d", "deposit")}
      ORDER BY d.${sql(Deposits.Columns.INCLUSION_TIME)} ASC, d.${sql(Deposits.Columns.ID)} ASC
      FOR UPDATE OF d`;
    if (due.length === 0) return 0;
    const entries = yield* Effect.forEach(due, Deposits.toMempoolLedgerEntry);
    yield* MempoolLedgerDB.reconcileDepositEntries(entries);
    const ids = due.map((entry) => entry[Deposits.Columns.ID]);
    yield* sql`UPDATE ${sql(Deposits.tableName)} SET ${sql(Deposits.Columns.STATUS)} = ${Deposits.Status.Projected}
      WHERE ${sql.in(Deposits.Columns.ID, ids)} AND ${sql(Deposits.Columns.STATUS)} = ${Deposits.Status.Awaiting}`;
    return entries.length;
  });

/** POSIX ms at the start of the view's slot, through the caller's slot mapping. */
export type ViewTime = (slot: number) => number;

/**
 * Ingests `plan` in the caller's transaction: the follower-change driver's
 * gated transaction (`withFollowerWrite` under `FollowerDriverWrite`).
 * `cutoffMs` bounds the deposit projection; the driver passes the view's
 * time.
 */
export const reconcileFollowerEvents = (
  plan: IngestionPlan,
  input: Readonly<{
    network: Network;
    slotToUnixTime: ViewTime;
    cutoffMs: number;
  }>,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    if (!(yield* followerViewValid(plan.view)))
      return { kind: "stale" } as FollowerIngestionOutcome;
    const deposits = yield* ingestKind(
      "deposit",
      plan.events.filter((event) => event.kind === "deposit"),
      input.network,
    );
    const withdrawals = yield* ingestKind(
      "withdrawal",
      plan.events.filter((event) => event.kind === "withdrawal"),
      input.network,
    );
    // Every row is either backed by its canonical admission or an orphan the
    // recovery removes; an unassociated row is never eligible.
    for (const kind of ["deposit", "withdrawal"] as const) {
      const unassociated = yield* sql<{ event_id: Buffer }>`SELECT event_id
        FROM ${sql(EVENT_TABLE[kind])} WHERE l1_event_key IS NULL LIMIT 1`;
      if (unassociated.length !== 0)
        return yield* fail(
          EVENT_TABLE[kind],
          "Local event eligibility is not backed by this canonical history",
          unassociated[0]!.event_id.toString("hex"),
        );
    }
    const orphans = yield* countOrphansAwaitingRecovery;
    const spendableUpserts = yield* reconcileAlreadyProjectedDeposits;
    const projected = yield* projectAwaitingDeposits(new Date(input.cutoffMs));
    const ingestedThroughMs = input.slotToUnixTime(plan.view.point.slot);
    yield* sql`INSERT INTO follower_event_ingestion
        (id, generation, slot, block_hash, height, ingested_through_ms, updated_at)
      VALUES (true, ${plan.view.generation}, ${plan.view.point.slot}, ${Buffer.from(plan.view.point.hash)},
        ${plan.view.height}, ${ingestedThroughMs}, NOW())
      ON CONFLICT (id) DO UPDATE SET generation = EXCLUDED.generation, slot = EXCLUDED.slot,
        block_hash = EXCLUDED.block_hash, height = EXCLUDED.height,
        ingested_through_ms = EXCLUDED.ingested_through_ms, updated_at = NOW()`;
    return {
      kind: "applied",
      ingestion: {
        inserted: deposits.inserted + withdrawals.inserted,
        locationsMoved: withdrawals.locationsMoved,
        orphans,
        retiredUnseen: deposits.retiredUnseen + withdrawals.retiredUnseen,
        projected,
        spendableUpserts,
        refused: [...deposits.refused, ...withdrawals.refused],
      },
    } as FollowerIngestionOutcome;
  }).pipe(sqlErrorToDatabaseError(table, "Failed follower event ingestion"));

/**
 * The view the driver last ingested events through, while it is still on
 * the follower's chain; `null` before the first ingestion or after a rewind
 * removed it. A commit plans at its write permit's view, which the driver
 * advances in the same transaction as this row; a model fixture without a
 * permit plans at this one (`commitEventHorizon`).
 */
export const followerIngestedView = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  // The ingestion row and its view check read one cursor state.
  return yield* sql.withTransaction(
    Effect.gen(function* () {
      const rows = yield* sql<{
        generation: string;
        slot: string;
        block_hash: Buffer;
        height: string;
      }>`SELECT generation::text AS generation, slot::text AS slot, block_hash,
          height::text AS height
        FROM follower_event_ingestion`;
      const row = rows[0];
      if (row === undefined) return null;
      const view = {
        generation: Number(row.generation),
        point: { slot: Number(row.slot), hash: Buffer.from(row.block_hash) },
        height: Number(row.height),
      };
      return (yield* followerViewValid(view)) ? view : null;
    }),
  );
}).pipe(
  sqlErrorToDatabaseError(table, "Failed to read the follower ingestion view"),
);
