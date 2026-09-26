import { SqlClient, type Statement } from "@effect/sql";
import type { Network } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { historyIncarnationEntry } from "../l1-event-history-entries.js";
import {
  type HistoryIncarnation,
  historyIncarnationDigest,
} from "../l1-event-history-provenance.js";
import type { HistoryOwnerChange } from "../services/event-history-owner.js";
import * as Deposits from "./deposits.js";
import { requireSourceTransaction } from "./eventHistoryAuthority.js";
import { repairUnpublishedHistoryLedger } from "./eventHistoryLedgerRepair.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";
import * as Withdrawals from "./withdrawals.js";

const table = "event_history_incarnations";
type Association = {
  event_id: Buffer;
  history_binding_digest: Buffer | null;
  history_incarnation_id: Buffer | null;
};
// The immutable columns each ordinary upsert requires equal before it
// refreshes a location, and the location it refreshes.
type DepositRow = Association & {
  event_info: Buffer;
  inclusion_time: Date;
  inclusion_whole_ms: boolean;
  deposit_l1_tx_hash: Buffer;
  ledger_tx_id: Buffer;
  ledger_output: Buffer;
  ledger_address: string;
};
type WithdrawalRow = Association & {
  raw_event_info: Buffer;
  inclusion_time: Date;
  inclusion_whole_ms: boolean;
  withdrawal_l1_tx_hash: Buffer;
  withdrawal_l1_output_index: number;
  asset_name: Buffer;
  l2_outref: Buffer;
  l2_owner: Buffer;
  l2_value: Buffer;
  l1_address: Buffer;
  l1_datum: Buffer;
  refund_address: Buffer;
  refund_datum: Buffer;
};
const fail = (message: string, cause?: unknown) =>
  Effect.fail(new DatabaseError({ table, message, cause }));

/** Incarnations (and event rows) read, verified and written per statement.
 * Each walk costs a constant number of statements per chunk plus one UPDATE
 * per moved location, and opens no savepoint. */
export const MATERIALIZATION_CHUNK = 500;

const chunks = <A>(values: readonly A[]) => {
  const result: A[][] = [];
  for (let at = 0; at < values.length; at += MATERIALIZATION_CHUNK)
    result.push(values.slice(at, at + MATERIALIZATION_CHUNK));
  return result;
};
const same = (a: Uint8Array, b: Uint8Array) => Buffer.compare(a, b) === 0;
// SQL equality on the stored timestamp: rows hold whole milliseconds, as does
// every Date a history entry carries.
const sameTime = (
  row: { inclusion_time: Date; inclusion_whole_ms: boolean },
  value: Date,
) => row.inclusion_whole_ms && row.inclusion_time.getTime() === value.getTime();
const sameDeposit = (row: DepositRow, entry: Deposits.Entry) =>
  same(row.event_info, entry[Deposits.Columns.INFO]) &&
  sameTime(row, entry[Deposits.Columns.INCLUSION_TIME]) &&
  same(row.ledger_tx_id, entry[Deposits.Columns.LEDGER_TX_ID]) &&
  same(row.ledger_output, entry[Deposits.Columns.LEDGER_OUTPUT]) &&
  row.ledger_address === entry[Deposits.Columns.LEDGER_ADDRESS];
const sameWithdrawal = (row: WithdrawalRow, entry: Withdrawals.Entry) =>
  same(row.raw_event_info, entry[Withdrawals.Columns.RAW_EVENT_INFO]) &&
  sameTime(row, entry[Withdrawals.Columns.INCLUSION_TIME]) &&
  same(row.asset_name, entry[Withdrawals.Columns.ASSET_NAME]) &&
  same(row.l2_outref, entry[Withdrawals.Columns.L2_OUTREF]) &&
  same(row.l2_owner, entry[Withdrawals.Columns.L2_OWNER]) &&
  same(row.l2_value, entry[Withdrawals.Columns.L2_VALUE]) &&
  same(row.l1_address, entry[Withdrawals.Columns.L1_ADDRESS]) &&
  same(row.l1_datum, entry[Withdrawals.Columns.L1_DATUM]) &&
  same(row.refund_address, entry[Withdrawals.Columns.REFUND_ADDRESS]) &&
  same(row.refund_datum, entry[Withdrawals.Columns.REFUND_DATUM]);

/** One chunk of the walk, in walk order: the same checks, verdicts and
 * messages as materializing each incarnation alone. Reads lock every
 * incarnation and event row of the chunk in one statement per table; new
 * admissions are inserted in one statement per table after every check. */
const materializeChunk = (
  bindingDigest: string,
  incarnations: readonly HistoryIncarnation[],
  network: Network,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const binding = Buffer.from(bindingDigest, "hex");
    const ids = incarnations.map((value) => Buffer.from(value.id, "hex"));
    const stored = new Map(
      (yield* sql<{
        incarnation_id: Buffer;
        incarnation_digest: Buffer;
        origin_canonical: boolean;
        kind: string;
        event_id: Buffer;
      }>`
      SELECT incarnation_id, incarnation_digest, origin_canonical, kind, event_id FROM event_history_incarnations
      WHERE binding_digest = ${binding} AND ${sql.in("incarnation_id", ids)} FOR UPDATE`).map(
        (row) => [row.incarnation_id.toString("hex"), row] as const,
      ),
    );
    const eventIds = (kind: HistoryIncarnation["kind"]) => [
      ...new Map(
        incarnations
          .filter((value) => value.kind === kind)
          .map((value) => [
            value.event.idCbor,
            Buffer.from(value.event.idCbor, "hex"),
          ]),
      ).values(),
    ];
    const depositIds = eventIds("deposit");
    const withdrawalIds = eventIds("withdrawal");
    const deposits = new Map(
      (depositIds.length === 0
        ? []
        : yield* sql<DepositRow>`SELECT event_id, history_binding_digest, history_incarnation_id,
            event_info, inclusion_time, date_trunc('milliseconds', inclusion_time) = inclusion_time AS inclusion_whole_ms,
            deposit_l1_tx_hash, ledger_tx_id, ledger_output, ledger_address
          FROM ${sql(Deposits.tableName)} WHERE ${sql.in("event_id", depositIds)} FOR UPDATE`
      ).map((row) => [row.event_id.toString("hex"), row] as const),
    );
    const withdrawals = new Map(
      (withdrawalIds.length === 0
        ? []
        : yield* sql<WithdrawalRow>`SELECT event_id, history_binding_digest, history_incarnation_id,
            raw_event_info, inclusion_time, date_trunc('milliseconds', inclusion_time) = inclusion_time AS inclusion_whole_ms,
            withdrawal_l1_tx_hash, withdrawal_l1_output_index, asset_name, l2_outref, l2_owner, l2_value,
            l1_address, l1_datum, refund_address, refund_datum
          FROM ${sql(Withdrawals.tableName)} WHERE ${sql.in("event_id", withdrawalIds)} FOR UPDATE`
      ).map((row) => [row.event_id.toString("hex"), row] as const),
    );
    type Insert = {
      id: string;
      eventId: Buffer;
      entry: Readonly<Record<string, Statement.Argument>>;
    };
    const inserts: Record<string, Insert[]> = {
      [Deposits.tableName]: [],
      [Withdrawals.tableName]: [],
    };
    for (const incarnation of incarnations) {
      const id = Buffer.from(incarnation.id, "hex");
      const eventId = Buffer.from(incarnation.event.idCbor, "hex");
      const row = stored.get(incarnation.id);
      if (
        incarnation.bindingDigest !== bindingDigest ||
        row === undefined ||
        row.incarnation_digest.toString("hex") !==
          historyIncarnationDigest(incarnation) ||
        row.origin_canonical !== (incarnation.placement !== null) ||
        row.kind !== incarnation.kind ||
        !row.event_id.equals(eventId)
      )
        return yield* fail(
          "History materialization incarnation changed",
          incarnation.id,
        );
      const previous = (
        incarnation.kind === "deposit" ? deposits : withdrawals
      ).get(incarnation.event.idCbor);
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
        inserts[
          materialized.kind === "deposit"
            ? Deposits.tableName
            : Withdrawals.tableName
        ].push({
          id: incarnation.id,
          eventId,
          // Every column is a primitive, or the rendered JSONB cast above.
          entry: {
            ...payload,
            history_binding_digest: binding,
            history_incarnation_id: id,
          } as Insert["entry"],
        });
        continue;
      }
      // An existing exact row keeps its L2 state; only a continuation's new
      // location is written, and only when it moved. Conflicting immutable
      // bytes are refused as the ordinary writers refuse them.
      if (materialized.kind === "deposit") {
        const entry = materialized.entry;
        const current = previous as DepositRow;
        if (!sameDeposit(current, entry))
          return yield* Effect.fail(
            new DatabaseError({
              table: Deposits.tableName,
              message:
                "Refusing to upsert deposit because the same event_id has conflicting persisted payload",
              cause: `event_id=${incarnation.event.idCbor}`,
            }),
          );
        const location = entry[Deposits.Columns.DEPOSIT_L1_TX_HASH];
        if (!current.deposit_l1_tx_hash.equals(location)) {
          const updated =
            yield* sql`UPDATE ${sql(Deposits.tableName)} SET ${sql(Deposits.Columns.DEPOSIT_L1_TX_HASH)} = ${location}
            WHERE event_id = ${eventId} RETURNING event_id`;
          if (updated.length !== 1)
            return yield* fail(
              "History admission row changed before materialization",
              incarnation.id,
            );
        }
      } else {
        const entry = materialized.entry;
        const current = previous as WithdrawalRow;
        if (!sameWithdrawal(current, entry))
          return yield* Effect.fail(
            new DatabaseError({
              table: Withdrawals.tableName,
              message:
                "Refusing to upsert withdrawal because the same event_id has conflicting persisted payload",
              cause: `event_id=${incarnation.event.idCbor}`,
            }),
          );
        const txHash = entry[Withdrawals.Columns.WITHDRAWAL_L1_TX_HASH];
        const outputIndex =
          entry[Withdrawals.Columns.WITHDRAWAL_L1_OUTPUT_INDEX];
        if (
          !current.withdrawal_l1_tx_hash.equals(txHash) ||
          current.withdrawal_l1_output_index !== outputIndex
        ) {
          const updated =
            yield* sql`UPDATE ${sql(Withdrawals.tableName)} SET ${sql(Withdrawals.Columns.WITHDRAWAL_L1_TX_HASH)} = ${txHash},
            ${sql(Withdrawals.Columns.WITHDRAWAL_L1_OUTPUT_INDEX)} = ${outputIndex}, updated_at = NOW()
            WHERE event_id = ${eventId} RETURNING event_id`;
          if (updated.length !== 1)
            return yield* fail(
              "History admission row changed before materialization",
              incarnation.id,
            );
        }
      }
    }
    for (const [eventTable, rows] of Object.entries(inserts)) {
      if (rows.length === 0) continue;
      const columns = Object.keys(rows[0]!.entry);
      // Render values explicitly so the withdrawal JSONB cast survives SQL
      // parameter construction, as in the ordinary withdrawal writer.
      const inserted = yield* sql<{
        event_id: Buffer;
      }>`INSERT INTO ${sql(eventTable)}
        (${sql.csv(columns.map((column) => sql`${sql(column)}`))})
        VALUES ${sql.csv(rows.map(({ entry }) => sql`(${sql.csv(columns.map((column) => sql`${entry[column]}`))})`))}
        ON CONFLICT (event_id) DO NOTHING RETURNING event_id`;
      if (inserted.length !== rows.length) {
        const written = new Set(
          inserted.map((row) => row.event_id.toString("hex")),
        );
        return yield* fail(
          "History admission row changed before materialization",
          rows.find((row) => !written.has(row.eventId.toString("hex")))?.id,
        );
      }
    }
  });

// An event row is eligible only when associated with its own canonical
// admission in this binding, of its table's kind.
const ineligible = (
  eventTable: string,
  kind: HistoryIncarnation["kind"],
  binding: Buffer,
  scope?: Readonly<{ eventIds: readonly Buffer[]; ids: readonly Buffer[] }>,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<{
      event_id: Buffer;
    }>`SELECT e.event_id FROM ${sql(eventTable)} e
      LEFT JOIN event_history_incarnations i
        ON i.binding_digest = e.history_binding_digest AND i.incarnation_id = e.history_incarnation_id
      WHERE ${
        scope === undefined
          ? sql`TRUE`
          : sql`(${sql.in("e.event_id", scope.eventIds)} OR (e.history_binding_digest = ${binding}
            AND ${sql.in("e.history_incarnation_id", scope.ids)}))`
      } AND (i.binding_digest IS DISTINCT FROM ${binding}
        OR i.origin_canonical IS DISTINCT FROM true OR i.kind IS DISTINCT FROM ${kind}
        OR i.event_id IS DISTINCT FROM e.event_id) LIMIT 1`;
  });

/** Bounded SQL materialization inside the source owner's transaction: recovery,
 * or a forward append at the head of its Ready generation.
 * A new live admission starts unclassified. Continuations and retirement retain
 * the exact existing row's L2 state. Unassociated rows and orphan-dependent L2
 * state require explicit repair; neither is silently adopted or discarded.
 * A forward append walks only the incarnations its block staged, and checks
 * eligibility of only the event rows that share their public IDs or are
 * associated with them; seed, rollback and resume walk and check everything.
 * This callback performs no network/native/cache work and never opens Ready.
 */
export const materializeCanonicalHistory = (
  change: HistoryOwnerChange,
  network: Network,
) =>
  Effect.gen(function* () {
    const token = yield* requireSourceTransaction;
    const checkpoint = change.after;
    if (token.deploymentIdentity !== checkpoint.manifestId)
      return yield* fail("History materialization deployment changed");
    const sql = yield* SqlClient.SqlClient;
    const binding = Buffer.from(checkpoint.bindingDigest, "hex");
    const cursor = yield* sql<{
      revision: string;
      head_hash: Buffer;
      snapshot_digest: Buffer;
    }>`
    SELECT revision::text, head_hash, snapshot_digest FROM event_history_cursor
    WHERE binding_digest = ${binding} FOR UPDATE`;
    if (
      cursor.length !== 1 ||
      cursor[0]!.revision !== checkpoint.revision ||
      cursor[0]!.head_hash.toString("hex") !== checkpoint.head.id ||
      cursor[0]!.snapshot_digest.toString("hex") !==
        checkpoint.capture.snapshotDigest
    )
      return yield* fail("History materialization checkpoint changed");
    yield* repairUnpublishedHistoryLedger(change);
    const walked =
      change.kind === "forward"
        ? change.changes.map(({ after }) => after)
        : checkpoint.incarnations;
    for (const chunk of chunks(walked))
      yield* materializeChunk(checkpoint.bindingDigest, chunk, network);
    // Every existing event row must remain associated with its own canonical
    // admission, including retired origins. This also rejects incomplete startup
    // materialization when local rows predate the authenticated replay anchor.
    // A forward block changes only its staged incarnations, so only rows
    // sharing their public IDs or associated with them can have changed
    // eligibility here; recovery rechecks every row.
    for (const [kind, eventTable] of [
      ["deposit", Deposits.tableName],
      ["withdrawal", Withdrawals.tableName],
    ] as const) {
      const scopes =
        change.kind === "forward"
          ? chunks(walked).map((chunk) => ({
              eventIds: chunk.map((value) =>
                Buffer.from(value.event.idCbor, "hex"),
              ),
              ids: chunk.map((value) => Buffer.from(value.id, "hex")),
            }))
          : [undefined];
      for (const scope of scopes) {
        const invalid = yield* ineligible(eventTable, kind, binding, scope);
        if (invalid.length !== 0)
          return yield* fail(
            "Local event eligibility is not backed by this canonical history",
            invalid[0]!.event_id.toString("hex"),
          );
      }
    }
  }).pipe(
    sqlErrorToDatabaseError(table, "Failed canonical history materialization"),
  );
