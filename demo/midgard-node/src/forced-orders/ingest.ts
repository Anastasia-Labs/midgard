/**
 * The forced-order ingestion hook of the follower-change driver (plan §12.3
 * steps 2 to 4, §7.3). On every driver run it reads the live forced orders
 * at the follower's view, and for each one the node has no
 * `forced_transaction_utxos` row yet:
 *
 * - `resolved`: rebuilds the order's transaction from the stored field
 *   preimages;
 * - `carriage_pending`: resolves the carriage outrefs its block did not
 *   create, through the ledger at the block's parent point, the ledger at
 *   the tip, then the content sources (each answer hash-checked), and
 *   rebuilds the transaction when all of them resolve;
 * - `malformed`: nothing to rebuild.
 *
 * Rebuilt rows are inserted in one node transaction that re-checks the
 * follower view. An order whose carriage no source has yet keeps the node
 * unready with `forced_order_carriage_pending` and is retried on the
 * driver's backoff; it never exits the process and never writes a
 * verdict. Its inclusion time bounds the commit horizon
 * (`forcedOrderHorizon`), so it also holds every block that would end at
 * or after it. With no content source configured the pending detail
 * leads with `NO_CONTENT_SOURCE`: only the local ledger and the
 * follower's own transactions can resolve its carriage. An order one of the three ruled admission stops refuses (the
 * auxiliary-data hash, the script program envelope, the output value size)
 * holds `forced_order_admission_stopped`, naming the stop; any other order
 * that cannot be rebuilt holds `forced_order_ingestion_failed`.
 *
 * A rollback that removes an order removes its follower key and its order
 * row (N10b). In the same transaction, while the follower is caught up, the
 * hook deletes each node row without a header whose order is gone and that
 * no unfinished block journal holds. A row such a journal holds is an
 * orphan for the event-history recovery (`countOrphanedAdmissions`) and
 * keeps the node unready with `l1_events_orphan_recovery` until that
 * journal is disposed of; a row with a header is its header's. An order
 * that lands again is ingested again from its own bytes, to the same row.
 */
import type { MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import type { MidgardForcedTxAdmissionStopped } from "@al-ft/midgard-core/consensus-validation";
import {
  type FactStore,
  type LedgerOutputs,
  resolveOutputs,
  type TxContentSource,
  type View,
} from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import type { UTxO } from "@lucid-evolution/lucid";
import { Cause, Effect, Exit, Option } from "effect";

import { followerViewValid } from "../database/follower-schema.js";
import { ForcedTransactionsDB } from "../database/index.js";
import {
  abandonedForcedAdmission,
  orphanedForcedAdmission,
} from "../database/l1-admission-identity.js";
import {
  type DriverHold,
  type DriverHook,
  EVENTS_ORPHAN_RECOVERY,
} from "../l1-events/driver.js";
import type { NodeConfig } from "../services/config.js";
import type { Database } from "../services/database.js";
import {
  carriageFieldPreimages,
  carriageOutRefs,
  carriageVector,
  decodeFieldPreimages,
} from "./carriage.js";
import type { ForcedOrderConfig } from "./config.js";
import { authenticOrder, outRefLabel } from "./derive.js";
import { forcedOrderEntry } from "./entry.js";
import { type ForcedOrderRow, forcedOrdersAt } from "./reads.js";

/** An order's carriage resolved from no source yet; it is retried. */
export const FORCED_ORDER_CARRIAGE_PENDING = "forced_order_carriage_pending";
/**
 * The note a pending carriage's detail leads with, and the node logs at
 * start, when `L1_TX_CONTENT_SOURCES` is unset.
 */
export const NO_CONTENT_SOURCE =
  "no L1 tx content source is configured: set L1_TX_CONTENT_SOURCES so carriage the local ledger and the follower's transactions lack can resolve";
/** An order could not be rebuilt (malformed, or its bytes do not open). */
export const FORCED_ORDER_INGESTION_FAILED = "forced_order_ingestion_failed";
/**
 * One of the three ruled admission stops refused an order's transaction:
 * its auxiliary-data hash, a script program envelope, or an output value's
 * size. The detail names the stop. It holds the horizon as a failure does.
 */
export const FORCED_ORDER_ADMISSION_STOPPED = "forced_order_admission_stopped";

/** The ruled stops `forced_order_admission_stopped` names. */
const RULED_STOPS: ReadonlySet<string> = new Set([
  "E_AUX_DATA_FORBIDDEN",
  "E_SCRIPT_PROGRAM_ENCODING",
  "E_VALUE_SIZE",
]);

/** Runs a node database effect (the node's runtime, or a test's). */
export type RunDatabase = <A, E>(
  effect: Effect.Effect<A, E, Database | NodeConfig>,
) => Promise<Exit.Exit<A, E>>;

export type ForcedOrderIngestionOptions = Readonly<{
  store: FactStore;
  config: ForcedOrderConfig;
  consensusProfile: MidgardConsensusProfile;
  /** §12.3 steps 2 and 3; absent leaves only the content sources. */
  ledger?: LedgerOutputs;
  /** §12.3 step 4, in order. */
  sources?: readonly TxContentSource[];
  /**
   * Whether `sources` includes a configured remote content source
   * (`L1_TX_CONTENT_SOURCES`); when false a pending carriage's detail leads
   * with `NO_CONTENT_SOURCE`.
   */
  contentSourcesConfigured: boolean;
  run: RunDatabase;
  /**
   * Whether the follower is caught up; rows whose order is gone are deleted
   * only then (a follower replaying from behind has not yet re-derived every
   * order). Absent: always.
   */
  caughtUp?: () => boolean;
  log?: (line: string) => void;
}>;

const DETAIL_LIMIT = 1_000;

const failureText = <E>(exit: Exit.Exit<unknown, E>): string =>
  Exit.isFailure(exit) ? Cause.pretty(exit.cause) : "";

/** The outrefs of orders that already have a node row. */
const ingestedOrders = (orders: readonly ForcedOrderRow[]) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const hashes = [...new Set(orders.map((o) => o.utxo.txHash))].map((h) =>
      Buffer.from(h, "hex"),
    );
    const rows = yield* sql<{
      tx_order_l1_tx_hash: Buffer;
      tx_order_l1_output_index: number;
    }>`SELECT tx_order_l1_tx_hash, tx_order_l1_output_index
      FROM ${sql(ForcedTransactionsDB.tableName)}
      WHERE tx_order_l1_tx_hash IN ${sql.in(hashes)}`;
    return new Set(
      rows.map((row) =>
        outRefLabel({
          txHash: Buffer.from(row.tx_order_l1_tx_hash),
          index: Number(row.tx_order_l1_output_index),
        }),
      ),
    );
  });

type OrderOutRef = Readonly<{
  tx_order_l1_tx_hash: Buffer;
  tx_order_l1_output_index: number;
}>;

const rowLabel = (row: OrderOutRef): string =>
  outRefLabel({
    txHash: Buffer.from(row.tx_order_l1_tx_hash),
    index: Number(row.tx_order_l1_output_index),
  });

/** What one write at a view did. */
type ViewWrite =
  | Readonly<{ kind: "stale" }>
  | Readonly<{
      kind: "written";
      /** Rows deleted because their order is gone. */
      deleted: readonly string[];
      /** Rows whose order is gone that an unfinished block journal holds. */
      orphaned: readonly string[];
    }>;

/**
 * If the follower is still at `view`: deletes the rows whose order is gone
 * (when `sweep`), inserts `entries`, and names the orphans left for the
 * recovery. The table lock orders this against a block journal's due-set
 * check, which holds it in SHARE mode until the journal commits.
 */
const writeAtView = (
  view: View,
  entries: readonly ForcedTransactionsDB.Entry[],
  sweep: boolean,
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        if (!(yield* followerViewValid(view)))
          return { kind: "stale" } as ViewWrite;
        yield* sql`LOCK TABLE ${sql(ForcedTransactionsDB.tableName)} IN SHARE ROW EXCLUSIVE MODE`;
        const deleted = sweep
          ? yield* sql<OrderOutRef>`DELETE FROM ${sql(ForcedTransactionsDB.tableName)} t
              WHERE ${abandonedForcedAdmission(sql, "t")}
              RETURNING t.tx_order_l1_tx_hash, t.tx_order_l1_output_index`
          : [];
        if (entries.length > 0)
          yield* ForcedTransactionsDB.insertEntries(entries);
        const orphaned = yield* sql<OrderOutRef>`SELECT
            t.tx_order_l1_tx_hash, t.tx_order_l1_output_index
          FROM ${sql(ForcedTransactionsDB.tableName)} t
          WHERE ${orphanedForcedAdmission(sql, "t")}
          ORDER BY t.tx_order_l1_tx_hash, t.tx_order_l1_output_index`;
        return {
          kind: "written",
          deleted: deleted.map(rowLabel),
          orphaned: orphaned.map(rowLabel),
        } as ViewWrite;
      }),
    );
  });

/** The field preimages of a pending order, or the outrefs still unresolved. */
const resolvePending = async (
  options: ForcedOrderIngestionOptions,
  row: ForcedOrderRow,
  order: SDK.TxOrderUTxOV1,
): Promise<
  | Readonly<{ kind: "ok"; preimages: Buffer[] }>
  | Readonly<{ kind: "pending"; detail: string }>
> => {
  if (row.mintRedeemer === null)
    throw new Error("a pending order has no stored mint redeemer");
  const carriage = carriageVector(row.mintRedeemer);
  const datums = new Map<string, Buffer | null>(
    Object.entries(row.blockDatums).map(([label, datum]) => [
      label,
      datum === null ? null : Buffer.from(datum, "hex"),
    ]),
  );
  const missing = carriageOutRefs(carriage, row.referenceInputs).filter(
    (outRef) => !datums.has(outRefLabel(outRef)),
  );
  const outcome = await resolveOutputs({
    outRefs: missing,
    parent: row.parent,
    ...(options.ledger === undefined ? {} : { ledger: options.ledger }),
    ...(options.sources === undefined ? {} : { sources: options.sources }),
  });
  for (const { outRef, output, step, source } of outcome.resolved) {
    datums.set(outRefLabel(outRef), output.datum);
    options.log?.(
      `forced order ${outRefLabel(row.outRef)}: carriage ${outRefLabel(outRef)} resolved (${step}${source === undefined ? "" : ` ${source}`})`,
    );
  }
  if (outcome.pending.length > 0)
    return {
      kind: "pending",
      detail: `${outRefLabel(row.outRef)} awaits ${outcome.pending.map(outRefLabel).join(", ")}${outcome.notes.length === 0 ? "" : ` (${outcome.notes.join("; ")})`}`,
    };
  return {
    kind: "ok",
    preimages: carriageFieldPreimages({
      payload: order.datum.event.tx,
      carriage,
      referenceInputs: row.referenceInputs,
      datumOf: (outRef) => datums.get(outRefLabel(outRef)) ?? null,
    }),
  };
};

/** The preimages of one order, a pending note, or why it cannot be rebuilt. */
export type OrderPreimages =
  | Readonly<{ kind: "ok"; order: SDK.TxOrderUTxOV1; preimages: Buffer[] }>
  | Readonly<{ kind: "pending"; detail: string }>
  | Readonly<{ kind: "failed"; detail: string }>;

const preimagesOf = async (
  options: ForcedOrderIngestionOptions,
  row: ForcedOrderRow,
): Promise<OrderPreimages> => {
  const label = outRefLabel(row.outRef);
  const order = authenticOrder(row.utxo, options.config.policyId);
  if (order === null)
    return { kind: "failed", detail: `${label}: no longer authenticates` };
  if (row.status === "malformed")
    return { kind: "failed", detail: `${label}: ${row.detail ?? "malformed"}` };
  try {
    if (row.status === "resolved") {
      if (row.fieldPreimages === null)
        throw new Error("a resolved order has no stored field preimages");
      return {
        kind: "ok",
        order,
        preimages: decodeFieldPreimages(row.fieldPreimages),
      };
    }
    const pending = await resolvePending(options, row, order);
    return pending.kind === "ok" ? { ...pending, order } : pending;
  } catch (error) {
    return {
      kind: "failed",
      detail: `${label}: ${error instanceof Error ? error.message : String(error)}`,
    };
  }
};

/** The node row of an order whose preimages are in hand. */
const entryOf = async (
  options: ForcedOrderIngestionOptions,
  outcome: OrderPreimages,
  programMaterial: () => readonly UTxO[],
): Promise<
  | Readonly<{ kind: "entry"; entry: ForcedTransactionsDB.Entry }>
  | Exclude<OrderPreimages, { kind: "ok" }>
  | Readonly<{ kind: "stopped"; detail: string }>
> => {
  if (outcome.kind !== "ok") return outcome;
  const exit = await options.run(
    forcedOrderEntry({
      order: outcome.order,
      fieldPreimages: outcome.preimages,
      consensusProfile: options.consensusProfile,
      programMaterial,
    }),
  );
  const label = `${outcome.order.utxo.txHash}#${outcome.order.utxo.outputIndex.toString()}`;
  if (Exit.isSuccess(exit)) return { kind: "entry", entry: exit.value };
  const stop = ruledStop(exit.cause);
  return stop === undefined
    ? { kind: "failed", detail: `${label}: ${failureText(exit)}` }
    : {
        kind: "stopped",
        detail: `${label}: ${stop.code} ${stop.violation.featureId} (${stop.violation.detail})`,
      };
};

/** The ruled admission stop a failed entry stopped at, if it was one. */
const ruledStop = <E>(
  cause: Cause.Cause<E>,
): MidgardForcedTxAdmissionStopped | undefined => {
  const failure = Cause.failureOption(cause);
  if (Option.isNone(failure)) return undefined;
  const error = failure.value as Partial<MidgardForcedTxAdmissionStopped>;
  return error?._tag === "MidgardForcedTxAdmissionStopped" &&
    error.code !== undefined &&
    RULED_STOPS.has(error.code)
    ? (error as MidgardForcedTxAdmissionStopped)
    : undefined;
};

/** The driver hook that ingests the follower's forced orders. */
export const forcedOrderIngestionHook =
  (options: ForcedOrderIngestionOptions): DriverHook =>
  async () => {
    const view = await options.store.currentView();
    if (view === null) return undefined;
    const read = await forcedOrdersAt(
      options.store,
      options.config,
      view.point,
    );
    if (read.kind === "point_not_canonical") return undefined; // the next run reads the new view
    if (read.kind !== "ok")
      return {
        reason: FORCED_ORDER_INGESTION_FAILED,
        detail: `forced orders at ${view.point.slot.toString()}: ${read.kind} (${read.detail})`,
      };
    const done =
      read.orders.length === 0
        ? Exit.succeed(new Set<string>())
        : await options.run(ingestedOrders(read.orders));
    if (Exit.isFailure(done))
      return {
        reason: FORCED_ORDER_INGESTION_FAILED,
        detail: failureText(done),
      };
    const programMaterial = (): readonly UTxO[] => read.programMaterial;
    const entries: ForcedTransactionsDB.Entry[] = [];
    const pending: string[] = [];
    const stopped: string[] = [];
    const failed: string[] = [];
    for (const row of read.orders) {
      if (done.value.has(outRefLabel(row.outRef))) continue;
      const outcome = await entryOf(
        options,
        await preimagesOf(options, row),
        programMaterial,
      );
      if (outcome.kind === "entry") entries.push(outcome.entry);
      else if (outcome.kind === "pending") pending.push(outcome.detail);
      else if (outcome.kind === "stopped") stopped.push(outcome.detail);
      else failed.push(outcome.detail);
    }
    const sweep = options.caughtUp?.() ?? true;
    const written = await options.run(writeAtView(view, entries, sweep));
    const orphaned: string[] = [];
    if (Exit.isFailure(written)) failed.push(`write: ${failureText(written)}`);
    else if (written.value.kind === "written") {
      const { deleted } = written.value;
      orphaned.push(...written.value.orphaned);
      if (entries.length > 0)
        options.log?.(
          `ingested ${entries.length.toString()} forced order(s) at ${view.point.slot.toString()}`,
        );
      if (deleted.length > 0)
        options.log?.(
          `deleted ${deleted.length.toString()} forced row(s) whose order left the chain: ${deleted.join(", ")}`,
        );
    }
    const hold = (reason: string, details: readonly string[]): DriverHold => ({
      reason,
      detail: details.join(" | ").slice(0, DETAIL_LIMIT),
    });
    const more = (count: number, what: string): string[] =>
      count === 0 ? [] : [`${count.toString()} more ${what}`];
    if (failed.length > 0)
      return hold(FORCED_ORDER_INGESTION_FAILED, [
        ...failed,
        ...more(stopped.length, "stopped at a ruled admission stop"),
        ...more(pending.length, "await carriage"),
      ]);
    if (stopped.length > 0)
      return hold(FORCED_ORDER_ADMISSION_STOPPED, [
        ...stopped,
        ...more(pending.length, "await carriage"),
      ]);
    if (orphaned.length > 0)
      return hold(EVENTS_ORPHAN_RECOVERY, [
        `forced row(s) whose order left the chain wait for their block journal's recovery: ${orphaned.join(", ")}`,
      ]);
    if (pending.length > 0)
      return hold(FORCED_ORDER_CARRIAGE_PENDING, [
        ...(options.contentSourcesConfigured ? [] : [NO_CONTENT_SOURCE]),
        ...pending,
      ]);
    return undefined;
  };
