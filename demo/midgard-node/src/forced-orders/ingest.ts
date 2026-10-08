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
 * verdict. An order that cannot be rebuilt holds
 * `forced_order_ingestion_failed`.
 */
import type { MidgardConsensusProfile } from "@al-ft/midgard-core/consensus-profile";
import {
  type FactStore,
  type LedgerOutputs,
  postgresDialect,
  resolveOutputs,
  type TxContentSource,
  type View,
  viewValidQuery,
} from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import type { UTxO } from "@lucid-evolution/lucid";
import { Cause, Effect, Exit } from "effect";

import { numbered } from "../database/follower-schema.js";
import { ForcedTransactionsDB } from "../database/index.js";
import type { DriverHold, DriverHook } from "../l1-events/driver.js";
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
/** An order could not be rebuilt (malformed, or its bytes do not open). */
export const FORCED_ORDER_INGESTION_FAILED = "forced_order_ingestion_failed";

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
  run: RunDatabase;
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

/** Inserts `entries` if the follower is still at `view`. */
const insertAtView = (
  view: View,
  entries: readonly ForcedTransactionsDB.Entry[],
) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.gen(function* () {
        const check = viewValidQuery(postgresDialect, view);
        const valid = yield* sql.unsafe<{ valid: boolean }>(
          numbered(check.sql),
          check.params as never,
        );
        if (valid[0]?.valid !== true) return "stale" as const;
        yield* ForcedTransactionsDB.insertEntries(entries);
        return "inserted" as const;
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
  return Exit.isSuccess(exit)
    ? { kind: "entry", entry: exit.value }
    : {
        kind: "failed",
        detail: `${outcome.order.utxo.txHash}#${outcome.order.utxo.outputIndex.toString()}: ${failureText(exit)}`,
      };
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
    if (read.orders.length === 0) return undefined;
    const done = await options.run(ingestedOrders(read.orders));
    if (Exit.isFailure(done))
      return {
        reason: FORCED_ORDER_INGESTION_FAILED,
        detail: failureText(done),
      };
    const programMaterial = (): readonly UTxO[] => read.programMaterial;
    const entries: ForcedTransactionsDB.Entry[] = [];
    const pending: string[] = [];
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
      else failed.push(outcome.detail);
    }
    if (entries.length > 0) {
      const written = await options.run(insertAtView(view, entries));
      if (Exit.isFailure(written))
        failed.push(`insert: ${failureText(written)}`);
      else if (written.value === "inserted")
        options.log?.(
          `ingested ${entries.length.toString()} forced order(s) at ${view.point.slot.toString()}`,
        );
    }
    const hold = (reason: string, details: readonly string[]): DriverHold => ({
      reason,
      detail: details.join(" | ").slice(0, DETAIL_LIMIT),
    });
    if (failed.length > 0)
      return hold(FORCED_ORDER_INGESTION_FAILED, [
        ...failed,
        ...(pending.length === 0
          ? []
          : [`${pending.length.toString()} more await carriage`]),
      ]);
    if (pending.length > 0) return hold(FORCED_ORDER_CARRIAGE_PENDING, pending);
    return undefined;
  };
