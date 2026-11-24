/**
 * The ledgers the landed-block rows describe: `confirmed_ledger` at the
 * frontier, then each processed row's net delta in queue order. Every
 * landed header's post-state is `confirmed_ledger` plus the deltas of the
 * processed rows from the frontier to it; nothing else is kept.
 */
import {
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect } from "effect";

import { ConfirmedLedgerDB, DepositsDB } from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import * as Ledger from "../database/utils/ledger.js";
import {
  Frontier,
  type HeaderRoot,
  type LandedBlockRow,
  tableName,
} from "./store.js";

/** A ledger by hex outref. */
export type LedgerMap = Map<string, Buffer>;

export const ledgerMap = (entries: readonly Ledger.MinimalEntry[]): LedgerMap =>
  new Map(
    entries.map((entry) => [
      Buffer.from(entry[Ledger.Columns.OUTREF]).toString("hex"),
      Buffer.from(entry[Ledger.Columns.OUTPUT]),
    ]),
  );

/** `ledger` with `row`'s delta applied, in place. */
export const applyDelta = (
  ledger: LedgerMap,
  row: Pick<LandedBlockRow, "spent" | "produced">,
): LedgerMap => {
  for (const outRef of row.spent) ledger.delete(outRef.toString("hex"));
  for (const entry of row.produced)
    ledger.set(entry.outref.toString("hex"), Buffer.from(entry.output));
  return ledger;
};

/**
 * The processed rows that chain from `from` by parent links, in order. The
 * unique processed-parent index makes the chain a path.
 */
export const processedChain = (
  rows: readonly LandedBlockRow[],
  from: string,
): LandedBlockRow[] => {
  const byParent = new Map(
    rows
      .filter((row) => row.state === "processed")
      .map((row) => [row.parentHeaderHash, row] as const),
  );
  const chain: LandedBlockRow[] = [];
  const seen = new Set<string>();
  for (
    let next = byParent.get(from);
    next !== undefined && !seen.has(next.headerHash);
    next = byParent.get(next.headerHash)
  ) {
    seen.add(next.headerHash);
    chain.push(next);
  }
  return chain;
};

/**
 * The full `mempool_ledger` / `confirmed_ledger` rows of outputs: the
 * transaction id of a deposit's output is its ledger id, every other
 * output's is the id its outref names.
 */
export const ledgerRows = (
  entries: readonly Ledger.MinimalEntry[],
  depositTxIds: ReadonlyMap<string, Buffer>,
) =>
  Effect.try({
    try: () =>
      entries.map(
        (entry): Ledger.EntryNoTimeStamp => ({
          [Ledger.Columns.TX_ID]:
            depositTxIds.get(entry.outref.toString("hex")) ??
            Buffer.from(decodeMidgardSpendInputItem(entry.outref).txId),
          [Ledger.Columns.OUTREF]: Buffer.from(entry.outref),
          [Ledger.Columns.OUTPUT]: Buffer.from(entry.output),
          [Ledger.Columns.ADDRESS]: encodeMidgardAddressText(
            decodeMidgardTxOutput(entry.output).address,
          ),
        }),
      ),
    catch: (cause) =>
      new DatabaseError({
        table: tableName,
        message: "A landed block's output is not a canonical ledger output",
        cause,
      }),
  });

/** The deposits `ids` name: ledger outref (hex) to ledger tx id and event id. */
export const depositOutputs = (ids: readonly Buffer[]) =>
  Effect.gen(function* () {
    const byOutRef = new Map<string, { txId: Buffer; eventId: Buffer }>();
    if (ids.length === 0) return byOutRef;
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const rows = yield* sql<DepositsDB.Entry>`SELECT * FROM deposits_utxos
      WHERE event_id = ANY(${pg.array(ids.map((id) => `\\x${id.toString("hex")}`))}::bytea[])`;
    for (const row of rows) {
      const entry = yield* DepositsDB.toLedgerEntry(row);
      byOutRef.set(entry[Ledger.Columns.OUTREF].toString("hex"), {
        txId: Buffer.from(row[DepositsDB.Columns.LEDGER_TX_ID]),
        eventId: Buffer.from(row[DepositsDB.Columns.ID]),
      });
    }
    return byOutRef;
  }).pipe(
    Effect.mapError(
      (cause) =>
        new DatabaseError({
          table: DepositsDB.tableName,
          message: "Failed to read landed deposits",
          cause,
        }),
    ),
  );

/**
 * Runs `read` on one snapshot: a `REPEATABLE READ, READ ONLY` transaction
 * of its own. A fold or unfold writes `confirmed_ledger`, the frontier and
 * the landed rows in one transaction, so what `read` reads of them is one
 * state a fold left: the entries are the post-state of the header the
 * frontier names, and the rows chain from it. Read statement by statement,
 * a fold, or an unfold on a rollback, committing between two of the reads
 * would pair a frontier with a ledger that is not its post-state, which
 * a root check would take for a divergence.
 */
export const onOneSnapshot = <A, E, R>(read: Effect.Effect<A, E, R>) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql.withTransaction(
      Effect.zipRight(
        sql`SET TRANSACTION ISOLATION LEVEL REPEATABLE READ, READ ONLY`,
        read,
      ),
    );
  });

/** The frontier and the processed chain after it. */
export type LandedChain = Readonly<{
  frontier: HeaderRoot;
  chain: readonly LandedBlockRow[];
}>;

/** The ledger at the frontier and the processed chain after it. */
export type LandedLedger = LandedChain &
  Readonly<{ confirmed: readonly Ledger.EntryNoTimeStamp[] }>;

export const landedLedger = (rows: readonly LandedBlockRow[]) =>
  Effect.gen(function* () {
    const frontier = yield* Frontier.retrieve;
    if (frontier === undefined) return undefined;
    const confirmed = yield* ConfirmedLedgerDB.retrieve;
    return {
      frontier,
      confirmed,
      chain: processedChain(rows, frontier.headerHash),
    } satisfies LandedLedger;
  });

/** The frontier and the processed chain after it, reading no ledger row. */
export const landedChain = (rows: readonly LandedBlockRow[]) =>
  Frontier.retrieve.pipe(
    Effect.map((frontier): LandedChain | undefined =>
      frontier === undefined
        ? undefined
        : { frontier, chain: processedChain(rows, frontier.headerHash) },
    ),
  );

/**
 * Where a walk from `headerHash` starts on the landed chain: its post-state
 * root and the chain rows through it (none at the frontier), or undefined
 * off the chain. It reads no `confirmed_ledger` row.
 */
export const landedStart = (
  rows: readonly LandedBlockRow[],
  headerHash: string,
) =>
  landedChain(rows).pipe(
    Effect.map((landed) => {
      if (landed === undefined) return undefined;
      if (landed.frontier.headerHash === headerHash)
        return { root: landed.frontier.utxosRoot, through: [] };
      const index = landed.chain.findIndex(
        (row) => row.headerHash === headerHash,
      );
      return index < 0
        ? undefined
        : {
            root: landed.chain[index]!.utxosRoot,
            through: landed.chain.slice(0, index + 1),
          };
    }),
  );
