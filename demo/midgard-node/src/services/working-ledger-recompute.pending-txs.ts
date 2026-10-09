import {
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  MempoolDB,
  MempoolInclusionsDB,
  MempoolLedgerDB,
  MempoolTxDeltasDB,
  ProcessedMempoolDB,
} from "../database/index.js";
import { DatabaseError } from "../database/utils/common.js";
import * as Ledger from "../database/utils/ledger.js";
import * as Tx from "../database/utils/tx.js";
import { resolveTxDeltaForCommit } from "../mpf/commit-rejection.js";

/**
 * The pending working ledger: every admitted transaction not yet in a
 * processed block, with its exact spends and outputs, and the helpers that
 * read and write `mempool_ledger` rows. Shared by every path that recomputes
 * the working ledger after its base changed (a landed-block rebase, a
 * state-queue correction).
 */

export const table = MempoolLedgerDB.tableName;

export const failure = (message: string, cause?: unknown) =>
  new DatabaseError({ table, message, cause });

export const hex = (value: Buffer) => value.toString("hex");

export const byteaArray = (values: readonly Buffer[]) =>
  values.map((value) => `\\x${value.toString("hex")}`);

export type LedgerRow = MempoolLedgerDB.EntryNoTimeStamp;

export type PendingTx = Readonly<{
  entry: Tx.EntryWithTimeStamp;
  source: "mempool" | "processed";
  spent: readonly Buffer[];
  produced: readonly LedgerRow[];
}>;

const producedRow = (
  txId: Buffer,
  entry: Ledger.MinimalEntry,
): Effect.Effect<LedgerRow, DatabaseError> =>
  Effect.try({
    try: () => ({
      [MempoolLedgerDB.Columns.TX_ID]: Buffer.from(txId),
      [MempoolLedgerDB.Columns.OUTREF]: Buffer.from(
        entry[Ledger.Columns.OUTREF],
      ),
      [MempoolLedgerDB.Columns.OUTPUT]: Buffer.from(
        entry[Ledger.Columns.OUTPUT],
      ),
      [MempoolLedgerDB.Columns.ADDRESS]: encodeMidgardAddressText(
        decodeMidgardTxOutput(entry[Ledger.Columns.OUTPUT]).address,
      ),
      [MempoolLedgerDB.Columns.SOURCE_EVENT_ID]: null,
    }),
    catch: (cause) =>
      failure("Pending transaction output is not a canonical ledger output", {
        txId: hex(txId),
        cause,
      }),
  });

/** Each pending transaction's admission sequence (`tx_admissions.arrival_seq`,
 * taken from a sequence as each submission arrives), by tx id. A pending
 * transaction with no admission row has none. */
const arrivalSeqs = (txIds: readonly Buffer[]) =>
  Effect.gen(function* () {
    if (txIds.length === 0) return new Map<string, bigint>();
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{ tx_id: Buffer; arrival_seq: string | bigint }>`
      SELECT tx_id, arrival_seq FROM tx_admissions
      WHERE tx_id IN ${sql.in(txIds)}`.pipe(
      Effect.mapError((cause) =>
        failure("Pending transactions' admission order cannot be read", cause),
      ),
    );
    return new Map(
      rows.map((row) => [hex(row.tx_id), BigInt(row.arrival_seq)] as const),
    );
  });

/**
 * `pending` in replay order. A transaction never comes before a pending
 * transaction whose output it spends; beyond that, the order is the time
 * stamp, then admission order (`arrival_seq`, a transaction with none
 * after those with one), then tx id. Each transaction, in that base order,
 * is placed after every pending producer of an output it spends that is
 * not placed yet.
 */
export const replayOrder = (
  pending: readonly PendingTx[],
  arrival: ReadonlyMap<string, bigint>,
): PendingTx[] => {
  const seqOf = (tx: PendingTx) => arrival.get(hex(tx.entry[Tx.Columns.TX_ID]));
  const base = [...pending].sort((left, right) => {
    const byTime =
      left.entry[Tx.Columns.TIMESTAMPTZ].getTime() -
      right.entry[Tx.Columns.TIMESTAMPTZ].getTime();
    if (byTime !== 0) return byTime;
    const l = seqOf(left);
    const r = seqOf(right);
    if (l !== r) {
      if (l === undefined) return 1;
      if (r === undefined) return -1;
      return l < r ? -1 : 1;
    }
    return Buffer.compare(
      left.entry[Tx.Columns.TX_ID],
      right.entry[Tx.Columns.TX_ID],
    );
  });
  const producerOf = new Map<string, PendingTx>();
  for (const tx of base)
    for (const row of tx.produced)
      producerOf.set(hex(row[MempoolLedgerDB.Columns.OUTREF]), tx);
  const placed = new Set<PendingTx>();
  const visiting = new Set<PendingTx>();
  const ordered: PendingTx[] = [];
  const place = (root: PendingTx) => {
    // Iterative depth-first: a transaction is placed once every pending
    // producer of what it spends is. A cycle cannot occur between valid
    // transactions (an output names its producer's id); `visiting` only
    // keeps the walk finite if one did.
    const stack: { tx: PendingTx; next: number }[] = [{ tx: root, next: 0 }];
    visiting.add(root);
    while (stack.length > 0) {
      const top = stack[stack.length - 1]!;
      if (top.next < top.tx.spent.length) {
        const producer = producerOf.get(hex(top.tx.spent[top.next]!));
        top.next += 1;
        if (
          producer !== undefined &&
          !placed.has(producer) &&
          !visiting.has(producer)
        ) {
          visiting.add(producer);
          stack.push({ tx: producer, next: 0 });
        }
        continue;
      }
      stack.pop();
      visiting.delete(top.tx);
      placed.add(top.tx);
      ordered.push(top.tx);
    }
  };
  for (const tx of base) if (!placed.has(tx)) place(tx);
  return ordered;
};

const loadPending = (undecodable: "fail" | "inert") =>
  Effect.gen(function* () {
    const mempool = yield* MempoolInclusionsDB.retrievePendingEntries(
      MempoolDB.tableName,
    );
    const processed = yield* MempoolInclusionsDB.retrievePendingEntries(
      ProcessedMempoolDB.tableName,
    );
    const seen = new Set<string>();
    const entries: {
      entry: Tx.EntryWithTimeStamp;
      source: PendingTx["source"];
    }[] = [];
    for (const [rows, source] of [
      [mempool, "mempool"],
      [processed, "processed"],
    ] as const)
      for (const entry of rows) {
        const id = hex(entry[Tx.Columns.TX_ID]);
        if (seen.has(id)) continue;
        seen.add(id);
        entries.push({ entry, source });
      }
    const txIds = entries.map(({ entry }) => entry[Tx.Columns.TX_ID]);
    const deltas = yield* MempoolTxDeltasDB.retrieveByTxIds(txIds);
    const pending: PendingTx[] = [];
    for (const { entry, source } of entries) {
      const txId = entry[Tx.Columns.TX_ID];
      const resolved = yield* resolveTxDeltaForCommit(
        entry,
        deltas.get(hex(txId)),
      );
      if (resolved._tag === "Rejected") {
        if (undecodable === "inert") {
          pending.push({ entry, source, spent: [], produced: [] });
          continue;
        }
        return yield* Effect.fail(
          failure(
            "A pending transaction cannot be decoded, so its dependence on reopened state cannot be decided",
            hex(txId),
          ),
        );
      }
      const produced: LedgerRow[] = [];
      for (const output of resolved.produced)
        produced.push(yield* producedRow(txId, output));
      pending.push({ entry, source, spent: resolved.spent, produced });
    }
    return replayOrder(pending, yield* arrivalSeqs(txIds));
  });

/** Every pending (unmarked) transaction whose ledger effects are in
 * `mempool_ledger`, in replay order (`replayOrder`), with its exact spends
 * and outputs. A
 * row a block's inclusion mark holds is in that block, not pending. Fails on
 * a transaction that cannot be decoded. */
export const loadPendingTxs = loadPending("fail");

/** `loadPendingTxs`, with a transaction that cannot be decoded kept as one
 * that spends and produces nothing. */
export const loadPendingTxsKeepingUndecodable = loadPending("inert");
