/**
 * The working-ledger recompute (plan §7.3, P4): `mempool_ledger` rebuilt
 * from a base ledger, in this order:
 *
 * 1. the base: the processed landed tip's ledger and this node's live own
 *    block's delta (the caller's);
 * 2. the deposits projected into the working ledger outside any block;
 * 3. every pending transaction, in admission order, re-simulated on what
 *    came before it. A transaction whose input is gone is rejected
 *    ("direct"), with every transaction that spends a rejected one's output
 *    ("dependent") and every co-member of a batch it was accepted in
 *    ("batch"), transitively; the rejections commit with the rebuild or not
 *    at all.
 *
 * A pending transaction a foreign base block included is dropped from the
 * pending sets (it is in the base); one this node's own base block included
 * is left to that block's finalization. Either is settled by the base: a
 * batch it was accepted in with a rejected transaction rejects the batch's
 * other pending members ("batch") and is not refused for it. A projected deposit whose output a
 * surviving transaction spends is `consumed`, any other `projected`.
 *
 * Runs inside the caller's transaction.
 */
import {
  decodeMidgardSpendInputItem,
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  DepositsDB,
  MempoolDB,
  MempoolLedgerDB,
  MempoolTxDeltasDB,
  ProcessedMempoolDB,
} from "../database/index.js";
import * as Tx from "../database/utils/tx.js";
import {
  failure,
  hex,
  type LedgerRow,
  loadPendingTxs,
  type PendingTx,
} from "./working-ledger-recompute.pending-txs.js";
import {
  closeRejections,
  recordRejections,
  type RejectionCodes,
  type Rejections,
  txIdHex,
} from "./working-ledger-recompute.reject-closure.js";

const Columns = MempoolLedgerDB.Columns;

type Provenance = Readonly<{ txId: Buffer; sourceEventId: Buffer | null }>;

/** Replays `pending` on `base`, skipping `rejected`; rejects what cannot apply. */
const simulate = (
  base: ReadonlyMap<string, Buffer>,
  pending: readonly PendingTx[],
  rejected: Rejections,
  reject?: (tx: PendingTx, reason: "direct" | "dependent") => void,
) => {
  const ledger = new Map(base);
  const producer = new Map<string, string>();
  for (const tx of pending)
    for (const row of tx.produced)
      producer.set(hex(row[Columns.OUTREF]), txIdHex(tx));
  const out = new Set(rejected.keys());
  let changed = false;
  for (const tx of pending) {
    const id = txIdHex(tx);
    if (out.has(id)) continue;
    const missing = tx.spent.map(hex).filter((key) => !ledger.has(key));
    if (missing.length > 0) {
      out.add(id);
      changed = true;
      reject?.(
        tx,
        missing.some((key) => out.has(producer.get(key) ?? ""))
          ? "dependent"
          : "direct",
      );
      continue;
    }
    for (const key of tx.spent.map(hex)) ledger.delete(key);
    for (const row of tx.produced)
      ledger.set(hex(row[Columns.OUTREF]), Buffer.from(row[Columns.OUTPUT]));
  }
  return { ledger, changed };
};

const provenance = (ledger: ReadonlyMap<string, Buffer>) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const known = new Map<string, Provenance>();
    const confirmed = yield* sql<{ outref: Buffer; tx_id: Buffer }>`
      SELECT outref, tx_id FROM confirmed_ledger`;
    for (const row of confirmed)
      known.set(hex(row.outref), { txId: row.tx_id, sourceEventId: null });
    const working = yield* sql<{
      outref: Buffer;
      tx_id: Buffer;
      source_event_id: Buffer | null;
    }>`SELECT outref, tx_id, source_event_id FROM mempool_ledger`;
    for (const row of working)
      if (ledger.has(hex(row.outref)))
        known.set(hex(row.outref), {
          txId: row.tx_id,
          sourceEventId: row.source_event_id,
        });
    return known;
  });

const ledgerRow = (
  outRef: string,
  output: Buffer,
  known: Provenance | undefined,
): Effect.Effect<LedgerRow, ReturnType<typeof failure>> =>
  Effect.try({
    try: () => {
      const key = Buffer.from(outRef, "hex");
      return {
        [Columns.TX_ID]:
          known?.txId ?? Buffer.from(decodeMidgardSpendInputItem(key).txId),
        [Columns.OUTREF]: key,
        [Columns.OUTPUT]: output,
        [Columns.ADDRESS]: encodeMidgardAddressText(
          decodeMidgardTxOutput(output).address,
        ),
        [Columns.SOURCE_EVENT_ID]: known?.sourceEventId ?? null,
      };
    },
    catch: (cause) =>
      failure("A recomputed ledger output is not canonical", {
        outRef,
        cause,
      }),
  });

export type WorkingLedgerRebuild = Readonly<{
  /** The rebuilt working ledger, by hex outref. */
  ledger: ReadonlyMap<string, Buffer>;
  rejectedTxIds: readonly Buffer[];
  droppedTxIds: readonly Buffer[];
}>;

export const rebuildWorkingLedger = (input: {
  /** The base ledger by hex outref: the processed tip and live own block. */
  readonly base: ReadonlyMap<string, Buffer>;
  /** Deposit outputs the base holds, with their ledger ids and event ids. */
  readonly baseDeposits: ReadonlyMap<string, Provenance>;
  /** Pending transactions a foreign base block included (hex ids). */
  readonly includedByForeign: ReadonlySet<string>;
  /** Pending transactions this node's own base blocks included (hex ids). */
  readonly includedByOwn: ReadonlySet<string>;
  readonly codes: RejectionCodes;
}) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const all = yield* loadPendingTxs;
    const dropped = all.filter((tx) =>
      input.includedByForeign.has(txIdHex(tx)),
    );
    const pending = all.filter(
      (tx) =>
        !input.includedByForeign.has(txIdHex(tx)) &&
        !input.includedByOwn.has(txIdHex(tx)),
    );
    const base = new Map(input.base);
    const deposits = new Map(input.baseDeposits);
    const projected = yield* sql<DepositsDB.Entry>`SELECT * FROM deposits_utxos
      WHERE projected_header_hash IS NULL
        AND status IN ('projected', 'consumed')`;
    const outside: Buffer[] = [];
    for (const row of projected) {
      const entry = yield* DepositsDB.toMempoolLedgerEntry(row);
      const key = hex(entry[Columns.OUTREF]);
      base.set(key, Buffer.from(entry[Columns.OUTPUT]));
      deposits.set(key, {
        txId: entry[Columns.TX_ID],
        sourceEventId: entry.source_event_id,
      });
      outside.push(entry.source_event_id);
    }
    // A batch co-member a base block includes is settled by that block.
    const settled = new Set([
      ...input.includedByForeign,
      ...input.includedByOwn,
    ]);
    const rejected = yield* closeRejections({
      pending,
      spread: (reject, done) => simulate(base, pending, done, reject).changed,
      settled,
    });
    const { ledger } = simulate(base, pending, rejected);
    const known = yield* provenance(ledger);
    for (const tx of pending)
      for (const row of tx.produced)
        known.set(hex(row[Columns.OUTREF]), {
          txId: row[Columns.TX_ID],
          sourceEventId: null,
        });
    for (const [key, value] of deposits) known.set(key, value);
    const rows: LedgerRow[] = [];
    for (const [outRef, output] of ledger)
      rows.push(yield* ledgerRow(outRef, output, known.get(outRef)));
    yield* sql`DELETE FROM mempool_ledger`;
    for (let start = 0; start < rows.length; start += 1_000)
      yield* sql`INSERT INTO mempool_ledger ${sql.insert(rows.slice(start, start + 1_000))}`;
    const droppedIds = dropped.map((tx) => tx.entry[Tx.Columns.TX_ID]);
    if (droppedIds.length > 0) {
      yield* MempoolDB.clearTxs(droppedIds);
      yield* ProcessedMempoolDB.clearTxs(droppedIds);
      yield* MempoolTxDeltasDB.clearTxs(droppedIds);
    }
    const rejectedTxIds = yield* recordRejections(
      rejected,
      input.codes,
      [...settled].map((id) => Buffer.from(id, "hex")),
    );
    // A deposit outside any block is consumed exactly when a surviving
    // transaction spent its output.
    const present = new Set(
      rows
        .map((row) => row[Columns.SOURCE_EVENT_ID])
        .filter((id): id is Buffer => id !== null)
        .map(hex),
    );
    yield* DepositsDB.unconsumeByEventIds(
      outside.filter((id) => present.has(hex(id))),
    );
    yield* DepositsDB.markConsumedByEventIds(
      outside.filter((id) => !present.has(hex(id))),
    );
    return {
      ledger,
      rejectedTxIds,
      droppedTxIds: droppedIds,
    } satisfies WorkingLedgerRebuild;
  });
