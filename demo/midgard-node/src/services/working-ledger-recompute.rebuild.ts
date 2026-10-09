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
 *    ("dependent"), transitively; the rejections commit with the rebuild or
 *    not at all.
 *
 * Pending means unmarked: a row a processed base block includes is marked
 * by it (`mempoolInclusions.ts`) and stays in its table until the block
 * folds, so it is not replayed; the live own block's members are excluded
 * by id. A projected deposit whose output a surviving
 * transaction spends is `consumed`, any other `projected`.
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

import { DepositsDB, MempoolLedgerDB } from "../database/index.js";
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
  type Reject,
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
  reject?: Reject,
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
      const causes = [
        ...new Set(
          missing
            .map((key) => producer.get(key))
            .filter((id): id is string => id !== undefined && out.has(id)),
        ),
      ];
      reject?.(tx, causes.length > 0 ? "dependent" : "direct", causes);
      continue;
    }
    for (const key of tx.spent.map(hex)) ledger.delete(key);
    for (const row of tx.produced)
      ledger.set(hex(row[Columns.OUTREF]), Buffer.from(row[Columns.OUTPUT]));
  }
  return { ledger, changed };
};

/** Each rebuilt output's ledger ids, and the current rows the rebuild drops. */
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
    const dropped: Buffer[] = [];
    for (const row of working)
      if (ledger.has(hex(row.outref)))
        known.set(hex(row.outref), {
          txId: row.tx_id,
          sourceEventId: row.source_event_id,
        });
      else dropped.push(row.outref);
    return { known, dropped };
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
    const rejected = yield* closeRejections({
      pending,
      spread: (reject, done) => simulate(base, pending, done, reject).changed,
    });
    const { ledger } = simulate(base, pending, rejected);
    const { known, dropped } = yield* provenance(ledger);
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
    // A row the rebuild keeps is updated in place, so it keeps its
    // time_stamp_tz; a row new to the ledger takes the column default.
    for (let start = 0; start < dropped.length; start += 1_000)
      yield* sql`DELETE FROM mempool_ledger
        WHERE outref IN ${sql.in(dropped.slice(start, start + 1_000))}`;
    for (let start = 0; start < rows.length; start += 1_000)
      yield* sql`INSERT INTO mempool_ledger ${sql.insert(rows.slice(start, start + 1_000))}
        ON CONFLICT (outref) DO UPDATE SET tx_id = EXCLUDED.tx_id,
          output = EXCLUDED.output, address = EXCLUDED.address,
          source_event_id = EXCLUDED.source_event_id`;
    const rejectedTxIds = yield* recordRejections(rejected, input.codes);
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
    return { ledger, rejectedTxIds } satisfies WorkingLedgerRebuild;
  });
