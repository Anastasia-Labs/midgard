/**
 * The working-ledger recompute (plan §7.3, P4): `mempool_ledger` rebuilt
 * at the keys it can change, from the ledger at the frontier, in this order:
 *
 * 1. the base: `confirmed_ledger`, then each step's delta in order (the
 *    processed landed chain and this node's live own block, the caller's);
 * 2. the deposits projected into the working ledger outside any block;
 * 3. every pending transaction, in admission order, re-simulated on what
 *    came before it. A transaction an input or reference input of which is
 *    gone is rejected ("direct"), with every transaction that spends or
 *    reads a rejected one's output ("dependent"), transitively; the
 *    rejections commit with the rebuild or not at all.
 *
 * Pending means unmarked: a row a processed base block includes is marked
 * by it (`mempoolInclusions.ts`) and stays in its table until the block
 * folds, so it is not replayed; the live own block's members are excluded
 * by id. A projected deposit whose output a surviving
 * transaction spends is `consumed`, any other `projected`.
 *
 * Only the keys `K` are read and written: the caller's keys (every output
 * the working ledger may hold that the target does not: a removed block's,
 * a dead journal's, a transaction a processed block included), each step's
 * delta, the base deposits, every loaded pending transaction's spends,
 * reads and outputs, and the deposits projected outside any block. Outside
 * `K`, `mempool_ledger` equals `confirmed_ledger` before the rebuild and
 * after it, so the rows there stay as they are.
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
import type * as Ledger from "../database/utils/ledger.js";
import {
  failure,
  hex,
  type LedgerRow,
  loadPendingTxs,
  type PendingTx,
  readsOf,
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

/** Replays `pending` on `base`, skipping `rejected`; rejects what cannot
 * apply: a transaction an output it spends or reads by reference is not in
 * the ledger before it. */
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
    const missing = readsOf(tx)
      .map(hex)
      .filter((key) => !ledger.has(key));
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

type WorkingRow = Readonly<{
  outref: Buffer;
  tx_id: Buffer;
  output: Buffer;
  address: string;
  source_event_id: Buffer | null;
}>;

/** The rows `read` returns at the hex outrefs `keys`, by hex outref. */
const rowsAt = <A extends { readonly outref: Buffer }>(
  read: (outRefs: readonly Buffer[]) => Effect.Effect<readonly A[], unknown>,
  keys: ReadonlySet<string>,
) =>
  Effect.gen(function* () {
    const outRefs = [...keys].map((key) => Buffer.from(key, "hex"));
    const rows = new Map<string, A>();
    for (let start = 0; start < outRefs.length; start += 1_000)
      for (const row of yield* read(outRefs.slice(start, start + 1_000)))
        rows.set(hex(row.outref), row);
    return rows;
  });

const sameRow = (left: LedgerRow, right: WorkingRow | undefined) => {
  const source = left[Columns.SOURCE_EVENT_ID];
  return (
    right !== undefined &&
    left[Columns.TX_ID].equals(right.tx_id) &&
    left[Columns.OUTPUT].equals(right.output) &&
    left[Columns.ADDRESS] === right.address &&
    (source === null
      ? right.source_event_id === null
      : right.source_event_id !== null && source.equals(right.source_event_id))
  );
};

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
  /** The rebuilt working ledger at the keys it read, by hex outref. */
  ledger: ReadonlyMap<string, Buffer>;
  /** The keys it read and wrote (hex outrefs): `K`. */
  keys: ReadonlySet<string>;
  rejectedTxIds: readonly Buffer[];
}>;

/** A step's delta: the outputs it spends and the ones it produces. */
export type StepDelta = Readonly<{
  spent: readonly Buffer[];
  produced: readonly Ledger.MinimalEntry[];
}>;

export const rebuildWorkingLedger = (input: {
  /** The steps after the frontier, in order: the processed landed chain
   * and this node's live own block. */
  readonly steps: readonly StepDelta[];
  /** Further outputs (hex outrefs) the working ledger may hold that the
   * target does not. */
  readonly keys: ReadonlySet<string>;
  /** Deposit outputs the steps hold, with their ledger ids and event ids. */
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
    const keys = new Set(input.keys);
    for (const step of input.steps) {
      for (const outRef of step.spent) keys.add(hex(outRef));
      for (const entry of step.produced) keys.add(hex(entry.outref));
    }
    for (const key of input.baseDeposits.keys()) keys.add(key);
    for (const tx of all) {
      for (const outRef of readsOf(tx)) keys.add(hex(outRef));
      for (const row of tx.produced) keys.add(hex(row[Columns.OUTREF]));
    }
    const deposits = new Map(input.baseDeposits);
    const projected = yield* sql<DepositsDB.Entry>`SELECT * FROM deposits_utxos
      WHERE projected_header_hash IS NULL
        AND status IN ('projected', 'consumed')`;
    const outside: Buffer[] = [];
    const outsideOutputs = new Map<string, Buffer>();
    for (const row of projected) {
      const entry = yield* DepositsDB.toMempoolLedgerEntry(row);
      const key = hex(entry[Columns.OUTREF]);
      keys.add(key);
      outsideOutputs.set(key, Buffer.from(entry[Columns.OUTPUT]));
      deposits.set(key, {
        txId: entry[Columns.TX_ID],
        sourceEventId: entry.source_event_id,
      });
      outside.push(entry.source_event_id);
    }
    const confirmed = yield* rowsAt(
      (outRefs) => sql<{ outref: Buffer; output: Buffer; tx_id: Buffer }>`
        SELECT outref, output, tx_id FROM confirmed_ledger
        WHERE outref IN ${sql.in(outRefs)}`,
      keys,
    );
    const base = new Map<string, Buffer>();
    for (const [key, row] of confirmed) base.set(key, row.output);
    for (const step of input.steps) {
      for (const outRef of step.spent) base.delete(hex(outRef));
      for (const entry of step.produced)
        base.set(hex(entry.outref), Buffer.from(entry.output));
    }
    for (const [key, output] of outsideOutputs) base.set(key, output);
    const rejected = yield* closeRejections({
      pending,
      spread: (reject, done) => simulate(base, pending, done, reject).changed,
    });
    const { ledger } = simulate(base, pending, rejected);
    const working = yield* rowsAt(
      (outRefs) => sql<WorkingRow>`
        SELECT outref, tx_id, output, address, source_event_id
        FROM mempool_ledger WHERE outref IN ${sql.in(outRefs)}`,
      keys,
    );
    // An output's ledger ids: its deposit's, else its pending producer's,
    // else the working row's it keeps, else the confirmed row's.
    const known = new Map<string, Provenance>();
    for (const [key, row] of confirmed)
      known.set(key, { txId: row.tx_id, sourceEventId: null });
    const dropped: Buffer[] = [];
    for (const [key, row] of working)
      if (ledger.has(key))
        known.set(key, { txId: row.tx_id, sourceEventId: row.source_event_id });
      else dropped.push(row.outref);
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
    // A row the rebuild keeps unchanged is not written, and a changed one is
    // updated in place: both keep their time_stamp_tz; a row new to the
    // ledger takes the column default.
    const changed = rows.filter(
      (row) => !sameRow(row, working.get(hex(row[Columns.OUTREF]))),
    );
    for (let start = 0; start < dropped.length; start += 1_000)
      yield* sql`DELETE FROM mempool_ledger
        WHERE outref IN ${sql.in(dropped.slice(start, start + 1_000))}`;
    for (let start = 0; start < changed.length; start += 1_000)
      yield* sql`INSERT INTO mempool_ledger ${sql.insert(changed.slice(start, start + 1_000))}
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
    return { ledger, keys, rejectedTxIds } satisfies WorkingLedgerRebuild;
  });
