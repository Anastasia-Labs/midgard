/**
 * The landed-block fork simulator's mempool (N3): pending transactions
 * admitted the way the node's admission leaves them (mempool row, delta,
 * working-ledger outputs, one acceptance receipt per accepted batch), and
 * the model of what a working-ledger rebuild makes of them.
 *
 * The model replays the pending transactions in admission order on a base
 * ledger. One a base block includes leaves the pending set: a foreign
 * block's is dropped, an own block's waits for that block's merge. Of the
 * rest, one whose input is gone is rejected ("direct"), one that spends a
 * rejected transaction's output with it ("dependent"), and every pending
 * co-member of an acceptance receipt that holds a rejected transaction
 * ("batch"), transitively; a co-member a base block includes is settled by
 * it. A rejection is final, and reverses the receipts it touches.
 */
import {
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  MempoolDB,
  MempoolTxDeltasDB,
  TxUtils,
} from "../../src/database/index.js";
import type * as Ledger from "../../src/database/utils/ledger.js";
import { withHistoryWrite } from "../../src/services/event-history-producer.js";

const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");

export type SimPendingTx = Readonly<{
  id: Buffer;
  spent: readonly Buffer[];
  produced: readonly Ledger.MinimalEntry[];
  at: Date;
}>;

export type SimRejection = "direct" | "dependent" | "batch";

/** One accepted batch's receipt: its transaction ids (hex). */
export type SimReceipt = { ids: readonly string[]; reversed: boolean };

export type SimMempool = {
  /** Pending, in admission order (own-included ones until their merge). */
  survivors: SimPendingTx[];
  rejected: Map<string, SimRejection>;
  receipts: SimReceipt[];
  admitted: number;
  poolUsed: number;
};

export const newSimMempool = (): SimMempool => ({
  survivors: [],
  rejected: new Map(),
  receipts: [],
  admitted: 0,
  poolUsed: 0,
});

/** What the base blocks include, as hex transaction ids. */
export type SimIncluded = Readonly<{
  foreign: ReadonlySet<string>;
  own: ReadonlySet<string>;
}>;

export type SimSettlement = Readonly<{
  ledger: Map<string, Buffer>;
  newly: readonly (readonly [string, SimRejection])[];
  dropped: readonly string[];
  /** A batch was rejected around a co-member a base block settled. */
  batchSettled: boolean;
}>;

/**
 * Rebuilds the model's working ledger on `base`; moves the pending
 * transactions that cannot apply to the rejections and drops the ones a
 * foreign base block includes. A string is a model failure: a batch the
 * rebuild could neither reject nor settle.
 */
export const settleMempool = (
  mempool: SimMempool,
  base: ReadonlyMap<string, Buffer>,
  included: SimIncluded,
): SimSettlement | string => {
  const settled = new Set([...included.foreign, ...included.own]);
  const pending = mempool.survivors.filter((tx) => !settled.has(hex(tx.id)));
  const pendingIds = new Set(pending.map((tx) => hex(tx.id)));
  const rejected = new Map<string, SimRejection>();
  const simulate = (record: boolean) => {
    const ledger = new Map(base);
    const producer = new Map<string, string>();
    for (const tx of pending)
      for (const entry of tx.produced)
        producer.set(hex(entry.outref), hex(tx.id));
    let changed = false;
    for (const tx of pending) {
      const id = hex(tx.id);
      if (rejected.has(id)) continue;
      const missing = tx.spent.map(hex).filter((key) => !ledger.has(key));
      if (missing.length > 0) {
        if (!record) continue;
        rejected.set(
          id,
          missing.some((key) => rejected.has(producer.get(key) ?? ""))
            ? "dependent"
            : "direct",
        );
        changed = true;
        continue;
      }
      for (const key of tx.spent.map(hex)) ledger.delete(key);
      for (const entry of tx.produced)
        ledger.set(hex(entry.outref), Buffer.from(entry.output));
    }
    return { ledger, changed };
  };
  let batchSettled = false;
  for (let widened = true; widened; ) {
    widened = false;
    while (simulate(true).changed);
    for (const receipt of mempool.receipts) {
      if (receipt.reversed || !receipt.ids.some((id) => rejected.has(id)))
        continue;
      for (const id of receipt.ids) {
        if (rejected.has(id)) continue;
        if (settled.has(id)) {
          batchSettled = true;
          continue;
        }
        if (!pendingIds.has(id))
          return `model: batch ${receipt.ids.join(",")} holds ${id}, neither pending nor settled`;
        rejected.set(id, "batch");
        widened = true;
      }
    }
  }
  const { ledger } = simulate(false);
  for (const receipt of mempool.receipts)
    if (!receipt.reversed && receipt.ids.some((id) => rejected.has(id)))
      receipt.reversed = true;
  const dropped = mempool.survivors
    .map((tx) => hex(tx.id))
    .filter((id) => included.foreign.has(id));
  const gone = new Set([...dropped, ...rejected.keys()]);
  mempool.survivors = mempool.survivors.filter((tx) => !gone.has(hex(tx.id)));
  for (const [id, reason] of rejected) mempool.rejected.set(id, reason);
  return { ledger, newly: [...rejected], dropped, batchSettled };
};

/** Admits `txs` the way the node's admission leaves them: mempool, delta, working ledger. */
export const admitPending = (txs: readonly SimPendingTx[]) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* withHistoryWrite(
      Effect.gen(function* () {
        for (const tx of txs) {
          yield* TxUtils.insertEntry(MempoolDB.tableName, {
            [TxUtils.Columns.TX_ID]: tx.id,
            [TxUtils.Columns.TX]: Buffer.from("a1".repeat(16), "hex"),
            [TxUtils.Columns.TIMESTAMPTZ]: tx.at,
          });
          for (const outRef of tx.spent)
            yield* sql`DELETE FROM mempool_ledger WHERE outref = ${outRef}`;
          for (const entry of tx.produced)
            yield* sql`INSERT INTO mempool_ledger ${sql.insert({
              tx_id: tx.id,
              outref: entry.outref,
              output: entry.output,
              address: encodeMidgardAddressText(
                decodeMidgardTxOutput(entry.output).address,
              ),
              source_event_id: null,
            })}`;
        }
        yield* MempoolTxDeltasDB.upsertMany(
          txs.map((tx) => ({
            txId: tx.id,
            spent: tx.spent,
            produced: tx.produced,
          })),
        );
      }),
    );
  });

const BINDING = Buffer.alloc(32, 0x5a);
const DIGEST = Buffer.alloc(32, 0x5b);

/** The history cursor acceptance receipts hang off. */
export const insertSimCursor = Effect.flatMap(
  SqlClient.SqlClient,
  (sql) => sql`INSERT INTO event_history_cursor (binding_digest, manifest_id,
      origin_receipt, origin_receipt_digest, anchor_hash, anchor_slot,
      anchor_height, anchor_snapshot_digest, head_hash, head_slot,
      head_height, head_application_revision, snapshot_digest, revision,
      addresses)
    VALUES (${BINDING}, ${Buffer.alloc(32, 0xde)}, 'origin', ${DIGEST},
      ${DIGEST}, 0, 0, ${DIGEST}, ${DIGEST}, 0, 0, NULL, ${DIGEST}, 0,
      '[]'::jsonb)`,
);

/** One unreversed acceptance receipt for the batch `ids`. */
export const insertSimReceipt = (ids: readonly Buffer[]) =>
  withHistoryWrite(
    Effect.flatMap(SqlClient.SqlClient, (sql) => {
      const array = (sql as unknown as { array: (v: string[]) => unknown })
        .array;
      return sql`INSERT INTO event_history_l2_ledger_receipts (binding_digest,
          owner_generation, checkpoint_revision, head_hash, snapshot_digest,
          tx_ids, reference_outrefs, ledger_before, reference_before,
          deposits_before, payloads_before)
        VALUES (${BINDING}, 0, 0, ${DIGEST}, ${DIGEST},
          ${array(ids.map((id) => `\\x${hex(id)}`)) as never}::bytea[],
          '{}'::bytea[], '[]'::jsonb, '[]'::jsonb, '[]'::jsonb, '[]'::jsonb)`;
    }),
  );
