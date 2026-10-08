/**
 * The landed-block fork simulator's mempool (N3): pending transactions
 * admitted the way the node's admission leaves them (mempool row, delta,
 * working-ledger outputs), and the model of what a working-ledger rebuild
 * makes of them.
 *
 * The model replays the pending transactions in admission order on a base
 * ledger. One a block on the base's lineage includes is settled, not
 * pending: its row stays (marked) until that block's fold is final (its
 * retained fold pruned), then leaves; a rollback that takes the block off
 * the lineage makes it pending again. Of the rest, one whose input is gone
 * is rejected ("direct"), and one that spends a rejected transaction's
 * output with it ("dependent"), transitively. A rejection is final unless
 * the node revives an own block: that deletes the rejections of its members
 * and the dependent rejections every recorded cause of which is deleted,
 * transitively.
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
import { withFollowerWrite } from "../../src/services/follower-write-gate.js";

const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");

export type SimPendingTx = Readonly<{
  id: Buffer;
  spent: readonly Buffer[];
  produced: readonly Ledger.MinimalEntry[];
  at: Date;
  /** The row's transaction bytes (a filler unless given). */
  cbor?: Buffer;
}>;

export type SimRejection = "direct" | "dependent";

export type SimMempool = {
  /** Every row, in admission order: pending, or included by a block that has not folded. */
  survivors: SimPendingTx[];
  /** Every transaction admitted, by hex id. */
  txs: Map<string, SimPendingTx>;
  rejected: Map<string, SimRejection>;
  /** The rejected transactions a dependent rejection follows from. */
  causes: Map<string, readonly string[]>;
  admitted: number;
  poolUsed: number;
};

export const newSimMempool = (): SimMempool => ({
  survivors: [],
  txs: new Map(),
  rejected: new Map(),
  causes: new Map(),
  admitted: 0,
  poolUsed: 0,
});

/**
 * The settlement oracle, as hex transaction ids: what the blocks on the
 * base's lineage that the node processed include (the record a rollback
 * rewinds and a fold keeps), and the live own block's members.
 */
export type SimIncluded = Readonly<{
  settled: ReadonlySet<string>;
  /** Settled by a block whose fold is final (released): those rows are gone. */
  released: ReadonlySet<string>;
}>;

export type SimSettlement = Readonly<{
  ledger: Map<string, Buffer>;
  newly: readonly (readonly [string, SimRejection])[];
}>;

/**
 * Rebuilds the model's working ledger on `base`; moves the pending
 * transactions that cannot apply to the rejections and drops the ones a
 * block whose fold is final includes.
 */
export const settleMempool = (
  mempool: SimMempool,
  base: ReadonlyMap<string, Buffer>,
  included: SimIncluded,
): SimSettlement => {
  const { settled } = included;
  const pending = mempool.survivors.filter((tx) => !settled.has(hex(tx.id)));
  const rejected = new Map<string, SimRejection>();
  const causes = new Map<string, readonly string[]>();
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
        const after = [
          ...new Set(
            missing
              .map((key) => producer.get(key) ?? "")
              .filter((producerId) => rejected.has(producerId)),
          ),
        ];
        rejected.set(id, after.length > 0 ? "dependent" : "direct");
        if (after.length > 0) causes.set(id, after);
        changed = true;
        continue;
      }
      for (const key of tx.spent.map(hex)) ledger.delete(key);
      for (const entry of tx.produced)
        ledger.set(hex(entry.outref), Buffer.from(entry.output));
    }
    return { ledger, changed };
  };
  while (simulate(true).changed);
  const { ledger } = simulate(false);
  mempool.survivors = mempool.survivors.filter(
    (tx) => !rejected.has(hex(tx.id)) && !included.released.has(hex(tx.id)),
  );
  for (const [id, reason] of rejected) mempool.rejected.set(id, reason);
  for (const [id, after] of causes) mempool.causes.set(id, after);
  return { ledger, newly: [...rejected] };
};

/**
 * A revived own block's members (hex ids): their rejections go, and so does
 * every rejection all of whose recorded causes went, transitively. Returns
 * the members whose rejection went.
 */
export const clearRevivedRejections = (
  mempool: SimMempool,
  members: readonly string[],
) => {
  const unrejected = members.filter((id) => mempool.rejected.has(id));
  const cleared = new Set(members);
  for (let widened = true; widened; ) {
    widened = false;
    for (const [id, after] of mempool.causes)
      if (!cleared.has(id) && after.every((cause) => cleared.has(cause))) {
        cleared.add(id);
        widened = true;
      }
  }
  for (const id of cleared) {
    mempool.rejected.delete(id);
    mempool.causes.delete(id);
  }
  return unrejected;
};

/**
 * A disposed own block's members (hex ids) are pending again. One with no
 * row (it was rejected, then a revival deleted its rejection) is restored
 * from the journal at the journal's time; the node orders pending rows by
 * time, then id.
 */
export const restoreDisposedMembers = (
  mempool: SimMempool,
  members: readonly string[],
  at: Date,
) => {
  const rows = new Set(mempool.survivors.map((tx) => hex(tx.id)));
  const restored = members.filter(
    (id) => !rows.has(id) && !mempool.rejected.has(id) && mempool.txs.has(id),
  );
  if (restored.length === 0) return 0;
  mempool.survivors.push(
    ...restored.map((id) => ({ ...mempool.txs.get(id)!, at })),
  );
  mempool.survivors.sort(
    (left, right) =>
      left.at.getTime() - right.at.getTime() ||
      Buffer.compare(left.id, right.id),
  );
  return restored.length;
};

/** Admits `txs` the way the node's admission leaves them: mempool, delta, working ledger. */
export const admitPending = (txs: readonly SimPendingTx[]) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    yield* withFollowerWrite(
      Effect.gen(function* () {
        for (const tx of txs) {
          yield* TxUtils.insertEntry(MempoolDB.tableName, {
            [TxUtils.Columns.TX_ID]: tx.id,
            [TxUtils.Columns.TX]:
              tx.cbor ?? Buffer.from("a1".repeat(16), "hex"),
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
