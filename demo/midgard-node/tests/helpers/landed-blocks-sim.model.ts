/**
 * The landed-block fork simulator's independent model (N3): what the node's
 * landed-block rows, `confirmed_ledger`, working ledger, mempool, rejections
 * and deposit statuses must be after processing, derived from the canonical
 * queue, the traffic's registry and the simulated mempool alone. It shares
 * nothing with the code under test beyond the universe's outputs.
 *
 * The mempool model replays the surviving pending transactions in admission
 * order on the processed tip's ledger: one whose input is gone is rejected
 * ("direct"), one that spends a rejected transaction's output with it
 * ("dependent"); a rejection is final.
 */
import {
  decodeMidgardTxOutput,
  encodeMidgardAddressText,
} from "@al-ft/midgard-core/codec";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  DepositsDB,
  MempoolDB,
  MempoolTxDeltasDB,
  TxUtils,
} from "../../src/database/index.js";
import type * as Ledger from "../../src/database/utils/ledger.js";
import { REBASE_REJECTIONS } from "../../src/landed-blocks/index.js";
import { withHistoryWrite } from "../../src/services/event-history-producer.js";
import type { SimRegistry } from "./landed-blocks-sim.traffic.js";
import type { SimUniverse } from "./landed-blocks-sim.universe.js";

const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");

export type SimPendingTx = Readonly<{
  id: Buffer;
  spent: readonly Buffer[];
  produced: readonly Ledger.MinimalEntry[];
  at: Date;
}>;

export type SimMempool = {
  survivors: SimPendingTx[];
  rejected: Map<string, "direct" | "dependent">;
  admitted: number;
  poolUsed: number;
};

export const newSimMempool = (): SimMempool => ({
  survivors: [],
  rejected: new Map(),
  admitted: 0,
  poolUsed: 0,
});

/**
 * Replays the survivors on `base`; moves the ones that cannot apply to the
 * rejections. Returns the working ledger and this round's rejections.
 */
export const settleMempool = (
  mempool: SimMempool,
  base: ReadonlyMap<string, Buffer>,
) => {
  const ledger = new Map(base);
  const producer = new Map<string, string>();
  for (const tx of mempool.survivors)
    for (const entry of tx.produced)
      producer.set(hex(entry.outref), hex(tx.id));
  const out = new Set<string>();
  const newly: [string, "direct" | "dependent"][] = [];
  for (const tx of mempool.survivors) {
    const missing = tx.spent.map(hex).filter((key) => !ledger.has(key));
    if (missing.length > 0) {
      out.add(hex(tx.id));
      newly.push([
        hex(tx.id),
        missing.some((key) => out.has(producer.get(key) ?? ""))
          ? "dependent"
          : "direct",
      ]);
      continue;
    }
    for (const key of tx.spent.map(hex)) ledger.delete(key);
    for (const entry of tx.produced)
      ledger.set(hex(entry.outref), Buffer.from(entry.output));
  }
  mempool.survivors = mempool.survivors.filter((tx) => !out.has(hex(tx.id)));
  for (const [id, reason] of newly) mempool.rejected.set(id, reason);
  return { ledger, newly };
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

/**
 * The node's state the model is compared with. A consumed deposit names its
 * merged block by height: a merge a rollback undid can be re-anchored on an
 * equal ledger of another fork, which keeps the header it was consumed at.
 */
export const readActual = (registry: SimRegistry) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const confirmed = yield* sql<{ outref: Buffer; output: Buffer }>`
    SELECT outref, output FROM confirmed_ledger`;
    const working = yield* sql<{
      outref: Buffer;
      output: Buffer;
      source_event_id: Buffer | null;
    }>`SELECT outref, output, source_event_id FROM mempool_ledger`;
    const mempool = yield* sql<{ tx_id: Buffer }>`SELECT tx_id FROM mempool`;
    const rejections = yield* sql<{ tx_id: Buffer; reject_code: string }>`
    SELECT tx_id, reject_code FROM tx_rejections`;
    const deposits = yield* sql<{
      event_id: Buffer;
      status: string;
      projected_header_hash: Buffer | null;
    }>`SELECT event_id, status, projected_header_hash FROM deposits_utxos`;
    const sorted = <A>(items: A[]) => items.sort();
    return {
      confirmed: sorted(
        confirmed.map((row) => `${hex(row.outref)}=${hex(row.output)}`),
      ),
      working: sorted(
        working.map(
          (row) =>
            `${hex(row.outref)}=${hex(row.output)}@${row.source_event_id === null ? "-" : hex(row.source_event_id)}`,
        ),
      ),
      mempool: sorted(mempool.map((row) => hex(row.tx_id))),
      rejections: sorted(
        rejections.map((row) => `${hex(row.tx_id)}:${row.reject_code}`),
      ),
      deposits: sorted(
        deposits.map(
          (row) =>
            `${hex(row.event_id)}:${row.status}@${
              row.projected_header_hash === null
                ? "-"
                : row.status === "consumed"
                  ? `h${registry.get(hex(row.projected_header_hash))?.h ?? "?"}`
                  : hex(row.projected_header_hash)
            }`,
        ),
      ),
    };
  });

export type ActualState = Effect.Effect.Success<ReturnType<typeof readActual>>;

/** The canonical queue as the model reads it: the root's header, then the nodes' headers. */
export type ModelQueueHeaders = Readonly<{
  root: string;
  nodes: readonly string[];
}>;

/** The processed prefix: every node up to the first bad one, and that bad one. */
export const processedPrefix = (
  registry: SimRegistry,
  queue: ModelQueueHeaders,
) => {
  const prefix: string[] = [];
  for (const node of queue.nodes) {
    if (registry.get(node)!.bad) return { prefix, bad: node };
    prefix.push(node);
  }
  return { prefix, bad: undefined };
};

/** The headers by height on the lineage ending at `tip`. */
const lineage = (registry: SimRegistry, tip: string) => {
  const byHeight = new Map<number, string>();
  for (
    let header: string | null = tip;
    header !== null;
    header = registry.get(header)!.prevHeaderHash
  )
    byHeight.set(registry.get(header)!.h, header);
  return byHeight;
};

/** What the node's state must be, given the canonical queue and the mempool model. */
export const expectedState = (
  universe: SimUniverse,
  registry: SimRegistry,
  queue: ModelQueueHeaders,
  workingLedger: ReadonlyMap<string, Buffer>,
  mempool: SimMempool,
) => {
  const { prefix } = processedPrefix(registry, queue);
  const tip = prefix.at(-1) ?? queue.root;
  const root = registry.get(queue.root)!;
  const tipInfo = registry.get(tip)!;
  const headers = lineage(registry, tip);
  const depositSource = new Map(
    universe.deposits.map((deposit) => [
      hex(deposit.entry.outref),
      hex(deposit.row[DepositsDB.Columns.ID]),
    ]),
  );
  const deposits = universe.deposits.map((deposit) => {
    const header = deposit.h <= tipInfo.h ? headers.get(deposit.h) : undefined;
    const status =
      header === undefined
        ? "awaiting"
        : deposit.h <= root.h
          ? "consumed"
          : "projected";
    const at =
      header === undefined
        ? "-"
        : status === "consumed"
          ? `h${deposit.h}`
          : header;
    return `${hex(deposit.row[DepositsDB.Columns.ID])}:${status}@${at}`;
  });
  const code = (reason: "direct" | "dependent") =>
    REBASE_REJECTIONS[reason].code;
  return {
    tip,
    tipRoot: universe.root(tipInfo.h, tipInfo.b),
    actual: {
      confirmed: universe
        .ledger(root.h, root.b)
        .map((entry) => `${hex(entry.outref)}=${hex(entry.output)}`)
        .sort(),
      working: [...workingLedger]
        .map(
          ([outRef, output]) =>
            `${outRef}=${hex(output)}@${depositSource.get(outRef) ?? "-"}`,
        )
        .sort(),
      mempool: mempool.survivors.map((tx) => hex(tx.id)).sort(),
      rejections: [...mempool.rejected]
        .map(([id, reason]) => `${id}:${code(reason)}`)
        .sort(),
      deposits: deposits.sort(),
    } satisfies ActualState,
  };
};

/** The first difference between the node's state and the model's, if any. */
export const stateDifference = (
  actual: ActualState,
  expected: ActualState,
): string | null => {
  for (const key of Object.keys(expected) as (keyof ActualState)[]) {
    const left = actual[key];
    const right = expected[key];
    if (JSON.stringify(left) === JSON.stringify(right)) continue;
    const extra = left.filter((item) => !right.includes(item));
    const missing = right.filter((item) => !left.includes(item));
    return `${key}: node has ${JSON.stringify(extra).slice(0, 500)} the model lacks, lacks ${JSON.stringify(missing).slice(0, 500)}`;
  }
  return null;
};
