/**
 * The landed-block fork simulator's independent model (N3): what the node's
 * landed-block rows, confirmed-ledger frontier, `confirmed_ledger`, working
 * ledger, mempool, rejections and deposit statuses must be after
 * processing, derived from the canonical queue, the traffic's registry, the
 * simulated mempool (`landed-blocks-sim.mempool.ts`), the model's settlement
 * record (which block on the processed chain includes each transaction) and
 * the frontier the model itself derived on the previous check. It shares nothing with the
 * code under test beyond the universe's outputs.
 *
 * Processing walks the root's lineage from the frontier, then the queue's
 * nodes, and stops at a block that does not replay to its header or whose
 * DA payload is still missing; the frontier ends at the last processed
 * header the root passed. A frontier off the root's lineage (a merge a
 * rollback undid) stays where it is, unless the root's ledger is the
 * frontier's (then it re-anchors at the root).
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { DepositsDB } from "../../src/database/index.js";
import { REBASE_REJECTIONS } from "../../src/landed-blocks/index.js";
import type { SimMempool } from "./landed-blocks-sim.mempool.js";
import type { SimRegistry } from "./landed-blocks-sim.traffic.js";
import type { SimUniverse } from "./landed-blocks-sim.universe.js";

const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");

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
    const mempool = yield* sql<{
      tx_id: Buffer;
      included_by: Buffer | null;
    }>`SELECT tx_id, included_by FROM mempool`;
    const settlements = yield* sql<{ tx_id: Buffer; settled_by: Buffer }>`
    SELECT s.tx_id, s.settled_by
    FROM event_history_l2_ledger_receipt_settlements s
    JOIN event_history_l2_ledger_receipts r ON r.sequence = s.receipt_sequence
    WHERE r.reversed_at_revision IS NULL`;
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
      marked: sorted(
        mempool
          .filter((row) => row.included_by !== null)
          .map((row) => `${hex(row.tx_id)}@${hex(row.included_by!)}`),
      ),
      settlements: sorted(
        settlements.map((row) => `${hex(row.tx_id)}@${hex(row.settled_by)}`),
      ),
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

/** The root's lineage, genesis first. */
export const rootLineage = (registry: SimRegistry, root: string) =>
  [...lineage(registry, root)]
    .sort(([left], [right]) => left - right)
    .map(([, header]) => header);

/** Why processing stops at a header. */
export type ModelStop = Readonly<{
  header: string;
  kind: "invalid" | "awaiting_da";
  /** The root passed it (it was merged before it was processed). */
  merged: boolean;
}>;

export type ModelProcessing =
  | Readonly<{ kind: "behind"; frontier: string }>
  | Readonly<{
      kind: "processed";
      /** The frontier the check started from (`undefined`: none yet). */
      from: string | undefined;
      reanchored: boolean;
      frontier: string;
      /** Every header processed past `from`, in order. */
      processed: readonly string[];
      /** The processed headers past the frontier: the node's rows. */
      rows: readonly string[];
      tip: string;
      stop: ModelStop | undefined;
    }>;

/**
 * Where processing must leave the node at `queue`, from the frontier the
 * model derived before (`previous`). `served(header)` says whether a
 * foreign block's DA payload is available by now.
 */
export const modelProcessing = (
  registry: SimRegistry,
  queue: ModelQueueHeaders,
  previous: string | undefined,
  served: (header: string) => boolean,
): ModelProcessing => {
  const rooted = rootLineage(registry, queue.root);
  const position = previous === undefined ? 0 : rooted.indexOf(previous);
  const rootInfo = registry.get(queue.root)!;
  let reanchored = false;
  let start = position;
  if (position < 0) {
    const info = registry.get(previous!)!;
    if (info.h !== rootInfo.h || info.b !== rootInfo.b)
      return { kind: "behind", frontier: previous! };
    reanchored = true;
    start = rooted.length - 1;
  }
  const merged = new Set(rooted.slice(start + 1));
  const sequence = [...rooted.slice(start + 1), ...queue.nodes];
  const processed: string[] = [];
  let stop: ModelStop | undefined;
  for (const header of sequence) {
    const info = registry.get(header)!;
    if (info.bad || (!info.own && !served(header))) {
      stop = {
        header,
        kind: info.bad ? "invalid" : "awaiting_da",
        merged: merged.has(header),
      };
      break;
    }
    processed.push(header);
  }
  // The frontier folds to the root once the root's lineage is processed,
  // never part of the way.
  const passed = processed.includes(queue.root)
    ? processed.filter((header) => merged.has(header))
    : [];
  const frontier = passed.at(-1) ?? rooted[start]!;
  return {
    kind: "processed",
    from: previous,
    reanchored,
    frontier,
    processed,
    rows: processed.slice(passed.length),
    tip: processed.at(-1) ?? frontier,
    stop,
  };
};

/**
 * What the node's state must be: `confirmed_ledger` at the frontier, the
 * working ledger the model rebuilt, the mempool (a row a block on the
 * processed chain includes marked by it) and rejections, the receipt
 * members of unreversed receipts recorded settled by the block in
 * `settledBy` that includes them, and every deposit's status (consumed up
 * to the frontier, projected to its header on the processed chain).
 */
export const expectedState = (
  universe: SimUniverse,
  registry: SimRegistry,
  model: Readonly<{ frontier: string; tip: string }>,
  workingLedger: ReadonlyMap<string, Buffer>,
  mempool: SimMempool,
  settledBy: ReadonlyMap<string, string>,
) => {
  const frontier = registry.get(model.frontier)!;
  const tipInfo = registry.get(model.tip)!;
  const headers = lineage(registry, model.tip);
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
        : deposit.h <= frontier.h
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
  return {
    confirmed: universe
      .ledger(frontier.h, frontier.b)
      .map((entry) => `${hex(entry.outref)}=${hex(entry.output)}`)
      .sort(),
    working: [...workingLedger]
      .map(
        ([outRef, output]) =>
          `${outRef}=${hex(output)}@${depositSource.get(outRef) ?? "-"}`,
      )
      .sort(),
    mempool: mempool.survivors.map((tx) => hex(tx.id)).sort(),
    marked: mempool.survivors
      .filter((tx) => settledBy.has(hex(tx.id)))
      .map((tx) => `${hex(tx.id)}@${settledBy.get(hex(tx.id))}`)
      .sort(),
    settlements: mempool.receipts
      .filter((receipt) => !receipt.reversed)
      .flatMap((receipt) =>
        receipt.ids.flatMap((id) => {
          const header = settledBy.get(id);
          return header === undefined ? [] : [`${id}@${header}`];
        }),
      )
      .sort(),
    rejections: [...mempool.rejected]
      .map(([id, reason]) => `${id}:${REBASE_REJECTIONS[reason].code}`)
      .sort(),
    deposits: deposits.sort(),
  } satisfies ActualState;
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
