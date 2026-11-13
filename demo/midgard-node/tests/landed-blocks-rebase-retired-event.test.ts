/**
 * A node that was down while a deposit, a withdrawal or a forced order was
 * admitted on L1, included in another operator's block and retired (the
 * deposit absorbed, the withdrawal paid out, the order spent) restarts to a
 * follower view where the event is retired. The foreign block replays
 * against the follower's facts, which keep the retired event until the
 * prune passes it, so it lands processed and names the event. The
 * follower-change driver's recompute (`rebaseIfDue`) must then rebase onto
 * that block without any manual step:
 *
 * - processing the block writes the node's row of each event it names from
 *   the facts its replay checked, so the rebase finds it once the prune has
 *   taken the event out of the follower's view;
 * - a block processed before that, its retired event not yet ingested,
 *   converges as ingestion writes the row while a processed block names it;
 * - either write leaves the other's row as it is, and both write the same
 *   bytes;
 * - an event retired unseen that only a removed block names, or none,
 *   still gets no row, and a deposit no mempool projection.
 *
 * Each kind runs on a real follower store in the node database, on a
 * simulated chain, with the production replayer on an honest block naming
 * it.
 */
import type { FactStore, OutRef } from "@al-ft/midgard-l1-follower";
import { type EventKind, eventsAt } from "@al-ft/midgard-l1-follower/events";
import type * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import {
  type NamedEvents,
  recordNamedEvents,
} from "../src/database/follower-events.named.js";
import * as MempoolLedgerDB from "../src/database/mempoolLedger.js";
import { readUserEvent } from "../src/l1-events/entries.js";
import { rebasePlan } from "../src/landed-blocks/rebase-target.js";
import { processRow } from "../src/landed-blocks/settlements.js";
import {
  type LandedBlockRow,
  retrieveRows,
} from "../src/landed-blocks/store.js";
import { planCurrentView } from "../src/services/l1-follower.readiness.js";
import { testDriverRecompute, testWrite } from "./helpers/driver-recompute.js";
import { reconcileOverDead } from "./helpers/follower-view.js";
import { FORCED_CONFIG } from "./helpers/forced-orders-chain.js";
import {
  ingestionHook,
  UNCHANGED,
} from "./helpers/forced-orders-node-store.js";
import {
  admissionTx,
  eventOrder,
  EVENTS_CONFIG,
  listOf,
  retirementTx,
} from "./helpers/l1-events-chain.js";
import type { ChainDriver } from "./helpers/l1-events-store.js";
import {
  freshNative,
  landedRow,
  processOf,
  run,
  seed,
} from "./landed-blocks-rebase.fixture.js";
import {
  clock,
  depositBlock,
  FIXTURE_ORDER_ID,
  fixtureOrderTx,
  followChain,
  forcedBlock,
  idKey,
  K,
  NETWORK,
  nonceRef,
  replay,
  retain,
  retainedRow,
} from "./landed-blocks-replay-foreign.fixture.js";
import { spendTx } from "./landed-blocks-replay-foreign.window.js";
import { withdrawalBlock } from "./landed-blocks-replay-foreign.withdrawal.js";

const opened: FactStore[] = [];
beforeEach(() => {
  clock.offsetMs = 0;
});
afterEach(async () => {
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

type Kind = EventKind | "forced";

/** The event the follower's projection reads at its view, as the replay decodes it. */
const readAt = async (store: FactStore, kind: EventKind, id: string) => {
  const view = (await store.currentView())!;
  const read = await eventsAt(store, listOf(kind), view.point);
  if (read.kind !== "ok") throw new Error(`eventsAt: ${read.kind}`);
  const event = read.value.find((candidate) => candidate.idCbor === id)!;
  expect(event.retirement).not.toBeNull();
  const reading = readUserEvent(event, NETWORK);
  if (reading.kind === "undecodable") throw new Error(reading.detail);
  return { event, entry: reading.entry };
};

/** Replays `block` with the production replayer at the store's view. */
const replayed = async (store: FactStore, block: SDK.DaPayload) => {
  await retain(await retainedRow(block));
  const outcome = await replay(store, block);
  if (outcome.kind !== "replayed") throw new Error(JSON.stringify(outcome));
  return outcome.named;
};

/** Admits a `kind` event of `nonce` and retires it in the next block. */
const admitAndRetire = async (
  chain: ChainDriver,
  kind: EventKind,
  nonce: OutRef,
) => {
  const order = eventOrder(kind, nonce, { inclusionTime: 2n });
  const [admitted] = await chain.forward([
    admissionTx(order, chain.chain.nonce()),
  ]);
  const reason = kind === "deposit" ? "absorbed" : "payout_initialized";
  await chain.forward([
    retirementTx(
      order,
      { txHash: admitted!, index: 0 },
      reason,
      chain.chain.nonce(),
    ),
  ]);
};

/**
 * Each kind: the node table its rows live in, the landed row's fields
 * naming it, and its retirement on the chain, giving the facts the replay
 * checked the block against.
 */
const KINDS: Record<
  Kind,
  Readonly<{
    table: string;
    column: string;
    names: (id: string) => Partial<LandedBlockRow>;
    retire: (
      store: FactStore,
      chain: ChainDriver,
      nonce: OutRef,
    ) => Promise<NamedEvents>;
  }>
> = {
  deposit: {
    table: "deposits_utxos",
    column: "event_id",
    names: (id) => ({ depositIds: [Buffer.from(id, "hex")] }),
    retire: async (store, chain, nonce) => {
      await admitAndRetire(chain, "deposit", nonce);
      const read = await readAt(store, "deposit", idKey(nonce));
      if (read.entry.kind !== "deposit") throw new Error("not a deposit");
      const named = await replayed(store, await depositBlock(read.entry.entry));
      expect(named).toEqual({ events: [read], forced: [] });
      return named;
    },
  },
  withdrawal: {
    table: "withdrawal_utxos",
    column: "event_id",
    names: (id) => ({
      withdrawals: [
        { id, validity: "WithdrawalIsValid", detail: null, settlement: "00" },
      ],
    }),
    retire: async (store, chain, nonce) => {
      await admitAndRetire(chain, "withdrawal", nonce);
      const read = await readAt(store, "withdrawal", idKey(nonce));
      const { block } = await withdrawalBlock(store, nonce);
      const named = await replayed(store, block);
      expect(named).toEqual({ events: [read], forced: [] });
      return named;
    },
  },
  forced: {
    table: "forced_transaction_utxos",
    column: "tx_order_id",
    names: (id) => ({ forcedIds: [Buffer.from(id, "hex")] }),
    retire: async (store, chain) => {
      const [order] = await chain.forward([
        await fixtureOrderTx(chain, FIXTURE_ORDER_ID),
      ]);
      await chain.forward([
        spendTx({ txHash: order!, index: 0 }, chain.chain.nonce()),
      ]);
      const named = await replayed(store, await forcedBlock());
      expect(named.forced).toHaveLength(1);
      return named;
    },
  },
};

const idOf = (kind: Kind, nonce: OutRef) =>
  idKey(kind === "forced" ? FIXTURE_ORDER_ID : nonce);

/**
 * How the seeded foreign block names the event: as a processed block (its
 * row never written: a block processed before the fix), as a block a
 * rollback took off the landed chain, or not at all.
 */
type Naming = "processed" | "removed" | "none";

const seeded = (kind: Kind, id: string, naming: Naming) =>
  naming === "none"
    ? {}
    : {
        ...KINDS[kind].names(id),
        ...(naming === "removed"
          ? ({ state: "removed", applied: true } as const)
          : {}),
      };

/**
 * A restarted node with the frontier ledger the rebase fixture seeds, its
 * foreign block naming the event as `naming` says, and a follower store
 * over the chain the event is retired on.
 */
const restarted = async (kind: Kind, naming: Naming) => {
  const nonce = nonceRef();
  const id = idOf(kind, nonce);
  const globals = await processOf(freshNative());
  await seed(globals, seeded(kind, id, naming));
  const { store, chain } = await followChain();
  opened.push(store);
  const named = await KINDS[kind].retire(store, chain, nonce);
  return { globals, store, chain, id, named };
};

type Restarted = Awaited<ReturnType<typeof restarted>>;

/** The foreign block processed as the landed hook processes it, with the facts its replay checked. */
const processBlock = ({ globals, id, named }: Restarted, kind: Kind) =>
  run(
    globals,
    testWrite(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        yield* sql`DELETE FROM node_landed_blocks`;
        yield* processRow(landedRow(KINDS[kind].names(id)), undefined, named);
      }),
    ),
  );

/**
 * Ingestion of the event at the follower's view: the sink's, projecting
 * the deposits due by `cutoffMs`, or the forced-order hook.
 */
const ingest = async (
  { globals, store }: Restarted,
  kind: Kind,
  cutoffMs = 0,
) => {
  if (kind === "forced") {
    expect(await ingestionHook(store, FORCED_CONFIG).hook(UNCHANGED)).toBe(
      undefined,
    );
    return;
  }
  const read = await planCurrentView(store, EVENTS_CONFIG);
  if (read.kind !== "ok") throw new Error(read.detail);
  await run(
    globals,
    testWrite(
      reconcileOverDead(read.plan, {
        network: NETWORK,
        slotToUnixTime: (slot) => slot * 1_000,
        cutoffMs,
      }),
    ),
  );
};

/** The node's rows of `kind`, each as JSON without its write times. */
const rowsOf = ({ globals }: Pick<Restarted, "globals">, kind: Kind) =>
  run(
    globals,
    Effect.flatMap(
      SqlClient.SqlClient,
      (sql) => sql<{ row: unknown }>`
        SELECT to_jsonb(e) - 'created_at' - 'updated_at' AS row
        FROM ${sql(KINDS[kind].table)} e`,
    ),
  ).then((rows) => rows.map(({ row }) => row));

const idsOf = async (node: Pick<Restarted, "globals">, kind: Kind) =>
  (await rowsOf(node, kind)).map(
    (row) => (row as Record<string, string>)[KINDS[kind].column],
  );

/** One driver run's rebase at the follower's view, and what it leaves. */
const rebased = async ({ globals, store, id }: Restarted, kind: Kind) => {
  const hold = await run(
    globals,
    Effect.flatMap(
      testDriverRecompute({
        planCurrent: () => planCurrentView(store, EVENTS_CONFIG),
      }),
      (recompute) => recompute.rebaseIfDue("restart after downtime"),
    ),
  );
  expect(hold).toBeUndefined();
  expect((await run(globals, rebasePlan())).kind).toBe("none");
  expect((await run(globals, retrieveRows)).map((row) => row.applied)).toEqual([
    true,
  ]);
  expect(await idsOf({ globals }, kind)).toEqual([`\\x${id}`]);
};

describe.each<{ kind: Kind; retired: string }>([
  { kind: "deposit", retired: "deposit was absorbed" },
  { kind: "withdrawal", retired: "withdrawal was paid out" },
  { kind: "forced", retired: "forced order was spent" },
])(
  "a restarted node rebases onto a foreign block whose $retired while it was down",
  ({ kind }) => {
    it("writes the event's row as it processes the block, so the rebase finds it once the prune took the event", async () => {
      const node = await restarted(kind, "none");
      await processBlock(node, kind);
      for (let block = 0; block < K + 3; block++) await node.chain.forward([]);
      await node.store.prune();
      const plan = await planCurrentView(node.store, EVENTS_CONFIG);
      expect(plan.kind === "ok" && plan.plan.events).toEqual([]);
      await rebased(node, kind);
    });

    it("converges for a block processed before its row was written, the event still in the follower's view", async () => {
      const node = await restarted(kind, "processed");
      await ingest(node, kind);
      await rebased(node, kind);
    });

    it("writes the row ingestion writes, and either write leaves the other's row as it is", async () => {
      const node = await restarted(kind, "none");
      await processBlock(node, kind);
      const written = await rowsOf(node, kind);
      expect(written).toHaveLength(1);
      await ingest(node, kind);
      expect(await rowsOf(node, kind)).toEqual(written);
      await run(
        node.globals,
        Effect.flatMap(
          SqlClient.SqlClient,
          (sql) => sql`DELETE FROM ${sql(KINDS[kind].table)}`,
        ),
      );
      await ingest(node, kind);
      expect(await rowsOf(node, kind)).toEqual(written);
      await run(node.globals, testWrite(recordNamedEvents(node.named)));
      expect(await rowsOf(node, kind)).toEqual(written);
    });
  },
);

describe.each<{ kind: Kind; retired: string }>([
  { kind: "deposit", retired: "deposit absorbed" },
  { kind: "withdrawal", retired: "withdrawal paid out" },
  { kind: "forced", retired: "forced order spent" },
])("a $retired before the node saw it", ({ kind }) => {
  it.each<{ naming: Naming; by: string }>([
    { naming: "removed", by: "only a removed landed block" },
    { naming: "none", by: "no landed block" },
  ])("gets no row when $by names it", async ({ naming }) => {
    const node = await restarted(kind, naming);
    // Past every inclusion time: a deposit row would be projected.
    await ingest(node, kind, Date.UTC(2100, 0));
    expect(await rowsOf(node, kind)).toEqual([]);
    expect(
      await run(
        node.globals,
        Effect.flatMap(
          SqlClient.SqlClient,
          (sql) => sql`SELECT 1 FROM ${sql(MempoolLedgerDB.tableName)}`,
        ),
      ),
    ).toEqual([]);
  });
});
