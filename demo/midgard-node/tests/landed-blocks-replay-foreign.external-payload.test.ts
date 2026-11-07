/**
 * The production foreign replayer on a real follower store, for a deposit or
 * withdrawal whose payload is external: the follower reads the payload from
 * the retention output the admission tx references, so it refuses the order
 * (`retention_output_unseen`) when that output is not in its facts, which
 * means the output was created before the follower store's origin. A block
 * naming such an order is judged against a store whose origin is too late
 * for it (`origin_after_frontier`), never `invalid`. A block naming an event
 * nobody admitted stays `invalid` while the follower's refusals in the
 * block's range are still in its facts (the prune at or before `T(B)`), and
 * past that it is a bounded wait (`event_unknown`).
 *
 * The bench's slot configuration maps slot `s` to `s * 1000` ms; the view's
 * clock is moved forward so the view's horizon reaches each block's end.
 */
import type {
  FactStore,
  FactStoreOptions,
  OutRef,
} from "@al-ft/midgard-l1-follower";
import type { EventKind } from "@al-ft/midgard-l1-follower/events";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { replayForeignBlock } from "../src/landed-blocks/replay-foreign.js";
import { FORCED_CONFIG } from "./helpers/forced-orders-chain.js";
import { db } from "./helpers/forced-orders-node-store.js";
import {
  admissionTx,
  ELSEWHERE,
  eventKeyOf,
  eventOrder,
  EVENTS_CONFIG,
} from "./helpers/l1-events-chain.js";
import type { ChainDriver } from "./helpers/l1-events-store.js";
import { AVAILABILITY } from "./landed-blocks-replay-foreign.availability.js";
import {
  clock,
  depositBlock,
  depositsAt,
  emptyBlock,
  followChain,
  idKey,
  inNode,
  inputFor,
  K,
  naming,
  nonceRef,
  retain,
  retainedRow,
} from "./landed-blocks-replay-foreign.fixture.js";
import {
  originSlot,
  reach,
  replayAt,
  SLOT_MS,
  startFor,
  strangerDepositBlock,
  windowed,
} from "./landed-blocks-replay-foreign.window.js";
import { resetApplicationTables } from "./utils.js";

const opened: FactStore[] = [];
const follow = async (extra: Partial<FactStoreOptions> = {}) => {
  const followed = await followChain(extra);
  opened.push(followed.store);
  return followed;
};

beforeEach(async () => {
  clock.offsetMs = 0;
  await db(resetApplicationTables);
});
afterEach(async () => {
  vi.restoreAllMocks();
  await Promise.all(opened.splice(0).map((store) => store.close()));
});

/** A retention output the follower's facts never held. */
const UNSEEN: OutRef = { txHash: Buffer.alloc(32, 0xab), index: 0 };

/**
 * Admits an external `kind` order included at `inclusionTime`, its payload
 * read from `UNSEEN`, or from a retention output created first when `seen`.
 */
const admitExternal = async (
  chain: ChainDriver,
  kind: EventKind,
  inclusionTime: bigint,
  seen = false,
) => {
  const nonce = nonceRef();
  const order = eventOrder(kind, nonce, { external: true, inclusionTime });
  let retainedRef = UNSEEN;
  if (seen) {
    const [hash] = await chain.forward([
      {
        inputs: [nonceRef()],
        outputs: [order.retained!],
        nonce: chain.chain.nonce(),
      },
    ]);
    retainedRef = { txHash: hash!, index: 0 };
  }
  await chain.forward([admissionTx(order, chain.chain.nonce(), retainedRef)]);
  return { nonce, admittedAt: chain.tip.point.slot };
};

/** A block over the window starting at `start` naming the `kind` event of `nonce`. */
const namingBlock = async (kind: EventKind, nonce: OutRef, start: bigint) =>
  windowed(
    await naming(await emptyBlock(), {
      [kind === "deposit" ? "deposits" : "withdrawals"]: [[idKey(nonce), "00"]],
    }),
    start,
  );

/** The follower's refusals of the `kind` order of `nonce`, as (reason, kind). */
const refusalsOf = (store: FactStore, nonce: OutRef) =>
  store.transaction("read", async (tx) =>
    (
      await tx.query(
        "SELECT reason, kind FROM node_l1_event_refusals WHERE event_key = ?",
        [Buffer.from(eventKeyOf(nonce), "hex")],
      )
    ).map((row) => [String(row.reason), String(row.kind)]),
  );

const lateOrigin = (kind: EventKind) => ({
  kind: "origin_after_frontier",
  detail: expect.stringMatching(
    new RegExp(
      `^${kind} .* was refused by the follower: the retention output its payload is read from is not in its facts`,
    ),
  ) as unknown,
});

describe("a deposit or withdrawal whose retention output is outside the follower's facts", () => {
  it.each(["deposit", "withdrawal"] as const)(
    "judges a block naming the %s against a store whose origin is too late, not invalid",
    async (kind) => {
      const { store, chain } = await follow();
      const start = startFor(await originSlot(store));
      const { nonce } = await admitExternal(chain, kind, start + SLOT_MS);
      const block = await namingBlock(kind, nonce, start);
      await reach(store, block);
      expect(await replayAt(store, block)).toMatchObject(lateOrigin(kind));
      expect(await refusalsOf(store, nonce)).toEqual([
        ["retention_output_unseen", kind],
      ]);
    },
  );

  it("replays a block naming the deposit once the follower saw its retention output", async () => {
    const { store, chain } = await follow();
    const start = startFor(await originSlot(store));
    const { nonce } = await admitExternal(
      chain,
      "deposit",
      start + SLOT_MS,
      true,
    );
    expect(await refusalsOf(store, nonce)).toEqual([]);
    const [known] = await depositsAt(store);
    const block = await windowed(await depositBlock(known!.entry), start);
    await reach(store, block);
    expect(await replayAt(store, block)).toMatchObject({ kind: "replayed" });
  });

  it("judges invalid a block naming a deposit nobody admitted, beside a refused one", async () => {
    const { store, chain } = await follow();
    const start = startFor(await originSlot(store));
    await admitExternal(chain, "deposit", start + SLOT_MS);
    const block = await strangerDepositBlock(start);
    await reach(store, block);
    expect(await replayAt(store, block)).toMatchObject({
      kind: "invalid",
      detail: expect.stringContaining("whose facts cover the block's range"),
    });
  });

  it("judges invalid a block naming as a deposit the id of a refused withdrawal", async () => {
    const { store, chain } = await follow();
    const start = startFor(await originSlot(store));
    const { nonce } = await admitExternal(chain, "withdrawal", start + SLOT_MS);
    const block = await namingBlock("deposit", nonce, start);
    await reach(store, block);
    expect(await replayAt(store, block)).toMatchObject({
      kind: "invalid",
      detail: expect.stringContaining("whose facts cover the block's range"),
    });
  });

  it("judges invalid a block naming a deposit refused for its datum", async () => {
    const { store, chain } = await follow();
    const start = startFor(await originSlot(store));
    const nonce = nonceRef();
    const order = eventOrder("deposit", nonce, {
      inclusionTime: start + SLOT_MS,
    });
    await chain.forward([
      admissionTx(
        {
          ...order,
          order: { ...order.order, datum: Buffer.from("d87980", "hex") },
        },
        chain.chain.nonce(),
      ),
    ]);
    expect((await refusalsOf(store, nonce)).map(([reason]) => reason)).toEqual([
      "malformed",
    ]);
    const block = await namingBlock("deposit", nonce, start);
    await reach(store, block);
    expect(await replayAt(store, block)).toMatchObject({
      kind: "invalid",
      detail: expect.stringContaining("whose facts cover the block's range"),
    });
  });

  it("judges invalid a block naming a deposit by bytes that are no event id", async () => {
    const { store, chain } = await follow();
    const start = startFor(await originSlot(store));
    await admitExternal(chain, "deposit", start + SLOT_MS);
    const block = await windowed(
      await naming(await emptyBlock(), { deposits: [["ff", "00"]] }),
      start,
    );
    await reach(store, block);
    expect(await replayAt(store, block)).toMatchObject({
      kind: "invalid",
      detail: expect.stringContaining("whose facts cover the block's range"),
    });
  });

  it("waits, not invalid, while the follower's refusals cannot be read", async () => {
    const { store, chain } = await follow();
    const start = startFor(await originSlot(store));
    const block = await strangerDepositBlock(start);
    await reach(store, block);
    await chain.forward([]);
    const transaction = store.transaction.bind(store);
    // Only the refusals' read fails: every other read reaches the facts.
    vi.spyOn(store, "transaction").mockImplementation(((mode, run) =>
      transaction(mode, (tx) =>
        run(
          new Proxy(tx, {
            get: (target, key) => {
              const value = Reflect.get(target, key) as unknown;
              if (key !== "query" || typeof value !== "function")
                return typeof value === "function" ? value.bind(target) : value;
              return (sql: string, params: unknown[]) =>
                sql.includes("node_l1_event_refusals")
                  ? Promise.reject(new Error("offline"))
                  : (value as typeof tx.query).call(
                      target,
                      sql,
                      params as never,
                    );
            },
          }),
        ),
      )) as typeof store.transaction);
    expect(await replayAt(store, block)).toMatchObject({
      kind: "event_unknown",
      detail: expect.stringContaining("the follower's refusals are unreadable"),
    });
  });

  it("judges invalid a block replayed at a view from before the refusal", async () => {
    const { store, chain } = await follow();
    await chain.forward([]);
    const start = startFor(await originSlot(store));
    const nonce = nonceRef();
    const block = await namingBlock("deposit", nonce, start);
    await reach(store, block);
    await retain(await retainedRow(block));
    const input = await inputFor(store, block);
    const order = eventOrder("deposit", nonce, {
      external: true,
      inclusionTime: start + SLOT_MS,
    });
    await chain.forward([admissionTx(order, chain.chain.nonce(), UNSEEN)]);
    expect(await refusalsOf(store, nonce)).toEqual([
      ["retention_output_unseen", "deposit"],
    ]);
    expect((await store.cursor())!.generation).toBe(input.view.generation);
    expect(
      await inNode(
        replayForeignBlock({
          store,
          events: EVENTS_CONFIG,
          forcedOrders: FORCED_CONFIG,
          availability: () => AVAILABILITY,
        })(input),
      ),
    ).toMatchObject({
      kind: "invalid",
      detail: expect.stringContaining("whose facts cover the block's range"),
    });
  });
});

/** The deployment's one-shot outref, which the protocol-init tx spends. */
const ONE_SHOT: OutRef = { txHash: Buffer.alloc(32, 0x0e), index: 0 };

describe("a follower store that holds the protocol-init spend, its origin after the retention output", () => {
  it("judges a block naming the withdrawal against a store whose origin is too late, not invalid", async () => {
    const { store, chain } = await follow();
    store.watchProtocolInit(ONE_SHOT);
    await chain.forward([
      {
        inputs: [ONE_SHOT],
        outputs: [{ address: ELSEWHERE, lovelace: 2_000_000n }],
        nonce: chain.chain.nonce(),
      },
    ]);
    const start = startFor((await originSlot(store)) - 1);
    const { nonce } = await admitExternal(chain, "withdrawal", start + SLOT_MS);
    const block = await namingBlock("withdrawal", nonce, start);
    await reach(store, block);
    await retain(await retainedRow(block));
    expect(
      await inNode(
        replayForeignBlock({
          store,
          events: EVENTS_CONFIG,
          forcedOrders: FORCED_CONFIG,
          availability: () => AVAILABILITY,
          hubOracleOneShot: ONE_SHOT,
        })(await inputFor(store, block)),
      ),
    ).toMatchObject(lateOrigin("withdrawal"));
  });

  it("judges invalid there a block naming a deposit nobody admitted", async () => {
    const { store, chain } = await follow();
    store.watchProtocolInit(ONE_SHOT);
    await chain.forward([
      {
        inputs: [ONE_SHOT],
        outputs: [{ address: ELSEWHERE, lovelace: 2_000_000n }],
        nonce: chain.chain.nonce(),
      },
    ]);
    const start = startFor((await originSlot(store)) - 1);
    await admitExternal(chain, "withdrawal", start + SLOT_MS);
    const block = await strangerDepositBlock(start);
    await reach(store, block);
    await retain(await retainedRow(block));
    expect(
      await inNode(
        replayForeignBlock({
          store,
          events: EVENTS_CONFIG,
          forcedOrders: FORCED_CONFIG,
          availability: () => AVAILABILITY,
          hubOracleOneShot: ONE_SHOT,
        })(await inputFor(store, block)),
      ),
    ).toMatchObject({
      kind: "invalid",
      detail: expect.stringContaining("whose facts cover the block's range"),
    });
  });
});

describe("the follower's refusals and the prune at T(B)", () => {
  /** A refused deposit admitted after `T(B)`, the store's origin, then `K + 3` more blocks, pruned through `slot(admittedAt, origin)`. */
  const prunedPast = async (
    slot: (admittedAt: number, origin: number) => number,
  ) => {
    const floor: { slot: number | null } = { slot: null };
    const { store, chain } = await follow({
      pruneFloors: [{ name: "test", floor: () => Promise.resolve(floor.slot) }],
    });
    const origin = await originSlot(store);
    const start = startFor(origin);
    const { nonce, admittedAt } = await admitExternal(
      chain,
      "deposit",
      start + SLOT_MS,
    );
    for (let block = 0; block < K + 3; block++) await chain.forward([]);
    floor.slot = slot(admittedAt, origin);
    await store.prune();
    expect((await store.cursor())!.prunedThroughSlot).toBe(floor.slot);
    const block = await namingBlock("deposit", nonce, start);
    await reach(store, block);
    return { store, nonce, origin, block, pruned: floor.slot };
  };

  it("keeps the refusal, and the late-origin judgement, with the prune at T(B)", async () => {
    const { store, nonce, block } = await prunedPast((_, origin) => origin);
    expect(await refusalsOf(store, nonce)).toHaveLength(1);
    expect(await replayAt(store, block)).toMatchObject(lateOrigin("deposit"));
  });

  // The hold is the local fault's: eight runs with no progress past the
  // block, then the driver's transient budget is spent and the node exits.
  it("waits, not invalid, once the prune passed T(B) and the refusal is gone", async () => {
    const { store, nonce, origin, block, pruned } = await prunedPast(
      (admittedAt) => admittedAt,
    );
    expect(await refusalsOf(store, nonce)).toEqual([]);
    expect(await replayAt(store, block)).toMatchObject({
      kind: "event_unknown",
      detail: expect.stringContaining(
        `the facts are pruned through slot ${pruned.toString()}, past slot ${origin.toString()}`,
      ),
    });
  });
});
