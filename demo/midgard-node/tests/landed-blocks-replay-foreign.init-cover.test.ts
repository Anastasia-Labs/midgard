/**
 * The production foreign replayer, through the node's landed ports, on a
 * follower store that holds the protocol-init spend (the valid tx spending
 * the deployment's `hubOracleOneShot`) at the view. No event is admitted
 * before that spend, so such a store saw every event ever admitted: the
 * events a block's window can name are admitted after
 * `max(T(B), the spend's slot less one)`, and an absent one is the block's
 * fault (`invalid`) even when `T(B)` is before the store's origin. A store
 * without that spend at the view keeps `origin_after_frontier` for such a
 * block.
 *
 * The bench's slot configuration maps slot `s` to `s * 1000` ms; the view's
 * clock is moved forward so the view's horizon reaches each block's end.
 */
import type {
  FactStore,
  FactStoreOptions,
  OutRef,
} from "@al-ft/midgard-l1-follower";
import type * as SDK from "@al-ft/midgard-sdk";
import { Effect } from "effect";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { nodeLandedBlockPorts } from "../src/landed-blocks/node-ports.js";
import type { ReplayInput } from "../src/landed-blocks/replay.js";
import { FORCED_CONFIG } from "./helpers/forced-orders-chain.js";
import { db } from "./helpers/forced-orders-node-store.js";
import { ELSEWHERE, EVENTS_CONFIG } from "./helpers/l1-events-chain.js";
import type { ChainDriver } from "./helpers/l1-events-store.js";
import { SIM_QUEUE_CONFIG } from "./helpers/state-queue-sim.fixtures.js";
import { AVAILABILITY } from "./landed-blocks-replay-foreign.availability.js";
import {
  clock,
  depositBlock,
  followChain,
  inNode,
  inputFor,
  K,
  retain,
  retainedRow,
} from "./landed-blocks-replay-foreign.fixture.js";
import {
  admitDeposit,
  originSlot,
  reach,
  SLOT_MS,
  startFor,
  strangerDepositBlock,
  strangerOrderBlock,
  windowed,
} from "./landed-blocks-replay-foreign.window.js";
import { resetApplicationTables } from "./utils.js";

/** The deployment's one-shot outref, which the protocol-init tx spends. */
const ONE_SHOT: OutRef = { txHash: Buffer.alloc(32, 0x0e), index: 0 };

const opened: FactStore[] = [];
/** A followed chain whose store watches `ONE_SHOT`, as the follower's loop sets it. */
const follow = async (extra: Partial<FactStoreOptions> = {}) => {
  const followed = await followChain(extra);
  opened.push(followed.store);
  followed.store.watchProtocolInit(ONE_SHOT);
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

/** Lands the protocol-init tx in the next block; returns that block's slot. */
const landInit = async (chain: ChainDriver) => {
  await chain.forward([
    {
      inputs: [ONE_SHOT],
      outputs: [{ address: ELSEWHERE, lovelace: 2_000_000n }],
      nonce: chain.chain.nonce(),
    },
  ]);
  return chain.tip.point.slot;
};

/** The node's landed ports over `store`, with the deployment's one-shot. */
const portsOver = (store: FactStore) =>
  nodeLandedBlockPorts(
    store,
    {
      projection: EVENTS_CONFIG,
      forcedOrders: FORCED_CONFIG,
      stateQueue: SIM_QUEUE_CONFIG,
      origin: { hubOracleOneShot: ONE_SHOT },
    },
    // Not reached: no landed row asks for a rebase here.
    () => Effect.succeed(undefined),
    () => AVAILABILITY,
  );

/** Retains `block` and replays it through the ports at the view (or `input`'s), its node output entered at `enteredSlot` (default: the view's slot). */
const replayed = async (
  store: FactStore,
  block: SDK.DaPayload,
  enteredSlot?: number | null,
  input?: ReplayInput,
) => {
  await retain(await retainedRow(block));
  const at = input ?? (await inputFor(store, block));
  return inNode(
    portsOver(store).replay(
      enteredSlot === undefined ? at : { ...at, enteredSlot },
    ),
  );
};

/** A block whose `T(B)` is the slot before the store's origin, the view reaching its end. */
const earlyBlock = async (
  store: FactStore,
  build: (start: bigint) => Promise<SDK.DaPayload>,
) => {
  const block = await build(startFor((await originSlot(store)) - 1));
  await reach(store, block);
  return block;
};

const lateOrigin = (store: FactStore) =>
  originSlot(store).then((origin) => ({
    kind: "origin_after_frontier",
    detail: expect.stringContaining(
      `the follower's origin at slot ${origin.toString()} is after slot ${(origin - 1).toString()}`,
    ) as unknown,
  }));

describe("a follower store that holds the protocol-init spend, T(B) before its origin", () => {
  it("judges an absent deposit invalid", async () => {
    const { store, chain } = await follow();
    await landInit(chain);
    const block = await earlyBlock(store, strangerDepositBlock);
    expect(await replayed(store, block)).toMatchObject({
      kind: "invalid",
      detail: expect.stringContaining("whose facts cover the block's range"),
    });
  });

  it("judges an absent forced order invalid", async () => {
    const { store, chain } = await follow();
    await landInit(chain);
    const block = await earlyBlock(store, strangerOrderBlock);
    expect(await replayed(store, block)).toMatchObject({
      kind: "invalid",
      detail: expect.stringMatching(
        /^forced order .* is not admitted at the view/,
      ),
    });
  });

  it("replays a block whose named deposit is known", async () => {
    const { store, chain } = await follow();
    await landInit(chain);
    const start = startFor((await originSlot(store)) - 1);
    const { entry } = await admitDeposit(chain, start + SLOT_MS);
    const block = await earlyBlock(store, async (at) =>
      windowed(await depositBlock(entry), at),
    );
    expect(await replayed(store, block)).toMatchObject({
      kind: "replayed",
      depositIds: [Buffer.from(entry.idCbor, "hex")],
    });
  });

  // The facts are complete for the block's whole window only once the view
  // reaches its end: the horizon check before the coverage, in
  // `foreignBlockReplayer`'s `replay` (src/landed-blocks/replay-foreign.ts),
  // holds the block until then.
  it("holds an absent deposit, not invalid, while the view does not reach the block's end", async () => {
    const { store, chain } = await follow();
    await landInit(chain);
    const block = await strangerDepositBlock(
      startFor((await originSlot(store)) - 1),
    );
    await reach(store, block, -1);
    expect(await replayed(store, block)).toEqual({
      kind: "missing",
      detail: "the block ends past what the follower view can know",
    });
  });

  it("keeps the late-origin hold for a block whose node output was seeded", async () => {
    const { store, chain } = await follow();
    await landInit(chain);
    const block = await earlyBlock(store, strangerDepositBlock);
    expect(await replayed(store, block, null)).toMatchObject({
      kind: "origin_after_frontier",
      detail: expect.stringContaining(
        "the block's node output predates the follower's origin",
      ),
    });
  });

  it("waits, not the late-origin hold, while the protocol-init fact is unreadable", async () => {
    const { store, chain } = await follow();
    await landInit(chain);
    const block = await earlyBlock(store, strangerDepositBlock);
    vi.spyOn(store, "protocolInit").mockRejectedValue(new Error("offline"));
    expect(await replayed(store, block)).toMatchObject({
      kind: "event_unknown",
      detail: expect.stringContaining("the protocol-init fact is unreadable"),
    });
  });
});

describe("the protocol-init fact and the prune", () => {
  /** A covering store pruned through the init spend's slot. */
  const prunedThroughInit = async () => {
    const floor: { slot: number | null } = { slot: null };
    const { store, chain } = await follow({
      pruneFloors: [{ name: "test", floor: () => Promise.resolve(floor.slot) }],
    });
    const initSlot = await landInit(chain);
    for (let block = 0; block < K + 3; block++) await chain.forward([]);
    floor.slot = initSlot;
    await store.prune();
    expect((await store.cursor())!.prunedThroughSlot).toBe(initSlot);
    return { store, initSlot };
  };

  it("outlives the prune: an absent deposit is still invalid, an absent forced order waits", async () => {
    const { store, initSlot } = await prunedThroughInit();
    expect(await store.protocolInit(ONE_SHOT)).toMatchObject({
      slot: initSlot,
    });
    const deposit = await earlyBlock(store, strangerDepositBlock);
    expect(await replayed(store, deposit)).toMatchObject({ kind: "invalid" });
    const order = await earlyBlock(store, strangerOrderBlock);
    expect(await replayed(store, order)).toMatchObject({
      kind: "event_unknown",
      detail: expect.stringContaining(
        `the facts are pruned through slot ${initSlot.toString()}, past slot ${(initSlot - 1).toString()}`,
      ),
    });
  });

  it("keeps T(B) as the start for a block after the init spend: an absent forced order is invalid with the prune at T(B)", async () => {
    const { store, initSlot } = await prunedThroughInit();
    const block = await strangerOrderBlock(startFor(initSlot));
    await reach(store, block);
    expect(await replayed(store, block, initSlot)).toMatchObject({
      kind: "invalid",
      detail: expect.stringMatching(
        /^forced order .* is not admitted at the view/,
      ),
    });
  });
});

describe("a follower store without the protocol-init spend at the view", () => {
  it("keeps the late-origin hold when the store never saw the spend", async () => {
    const { store, chain } = await follow();
    await chain.forward([]);
    const block = await earlyBlock(store, strangerDepositBlock);
    expect(await replayed(store, block)).toMatchObject(await lateOrigin(store));
  });

  it("keeps the late-origin hold once a rollback removed the spend", async () => {
    const { store, chain } = await follow();
    await landInit(chain);
    await chain.forward([]);
    await chain.backward(2);
    await chain.forward([]);
    expect(await store.protocolInit(ONE_SHOT)).toBeNull();
    const block = await earlyBlock(store, strangerDepositBlock);
    expect(await replayed(store, block)).toMatchObject(await lateOrigin(store));
  });

  it("keeps the late-origin hold when the spend landed after the view", async () => {
    const { store, chain } = await follow();
    await chain.forward([]);
    const block = await earlyBlock(store, strangerDepositBlock);
    const input = await inputFor(store, block);
    await landInit(chain);
    expect(await replayed(store, block, undefined, input)).toMatchObject(
      await lateOrigin(store),
    );
  });
});
