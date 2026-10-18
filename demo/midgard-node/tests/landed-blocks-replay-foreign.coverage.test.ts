/**
 * The production foreign replayer on a real follower store: a deposit or
 * withdrawal a block names and the view does not know is the block's fault
 * (`invalid`) once the facts cover the block's range, and a bounded wait
 * (`event_unknown`) while they do not. The facts cover it when the
 * follower store is still in the view's generation, its origin is at or
 * before the earliest slot the block's events can be admitted at (`T(B)`:
 * the window's start less the event wait and the longest event validity
 * range) and the prune is still below the slot the block's node output
 * entered the facts at.
 *
 * The bench's slot configuration maps slot `s` to `s * 1000` ms; the view's
 * clock is moved forward so the view's horizon reaches each block's end.
 */
import type { FactStore } from "@al-ft/midgard-l1-follower";
import * as SDK from "@al-ft/midgard-sdk";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import type { ReplayInput } from "../src/landed-blocks/replay.js";
import { rebind } from "./foreign-block-import.fixture.js";
import { db } from "./helpers/forced-orders-node-store.js";
import {
  admissionTx,
  eventOrder,
  retirementTx,
} from "./helpers/l1-events-chain.js";
import type { ChainDriver } from "./helpers/l1-events-store.js";
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
  replayerFor,
  retain,
  retainedRow,
} from "./landed-blocks-replay-foreign.fixture.js";
import { resetApplicationTables } from "./utils.js";

const opened: FactStore[] = [];
const follow = async (...args: Parameters<typeof followChain>) => {
  const followed = await followChain(...args);
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

const SLOT_MS = 1_000n;

/** The window start whose earliest admission slot `T(B)` is `slot`. */
const startFor = (slot: number) =>
  BigInt(slot) * SLOT_MS +
  BigInt(SDK.EVENT_WAIT_DURATION_MS) +
  SDK.MAX_EVENT_VALIDITY_RANGE_LENGTH_MS;

/** `payload` over the window `(start, start + 1 s]`, committed at its end. */
const windowed = (payload: SDK.DaPayload, start: bigint) =>
  rebind({
    ...payload,
    block_body: {
      ...payload.block_body,
      header: {
        ...payload.block_body.header,
        startTime: start,
        endTime: start + SLOT_MS,
        blockSlot: (start + SLOT_MS) / SLOT_MS,
      },
    },
  });

/** Sets the view's clock so its horizon is `by` ms past the block's end. */
const reach = async (store: FactStore, payload: SDK.DaPayload, by = 0) => {
  const view = (await store.currentView())!;
  clock.offsetMs =
    Number(payload.block_body.header.endTime) -
    view.point.slot * 1_000 -
    SDK.EVENT_WAIT_DURATION_MS +
    1 +
    by;
};

const originSlot = async (store: FactStore) =>
  (await store.cursor())!.origin.slot;

/** Retains `payload` and replays it at the view, its node output entered at `enteredSlot` (default: the view's slot). */
const replayAt = async (
  store: FactStore,
  payload: SDK.DaPayload,
  enteredSlot?: number | null,
) => {
  await retain(await retainedRow(payload));
  const input: ReplayInput = await inputFor(store, payload);
  return inNode(
    replayerFor(store)(
      enteredSlot === undefined ? input : { ...input, enteredSlot },
    ),
  );
};

/** Admits a deposit included at `inclusionTime`; returns it with its order. */
const admitDeposit = async (chain: ChainDriver, inclusionTime: bigint) => {
  const order = eventOrder("deposit", nonceRef(), { inclusionTime });
  const [hash] = await chain.forward([admissionTx(order, chain.chain.nonce())]);
  const [known] = (await depositsAt(chain.store)).filter(
    ({ event }) => event.inclusionTime === inclusionTime,
  );
  return { order, outRef: { txHash: hash!, index: 0 }, entry: known!.entry };
};

/** A block over the window starting at `start` naming a deposit no one admitted. */
const strangerDepositBlock = async (start: bigint) =>
  windowed(
    await naming(await emptyBlock(), {
      deposits: [[idKey(nonceRef()), "00"]],
    }),
    start,
  );

describe("an absent deposit or withdrawal where the facts cover the block's range", () => {
  it("is invalid for a deposit, the origin at T(B) and the prune below the block's entry", async () => {
    const { store, chain } = await follow();
    await chain.forward([]);
    const block = await strangerDepositBlock(startFor(await originSlot(store)));
    await reach(store, block);
    expect(await replayAt(store, block)).toMatchObject({
      kind: "invalid",
      detail: expect.stringContaining("whose facts cover the block's range"),
    });
  });

  it("is invalid for a withdrawal", async () => {
    const { store, chain } = await follow();
    await chain.forward([]);
    const block = await windowed(
      await naming(await emptyBlock(), {
        withdrawals: [[idKey(nonceRef()), "00"]],
      }),
      startFor(await originSlot(store)),
    );
    await reach(store, block);
    expect(await replayAt(store, block)).toMatchObject({
      kind: "invalid",
      detail: expect.stringMatching(
        /^withdrawal .* is not admitted at the view/,
      ),
    });
  });

  it("accepts an honest deposit included at the window's end, after a view one block short of it waits", async () => {
    const { store, chain } = await follow();
    const start = startFor(await originSlot(store));
    const { entry } = await admitDeposit(chain, start + SLOT_MS);
    const block = await windowed(await depositBlock(entry), start);
    await reach(store, block, -1);
    expect(await replayAt(store, block)).toEqual({
      kind: "missing",
      detail: "the block ends past what the follower view can know",
    });
    await chain.forward([]);
    expect(await replayAt(store, block)).toMatchObject({
      kind: "replayed",
      depositIds: [Buffer.from(entry.idCbor, "hex")],
    });
  });
});

describe("a retired deposit and the prune at the block's entry", () => {
  /** A deposit retired after the block's entry at `entered`, then `K + 3` more blocks. */
  const retiredAfterEntry = async () => {
    const floor: { slot: number | null } = { slot: null };
    const { store, chain } = await follow({
      pruneFloors: [{ name: "test", floor: () => Promise.resolve(floor.slot) }],
    });
    const start = startFor(await originSlot(store));
    const { order, outRef, entry } = await admitDeposit(chain, start + SLOT_MS);
    const entered = chain.tip.point.slot;
    await chain.forward([
      retirementTx(order, outRef, "absorbed", chain.chain.nonce()),
    ]);
    const retiredAt = chain.tip.point.slot;
    for (let block = 0; block < K + 3; block++) await chain.forward([]);
    const block = await windowed(await depositBlock(entry), start);
    await reach(store, block);
    return { store, floor, block, entered, retiredAt };
  };

  it("finds a deposit retired after the block's entry with the prune one slot below it", async () => {
    const { store, floor, block, entered } = await retiredAfterEntry();
    floor.slot = entered - 1;
    await store.prune();
    expect((await store.cursor())!.prunedThroughSlot).toBe(entered - 1);
    expect(await replayAt(store, block, entered)).toMatchObject({
      kind: "replayed",
    });
  });

  it("waits, not invalid, once the prune reached the block's entry and the deposit is gone", async () => {
    const { store, floor, block, retiredAt } = await retiredAfterEntry();
    floor.slot = retiredAt;
    await store.prune();
    expect((await store.cursor())!.prunedThroughSlot).toBe(retiredAt);
    expect(await replayAt(store, block, retiredAt)).toMatchObject({
      kind: "event_unknown",
      detail: expect.stringContaining(
        `the facts are pruned through slot ${retiredAt.toString()}, not below the block's entry at slot ${retiredAt.toString()}`,
      ),
    });
  });
});

describe("the follower's origin against T(B)", () => {
  it("waits on an absent deposit when the origin is one slot after T(B)", async () => {
    const { store, chain } = await follow();
    await chain.forward([]);
    const origin = await originSlot(store);
    const block = await strangerDepositBlock(startFor(origin - 1));
    await reach(store, block);
    expect(await replayAt(store, block)).toMatchObject({
      kind: "event_unknown",
      detail: expect.stringContaining(
        `the follower's origin at slot ${origin.toString()} is after slot ${(origin - 1).toString()}`,
      ),
    });
  });

  it("accepts a block whose named deposit is known, whatever the origin", async () => {
    const { store, chain } = await follow();
    const start = startFor((await originSlot(store)) - 1);
    const { entry } = await admitDeposit(chain, start + SLOT_MS);
    const block = await windowed(await depositBlock(entry), start);
    await reach(store, block);
    expect(await replayAt(store, block)).toMatchObject({ kind: "replayed" });
  });

  it("waits on an absent deposit when the block's node output was seeded", async () => {
    const { store, chain } = await follow();
    await chain.forward([]);
    const block = await strangerDepositBlock(startFor(await originSlot(store)));
    await reach(store, block);
    expect(await replayAt(store, block, null)).toMatchObject({
      kind: "event_unknown",
      detail: expect.stringContaining(
        "the block's node output predates the follower's origin",
      ),
    });
  });
});

describe("the follower store's generation against the view's", () => {
  it("waits on an absent deposit when the follower store changed generation since the view", async () => {
    const { store, chain } = await follow();
    await chain.forward([]);
    const block = await strangerDepositBlock(startFor(await originSlot(store)));
    await reach(store, block);
    await retain(await retainedRow(block));
    const input = await inputFor(store, block);
    await chain.forward([]);
    await chain.backward(1);
    expect((await store.cursor())!.generation).not.toBe(input.view.generation);
    expect(await inNode(replayerFor(store)(input))).toMatchObject({
      kind: "event_unknown",
      detail: expect.stringContaining(
        "the follower store changed generation since the view",
      ),
    });
  });
});
