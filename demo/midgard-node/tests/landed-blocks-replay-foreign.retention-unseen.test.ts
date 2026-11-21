/**
 * The production foreign replayer on a real follower store, for a deposit or
 * withdrawal whose payload is external and whose admission references no
 * retention output the follower's facts hold with that payload.
 *
 * The follower refuses it as `retention_output_unseen` only where a
 * referenced output is absent from its facts, and its refusal names each
 * such output (`txHash#index`); a block naming the order is then judged
 * against a store whose origin is too late for it (`origin_after_frontier`),
 * the detail naming the same outputs, as long as the refusal is in the facts
 * at the view, whatever the prune did to the block's forced orders. Where
 * every referenced output is in the facts, at another address, without an
 * inline datum or with another payload, the order is `malformed` and a
 * block naming it is `invalid`.
 */
import type {
  FactStore,
  FactStoreOptions,
  OutRef,
} from "@al-ft/midgard-l1-follower";
import type { EventKind } from "@al-ft/midgard-l1-follower/events";
import type { SimOutput } from "@al-ft/midgard-l1-follower/testing";
import { afterEach, beforeEach, describe, expect, it, vi } from "vitest";

import { db } from "./helpers/forced-orders-node-store.js";
import {
  admissionTx,
  eventKeyOf,
  eventOrder,
  EVENTS_CONFIG,
} from "./helpers/l1-events-chain.js";
import type { ChainDriver } from "./helpers/l1-events-store.js";
import {
  clock,
  emptyBlock,
  followChain,
  idKey,
  K,
  naming,
  nonceRef,
} from "./landed-blocks-replay-foreign.fixture.js";
import {
  originSlot,
  reach,
  replayAt,
  SLOT_MS,
  startFor,
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

/** A referenced output the follower's facts never held. */
const ABSENT: OutRef = { txHash: Buffer.alloc(32, 0xab), index: 3 };
const ABSENT_TEXT = `${"ab".repeat(32)}#3`;

const retentionAddress = (kind: EventKind) =>
  Buffer.from(
    EVENTS_CONFIG.lists.find((list) => list.kind === kind)!.retentionAddress,
    "hex",
  );

/** Creates `output` in its own block; its outref. */
const create = async (chain: ChainDriver, output: SimOutput) => {
  const [hash] = await chain.forward([
    { inputs: [nonceRef()], outputs: [output], nonce: chain.chain.nonce() },
  ]);
  return { txHash: hash!, index: 0 };
};

/**
 * Admits an external `kind` order included at `inclusionTime` whose
 * admission references `references` (made from the order's own retention
 * output, which none of them need hold).
 */
const admitReferencing = async (
  chain: ChainDriver,
  kind: EventKind,
  inclusionTime: bigint,
  references: (retained: SimOutput) => Promise<readonly OutRef[]>,
) => {
  const nonce = nonceRef();
  const order = eventOrder(kind, nonce, { external: true, inclusionTime });
  const referenceInputs = await references(order.retained!);
  await chain.forward([
    { ...admissionTx(order, chain.chain.nonce()), referenceInputs },
  ]);
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

/** The follower's refusals of the `kind` order of `nonce`, as (reason, detail). */
const refusalsOf = (store: FactStore, nonce: OutRef) =>
  store.transaction("read", async (tx) =>
    (
      await tx.query(
        "SELECT reason, detail FROM node_l1_event_refusals WHERE event_key = ?",
        [Buffer.from(eventKeyOf(nonce), "hex")],
      )
    ).map((row) => [String(row.reason), String(row.detail)]),
  );

/** Follows, admits per `references`, and replays a block naming the order. */
const judged = async (
  kind: EventKind,
  references: (
    chain: ChainDriver,
    retained: SimOutput,
  ) => Promise<readonly OutRef[]>,
) => {
  const { store, chain } = await follow();
  const start = startFor(await originSlot(store));
  const { nonce } = await admitReferencing(
    chain,
    kind,
    start + SLOT_MS,
    (retained) => references(chain, retained),
  );
  const block = await namingBlock(kind, nonce, start);
  await reach(store, block);
  return {
    refusals: await refusalsOf(store, nonce),
    verdict: await replayAt(store, block),
  };
};

const malformedAndInvalid = {
  verdict: {
    kind: "invalid",
    detail: expect.stringContaining("whose facts cover the block's range"),
  },
};

describe("an external order whose referenced outputs are all in the follower's facts", () => {
  it("refuses as malformed, and judges invalid a block naming, a withdrawal whose referenced output sits at the deposit retention address", async () => {
    const { refusals, verdict } = await judged(
      "withdrawal",
      async (chain, retained) => [
        await create(chain, {
          ...retained,
          address: retentionAddress("deposit"),
        }),
      ],
    );
    expect(refusals.map(([reason]) => reason)).toEqual(["malformed"]);
    expect({ verdict }).toMatchObject(malformedAndInvalid);
  });

  it("refuses as malformed, and judges invalid a block naming, a deposit whose referenced retention output holds another order's payload", async () => {
    const { refusals, verdict } = await judged("deposit", async (chain) => [
      await create(
        chain,
        eventOrder("deposit", nonceRef(), {
          external: true,
          inclusionTime: 5n,
        }).retained!,
      ),
    ]);
    expect(refusals.map(([reason]) => reason)).toEqual(["malformed"]);
    expect({ verdict }).toMatchObject(malformedAndInvalid);
  });

  it("refuses as malformed, and judges invalid a block naming, a deposit whose referenced retention output has no inline datum", async () => {
    const { refusals, verdict } = await judged(
      "deposit",
      async (chain, retained) => [
        await create(chain, {
          address: retained.address,
          lovelace: retained.lovelace,
        }),
      ],
    );
    expect(refusals.map(([reason]) => reason)).toEqual(["malformed"]);
    expect({ verdict }).toMatchObject(malformedAndInvalid);
  });
});

describe("an external order referencing an output absent from the follower's facts", () => {
  it.each(["deposit", "withdrawal"] as const)(
    "names the absent output in the follower's refusal of the %s and in the late-origin judgement",
    async (kind) => {
      const { refusals, verdict } = await judged(kind, () =>
        Promise.resolve([ABSENT]),
      );
      expect(refusals).toEqual([["retention_output_unseen", ABSENT_TEXT]]);
      expect(verdict).toMatchObject({
        kind: "origin_after_frontier",
        detail: expect.stringMatching(
          new RegExp(
            `^${kind} .* was refused by the follower: .*; referenced outputs absent from the facts: ${ABSENT_TEXT}$`,
          ),
        ) as unknown,
      });
    },
  );

  it("names only the absent output where another referenced output is in the facts with another payload", async () => {
    let present = "";
    const { refusals, verdict } = await judged("deposit", async (chain) => {
      const other = await create(
        chain,
        eventOrder("deposit", nonceRef(), {
          external: true,
          inclusionTime: 5n,
        }).retained!,
      );
      present = `${other.txHash.toString("hex")}#0`;
      return [other, ABSENT];
    });
    expect(refusals).toEqual([["retention_output_unseen", ABSENT_TEXT]]);
    expect(verdict).toMatchObject({
      kind: "origin_after_frontier",
      detail: expect.stringMatching(
        new RegExp(`referenced outputs absent from the facts: ${ABSENT_TEXT}$`),
      ) as unknown,
    });
    expect((verdict as { detail: string }).detail).not.toContain(present);
  });
});

describe("a refused order while the prune passed the slot the block's events are admitted after", () => {
  // The refusal is in the facts at the view, so the store's lack of the
  // output is known; that the prune may have dropped a forced order or
  // another refusal does not make it a wait.
  it("judges a block naming the deposit against a store whose origin is too late, not a wait", async () => {
    const floor: { slot: number | null } = { slot: null };
    const { store, chain } = await follow({
      pruneFloors: [{ name: "test", floor: () => Promise.resolve(floor.slot) }],
    });
    const origin = await originSlot(store);
    const start = startFor(origin);
    await chain.forward([]);
    const { nonce, admittedAt } = await admitReferencing(
      chain,
      "deposit",
      start + SLOT_MS,
      () => Promise.resolve([ABSENT]),
    );
    for (let block = 0; block < K + 3; block++) await chain.forward([]);
    floor.slot = admittedAt - 1;
    await store.prune();
    const pruned = (await store.cursor())!.prunedThroughSlot;
    expect(pruned).toBe(floor.slot);
    expect(pruned).toBeGreaterThan(origin);
    expect(await refusalsOf(store, nonce)).toEqual([
      ["retention_output_unseen", ABSENT_TEXT],
    ]);
    const block = await namingBlock("deposit", nonce, start);
    await reach(store, block);
    expect(await replayAt(store, block)).toMatchObject({
      kind: "origin_after_frontier",
      detail: expect.stringContaining(ABSENT_TEXT),
    });
  });
});
