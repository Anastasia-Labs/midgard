import { mkdtempSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";

import { chainPoint, IntersectNotFoundError } from "@al-ft/l1-node-transport";
import { afterAll, afterEach, describe, expect, it } from "vitest";

import {
  type BlockSummary,
  type Cursor,
  intersectionFailure,
  originAnchor,
  type OriginConfig,
  type Point,
  protocolInitStatus,
  startFromOrigin,
} from "../src/index.js";
import { type FakeBlock, fakeChains } from "./support/fake-chain.js";
import { testDatabases } from "./support/postgres.js";
import {
  chain,
  fill,
  ORIGIN,
  storeAdapters,
  tx,
} from "./support/small-chain.js";

const databases = testDatabases();
const scratch = mkdtempSync(join(tmpdir(), "l1-follower-origin-"));
const fakes = fakeChains();

afterEach(async () => {
  await fakes.cleanup();
});
afterAll(async () => {
  await databases.dropAll();
  rmSync(scratch, { recursive: true, force: true });
});

const adapters = storeAdapters(databases, scratch);
const hex = (bytes: Buffer): string => bytes.toString("hex");

/**
 * The small chain origin(100) <- b1(101) <- b2(103) <- b3(105). TX1 in b1
 * spends fill(0x01)#0: it stands for the protocol-init tx spending the
 * hubOracleOneShot outref. TX3 in b3 lists fill(0x02)#0 as an input but
 * fails phase 2, so it never spends it.
 */
const INIT_SPENT: OriginConfig["hubOracleOneShot"] = {
  txHash: fill(0x01),
  index: 0,
};
const NEVER_SPENT: OriginConfig["hubOracleOneShot"] = {
  txHash: fill(0x02),
  index: 0,
};
const blocks = chain();
const at = (index: number): BlockSummary => blocks[index] as BlockSummary;
const B1 = at(0).point;
const B3 = at(2).point;

/** The small chain as the fake node serves it; the gate never decodes the payloads. */
const served: FakeBlock[] = blocks.map((block) => ({
  slot: block.point.slot,
  hash: hex(block.point.hash),
  blockNo: block.height,
  prevHash: block.parentHash === null ? null : hex(block.parentHash),
  block: "80",
}));
const base = { slot: ORIGIN.point.slot, hash: hex(ORIGIN.point.hash) };

const tipError = (resuming: boolean) =>
  new IntersectNotFoundError(
    { point: chainPoint(105n, hex(B3.hash)), blockNo: 53n },
    resuming,
  );

describe("R4 origin_not_on_chain: classification", () => {
  it("a fresh start whose FindIntersect([O]) fails is R4", () => {
    expect(intersectionFailure(tipError(false), null, ORIGIN.point)).toEqual({
      kind: "intervention",
      reason: "origin_not_on_chain",
      detail: expect.stringMatching(
        /^l1Origin 100\.a0a0.* is not on the node's chain/u,
      ) as unknown,
    });
  });

  it("a resumed stream or a store with a cursor is R2, never R4", () => {
    const cursor = { origin: ORIGIN.point } as unknown as Cursor;
    for (const [error, stored] of [
      [tipError(true), null],
      [tipError(false), cursor],
    ] as const)
      expect(intersectionFailure(error, stored, ORIGIN.point).reason).toBe(
        "intersection_outside_history",
      );
  });

  it("the first block after O fixes O's height, and one that does not extend O is R4", () => {
    const first = {
      point: chainPoint(101n, hex(B1.hash)),
      blockNo: 51n,
      prevHash: hex(ORIGIN.point.hash),
    };
    expect(originAnchor(ORIGIN.point, first)).toEqual({
      point: ORIGIN.point,
      height: 50,
    });
    expect(
      originAnchor(ORIGIN.point, { ...first, prevHash: hex(fill(0xee)) }),
    ).toMatchObject({ kind: "intervention", reason: "origin_not_on_chain" });
  });
});

describe.each(adapters)(
  "origin gate over the fake sidecar ($name)",
  (adapter) => {
    const credit = 10;

    it("a wrong origin hash is R4; the store stays empty and the transport stays up", async () => {
      const { store } = await adapter.open(2);
      await store.start();
      const transport = await fakes.transport(base, served);
      const wrong: Point = { slot: ORIGIN.point.slot, hash: fill(0xee) };
      const result = await startFromOrigin({
        store,
        transport,
        origin: wrong,
        credit,
      });
      expect(result).toMatchObject({
        kind: "intervention",
        reason: "origin_not_on_chain",
      });
      expect(await store.cursor()).toBeNull();
      // Unready, not dead: the sidecar session lives on for the next attempt.
      expect(transport.readiness.ready).toBe(true);
      // The corrected origin starts on the same transport, with no restart.
      const corrected = await startFromOrigin({
        store,
        transport,
        origin: ORIGIN.point,
        credit,
      });
      expect(corrected).toMatchObject({ kind: "initialized" });
      if (corrected.kind === "initialized") await corrected.stream.close();
      await store.close();
    });

    it("the right origin initializes at O's height and hands back the first block", async () => {
      const { store } = await adapter.open(2);
      await store.start();
      const transport = await fakes.transport(base, served);
      const result = await startFromOrigin({
        store,
        transport,
        origin: ORIGIN.point,
        credit,
      });
      if (result.kind !== "initialized")
        throw new Error(`expected initialized, got ${JSON.stringify(result)}`);
      expect(result.cursor.height).toBe(ORIGIN.height);
      expect(result.cursor.origin.hash.equals(ORIGIN.point.hash)).toBe(true);
      expect(result.first.point).toEqual(chainPoint(101n, hex(B1.hash)));
      // The runner applies the first block, then follows on.
      for (const block of blocks)
        expect(await store.applyBlock(block)).toMatchObject({
          kind: "applied",
        });
      result.stream.ack(result.first.seq);
      await result.stream.close();
      expect(
        await protocolInitStatus(
          store,
          { origin: ORIGIN.point, hubOracleOneShot: INIT_SPENT },
          B3,
        ),
      ).toMatchObject({ kind: "seen", slot: B1.slot });
      // A restart with the same origin resumes; another origin is refused.
      expect(
        await startFromOrigin({
          store,
          transport,
          origin: ORIGIN.point,
          credit,
        }),
      ).toMatchObject({ kind: "resume" });
      // Another origin is the named unready reason, and the store is untouched.
      const mismatch = await startFromOrigin({
        store,
        transport,
        origin: B1,
        credit,
      });
      expect(mismatch).toEqual({
        kind: "intervention",
        reason: "origin_mismatch",
        detail: expect.stringMatching(
          /^the store was initialized at l1Origin 100\.a0a0.*, but the configured l1Origin is 101\./u,
        ) as unknown,
      });
      expect((await store.cursor())?.origin.slot).toBe(ORIGIN.point.slot);
      await store.close();
    });

    it("a cursor written between the check and initialize is origin_mismatch too", async () => {
      const { store } = await adapter.open(2);
      await store.start();
      const transport = await fakes.transport(base, served);
      // Another writer initializes the store at B1 after the cursor check.
      const racing = {
        ...store,
        cursor: async () => null,
        initialize: async () => {
          await store.initialize({ point: B1, height: ORIGIN.height + 1 });
          return store.initialize({
            point: ORIGIN.point,
            height: ORIGIN.height,
          });
        },
      } as typeof store;
      expect(
        await startFromOrigin({
          store: racing,
          transport,
          origin: ORIGIN.point,
          credit,
        }),
      ).toMatchObject({ kind: "intervention", reason: "origin_mismatch" });
      await store.close();
    });

    it("an origin after protocol init is R3 once caught up, pending before", async () => {
      const { store } = await adapter.open(2);
      await store.start();
      const transport = await fakes.transport(base, served);
      // O = b1: the protocol-init tx (TX1, in b1) lies at the origin, not after it.
      const result = await startFromOrigin({
        store,
        transport,
        origin: B1,
        credit,
      });
      if (result.kind !== "initialized")
        throw new Error(`expected initialized, got ${JSON.stringify(result)}`);
      await result.stream.close();
      const config = { origin: B1, hubOracleOneShot: INIT_SPENT };
      expect(await store.applyBlock(at(1))).toMatchObject({ kind: "applied" });
      expect(await protocolInitStatus(store, config, B3)).toMatchObject({
        kind: "pending",
      });
      expect(await store.applyBlock(at(2))).toMatchObject({ kind: "applied" });
      expect(await protocolInitStatus(store, config, B3)).toEqual({
        kind: "intervention",
        reason: "origin_after_protocol_init",
        detail: expect.stringMatching(
          /^caught up at 105\..* from l1Origin 101\..* without seeing the tx that spends hubOracleOneShot 0101.*#0/u,
        ) as unknown,
      });
      await store.close();
    });
  },
);

describe.each(adapters)("R3 completeness assertion ($name)", (adapter) => {
  const startedAt = async (
    origin: Readonly<{ point: Point; height: number }>,
    applied: readonly BlockSummary[],
  ) => {
    const { store } = await adapter.open(2);
    await store.start();
    expect(await store.initialize(origin)).toMatchObject({
      kind: "initialized",
    });
    for (const block of applied)
      expect(await store.applyBlock(block)).toMatchObject({ kind: "applied" });
    return store;
  };

  it("an origin before the init tx sees its spend: never R3", async () => {
    const store = await startedAt(ORIGIN, blocks);
    expect(
      await protocolInitStatus(
        store,
        { origin: ORIGIN.point, hubOracleOneShot: INIT_SPENT },
        B3,
      ),
    ).toEqual({ kind: "seen", txHash: blocks[0]!.txs[0]!.hash, slot: 101 });
    await store.close();
  });

  it("a phase-2-failed tx listing the outref as an input is not its spend", async () => {
    const store = await startedAt(ORIGIN, blocks);
    expect(
      await protocolInitStatus(
        store,
        { origin: ORIGIN.point, hubOracleOneShot: NEVER_SPENT },
        B3,
      ),
    ).toMatchObject({
      kind: "intervention",
      reason: "origin_after_protocol_init",
    });
    await store.close();
  });

  it("is not sticky: an init tx that lands later clears it", async () => {
    const store = await startedAt(ORIGIN, blocks);
    const config = { origin: ORIGIN.point, hubOracleOneShot: NEVER_SPENT };
    expect(await protocolInitStatus(store, config, B3)).toMatchObject({
      reason: "origin_after_protocol_init",
    });
    const b4: BlockSummary = {
      point: { slot: 107, hash: fill(0xc4) },
      height: 54,
      parentHash: B3.hash,
      txs: [
        tx(fill(0xb4), {
          inputs: [NEVER_SPENT],
          mint: new Map([[hex(fill(0x33, 28)), new Map([["aa", 1n]])]]),
        }),
      ],
    };
    expect(await store.applyBlock(b4)).toMatchObject({ kind: "applied" });
    expect(await protocolInitStatus(store, config, b4.point)).toMatchObject({
      kind: "seen",
      slot: 107,
    });
    await store.close();
  });

  it("an uninitialized store is pending, never R3", async () => {
    const { store } = await adapter.open(2);
    await store.start();
    expect(
      await protocolInitStatus(
        store,
        { origin: ORIGIN.point, hubOracleOneShot: INIT_SPENT },
        B3,
      ),
    ).toMatchObject({ kind: "pending" });
    await store.close();
  });
});
