/**
 * A block whose parent's ledger this node cannot give, neither from its
 * store nor rebuilt by the processed chain's deltas, holds as this node's
 * own local fault with its alarm, and nothing is recorded for it: when the
 * store retains no root on the chain to the parent, when a delta on that
 * chain does not apply to the retained ledger, and when the deltas reach
 * another root than the parent's.
 */
import * as SDK from "@al-ft/midgard-sdk/core";
import { Effect } from "effect";
import { expect, it } from "vitest";

import { LANDED_BLOCK_LOCAL_FAULT } from "../src/landed-blocks/holds.js";
import { processLandedQueue } from "../src/landed-blocks/process.js";
import {
  insertRow,
  type LandedBlockRow,
  retrieveRows,
} from "../src/landed-blocks/store.js";
import { simDigest } from "./helpers/landed-blocks-sim.universe.js";
import {
  A1,
  type Block,
  block,
  entry,
  fixture,
  G0,
  G1,
  harness,
  inInstalledNode,
  localFaultAlarms,
  ports,
  queueOf,
  rootOf,
} from "./landed-blocks-own.fixture.js";

const B1 = entry("b1", 5_000_000n);

/** `a`, own block `b` on it with its journal, and a harness that holds
 * both journals. */
const setup = async () => {
  const fx = await fixture();
  const { a } = fx;
  const b = block(
    3,
    { hash: a.hash, root: a.header.utxosRoot, endTime: a.header.endTime },
    await rootOf([G1, B1]),
  );
  const state = harness();
  state.journals.set(a.hash, fx.journalA);
  state.journals.set(b.hash, {
    baseTailHeaderHash: a.hash,
    baseUtxosRoot: a.header.utxosRoot,
    expectedUtxosRoot: b.header.utxosRoot,
    spent: [A1.outref],
    produced: [B1],
    depositIds: [],
    withdrawals: [],
    forcedIds: [],
    txIds: [simDigest("own:tx-b")],
  });
  const process = (nodes: readonly Block[]) =>
    processLandedQueue(ports(state), queueOf(fx.genesisState, nodes));
  return { ...fx, b, state, process };
};

/** Processes `[a, b]` and expects `b` held as a local fault whose detail
 * names `why`, with one alarm and no row for `b`. */
const expectHeld = (
  { a, b, process }: Awaited<ReturnType<typeof setup>>,
  why: string,
) =>
  Effect.gen(function* () {
    const alarms = yield* localFaultAlarms();
    const held = yield* process([a, b]);
    expect(held?.reason).toBe(LANDED_BLOCK_LOCAL_FAULT);
    expect(held?.detail).toContain(why);
    expect((yield* localFaultAlarms()) - alarms).toBe(1n);
    expect((yield* retrieveRows).map((row) => row.headerHash)).toEqual([
      a.hash,
    ]);
  });

/** A processed row for `a` on genesis carrying the delta `spent`/`produced`
 * instead of `a`'s; the store never held `a`'s root. */
const rowOfA = (
  a: Block,
  genesisRoot: string,
  delta: Pick<LandedBlockRow, "spent" | "produced">,
): LandedBlockRow => ({
  headerHash: a.hash,
  parentHeaderHash: SDK.GENESIS_HEADER_HASH,
  parentUtxosRoot: genesisRoot,
  utxosRoot: a.header.utxosRoot,
  kind: "foreign",
  state: "processed",
  applied: false,
  ...delta,
  depositIds: [],
  withdrawals: [],
  forcedIds: [],
  txIds: [],
});

it("holds a block as a local fault when the store retains no root on the processed chain to its parent", async () => {
  const fx = await setup();
  await inInstalledNode(fx.state)(
    Effect.gen(function* () {
      yield* fx.process([fx.a]);
      const ledgers = yield* Effect.promise(() => fx.state.ledgers);
      yield* Effect.promise(() => ledgers.retain(new Set()));
      yield* expectHeld(fx, "retains neither");
    }),
  );
}, 120_000);

it("holds a block as a local fault when a processed delta to its parent does not apply to the retained ledger", async () => {
  const fx = await setup();
  await inInstalledNode(fx.state)(
    Effect.gen(function* () {
      // Spends an out-ref genesis does not hold.
      yield* insertRow(
        rowOfA(fx.a, fx.genesisRoot, { spent: [A1.outref], produced: [A1] }),
      );
      yield* expectHeld(fx, "do not apply to the retained ledger");
    }),
  );
}, 120_000);

it("holds a block as a local fault when the processed deltas reach another root than its parent's", async () => {
  const fx = await setup();
  await inInstalledNode(fx.state)(
    Effect.gen(function* () {
      yield* insertRow(
        rowOfA(fx.a, fx.genesisRoot, {
          spent: [G0.outref],
          produced: [entry("other", 9_000_000n)],
        }),
      );
      yield* expectHeld(fx, `not ${fx.a.header.utxosRoot}`);
    }),
  );
}, 120_000);
