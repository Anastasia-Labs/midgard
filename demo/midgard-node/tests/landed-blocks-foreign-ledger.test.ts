/**
 * Foreign landed blocks replayed by the production replayer on the node's
 * ledger store, whose outputs come from the node database by out-ref:
 * `confirmed_ledger` first, then the produced outputs of landed blocks not
 * yet folded. An honest block is processed without reading the whole
 * confirmed ledger, its ledger after it kept before its row; an output no
 * table has is a local fault on the block, never a verdict; and a crash
 * between keeping the ledger and writing the row leaves only a root the
 * next `retain` drops, the restart processing the block again.
 */
import { EMPTY_LEDGER_ROOT } from "@al-ft/midgard-fault-proofs/ledger-store";
import { storedNodesOf } from "@al-ft/midgard-fault-proofs/test-support/ledger-store-conformance";
import type { FactStore } from "@al-ft/midgard-l1-follower";
import * as SDKStateQueue from "@al-ft/midgard-sdk/state-queue";
import { Effect } from "effect";
import { afterEach, beforeEach, describe, expect, it } from "vitest";

import { isRetriedHold, localFaultSubject } from "../src/l1-events/driver.js";
import {
  LANDED_BLOCK_LOCAL_FAULT,
  LANDED_BLOCK_REBASE_PENDING,
  LANDED_BLOCK_REPLAY_FAILED,
} from "../src/landed-blocks/holds.js";
import { retainedLedgerRoots } from "../src/landed-blocks/ledger-retention.js";
import { processLandedQueue } from "../src/landed-blocks/process.js";
import { insertRow, retrieveRows } from "../src/landed-blocks/store.js";
import { withFollowerWrite } from "../src/services/follower-write-gate.js";
import { simHeader } from "./helpers/state-queue-sim.fixtures.js";
import {
  confirmedState,
  foreignPorts,
  fullScans,
  landedQueue,
  nodeLedgers,
  opening,
  retainLedgerOf,
  seed,
  spendingBlock,
} from "./landed-blocks-foreign-ledger.fixture.js";
import { localFaultAlarms } from "./landed-blocks-own.fixture.js";
import { attested } from "./landed-blocks-replay-foreign.availability.js";
import {
  followChain,
  inNode,
  retain,
  retainedRow,
} from "./landed-blocks-replay-foreign.fixture.js";
import { resetApplicationTables } from "./utils.js";

const hexOf = (bytes: Uint8Array) => Buffer.from(bytes).toString("hex");

const rowsByHash = retrieveRows.pipe(
  Effect.map((rows) =>
    rows.map((row) => [row.headerHash, row.kind, row.state] as const),
  ),
);

describe("foreign landed blocks on the node's ledger store", () => {
  const opened: FactStore[] = [];
  beforeEach(async () => {
    await inNode(resetApplicationTables);
  });
  afterEach(async () => {
    await Promise.all(opened.splice(0).map((store) => store.close()));
  });

  /** The spending block on the queue root at its parent, its DA retained,
   * with that root's ledger retained and `confirmed` in `confirmed_ledger`. */
  const onConfirmedRoot = async (confirmed: "spent" | "empty") => {
    const { store: follower } = await followChain();
    opened.push(follower);
    const block = await spendingBlock("11".repeat(28));
    await retain(await retainedRow(block.payload));
    const root = confirmedState(
      block.header.prevHeaderHash,
      block.prior,
      block.header.startTime,
    );
    const queue = await landedQueue(follower, root, [
      { ...block, status: await attested(block.payload) },
    ]);
    const setUp = Effect.gen(function* () {
      yield* seed(confirmed === "spent" ? [block.spent] : [], {
        headerHash: root.headerHash,
        utxosRoot: root.utxoRoot,
      });
      const ledgers = yield* nodeLedgers;
      yield* Effect.promise(() => retainLedgerOf(ledgers.store, [block.spent]));
      return ledgers;
    });
    return { follower, block, queue, setUp };
  };

  it("processes an honest block spending a confirmed_ledger output without scanning the confirmed ledger, keeping the ledger after it", async () => {
    const { follower, block, queue, setUp } = await onConfirmedRoot("spent");
    await inNode(
      Effect.gen(function* () {
        const { store } = yield* setUp;
        const before = yield* fullScans;
        const held = yield* processLandedQueue(
          foreignPorts(follower, store),
          queue,
        );
        expect(held?.reason).toBe(LANDED_BLOCK_REBASE_PENDING);
        expect(yield* rowsByHash).toEqual([
          [block.hash, "foreign", "processed"],
        ]);
        expect((yield* fullScans) - before).toBe(0);
        // The row records the block's net delta: the output it spent, and
        // exactly what the kept ledger after it holds.
        const [row] = yield* retrieveRows;
        expect(row!.spent.map(hexOf)).toEqual([hexOf(block.spent.outref)]);
        const kept = yield* Effect.promise(async () => {
          const after = await store.at(block.header.utxosRoot);
          const entries: string[] = [];
          for await (const { outRef, output } of after.entries())
            entries.push(`${hexOf(outRef)}=${hexOf(output)}`);
          return entries.sort();
        });
        expect(kept).toHaveLength(1);
        expect(
          row!.produced
            .map(({ outref, output }) => `${hexOf(outref)}=${hexOf(output)}`)
            .sort(),
        ).toEqual(kept);
      }),
    );
  }, 120_000);

  it("processes a block spending an output a landed block not yet folded produced, read from that block's row", async () => {
    const { store: follower } = await followChain();
    opened.push(follower);
    const rootHash = "22".repeat(28);
    const probe = await spendingBlock(rootHash);
    const parentHeader = {
      ...simHeader(7, rootHash),
      prevUtxosRoot: EMPTY_LEDGER_ROOT,
      utxosRoot: probe.prior,
      startTime: 0n,
      endTime: probe.header.startTime,
    };
    const parentHash = SDKStateQueue.stateQueueHeaderHash(parentHeader);
    const block = await spendingBlock(parentHash);
    await retain(await retainedRow(block.payload));
    const queue = await landedQueue(
      follower,
      confirmedState(rootHash, EMPTY_LEDGER_ROOT, 0n),
      [
        { hash: parentHash, header: parentHeader, status: "Unattested" },
        { ...block, status: await attested(block.payload) },
      ],
    );
    await inNode(
      Effect.gen(function* () {
        // `confirmed_ledger` is empty: the spent output is in the parent's
        // processed row alone.
        yield* seed([], { headerHash: rootHash, utxosRoot: EMPTY_LEDGER_ROOT });
        yield* withFollowerWrite(
          insertRow({
            headerHash: parentHash,
            parentHeaderHash: rootHash,
            parentUtxosRoot: EMPTY_LEDGER_ROOT,
            utxosRoot: block.prior,
            kind: "foreign",
            state: "processed",
            applied: true,
            spent: [],
            produced: [block.spent],
            depositIds: [],
            withdrawals: [],
            forcedIds: [],
            txIds: [],
          }),
        );
        const { store } = yield* nodeLedgers;
        yield* Effect.promise(() => retainLedgerOf(store, [block.spent]));
        const held = yield* processLandedQueue(
          foreignPorts(follower, store),
          queue,
        );
        expect(held?.reason).toBe(LANDED_BLOCK_REBASE_PENDING);
        const rows = yield* rowsByHash;
        expect(rows).toHaveLength(2);
        expect(rows).toContainEqual([parentHash, "foreign", "processed"]);
        expect(rows).toContainEqual([block.hash, "foreign", "processed"]);
      }),
    );
  }, 120_000);

  it("holds a block whose parent ledger holds an output no table has as a retried local fault with the alarm, recording nothing", async () => {
    const { follower, block, queue, setUp } = await onConfirmedRoot("empty");
    await inNode(
      Effect.gen(function* () {
        const { store } = yield* setUp;
        const alarms = yield* localFaultAlarms();
        const held = yield* processLandedQueue(
          foreignPorts(follower, store),
          queue,
        );
        expect(held?.reason).toBe(LANDED_BLOCK_LOCAL_FAULT);
        expect(isRetriedHold(held!)).toBe(true);
        expect(localFaultSubject(held!)).toBe(block.hash);
        expect(held?.detail).toContain("LedgerOutputMissing");
        expect((yield* localFaultAlarms()) - alarms).toBe(1n);
        expect(yield* rowsByHash).toEqual([]);
        expect(
          yield* Effect.promise(() => opening(store, block.header.utxosRoot)),
        ).toBe("LedgerNotRetained");
      }),
    );
  }, 120_000);

  it("leaves only a root the next retain drops when the row write fails after the ledger is kept, and the restart processes the block", async () => {
    const { follower, block, queue, setUp } = await onConfirmedRoot("spent");
    await inNode(
      Effect.gen(function* () {
        const { storage, store } = yield* setUp;
        const base = foreignPorts(follower, store);
        let kept = false;
        let crashed = false;
        const crashing: typeof base = {
          ...base,
          persistLedger: (ledger) =>
            base
              .persistLedger(ledger)
              .pipe(Effect.tap(() => Effect.sync(() => (kept = true)))),
          write: (work) =>
            kept && !crashed
              ? Effect.sync(() => (crashed = true)).pipe(
                  Effect.zipRight(Effect.fail(new Error("row write lost"))),
                )
              : base.write(work),
        };
        const reachable = async (roots: ReadonlySet<string>) => {
          const nodes = new Set<string>();
          for (const root of roots)
            for (const hash of (await storedNodesOf(storage, root)).keys())
              nodes.add(hash);
          return nodes.size;
        };
        const retainAndCount = Effect.gen(function* () {
          const roots = yield* retainedLedgerRoots(base.queueHistory, queue, 0);
          yield* Effect.promise(() => store.retain(roots));
          const counts = yield* Effect.promise(() => storage.counts());
          return {
            roots,
            nodes: counts.nodes,
            reachable: yield* Effect.promise(() => reachable(roots)),
          };
        });

        const lost = yield* processLandedQueue(crashing, queue);
        expect(crashed).toBe(true);
        expect(lost?.reason).toBe(LANDED_BLOCK_REPLAY_FAILED);
        expect(yield* rowsByHash).toEqual([]);
        const after = block.header.utxosRoot;
        expect(yield* Effect.promise(() => opening(store, after))).toBe(
          "opened",
        );

        const dropped = yield* retainAndCount;
        expect(dropped.roots.has(after)).toBe(false);
        expect(yield* Effect.promise(() => opening(store, after))).toBe(
          "LedgerNotRetained",
        );
        expect(dropped.nodes).toBe(dropped.reachable);

        const restarted = yield* processLandedQueue(crashing, queue);
        expect(restarted?.reason).toBe(LANDED_BLOCK_REBASE_PENDING);
        expect(yield* rowsByHash).toEqual([
          [block.hash, "foreign", "processed"],
        ]);
        const again = yield* retainAndCount;
        expect(again.roots.has(after)).toBe(true);
        expect(yield* Effect.promise(() => opening(store, after))).toBe(
          "opened",
        );
        expect(again.nodes).toBe(again.reachable);
      }),
    );
  }, 120_000);
});
