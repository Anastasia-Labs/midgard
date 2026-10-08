/**
 * What the simulated node does around landed-block processing (N3): it
 * admits pending transactions (one acceptance receipt per accepted batch),
 * commits its own block on the processed tip (the journal, then the
 * working-ledger move onto it), finalizes its journal once the block is
 * merged (the transactions it included leave the mempool; landed-block
 * processing folds it into `confirmed_ledger`), and resolves its active
 * journal the way the node's journal
 * resolution does: abandoned once its base is no longer the processed tip,
 * revived when an abandoned block lands anyway. Every resolution ends with
 * the working ledger and native MPF moved onto the processed chain.
 */
import { Effect } from "effect";

import { MempoolDB } from "../../src/database/index.js";
import { moveNativeRoot, rebaseSql } from "../../src/landed-blocks/rebase.js";
import { rebaseTargetOf } from "../../src/landed-blocks/rebase-target.js";
import { retrieveRows } from "../../src/landed-blocks/store.js";
import type { Database } from "../../src/services/database.js";
import { withHistoryWrite } from "../../src/services/event-history-producer.js";
import { makeOutRefCbor } from "../midgard-output-helpers.js";
import {
  admitPending,
  insertSimReceipt,
  type SimPendingTx,
} from "./landed-blocks-sim.mempool.js";
import {
  insertOwnJournal,
  JournalStatus,
  ownBlockOn,
  setJournalStatus,
} from "./landed-blocks-sim.own.js";
import type { LandedSimEnv } from "./landed-blocks-sim.ports.js";
import { simDigest, simOutput } from "./landed-blocks-sim.universe.js";

const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");

type Run = <A, E>(effect: Effect.Effect<A, E, Database>) => Promise<A>;

export const simNode = (env: LandedSimEnv, run: Run) => {
  const { stats, mempool, book } = env;

  /** Moves the working ledger and native MPF onto the processed chain (and live own block). */
  const rebaseOnto = async () => {
    const plan = await run(Effect.flatMap(retrieveRows, rebaseTargetOf));
    if (plan.kind !== "ready") return `no rebase target: ${plan.detail}`;
    await run(
      moveNativeRoot(env.owner.current, plan.target, {
        assertCurrent: Effect.void,
      }),
    );
    await run(withHistoryWrite(rebaseSql(plan.target)));
    if (plan.target.live !== undefined) stats.liveRebases += 1;
    return plan.target;
  };

  /** Abandons the active journal (its base left the processed tip). */
  const abandonActive = async (canonical: ReadonlySet<string>) => {
    const active = book.active!;
    await run(setJournalStatus(active, JournalStatus.Abandoned));
    book.active = undefined;
    stats.ownResolutions += 1;
    if (!canonical.has(book.blocks.get(active)!.parentHash))
      stats.ownOnRemovedBase += 1;
  };

  /**
   * An own merged block's local finalization, as far as the simulation sees
   * it: its journal is locally applied and the transactions it included
   * leave the mempool.
   */
  const finalizeOwn = async (header: string) => {
    await run(setJournalStatus(header, JournalStatus.LocallyApplied));
    const { txIds } = book.blocks.get(header)!;
    if (txIds.length > 0)
      await run(withHistoryWrite(MempoolDB.clearTxs([...txIds])));
    const ids = new Set(txIds.map(hex));
    mempool.survivors = mempool.survivors.filter((tx) => !ids.has(hex(tx.id)));
    stats.ownMerges += 1;
  };

  /** Revives an abandoned own block that landed (finalized if merged). */
  const revive = async (
    header: string,
    merged: boolean,
    canonical: ReadonlySet<string>,
  ) => {
    if (book.active !== undefined && book.active !== header)
      await abandonActive(canonical);
    if (merged) await finalizeOwn(header);
    else
      await run(setJournalStatus(header, JournalStatus.SubmittedUnconfirmed));
    book.active = merged ? undefined : header;
    stats.ownRevivals += 1;
  };

  /** The active journal's block was merged: its journal finalizes. */
  const finalizeMerged = async (rooted: ReadonlySet<string>) => {
    if (book.active === undefined || !rooted.has(book.active)) return;
    await finalizeOwn(book.active);
    book.active = undefined;
  };

  /**
   * Admits pending chains (one spending `tip`'s `X`, one spending that) and
   * single spends of `Y` and the pool, every third admission as one batch.
   */
  const admit = async (
    tip: Readonly<{ h: number; b: number }>,
    ledger: ReadonlyMap<string, Buffer>,
  ) => {
    const spentBySurvivor = new Set(
      mempool.survivors.flatMap((tx) => tx.spent.map(hex)),
    );
    const next = (spent: Buffer[]): SimPendingTx => {
      mempool.admitted += 1;
      const id = simDigest(`tx:${env.label}:${mempool.admitted}`);
      return {
        id,
        spent,
        produced: [
          {
            outref: makeOutRefCbor(id, 0),
            output: simOutput(9_000_000n + BigInt(mempool.admitted)),
          },
        ],
        at: new Date(
          Date.parse("2026-10-01T00:00:00.000Z") + mempool.admitted * 1_000,
        ),
      };
    };
    const free = (outRef: Buffer) =>
      ledger.has(hex(outRef)) && !spentBySurvivor.has(hex(outRef));
    const txs: SimPendingTx[] = [];
    if (tip.h >= 1) {
      const x = env.universe.x(tip.h, tip.b).outref;
      if (free(x)) {
        const first = next([x]);
        txs.push(first, next([first.produced[0]!.outref]));
      }
      const y = env.universe.y(tip.h).outref;
      if (free(y)) txs.push(next([y]));
    }
    const pool = env.universe.pool[mempool.poolUsed];
    if (pool !== undefined && free(pool.outref)) {
      mempool.poolUsed += 1;
      txs.push(next([pool.outref]));
    }
    if (txs.length === 0) return;
    await run(admitPending(txs));
    const batches =
      txs.length > 1 && mempool.admitted % 3 === 0
        ? [txs]
        : txs.map((tx) => [tx]);
    for (const batch of batches) {
      await run(insertSimReceipt(batch.map((tx) => tx.id)));
      mempool.receipts.push({
        ids: batch.map((tx) => hex(tx.id)),
        reversed: false,
      });
    }
    mempool.survivors.push(...txs);
    stats.admitted += txs.length;
  };

  /**
   * The traffic's block this node may commit on `tip`: one it has not
   * committed, and that it never processed (no row of it is left).
   */
  const candidateOn = async (tip: string) => {
    const rows = new Set(
      (await run(retrieveRows)).map((row) => row.headerHash),
    );
    for (const [hash, info] of env.registry)
      if (
        info.prevHeaderHash === tip &&
        info.ownHeader !== undefined &&
        !book.blocks.has(hash) &&
        !rows.has(hash)
      )
        return hash;
    return undefined;
  };

  /** Commits an own block on the processed tip `tip`, the queue's tail. */
  const commit = async (tip: string, candidate: string | undefined) => {
    const { block, info, journal } = ownBlockOn(
      env.universe,
      env.registry,
      book,
      tip,
      mempool.survivors,
      candidate,
    );
    await run(insertOwnJournal(journal));
    env.registry.set(block.headerHash, info);
    book.blocks.set(block.headerHash, block);
    book.active = block.headerHash;
    stats.ownCommits += 1;
    const target = await rebaseOnto();
    if (typeof target === "string") return target;
    if (target.live?.headerHash !== block.headerHash)
      return `the commit's rebase target holds live block ${target.live?.headerHash ?? "none"}, not ${block.headerHash}`;
    return block;
  };

  return {
    rebaseOnto,
    abandonActive,
    revive,
    finalizeMerged,
    admit,
    candidateOn,
    commit,
  };
};
