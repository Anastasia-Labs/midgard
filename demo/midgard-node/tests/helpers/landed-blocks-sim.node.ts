/**
 * What the simulated node does around landed-block processing (N3, I3): it
 * admits pending transactions, commits its own block on the processed tip (the journal, then the
 * working-ledger move onto it), and finalizes its journal once the block is
 * merged (landed-block processing folds it into `confirmed_ledger`; the
 * transactions it included stay marked until that fold is final). The
 * rebase disposes of and revives its journals; the node keeps its book in
 * step with that, finalizes a revived block locally the way the commit path
 * does, and stands in for S6 deriving an active commit dead when its base
 * left without a rebase.
 */
import { Effect } from "effect";

import { MempoolTxDeltasDB } from "../../src/database/index.js";
import {
  disposeJournals,
  type OwnJournalDisposition,
} from "../../src/landed-blocks/own-journals.js";
import { moveNativeRoot, rebaseSql } from "../../src/landed-blocks/rebase.js";
import { rebaseTargetOf } from "../../src/landed-blocks/rebase-target.js";
import { retrieveRows } from "../../src/landed-blocks/store.js";
import type { Database } from "../../src/services/database.js";
import { withFollowerWrite } from "../../src/services/follower-write-gate.js";
import { makeOutRefCbor } from "../midgard-output-helpers.js";
import {
  admitPending,
  clearRevivedRejections,
  restoreDisposedMembers,
  type SimPendingTx,
} from "./landed-blocks-sim.mempool.js";
import {
  insertOwnJournal,
  JournalStatus,
  journalStatuses,
  ownBlockOn,
  setJournalStatus,
} from "./landed-blocks-sim.own.js";
import type { LandedSimEnv } from "./landed-blocks-sim.ports.js";
import { simDigest, simOutput } from "./landed-blocks-sim.universe.js";

const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");

type Run = <A, E>(effect: Effect.Effect<A, E, Database>) => Promise<A>;

export const simNode = (env: LandedSimEnv, run: Run) => {
  const { stats, mempool, book } = env;

  /**
   * A rebase that revives own blocks deletes their members' rejections. A
   * disposal restores such a member's row from its journal bytes, which the
   * node decodes once the rejection deleted its delta; the simulator's
   * filler bytes do not decode, so it puts the delta back for them.
   */
  const reviveRejections = async (revived: readonly string[]) => {
    const unrejected = revived.flatMap((header) =>
      clearRevivedRejections(mempool, book.blocks.get(header)!.txIds.map(hex)),
    );
    stats.revivalUnrejected += unrejected.length;
    if (unrejected.length > 0)
      await run(
        withFollowerWrite(
          MempoolTxDeltasDB.upsertMany(
            unrejected.map((id) => {
              const tx = mempool.txs.get(id)!;
              return { txId: tx.id, spent: tx.spent, produced: tx.produced };
            }),
          ),
        ),
      );
  };

  /** A disposed own block's members are pending again. */
  const restoreMembers = (header: string) => {
    const block = book.blocks.get(header)!;
    stats.unrejectedRestored += restoreDisposedMembers(
      mempool,
      block.txIds.map(hex),
      block.at,
    );
  };

  /** Moves the working ledger and native MPF onto the processed chain (and live own block). */
  const rebaseOnto = async () => {
    const plan = await run(Effect.flatMap(retrieveRows, rebaseTargetOf));
    if (plan.kind !== "ready") return `no rebase target: ${plan.detail}`;
    await run(
      moveNativeRoot(env.owner.current, plan.target, {
        assertCurrent: Effect.void,
      }),
    );
    await run(withFollowerWrite(rebaseSql(plan.target)));
    for (const disposal of plan.target.journals.dispose)
      restoreMembers(disposal.headerHash);
    await reviveRejections(plan.target.journals.revive);
    if (plan.target.live !== undefined) stats.liveRebases += 1;
    return plan.target;
  };

  const resolved = (header: string, canonical: ReadonlySet<string>) => {
    book.active = undefined;
    stats.ownResolutions += 1;
    if (!canonical.has(book.blocks.get(header)!.parentHash))
      stats.ownOnRemovedBase += 1;
  };

  /**
   * An own merged block's local finalization, as far as the simulation sees
   * it: its journal is locally applied. The transactions it included stay
   * in the mempool, marked by the block, until its fold is final.
   */
  const finalizeOwn = async (header: string) => {
    await run(setJournalStatus(header, JournalStatus.LocallyApplied));
    stats.ownMerges += 1;
  };

  /**
   * S6 derived the active commit dead (its base left the processed tip
   * without a rebase): the node disposes of its journal.
   */
  const abandonActive = async (canonical: ReadonlySet<string>) => {
    const active = book.active!;
    await run(
      withFollowerWrite(
        disposeJournals([
          {
            headerHash: active,
            cause: "its signed commit is dead",
            active: true,
          },
        ]),
      ),
    );
    restoreMembers(active);
    resolved(active, canonical);
  };

  /** The book after a rebase that disposed of and revived `journals`. */
  const followDisposition = async (
    journals: OwnJournalDisposition,
    canonical: ReadonlySet<string>,
  ) => {
    for (const disposal of journals.dispose) {
      restoreMembers(disposal.headerHash);
      if (disposal.headerHash === book.active)
        resolved(disposal.headerHash, canonical);
    }
    stats.ownRevivals += journals.revive.length;
    await reviveRejections(journals.revive);
  };

  /** The commit path finalizes every revived block locally. */
  const finalizeRevived = async () => {
    const statuses = await run(journalStatuses);
    let finalized = 0;
    for (const [header, status] of statuses)
      if (status === JournalStatus.ObservedWaitingStability) {
        await run(setJournalStatus(header, JournalStatus.LocallyApplied));
        if (book.active === header) book.active = undefined;
        finalized += 1;
      }
    return finalized;
  };

  /** The active journal's block was merged: its journal finalizes. */
  const finalizeMerged = async (rooted: ReadonlySet<string>) => {
    if (book.active === undefined || !rooted.has(book.active)) return;
    await finalizeOwn(book.active);
    book.active = undefined;
  };

  /**
   * Admits pending chains (one spending `tip`'s `X`, one spending that) and
   * single spends of `Y` and the pool.
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
    mempool.survivors.push(...txs);
    for (const tx of txs) mempool.txs.set(hex(tx.id), tx);
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

  /**
   * Commits an own block on the processed tip `tip`, the queue's tail,
   * selecting from `pending` (the survivors no block on the chain includes).
   */
  const commit = async (
    tip: string,
    candidate: string | undefined,
    pending: readonly SimPendingTx[],
  ) => {
    const { block, info, journal } = ownBlockOn(
      env.universe,
      env.registry,
      book,
      tip,
      pending,
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
    followDisposition,
    finalizeRevived,
    finalizeMerged,
    admit,
    candidateOn,
    commit,
  };
};
