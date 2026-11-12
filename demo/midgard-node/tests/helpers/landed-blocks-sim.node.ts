/**
 * What the simulated node does around landed-block processing (N3, I3): it
 * admits pending transactions and commits its own block on the processed
 * tip (the journal, then the working-ledger move onto it). The rebase takes
 * a landed own block in, dead or not, and disposes of the dead journals;
 * the node keeps its model of the mempool in step with that. Nothing writes
 * a journal after its commit: its status and its death are derived.
 */
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import {
  MempoolTxDeltasDB,
  PendingBlockFinalizationsDB,
} from "../../src/database/index.js";
import {
  type OwnJournalDisposition,
  ownJournalDisposition,
} from "../../src/landed-blocks/own-journals.js";
import { moveNativeRoot, rebaseSql } from "../../src/landed-blocks/rebase.js";
import { rebaseTargetOf } from "../../src/landed-blocks/rebase-target.js";
import { retrieveRows } from "../../src/landed-blocks/store.js";
import type { NodeConfig } from "../../src/services/config.js";
import type { Database } from "../../src/services/database.js";
import { withFollowerWrite } from "../../src/services/follower-write-gate.js";
import { makeOutRefCbor } from "../midgard-output-helpers.js";
import {
  admitPending,
  clearRevivedRejections,
  restoreDisposedMembers,
  type SimPendingTx,
} from "./landed-blocks-sim.mempool.js";
import { insertOwnJournal, ownBlockOn } from "./landed-blocks-sim.own.js";
import type { LandedSimEnv } from "./landed-blocks-sim.ports.js";
import { simDigest, simOutput } from "./landed-blocks-sim.universe.js";
import { ownBlockStatuses } from "./own-block-facts.js";

const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");

type Run = <A, E>(
  effect: Effect.Effect<A, E, Database | NodeConfig>,
) => Promise<A>;

export const simNode = (env: LandedSimEnv, run: Run) => {
  const { stats, mempool, book } = env;
  // Journals seen applied (or folded), seen unlanded again after that, and
  // seen dead, by header.
  const applied = new Set<string>();
  const rolledBack = new Set<string>();
  const dead = new Set<string>();
  // Each journal row as its commit wrote it, by header.
  const written = new Map<string, string>();

  /** Every journal row, column by column, by header. */
  const journalRows = async () =>
    new Map(
      (
        await run(
          Effect.flatMap(
            SqlClient.SqlClient,
            (sql) =>
              sql<
                Record<string, unknown>
              >`SELECT * FROM pending_block_finalizations`,
          ),
        )
      ).map((row) => [
        hex(row.header_hash as Buffer),
        JSON.stringify(row, (_, value: unknown) =>
          typeof value === "bigint" ? value.toString() : value,
        ),
      ]),
    );

  /**
   * Applying an own block deletes its members' rejections. A disposal
   * restores such a member's row from its journal bytes, which the node
   * decodes once the rejection deleted its delta; the simulator's filler
   * bytes do not decode, so it puts the delta back for them.
   */
  const unrejectMembers = async (headers: readonly string[]) => {
    const unrejected = headers.flatMap((header) =>
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

  /**
   * The journals the rebase applied: each one's members' rejections are
   * gone. One seen dead before was taken in after all.
   */
  const followApplied = async () => {
    const newly: string[] = [];
    for (const [header, status] of await run(ownBlockStatuses)) {
      if (
        status !== PendingBlockFinalizationsDB.OwnBlockStatus.Applied &&
        status !== PendingBlockFinalizationsDB.OwnBlockStatus.Folded
      ) {
        if (
          applied.delete(header) &&
          status === PendingBlockFinalizationsDB.OwnBlockStatus.Unlanded
        ) {
          rolledBack.add(header);
          stats.ownRolledBack += 1;
        }
        continue;
      }
      if (applied.has(header)) continue;
      applied.add(header);
      newly.push(header);
      stats.ownApplied += 1;
      if (dead.delete(header)) stats.ownRevivals += 1;
      if (rolledBack.delete(header)) stats.ownRelanded += 1;
    }
    await unrejectMembers(newly);
  };

  /**
   * The book after a rebase over `journals`: a disposed journal's members
   * are pending again, and a journal newly dead is resolved. One dead before
   * and not now landed after all, or its block can land again.
   */
  const followRebase = async (
    journals: OwnJournalDisposition,
    canonical: ReadonlySet<string>,
  ) => {
    for (const disposal of journals.dispose) {
      const block = book.blocks.get(disposal.headerHash)!;
      stats.unrejectedRestored += restoreDisposedMembers(
        mempool,
        block.txIds.map(hex),
        block.at,
      );
    }
    await followApplied();
    const now = new Set(journals.dead.map(hex));
    for (const header of dead) if (!now.has(header)) dead.delete(header);
    for (const header of now) {
      if (dead.has(header)) continue;
      dead.add(header);
      stats.ownResolutions += 1;
      if (!canonical.has(book.blocks.get(header)!.parentHash))
        stats.ownOnRemovedBase += 1;
    }
  };

  /** Moves the working ledger and native MPF onto the processed chain (and live own block). */
  const rebaseOnto = async (canonical: ReadonlySet<string>) => {
    const plan = await run(
      Effect.flatMap(ownJournalDisposition, (journals) =>
        Effect.flatMap(retrieveRows, (rows) => rebaseTargetOf(rows, journals)),
      ),
    );
    if (plan.kind !== "ready") return `no rebase target: ${plan.detail}`;
    await run(
      moveNativeRoot(env.owner.current, plan.target, {
        assertCurrent: Effect.void,
      }),
    );
    await run(withFollowerWrite(rebaseSql(plan.target)));
    await followRebase(plan.target.journals, canonical);
    if (plan.target.live !== undefined) stats.liveRebases += 1;
    return plan.target;
  };

  /**
   * The journals' derived statuses equal a fresh derivation from the model
   * at a settled check: folded at the frontier or a retained fold
   * (`folded`), applied when processed past it (`processed`), unlanded
   * otherwise. A fold the prune deleted (`pruned`) derives folded from its
   * merged queue terminal; the sim's follower store is not the node
   * database, so that terminal is read in the store (`mergedInStore`). No
   * journal row changed since its commit wrote it, and the live journal
   * (the one the commit path signs and submits) is not dead.
   */
  const ownDifference = async (
    folds: Readonly<{
      folded: ReadonlySet<string>;
      pruned: ReadonlySet<string>;
      mergedInStore: ReadonlySet<string>;
    }>,
    processed: ReadonlySet<string>,
    live: string | undefined,
  ) => {
    const S = PendingBlockFinalizationsDB.OwnBlockStatus;
    const statuses = await run(ownBlockStatuses);
    const rows = await journalRows();
    const { folded, pruned, mergedInStore } = folds;
    for (const header of book.blocks.keys()) {
      if (pruned.has(header)) {
        if (!mergedInStore.has(header))
          return `own block ${header}'s fold was pruned with no merged queue terminal`;
        stats.ownPrunedFolds += 1;
      } else {
        const expected = folded.has(header)
          ? S.Folded
          : processed.has(header)
            ? S.Applied
            : S.Unlanded;
        if (statuses.get(header) !== expected)
          return `own block ${header} is ${statuses.get(header)}, the model's ${expected}`;
      }
      if (rows.get(header) !== written.get(header))
        return `own block ${header}'s journal row changed after its commit`;
    }
    if (live !== undefined && dead.has(live))
      return `dead own block ${live} is live`;
    stats.ownStatusChecks += 1;
    return null;
  };

  /** The live own block's header (hex), if any. */
  const liveHeader = async () => {
    const live = await run(PendingBlockFinalizationsDB.retrieveLive());
    return live._tag === "Some"
      ? hex(live.value[PendingBlockFinalizationsDB.Columns.HEADER_HASH])
      : undefined;
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
    canonical: ReadonlySet<string>,
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
    written.set(block.headerHash, (await journalRows()).get(block.headerHash)!);
    env.registry.set(block.headerHash, info);
    book.blocks.set(block.headerHash, block);
    stats.ownCommits += 1;
    const target = await rebaseOnto(canonical);
    if (typeof target === "string") return target;
    if (target.live?.headerHash !== block.headerHash)
      return `the commit's rebase target holds live block ${target.live?.headerHash ?? "none"}, not ${block.headerHash}`;
    return block;
  };

  return {
    followRebase,
    ownDifference,
    liveHeader,
    admit,
    candidateOn,
    commit,
  };
};
