/**
 * The landed-block fork simulator's node side (N3): its statistics, the
 * environment a scenario runs in, the stub ports processing runs on (honest,
 * exactly-once-checked replays with late DA and transient faults, foreign
 * blocks that include pending transactions; own blocks read from the node's
 * journals, their merge finalized by folding the journal's delta) and the
 * canonical queue read from the canonical blocks.
 */
import { type BlockSummary, type FactStore } from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { Effect, type Runtime } from "effect";

import {
  ConfirmedLedgerDB,
  DepositsDB,
  MempoolInclusionsDB,
  MutationJobsDB,
} from "../../src/database/index.js";
import { readQueueHistory } from "../../src/landed-blocks/history.js";
import { ownJournal } from "../../src/landed-blocks/journal.js";
import {
  ledgerEntries,
  ledgerMap,
  ledgerRows,
} from "../../src/landed-blocks/ledger.js";
import type { LandedBlockPorts } from "../../src/landed-blocks/ports.js";
import { rebasePlan } from "../../src/landed-blocks/rebase-target.js";
import { recordSettlements } from "../../src/landed-blocks/settlements.js";
import { retrieveRows } from "../../src/landed-blocks/store.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../../src/mpf/ledger-hydration.js";
import type { Database } from "../../src/services/database.js";
import { withHistoryWrite } from "../../src/services/event-history-producer.js";
import type { NativeMpfOwnerService } from "../../src/services/mpf-native-owner/protocol.js";
import type { SimMempool } from "./landed-blocks-sim.mempool.js";
import type { ModelQueueHeaders } from "./landed-blocks-sim.model.js";
import type { SimOwnBook } from "./landed-blocks-sim.own.js";
import {
  liveQueue,
  type SimRegistry,
  type TrafficStats,
} from "./landed-blocks-sim.traffic.js";
import { hasDeposit, type SimUniverse } from "./landed-blocks-sim.universe.js";
import { SIM_QUEUE_CONFIG } from "./state-queue-sim.fixtures.js";
import { isQueueOutput } from "./state-queue-sim.model.js";

const hex = (value: Uint8Array) => Buffer.from(value).toString("hex");

export type LandedSimStats = TrafficStats & {
  checks: number;
  comparedChecks: number;
  prunedChecks: number;
  replays: number;
  awaitingDaHeld: number;
  transientHeld: number;
  invalidHeld: number;
  rebases: number;
  restoredRoots: number;
  crashResumes: number;
  ownerRestarts: number;
  deferredRebases: number;
  relands: number;
  folds: number;
  behindHeld: number;
  behindHealed: number;
  rollbacksRemovingProcessed: number;
  admitted: number;
  directRejections: number;
  dependentRejections: number;
  rejectionsOnRollback: number;
  /** Rollbacks after which a rejected dependent's output is gone too. */
  latentHoleClosed: number;
  /** Pending transactions a processed foreign block included. */
  foreignIncluded: number;
  batchRejections: number;
  /** Batches rejected around a member a base block settled. */
  batchSettled: number;
  /** ...around a member an own block settled that had folded already. */
  foldThenRejectOwn: number;
  /** ...around a member a foreign block settled that had folded already. */
  foldThenRejectForeign: number;
  ownCommits: number;
  /**
   * Rebases whose target held a live own block (a commit's, a journal
   * resolution's, or one processing asked for).
   */
  liveRebases: number;
  ownProcessed: number;
  ownMerges: number;
  /** Own journals abandoned because their base left the processed tip. */
  ownResolutions: number;
  /** ...because their base was a foreign block a rollback removed. */
  ownOnRemovedBase: number;
  ownRevivals: number;
  /** Checks the node missed (down), and runs that met a merged header it never processed. */
  offlineChecks: number;
  coalescedMerges: number;
  bootstrapsPastGenesis: number;
  /** A long-late block held while the root passed it. */
  heldPastMerge: number;
  /** Checks behind on a rolled-back merge whose rows and ledger stayed put. */
  behindCompared: number;
};

export const zeroLandedSimStats = (): LandedSimStats => ({
  appends: 0,
  badBlocks: 0,
  lateDaBlocks: 0,
  attests: 0,
  merges: 0,
  tailRemovals: 0,
  relandedAppends: 0,
  checks: 0,
  comparedChecks: 0,
  prunedChecks: 0,
  replays: 0,
  awaitingDaHeld: 0,
  transientHeld: 0,
  invalidHeld: 0,
  rebases: 0,
  restoredRoots: 0,
  crashResumes: 0,
  ownerRestarts: 0,
  deferredRebases: 0,
  relands: 0,
  folds: 0,
  behindHeld: 0,
  behindHealed: 0,
  rollbacksRemovingProcessed: 0,
  admitted: 0,
  directRejections: 0,
  dependentRejections: 0,
  rejectionsOnRollback: 0,
  latentHoleClosed: 0,
  ownAppends: 0,
  foreignIncluded: 0,
  batchRejections: 0,
  batchSettled: 0,
  foldThenRejectOwn: 0,
  foldThenRejectForeign: 0,
  ownCommits: 0,
  liveRebases: 0,
  ownProcessed: 0,
  ownMerges: 0,
  ownResolutions: 0,
  ownOnRemovedBase: 0,
  ownRevivals: 0,
  offlineChecks: 0,
  coalescedMerges: 0,
  bootstrapsPastGenesis: 0,
  heldPastMerge: 0,
  behindCompared: 0,
});

/** The native owner the simulated node holds, reopenable as a restart. */
export type SimOwner = {
  current: NativeMpfOwnerService;
  reopen: () => Promise<void>;
};

export type LandedSimEnv = Readonly<{
  universe: SimUniverse;
  registry: SimRegistry;
  runtime: Runtime.Runtime<Database>;
  owner: SimOwner;
  mempool: SimMempool;
  stats: LandedSimStats;
  /** Distinguishes this scenario's transaction ids. */
  label: string;
  book: SimOwnBook;
  /** Leading checks the node is down for (it starts past genesis). */
  offlineFor: number;
  /** What each foreign block includes, fixed at its first replay. */
  includes: Map<string, readonly Buffer[]>;
  /** The check before which a long-late block's payload stays missing. */
  lateUntil: Map<string, number>;
  /** Checks a long-late block's payload stays missing after its first miss. */
  lateFor: number;
  /** The traffic merges whenever it can. */
  mergeHeavy: boolean;
  /** Own merged blocks whose local merge finalization completed. */
  completed: Set<string>;
}>;

export type Faults = {
  missing: string[];
  transient: number;
  violation?: string;
};

export const simPorts = (
  env: LandedSimEnv,
  store: FactStore,
  faults: Faults,
  served: Set<string>,
  requested: { value: boolean },
): LandedBlockPorts<never> => ({
  confirmView: (view) => Effect.promise(() => store.viewValid(view)),
  queueHistory: (view) => readQueueHistory(store, SIM_QUEUE_CONFIG, view),
  write: (work) => withHistoryWrite(work),
  replay: (input) =>
    Effect.gen(function* () {
      const sql = yield* SqlClient.SqlClient;
      const existing = yield* sql`SELECT 1 FROM node_landed_blocks
        WHERE header_hash = ${Buffer.from(input.headerHash, "hex")}`;
      if (existing.length > 0)
        faults.violation = `block ${input.headerHash} was replayed while its row exists`;
      env.stats.replays += 1;
      const info = env.registry.get(input.headerHash)!;
      if (info.own)
        faults.violation = `own block ${input.headerHash} was replayed`;
      if (info.longLate) {
        const until =
          env.lateUntil.get(input.headerHash) ?? env.stats.checks + env.lateFor;
        env.lateUntil.set(input.headerHash, until);
        if (env.stats.checks < until)
          return {
            kind: "missing",
            detail: "DA payload not available for a while",
          } as const;
      } else if (info.lateDa && !served.has(input.headerHash)) {
        served.add(input.headerHash);
        faults.missing.push(input.headerHash);
        return {
          kind: "missing",
          detail: "DA payload not available yet",
        } as const;
      }
      if (env.stats.replays % 11 === 5) {
        faults.transient += 1;
        return yield* Effect.fail(new Error("transient replay fault"));
      }
      const ledger = ledgerMap(input.parentEntries);
      for (const key of [...ledger.keys()])
        if (env.universe.xKeys.has(key)) ledger.delete(key);
      const ySpent = env.universe.ySpent(info.h);
      if (ySpent !== undefined) ledger.delete(hex(ySpent.outref));
      const produced = [
        env.universe.y(info.h),
        env.universe.x(info.h, info.b),
        ...(hasDeposit(info.h) ? [env.universe.deposit(info.h).entry] : []),
      ];
      for (const entry of produced)
        ledger.set(hex(entry.outref), Buffer.from(entry.output));
      const entries = ledgerEntries(ledger);
      // Every other block includes the node's pending (unmarked)
      // transactions that spend the parent's X (as the block does), with
      // every pending transaction that spends their outputs, fixed at its
      // first replay.
      let txIds = env.includes.get(input.headerHash);
      if (txIds === undefined) {
        const reached = new Set(
          [...input.parentEntries]
            .map((entry) => hex(entry.outref))
            .filter((key) => env.universe.xKeys.has(key)),
        );
        const pending = new Set(
          (yield* sql<{ tx_id: Buffer }>`SELECT tx_id FROM mempool
            WHERE included_by IS NULL`).map((row) => hex(row.tx_id)),
        );
        const included: Buffer[] = [];
        if ((info.h + info.b) % 2 === 0)
          for (const tx of env.mempool.survivors)
            if (
              pending.has(hex(tx.id)) &&
              tx.spent.length > 0 &&
              tx.spent.every((outRef) => reached.has(hex(outRef)))
            ) {
              included.push(tx.id);
              for (const entry of tx.produced) reached.add(hex(entry.outref));
            }
        txIds = included;
        env.includes.set(input.headerHash, txIds);
      }
      return {
        kind: "replayed",
        entries,
        root: yield* computeLedgerMpfRootFromLedgerEntries(entries),
        depositIds: hasDeposit(info.h)
          ? [env.universe.deposit(info.h).row[DepositsDB.Columns.ID]]
          : [],
        withdrawals: [],
        forcedIds: [],
        txIds,
      } as const;
    }),
  ownJournal,
  ownMergeCompleted: (headerHash) =>
    Effect.succeed(env.completed.has(headerHash)),
  // The node's local merge finalization, as far as processing sees it: the
  // block's delta folds into `confirmed_ledger`, the receipt members it
  // included are recorded settled, and the rows it marked leave the
  // mempool.
  finalizeOwnMerge: ({ headerHash }) =>
    withHistoryWrite(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const hash = headerHash.toString("hex");
        const row = (yield* retrieveRows).find(
          (item) => item.headerHash === hash,
        );
        if (row === undefined)
          return yield* Effect.fail(new Error(`no own row ${hash}`));
        for (const outRef of row.spent)
          yield* sql`DELETE FROM confirmed_ledger WHERE outref = ${outRef}`;
        yield* ConfirmedLedgerDB.insertMultiple([
          ...(yield* ledgerRows(row.produced, new Map())),
        ]);
        yield* recordSettlements([row]);
        yield* MempoolInclusionsDB.deleteIncluded(headerHash);
        // Its completed merge job, which the rebase's journal disposition
        // reads as merged and finalized here.
        const job = MutationJobsDB.confirmedMergeFinalizationJobId(hash);
        yield* MutationJobsDB.start({
          jobId: job,
          kind: MutationJobsDB.Kind.ConfirmedMergeFinalization,
        });
        yield* MutationJobsDB.markCompleted(job);
        env.completed.add(hash);
        const ids = new Set(row.txIds.map(hex));
        env.mempool.survivors = env.mempool.survivors.filter(
          (tx) => !ids.has(hex(tx.id)),
        );
        env.stats.ownMerges += 1;
      }),
    ),
  genesis: ledgerRows(env.universe.genesis, new Map()),
  requestRebase: () =>
    rebasePlan.pipe(
      Effect.map((plan) => {
        if (plan.kind === "blocked") return plan.detail;
        if (plan.kind === "ready") requested.value = true;
        return undefined;
      }),
    ),
  rebaseFailure: Effect.succeed(undefined),
});

/** The canonical queue's headers, read from the canonical blocks' live outputs. */
export const canonicalQueue = (
  blocks: readonly BlockSummary[],
): ModelQueueHeaders | undefined => {
  const live = new Map<string, Parameters<typeof liveQueue>[0][number]>();
  for (const block of blocks)
    for (const tx of block.txs) {
      if (!tx.isValid) continue;
      for (const input of tx.inputs)
        live.delete(`${hex(input.txHash)}#${input.index}`);
      tx.outputs.forEach((output, index) => {
        const simOutput = {
          address: output.address,
          lovelace: output.lovelace,
          assets: output.assets,
          ...(output.datum === null ? {} : { datum: output.datum }),
        };
        if (isQueueOutput(simOutput))
          live.set(`${hex(tx.hash)}#${index}`, {
            outRef: { txHash: tx.hash, index },
            output: simOutput,
          });
      });
    }
  const { root, nodes } = liveQueue([...live.values()]);
  return root === null
    ? undefined
    : { root: root.state.headerHash, nodes: nodes.map((node) => node.key!) };
};
