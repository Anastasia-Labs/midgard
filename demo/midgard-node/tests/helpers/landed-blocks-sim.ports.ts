/**
 * The landed-block fork simulator's node side (N3): its statistics, the
 * environment a scenario runs in, the stub ports processing runs on (honest,
 * exactly-once-checked replays with late DA and transient faults; no own
 * blocks) and the canonical queue read from the canonical blocks.
 */
import { type BlockSummary, type FactStore } from "@al-ft/midgard-l1-follower";
import { SqlClient } from "@effect/sql";
import { Effect, type Runtime } from "effect";

import { DepositsDB } from "../../src/database/index.js";
import {
  ledgerEntries,
  ledgerMap,
  ledgerRows,
} from "../../src/landed-blocks/ledger.js";
import type { LandedBlockPorts } from "../../src/landed-blocks/ports.js";
import { rebasePlan } from "../../src/landed-blocks/rebase-target.js";
import { computeLedgerMpfRootFromLedgerEntries } from "../../src/mpf/ledger-hydration.js";
import type { Database } from "../../src/services/database.js";
import { withHistoryWrite } from "../../src/services/event-history-producer.js";
import type { NativeMpfOwnerService } from "../../src/services/mpf-native-owner/protocol.js";
import type {
  ModelQueueHeaders,
  SimMempool,
} from "./landed-blocks-sim.model.js";
import {
  liveQueue,
  type SimRegistry,
  type TrafficStats,
} from "./landed-blocks-sim.traffic.js";
import { hasDeposit, type SimUniverse } from "./landed-blocks-sim.universe.js";
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
      if (info.lateDa && !served.has(input.headerHash)) {
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
      const produced = [
        env.universe.y(info.h),
        env.universe.x(info.h, info.b),
        ...(hasDeposit(info.h) ? [env.universe.deposit(info.h).entry] : []),
      ];
      for (const entry of produced)
        ledger.set(hex(entry.outref), Buffer.from(entry.output));
      const entries = ledgerEntries(ledger);
      return {
        kind: "replayed",
        entries,
        root: yield* computeLedgerMpfRootFromLedgerEntries(entries),
        depositIds: hasDeposit(info.h)
          ? [env.universe.deposit(info.h).row[DepositsDB.Columns.ID]]
          : [],
        withdrawals: [],
        forcedIds: [],
        txIds: [],
      } as const;
    }),
  ownJournal: () => Effect.succeed(undefined),
  ownMergeCompleted: () => Effect.succeed(false),
  finalizeOwnMerge: () =>
    Effect.fail(new Error("the simulator has no own blocks")),
  genesis: ledgerRows(env.universe.genesis, new Map()),
  requestRebase: () =>
    rebasePlan.pipe(
      Effect.map((plan) => {
        if (plan.kind === "blocked") return plan.detail;
        if (plan.kind === "ready") requested.value = true;
        return undefined;
      }),
    ),
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
