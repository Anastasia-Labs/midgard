import type { ChainSyncEvent } from "@al-ft/l1-node-transport";

import { encodeOutRef } from "../codec.js";
import { decodeBlock } from "../decode/block.js";
import {
  applyChainSyncEvent,
  type FollowStep,
  storePoint,
} from "../follow/chain-sync.js";
import {
  type FollowerProjection,
  projectionStoreOptions,
} from "../projection.js";
import type { DialectName } from "../sql/backend.js";
import { openSqliteFactStore } from "../sqlite.js";
import type { FactStore, FactStoreOptions } from "../store/fact-store.js";
import type { BlockSummary } from "../types.js";
import {
  EpisodeBuilder,
  type ForkCheckpoint,
  type ForkScenario,
  type ForkStep,
} from "./episodes.js";
import { diffDumps, dumpStore, type StoreDump } from "./replay.js";
import { diffPruned, dumpRetained } from "./retention.js";
import { Rng } from "./rng.js";
import { SimChain, type SimOrigin, simUniverse } from "./sim-chain.js";
import {
  createSeedRun,
  type ForkWalletSeed,
  type SeedStats,
  trackedWithWallets,
  zeroSeedStats,
} from "./wallet-seed-run.js";

/** The store options for projections over the simulator's universe. */
export const simStoreOptions = (
  projections: readonly FollowerProjection[],
  k: number,
  dialect: DialectName,
): FactStoreOptions =>
  projectionStoreOptions(
    projections,
    { securityParameter: k, trackedSet: simUniverse().tracked },
    dialect,
  );

export const SIM_ORIGIN: SimOrigin = {
  point: { slot: 50_000, hash: Buffer.alloc(32, 0x0a) },
  height: 1_000,
};

/** The scenario's events and checkpoints, deterministic per scenario. */
export const buildForkSteps = (
  scenario: ForkScenario,
  projections: readonly FollowerProjection[] = [],
  walletSeed?: ForkWalletSeed,
): { chain: SimChain; steps: readonly ForkStep[] } => {
  const universe = simUniverse();
  const chain = new SimChain(
    universe,
    SIM_ORIGIN,
    trackedWithWallets(
      simStoreOptions(projections, 1, "sqlite").trackedSet,
      walletSeed,
    ),
    walletSeed?.preOrigin ?? [],
  );
  const builder = new EpisodeBuilder(
    chain,
    new Rng(scenario.seed),
    projections.flatMap((p) => (p.traffic === undefined ? [] : [p.traffic])),
    projections.flatMap((p) => (p.protects === undefined ? [] : [p.protects])),
  );
  scenario.episodes.forEach((episode, index) =>
    builder.episode(episode, index),
  );
  return { chain, steps: builder.steps };
};

/** Where the events come from: the in-process list, or a real transport. */
export type EventSource = Readonly<{
  next: () => Promise<ChainSyncEvent | undefined>;
  ack?: (seq: bigint) => void;
}>;

export type ForkRunOptions = Readonly<{
  /** Opens a fresh, empty store under test. */
  open: (
    options: (dialect: DialectName) => FactStoreOptions,
  ) => Promise<FactStore>;
  k: number;
  projections?: readonly FollowerProjection[];
  /** Feeds the events (default: straight from the step list). */
  source?: (steps: readonly ForkStep[]) => Promise<EventSource>;
  /** Called with the store under test before the first event (fault seams). */
  prepare?: (store: FactStore) => void;
  walletSeed?: ForkWalletSeed;
}>;

export type ForkRunStats = SeedStats & {
  events: number;
  rollbacks: number;
  checkpoints: number;
  rowsChecked: number;
  /** Prunes run to completion (`ForkStep.prune`), and the rows they deleted. */
  prunes: number;
  prunedRows: number;
};

/** The budget of each prune step the simulator runs: small, so steps cut. */
const SIM_PRUNE_BUDGET = 2;
const SIM_PRUNE_MAX_STEPS = 100_000;

export type ForkRunOutcome =
  | Readonly<{ ok: true; stats: ForkRunStats }>
  | Readonly<{ ok: false; step: number; reason: string; stats: ForkRunStats }>;

class ForkFailure extends Error {}

const describe = (step: FollowStep): string =>
  JSON.stringify(step.result, (_, value: unknown) =>
    typeof value === "bigint"
      ? value.toString()
      : Buffer.isBuffer(value)
        ? value.toString("hex")
        : value,
  ).slice(0, 300);

const sameEvent = (a: ChainSyncEvent, b: ChainSyncEvent): boolean =>
  a.kind === b.kind &&
  a.seq === b.seq &&
  JSON.stringify([a.point, a.tip], (_, v: unknown) =>
    typeof v === "bigint" ? v.toString() : v,
  ) ===
    JSON.stringify([b.point, b.tip], (_, v: unknown) =>
      typeof v === "bigint" ? v.toString() : v,
    ) &&
  (a.kind === "roll_backward" ||
    (b.kind === "roll_forward" && Buffer.from(a.block).equals(b.block)));

/**
 * Whether the store holds a checkpoint's facts and exactly the model's live
 * tracked outputs; the first failure as text, or null.
 */
export const checkpointFailure = async (
  store: FactStore,
  checkpoint: ForkCheckpoint,
): Promise<string | null> => {
  for (const check of checkpoint.checks) {
    const where = `${checkpoint.label}: ${check.what}`;
    if (check.kind === "spender") {
      const spender = await store.spenderOf(check.outRef);
      const ok =
        check.spentBy === null
          ? spender.kind === "unspent"
          : spender.kind === "spent" && spender.txHash.equals(check.spentBy);
      if (!ok)
        return `${where}: expected ${check.spentBy === null ? "unspent" : `spent by ${check.spentBy.toString("hex")}`}, got ${spender.kind === "spent" ? `spent by ${spender.txHash.toString("hex")}` : spender.kind}`;
      continue;
    }
    const stored = await store.txByHash(check.hash);
    if (check.kind === "tx_absent") {
      if (stored !== null) return `${where}: tx is still stored`;
      continue;
    }
    if (
      stored === null ||
      stored.isValid !== check.isValid ||
      stored.invalidAfter !== check.invalidAfter
    )
      return `${where}: expected stored (valid ${check.isValid}, invalidAfter ${check.invalidAfter}), got ${stored === null ? "absent" : `valid ${stored.isValid}, invalidAfter ${stored.invalidAfter}`}`;
  }
  const live = await store.transaction("read", async (tx) =>
    (
      await tx.query(
        "SELECT tx_hash, output_index FROM l1_outputs WHERE spent_slot IS NULL",
      )
    )
      .map((row) =>
        encodeOutRef({
          txHash: Buffer.from(row.tx_hash as Uint8Array),
          index: Number(row.output_index),
        }).toString("hex"),
      )
      .sort(),
  );
  return live.join() === checkpoint.liveTracked.join()
    ? null
    : `${checkpoint.label}: live tracked outputs differ from the model (${live.length} stored, ${checkpoint.liveTracked.length} expected)`;
};

const listSource = (steps: readonly ForkStep[]): EventSource => {
  let index = 0;
  return {
    next: () => Promise.resolve(steps[index++]?.event),
  };
};

/**
 * Runs a scenario through the sequential writer (`applyChainSyncEvent`,
 * F6 being deferred), pruning to completion after each step flagged
 * `prune`. After every event: every fact and registered temporal table
 * equals a fresh forward-only SQLite replay of the model's current chain
 * (once pruned: holds only rows of it and every row retention keeps),
 * INV1–INV6 hold, the tracked-outref cache equals the
 * replay's and every projection check passes. At each checkpoint the
 * shape's facts and the model's live tracked set hold.
 */
export const runForkScenario = async (
  scenario: ForkScenario,
  options: ForkRunOptions,
): Promise<ForkRunOutcome> => {
  const projections = options.projections ?? [];
  const walletSeed = options.walletSeed;
  const { chain, steps } = buildForkSteps(scenario, projections, walletSeed);
  const optionsFor = (dialect: DialectName): FactStoreOptions => {
    const base = simStoreOptions(projections, options.k, dialect);
    return {
      ...base,
      trackedSet: trackedWithWallets(base.trackedSet, walletSeed),
    };
  };
  const stats: ForkRunStats = {
    events: 0,
    rollbacks: 0,
    checkpoints: 0,
    rowsChecked: 0,
    prunes: 0,
    prunedRows: 0,
    ...zeroSeedStats(),
  };
  const store = await options.open(optionsFor);
  options.prepare?.(store);
  const canonical: BlockSummary[] = [];
  const seedRun = createSeedRun({
    store,
    walletSeed,
    canonical,
    origin: SIM_ORIGIN,
    stats,
  });
  /** A fresh forward-only replay of `canonical`, tracking and seeding where the store did. */
  const rebuild = async (): Promise<FactStore> => {
    const reference = openSqliteFactStore({
      ...optionsFor("sqlite"),
      path: ":memory:",
    });
    // Every result is checked: a refusal here (store_locked included) is a
    // failed run, never a replay that silently stopped.
    const started = await reference.start();
    if (started.kind !== "ready")
      throw new ForkFailure(`reference start: ${started.kind}`);
    const init = await reference.initialize(SIM_ORIGIN);
    if (init.kind !== "initialized")
      throw new ForkFailure(`reference initialize: ${init.kind}`);
    await seedRun.replay(reference, SIM_ORIGIN.height);
    for (const block of canonical) {
      const applied = await reference.applyBlock(block);
      if (applied.kind !== "applied")
        throw new ForkFailure(`reference replay: ${applied.kind}`);
      await seedRun.replay(reference, block.height);
    }
    return reference;
  };
  let reference = await rebuild();
  /** Prunes the store under test to completion, in small budgeted steps. */
  const pruneAll = async (): Promise<void> => {
    for (let n = 0; n < SIM_PRUNE_MAX_STEPS; n += 1) {
      const pruned = await store.prune(SIM_PRUNE_BUDGET);
      if ("kind" in pruned)
        throw new ForkFailure(
          `prune: ${pruned.kind === "error" ? pruned.error.message : pruned.detail}`,
        );
      for (const rows of Object.values(pruned.deleted))
        stats.prunedRows += rows;
      if (pruned.done) {
        stats.prunes += 1;
        return;
      }
    }
    throw new ForkFailure(`prune: not done after ${SIM_PRUNE_MAX_STEPS} steps`);
  };
  /**
   * The store against the fresh replay. Unpruned (prunedThroughSlot still
   * the origin's), every table equals the replay's. Pruned, the store holds
   * only rows of the replay and every row retention keeps (`diffPruned`).
   */
  const compare = async (expected: StoreDump): Promise<string | null> => {
    const actual = await dumpStore(store);
    const cursor = await store.cursor();
    const s = cursor?.prunedThroughSlot ?? SIM_ORIGIN.point.slot;
    if (s === SIM_ORIGIN.point.slot) return diffDumps(actual, expected);
    const pins = optionsFor("sqlite").retentionPins ?? {};
    return diffPruned(actual, expected, await dumpRetained(reference, pins, s));
  };
  let index = 0;
  try {
    const started = await store.start();
    if (started.kind !== "ready")
      throw new ForkFailure(`start: ${started.detail}`);
    const init = await store.initialize(SIM_ORIGIN);
    if (init.kind !== "initialized")
      throw new ForkFailure(`initialize: ${init.kind}`);
    seedRun.start();
    const source = await (
      options.source ?? ((s) => Promise.resolve(listSource(s)))
    )(steps);
    for (index = 0; index < steps.length; index += 1) {
      const step = steps[index] as ForkStep;
      const event = await source.next();
      if (event === undefined || !sameEvent(event, step.event))
        throw new ForkFailure(
          `event ${index}: the source delivered ${event === undefined ? "nothing" : `${event.kind} #${event.seq}`}, expected ${step.event.kind} #${step.event.seq}`,
        );
      const result = await applyChainSyncEvent(store, event);
      const expected = event.kind === "roll_forward" ? "applied" : "rewound";
      if (result.result.kind !== expected)
        throw new ForkFailure(`${event.kind}: ${describe(result)}`);
      source.ack?.(event.seq);
      stats.events += 1;
      if (event.kind === "roll_forward") {
        const block = decodeBlock(event.block);
        canonical.push(block);
        const applied = await reference.applyBlock(block);
        if (applied.kind !== "applied")
          throw new ForkFailure(`reference apply: ${applied.kind}`);
      } else {
        stats.rollbacks += 1;
        if (event.point.kind !== "point")
          throw new ForkFailure("rollback to the genesis");
        const target = storePoint(event.point);
        while (
          canonical.length > 0 &&
          !(canonical[canonical.length - 1] as BlockSummary).point.hash.equals(
            target.hash,
          )
        )
          canonical.pop();
        await reference.close();
        reference = await rebuild();
      }
      const seeded = await seedRun.afterEvent(index);
      if (typeof seeded === "string") throw new ForkFailure(seeded);
      if (seeded.rebuild) {
        await reference.close();
        reference = await rebuild();
      }
      if (step.prune === true) {
        await pruneAll();
        await seedRun.afterPrune();
      }
      const expectedDump = await dumpStore(reference);
      const diff = await compare(expectedDump);
      if (diff !== null)
        throw new ForkFailure(`state differs from a fresh replay: ${diff}`);
      for (const rows of Object.values(expectedDump))
        stats.rowsChecked += rows.length;
      const report = await store.checkInvariants();
      if (!report.ok)
        throw new ForkFailure(
          `invariants: ${report.violations.map((v) => v.check).join(", ")}`,
        );
      if (store.liveOutRefCount() !== reference.liveOutRefCount())
        throw new ForkFailure(
          `tracked-outref cache ${store.liveOutRefCount()} vs fresh ${reference.liveOutRefCount()}`,
        );
      if (step.checkpoint !== undefined) {
        const failure = await checkpointFailure(store, {
          ...step.checkpoint,
          liveTracked: seedRun.liveTracked(step.checkpoint.liveTracked),
        });
        if (failure !== null) throw new ForkFailure(failure);
        stats.checkpoints += 1;
      }
      for (const projection of projections) {
        const failure = await projection.check?.({ store, step, chain });
        if (failure !== undefined && failure !== null)
          throw new ForkFailure(`${projection.name}: ${failure}`);
      }
    }
    return { ok: true, stats };
  } catch (error) {
    if (error instanceof ForkFailure)
      return { ok: false, step: index, reason: error.message, stats };
    throw error;
  } finally {
    seedRun.close();
    await store.close();
    await reference.close();
  }
};
