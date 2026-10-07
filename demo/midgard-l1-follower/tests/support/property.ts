import {
  type FactStore,
  type FactStoreOptions,
  openSqliteFactStore,
  type OutRef,
} from "../../src/index.js";
import { rewindFaults } from "../../src/store/fact-store.js";
import type { RewindFault } from "../../src/store/rewind.js";
import {
  ChainGenerator,
  makeUniverse,
  randomOutput,
  Rng,
  type Universe,
} from "./chain.js";
import { diffDumps, dumpStore, FACT_QUERIES } from "./dump.js";
import {
  FIXTURE_DERIVATION,
  FIXTURE_TABLES,
  fixtureMigrations,
} from "./fixture.js";

export type PropertyOptions = Readonly<{
  /** Opens a fresh, empty store under test with the options for its dialect. */
  open: (
    optionsFor: (dialect: "postgres" | "sqlite") => FactStoreOptions,
  ) => Promise<FactStore>;
  seed: number;
  ops: number;
  k: number;
  fault?: RewindFault;
  /** Run a budgeted prune every n steps and compare inside the retained window. */
  pruneEvery?: number;
  pruneBudget?: number;
  /** Probability that a step is a rollback (when one is possible). */
  rollbackProbability?: number;
}>;

export type PropertyStats = {
  applies: number;
  rollbacks: number;
  noopRewinds: number;
  refusedBeyondK: number;
  refusedUnknown: number;
  maxHeight: number;
  maxDepth: number;
  prunes: number;
  finalHeight: number;
  finalLiveOutRefs: number;
  rowsChecked: number;
};

export type PropertyOutcome =
  | Readonly<{ ok: true; stats: PropertyStats }>
  | Readonly<{ ok: false; step: number; reason: string; stats: PropertyStats }>;

class PropertyFailure extends Error {}

export const storeOptions = (
  universe: Universe,
  k: number,
  dialect: "postgres" | "sqlite",
): FactStoreOptions => ({
  securityParameter: k,
  trackedSet: universe.tracked,
  temporalTables: FIXTURE_TABLES,
  migrations: [fixtureMigrations(dialect)],
  derivations: [FIXTURE_DERIVATION],
});

const windowQueries = (p: number): Record<string, string> => ({
  ...FACT_QUERIES,
  l1_blocks: `SELECT * FROM l1_blocks WHERE slot >= ${p}`,
  l1_txs: `SELECT * FROM l1_txs WHERE block_slot > ${p}`,
  l1_tx_mint_policies: `SELECT m.* FROM l1_tx_mint_policies m JOIN l1_txs t ON t.tx_hash = m.tx_hash WHERE t.block_slot > ${p}`,
  l1_outputs: `SELECT * FROM l1_outputs WHERE spent_slot IS NULL OR spent_slot > ${p}`,
  l1_output_assets: `SELECT a.* FROM l1_output_assets a JOIN l1_outputs o ON o.tx_hash = a.tx_hash AND o.output_index = a.output_index WHERE o.spent_slot IS NULL OR o.spent_slot > ${p}`,
  l1_scripts: `SELECT * FROM l1_scripts s WHERE EXISTS (SELECT 1 FROM l1_outputs o WHERE o.script_ref_hash = s.script_hash AND (o.spent_slot IS NULL OR o.spent_slot > ${p}))`,
  fixture_address_live_count: `SELECT * FROM fixture_address_live_count WHERE to_slot IS NULL OR to_slot > ${p}`,
  fixture_block_marks: `SELECT * FROM fixture_block_marks WHERE slot > ${p}`,
  fixture_spend_log: `SELECT * FROM fixture_spend_log WHERE spent_slot > ${p}`,
});

const liveKeys = async (store: FactStore): Promise<OutRef[]> =>
  store.transaction("read", async (tx) =>
    (
      await tx.query(
        "SELECT tx_hash, output_index FROM l1_outputs WHERE spent_slot IS NULL",
      )
    ).map((row) => ({
      txHash: Buffer.from(row.tx_hash as Uint8Array),
      index: Number(row.output_index),
    })),
  );

const expectKind = <T extends { kind: string }>(
  result: T,
  kind: string,
  what: string,
): void => {
  if (result.kind !== kind)
    throw new PropertyFailure(
      `${what}: expected ${kind}, got ${JSON.stringify(result, (_, v: unknown) => (typeof v === "bigint" ? v.toString() : v)).slice(0, 400)}`,
    );
};

/**
 * The §7.2 property test. A random chain from the origin under random
 * `apply(block)` and `rollback(depth <= min(k, height))` steps; after every
 * step every fact table and every registered temporal table must equal a
 * fresh forward-only replay of the current canonical chain (an in-memory
 * SQLite store that has never rewound), INV1–INV6 must hold, and the
 * tracked-outref cache must equal a fresh load.
 */
export const runProperty = async (
  options: PropertyOptions,
): Promise<PropertyOutcome> => {
  const rng = new Rng(options.seed);
  const universe = makeUniverse(rng);
  const origin = {
    point: { slot: 10_000, hash: rng.bytes(32) },
    height: 4_000,
  };
  const seedAddress = universe.trackedAddresses[0] ?? Buffer.alloc(29);
  const seeds = Array.from({ length: 4 }, () => ({
    outRef: { txHash: rng.bytes(32), index: rng.int(4) },
    output: {
      ...randomOutput(rng, universe),
      address: seedAddress,
      paymentCredential: {
        hash: Buffer.from(seedAddress.subarray(1, 29)),
        isScript: true,
      },
      stakeCredential: null,
    },
  }));
  const generator = new ChainGenerator(rng, universe, origin, seeds);
  const stats: PropertyStats = {
    applies: 0,
    rollbacks: 0,
    noopRewinds: 0,
    refusedBeyondK: 0,
    refusedUnknown: 0,
    maxHeight: 0,
    maxDepth: 0,
    prunes: 0,
    finalHeight: 0,
    finalLiveOutRefs: 0,
    rowsChecked: 0,
  };
  const bootstrap = async (store: FactStore): Promise<void> => {
    expectKind(await store.start(), "ready", "start");
    expectKind(await store.initialize(origin), "initialized", "initialize");
    expectKind(
      (await store.insertSeedOutputs(origin.point.slot, seeds)) ?? {
        kind: "null",
      },
      "seeded",
      "seed",
    );
  };
  const store = await options.open((dialect) =>
    storeOptions(universe, options.k, dialect),
  );
  if (options.fault !== undefined) rewindFaults.set(store, options.fault);
  let reference = openSqliteFactStore({
    ...storeOptions(universe, options.k, "sqlite"),
    path: ":memory:",
  });
  let step = 0;
  try {
    await bootstrap(store);
    await bootstrap(reference);
    let generation = 0;
    // Cardano never rolls back past its immutable tip (max tip height - k);
    // with pruning on, a deeper cumulative rollback is the R1 case instead.
    let immutable = 0;
    const p = options.rollbackProbability ?? 0.2;
    for (step = 1; step <= options.ops; step += 1) {
      const height = generator.length;
      const roll = rng.next();
      if (height > options.k && roll < 0.01) {
        const deep = generator.blocks[height - options.k - 2] ?? origin;
        const result = await store.rewind(deep.point);
        if (
          result.kind !== "intervention" ||
          result.reason !== "rollback_beyond_k"
        )
          throw new PropertyFailure(`deep rewind not refused: ${result.kind}`);
        stats.refusedBeyondK += 1;
      } else if (height > 0 && roll < 0.02) {
        const result = await store.rewind({
          slot: generator.tip.point.slot - 1,
          hash: rng.bytes(32),
        });
        if (
          result.kind !== "intervention" ||
          result.reason !== "intersection_outside_history"
        )
          throw new PropertyFailure(
            `unknown-point rewind not refused: ${result.kind}`,
          );
        stats.refusedUnknown += 1;
      } else if (height > 0 && roll < 0.02 + p) {
        const floor = options.pruneEvery === undefined ? 0 : immutable;
        const depth = rng.range(0, Math.min(options.k, height - floor));
        const target = generator.rollback(depth);
        const result = await store.rewind(target.point);
        if (depth === 0) {
          expectKind(result, "noop", "rewind 0");
          stats.noopRewinds += 1;
        } else {
          expectKind(result, "rewound", `rewind ${depth}`);
          if (result.kind === "rewound" && result.generation !== generation + 1)
            throw new PropertyFailure(
              `generation ${result.generation} after ${generation}`,
            );
          generation += 1;
          stats.rollbacks += 1;
          stats.maxDepth = Math.max(stats.maxDepth, depth);
          await reference.close();
          reference = openSqliteFactStore({
            ...storeOptions(universe, options.k, "sqlite"),
            path: ":memory:",
          });
          await bootstrap(reference);
          for (const block of generator.blocks)
            expectKind(await reference.applyBlock(block), "applied", "replay");
        }
      } else {
        const block = generator.extend();
        expectKind(await store.applyBlock(block), "applied", "apply");
        expectKind(
          await reference.applyBlock(block),
          "applied",
          "reference apply",
        );
        stats.applies += 1;
        stats.maxHeight = Math.max(stats.maxHeight, generator.length);
        immutable = Math.max(immutable, generator.length - options.k);
      }
      if (options.pruneEvery !== undefined && step % options.pruneEvery === 0) {
        const pruned = await store.prune(options.pruneBudget ?? 50);
        if ("kind" in pruned)
          throw new PropertyFailure(
            `prune failed: ${pruned.kind === "error" ? pruned.error.message : pruned.detail}`,
          );
        stats.prunes += 1;
      }
      const cursor = await store.cursor();
      const window =
        options.pruneEvery === undefined
          ? undefined
          : windowQueries(cursor?.prunedThroughSlot ?? 0);
      const [actual, expected] = await Promise.all([
        dumpStore(store, window),
        dumpStore(reference, window),
      ]);
      const diff = diffDumps(actual, expected);
      if (diff !== null)
        throw new PropertyFailure(`state differs from a fresh replay: ${diff}`);
      stats.rowsChecked += Object.values(expected).reduce(
        (sum, rows) => sum + rows.length,
        0,
      );
      const report = await store.checkInvariants();
      if (!report.ok)
        throw new PropertyFailure(
          `invariants: ${JSON.stringify(report.violations.map((v) => v.check))}`,
        );
      const live = await liveKeys(reference);
      if (
        store.liveOutRefCount() !== live.length ||
        !live.every((outRef) => store.isTrackedLive(outRef))
      )
        throw new PropertyFailure(
          `tracked-outref cache (${store.liveOutRefCount()}) differs from a fresh load (${live.length})`,
        );
    }
    stats.finalHeight = generator.length;
    stats.finalLiveOutRefs = store.liveOutRefCount();
    return { ok: true, stats };
  } catch (error) {
    if (error instanceof PropertyFailure)
      return { ok: false, step, reason: error.message, stats };
    throw error;
  } finally {
    await store.close();
    await reference.close();
  }
};
