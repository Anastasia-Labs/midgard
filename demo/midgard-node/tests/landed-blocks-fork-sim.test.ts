/**
 * In-order landed-block processing (plan §7.3, §15 N3) under the fork
 * simulator, on SQLite and Postgres follower stores with prune on. After
 * every chain-sync event the node processes the landed queue and runs the
 * working-ledger rebase it asks for, then equals a fresh derivation from
 * the canonical chain (`helpers/landed-blocks-sim.model.ts`): its rows, the
 * confirmed and working ledgers, the native MPF root, the mempool and its
 * inclusion marks, the rejections, the recorded receipt settlements and the
 * deposit statuses.
 *
 * Each suite proves its corpus exercised every case: every block processed
 * exactly once across restarts, crashes and head changes; a rollback that
 * removed a processed block reverts it, rejecting the pending transaction
 * that spent its output with its dependents (whose outputs leave the working
 * ledger too); a block whose replay misses its header's root is never
 * adopted and holds `landed_block_invalid` with the process up; late DA
 * and transient replay faults each hold by name and clear; a merge a
 * rollback undid is unfolded back to the root's lineage by header identity
 * (N5), its retained folds pruned once no rollback reaches them; and a
 * batch is rejected around a member an own block, and one a foreign block,
 * settled and folded before the rejection. A fork onto another header with
 * the frontier's root (`equalRootUnfolds`) is counted but not required: the
 * corpus does not reliably build one, so the deterministic
 * `confirmed-ledger-temporal.test.ts` pins it.
 */
import "./utils.js";

import { createHash } from "node:crypto";
import { mkdtemp, readFile, rm } from "node:fs/promises";
import { tmpdir } from "node:os";
import { join } from "node:path";

import {
  type FactStore,
  openPostgresFactStore,
  openSqliteFactStore,
} from "@al-ft/midgard-l1-follower";
import {
  forkCorpus,
  type ForkRunOptions,
  type ForkScenario,
  forkScenarioArbitrary,
  runForkScenario,
} from "@al-ft/midgard-l1-follower/testing";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Runtime } from "effect";
import fc from "fast-check";
import { Level } from "level";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import { MempoolLedgerDB } from "../src/database/index.js";
import { ledgerRows } from "../src/landed-blocks/ledger.js";
import { ledgerOutputToInsertBatchOp } from "../src/mpf/ledger-delta.js";
import type { Database } from "../src/services/database.js";
import {
  encodeNativeMpfEventLog,
  ProductionNativeMpfOwnerService,
} from "../src/services/mpf-native-owner/index.js";
import { prepareEventFlatDigest } from "../src/workers/utils/mpf-event-flat-digest.js";
import { insertDeposits } from "./helpers/event-rows.js";
import {
  DROP_ALL_TIMEOUT_MS,
  testDatabases,
} from "./helpers/l1-events-store.js";
import { landedBlocksSimProjection } from "./helpers/landed-blocks-sim.js";
import {
  insertSimCursor,
  newSimMempool,
} from "./helpers/landed-blocks-sim.mempool.js";
import { newSimOwnBook } from "./helpers/landed-blocks-sim.own.js";
import {
  type LandedSimStats,
  type SimOwner,
  zeroLandedSimStats,
} from "./helpers/landed-blocks-sim.ports.js";
import { newSimRegistry } from "./helpers/landed-blocks-sim.traffic.js";
import {
  createSimUniverse,
  type SimUniverse,
} from "./helpers/landed-blocks-sim.universe.js";
import { nativeOwnerBinaryPath } from "./helpers/native-owner-binary.js";
import { provideDatabaseLayers, resetApplicationTables } from "./utils.js";

const SIM_K = 6;
const RUNS = Number(process.env.LANDED_BLOCKS_FORK_SIM_RUNS ?? "6");
const databases = testDatabases();
let universe: SimUniverse;
let binarySha256: string;

beforeAll(async () => {
  await prepareEventFlatDigest();
  binarySha256 = createHash("sha256")
    .update(await readFile(nativeOwnerBinaryPath))
    .digest("hex");
  universe = await createSimUniverse();
}, 600_000);

afterAll(async () => {
  await databases.dropAll();
}, DROP_ALL_TIMEOUT_MS);

const openSqlite: ForkRunOptions["open"] = (optionsFor) =>
  Promise.resolve(
    openSqliteFactStore({ ...optionsFor("sqlite"), path: ":memory:" }),
  );

const openPostgres: ForkRunOptions["open"] = async (
  optionsFor,
): Promise<FactStore> =>
  openPostgresFactStore({
    ...optionsFor("postgres"),
    connection: { connectionString: await databases.create() },
  });

/** A native owner at the genesis root, in a fresh directory. */
const openOwner = async (): Promise<
  SimOwner & { dispose: () => Promise<void> }
> => {
  const directory = await mkdtemp(join(tmpdir(), "midgard-landed-sim-"));
  const options = {
    levelPath: join(directory, "ledger"),
    sidecarPath: join(directory, "ledger.sidecar"),
    binaryPath: nativeOwnerBinaryPath,
    binarySha256,
  };
  const seed = new Level<string, unknown>(options.levelPath, {
    valueEncoding: "json",
  });
  await seed.open();
  await seed.put("__root__", SDK.EMPTY_MERKLE_TREE_ROOT);
  await seed.close();
  const first = await ProductionNativeMpfOwnerService.create(options);
  const handle = await first.fork(SDK.EMPTY_MERKLE_TREE_ROOT);
  const applied = await first.applyEvents(
    handle,
    encodeNativeMpfEventLog(SDK.EMPTY_MERKLE_TREE_ROOT, [
      universe.genesis.map((entry) =>
        ledgerOutputToInsertBatchOp({
          outRef: entry.outref,
          outputCbor: entry.output,
        }),
      ),
    ]),
  );
  expect(applied.candidateRoot).toBe(universe.root(0, 0));
  await first.promote(handle);
  const owner = {
    current: first,
    reopen: async () => {
      await owner.current.close();
      owner.current = await ProductionNativeMpfOwnerService.create(options);
    },
    dispose: async () => {
      await owner.current.close();
      await rm(directory, { recursive: true, force: true });
    },
  };
  return owner;
};

const runScenario = (
  runtime: Runtime.Runtime<Database>,
  scenario: ForkScenario,
  open: ForkRunOptions["open"],
  stats: LandedSimStats,
  label: string,
  node: Readonly<{
    offlineFor: number;
    lateFor: number;
    mergeHeavy: boolean;
  }> = {
    // Every third scenario starts with the node down.
    offlineFor: label.length % 3 === 0 ? 8 : 0,
    lateFor: 6,
    mergeHeavy: false,
  },
) =>
  Effect.gen(function* () {
    yield* resetApplicationTables;
    yield* insertDeposits(universe.deposits.map(({ row }) => row));
    yield* insertSimCursor;
    // Node startup seeds the working ledger with the genesis outputs.
    yield* MempoolLedgerDB.insert([
      ...(yield* ledgerRows(universe.genesis, new Map())),
    ]);
    const outcome = yield* Effect.acquireUseRelease(
      Effect.promise(openOwner),
      (owner) =>
        Effect.promise(() =>
          runForkScenario(scenario, {
            open,
            k: SIM_K,
            projections: [
              landedBlocksSimProjection({
                universe,
                registry: newSimRegistry(),
                runtime,
                owner,
                mempool: newSimMempool(),
                stats,
                label,
                book: newSimOwnBook(),
                ...node,
                includes: new Map(),
                lateUntil: new Map(),
                published: { position: null },
              }),
            ],
          }),
        ),
      (owner) => Effect.promise(() => owner.dispose()),
    );
    if (!outcome.ok)
      return yield* Effect.die(
        new Error(`${label} step ${outcome.step}: ${outcome.reason}`),
      );
    return outcome.stats.prunes;
  });

const inNode = <A>(
  work: (
    runtime: Runtime.Runtime<Database>,
  ) => Effect.Effect<A, unknown, Database>,
) =>
  Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const runtime = yield* Effect.runtime<Database>();
        return yield* work(runtime);
      }),
    ),
  );

const expectEveryCase = (stats: LandedSimStats, prunes: number): void => {
  console.info("landed-blocks fork-sim", JSON.stringify(stats));
  expect(prunes).toBeGreaterThan(0);
  for (const field of [
    "comparedChecks",
    "prunedChecks",
    "appends",
    "merges",
    "tailRemovals",
    "folds",
    "badBlocks",
    "invalidHeld",
    "awaitingDaHeld",
    "transientHeld",
    "rebases",
    "restoredRoots",
    "crashResumes",
    "ownerRestarts",
    "deferredRebases",
    "relandedAppends",
    "relands",
    "unfolds",
    "retainedFolds",
    "prunedFolds",
    "rollbacksRemovingProcessed",
    "admitted",
    "directRejections",
    "dependentRejections",
    "rejectionsOnRollback",
    "latentHoleClosed",
    "foreignIncluded",
    "batchRejections",
    "batchSettled",
    // The own-block variant (`foldThenRejectOwn`) is too rare in this corpus
    // to count on; landed-blocks-rebase.test.ts pins it directly ("keeps a
    // batch co-member settled by this node's own block folded ...").
    "foldThenRejectForeign",
    "ownCommits",
    "ownAppends",
    "liveRebases",
    "ownProcessed",
    "ownMerges",
    "ownResolutions",
    "ownOnRemovedBase",
    "ownRevivals",
    "revivalUnrejected",
    "unrejectedRestored",
    "offlineChecks",
    "coalescedMerges",
    "bootstrapsPastGenesis",
    "heldPastMerge",
  ] as const)
    expect({ field, count: stats[field] > 0 }).toEqual({ field, count: true });
};

/** Long enough for blocks to land, merge, be rolled back and reland. */
const LONG: ForkScenario = {
  seed: 0x0b3,
  episodes: Array.from({ length: 10 }, (_, index) => ({
    shape: (
      ["reland", "never_reland", "changed_valid_to", "new_fork_only"] as const
    )[index % 4]!,
    depth: 1 + (index % SIM_K),
    extra: 1 + (index % 2),
    landAt: index,
    variant: index,
    lead: 3,
    prune: index % 2 === 1,
  })),
};

describe.each([
  ["sqlite", openSqlite],
  ["postgres", openPostgres],
] as const)(
  "landed-block processing under the fork simulator (%s)",
  (name, open) => {
    it("equals a fresh derivation from the canonical chain over the fork corpus (prune on) and a long run", async () => {
      const stats = zeroLandedSimStats();
      const prunes = await inNode((runtime) =>
        Effect.gen(function* () {
          let total = 0;
          let index = 0;
          for (const { scenario } of forkCorpus(SIM_K))
            total += yield* runScenario(
              runtime,
              scenario,
              open,
              stats,
              `${name}:corpus:${index++}`,
            );
          total += yield* runScenario(
            runtime,
            LONG,
            open,
            stats,
            `${name}:long`,
          );
          // The root moves often: the node comes up past several merges,
          // and a late payload stays missing past its block's merge.
          total += yield* runScenario(
            runtime,
            { ...LONG, seed: 0x0b4 },
            open,
            stats,
            `${name}:long-down`,
            { offlineFor: 40, lateFor: 24, mergeHeavy: true },
          );
          return total;
        }),
      );
      expectEveryCase(stats, prunes);
    }, 1_800_000);

    it(`equals a fresh derivation for ${RUNS} random scenarios (fast-check, prune on)`, async () => {
      const stats = zeroLandedSimStats();
      let index = 0;
      await inNode((runtime) =>
        Effect.promise(() =>
          fc.assert(
            fc.asyncProperty(forkScenarioArbitrary(SIM_K), (scenario) =>
              Runtime.runPromise(runtime)(
                runScenario(
                  runtime,
                  scenario,
                  open,
                  stats,
                  `${name}:fc:${index++}`,
                ),
              ).then(() => undefined),
            ),
            { numRuns: RUNS, seed: 0x0b3_0001 },
          ),
        ),
      );
      console.info("landed-blocks fork-sim (random)", JSON.stringify(stats));
      expect(stats.comparedChecks).toBeGreaterThan(0);
    }, 1_800_000);
  },
);
