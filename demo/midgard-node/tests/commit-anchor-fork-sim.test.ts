/**
 * The commit anchor under the follower's fork simulator (plan §8.1): at
 * every chain-sync event a commit is planned at the follower's view P with
 * the anchor A, the block d below P, and the end-time cap
 * time(A) + event_wait - 1 (`commitAnchorCapMs`). Every event the cap
 * admits sits strictly below A, whatever its validity bound at or after its
 * block's start; and while the anchor rule (`commitAnchorCanonicalText`, run
 * on the follower's own Postgres store, pruning included) says A is
 * canonical, it is, and so is every included event's block. A commit whose
 * anchor the rule reports gone is dropped, as the own-journal disposition
 * drops its journal at that rewind.
 */
import {
  type FactStore,
  type FollowerProjection,
  openPostgresFactStore,
  type StoredBlock,
} from "@al-ft/midgard-l1-follower";
import {
  forkCorpus,
  type ForkRunOptions,
  type ForkScenario,
  forkScenarioArbitrary,
  runForkScenario,
} from "@al-ft/midgard-l1-follower/testing";
import { EVENT_WAIT_DURATION_MS } from "@al-ft/midgard-sdk";
import fc from "fast-check";
import { afterAll, describe, expect, it } from "vitest";

import {
  type CommitAnchor,
  commitAnchorCanonicalText,
  commitAnchorCapMs,
  commitAnchorHeight,
} from "../src/database/commit-anchor.js";
import { testDatabases } from "./helpers/l1-events-store.js";

const SIM_K = 6;
const RUNS = Number(process.env.COMMIT_ANCHOR_FORK_SIM_RUNS ?? "8");
const databases = testDatabases();

afterAll(async () => {
  await databases.dropAll();
});

const openPostgres: ForkRunOptions["open"] = async (
  optionsFor,
): Promise<FactStore> =>
  openPostgresFactStore({
    ...optionsFor("postgres"),
    connection: { connectionString: await databases.create() },
  });

/** Model time: 1 s slots from zero. */
const slotTime = (slot: number) => slot * 1000;

/**
 * Inclusive validity upper bounds an event in a block can carry: at the
 * block's start and just after it. The cap must hold for every bound at or
 * after the block's start, not only the slot-aligned ones a ledger forms.
 */
const VALIDITY_OFFSETS_MS = [0, 1, 999, 1000] as const;

type Stats = {
  planned: number;
  included: number;
  /** Events in A itself or above it that the cap excluded. */
  excludedAtOrAbove: number;
  anchorChecks: number;
  anchorsGone: number;
  prunedAnchors: number;
};

type Planned = Readonly<{ anchor: CommitAnchor; members: readonly Buffer[] }>;

const anchorCanonical = async (
  store: FactStore,
  anchor: CommitAnchor,
): Promise<boolean> => {
  const rows = await store.transaction("read", (tx) =>
    tx.query(
      `SELECT ${commitAnchorCanonicalText("a")} AS canonical
        FROM (SELECT CAST(? AS bytea) AS commit_anchor_hash,
            CAST(? AS bigint) AS commit_anchor_height,
            CAST(? AS bigint) AS commit_anchor_slot) a`,
      [anchor.hash, anchor.height, anchor.slot],
    ),
  );
  return rows[0]?.canonical === true;
};

const anchorProjection = (depth: number, stats: Stats): FollowerProjection => {
  let planned: Planned[] = [];
  return {
    name: `commit anchor d=${depth.toString()}`,
    check: async ({ store, reference }) => {
      // Earlier commits: the anchor rule against the chain, and the members.
      const kept: Planned[] = [];
      for (const commit of planned) {
        stats.anchorChecks += 1;
        const canonical = await anchorCanonical(store, commit.anchor);
        const truth = await reference.isCanonical(commit.anchor.hash);
        if (canonical !== truth)
          return `anchor at height ${commit.anchor.height.toString()}: rule ${String(canonical)}, chain ${String(truth)}`;
        if (!canonical) {
          stats.anchorsGone += 1;
          continue;
        }
        if ((await store.blockByHash(commit.anchor.hash)) === null)
          stats.prunedAnchors += 1;
        for (const member of commit.members)
          if (!(await reference.isCanonical(member)))
            return `a member of a commit anchored at height ${commit.anchor.height.toString()} left the chain while the anchor stayed`;
        kept.push(commit);
      }
      planned = kept;
      // A new commit at the current view.
      const view = await store.currentView();
      if (view === null) return null;
      const anchorBlock = await store.blockAtHeight(
        commitAnchorHeight(view.height, depth),
      );
      if (anchorBlock === null) return null;
      const anchor: CommitAnchor = {
        hash: anchorBlock.hash,
        height: anchorBlock.height,
        slot: anchorBlock.slot,
      };
      const endMs = commitAnchorCapMs(slotTime(anchor.slot));
      const members: Buffer[] = [];
      for (
        let height = Math.max(0, anchor.height - 3);
        height <= view.height;
        height += 1
      ) {
        const block: StoredBlock | null = await store.blockAtHeight(height);
        if (block === null) continue;
        for (const offset of VALIDITY_OFFSETS_MS) {
          const inclusion =
            slotTime(block.slot) + offset + EVENT_WAIT_DURATION_MS;
          if (inclusion > endMs) {
            if (block.height >= anchor.height) stats.excludedAtOrAbove += 1;
            continue;
          }
          if (block.height >= anchor.height)
            return `an event of the block at height ${block.height.toString()} (validity ${offset.toString()} ms after its start) is due by the end time of a commit anchored at height ${anchor.height.toString()}`;
          stats.included += 1;
          members.push(block.hash);
        }
      }
      stats.planned += 1;
      planned.push({ anchor, members });
      if (planned.length > 3 * SIM_K) planned = planned.slice(-3 * SIM_K);
      return null;
    },
  };
};

const zeroStats = (): Stats => ({
  planned: 0,
  included: 0,
  excludedAtOrAbove: 0,
  anchorChecks: 0,
  anchorsGone: 0,
  prunedAnchors: 0,
});

const run = async (scenario: ForkScenario, depth: number, stats: Stats) => {
  const outcome = await runForkScenario(scenario, {
    open: openPostgres,
    k: SIM_K,
    projections: [anchorProjection(depth, stats)],
  });
  if (!outcome.ok) throw new Error(`step ${outcome.step}: ${outcome.reason}`);
};

describe("commit anchor under the fork simulator", () => {
  it.each([0, 2, SIM_K])(
    "admits only events strictly below the anchor, and keeps them while it is canonical (d = %i, corpus)",
    async (depth) => {
      const stats = zeroStats();
      for (const { scenario } of forkCorpus(SIM_K))
        await run(scenario, depth, stats);
      console.info("commit anchor fork-sim", depth, JSON.stringify(stats));
      expect(stats.planned).toBeGreaterThan(0);
      expect(stats.included).toBeGreaterThan(0);
      expect(stats.excludedAtOrAbove).toBeGreaterThan(0);
      // At d = k the anchor is k + 1 blocks deep: no rollback the
      // simulator allows (at most k) removes it.
      if (depth === SIM_K) expect(stats.anchorsGone).toBe(0);
      else expect(stats.anchorsGone).toBeGreaterThan(0);
      expect(stats.prunedAnchors).toBeGreaterThan(0);
    },
    300_000,
  );

  it(`holds for ${RUNS.toString()} random scenarios and depths (fast-check)`, async () => {
    const stats = zeroStats();
    await fc.assert(
      fc.asyncProperty(
        forkScenarioArbitrary(SIM_K),
        fc.integer({ min: 0, max: SIM_K }),
        (scenario, depth) => run(scenario, depth, stats),
      ),
      { numRuns: RUNS, seed: 0x0a_0c40 },
    );
    expect(stats.planned).toBeGreaterThan(0);
    expect(stats.included).toBeGreaterThan(0);
  }, 300_000);
});
