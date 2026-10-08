/**
 * The node operator set under the fork simulator (plan §15 F8, NC14): its
 * traffic runs the operator lifecycle (registration, activation, strikes,
 * scheduler shifts, retirement, slash, bond recovery) across the corpus's
 * rollbacks and prunes. After every chain-sync event the set the mirror
 * kept by reading only changed rows equals a fresh load of the same facts,
 * membership included, and its live lists equal a load of a fresh
 * forward-only replay. A second mirror reads only after the steps that
 * prune, as a node whose hook runs lag the follower: the prune has passed
 * its last read, so it reloads and equals a fresh load too. Each suite proves
 * it exercised both read paths, a prune past a read and every membership
 * state.
 */
import type { FollowerProjection } from "@al-ft/midgard-l1-follower";
import {
  type FactStore,
  openPostgresFactStore,
  openSqliteFactStore,
  type SqlTx,
} from "@al-ft/midgard-l1-follower";
import {
  forkCorpus,
  type ForkRunOptions,
  type ForkScenario,
  forkScenarioArbitrary,
  runForkScenario,
  type ScenarioTraffic,
  type SimOutput,
} from "@al-ft/midgard-l1-follower/testing";
import fc from "fast-check";
import { afterAll, beforeAll, describe, expect, it } from "vitest";

import {
  classifyOperatorMembership,
  createOperatorSetMirror,
  type OperatorMembershipState,
  type OperatorSet,
  type OperatorSetMirror,
  operatorSetProjection,
  type OperatorSetRead,
} from "../src/l1-operator-set/index.js";
import {
  DROP_ALL_TIMEOUT_MS,
  testDatabases,
} from "./helpers/l1-events-store.js";
import {
  loadOperatorSetChainFixture,
  OPERATOR_LISTS,
  operatorKey,
  OperatorSetChain,
  type OperatorSetChainFixture,
  txOf,
  type TxParts,
} from "./helpers/operator-set-chain.js";

const SIM_K = 6;
const RUNS = Number(process.env.L1_OPERATOR_SET_FORK_SIM_RUNS ?? "8");
const OWN = operatorKey(0x50);
const KEYS = [OWN, operatorKey(0x20), operatorKey(0x80), operatorKey(0xc0)];

const databases = testDatabases();
let fixture: OperatorSetChainFixture;

beforeAll(async () => {
  fixture = await loadOperatorSetChainFixture();
}, 120_000);
afterAll(async () => {
  await databases.dropAll();
}, DROP_ALL_TIMEOUT_MS);

type Stats = {
  checks: number;
  incremental: number;
  loads: number;
  /** Lagging reads in one generation that reloaded: a prune passed the last read. */
  prunedPastRead: number;
  rowsRead: number;
  states: Record<OperatorMembershipState, number>;
  operations: Record<string, number>;
};

const zeroStats = (): Stats => ({
  checks: 0,
  incremental: 0,
  loads: 0,
  prunedPastRead: 0,
  rowsRead: 0,
  states: { unknown: 0, active: 0, awaiting_activation: 0, removed: 0 },
  operations: {},
});

/** One operator lifecycle step per block, over the model's live outputs. */
const lifecycleTraffic =
  (lists: OperatorSetChain, stats: Stats): ScenarioTraffic =>
  ({ chain, rng, claim }) => {
    const live = chain.live();
    const landed = (name: string, parts: readonly TxParts[]) => {
      const inputs = parts.flatMap((part) => part.inputs);
      if (!inputs.every((input) => claim(input))) return [];
      stats.operations[name] = (stats.operations[name] ?? 0) + 1;
      return [txOf(parts, chain.nonce())];
    };
    if (lists.nodes(live, "active").length === 0)
      return landed("genesis", [lists.genesis(chain.outsideInput())]);
    const inList = (list: (typeof OPERATOR_LISTS)[number]) =>
      KEYS.filter((key) => lists.find(live, list, key) !== undefined);
    const registered = inList("registered");
    const active = inList("active");
    const retired = inList("retired");
    const unlisted = KEYS.filter(
      (key) =>
        !registered.includes(key) &&
        !active.includes(key) &&
        !retired.includes(key),
    );
    const options: (() => ReturnType<ScenarioTraffic>)[] = [];
    if (unlisted.length > 0)
      options.push(() =>
        landed("register", [
          lists.insert(live, "registered", rng.pick(unlisted)),
        ]),
      );
    if (registered.length > 0)
      options.push(() => {
        const key = rng.pick(registered);
        return landed("activate", [
          lists.remove(live, "registered", key),
          lists.insert(live, "active", key),
        ]);
      });
    if (active.length > 0) {
      options.push(() =>
        landed("strike", [lists.strike(live, rng.pick(active))]),
      );
      options.push(() =>
        landed("shift", [
          lists.shift(live, rng.pick(active), BigInt(chain.nonce())),
        ]),
      );
      options.push(() => {
        const key = rng.pick(active);
        return landed("retire", [
          lists.remove(live, "active", key),
          lists.insert(live, "retired", key),
        ]);
      });
      options.push(() =>
        landed("slash", [lists.remove(live, "active", rng.pick(active))]),
      );
    }
    if (retired.length > 0)
      options.push(() =>
        landed("recover", [lists.remove(live, "retired", rng.pick(retired))]),
      );
    if (options.length === 0 || !rng.chance(0.7)) return [];
    return rng.pick(options)();
  };

/** What the set holds, comparable across reads; `withActivity` adds the own history. */
const summary = (set: OperatorSet, withActivity: boolean) => ({
  registered: set.registered.map((node) => node.assetName),
  active: set.active.map((node) => [
    node.assetName,
    node.active?.inactivity_strikes.toString() ?? null,
  ]),
  ownRetired: set.ownRetired?.node.assetName ?? null,
  scheduler: `${set.scheduler?.utxo.txHash}#${set.scheduler?.utxo.outputIndex}`,
  hubOracle: `${set.hubOracle?.utxo.txHash}#${set.hubOracle?.utxo.outputIndex}`,
  unhealthy: set.unhealthy,
  ...(withActivity
    ? {
        ownActivity: [...set.ownActivity].sort((a, b) =>
          a.outRef < b.outRef ? -1 : 1,
        ),
        membership: classifyOperatorMembership(set, false).state,
      }
    : {}),
});

const differ = (a: unknown, b: unknown): string | null => {
  const left = JSON.stringify(a);
  const right = JSON.stringify(b);
  return left === right ? null : `${left} != ${right}`;
};

const refreshOf = (
  store: FactStore,
  mirror: OperatorSetMirror,
): Promise<OperatorSetRead> =>
  store.transaction("read", (tx: SqlTx) => mirror.refresh(tx, store.dialect));

/** The operator set plugged into the simulator: traffic, protection, check. */
const operatorSetSimProjection = (stats: Stats): FollowerProjection => {
  const lists = new OperatorSetChain(fixture);
  const policies = new Set(
    [
      ...OPERATOR_LISTS.map((list) => fixture.config[list]),
      fixture.config.scheduler,
      fixture.config.hubOracle,
    ].map((contract) => contract.policyId),
  );
  const fresh = () =>
    createOperatorSetMirror({ config: fixture.config, ownKey: OWN });
  const kept = fresh();
  const lagging = fresh();
  return {
    ...operatorSetProjection(fixture.config),
    traffic: lifecycleTraffic(lists, stats),
    protects: (output: SimOutput) =>
      [...(output.assets?.keys() ?? [])].some((policy) => policies.has(policy)),
    check: async ({ store, reference, step }) => {
      stats.checks += 1;
      // The lagging mirror reads first, so `loaded` below is fresh either way.
      const lastRead = lagging.view();
      if (lastRead === null || step.prune === true) {
        const lagged = await refreshOf(store, lagging);
        const load = await refreshOf(store, fresh());
        if (lagged.kind !== "ok" || load.kind !== "ok")
          return `lagging read: ${lagged.kind}/${load.kind}`;
        if (
          lastRead !== null &&
          lagged.loaded &&
          lagged.set.view.generation === lastRead.generation
        )
          stats.prunedPastRead += 1;
        const lag = differ(summary(lagged.set, true), summary(load.set, true));
        if (lag !== null) return `lagging set != fresh load: ${lag}`;
      }
      const incremental = await refreshOf(store, kept);
      const loaded = await refreshOf(store, fresh());
      const replayed = await refreshOf(reference, fresh());
      if (
        incremental.kind !== "ok" ||
        loaded.kind !== "ok" ||
        replayed.kind !== "ok"
      )
        return `read: ${incremental.kind}/${loaded.kind}/${replayed.kind}`;
      if (!loaded.loaded) return "a fresh mirror did not load";
      if (incremental.loaded) stats.loads += 1;
      else stats.incremental += 1;
      stats.rowsRead += incremental.rowsRead;
      stats.states[classifyOperatorMembership(incremental.set, false).state] +=
        1;
      const kept_ = differ(
        summary(incremental.set, true),
        summary(loaded.set, true),
      );
      if (kept_ !== null) return `kept set != fresh load: ${kept_}`;
      const replay = differ(
        summary(incremental.set, false),
        summary(replayed.set, false),
      );
      return replay === null ? null : `kept set != replay: ${replay}`;
    },
  };
};

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

const run = async (
  scenario: ForkScenario,
  open: ForkRunOptions["open"],
  stats: Stats,
): Promise<void> => {
  const outcome = await runForkScenario(scenario, {
    open,
    k: SIM_K,
    projections: [operatorSetSimProjection(stats)],
  });
  if (!outcome.ok) throw new Error(`step ${outcome.step}: ${outcome.reason}`);
};

/** Long enough for every lifecycle step to land, roll back and land again. */
const LONG: ForkScenario = {
  seed: 0x0b5,
  episodes: Array.from({ length: 10 }, (_, index) => ({
    shape: (
      ["reland", "never_reland", "changed_valid_to", "new_fork_only"] as const
    )[index % 4]!,
    depth: 1 + (index % SIM_K),
    extra: 1 + (index % 2),
    landAt: index,
    variant: index,
    lead: 3,
  })),
};

describe.each([
  ["sqlite", openSqlite],
  ["postgres", openPostgres],
] as const)("node operator set under the fork simulator (%s)", (_, open) => {
  it("holds over the follower's fork corpus and a long lifecycle run", async () => {
    const stats = zeroStats();
    for (const { scenario } of forkCorpus(SIM_K))
      await run(scenario, open, stats);
    await run(LONG, open, stats);
    console.info("node operator set fork-sim", JSON.stringify(stats));
    expect(stats.incremental).toBeGreaterThan(0);
    expect(stats.loads).toBeGreaterThan(0);
    expect(stats.prunedPastRead).toBeGreaterThan(0);
    expect(stats.rowsRead).toBeGreaterThan(0);
    for (const state of [
      "unknown",
      "active",
      "awaiting_activation",
      "removed",
    ] as const)
      expect({ state, seen: stats.states[state] > 0 }).toEqual({
        state,
        seen: true,
      });
    for (const operation of [
      "genesis",
      "register",
      "activate",
      "strike",
      "shift",
      "retire",
      "slash",
      "recover",
    ])
      expect({
        operation,
        seen: (stats.operations[operation] ?? 0) > 0,
      }).toEqual({ operation, seen: true });
  });

  it(`holds for ${RUNS} random scenarios (fast-check)`, async () => {
    const stats = zeroStats();
    await fc.assert(
      fc.asyncProperty(forkScenarioArbitrary(SIM_K), (scenario) =>
        run(scenario, open, stats),
      ),
      { numRuns: RUNS, seed: 0x0b5_0001 },
    );
    expect(stats.checks).toBeGreaterThan(0);
  });
});
