import fc from "fast-check";
import { afterAll, describe, expect, it } from "vitest";

import {
  openPostgresFactStore,
  seedWallets,
  type WalletSeeder,
} from "../../src/index.js";
import { rewindFaults } from "../../src/store/fact-store.js";
import {
  forkCorpus,
  type ForkRunOptions,
  forkScenarioArbitrary,
  type ForkWalletSeed,
  runForkScenario,
  type ScenarioTraffic,
  simUniverse,
  type SimUtxo,
} from "../../src/testing/index.js";
import { FIXTURE_PROJECTION, openSqlite, SIM_K } from "../support/fork-sim.js";
import { testDatabases } from "../support/postgres.js";

const RUNS = Number(process.env.L1_FORK_SIM_RUNS ?? "200");
const POSTGRES_RUNS = Number(process.env.L1_FORK_SIM_POSTGRES_RUNS ?? "20");

/** An own wallet: an enterprise key address, tracked by address only. */
const WALLET = Buffer.concat([Buffer.of(0x60), Buffer.alloc(28, 0x77)]);
/** A wallet the role adds after the origin (untracked until then). */
const ADDED = Buffer.concat([Buffer.of(0x60), Buffer.alloc(28, 0x66)]);

const preOrigin = (n: number, address: Buffer): SimUtxo => {
  const txHash = Buffer.alloc(32, 0xdd);
  txHash.writeUInt16BE(n, 30);
  return {
    outRef: { txHash, index: n % 3 },
    output: {
      address,
      lovelace: BigInt(n + 1) * 1_000_000n,
      ...(n === 2
        ? {
            assets: new Map([["5f".repeat(28), new Map([["01", 5n]])]]),
            datum: Buffer.from([0xd8, 0x79, 0x9f, 0x02, 0xff]),
          }
        : {}),
    },
  };
};

/** Pre-origin UTxOs: six at the wallet, two at an address nothing tracks. */
const PRE_ORIGIN: readonly SimUtxo[] = [
  ...Array.from({ length: 6 }, (_, n) => preOrigin(n, WALLET)),
  preOrigin(6, simUniverse().untrackedAddress),
  preOrigin(7, simUniverse().untrackedAddress),
];

const pay = (address: Buffer, lovelace: number) => ({
  address,
  lovelace: BigInt(lovelace) * 1_000_000n,
});

/**
 * Payments into the wallets, so they also hold post-origin outputs: to the
 * added wallet alone (a tx the follower does not store while the wallet is
 * untracked), to the own wallet, or to both (a stored tx whose output to the
 * added wallet gets no row while that wallet is untracked).
 */
const walletTraffic: ScenarioTraffic = ({ chain, rng }) => {
  if (!rng.chance(0.5)) return [];
  const shape = rng.range(0, 2);
  const outputs =
    shape === 0
      ? [pay(ADDED, rng.range(1, 9))]
      : shape === 1
        ? [pay(WALLET, rng.range(1, 9))]
        : [pay(WALLET, rng.range(1, 9)), pay(ADDED, rng.range(1, 9))];
  return [{ inputs: [chain.outsideInput()], nonce: chain.nonce(), outputs }];
};

const PROJECTIONS = [
  FIXTURE_PROJECTION,
  { name: "wallet-traffic", traffic: walletTraffic },
];

const SEED: ForkWalletSeed = {
  wallets: [WALLET],
  preOrigin: PRE_ORIGIN,
  startAfter: 3,
  added: { wallets: [ADDED], atEvent: 6 },
};

/** The plan's literal reading: seed once, never again after a rewind. */
const seedOnce: ForkWalletSeed["seeder"] = ({ store, ledger, wallets }) => {
  let done = false;
  const seeder: WalletSeeder = {
    owed: () => (done ? [] : wallets),
    ready: () => done,
    addWallets: () => undefined,
    step: async () => {
      if (done) return { kind: "ready" };
      const result = await seedWallets(store, ledger, wallets);
      if (result.kind === "pending") return { ...result, wallets };
      done = true;
      return { kind: "ready" };
    },
    close: () => undefined,
  };
  return seeder;
};

const corpus = forkCorpus(SIM_K);
const databases = testDatabases();

afterAll(async () => {
  await databases.dropAll();
});

const openPostgres: ForkRunOptions["open"] = async (optionsFor) => {
  const database = await databases.create();
  return openPostgresFactStore({
    ...optionsFor("postgres"),
    connection: { connectionString: database.url },
  });
};

const adapters = [
  { name: "sqlite", open: openSqlite, runs: RUNS },
  { name: "postgres", open: openPostgres, runs: POSTGRES_RUNS },
] as const;

type Totals = Record<
  | "seedRows"
  | "rewindsBelowSeed"
  | "seedUnspends"
  | "seedRowsRewound"
  | "postOriginSeeds"
  | "storedTxSeeds"
  | "orphanedSeeds",
  number
>;

const zero = (): Totals => ({
  seedRows: 0,
  rewindsBelowSeed: 0,
  seedUnspends: 0,
  seedRowsRewound: 0,
  postOriginSeeds: 0,
  storedTxSeeds: 0,
  orphanedSeeds: 0,
});

const add = (totals: Totals, stats: Totals): void => {
  for (const key of Object.keys(totals) as (keyof Totals)[])
    totals[key] += stats[key];
};

const runCorpus = async (
  options: Partial<ForkRunOptions>,
): Promise<{ failures: string[]; totals: Totals }> => {
  const failures: string[] = [];
  const totals = zero();
  for (const { name, scenario } of corpus) {
    const outcome = await runForkScenario(scenario, {
      open: openSqlite,
      k: SIM_K,
      projections: PROJECTIONS,
      walletSeed: SEED,
      ...options,
    });
    if (!outcome.ok) failures.push(`${name}: ${outcome.reason}`);
    else add(totals, outcome.stats);
  }
  return { failures, totals };
};

describe.each(adapters)(
  "LSQ wallet seed under the fork simulator ($name)",
  ({ open, runs }) => {
    it("rewinds the seed rows above each target, keeps the rest, and re-seeds", async () => {
      const totals = zero();
      for (const { name, scenario } of corpus) {
        const outcome = await runForkScenario(scenario, {
          open,
          k: SIM_K,
          projections: PROJECTIONS,
          walletSeed: SEED,
        });
        expect(outcome, name).toMatchObject({ ok: true });
        add(totals, outcome.stats);
      }
      // The corpus must exercise what it claims: seeds, rewinds below a seed
      // point that delete seed rows, seed rows made live again, post-origin
      // seeds of an added wallet (some from stored txs), and post-origin
      // seeds whose creating tx a fork orphaned.
      expect(totals.seedRows).toBeGreaterThan(corpus.length);
      expect(totals.rewindsBelowSeed).toBeGreaterThan(0);
      expect(totals.seedRowsRewound).toBeGreaterThan(0);
      expect(totals.seedUnspends).toBeGreaterThan(0);
      expect(totals.postOriginSeeds).toBeGreaterThan(0);
      expect(totals.storedTxSeeds).toBeGreaterThan(0);
      expect(totals.orphanedSeeds).toBeGreaterThan(0);
      console.info(`corpus: ${JSON.stringify(totals)}`);
    });

    it(`holds for ${runs} random scenarios (fast-check)`, async () => {
      const totals = zero();
      await fc.assert(
        fc.asyncProperty(forkScenarioArbitrary(SIM_K), async (scenario) => {
          const outcome = await runForkScenario(scenario, {
            open,
            k: SIM_K,
            projections: PROJECTIONS,
            walletSeed: SEED,
          });
          if (!outcome.ok)
            throw new Error(`step ${outcome.step}: ${outcome.reason}`);
          add(totals, outcome.stats);
        }),
        { numRuns: runs, seed: 0x0f4_5eed },
      );
      expect(totals.seedRowsRewound).toBeGreaterThan(0);
    });
  },
);

describe("LSQ wallet seed red checks (SQLite)", () => {
  it("leaves a phantom post-origin wallet output when a rewind keeps seed rows above its target", async () => {
    const { failures } = await runCorpus({
      prepare: (store) =>
        rewindFaults.set(store, "keep_seed_rows_above_target"),
    });
    expect(failures.length).toBeGreaterThan(0);
    // The orphaned tx's output to the added wallet stays live after the
    // rewind and the re-seed, though the ledger no longer has it.
    expect(failures[0]).toMatch(
      /wallet output [0-9a-f]+ is live in the store but not in the ledger/u,
    );
    console.info(
      `keep_seed_rows_above_target: ${failures.length}/${corpus.length} fail; first: ${failures[0]}`,
    );
  });

  it("fails when the seed is not owed again after a rewind below its point", async () => {
    const { failures } = await runCorpus({
      walletSeed: { ...SEED, seeder: seedOnce },
    });
    expect(failures.length).toBeGreaterThan(0);
    console.info(
      `seed once: ${failures.length}/${corpus.length} fail; first: ${failures[0]}`,
    );
  });

  it("fails when the seed never runs (pre-origin wallet UTxOs missing)", async () => {
    const outcome = await runForkScenario(corpus[0]!.scenario, {
      open: openSqlite,
      k: SIM_K,
      projections: PROJECTIONS,
      walletSeed: {
        ...SEED,
        seeder: () => ({
          owed: () => [],
          ready: () => true,
          addWallets: () => undefined,
          step: async () => Promise.resolve({ kind: "ready" }),
          close: () => undefined,
        }),
      },
    });
    expect(outcome.ok).toBe(false);
    expect(!outcome.ok && outcome.reason).toContain(
      "is in the ledger but not live in the store",
    );
  });
});
