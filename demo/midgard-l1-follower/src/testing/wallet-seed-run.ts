import {
  createWalletSeeder,
  type WalletLedger,
  type WalletSeeder,
  withTrackedAddresses,
} from "../follow/wallet-seed.js";
import type { FactStore } from "../store/fact-store.js";
import { insertSeedRowsIn, type SeedOutput } from "../store/seed.js";
import type { BlockSummary, TrackedSet } from "../types.js";
import { outRefHex, type SimOrigin, type SimUtxo } from "./sim-chain.js";
import {
  keysAt,
  readSeedRows,
  type SeedRow,
  seedRowsDiffer,
  simLedger,
  simUtxoSet,
  walletViewDiffer,
} from "./sim-wallet-ledger.js";

/**
 * Own wallets seeded from the simulated ledger (plan §5.3 step 4, F4): the
 * wallets join the tracked set, the ledger starts with `preOrigin`, and the
 * wallet seeder runs after every event from event `startAfter` on, so later
 * rollbacks cross its seed points. `added` wallets join through
 * `addWallets` after event `atEvent`, so the seed reads outputs paid to them
 * after the origin while they were untracked. A rewind deletes the seed rows
 * above its target and keeps the others. After every event the store's seed
 * rows must be exactly the ones seeded and not rewound (new rows only at the
 * cursor), the fresh-replay reference carries the same rows, and once the
 * seeder is ready the store's live rows at every wallet are exactly the
 * ledger's UTxOs there (no phantom, none missing).
 */
export type ForkWalletSeed = Readonly<{
  wallets: readonly Buffer[];
  preOrigin: readonly SimUtxo[];
  startAfter: number;
  added?: Readonly<{ wallets: readonly Buffer[]; atEvent: number }>;
  /** Replaces the production seeder (red checks). */
  seeder?: (
    input: Readonly<{
      store: FactStore;
      ledger: WalletLedger;
      wallets: readonly Buffer[];
    }>,
  ) => WalletSeeder;
}>;

export type SeedStats = {
  /** Seed rows written (wallet seed runs only). */
  seedRows: number;
  /** Rewinds whose target lay below a stored seed row's seed point. */
  rewindsBelowSeed: number;
  /** Seed rows a rewind made live again (spent above its target). */
  seedUnspends: number;
  /** Seed rows a rewind deleted (seeded above its target). */
  seedRowsRewound: number;
  /** Seed rows of outputs created after the origin (added wallets). */
  postOriginSeeds: number;
  /** Of those, seed rows of an output a stored tx created. */
  storedTxSeeds: number;
  /** Post-origin seed rows whose creating block a rewind removed. */
  orphanedSeeds: number;
};

export const zeroSeedStats = (): SeedStats => ({
  seedRows: 0,
  rewindsBelowSeed: 0,
  seedUnspends: 0,
  seedRowsRewound: 0,
  postOriginSeeds: 0,
  storedTxSeeds: 0,
  orphanedSeeds: 0,
});

/** The tracked set with the bootstrap wallets (not the added ones). */
export const trackedWithWallets = (
  tracked: TrackedSet,
  walletSeed: ForkWalletSeed | undefined,
): TrackedSet =>
  walletSeed === undefined
    ? tracked
    : withTrackedAddresses(tracked, walletSeed.wallets);

export type SeedRun = Readonly<{
  /** Creates the seeder; call once the store is initialized. */
  start(): void;
  /**
   * After every event: adds the wallets when due, seeds, and checks the
   * seed rows and the wallet view. A failure as text, or whether the
   * reference must be rebuilt.
   */
  afterEvent(index: number): Promise<string | { rebuild: boolean }>;
  /**
   * After a prune: seed rows the store no longer holds stop being expected
   * there (the reference still replays them). The runner's retention
   * comparison, run right after, fails if any of them was still retained.
   */
  afterPrune(): Promise<void>;
  /** The reference replay's step at `height`: tracks and seeds as the store did. */
  replay(reference: FactStore, height: number): Promise<void>;
  /** The model's live tracked keys, as the store can know them now. */
  liveTracked(model: readonly string[]): string[];
  close(): void;
}>;

export const createSeedRun = (
  input: Readonly<{
    store: FactStore;
    walletSeed: ForkWalletSeed | undefined;
    canonical: readonly BlockSummary[];
    origin: SimOrigin;
    stats: SeedStats;
  }>,
): SeedRun => {
  const { store, walletSeed, canonical, origin, stats } = input;
  /**
   * Every seed row the store holds, as written, with the cursor's height
   * when it was seeded: blocks at or below that height were applied without
   * it, every later block with it. A rewind below its seed point removes it.
   */
  const seeded = new Map<string, SeedRow & { knownFrom: number }>();
  /** Seed rows (keys of `seeded`) a prune removed from the store. */
  const prunedSeeds = new Set<string>();
  const expectedInStore = (): Map<string, SeedRow> =>
    new Map([...seeded].filter(([key]) => !prunedSeeds.has(key)));
  /**
   * The height from which the store has tracked the added wallets (null
   * before `addWallets`): the cursor's height when they were added, lowered
   * by every later rewind below it, since tracking outlives a rewind.
   */
  let addedFrom: number | null = null;
  const added = walletSeed?.added?.wallets ?? [];
  const preOriginKeys = new Set(
    (walletSeed?.preOrigin ?? []).map((utxo) => outRefHex(utxo.outRef)),
  );
  let seeder: WalletSeeder | undefined;
  let seederRan = false;
  const unsubscribe = store.onGeneration(({ rewound }) => {
    // Runs inside the event, before the runner drops the rolled-back blocks.
    const createdAbove = new Set(
      canonical.flatMap((block) =>
        block.point.slot > rewound.to.slot
          ? block.txs.map((tx) => tx.hash.toString("hex"))
          : [],
      ),
    );
    let below = false;
    for (const [key, row] of seeded)
      if (rewound.to.slot < row.seedSlot) {
        below = true;
        seeded.delete(key);
        prunedSeeds.delete(key);
        stats.seedRowsRewound += 1;
        if (createdAbove.has(row.seed.outRef.txHash.toString("hex")))
          stats.orphanedSeeds += 1;
      }
    if (below) stats.rewindsBelowSeed += 1;
    if (addedFrom !== null)
      addedFrom = Math.min(addedFrom, rewound.cursor.height);
    for (const outRef of rewound.unspent)
      if (seeded.has(outRefHex(outRef))) stats.seedUnspends += 1;
  });
  const ledgerNow = (): Map<string, SimUtxo> =>
    simUtxoSet(walletSeed?.preOrigin ?? [], canonical);

  const seedStep = async (
    run: WalletSeeder,
    seed: ForkWalletSeed,
  ): Promise<string | boolean> => {
    const status = await run.step();
    if (status.kind !== "ready")
      return `wallet seed: ${status.reason}: ${status.detail}`;
    seederRan = true;
    const view = await walletViewDiffer(store, ledgerNow(), [
      ...seed.wallets,
      ...(addedFrom === null ? [] : added),
    ]);
    if (view !== null) return `after the seed: ${view}`;
    const cursor = await store.cursor();
    const height = cursor?.height ?? origin.height;
    const now = await readSeedRows(store);
    let grew = false;
    for (const [key, row] of now)
      if (!seeded.has(key)) {
        if (row.seedSlot !== cursor?.point.slot)
          return `seed row ${key} appeared unseeded at seed slot ${row.seedSlot}`;
        seeded.set(key, { ...row, knownFrom: height });
        stats.seedRows += 1;
        if (!preOriginKeys.has(key)) {
          stats.postOriginSeeds += 1;
          if ((await store.txByHash(row.seed.outRef.txHash)) !== null)
            stats.storedTxSeeds += 1;
        }
        grew = true;
      }
    const drift = seedRowsDiffer(expectedInStore(), now);
    return drift === null ? grew : `seed: ${drift}`;
  };

  return {
    start: () => {
      if (walletSeed === undefined) return;
      seeder = (walletSeed.seeder ?? createWalletSeeder)({
        store,
        ledger: simLedger(origin.point, walletSeed.preOrigin, () => canonical),
        wallets: walletSeed.wallets,
      });
    },
    afterEvent: async (index) => {
      if (walletSeed === undefined || seeder === undefined)
        return { rebuild: false };
      let rebuild = false;
      if (
        added.length > 0 &&
        addedFrom === null &&
        index + 1 >= (walletSeed.added?.atEvent ?? 0)
      ) {
        seeder.addWallets(added);
        addedFrom = (await store.cursor())?.height ?? origin.height;
        rebuild = true;
      }
      if (index + 1 < walletSeed.startAfter) {
        // A rewind deletes exactly the seed rows above its target (§7.1).
        const touched = seedRowsDiffer(
          expectedInStore(),
          await readSeedRows(store),
        );
        return touched === null ? { rebuild } : touched;
      }
      const result = await seedStep(seeder, walletSeed);
      return typeof result === "string"
        ? result
        : { rebuild: rebuild || result };
    },
    afterPrune: async () => {
      if (walletSeed === undefined) return;
      const now = await readSeedRows(store);
      for (const key of seeded.keys()) if (!now.has(key)) prunedSeeds.add(key);
    },
    replay: async (reference, height) => {
      if (height === addedFrom)
        reference.setTrackedSet(
          withTrackedAddresses(reference.trackedSet(), added),
        );
      const bySlot = new Map<number, SeedOutput[]>();
      for (const row of seeded.values())
        if (row.knownFrom === height)
          bySlot.set(row.seedSlot, [
            ...(bySlot.get(row.seedSlot) ?? []),
            row.seed,
          ]);
      if (bySlot.size === 0) return;
      await reference.transaction("write", async (tx) => {
        for (const [slot, seeds] of bySlot)
          await insertSeedRowsIn(tx, reference.dialect, slot, seeds);
      });
      // Reload the tracked-outref cache with the seed rows.
      await reference.start();
    },
    liveTracked: (model) =>
      [
        // Before the first seed the store cannot know pre-origin UTxOs; once
        // added, the added wallets' UTxOs join the model's tracked set.
        ...model.filter((key) => seederRan || !preOriginKeys.has(key)),
        ...(addedFrom === null ? [] : keysAt(ledgerNow(), added)),
      ].sort(),
    close: () => {
      unsubscribe();
      seeder?.close();
    },
  };
};
