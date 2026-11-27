/**
 * The replayed ledger's hold on the store's roots (`replayed-ledger.ts`).
 *
 * Lookups, replays and `retain` overlap. A root a running replay started from
 * or is about to persist is pinned before the store is asked for it, and
 * every `retain` keeps the pinned roots. Opening a root first waits out each
 * `retain` already running, so a root that call drops is never opened
 * half-way through its deletion. A block is replayed by one caller at a time
 * (`exclusive`); a caller that waited finds the ledger its predecessor
 * persisted.
 */
import { ReplayLocalFault } from "./block-replay.faults.js";
import {
  type Ledger,
  LedgerNotRetained,
  type LedgerStore,
} from "./ledger-store/index.js";

/** A retained ledger and the release of its pin. */
export type HeldLedger = {
  readonly ledger: Ledger;
  readonly release: () => void;
};

/** A store failure other than a missing root says nothing of a block: it is
 * this process's own fault. */
export const storeCall = async <T>(
  what: string,
  run: () => Promise<T>,
): Promise<T> => {
  try {
    return await run();
  } catch (error) {
    if (error instanceof LedgerNotRetained) throw error;
    throw new ReplayLocalFault({
      message: `the ledger store failed to ${what}: ${error instanceof Error ? error.message : String(error)}`,
      cause: error,
    });
  }
};

export const createLedgerHolds = (store: LedgerStore) => {
  const pins = new Map<string, number>();
  const retaining = new Set<Promise<unknown>>();
  const locks = new Map<string, Promise<void>>();
  /** The roots this process has seen retained: one later found not retained
   * was lost, where a root never held is only not derived yet. */
  const seen = new Set<string>();

  const pin = (root: string) => pins.set(root, (pins.get(root) ?? 0) + 1);
  const unpin = (root: string) => {
    const count = (pins.get(root) ?? 1) - 1;
    if (count === 0) pins.delete(root);
    else pins.set(root, count);
  };
  /** A release that unpins `root` once, however often it is called. */
  const releaseOf = (root: string) => {
    let released = false;
    return () => {
      if (released) return;
      released = true;
      unpin(root);
    };
  };

  return {
    /** `root`'s retained ledger, pinned until released, or `undefined` when
     * the store does not retain it. */
    open: async (root: string): Promise<HeldLedger | undefined> => {
      pin(root);
      try {
        await Promise.allSettled([...retaining]);
        const ledger = await storeCall(`open ledger ${root}`, () =>
          store.at(root),
        );
        seen.add(root);
        return { ledger, release: releaseOf(root) };
      } catch (error) {
        unpin(root);
        if (error instanceof LedgerNotRetained) return undefined;
        throw error;
      }
    },
    /** Pins `root` ahead of persisting it; the release unpins it. */
    reserve: (root: string): (() => void) => {
      pin(root);
      return releaseOf(root);
    },
    /** Persists `ledger`, whose root `reserve` pinned. */
    persist: async (ledger: Ledger): Promise<void> => {
      await storeCall(`persist ledger ${ledger.root}`, () =>
        store.persist(ledger),
      );
      seen.add(ledger.root);
    },
    /** Runs `task` once every earlier task under `key` has settled. */
    exclusive: async <T>(key: string, task: () => Promise<T>): Promise<T> => {
      const prior = locks.get(key) ?? Promise.resolve();
      let done!: () => void;
      const mine = new Promise<void>((resolve) => {
        done = resolve;
      });
      const tail = prior.then(() => mine);
      locks.set(key, tail);
      await prior;
      try {
        return await task();
      } finally {
        done();
        if (locks.get(key) === tail) locks.delete(key);
      }
    },
    /**
     * Keeps `roots` and every pinned root, and drops the rest. Resolves to
     * the roots of the set this process had seen retained that are not
     * retained any more.
     */
    retain: async (roots: Iterable<string>): Promise<ReadonlySet<string>> => {
      const kept = new Set(roots);
      for (const root of pins.keys()) kept.add(root);
      // Forgotten now, so a root persisted or opened while this call runs,
      // which the store keeps, is seen again.
      for (const root of seen) if (!kept.has(root)) seen.delete(root);
      const running = storeCall("retain the ledger window", () =>
        store.retain(kept),
      );
      retaining.add(running);
      try {
        const { notRetained } = await running;
        return new Set([...notRetained].filter((root) => seen.has(root)));
      } finally {
        retaining.delete(running);
      }
    },
  };
};
