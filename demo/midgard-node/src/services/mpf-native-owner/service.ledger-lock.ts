/**
 * The native owner's ledger lock. A promotion, a `retain` and a canonical
 * restore each move or read the store's retained set, and they run one at a
 * time under it: a `retain` never drops the root a promotion or a restore is
 * moving the durable root to, because it reads the durable root only after
 * that move has ended. The lock covers a `retain` only until the store has
 * queued its replacement of the retained set; the deletion that follows
 * runs after the lock is released, in batches that later writes land
 * between, so a promotion waits at most for the batch in progress.
 */
import type { LedgerStore } from "@al-ft/midgard-fault-proofs/ledger-store";

/** What `LedgerStore.retain` resolves to. */
export type LedgerRetention = Awaited<ReturnType<LedgerStore["retain"]>>;

export const createOwnerLedgerLock = (store: LedgerStore) => {
  let tail: Promise<unknown> = Promise.resolve();
  let deletion: Promise<unknown> = Promise.resolve();
  const run = <T>(task: () => Promise<T>): Promise<T> => {
    const result = tail.then(task, task);
    tail = result.catch(() => undefined);
    return result;
  };
  return {
    run,
    /**
     * Under the lock, calls `store.retain` with the set `roots` answers and
     * resolves to what it resolves to. The lock is released once the store
     * has queued the replacement, before the deletion.
     */
    retain: (
      roots: () => Promise<ReadonlySet<string>>,
    ): Promise<LedgerRetention> =>
      run(async () => {
        const retaining = store.retain(await roots());
        deletion = retaining.catch(() => undefined);
        // Wrapped, so the lock does not wait for the deletion.
        return { retaining };
      }).then(({ retaining }) => retaining),
    /** Settles once the last `retain` called has replaced the retained set
     * and ended its deletion, or failed. */
    deleted: (): Promise<unknown> => deletion,
  };
};
