import { outRefKey } from "../codec.js";
import {
  asBuffer,
  asNumber,
  type Dialect,
  type SqlTx,
} from "../sql/backend.js";
import type {
  Cursor,
  OutRef,
  Point,
  StoreLocked,
  TrackedSet,
} from "../types.js";
import { readCursor } from "./rows.js";
import {
  insertSeedOutputsIn,
  type SeedCursorMoved,
  type SeedOutput,
  type SeedResult,
} from "./seed.js";

// The own wallets' seed points (`l1_wallet_seeds`, plan §5.3 step 4): a
// wallet with one has been read from the ledger at it, and tracked by every
// block applied since, so it is not read again. A rewind below the point
// deletes it with the wallet's seed rows (`rewindIn`); a start that does not
// track the wallet deletes it (`deleteUntrackedWalletSeedsIn`).

/**
 * Reads the UTxOs at `wallets` from the node's ledger state at `at` (the
 * store's cursor, held there while it runs). It rejects on a failed read.
 */
export type WalletSeedRead = (
  at: Point,
  wallets: readonly Buffer[],
) => Promise<readonly SeedOutput[]>;

export type WalletSeedWrite = Readonly<{
  kind: "seeded";
  /** The cursor the unseeded wallets were read and recorded at. */
  cursor: Cursor;
  /** Every requested wallet's seed point, by lowercase hex address. */
  seeds: ReadonlyMap<string, Point>;
  /** The wallets read from the ledger now; empty when all were seeded before. */
  read: readonly Buffer[];
  inserted: readonly OutRef[];
  /** Already stored as a row (created or seeded). */
  skipped: readonly OutRef[];
}>;

/** The ledger read rejected; nothing was written. */
export type WalletSeedReadFailed = Readonly<{
  kind: "read_failed";
  error: unknown;
}>;

/** The stored seed point of each of `addresses` that has one, by lowercase hex. */
export const readWalletSeedsIn = async (
  tx: SqlTx,
  addresses: readonly Buffer[],
): Promise<Map<string, Point>> => {
  const wanted = new Set(addresses.map((address) => address.toString("hex")));
  const seeds = new Map<string, Point>();
  // One row per seeded own wallet: the table stays small.
  for (const row of await tx.query(
    "SELECT address, seed_slot, seed_hash FROM l1_wallet_seeds",
  )) {
    const hex = asBuffer(row.address).toString("hex");
    if (wanted.has(hex))
      seeds.set(hex, {
        slot: asNumber(row.seed_slot),
        hash: asBuffer(row.seed_hash),
      });
  }
  return seeds;
};

/**
 * Deletes the seed point of every wallet whose address `tracked` lacks: the
 * blocks this store applies from now on hold none of its outputs, so its
 * seed stops covering it, and it is owed again once it is tracked again.
 */
export const deleteUntrackedWalletSeedsIn = async (
  tx: SqlTx,
  tracked: TrackedSet,
): Promise<void> => {
  for (const row of await tx.query("SELECT address FROM l1_wallet_seeds")) {
    const address = asBuffer(row.address);
    if (!tracked.addresses.has(address.toString("hex")))
      await tx.query("DELETE FROM l1_wallet_seeds WHERE address = ?", [
        address,
      ]);
  }
};

type StoreError = Readonly<{ kind: "error"; error: Error }>;

/** What the seed writes need from the store. */
export type SeedWriteContext = Readonly<{
  dialect: Dialect;
  /** Runs a task in the store's writer lane. */
  lane: <T>(task: () => Promise<T>) => Promise<T>;
  /** Why a write may not run now, or null when it may. */
  refusal: () => StoreError | StoreLocked | null;
  read: <T>(run: (tx: SqlTx) => Promise<T>) => Promise<T>;
  /** A write transaction under the writer lease's fence. */
  write: <T>(run: (tx: SqlTx) => Promise<T>) => Promise<T | StoreLocked>;
  /** The tracked-outref set, by `outRefKey`. */
  live: Set<string>;
  /** Refuses writes until a start reloads the tracked-outref set. */
  markLiveStale: () => void;
}>;

const unique = (addresses: readonly Buffer[]): Buffer[] => [
  ...new Map(
    addresses.map((address) => [address.toString("hex"), address]),
  ).values(),
];

const asError = (error: unknown): Error =>
  error instanceof Error ? error : new Error(String(error));

/** `FactStore.insertSeedOutputs` and `FactStore.seedWallets`. */
export const createSeedWrites = (context: SeedWriteContext) => {
  const { dialect, lane, read, write, live } = context;
  /**
   * After a seed write failed: a failure during COMMIT leaves its outcome
   * unknown, so the rows may be in and a retry would skip them. Take the
   * live ones from the store; if it cannot answer, refuse writes until a
   * start reloads.
   */
  const recover = async (outputs: readonly SeedOutput[]): Promise<void> => {
    try {
      const found = await read(async (tx) => {
        const rows: OutRef[] = [];
        for (const { outRef } of outputs)
          if (
            (
              await tx.query(
                "SELECT 1 AS one FROM l1_outputs WHERE tx_hash = ? AND output_index = ? AND spent_slot IS NULL",
                [outRef.txHash, outRef.index],
              )
            ).length > 0
          )
            rows.push(outRef);
        return rows;
      });
      for (const outRef of found) live.add(outRefKey(outRef));
    } catch {
      context.markLiveStale();
    }
  };
  /** Seed rows for `outputs`, read at `at`, which must still be the cursor. */
  const insertSeedOutputs = (
    at: Point,
    outputs: readonly SeedOutput[],
  ): Promise<SeedResult | SeedCursorMoved | StoreError | StoreLocked | null> =>
    lane(async () => {
      const refusal = context.refusal();
      if (refusal !== null) return refusal;
      try {
        const result = await write((tx) =>
          insertSeedOutputsIn(tx, dialect, at, outputs),
        );
        if (result?.kind === "store_locked") return result;
        if (result?.kind === "seeded")
          for (const outRef of result.inserted) live.add(outRefKey(outRef));
        return result;
      } catch (error) {
        await recover(outputs);
        return { kind: "error", error: asError(error) } as const;
      }
    });
  /**
   * The wallets among `addresses` with no seed point are read and written in
   * one lane task: the lane holds every other writer off while the ledger is
   * read, so the cursor the read ran at is still the cursor when their rows
   * and seed points land. Blocks wait meanwhile.
   */
  const seedWallets = (
    addresses: readonly Buffer[],
    readWallets: WalletSeedRead,
  ): Promise<
    WalletSeedWrite | WalletSeedReadFailed | StoreError | StoreLocked | null
  > =>
    lane(async () => {
      const refusal = context.refusal();
      if (refusal !== null) return refusal;
      let state: { cursor: Cursor; seeds: Map<string, Point> } | null;
      try {
        state = await read(async (tx) => {
          const cursor = await readCursor(tx, dialect);
          return cursor === null
            ? null
            : { cursor, seeds: await readWalletSeedsIn(tx, addresses) };
        });
      } catch (error) {
        return { kind: "error", error: asError(error) } as const;
      }
      if (state === null) return null;
      const { cursor, seeds } = state;
      const owed = unique(addresses).filter(
        (address) => !seeds.has(address.toString("hex")),
      );
      if (owed.length === 0)
        return {
          kind: "seeded",
          cursor,
          seeds,
          read: [],
          inserted: [],
          skipped: [],
        } as const;
      let outputs: readonly SeedOutput[];
      try {
        outputs = await readWallets(cursor.point, owed);
      } catch (error) {
        return { kind: "read_failed", error } as const;
      }
      try {
        const result = await write(async (tx) => {
          const written = await insertSeedOutputsIn(
            tx,
            dialect,
            cursor.point,
            outputs,
          );
          if (written?.kind !== "seeded")
            throw new Error("the cursor moved while the writer lane was held");
          for (const address of owed)
            await tx.query(
              "INSERT INTO l1_wallet_seeds (address, seed_slot, seed_hash) VALUES (?, ?, ?)",
              [address, cursor.point.slot, cursor.point.hash],
            );
          return written;
        });
        if (result.kind === "store_locked") return result;
        for (const outRef of result.inserted) live.add(outRefKey(outRef));
        for (const address of owed)
          seeds.set(address.toString("hex"), cursor.point);
        const { inserted, skipped } = result;
        return {
          kind: "seeded",
          cursor,
          seeds,
          read: owed,
          inserted,
          skipped,
        } as const;
      } catch (error) {
        await recover(outputs);
        return { kind: "error", error: asError(error) } as const;
      }
    });
  return { insertSeedOutputs, seedWallets };
};
