import type { TemporalRegistry } from "../registry.js";
import type { Dialect, SqlBackend, SqlTx } from "../sql/backend.js";
import type { BlockSummary, Cursor } from "../types.js";
import type { QualifiedTx } from "./qualify.js";

/** What an S3 derivation sees for one applied block. */
export type DerivationContext = Readonly<{
  /** The writer's open transaction: derive in it, never open another. */
  tx: SqlTx;
  dialect: Dialect;
  block: BlockSummary;
  /** The block's qualifying txs, in block order, with the rows they touch. */
  qualified: readonly QualifiedTx[];
  /** The cursor before this block. */
  previous: Cursor;
}>;

/**
 * An S3 derivation (§4.1, §7.2): a pure function of facts, class B/C content
 * and the manifest, run in the writer's transaction after the block's facts
 * are inserted. It writes only the D-t tables it lists, all registered, so
 * the generated rewind covers every row it writes.
 */
export type DerivationHook = Readonly<{
  name: string;
  writes: readonly string[];
  apply(context: DerivationContext): Promise<void>;
}>;

/** A table column whose values pin a tx (by hash) or a block (by slot) against pruning. */
export type RetentionPin = Readonly<{ table: string; column: string }>;

export type RetentionPins = Readonly<{
  txs?: readonly RetentionPin[];
  blocks?: readonly RetentionPin[];
}>;

/**
 * Retention for a role's class B table whose rows are decided by the facts
 * (the intent journal, §8.2). It runs in the prune step, after the boundary
 * is fixed and before spent outputs at or below it are deleted, so it reads
 * every fact its rows depend on. It deletes the rows terminal at or below
 * the boundary and returns how many; it is not budgeted.
 */
export type PruneHook = Readonly<{
  /** The table it prunes (the key of its count in `PruneResult.deleted`). */
  table: string;
  apply(
    context: Readonly<{ tx: SqlTx; dialect: Dialect; boundarySlot: number }>,
  ): Promise<number>;
}>;

export type StoreContext = Readonly<{
  backend: SqlBackend;
  dialect: Dialect;
  /** The security parameter k, in blocks. */
  k: number;
  registry: TemporalRegistry;
  derivations: readonly DerivationHook[];
  pins: RetentionPins;
  pruneHooks: readonly PruneHook[];
}>;
