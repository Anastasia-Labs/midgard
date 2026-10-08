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

/** What a prune hook sees: the step's write transaction and its boundary. */
export type PruneHookContext = Readonly<{
  tx: SqlTx;
  dialect: Dialect;
  boundarySlot: number;
}>;

/**
 * A role's hook in the prune step (§11). It runs after the boundary is
 * fixed and before any row at or below it is deleted, in the step's
 * transaction, so it reads every fact and every D-t row the step is about
 * to delete. Each hook declares its kind:
 *
 * - `retention`: decides, from the facts, which rows of its role's class B
 *   table to delete (the intent journal, §8.2), deletes them and returns how
 *   many; it is not budgeted. It is skipped while a store reset replays
 *   (the tracked-set record's `replaying` mark): the facts below the cursor
 *   are incomplete until the replay passes them.
 * - `record`: deletes nothing; it writes, in the same transaction, what the
 *   step's own retention deletes (keys whose closed D-t rows the step
 *   prunes). It runs whenever the step runs, replaying or not, since the
 *   deletes it records run then too.
 *
 * A hook that deletes any row is a `retention` hook.
 */
export type PruneHook =
  | Readonly<{
      kind: "retention";
      /** The table it prunes (the key of its count in `PruneResult.deleted`). */
      table: string;
      apply(context: PruneHookContext): Promise<number>;
    }>
  | Readonly<{
      kind: "record";
      /** The table it writes the record to. */
      table: string;
      apply(context: PruneHookContext): Promise<void>;
    }>;

/**
 * A role's floor on the prune boundary (§11): the boundary never passes the
 * slot it returns, so every fact spent or closed after that slot is kept
 * while the role still needs it; null sets no floor. It runs at the start of
 * each prune step, in its transaction. With several floors the lowest holds.
 */
export type PruneFloor = Readonly<{
  /** Who holds the floor (named in logs and in `FollowStatus.prune.floorLags`). */
  name: string;
  floor(
    context: Readonly<{ tx: SqlTx; dialect: Dialect }>,
  ): Promise<number | null>;
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
  pruneFloors: readonly PruneFloor[];
}>;
