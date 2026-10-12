import type { Dialect, SqlTx } from "../sql/backend.js";
import type { Cursor } from "../types.js";
import { readCursor } from "./rows.js";

/**
 * A retention pin a role writes against pruning: a row in one of its
 * `retentionPins` or registered `pinnedBy` tables.
 */
export type RetainedPin = Readonly<{
  /**
   * Whether every row the pin would hold is still stored, read under the
   * cursor lock: no prune step runs between this read and `insert`. The
   * cursor is null before the store is initialized.
   */
  retained(tx: SqlTx, cursor: Cursor | null): Promise<boolean>;
  /** Writes the pin; runs only when `retained` returned true. */
  insert(tx: SqlTx): Promise<void>;
}>;

/**
 * `pinned`: the pin is written and holds its rows from now on.
 * `already_pruned`: pruning removed (some of) the rows first; nothing was
 * written, and the history the pin would hold is gone.
 */
export type PinResult =
  | Readonly<{ kind: "pinned" }>
  | Readonly<{ kind: "already_pruned" }>;

/**
 * Pins under the cursor lock (§11), in the caller's write transaction. It
 * takes the cursor row lock every prune step takes first, so a prune step
 * either committed before the `retained` check (which then sees its
 * deletions) or starts after this transaction commits (and then sees the
 * pin). A pin written any other way can land after a prune step chose its
 * rows and hold nothing. It needs no writer lease and stays outside the
 * writer lane by design: a pin row is a role row, and the cursor lock is
 * the only ordering it needs.
 */
export const pinRetainedIn = async (
  tx: SqlTx,
  dialect: Dialect,
  pin: RetainedPin,
): Promise<PinResult> => {
  const cursor = await readCursor(tx, dialect, "update");
  if (!(await pin.retained(tx, cursor))) return { kind: "already_pruned" };
  await pin.insert(tx);
  return { kind: "pinned" };
};
