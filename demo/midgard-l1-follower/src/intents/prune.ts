import type { Dialect, SqlTx } from "../sql/backend.js";
import type { PruneHook } from "../store/context.js";
import { deriveIntentStatusesIn } from "./status.js";

/**
 * Deletes every intent terminal for k blocks (§8.2 retention): its
 * `terminalSlot` is at or below the prune boundary (the block k below the
 * cursor). Runs in the follower's prune step before spent outputs are
 * deleted at the same boundary, so a retained intent never loses a fact its
 * status reads; a dependant whose only dead reason is a pruned dependency
 * shares that dependency's terminal slot and goes in the same step. Events
 * go with their intent (cascade). Returns the number deleted.
 */
export const pruneIntentsIn = async (
  tx: SqlTx,
  dialect: Dialect,
  boundarySlot: number,
): Promise<number> => {
  const { states } = await deriveIntentStatusesIn(tx, dialect);
  const terminal = states
    .filter(
      (state) =>
        state.terminalSlot !== null && state.terminalSlot <= boundarySlot,
    )
    .map((state) => state.intent.txHash);
  for (let start = 0; start < terminal.length; start += 500) {
    const chunk = terminal.slice(start, start + 500);
    await tx.query(
      `DELETE FROM l1_intents WHERE tx_hash IN (${chunk.map(() => "?").join(", ")})`,
      chunk,
    );
  }
  return terminal.length;
};

/** The journal's retention, as a follower prune hook. */
export const INTENT_PRUNE_HOOK: PruneHook = {
  kind: "retention",
  table: "l1_intents",
  apply: ({ tx, dialect, boundarySlot }) =>
    pruneIntentsIn(tx, dialect, boundarySlot),
};
