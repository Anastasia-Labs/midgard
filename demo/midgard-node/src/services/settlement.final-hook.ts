/**
 * The settlement attempts' part in the follower's prune step (plan §8.2,
 * §11; §15 N6, D-N4): a pending attempt whose transaction landed valid at
 * or below the step's boundary (the block k below the cursor, so at depth
 * > k) is `final`. The intent journal's retention hook deletes that
 * attempt's journal entry in the same step, after which the journal derives
 * nothing for it; so this hook writes the level the deletion would lose, in
 * the step's transaction. It reads the step's facts (`l1_txs`), never the
 * journal, and the step deletes facts only after every hook ran, so its
 * place among the projections' hooks does not matter.
 *
 * It is a `record` hook: it deletes nothing, and it runs while a store reset
 * replays too. A replayed transaction at or below the boundary is a fact of
 * the canonical chain at depth > k under the replay's cursor (itself at or
 * below the node tip), so a replay never marks an attempt final early; it
 * marks nothing for a transaction it has not replayed yet.
 *
 * The node database holds the follower's tables, so the hook writes
 * `settlement_attempts` there. Postgres only: the node never runs SQLite.
 */
import type { FollowerProjection, PruneHook } from "@al-ft/midgard-l1-follower";

export const SETTLEMENT_ATTEMPTS_TABLE = "settlement_attempts";

export const settlementFinalHook: PruneHook = {
  kind: "record",
  table: SETTLEMENT_ATTEMPTS_TABLE,
  apply: async ({ tx, boundarySlot }) => {
    await tx.query(
      `UPDATE settlement_attempts a SET status = 'final'
        WHERE a.status = 'pending'
          AND EXISTS (SELECT 1 FROM l1_txs t
            WHERE t.tx_hash = decode(a.tx_hash, 'hex')
              AND t.is_valid AND t.block_slot <= ?)`,
      [boundarySlot],
    );
  },
};

/**
 * The node's settlement projection: no tracked set of its own (the
 * settlement wallet is a seeded node wallet, so its transactions are facts
 * already) and the hook above.
 */
export const settlementProjection: FollowerProjection = {
  name: "node_settlement",
  pruneHooks: [settlementFinalHook],
};
