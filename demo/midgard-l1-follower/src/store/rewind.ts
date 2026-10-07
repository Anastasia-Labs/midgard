import {
  asBuffer,
  asNumber,
  RollbackWith,
  type SqlTx,
} from "../sql/backend.js";
import type { Cursor, Intervention, OutRef, Point } from "../types.js";
import type { StoreContext } from "./context.js";
import { describeViolations, runInvariantChecks } from "./invariants.js";
import { readCursor } from "./rows.js";

export type Rewound = Readonly<{
  kind: "rewound";
  generation: number;
  from: Point;
  to: Point;
  depth: number;
  cursor: Cursor;
  /** Outrefs the rewind made live again (cache patch, §6 item 5). */
  unspent: readonly OutRef[];
  /**
   * Outrefs whose rows the rewind deleted: created above the target, or
   * seed rows whose seed point lies above it (§5.3 step 4).
   */
  deleted: readonly OutRef[];
}>;

export type RewindNoop = Readonly<{ kind: "noop"; cursor: Cursor }>;

/** Test-only fault injection: proves the property test catches a broken rewind. */
export type RewindFault =
  /** Leaves every D-t row above the target (the post-rewind check sees it). */
  | "skip_temporal_truncation"
  /** Cuts D-t tables one slot too low, deleting the target's own rows (only replay sees it). */
  | "temporal_cut_below_target"
  /** Un-spends in the store but not in the tracked-outref cache. */
  | "skip_unspend_cache_patch"
  /**
   * Keeps seed rows whose seed point lies above the target, re-stamped at
   * the target so the post-rewind INV6 probe cannot see them: the rule that
   * a rewind never touches a seed row. A fork that orphans a post-origin
   * output leaves it live (a phantom).
   */
  | "keep_seed_rows_above_target"
  /** Also deletes seed rows seeded at the target itself. */
  | "delete_seed_rows_at_target";

const intervention = (
  reason: Intervention["reason"],
  detail: string,
): Intervention => ({ kind: "intervention", reason, detail });

const outRefsFrom = (rows: readonly Record<string, unknown>[]): OutRef[] =>
  rows.map((row) => ({
    txHash: asBuffer(row.tx_hash),
    index: asNumber(row.output_index),
  }));

/**
 * `rewind(target)` per §7.1, in the caller's write transaction:
 * cursor row lock, target resolution (R1/R2), registry-driven D-t truncation
 * in reverse dependency order, fact un-spend and delete, cursor and
 * generation, the `l1_rollbacks` row, the scoped invariant check (R5), and
 * `pg_notify` (delivered on commit). Interventions roll the transaction back.
 */
export const rewindIn = async (
  tx: SqlTx,
  context: StoreContext,
  target: Point,
  fault: RewindFault | null,
): Promise<Rewound | RewindNoop | Intervention> => {
  const { dialect, registry, k } = context;
  const cursor = await readCursor(tx, dialect, "update");
  if (cursor === null)
    return intervention(
      "intersection_outside_history",
      "the store has no cursor",
    );
  if (
    target.slot === cursor.point.slot &&
    target.hash.equals(cursor.point.hash)
  )
    return { kind: "noop", cursor };
  const rows = await tx.query(
    "SELECT slot, height FROM l1_blocks WHERE hash = ?",
    [target.hash],
  );
  const found = rows[0];
  if (found === undefined) {
    if (
      target.slot < cursor.prunedThroughSlot ||
      target.slot < cursor.origin.slot
    )
      return intervention(
        "rollback_beyond_k",
        `target slot ${target.slot} is below the oldest retained block (slot ${cursor.prunedThroughSlot})`,
      );
    return intervention(
      "intersection_outside_history",
      `target ${target.hash.toString("hex")} at slot ${target.slot} is not on the stored chain`,
    );
  }
  const targetSlot = asNumber(found.slot);
  if (targetSlot !== target.slot)
    return intervention(
      "intersection_outside_history",
      `target hash is stored at slot ${targetSlot}, not ${target.slot}`,
    );
  const depth = cursor.height - asNumber(found.height);
  if (depth > k || targetSlot < cursor.prunedThroughSlot)
    return intervention(
      "rollback_beyond_k",
      `rollback of ${depth} blocks to slot ${targetSlot} exceeds k = ${k} or the retained window`,
    );
  if (fault !== "skip_temporal_truncation")
    for (const statement of registry.rewindStatements(
      fault === "temporal_cut_below_target" ? targetSlot - 1 : targetSlot,
    ))
      await tx.query(statement.sql, statement.params);
  const unspentRows = await tx.query(
    "UPDATE l1_outputs SET spent_slot = NULL, spent_tx = NULL WHERE spent_slot > ? RETURNING tx_hash, output_index",
    [targetSlot],
  );
  const deleted = outRefsFrom(
    await tx.query(
      "DELETE FROM l1_outputs WHERE created_slot > ? RETURNING tx_hash, output_index",
      [targetSlot],
    ),
  );
  // A seed row is a fact observed at its seed point: a rewind below that
  // point rewinds it, and the wallet seeder reads the wallet again at a
  // later cursor (§5.3 step 4). Seed rows at or below the target observed a
  // state that is still canonical and stay.
  if (fault === "keep_seed_rows_above_target")
    await tx.query("UPDATE l1_outputs SET seed_slot = ? WHERE seed_slot > ?", [
      targetSlot,
      targetSlot,
    ]);
  else
    deleted.push(
      ...outRefsFrom(
        await tx.query(
          fault === "delete_seed_rows_at_target"
            ? "DELETE FROM l1_outputs WHERE seed_slot >= ? RETURNING tx_hash, output_index"
            : "DELETE FROM l1_outputs WHERE seed_slot > ? RETURNING tx_hash, output_index",
          [targetSlot],
        ),
      ),
    );
  const gone = new Set(
    deleted.map((outRef) => `${outRef.txHash.toString("hex")}#${outRef.index}`),
  );
  await tx.query("DELETE FROM l1_event_keys WHERE first_canonical_slot > ?", [
    targetSlot,
  ]);
  await tx.query("DELETE FROM l1_txs WHERE block_slot > ?", [targetSlot]);
  await tx.query("DELETE FROM l1_blocks WHERE slot > ?", [targetSlot]);
  const generation = cursor.generation + 1;
  await tx.query(
    "UPDATE l1_follower_cursor SET slot = ?, hash = ?, height = ?, generation = ?",
    [targetSlot, target.hash, cursor.height - depth, generation],
  );
  await tx.query(
    "INSERT INTO l1_rollbacks (generation, from_slot, from_hash, to_slot, to_hash, depth_blocks) VALUES (?, ?, ?, ?, ?, ?)",
    [
      generation,
      cursor.point.slot,
      cursor.point.hash,
      targetSlot,
      target.hash,
      depth,
    ],
  );
  const report = await runInvariantChecks(tx, dialect, registry, "post_rewind");
  if (!report.ok)
    throw new RollbackWith(
      intervention(
        "store_integrity",
        `after rewind: ${describeViolations(report)}`,
      ),
    );
  if (dialect.name === "postgres")
    await tx.query("SELECT pg_notify('l1_generation', ?)", [
      String(generation),
    ]);
  const unspent =
    fault === "skip_unspend_cache_patch"
      ? []
      : outRefsFrom(
          unspentRows.filter(
            (row) =>
              !gone.has(
                `${asBuffer(row.tx_hash).toString("hex")}#${asNumber(row.output_index)}`,
              ),
          ),
        );
  return {
    kind: "rewound",
    generation,
    from: cursor.point,
    to: { slot: targetSlot, hash: target.hash },
    depth,
    cursor: {
      ...cursor,
      point: { slot: targetSlot, hash: target.hash },
      height: cursor.height - depth,
      generation,
    },
    unspent,
    deleted,
  };
};
