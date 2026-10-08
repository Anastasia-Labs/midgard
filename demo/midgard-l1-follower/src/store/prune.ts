import { prunePredicate } from "../registry.js";
import { asNumber, type SqlTx } from "../sql/backend.js";
import type { RetentionPin, StoreContext } from "./context.js";
import { readCursor } from "./rows.js";

/** Every 1,000th height is kept as an intersection checkpoint (§11). */
export const CHECKPOINT_INTERVAL = 1000;
/** `l1_rollbacks` keeps its last 1,000 rows (§11). */
export const ROLLBACK_LOG_ROWS = 1000;

export type PruneResult = Readonly<{
  /** Rows deleted per table in this step. */
  deleted: Readonly<Record<string, number>>;
  /** True when no table hit the budget: nothing more is prunable now. */
  done: boolean;
  /** The slot facts are complete from after this step. */
  prunedThroughSlot: number;
  /**
   * How many slots a role's floor holds the boundary below the block k
   * below the cursor; null when no floor holds it.
   */
  floorLagSlots: number | null;
}>;

const pinClauses = (
  pins: readonly RetentionPin[] | undefined,
  value: string,
): string =>
  (pins ?? [])
    .map(
      (pin) =>
        ` AND NOT EXISTS (SELECT 1 FROM ${pin.table} p WHERE p.${pin.column} = ${value})`,
    )
    .join("");

/**
 * One budgeted retention step (§11), in the caller's write transaction. The
 * boundary is the block k below the cursor: a rewind of depth k lands on it,
 * so nothing at or above it is touched. Order: spent outputs whose spend is
 * at or below the boundary, closed D-t rows (children first), txs nothing
 * retained refers to, unreferenced blocks (keeping checkpoints and the
 * origin), unreferenced scripts, and the rollback log beyond its last 1,000
 * rows. Each table deletes at most `budget` rows. The role's prune hooks run
 * first, except while a store reset replays.
 */
export const pruneIn = async (
  tx: SqlTx,
  context: StoreContext,
  budget: number,
): Promise<PruneResult> => {
  const { dialect, registry, k, pins } = context;
  const cursor = await readCursor(tx, dialect, "update");
  if (cursor === null)
    return {
      deleted: {},
      done: true,
      prunedThroughSlot: 0,
      floorLagSlots: null,
    };
  const rowId = dialect.rowId;
  const deleted: Record<string, number> = {};
  let done = true;
  const step = async (
    table: string,
    where: string,
    params: readonly (number | null)[],
  ): Promise<void> => {
    const rows = await tx.query(
      `DELETE FROM ${table} WHERE ${rowId} IN (SELECT ${rowId} FROM ${table} t WHERE ${where} LIMIT ?) RETURNING 1 AS one`,
      [...params, budget],
    );
    deleted[table] = (deleted[table] ?? 0) + rows.length;
    if (rows.length >= budget) done = false;
  };
  // The rollback log is bounded by count, not by depth: pruned even while
  // the chain above the origin is shorter than k.
  const pruneRollbackLog = (): Promise<void> =>
    step(
      "l1_rollbacks",
      "t.generation <= (SELECT max(generation) FROM l1_rollbacks) - ?",
      [ROLLBACK_LOG_ROWS],
    );
  const boundaryRows = await tx.query(
    "SELECT slot, height FROM l1_blocks WHERE height = ?",
    [cursor.height - k],
  );
  const boundary = boundaryRows[0];
  if (boundary === undefined) {
    await pruneRollbackLog();
    return {
      deleted,
      done,
      prunedThroughSlot: cursor.prunedThroughSlot,
      floorLagSlots: null,
    };
  }
  // A role's floor holds the boundary at or below its slot: the boundary
  // block becomes the last one at or below it.
  const kSlot = asNumber(boundary.slot);
  let floor: number | null = null;
  for (const role of context.pruneFloors) {
    const slot = await role.floor({ tx, dialect });
    if (slot !== null && (floor === null || slot < floor)) floor = slot;
  }
  const held = floor !== null && floor < kSlot;
  let boundaryHeight = asNumber(boundary.height);
  if (held) {
    const below = await tx.query(
      "SELECT height FROM l1_blocks WHERE slot <= ? ORDER BY slot DESC LIMIT 1",
      [floor],
    );
    boundaryHeight = below[0] === undefined ? 0 : asNumber(below[0].height);
  }
  const boundarySlot = Math.max(
    held ? floor! : kSlot,
    cursor.prunedThroughSlot,
  );
  const floorLagSlots = boundarySlot < kSlot ? kSlot - boundarySlot : null;
  if (boundarySlot > cursor.prunedThroughSlot)
    await tx.query("UPDATE l1_follower_cursor SET pruned_through_slot = ?", [
      boundarySlot,
    ]);
  // While a store reset replays from the origin (the tracked-set record's
  // `replaying` mark), the facts are incomplete until the replay reaches the
  // height the cursor held before the reset, so no projection hook decides
  // retention from them; this step's own retention below goes on, and a
  // skipped hook is no failure.
  const replaying =
    context.pruneHooks.length > 0 &&
    (
      await tx.query(
        "SELECT 1 AS one FROM l1_follower_tracked_set WHERE id = 1 AND replaying <> 0",
      )
    ).length > 0;
  if (!replaying)
    for (const hook of context.pruneHooks)
      deleted[hook.table] =
        (deleted[hook.table] ?? 0) +
        (await hook.apply({ tx, dialect, boundarySlot }));
  await step("l1_outputs", "t.spent_slot IS NOT NULL AND t.spent_slot <= ?", [
    boundarySlot,
  ]);
  // Children first; a parent waits for a later step while any child still
  // has prunable rows, so a budget cut never orphans a child row.
  const unfinished = new Set<string>();
  for (const table of [...registry.tables].reverse()) {
    const children = registry.tables.filter(
      (other) => other.parents?.includes(table.name) === true,
    );
    if (children.some((child) => unfinished.has(child.name))) {
      unfinished.add(table.name);
      done = false;
      continue;
    }
    const predicate = prunePredicate(table);
    if (predicate === null) continue;
    const before = deleted[table.name] ?? 0;
    await step(table.name, predicate, [boundarySlot]);
    if ((deleted[table.name] ?? 0) - before >= budget)
      unfinished.add(table.name);
  }
  await step(
    "l1_txs",
    `t.block_slot <= ?
      AND NOT EXISTS (SELECT 1 FROM l1_outputs o WHERE o.tx_hash = t.tx_hash)
      AND NOT EXISTS (SELECT 1 FROM l1_outputs o WHERE o.spent_tx = t.tx_hash)${pinClauses(pins.txs, "t.tx_hash")}`,
    [boundarySlot],
  );
  await step(
    "l1_blocks",
    `t.height < ? AND t.height % ${CHECKPOINT_INTERVAL} <> 0
      AND t.slot <> (SELECT origin_slot FROM l1_follower_cursor)
      AND NOT EXISTS (SELECT 1 FROM l1_txs x WHERE x.block_slot = t.slot)
      AND NOT EXISTS (SELECT 1 FROM l1_outputs o WHERE o.created_slot = t.slot)
      AND NOT EXISTS (SELECT 1 FROM l1_outputs o WHERE o.spent_slot = t.slot)${pinClauses(pins.blocks, "t.slot")}`,
    [boundaryHeight],
  );
  await step(
    "l1_scripts",
    "NOT EXISTS (SELECT 1 FROM l1_outputs o WHERE o.script_ref_hash = t.script_hash)",
    [],
  );
  await pruneRollbackLog();
  return { deleted, done, prunedThroughSlot: boundarySlot, floorLagSlots };
};
