/**
 * The keys a prune step deleted closed history rows of: followed units
 * (`WATCHER_UNIT_HISTORY_TABLE`) and state-queue headers
 * (`WATCHER_QUEUE_UNIT_HISTORY_TABLE`). A key's rows all close together,
 * and the same key can have rows again later (a unit minted again or back
 * at a followed address, a header committed again), so once its earlier
 * rows are gone the rows left cannot tell a whole history from a part of
 * one; this record can. A read or hold of a recorded key that no pin held
 * is not a whole history.
 */
import {
  type DialectName,
  type PruneHook,
  prunePredicate,
  type SqlTx,
  type TemporalTableSpec,
} from "@al-ft/midgard-l1-follower";

import {
  WATCHER_PRUNED_HEADERS_TABLE,
  WATCHER_PRUNED_UNITS_TABLE,
  WATCHER_QUEUE_UNIT_HISTORY_TABLE,
  WATCHER_UNIT_HISTORY_TABLE,
} from "./tables.js";

/** Each keyed history, and the record of its keys a prune step reached. */
const RECORDS = [
  {
    history: WATCHER_UNIT_HISTORY_TABLE,
    column: "unit",
    record: WATCHER_PRUNED_UNITS_TABLE,
  },
  {
    history: WATCHER_QUEUE_UNIT_HISTORY_TABLE,
    column: "header_hash",
    record: WATCHER_PRUNED_HEADERS_TABLE,
  },
] as const;

/**
 * The prune hooks that record, in the follower's prune step, every key with
 * a history row the step's boundary makes prunable: the rows the follower
 * deletes, by the same predicate (`prunePredicate`), in this step or, when
 * the budget cuts it, in a later step at the same boundary (no hold can be
 * written for the key in between: `partlyPrunedIn` refuses it). They run
 * before the deletes, in the same transaction, and delete nothing.
 */
export const prunedKeyHooks = (
  specs: readonly TemporalTableSpec[],
): readonly PruneHook[] =>
  RECORDS.map(({ history, column, record }) => {
    const spec = specs.find(({ name }) => name === history);
    const prunable = spec === undefined ? null : prunePredicate(spec);
    if (prunable === null)
      throw new Error(`${history} has no pruning predicate`);
    return {
      table: record,
      apply: async ({ tx, boundarySlot }) => {
        await tx.query(
          `INSERT INTO ${record} (${column}) SELECT DISTINCT t.${column} FROM ${history} t WHERE ${prunable} AND NOT EXISTS (SELECT 1 FROM ${record} x WHERE x.${column} = t.${column})`,
          [boundarySlot],
        );
        return 0;
      },
    };
  });

const recordedIn =
  (record: string, column: string) =>
  async (tx: SqlTx, key: Buffer): Promise<boolean> =>
    (
      await tx.query(
        `SELECT 1 AS one FROM ${record} WHERE ${column} = ? LIMIT 1`,
        [key],
      )
    ).length > 0;

/** Whether a prune step deleted closed history rows of a followed unit. */
export const unitPrunedIn = recordedIn(WATCHER_PRUNED_UNITS_TABLE, "unit");
/** Whether a prune step deleted closed queue history rows of a header. */
export const headerPrunedIn = recordedIn(
  WATCHER_PRUNED_HEADERS_TABLE,
  "header_hash",
);

/**
 * The two records, one table per keyed history (each key keeps its own
 * column, as the histories do).
 *
 * Class B, not D-t: the prune step writes them, at or below the k-deep
 * boundary no rewind reaches, so a rollback has nothing to undo; and they
 * record what this store deleted, which a fresh replay of the facts (the
 * D-t contract) cannot reproduce. A reset keeps them, as it keeps the pins:
 * a key the replay prunes again is recorded again. Never deleted: a key can
 * have rows again at any later slot.
 */
export const prunedKeysMigrationSql = (dialect: DialectName): string => {
  const bytes = dialect === "postgres" ? "bytea" : "BLOB";
  return `
-- class: B; retention: forever (one row per followed unit a prune step deleted closed history rows of)
CREATE TABLE ${WATCHER_PRUNED_UNITS_TABLE} (
  unit ${bytes} NOT NULL PRIMARY KEY
);

-- class: B; retention: forever (one row per header a prune step deleted closed queue history rows of)
CREATE TABLE ${WATCHER_PRUNED_HEADERS_TABLE} (
  header_hash ${bytes} NOT NULL PRIMARY KEY
);
`;
};
