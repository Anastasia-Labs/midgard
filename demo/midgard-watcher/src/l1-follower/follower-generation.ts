import type { DialectName, FactStore, SqlTx } from "@al-ft/midgard-l1-follower";

/**
 * The follower generation the watcher's decision driver last handled
 * (class B, one row). A rewind or a store reset raises the follower's
 * generation and leaves an `l1_rollbacks` row; the driver hears it pushed
 * (`onGeneration`) when it is subscribed, and pulls it by generation
 * (`FactStore.rewindsSince`) from this row otherwise: a reset at a start
 * the driver was not yet subscribed to, or a rewind its process stopped
 * before handling. The row moves only after the user-event history went
 * back to the target, so every rewind is handled at least once. A reset
 * keeps it (class B), which is what lets the driver see the reset.
 *
 * The migration seeds the row from the cursor of a store that already has
 * one (a store from before the row existed: what that process handled is
 * not known, so it is taken as handled); a store with no cursor gets no
 * row, and the driver's first pull then reads every logged rewind.
 */
export const WATCHER_FOLLOWER_GENERATION_TABLE = "watcher_follower_generation";

export const followerGenerationMigrationSql = (dialect: DialectName): string =>
  `
-- class: B; retention: one row forever; reset keeps it
CREATE TABLE ${WATCHER_FOLLOWER_GENERATION_TABLE} (
  id integer PRIMARY KEY CHECK (id = 1),
  generation ${dialect === "postgres" ? "bigint" : "INTEGER"} NOT NULL
);

INSERT INTO ${WATCHER_FOLLOWER_GENERATION_TABLE} (id, generation)
  SELECT 1, generation FROM l1_follower_cursor;
`;

const readIn = async (tx: SqlTx): Promise<number | null> => {
  const row = (
    await tx.query(
      `SELECT generation FROM ${WATCHER_FOLLOWER_GENERATION_TABLE} WHERE id = 1`,
    )
  )[0];
  return row === undefined ? null : Number(row.generation);
};

/** The generation the driver last handled; null before it handled one. */
export const readHandledFollowerGeneration = (
  store: Pick<FactStore, "transaction">,
): Promise<number | null> => store.transaction("read", readIn);

/** Records that every rewind up to `generation` was handled. */
export const writeHandledFollowerGeneration = (
  store: Pick<FactStore, "transaction">,
  generation: number,
): Promise<void> =>
  store.transaction("write", async (tx) => {
    await tx.query(
      `INSERT INTO ${WATCHER_FOLLOWER_GENERATION_TABLE} (id, generation) VALUES (1, ?)
       ON CONFLICT (id) DO UPDATE SET generation = excluded.generation`,
      [generation],
    );
  });
