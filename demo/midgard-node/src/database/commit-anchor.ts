/**
 * The commit anchor (plan §8.1): an event is committed only below the commit
 * anchor.
 *
 * A commit is planned at a follower view P (its write permit's view, the
 * view the follower-change driver ingested events through). Its anchor A is
 * the follower block d below P, at height h(P) - d, where d is the
 * deployment profile's commit-event depth (`l1_finality.commit_event_depth`,
 * `NodeConfig.COMMIT_EVENT_DEPTH`). The header end E is at most
 * time(A) + event_wait - 1. A user event is due at its inclusion time
 * I = valid_to + event_wait (inclusive valid_to), and a block includes the
 * events due at or before E, so every included event has
 * valid_to <= time(A) - 1: its L1 block is strictly below A on A's chain.
 * While A is canonical, so is every included event's block.
 *
 * The journal stores A (hash, height, slot) with the commit, rechecks E
 * against it inside the gated journal transaction, and keeps the commit only
 * while A is canonical: at signing (`recordSignedIntent`), at the
 * commit-stability predicate (which also waits until the follower tip is d
 * blocks above A) and in the own-journal disposition. At d = 0 A is P itself
 * and the cap is the ingestion horizon time(P) + event_wait - 1.
 */
import type { View } from "@al-ft/midgard-l1-follower";
import { heightAtDepth } from "@al-ft/midgard-l1-follower/heads";
import { EVENT_WAIT_DURATION_MS } from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { followerViewValid } from "./follower-schema.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

/** A commit's anchor block: the follower block d below its planning view. */
export type CommitAnchor = Readonly<{
  hash: Buffer;
  height: number;
  slot: number;
}>;

/**
 * The anchor block of a view, or why it has none. `view_gone`: a rewind
 * removed the view. `outside_history`: no follower block at the anchor
 * height (the chain above the origin is not d blocks long yet, or the block
 * was pruned). Both are transient: the caller plans nothing and retries.
 */
export type CommitAnchorBlock =
  | Readonly<{ kind: "anchored"; anchor: CommitAnchor }>
  | Readonly<{
      kind: "unavailable";
      reason: "view_gone" | "outside_history";
      detail: string;
    }>;

/** `CommitAnchorBlock` with the header-end cap of an available anchor. */
export type CommitAnchorRead =
  | Readonly<{ kind: "anchored"; anchor: CommitAnchor; capMs: number }>
  | Extract<CommitAnchorBlock, { kind: "unavailable" }>;

const table = "l1_blocks";

/** The anchor height under a view at `viewHeight`: the block at heads depth d + 1. */
export const commitAnchorHeight = (viewHeight: number, depth: number): number =>
  heightAtDepth(viewHeight, depth + 1);

/** The header-end cap an anchor dated `anchorTimeMs` sets. */
export const commitAnchorCapMs = (anchorTimeMs: number): number =>
  anchorTimeMs + EVENT_WAIT_DURATION_MS - 1;

const requireDepth = (depth: number) =>
  Number.isSafeInteger(depth) && depth >= 0
    ? Effect.void
    : Effect.fail(
        new DatabaseError({
          table,
          message: "The commit-event depth must be a non-negative integer",
          cause: String(depth),
        }),
      );

/**
 * The anchor block of `view`, read in one transaction (a savepoint inside
 * the caller's): the view check takes the follower cursor `FOR SHARE`, so
 * the block read at the anchor height is the view's own ancestor.
 */
export const readCommitAnchorBlock = (input: {
  readonly view: View;
  readonly depth: number;
}): Effect.Effect<CommitAnchorBlock, DatabaseError, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    yield* requireDepth(input.depth);
    const height = commitAnchorHeight(input.view.height, input.depth);
    const sql = yield* SqlClient.SqlClient;
    const read = yield* sql.withTransaction(
      Effect.gen(function* () {
        if (!(yield* followerViewValid(input.view))) return "view_gone";
        if (height < 0) return null;
        const rows = yield* sql<{ slot: string; hash: Buffer }>`
          SELECT slot::text AS slot, hash FROM l1_blocks
          WHERE height = ${height}`;
        return rows[0] ?? null;
      }),
    );
    if (read === "view_gone")
      return {
        kind: "unavailable",
        reason: "view_gone",
        detail: `the planning view at height ${input.view.height.toString()} left the follower's chain`,
      } satisfies CommitAnchorBlock;
    if (read === null)
      return {
        kind: "unavailable",
        reason: "outside_history",
        detail: `no follower block at anchor height ${height.toString()} (view height ${input.view.height.toString()}, d=${input.depth.toString()}): below the origin or pruned`,
      } satisfies CommitAnchorBlock;
    return {
      kind: "anchored",
      anchor: { hash: Buffer.from(read.hash), height, slot: Number(read.slot) },
    } satisfies CommitAnchorBlock;
  }).pipe(sqlErrorToDatabaseError(table, "Failed to read the commit anchor"));

/** `readCommitAnchorBlock` with the anchor's header-end cap, dated by `slotToUnixTime`. */
export const readCommitAnchor = (input: {
  readonly view: View;
  readonly depth: number;
  readonly slotToUnixTime: (slot: number) => number;
}): Effect.Effect<CommitAnchorRead, DatabaseError, SqlClient.SqlClient> =>
  Effect.gen(function* () {
    const block = yield* readCommitAnchorBlock(input);
    if (block.kind === "unavailable") return block;
    const time = input.slotToUnixTime(block.anchor.slot);
    const capMs = commitAnchorCapMs(time);
    if (!Number.isSafeInteger(time) || !Number.isSafeInteger(capMs))
      return yield* Effect.fail(
        new DatabaseError({
          table,
          message: "The commit anchor's time cannot form a safe cap",
          cause: `slot=${block.anchor.slot.toString()},time=${String(time)}`,
        }),
      );
    return { kind: "anchored", anchor: block.anchor, capMs };
  });

const IDENTIFIER = /^[a-z_][a-z0-9_]*$/;

/**
 * SQL text (no parameters) of the condition "the anchor of the journal row
 * aliased `alias` is canonical": the follower stores its block (by hash and
 * slot), or it lies at or below the pruned boundary with no block stored at
 * its height. A pruned anchor lies more than k blocks below the follower
 * tip, where the block at its height is final, and the follower keeps no
 * older block to compare it with; the own-journal disposition, which runs at
 * every rewind, disposes of an unfinished journal whose anchor a rewind
 * removed while the block replacing it is still stored. A row without an
 * anchor is never canonical. For both the node's SQL client (`sql.literal`)
 * and the follower's store transactions.
 */
export const commitAnchorCanonicalText = (alias: string): string => {
  if (!IDENTIFIER.test(alias))
    throw new Error(`Invalid SQL alias for the commit anchor: ${alias}`);
  return `(${alias}.commit_anchor_hash IS NOT NULL AND (
      EXISTS (SELECT 1 FROM l1_blocks anchor_block
        WHERE anchor_block.hash = ${alias}.commit_anchor_hash
          AND anchor_block.slot = ${alias}.commit_anchor_slot)
      OR (${alias}.commit_anchor_slot <= (SELECT pruned_through_slot FROM l1_follower_cursor)
        AND NOT EXISTS (SELECT 1 FROM l1_blocks anchor_height
          WHERE anchor_height.height = ${alias}.commit_anchor_height))))`;
};

/** `commitAnchorCanonicalText` as a fragment of the node's SQL client. */
export const commitAnchorCanonical = (
  sql: SqlClient.SqlClient,
  alias: string,
) => sql.literal(commitAnchorCanonicalText(alias));

/** The anchor columns of a journal row, all three or none. */
export const commitAnchorOf = (row: {
  readonly commit_anchor_hash?: Buffer | null;
  readonly commit_anchor_height?: bigint | number | string | null;
  readonly commit_anchor_slot?: bigint | number | string | null;
}): CommitAnchor | null =>
  row.commit_anchor_hash == null ||
  row.commit_anchor_height == null ||
  row.commit_anchor_slot == null
    ? null
    : {
        hash: Buffer.from(row.commit_anchor_hash),
        height: Number(row.commit_anchor_height),
        slot: Number(row.commit_anchor_slot),
      };
