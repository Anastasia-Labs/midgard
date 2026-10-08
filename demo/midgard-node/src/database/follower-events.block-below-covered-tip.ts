import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { type CoveredTipHeads, l1BlockBelowCoveredTip } from "../l1-heads.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

/**
 * The follower block d below its covered tip (`l1BlockBelowCoveredTip`, the
 * block at heads depth d + 1), from the cursor and block rows read in one
 * statement, so both come from one snapshot. Reads no L1 and takes no lock.
 */
export const followerBlockBelowCoveredTip = (lagBlocks: number) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const rows = yield* sql<{
      slot: string;
      hash: Buffer;
      height: string;
      block_slot: string | null;
      block_hash: Buffer | null;
      block_height: string | null;
    }>`SELECT c.slot::text AS slot, c.hash, c.height::text AS height,
        b.slot::text AS block_slot, b.hash AS block_hash,
        b.height::text AS block_height
      FROM l1_follower_cursor c
      LEFT JOIN l1_blocks b ON b.height = c.height - ${lagBlocks}`;
    const row = rows[0];
    const heads: CoveredTipHeads = {
      cursor: () =>
        Promise.resolve(
          row === undefined
            ? null
            : {
                point: { slot: Number(row.slot), hash: row.hash },
                height: Number(row.height),
              },
        ),
      blockAtHeight: (height) =>
        Promise.resolve(
          row?.block_slot == null ||
            row.block_hash === null ||
            Number(row.block_height) !== height
            ? null
            : { slot: Number(row.block_slot), hash: row.block_hash, height },
        ),
    };
    return yield* Effect.tryPromise({
      try: () => l1BlockBelowCoveredTip(heads, lagBlocks),
      catch: (cause) =>
        new DatabaseError({
          table: "l1_blocks",
          message: "Invalid horizon lag for the follower's covered tip",
          cause,
        }),
    });
  }).pipe(
    sqlErrorToDatabaseError(
      "l1_blocks",
      "Failed to read the follower block below its covered tip",
    ),
  );
