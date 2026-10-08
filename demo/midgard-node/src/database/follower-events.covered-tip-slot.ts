import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";

/**
 * The follower's covered tip slot (its store cursor), the node's L1 tip for
 * `l1SlotNow` (see `l1-heads.ts`). Fails while the follower has no cursor.
 */
export const followerCoveredTipSlot = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    slot: string;
  }>`SELECT slot::text AS slot FROM l1_follower_cursor`;
  const slot = rows[0]?.slot;
  if (slot === undefined)
    return yield* Effect.fail(
      new DatabaseError({
        table: "l1_follower_cursor",
        message: "The L1 follower has no covered tip yet",
        cause: undefined,
      }),
    );
  return Number(slot);
}).pipe(
  sqlErrorToDatabaseError(
    "l1_follower_cursor",
    "Failed to read the L1 follower's covered tip",
  ),
);
