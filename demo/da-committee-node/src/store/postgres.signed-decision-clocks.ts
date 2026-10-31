import type { Pool } from "pg";

import type { StateQueueHeaderRecord } from "../domain.js";
import type { SignedHeader } from "../l1/follower/obligations.js";
import { decodeRecord } from "./postgres.assert-postgres-decision-retry.js";
import { storedHeaderBlockTimeMs } from "./retention.block-time.js";

/** Unknown legacy commit clocks remain held until the exact authenticated retained node supplies them. */
export const readSignedDecisionClocks = async (
  pool: Pick<Pool, "query">,
): Promise<readonly SignedHeader[]> => {
  const result = await pool.query<{
    readonly header_hash: string;
    readonly block_time_ms: string | null;
    readonly latest_block_time_ms: string | null;
    readonly header_record: unknown | null;
  }>(
    `SELECT s.header_hash, MIN(s.block_time_ms)::text AS block_time_ms,
              MAX(s.block_time_ms)::text AS latest_block_time_ms,
              h.record AS header_record
       FROM committee_da_signatures s
       LEFT JOIN committee_state_queue_headers h ON h.header_hash = s.header_hash
       WHERE s.end_time_ms IS NOT NULL
       GROUP BY s.header_hash, h.record ORDER BY s.header_hash`,
  );
  const signed: SignedHeader[] = [];
  for (const row of result.rows) {
    if (row.block_time_ms !== row.latest_block_time_ms)
      throw new Error(
        "Signed header has conflicting authenticated commit clocks",
      );
    let blockTimeMs =
      row.block_time_ms === null ? null : BigInt(row.block_time_ms);
    if (row.header_record !== null) {
      const header = decodeRecord<StateQueueHeaderRecord>(row.header_record);
      if (header.headerHash !== row.header_hash)
        throw new Error("Signed decision clock names another stored header");
      const retainedClock = BigInt(storedHeaderBlockTimeMs(header));
      if (blockTimeMs !== null && blockTimeMs !== retainedClock)
        throw new Error(
          "Signed clock disagrees with its authenticated retained node",
        );
      blockTimeMs = retainedClock;
      await pool.query(
        "UPDATE committee_da_signatures SET block_time_ms = $2 WHERE header_hash = $1 AND end_time_ms IS NOT NULL AND block_time_ms IS NULL",
        [row.header_hash, blockTimeMs.toString()],
      );
    }
    signed.push({ headerHash: row.header_hash, blockTimeMs });
  }
  return signed;
};
