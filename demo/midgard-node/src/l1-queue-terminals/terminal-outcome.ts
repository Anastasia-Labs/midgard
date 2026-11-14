/**
 * A header's newest queue terminal (N4), as a SQL fragment with no service
 * behind it, so a module the S3 determinism lint reaches (the own-block
 * status, `pendingBlockFinalizations.status-sql.ts`) can read it.
 */
import type { SqlClient } from "@effect/sql";

import { QUEUE_TERMINALS_TABLE } from "./schema.js";

/** The newest terminal outcome of `headerColumn`'s header, or NULL. */
export const newestTerminalOutcome = (
  sql: SqlClient.SqlClient,
  headerColumn: string,
) => sql`(
  SELECT terminal.terminal_outcome FROM ${sql(QUEUE_TERMINALS_TABLE)} AS terminal
  WHERE terminal.header_hash = ${sql(headerColumn)}
  ORDER BY terminal.height DESC, terminal.tx_index DESC
  LIMIT 1)`;
