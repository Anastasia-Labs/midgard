import { SqlClient } from "@effect/sql";
import { Effect } from "effect";

import { withFollowerWrite } from "../services/follower-write-gate.js";
import { DatabaseError, sqlErrorToDatabaseError } from "./utils/common.js";
import { withdrawalLedgerOutRefColumns } from "./withdrawal-ledger-outref.decode.js";

export const WITHDRAWAL_OUTREF_BACKFILL_BATCH = 256;
export const WITHDRAWAL_OUTREF_MAX_BYTES = 65_536;
export const WITHDRAWAL_OUTREF_DECODE_BUDGET_MS = 100;
export const WITHDRAWAL_OUTREF_BACKFILL_MAX_MS = 2_000;

/** Active pending rows take precedence over historical rows. Status is durable progress. */
export const backfillWithdrawalLedgerOutRefs = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const [previous] = yield* sql<{
    statement_timeout: string;
    lock_timeout: string;
  }>`SELECT current_setting('statement_timeout') AS statement_timeout, current_setting('lock_timeout') AS lock_timeout`;
  yield* sql`SELECT set_config('statement_timeout', '500ms', true), set_config('lock_timeout', '100ms', true)`;
  const active = yield* sql<{
    event_id: Buffer;
    l2_outref: Buffer | null;
  }>`SELECT event_id, CASE WHEN octet_length(l2_outref) <= ${WITHDRAWAL_OUTREF_MAX_BYTES} THEN l2_outref END AS l2_outref FROM withdrawal_utxos
    WHERE l2_ledger_outref_state = 'unknown' AND status IN ('awaiting', 'projected')
      AND (validity IS NULL OR validity = 'WithdrawalIsValid') ORDER BY event_id ASC
    LIMIT ${WITHDRAWAL_OUTREF_BACKFILL_BATCH} FOR UPDATE`;
  const remaining = WITHDRAWAL_OUTREF_BACKFILL_BATCH - active.length;
  const archived =
    remaining === 0
      ? []
      : yield* sql<{
          event_id: Buffer;
          l2_outref: Buffer | null;
        }>`SELECT event_id, CASE WHEN octet_length(l2_outref) <= ${WITHDRAWAL_OUTREF_MAX_BYTES} THEN l2_outref END AS l2_outref FROM withdrawal_utxos
    WHERE l2_ledger_outref_state = 'unknown' AND (status IN ('awaiting', 'projected')
      AND (validity IS NULL OR validity = 'WithdrawalIsValid')) IS NOT TRUE ORDER BY event_id ASC
    LIMIT ${remaining} FOR UPDATE`;
  const started = performance.now();
  const normalized = [];
  for (const row of [...active, ...archived]) {
    if (row.l2_outref === null)
      return yield* Effect.fail(
        new DatabaseError({
          table: "withdrawal_utxos",
          message: "E_WITHDRAWAL_READ_BACKFILL_INPUT_LIMIT",
          cause:
            "oversized unknown stored outref requires operator inspection; original bytes are preserved",
        }),
      );
    normalized.push({
      ...row,
      ...withdrawalLedgerOutRefColumns(row.l2_outref),
    });
    // Complete at least one row; a busy CPU reduces batch size without resetting progress.
    if (performance.now() - started >= WITHDRAWAL_OUTREF_DECODE_BUDGET_MS)
      break;
  }
  if (normalized.length > 0) {
    const written =
      yield* sql`UPDATE withdrawal_utxos AS stored SET l2_ledger_outref = batch.ledger_key,
      l2_ledger_outref_state = batch.state FROM (VALUES ${sql.csv(normalized.map((row) => sql`(${row.event_id}::bytea, ${row.l2_outref}::bytea, ${row.l2_ledger_outref}::bytea, ${row.l2_ledger_outref_state}::text)`))})
      AS batch(event_id, raw_outref, ledger_key, state)
      WHERE stored.event_id = batch.event_id AND stored.l2_outref = batch.raw_outref AND stored.l2_ledger_outref_state = 'unknown'
      RETURNING stored.event_id`;
    if (written.length !== normalized.length)
      return yield* Effect.fail(
        new DatabaseError({
          table: "withdrawal_utxos",
          message: "E_WITHDRAWAL_READ_AUTHORITY_CONFLICT",
          cause: "derived outref row changed within its gated view",
        }),
      );
  }
  yield* sql`SELECT set_config('statement_timeout', ${previous!.statement_timeout}, true), set_config('lock_timeout', ${previous!.lock_timeout}, true)`;
  return normalized.length;
}).pipe(
  Effect.timeoutFail({
    duration: WITHDRAWAL_OUTREF_BACKFILL_MAX_MS,
    onTimeout: () =>
      new DatabaseError({
        table: "withdrawal_utxos",
        message: "E_WITHDRAWAL_READ_BACKFILL_BUDGET",
        cause: "bounded gated backfill timed out",
      }),
  }),
  sqlErrorToDatabaseError(
    "withdrawal_utxos",
    "Failed bounded withdrawal outref backfill",
  ),
  withFollowerWrite,
);

export const publicWithdrawalReadAuthorityReady = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const unknown = yield* sql`SELECT event_id FROM withdrawal_utxos
    WHERE l2_ledger_outref_state = 'unknown' AND status IN ('awaiting', 'projected')
      AND (validity IS NULL OR validity = 'WithdrawalIsValid') LIMIT 1`;
  return unknown.length === 0;
}).pipe(
  sqlErrorToDatabaseError(
    "withdrawal_utxos",
    "Failed withdrawal read authority check",
  ),
);
