/**
 * Intent-journal refusal holds in the node database (`intent_refusal_holds`,
 * migration 0010). A refusal is raised by whichever process refused the
 * record: the main process, or the commit and settlement worker threads,
 * which each run their own journal. The row is what crosses that boundary:
 * the main process reads the table into its journal's holds, so `/readyz`
 * names a worker's refusal as it names its own.
 *
 * A family's hold clears on that family's next successful record (in the
 * record's own SQL transaction), or once a landed transaction other than the
 * refused one spends one of the refused transaction's inputs: the work it
 * would have done is gone, so nothing is left to unblock. It never clears on
 * a timer.
 */
import { decodeTransaction, encodeOutRef } from "@al-ft/midgard-l1-follower";
import type { SqlClient, SqlError } from "@effect/sql";
import { Effect } from "effect";

import { byteaArrayLiteral } from "../database/follower-schema.js";
import type { DriverHold } from "../l1-events/driver.js";

/** The refused transaction's inputs as 34-byte outrefs; none when its bytes do not decode. */
const refusedInputs = (signedTxCbor: string): Buffer[] => {
  try {
    return decodeTransaction(Buffer.from(signedTxCbor, "hex")).inputs.map(
      encodeOutRef,
    );
  } catch {
    return [];
  }
};

/** Writes (or replaces) `family`'s hold for the refused transaction. */
export const persistRefusalHold = (
  sql: SqlClient.SqlClient,
  family: string,
  hold: DriverHold,
  txHash: string,
  signedTxCbor: string,
): Effect.Effect<void, SqlError.SqlError> =>
  sql`INSERT INTO intent_refusal_holds (family, reason, detail, tx_hash, inputs)
      VALUES (${family}, ${hold.reason}, ${hold.detail},
        ${Buffer.from(txHash, "hex")},
        ${byteaArrayLiteral(refusedInputs(signedTxCbor))}::bytea[])
      ON CONFLICT (family) DO UPDATE SET
        reason = EXCLUDED.reason, detail = EXCLUDED.detail,
        tx_hash = EXCLUDED.tx_hash, inputs = EXCLUDED.inputs,
        raised_at = NOW()`.pipe(Effect.asVoid);

/** Clears `family`'s hold: its record succeeded. */
export const clearRefusalHold = (
  sql: SqlClient.SqlClient,
  family: string,
): Effect.Effect<void, SqlError.SqlError> =>
  sql`DELETE FROM intent_refusal_holds WHERE family = ${family}`.pipe(
    Effect.asVoid,
  );

/**
 * Clears every hold whose refused transaction lost an input to another
 * landed transaction (a valid one's inputs, or a phase-2-failed one's
 * collateral), then reads the holds that stand, one per family.
 */
export const releaseAndReadRefusalHolds = (
  sql: SqlClient.SqlClient,
): Effect.Effect<ReadonlyMap<string, DriverHold>, SqlError.SqlError> =>
  Effect.gen(function* () {
    yield* sql`DELETE FROM intent_refusal_holds h
      WHERE EXISTS (
        SELECT 1 FROM l1_txs t
        WHERE t.tx_hash <> h.tx_hash
          AND CASE WHEN t.is_valid THEN t.inputs && h.inputs
                   ELSE t.collaterals && h.inputs END)`;
    const rows = yield* sql<{
      readonly family: string;
      readonly reason: string;
      readonly detail: string;
    }>`SELECT family, reason, detail FROM intent_refusal_holds ORDER BY family`;
    return new Map(
      rows.map(({ family, reason, detail }) => [family, { reason, detail }]),
    );
  });

const messageOf = (cause: unknown): string =>
  cause instanceof Error
    ? `${cause.message}${cause.cause instanceof Error ? `: ${cause.cause.message}` : ""}`
    : String(cause);

/**
 * One journal's view of the holds: the last read of the table, its own
 * writes, and refusals whose hold write failed (kept in this process until
 * the family's next success).
 */
export const refusalHoldsOver = (sql: SqlClient.SqlClient) => {
  let persisted: ReadonlyMap<string, DriverHold> = new Map();
  const unpersisted = new Map<string, DriverHold>();
  return {
    /** Holds `family`'s refusal of the transaction. Never fails. */
    raise: (
      family: string,
      hold: DriverHold,
      txHash: string,
      signedTxCbor: string,
    ): Effect.Effect<void> =>
      persistRefusalHold(sql, family, hold, txHash, signedTxCbor).pipe(
        Effect.matchEffect({
          onSuccess: () =>
            Effect.sync(() => {
              unpersisted.delete(family);
              persisted = new Map([...persisted, [family, hold]]);
            }),
          onFailure: (cause) =>
            Effect.sync(() => unpersisted.set(family, hold)).pipe(
              Effect.zipRight(
                Effect.logWarning(
                  `Intent journal: the ${family} refusal hold was not written (held in this process only): ${messageOf(cause)}`,
                ),
              ),
            ),
        }),
      ),
    /** The delete, for the successful record's own transaction. */
    clearIn: (family: string) => clearRefusalHold(sql, family),
    /** `family`'s record succeeded and its transaction committed. */
    cleared: (family: string): Effect.Effect<void> =>
      Effect.sync(() => {
        unpersisted.delete(family);
        persisted = new Map([...persisted].filter(([at]) => at !== family));
      }),
    holds: (): readonly DriverHold[] => [
      ...new Map([...persisted, ...unpersisted]).values(),
    ],
    refresh: (): Effect.Effect<void> =>
      releaseAndReadRefusalHolds(sql).pipe(
        Effect.matchEffect({
          onSuccess: (read) =>
            Effect.sync(() => {
              persisted = read;
            }),
          onFailure: (cause) =>
            Effect.logWarning(
              `Intent journal: the refusal holds were not re-read (the last read stands): ${messageOf(cause)}`,
            ),
        }),
      ),
  };
};
