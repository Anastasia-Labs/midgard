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
 * a timer. A refused transaction whose bytes no decoder reads names no
 * inputs (no ledger accepts it either), so only the next success clears it.
 */
import { decodeTransaction, encodeOutRef } from "@al-ft/midgard-l1-follower";
import type { SqlClient, SqlError } from "@effect/sql";
import { CML } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { byteaArrayLiteral } from "../database/follower-schema.js";
import type { DriverHold } from "../l1-events/driver.js";

/** The body's inputs by CML's ledger decoder, freeing every handle. */
const ledgerDecodedInputs = (bytes: Buffer): Buffer[] => {
  const tx = CML.Transaction.from_cbor_bytes(bytes);
  try {
    const body = tx.body();
    try {
      const inputs = body.inputs();
      try {
        const outRefs: Buffer[] = [];
        for (let index = 0; index < inputs.len(); index += 1) {
          const input = inputs.get(index);
          const id = input.transaction_id();
          try {
            outRefs.push(
              encodeOutRef({
                txHash: Buffer.from(id.to_hex(), "hex"),
                index: Number(input.index()),
              }),
            );
          } finally {
            id.free();
            input.free();
          }
        }
        return outRefs;
      } finally {
        inputs.free();
      }
    } finally {
      body.free();
    }
  } finally {
    tx.free();
  }
};

/**
 * The refused transaction's inputs as 34-byte outrefs. Bytes the follower's
 * decoder refuses (the `intent_undecodable` refusal) are read by the ledger
 * library's decoder instead, so a hold names the inputs of any transaction
 * a ledger could accept; only bytes neither decoder reads hold no inputs.
 */
export const refusedInputs = (signedTxCbor: string): Buffer[] => {
  const bytes = Buffer.from(signedTxCbor, "hex");
  try {
    return decodeTransaction(bytes).inputs.map(encodeOutRef);
  } catch {
    try {
      return ledgerDecodedInputs(bytes);
    } catch {
      return [];
    }
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
 * collateral: either way the follower marks the output spent), then reads
 * the holds that stand, one per family. Each held input is probed by the
 * outputs' primary key, so the work is bounded by the held inputs, not by
 * the tracked transactions.
 */
export const releaseAndReadRefusalHolds = (
  sql: SqlClient.SqlClient,
): Effect.Effect<ReadonlyMap<string, DriverHold>, SqlError.SqlError> =>
  Effect.gen(function* () {
    yield* sql`DELETE FROM intent_refusal_holds h
      WHERE EXISTS (
        SELECT 1 FROM unnest(h.inputs) AS held(outref)
        JOIN l1_outputs o
          ON o.tx_hash = substring(held.outref FROM 1 FOR 32)
         AND o.output_index = get_byte(held.outref, 32) * 256 + get_byte(held.outref, 33)
        WHERE o.spent_slot IS NOT NULL
          AND o.spent_tx IS DISTINCT FROM h.tx_hash)`;
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
