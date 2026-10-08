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
import { Effect, Schedule } from "effect";

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

/** Attempts at one hold write before it is left to the next record or refresh. */
const HOLD_WRITE_RETRIES = Schedule.exponential("100 millis").pipe(
  Schedule.intersect(Schedule.recurs(3)),
);

/** A refusal hold whose write to `intent_refusal_holds` has not landed. */
export type UnwrittenHold = Readonly<{
  family: string;
  hold: DriverHold;
  txHash: string;
  signedTxCbor: string;
}>;

/**
 * One journal's view of the holds: the last read of the table, its own
 * writes, and refusals whose hold write has not landed yet. An unwritten
 * hold is never left only in a worker's memory (I1-H1): it is written again
 * on every record (`flush`) and every refresh until it lands, and until then
 * it is named in `holds()`. A worker thread, whose journal ends with it,
 * hands its unwritten holds to the main process (`handOff`), whose journal
 * takes them over (`adopt`): `/readyz` names them from then on, and the
 * main process's refresh at every tip writes them until they land.
 */
export const refusalHoldsOver = (sql: SqlClient.SqlClient) => {
  let persisted: ReadonlyMap<string, DriverHold> = new Map();
  const unpersisted = new Map<string, Omit<UnwrittenHold, "family">>();
  const write = (
    family: string,
    hold: DriverHold,
    txHash: string,
    signedTxCbor: string,
    retry: boolean,
  ): Effect.Effect<void> =>
    persistRefusalHold(sql, family, hold, txHash, signedTxCbor).pipe(
      retry ? Effect.retry(HOLD_WRITE_RETRIES) : (effect) => effect,
      Effect.matchEffect({
        onSuccess: () =>
          Effect.sync(() => {
            // A newer refusal of the family may have replaced this one.
            if (unpersisted.get(family)?.txHash === txHash)
              unpersisted.delete(family);
            persisted = new Map([...persisted, [family, hold]]);
          }),
        onFailure: (cause) =>
          Effect.sync(() =>
            unpersisted.set(family, { hold, txHash, signedTxCbor }),
          ).pipe(
            Effect.zipRight(
              Effect.logWarning(
                `Intent journal: the ${family} refusal hold is not written yet (named here until it lands; retried on the next record or refresh): ${messageOf(cause)}`,
              ),
            ),
          ),
      }),
    );
  /** Writes every unwritten hold again. Never fails. */
  const flush: Effect.Effect<void> = Effect.suspend(() =>
    Effect.forEach(
      [...unpersisted],
      ([family, { hold, txHash, signedTxCbor }]) =>
        write(family, hold, txHash, signedTxCbor, false),
      { discard: true },
    ),
  );
  return {
    /** Holds `family`'s refusal of the transaction. Never fails. */
    raise: (
      family: string,
      hold: DriverHold,
      txHash: string,
      signedTxCbor: string,
    ): Effect.Effect<void> => write(family, hold, txHash, signedTxCbor, true),
    /** Writes the holds whose write has not landed yet. Never fails. */
    flush,
    /** The delete, for the successful record's own transaction. */
    clearIn: (family: string) => clearRefusalHold(sql, family),
    /** `family`'s record succeeded and its transaction committed. */
    cleared: (family: string): Effect.Effect<void> =>
      Effect.sync(() => {
        unpersisted.delete(family);
        persisted = new Map([...persisted].filter(([at]) => at !== family));
      }),
    holds: (): readonly DriverHold[] => [
      ...new Map([
        ...persisted,
        ...[...unpersisted].map(
          ([family, { hold }]) => [family, hold] as const,
        ),
      ]).values(),
    ],
    /** Returns the unwritten holds and forgets them: the caller owns them now. */
    handOff: (): readonly UnwrittenHold[] => {
      const handed = [...unpersisted].map(([family, rest]) => ({
        family,
        ...rest,
      }));
      unpersisted.clear();
      return handed;
    },
    /**
     * Takes over another journal's unwritten holds: named in `holds()` at
     * once, and written on the next record or refresh until they land.
     */
    adopt: (holds: readonly UnwrittenHold[]): void => {
      for (const { family, ...rest } of holds) unpersisted.set(family, rest);
    },
    refresh: (): Effect.Effect<void> =>
      flush.pipe(
        Effect.zipRight(releaseAndReadRefusalHolds(sql)),
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
