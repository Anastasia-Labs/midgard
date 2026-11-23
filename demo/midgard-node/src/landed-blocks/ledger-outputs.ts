/**
 * The ledger store's output reader: the output bytes of a UTxO by its
 * out-ref, from `confirmed_ledger` first, then from the produced outputs of
 * any landed block still in `node_landed_blocks`.
 *
 * Together these hold every output of a root on the processed chain from
 * the confirmed frontier to its newest processed block: the frontier's
 * ledger is `confirmed_ledger`, and each block after it is a landed row
 * that is not yet folded. An out-ref names its output's bytes (the
 * transaction id hashes the body), so a row that a later block spent, or
 * one that is removed, still answers truthfully; the trie, not this reader,
 * decides whether a ledger holds the out-ref.
 */
import type { OutRefHex } from "@al-ft/midgard-fault-proofs/ledger-store";
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { Effect } from "effect";

import * as ConfirmedLedger from "../database/confirmedLedger.js";
import {
  type DatabaseError,
  sqlErrorToDatabaseError,
} from "../database/utils/common.js";
import * as Ledger from "../database/utils/ledger.js";
import type { Database } from "../services/database.js";
import { bytea, tableName } from "./store.js";

/**
 * The outputs of `outRefs`, in order; `undefined` where neither table has
 * one. Two indexed queries: the `confirmed_ledger` out-ref index, then one
 * GIN probe of `node_landed_blocks.produced_outrefs` per out-ref it missed.
 */
export const readLedgerOutputs = (
  outRefs: readonly OutRefHex[],
): Effect.Effect<(Buffer | undefined)[], DatabaseError, Database> =>
  Effect.gen(function* () {
    if (outRefs.length === 0) return [];
    const found = new Map<OutRefHex, Buffer>();
    const confirmed = yield* Ledger.retrieveEntriesByOutRefs(
      ConfirmedLedger.tableName,
      outRefs.map((outRef) => Buffer.from(outRef, "hex")),
    );
    for (const entry of confirmed)
      found.set(Buffer.from(entry.outref).toString("hex"), entry.output);
    const missed = outRefs.filter((outRef) => !found.has(outRef));
    if (missed.length > 0)
      for (const row of yield* landedOutputs(missed))
        found.set(Buffer.from(row.outref).toString("hex"), row.output);
    return outRefs.map((outRef) => found.get(outRef));
  });

const landedOutputs = (outRefs: readonly OutRefHex[]) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    const keys = outRefs.map((outRef) => Buffer.from(outRef, "hex"));
    return yield* sql<{ outref: Uint8Array; output: Uint8Array }>`
      SELECT DISTINCT ON (k.outref)
        k.outref,
        b.produced_outputs[array_position(b.produced_outrefs, k.outref)] AS output
      FROM unnest(${pg.array(bytea(keys))}::bytea[]) AS k(outref)
      JOIN ${sql(tableName)} b ON b.produced_outrefs @> ARRAY[k.outref]
    `.pipe(
      Effect.map((rows) =>
        rows.map((row) => ({
          outref: Buffer.from(row.outref),
          output: Buffer.from(row.output),
        })),
      ),
    );
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to read the produced outputs of landed blocks by out-ref",
    ),
  );
