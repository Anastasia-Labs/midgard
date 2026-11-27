/**
 * The ledger store's output reader: the output bytes of a UTxO by its
 * out-ref, from `confirmed_ledger` first, then from the produced outputs of
 * any landed block still in `node_landed_blocks`, then from the
 * `confirmed_ledger` rows a fold spent (`node_confirmed_ledger_spent`).
 *
 * The first two hold every output of a root on the processed chain from
 * the confirmed frontier to its newest processed block: the frontier's
 * ledger is `confirmed_ledger`, and each block after it is a landed row
 * that is not yet folded. The third keeps a confirmed root older than the
 * frontier readable while the fold that left it can still be unfolded: a
 * peer may be fetching that root's ledger snapshot while the node folds.
 * An out-ref names its output's bytes (the transaction id hashes the body),
 * so a row that a later block spent, or one that is removed, still answers
 * truthfully; the trie, not this reader, decides whether a ledger holds the
 * out-ref.
 *
 * An out-ref `confirmed_ledger` does not have is looked up in all three
 * again in one statement, so on one snapshot: a fold moves a landed block's
 * outputs into `confirmed_ledger` and deletes its landed row in one
 * transaction, and a read that looked at the two tables on either side of
 * that commit would find the output in neither.
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

const spentTableName = "node_confirmed_ledger_spent";

type OutputRow = { outref: Uint8Array; output: Uint8Array };

/**
 * The outputs of `outRefs`, in order; `undefined` where no table has one.
 * One indexed query of `confirmed_ledger`; then, only for the out-refs it
 * missed, one statement over all three tables (`unconfirmedOutputs`).
 */
export const readLedgerOutputs = (
  outRefs: readonly OutRefHex[],
): Effect.Effect<(Buffer | undefined)[], DatabaseError, Database> =>
  Effect.gen(function* () {
    if (outRefs.length === 0) return [];
    const found = new Map<OutRefHex, Buffer>();
    const record = (rows: readonly OutputRow[]) => {
      for (const row of rows)
        found.set(
          Buffer.from(row.outref).toString("hex"),
          Buffer.from(row.output),
        );
    };
    record(
      yield* Ledger.retrieveEntriesByOutRefs(
        ConfirmedLedger.tableName,
        outRefKeys(outRefs),
      ),
    );
    const missed = outRefs.filter((outRef) => !found.has(outRef));
    if (missed.length > 0) record(yield* unconfirmedOutputs(missed));
    return outRefs.map((outRef) => found.get(outRef));
  });

const outRefKeys = (outRefs: readonly OutRefHex[]) =>
  outRefs.map((outRef) => Buffer.from(outRef, "hex"));

/**
 * The outputs of out-refs a read of `confirmed_ledger` missed, from one
 * snapshot of all three tables: the `confirmed_ledger` out-ref index again
 * (a fold may have moved the output there since), one GIN probe of
 * `node_landed_blocks.produced_outrefs` per out-ref, and the
 * `node_confirmed_ledger_spent` out-ref index. An out-ref two of them hold
 * has the same bytes in both.
 */
const unconfirmedOutputs = (outRefs: readonly OutRefHex[]) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const keys = () => (sql as PgClient).array(bytea(outRefKeys(outRefs)));
    return yield* sql<OutputRow>`
      SELECT outref, output
      FROM ${sql(ConfirmedLedger.tableName)}
      WHERE outref = ANY(${keys()}::bytea[])
      UNION ALL
      SELECT
        k.outref,
        b.produced_outputs[array_position(b.produced_outrefs, k.outref)] AS output
      FROM unnest(${keys()}::bytea[]) AS k(outref)
      JOIN ${sql(tableName)} b ON b.produced_outrefs @> ARRAY[k.outref]
      UNION ALL
      SELECT outref, output
      FROM ${sql(spentTableName)}
      WHERE outref = ANY(${keys()}::bytea[])`;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to read by out-ref the outputs confirmed_ledger does not hold",
    ),
  );
