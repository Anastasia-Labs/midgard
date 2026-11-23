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
 * Up to three indexed queries, each only for the out-refs the ones before
 * it missed: the `confirmed_ledger` out-ref index, one GIN probe of
 * `node_landed_blocks.produced_outrefs` per out-ref, then the
 * `node_confirmed_ledger_spent` out-ref index.
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
    const missed = () => outRefs.filter((outRef) => !found.has(outRef));
    record(
      yield* Ledger.retrieveEntriesByOutRefs(
        ConfirmedLedger.tableName,
        outRefKeys(outRefs),
      ),
    );
    const notConfirmed = missed();
    if (notConfirmed.length > 0) record(yield* landedOutputs(notConfirmed));
    const notLanded = missed();
    if (notLanded.length > 0) record(yield* spentOutputs(notLanded));
    return outRefs.map((outRef) => found.get(outRef));
  });

const outRefKeys = (outRefs: readonly OutRefHex[]) =>
  outRefs.map((outRef) => Buffer.from(outRef, "hex"));

const landedOutputs = (outRefs: readonly OutRefHex[]) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    const pg = sql as PgClient;
    return yield* sql<OutputRow>`
      SELECT DISTINCT ON (k.outref)
        k.outref,
        b.produced_outputs[array_position(b.produced_outrefs, k.outref)] AS output
      FROM unnest(${pg.array(bytea(outRefKeys(outRefs)))}::bytea[]) AS k(outref)
      JOIN ${sql(tableName)} b ON b.produced_outrefs @> ARRAY[k.outref]
    `;
  }).pipe(
    sqlErrorToDatabaseError(
      tableName,
      "Failed to read the produced outputs of landed blocks by out-ref",
    ),
  );

const spentOutputs = (outRefs: readonly OutRefHex[]) =>
  Effect.gen(function* () {
    const sql = yield* SqlClient.SqlClient;
    return yield* sql<OutputRow>`
      SELECT DISTINCT ON (outref) outref, output
      FROM ${sql(spentTableName)}
      WHERE outref IN ${sql.in(outRefKeys(outRefs))}
    `;
  }).pipe(
    sqlErrorToDatabaseError(
      spentTableName,
      "Failed to read the confirmed outputs folds spent by out-ref",
    ),
  );
