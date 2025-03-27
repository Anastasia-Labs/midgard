import { LatestLedgerDB, MempoolLedgerDB } from "@/database/index.js";
import { CML, OutRef } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { Pool } from "pg";

/**
 * Validates a list of transaction inputs against the latest ledger and mempool.
 *
 * The function checks for the following conditions:
 * - NO-INPUT: The input does not exist in either the latest ledger or the mempool.
 * - INPUT-NO-IDX: The input exists but has no index.
 * - WITHDRAWN-INPUT: The input has already been withdrawn. // TODO
 * - DOUBLE-SPEND: The input has already been spent. // TODO
 * - DOUBLE-WITHDRAW: The input has already been withdrawn twice. // TODO
 *
 * @param pool - The database connection pool.
 * @param inputs - The list of transaction inputs to validate.
 * @returns A boolean indicating whether the inputs are valid.
 * @throws An error if any of the inputs are invalid.
 */
export const validateInputs = (
  pool: Pool,
  inputs: Uint8Array[],
): Effect.Effect<boolean, Error> =>
  Effect.gen(function* () {
    for (let i = 0; i < inputs.length; i++) {
      const txIn = inputs[i];
      const entryMempool = yield* Effect.tryPromise({
        try: () => MempoolLedgerDB.retrieveByTxIns(pool, [txIn]),
        catch: (e) => new Error(`Failed to retrieve UTxO from mempool: ${e}`),
      });
      if (entryMempool.length === 1) {
        // Found in mempool, continue
        continue;
      }
      const entryLedger = yield* Effect.tryPromise({
        try: () => LatestLedgerDB.retrieveByTxIns(pool, [txIn]),
        catch: (e) => new Error(`Failed to retrieve UTxO from ledger: ${e}`),
      });
      if (entryLedger.length === 1) {
        // Found in ledger, continue
        continue;
      }
      throw new Error(
        `Could not spend UTxO: ${txIn.toString()}\nIt does not exist or was already spent.`,
      );
    }
    return true;
  });
