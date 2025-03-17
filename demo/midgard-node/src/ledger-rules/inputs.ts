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
  inputs: OutRef[],
): Effect.Effect<boolean, Error> =>
  Effect.gen(function* () {
    for (let i = 0; i < inputs.length; i++) {
      const outRef = inputs[i];

      const [entryLedger, entryMempool] = yield* Effect.all([
        Effect.tryPromise({
          try: () => LatestLedgerDB.retrieveByOutRef(pool, outRef),
          catch: (e) =>
            new Error(`Failed to retrieve UTxO from latest ledger: ${e}`),
        }),
        Effect.tryPromise({
          try: () => MempoolLedgerDB.retrieveByOutRef(pool, outRef),
          catch: (e) => new Error(`Failed to retrieve UTxO from mempool: ${e}`),
        }),
      ]);

      const valid =
        (entryMempool.length === 1 && entryLedger.length === 0) ||
        (entryMempool.length === 0 && entryLedger.length === 1);

      if (!valid) {
        throw new Error(
          `Could not spend UTxO: ${JSON.stringify({
            txHash: outRef.txHash,
            outputIndex: outRef.outputIndex,
          })}\nIt does not exist or was already spent.`,
        );
      }
    }
    return true;
  });
