import "./utils.js";

import { SqlClient } from "@effect/sql";
import { Effect, Either } from "effect";
import { expect, it } from "vitest";

import { DepositsDB, MempoolDB, TxUtils } from "../src/database/index.js";
import { restoreSpeculativeLedgerAfterCorrection } from "../src/services/state-queue-correction-ledger-restore.js";
import {
  deterministicFixtureBytes,
  deterministicFixtureOutputReferenceId,
  deterministicFixtureTxHash,
  provideDatabaseLayers,
  resetApplicationTables,
} from "./utils.js";

const snapshotRestoreRows = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  return {
    mempool: yield* sql`SELECT tx_id, tx FROM mempool ORDER BY tx_id`,
    processed:
      yield* sql`SELECT tx_id, tx FROM processed_mempool ORDER BY tx_id`,
    deltas: yield* sql`SELECT * FROM mempool_tx_deltas ORDER BY tx_id`,
    ledger: yield* sql`SELECT * FROM mempool_ledger ORDER BY outref`,
    deposits:
      yield* sql`SELECT event_id, status FROM deposits_utxos ORDER BY event_id`,
    rejections: yield* sql`SELECT * FROM tx_rejections ORDER BY tx_id`,
  };
});

// A pending transaction whose spends are unknown may spend the reopened
// deposit's output, so the restore can neither keep it nor reverse it.
it("refuses a correction's ledger restore when a pending transaction has no delta row and does not decode, and changes nothing", async () => {
  await Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* resetApplicationTables;
        const depositId = deterministicFixtureOutputReferenceId(
          "ledger-restore.reopened-deposit",
        );
        yield* DepositsDB.insertEntries([
          {
            [DepositsDB.Columns.ID]: depositId,
            [DepositsDB.Columns.INFO]: deterministicFixtureBytes(
              "ledger-restore.deposit-info",
              48,
            ),
            [DepositsDB.Columns.INCLUSION_TIME]: new Date(
              Date.parse("2026-09-26T00:00:00.000Z"),
            ),
            [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: deterministicFixtureTxHash(
              "ledger-restore.deposit-l1-tx",
            ),
            [DepositsDB.Columns.LEDGER_TX_ID]: deterministicFixtureTxHash(
              "ledger-restore.deposit-ledger-tx",
            ),
            [DepositsDB.Columns.LEDGER_OUTPUT]: deterministicFixtureBytes(
              "ledger-restore.deposit-output",
              80,
            ),
            [DepositsDB.Columns.LEDGER_ADDRESS]:
              "addr_test1vzcsc5wzu3vsnjek2n80ayce53r4ha2g6wyetqddrp8z04q3yzv6k",
            [DepositsDB.Columns.PROJECTED_HEADER_HASH]: null,
            [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
          },
        ]);
        const undecodableTxId = deterministicFixtureTxHash(
          "ledger-restore.undecodable-tx",
        );
        yield* TxUtils.insertEntry(MempoolDB.tableName, {
          [TxUtils.Columns.TX_ID]: undecodableTxId,
          [TxUtils.Columns.TX]: Buffer.from("a1".repeat(96), "hex"),
        });
        const before = yield* snapshotRestoreRows;

        const restored = yield* Effect.either(
          restoreSpeculativeLedgerAfterCorrection({
            withdrawals: [],
            reopenedDepositEventIds: [depositId],
          }),
        );

        expect(
          Either.mapLeft(restored, ({ message, cause }) => ({
            message,
            cause,
          })),
        ).toEqual(
          Either.left({
            message:
              "A pending transaction cannot be decoded, so its dependence on reopened state cannot be decided",
            cause: undecodableTxId.toString("hex"),
          }),
        );
        expect(yield* snapshotRestoreRows).toEqual(before);
      }),
    ),
  );
});
