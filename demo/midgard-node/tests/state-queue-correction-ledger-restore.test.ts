import "./utils.js";

import { SqlClient } from "@effect/sql";
import { Effect, Either } from "effect";
import { describe, expect, it, vi } from "vitest";

import {
  DepositsDB,
  MempoolDB,
  MempoolInclusionsDB,
  TxUtils,
} from "../src/database/index.js";
import * as MempoolInclusions from "../src/database/mempoolInclusions.js";
import { currentLivenessReasons } from "../src/services/globals.liveness-reasons.js";
import {
  CORRECTION_RESTORE_BATCH_UNDECIDED,
  restoreSpeculativeLedgerAfterCorrection,
  REWIND_REJECT_CODE_REOPENED_DEPOSIT_INPUT,
} from "../src/services/state-queue-correction-ledger-restore.js";
import * as RejectClosure from "../src/services/working-ledger-recompute.reject-closure.js";
import { insertDeposits } from "./helpers/event-rows.js";
import { admitPending } from "./helpers/landed-blocks-sim.mempool.js";
import { immutableRow } from "./helpers/receipt-member-rows.js";
import {
  freshNative,
  hex,
  pendingTx,
  processOf,
  receipt,
  rejections,
  run,
  seed,
  sqlRun,
  unreversedReceipts,
} from "./landed-blocks-rebase.fixture.js";
import {
  deterministicFixtureBytes,
  deterministicFixtureOutputReferenceId,
  deterministicFixtureTxHash,
  provideDatabaseLayers,
  resetApplicationTables,
} from "./utils.js";

const reopenedDeposit = (label: string) => ({
  [DepositsDB.Columns.ID]: deterministicFixtureOutputReferenceId(
    `ledger-restore.${label}`,
  ),
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
});

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
        const deposit = reopenedDeposit("reopened-deposit");
        const depositId = deposit[DepositsDB.Columns.ID];
        yield* insertDeposits([deposit]);
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

const MARKING_BLOCK = "e4".repeat(28);

/**
 * A reopened deposit, the pending `x` spending its output, and the receipt
 * `[x, other]`; `other` is pending and marked by an own block (`marked`)
 * or out of the pending tables with no decision (`gone`).
 */
const reopenedBatch = async (other: "marked" | "gone") => {
  const globals = await processOf(freshNative());
  await seed(globals);
  const deposit = reopenedDeposit("batch-deposit");
  const outRef = await run(
    globals,
    Effect.gen(function* () {
      yield* insertDeposits([deposit]);
      return (yield* DepositsDB.toMempoolLedgerEntry(deposit)).outref;
    }),
  );
  const x = pendingTx("x", [outRef], 1);
  const m = pendingTx("m", [], 2);
  if (other === "marked") {
    await run(globals, admitPending([x, m]));
    await sqlRun(globals, () =>
      MempoolInclusionsDB.markIncluded(MARKING_BLOCK, [m.id]),
    );
  } else await run(globals, admitPending([x]));
  await receipt(globals, [x.id, m.id]);
  const restore = () =>
    run(
      globals,
      Effect.either(
        restoreSpeculativeLedgerAfterCorrection({
          withdrawals: [],
          reopenedDepositEventIds: [deposit[DepositsDB.Columns.ID]],
        }),
      ),
    );
  const reasons = () => Effect.runPromise(currentLivenessReasons(globals));
  return { globals, x, m, restore, reasons };
};

describe(
  "a correction's ledger restore over an acceptance receipt",
  { concurrent: false },
  () => {
    it("takes a co-member an own block marks as settled, and reverses the receipt", async () => {
      const { globals, x, restore, reasons } = await reopenedBatch("marked");
      expect(Either.isRight(await restore())).toBe(true);
      expect(await reasons()).not.toContain(CORRECTION_RESTORE_BATCH_UNDECIDED);
      expect(await rejections(globals)).toEqual([
        [hex(x.id), REWIND_REJECT_CODE_REOPENED_DEPOSIT_INPUT],
      ]);
      expect(await unreversedReceipts(globals)).toBe(0);
    });

    it("mutant: a restore without the marked rows finds the marked co-member undecided", async () => {
      const { globals, restore, reasons } = await reopenedBatch("marked");
      const spy = vi
        .spyOn(MempoolInclusions, "markedTxIds", "get")
        .mockReturnValue(Effect.succeed([]));
      const failed = await restore();
      spy.mockRestore();
      expect(
        Either.isLeft(failed) &&
          RejectClosure.undecidedBatchMemberIn(failed.left),
      ).toBeInstanceOf(RejectClosure.UndecidedBatchMember);
      expect(await reasons()).toContain(CORRECTION_RESTORE_BATCH_UNDECIDED);
      expect(await rejections(globals)).toEqual([]);
      expect(await unreversedReceipts(globals)).toBe(1);
    });

    it("names correction_restore_batch_undecided for an undecided co-member, and clears it once a restore completes", async () => {
      const { globals, m, restore, reasons } = await reopenedBatch("gone");
      const failed = await restore();
      expect(
        Either.isLeft(failed) &&
          RejectClosure.undecidedBatchMemberIn(failed.left),
      ).toBeInstanceOf(RejectClosure.UndecidedBatchMember);
      expect(await reasons()).toContain(CORRECTION_RESTORE_BATCH_UNDECIDED);
      expect(await unreversedReceipts(globals)).toBe(1);

      // The member's block folds: the next restore decides it.
      await sqlRun(globals, () => immutableRow(m.id));
      expect(Either.isRight(await restore())).toBe(true);
      expect(await reasons()).not.toContain(CORRECTION_RESTORE_BATCH_UNDECIDED);
      expect(await unreversedReceipts(globals)).toBe(0);
    });

    it("mutant: a restore that does not find the undecided member raises no named reason", async () => {
      const { restore, reasons } = await reopenedBatch("gone");
      const spy = vi
        .spyOn(RejectClosure, "findUndecidedBatchMember")
        .mockReturnValue(undefined);
      expect(Either.isLeft(await restore())).toBe(true);
      spy.mockRestore();
      expect(await reasons()).not.toContain(CORRECTION_RESTORE_BATCH_UNDECIDED);
    });
  },
);
