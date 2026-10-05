import { it } from "@effect/vitest";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import { MempoolLedgerDB, WithdrawalsDB } from "../../src/database/index.js";
import {
  address1,
  databaseFixtureBytes,
  databaseOutputReferenceId,
  databaseTxHash,
  isolatedDb,
  ledgerEntry1,
  makeHistoryWithdrawalEntry,
} from "./fixtures.js";

const withdrawal = (
  label: string,
  status: WithdrawalsDB.Status,
  validity: WithdrawalsDB.Validity | null,
): WithdrawalsDB.Entry => ({
  ...makeHistoryWithdrawalEntry(),
  [WithdrawalsDB.Columns.ID]: databaseOutputReferenceId(`wallet-view-${label}`),
  [WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH]: databaseTxHash(
    `wallet-view-l1-${label}`,
  ),
  [WithdrawalsDB.Columns.L2_OUTREF]: databaseOutputReferenceId(
    `wallet-view-l2-${label}`,
    1n,
  ),
  [WithdrawalsDB.Columns.STATUS]: status,
  ...(validity === null
    ? {}
    : {
        [WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]: databaseFixtureBytes(
          `wallet-view-${label}`,
          64,
        ),
        [WithdrawalsDB.Columns.VALIDITY]: validity,
        [WithdrawalsDB.Columns.VALIDITY_DETAIL]: { checked: true },
        [WithdrawalsDB.Columns.PROJECTED_HEADER_HASH]: databaseFixtureBytes(
          "wallet-view-header",
          28,
        ),
      }),
});

export const registerWalletViewTests = () => {
  describe("wallet view of an address", () => {
    it.effect(
      "hides exactly the outrefs admission refuses as pending-withdrawal inputs",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            yield* WithdrawalsDB.clear;
            yield* MempoolLedgerDB.clear;
            const unclassified = withdrawal(
              "unclassified",
              WithdrawalsDB.Status.Awaiting,
              null,
            );
            const projectedValid = withdrawal(
              "projected-valid",
              WithdrawalsDB.Status.Projected,
              WithdrawalsDB.Validity.WithdrawalIsValid,
            );
            // Classified invalid: its output stays the user's to spend.
            const projectedInvalid = withdrawal(
              "projected-invalid",
              WithdrawalsDB.Status.Projected,
              WithdrawalsDB.Validity.SpentWithdrawalUtxo,
            );
            yield* WithdrawalsDB.insertEntries([
              unclassified,
              projectedValid,
              projectedInvalid,
            ]);
            const named = yield* Effect.forEach(
              [unclassified, projectedValid, projectedInvalid],
              WithdrawalsDB.toLedgerOutRef,
            );
            yield* MempoolLedgerDB.insert([
              ledgerEntry1,
              ...named.map((outref) => ({ ...ledgerEntry1, outref })),
            ]);

            const hex = (entries: readonly { readonly outref: Buffer }[]) =>
              entries.map((entry) => entry.outref.toString("hex")).sort();
            const visible = hex(
              yield* MempoolLedgerDB.retrieveSpendableByAddress(address1),
            );
            expect(visible).toEqual(
              hex([ledgerEntry1, { ...ledgerEntry1, outref: named[2]! }]),
            );
            // Single-sourced: what the view hides is admission's own set.
            const pending =
              yield* WithdrawalsDB.retrievePendingLedgerOutRefHexes;
            expect([...pending].sort()).toEqual(
              named
                .slice(0, 2)
                .map((outref) => outref.toString("hex"))
                .sort(),
            );
            // The per-outref reads stay unfiltered: a withdrawal of the output
            // it names must still find it.
            expect(
              yield* MempoolLedgerDB.retrieveSpendableByTxOutRefs(named),
            ).toHaveLength(3);
          }),
        ),
    );
  });
};
