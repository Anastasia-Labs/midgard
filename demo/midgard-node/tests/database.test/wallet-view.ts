import { createHash } from "node:crypto";

import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk/core";
import { SqlClient, Statement } from "@effect/sql";
import { it } from "@effect/vitest";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import { spendableUtxoPage } from "../../src/database/http-read-pages.js";
import { MempoolLedgerDB, WithdrawalsDB } from "../../src/database/index.js";
import { decodeWithdrawalLedgerOutRef } from "../../src/database/withdrawal-ledger-outref.decode.js";
import { insertWithdrawals } from "../helpers/event-rows.js";
import {
  databaseFixtureBytes,
  databaseOutputReferenceId,
  databaseTxHash,
} from "./fixtures.bytes.js";
import { isolatedDb } from "./fixtures.environment.js";
import { makeHistoryWithdrawalEntry } from "./fixtures.events.js";
import { address1, ledgerEntry1 } from "./fixtures.ledger.js";
import { populateUnrelatedWalletViewEntries } from "./wallet-view.unrelated-ledger-entries.js";

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
    it.effect("decodes supported output index extrema", () =>
      Effect.sync(() => {
        const txId = Buffer.alloc(32, 0x11);
        for (const outputIndex of [0, 23, 24, 255, 256, 65535]) {
          const raw = Buffer.from(
            LucidData.to(
              {
                transactionId: txId.toString("hex"),
                outputIndex: BigInt(outputIndex),
              },
              SDK.OutputReference,
            ),
            "hex",
          );
          expect(decodeWithdrawalLedgerOutRef(raw)).toEqual(
            encodeMidgardSpendInputItem({ txId, outputIndex }),
          );
        }
      }),
    );

    it.effect(
      "uses candidate indexes on the actual page query with more than ten thousand unrelated pending withdrawals",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const seed = withdrawal(
              "indexed-unrelated-seed",
              WithdrawalsDB.Status.Awaiting,
              null,
            );
            yield* insertWithdrawals([seed]);
            // Imported enum members can create reverse-name properties after bundling.
            // Copy the actual seeded row's SQL keys instead of enumerating that object.
            const copied = Object.keys(seed).filter(
              (column) =>
                ![
                  WithdrawalsDB.Columns.ID,
                  WithdrawalsDB.Columns.L2_OUTREF,
                  WithdrawalsDB.Columns.L2_LEDGER_OUTREF,
                  WithdrawalsDB.Columns.L2_LEDGER_OUTREF_STATE,
                  WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH,
                ].some((excluded) => excluded === column),
            );
            yield* sql`INSERT INTO withdrawal_utxos (event_id, l2_outref, l2_ledger_outref, l2_ledger_outref_state, withdrawal_l1_tx_hash, ${sql.csv(copied.map((column) => sql`${sql(column)}`))})
        SELECT decode('d8799f5820' || lpad(to_hex(n), 64, '0') || '00ff', 'hex'),
          decode('d8799f5820' || lpad(to_hex(n), 64, '0') || '00ff', 'hex'),
          decode('825820' || lpad(to_hex(n), 64, '0') || '190000', 'hex'), 'decoded', decode(lpad(to_hex(n), 64, '0'), 'hex'),
          ${sql.csv(copied.map((column) => sql`${sql(column)}`))}
        FROM withdrawal_utxos CROSS JOIN generate_series(1, 12000) AS n WHERE event_id = ${seed.event_id}`;
            const candidates = Array.from({ length: 1000 }, (_, index) => ({
              ...ledgerEntry1,
              outref: encodeMidgardSpendInputItem({
                txId: createHash("sha256")
                  .update(`wallet-${index.toString()}`)
                  .digest(),
                outputIndex: 0,
              }),
            })).sort((left, right) =>
              Buffer.compare(left.outref, right.outref),
            );
            yield* MempoolLedgerDB.insert(candidates);
            yield* populateUnrelatedWalletViewEntries(12000);
            yield* sql`ANALYZE withdrawal_utxos`;
            yield* sql`ANALYZE mempool_ledger`;
            let compiled:
              | ReturnType<Statement.Statement<unknown>["compile"]>
              | undefined;
            const observed = new Proxy(sql, {
              apply(target, thisArg, args: unknown[]) {
                const statement: Statement.Statement<unknown> = Reflect.apply(
                  target,
                  thisArg,
                  args,
                );
                if (Array.isArray(args[0]) && "raw" in args[0]) {
                  const next = statement.compile();
                  if (next[0].includes("midgard-public-utxo-page"))
                    compiled = next;
                }
                return statement;
              },
            });
            const page = yield* spendableUtxoPage(address1, {
              limit: 25,
              cursor: undefined,
            }).pipe(Effect.provideService(SqlClient.SqlClient, observed));
            expect(page.map((row) => row.outref)).toEqual(
              candidates.slice(0, 26).map((row) => row.outref),
            );
            expect(compiled).toBeDefined();
            const explained = yield* sql.unsafe<{ "QUERY PLAN": unknown }>(
              `EXPLAIN (ANALYZE, BUFFERS, FORMAT JSON) ${compiled![0]}`,
              compiled![1],
            );
            const plan = JSON.stringify(explained[0]!["QUERY PLAN"]);
            console.info("bounded UTXO query EXPLAIN", plan);
            expect(plan).toContain("idx_mempool_ledger_address_outref");
            expect(plan).toContain("idx_withdrawal_ledger_outref_pending");
          }),
        ),
    );

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
            yield* insertWithdrawals([
              unclassified,
              projectedValid,
              projectedInvalid,
            ]);
            // Fixed ledger keys come independently from the authored logical references,
            // so mutating the extracted bridge cannot change the fixture's oracle too.
            const authored = [unclassified, projectedValid, projectedInvalid];
            const named = authored.map((entry) => {
              const logical = LucidData.from(
                entry.l2_outref.toString("hex"),
                SDK.OutputReference,
              );
              return encodeMidgardSpendInputItem({
                txId: Buffer.from(logical.transactionId, "hex"),
                outputIndex: Number(logical.outputIndex),
              });
            });
            expect(
              yield* Effect.forEach(authored, WithdrawalsDB.toLedgerOutRef),
            ).toEqual(named);
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
            expect(
              hex(
                yield* spendableUtxoPage(address1, {
                  limit: 100,
                  cursor: undefined,
                }),
              ),
            ).toEqual(visible);
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

    it.effect("leaves an undecodable withdrawal out of the pending set", () =>
      isolatedDb(
        Effect.gen(function* () {
          const valid = withdrawal(
            "undecodable-valid",
            WithdrawalsDB.Status.Awaiting,
            null,
          );
          const undecodable = {
            ...withdrawal("undecodable", WithdrawalsDB.Status.Awaiting, null),
            l2_outref: Buffer.from("ff", "hex"),
          };
          yield* insertWithdrawals([valid, undecodable]);
          const pending = yield* WithdrawalsDB.retrievePendingLedgerOutRefHexes;
          expect(pending.size).toBe(1);
        }),
      ),
    );
  });
};
