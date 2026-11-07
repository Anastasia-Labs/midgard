import { createHash } from "node:crypto";

import { encodeMidgardSpendInputItem } from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient, Statement } from "@effect/sql";
import { it } from "@effect/vitest";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";
import { describe, expect } from "vitest";

import { reconcileFollowerEvents } from "../../src/database/follower-events.js";
import { spendableUtxoPage } from "../../src/database/http-read-pages.js";
import { MempoolLedgerDB, WithdrawalsDB } from "../../src/database/index.js";
import {
  backfillWithdrawalLedgerOutRefs,
  publicWithdrawalReadAuthorityReady,
  WITHDRAWAL_OUTREF_BACKFILL_BATCH,
  WITHDRAWAL_OUTREF_DECODE_BUDGET_MS,
  WITHDRAWAL_OUTREF_MAX_BYTES,
} from "../../src/database/withdrawal-ledger-outref.backfill.js";
import { decodeWithdrawalLedgerOutRef } from "../../src/database/withdrawal-ledger-outref.decode.js";
import { withFollowerWrite } from "../../src/services/follower-write-gate.js";
import { insertWithdrawals } from "../helpers/event-rows.js";
import { modelSlotTime, writeFollowerView } from "../helpers/follower-view.js";
import {
  address1,
  databaseFixtureBytes,
  databaseOutputReferenceId,
  databaseTxHash,
  isolatedDb,
  ledgerEntry1,
  makeHistoryWithdrawalEntry,
} from "./fixtures.js";
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
    it.effect(
      "decodes supported extrema and the largest permitted noncanonical legacy reference within its synchronous budget",
      () =>
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
            expect(raw.length).toBeLessThan(WITHDRAWAL_OUTREF_MAX_BYTES);
            expect(decodeWithdrawalLedgerOutRef(raw)).toEqual(
              encodeMidgardSpendInputItem({ txId, outputIndex }),
            );
          }
          // Empty chunks and a padded uint64 preserve the logical Data value; this is
          // accepted by the actual SDK decoder, with the most chunk headers the bound permits.
          const prefix = "d8799f5f";
          const suffix = `5820${txId.toString("hex")}ff1b000000000000ffffff`;
          const raw = Buffer.from(
            prefix +
              "40".repeat(
                WITHDRAWAL_OUTREF_MAX_BYTES -
                  (prefix.length + suffix.length) / 2,
              ) +
              suffix,
            "hex",
          );
          expect(raw.length).toBe(WITHDRAWAL_OUTREF_MAX_BYTES);
          const started = performance.now();
          expect(decodeWithdrawalLedgerOutRef(raw)).toEqual(
            encodeMidgardSpendInputItem({ txId, outputIndex: 65535 }),
          );
          const elapsedMs = performance.now() - started;
          expect(elapsedMs).toBeLessThan(WITHDRAWAL_OUTREF_DECODE_BUDGET_MS);
          console.info("R7 legacy outref decode", {
            bytes: raw.length,
            elapsedMs,
          });
        }),
    );

    for (const heldStatus of [
      WithdrawalsDB.Status.Awaiting,
      WithdrawalsDB.Status.Finalized,
    ])
      it.effect(
        `holds an oversized ${heldStatus} unknown before decoding and rolls back real follower ingestion without changing raw rows or its point`,
        () =>
          isolatedDb(
            Effect.gen(function* () {
              const valid = withdrawal(
                "oversize-valid",
                WithdrawalsDB.Status.Awaiting,
                null,
              );
              const prefix = "d8799f5f";
              const suffix = `5820${"11".repeat(32)}ff1b000000000000ffffff`;
              const oversized = {
                ...withdrawal(
                  "oversize-held",
                  heldStatus,
                  heldStatus === WithdrawalsDB.Status.Finalized
                    ? WithdrawalsDB.Validity.WithdrawalIsValid
                    : null,
                ),
                l2_outref: Buffer.from(
                  prefix +
                    "40".repeat(
                      WITHDRAWAL_OUTREF_MAX_BYTES +
                        1 -
                        (prefix.length + suffix.length) / 2,
                    ) +
                    suffix,
                  "hex",
                ),
              };
              // This over-limit encoding is semantically decodable, so a missing input
              // bound cannot hide behind the legacy undecodable-row policy.
              expect(decodeWithdrawalLedgerOutRef(oversized.l2_outref)).toEqual(
                encodeMidgardSpendInputItem({
                  txId: Buffer.alloc(32, 0x11),
                  outputIndex: 65535,
                }),
              );
              yield* insertWithdrawals([valid, oversized]);
              const sql = yield* SqlClient.SqlClient;
              yield* sql`UPDATE withdrawal_utxos SET l1_event_key = ${Buffer.alloc(32, 0x31)}, l1_origin_outref = ${Buffer.alloc(34, 0x32)}`;
              const before =
                yield* sql`SELECT * FROM withdrawal_utxos ORDER BY event_id`;
              const settings =
                yield* sql`SELECT current_setting('statement_timeout') AS statement_timeout, current_setting('lock_timeout') AS lock_timeout`;
              const plan = yield* writeFollowerView(100, []);
              const pointBefore =
                yield* sql`SELECT * FROM follower_event_ingestion`;
              const held = yield* Effect.either(
                withFollowerWrite(
                  reconcileFollowerEvents(plan, {
                    network: "Custom",
                    slotToUnixTime: modelSlotTime,
                    cutoffMs: modelSlotTime(100),
                  }),
                ),
              );
              expect(held._tag).toBe("Left");
              if (held._tag === "Left")
                expect(held.left.message).toBe(
                  "E_WITHDRAWAL_READ_BACKFILL_INPUT_LIMIT",
                );
              expect(
                yield* sql`SELECT * FROM withdrawal_utxos ORDER BY event_id`,
              ).toEqual(before);
              expect(
                yield* sql`SELECT * FROM follower_event_ingestion`,
              ).toEqual(pointBefore);
              expect(
                yield* sql`SELECT current_setting('statement_timeout') AS statement_timeout, current_setting('lock_timeout') AS lock_timeout`,
              ).toEqual(settings);
              expect(yield* publicWithdrawalReadAuthorityReady).toBe(false);
              const page = yield* Effect.either(
                spendableUtxoPage(address1, { limit: 25, cursor: undefined }),
              );
              expect(page._tag).toBe("Left");
              if (page._tag === "Left")
                expect(page.left.message).toBe("E_PUBLIC_READ_MIGRATING");
            }),
          ),
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
            yield* backfillWithdrawalLedgerOutRefs;
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
            console.info("R7 bounded UTXO query EXPLAIN", plan);
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
            const held = yield* Effect.either(
              spendableUtxoPage(address1, { limit: 100, cursor: undefined }),
            );
            expect(held._tag).toBe("Left");
            if (held._tag === "Left")
              expect(held.left.message).toBe("E_PUBLIC_READ_MIGRATING");
            const sql = yield* SqlClient.SqlClient;
            const before =
              yield* sql`SELECT event_id, raw_event_info, l2_outref, status, validity FROM withdrawal_utxos ORDER BY event_id`;
            expect(yield* backfillWithdrawalLedgerOutRefs).toBe(3);
            expect(yield* publicWithdrawalReadAuthorityReady).toBe(true);
            expect(
              hex(
                yield* spendableUtxoPage(address1, {
                  limit: 100,
                  cursor: undefined,
                }),
              ),
            ).toEqual(visible);
            expect(
              yield* sql`SELECT event_id, raw_event_info, l2_outref, status, validity FROM withdrawal_utxos ORDER BY event_id`,
            ).toEqual(before);
            // A restored unknown row in a later view must hold again, even after a ready read.
            yield* sql`UPDATE withdrawal_utxos SET l2_ledger_outref = NULL, l2_ledger_outref_state = 'unknown' WHERE event_id = ${unclassified.event_id}`;
            expect(yield* publicWithdrawalReadAuthorityReady).toBe(false);
            const restored = yield* Effect.either(
              spendableUtxoPage(address1, { limit: 100, cursor: undefined }),
            );
            expect(restored._tag).toBe("Left");
            expect(yield* backfillWithdrawalLedgerOutRefs).toBe(1);
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

    it.effect(
      "prioritizes active unknowns over larger finalized history and bounds each gated batch",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const active = {
              ...withdrawal(
                "priority-active",
                WithdrawalsDB.Status.Awaiting,
                null,
              ),
              // Every archived id sorts before this one: removing active priority must fail.
              event_id: Buffer.from(
                LucidData.to(
                  { transactionId: "ff".repeat(32), outputIndex: 65535n },
                  SDK.OutputReference,
                ),
                "hex",
              ),
            };
            const archived = Array.from({ length: 300 }, (_, index) =>
              withdrawal(
                `priority-archive-${index}`,
                WithdrawalsDB.Status.Finalized,
                WithdrawalsDB.Validity.WithdrawalIsValid,
              ),
            );
            yield* insertWithdrawals([...archived, active]);
            const sql = yield* SqlClient.SqlClient;
            const before =
              yield* sql`SELECT event_id, raw_event_info, l2_outref, status, validity FROM withdrawal_utxos ORDER BY event_id`;
            const started = performance.now();
            const changed = yield* backfillWithdrawalLedgerOutRefs;
            expect(changed).toBeGreaterThan(0);
            expect(changed).toBeLessThanOrEqual(
              WITHDRAWAL_OUTREF_BACKFILL_BATCH,
            );
            expect(performance.now() - started).toBeLessThan(2_000);
            expect(yield* publicWithdrawalReadAuthorityReady).toBe(true);
            const states = yield* sql<{
              l2_ledger_outref_state: string;
              count: number;
            }>`SELECT l2_ledger_outref_state, COUNT(*)::int AS count FROM withdrawal_utxos GROUP BY l2_ledger_outref_state`;
            expect(
              states.find((row) => row.l2_ledger_outref_state === "unknown")
                ?.count,
            ).toBe(301 - changed);
            expect(
              yield* sql`SELECT event_id, raw_event_info, l2_outref, status, validity FROM withdrawal_utxos ORDER BY event_id`,
            ).toEqual(before);
          }),
        ),
    );
    it.effect(
      "rolls derived progress back with its outer follower transaction and ignores only proven undecodable rows",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const valid = withdrawal(
              "rollback-valid",
              WithdrawalsDB.Status.Awaiting,
              null,
            );
            const undecodable = {
              ...withdrawal(
                "rollback-undecodable",
                WithdrawalsDB.Status.Awaiting,
                null,
              ),
              l2_outref: Buffer.from("ff", "hex"),
            };
            yield* insertWithdrawals([valid, undecodable]);
            const sql = yield* SqlClient.SqlClient;
            const before =
              yield* sql`SELECT * FROM withdrawal_utxos ORDER BY event_id`;
            const aborted = yield* Effect.either(
              withFollowerWrite(
                Effect.gen(function* () {
                  expect(yield* backfillWithdrawalLedgerOutRefs).toBe(2);
                  expect(yield* publicWithdrawalReadAuthorityReady).toBe(true);
                  return yield* Effect.fail("causal-test-rollback");
                }),
              ),
            );
            expect(aborted._tag).toBe("Left");
            if (aborted._tag === "Left")
              expect(aborted.left).toBe("causal-test-rollback");
            expect(
              yield* sql`SELECT * FROM withdrawal_utxos ORDER BY event_id`,
            ).toEqual(before);
            expect(yield* publicWithdrawalReadAuthorityReady).toBe(false);
            expect(yield* backfillWithdrawalLedgerOutRefs).toBe(2);
            expect(
              yield* sql`SELECT l2_ledger_outref, l2_ledger_outref_state FROM withdrawal_utxos WHERE event_id = ${undecodable.event_id}`,
            ).toEqual([
              { l2_ledger_outref: null, l2_ledger_outref_state: "undecodable" },
            ]);
            expect(yield* publicWithdrawalReadAuthorityReady).toBe(true);
            const pending =
              yield* WithdrawalsDB.retrievePendingLedgerOutRefHexes;
            expect(pending.size).toBe(1);
          }),
        ),
    );
  });
};
