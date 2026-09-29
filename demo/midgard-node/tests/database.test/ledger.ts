import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { toHex } from "@lucid-evolution/lucid";
import { Duration, Effect, Fiber, Option, TestClock } from "effect";
import { describe, expect } from "vitest";

import {
  parseReconciliationResult,
  reconcileTxCommittedProgram,
  RECONCILIATION_SCHEMA_VERSION,
} from "../../src/commands/reconcile.js";
import {
  AddressHistoryDB,
  BlocksDB,
  ConfirmedLedgerDB,
  DepositSubmissionAttemptsDB,
  ImmutableDB,
  LedgerUtils,
  MempoolDB,
  MempoolLedgerDB,
  MempoolTxDeltasDB,
  ProcessedMempoolDB,
  StateQueueMutationLeasesDB,
  TxAdmissionsDB,
  TxUtils,
} from "../../src/database/index.js";
import { NodeConfig } from "../../src/services/config.js";
import { BatchSql } from "../../src/services/database.js";
import {
  makeWriteBehind,
  WriteBehind,
} from "../../src/services/write-behind.js";
import { ProcessedTx } from "../../src/utils.js";
import { resetApplicationTables } from ".././utils.js";
import {
  address1,
  address2,
  blockHeader1,
  blockHeader2,
  databaseFixtureBytes,
  databaseOutputReferenceId,
  databaseTxHash,
  emptyProgramMaterialSidecar,
  isolatedDb,
  ledgerEntry1,
  ledgerEntry2,
  makeDepositSubmissionAttempt,
  makeValidNativeImmutableEntry,
  removeTimestampFromLedgerEntry,
  removeTimestampFromTxEntry,
  retrieveAllMempool,
  tx1,
  tx2,
  tx3,
  txEntry1,
  txEntry2,
  txId1,
  txId2,
} from "./fixtures.js";

export const registerLedgerTests = () => {
  describe("StateQueueMutationLeasesDB", () => {
    it.effect(
      "returns Busy instead of failing when the state-queue lease is already held",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            const first = yield* StateQueueMutationLeasesDB.tryAcquire({
              holder: "first",
            });
            expect(first._tag).toBe("Acquired");
            if (first._tag !== "Acquired") {
              return;
            }

            const second = yield* StateQueueMutationLeasesDB.tryAcquire({
              holder: "second",
            });
            expect(second._tag).toBe("Busy");
            if (second._tag === "Busy") {
              expect(
                second.activeLease?.[StateQueueMutationLeasesDB.Columns.HOLDER],
              ).toBe("first");
            }

            yield* StateQueueMutationLeasesDB.release(first.token);
            const third = yield* StateQueueMutationLeasesDB.tryAcquire({
              holder: "third",
            });
            expect(third._tag).toBe("Acquired");
            if (third._tag === "Acquired") {
              yield* StateQueueMutationLeasesDB.release(third.token);
            }
          }),
        ),
    );

    it.effect(
      "tryWithLease releases successful work and marks failed work without leaving an active lease",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            const success = yield* StateQueueMutationLeasesDB.tryWithLease(
              "success",
              (token) => Effect.succeed(token),
            );
            expect(success._tag).toBe("Ran");
            expect(yield* StateQueueMutationLeasesDB.retrieveActive()).toBe(
              undefined,
            );

            const failure = yield* StateQueueMutationLeasesDB.tryWithLease(
              "failure",
              () => Effect.fail(new Error("boom")),
            ).pipe(Effect.either);
            expect(failure._tag).toBe("Left");
            expect(yield* StateQueueMutationLeasesDB.retrieveActive()).toBe(
              undefined,
            );

            const sql = yield* SqlClient.SqlClient;
            const rows = yield* sql<{
              status: StateQueueMutationLeasesDB.Status;
              last_error: string | null;
            }>`SELECT status, last_error FROM ${sql(
              StateQueueMutationLeasesDB.tableName,
            )}
            WHERE holder = ${"failure"}
            ORDER BY acquired_at DESC
            LIMIT 1`;
            expect(rows[0]?.status).toBe(
              StateQueueMutationLeasesDB.Status.Failed,
            );
            expect(rows[0]?.last_error).toContain("boom");
          }),
        ),
    );

    it.live(
      "tryWithLease keeps long-running state-queue work leased past the initial ttl",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const ttlMs = 1_000;
            const result = yield* StateQueueMutationLeasesDB.tryWithLease(
              "long-running-work",
              (token) =>
                Effect.sleep(Duration.millis(ttlMs + 250)).pipe(
                  Effect.andThen(StateQueueMutationLeasesDB.revalidate(token)),
                  Effect.as(token),
                ),
              {
                ttlMs,
                renewIntervalMs: 100,
              },
            );

            expect(result._tag).toBe("Ran");
            expect(yield* StateQueueMutationLeasesDB.retrieveActive()).toBe(
              undefined,
            );

            const sql = yield* SqlClient.SqlClient;
            const rows = yield* sql<{
              status: StateQueueMutationLeasesDB.Status;
            }>`
            SELECT status FROM ${sql(StateQueueMutationLeasesDB.tableName)}
            WHERE holder = ${"long-running-work"}
            ORDER BY acquired_at DESC
            LIMIT 1`;
            expect(rows[0]?.status).toBe(
              StateQueueMutationLeasesDB.Status.Released,
            );
          }),
        ),
    );
  });

  describe("BlocksDB", () => {
    it.effect(
      "insert, retrieve all, retrieve by header, retrieve by tx, clear block, clear all",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            // insert with some txs
            yield* BlocksDB.insert(blockHeader1, [tx1, tx2]);
            yield* BlocksDB.insert(blockHeader2, [tx3]);

            // retrieve tx hashes by header
            const txs =
              yield* BlocksDB.retrieveTxHashesByHeaderHash(blockHeader1);
            const txsHex = txs.map((row) => toHex(row));
            expect(new Set(txsHex)).toStrictEqual(
              new Set([toHex(tx1), toHex(tx2)]),
            );

            // retrieve header by tx hash
            const retrievedHeader =
              yield* BlocksDB.retrieveHeaderHashByTxHash(tx1);
            expect(toHex(retrievedHeader)).toEqual(toHex(blockHeader1));

            // retrieve all
            const all = yield* BlocksDB.retrieve;
            expect(
              new Set(
                all.map((a) => ({
                  [BlocksDB.Columns.HEADER_HASH]:
                    a[BlocksDB.Columns.HEADER_HASH],
                  [BlocksDB.Columns.TX_ID]: a[BlocksDB.Columns.TX_ID],
                })),
              ),
            ).toStrictEqual(
              new Set([
                {
                  [BlocksDB.Columns.HEADER_HASH]: blockHeader1,
                  [BlocksDB.Columns.TX_ID]: tx1,
                },
                {
                  [BlocksDB.Columns.HEADER_HASH]: blockHeader1,
                  [BlocksDB.Columns.TX_ID]: tx2,
                },
                {
                  [BlocksDB.Columns.HEADER_HASH]: blockHeader2,
                  [BlocksDB.Columns.TX_ID]: tx3,
                },
              ]),
            );

            //clear block
            yield* BlocksDB.clearBlock(blockHeader1);
            const afterClear = yield* BlocksDB.retrieve;
            expect(
              new Set(
                afterClear.map((a) => ({
                  [BlocksDB.Columns.HEADER_HASH]:
                    a[BlocksDB.Columns.HEADER_HASH],
                  [BlocksDB.Columns.TX_ID]: a[BlocksDB.Columns.TX_ID],
                })),
              ),
            ).toStrictEqual(
              new Set([
                {
                  [BlocksDB.Columns.HEADER_HASH]: blockHeader2,
                  [BlocksDB.Columns.TX_ID]: tx3,
                },
              ]),
            );

            // clear all
            yield* BlocksDB.clear;
            const afterClearAll = yield* BlocksDB.retrieve;
            expect(afterClearAll.length).toEqual(0);
          }),
        ),
    );
  });

  describe("MempoolDB", () => {
    it.effect(
      "insert, retrieve single, retrieve all, retrieve cbor by hash, retrieve cbors by hashes, retrieve count, clear txs, clear all",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            const pTxId1 = databaseTxHash("mempool.tx-1");
            const pTx1 = databaseFixtureBytes("mempool.tx-1-cbor", 64);
            const pSpent1 = databaseFixtureBytes("mempool.tx-1-spent", 32);
            const processedTx1: ProcessedTx = {
              txId: pTxId1,
              txCbor: pTx1,
              spent: [pSpent1],
              produced: [ledgerEntry1],
            };
            const pTxId2 = databaseTxHash("mempool.tx-2");
            const pTx2 = databaseFixtureBytes("mempool.tx-2-cbor", 64);
            const pSpent2 = databaseFixtureBytes("mempool.tx-2-spent", 32);
            const processedTx2: ProcessedTx = {
              txId: pTxId2,
              txCbor: pTx2,
              spent: [pSpent2],
              produced: [ledgerEntry2],
            };

            // insert multiple
            yield* MempoolDB.insertMultiple([processedTx1, processedTx2]);

            const sql = yield* SqlClient.SqlClient;
            const legacyRows = yield* sql<{
              readonly tx_id: Buffer;
              readonly tx: Buffer | null;
            }>`SELECT tx_id, tx FROM mempool ORDER BY tx_id`;
            expect(legacyRows).toHaveLength(2);
            expect(legacyRows.every((row) => row.tx !== null)).toBe(true);

            // retrieve tx cbor by hash
            const gotOne = yield* MempoolDB.retrieveTxCborByHash(pTxId1);
            expect(toHex(gotOne)).toEqual(toHex(pTx1));

            // retrieve tx cbor by hashes
            const gotMany = yield* MempoolDB.retrieveTxCborsByHashes([
              pTxId1,
              pTxId2,
            ]);
            expect(new Set(gotMany.map((r) => toHex(r)))).toStrictEqual(
              new Set([toHex(pTx1), toHex(pTx2)]),
            );

            // retrieve all
            const gotAll = yield* retrieveAllMempool;
            expect(
              new Set(gotAll.map((e) => removeTimestampFromTxEntry(e))),
            ).toStrictEqual(
              new Set([
                {
                  [TxUtils.Columns.TX_ID]: pTxId1,
                  [TxUtils.Columns.TX]: pTx1,
                },
                {
                  [TxUtils.Columns.TX_ID]: pTxId2,
                  [TxUtils.Columns.TX]: pTx2,
                },
              ]),
            );

            // retrieve count
            const gotCount: bigint = yield* MempoolDB.retrieveTxCount;
            expect(gotCount).toEqual(2n);

            // clearTxs
            yield* MempoolDB.clearTxs([pTxId1]);
            const afterClear = yield* retrieveAllMempool;
            expect(
              new Set(afterClear.map((e) => removeTimestampFromTxEntry(e))),
            ).toStrictEqual(
              new Set([
                {
                  [TxUtils.Columns.TX_ID]: pTxId2,
                  [TxUtils.Columns.TX]: pTx2,
                },
              ]),
            );

            // clearAll
            yield* MempoolDB.clear;
            const afterClearAll = yield* retrieveAllMempool;
            expect(afterClearAll.length).toEqual(0);

            // insert single
            yield* resetApplicationTables;
            yield* MempoolDB.insert(processedTx1);
            const afterInsertOne = yield* retrieveAllMempool;
            expect(
              afterInsertOne.map((e) => removeTimestampFromTxEntry(e)),
            ).toStrictEqual([
              {
                [TxUtils.Columns.TX_ID]: pTxId1,
                [TxUtils.Columns.TX]: pTx1,
              },
            ]);
          }),
        ),
    );

    it.effect(
      "walks strict oldest-first keyset pages without skips across timestamp ties",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const timestamps = [
              new Date("2026-07-10T12:00:00.000Z"),
              new Date("2026-07-10T12:00:00.000Z"),
              new Date("2026-07-10T12:00:00.000Z"),
              new Date("2026-07-10T12:00:01.000Z"),
              new Date("2026-07-10T12:00:01.000Z"),
              new Date("2026-07-10T12:00:02.000Z"),
            ];
            const rows = timestamps.map((time_stamp_tz, index) => ({
              tx_id: databaseTxHash(`mempool.keyset-${index.toString()}`),
              tx: databaseFixtureBytes(
                `mempool.keyset-${index.toString()}`,
                64,
              ),
              time_stamp_tz,
            }));
            yield* sql`INSERT INTO mempool ${sql.insert(rows)}`;
            const retrieved: TxUtils.EntryWithTimeStamp[] = [];
            let after: MempoolDB.MempoolCursor | undefined;
            do {
              const page = yield* MempoolDB.retrievePage({ after, limit: 2 });
              retrieved.push(...page.entries);
              after = page.nextCursor ?? undefined;
            } while (after !== undefined);

            const expected = [...rows].sort((left, right) => {
              const timeOrder =
                left.time_stamp_tz.getTime() - right.time_stamp_tz.getTime();
              return timeOrder !== 0
                ? timeOrder
                : Buffer.compare(left.tx_id, right.tx_id);
            });
            expect(retrieved.map((row) => row.tx_id.toString("hex"))).toEqual(
              expected.map((row) => row.tx_id.toString("hex")),
            );
            expect(retrieved.map((row) => row.tx.toString("hex"))).toEqual(
              expected.map((row) => row.tx.toString("hex")),
            );
            expect(
              new Set(retrieved.map((row) => row.tx_id.toString("hex"))).size,
            ).toBe(6);

            const snapshotBound = new Date("2026-07-10T12:00:01.000Z");
            const snapshotRows: TxUtils.EntryWithTimeStamp[] = [];
            let snapshotAfter: MempoolDB.MempoolCursor | undefined;
            do {
              const page = yield* MempoolDB.retrievePage({
                after: snapshotAfter,
                limit: 2,
                upTo: snapshotBound,
              });
              snapshotRows.push(...page.entries);
              snapshotAfter = page.nextCursor ?? undefined;
            } while (snapshotAfter !== undefined);
            expect(
              snapshotRows.map((row) => row.tx_id.toString("hex")),
            ).toEqual(
              expected
                .filter(
                  (row) =>
                    row.time_stamp_tz.getTime() <= snapshotBound.getTime(),
                )
                .map((row) => row.tx_id.toString("hex")),
            );
          }),
        ),
    );

    it.effect(
      "fails closed before creating a mempool membership without payload bytes",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const sql = yield* SqlClient.SqlClient;
            const txId = databaseTxHash("mempool.missing-payload");
            const result = yield* Effect.either(
              sql`INSERT INTO mempool (tx_id) VALUES (${txId})`,
            );

            expect(result._tag).toBe("Left");
            expect(yield* MempoolDB.retrieveTxCount).toBe(0n);
          }),
        ),
    );
  });

  describe("WriteBehind", () => {
    it.effect("flushes deferred delta and produced-address rows", () =>
      isolatedDb(
        Effect.gen(function* () {
          const txId = databaseTxHash("write-behind.flush");
          const processedTx: ProcessedTx = {
            txId,
            txCbor: databaseFixtureBytes("write-behind.flush-cbor", 64),
            spent: [databaseFixtureBytes("write-behind.flush-spent", 36)],
            produced: [
              {
                ...ledgerEntry1,
                [LedgerUtils.Columns.TX_ID]: txId,
              },
            ],
          };
          const writeBehind = yield* WriteBehind;
          yield* MempoolDB.insertMultiple([processedTx]);
          expect((yield* writeBehind.depths).totalDepth).toBeGreaterThan(0);

          yield* writeBehind.flushNow;
          expect((yield* writeBehind.depths).totalDepth).toBe(0);
          const deltas = yield* MempoolTxDeltasDB.retrieveByTxIds([txId]);
          expect(deltas.has(txId.toString("hex"))).toBe(true);
          const addressTxs = yield* AddressHistoryDB.retrieve(address1);
          expect(addressTxs.map((tx) => tx.toString("hex"))).toContain(
            processedTx.txCbor.toString("hex"),
          );
        }),
      ),
    );

    it.effect("keeps relaxed derived-flush durability transaction-local", () =>
      isolatedDb(
        Effect.gen(function* () {
          const writeBehind = yield* WriteBehind;
          const sql = yield* SqlClient.SqlClient;
          const batchSql = yield* BatchSql;
          const readSetting = (client: SqlClient.SqlClient) =>
            client<{ readonly synchronous_commit: string }>`
            SHOW synchronous_commit`.pipe(
              Effect.map((rows) => rows[0]?.synchronous_commit),
            );
          expect(yield* readSetting(sql)).toBe("on");
          expect(yield* readSetting(batchSql)).toBe("on");
          yield* writeBehind.enqueueTxDeltas([
            {
              txId: databaseTxHash("write-behind.local-sync-setting"),
              spent: [],
              produced: [],
            },
          ]);
          yield* writeBehind.flushNow;
          expect(yield* readSetting(sql)).toBe("on");
          expect(yield* readSetting(batchSql)).toBe("on");
        }),
      ),
    );

    it.effect("flushes a non-empty queue on the configured interval", () =>
      isolatedDb(
        Effect.gen(function* () {
          const txId = databaseTxHash("write-behind.interval");
          const writeBehind = yield* WriteBehind;
          const writer = yield* Effect.fork(writeBehind.run);
          yield* writeBehind.enqueueTxDeltas([
            {
              txId,
              spent: [],
              produced: [],
            },
          ]);
          yield* Effect.yieldNow();
          yield* TestClock.adjust(Duration.millis(250));
          yield* Effect.yieldNow();
          // TestClock releases the writer's interval sleep, but the PostgreSQL
          // promise completes on real I/O time rather than simulated time.
          yield* Effect.promise(
            () => new Promise<void>((resolve) => setTimeout(resolve, 100)),
          );
          const deltas = yield* MempoolTxDeltasDB.retrieveByTxIds([txId]);
          expect(deltas.has(txId.toString("hex"))).toBe(true);
          yield* Fiber.interrupt(writer);
        }),
      ),
    );

    it.effect("flushes immediately when the configured row batch is full", () =>
      isolatedDb(
        Effect.gen(function* () {
          const baseConfig = yield* NodeConfig;
          const batchSql = yield* BatchSql;
          yield* Effect.scoped(
            Effect.gen(function* () {
              const writeBehind = yield* makeWriteBehind;
              const writer = yield* Effect.fork(writeBehind.run);
              const txIds = [
                databaseTxHash("write-behind.size-1"),
                databaseTxHash("write-behind.size-2"),
              ];
              yield* writeBehind.enqueueTxDeltas(
                txIds.map((txId) => ({ txId, spent: [], produced: [] })),
              );
              yield* Effect.promise(
                () => new Promise<void>((resolve) => setTimeout(resolve, 100)),
              );
              const deltas = yield* MempoolTxDeltasDB.retrieveByTxIds(txIds);
              expect(deltas.size).toBe(2);
              yield* Fiber.interrupt(writer);
            }).pipe(
              Effect.provideService(NodeConfig, {
                ...baseConfig,
                WRITE_BEHIND_MAX_BATCH: 2,
                WRITE_BEHIND_FLUSH_INTERVAL_MS: 10_000,
                WRITE_BEHIND_QUEUE_CAPACITY: 10,
              }),
              Effect.provideService(BatchSql, batchSql),
            ),
          );
        }),
      ),
    );

    it.effect("retains a failed flush batch and retries it without loss", () =>
      isolatedDb(
        Effect.gen(function* () {
          const baseConfig = yield* NodeConfig;
          const batchSql = yield* BatchSql;
          const sql = yield* SqlClient.SqlClient;
          yield* Effect.scoped(
            Effect.gen(function* () {
              const writeBehind = yield* makeWriteBehind;
              const txId = databaseTxHash("write-behind.retry-retained");
              yield* writeBehind.enqueueTxDeltas([
                { txId, spent: [], produced: [] },
              ]);
              yield* writeBehind.enqueueAddressHistory([
                {
                  [LedgerUtils.Columns.TX_ID]: txId,
                  [LedgerUtils.Columns.ADDRESS]: address1,
                },
              ]);
              // The delta statement runs first. Failing the second statement
              // must roll the delta back and retain both queued rows. Renaming
              // the migration-built table away makes the statement fail; renaming
              // it back restores exactly that table.
              yield* sql`ALTER TABLE address_history RENAME TO address_history_withheld`;
              const failed = yield* Effect.either(writeBehind.flushNow);
              expect(failed._tag).toBe("Left");
              expect((yield* writeBehind.depths).totalDepth).toBe(2);
              expect(
                (yield* MempoolTxDeltasDB.retrieveByTxIds([txId])).size,
              ).toBe(0);

              yield* sql`ALTER TABLE address_history_withheld RENAME TO address_history`;
              yield* writeBehind.flushNow;
              expect((yield* writeBehind.depths).totalDepth).toBe(0);
              const deltas = yield* MempoolTxDeltasDB.retrieveByTxIds([txId]);
              expect(deltas.has(txId.toString("hex"))).toBe(true);
              const historyCount = yield* sql<{ readonly count: string }>`
              SELECT COUNT(*)::text AS count FROM address_history`;
              expect(historyCount[0]?.count).toBe("1");
            }).pipe(
              Effect.provideService(NodeConfig, baseConfig),
              Effect.provideService(BatchSql, batchSql),
            ),
          );
        }),
      ),
    );

    it.effect(
      "falls back to an inline write when the bounded queue is full",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const baseConfig = yield* NodeConfig;
            const batchSql = yield* BatchSql;
            yield* Effect.scoped(
              Effect.gen(function* () {
                const writeBehind = yield* makeWriteBehind;
                const first = databaseTxHash("write-behind.overflow-1");
                const second = databaseTxHash("write-behind.overflow-2");
                yield* writeBehind.enqueueTxDeltas([
                  { txId: first, spent: [], produced: [] },
                  { txId: second, spent: [], produced: [] },
                ]);

                const inline = yield* MempoolTxDeltasDB.retrieveByTxIds([
                  first,
                  second,
                ]);
                expect(inline.has(first.toString("hex"))).toBe(false);
                expect(inline.has(second.toString("hex"))).toBe(true);
                yield* writeBehind.flushNow;
                const complete = yield* MempoolTxDeltasDB.retrieveByTxIds([
                  first,
                  second,
                ]);
                expect(complete.size).toBe(2);
              }).pipe(
                Effect.provideService(NodeConfig, {
                  ...baseConfig,
                  WRITE_BEHIND_MAX_BATCH: 1_000,
                  WRITE_BEHIND_FLUSH_INTERVAL_MS: 10_000,
                  WRITE_BEHIND_QUEUE_CAPACITY: 1,
                }),
                Effect.provideService(BatchSql, batchSql),
              ),
            );
          }),
        ),
    );

    it.effect("retains failed inline overflow until persistence recovers", () =>
      isolatedDb(
        Effect.gen(function* () {
          const baseConfig = yield* NodeConfig;
          const batchSql = yield* BatchSql;
          const sql = yield* SqlClient.SqlClient;
          yield* Effect.scoped(
            Effect.gen(function* () {
              const writeBehind = yield* makeWriteBehind;
              const first = databaseTxHash("write-behind.retry-overflow-1");
              const second = databaseTxHash("write-behind.retry-overflow-2");
              // Withhold the migration-built table so the inline write fails;
              // renaming it back restores exactly that table.
              yield* sql`ALTER TABLE mempool_tx_deltas RENAME TO mempool_tx_deltas_withheld`;
              const enqueueFiber = yield* Effect.fork(
                writeBehind.enqueueTxDeltas([
                  { txId: first, spent: [], produced: [] },
                  { txId: second, spent: [], produced: [] },
                ]),
              );

              // PostgreSQL I/O runs on the live clock; allow the first inline
              // attempt to fail, then prove enqueue has not falsely completed.
              yield* Effect.promise(
                () => new Promise<void>((resolve) => setTimeout(resolve, 100)),
              );
              expect(Option.isNone(yield* Fiber.poll(enqueueFiber))).toBe(true);
              expect((yield* writeBehind.depths).totalDepth).toBe(1);

              yield* sql`ALTER TABLE mempool_tx_deltas_withheld RENAME TO mempool_tx_deltas`;
              yield* TestClock.adjust(Duration.millis(10));
              yield* Fiber.join(enqueueFiber);
              const inline = yield* MempoolTxDeltasDB.retrieveByTxIds([
                first,
                second,
              ]);
              expect(inline.has(first.toString("hex"))).toBe(false);
              expect(inline.has(second.toString("hex"))).toBe(true);

              yield* writeBehind.flushNow;
              expect(
                (yield* MempoolTxDeltasDB.retrieveByTxIds([first, second]))
                  .size,
              ).toBe(2);
            }).pipe(
              Effect.provideService(NodeConfig, {
                ...baseConfig,
                WRITE_BEHIND_MAX_BATCH: 1_000,
                WRITE_BEHIND_FLUSH_INTERVAL_MS: 10,
                WRITE_BEHIND_QUEUE_CAPACITY: 1,
              }),
              Effect.provideService(BatchSql, batchSql),
            ),
          );
        }),
      ),
    );

    it.effect(
      "does not enqueue auxiliary rows when the accept transaction fails",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            const txId = databaseTxHash("write-behind.rollback");
            const txCanonicalCbor = databaseFixtureBytes(
              "write-behind.rollback-cbor",
              64,
            );
            const admitted = yield* TxAdmissionsDB.admit({
              txId,
              txCanonicalCbor,
              programMaterialSidecarCbor: emptyProgramMaterialSidecar,
              submitSource: "native",
              currentBacklog: 0n,
              maxBacklog: 10,
            });
            const writeBehind = yield* WriteBehind;
            const result = yield* Effect.either(
              TxAdmissionsDB.markAccepted({
                rows: [admitted.entry],
                leaseOwner: "not-the-active-lease",
                processedTxs: [
                  {
                    txId,
                    txCbor: txCanonicalCbor,
                    spent: [],
                    produced: [],
                  },
                ],
              }),
            );
            expect(result._tag).toBe("Left");
            expect((yield* writeBehind.depths).totalDepth).toBe(0);
            expect(yield* MempoolDB.retrieveTxCount).toBe(0n);
          }),
        ),
    );

    it.effect("removes delta rows whose mempool transaction was cleared", () =>
      isolatedDb(
        Effect.gen(function* () {
          const txId = databaseTxHash("write-behind.orphan");
          yield* MempoolTxDeltasDB.upsertMany([
            { txId, spent: [], produced: [] },
          ]);
          expect(yield* MempoolTxDeltasDB.deleteOrphans).toBe(1);
          expect((yield* MempoolTxDeltasDB.retrieveByTxIds([txId])).size).toBe(
            0,
          );
        }),
      ),
    );
  });

  describe("ProcessedMempoolDB", () => {
    it.effect(
      "insert tx, insert txs, retrieve all, retrieve cbor by hash, retrieve cbors by hashes, clear all",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            // insert txs
            yield* ProcessedMempoolDB.insertTxs([txEntry1, txEntry2]);

            // retrieve tx cbor by hash
            const gotOne =
              yield* ProcessedMempoolDB.retrieveTxCborByHash(txId1);
            expect(toHex(gotOne)).toEqual(toHex(tx1));

            // retrieve tx cbors by hashes
            const gotMany = yield* ProcessedMempoolDB.retrieveTxCborsByHashes([
              txId1,
              txId2,
            ]);
            expect(new Set(gotMany.map((r) => toHex(r)))).toStrictEqual(
              new Set([toHex(tx1), toHex(tx2)]),
            );

            // retrieve all
            const gotAll = yield* ProcessedMempoolDB.retrieve;
            expect(
              new Set(gotAll.map((e) => removeTimestampFromTxEntry(e))),
            ).toStrictEqual(
              new Set([
                {
                  [TxUtils.Columns.TX_ID]: txId1,
                  [TxUtils.Columns.TX]: tx1,
                },
                {
                  [TxUtils.Columns.TX_ID]: txId2,
                  [TxUtils.Columns.TX]: tx2,
                },
              ]),
            );

            // clear all
            yield* ProcessedMempoolDB.clear;
            const afterClearAll = yield* ProcessedMempoolDB.retrieve;
            expect(afterClearAll.length).toEqual(0);

            // insert single
            yield* ProcessedMempoolDB.insertTx(txEntry1);
            const afterInsertOne = yield* ProcessedMempoolDB.retrieve;
            expect(
              afterInsertOne.map((e) => removeTimestampFromTxEntry(e)),
            ).toStrictEqual([
              {
                [TxUtils.Columns.TX_ID]: txId1,
                [TxUtils.Columns.TX]: tx1,
              },
            ]);
          }),
        ),
    );
  });

  describe("ImmutableDB", () => {
    it.effect(
      "insert tx, insert txs, retrieve all, retrieve cbor by hash, retrieve cbor by hashes, clear all",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            // insert txs
            yield* ImmutableDB.insertTxs([txEntry1, txEntry2]);

            // retrieve tx cbor by hash
            const gotOne = yield* ImmutableDB.retrieveTxCborByHash(txId1);
            expect(toHex(gotOne)).toEqual(toHex(tx1));

            // retrieve tx cbors by hashes
            const gotMany = yield* ImmutableDB.retrieveTxCborsByHashes([
              txId1,
              txId2,
            ]);
            expect(new Set(gotMany.map((r) => toHex(r)))).toStrictEqual(
              new Set([toHex(tx1), toHex(tx2)]),
            );

            // retrieve all
            const gotAll: readonly TxUtils.EntryWithTimeStamp[] =
              yield* ImmutableDB.retrieve;
            expect(
              new Set(
                gotAll.map((e: TxUtils.EntryWithTimeStamp) =>
                  removeTimestampFromTxEntry(e),
                ),
              ),
            ).toStrictEqual(
              new Set([
                {
                  [TxUtils.Columns.TX_ID]: txId1,
                  [TxUtils.Columns.TX]: tx1,
                },
                {
                  [TxUtils.Columns.TX_ID]: txId2,
                  [TxUtils.Columns.TX]: tx2,
                },
              ]),
            );

            // clear all
            yield* ImmutableDB.clear;
            const afterClearAll = yield* ImmutableDB.retrieve;
            expect(afterClearAll.length).toEqual(0);

            // insert single
            yield* ImmutableDB.insertTx(txEntry1);
            const afterInsertOne = yield* ImmutableDB.retrieve;
            expect(
              afterInsertOne.map((e) => removeTimestampFromTxEntry(e)),
            ).toStrictEqual([
              {
                [TxUtils.Columns.TX_ID]: txId1,
                [TxUtils.Columns.TX]: tx1,
              },
            ]);
          }),
        ),
    );

    it.effect("insertTxsValidatedNative accepts valid native payloads", (_) =>
      isolatedDb(
        Effect.gen(function* () {
          const valid = makeValidNativeImmutableEntry();

          yield* ImmutableDB.insertTxsValidatedNative([valid]);
          const stored = yield* ImmutableDB.retrieve;
          expect(stored).toHaveLength(1);
          expect(
            stored[0][TxUtils.Columns.TX_ID].equals(
              valid[TxUtils.Columns.TX_ID],
            ),
          ).toBe(true);
        }),
      ),
    );

    it.effect(
      "insertTxsValidatedNative rejects malformed or mismatched payloads",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            const valid = makeValidNativeImmutableEntry();
            const mismatchedTxId = Buffer.from(valid[TxUtils.Columns.TX_ID]);
            mismatchedTxId[0] ^= 0xff;
            const malformed: TxUtils.Entry = {
              [TxUtils.Columns.TX_ID]: Buffer.alloc(32, 7),
              [TxUtils.Columns.TX]: Buffer.alloc(64, 1),
            };
            const mismatch: TxUtils.Entry = {
              [TxUtils.Columns.TX_ID]: mismatchedTxId,
              [TxUtils.Columns.TX]: valid[TxUtils.Columns.TX],
            };

            const malformedResult = yield* Effect.either(
              ImmutableDB.insertTxsValidatedNative([malformed]),
            );
            expect(malformedResult._tag).toBe("Left");
            if (malformedResult._tag === "Left") {
              expect(malformedResult.left.message).toContain(
                "Failed native tx payload validation for immutable insertion",
              );
            }

            const mismatchResult = yield* Effect.either(
              ImmutableDB.insertTxsValidatedNative([mismatch]),
            );
            expect(mismatchResult._tag).toBe("Left");
            if (mismatchResult._tag === "Left") {
              expect(mismatchResult.left.message).toContain(
                "Failed native tx payload validation for immutable insertion",
              );
            }

            const remaining = yield* ImmutableDB.retrieve;
            expect(remaining).toHaveLength(0);
          }),
        ),
    );
  });

  describe("MempoolLedgerDB", () => {
    it.effect(
      "insert, retrieve by address, retrieve by outrefs, retrieve all, clearUTxOs, clearAll",
      () =>
        isolatedDb(
          Effect.gen(function* () {
            // insert
            yield* MempoolLedgerDB.insert([ledgerEntry1, ledgerEntry2]);

            // retrieve by address
            const atAddress =
              yield* MempoolLedgerDB.retrieveByAddress(address1);
            expect(
              new Set(atAddress.map((e) => removeTimestampFromLedgerEntry(e))),
            ).toStrictEqual(new Set([ledgerEntry1]));

            // retrieve by outrefs
            const byOutRefs = yield* MempoolLedgerDB.retrieveByTxOutRefs([
              ledgerEntry2[LedgerUtils.Columns.OUTREF],
              databaseFixtureBytes("mempool-ledger.missing-outref", 36),
            ]);
            expect(
              new Set(byOutRefs.map((e) => removeTimestampFromLedgerEntry(e))),
            ).toStrictEqual(new Set([ledgerEntry2]));

            // retrieve by empty outref set
            const emptyOutRefs = yield* MempoolLedgerDB.retrieveByTxOutRefs([]);
            expect(emptyOutRefs).toStrictEqual([]);

            // retrieve all
            const all = yield* MempoolLedgerDB.retrieve;
            expect(
              new Set(all.map((e) => removeTimestampFromLedgerEntry(e))),
            ).toStrictEqual(new Set([ledgerEntry1, ledgerEntry2]));

            // clear UTxOs
            yield* MempoolLedgerDB.clearUTxOs([
              ledgerEntry1[LedgerUtils.Columns.OUTREF],
            ]);
            const afterClear = yield* MempoolLedgerDB.retrieve;
            expect(
              new Set(afterClear.map((e) => removeTimestampFromLedgerEntry(e))),
            ).toStrictEqual(new Set([ledgerEntry2]));

            // clear all
            yield* MempoolLedgerDB.clear;
            const afterClearAll = yield* MempoolLedgerDB.retrieve;
            expect(afterClearAll.length).toEqual(0);
          }),
        ),
    );
  });

  describe("ConfirmedLedgerDB", () => {
    it.effect("insert multiple, retrieve", () =>
      isolatedDb(
        Effect.gen(function* () {
          // insert
          yield* ConfirmedLedgerDB.insertMultiple([ledgerEntry1, ledgerEntry2]);

          // retrieve all
          const all = yield* ConfirmedLedgerDB.retrieve;
          expect(
            new Set(all.map((e) => removeTimestampFromLedgerEntry(e))),
          ).toStrictEqual(new Set([ledgerEntry1, ledgerEntry2]));

          // clear UTxOs
          yield* ConfirmedLedgerDB.clearUTxOs([
            ledgerEntry1[LedgerUtils.Columns.OUTREF],
          ]);
          const afterClear = yield* ConfirmedLedgerDB.retrieve;
          expect(
            new Set(afterClear.map((e) => removeTimestampFromLedgerEntry(e))),
          ).toStrictEqual(new Set([ledgerEntry2]));

          // clear all
          yield* ConfirmedLedgerDB.clear;
          const afterClearAll = yield* ConfirmedLedgerDB.retrieve;
          expect(afterClearAll.length).toEqual(0);
        }),
      ),
    );
  });

  describe("AddressHistoryDB", () => {
    it.effect("insert, retrieve, clears tx hash, clear all", () =>
      isolatedDb(
        Effect.gen(function* () {
          const pTxId1 = databaseTxHash("address-history.tx-1");
          const pTx1 = databaseFixtureBytes("address-history.tx-1-cbor", 64);
          const pSpent1 = databaseFixtureBytes(
            "address-history.tx-1-spent",
            32,
          );
          const processedTx1: ProcessedTx = {
            txId: pTxId1,
            txCbor: pTx1,
            spent: [pSpent1],
            produced: [ledgerEntry1],
          };
          const ahEntry1: AddressHistoryDB.Entry = {
            [LedgerUtils.Columns.TX_ID]: pTxId1,
            [LedgerUtils.Columns.ADDRESS]: address1,
          };
          const pTxId2 = databaseTxHash("address-history.tx-2");
          const pTx2 = databaseFixtureBytes("address-history.tx-2-cbor", 64);
          const pSpent2 = databaseFixtureBytes(
            "address-history.tx-2-spent",
            32,
          );
          const processedTx2: ProcessedTx = {
            txId: pTxId2,
            txCbor: pTx2,
            spent: [pSpent2],
            produced: [ledgerEntry2],
          };
          const ahEntry2: AddressHistoryDB.Entry = {
            [LedgerUtils.Columns.TX_ID]: pTxId2,
            [LedgerUtils.Columns.ADDRESS]: address2,
          };

          // via mempool
          // insert
          yield* MempoolDB.insertMultiple([processedTx1, processedTx2]);
          yield* AddressHistoryDB.insertEntries([ahEntry1, ahEntry2]);

          // retrieve
          const expectedViaMempool = yield* AddressHistoryDB.retrieve(address1);
          expect(expectedViaMempool.map((t) => toHex(t))).toStrictEqual([
            toHex(pTx1),
          ]);

          // clears tx hash
          yield* AddressHistoryDB.delTxHash(pTxId1);
          const afterClear = yield* AddressHistoryDB.retrieve(address1);
          expect(afterClear).toStrictEqual([]);

          //clears all
          yield* AddressHistoryDB.clear;
          const afterClearAll1 = yield* AddressHistoryDB.retrieve(address1);
          const afterClearAll2 = yield* AddressHistoryDB.retrieve(address2);
          expect([...afterClearAll1, ...afterClearAll2]).toStrictEqual([]);

          // via immutable
          const txEntry1: TxUtils.Entry = {
            [TxUtils.Columns.TX_ID]: pTxId1,
            [TxUtils.Columns.TX]: pTx1,
          };
          const txEntry2: TxUtils.Entry = {
            [TxUtils.Columns.TX_ID]: pTxId2,
            [TxUtils.Columns.TX]: pTx2,
          };
          yield* resetApplicationTables;

          // insert
          yield* ImmutableDB.insertTxs([txEntry1, txEntry2]);
          yield* AddressHistoryDB.insertEntries([ahEntry1, ahEntry2]);

          // retrieve
          const expectedViaImmutable =
            yield* AddressHistoryDB.retrieve(address1);
          expect(expectedViaImmutable.map((t) => toHex(t))).toStrictEqual([
            toHex(pTx1),
          ]);

          // clears tx hash
          yield* AddressHistoryDB.delTxHash(pTxId1);
          const afterClearImmutable =
            yield* AddressHistoryDB.retrieve(address1);
          expect(afterClearImmutable).toStrictEqual([]);

          //clears all
          yield* AddressHistoryDB.clear;
          const afterClearAllImmutable1 =
            yield* AddressHistoryDB.retrieve(address1);
          const afterClearAllImmutable2 =
            yield* AddressHistoryDB.retrieve(address2);
          expect([
            ...afterClearAllImmutable1,
            ...afterClearAllImmutable2,
          ]).toStrictEqual([]);
        }),
      ),
    );

    it.effect("submit tx pipeline inserts a tx id in address db history", () =>
      isolatedDb(
        Effect.gen(function* () {
          const writeBehind = yield* WriteBehind;
          const thisWalletAddress = address2;
          const firstTxId = databaseTxHash("address-history.pipeline.tx-1");
          const firstProcessedTx: ProcessedTx = {
            txId: firstTxId,
            txCbor: databaseFixtureBytes(
              "address-history.pipeline.tx-1-cbor",
              64,
            ),
            spent: [
              databaseFixtureBytes("address-history.pipeline.tx-1-spent", 36),
            ],
            produced: [
              {
                [LedgerUtils.Columns.TX_ID]: firstTxId,
                [LedgerUtils.Columns.OUTREF]: databaseFixtureBytes(
                  "address-history.pipeline.tx-1-output-1-outref",
                  36,
                ),
                [LedgerUtils.Columns.OUTPUT]: databaseFixtureBytes(
                  "address-history.pipeline.tx-1-output-1",
                  80,
                ),
                [LedgerUtils.Columns.ADDRESS]: address1,
              },
              {
                [LedgerUtils.Columns.TX_ID]: firstTxId,
                [LedgerUtils.Columns.OUTREF]: databaseFixtureBytes(
                  "address-history.pipeline.tx-1-output-2-outref",
                  36,
                ),
                [LedgerUtils.Columns.OUTPUT]: databaseFixtureBytes(
                  "address-history.pipeline.tx-1-output-2",
                  80,
                ),
                [LedgerUtils.Columns.ADDRESS]: thisWalletAddress,
              },
            ],
          };
          yield* MempoolDB.insertMultiple([firstProcessedTx]);
          yield* writeBehind.flushNow;

          const sql = yield* SqlClient.SqlClient;
          const result1 =
            yield* sql<AddressHistoryDB.Entry>`SELECT * FROM address_history`;
          expect(
            result1.map((r) => r[LedgerUtils.Columns.ADDRESS]).sort(),
          ).toStrictEqual([address1, thisWalletAddress].sort());

          // two outputs for the same address should still produce one unique row
          yield* resetApplicationTables;
          const secondTxId = databaseTxHash("address-history.pipeline.tx-2");
          const secondProcessedTx: ProcessedTx = {
            txId: secondTxId,
            txCbor: databaseFixtureBytes(
              "address-history.pipeline.tx-2-cbor",
              64,
            ),
            spent: [
              databaseFixtureBytes("address-history.pipeline.tx-2-spent", 36),
            ],
            produced: [
              {
                [LedgerUtils.Columns.TX_ID]: secondTxId,
                [LedgerUtils.Columns.OUTREF]: databaseFixtureBytes(
                  "address-history.pipeline.tx-2-output-1-outref",
                  36,
                ),
                [LedgerUtils.Columns.OUTPUT]: databaseFixtureBytes(
                  "address-history.pipeline.tx-2-output-1",
                  80,
                ),
                [LedgerUtils.Columns.ADDRESS]: thisWalletAddress,
              },
              {
                [LedgerUtils.Columns.TX_ID]: secondTxId,
                [LedgerUtils.Columns.OUTREF]: databaseFixtureBytes(
                  "address-history.pipeline.tx-2-output-2-outref",
                  36,
                ),
                [LedgerUtils.Columns.OUTPUT]: databaseFixtureBytes(
                  "address-history.pipeline.tx-2-output-2",
                  80,
                ),
                [LedgerUtils.Columns.ADDRESS]: thisWalletAddress,
              },
            ],
          };
          yield* MempoolDB.insertMultiple([secondProcessedTx]);
          yield* writeBehind.flushNow;

          const result2 =
            yield* sql<AddressHistoryDB.Entry>`SELECT * FROM address_history`;
          expect(
            result2.map((r) => r[LedgerUtils.Columns.ADDRESS]),
          ).toStrictEqual([thisWalletAddress]);
        }),
      ),
    );
  });

  describe("DepositSubmissionAttemptsDB", () => {
    it.effect(
      "stores submitted attempts idempotently and rejects tx hash payload drift",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            const attempt = makeDepositSubmissionAttempt();
            const first =
              yield* DepositSubmissionAttemptsDB.insertSubmitted(attempt);
            const second =
              yield* DepositSubmissionAttemptsDB.insertSubmitted(attempt);

            expect(
              first[DepositSubmissionAttemptsDB.Columns.TX_HASH].equals(
                second[DepositSubmissionAttemptsDB.Columns.TX_HASH],
              ),
            ).toEqual(true);
            expect(
              first[DepositSubmissionAttemptsDB.Columns.CONFIRMATION_STATUS],
            ).toEqual(
              DepositSubmissionAttemptsDB.Status.SubmittedConfirmationUnknown,
            );

            const conflict = yield* Effect.either(
              DepositSubmissionAttemptsDB.insertSubmitted({
                ...attempt,
                [DepositSubmissionAttemptsDB.Columns.EXPECTED_LOVELACE]: "2",
              }),
            );
            expect(conflict._tag).toEqual("Left");
          }),
        ),
    );

    it.effect("stores bigint metadata as stable JSON strings", (_) =>
      isolatedDb(
        Effect.gen(function* () {
          const attempt = makeDepositSubmissionAttempt();
          const metadata =
            attempt[DepositSubmissionAttemptsDB.Columns.METADATA];
          const bigintAttempt: DepositSubmissionAttemptsDB.InsertSubmittedInput =
            {
              ...attempt,
              [DepositSubmissionAttemptsDB.Columns.METADATA]: {
                ...metadata,
                nonceInput: {
                  ...metadata.nonceInput,
                  outputIndex: 1n as unknown as number,
                },
                validTo: 1_800_000_000_000n as unknown as number,
                inclusionTime: 1_800_000_060_000n as unknown as number,
              },
            };

          const first =
            yield* DepositSubmissionAttemptsDB.insertSubmitted(bigintAttempt);
          const second =
            yield* DepositSubmissionAttemptsDB.insertSubmitted(bigintAttempt);
          const rawStoredMetadata = first[
            DepositSubmissionAttemptsDB.Columns.METADATA
          ] as unknown;
          const storedMetadata = (
            typeof rawStoredMetadata === "string"
              ? JSON.parse(rawStoredMetadata)
              : rawStoredMetadata
          ) as {
            readonly nonceInput: { readonly outputIndex: string };
            readonly validTo: string;
            readonly inclusionTime: string;
          };

          expect(
            first[DepositSubmissionAttemptsDB.Columns.TX_HASH].equals(
              second[DepositSubmissionAttemptsDB.Columns.TX_HASH],
            ),
          ).toEqual(true);
          expect(storedMetadata.nonceInput.outputIndex).toEqual("1");
          expect(storedMetadata.validTo).toEqual("1800000000000");
          expect(storedMetadata.inclusionTime).toEqual("1800000060000");
        }),
      ),
    );

    it.effect(
      "tracks confirmation, reconciliation, ambiguity, and open attempts",
      (_) =>
        isolatedDb(
          Effect.gen(function* () {
            const confirmed = makeDepositSubmissionAttempt({
              txHash: databaseTxHash("deposit-submission.confirmed"),
              eventId: databaseOutputReferenceId(
                "deposit-submission.confirmed",
              ),
            });
            const reconciled = makeDepositSubmissionAttempt({
              txHash: databaseTxHash("deposit-submission.reconciled"),
              eventId: databaseOutputReferenceId(
                "deposit-submission.reconciled",
              ),
            });
            const ambiguous = makeDepositSubmissionAttempt({
              txHash: databaseTxHash("deposit-submission.ambiguous"),
              eventId: databaseOutputReferenceId(
                "deposit-submission.ambiguous",
              ),
            });

            yield* DepositSubmissionAttemptsDB.insertSubmitted(confirmed);
            yield* DepositSubmissionAttemptsDB.insertSubmitted(reconciled);
            yield* DepositSubmissionAttemptsDB.insertSubmitted(ambiguous);

            yield* DepositSubmissionAttemptsDB.markConfirmed(
              confirmed[DepositSubmissionAttemptsDB.Columns.TX_HASH],
            );
            yield* DepositSubmissionAttemptsDB.markReconciled(
              reconciled[DepositSubmissionAttemptsDB.Columns.TX_HASH],
            );
            yield* DepositSubmissionAttemptsDB.markAmbiguous(
              ambiguous[DepositSubmissionAttemptsDB.Columns.TX_HASH],
              "confirmation timed out",
            );

            const open =
              yield* DepositSubmissionAttemptsDB.retrieveOpenAttempts();
            expect(open).toHaveLength(1);
            expect(
              open[0]?.[
                DepositSubmissionAttemptsDB.Columns.CONFIRMATION_STATUS
              ],
            ).toEqual(DepositSubmissionAttemptsDB.Status.Ambiguous);
          }),
        ),
    );
  });

  describe("Reconciliation commands", () => {
    it.effect("returns stable JSON for an unknown tx-committed target", (_) =>
      isolatedDb(
        Effect.gen(function* () {
          const txHash = databaseTxHash("reconcile.tx-committed.unknown");
          const resolved = yield* reconcileTxCommittedProgram({ txHash });

          expect(resolved.schemaVersion).toEqual(
            "midgard-e2e-reconciliation-v1",
          );
          expect(resolved.milestone).toEqual("tx-committed");
          expect(resolved.status).toEqual("ambiguous");
          expect(resolved.safeToRetryOriginalStep).toEqual(false);
          expect(resolved.target).toEqual({ txHash: txHash.toString("hex") });
          expect(
            resolved.evidence.some((entry) => entry.kind === "tx_status"),
          ).toEqual(true);
          expect(parseReconciliationResult(resolved)).toEqual(resolved);
          const missingMilestone = {
            ...resolved,
          } as Record<string, unknown>;
          delete missingMilestone.milestone;
          expect(() => parseReconciliationResult(missingMilestone)).toThrow(
            "missing required field",
          );
          expect(() =>
            parseReconciliationResult({ ...resolved, unexpected: true }),
          ).toThrow("unknown field");
          expect(() =>
            parseReconciliationResult({
              ...resolved,
              schemaVersion: "midgard-e2e-reconciliation-v0",
            }),
          ).toThrow(RECONCILIATION_SCHEMA_VERSION);
          expect(() =>
            parseReconciliationResult({
              ...resolved,
              evidence: [
                {
                  ...resolved.evidence[0]!,
                  unexpected: true,
                },
              ],
            }),
          ).toThrow("unknown field");
          expect(() =>
            parseReconciliationResult({
              ...resolved,
              target: { ...resolved.target, milestoneSpecific: { ok: true } },
            }),
          ).toThrow("unknown field");
          expect(
            parseReconciliationResult({
              ...resolved,
              evidence: resolved.evidence.map((entry) => ({
                ...entry,
                detail: { ...entry.detail, diagnosticSpecific: ["retained"] },
              })),
            }).evidence[0]?.detail,
          ).toHaveProperty("diagnosticSpecific");
          expect(() =>
            parseReconciliationResult({
              ...resolved,
              safeToRetryOriginalStep: true,
            }),
          ).toThrow("status, retry, or repair binding is inconsistent");
          const repairedBy = (action: string) => ({
            ...resolved,
            status: "repaired",
            safeToRetryOriginalStep: true,
            nextAction: null,
            repairActions: [action],
          });
          expect(parseReconciliationResult(repairedBy("merge_action"))).toEqual(
            repairedBy("merge_action"),
          );
          // `reconcile deposit-projected --repair` is deleted; nothing can
          // produce these actions, so a result claiming them is refused.
          for (const deleted of [
            "reconcile_deposit_submission_attempt",
            "reconcile_visible_deposit_utxos",
            "project_deposits_to_mempool_ledger",
          ]) {
            expect(() =>
              parseReconciliationResult(repairedBy(deleted)),
            ).toThrow("repairActions[0] must be one of");
          }
          expect(() =>
            parseReconciliationResult({
              ...resolved,
              evidence: [
                {
                  ...resolved.evidence[0]!,
                  detail: {
                    ...resolved.evidence[0]!.detail,
                    nonJsonObject: new Date("2026-01-01T00:00:00.000Z"),
                  },
                },
              ],
            }),
          ).toThrow("plain JSON objects");
        }),
      ),
    );
  });
};
