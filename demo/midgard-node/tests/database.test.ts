import { createHash } from "node:crypto";
import fs from "node:fs";
import path from "node:path";

import {
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { it } from "@effect/vitest";
import { toHex } from "@lucid-evolution/lucid";
import { Duration, Effect } from "effect";
import { beforeAll, describe, expect } from "vitest";

import {
  DepositStatusCommandError,
  resolveDepositStatusProgram,
} from "../src/commands/deposit-status.js";
import { reconcileTxCommittedProgram } from "../src/commands/reconcile.js";
import {
  // Address history
  AddressHistoryDB,
  // Block
  BlocksDB,
  // Utils
  CommonUtils,
  ConfirmedLedgerDB,
  DaPayloadsDB,
  DepositIngestionCursorDB,
  DepositSubmissionAttemptsDB,
  DepositsDB,
  // Tx
  ImmutableDB,
  // Ledger
  LatestLedgerDB,
  LedgerUtils,
  MempoolDB,
  MempoolLedgerDB,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  StateQueueMutationLeasesDB,
  TxAdmissionsDB,
  TxUtils,
} from "../src/database/index.js";
import * as MigrationRunner from "../src/database/migrations/runner.js";
import { projectDepositsToMempoolLedger } from "../src/fibers/project-deposits-to-mempool-ledger.js";
import { Globals } from "../src/services/globals.js";
import { ProcessedTx } from "../src/utils.js";
import { resolveIncludedDepositEntriesForWindow } from "../src/workers/utils/mpf.js";
import {
  deterministicFixtureBytes,
  deterministicFixtureOutputReferenceId,
  deterministicFixtureTxHash,
  expectLedgerUtxos,
  provideDatabaseLayers,
} from "./utils.js";

const flushAll = Effect.gen(function* () {
  yield* Effect.all(
    [
      MempoolLedgerDB.clear,
      LatestLedgerDB.clear,
      ConfirmedLedgerDB.clear,
      BlocksDB.clear,
      ImmutableDB.clear,
      MempoolDB.clear,
      AddressHistoryDB.clear,
      ProcessedMempoolDB.clear,
      DepositsDB.clear,
      DepositSubmissionAttemptsDB.clear,
      DepositIngestionCursorDB.clear,
      PendingBlockFinalizationsDB.clear,
      DaPayloadsDB.clear,
      CommonUtils.clearTable(TxAdmissionsDB.tableName),
      CommonUtils.clearTable(MutationJobsDB.tableName),
      CommonUtils.clearTable(StateQueueMutationLeasesDB.tableName),
    ],
    { discard: true },
  );
});

const databaseFixtureBytes = (label: string, length: number): Buffer =>
  deterministicFixtureBytes(`database:${label}`, length);

const databaseTxHash = (label: string): Buffer =>
  deterministicFixtureTxHash(`database:${label}`);

const databaseOutputReferenceId = (
  label: string,
  outputIndex: number | bigint = 0n,
): Buffer =>
  deterministicFixtureOutputReferenceId(`database:${label}`, outputIndex);

beforeAll(async () => {
  await Effect.runPromise(
    provideDatabaseLayers(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        // Ensure a clean schema: drop tables (and thus indexes) if they exist
        yield* sql`
          DROP SCHEMA public CASCADE;
          CREATE SCHEMA public;`;
        yield* MigrationRunner.migrate({
          appVersion: "test",
          actor: "database.test",
        });
        yield* flushAll;
      }),
    ),
  );
});

describe("Database: initialization and basic operations", () => {
  it.effect("initialize and flush", (_) =>
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* flushAll;
        // Smoke select to ensure connection works
        const sql = yield* SqlClient.SqlClient;
        const now = yield* sql<Date>`SELECT NOW()`;
        expect(now.length).toBeGreaterThan(0);
      }),
    ),
  );
});

describe("DaPayloadsDB", () => {
  it.effect(
    "stores payloads idempotently and rejects conflicting bytes for a header",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;
          const headerHash = databaseFixtureBytes("da-payload-header", 28);
          const payload = databaseFixtureBytes("da-payload-cbor", 48);
          const payloadHash = createHash("sha256").update(payload).digest();
          const insert = {
            [DaPayloadsDB.Columns.HEADER_HASH]: headerHash,
            [DaPayloadsDB.Columns.VERSION]: 1,
            [DaPayloadsDB.Columns.PAYLOAD_CBOR]: payload,
            [DaPayloadsDB.Columns.PAYLOAD_SHA256]: payloadHash,
            [DaPayloadsDB.Columns.UTXOS_ROOT]: "11".repeat(32),
            [DaPayloadsDB.Columns.TRANSACTIONS_ROOT]: "22".repeat(32),
            [DaPayloadsDB.Columns.DEPOSITS_ROOT]: "33".repeat(32),
            [DaPayloadsDB.Columns.WITHDRAWALS_ROOT]: "44".repeat(32),
            [DaPayloadsDB.Columns.BLOCK_START_TIME]: new Date(
              "2026-06-12T00:00:00.000Z",
            ),
            [DaPayloadsDB.Columns.BLOCK_END_TIME]: new Date(
              "2026-06-12T00:00:10.000Z",
            ),
          };

          yield* DaPayloadsDB.upsertAvailable(insert);
          yield* DaPayloadsDB.upsertAvailable(insert);

          const stored = yield* DaPayloadsDB.retrieveByHeaderHash(headerHash);
          expect(stored._tag).toBe("Some");
          if (stored._tag === "Some") {
            expect(stored.value[DaPayloadsDB.Columns.PAYLOAD_CBOR]).toEqual(
              payload,
            );
          }

          const conflict = yield* Effect.either(
            DaPayloadsDB.upsertAvailable({
              ...insert,
              [DaPayloadsDB.Columns.PAYLOAD_CBOR]: Buffer.from([...payload, 0]),
            }),
          );
          expect(conflict._tag).toBe("Left");
        }),
      ),
  );

  it.effect(
    "retrieves only finalized pending journals missing DA payload rows",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;
          const sql = yield* SqlClient.SqlClient;
          const missingHeader = databaseFixtureBytes(
            "missing-da-payload-finalized-header",
            28,
          );
          const coveredHeader = databaseFixtureBytes(
            "covered-da-payload-finalized-header",
            28,
          );
          const activeHeader = databaseFixtureBytes(
            "active-da-payload-header",
            28,
          );
          const baseTime = new Date("2026-06-12T00:00:00.000Z");
          const row = (
            headerHash: Buffer,
            status: PendingBlockFinalizationsDB.Status,
          ) => ({
            [PendingBlockFinalizationsDB.Columns.HEADER_HASH]: headerHash,
            [PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH]:
              databaseTxHash(`submitted-${headerHash.toString("hex")}`),
            [PendingBlockFinalizationsDB.Columns.STATE_QUEUE_LEASE_TOKEN]:
              "lease",
            [PendingBlockFinalizationsDB.Columns.BASE_SNAPSHOT_ID]: "snapshot",
            [PendingBlockFinalizationsDB.Columns.BASE_TAIL_OUT_REF]: "base#0",
            [PendingBlockFinalizationsDB.Columns.BASE_TAIL_HEADER_HASH]:
              databaseFixtureBytes("base-tail-header", 28),
            [PendingBlockFinalizationsDB.Columns.BASE_TAIL_DATUM_CBOR]:
              "d87980",
            [PendingBlockFinalizationsDB.Columns.BASE_UTXOS_ROOT]:
              SDK.EMPTY_MERKLE_TREE_ROOT,
            [PendingBlockFinalizationsDB.Columns.BASE_TRANSACTIONS_ROOT]:
              SDK.EMPTY_MERKLE_TREE_ROOT,
            [PendingBlockFinalizationsDB.Columns.BASE_DEPOSITS_ROOT]:
              SDK.EMPTY_MERKLE_TREE_ROOT,
            [PendingBlockFinalizationsDB.Columns.BASE_WITHDRAWALS_ROOT]:
              SDK.EMPTY_MERKLE_TREE_ROOT,
            [PendingBlockFinalizationsDB.Columns.BLOCK_START_TIME]: baseTime,
            [PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME]: new Date(
              baseTime.getTime() + 1_000,
            ),
            [PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT]:
              SDK.EMPTY_MERKLE_TREE_ROOT,
            [PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSACTIONS_ROOT]:
              SDK.EMPTY_MERKLE_TREE_ROOT,
            [PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSITS_ROOT]:
              SDK.EMPTY_MERKLE_TREE_ROOT,
            [PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWALS_ROOT]:
              SDK.EMPTY_MERKLE_TREE_ROOT,
            [PendingBlockFinalizationsDB.Columns.STATUS]: status,
            [PendingBlockFinalizationsDB.Columns.OBSERVED_CONFIRMED_AT_MS]: 1n,
          });
          yield* sql`INSERT INTO ${sql(
            PendingBlockFinalizationsDB.tableName,
          )} ${sql.insert([
            row(missingHeader, PendingBlockFinalizationsDB.Status.Finalized),
            row(coveredHeader, PendingBlockFinalizationsDB.Status.Finalized),
            row(
              activeHeader,
              PendingBlockFinalizationsDB.Status.SubmittedUnconfirmed,
            ),
          ])}`;
          yield* DaPayloadsDB.upsertAvailable({
            [DaPayloadsDB.Columns.HEADER_HASH]: coveredHeader,
            [DaPayloadsDB.Columns.VERSION]: 1,
            [DaPayloadsDB.Columns.PAYLOAD_CBOR]: Buffer.from("a100", "hex"),
            [DaPayloadsDB.Columns.PAYLOAD_SHA256]: createHash("sha256")
              .update(Buffer.from("a100", "hex"))
              .digest(),
            [DaPayloadsDB.Columns.UTXOS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
            [DaPayloadsDB.Columns.TRANSACTIONS_ROOT]:
              SDK.EMPTY_MERKLE_TREE_ROOT,
            [DaPayloadsDB.Columns.DEPOSITS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
            [DaPayloadsDB.Columns.WITHDRAWALS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
            [DaPayloadsDB.Columns.BLOCK_START_TIME]: baseTime,
            [DaPayloadsDB.Columns.BLOCK_END_TIME]: new Date(
              baseTime.getTime() + 1_000,
            ),
          });

          const missing =
            yield* PendingBlockFinalizationsDB.retrieveFinalizedMissingDaPayloads(
              {
                limit: 10,
              },
            );
          expect(
            missing.map((record) =>
              record[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString(
                "hex",
              ),
            ),
          ).toEqual([missingHeader.toString("hex")]);

          const covered =
            yield* PendingBlockFinalizationsDB.retrieveFinalizedMissingDaPayloads(
              {
                headerHash: coveredHeader,
                limit: 10,
              },
            );
          expect(covered).toEqual([]);
        }),
      ),
  );
});

describe("PendingBlockFinalizationsDB", () => {
  const pendingSubmissionFixture = (
    headerHash: Buffer,
  ): PendingBlockFinalizationsDB.PrepareInput => {
    const blockStartTime = new Date("2026-06-12T00:00:00.000Z");
    return {
      headerHash,
      metadata: {
        stateQueueLeaseToken: "lease-token",
        baseSnapshotId: "snapshot",
        baseTailOutRef: "base#0",
        baseTailHeaderHash: databaseFixtureBytes("base-tail-header", 28),
        baseTailDatumCbor: "d87980",
        baseRoots: {
          utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
        blockStartTime,
        expectedRoots: {
          utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
          withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
        },
      },
      blockEndTime: new Date(blockStartTime.getTime() + 60_000),
      depositEventIds: [],
      depositEntries: [],
      withdrawalEventIds: [],
      withdrawalEntries: [],
      mempoolTxIds: [],
      mempoolTxs: [],
      mempoolTxSourceTable: "none",
      utxoEntries: [],
    };
  };

  it.effect(
    "can discard and replace no-submission pending journals for retry recovery",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;
          const headerHash = databaseFixtureBytes(
            "retryable-pending-header",
            28,
          );
          const input = pendingSubmissionFixture(headerHash);

          yield* PendingBlockFinalizationsDB.preparePendingSubmission(input);
          yield* PendingBlockFinalizationsDB.discardUnsubmittedPendingSubmission(
            headerHash,
          );
          let active = yield* PendingBlockFinalizationsDB.retrieveActive();
          expect(active._tag).toBe("None");

          yield* PendingBlockFinalizationsDB.preparePendingSubmission(input);
          yield* PendingBlockFinalizationsDB.markAbandoned(headerHash);
          active = yield* PendingBlockFinalizationsDB.retrieveActive();
          expect(active._tag).toBe("None");

          yield* PendingBlockFinalizationsDB.preparePendingSubmission(input);
          active = yield* PendingBlockFinalizationsDB.retrieveActive();
          expect(active._tag).toBe("Some");
          if (active._tag === "Some") {
            expect(
              active.value[PendingBlockFinalizationsDB.Columns.STATUS],
            ).toBe(PendingBlockFinalizationsDB.Status.PendingSubmission);
            expect(
              active.value[
                PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH
              ],
            ).toBeNull();
          }
        }),
      ),
  );

  it.effect(
    "deletes superseded pre-submit journals after the replacement journal finalizes",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;
          const staleHeaderHash = databaseFixtureBytes(
            "superseded-stale-pending-header",
            28,
          );
          const finalizedHeaderHash = databaseFixtureBytes(
            "superseded-finalized-pending-header",
            28,
          );
          const staleInput = {
            ...pendingSubmissionFixture(staleHeaderHash),
            utxoEntries: [
              {
                [PendingBlockFinalizationsDB.UtxoColumns.OUTREF]:
                  databaseOutputReferenceId("superseded-stale-utxo", 0n),
                [PendingBlockFinalizationsDB.UtxoColumns.OUTPUT]:
                  databaseFixtureBytes("superseded-stale-output", 48),
              },
            ],
          };

          yield* PendingBlockFinalizationsDB.preparePendingSubmission(
            staleInput,
          );
          yield* PendingBlockFinalizationsDB.markAbandoned(staleHeaderHash);

          const replacementFixture =
            pendingSubmissionFixture(finalizedHeaderHash);
          const replacementInput = {
            ...replacementFixture,
            metadata: {
              ...replacementFixture.metadata,
              baseTailOutRef: "replacement-base#1",
            },
          };
          yield* PendingBlockFinalizationsDB.preparePendingSubmission(
            replacementInput,
          );
          yield* PendingBlockFinalizationsDB.markSubmitted(
            finalizedHeaderHash,
            databaseTxHash("superseded-finalization-submitted"),
          );
          yield* PendingBlockFinalizationsDB.markLocalFinalizationComplete(
            finalizedHeaderHash,
          );
          yield* PendingBlockFinalizationsDB.markFinalized(
            finalizedHeaderHash,
          );

          const stale =
            yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
              staleHeaderHash,
            );
          const finalized =
            yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
              finalizedHeaderHash,
            );
          expect(stale._tag).toBe("None");
          expect(finalized._tag).toBe("Some");

          const sql = yield* SqlClient.SqlClient;
          const staleUtxos = yield* sql<{ count: number }>`SELECT COUNT(*)::int AS count
            FROM pending_block_finalization_utxos
            WHERE header_hash = ${staleHeaderHash}`;
          expect(staleUtxos[0]?.count).toBe(0);
        }),
      ),
  );
});

describe("StateQueueMutationLeasesDB", () => {
  it.effect(
    "returns Busy instead of failing when the state-queue lease is already held",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

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
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

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
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

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
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

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
                [BlocksDB.Columns.HEADER_HASH]: a[BlocksDB.Columns.HEADER_HASH],
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
                [BlocksDB.Columns.HEADER_HASH]: a[BlocksDB.Columns.HEADER_HASH],
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
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

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
          const gotAll = yield* MempoolDB.retrieve;
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
          const afterClear = yield* MempoolDB.retrieve;
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
          const afterClearAll = yield* MempoolDB.retrieve;
          expect(afterClearAll.length).toEqual(0);

          // insert single
          yield* flushAll;
          yield* MempoolDB.insert(processedTx1);
          const afterInsertOne = yield* MempoolDB.retrieve;
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
});

describe("ProcessedMempoolDB", () => {
  it.effect(
    "insert tx, insert txs, retrieve all, retrieve cbor by hash, retrieve cbors by hashes, clear all",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          // insert txs
          yield* ProcessedMempoolDB.insertTxs([txEntry1, txEntry2]);

          // retrieve tx cbor by hash
          const gotOne = yield* ProcessedMempoolDB.retrieveTxCborByHash(txId1);
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
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

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
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* flushAll;
        const valid = makeValidNativeImmutableEntry();

        yield* ImmutableDB.insertTxsValidatedNative([valid]);
        const stored = yield* ImmutableDB.retrieve;
        expect(stored).toHaveLength(1);
        expect(
          stored[0][TxUtils.Columns.TX_ID].equals(valid[TxUtils.Columns.TX_ID]),
        ).toBe(true);
      }),
    ),
  );

  it.effect(
    "insertTxsValidatedNative rejects malformed or mismatched payloads",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;
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

describe("LatestLedgerDB", () => {
  it.effect("insert multiple, retrieve, clear UTxOs, clear all", () =>
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* flushAll;

        // insert multiple
        yield* LatestLedgerDB.insertMultiple([ledgerEntry1, ledgerEntry2]);

        // retrieve all
        const all = yield* LatestLedgerDB.retrieve;
        expectLedgerUtxos(all, [ledgerEntry1, ledgerEntry2]);

        // clear UTxOs
        yield* LatestLedgerDB.clearUTxOs([
          ledgerEntry1[LedgerUtils.Columns.OUTREF],
        ]);
        const afterClear = yield* LatestLedgerDB.retrieve;
        expectLedgerUtxos(afterClear, [ledgerEntry2]);

        // clear all
        yield* LatestLedgerDB.clear;
        const afterClearAll = yield* LatestLedgerDB.retrieve;
        expect(afterClearAll.length).toEqual(0);
      }),
    ),
  );
});

describe("MempoolLedgerDB", () => {
  it.effect(
    "insert, retrieve by address, retrieve by outrefs, retrieve all, clearUTxOs, clearAll",
    () =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          // insert
          yield* MempoolLedgerDB.insert([ledgerEntry1, ledgerEntry2]);

          // retrieve by address
          const atAddress = yield* MempoolLedgerDB.retrieveByAddress(address1);
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
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* flushAll;

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
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* flushAll;

        const pTxId1 = databaseTxHash("address-history.tx-1");
        const pTx1 = databaseFixtureBytes("address-history.tx-1-cbor", 64);
        const pSpent1 = databaseFixtureBytes("address-history.tx-1-spent", 32);
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
        const pSpent2 = databaseFixtureBytes("address-history.tx-2-spent", 32);
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
        yield* flushAll;

        // insert
        yield* ImmutableDB.insertTxs([txEntry1, txEntry2]);
        yield* AddressHistoryDB.insertEntries([ahEntry1, ahEntry2]);

        // retrieve
        const expectedViaImmutable = yield* AddressHistoryDB.retrieve(address1);
        expect(expectedViaImmutable.map((t) => toHex(t))).toStrictEqual([
          toHex(pTx1),
        ]);

        // clears tx hash
        yield* AddressHistoryDB.delTxHash(pTxId1);
        const afterClearImmutable = yield* AddressHistoryDB.retrieve(address1);
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
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* flushAll;
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

        const sql = yield* SqlClient.SqlClient;
        const result1 =
          yield* sql<AddressHistoryDB.Entry>`SELECT * FROM address_history`;
        expect(
          result1.map((r) => r[LedgerUtils.Columns.ADDRESS]).sort(),
        ).toStrictEqual([address1, thisWalletAddress].sort());

        // two outputs for the same address should still produce one unique row
        yield* flushAll;
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
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const attempt = makeDepositSubmissionAttempt();
          const first = yield* DepositSubmissionAttemptsDB.insertSubmitted(
            attempt,
          );
          const second = yield* DepositSubmissionAttemptsDB.insertSubmitted(
            attempt,
          );

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
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* flushAll;

        const attempt = makeDepositSubmissionAttempt();
        const metadata = attempt[DepositSubmissionAttemptsDB.Columns.METADATA];
        const bigintAttempt: DepositSubmissionAttemptsDB.InsertSubmittedInput = {
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
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const confirmed = makeDepositSubmissionAttempt({
            txHash: databaseTxHash("deposit-submission.confirmed"),
            eventId: databaseOutputReferenceId("deposit-submission.confirmed"),
          });
          const reconciled = makeDepositSubmissionAttempt({
            txHash: databaseTxHash("deposit-submission.reconciled"),
            eventId: databaseOutputReferenceId(
              "deposit-submission.reconciled",
            ),
          });
          const ambiguous = makeDepositSubmissionAttempt({
            txHash: databaseTxHash("deposit-submission.ambiguous"),
            eventId: databaseOutputReferenceId("deposit-submission.ambiguous"),
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
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* flushAll;

        const txHash = databaseTxHash("reconcile.tx-committed.unknown");
        const resolved = yield* reconcileTxCommittedProgram({ txHash });

        expect(resolved.schemaVersion).toEqual(
          "midgard-e2e-reconciliation-v1",
        );
        expect(resolved.milestone).toEqual("tx-committed");
        expect(resolved.status).toEqual("ambiguous");
        expect(resolved.safeToRetryOriginalStep).toEqual(true);
        expect(resolved.target).toEqual({ txHash: txHash.toString("hex") });
        expect(resolved.evidence.some((entry) => entry.kind === "tx_status"))
          .toEqual(true);
      }),
    ),
  );
});

describe("DepositsDB and MempoolLedgerDB exact-once projection", () => {
  it.effect("rejects payload drift for the same deposit event_id", (_) =>
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* flushAll;

        const eventId = databaseOutputReferenceId("deposits.payload-drift", 0);
        const first = makeDepositEntry({
          [DepositsDB.Columns.ID]: eventId,
        });
        const conflicting = makeDepositEntry({
          [DepositsDB.Columns.ID]: eventId,
          [DepositsDB.Columns.INFO]: databaseFixtureBytes(
            "deposits.payload-drift.conflicting-info",
            48,
          ),
        });

        yield* DepositsDB.insertEntries([first]);
        const result = yield* Effect.either(
          DepositsDB.insertEntries([conflicting]),
        );

        expect(result._tag).toEqual("Left");
      }),
    ),
  );

  it.effect("retrieves one deposit by event_id", (_) =>
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* flushAll;

        const deposit = makeDepositEntry();
        yield* DepositsDB.insertEntries([deposit]);

        const retrieved = yield* DepositsDB.retrieveByEventId(
          deposit[DepositsDB.Columns.ID],
        );
        expect(retrieved._tag).toEqual("Some");
        if (retrieved._tag !== "Some") {
          throw new Error("expected deposit lookup to return a row");
        }

        expect(
          retrieved.value[DepositsDB.Columns.ID].equals(
            deposit[DepositsDB.Columns.ID],
          ),
        ).toEqual(true);
        expect(
          retrieved.value[DepositsDB.Columns.DEPOSIT_L1_TX_HASH].equals(
            deposit[DepositsDB.Columns.DEPOSIT_L1_TX_HASH],
          ),
        ).toEqual(true);
      }),
    ),
  );

  it.effect(
    "retrieves deposits by Cardano tx hash in deterministic order",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const sharedCardanoTxHash = databaseTxHash(
            "deposits.shared-cardano-tx.deterministic-order",
          );
          const first = makeDepositEntry({
            [DepositsDB.Columns.INCLUSION_TIME]: new Date(
              "2026-04-13T17:00:00.000Z",
            ),
            [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: sharedCardanoTxHash,
            [DepositsDB.Columns.ID]: databaseOutputReferenceId(
              "deposits.deterministic-order.first",
              0,
            ),
          });
          const second = makeDepositEntry({
            [DepositsDB.Columns.INCLUSION_TIME]: new Date(
              "2026-04-13T17:00:01.000Z",
            ),
            [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: sharedCardanoTxHash,
            [DepositsDB.Columns.ID]: databaseOutputReferenceId(
              "deposits.deterministic-order.second",
              1,
            ),
          });
          yield* DepositsDB.insertEntries([second, first]);

          const retrieved =
            yield* DepositsDB.retrieveByCardanoTxHash(sharedCardanoTxHash);

          expect(retrieved).toHaveLength(2);
          expect(
            retrieved[0]?.[DepositsDB.Columns.ID].equals(
              first[DepositsDB.Columns.ID],
            ),
          ).toEqual(true);
          expect(
            retrieved[1]?.[DepositsDB.Columns.ID].equals(
              second[DepositsDB.Columns.ID],
            ),
          ).toEqual(true);
        }),
      ),
  );

  it.effect(
    "rejects ambiguous cardanoTxHash lookups and requires eventId to disambiguate",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const sharedCardanoTxHash = databaseTxHash(
            "deposits.shared-cardano-tx.ambiguous-status",
          );
          yield* DepositsDB.insertEntries([
            makeDepositEntry({
              [DepositsDB.Columns.ID]: databaseOutputReferenceId(
                "deposits.ambiguous.first",
                0,
              ),
              [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: sharedCardanoTxHash,
            }),
            makeDepositEntry({
              [DepositsDB.Columns.ID]: databaseOutputReferenceId(
                "deposits.ambiguous.second",
                1,
              ),
              [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: sharedCardanoTxHash,
            }),
          ]);

          const result = yield* Effect.either(
            resolveDepositStatusProgram({
              cardanoTxHash: sharedCardanoTxHash,
            }),
          );

          expect(result._tag).toEqual("Left");
          if (result._tag !== "Left") {
            throw new Error("expected ambiguous lookup to fail");
          }
          expect(result.left).toBeInstanceOf(DepositStatusCommandError);
          if (!(result.left instanceof DepositStatusCommandError)) {
            throw new Error("expected DepositStatusCommandError");
          }
          expect(result.left.status).toEqual(409);
        }),
      ),
  );

  it.effect(
    "allows eventId to disambiguate a shared cardanoTxHash lookup",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const sharedCardanoTxHash = databaseTxHash(
            "deposits.shared-cardano-tx.event-id-disambiguates",
          );
          const first = makeDepositEntry({
            [DepositsDB.Columns.ID]: databaseOutputReferenceId(
              "deposits.disambiguates.first",
              0,
            ),
            [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: sharedCardanoTxHash,
          });
          const second = makeDepositEntry({
            [DepositsDB.Columns.ID]: databaseOutputReferenceId(
              "deposits.disambiguates.second",
              1,
            ),
            [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]: sharedCardanoTxHash,
          });
          yield* DepositsDB.insertEntries([first, second]);

          const resolved = yield* resolveDepositStatusProgram({
            eventId: second[DepositsDB.Columns.ID],
            cardanoTxHash: sharedCardanoTxHash,
          });

          expect(
            resolved[DepositsDB.Columns.ID].equals(
              second[DepositsDB.Columns.ID],
            ),
          ).toEqual(true);
        }),
      ),
  );

  it.effect(
    "projects a deposit into mempool_ledger exactly once by source_event_id",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const deposit = makeDepositEntry();
          yield* DepositsDB.insertEntries([deposit]);
          const mempoolEntry = yield* DepositsDB.toMempoolLedgerEntry(deposit);

          yield* MempoolLedgerDB.insertDepositEntriesStrict([mempoolEntry]);
          yield* DepositsDB.markAwaitingAsProjected([
            deposit[DepositsDB.Columns.ID],
          ]);

          const duplicateResult = yield* Effect.either(
            MempoolLedgerDB.insertDepositEntriesStrict([mempoolEntry]),
          );
          expect(duplicateResult._tag).toEqual("Left");

          const mempoolRows = yield* MempoolLedgerDB.retrieve;
          expect(mempoolRows).toHaveLength(1);
          expect(
            mempoolRows[0]?.[MempoolLedgerDB.Columns.SOURCE_EVENT_ID]?.equals(
              deposit[DepositsDB.Columns.ID],
            ),
          ).toEqual(true);

          const projectedRows = yield* DepositsDB.retrieveProjectedEntries();
          expect(projectedRows).toHaveLength(1);
          expect(projectedRows[0]?.[DepositsDB.Columns.STATUS]).toEqual(
            DepositsDB.Status.Projected,
          );
        }),
      ),
  );

  it.effect("projects only deposits whose inclusion time has arrived", (_) =>
    provideDatabaseLayers(
      Effect.gen(function* () {
        yield* flushAll;

        const pastDeposit = makeDepositEntry({
          [DepositsDB.Columns.INCLUSION_TIME]: new Date(
            "2020-01-01T00:00:00.000Z",
          ),
        });
        const futureDeposit = makeDepositEntry({
          [DepositsDB.Columns.INCLUSION_TIME]: new Date(
            "2099-01-01T00:00:00.000Z",
          ),
        });
        yield* DepositsDB.insertEntries([pastDeposit, futureDeposit]);

        yield* projectDepositsToMempoolLedger.pipe(
          Effect.provide(Globals.Default),
        );

        const projectedRows = yield* DepositsDB.retrieveProjectedEntries();
        expect(projectedRows).toHaveLength(1);
        expect(
          projectedRows[0]?.[DepositsDB.Columns.ID].equals(
            pastDeposit[DepositsDB.Columns.ID],
          ),
        ).toEqual(true);

        const awaitingRows = yield* DepositsDB.retrieveAwaitingEntries();
        expect(awaitingRows).toHaveLength(1);
        expect(
          awaitingRows[0]?.[DepositsDB.Columns.ID].equals(
            futureDeposit[DepositsDB.Columns.ID],
          ),
        ).toEqual(true);
      }),
    ),
  );

  it.effect(
    "rejects source_event_id payload drift during idempotent projection reconciliation",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const deposit = makeDepositEntry();
          yield* DepositsDB.insertEntries([deposit]);
          const mempoolEntry = yield* DepositsDB.toMempoolLedgerEntry(deposit);
          yield* MempoolLedgerDB.insertDepositEntriesStrict([mempoolEntry]);

          const conflictingEntry = {
            ...mempoolEntry,
            [MempoolLedgerDB.Columns.OUTPUT]: databaseFixtureBytes(
              "deposits.projection-reconciliation.conflicting-output",
              80,
            ),
          };

          const result = yield* Effect.either(
            MempoolLedgerDB.reconcileDepositEntries([conflictingEntry]),
          );
          expect(result._tag).toEqual("Left");
        }),
      ),
  );

  it.effect(
    "assigns and clears a projected header hash for a projected deposit",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const deposit = makeDepositEntry();
          const headerHash = databaseFixtureBytes(
            "deposits.projected-header",
            28,
          );
          yield* DepositsDB.insertEntries([deposit]);
          yield* DepositsDB.markAwaitingAsProjected([
            deposit[DepositsDB.Columns.ID],
          ]);
          yield* DepositsDB.markProjectedByEventIds(
            [deposit[DepositsDB.Columns.ID]],
            headerHash,
          );

          const assignedRows = yield* DepositsDB.retrieveAllEntries();
          expect(
            assignedRows[0]?.[DepositsDB.Columns.PROJECTED_HEADER_HASH]?.equals(
              headerHash,
            ),
          ).toEqual(true);

          yield* DepositsDB.clearProjectedHeaderAssignmentByEventIds(
            [deposit[DepositsDB.Columns.ID]],
            headerHash,
          );

          const clearedRows = yield* DepositsDB.retrieveAllEntries();
          expect(
            clearedRows[0]?.[DepositsDB.Columns.PROJECTED_HEADER_HASH],
          ).toBeNull();
        }),
      ),
  );

  it.effect(
    "treats projection claiming as idempotent once a deposit is already projected",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const deposit = makeDepositEntry();
          yield* DepositsDB.insertEntries([deposit]);
          yield* DepositsDB.markAwaitingAsProjected([
            deposit[DepositsDB.Columns.ID],
          ]);
          yield* DepositsDB.markAwaitingAsProjected([
            deposit[DepositsDB.Columns.ID],
          ]);

          const rows = yield* DepositsDB.retrieveAllEntries();
          expect(rows).toHaveLength(1);
          expect(rows[0]?.[DepositsDB.Columns.STATUS]).toEqual(
            DepositsDB.Status.Projected,
          );
          expect(
            rows[0]?.[DepositsDB.Columns.PROJECTED_HEADER_HASH],
          ).toBeNull();
        }),
      ),
  );

  it.effect(
    "re-includes an overdue projected deposit whose earlier header assignment was abandoned",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const currentBlockStartTime = new Date("2026-04-13T17:28:10.000Z");
          const overdueProjectedDeposit = makeDepositEntry({
            [DepositsDB.Columns.INCLUSION_TIME]: new Date(
              currentBlockStartTime.getTime() - 1_000,
            ),
            [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
          });
          yield* DepositsDB.insertEntries([overdueProjectedDeposit]);

          const included = yield* resolveIncludedDepositEntriesForWindow({
            currentBlockStartTime,
            effectiveEndTime: new Date(currentBlockStartTime.getTime() + 1_000),
          });

          expect(included).toHaveLength(1);
          expect(
            included[0]?.[DepositsDB.Columns.ID].equals(
              overdueProjectedDeposit[DepositsDB.Columns.ID],
            ),
          ).toEqual(true);
          expect(included[0]?.[DepositsDB.Columns.STATUS]).toEqual(
            DepositsDB.Status.Projected,
          );
        }),
      ),
  );

  it.effect(
    "fails closed when an overdue deposit was never projected before its window closed",
    (_) =>
      provideDatabaseLayers(
        Effect.gen(function* () {
          yield* flushAll;

          const currentBlockStartTime = new Date("2026-04-13T17:28:10.000Z");
          const overdueAwaitingDeposit = makeDepositEntry({
            [DepositsDB.Columns.INCLUSION_TIME]: new Date(
              currentBlockStartTime.getTime() - 1_000,
            ),
          });
          yield* DepositsDB.insertEntries([overdueAwaitingDeposit]);

          const result = yield* Effect.either(
            resolveIncludedDepositEntriesForWindow({
              currentBlockStartTime,
              effectiveEndTime: new Date(
                currentBlockStartTime.getTime() + 1_000,
              ),
            }),
          );

          expect(result._tag).toEqual("Left");
        }),
      ),
  );
});

const blockHeader1 = databaseFixtureBytes("blocks.header-1", 32);
const blockHeader2 = databaseFixtureBytes("blocks.header-2", 32);

const txId1 = databaseTxHash("shared.tx-1");
const txId2 = databaseTxHash("shared.tx-2");

const tx1 = databaseFixtureBytes("shared.tx-1-cbor", 64);
const tx2 = databaseFixtureBytes("shared.tx-2-cbor", 64);
const tx3 = databaseFixtureBytes("shared.tx-3-cbor", 64);

const outref1 = databaseFixtureBytes("shared.outref-1", 36);
const outref2 = databaseFixtureBytes("shared.outref-2", 36);

const output1 = databaseFixtureBytes("shared.output-1", 80);
const output2 = databaseFixtureBytes("shared.output-2", 80);

type TxFixture = {
  readonly cborHex: string;
  readonly txId: string;
};

const fixturePath = path.resolve(__dirname, "./txs/txs_0.json");
const firstFixture = (
  JSON.parse(fs.readFileSync(fixturePath, "utf8")) as readonly TxFixture[]
)[0];

const makeValidNativeImmutableEntry = (): TxUtils.Entry => {
  const nativeTx = cardanoTxBytesToMidgardNativeTxCanonicalCbor(
    Buffer.from(firstFixture.cborHex, "hex"),
  );
  const txId = computeMidgardNativeTxId(
    decodeMidgardNativeTxFullFromCanonicalCbor(nativeTx),
  );
  return {
    [TxUtils.Columns.TX_ID]: txId,
    [TxUtils.Columns.TX]: nativeTx,
  };
};

const address1 =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";
const address2 =
  "addr_test1vzcsc5wzu3vsnjek2n80ayce53r4ha2g6wyetqddrp8z04q3yzv6k";

const txEntry1: TxUtils.Entry = {
  [TxUtils.Columns.TX_ID]: txId1,
  [TxUtils.Columns.TX]: tx1,
};

const txEntry2: TxUtils.Entry = {
  [TxUtils.Columns.TX_ID]: txId2,
  [TxUtils.Columns.TX]: tx2,
};

const removeTimestampFromTxEntry = (
  e: TxUtils.Entry,
): TxUtils.EntryNoTimeStamp => {
  return {
    [TxUtils.Columns.TX_ID]: e[TxUtils.Columns.TX_ID],
    [TxUtils.Columns.TX]: e[TxUtils.Columns.TX],
  };
};

const ledgerEntry1: LedgerUtils.Entry = {
  [LedgerUtils.Columns.TX_ID]: txId1,
  [LedgerUtils.Columns.OUTREF]: outref1,
  [LedgerUtils.Columns.OUTPUT]: output1,
  [LedgerUtils.Columns.ADDRESS]: address1,
};

const ledgerEntry2: LedgerUtils.Entry = {
  [LedgerUtils.Columns.TX_ID]: txId2,
  [LedgerUtils.Columns.OUTREF]: outref2,
  [LedgerUtils.Columns.OUTPUT]: output2,
  [LedgerUtils.Columns.ADDRESS]: address2,
};

const removeTimestampFromLedgerEntry = (
  e: LedgerUtils.Entry,
): LedgerUtils.EntryNoTimeStamp => {
  return {
    [LedgerUtils.Columns.TX_ID]: e[LedgerUtils.Columns.TX_ID],
    [LedgerUtils.Columns.OUTREF]: e[LedgerUtils.Columns.OUTREF],
    [LedgerUtils.Columns.OUTPUT]: e[LedgerUtils.Columns.OUTPUT],
    [LedgerUtils.Columns.ADDRESS]: e[LedgerUtils.Columns.ADDRESS],
  };
};

const depositFixtureBaseTimeMs = Date.parse("2026-04-13T17:28:10.000Z");
let depositFixtureSequence = 0;

const makeDepositEntry = (
  overrides: Partial<DepositsDB.Entry> = {},
): DepositsDB.Entry => {
  const fixtureIndex = depositFixtureSequence;
  depositFixtureSequence += 1;
  const fixtureLabel = `entry-${fixtureIndex.toString().padStart(4, "0")}`;
  const eventId =
    overrides[DepositsDB.Columns.ID] ??
    databaseOutputReferenceId(`deposits.${fixtureLabel}`, fixtureIndex);
  return {
    [DepositsDB.Columns.ID]: eventId,
    [DepositsDB.Columns.INFO]:
      overrides[DepositsDB.Columns.INFO] ??
      databaseFixtureBytes(`deposits.${fixtureLabel}.info`, 48),
    [DepositsDB.Columns.INCLUSION_TIME]:
      overrides[DepositsDB.Columns.INCLUSION_TIME] ??
      new Date(depositFixtureBaseTimeMs + fixtureIndex),
    [DepositsDB.Columns.DEPOSIT_L1_TX_HASH]:
      overrides[DepositsDB.Columns.DEPOSIT_L1_TX_HASH] ??
      databaseTxHash(`deposits.${fixtureLabel}.l1-tx`),
    [DepositsDB.Columns.LEDGER_TX_ID]:
      overrides[DepositsDB.Columns.LEDGER_TX_ID] ??
      databaseTxHash(`deposits.${fixtureLabel}.ledger-tx`),
    [DepositsDB.Columns.LEDGER_OUTPUT]:
      overrides[DepositsDB.Columns.LEDGER_OUTPUT] ??
      databaseFixtureBytes(`deposits.${fixtureLabel}.ledger-output`, 80),
    [DepositsDB.Columns.LEDGER_ADDRESS]:
      overrides[DepositsDB.Columns.LEDGER_ADDRESS] ?? address1,
    [DepositsDB.Columns.PROJECTED_HEADER_HASH]:
      overrides[DepositsDB.Columns.PROJECTED_HEADER_HASH] ?? null,
    [DepositsDB.Columns.STATUS]:
      overrides[DepositsDB.Columns.STATUS] ?? DepositsDB.Status.Awaiting,
  };
};

const makeDepositSubmissionAttempt = ({
  txHash = databaseTxHash("deposit-submission.default"),
  eventId = databaseOutputReferenceId("deposit-submission.default"),
}: {
  readonly txHash?: Buffer;
  readonly eventId?: Buffer;
} = {}): DepositSubmissionAttemptsDB.InsertSubmittedInput => ({
  [DepositSubmissionAttemptsDB.Columns.TX_HASH]: txHash,
  [DepositSubmissionAttemptsDB.Columns.DEPOSIT_EVENT_ID]: eventId,
  [DepositSubmissionAttemptsDB.Columns.EXPECTED_DEPOSIT_OUT_REF]: `${txHash.toString(
    "hex",
  )}#0`,
  [DepositSubmissionAttemptsDB.Columns.EXPECTED_L2_ADDRESS]: address1,
  [DepositSubmissionAttemptsDB.Columns.EXPECTED_LOVELACE]: "1000000",
  [DepositSubmissionAttemptsDB.Columns.EXPECTED_ASSETS]: {
    lovelace: "1000000",
  },
  [DepositSubmissionAttemptsDB.Columns.METADATA]: {
    depositAddress: address1,
    depositEventId: eventId.toString("hex"),
    depositAssetName: "00".repeat(32),
    depositAuthUnit: `${"11".repeat(28)}${"00".repeat(32)}`,
    nonceInput: {
      txHash: databaseTxHash("deposit-submission.nonce").toString("hex"),
      outputIndex: 0,
    },
    validTo: 1_800_000_000_000,
    inclusionTime: 1_800_000_060_000,
  },
  [DepositSubmissionAttemptsDB.Columns.FUNDING_OUT_REFS]: [
    `${databaseTxHash("deposit-submission.funding").toString("hex")}#0`,
  ],
});
