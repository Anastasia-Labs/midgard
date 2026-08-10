import { spawn, spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import fs from "node:fs";
import path from "node:path";

import {
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS_V1 } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { RejectCodes } from "@al-ft/midgard-validation/types";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { SqlClient } from "@effect/sql";
import type { PgClient } from "@effect/sql-pg/PgClient";
import { it } from "@effect/vitest";
import { CML, Data as LucidData, toHex } from "@lucid-evolution/lucid";
import {
  Deferred,
  Duration,
  Effect,
  Fiber,
  Option,
  Ref,
  Schedule,
  TestClock,
} from "effect";
import { beforeAll, describe, expect, it as vitestIt } from "vitest";

import {
  DepositStatusCommandError,
  resolveDepositStatusProgram,
} from "../src/commands/deposit-status.js";
import { buildSubmitRouter } from "../src/commands/listen-router.js";
import { reconcileTxCommittedProgram } from "../src/commands/reconcile.js";
import {
  // Address history
  AddressHistoryDB,
  // Block
  BlocksDB,
  CommitBuildCalibrationDB,
  // Utils
  CommonUtils,
  ConfirmedLedgerDB,
  DaPayloadAnnouncementsDB,
  DaPayloadPublicationsDB,
  DaPayloadsDB,
  DepositsDB,
  DepositSubmissionAttemptsDB,
  ForcedTransactionsDB,
  ForeignTipReconciliationsDB,
  // Tx
  ImmutableDB,
  // Ledger
  LedgerUtils,
  MempoolDB,
  MempoolLedgerDB,
  MempoolTxDeltasDB,
  MpfEngineStateDB,
  MutationJobsDB,
  PendingBlockFinalizationsDB,
  ProcessedMempoolDB,
  StateQueueMutationLeasesDB,
  TxAdmissionsDB,
  TxRejectionsDB,
  TxUtils,
  WithdrawalsDB,
} from "../src/database/index.js";
import * as MigrationRunner from "../src/database/migrations/runner.js";
import { DatabaseError } from "../src/database/utils/common.js";
import { superviseHostProcess } from "../src/e2e/service-supervisor.js";
import {
  admissionBacklogGaugeFiber,
  commitAdmissionBacklogSlot,
  noteLocalAdmit,
  readAdmissionBacklogGauge,
  refreshAdmissionBacklogGauge,
  releaseAdmissionBacklogSlot,
  reserveAdmissionBacklogSlot,
} from "../src/fibers/admission-backlog-gauge.js";
import { projectDepositsToMempoolLedger } from "../src/fibers/project-deposits-to-mempool-ledger.js";
import {
  requestTxQueueProcessorWakeup,
  withAdmissionLeaseRecovery,
} from "../src/fibers/tx-queue-processor.js";
import { AdmissionWriter } from "../src/services/admission-writer.js";
import { NodeConfig } from "../src/services/config.js";
import { AdmissionSql, BatchSql } from "../src/services/database.js";
import { Globals } from "../src/services/globals.js";
import { Lucid } from "../src/services/lucid.js";
import {
  makeMempoolLedgerCacheService,
  MempoolLedgerCache,
} from "../src/services/mempool-ledger-cache.js";
import {
  ValidationPool,
  type ValidationPoolService,
  ValidationWorkerError,
} from "../src/services/validation-pool.js";
import { makeWriteBehind, WriteBehind } from "../src/services/write-behind.js";
import {
  applyConfirmedLedgerDelta,
  decodeConfirmedLedgerDelta,
  materializeConfirmedLedgerSnapshot,
} from "../src/transactions/state-queue/confirmed-ledger-snapshot.js";
import { breakDownTx, ProcessedTx } from "../src/utils.js";
import { revalidateAndPersistSpeculativeCandidateSources } from "../src/workers/commit-block-header.js";
import { buildDaPayloadInsert } from "../src/workers/commit-block-header/da-payload.js";
import {
  resolveDepositsRoot,
  resolveWithdrawalsRoot,
} from "../src/workers/commit-block-header/event-roots.js";
import { resolvePendingJournalLedgerState } from "../src/workers/commit-block-header/pending-journal.js";
import { selectCommitTxCandidates } from "../src/workers/utils/commit-block-planner.js";
import {
  computeLedgerMpfRootFromLedgerEntries,
  ledgerPayloadAggregateFromEntries,
  resolveIncludedDepositEntriesForWindow,
  resolveTxDeltaForCommit,
} from "../src/workers/utils/mpf.js";
import { makeCardanoSignedMapOutputTxBytes } from "./helpers/cardano-native-fixtures.js";
import { makeMidgardTxOutput } from "./midgard-output-helpers.js";
import {
  deterministicFixtureBytes,
  deterministicFixtureOutputReferenceId,
  deterministicFixtureTxHash,
  provideDatabaseLayers,
} from "./utils.js";

const flushAll = Effect.gen(function* () {
  yield* Effect.all(
    [
      MempoolLedgerDB.clear,
      ConfirmedLedgerDB.clear,
      BlocksDB.clear,
      ImmutableDB.clear,
      MempoolDB.clear,
      AddressHistoryDB.clear,
      ProcessedMempoolDB.clear,
      DepositsDB.clear,
      ForcedTransactionsDB.clear,
      ForeignTipReconciliationsDB.clear,
      DepositSubmissionAttemptsDB.clear,
      PendingBlockFinalizationsDB.clear,
      DaPayloadsDB.clear,
      CommonUtils.clearTable(TxAdmissionsDB.tableName),
      TxRejectionsDB.clear,
      CommonUtils.clearTable(MutationJobsDB.tableName),
      CommonUtils.clearTable(StateQueueMutationLeasesDB.tableName),
    ],
    { discard: true },
  );
});

const isolatedDb = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  provideDatabaseLayers(
    Effect.gen(function* () {
      yield* flushAll;
      return yield* effect;
    }),
  );

type SubmitHttpResult = {
  readonly status: number;
  readonly body: Record<string, unknown>;
};

type TxQueueWakeRequirements =
  | BatchSql
  | NodeConfig
  | Globals
  | Lucid
  | WriteBehind
  | ValidationPool
  | MempoolLedgerCache;

const submitThroughRouter = <R>(
  txCanonicalCbor: Buffer,
  wakeTxQueueProcessor: Effect.Effect<void, never, R>,
): Effect.Effect<
  SubmitHttpResult,
  unknown,
  R | SqlClient.SqlClient | NodeConfig | Globals | AdmissionWriter
> =>
  Effect.gen(function* () {
    const response = yield* buildSubmitRouter(wakeTxQueueProcessor).pipe(
      Effect.provideService(
        HttpServerRequest.HttpServerRequest,
        HttpServerRequest.fromWeb(
          new Request("http://midgard.test/submit", {
            method: "POST",
            headers: { "content-type": "application/cbor" },
            body: new Uint8Array(txCanonicalCbor),
          }),
        ),
      ),
    );
    const webResponse = HttpServerResponse.toWeb(response);
    const body = yield* Effect.tryPromise({
      try: () => webResponse.json() as Promise<Record<string, unknown>>,
      catch: (cause) => cause,
    });
    return { status: webResponse.status, body };
  });

const makeNativeSubmitTx = (): {
  readonly txId: Buffer;
  readonly txIdHex: string;
  readonly txCanonicalCbor: Buffer;
} => {
  const txCanonicalCbor = cardanoTxBytesToMidgardNativeTxCanonicalCbor(
    makeCardanoSignedMapOutputTxBytes(),
  );
  const txId = computeMidgardNativeTxId(
    decodeMidgardNativeTxFullFromCanonicalCbor(txCanonicalCbor),
  );
  return { txId, txIdHex: txId.toString("hex"), txCanonicalCbor };
};

const expectSubmitBody = (
  result: SubmitHttpResult,
  expected: {
    readonly status: 200 | 202;
    readonly txIdHex: string;
    readonly duplicate: boolean;
  },
): void => {
  expect(result.status).toBe(expected.status);
  expect(result.body).toEqual({
    txId: expected.txIdHex,
    status: TxAdmissionsDB.Status.Queued,
    firstSeenAt: expect.any(String),
    lastSeenAt: expect.any(String),
    duplicate: expected.duplicate,
  });
};

const retrieveAllMempool = MempoolDB.retrievePage({ limit: 100_000 }).pipe(
  Effect.map((page) => page.entries),
);

const databaseFixtureBytes = (label: string, length: number): Buffer =>
  deterministicFixtureBytes(`database:${label}`, length);

const databaseTxHash = (label: string): Buffer =>
  deterministicFixtureTxHash(`database:${label}`);

const databaseOutputReferenceId = (
  label: string,
  outputIndex: number | bigint = 0n,
): Buffer =>
  deterministicFixtureOutputReferenceId(`database:${label}`, outputIndex);

const collectChildProcess = (child: ReturnType<typeof spawn>) => {
  let stdout = "";
  let stderr = "";
  child.stdout?.on("data", (chunk: Buffer) => {
    stdout += chunk.toString("utf8");
  });
  child.stderr?.on("data", (chunk: Buffer) => {
    stderr += chunk.toString("utf8");
  });
  return new Promise<{
    readonly code: number | null;
    readonly signal: NodeJS.Signals | null;
    readonly stdout: string;
    readonly stderr: string;
  }>((resolve, reject) => {
    child.once("error", reject);
    child.once("exit", (code, signal) =>
      resolve({ code, signal, stdout, stderr }),
    );
  });
};

const bundleChildProcessHelper = (relativeSourcePath: string): string => {
  const cwd = path.resolve(__dirname, "..");
  const sourcePath = path.resolve(__dirname, relativeSourcePath);
  const outputDirectory = path.resolve(cwd, ".probe-dist");
  fs.mkdirSync(outputDirectory, { recursive: true });
  const outputPath = path.resolve(
    outputDirectory,
    `${path.basename(relativeSourcePath, ".ts")}-${process.pid.toString()}.mjs`,
  );
  const esbuild = path.resolve(cwd, "node_modules/.bin/esbuild");
  const result = spawnSync(
    esbuild,
    [
      sourcePath,
      "--bundle",
      "--platform=node",
      "--format=esm",
      "--packages=external",
      "--alias:@=./src",
      "--loader:.sql=text",
      `--outfile=${outputPath}`,
    ],
    { cwd, encoding: "utf8" },
  );
  if (result.status !== 0) {
    const diagnostic =
      result.error?.message ??
      (result.stderr?.trim() || undefined) ??
      `status=${String(result.status)}, signal=${String(result.signal)}`;
    throw new Error(
      `Failed to bundle child helper ${relativeSourcePath}: ${diagnostic}`,
    );
  }
  return outputPath;
};

const databaseChildProcessEnv = (): NodeJS.ProcessEnv => {
  const env = { ...process.env };
  const required = [
    "POSTGRES_HOST",
    "POSTGRES_PORT",
    "POSTGRES_USER",
    "POSTGRES_PASSWORD",
    "POSTGRES_DB",
  ] as const;
  for (const key of required) {
    const value = process.env[key];
    if (value === undefined || value === "") {
      throw new Error(`Missing explicit child database setting: ${key}`);
    }
    env[key] = value;
  }
  return env;
};

const daPayloadInsertFixture = (label: string): DaPayloadsDB.InsertInput => {
  const headerHash = databaseFixtureBytes(`${label}-header`, 28);
  const payload = databaseFixtureBytes(`${label}-payload`, 96);
  return {
    [DaPayloadsDB.Columns.HEADER_HASH]: headerHash,
    [DaPayloadsDB.Columns.VERSION]: Number(SDK.DA_PAYLOAD_V2_VERSION),
    [DaPayloadsDB.Columns.PAYLOAD_CBOR]: payload,
    [DaPayloadsDB.Columns.PAYLOAD_SHA256]: createHash("sha256")
      .update(payload)
      .digest(),
    [DaPayloadsDB.Columns.UTXOS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.FORCED_TRANSACTIONS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.TRANSACTIONS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.DEPOSITS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.WITHDRAWALS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.WITHDRAWAL_COUNT]: 0n,
    [DaPayloadsDB.Columns.FORCED_TRANSACTION_COUNT]: 0n,
    [DaPayloadsDB.Columns.L2_TRANSACTION_COUNT]: 0n,
    [DaPayloadsDB.Columns.DEPOSIT_COUNT]: 0n,
    [DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]: 0n,
    [DaPayloadsDB.Columns.TRANSITION_STEP_COUNT]: 0n,
    [DaPayloadsDB.Columns.BLOCK_START_TIME]: new Date(),
    [DaPayloadsDB.Columns.BLOCK_END_TIME]: new Date(),
  };
};

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
    isolatedDb(
      Effect.gen(function* () {
        // Smoke select to ensure connection works
        const sql = yield* SqlClient.SqlClient;
        const now = yield* sql<Date>`SELECT NOW()`;
        expect(now.length).toBeGreaterThan(0);
      }),
    ),
  );

  it.effect(
    "requires every active mempool row to carry canonical transaction bytes",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const columns = yield* sql<{
            readonly is_nullable: "YES" | "NO";
          }>`SELECT is_nullable
            FROM information_schema.columns
            WHERE table_schema = 'public'
              AND table_name = 'mempool'
              AND column_name = 'tx'`;
          expect(columns).toHaveLength(1);
          expect(columns[0]?.is_nullable).toBe("NO");
        }),
      ),
  );

  it.effect("drops the superseded admission-payload hash lookup index", () =>
    isolatedDb(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const indexes = yield* sql<{ readonly index_name: string }>`
            SELECT indexname AS index_name
            FROM pg_indexes
            WHERE schemaname = 'public'
              AND indexname = 'idx_tx_admission_payloads_tx_id_hash'`;
        expect(indexes).toEqual([]);
      }),
    ),
  );

  it.effect(
    "uses a dedicated active-lease index for owner and tx-id point lookups",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const indexes = yield* sql<{
            readonly index_name: string;
            readonly index_definition: string;
          }>`SELECT
              indexname AS index_name,
              indexdef AS index_definition
            FROM pg_indexes
            WHERE schemaname = 'public'
              AND tablename = 'tx_admissions'
              AND indexname IN (
                'idx_tx_admissions_active_lease',
                'idx_tx_admissions_lease'
              )
            ORDER BY indexname`;
          expect(indexes).toHaveLength(2);
          expect(indexes[0]?.index_name).toBe("idx_tx_admissions_active_lease");
          expect(indexes[0]?.index_definition).toContain(
            "(lease_owner, tx_id)",
          );
          expect(indexes[0]?.index_definition).toContain(
            "WHERE (status = 'validating'::tx_admission_status)",
          );
          expect(indexes[1]?.index_name).toBe("idx_tx_admissions_lease");
          expect(indexes[1]?.index_definition).toContain("(lease_expires_at)");
        }),
      ),
  );

  it.effect("keeps only the rebuildable transaction-delta cache unlogged", () =>
    isolatedDb(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const persistence = yield* sql<{
          readonly relation_name: string;
          readonly persistence: "p" | "u";
        }>`SELECT relname AS relation_name, relpersistence AS persistence
          FROM pg_class
          WHERE relname IN (
            'mempool_tx_deltas',
            'tx_admissions',
            'tx_admission_payloads'
          )
          ORDER BY relname`;
        expect(persistence).toEqual([
          { relation_name: "mempool_tx_deltas", persistence: "u" },
          { relation_name: "tx_admission_payloads", persistence: "p" },
          { relation_name: "tx_admissions", persistence: "p" },
        ]);
      }),
    ),
  );

  it.effect("keeps admission and batch traffic on distinct labeled pools", () =>
    isolatedDb(
      Effect.gen(function* () {
        const batchSql = yield* BatchSql;
        const admissionSql = yield* AdmissionSql;
        const [batch, admission] = yield* Effect.all(
          [
            batchSql<{
              readonly application_name: string;
              readonly backend_pid: number;
            }>`SELECT current_setting('application_name') AS application_name, pg_backend_pid() AS backend_pid`,
            admissionSql<{
              readonly application_name: string;
              readonly backend_pid: number;
            }>`SELECT current_setting('application_name') AS application_name, pg_backend_pid() AS backend_pid`,
          ],
          { concurrency: "unbounded" },
        );

        expect(batch[0]?.application_name).toBe("midgard-node-batch");
        expect(admission[0]?.application_name).toBe("midgard-node-admission");
        expect(batch[0]?.backend_pid).not.toBe(admission[0]?.backend_pid);
      }),
    ),
  );
});

describe("TxAdmissionsDB", () => {
  it.effect(
    "preserves exact submit-router HTTP parity for new, duplicate, conflict, and backlog-full requests",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const admissionSql = yield* AdmissionSql;
          const baseConfig = yield* NodeConfig;
          let testConfig = {
            ...baseConfig,
            MAX_DURABLE_ADMISSION_BACKLOG: 2,
          };
          const submit = (txCanonicalCbor: Buffer) =>
            submitThroughRouter(txCanonicalCbor, Effect.void).pipe(
              Effect.provideService(SqlClient.SqlClient, admissionSql),
              Effect.provideService(NodeConfig, testConfig),
            );

          const firstTx = makeNativeSubmitTx();
          const first = yield* submit(firstTx.txCanonicalCbor);
          expectSubmitBody(first, {
            status: 202,
            txIdHex: firstTx.txIdHex,
            duplicate: false,
          });
          const duplicate = yield* submit(firstTx.txCanonicalCbor);
          expectSubmitBody(duplicate, {
            status: 200,
            txIdHex: firstTx.txIdHex,
            duplicate: true,
          });
          expect(duplicate.body.firstSeenAt).toBe(first.body.firstSeenAt);
          const stored = yield* TxAdmissionsDB.getByTxId(firstTx.txId);
          expect(stored?.request_count).toBe(2n);

          const conflictTx = makeNativeSubmitTx();
          yield* TxAdmissionsDB.admit({
            txId: conflictTx.txId,
            txCanonicalCbor: Buffer.concat([
              conflictTx.txCanonicalCbor,
              Buffer.from([0]),
            ]),
            submitSource: "native",
            currentBacklog: 0n,
            maxBacklog: 10,
          });
          const conflict = yield* submit(conflictTx.txCanonicalCbor);
          expect(conflict.status).toBe(409);
          expect(conflict.body).toEqual({
            error: "E_TX_ID_BYTES_CONFLICT",
            message: expect.stringContaining(conflictTx.txIdHex),
            txId: conflictTx.txIdHex,
          });

          const globals = yield* Globals;
          yield* Ref.set(globals.ADMISSION_BACKLOG_GAUGE, {
            ADMISSION_BACKLOG_BASE: 2n,
            ADMISSION_BACKLOG_LOCAL_DELTA: 0n,
            ADMISSION_BACKLOG_IN_FLIGHT: 0n,
            ADMISSION_BACKLOG_REFRESHED_AT: Date.now(),
          });
          const duplicateWhileFull = yield* submit(firstTx.txCanonicalCbor);
          expectSubmitBody(duplicateWhileFull, {
            status: 200,
            txIdHex: firstTx.txIdHex,
            duplicate: true,
          });
          const newWhileFullTx = makeNativeSubmitTx();
          const newWhileFull = yield* submit(newWhileFullTx.txCanonicalCbor);
          expect(newWhileFull).toEqual({
            status: 503,
            body: {
              error: "Durable submission admission backlog is full",
              backlog: "2",
              maxBacklog: "2",
            },
          });

          const identicalTx = makeNativeSubmitTx();
          testConfig = {
            ...testConfig,
            MAX_DURABLE_ADMISSION_BACKLOG: 100,
          };
          yield* Ref.set(globals.ADMISSION_BACKLOG_GAUGE, {
            ADMISSION_BACKLOG_BASE: 0n,
            ADMISSION_BACKLOG_LOCAL_DELTA: 0n,
            ADMISSION_BACKLOG_IN_FLIGHT: 0n,
            ADMISSION_BACKLOG_REFRESHED_AT: Date.now(),
          });
          const identical = yield* Effect.all(
            Array.from({ length: 12 }, () =>
              submit(identicalTx.txCanonicalCbor),
            ),
            { concurrency: "unbounded" },
          );
          expect(
            identical.filter((result) => result.status === 202),
          ).toHaveLength(1);
          expect(
            identical.filter((result) => result.status === 200),
          ).toHaveLength(11);
          expect(
            identical.every(
              (result) =>
                result.body.txId === identicalTx.txIdHex &&
                result.body.status === TxAdmissionsDB.Status.Queued,
            ),
          ).toBe(true);
          expect(
            (yield* TxAdmissionsDB.getByTxId(identicalTx.txId))?.request_count,
          ).toBe(12n);
        }).pipe(Effect.provide(Globals.Default)),
      ),
  );

  it.effect(
    "holds a stale refresh, bounds parallel distinct HTTP admits, then recovers after one refresh",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const admissionSql = yield* AdmissionSql;
          const baseConfig = yield* NodeConfig;
          const testConfig = {
            ...baseConfig,
            MAX_DURABLE_ADMISSION_BACKLOG: 5,
            ADMISSION_BACKLOG_REFRESH_MS: 10,
          };
          const submit = (txCanonicalCbor: Buffer) =>
            submitThroughRouter(txCanonicalCbor, Effect.void).pipe(
              Effect.provideService(SqlClient.SqlClient, admissionSql),
              Effect.provideService(NodeConfig, testConfig),
            );
          const maxBacklog = 5;
          for (let index = 0; index < 3; index += 1) {
            yield* TxAdmissionsDB.admit({
              txId: databaseTxHash(`admission.http-frozen-seed-${index}`),
              txCanonicalCbor: databaseFixtureBytes(
                `admission.http-frozen-seed-${index}`,
                64,
              ),
              submitSource: "native",
              currentBacklog: BigInt(index),
              maxBacklog,
            });
          }
          yield* refreshAdmissionBacklogGauge;
          expect(yield* readAdmissionBacklogGauge).toBe(3n);

          const unfreeze = yield* Deferred.make<void>();
          const refreshFiber = yield* Effect.fork(
            admissionBacklogGaugeFiber(
              Schedule.spaced(Duration.millis(10)),
              Deferred.await(unfreeze),
            ).pipe(Effect.provideService(NodeConfig, testConfig)),
          );
          yield* Effect.gen(function* () {
            const attempts = Array.from({ length: 10 }, () =>
              makeNativeSubmitTx(),
            );
            const results = yield* Effect.all(
              attempts.map((tx) => submit(tx.txCanonicalCbor)),
              { concurrency: "unbounded" },
            ).pipe(
              Effect.timeoutFail({
                duration: Duration.seconds(10),
                onTimeout: () =>
                  new Error(
                    "Parallel stale-gauge HTTP submissions exceeded 10 seconds",
                  ),
              }),
            );
            expect(
              results.filter((result) => result.status === 202),
            ).toHaveLength(2);
            const rejected = results.filter((result) => result.status === 503);
            expect(rejected).toHaveLength(8);
            expect(
              rejected.every(
                (result) =>
                  result.body.backlog === "5" && result.body.maxBacklog === "5",
              ),
            ).toBe(true);
            expect(yield* TxAdmissionsDB.countBacklog).toBe(5n);

            const sql = yield* SqlClient.SqlClient;
            yield* sql`UPDATE ${sql(TxAdmissionsDB.tableName)}
              SET status = 'accepted', terminal_at = NOW(), updated_at = NOW()
              WHERE status IN ('queued', 'validating')`;
            yield* Deferred.succeed(unfreeze, undefined);
            let refreshed = false;
            for (let attempt = 0; attempt < 50; attempt += 1) {
              const currentGauge = yield* readAdmissionBacklogGauge;
              if (currentGauge === 0n) {
                refreshed = true;
                break;
              }
              yield* TestClock.adjust(Duration.millis(10));
            }
            expect(refreshed).toBe(true);
            const recoveryTx = makeNativeSubmitTx();
            expect((yield* submit(recoveryTx.txCanonicalCbor)).status).toBe(
              202,
            );
          }).pipe(
            Effect.ensuring(
              Deferred.succeed(unfreeze, undefined).pipe(
                Effect.ignore,
                Effect.zipRight(Fiber.interruptFork(refreshFiber)),
              ),
            ),
          );
        }).pipe(Effect.provide(Globals.Default)),
      ),
  );

  it.effect(
    "keeps submit latency isolated while the batch pool is held and labels the resumed drain",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const batchSql = yield* BatchSql;
          const admissionSql = yield* AdmissionSql;
          const nodeConfig = yield* NodeConfig;
          const globals = yield* Globals;
          let validationRuns = 0;
          const cache = yield* makeMempoolLedgerCacheService(
            globals,
            MempoolLedgerDB.retrieveSpendable.pipe(
              Effect.provideService(SqlClient.SqlClient, batchSql),
            ),
          );
          const validationPool: ValidationPoolService = {
            poolSize: 1,
            ready: Effect.void,
            stats: Effect.succeed({
              busyWorkers: 0,
              queueDepth: 0,
              oldestInFlightAgeMs: 0,
              liveWorkers: 1,
              restartingWorkers: 0,
            }),
            runPhaseAChunk: (txs) =>
              Effect.sync(() => {
                validationRuns += 1;
              }).pipe(
                Effect.zipRight(Effect.sleep(Duration.millis(250))),
                Effect.as({
                  accepted: [],
                  rejected: txs.map((tx) => ({
                    txId: tx.txId,
                    code: RejectCodes.InvalidSignature,
                    detail: "phase1 pool-isolation hold",
                  })),
                }),
              ),
            evaluateScript: () =>
              Effect.fail(
                new ValidationWorkerError({
                  message: "unexpected script evaluation",
                }),
              ),
          };
          const lucid = {
            api: { currentSlot: () => 0 },
          } as unknown as Lucid;
          const submit = (
            txCanonicalCbor: Buffer,
            wake: Effect.Effect<void, never, TxQueueWakeRequirements>,
          ) =>
            submitThroughRouter(txCanonicalCbor, wake).pipe(
              Effect.provideService(SqlClient.SqlClient, admissionSql),
              Effect.provideService(ValidationPool, validationPool),
              Effect.provideService(MempoolLedgerCache, cache),
              Effect.provideService(Lucid, lucid),
            );
          const percentile99 = (samples: readonly number[]): number =>
            [...samples].sort((left, right) => left - right)[
              Math.max(0, Math.ceil(samples.length * 0.99) - 1)
            ] ?? 0;
          const measure = (
            count: number,
            wake: Effect.Effect<void, never, TxQueueWakeRequirements>,
          ) =>
            Effect.all(
              Array.from({ length: count }, () =>
                Effect.gen(function* () {
                  const tx = makeNativeSubmitTx();
                  const startedAt = performance.now();
                  const response = yield* submit(tx.txCanonicalCbor, wake);
                  expect(response.status).toBe(202);
                  return performance.now() - startedAt;
                }),
              ),
              { concurrency: "unbounded" },
            );

          const baselineP99 = percentile99(yield* measure(24, Effect.void));
          const releases = yield* Effect.forEach(
            Array.from({ length: nodeConfig.POSTGRES_BATCH_POOL_SIZE }),
            () => Deferred.make<void>(),
          );
          const acquired = yield* Effect.forEach(releases, () =>
            Deferred.make<void>(),
          );
          const holders = yield* Effect.forEach(releases, (release, index) =>
            Effect.fork(
              batchSql.withTransaction(
                Effect.gen(function* () {
                  yield* batchSql`SELECT 1`;
                  yield* Deferred.succeed(acquired[index]!, undefined);
                  yield* Deferred.await(release);
                }),
              ),
            ),
          );
          const releaseHolders = Effect.forEach(releases, (release) =>
            Deferred.succeed(release, undefined),
          ).pipe(
            Effect.zipRight(Effect.forEach(holders, Fiber.interrupt)),
            Effect.asVoid,
          );
          yield* Effect.gen(function* () {
            yield* Effect.forEach(acquired, Deferred.await);

            const saturatedP99 = percentile99(
              yield* measure(24, requestTxQueueProcessorWakeup),
            );
            expect(saturatedP99).toBeLessThanOrEqual(1_000);
            expect(saturatedP99).toBeLessThanOrEqual(baselineP99 * 1.2);

            const activity = yield* admissionSql<{
              readonly application_name: string;
              readonly state: string;
              readonly count: number;
            }>`SELECT application_name, state, COUNT(*)::int AS count
            FROM pg_stat_activity
            WHERE datname = current_database()
              AND application_name IN ('midgard-node-admission', 'midgard-node-batch')
            GROUP BY application_name, state`;
            expect(
              activity.some(
                (row) =>
                  row.application_name === "midgard-node-admission" &&
                  row.count >= 1,
              ),
            ).toBe(true);
            expect(
              activity
                .filter((row) => row.application_name === "midgard-node-batch")
                .reduce((sum, row) => sum + row.count, 0),
            ).toBe(nodeConfig.POSTGRES_BATCH_POOL_SIZE);
            yield* TestClock.adjust(Duration.millis(300));
            expect(validationRuns).toBe(0);
            expect(
              yield* Ref.get(globals.TX_QUEUE_PROCESSOR_ACTIVE),
            ).toBeGreaterThan(0);

            yield* Deferred.succeed(releases[0]!, undefined);
            let drainObserved = false;
            for (let attempt = 0; attempt < 100; attempt += 1) {
              if (validationRuns > 0) {
                drainObserved = true;
                break;
              }
              yield* TestClock.adjust(Duration.millis(5));
            }
            expect(drainObserved).toBe(true);
            const batchBackends = yield* admissionSql<{
              readonly application_name: string;
              readonly count: number;
            }>`SELECT application_name, COUNT(*)::int AS count
            FROM pg_stat_activity
            WHERE datname = current_database()
              AND application_name = 'midgard-node-batch'
            GROUP BY application_name`;
            expect(batchBackends[0]?.count).toBe(
              nodeConfig.POSTGRES_BATCH_POOL_SIZE,
            );

            yield* Effect.forEach(releases.slice(1), (release) =>
              Deferred.succeed(release, undefined),
            );
            yield* Effect.forEach(holders, Fiber.join);
            for (let attempt = 0; attempt < 100; attempt += 1) {
              if ((yield* Ref.get(globals.TX_QUEUE_PROCESSOR_ACTIVE)) === 0) {
                break;
              }
              yield* TestClock.adjust(Duration.millis(5));
            }
            expect(yield* Ref.get(globals.TX_QUEUE_PROCESSOR_ACTIVE)).toBe(0);
          }).pipe(Effect.ensuring(releaseHolders));
        }).pipe(Effect.provide(Globals.Default)),
      ),
  );

  it.effect("round-trips ordered bytea arrays without changing bytes", () =>
    isolatedDb(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const pg = sql as PgClient;
        const duplicate = Buffer.from([
          0x00, 0xff, 0x80, 0x7f, 0x5c, 0x27, 0x00,
        ]);
        const expected = [
          Buffer.alloc(32, 0x00),
          duplicate,
          Buffer.alloc(32, 0xff),
          Buffer.from(duplicate),
        ];
        const rows = yield* sql<{
          readonly value: Buffer;
          readonly ordinal: number;
        }>`SELECT value, ordinality::int AS ordinal
          FROM unnest(${pg.array(
            expected.map((value) => `\\x${value.toString("hex")}`),
          )}::bytea[])
            WITH ORDINALITY AS input_bytes(value, ordinality)
          ORDER BY ordinality`;

        expect(rows.map((row) => row.ordinal)).toStrictEqual([1, 2, 3, 4]);
        expect(rows.map((row) => row.value)).toStrictEqual(expected);
      }),
    ),
  );

  it.effect(
    "accepts a 2048-row compact batch through bytea-array predicates",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const rowCount = 2_048;
          const sql = yield* SqlClient.SqlClient;
          const txs = Array.from({ length: rowCount }, (_, index) => {
            const label = `admission.bulk-array-${index.toString()}`;
            const txId = databaseTxHash(label);
            const txCanonicalCbor = databaseFixtureBytes(`${label}.cbor`, 64);
            const source = {
              [LedgerUtils.Columns.TX_ID]: databaseTxHash(`${label}.source`),
              [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
                `${label}.source`,
              ),
              [LedgerUtils.Columns.OUTPUT]: databaseFixtureBytes(
                `${label}.source-output`,
                80,
              ),
              [LedgerUtils.Columns.ADDRESS]: address1,
            } satisfies LedgerUtils.Entry;
            const produced = {
              [LedgerUtils.Columns.TX_ID]: txId,
              [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
                `${label}.produced`,
              ),
              [LedgerUtils.Columns.OUTPUT]: databaseFixtureBytes(
                `${label}.produced-output`,
                80,
              ),
              [LedgerUtils.Columns.ADDRESS]: address1,
            } satisfies LedgerUtils.Entry;
            return { txId, txCanonicalCbor, source, produced };
          });
          yield* sql`INSERT INTO ${sql(TxAdmissionsDB.tableName)} ${sql.insert(
            txs.map(({ txId }) => ({
              tx_id: txId,
              status: TxAdmissionsDB.Status.Queued,
              submit_source: "native",
            })),
          )}`;
          yield* sql`INSERT INTO ${sql(
            TxAdmissionsDB.payloadTableName,
          )} ${sql.insert(
            txs.map(({ txId, txCanonicalCbor }) => ({
              tx_id: txId,
              tx_canonical_cbor: txCanonicalCbor,
              tx_canonical_cbor_sha256: createHash("sha256")
                .update(txCanonicalCbor)
                .digest(),
            })),
          )}`;
          yield* MempoolLedgerDB.insert(txs.map(({ source }) => source));

          const leaseOwner = "database-test:bulk-array-accept";
          const claimed = yield* TxAdmissionsDB.claimBatch({
            limit: rowCount,
            leaseOwner,
            leaseDurationMs: 30_000,
          });
          expect(claimed).toHaveLength(rowCount);
          yield* TxAdmissionsDB.markAccepted({
            rows: claimed,
            leaseOwner,
            processedTxs: txs.map(
              ({ txId, txCanonicalCbor, source, produced }) => ({
                txId,
                txCbor: txCanonicalCbor,
                spent: [source[LedgerUtils.Columns.OUTREF]],
                produced: [produced],
              }),
            ),
          });

          const accepted = yield* sql<{
            readonly count: number;
          }>`SELECT COUNT(*)::int AS count
            FROM ${sql(TxAdmissionsDB.tableName)}
            WHERE status = ${TxAdmissionsDB.Status.Accepted}`;
          expect(accepted[0]?.count).toBe(rowCount);
          expect(yield* MempoolDB.retrieveTxCount).toBe(BigInt(rowCount));
          const inlinePayloads = yield* sql<{
            readonly tx_id: Buffer;
            readonly tx: Buffer;
          }>`SELECT tx_id, tx FROM mempool`;
          expect(
            new Map(
              inlinePayloads.map((entry) => [
                entry.tx_id.toString("hex"),
                entry.tx.toString("hex"),
              ]),
            ),
          ).toEqual(
            new Map(
              txs.map((entry) => [
                entry.txId.toString("hex"),
                entry.txCanonicalCbor.toString("hex"),
              ]),
            ),
          );
          const ledger = yield* MempoolLedgerDB.retrieveSpendable;
          expect(ledger).toHaveLength(rowCount);
          expect(
            new Set(ledger.map((entry) => entry.outref.toString("hex"))),
          ).toEqual(
            new Set(
              txs.map(({ produced }) =>
                produced[LedgerUtils.Columns.OUTREF].toString("hex"),
              ),
            ),
          );
          expect((yield* (yield* WriteBehind).depths).totalDepth).toBe(
            rowCount * 2,
          );
        }),
      ),
  );

  it.effect(
    "preserves binary produced columns and explicit or default timestamps",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const binary = (length: number, seed: number): Buffer => {
            const value = Buffer.from(
              Array.from({ length }, (_, index) => (seed + index * 131) & 0xff),
            );
            value[0] = 0x00;
            value[1] = 0xff;
            value[2] = 0x80;
            return value;
          };
          const txs = [0, 1].map((index) => {
            const txId = binary(32, 17 + index);
            const txCanonicalCbor = binary(64, 71 + index);
            const source = {
              [LedgerUtils.Columns.TX_ID]: binary(32, 101 + index),
              [LedgerUtils.Columns.OUTREF]: binary(36, 131 + index),
              [LedgerUtils.Columns.OUTPUT]: binary(80, 151 + index),
              [LedgerUtils.Columns.ADDRESS]: address1,
            } satisfies LedgerUtils.Entry;
            return { txId, txCanonicalCbor, source };
          });
          yield* Effect.all(
            txs.map(({ txId, txCanonicalCbor }) =>
              TxAdmissionsDB.admit({
                txId,
                txCanonicalCbor,
                submitSource: "native",
                currentBacklog: 0n,
                maxBacklog: 10,
              }),
            ),
            { concurrency: "unbounded", discard: true },
          );
          yield* MempoolLedgerDB.insert(txs.map(({ source }) => source));
          const leaseOwner = "database-test:produced-array-binary";
          const claimed = yield* TxAdmissionsDB.claimBatch({
            limit: txs.length,
            leaseOwner,
            leaseDurationMs: 30_000,
          });
          const explicitTimestamp = new Date("2026-07-10T11:22:33.456Z");
          const produced = [
            {
              [LedgerUtils.Columns.TX_ID]: txs[0]!.txId,
              [LedgerUtils.Columns.OUTREF]: binary(36, 191),
              [LedgerUtils.Columns.OUTPUT]: binary(96, 211),
              [LedgerUtils.Columns.ADDRESS]: address1,
              [LedgerUtils.Columns.TIMESTAMPTZ]: explicitTimestamp,
            },
            {
              [LedgerUtils.Columns.TX_ID]: txs[1]!.txId,
              [LedgerUtils.Columns.OUTREF]: binary(36, 231),
              [LedgerUtils.Columns.OUTPUT]: binary(96, 251),
              [LedgerUtils.Columns.ADDRESS]: address2,
            },
          ] satisfies readonly LedgerUtils.Entry[];
          const sql = yield* SqlClient.SqlClient;
          const before = yield* sql<{
            readonly now: Date;
          }>`SELECT clock_timestamp() AS now`;
          yield* TxAdmissionsDB.markAccepted({
            rows: claimed,
            leaseOwner,
            processedTxs: txs.map((tx, index) => ({
              txId: tx.txId,
              txCbor: tx.txCanonicalCbor,
              spent: [tx.source[LedgerUtils.Columns.OUTREF]],
              produced: [produced[index]!],
            })),
          });
          const after = yield* sql<{
            readonly now: Date;
          }>`SELECT clock_timestamp() AS now`;

          const persisted = yield* MempoolLedgerDB.retrieveByTxOutRefs(
            produced.map((entry) => entry[LedgerUtils.Columns.OUTREF]),
          );
          expect(persisted).toHaveLength(2);
          const byOutref = new Map(
            persisted.map((entry) => [
              entry[MempoolLedgerDB.Columns.OUTREF].toString("hex"),
              entry,
            ]),
          );
          for (const entry of produced) {
            const actual = byOutref.get(
              entry[LedgerUtils.Columns.OUTREF].toString("hex"),
            );
            expect(actual?.[MempoolLedgerDB.Columns.TX_ID]).toEqual(
              entry[LedgerUtils.Columns.TX_ID],
            );
            expect(actual?.[MempoolLedgerDB.Columns.OUTPUT]).toEqual(
              entry[LedgerUtils.Columns.OUTPUT],
            );
            expect(actual?.[MempoolLedgerDB.Columns.ADDRESS]).toBe(
              entry[LedgerUtils.Columns.ADDRESS],
            );
          }
          expect(
            byOutref
              .get(produced[0]![LedgerUtils.Columns.OUTREF].toString("hex"))
              ?.[MempoolLedgerDB.Columns.TIMESTAMPTZ].getTime(),
          ).toBe(explicitTimestamp.getTime());
          const defaultTimestamp = byOutref.get(
            produced[1]![LedgerUtils.Columns.OUTREF].toString("hex"),
          )?.[MempoolLedgerDB.Columns.TIMESTAMPTZ];
          expect(defaultTimestamp?.getTime()).toBeGreaterThanOrEqual(
            before[0]!.now.getTime(),
          );
          expect(defaultTimestamp?.getTime()).toBeLessThanOrEqual(
            after[0]!.now.getTime(),
          );
        }),
      ),
  );

  it.effect(
    "rolls back duplicate produced outrefs under strict uniqueness",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const inputs = [0, 1].map((index) => ({
            txId: databaseTxHash(
              `admission.produced-array-conflict-${index.toString()}`,
            ),
            txCanonicalCbor: databaseFixtureBytes(
              `admission.produced-array-conflict-${index.toString()}`,
              64,
            ),
            source: {
              ...ledgerEntry1,
              [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
                `admission.produced-array-conflict-source-${index.toString()}`,
              ),
            } satisfies LedgerUtils.Entry,
          }));
          yield* Effect.all(
            inputs.map(({ txId, txCanonicalCbor }) =>
              TxAdmissionsDB.admit({
                txId,
                txCanonicalCbor,
                submitSource: "native",
                currentBacklog: 0n,
                maxBacklog: 10,
              }),
            ),
            { concurrency: "unbounded", discard: true },
          );
          yield* MempoolLedgerDB.insert(inputs.map(({ source }) => source));
          const leaseOwner = "database-test:produced-array-conflict";
          const claimed = yield* TxAdmissionsDB.claimBatch({
            limit: inputs.length,
            leaseOwner,
            leaseDurationMs: 30_000,
          });
          const duplicateOutref = Buffer.concat([
            Buffer.from([0x00, 0xff, 0x80, 0x00]),
            databaseFixtureBytes("admission.produced-array-conflict", 32),
          ]);
          const result = yield* Effect.either(
            TxAdmissionsDB.markAccepted({
              rows: claimed,
              leaseOwner,
              processedTxs: inputs.map((input, index) => ({
                txId: input.txId,
                txCbor: input.txCanonicalCbor,
                spent: [input.source[LedgerUtils.Columns.OUTREF]],
                produced: [
                  {
                    [LedgerUtils.Columns.TX_ID]: input.txId,
                    [LedgerUtils.Columns.OUTREF]: duplicateOutref,
                    [LedgerUtils.Columns.OUTPUT]: databaseFixtureBytes(
                      `admission.produced-array-conflict-output-${index.toString()}`,
                      80,
                    ),
                    [LedgerUtils.Columns.ADDRESS]: address1,
                  },
                ],
              })),
            }),
          );

          expect(result._tag).toBe("Left");
          expect(yield* MempoolDB.retrieveTxCount).toBe(0n);
          const ledger = yield* MempoolLedgerDB.retrieveSpendable;
          expect(
            new Set(ledger.map((entry) => entry.outref.toString("hex"))),
          ).toEqual(
            new Set(
              inputs.map(({ source }) =>
                source[LedgerUtils.Columns.OUTREF].toString("hex"),
              ),
            ),
          );
          const admissions = yield* Effect.forEach(inputs, ({ txId }) =>
            TxAdmissionsDB.getByTxId(txId),
          );
          expect(
            admissions.every(
              (entry) => entry?.status === TxAdmissionsDB.Status.Validating,
            ),
          ).toBe(true);
          expect((yield* (yield* WriteBehind).depths).totalDepth).toBe(0);
        }),
      ),
  );

  it.effect(
    "rolls back the complete accept transaction on a mempool membership conflict",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const txId = databaseTxHash("admission.membership-conflict");
          const txCanonicalCbor = databaseFixtureBytes(
            "admission.membership-conflict-cbor",
            64,
          );
          const source = {
            ...ledgerEntry1,
            [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
              "admission.membership-conflict-source",
            ),
          } satisfies LedgerUtils.Entry;
          const produced = {
            ...ledgerEntry2,
            [LedgerUtils.Columns.TX_ID]: txId,
            [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
              "admission.membership-conflict-produced",
            ),
          } satisfies LedgerUtils.Entry;
          yield* TxAdmissionsDB.admit({
            txId,
            txCanonicalCbor,
            submitSource: "native",
            currentBacklog: 0n,
            maxBacklog: 10,
          });
          yield* MempoolLedgerDB.insert([source]);
          const leaseOwner = "database-test:membership-conflict";
          const claimed = yield* TxAdmissionsDB.claimBatch({
            limit: 1,
            leaseOwner,
            leaseDurationMs: 30_000,
          });
          const sql = yield* SqlClient.SqlClient;
          yield* sql`INSERT INTO mempool (tx_id, tx) VALUES (${txId}, ${txCanonicalCbor})`;

          const result = yield* Effect.either(
            TxAdmissionsDB.markAccepted({
              rows: claimed,
              leaseOwner,
              processedTxs: [
                {
                  txId,
                  txCbor: txCanonicalCbor,
                  spent: [source[LedgerUtils.Columns.OUTREF]],
                  produced: [produced],
                },
              ],
            }),
          );

          expect(result._tag).toBe("Left");
          expect((yield* TxAdmissionsDB.getByTxId(txId))?.status).toBe(
            TxAdmissionsDB.Status.Validating,
          );
          expect(yield* MempoolDB.retrieveTxCount).toBe(1n);
          const ledger = yield* MempoolLedgerDB.retrieveSpendable;
          expect(
            ledger.map((entry) => entry.outref.toString("hex")),
          ).toStrictEqual([source[LedgerUtils.Columns.OUTREF].toString("hex")]);
          expect((yield* (yield* WriteBehind).depths).totalDepth).toBe(0);
        }),
      ),
  );

  it.effect(
    "preserves new, duplicate, conflict, and backlog-full admission semantics",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const txId = databaseTxHash("admission.state-machine");
          const txCbor = databaseFixtureBytes("admission.state-machine", 64);
          const first = yield* TxAdmissionsDB.admit({
            txId,
            txCanonicalCbor: txCbor,
            submitSource: "native",
            currentBacklog: 0n,
            maxBacklog: 10,
          });
          expect(first.kind).toBe("new");
          expect(first.entry.request_count).toBe(1n);

          const duplicate = yield* TxAdmissionsDB.admit({
            txId,
            txCanonicalCbor: txCbor,
            submitSource: "native",
            currentBacklog: 10n,
            maxBacklog: 10,
          });
          expect(duplicate.kind).toBe("duplicate");
          expect(duplicate.entry.request_count).toBe(2n);
          expect(duplicate.entry.first_seen_at.getTime()).toBe(
            first.entry.first_seen_at.getTime(),
          );

          const conflict = yield* Effect.either(
            TxAdmissionsDB.admit({
              txId,
              txCanonicalCbor: databaseFixtureBytes(
                "admission.state-machine-conflict",
                64,
              ),
              submitSource: "native",
              currentBacklog: 0n,
              maxBacklog: 10,
            }),
          );
          expect(conflict._tag).toBe("Left");
          if (conflict._tag === "Left") {
            expect(conflict.left._tag).toBe("TxAdmissionConflictError");
          }

          const backlogFull = yield* Effect.either(
            TxAdmissionsDB.admit({
              txId: databaseTxHash("admission.backlog-full"),
              txCanonicalCbor: databaseFixtureBytes(
                "admission.backlog-full",
                64,
              ),
              submitSource: "native",
              currentBacklog: 10n,
              maxBacklog: 10,
            }),
          );
          expect(backlogFull._tag).toBe("Left");
          if (backlogFull._tag === "Left") {
            expect(backlogFull.left._tag).toBe("TxAdmissionBacklogFullError");
            if (backlogFull.left._tag === "TxAdmissionBacklogFullError") {
              expect(backlogFull.left.backlog).toBe(10n);
            }
          }
        }),
      ),
  );

  it.effect("arbitrates parallel identical admissions exactly once", () =>
    isolatedDb(
      Effect.gen(function* () {
        const txId = databaseTxHash("admission.concurrent");
        const txCanonicalCbor = databaseFixtureBytes(
          "admission.concurrent",
          64,
        );
        const results = yield* Effect.all(
          Array.from({ length: 12 }, () =>
            TxAdmissionsDB.admit({
              txId,
              txCanonicalCbor,
              submitSource: "native",
              currentBacklog: 0n,
              maxBacklog: 100,
            }),
          ),
          { concurrency: "unbounded" },
        );
        expect(results.filter((result) => result.kind === "new")).toHaveLength(
          1,
        );
        expect(
          results.filter((result) => result.kind === "duplicate"),
        ).toHaveLength(11);
        const row = yield* TxAdmissionsDB.getByTxId(txId);
        expect(row?.request_count).toBe(12n);
      }),
    ),
  );

  it.effect(
    "resolves an identical reserved-batch insert that waited on a concurrent commit as a duplicate",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const firstInserted = yield* Deferred.make<number>();
          const secondStarted = yield* Deferred.make<number>();
          const releaseFirst = yield* Deferred.make<void>();
          const request = {
            txId: databaseTxHash("admission.reserved-concurrent"),
            txCanonicalCbor: databaseFixtureBytes(
              "admission.reserved-concurrent",
              64,
            ),
            submitSource: "native" as const,
          };
          const firstFiber = yield* Effect.fork(
            sql.withTransaction(
              Effect.gen(function* () {
                const [backend] = yield* sql<{
                  readonly pid: number;
                }>`SELECT pg_backend_pid()::int AS pid`;
                const outcomes = yield* TxAdmissionsDB.admitReservedBatch([
                  request,
                ]);
                yield* Deferred.succeed(firstInserted, backend!.pid);
                yield* Deferred.await(releaseFirst);
                return outcomes;
              }),
            ),
          );
          const firstPid = yield* Deferred.await(firstInserted);
          const secondFiber = yield* Effect.fork(
            sql.withTransaction(
              Effect.gen(function* () {
                const [backend] = yield* sql<{
                  readonly pid: number;
                }>`SELECT pg_backend_pid()::int AS pid`;
                yield* Deferred.succeed(secondStarted, backend!.pid);
                return yield* TxAdmissionsDB.admitReservedBatch([request]);
              }),
            ),
          );
          const secondPid = yield* Deferred.await(secondStarted);
          let observedBlockedInsert = false;
          for (let attempt = 0; attempt < 100; attempt += 1) {
            const [blocking] = yield* sql<{
              readonly blocked: boolean;
            }>`SELECT ${firstPid} = ANY(pg_blocking_pids(${secondPid})) AS blocked`;
            if (blocking?.blocked === true) {
              observedBlockedInsert = true;
              break;
            }
            yield* Effect.promise(
              () => new Promise<void>((resolve) => setTimeout(resolve, 10)),
            );
          }
          expect(observedBlockedInsert).toBe(true);
          yield* Deferred.succeed(releaseFirst, undefined);
          const [first, second] = yield* Effect.all(
            [Fiber.join(firstFiber), Fiber.join(secondFiber)],
            { concurrency: "unbounded" },
          );
          expect(first[0]?._tag).toBe("Success");
          expect(second[0]?._tag).toBe("Success");
          if (first[0]?._tag === "Success") {
            expect(first[0].result.kind).toBe("new");
          }
          if (second[0]?._tag === "Success") {
            expect(second[0].result.kind).toBe("duplicate");
          }
          expect(
            (yield* TxAdmissionsDB.getByTxId(request.txId))?.request_count,
          ).toBe(2n);
        }),
      ),
  );

  it.effect(
    "orders overlapping opposite-order reserved batches without deadlock",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          yield* sql`CREATE OR REPLACE FUNCTION phase1_admission_order_barrier()
            RETURNS trigger
            LANGUAGE plpgsql
            AS $$
            BEGIN
              PERFORM pg_advisory_xact_lock(
                hashtextextended(encode(NEW.tx_id, 'hex'), 0)
              );
              PERFORM pg_sleep(0.05);
              RETURN NEW;
            END;
            $$`;
          yield* sql`CREATE TRIGGER phase1_admission_order_barrier
            BEFORE INSERT ON tx_admissions
            FOR EACH ROW
            EXECUTE FUNCTION phase1_admission_order_barrier()`;
          const requestA = {
            txId: databaseTxHash("admission.reserved-order-a"),
            txCanonicalCbor: databaseFixtureBytes(
              "admission.reserved-order-a",
              64,
            ),
            submitSource: "native" as const,
          };
          const requestB = {
            txId: databaseTxHash("admission.reserved-order-b"),
            txCanonicalCbor: databaseFixtureBytes(
              "admission.reserved-order-b",
              64,
            ),
            submitSource: "native" as const,
          };
          const start = yield* Deferred.make<void>();
          const run = (requests: readonly (typeof requestA)[]) =>
            sql.withTransaction(
              Effect.gen(function* () {
                yield* sql`SET LOCAL lock_timeout = '5s'`;
                yield* sql`SET LOCAL statement_timeout = '10s'`;
                yield* Deferred.await(start);
                return yield* TxAdmissionsDB.admitReservedBatch(requests);
              }),
            );
          const forward = yield* Effect.fork(run([requestA, requestB]));
          const reverse = yield* Effect.fork(run([requestB, requestA]));
          yield* Deferred.succeed(start, undefined);
          const outcomes = [
            ...(yield* Fiber.join(forward)),
            ...(yield* Fiber.join(reverse)),
          ];
          for (const request of [requestA, requestB]) {
            const kinds = outcomes
              .filter(
                (outcome) =>
                  outcome._tag === "Success" &&
                  outcome.result.entry.tx_id.equals(request.txId),
              )
              .map((outcome) =>
                outcome._tag === "Success" ? outcome.result.kind : "conflict",
              )
              .sort();
            expect(kinds).toEqual(["duplicate", "new"]);
            expect(
              (yield* TxAdmissionsDB.getByTxId(request.txId))?.request_count,
            ).toBe(2n);
          }
        }),
      ),
  );

  it.effect(
    "does not overshoot a stale live count when local admits fill the cap",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const maxBacklog = 5;
          let accepted = 0;
          let rejected = 0;
          for (let index = 0; index < maxBacklog + 3; index += 1) {
            const result = yield* Effect.either(
              TxAdmissionsDB.admit({
                txId: databaseTxHash(
                  `admission.stale-gauge-${index.toString()}`,
                ),
                txCanonicalCbor: databaseFixtureBytes(
                  `admission.stale-gauge-${index.toString()}`,
                  64,
                ),
                submitSource: "native",
                currentBacklog: yield* readAdmissionBacklogGauge,
                maxBacklog,
              }),
            );
            if (result._tag === "Right") {
              accepted += 1;
              yield* noteLocalAdmit;
            } else {
              rejected += 1;
            }
          }
          expect(accepted).toBe(maxBacklog);
          expect(rejected).toBe(3);
          expect(yield* TxAdmissionsDB.countBacklog).toBe(BigInt(maxBacklog));
        }).pipe(Effect.provide(Globals.Default)),
      ),
  );

  it.effect("does not overshoot the cap under parallel distinct admits", () =>
    isolatedDb(
      Effect.gen(function* () {
        const maxBacklog = 5;
        const results = yield* Effect.all(
          Array.from({ length: 16 }, (_, index) =>
            Effect.gen(function* () {
              const reservation =
                yield* reserveAdmissionBacklogSlot(maxBacklog);
              const result = yield* Effect.either(
                TxAdmissionsDB.admit({
                  txId: databaseTxHash(
                    `admission.parallel-cap-${index.toString()}`,
                  ),
                  txCanonicalCbor: databaseFixtureBytes(
                    `admission.parallel-cap-${index.toString()}`,
                    64,
                  ),
                  submitSource: "native",
                  currentBacklog: reservation.currentBacklog,
                  maxBacklog,
                }),
              );
              if (reservation.reserved) {
                if (result._tag === "Right" && result.right.kind === "new") {
                  yield* commitAdmissionBacklogSlot;
                } else {
                  yield* releaseAdmissionBacklogSlot;
                }
              }
              return result;
            }),
          ),
          { concurrency: "unbounded" },
        );

        expect(
          results.filter(
            (result) => result._tag === "Right" && result.right.kind === "new",
          ),
        ).toHaveLength(maxBacklog);
        expect(
          results.filter(
            (result) =>
              result._tag === "Left" &&
              result.left._tag === "TxAdmissionBacklogFullError",
          ),
        ).toHaveLength(results.length - maxBacklog);
        expect(yield* TxAdmissionsDB.countBacklog).toBe(BigInt(maxBacklog));
        expect(yield* readAdmissionBacklogGauge).toBe(BigInt(maxBacklog));
      }).pipe(Effect.provide(Globals.Default)),
    ),
  );

  it.effect("batch-updates per-row rejection metadata under one lease", () =>
    isolatedDb(
      Effect.gen(function* () {
        const inputs = [
          {
            txId: databaseTxHash("admission.reject-1"),
            txCanonicalCbor: databaseFixtureBytes("admission.reject-1", 64),
          },
          {
            txId: databaseTxHash("admission.reject-2"),
            txCanonicalCbor: databaseFixtureBytes("admission.reject-2", 64),
          },
        ];
        yield* Effect.all(
          inputs.map((input) =>
            TxAdmissionsDB.admit({
              ...input,
              submitSource: "native",
              currentBacklog: 0n,
              maxBacklog: 10,
            }),
          ),
          { concurrency: "unbounded" },
        );
        const leaseOwner = "database-test:rejections";
        const claimed = yield* TxAdmissionsDB.claimBatch({
          limit: 2,
          leaseOwner,
          leaseDurationMs: 30_000,
        });
        yield* TxAdmissionsDB.markRejected({
          rows: claimed,
          leaseOwner,
          rejectedTxs: [
            {
              txId: inputs[0]!.txId,
              code: RejectCodes.InputNotFound,
              detail: "missing input",
            },
            {
              txId: inputs[1]!.txId,
              code: RejectCodes.DoubleSpend,
              detail: null,
            },
          ],
        });

        const first = yield* TxAdmissionsDB.getByTxId(inputs[0]!.txId);
        const second = yield* TxAdmissionsDB.getByTxId(inputs[1]!.txId);
        expect(first?.status).toBe(TxAdmissionsDB.Status.Rejected);
        expect(first?.reject_code).toBe(RejectCodes.InputNotFound);
        expect(first?.reject_detail).toBe("missing input");
        expect(second?.status).toBe(TxAdmissionsDB.Status.Rejected);
        expect(second?.reject_code).toBe(RejectCodes.DoubleSpend);
        expect(second?.reject_detail).toBeNull();
      }),
    ),
  );

  it.effect("claims disjoint leases for two concurrent validation loops", () =>
    isolatedDb(
      Effect.gen(function* () {
        const inputs = Array.from({ length: 8 }, (_, index) => ({
          txId: databaseTxHash(`admission.parallel-claim-${index}`),
          txCanonicalCbor: databaseFixtureBytes(
            `admission.parallel-claim-${index}`,
            64,
          ),
        }));
        yield* Effect.all(
          inputs.map((input) =>
            TxAdmissionsDB.admit({
              ...input,
              submitSource: "native",
              currentBacklog: 0n,
              maxBacklog: 20,
            }),
          ),
          { concurrency: "unbounded" },
        );
        const [left, right] = yield* Effect.all(
          [
            TxAdmissionsDB.claimBatch({
              limit: 4,
              leaseOwner: "parallel-loop:left",
              leaseDurationMs: 30_000,
            }),
            TxAdmissionsDB.claimBatch({
              limit: 4,
              leaseOwner: "parallel-loop:right",
              leaseDurationMs: 30_000,
            }),
          ],
          { concurrency: "unbounded" },
        );
        const leftIds = new Set(
          left.map((entry) => entry.tx_id.toString("hex")),
        );
        const rightIds = new Set(
          right.map((entry) => entry.tx_id.toString("hex")),
        );
        expect(left).toHaveLength(4);
        expect(right).toHaveLength(4);
        expect([...leftIds].some((txId) => rightIds.has(txId))).toBe(false);
        expect(new Set([...leftIds, ...rightIds]).size).toBe(8);
      }),
    ),
  );

  it.effect(
    "loads exact payloads after a lightweight ordered claim and fails closed on loss",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const inputs = Array.from({ length: 3 }, (_, index) => ({
            txId: databaseTxHash(`admission.split-claim-${index}`),
            txCanonicalCbor: databaseFixtureBytes(
              `admission.split-claim-${index}`,
              64 + index,
            ),
          }));
          for (const input of inputs) {
            yield* TxAdmissionsDB.admit({
              ...input,
              submitSource: "native",
              currentBacklog: 0n,
              maxBacklog: 10,
            });
          }
          const leaseOwner = "database-test:split-claim";
          const claimed = yield* TxAdmissionsDB.claimBatchLease({
            limit: inputs.length,
            leaseOwner,
            leaseDurationMs: 30_000,
          });
          expect(claimed).toHaveLength(inputs.length);
          expect("tx_canonical_cbor" in claimed[0]!).toBe(false);

          const loaded = yield* TxAdmissionsDB.loadClaimedPayloads({
            claimed,
            leaseOwner,
          });
          expect(loaded.map((entry) => entry.tx_id)).toEqual(
            inputs.map((input) => input.txId),
          );
          expect(loaded.map((entry) => entry.tx_canonical_cbor)).toEqual(
            inputs.map((input) => input.txCanonicalCbor),
          );

          const sql = yield* SqlClient.SqlClient;
          yield* sql`DELETE FROM tx_admission_payloads
            WHERE tx_id = ${inputs[1]!.txId}`;
          const missingPayload = yield* Effect.either(
            TxAdmissionsDB.loadClaimedPayloads({ claimed, leaseOwner }),
          );
          expect(missingPayload._tag).toBe("Left");
          const stillLeased = yield* TxAdmissionsDB.getByTxId(inputs[0]!.txId);
          expect(stillLeased?.status).toBe(TxAdmissionsDB.Status.Validating);
          expect(stillLeased?.lease_owner).toBe(leaseOwner);

          yield* TxAdmissionsDB.releaseForRetry({
            txIds: claimed.map((entry) => entry.tx_id),
            leaseOwner,
            baseDelayMs: 0,
            maxDelayMs: 0,
          });
          const recovered = yield* TxAdmissionsDB.getByTxId(inputs[0]!.txId);
          expect(recovered?.status).toBe(TxAdmissionsDB.Status.Queued);
          expect(recovered?.lease_owner).toBeNull();
        }),
      ),
  );

  it.effect("keeps relaxed claim durability transaction-local", () =>
    isolatedDb(
      Effect.gen(function* () {
        const sql = yield* SqlClient.SqlClient;
        const admissionSql = yield* AdmissionSql;
        const readSetting = (client: SqlClient.SqlClient) =>
          client<{ readonly synchronous_commit: string }>`
            SHOW synchronous_commit`.pipe(
            Effect.map((rows) => rows[0]?.synchronous_commit),
          );
        const txId = databaseTxHash("admission.local-sync-setting");
        yield* TxAdmissionsDB.admit({
          txId,
          txCanonicalCbor: databaseFixtureBytes(
            "admission.local-sync-setting",
            64,
          ),
          submitSource: "native",
          currentBacklog: 0n,
          maxBacklog: 10,
        });
        expect(yield* readSetting(sql)).toBe("on");
        expect(yield* readSetting(admissionSql)).toBe("on");
        const claimed = yield* TxAdmissionsDB.claimBatch({
          limit: 1,
          leaseOwner: "local-sync-setting",
          leaseDurationMs: 30_000,
        });
        expect(claimed).toHaveLength(1);
        expect(yield* readSetting(sql)).toBe("on");
        expect(yield* readSetting(admissionSql)).toBe("on");
      }),
    ),
  );

  it.effect(
    "orders duplicate arrival sequences by tx id without a full-history unique index",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const txIds = [
            databaseTxHash("admission.arrival-tie-c"),
            databaseTxHash("admission.arrival-tie-a"),
            databaseTxHash("admission.arrival-tie-b"),
          ];
          yield* Effect.all(
            txIds.map((txId) =>
              TxAdmissionsDB.admit({
                txId,
                txCanonicalCbor: databaseFixtureBytes(
                  `admission.arrival-tie-${txId.toString("hex")}`,
                  64,
                ),
                submitSource: "native",
                currentBacklog: 0n,
                maxBacklog: 10,
              }),
            ),
            { concurrency: "unbounded" },
          );
          const sql = yield* SqlClient.SqlClient;
          yield* sql`UPDATE tx_admissions SET arrival_seq = 7`;
          const claimed = yield* TxAdmissionsDB.claimBatch({
            limit: 3,
            leaseOwner: "arrival-tie",
            leaseDurationMs: 30_000,
          });
          expect(claimed.map((row) => row.tx_id.toString("hex"))).toEqual(
            [...txIds].sort(Buffer.compare).map((txId) => txId.toString("hex")),
          );

          const indexes = yield* sql<{
            readonly indexname: string;
            readonly indexdef: string;
          }>`SELECT indexname, indexdef
            FROM pg_indexes
            WHERE tablename = 'tx_admissions'`;
          expect(
            indexes.some(
              (index) => index.indexname === "tx_admissions_arrival_seq_key",
            ),
          ).toBe(false);
          expect(
            indexes.find(
              (index) => index.indexname === "idx_tx_admissions_queued_arrival",
            )?.indexdef,
          ).toContain("(arrival_seq, tx_id)");
        }),
      ),
  );

  it.effect(
    "releases a validation lease after a worker infrastructure crash",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const txId = databaseTxHash("admission.worker-crash");
          const txCanonicalCbor = databaseFixtureBytes(
            "admission.worker-crash",
            64,
          );
          yield* TxAdmissionsDB.admit({
            txId,
            txCanonicalCbor,
            submitSource: "native",
            currentBacklog: 0n,
            maxBacklog: 10,
          });
          const leaseOwner = "worker-crash:lease";
          const claimed = yield* TxAdmissionsDB.claimBatch({
            limit: 1,
            leaseOwner,
            leaseDurationMs: 30_000,
          });
          const exit = yield* Effect.exit(
            withAdmissionLeaseRecovery(
              Effect.die(
                new ValidationWorkerError({
                  message: "worker crashed during UPLC evaluation",
                }),
              ),
              TxAdmissionsDB.releaseForRetry({
                txIds: claimed.map((entry) => entry.tx_id),
                leaseOwner,
                baseDelayMs: 0,
                maxDelayMs: 0,
              }),
            ),
          );
          expect(exit._tag).toBe("Failure");
          const recovered = yield* TxAdmissionsDB.getByTxId(txId);
          expect(recovered?.status).toBe(TxAdmissionsDB.Status.Queued);
          expect(recovered?.lease_owner).toBeNull();
          expect(recovered?.lease_expires_at).toBeNull();

          const retryLeaseOwner = "worker-crash:retry";
          const retryClaim = yield* TxAdmissionsDB.claimBatch({
            limit: 1,
            leaseOwner: retryLeaseOwner,
            leaseDurationMs: 30_000,
          });
          const source = {
            ...ledgerEntry1,
            [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
              "admission.worker-crash-source",
            ),
          };
          const produced = {
            ...ledgerEntry1,
            [LedgerUtils.Columns.TX_ID]: txId,
            [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
              "admission.worker-crash-produced",
            ),
          };
          yield* MempoolLedgerDB.insert([source]);
          yield* TxAdmissionsDB.markAccepted({
            rows: retryClaim,
            leaseOwner: retryLeaseOwner,
            processedTxs: [
              {
                txId,
                txCbor: txCanonicalCbor,
                spent: [source[LedgerUtils.Columns.OUTREF]],
                produced: [produced],
              },
            ],
          });
          yield* (yield* WriteBehind).flushNow;

          const sql = yield* SqlClient.SqlClient;
          const membership = yield* sql<{
            readonly tx: Buffer;
          }>`SELECT tx FROM mempool WHERE tx_id = ${txId}`;
          expect(membership).toHaveLength(1);
          expect(membership[0]?.tx).toEqual(txCanonicalCbor);
          expect((yield* TxAdmissionsDB.getByTxId(txId))?.status).toBe(
            TxAdmissionsDB.Status.Accepted,
          );
          yield* sql`DELETE FROM tx_admission_payloads WHERE tx_id = ${txId}`;
          expect(yield* MempoolDB.retrieveTxCborByHash(txId)).toEqual(
            txCanonicalCbor,
          );
          expect(
            yield* MempoolDB.retrieveTxCborsByHashes([txId]),
          ).toStrictEqual([txCanonicalCbor]);

          const page = yield* MempoolDB.retrievePage({ limit: 10 });
          const selection = selectCommitTxCandidates({
            mempoolTxs: page.entries,
            processedMempoolTxs: [],
          });
          expect(selection.sourceTable).toBe("mempool");
          expect(selection.candidateTxHashes).toStrictEqual([txId]);
          expect(selection.candidateTxs.map((entry) => entry.tx)).toStrictEqual(
            [txCanonicalCbor],
          );
          expect(
            (yield* AddressHistoryDB.retrieve(address1)).map(toHex),
          ).toContain(toHex(txCanonicalCbor));
        }),
      ),
  );

  it.effect(
    "survives SIGKILL after accept commit and positively decodes the lost write-behind delta after restart",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const tx = makeNativeSubmitTx();
          const processed = yield* breakDownTx(tx.txCanonicalCbor);
          expect(processed.txId).toEqual(tx.txId);
          expect(processed.spent).toHaveLength(1);
          expect(processed.produced.length).toBeGreaterThan(0);
          const source = {
            ...ledgerEntry1,
            [LedgerUtils.Columns.OUTREF]: processed.spent[0]!,
          } satisfies LedgerUtils.Entry;
          yield* MempoolLedgerDB.insert([source]);
          yield* TxAdmissionsDB.admit({
            txId: tx.txId,
            txCanonicalCbor: tx.txCanonicalCbor,
            submitSource: "native",
            currentBacklog: 0n,
            maxBacklog: 10,
          });
          const leaseOwner = "phase1-write-behind-crash";
          const claimed = yield* TxAdmissionsDB.claimBatch({
            limit: 1,
            leaseOwner,
            leaseDurationMs: 30_000,
          });
          expect(claimed).toHaveLength(1);

          const helper = bundleChildProcessHelper(
            "./helpers/phase1-write-behind-crash-process.ts",
          );
          const crashInput = JSON.stringify({
            txIdHex: tx.txIdHex,
            txCanonicalCborHex: tx.txCanonicalCbor.toString("hex"),
            leaseOwner,
            spentOutrefHexes: processed.spent.map((value) =>
              value.toString("hex"),
            ),
            produced: processed.produced.map((entry) => ({
              txIdHex: entry[LedgerUtils.Columns.TX_ID].toString("hex"),
              outrefHex: entry[LedgerUtils.Columns.OUTREF].toString("hex"),
              outputHex: entry[LedgerUtils.Columns.OUTPUT].toString("hex"),
              address: entry[LedgerUtils.Columns.ADDRESS],
            })),
          });
          const crashMarker = "phase1_accept_committed_before_write_behind";
          const supervised = yield* Effect.promise(() =>
            superviseHostProcess({
              service: "phase1-write-behind-crash",
              command: process.execPath,
              args: [helper],
              cwd: path.resolve(__dirname, ".."),
              env: {
                ...databaseChildProcessEnv(),
                PHASE1_WRITE_BEHIND_CRASH_INPUT: crashInput,
              },
              envInheritance: "none",
              rawLogPath: path.resolve(
                __dirname,
                "../.probe-dist/phase1-write-behind-crash.log",
              ),
              timeoutMs: 10_000,
              maxRestarts: 0,
              terminateOnOutput: { marker: crashMarker, signal: "SIGKILL" },
            }),
          );
          expect(supervised.status).toBe("restart_budget_exhausted");
          expect(supervised.attempts[0]?.signal).toBe("SIGKILL");
          expect(supervised.attempts[0]?.outputTermination).toMatchObject({
            marker: crashMarker,
            signal: "SIGKILL",
          });

          expect((yield* TxAdmissionsDB.getByTxId(tx.txId))?.status).toBe(
            TxAdmissionsDB.Status.Accepted,
          );
          expect(yield* MempoolDB.retrieveTxCount).toBe(1n);
          expect(
            (yield* MempoolTxDeltasDB.retrieveByTxIds([tx.txId])).size,
          ).toBe(0);
          expect(yield* AddressHistoryDB.retrieve(address1)).toEqual([]);

          // A new runtime has no in-memory write-behind queue. The durable
          // mempool row carries canonical CBOR inline, so the commit fallback
          // can reconstruct the exact delta without an admission-table join.
          const page = yield* MempoolDB.retrievePage({ limit: 1 });
          expect(page.entries).toHaveLength(1);
          const restarted = yield* resolveTxDeltaForCommit(
            page.entries[0]!,
            undefined,
          );
          expect(restarted._tag).toBe("Decoded");
          if (restarted._tag === "Decoded") {
            expect(restarted.spent).toStrictEqual(processed.spent);
            expect(restarted.produced).toStrictEqual(
              processed.produced.map((entry) => ({
                [LedgerUtils.Columns.OUTREF]: entry[LedgerUtils.Columns.OUTREF],
                [LedgerUtils.Columns.OUTPUT]: entry[LedgerUtils.Columns.OUTPUT],
              })),
            );
          }
          expect(yield* AddressHistoryDB.retrieve(address1)).toEqual([]);
        }),
      ),
  );

  it.effect(
    "atomically accepts multiple rows and consumes the matching deposit source",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const deposit = makeDepositEntry({
            [DepositsDB.Columns.PROJECTED_HEADER_HASH]: databaseFixtureBytes(
              "admission.array-deposit.header",
              28,
            ),
            [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
          });
          yield* DepositsDB.insertEntries([deposit]);
          const depositSource = yield* DepositsDB.toMempoolLedgerEntry(deposit);
          const normalSource = {
            ...ledgerEntry2,
            [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
              "admission.array-deposit.normal-source",
            ),
          } satisfies LedgerUtils.Entry;
          yield* MempoolLedgerDB.insert([depositSource, normalSource]);

          const inputs = [
            {
              txId: databaseTxHash("admission.array-deposit.first"),
              txCanonicalCbor: databaseFixtureBytes(
                "admission.array-deposit.first",
                64,
              ),
              source: depositSource,
            },
            {
              txId: databaseTxHash("admission.array-deposit.second"),
              txCanonicalCbor: databaseFixtureBytes(
                "admission.array-deposit.second",
                64,
              ),
              source: normalSource,
            },
          ];
          yield* Effect.all(
            inputs.map(({ txId, txCanonicalCbor }) =>
              TxAdmissionsDB.admit({
                txId,
                txCanonicalCbor,
                submitSource: "native",
                currentBacklog: 0n,
                maxBacklog: 10,
              }),
            ),
            { concurrency: "unbounded", discard: true },
          );
          const leaseOwner = "database-test:array-deposit-accept";
          const claimed = yield* TxAdmissionsDB.claimBatch({
            limit: inputs.length,
            leaseOwner,
            leaseDurationMs: 30_000,
          });
          const produced = inputs.map((input, index) => ({
            [LedgerUtils.Columns.TX_ID]: input.txId,
            [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
              `admission.array-deposit.produced-${index.toString()}`,
            ),
            [LedgerUtils.Columns.OUTPUT]: databaseFixtureBytes(
              `admission.array-deposit.output-${index.toString()}`,
              80,
            ),
            [LedgerUtils.Columns.ADDRESS]: address1,
          }));
          yield* TxAdmissionsDB.markAccepted({
            rows: claimed,
            leaseOwner,
            processedTxs: inputs.map((input, index) => ({
              txId: input.txId,
              txCbor: input.txCanonicalCbor,
              spent: [input.source[LedgerUtils.Columns.OUTREF]],
              produced: [produced[index]!],
            })),
          });

          const refreshedDeposit = yield* DepositsDB.retrieveByEventId(
            deposit[DepositsDB.Columns.ID],
          );
          expect(Option.isSome(refreshedDeposit)).toBe(true);
          if (Option.isSome(refreshedDeposit)) {
            expect(refreshedDeposit.value[DepositsDB.Columns.STATUS]).toBe(
              DepositsDB.Status.Consumed,
            );
          }
          expect(yield* MempoolDB.retrieveTxCount).toBe(2n);
          const ledger = yield* MempoolLedgerDB.retrieveSpendable;
          expect(
            new Set(ledger.map((entry) => entry.outref.toString("hex"))),
          ).toEqual(
            new Set(
              produced.map((entry) =>
                entry[LedgerUtils.Columns.OUTREF].toString("hex"),
              ),
            ),
          );
          const accepted = yield* Effect.forEach(inputs, ({ txId }) =>
            TxAdmissionsDB.getByTxId(txId),
          );
          expect(
            accepted.every(
              (entry) => entry?.status === TxAdmissionsDB.Status.Accepted,
            ),
          ).toBe(true);
        }),
      ),
  );

  it.effect(
    "rolls back deposit consumption when bytea-array lease counts mismatch",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const deposit = makeDepositEntry({
            [DepositsDB.Columns.PROJECTED_HEADER_HASH]: databaseFixtureBytes(
              "admission.array-deposit-rollback.header",
              28,
            ),
            [DepositsDB.Columns.STATUS]: DepositsDB.Status.Projected,
          });
          yield* DepositsDB.insertEntries([deposit]);
          const depositSource = yield* DepositsDB.toMempoolLedgerEntry(deposit);
          const normalSource = {
            ...ledgerEntry2,
            [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
              "admission.array-deposit-rollback.normal-source",
            ),
          } satisfies LedgerUtils.Entry;
          yield* MempoolLedgerDB.insert([depositSource, normalSource]);
          const inputs = [
            {
              txId: databaseTxHash("admission.array-deposit-rollback.first"),
              txCanonicalCbor: databaseFixtureBytes(
                "admission.array-deposit-rollback.first",
                64,
              ),
              source: depositSource,
            },
            {
              txId: databaseTxHash("admission.array-deposit-rollback.second"),
              txCanonicalCbor: databaseFixtureBytes(
                "admission.array-deposit-rollback.second",
                64,
              ),
              source: normalSource,
            },
          ];
          yield* Effect.all(
            inputs.map(({ txId, txCanonicalCbor }) =>
              TxAdmissionsDB.admit({
                txId,
                txCanonicalCbor,
                submitSource: "native",
                currentBacklog: 0n,
                maxBacklog: 10,
              }),
            ),
            { concurrency: "unbounded", discard: true },
          );
          const leaseOwner = "database-test:array-deposit-rollback";
          const claimed = yield* TxAdmissionsDB.claimBatch({
            limit: inputs.length,
            leaseOwner,
            leaseDurationMs: 30_000,
          });
          const sql = yield* SqlClient.SqlClient;
          yield* sql`UPDATE ${sql(TxAdmissionsDB.tableName)}
            SET lease_owner = 'other-owner'
            WHERE tx_id = ${inputs[1]!.txId}`;
          const result = yield* Effect.either(
            TxAdmissionsDB.markAccepted({
              rows: claimed,
              leaseOwner,
              processedTxs: inputs.map((input, index) => ({
                txId: input.txId,
                txCbor: input.txCanonicalCbor,
                spent: [input.source[LedgerUtils.Columns.OUTREF]],
                produced: [
                  {
                    [LedgerUtils.Columns.TX_ID]: input.txId,
                    [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
                      `admission.array-deposit-rollback.produced-${index.toString()}`,
                    ),
                    [LedgerUtils.Columns.OUTPUT]: databaseFixtureBytes(
                      `admission.array-deposit-rollback.output-${index.toString()}`,
                      80,
                    ),
                    [LedgerUtils.Columns.ADDRESS]: address1,
                  },
                ],
              })),
            }),
          );

          expect(result._tag).toBe("Left");
          expect(yield* MempoolDB.retrieveTxCount).toBe(0n);
          const refreshedDeposit = yield* DepositsDB.retrieveByEventId(
            deposit[DepositsDB.Columns.ID],
          );
          expect(Option.isSome(refreshedDeposit)).toBe(true);
          if (Option.isSome(refreshedDeposit)) {
            expect(refreshedDeposit.value[DepositsDB.Columns.STATUS]).toBe(
              DepositsDB.Status.Projected,
            );
          }
          const ledger = yield* MempoolLedgerDB.retrieveSpendable;
          expect(
            new Set(ledger.map((entry) => entry.outref.toString("hex"))),
          ).toEqual(
            new Set(
              [depositSource, normalSource].map((entry) =>
                entry[LedgerUtils.Columns.OUTREF].toString("hex"),
              ),
            ),
          );
          expect((yield* (yield* WriteBehind).depths).totalDepth).toBe(0);
        }),
      ),
  );

  it.effect(
    "rolls back a batch rejection when the lease count mismatches",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const inputs = [
            {
              txId: databaseTxHash("admission.reject-mismatch-1"),
              txCanonicalCbor: databaseFixtureBytes(
                "admission.reject-mismatch-1",
                64,
              ),
            },
            {
              txId: databaseTxHash("admission.reject-mismatch-2"),
              txCanonicalCbor: databaseFixtureBytes(
                "admission.reject-mismatch-2",
                64,
              ),
            },
          ];
          yield* Effect.all(
            inputs.map((input) =>
              TxAdmissionsDB.admit({
                ...input,
                submitSource: "native",
                currentBacklog: 0n,
                maxBacklog: 10,
              }),
            ),
            { concurrency: "unbounded" },
          );
          const leaseOwner = "database-test:rejection-mismatch";
          const claimed = yield* TxAdmissionsDB.claimBatch({
            limit: 2,
            leaseOwner,
            leaseDurationMs: 30_000,
          });
          const sql = yield* SqlClient.SqlClient;
          yield* sql`UPDATE tx_admissions SET lease_owner = 'other-owner' WHERE tx_id = ${inputs[1]!.txId}`;

          const result = yield* Effect.either(
            TxAdmissionsDB.markRejected({
              rows: claimed,
              leaseOwner,
              rejectedTxs: [
                {
                  txId: inputs[0]!.txId,
                  code: RejectCodes.InputNotFound,
                  detail: "first",
                },
                {
                  txId: inputs[1]!.txId,
                  code: RejectCodes.DoubleSpend,
                  detail: "second",
                },
              ],
            }),
          );
          expect(result._tag).toBe("Left");
          const first = yield* TxAdmissionsDB.getByTxId(inputs[0]!.txId);
          expect(first?.status).toBe(TxAdmissionsDB.Status.Validating);
          const rejectionCount = yield* sql<{
            readonly count: string;
          }>`SELECT COUNT(*)::text AS count FROM tx_rejections`;
          expect(rejectionCount[0]?.count).toBe("0");
        }),
      ),
  );

  it.effect(
    "copies the durable canonical CBOR on the fallback accepted path",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const txId = databaseTxHash("admission.accept-fallback-inline");
          const txCanonicalCbor = databaseFixtureBytes(
            "admission.accept-fallback-inline",
            64,
          );
          yield* TxAdmissionsDB.admit({
            txId,
            txCanonicalCbor,
            submitSource: "native",
            currentBacklog: 0n,
            maxBacklog: 10,
          });
          const leaseOwner = "database-test:accept-fallback-inline";
          const claimed = yield* TxAdmissionsDB.claimBatch({
            limit: 1,
            leaseOwner,
            leaseDurationMs: 30_000,
          });

          yield* TxAdmissionsDB.markAccepted({
            rows: claimed,
            leaseOwner,
            processedTxs: [
              {
                txId,
                txCbor: txCanonicalCbor,
                spent: [],
                produced: [],
              },
            ],
          });

          const sql = yield* SqlClient.SqlClient;
          const memberships = yield* sql<{ readonly tx: Buffer }>`
            SELECT tx FROM mempool WHERE tx_id = ${txId}`;
          expect(memberships).toEqual([{ tx: txCanonicalCbor }]);
          expect((yield* TxAdmissionsDB.getByTxId(txId))?.status).toBe(
            TxAdmissionsDB.Status.Accepted,
          );
          yield* sql`DELETE FROM tx_admission_payloads WHERE tx_id = ${txId}`;
          expect(yield* MempoolDB.retrieveTxCborByHash(txId)).toEqual(
            txCanonicalCbor,
          );
        }),
      ),
  );

  it.effect(
    "refuses terminal acceptance when the durable admission payload is missing",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const txId = databaseTxHash("admission.accept-missing-payload");
          const txCanonicalCbor = databaseFixtureBytes(
            "admission.accept-missing-payload",
            64,
          );
          yield* TxAdmissionsDB.admit({
            txId,
            txCanonicalCbor,
            submitSource: "native",
            currentBacklog: 0n,
            maxBacklog: 10,
          });
          const leaseOwner = "database-test:accept-missing-payload";
          const claimed = yield* TxAdmissionsDB.claimBatch({
            limit: 1,
            leaseOwner,
            leaseDurationMs: 30_000,
          });
          const sql = yield* SqlClient.SqlClient;
          yield* sql`DELETE FROM tx_admission_payloads WHERE tx_id = ${txId}`;

          const result = yield* Effect.either(
            TxAdmissionsDB.markAccepted({
              rows: claimed,
              leaseOwner,
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
          expect(yield* MempoolDB.retrieveTxCount).toBe(0n);
          const admissions = yield* sql<{
            readonly status: TxAdmissionsDB.Status;
            readonly lease_owner: string | null;
          }>`SELECT status, lease_owner FROM tx_admissions WHERE tx_id = ${txId}`;
          expect(admissions).toHaveLength(1);
          expect(admissions[0]?.status).toBe(TxAdmissionsDB.Status.Validating);
          expect(admissions[0]?.lease_owner).toBe(leaseOwner);
        }),
      ),
  );

  it.effect(
    "rolls back the compact accepted fast path when the lease count mismatches",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const inputs = [
            {
              txId: databaseTxHash("admission.accept-mismatch-1"),
              txCanonicalCbor: databaseFixtureBytes(
                "admission.accept-mismatch-1",
                64,
              ),
            },
            {
              txId: databaseTxHash("admission.accept-mismatch-2"),
              txCanonicalCbor: databaseFixtureBytes(
                "admission.accept-mismatch-2",
                64,
              ),
            },
          ];
          yield* Effect.all(
            inputs.map((input) =>
              TxAdmissionsDB.admit({
                ...input,
                submitSource: "native",
                currentBacklog: 0n,
                maxBacklog: 10,
              }),
            ),
            { concurrency: "unbounded" },
          );
          yield* MempoolLedgerDB.insert([ledgerEntry1, ledgerEntry2]);
          const leaseOwner = "database-test:accept-mismatch";
          const claimed = yield* TxAdmissionsDB.claimBatch({
            limit: 2,
            leaseOwner,
            leaseDurationMs: 30_000,
          });
          const sql = yield* SqlClient.SqlClient;
          yield* sql`UPDATE tx_admissions
            SET lease_owner = 'other-owner'
            WHERE tx_id = ${inputs[1]!.txId}`;

          const processedTxs: readonly ProcessedTx[] = inputs.map(
            (input, index) => {
              const source = index === 0 ? ledgerEntry1 : ledgerEntry2;
              return {
                txId: input.txId,
                txCbor: input.txCanonicalCbor,
                spent: [source[LedgerUtils.Columns.OUTREF]],
                produced: [
                  {
                    ...source,
                    [LedgerUtils.Columns.TX_ID]: input.txId,
                    [LedgerUtils.Columns.OUTREF]: databaseOutputReferenceId(
                      `admission.accept-mismatch-produced-${index.toString()}`,
                    ),
                  },
                ],
              };
            },
          );
          const result = yield* Effect.either(
            TxAdmissionsDB.markAccepted({
              rows: claimed,
              leaseOwner,
              processedTxs,
            }),
          );
          expect(result._tag).toBe("Left");
          expect(yield* MempoolDB.retrieveTxCount).toBe(0n);
          const ledger = yield* MempoolLedgerDB.retrieveSpendable;
          expect(
            new Set(ledger.map((entry) => entry.outref.toString("hex"))),
          ).toEqual(
            new Set(
              [ledgerEntry1, ledgerEntry2].map((entry) =>
                entry[LedgerUtils.Columns.OUTREF].toString("hex"),
              ),
            ),
          );
          const first = yield* TxAdmissionsDB.getByTxId(inputs[0]!.txId);
          expect(first?.status).toBe(TxAdmissionsDB.Status.Validating);
          expect((yield* (yield* WriteBehind).depths).totalDepth).toBe(0);
        }),
      ),
  );
});

describe("DaPayloadsDB", () => {
  it.effect(
    "stores payloads idempotently and rejects conflicting bytes for a header",
    (_) =>
      isolatedDb(
        Effect.gen(function* () {
          const headerHash = databaseFixtureBytes("da-payload-header", 28);
          const payload = databaseFixtureBytes("da-payload-cbor", 48);
          const payloadHash = createHash("sha256").update(payload).digest();
          const insert = {
            [DaPayloadsDB.Columns.HEADER_HASH]: headerHash,
            [DaPayloadsDB.Columns.VERSION]: Number(SDK.DA_PAYLOAD_V2_VERSION),
            [DaPayloadsDB.Columns.PAYLOAD_CBOR]: payload,
            [DaPayloadsDB.Columns.PAYLOAD_SHA256]: payloadHash,
            [DaPayloadsDB.Columns.UTXOS_ROOT]: "11".repeat(32),
            [DaPayloadsDB.Columns.FORCED_TRANSACTIONS_ROOT]:
              SDK.EMPTY_MERKLE_TREE_ROOT,
            [DaPayloadsDB.Columns.TRANSACTIONS_ROOT]: "22".repeat(32),
            [DaPayloadsDB.Columns.DEPOSITS_ROOT]: "33".repeat(32),
            [DaPayloadsDB.Columns.WITHDRAWALS_ROOT]: "44".repeat(32),
            [DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT]:
              SDK.EMPTY_MERKLE_TREE_ROOT,
            [DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT]:
              SDK.EMPTY_MERKLE_TREE_ROOT,
            [DaPayloadsDB.Columns.WITHDRAWAL_COUNT]: 0n,
            [DaPayloadsDB.Columns.FORCED_TRANSACTION_COUNT]: 0n,
            [DaPayloadsDB.Columns.L2_TRANSACTION_COUNT]: 0n,
            [DaPayloadsDB.Columns.DEPOSIT_COUNT]: 0n,
            [DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]: 0n,
            [DaPayloadsDB.Columns.TRANSITION_STEP_COUNT]: 0n,
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
    "rolls back a local-finalization mutation on DA conflict and retries idempotently",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const insert = {
            ...daPayloadInsertFixture("da-finalization-atomicity"),
            [DaPayloadsDB.Columns.BLOCK_START_TIME]: new Date(
              "2026-07-10T00:00:00.000Z",
            ),
            [DaPayloadsDB.Columns.BLOCK_END_TIME]: new Date(
              "2026-07-10T00:00:20.000Z",
            ),
          };
          yield* DaPayloadsDB.upsertAvailable(insert);
          const sql = yield* SqlClient.SqlClient;
          const transaction = Effect.gen(function* () {
            yield* sql`UPDATE da_payloads
              SET block_start_time = ${new Date("2026-07-11T00:00:00.000Z")}
              WHERE header_hash = ${insert.header_hash}`;
            yield* DaPayloadsDB.upsertAvailable({
              ...insert,
              [DaPayloadsDB.Columns.PAYLOAD_CBOR]: Buffer.from([
                ...insert.payload_cbor,
                0,
              ]),
            });
          });

          const failed = yield* Effect.either(sql.withTransaction(transaction));
          expect(failed._tag).toBe("Left");
          const afterRollback = yield* DaPayloadsDB.retrieveByHeaderHash(
            insert.header_hash,
          );
          expect(afterRollback._tag).toBe("Some");
          if (afterRollback._tag === "Some") {
            expect(afterRollback.value.block_start_time).toEqual(
              insert.block_start_time,
            );
            expect(afterRollback.value.payload_cbor).toEqual(
              insert.payload_cbor,
            );
          }

          yield* sql.withTransaction(DaPayloadsDB.upsertAvailable(insert));
          yield* sql.withTransaction(DaPayloadsDB.upsertAvailable(insert));
        }),
      ),
  );

  it.effect(
    "seeds a persisted payload after restart, claims once, and resumes after a failed scan",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const insert = daPayloadInsertFixture("da-publication-restart");
          const peers = [
            {
              signerIndex: 0,
              daVkey: "01".repeat(32),
              peerId: "peer-a",
              multiaddrs: ["/ip4/127.0.0.1/tcp/4101"],
              roles: ["committee"],
            },
            {
              signerIndex: 1,
              daVkey: "02".repeat(32),
              peerId: "peer-b",
              multiaddrs: ["/ip4/127.0.0.1/tcp/4102"],
              roles: ["committee"],
            },
            {
              signerIndex: 2,
              daVkey: "03".repeat(32),
              peerId: "peer-c",
              multiaddrs: ["/ip4/127.0.0.1/tcp/4103"],
              roles: ["committee"],
            },
          ] as const;
          // Simulates a crash after durable payload commit but before the
          // publication outbox was seeded.
          yield* DaPayloadsDB.upsertAvailable(insert);
          expect(yield* DaPayloadPublicationsDB.backlogCount(15)).toBe(0);
          yield* DaPayloadPublicationsDB.seedRecentPayloads({
            peers,
            retentionDays: 15,
          });
          expect(yield* DaPayloadPublicationsDB.backlogCount(15)).toBe(3);

          const firstClaim = yield* DaPayloadPublicationsDB.claimDue({
            retentionDays: 15,
            limit: 10,
            leaseOwner: "reconciler-a",
            leaseToken: "lease-a",
            leaseMs: 30_000,
          });
          expect(firstClaim).toHaveLength(3);
          const competingClaim = yield* DaPayloadPublicationsDB.claimDue({
            retentionDays: 15,
            limit: 10,
            leaseOwner: "reconciler-b",
            leaseToken: "lease-b",
            leaseMs: 30_000,
          });
          expect(competingClaim).toHaveLength(0);
          const sql = yield* SqlClient.SqlClient;

          expect(
            yield* DaPayloadPublicationsDB.recordAttempt({
              headerHash: insert.header_hash,
              peer: peers[0],
              status: "accepted",
              retryBackoffMs: 1,
              retryBackoffMaxMs: 2,
              lease: { owner: "reconciler-a", token: "stale-token" },
            }),
          ).toBe(false);
          // A detached foreground straggler is deliberately unleased. It must
          // not clear or overwrite the active reconciler claim.
          expect(
            yield* DaPayloadPublicationsDB.recordAttempt({
              headerHash: insert.header_hash,
              peer: peers[0],
              status: "transport_error",
              error: "late foreground failure during reconciler claim",
              retryBackoffMs: 1,
              retryBackoffMaxMs: 2,
            }),
          ).toBe(false);
          const fencedRows = yield* sql<{
            readonly status: string;
            readonly lease_token: string | null;
          }>`
            SELECT status, lease_token FROM da_payload_publications
            WHERE header_hash = ${insert.header_hash} AND peer_id = 'peer-a'
          `;
          expect(fencedRows[0]).toEqual({
            status: "pending",
            lease_token: "lease-a",
          });

          yield* DaPayloadPublicationsDB.releaseClaim({
            headerHash: insert.header_hash,
            peerId: "peer-a",
            leaseOwner: "reconciler-a",
            leaseToken: "lease-a",
          });
          const resumed = yield* DaPayloadPublicationsDB.claimDue({
            retentionDays: 15,
            limit: 10,
            leaseOwner: "reconciler-b",
            leaseToken: "lease-b",
            leaseMs: 30_000,
          });
          expect(resumed.map((row) => row.peer_id)).toEqual(["peer-a"]);

          expect(
            yield* DaPayloadPublicationsDB.recordAttempt({
              headerHash: insert.header_hash,
              peer: peers[0],
              status: "accepted",
              retryBackoffMs: 1,
              retryBackoffMaxMs: 2,
              lease: { owner: "reconciler-b", token: "lease-b" },
            }),
          ).toBe(true);
          expect(
            yield* DaPayloadPublicationsDB.recordAttempt({
              headerHash: insert.header_hash,
              peer: peers[1],
              status: "duplicate",
              retryBackoffMs: 1,
              retryBackoffMaxMs: 2,
              lease: { owner: "reconciler-a", token: "lease-a" },
            }),
          ).toBe(true);
          expect(
            yield* DaPayloadPublicationsDB.recordAttempt({
              headerHash: insert.header_hash,
              peer: peers[2],
              status: "transport_error",
              error: "first process exited",
              retryBackoffMs: 1,
              retryBackoffMaxMs: 2,
              lease: { owner: "reconciler-a", token: "lease-a" },
            }),
          ).toBe(true);
          expect(
            yield* DaPayloadPublicationsDB.recordAttempt({
              headerHash: insert.header_hash,
              peer: peers[2],
              status: "accepted",
              retryBackoffMs: 1,
              retryBackoffMaxMs: 2,
            }),
          ).toBe(true);
          // Late failures cannot downgrade success; conflict is evidence-grade
          // and has monotone precedence over every other outcome.
          expect(
            yield* DaPayloadPublicationsDB.recordAttempt({
              headerHash: insert.header_hash,
              peer: peers[0],
              status: "transport_error",
              error: "late straggler failure",
              retryBackoffMs: 1,
              retryBackoffMaxMs: 2,
            }),
          ).toBe(true);
          expect(
            yield* DaPayloadPublicationsDB.recordAttempt({
              headerHash: insert.header_hash,
              peer: peers[0],
              status: "conflict",
              error: "divergent bytes",
              retryBackoffMs: 1,
              retryBackoffMaxMs: 2,
            }),
          ).toBe(true);
          expect(
            yield* DaPayloadPublicationsDB.recordAttempt({
              headerHash: insert.header_hash,
              peer: peers[0],
              status: "accepted",
              retryBackoffMs: 1,
              retryBackoffMaxMs: 2,
            }),
          ).toBe(true);
          const statuses = yield* sql<{
            readonly peer_id: string;
            readonly status: string;
          }>`SELECT peer_id, status FROM da_payload_publications ORDER BY peer_id`;
          expect(statuses).toEqual([
            { peer_id: "peer-a", status: "conflict" },
            { peer_id: "peer-b", status: "duplicate" },
            { peer_id: "peer-c", status: "accepted" },
          ]);
          expect(yield* DaPayloadPublicationsDB.backlogCount(15)).toBe(0);
          expect(yield* DaPayloadPublicationsDB.conflictCount(15)).toBe(1);
        }),
      ),
  );

  it.effect(
    "retries announcements durably and fences stale claim writers",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const insert = daPayloadInsertFixture("da-announcement-outbox");
          yield* DaPayloadsDB.upsertAvailable(insert);
          yield* DaPayloadAnnouncementsDB.seedRecentPayloads(15);
          expect(yield* DaPayloadAnnouncementsDB.backlogCount(15)).toBe(1);

          const claimed = yield* DaPayloadAnnouncementsDB.claimDue({
            retentionDays: 15,
            limit: 1,
            leaseOwner: "announcer-a",
            leaseToken: "announcement-lease-a",
            leaseMs: 30_000,
          });
          expect(claimed).toHaveLength(1);
          expect(
            yield* DaPayloadAnnouncementsDB.recordAttempt({
              headerHash: insert.header_hash,
              published: true,
              retryBackoffMs: 1,
              retryBackoffMaxMs: 2,
              lease: { owner: "announcer-a", token: "stale-token" },
            }),
          ).toBe(false);
          expect(
            yield* DaPayloadAnnouncementsDB.recordAttempt({
              headerHash: insert.header_hash,
              published: false,
              error: "late foreground failure during announcement claim",
              retryBackoffMs: 1,
              retryBackoffMaxMs: 2,
            }),
          ).toBe(false);
          const sql = yield* SqlClient.SqlClient;
          const fenced = yield* sql<{
            readonly status: string;
            readonly lease_token: string | null;
          }>`SELECT status, lease_token FROM da_payload_announcements
             WHERE header_hash = ${insert.header_hash}`;
          expect(fenced[0]).toEqual({
            status: "pending",
            lease_token: "announcement-lease-a",
          });

          expect(
            yield* DaPayloadAnnouncementsDB.recordAttempt({
              headerHash: insert.header_hash,
              published: false,
              error: "zero recipients",
              retryBackoffMs: 1,
              retryBackoffMaxMs: 2,
              lease: {
                owner: "announcer-a",
                token: "announcement-lease-a",
              },
            }),
          ).toBe(true);
          yield* sql`UPDATE da_payload_announcements
            SET next_retry_at = NOW()
            WHERE header_hash = ${insert.header_hash}`;
          const retry = yield* DaPayloadAnnouncementsDB.claimDue({
            retentionDays: 15,
            limit: 1,
            leaseOwner: "announcer-b",
            leaseToken: "announcement-lease-b",
            leaseMs: 30_000,
          });
          expect(retry).toHaveLength(1);
          expect(
            yield* DaPayloadAnnouncementsDB.recordAttempt({
              headerHash: insert.header_hash,
              published: true,
              retryBackoffMs: 1,
              retryBackoffMaxMs: 2,
              lease: {
                owner: "announcer-b",
                token: "announcement-lease-b",
              },
            }),
          ).toBe(true);
          expect(
            yield* DaPayloadAnnouncementsDB.recordAttempt({
              headerHash: insert.header_hash,
              published: false,
              error: "late duplicate callback",
              retryBackoffMs: 1,
              retryBackoffMaxMs: 2,
            }),
          ).toBe(true);
          expect(yield* DaPayloadAnnouncementsDB.backlogCount(15)).toBe(0);
          const finalRows = yield* sql<{
            readonly status: string;
            readonly attempts: number;
          }>`SELECT status, attempts FROM da_payload_announcements
             WHERE header_hash = ${insert.header_hash}`;
          expect(finalRows[0]).toMatchObject({ status: "published" });
          expect(finalRows[0]?.attempts).toBeGreaterThanOrEqual(3);
        }),
      ),
  );

  it.effect(
    "retrieves only finalized pending journals missing DA payload rows",
    (_) =>
      isolatedDb(
        Effect.gen(function* () {
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
            [PendingBlockFinalizationsDB.Columns.HEADER_CBOR]: Buffer.from(
              "d87980",
              "hex",
            ),
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
            [PendingBlockFinalizationsDB.Columns.BASE_FORCED_TRANSACTIONS_ROOT]:
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
            [PendingBlockFinalizationsDB.Columns
              .EXPECTED_FORCED_TRANSACTIONS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
            [PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSACTIONS_ROOT]:
              SDK.EMPTY_MERKLE_TREE_ROOT,
            [PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSITS_ROOT]:
              SDK.EMPTY_MERKLE_TREE_ROOT,
            [PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWALS_ROOT]:
              SDK.EMPTY_MERKLE_TREE_ROOT,
            [PendingBlockFinalizationsDB.Columns
              .EXPECTED_TRANSITION_TRACE_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
            [PendingBlockFinalizationsDB.Columns.EXPECTED_EVENT_TO_STEP_ROOT]:
              SDK.EMPTY_MERKLE_TREE_ROOT,
            [PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWAL_COUNT]: 0n,
            [PendingBlockFinalizationsDB.Columns
              .EXPECTED_FORCED_TRANSACTION_COUNT]: 0n,
            [PendingBlockFinalizationsDB.Columns.EXPECTED_L2_TRANSACTION_COUNT]:
              0n,
            [PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSIT_COUNT]: 0n,
            [PendingBlockFinalizationsDB.Columns.EXPECTED_TOTAL_EVENT_COUNT]:
              0n,
            [PendingBlockFinalizationsDB.Columns
              .EXPECTED_TRANSITION_STEP_COUNT]: 0n,
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
            [DaPayloadsDB.Columns.VERSION]: Number(SDK.DA_PAYLOAD_V2_VERSION),
            [DaPayloadsDB.Columns.PAYLOAD_CBOR]: Buffer.from("a100", "hex"),
            [DaPayloadsDB.Columns.PAYLOAD_SHA256]: createHash("sha256")
              .update(Buffer.from("a100", "hex"))
              .digest(),
            [DaPayloadsDB.Columns.UTXOS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
            [DaPayloadsDB.Columns.FORCED_TRANSACTIONS_ROOT]:
              SDK.EMPTY_MERKLE_TREE_ROOT,
            [DaPayloadsDB.Columns.TRANSACTIONS_ROOT]:
              SDK.EMPTY_MERKLE_TREE_ROOT,
            [DaPayloadsDB.Columns.DEPOSITS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
            [DaPayloadsDB.Columns.WITHDRAWALS_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
            [DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT]:
              SDK.EMPTY_MERKLE_TREE_ROOT,
            [DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT]:
              SDK.EMPTY_MERKLE_TREE_ROOT,
            [DaPayloadsDB.Columns.WITHDRAWAL_COUNT]: 0n,
            [DaPayloadsDB.Columns.FORCED_TRANSACTION_COUNT]: 0n,
            [DaPayloadsDB.Columns.L2_TRANSACTION_COUNT]: 0n,
            [DaPayloadsDB.Columns.DEPOSIT_COUNT]: 0n,
            [DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]: 0n,
            [DaPayloadsDB.Columns.TRANSITION_STEP_COUNT]: 0n,
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
    const emptyRoots = {
      utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    };
    const emptyExpectedRoots = {
      ...emptyRoots,
      transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
    };
    const emptyExpectedCounts = {
      withdrawalCount: 0n,
      forcedTransactionCount: 0n,
      l2TransactionCount: 0n,
      depositCount: 0n,
      totalEventCount: 0n,
      transitionStepCount: 0n,
    };
    const header: SDK.Header = {
      prevUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
      utxosRoot: emptyExpectedRoots.utxosRoot,
      withdrawalsRoot: emptyExpectedRoots.withdrawalsRoot,
      forcedTransactionsRoot: emptyExpectedRoots.forcedTransactionsRoot,
      transactionsRoot: emptyExpectedRoots.transactionsRoot,
      depositsRoot: emptyExpectedRoots.depositsRoot,
      transitionTraceRoot: emptyExpectedRoots.transitionTraceRoot,
      eventToStepRoot: emptyExpectedRoots.eventToStepRoot,
      withdrawalCount: emptyExpectedCounts.withdrawalCount,
      forcedTransactionCount: emptyExpectedCounts.forcedTransactionCount,
      l2TransactionCount: emptyExpectedCounts.l2TransactionCount,
      depositCount: emptyExpectedCounts.depositCount,
      totalEventCount: emptyExpectedCounts.totalEventCount,
      transitionStepCount: emptyExpectedCounts.transitionStepCount,
      startTime: 1n,
      endTime: 2n,
      prevHeaderHash: "11".repeat(28),
      operatorVkey: "22".repeat(28),
      protocolVersion: 1n,
    };
    return {
      headerHash,
      headerCbor: Buffer.from(
        LucidData.to(header as never, SDK.Header as never),
        "hex",
      ),
      metadata: {
        stateQueueLeaseToken: "lease-token",
        baseSnapshotId: "snapshot",
        baseTailOutRef: "base#0",
        baseTailHeaderHash: databaseFixtureBytes("base-tail-header", 28),
        baseTailDatumCbor: "d87980",
        baseRoots: emptyRoots,
        blockStartTime,
        expectedRoots: emptyExpectedRoots,
        expectedCounts: emptyExpectedCounts,
      },
      blockEndTime: new Date(blockStartTime.getTime() + 60_000),
      depositEventIds: [],
      depositEntries: [],
      forcedTransactionEventIds: [],
      forcedTransactionEntries: [],
      withdrawalEventIds: [],
      withdrawalEntries: [],
      mempoolTxIds: [],
      mempoolTxs: [],
      mempoolTxSourceTable: "none",
      transitionTraceMembers: [],
      eventToStepMembers: [],
      utxoEntries: [],
    };
  };
  const speculativeCandidateEventSnapshot = {
    candidateEndTime: new Date("2100-01-01T00:00:00.000Z"),
    excludedUserEventIds: {
      depositEventIds: new Set<string>(),
      forcedTransactionEventIds: new Set<string>(),
      withdrawalEventIds: new Set<string>(),
    },
  } as const;

  it.effect(
    "can discard and replace no-submission pending journals for retry recovery",
    (_) =>
      isolatedDb(
        Effect.gen(function* () {
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
              typeof active.value[
                PendingBlockFinalizationsDB.Columns.EXPECTED_TOTAL_EVENT_COUNT
              ],
            ).toBe("bigint");
            expect(
              active.value[
                PendingBlockFinalizationsDB.Columns.EXPECTED_TOTAL_EVENT_COUNT
              ],
            ).toBe(0n);
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
    "retrieves lease-token journal evidence across active and abandoned statuses",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const leaseToken = "all-status-lease-token";
          const headerHash = databaseFixtureBytes(
            "all-status-lease-token-header",
            28,
          );
          const input = pendingSubmissionFixture(headerHash);
          yield* PendingBlockFinalizationsDB.preparePendingSubmission({
            ...input,
            metadata: {
              ...input.metadata,
              stateQueueLeaseToken: leaseToken,
            },
          });

          const pending =
            yield* PendingBlockFinalizationsDB.retrieveByStateQueueLeaseToken(
              leaseToken,
            );
          expect(pending).toHaveLength(1);
          expect(pending[0]?.[PendingBlockFinalizationsDB.Columns.STATUS]).toBe(
            PendingBlockFinalizationsDB.Status.PendingSubmission,
          );

          yield* PendingBlockFinalizationsDB.markAbandoned(headerHash);
          expect(
            yield* PendingBlockFinalizationsDB.retrieveActiveByStateQueueLeaseToken(
              leaseToken,
            ),
          ).toEqual([]);

          const allStatuses =
            yield* PendingBlockFinalizationsDB.retrieveByStateQueueLeaseToken(
              leaseToken,
            );
          expect(allStatuses).toHaveLength(1);
          expect(
            allStatuses[0]?.[PendingBlockFinalizationsDB.Columns.STATUS],
          ).toBe(PendingBlockFinalizationsDB.Status.Abandoned);
          expect(
            yield* PendingBlockFinalizationsDB.retrieveByStateQueueLeaseToken(
              "missing-lease-token",
            ),
          ).toEqual([]);
        }),
      ),
  );

  it.effect(
    "round-trips Architecture G replay journals and rejects inconsistent root counts",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const headerHash = databaseFixtureBytes(
            "architecture-g-replay-header",
            28,
          );
          const eventLog = Buffer.alloc(92, 7);
          const replay = {
            schema: 1 as const,
            ownerBinarySha256: databaseFixtureBytes(
              "architecture-g-binary-sha",
              32,
            ),
            baseRoot: databaseFixtureBytes("architecture-g-base-root", 32),
            candidateRoot: databaseFixtureBytes(
              "architecture-g-candidate-root",
              32,
            ),
            eventLog,
            eventLogDigest: databaseFixtureBytes(
              "architecture-g-event-digest",
              32,
            ),
            eventRoots: Buffer.concat([
              databaseFixtureBytes("architecture-g-event-root-0", 32),
              databaseFixtureBytes("architecture-g-event-root-1", 32),
            ]),
            eventCount: 2,
          };
          yield* PendingBlockFinalizationsDB.preparePendingSubmission({
            ...pendingSubmissionFixture(headerHash),
            nativeMpfReplay: replay,
          });
          const active = yield* PendingBlockFinalizationsDB.retrieveActive();
          expect(active._tag).toBe("Some");
          if (Option.isSome(active)) {
            expect(active.value.nativeMpfReplay).toEqual(replay);
          }

          yield* PendingBlockFinalizationsDB.discardUnsubmittedPendingSubmission(
            headerHash,
          );
          const invalid = yield* Effect.either(
            PendingBlockFinalizationsDB.preparePendingSubmission({
              ...pendingSubmissionFixture(
                databaseFixtureBytes("architecture-g-invalid-header", 28),
              ),
              nativeMpfReplay: {
                ...replay,
                eventCount: 3,
              },
            }),
          );
          expect(invalid._tag).toBe("Left");
        }),
      ),
  );

  it.effect(
    "only abandons pending-submission journals that still have no submitted tx hash",
    (_) =>
      isolatedDb(
        Effect.gen(function* () {
          const submittedHeaderHash = databaseFixtureBytes(
            "submitted-cas-pending-header",
            28,
          );
          const unsubmittedHeaderHash = databaseFixtureBytes(
            "unsubmitted-cas-pending-header",
            28,
          );
          const submittedTxHash = databaseTxHash("submitted-cas-tx");

          yield* PendingBlockFinalizationsDB.preparePendingSubmission(
            pendingSubmissionFixture(unsubmittedHeaderHash),
          );
          const unsubmittedAbandoned =
            yield* PendingBlockFinalizationsDB.markUnsubmittedAbandoned(
              unsubmittedHeaderHash,
            );
          expect(unsubmittedAbandoned).toBe(true);

          const unsubmitted =
            yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
              unsubmittedHeaderHash,
            );
          expect(unsubmitted._tag).toBe("Some");
          if (unsubmitted._tag === "Some") {
            expect(
              unsubmitted.value[PendingBlockFinalizationsDB.Columns.STATUS],
            ).toBe(PendingBlockFinalizationsDB.Status.Abandoned);
            expect(
              unsubmitted.value[
                PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH
              ],
            ).toBeNull();
          }

          yield* PendingBlockFinalizationsDB.preparePendingSubmission(
            pendingSubmissionFixture(submittedHeaderHash),
          );
          yield* PendingBlockFinalizationsDB.markSubmitted(
            submittedHeaderHash,
            submittedTxHash,
          );
          const submittedAbandoned =
            yield* PendingBlockFinalizationsDB.markUnsubmittedAbandoned(
              submittedHeaderHash,
            );
          expect(submittedAbandoned).toBe(false);

          const submitted =
            yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
              submittedHeaderHash,
            );
          expect(submitted._tag).toBe("Some");
          if (submitted._tag === "Some") {
            expect(
              submitted.value[PendingBlockFinalizationsDB.Columns.STATUS],
            ).toBe(
              PendingBlockFinalizationsDB.Status
                .SubmittedLocalFinalizationPending,
            );
            expect(
              submitted.value[
                PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH
              ],
            ).toEqual(submittedTxHash);
          }
        }),
      ),
  );

  it.effect(
    "keeps only the submitted base journal across all memory-only speculative crash checkpoints",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const headerHash = databaseFixtureBytes(
            "speculative-crash-base-header",
            28,
          );
          const submittedTxHash = databaseTxHash(
            "speculative-crash-base-submission",
          );
          yield* PendingBlockFinalizationsDB.preparePendingSubmission(
            pendingSubmissionFixture(headerHash),
          );
          yield* PendingBlockFinalizationsDB.markSubmitted(
            headerHash,
            submittedTxHash,
          );

          const memoryOnlyCheckpoints: readonly string[] = [
            "mid_build",
            "candidate_ready",
          ];
          for (const checkpoint of memoryOnlyCheckpoints) {
            const active = yield* PendingBlockFinalizationsDB.retrieveActive();
            expect(active._tag, checkpoint).toBe("Some");
            if (Option.isSome(active)) {
              expect(
                active.value[PendingBlockFinalizationsDB.Columns.HEADER_HASH],
                checkpoint,
              ).toEqual(headerHash);
              expect(
                active.value[
                  PendingBlockFinalizationsDB.Columns.SUBMITTED_TX_HASH
                ],
                checkpoint,
              ).toEqual(submittedTxHash);
            }
          }

          // Confirmation wake is durable only as the existing N journal's
          // recovery transition. A crash before N+1 journal preparation still
          // leaves no speculative or second active row.
          yield* PendingBlockFinalizationsDB.markObservedWaitingStability(
            headerHash,
            BigInt(Date.now()),
            submittedTxHash,
          );
          const afterWake = yield* PendingBlockFinalizationsDB.retrieveActive();
          expect(Option.isSome(afterWake)).toBe(true);
          if (Option.isSome(afterWake)) {
            expect(
              afterWake.value[PendingBlockFinalizationsDB.Columns.HEADER_HASH],
            ).toEqual(headerHash);
            expect(
              afterWake.value[PendingBlockFinalizationsDB.Columns.STATUS],
            ).toBe(PendingBlockFinalizationsDB.Status.ObservedWaitingStability);
          }
          const sql = yield* SqlClient.SqlClient;
          const [{ activeCount }] = yield* sql<{
            readonly activeCount: number;
          }>`SELECT COUNT(*)::int AS "activeCount"
            FROM ${sql(PendingBlockFinalizationsDB.tableName)}
            WHERE status IN (
              ${PendingBlockFinalizationsDB.Status.PendingSubmission},
              ${PendingBlockFinalizationsDB.Status.SubmittedLocalFinalizationPending},
              ${PendingBlockFinalizationsDB.Status.ObservedWaitingStability}
            )`;
          expect(activeCount).toBe(1);
        }),
      ),
  );

  it.effect(
    "atomically couples speculative projection writes to pending-journal preparation",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const makeInput = (
            headerLabel: string,
            deposit: DepositsDB.Entry,
          ) => {
            const base = pendingSubmissionFixture(
              databaseFixtureBytes(headerLabel, 28),
            );
            return {
              ...base,
              depositEventIds: [deposit[DepositsDB.Columns.ID]],
              depositEntries: [deposit],
            };
          };

          // Crash before prepare: neither projection nor a candidate journal
          // exists because candidate-ready is entirely memory-only.
          const beforeDeposit = makeDepositEntry();
          yield* DepositsDB.insertEntries([beforeDeposit]);
          const beforeRows = yield* DepositsDB.retrieveAllEntries();
          const beforeAfter = beforeRows.find((entry) =>
            entry[DepositsDB.Columns.ID].equals(
              beforeDeposit[DepositsDB.Columns.ID],
            ),
          );
          expect(beforeAfter?.[DepositsDB.Columns.STATUS]).toBe(
            DepositsDB.Status.Awaiting,
          );
          expect(
            Option.isNone(
              yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
                databaseFixtureBytes("atomic-before-header", 28),
              ),
            ),
          ).toBe(true);

          // Crash/failure during prepare: a deferred projection write and the
          // journal insert share one SQL transaction, so both roll back.
          const duringDeposit = makeDepositEntry();
          yield* DepositsDB.insertEntries([duringDeposit]);
          const duringInput = makeInput("atomic-during-header", duringDeposit);
          const duringResult = yield* Effect.either(
            PendingBlockFinalizationsDB.preparePendingSubmission(duringInput, {
              beforeJournalInsert: DepositsDB.markAwaitingAsProjected([
                duringDeposit[DepositsDB.Columns.ID],
              ]).pipe(
                Effect.andThen(
                  Effect.fail(
                    new DatabaseError({
                      table: PendingBlockFinalizationsDB.tableName,
                      message: "Injected crash during journal preparation",
                      cause: "crash_during_prepare",
                    }),
                  ),
                ),
              ),
            }),
          );
          expect(duringResult._tag).toBe("Left");
          const duringRows = yield* DepositsDB.retrieveAllEntries();
          const duringAfter = duringRows.find((entry) =>
            entry[DepositsDB.Columns.ID].equals(
              duringDeposit[DepositsDB.Columns.ID],
            ),
          );
          expect(duringAfter?.[DepositsDB.Columns.STATUS]).toBe(
            DepositsDB.Status.Awaiting,
          );
          expect(
            Option.isNone(
              yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
                duringInput.headerHash,
              ),
            ),
          ).toBe(true);

          // Crash immediately after prepare: the projection and its complete
          // pending journal are both durable, never only one of the pair.
          const afterDeposit = makeDepositEntry();
          yield* DepositsDB.insertEntries([afterDeposit]);
          const afterInput = makeInput("atomic-after-header", afterDeposit);
          yield* PendingBlockFinalizationsDB.preparePendingSubmission(
            afterInput,
            {
              beforeJournalInsert: DepositsDB.markAwaitingAsProjected([
                afterDeposit[DepositsDB.Columns.ID],
              ]),
            },
          );
          const afterRows = yield* DepositsDB.retrieveAllEntries();
          const afterProjected = afterRows.find((entry) =>
            entry[DepositsDB.Columns.ID].equals(
              afterDeposit[DepositsDB.Columns.ID],
            ),
          );
          expect(afterProjected?.[DepositsDB.Columns.STATUS]).toBe(
            DepositsDB.Status.Projected,
          );
          const afterJournal =
            yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
              afterInput.headerHash,
            );
          expect(Option.isSome(afterJournal)).toBe(true);
          if (Option.isSome(afterJournal)) {
            expect(afterJournal.value.depositEventIds).toEqual([
              afterDeposit[DepositsDB.Columns.ID],
            ]);
          }
        }),
      ),
  );

  vitestIt(
    "survives real process kills before, during, and after atomic journal prepare",
    async () => {
      const deposits = [
        makeDepositEntry(),
        makeDepositEntry(),
        makeDepositEntry(),
      ];
      await Effect.runPromise(isolatedDb(DepositsDB.insertEntries(deposits)));
      const helper = bundleChildProcessHelper(
        "helpers/pending-journal-crash-process.ts",
      );
      const cwd = path.resolve(__dirname, "..");
      const checkpoints = ["before", "during", "after"] as const;
      try {
        for (const [index, checkpoint] of checkpoints.entries()) {
          const deposit = deposits[index]!;
          const headerHash = databaseFixtureBytes(
            `speculative-process-crash-${checkpoint}`,
            28,
          );
          const child = spawn(
            process.execPath,
            [
              helper,
              checkpoint,
              deposit[DepositsDB.Columns.ID].toString("hex"),
              headerHash.toString("hex"),
            ],
            {
              cwd,
              env: databaseChildProcessEnv(),
              shell: false,
              stdio: ["ignore", "pipe", "pipe"],
            },
          );
          const result = await collectChildProcess(child);
          expect(result.signal, result.stderr).toBe("SIGKILL");

          const snapshot = await Effect.runPromise(
            provideDatabaseLayers(
              Effect.all({
                rows: DepositsDB.retrieveAllEntries(),
                journal:
                  PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash),
              }),
            ),
          );
          const persistedDeposit = snapshot.rows.find((entry) =>
            entry[DepositsDB.Columns.ID].equals(deposit[DepositsDB.Columns.ID]),
          );
          if (checkpoint === "after") {
            expect(persistedDeposit?.[DepositsDB.Columns.STATUS]).toBe(
              DepositsDB.Status.Projected,
            );
            expect(Option.isSome(snapshot.journal)).toBe(true);
          } else {
            expect(persistedDeposit?.[DepositsDB.Columns.STATUS]).toBe(
              DepositsDB.Status.Awaiting,
            );
            expect(Option.isNone(snapshot.journal)).toBe(true);
          }
        }
      } finally {
        fs.rmSync(helper, { force: true });
      }
    },
    30_000,
  );

  it.effect(
    "fails closed when a same-count speculative source changes before atomic prepare",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const candidateDeposit = makeDepositEntry();
          yield* DepositsDB.insertEntries([candidateDeposit]);
          const candidateRoot = yield* resolveDepositsRoot([candidateDeposit]);
          expect(Option.isSome(candidateRoot)).toBe(true);
          if (Option.isNone(candidateRoot)) return;

          const sql = yield* SqlClient.SqlClient;
          const changedInfo = databaseFixtureBytes(
            "speculative-source-changed-info",
            48,
          );
          yield* sql`UPDATE ${sql(DepositsDB.tableName)}
            SET ${sql(DepositsDB.Columns.INFO)} = ${changedInfo}
            WHERE ${sql(DepositsDB.Columns.ID)} = ${candidateDeposit[DepositsDB.Columns.ID]}`;

          const inputBase = pendingSubmissionFixture(
            databaseFixtureBytes("speculative-source-changed-header", 28),
          );
          const input = {
            ...inputBase,
            depositEventIds: [candidateDeposit[DepositsDB.Columns.ID]],
            depositEntries: [candidateDeposit],
          };
          const attempt = yield* Effect.either(
            StateQueueMutationLeasesDB.tryWithLease(
              "speculative-source-revalidation-test",
              (stateQueueLeaseToken) =>
                MpfEngineStateDB.tryWithLedgerStoreLease(
                  "speculative-source-revalidation-test",
                  (activeMpfLeaseOwner) =>
                    PendingBlockFinalizationsDB.preparePendingSubmission(
                      input,
                      {
                        beforeJournalInsert:
                          revalidateAndPersistSpeculativeCandidateSources({
                            includedDepositEntries: [candidateDeposit],
                            includedForcedTransactionEntries: [],
                            includedWithdrawalEntries: [],
                            selectedMempoolTxs: [],
                            rejectedMempoolTxs: [],
                            mempoolTxSourceTable: "none",
                            rejectionEntries: [],
                            expectedEventRoots: {
                              deposits: candidateRoot.value,
                              forcedTransactions: SDK.EMPTY_MERKLE_TREE_ROOT,
                              withdrawals: SDK.EMPTY_MERKLE_TREE_ROOT,
                            },
                            ...speculativeCandidateEventSnapshot,
                            stateQueueLeaseToken,
                            activeMpfLeaseOwner,
                          }),
                      },
                    ),
                ),
            ),
          );
          expect(attempt._tag).toBe("Left");
          expect(
            Option.isNone(
              yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
                input.headerHash,
              ),
            ),
          ).toBe(true);
          const rows = yield* DepositsDB.retrieveAllEntries();
          const retained = rows.find((entry) =>
            entry[DepositsDB.Columns.ID].equals(
              candidateDeposit[DepositsDB.Columns.ID],
            ),
          );
          expect(retained?.[DepositsDB.Columns.INFO]).toEqual(changedInfo);
          expect(retained?.[DepositsDB.Columns.STATUS]).toBe(
            DepositsDB.Status.Awaiting,
          );
        }),
      ),
  );

  it.effect(
    "rejects changed inline mempool payloads and duplicate selected/rejected ids",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const txId = databaseTxHash("speculative-accepted-payload");
          const txCbor = databaseFixtureBytes(
            "speculative-accepted-payload-cbor",
            96,
          );
          yield* TxAdmissionsDB.tryInsert({
            txId,
            txCanonicalCbor: txCbor,
            submitSource: "native",
          });
          yield* sql`INSERT INTO ${sql(MempoolDB.tableName)} (
              ${sql(TxUtils.Columns.TX_ID)},
              ${sql(TxUtils.Columns.TX)}
            ) VALUES (${txId}, ${txCbor})`;
          const page = yield* MempoolDB.retrievePage({ limit: 10 });
          const candidate = page.entries.find((entry) =>
            entry[TxUtils.Columns.TX_ID].equals(txId),
          );
          expect(candidate).toBeDefined();
          if (candidate === undefined) return;
          expect(candidate[TxUtils.Columns.TX]).toEqual(txCbor);

          // Changing the authoritative inline payload after candidate-ready
          // must fail exact source revalidation even though membership and
          // transaction count are unchanged.
          const changedTxCbor = databaseFixtureBytes(
            "speculative-changed-inline-payload-cbor",
            96,
          );
          yield* sql`UPDATE ${sql(MempoolDB.tableName)}
            SET ${sql(TxUtils.Columns.TX)} = ${changedTxCbor}
            WHERE ${sql(TxUtils.Columns.TX_ID)} = ${txId}`;
          const changedPayload = yield* Effect.either(
            StateQueueMutationLeasesDB.tryWithLease(
              "speculative-changed-inline-payload-test",
              (stateQueueLeaseToken) =>
                MpfEngineStateDB.tryWithLedgerStoreLease(
                  "speculative-changed-inline-payload-test",
                  (activeMpfLeaseOwner) =>
                    revalidateAndPersistSpeculativeCandidateSources({
                      includedDepositEntries: [],
                      includedForcedTransactionEntries: [],
                      includedWithdrawalEntries: [],
                      selectedMempoolTxs: [candidate],
                      rejectedMempoolTxs: [],
                      mempoolTxSourceTable: MempoolDB.tableName,
                      rejectionEntries: [],
                      expectedEventRoots: {
                        deposits: SDK.EMPTY_MERKLE_TREE_ROOT,
                        forcedTransactions: SDK.EMPTY_MERKLE_TREE_ROOT,
                        withdrawals: SDK.EMPTY_MERKLE_TREE_ROOT,
                      },
                      ...speculativeCandidateEventSnapshot,
                      stateQueueLeaseToken,
                      activeMpfLeaseOwner,
                    }),
                ),
            ),
          );
          expect(changedPayload._tag).toBe("Left");

          const duplicateTxId = databaseTxHash("speculative-duplicate-union");
          yield* ProcessedMempoolDB.insertTx({
            [TxUtils.Columns.TX_ID]: duplicateTxId,
            [TxUtils.Columns.TX]: databaseFixtureBytes(
              "speculative-duplicate-union-cbor",
              96,
            ),
          });
          const processed = yield* ProcessedMempoolDB.retrieve;
          const duplicateCandidate = processed.find((entry) =>
            entry[TxUtils.Columns.TX_ID].equals(duplicateTxId),
          );
          expect(duplicateCandidate).toBeDefined();
          if (duplicateCandidate === undefined) return;
          const duplicateUnion = yield* Effect.either(
            StateQueueMutationLeasesDB.tryWithLease(
              "speculative-duplicate-union-test",
              (stateQueueLeaseToken) =>
                MpfEngineStateDB.tryWithLedgerStoreLease(
                  "speculative-duplicate-union-test",
                  (activeMpfLeaseOwner) =>
                    revalidateAndPersistSpeculativeCandidateSources({
                      includedDepositEntries: [],
                      includedForcedTransactionEntries: [],
                      includedWithdrawalEntries: [],
                      selectedMempoolTxs: [duplicateCandidate],
                      rejectedMempoolTxs: [duplicateCandidate],
                      mempoolTxSourceTable: ProcessedMempoolDB.tableName,
                      rejectionEntries: [
                        {
                          [TxRejectionsDB.Columns.TX_ID]: duplicateTxId,
                          [TxRejectionsDB.Columns.REJECT_CODE]:
                            "E_TEST_DUPLICATE_UNION",
                          [TxRejectionsDB.Columns.REJECT_DETAIL]:
                            "duplicate selected/rejected id test",
                        },
                      ],
                      expectedEventRoots: {
                        deposits: SDK.EMPTY_MERKLE_TREE_ROOT,
                        forcedTransactions: SDK.EMPTY_MERKLE_TREE_ROOT,
                        withdrawals: SDK.EMPTY_MERKLE_TREE_ROOT,
                      },
                      ...speculativeCandidateEventSnapshot,
                      stateQueueLeaseToken,
                      activeMpfLeaseOwner,
                    }),
                ),
            ),
          );
          expect(duplicateUnion._tag).toBe("Left");
        }),
      ),
  );

  it.effect(
    "rolls back projections, ledger reconciliation and rejections when journal member insertion fails",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const deposit = makeDepositEntry();
          yield* DepositsDB.insertEntries([deposit]);
          const depositRoot = yield* resolveDepositsRoot([deposit]);
          expect(Option.isSome(depositRoot)).toBe(true);
          if (Option.isNone(depositRoot)) return;

          const rejectedTxId = databaseTxHash("speculative-atomic-rejected-tx");
          yield* ProcessedMempoolDB.insertTx({
            [TxUtils.Columns.TX_ID]: rejectedTxId,
            [TxUtils.Columns.TX]: databaseFixtureBytes(
              "speculative-atomic-rejected-cbor",
              96,
            ),
          });
          const rejectedTx = (yield* ProcessedMempoolDB.retrieve).find(
            (entry) => entry[TxUtils.Columns.TX_ID].equals(rejectedTxId),
          );
          expect(rejectedTx).toBeDefined();
          if (rejectedTx === undefined) return;

          const base = pendingSubmissionFixture(
            databaseFixtureBytes("speculative-atomic-failing-header", 28),
          );
          // Duplicate journal members pass set-equality preflight but violate
          // the (header_hash, member_id) primary key after the deferred writes
          // have run, forcing a transaction rollback at the sharp boundary.
          const failingInput = {
            ...base,
            depositEventIds: [
              deposit[DepositsDB.Columns.ID],
              deposit[DepositsDB.Columns.ID],
            ],
            depositEntries: [deposit, deposit],
          };
          const result = yield* Effect.either(
            StateQueueMutationLeasesDB.tryWithLease(
              "speculative-atomic-rollback-test",
              (stateQueueLeaseToken) =>
                MpfEngineStateDB.tryWithLedgerStoreLease(
                  "speculative-atomic-rollback-test",
                  (activeMpfLeaseOwner) =>
                    PendingBlockFinalizationsDB.preparePendingSubmission(
                      failingInput,
                      {
                        beforeJournalInsert:
                          revalidateAndPersistSpeculativeCandidateSources({
                            includedDepositEntries: [deposit],
                            includedForcedTransactionEntries: [],
                            includedWithdrawalEntries: [],
                            selectedMempoolTxs: [],
                            rejectedMempoolTxs: [rejectedTx],
                            mempoolTxSourceTable: ProcessedMempoolDB.tableName,
                            rejectionEntries: [
                              {
                                [TxRejectionsDB.Columns.TX_ID]: rejectedTxId,
                                [TxRejectionsDB.Columns.REJECT_CODE]:
                                  "E_TEST_ATOMIC_ROLLBACK",
                                [TxRejectionsDB.Columns.REJECT_DETAIL]:
                                  "injected rejected tx for rollback test",
                              },
                            ],
                            expectedEventRoots: {
                              deposits: depositRoot.value,
                              forcedTransactions: SDK.EMPTY_MERKLE_TREE_ROOT,
                              withdrawals: SDK.EMPTY_MERKLE_TREE_ROOT,
                            },
                            ...speculativeCandidateEventSnapshot,
                            stateQueueLeaseToken,
                            activeMpfLeaseOwner,
                          }),
                      },
                    ),
                ),
            ),
          );
          expect(result._tag).toBe("Left");
          expect(
            Option.isNone(
              yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
                failingInput.headerHash,
              ),
            ),
          ).toBe(true);
          const deposits = yield* DepositsDB.retrieveAllEntries();
          const retainedDeposit = deposits.find((entry) =>
            entry[DepositsDB.Columns.ID].equals(deposit[DepositsDB.Columns.ID]),
          );
          expect(retainedDeposit?.[DepositsDB.Columns.STATUS]).toBe(
            DepositsDB.Status.Awaiting,
          );
          expect(
            yield* MempoolLedgerDB.retrieveBySourceEventIds([
              deposit[DepositsDB.Columns.ID],
            ]),
          ).toHaveLength(0);
          expect(
            (yield* ProcessedMempoolDB.retrieve).some((entry) =>
              entry[TxUtils.Columns.TX_ID].equals(rejectedTxId),
            ),
          ).toBe(true);
          expect(
            yield* TxRejectionsDB.retrieveByTxId(rejectedTxId),
          ).toHaveLength(0);
        }),
      ),
  );

  it.effect(
    "persists an in-memory withdrawal classification atomically with its journal",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const withdrawalId = databaseOutputReferenceId(
            "speculative-memory-withdrawal",
            0n,
          );
          const currentWithdrawal: WithdrawalsDB.Entry = {
            [WithdrawalsDB.Columns.ID]: withdrawalId,
            [WithdrawalsDB.Columns.RAW_EVENT_INFO]: databaseFixtureBytes(
              "speculative-memory-withdrawal-raw",
              96,
            ),
            [WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]: null,
            [WithdrawalsDB.Columns.INCLUSION_TIME]: new Date(
              "2026-04-13T18:00:00.000Z",
            ),
            [WithdrawalsDB.Columns.WITHDRAWAL_L1_TX_HASH]: databaseTxHash(
              "speculative-memory-withdrawal-l1",
            ),
            [WithdrawalsDB.Columns.WITHDRAWAL_L1_OUTPUT_INDEX]: 0,
            [WithdrawalsDB.Columns.ASSET_NAME]: databaseFixtureBytes(
              "speculative-memory-withdrawal-asset",
              32,
            ),
            [WithdrawalsDB.Columns.L2_OUTREF]: databaseOutputReferenceId(
              "speculative-memory-withdrawal-l2",
              1n,
            ),
            [WithdrawalsDB.Columns.L2_OWNER]: databaseFixtureBytes(
              "speculative-memory-withdrawal-owner",
              28,
            ),
            [WithdrawalsDB.Columns.L2_VALUE]: databaseFixtureBytes(
              "speculative-memory-withdrawal-value",
              48,
            ),
            [WithdrawalsDB.Columns.L1_ADDRESS]: databaseFixtureBytes(
              "speculative-memory-withdrawal-address",
              32,
            ),
            [WithdrawalsDB.Columns.L1_DATUM]: databaseFixtureBytes(
              "speculative-memory-withdrawal-datum",
              16,
            ),
            [WithdrawalsDB.Columns.REFUND_ADDRESS]: databaseFixtureBytes(
              "speculative-memory-withdrawal-refund-address",
              32,
            ),
            [WithdrawalsDB.Columns.REFUND_DATUM]: databaseFixtureBytes(
              "speculative-memory-withdrawal-refund-datum",
              16,
            ),
            [WithdrawalsDB.Columns.VALIDITY]: null,
            [WithdrawalsDB.Columns.VALIDITY_DETAIL]: {},
            [WithdrawalsDB.Columns.PROJECTED_HEADER_HASH]: null,
            [WithdrawalsDB.Columns.STATUS]: WithdrawalsDB.Status.Awaiting,
          };
          yield* WithdrawalsDB.insertEntries([currentWithdrawal]);
          const settlementEventInfo = databaseFixtureBytes(
            "speculative-memory-withdrawal-settlement",
            64,
          );
          const candidateWithdrawal: WithdrawalsDB.Entry = {
            ...currentWithdrawal,
            [WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO]: settlementEventInfo,
            [WithdrawalsDB.Columns.VALIDITY]:
              WithdrawalsDB.Validity.WithdrawalIsValid,
            [WithdrawalsDB.Columns.VALIDITY_DETAIL]: {
              source: "speculative-memory",
            },
            [WithdrawalsDB.Columns.STATUS]: WithdrawalsDB.Status.Projected,
          };
          const withdrawalRoot = yield* resolveWithdrawalsRoot([
            candidateWithdrawal,
          ]);
          expect(Option.isSome(withdrawalRoot)).toBe(true);
          if (Option.isNone(withdrawalRoot)) return;

          const base = pendingSubmissionFixture(
            databaseFixtureBytes("speculative-memory-withdrawal-header", 28),
          );
          const input = {
            ...base,
            withdrawalEventIds: [withdrawalId],
            withdrawalEntries: [candidateWithdrawal],
          };
          const prepared = yield* StateQueueMutationLeasesDB.tryWithLease(
            "speculative-memory-withdrawal-test",
            (stateQueueLeaseToken) =>
              MpfEngineStateDB.tryWithLedgerStoreLease(
                "speculative-memory-withdrawal-test",
                (activeMpfLeaseOwner) =>
                  PendingBlockFinalizationsDB.preparePendingSubmission(input, {
                    beforeJournalInsert:
                      revalidateAndPersistSpeculativeCandidateSources({
                        includedDepositEntries: [],
                        includedForcedTransactionEntries: [],
                        includedWithdrawalEntries: [candidateWithdrawal],
                        selectedMempoolTxs: [],
                        rejectedMempoolTxs: [],
                        mempoolTxSourceTable: "none",
                        rejectionEntries: [],
                        expectedEventRoots: {
                          deposits: SDK.EMPTY_MERKLE_TREE_ROOT,
                          forcedTransactions: SDK.EMPTY_MERKLE_TREE_ROOT,
                          withdrawals: withdrawalRoot.value,
                        },
                        ...speculativeCandidateEventSnapshot,
                        stateQueueLeaseToken,
                        activeMpfLeaseOwner,
                      }),
                  }),
              ),
          );
          expect(prepared._tag).toBe("Ran");
          if (prepared._tag === "Ran") {
            expect(prepared.value._tag).toBe("Ran");
          }
          const persisted =
            yield* WithdrawalsDB.retrieveByEventId(withdrawalId);
          expect(Option.isSome(persisted)).toBe(true);
          if (Option.isSome(persisted)) {
            expect(
              persisted.value[WithdrawalsDB.Columns.SETTLEMENT_EVENT_INFO],
            ).toEqual(settlementEventInfo);
            expect(persisted.value[WithdrawalsDB.Columns.STATUS]).toBe(
              WithdrawalsDB.Status.Projected,
            );
          }
          expect(
            Option.isSome(
              yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
                input.headerHash,
              ),
            ),
          ).toBe(true);
        }),
      ),
  );
});

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
          yield* flushAll;
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
            tx: databaseFixtureBytes(`mempool.keyset-${index.toString()}`, 64),
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
          expect(snapshotRows.map((row) => row.tx_id.toString("hex"))).toEqual(
            expected
              .filter(
                (row) => row.time_stamp_tz.getTime() <= snapshotBound.getTime(),
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
            // must roll the delta back and retain both queued rows.
            yield* sql`DROP TABLE address_history`;
            const failed = yield* Effect.either(writeBehind.flushNow);
            expect(failed._tag).toBe("Left");
            expect((yield* writeBehind.depths).totalDepth).toBe(2);
            expect(
              (yield* MempoolTxDeltasDB.retrieveByTxIds([txId])).size,
            ).toBe(0);

            yield* AddressHistoryDB.createTable;
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
            yield* sql`DROP TABLE mempool_tx_deltas`;
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

            yield* MempoolTxDeltasDB.createTable;
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
              (yield* MempoolTxDeltasDB.retrieveByTxIds([first, second])).size,
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
        expect((yield* MempoolTxDeltasDB.retrieveByTxIds([txId])).size).toBe(0);
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
          stored[0][TxUtils.Columns.TX_ID].equals(valid[TxUtils.Columns.TX_ID]),
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
    "persists exact signed bytes before submission and rejects immutable payload drift",
    (_) =>
      isolatedDb(
        Effect.gen(function* () {
          const attempt = makeDepositSubmissionAttempt();
          const first =
            yield* DepositSubmissionAttemptsDB.insertPrepared(attempt);
          const second =
            yield* DepositSubmissionAttemptsDB.insertPrepared(attempt);

          expect(
            first[DepositSubmissionAttemptsDB.Columns.TX_HASH].equals(
              second[DepositSubmissionAttemptsDB.Columns.TX_HASH],
            ),
          ).toEqual(true);
          expect(
            first[DepositSubmissionAttemptsDB.Columns.SIGNED_TX_CBOR].equals(
              attempt[DepositSubmissionAttemptsDB.Columns.SIGNED_TX_CBOR],
            ),
          ).toEqual(true);
          expect(first[DepositSubmissionAttemptsDB.Columns.STATUS]).toEqual(
            DepositSubmissionAttemptsDB.Status.Prepared,
          );
          expect(
            first[DepositSubmissionAttemptsDB.Columns.ATTEMPT_COUNT],
          ).toEqual(0);
          expect(
            first[DepositSubmissionAttemptsDB.Columns.LAST_SUBMISSION_AT],
          ).toBeNull();

          const signedBytesConflict = yield* Effect.either(
            DepositSubmissionAttemptsDB.insertPrepared({
              ...attempt,
              [DepositSubmissionAttemptsDB.Columns.SIGNED_TX_CBOR]:
                databaseFixtureBytes("deposit-submission.different-signed", 96),
            }),
          );
          expect(signedBytesConflict._tag).toEqual("Left");

          const payloadConflict = yield* Effect.either(
            DepositSubmissionAttemptsDB.insertPrepared({
              ...attempt,
              [DepositSubmissionAttemptsDB.Columns.EXPECTED_LOVELACE]: "2",
            }),
          );
          expect(payloadConflict._tag).toEqual("Left");

          const stored = Option.getOrThrow(
            yield* DepositSubmissionAttemptsDB.retrieveByTxHash(
              attempt[DepositSubmissionAttemptsDB.Columns.TX_HASH],
            ),
          );
          expect(
            stored[DepositSubmissionAttemptsDB.Columns.SIGNED_TX_CBOR].equals(
              attempt[DepositSubmissionAttemptsDB.Columns.SIGNED_TX_CBOR],
            ),
          ).toEqual(true);
          expect(
            stored[DepositSubmissionAttemptsDB.Columns.EXPECTED_LOVELACE],
          ).toEqual("1000000");
        }),
      ),
  );

  it.effect("stores bigint metadata as stable JSON strings", (_) =>
    isolatedDb(
      Effect.gen(function* () {
        const attempt = makeDepositSubmissionAttempt();
        const metadata = attempt[DepositSubmissionAttemptsDB.Columns.METADATA];
        const bigintAttempt: DepositSubmissionAttemptsDB.InsertPreparedInput = {
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
          yield* DepositSubmissionAttemptsDB.insertPrepared(bigintAttempt);
        const second =
          yield* DepositSubmissionAttemptsDB.insertPrepared(bigintAttempt);
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

  it.effect("atomically claims one provider submission attempt", (_) =>
    isolatedDb(
      Effect.gen(function* () {
        const attempt = makeDepositSubmissionAttempt();
        yield* DepositSubmissionAttemptsDB.insertPrepared(attempt);
        const txHash = attempt[DepositSubmissionAttemptsDB.Columns.TX_HASH];

        const claims = yield* Effect.all(
          [
            DepositSubmissionAttemptsDB.beginSubmission(txHash).pipe(
              Effect.either,
            ),
            DepositSubmissionAttemptsDB.beginSubmission(txHash).pipe(
              Effect.either,
            ),
          ],
          { concurrency: "unbounded" },
        );
        expect(claims.filter((claim) => claim._tag === "Right")).toHaveLength(
          1,
        );
        expect(claims.filter((claim) => claim._tag === "Left")).toHaveLength(1);

        const claimed = Option.getOrThrow(
          yield* DepositSubmissionAttemptsDB.retrieveByTxHash(txHash),
        );
        expect(claimed[DepositSubmissionAttemptsDB.Columns.STATUS]).toEqual(
          DepositSubmissionAttemptsDB.Status.SubmissionUnknown,
        );
        expect(
          claimed[DepositSubmissionAttemptsDB.Columns.ATTEMPT_COUNT],
        ).toEqual(1);
        expect(
          claimed[DepositSubmissionAttemptsDB.Columns.LAST_SUBMISSION_AT],
        ).not.toBeNull();

        const submitted = yield* DepositSubmissionAttemptsDB.markSubmitted(
          txHash,
          `tx:${txHash.toString("hex")}`,
        );
        expect(submitted[DepositSubmissionAttemptsDB.Columns.STATUS]).toEqual(
          DepositSubmissionAttemptsDB.Status.Submitted,
        );
        expect(
          submitted[
            DepositSubmissionAttemptsDB.Columns.PROVIDER_ACKNOWLEDGEMENT
          ],
        ).toEqual(`tx:${txHash.toString("hex")}`);
        expect(
          yield* Effect.either(
            DepositSubmissionAttemptsDB.beginSubmission(txHash),
          ),
        ).toMatchObject({ _tag: "Left" });
      }),
    ),
  );

  it.effect(
    "retrieves every unresolved lifecycle state and excludes terminal states",
    (_) =>
      isolatedDb(
        Effect.gen(function* () {
          const prepared = makeDepositSubmissionAttempt({
            txHash: databaseTxHash("deposit-submission.prepared"),
            eventId: databaseOutputReferenceId("deposit-submission.prepared"),
          });
          const unknown = makeDepositSubmissionAttempt({
            txHash: databaseTxHash("deposit-submission.unknown"),
            eventId: databaseOutputReferenceId("deposit-submission.unknown"),
          });
          const submitted = makeDepositSubmissionAttempt({
            txHash: databaseTxHash("deposit-submission.submitted"),
            eventId: databaseOutputReferenceId("deposit-submission.submitted"),
          });
          const confirmed = makeDepositSubmissionAttempt({
            txHash: databaseTxHash("deposit-submission.confirmed"),
            eventId: databaseOutputReferenceId("deposit-submission.confirmed"),
          });
          const reconciled = makeDepositSubmissionAttempt({
            txHash: databaseTxHash("deposit-submission.reconciled"),
            eventId: databaseOutputReferenceId("deposit-submission.reconciled"),
          });
          const ambiguous = makeDepositSubmissionAttempt({
            txHash: databaseTxHash("deposit-submission.ambiguous"),
            eventId: databaseOutputReferenceId("deposit-submission.ambiguous"),
          });
          const expired = makeDepositSubmissionAttempt({
            txHash: databaseTxHash("deposit-submission.expired"),
            eventId: databaseOutputReferenceId("deposit-submission.expired"),
          });

          yield* Effect.forEach(
            [
              prepared,
              unknown,
              submitted,
              confirmed,
              reconciled,
              ambiguous,
              expired,
            ],
            DepositSubmissionAttemptsDB.insertPrepared,
            { discard: true },
          );

          yield* DepositSubmissionAttemptsDB.beginSubmission(
            unknown[DepositSubmissionAttemptsDB.Columns.TX_HASH],
          );
          yield* DepositSubmissionAttemptsDB.beginSubmission(
            submitted[DepositSubmissionAttemptsDB.Columns.TX_HASH],
          );
          yield* DepositSubmissionAttemptsDB.beginSubmission(
            confirmed[DepositSubmissionAttemptsDB.Columns.TX_HASH],
          );
          yield* DepositSubmissionAttemptsDB.beginSubmission(
            reconciled[DepositSubmissionAttemptsDB.Columns.TX_HASH],
          );
          yield* DepositSubmissionAttemptsDB.beginSubmission(
            ambiguous[DepositSubmissionAttemptsDB.Columns.TX_HASH],
          );
          yield* DepositSubmissionAttemptsDB.markSubmitted(
            submitted[DepositSubmissionAttemptsDB.Columns.TX_HASH],
            "provider accepted",
          );
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
          yield* DepositSubmissionAttemptsDB.markExpired(
            expired[DepositSubmissionAttemptsDB.Columns.TX_HASH],
            "validity interval expired while transaction remained absent",
          );

          const open =
            yield* DepositSubmissionAttemptsDB.retrieveOpenAttempts();
          expect(open).toHaveLength(4);
          expect(
            new Set(
              open.map(
                (attempt) =>
                  attempt[DepositSubmissionAttemptsDB.Columns.STATUS],
              ),
            ),
          ).toEqual(
            new Set([
              DepositSubmissionAttemptsDB.Status.Prepared,
              DepositSubmissionAttemptsDB.Status.SubmissionUnknown,
              DepositSubmissionAttemptsDB.Status.Submitted,
              DepositSubmissionAttemptsDB.Status.Ambiguous,
            ]),
          );
        }),
      ),
  );

  it.effect("rejects invalid transitions and malformed durable payloads", (_) =>
    isolatedDb(
      Effect.gen(function* () {
        const prepared = makeDepositSubmissionAttempt();
        const txHash = prepared[DepositSubmissionAttemptsDB.Columns.TX_HASH];
        yield* DepositSubmissionAttemptsDB.insertPrepared(prepared);

        expect(
          yield* Effect.either(
            DepositSubmissionAttemptsDB.markSubmitted(txHash, "accepted"),
          ),
        ).toMatchObject({ _tag: "Left" });

        yield* DepositSubmissionAttemptsDB.markConfirmed(txHash);
        expect(
          yield* Effect.either(
            DepositSubmissionAttemptsDB.beginSubmission(txHash),
          ),
        ).toMatchObject({ _tag: "Left" });
        const preservedConfirmed =
          yield* DepositSubmissionAttemptsDB.markAmbiguous(
            txHash,
            "a stale response-loss writer must preserve stronger evidence",
          );
        expect(
          preservedConfirmed[DepositSubmissionAttemptsDB.Columns.STATUS],
        ).toBe(DepositSubmissionAttemptsDB.Status.Confirmed);

        const expired = makeDepositSubmissionAttempt({
          txHash: databaseTxHash("deposit-submission.terminal-expired"),
          eventId: databaseOutputReferenceId(
            "deposit-submission.terminal-expired",
          ),
        });
        const expiredTxHash =
          expired[DepositSubmissionAttemptsDB.Columns.TX_HASH];
        yield* DepositSubmissionAttemptsDB.insertPrepared(expired);
        yield* DepositSubmissionAttemptsDB.markExpired(
          expiredTxHash,
          "validity interval expired",
        );
        expect(
          yield* Effect.either(
            DepositSubmissionAttemptsDB.beginSubmission(expiredTxHash),
          ),
        ).toMatchObject({ _tag: "Left" });
        expect(
          yield* Effect.either(
            DepositSubmissionAttemptsDB.markConfirmed(expiredTxHash),
          ),
        ).toMatchObject({ _tag: "Left" });

        const emptySignedBytes = makeDepositSubmissionAttempt({
          txHash: databaseTxHash("deposit-submission.empty-signed"),
          eventId: databaseOutputReferenceId("deposit-submission.empty-signed"),
        });
        expect(
          yield* Effect.either(
            DepositSubmissionAttemptsDB.insertPrepared({
              ...emptySignedBytes,
              [DepositSubmissionAttemptsDB.Columns.SIGNED_TX_CBOR]:
                Buffer.alloc(0),
            }),
          ),
        ).toMatchObject({ _tag: "Left" });

        const malformedEvent = makeDepositSubmissionAttempt({
          txHash: databaseTxHash("deposit-submission.bad-event"),
          eventId: Buffer.alloc(0),
        });
        expect(
          yield* Effect.either(
            DepositSubmissionAttemptsDB.insertPrepared(malformedEvent),
          ),
        ).toMatchObject({ _tag: "Left" });
      }),
    ),
  );

  it.effect(
    "keeps an event unique while active and permits a new operation after a never-claimed expiry",
    (_) =>
      isolatedDb(
        Effect.gen(function* () {
          const eventId = databaseOutputReferenceId(
            "deposit-submission.variable-event",
            24,
          );
          expect(eventId).toHaveLength(40);
          const first = makeDepositSubmissionAttempt({
            txHash: databaseTxHash("deposit-submission.active-event.first"),
            eventId,
          });
          const second = makeDepositSubmissionAttempt({
            txHash: databaseTxHash("deposit-submission.active-event.second"),
            eventId,
          });

          yield* DepositSubmissionAttemptsDB.insertPrepared(first);
          expect(
            yield* DepositSubmissionAttemptsDB.insertPrepared(second).pipe(
              Effect.either,
            ),
          ).toMatchObject({ _tag: "Left" });

          yield* DepositSubmissionAttemptsDB.markExpired(
            first[DepositSubmissionAttemptsDB.Columns.TX_HASH],
            "never-claimed transaction expired",
          );
          const replacement =
            yield* DepositSubmissionAttemptsDB.insertPrepared(second);
          expect(replacement[DepositSubmissionAttemptsDB.Columns.STATUS]).toBe(
            DepositSubmissionAttemptsDB.Status.Prepared,
          );

          const sameEvent =
            yield* DepositSubmissionAttemptsDB.retrieveByEventId(eventId);
          expect(sameEvent).toHaveLength(2);
          expect(
            sameEvent.map(
              (attempt) => attempt[DepositSubmissionAttemptsDB.Columns.STATUS],
            ),
          ).toEqual([
            DepositSubmissionAttemptsDB.Status.Expired,
            DepositSubmissionAttemptsDB.Status.Prepared,
          ]);
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

        expect(resolved.schemaVersion).toEqual("midgard-e2e-reconciliation-v1");
        expect(resolved.milestone).toEqual("tx-committed");
        expect(resolved.status).toEqual("ambiguous");
        expect(resolved.safeToRetryOriginalStep).toEqual(true);
        expect(resolved.target).toEqual({ txHash: txHash.toString("hex") });
        expect(
          resolved.evidence.some((entry) => entry.kind === "tx_status"),
        ).toEqual(true);
      }),
    ),
  );
});

describe("DepositsDB and MempoolLedgerDB exact-once projection", () => {
  it.effect("rejects payload drift for the same deposit event_id", (_) =>
    isolatedDb(
      Effect.gen(function* () {
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
    isolatedDb(
      Effect.gen(function* () {
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
      isolatedDb(
        Effect.gen(function* () {
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
      isolatedDb(
        Effect.gen(function* () {
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
      isolatedDb(
        Effect.gen(function* () {
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
      isolatedDb(
        Effect.gen(function* () {
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
    isolatedDb(
      Effect.gen(function* () {
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
      isolatedDb(
        Effect.gen(function* () {
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
      isolatedDb(
        Effect.gen(function* () {
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
      isolatedDb(
        Effect.gen(function* () {
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
      isolatedDb(
        Effect.gen(function* () {
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
      isolatedDb(
        Effect.gen(function* () {
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
} = {}): DepositSubmissionAttemptsDB.InsertPreparedInput => ({
  [DepositSubmissionAttemptsDB.Columns.TX_HASH]: txHash,
  [DepositSubmissionAttemptsDB.Columns.DEPOSIT_EVENT_ID]: eventId,
  [DepositSubmissionAttemptsDB.Columns.SIGNED_TX_CBOR]: databaseFixtureBytes(
    `deposit-submission.signed.${txHash.toString("hex")}`,
    96,
  ),
  [DepositSubmissionAttemptsDB.Columns.EXPECTED_DEPOSIT_OUT_REF]:
    `${txHash.toString("hex")}#0`,
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
  [DepositSubmissionAttemptsDB.Columns.DEPENDENCY_OUT_REFS]: {
    spend: [`${databaseTxHash("deposit-submission.spend").toString("hex")}#0`],
    collateral: [
      `${databaseTxHash("deposit-submission.collateral").toString("hex")}#1`,
    ],
    reference: [
      `${databaseTxHash("deposit-submission.reference").toString("hex")}#2`,
    ],
  },
});

describe("Phase 3 MPF durable state", () => {
  it.effect("persists EWMA and keeps audit divergence sticky", () =>
    isolatedDb(
      Effect.gen(function* () {
        const before = yield* CommitBuildCalibrationDB.retrieve;
        const updated = yield* CommitBuildCalibrationDB.update(2.5);
        expect(updated.msPerTxEwma).toBe(2.5);
        expect(updated.sampleCount).toBe(before.sampleCount + 1n);

        yield* MpfEngineStateDB.recordLedgerAudit({
          rootHex: SDK.EMPTY_MERKLE_TREE_ROOT,
          diverged: true,
        });
        yield* MpfEngineStateDB.recordLedgerAudit({
          rootHex: SDK.EMPTY_MERKLE_TREE_ROOT,
          diverged: false,
        });
        expect(
          (yield* MpfEngineStateDB.assertLedgerAuditHealthy.pipe(Effect.either))
            ._tag,
        ).toBe("Left");
        yield* MpfEngineStateDB.acknowledgeCleanLedgerAudit(
          SDK.EMPTY_MERKLE_TREE_ROOT,
        );
        expect(
          (yield* MpfEngineStateDB.assertLedgerAuditHealthy.pipe(Effect.either))
            ._tag,
        ).toBe("Right");
        expect(
          yield* MpfEngineStateDB.acquireLedgerStoreLease({
            owner: "audit:test",
            ttlMs: 60_000,
          }),
        ).toBe(true);
        expect(
          yield* MpfEngineStateDB.acquireLedgerStoreLease({
            owner: "commit:test",
            ttlMs: 60_000,
          }),
        ).toBe(false);
        yield* MpfEngineStateDB.releaseLedgerStoreLease("audit:test");
      }),
    ),
  );

  it.live(
    "renews ordered state-queue and MPF leases and excludes merge/commit competitors",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const result = yield* StateQueueMutationLeasesDB.tryWithLease(
            "mpf-audit-test",
            (stateQueueToken) =>
              Effect.gen(function* () {
                const mpfResult =
                  yield* MpfEngineStateDB.tryWithLedgerStoreLease(
                    "mpf-audit-test",
                    (mpfOwner) =>
                      Effect.gen(function* () {
                        yield* Effect.sleep("180 millis");
                        yield* StateQueueMutationLeasesDB.revalidate(
                          stateQueueToken,
                        );
                        yield* MpfEngineStateDB.revalidateLedgerStoreLease(
                          mpfOwner,
                        );
                        const mergeAttempt =
                          yield* StateQueueMutationLeasesDB.tryAcquire({
                            holder: "merge-test",
                            ttlMs: 100,
                          });
                        expect(mergeAttempt._tag).toBe("Busy");
                        expect(
                          yield* MpfEngineStateDB.acquireLedgerStoreLease({
                            owner: "commit-test",
                            ttlMs: 100,
                          }),
                        ).toBe(false);
                      }),
                    { ttlMs: 90, renewIntervalMs: 20 },
                  );
                expect(mpfResult._tag).toBe("Ran");
              }),
            { ttlMs: 90, renewIntervalMs: 20 },
          );
          expect(result._tag).toBe("Ran");
        }),
      ),
  );

  it.effect(
    "replays a depth-three ledger delta chain from confirmed state",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const sql = yield* SqlClient.SqlClient;
          const address = CML.Address.from_bech32(address1);
          const entry = (byte: number): LedgerUtils.Entry => ({
            [LedgerUtils.Columns.TX_ID]: Buffer.alloc(32, byte),
            [LedgerUtils.Columns.OUTREF]: Buffer.from(
              CML.TransactionInput.new(
                CML.TransactionHash.from_hex(
                  byte.toString(16).padStart(2, "0").repeat(32),
                ),
                0n,
              ).to_cbor_bytes(),
            ),
            [LedgerUtils.Columns.OUTPUT]: Buffer.from(
              makeMidgardTxOutput(
                address,
                CML.Value.from_coin(1_000_000n + BigInt(byte)),
              ).to_cbor_bytes(),
            ),
            [LedgerUtils.Columns.ADDRESS]: address1,
          });
          const [a, untouched, c, d, e] = [1, 2, 3, 4, 5].map(entry) as [
            LedgerUtils.Entry,
            LedgerUtils.Entry,
            LedgerUtils.Entry,
            LedgerUtils.Entry,
            LedgerUtils.Entry,
          ];
          yield* ConfirmedLedgerDB.insertMultiple([a, untouched]);
          const states = [
            [a, untouched],
            [untouched, c],
            [untouched, d],
            [untouched, d, e],
          ];
          const roots = yield* Effect.forEach(
            states,
            computeLedgerMpfRootFromLedgerEntries,
          );
          const headerValues: SDK.Header[] = [];
          const headers: Buffer[] = [];
          for (let index = 0; index < 3; index += 1) {
            const header: SDK.Header = {
              prevUtxosRoot: roots[index]!,
              utxosRoot: roots[index + 1]!,
              withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
              forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
              transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
              depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
              transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
              eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
              withdrawalCount: 0n,
              forcedTransactionCount: 0n,
              l2TransactionCount: 0n,
              depositCount: 0n,
              totalEventCount: 0n,
              transitionStepCount: 0n,
              startTime: BigInt(index * 2_000),
              endTime: BigInt(index * 2_000 + 1_000),
              prevHeaderHash:
                index === 0
                  ? "00".repeat(28)
                  : headers[index - 1]!.toString("hex"),
              operatorVkey: "11".repeat(28),
              protocolVersion: 1n,
            };
            headerValues.push(header);
            headers.push(
              Buffer.from(yield* SDK.hashBlockHeader(header), "hex"),
            );
          }
          const prepare = (
            index: number,
            spent: readonly Buffer[],
            produced: readonly LedgerUtils.Entry[],
          ) =>
            Effect.gen(function* () {
              yield* PendingBlockFinalizationsDB.preparePendingSubmission({
                headerHash: headers[index]!,
                headerCbor: Buffer.from(
                  LucidData.to(
                    headerValues[index]! as never,
                    SDK.Header as never,
                  ),
                  "hex",
                ),
                metadata: {
                  stateQueueLeaseToken: `phase3-${index.toString()}`,
                  baseSnapshotId: `phase3-${index.toString()}`,
                  baseTailOutRef: `phase3#${index.toString()}`,
                  baseTailHeaderHash:
                    index === 0 ? Buffer.alloc(28) : headers[index - 1]!,
                  baseTailDatumCbor: "d87980",
                  baseRoots: {
                    utxosRoot: roots[index]!,
                    forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                  },
                  blockStartTime: new Date(index * 2_000),
                  expectedRoots: {
                    utxosRoot: roots[index + 1]!,
                    forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                    transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                    depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                    withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                    transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                    eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                  },
                  expectedCounts: {
                    withdrawalCount: 0n,
                    forcedTransactionCount: 0n,
                    l2TransactionCount: 0n,
                    depositCount: 0n,
                    totalEventCount: 0n,
                    transitionStepCount: 0n,
                  },
                },
                blockEndTime: new Date(index * 2_000 + 1_000),
                depositEventIds: [],
                depositEntries: [],
                forcedTransactionEventIds: [],
                forcedTransactionEntries: [],
                withdrawalEventIds: [],
                withdrawalEntries: [],
                mempoolTxIds: [],
                mempoolTxs: [],
                mempoolTxSourceTable: "none",
                transitionTraceMembers: [],
                eventToStepMembers: [],
                utxoEntries: [],
                ledgerDelta: {
                  spent,
                  produced: produced.map((item) => ({
                    [PendingBlockFinalizationsDB.UtxoColumns.OUTREF]:
                      item[LedgerUtils.Columns.OUTREF],
                    [PendingBlockFinalizationsDB.UtxoColumns.OUTPUT]:
                      item[LedgerUtils.Columns.OUTPUT],
                  })),
                },
                utxoPayloadAggregate: ledgerPayloadAggregateFromEntries(
                  states[index + 1]!,
                ),
              });
              yield* sql`UPDATE ${sql(
                PendingBlockFinalizationsDB.tableName,
              )} SET status = ${PendingBlockFinalizationsDB.Status.Finalized}
              WHERE header_hash = ${headers[index]!}`;
            });
          yield* prepare(0, [a[LedgerUtils.Columns.OUTREF]], [c]);
          yield* prepare(1, [c[LedgerUtils.Columns.OUTREF]], [d]);
          yield* prepare(2, [], [e]);

          const found = yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(
            headers[2]!,
          );
          expect(found._tag).toBe("Some");
          if (found._tag === "None") return;
          expect(found.value.utxoPayloadAggregate).toEqual(
            ledgerPayloadAggregateFromEntries(states[3]!),
          );
          const snapshot = yield* materializeConfirmedLedgerSnapshot(
            found.value,
          );
          expect(snapshot.root).toBe(roots[3]);
          expect(
            snapshot.entries.map((item) =>
              item[LedgerUtils.Columns.OUTREF].toString("hex"),
            ),
          ).toEqual(
            states[3]!.map((item) =>
              item[LedgerUtils.Columns.OUTREF].toString("hex"),
            ),
          );

          const fullUtxos = snapshot.entries.map((item) => ({
            outref: item[LedgerUtils.Columns.OUTREF],
            output: item[LedgerUtils.Columns.OUTPUT],
          }));
          const v2Insert = yield* buildDaPayloadInsert({
            record: found.value,
            utxos: fullUtxos,
            envelope: { mode: "off", zstdLevel: 3 },
          });
          const v3Insert = yield* buildDaPayloadInsert({
            record: found.value,
            utxos: fullUtxos,
            envelope: { mode: "zstd", zstdLevel: 3 },
          });
          const v3Unwrapped = yield* Effect.tryPromise({
            try: () =>
              unwrapDaPayload(v3Insert.payload_cbor, {
                maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
                schemaVersion: v3Insert.version,
              }),
            catch: (cause) => cause,
          });
          const expectedPayloadOutrefs = states[3]!
            .map((item) => item[LedgerUtils.Columns.OUTREF].toString("hex"))
            .sort();
          for (const payloadBytes of [
            v2Insert.payload_cbor,
            v3Unwrapped.innerBytes,
          ]) {
            const payload = SDK.decodeDaPayloadV2(payloadBytes);
            expect(payload.block_body.utxos.map(([outref]) => outref)).toEqual(
              expectedPayloadOutrefs,
            );
            expect(payload.block_body.header.utxosRoot).toBe(roots[3]);
          }

          for (let index = 0; index < headers.length; index += 1) {
            const header = headers[index]!;
            const journal =
              yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(header);
            if (journal._tag === "None") throw new Error("missing journal");
            const delta = yield* decodeConfirmedLedgerDelta(journal.value);
            if (delta === undefined) throw new Error("missing delta");
            yield* applyConfirmedLedgerDelta(delta);
            const confirmedAtStep = yield* ConfirmedLedgerDB.retrieve;
            expect(
              confirmedAtStep.map((item) =>
                item[LedgerUtils.Columns.OUTREF].toString("hex"),
              ),
            ).toEqual(
              states[index + 1]!.map((item) =>
                item[LedgerUtils.Columns.OUTREF].toString("hex"),
              ),
            );
            expect(
              yield* computeLedgerMpfRootFromLedgerEntries(confirmedAtStep),
            ).toBe(roots[index + 1]);
          }
          const confirmedAfter = yield* ConfirmedLedgerDB.retrieve;
          expect(
            confirmedAfter.some((item) =>
              item[LedgerUtils.Columns.OUTREF].equals(
                untouched[LedgerUtils.Columns.OUTREF],
              ),
            ),
          ).toBe(true);
          expect(
            yield* computeLedgerMpfRootFromLedgerEntries(confirmedAfter),
          ).toBe(roots[3]);
        }),
      ),
  );

  it.effect(
    "materializes a first-block journal across an implicit genesis base",
    () =>
      isolatedDb(
        Effect.gen(function* () {
          const address = CML.Address.from_bech32(address1);
          const entry = (byte: number): LedgerUtils.Entry => ({
            [LedgerUtils.Columns.TX_ID]: Buffer.alloc(32, byte),
            [LedgerUtils.Columns.OUTREF]: Buffer.from(
              CML.TransactionInput.new(
                CML.TransactionHash.from_hex(
                  byte.toString(16).padStart(2, "0").repeat(32),
                ),
                0n,
              ).to_cbor_bytes(),
            ),
            [LedgerUtils.Columns.OUTPUT]: Buffer.from(
              makeMidgardTxOutput(
                address,
                CML.Value.from_coin(1_000_000n + BigInt(byte)),
              ).to_cbor_bytes(),
            ),
            [LedgerUtils.Columns.ADDRESS]: address1,
          });
          const genesis = entry(21);
          const deposit = entry(22);
          const selectedBaseUtxosRoot =
            yield* computeLedgerMpfRootFromLedgerEntries([genesis]);
          const finalEntries = [genesis, deposit];
          const expectedFinalUtxosRoot =
            yield* computeLedgerMpfRootFromLedgerEntries(finalEntries);
          const journalState = yield* resolvePendingJournalLedgerState({
            recordedBaseUtxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
            selectedBaseUtxosRoot,
            expectedFinalUtxosRoot,
            expectedFinalEntryCount: finalEntries.length,
            implicitGenesisEntries: [genesis],
            transitionDelta: {
              spent: [],
              produced: [
                {
                  outref: deposit[LedgerUtils.Columns.OUTREF],
                  output: deposit[LedgerUtils.Columns.OUTPUT],
                },
              ],
            },
          });
          expect(journalState.ledgerDelta).toBeUndefined();
          expect(journalState.utxoEntries).toHaveLength(2);

          const unexplainedDivergence = yield* resolvePendingJournalLedgerState(
            {
              recordedBaseUtxosRoot: "ff".repeat(32),
              selectedBaseUtxosRoot,
              expectedFinalUtxosRoot,
              expectedFinalEntryCount: finalEntries.length,
              implicitGenesisEntries: [genesis],
              transitionDelta: {
                spent: [],
                produced: [],
              },
            },
          ).pipe(Effect.either);
          expect(unexplainedDivergence._tag).toBe("Left");

          const headerHash = Buffer.alloc(28, 23);
          yield* PendingBlockFinalizationsDB.preparePendingSubmission({
            headerHash,
            headerCbor: Buffer.from("d87980", "hex"),
            metadata: {
              stateQueueLeaseToken: "implicit-genesis-test",
              baseSnapshotId: "implicit-genesis-test",
              baseTailOutRef: "genesis#0",
              baseTailHeaderHash: Buffer.alloc(28),
              baseTailDatumCbor: "d87980",
              baseRoots: {
                utxosRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
              },
              blockStartTime: new Date(0),
              expectedRoots: {
                utxosRoot: expectedFinalUtxosRoot,
                forcedTransactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                transactionsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                depositsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                withdrawalsRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                transitionTraceRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
                eventToStepRoot: SDK.EMPTY_MERKLE_TREE_ROOT,
              },
              expectedCounts: {
                withdrawalCount: 0n,
                forcedTransactionCount: 0n,
                l2TransactionCount: 0n,
                depositCount: 1n,
                totalEventCount: 1n,
                transitionStepCount: 1n,
              },
            },
            blockEndTime: new Date(1_000),
            depositEventIds: [],
            depositEntries: [],
            forcedTransactionEventIds: [],
            forcedTransactionEntries: [],
            withdrawalEventIds: [],
            withdrawalEntries: [],
            mempoolTxIds: [],
            mempoolTxs: [],
            mempoolTxSourceTable: "none",
            transitionTraceMembers: [],
            eventToStepMembers: [],
            utxoEntries: journalState.utxoEntries.map((produced) => ({
              [PendingBlockFinalizationsDB.UtxoColumns.OUTREF]: produced.outref,
              [PendingBlockFinalizationsDB.UtxoColumns.OUTPUT]: produced.output,
            })),
            ledgerDelta: undefined,
            utxoPayloadAggregate:
              ledgerPayloadAggregateFromEntries(finalEntries),
          });
          const journal =
            yield* PendingBlockFinalizationsDB.retrieveByHeaderHash(headerHash);
          expect(journal._tag).toBe("Some");
          if (journal._tag === "None") return;
          const materialized = yield* materializeConfirmedLedgerSnapshot(
            journal.value,
          );
          expect(materialized.root).toBe(expectedFinalUtxosRoot);
          expect(materialized.entries).toHaveLength(finalEntries.length);
        }),
      ),
  );
});
