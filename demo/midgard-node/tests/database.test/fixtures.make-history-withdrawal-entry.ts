import { spawn, spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { default as fs } from "node:fs";
import { default as path } from "node:path";

import {
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { MIDGARD_CONSENSUS_PROFILE_ID } from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { beforeAll } from "vitest";

import {
  DaPayloadsDB,
  LedgerUtils,
  TxUtils,
  WithdrawalsDB,
} from "../../src/database/index.js";
import * as MigrationRunner from "../../src/database/migrations/runner.js";
import {
  deterministicFixtureOutputReferenceId,
  provideDatabaseLayers,
  resetApplicationTables,
} from ".././utils.js";
import {
  databaseFixtureBytes,
  databaseTestDirectory,
  databaseTxHash,
} from "./fixtures.make-material-proof-submit-tx.js";

export const databaseOutputReferenceId = (
  label: string,
  outputIndex: number | bigint = 0n,
): Buffer =>
  deterministicFixtureOutputReferenceId(`database:${label}`, outputIndex);

export const collectChildProcess = (child: ReturnType<typeof spawn>) => {
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

export const bundleChildProcessHelper = (
  relativeSourcePath: string,
): string => {
  const cwd = path.resolve(databaseTestDirectory, "..");
  const sourcePath = path.resolve(databaseTestDirectory, relativeSourcePath);
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

export const databaseChildProcessEnv = (): NodeJS.ProcessEnv => {
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

export const daPayloadInsertFixture = (
  label: string,
): DaPayloadsDB.InsertInput => {
  const headerHash = databaseFixtureBytes(`${label}-header`, 28);
  const payload = databaseFixtureBytes(`${label}-payload`, 96);
  return {
    [DaPayloadsDB.Columns.HEADER_HASH]: headerHash,
    [DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID]: MIDGARD_CONSENSUS_PROFILE_ID,
    [DaPayloadsDB.Columns.VERSION]: 1,
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
    [DaPayloadsDB.Columns.VALIDATION_TRACES_ROOT]: SDK.EMPTY_MERKLE_TREE_ROOT,
    [DaPayloadsDB.Columns.WITHDRAWAL_COUNT]: 0n,
    [DaPayloadsDB.Columns.FORCED_TRANSACTION_COUNT]: 0n,
    [DaPayloadsDB.Columns.L2_TRANSACTION_COUNT]: 0n,
    [DaPayloadsDB.Columns.DEPOSIT_COUNT]: 0n,
    [DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]: 0n,
    [DaPayloadsDB.Columns.TRANSITION_STEP_COUNT]: 0n,
    [DaPayloadsDB.Columns.VALIDATION_TRACE_COUNT]: 0n,
    [DaPayloadsDB.Columns.BLOCK_START_TIME]: new Date(),
    [DaPayloadsDB.Columns.BLOCK_END_TIME]: new Date(),
  };
};

export const registerDatabaseSetup = () => {
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
          yield* resetApplicationTables;
        }),
      ),
    );
  });
};

export const makeHistoryWithdrawalEntry = (): WithdrawalsDB.Entry => {
  return {
    [WithdrawalsDB.Columns.ID]: databaseOutputReferenceId(
      "history-pointer-withdrawal",
    ),
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
    [WithdrawalsDB.Columns.CLASSIFICATION_REVISION]: 0,
    [WithdrawalsDB.Columns.REOPENED_FROM_HEADER_HASH]: null,
    [WithdrawalsDB.Columns.VALIDITY_DETAIL]: {},
    [WithdrawalsDB.Columns.PROJECTED_HEADER_HASH]: null,
    [WithdrawalsDB.Columns.STATUS]: WithdrawalsDB.Status.Awaiting,
  };
};

export const blockHeader1 = databaseFixtureBytes("blocks.header-1", 32);

export const blockHeader2 = databaseFixtureBytes("blocks.header-2", 32);

export const txId1 = databaseTxHash("shared.tx-1");

export const txId2 = databaseTxHash("shared.tx-2");

export const tx1 = databaseFixtureBytes("shared.tx-1-cbor", 64);

export const tx2 = databaseFixtureBytes("shared.tx-2-cbor", 64);

export const tx3 = databaseFixtureBytes("shared.tx-3-cbor", 64);

const outref1 = databaseFixtureBytes("shared.outref-1", 36);

const outref2 = databaseFixtureBytes("shared.outref-2", 36);

const output1 = databaseFixtureBytes("shared.output-1", 80);

const output2 = databaseFixtureBytes("shared.output-2", 80);

type TxFixture = {
  readonly cborHex: string;
  readonly txId: string;
};

const fixturePath = path.resolve(databaseTestDirectory, "./txs/txs_0.json");

const firstFixture = (
  JSON.parse(fs.readFileSync(fixturePath, "utf8")) as readonly TxFixture[]
)[0];

export const makeValidNativeImmutableEntry = (): TxUtils.Entry => {
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

export const address1 =
  "addr_test1wzylc3gg4h37gt69yx057gkn4egefs5t9rsycmryecpsenswtdp58";

export const address2 =
  "addr_test1vzcsc5wzu3vsnjek2n80ayce53r4ha2g6wyetqddrp8z04q3yzv6k";

export const txEntry1: TxUtils.Entry = {
  [TxUtils.Columns.TX_ID]: txId1,
  [TxUtils.Columns.TX]: tx1,
};

export const txEntry2: TxUtils.Entry = {
  [TxUtils.Columns.TX_ID]: txId2,
  [TxUtils.Columns.TX]: tx2,
};

export const removeTimestampFromTxEntry = (
  e: TxUtils.Entry,
): TxUtils.EntryNoTimeStamp => {
  return {
    [TxUtils.Columns.TX_ID]: e[TxUtils.Columns.TX_ID],
    [TxUtils.Columns.TX]: e[TxUtils.Columns.TX],
  };
};

export const ledgerEntry1: LedgerUtils.Entry = {
  [LedgerUtils.Columns.TX_ID]: txId1,
  [LedgerUtils.Columns.OUTREF]: outref1,
  [LedgerUtils.Columns.OUTPUT]: output1,
  [LedgerUtils.Columns.ADDRESS]: address1,
};

export const ledgerEntry2: LedgerUtils.Entry = {
  [LedgerUtils.Columns.TX_ID]: txId2,
  [LedgerUtils.Columns.OUTREF]: outref2,
  [LedgerUtils.Columns.OUTPUT]: output2,
  [LedgerUtils.Columns.ADDRESS]: address2,
};

export const removeTimestampFromLedgerEntry = (
  e: LedgerUtils.Entry,
): LedgerUtils.EntryNoTimeStamp => {
  return {
    [LedgerUtils.Columns.TX_ID]: e[LedgerUtils.Columns.TX_ID],
    [LedgerUtils.Columns.OUTREF]: e[LedgerUtils.Columns.OUTREF],
    [LedgerUtils.Columns.OUTPUT]: e[LedgerUtils.Columns.OUTPUT],
    [LedgerUtils.Columns.ADDRESS]: e[LedgerUtils.Columns.ADDRESS],
  };
};

export const depositFixtureBaseTimeMs = Date.parse("2026-04-13T17:28:10.000Z");
