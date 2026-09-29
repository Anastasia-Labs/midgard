import { spawn, spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import { default as fs } from "node:fs";
import { default as path } from "node:path";

import {
  decodeMidgardCekProgramEnvelope,
  encodeMidgardCekProgramEnvelope,
  encodeMidgardCekProgramMaterialSidecar,
  encodeMidgardCekTermNode,
  encodeMidgardProofSubmission,
  hashMidgardCekTermNode,
} from "@al-ft/midgard-core/cek-proof";
import {
  cardanoTxBytesToMidgardNativeTxCanonicalCbor,
  computeMidgardNativeTxId,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  EMPTY_CBOR_LIST,
  EMPTY_NULL_ROOT,
  encodeMidgardNativeTxCanonical,
  encodeMidgardVersionedScriptListPreimage,
  materializeMidgardNativeTxFromCanonical,
  MIDGARD_NATIVE_NETWORK_ID_NONE,
  MIDGARD_NATIVE_TX_VERSION,
  MIDGARD_POSIX_TIME_NONE,
  type MidgardNativeTxCanonical,
} from "@al-ft/midgard-core/codec";
import { encodeCbor } from "@al-ft/midgard-core/codec/cbor";
import {
  MIDGARD_CONSENSUS_PROFILE,
  MIDGARD_CONSENSUS_PROFILE_ID,
  type MidgardConsensusProfile,
} from "@al-ft/midgard-core/consensus-profile";
import * as SDK from "@al-ft/midgard-sdk";
import { HttpServerRequest, HttpServerResponse } from "@effect/platform";
import { SqlClient } from "@effect/sql";
import { Effect } from "effect";
import { beforeAll, expect } from "vitest";

import { buildSubmitRouter } from "../../src/commands/listen-router.js";
import {
  CekProgramMaterialDB,
  DaPayloadsDB,
  DepositsDB,
  DepositSubmissionAttemptsDB,
  LedgerUtils,
  MempoolDB,
  TxAdmissionsDB,
  TxUtils,
  WithdrawalsDB,
} from "../../src/database/index.js";
import * as MigrationRunner from "../../src/database/migrations/runner.js";
import { AdmissionWriter } from "../../src/services/admission-writer.js";
import { NodeConfig } from "../../src/services/config.js";
import { BatchSql } from "../../src/services/database.js";
import { Globals } from "../../src/services/globals.js";
import { Lucid } from "../../src/services/lucid.js";
import { MempoolLedgerCache } from "../../src/services/mempool-ledger-cache.js";
import { ValidationPool } from "../../src/services/validation-pool.js";
import { WriteBehind } from "../../src/services/write-behind.js";
import { makeCardanoSignedMapOutputTxBytes } from ".././helpers/cardano-native-fixtures.js";
import { makeOutRefCbor } from ".././midgard-output-helpers.js";
import {
  deterministicFixtureBytes,
  deterministicFixtureOutputReferenceId,
  deterministicFixtureTxHash,
  provideDatabaseLayers,
  resetApplicationTables,
} from ".././utils.js";

const databaseTestDirectory = path.resolve(__dirname, "..");

export const isolatedDb = <A, E, R>(effect: Effect.Effect<A, E, R>) =>
  provideDatabaseLayers(
    Effect.gen(function* () {
      yield* resetApplicationTables;
      return yield* effect;
    }),
  );

type SubmitHttpResult = {
  readonly status: number;
  readonly body: Record<string, unknown>;
};

export type TxQueueWakeRequirements =
  | BatchSql
  | NodeConfig
  | Globals
  | Lucid
  | WriteBehind
  | ValidationPool
  | MempoolLedgerCache;

export const submitThroughRouter = <R>(
  requestBody: Buffer,
  wakeTxQueueProcessor: Effect.Effect<void, never, R>,
  options: {
    readonly contentType?: string;
    readonly consensusProfile?: MidgardConsensusProfile;
  } = {},
): Effect.Effect<
  SubmitHttpResult,
  unknown,
  R | SqlClient.SqlClient | NodeConfig | Globals | AdmissionWriter
> =>
  Effect.gen(function* () {
    const response = yield* buildSubmitRouter(
      wakeTxQueueProcessor,
      undefined,
      options.consensusProfile ?? MIDGARD_CONSENSUS_PROFILE,
    ).pipe(
      Effect.provideService(
        HttpServerRequest.HttpServerRequest,
        HttpServerRequest.fromWeb(
          new Request("http://midgard.test/submit", {
            method: "POST",
            headers: {
              "content-type":
                options.contentType ?? "application/vnd.midgard.v1+cbor",
              "content-length": requestBody.length.toString(),
            },
            body: new Uint8Array(requestBody),
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

export const makeNativeSubmitTx = (): {
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

export const makeProofSubmitTx = (): {
  readonly txId: Buffer;
  readonly txIdHex: string;
  readonly txCanonicalCbor: Buffer;
} => {
  const canonical: MidgardNativeTxCanonical = {
    version: MIDGARD_NATIVE_TX_VERSION,
    validity: "TxIsValid",
    body: {
      spendInputsPreimageCbor: EMPTY_CBOR_LIST,
      referenceInputsPreimageCbor: EMPTY_CBOR_LIST,
      outputsPreimageCbor: EMPTY_CBOR_LIST,
      fee: 0n,
      validityIntervalStart: MIDGARD_POSIX_TIME_NONE,
      validityIntervalEnd: MIDGARD_POSIX_TIME_NONE,
      requiredObserversPreimageCbor: EMPTY_CBOR_LIST,
      requiredSignersPreimageCbor: EMPTY_CBOR_LIST,
      mintPreimageCbor: EMPTY_CBOR_LIST,
      scriptIntegrityHash: EMPTY_NULL_ROOT,
      auxiliaryDataHash: EMPTY_NULL_ROOT,
      networkId: MIDGARD_NATIVE_NETWORK_ID_NONE,
    },
    witnessSet: {
      addrTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
      redeemerTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  };
  const txCanonicalCbor = encodeMidgardNativeTxCanonical(
    materializeMidgardNativeTxFromCanonical(canonical),
  );
  const txId = computeMidgardNativeTxId(
    decodeMidgardNativeTxFullFromCanonicalCbor(txCanonicalCbor),
  );
  return { txId, txIdHex: txId.toString("hex"), txCanonicalCbor };
};

export const makeMaterialProofSubmitTx = (nonce: number) => {
  const term = { kind: "variable" as const, index: BigInt(nonce) };
  const preimage = encodeMidgardCekTermNode(term);
  const material = {
    kind: "term" as const,
    root: hashMidgardCekTermNode(term),
    preimage,
  };
  const envelope = decodeMidgardCekProgramEnvelope(
    encodeMidgardCekProgramEnvelope({
      uplcVersion: [1n, 1n, 0n],
      termRoot: material.root,
      nodeCount: 1n,
      materialByteLength: BigInt(preimage.length),
    }),
  );
  const base = decodeMidgardNativeTxFullFromCanonicalCbor(
    makeNativeSubmitTx().txCanonicalCbor,
  );
  const canonical: MidgardNativeTxCanonical = {
    version: base.version,
    validity: base.validity,
    body: {
      ...base.body,
      fee: base.body.fee + BigInt(nonce),
    },
    witnessSet: {
      ...base.witnessSet,
      scriptTxWitsPreimageCbor: encodeMidgardVersionedScriptListPreimage([
        {
          language: "MidgardV1",
          scriptBytes: encodeMidgardCekProgramEnvelope(envelope),
        },
      ]),
    },
  };
  const txCanonicalCbor = encodeMidgardNativeTxCanonical(
    materializeMidgardNativeTxFromCanonical(canonical),
  );
  const txId = computeMidgardNativeTxId(
    decodeMidgardNativeTxFullFromCanonicalCbor(txCanonicalCbor),
  );
  const sidecarCbor = encodeMidgardCekProgramMaterialSidecar([material]);
  return {
    txId,
    txIdHex: txId.toString("hex"),
    txCanonicalCbor,
    material,
    envelope,
    sidecarCbor,
    proofEnvelope: encodeMidgardProofSubmission({
      transactionCbor: txCanonicalCbor,
      programMaterial: [material],
    }),
  };
};

export const makeReferenceMaterialProofSubmitTx = (nonce: number) => {
  const attached = makeMaterialProofSubmitTx(nonce);
  const referenceOutRef = makeOutRefCbor(
    databaseTxHash(`accepted-reference-material-${nonce.toString()}`),
    0,
  );
  const attachedCanonical = decodeMidgardNativeTxFullFromCanonicalCbor(
    attached.txCanonicalCbor,
  );
  const canonical: MidgardNativeTxCanonical = {
    ...attachedCanonical,
    body: {
      ...attachedCanonical.body,
      referenceInputsPreimageCbor: encodeCbor([referenceOutRef]),
    },
    witnessSet: {
      ...attachedCanonical.witnessSet,
      scriptTxWitsPreimageCbor: EMPTY_CBOR_LIST,
    },
  };
  const txCanonicalCbor = encodeMidgardNativeTxCanonical(
    materializeMidgardNativeTxFromCanonical(canonical),
  );
  const txId = computeMidgardNativeTxId(
    decodeMidgardNativeTxFullFromCanonicalCbor(txCanonicalCbor),
  );
  return {
    ...attached,
    txId,
    txIdHex: txId.toString("hex"),
    txCanonicalCbor,
    referenceOutRef,
    proofEnvelope: encodeMidgardProofSubmission({
      transactionCbor: txCanonicalCbor,
      programMaterial: [attached.material],
    }),
  };
};

export const expectSubmitBody = (
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

export const retrieveAllMempool = MempoolDB.retrievePage({
  limit: 100_000,
}).pipe(Effect.map((page) => page.entries));

export const databaseFixtureBytes = (label: string, length: number): Buffer =>
  deterministicFixtureBytes(`database:${label}`, length);

export const emptyProgramMaterialSidecar =
  encodeMidgardCekProgramMaterialSidecar([]);
export const emptyProgramMaterialSidecarSha256 = createHash("sha256")
  .update(emptyProgramMaterialSidecar)
  .digest();

export const readCekProgramMaterialStoreStats = Effect.gen(function* () {
  const sql = yield* SqlClient.SqlClient;
  const rows = yield* sql<{
    readonly entry_count: string;
    readonly membership_count: string;
    readonly owner_count: string;
    readonly total_bytes: string;
  }>`SELECT
      (SELECT COUNT(*)::text
        FROM ${sql(CekProgramMaterialDB.entryTableName)}) AS entry_count,
      (SELECT COUNT(*)::text
        FROM ${sql(CekProgramMaterialDB.membershipTableName)})
        AS membership_count,
      (SELECT COUNT(*)::text
        FROM ${sql(CekProgramMaterialDB.admissionOwnerTableName)})
        AS owner_count,
      (
        COALESCE((
          SELECT SUM(
            octet_length(material_root) + octet_length(da_value_cbor)
          )
          FROM ${sql(CekProgramMaterialDB.entryTableName)}
        ), 0)
        + COALESCE((
          SELECT SUM(
            octet_length(program_envelope_hash)
              + octet_length(material_root)
              + 1
          )
          FROM ${sql(CekProgramMaterialDB.membershipTableName)}
        ), 0)
        + COALESCE((
          SELECT SUM(
            octet_length(tx_id)
              + octet_length(program_envelope_hash)
              + octet_length(material_root)
          )
          FROM ${sql(CekProgramMaterialDB.admissionOwnerTableName)}
        ), 0)
      )::text AS total_bytes`;
  return rows[0]!;
});

export const wrapNativeSubmitTx = (txCanonicalCbor: Buffer): Buffer =>
  encodeMidgardProofSubmission({
    transactionCbor: txCanonicalCbor,
    programMaterial: [],
  });

export const databaseTxHash = (label: string): Buffer =>
  deterministicFixtureTxHash(`database:${label}`);

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

const depositFixtureBaseTimeMs = Date.parse("2026-04-13T17:28:10.000Z");
let depositFixtureSequence = 0;

export const makeDepositEntry = (
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

export const makeDepositSubmissionAttempt = ({
  txHash = databaseTxHash("deposit-submission.default"),
  eventId = databaseOutputReferenceId("deposit-submission.default"),
}: {
  readonly txHash?: Buffer;
  readonly eventId?: Buffer;
} = {}): DepositSubmissionAttemptsDB.InsertSubmittedInput => ({
  [DepositSubmissionAttemptsDB.Columns.TX_HASH]: txHash,
  [DepositSubmissionAttemptsDB.Columns.DEPOSIT_EVENT_ID]: eventId,
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
    structuralLovelace: "0",
    orderOutputIndex: 0,
    l2DatumCbor: null,
    transactionCbor: "84a3008001800200a0f5f6",
  },
  [DepositSubmissionAttemptsDB.Columns.FUNDING_OUT_REFS]: [
    `${databaseTxHash("deposit-submission.funding").toString("hex")}#0`,
  ],
});
