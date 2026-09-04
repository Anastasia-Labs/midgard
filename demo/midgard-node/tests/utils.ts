import { createHash } from "node:crypto";

import * as SDK from "@al-ft/midgard-sdk";
import { SqlClient } from "@effect/sql";
import { type Address, Data as LucidData } from "@lucid-evolution/lucid";
import { Effect, Layer } from "effect";
import { expect } from "vitest";

import * as TxAdmissionsDB from "../src/database/txAdmissions.js";
import * as LedgerUtils from "../src/database/utils/ledger.js";
import {
  ADMISSION_WRITE_BATCH_MAX_ROWS,
  ADMISSION_WRITE_BATCH_TARGET_ROWS,
  ADMISSION_WRITE_QUEUE_CAPACITY,
  ADMISSION_WRITE_SHARD_COUNT,
  AdmissionWriter,
  AdmissionWriterLive,
  makeAdmissionWriterWithOptions,
} from "../src/services/admission-writer.js";
import { NodeConfig } from "../src/services/config.js";
import { AdmissionSql, Database } from "../src/services/database.js";
import { WriteBehindLive } from "../src/services/write-behind.js";
import { applyMidgardNodeTestEnv } from "./test-env.js";

// Importing this module is what pins a test file to its worker's database
// shard; see tests/test-env.ts for why the shard is assigned rather than read
// from an ambient POSTGRES_DB.
applyMidgardNodeTestEnv();

const AdmissionWriterTestLive = Layer.scoped(
  AdmissionWriter,
  Effect.gen(function* () {
    const admissionSql = yield* AdmissionSql;
    return yield* makeAdmissionWriterWithOptions(
      (requests) =>
        TxAdmissionsDB.admitReservedBatch(requests).pipe(
          Effect.provideService(SqlClient.SqlClient, admissionSql),
        ),
      {
        shardCount: ADMISSION_WRITE_SHARD_COUNT,
        batchMaxRows: ADMISSION_WRITE_BATCH_MAX_ROWS,
        batchTargetRows: ADMISSION_WRITE_BATCH_TARGET_ROWS,
        batchDeadlineMs: 0,
        queueCapacity: ADMISSION_WRITE_QUEUE_CAPACITY,
      },
    );
  }),
);

const admissionWriterLayer =
  process.env.PHASE1_ADMISSION_OPERATOR === "1"
    ? AdmissionWriterLive
    : AdmissionWriterTestLive;

export const provideDatabaseLayers = <A, E, R>(eff: Effect.Effect<A, E, R>) =>
  eff.pipe(
    Effect.provide(WriteBehindLive),
    Effect.provide(admissionWriterLayer),
    Effect.provide(Database.layer),
    Effect.provide(NodeConfig.layer),
  );

export const deterministicFixtureBytes = (
  label: string,
  length: number,
): Buffer => {
  const chunks: Buffer[] = [];
  let bytesGenerated = 0;
  for (let counter = 0; bytesGenerated < length; counter += 1) {
    const chunk = createHash("sha256")
      .update("midgard-node-test-fixture")
      .update("\0")
      .update(label)
      .update("\0")
      .update(counter.toString())
      .digest();
    chunks.push(chunk);
    bytesGenerated += chunk.length;
  }
  return Buffer.concat(chunks).subarray(0, length);
};

export const deterministicFixtureTxHash = (label: string): Buffer =>
  deterministicFixtureBytes(`tx-hash:${label}`, 32);

export const deterministicFixtureOutputReference = (
  label: string,
  outputIndex: number | bigint = 0n,
): SDK.OutputReference => ({
  transactionId: deterministicFixtureTxHash(
    `output-reference:${label}`,
  ).toString("hex"),
  outputIndex: BigInt(outputIndex),
});

export const deterministicFixtureOutputReferenceId = (
  label: string,
  outputIndex: number | bigint = 0n,
): Buffer =>
  Buffer.from(
    LucidData.to(
      deterministicFixtureOutputReference(label, outputIndex),
      SDK.OutputReference,
    ),
    "hex",
  );

type LedgerLikeEntry = {
  readonly [LedgerUtils.Columns.TX_ID]: Buffer;
  readonly [LedgerUtils.Columns.OUTREF]: Buffer;
  readonly [LedgerUtils.Columns.OUTPUT]: Buffer;
  readonly [LedgerUtils.Columns.ADDRESS]: Address;
};

const withoutLedgerTimestamp = (
  entry: LedgerLikeEntry,
): LedgerUtils.EntryNoTimeStamp => ({
  [LedgerUtils.Columns.TX_ID]: entry[LedgerUtils.Columns.TX_ID],
  [LedgerUtils.Columns.OUTREF]: entry[LedgerUtils.Columns.OUTREF],
  [LedgerUtils.Columns.OUTPUT]: entry[LedgerUtils.Columns.OUTPUT],
  [LedgerUtils.Columns.ADDRESS]: entry[LedgerUtils.Columns.ADDRESS],
});

const sortLedgerEntries = (
  entries: readonly LedgerUtils.EntryNoTimeStamp[],
): LedgerUtils.EntryNoTimeStamp[] =>
  [...entries].sort((left, right) =>
    Buffer.compare(
      left[LedgerUtils.Columns.OUTREF],
      right[LedgerUtils.Columns.OUTREF],
    ),
  );

export const expectLedgerUtxos = (
  actual: readonly LedgerLikeEntry[],
  expected: readonly LedgerLikeEntry[],
): void => {
  expect(
    sortLedgerEntries(actual.map((entry) => withoutLedgerTimestamp(entry))),
  ).toStrictEqual(
    sortLedgerEntries(expected.map((entry) => withoutLedgerTimestamp(entry))),
  );
};
