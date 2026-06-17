import { encodeMidgardNativeTxCompact } from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { decodeMidgardTxCommitmentsFromCanonicalCbor } from "@al-ft/midgard-validation";
import { Effect } from "effect";

import {
  DaPayloadsDB,
  MempoolLedgerDB,
  PendingBlockFinalizationsDB,
} from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { keyValuePhasRoot, type MpfError } from "@/workers/utils/mpf.js";

type PayloadRootSet = {
  readonly utxosRoot: string;
  readonly transactionsRoot: string;
  readonly depositsRoot: string;
  readonly withdrawalsRoot: string;
};

const bufferEntry = (key: Buffer, value: Buffer): SDK.DaPayloadEntry => [
  key.toString("hex"),
  value.toString("hex"),
];

const sortedEntries = (
  entries: readonly SDK.DaPayloadEntry[],
): SDK.DaPayloadEntry[] =>
  [...entries].sort(([left], [right]) =>
    left < right ? -1 : left > right ? 1 : 0,
  );

const entryKeys = (entries: readonly SDK.DaPayloadEntry[]): readonly Buffer[] =>
  entries.map(([key]) => Buffer.from(key, "hex"));

const entryValues = (
  entries: readonly SDK.DaPayloadEntry[],
): readonly Buffer[] => entries.map(([, value]) => Buffer.from(value, "hex"));

const transactionRootValue = (
  txCanonicalCbor: Buffer,
): Effect.Effect<Buffer, DatabaseError> =>
  Effect.try({
    try: () =>
      encodeMidgardNativeTxCompact(
        decodeMidgardTxCommitmentsFromCanonicalCbor(txCanonicalCbor)
          .transactionCompact,
      ),
    catch: (cause) =>
      new DatabaseError({
        table: PendingBlockFinalizationsDB.tableName,
        message:
          "Failed to derive compact transaction root value for DA payload",
        cause,
      }),
  });

export const computeDaPayloadRoots = (
  payload: SDK.DaPayloadV1,
): Effect.Effect<PayloadRootSet, DatabaseError | MpfError> =>
  Effect.gen(function* () {
    const transactionValues = yield* Effect.forEach(
      entryValues(payload.block_body.transactions),
      transactionRootValue,
    );
    const [utxosRoot, transactionsRoot, depositsRoot, withdrawalsRoot] =
      yield* Effect.all(
        [
          keyValuePhasRoot(
            entryKeys(payload.block_body.utxos),
            entryValues(payload.block_body.utxos),
          ),
          keyValuePhasRoot(
            entryKeys(payload.block_body.transactions),
            transactionValues,
          ),
          keyValuePhasRoot(
            entryKeys(payload.block_body.deposits),
            entryValues(payload.block_body.deposits),
          ),
          keyValuePhasRoot(
            entryKeys(payload.block_body.withdrawals),
            entryValues(payload.block_body.withdrawals),
          ),
        ],
        { concurrency: "unbounded" },
      );
    return {
      utxosRoot,
      transactionsRoot,
      depositsRoot,
      withdrawalsRoot,
    };
  });

const expectedRoots = (
  record: PendingBlockFinalizationsDB.Record,
): PayloadRootSet => ({
  utxosRoot: record[PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT],
  transactionsRoot:
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSACTIONS_ROOT],
  depositsRoot:
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSITS_ROOT],
  withdrawalsRoot:
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWALS_ROOT],
});

const rootMismatches = (
  expected: PayloadRootSet,
  actual: PayloadRootSet,
): readonly string[] =>
  [
    expected.utxosRoot === actual.utxosRoot ? null : "utxos_root",
    expected.transactionsRoot === actual.transactionsRoot
      ? null
      : "transactions_root",
    expected.depositsRoot === actual.depositsRoot ? null : "deposits_root",
    expected.withdrawalsRoot === actual.withdrawalsRoot
      ? null
      : "withdrawals_root",
  ].filter((field): field is string => field !== null);

export const buildDaPayloadInsert = ({
  record,
  utxos,
}: {
  readonly record: PendingBlockFinalizationsDB.Record;
  readonly utxos: readonly MempoolLedgerDB.EntryWithTimeStamp[];
}): Effect.Effect<DaPayloadsDB.InsertInput, DatabaseError | MpfError> =>
  Effect.gen(function* () {
    const payload: SDK.DaPayloadV1 = {
      version: SDK.DA_PAYLOAD_V1_VERSION,
      header_hash:
        record[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex"),
      block_body: {
        utxos: sortedEntries(
          utxos.map((entry) =>
            bufferEntry(
              entry[MempoolLedgerDB.Columns.OUTREF],
              entry[MempoolLedgerDB.Columns.OUTPUT],
            ),
          ),
        ),
        transactions: sortedEntries(
          record.txMembers.map((member) =>
            bufferEntry(
              member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
              member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
            ),
          ),
        ),
        deposits: sortedEntries(
          record.depositMembers.map((member) =>
            bufferEntry(
              member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
              member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
            ),
          ),
        ),
        withdrawals: sortedEntries(
          record.withdrawalMembers.map((member) =>
            bufferEntry(
              member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
              member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
            ),
          ),
        ),
      },
    };
    const roots = yield* computeDaPayloadRoots(payload);
    const mismatches = rootMismatches(expectedRoots(record), roots);
    if (mismatches.length > 0) {
      return yield* Effect.fail(
        new DatabaseError({
          table: DaPayloadsDB.tableName,
          message:
            "Refusing to persist DA payload because recomputed roots do not match the pending block commitment",
          cause: `header_hash=${payload.header_hash},mismatches=${mismatches.join(
            ",",
          )}`,
        }),
      );
    }
    const payloadCbor = SDK.encodeDaPayloadV1(payload);
    return {
      [DaPayloadsDB.Columns.HEADER_HASH]:
        record[PendingBlockFinalizationsDB.Columns.HEADER_HASH],
      [DaPayloadsDB.Columns.VERSION]: Number(SDK.DA_PAYLOAD_V1_VERSION),
      [DaPayloadsDB.Columns.PAYLOAD_CBOR]: payloadCbor,
      [DaPayloadsDB.Columns.PAYLOAD_SHA256]: Buffer.from(
        SDK.daPayloadHashHex(payloadCbor),
        "hex",
      ),
      [DaPayloadsDB.Columns.UTXOS_ROOT]: roots.utxosRoot,
      [DaPayloadsDB.Columns.TRANSACTIONS_ROOT]: roots.transactionsRoot,
      [DaPayloadsDB.Columns.DEPOSITS_ROOT]: roots.depositsRoot,
      [DaPayloadsDB.Columns.WITHDRAWALS_ROOT]: roots.withdrawalsRoot,
      [DaPayloadsDB.Columns.BLOCK_START_TIME]:
        record[PendingBlockFinalizationsDB.Columns.BLOCK_START_TIME],
      [DaPayloadsDB.Columns.BLOCK_END_TIME]:
        record[PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME],
    };
  });
