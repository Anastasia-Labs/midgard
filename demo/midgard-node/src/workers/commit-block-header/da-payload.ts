import { encodeMidgardNativeTxCompact } from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { decodeMidgardTxCommitmentsFromCanonicalCbor } from "@al-ft/midgard-validation";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { DaPayloadsDB, PendingBlockFinalizationsDB } from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import { keyValuePhasRoot, type MpfError } from "@/workers/utils/mpf.js";

import { buildAuthenticatedRootFromEncodedEntries } from "./transition-roots.js";

type PayloadRootSet = {
  readonly utxosRoot: string;
  readonly withdrawalsRoot: string;
  readonly forcedTransactionsRoot: string;
  readonly transactionsRoot: string;
  readonly depositsRoot: string;
  readonly transitionTraceRoot: string;
  readonly eventToStepRoot: string;
};

type PayloadCountSet = {
  readonly withdrawalCount: bigint;
  readonly forcedTransactionCount: bigint;
  readonly l2TransactionCount: bigint;
  readonly depositCount: bigint;
  readonly totalEventCount: bigint;
  readonly transitionStepCount: bigint;
};

type PayloadUtxoEntry = {
  readonly outref: Buffer;
  readonly output: Buffer;
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
  payload: SDK.DaPayloadV2,
): Effect.Effect<PayloadRootSet, DatabaseError | MpfError> =>
  Effect.gen(function* () {
    const body = payload.block_body;
    const transactionValues = yield* Effect.forEach(
      entryValues(body.transactions),
      transactionRootValue,
    );
    const [
      utxosRoot,
      withdrawalsRoot,
      forcedTransactionsRoot,
      transactionsRoot,
      depositsRoot,
      transitionTraceRoot,
      eventToStepRoot,
    ] = yield* Effect.all(
      [
        keyValuePhasRoot(entryKeys(body.utxos), entryValues(body.utxos)),
        authenticatedPayloadRoot(
          SDK.ROOT_DOMAINS.withdrawals,
          body.withdrawals,
        ),
        authenticatedPayloadRoot(
          SDK.ROOT_DOMAINS.forcedTransactions,
          body.forced_transactions,
        ),
        buildAuthenticatedRootFromEncodedEntries(
          SDK.ROOT_DOMAINS.transactions,
          entryKeys(body.transactions).map((key, index) => ({
            key,
            value: transactionValues[index]!,
          })),
        ).pipe(Effect.map((root) => root.root)),
        authenticatedPayloadRoot(SDK.ROOT_DOMAINS.deposits, body.deposits),
        authenticatedPayloadRoot(
          SDK.ROOT_DOMAINS.transitionTrace,
          body.transition_trace,
        ),
        authenticatedPayloadRoot(
          SDK.ROOT_DOMAINS.eventToStep,
          body.event_to_step,
        ),
      ],
      { concurrency: "unbounded" },
    );
    return {
      utxosRoot,
      withdrawalsRoot,
      forcedTransactionsRoot,
      transactionsRoot,
      depositsRoot,
      transitionTraceRoot,
      eventToStepRoot,
    };
  });

const authenticatedPayloadRoot = (
  domain: SDK.RootDomain,
  entries: readonly SDK.DaPayloadEntry[],
): Effect.Effect<string, MpfError> =>
  buildAuthenticatedRootFromEncodedEntries(
    domain,
    entryKeys(entries).map((key, index) => ({
      key,
      value: entryValues(entries)[index]!,
    })),
  ).pipe(Effect.map((root) => root.root));

const expectedRoots = (
  record: PendingBlockFinalizationsDB.Record,
): PayloadRootSet => ({
  utxosRoot: record[PendingBlockFinalizationsDB.Columns.EXPECTED_UTXOS_ROOT],
  withdrawalsRoot:
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWALS_ROOT],
  forcedTransactionsRoot:
    record[
      PendingBlockFinalizationsDB.Columns.EXPECTED_FORCED_TRANSACTIONS_ROOT
    ],
  transactionsRoot:
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSACTIONS_ROOT],
  depositsRoot:
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSITS_ROOT],
  transitionTraceRoot:
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSITION_TRACE_ROOT],
  eventToStepRoot:
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_EVENT_TO_STEP_ROOT],
});

const expectedCounts = (
  record: PendingBlockFinalizationsDB.Record,
): PayloadCountSet => ({
  withdrawalCount:
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_WITHDRAWAL_COUNT],
  forcedTransactionCount:
    record[
      PendingBlockFinalizationsDB.Columns.EXPECTED_FORCED_TRANSACTION_COUNT
    ],
  l2TransactionCount:
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_L2_TRANSACTION_COUNT],
  depositCount:
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_DEPOSIT_COUNT],
  totalEventCount:
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_TOTAL_EVENT_COUNT],
  transitionStepCount:
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_TRANSITION_STEP_COUNT],
});

const headerRoots = (header: SDK.Header): PayloadRootSet => ({
  utxosRoot: header.utxosRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
});

const headerCounts = (header: SDK.Header): PayloadCountSet => ({
  withdrawalCount: header.withdrawalCount,
  forcedTransactionCount: header.forcedTransactionCount,
  l2TransactionCount: header.l2TransactionCount,
  depositCount: header.depositCount,
  totalEventCount: header.totalEventCount,
  transitionStepCount: header.transitionStepCount,
});

const rootMismatches = (
  expected: PayloadRootSet,
  actual: PayloadRootSet,
): readonly string[] =>
  [
    expected.utxosRoot === actual.utxosRoot ? null : "utxos_root",
    expected.withdrawalsRoot === actual.withdrawalsRoot
      ? null
      : "withdrawals_root",
    expected.forcedTransactionsRoot === actual.forcedTransactionsRoot
      ? null
      : "forced_transactions_root",
    expected.transactionsRoot === actual.transactionsRoot
      ? null
      : "transactions_root",
    expected.depositsRoot === actual.depositsRoot ? null : "deposits_root",
    expected.transitionTraceRoot === actual.transitionTraceRoot
      ? null
      : "transition_trace_root",
    expected.eventToStepRoot === actual.eventToStepRoot
      ? null
      : "event_to_step_root",
  ].filter((field): field is string => field !== null);

const countMismatches = (
  expected: PayloadCountSet,
  actual: PayloadCountSet,
): readonly string[] =>
  [
    expected.withdrawalCount === actual.withdrawalCount
      ? null
      : "withdrawal_count",
    expected.forcedTransactionCount === actual.forcedTransactionCount
      ? null
      : "forced_transaction_count",
    expected.l2TransactionCount === actual.l2TransactionCount
      ? null
      : "l2_transaction_count",
    expected.depositCount === actual.depositCount ? null : "deposit_count",
    expected.totalEventCount === actual.totalEventCount
      ? null
      : "total_event_count",
    expected.transitionStepCount === actual.transitionStepCount
      ? null
      : "transition_step_count",
  ].filter((field): field is string => field !== null);

const payloadMemberCounts = (payload: SDK.DaPayloadV2): PayloadCountSet => ({
  withdrawalCount: BigInt(payload.block_body.withdrawals.length),
  forcedTransactionCount: BigInt(payload.block_body.forced_transactions.length),
  l2TransactionCount: BigInt(payload.block_body.transactions.length),
  depositCount: BigInt(payload.block_body.deposits.length),
  totalEventCount:
    BigInt(payload.block_body.withdrawals.length) +
    BigInt(payload.block_body.forced_transactions.length) +
    BigInt(payload.block_body.transactions.length) +
    BigInt(payload.block_body.deposits.length),
  transitionStepCount: BigInt(payload.block_body.transition_trace.length),
});

const payloadDeclaredCounts = (payload: SDK.DaPayloadV2): PayloadCountSet =>
  payload.block_body.counts;

const decodeHeader = (
  record: PendingBlockFinalizationsDB.Record,
): Effect.Effect<SDK.Header, DatabaseError> =>
  Effect.try({
    try: () =>
      LucidData.from(
        record[PendingBlockFinalizationsDB.Columns.HEADER_CBOR].toString("hex"),
        SDK.Header as never,
      ) as SDK.Header,
    catch: (cause) =>
      new DatabaseError({
        table: PendingBlockFinalizationsDB.tableName,
        message: "Failed to decode pending block header CBOR for DA payload",
        cause,
      }),
  });

const verifyPayloadCommitments = ({
  record,
  header,
  payload,
  roots,
}: {
  readonly record: PendingBlockFinalizationsDB.Record;
  readonly header: SDK.Header;
  readonly payload: SDK.DaPayloadV2;
  readonly roots: PayloadRootSet;
}): Effect.Effect<void, DatabaseError> =>
  Effect.gen(function* () {
    const headerHash =
      record[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex");
    const computedHeaderHash = yield* SDK.hashBlockHeader(header).pipe(
      Effect.mapError(
        (cause) =>
          new DatabaseError({
            table: DaPayloadsDB.tableName,
            message: "Failed to hash pending block header for DA payload",
            cause,
          }),
      ),
    );
    const expected = expectedRoots(record);
    const counts = expectedCounts(record);
    const mismatches = [
      ...rootMismatches(expected, roots),
      ...rootMismatches(headerRoots(header), roots).map(
        (field) => `header_${field}`,
      ),
      ...countMismatches(counts, payloadDeclaredCounts(payload)),
      ...countMismatches(counts, payloadMemberCounts(payload)).map(
        (field) => `member_${field}`,
      ),
      ...countMismatches(
        headerCounts(header),
        payloadDeclaredCounts(payload),
      ).map((field) => `header_${field}`),
      payload.block_body.event_to_step.length ===
      payload.block_body.transition_trace.length
        ? null
        : "event_to_step_count",
      payload.block_body.header_hash === headerHash
        ? null
        : "payload_header_hash",
      computedHeaderHash === headerHash ? null : "computed_header_hash",
    ].filter((field): field is string => field !== null);
    if (mismatches.length > 0) {
      return yield* Effect.fail(
        new DatabaseError({
          table: DaPayloadsDB.tableName,
          message:
            "Refusing to persist DA payload because recomputed commitments do not match the pending block header",
          cause: `header_hash=${headerHash},mismatches=${mismatches.join(",")}`,
        }),
      );
    }
  });

export const buildDaPayloadInsert = ({
  record,
  utxos,
}: {
  readonly record: PendingBlockFinalizationsDB.Record;
  readonly utxos?: readonly PayloadUtxoEntry[];
}): Effect.Effect<DaPayloadsDB.InsertInput, DatabaseError | MpfError> =>
  Effect.gen(function* () {
    const payloadUtxos =
      utxos ??
      record.utxoMembers.map((member) => ({
        outref: member[PendingBlockFinalizationsDB.UtxoColumns.OUTREF],
        output: member[PendingBlockFinalizationsDB.UtxoColumns.OUTPUT],
      }));
    const header = yield* decodeHeader(record);
    const counts = expectedCounts(record);
    const payload: SDK.DaPayloadV2 = {
      version: SDK.DA_PAYLOAD_V2_VERSION,
      block_body: {
        header_hash:
          record[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString(
            "hex",
          ),
        header,
        utxos: sortedEntries(
          payloadUtxos.map((entry) => bufferEntry(entry.outref, entry.output)),
        ),
        withdrawals: sortedEntries(
          record.withdrawalMembers.map((member) =>
            bufferEntry(
              member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
              member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
            ),
          ),
        ),
        forced_transactions: sortedEntries(
          record.forcedTransactionMembers.map((member) =>
            bufferEntry(
              member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
              member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
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
        transition_trace: sortedEntries(
          record.transitionTraceMembers.map((member) =>
            bufferEntry(
              member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
              member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
            ),
          ),
        ),
        event_to_step: sortedEntries(
          record.eventToStepMembers.map((member) =>
            bufferEntry(
              member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
              member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
            ),
          ),
        ),
        counts,
      },
    };
    const roots = yield* computeDaPayloadRoots(payload);
    yield* verifyPayloadCommitments({ record, header, payload, roots });
    const payloadCbor = SDK.encodeDaPayloadV2(payload);
    return {
      [DaPayloadsDB.Columns.HEADER_HASH]:
        record[PendingBlockFinalizationsDB.Columns.HEADER_HASH],
      [DaPayloadsDB.Columns.VERSION]: Number(SDK.DA_PAYLOAD_V2_VERSION),
      [DaPayloadsDB.Columns.PAYLOAD_CBOR]: payloadCbor,
      [DaPayloadsDB.Columns.PAYLOAD_SHA256]: Buffer.from(
        SDK.daPayloadHashHex(payloadCbor),
        "hex",
      ),
      [DaPayloadsDB.Columns.UTXOS_ROOT]: roots.utxosRoot,
      [DaPayloadsDB.Columns.FORCED_TRANSACTIONS_ROOT]:
        roots.forcedTransactionsRoot,
      [DaPayloadsDB.Columns.TRANSACTIONS_ROOT]: roots.transactionsRoot,
      [DaPayloadsDB.Columns.DEPOSITS_ROOT]: roots.depositsRoot,
      [DaPayloadsDB.Columns.WITHDRAWALS_ROOT]: roots.withdrawalsRoot,
      [DaPayloadsDB.Columns.TRANSITION_TRACE_ROOT]: roots.transitionTraceRoot,
      [DaPayloadsDB.Columns.EVENT_TO_STEP_ROOT]: roots.eventToStepRoot,
      [DaPayloadsDB.Columns.WITHDRAWAL_COUNT]: counts.withdrawalCount,
      [DaPayloadsDB.Columns.FORCED_TRANSACTION_COUNT]:
        counts.forcedTransactionCount,
      [DaPayloadsDB.Columns.L2_TRANSACTION_COUNT]: counts.l2TransactionCount,
      [DaPayloadsDB.Columns.DEPOSIT_COUNT]: counts.depositCount,
      [DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]: counts.totalEventCount,
      [DaPayloadsDB.Columns.TRANSITION_STEP_COUNT]: counts.transitionStepCount,
      [DaPayloadsDB.Columns.BLOCK_START_TIME]:
        record[PendingBlockFinalizationsDB.Columns.BLOCK_START_TIME],
      [DaPayloadsDB.Columns.BLOCK_END_TIME]:
        record[PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME],
    };
  });
