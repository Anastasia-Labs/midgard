import {
  encodeMidgardCekProgramMaterialDaValueV1,
  mergeMidgardCekProgramMaterialSidecarsV1,
} from "@al-ft/midgard-core/cek-proof";
import { MIDGARD_CONSENSUS_PROFILE_V1 } from "@al-ft/midgard-core/consensus-profile-v1";
import { wrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { Duration, Effect, Metric } from "effect";

import { readDaHardeningConfig } from "@/da/hardening-config.js";
import {
  DaPayloadsDB,
  ForcedTransactionsDB,
  PendingBlockFinalizationsDB,
} from "@/database/index.js";
import { DatabaseError } from "@/database/utils/common.js";
import {
  encodeTransactionRootValue,
  keyValuePhasRoot,
  type MpfError,
} from "@/workers/utils/mpf.js";

import { buildAuthenticatedRootFromEncodedEntries } from "./transition-roots.js";

type PayloadRootSet = {
  readonly utxosRoot: string;
  readonly withdrawalsRoot: string;
  readonly forcedTransactionsRoot: string;
  readonly transactionsRoot: string;
  readonly depositsRoot: string;
  readonly transitionTraceRoot: string;
  readonly eventToStepRoot: string;
  readonly validationTracesRoot: string;
};

type PayloadCountSet = {
  readonly withdrawalCount: bigint;
  readonly forcedTransactionCount: bigint;
  readonly l2TransactionCount: bigint;
  readonly depositCount: bigint;
  readonly totalEventCount: bigint;
  readonly transitionStepCount: bigint;
  readonly validationTraceCount: bigint;
};

type PayloadUtxoEntry = {
  readonly outref: Buffer;
  readonly output: Buffer;
};

const daPayloadBytesUncompressedGauge = Metric.gauge(
  "da_payload_bytes_uncompressed",
  { description: "Uncompressed canonical DA inner payload bytes per block" },
);
const daPayloadBytesEnvelopeGauge = Metric.gauge("da_payload_bytes_envelope", {
  description: "Stored/transmitted DA payload bytes per block",
});
const daPayloadCompressionRatioGauge = Metric.gauge(
  "da_payload_compression_ratio",
  { description: "Uncompressed bytes divided by stored DA payload bytes" },
);
const daPayloadCompressDurationTimer = Metric.timer(
  "da_payload_compress_duration_ms",
  "Duration of DA payload envelope construction and compression",
);

const bufferEntry = (key: Buffer, value: Buffer): SDK.DaPayloadEntry => [
  key.toString("hex"),
  value.toString("hex"),
];

const journalCekProgramMaterial = (
  record: PendingBlockFinalizationsDB.Record,
): readonly SDK.DaPayloadEntry[] => {
  const sidecars: Buffer[] = [];
  for (const member of record.txMembers) {
    const sidecar =
      member[
        PendingBlockFinalizationsDB.MemberColumns
          .CEK_PROGRAM_MATERIAL_SIDECAR_CBOR
      ];
    if (sidecar == null) {
      throw new Error(
        `V1 transaction ${member[
          PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID
        ].toString("hex")} is missing journaled CEK program material`,
      );
    }
    sidecars.push(Buffer.from(sidecar));
  }
  for (const member of record.forcedTransactionMembers) {
    const forced = ForcedTransactionsDB.decodeForcedTransactionJournalMemberV1(
      member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
    );
    sidecars.push(Buffer.from(forced.programMaterialSidecarCbor));
  }
  return Object.freeze(
    mergeMidgardCekProgramMaterialSidecarsV1(sidecars).map((entry) =>
      bufferEntry(
        Buffer.from(entry.root),
        encodeMidgardCekProgramMaterialDaValueV1(entry),
      ),
    ),
  );
};

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

export const computeDaPayloadRoots = (
  payload: SDK.DaPayloadV1,
): Effect.Effect<PayloadRootSet, DatabaseError | MpfError> =>
  Effect.gen(function* () {
    const body = payload.block_body;
    const transactionValues = entryValues(body.transactions);
    const [
      utxosRoot,
      withdrawalsRoot,
      forcedTransactionsRoot,
      transactionsRoot,
      depositsRoot,
      transitionTraceRoot,
      eventToStepRoot,
      validationTracesRoot,
    ] = yield* Effect.all(
      [
        keyValuePhasRoot(entryKeys(body.utxos), entryValues(body.utxos)),
        authenticatedPayloadRoot(
          SDK.ROOT_DOMAINS.withdrawals,
          body.withdrawals,
        ),
        authenticatedPayloadRoot(
          SDK.ROOT_DOMAINS.forcedTransactionsV1,
          body.forced_transactions,
        ),
        buildAuthenticatedRootFromEncodedEntries(
          SDK.ROOT_DOMAINS.transactionsV1,
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
        authenticatedPayloadRoot(
          SDK.ROOT_DOMAINS.validationTraces,
          body.validation_traces,
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
      validationTracesRoot,
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
  validationTracesRoot:
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_VALIDATION_TRACES_ROOT],
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
  validationTraceCount:
    record[PendingBlockFinalizationsDB.Columns.EXPECTED_VALIDATION_TRACE_COUNT],
});

const headerRoots = (header: SDK.HeaderV1): PayloadRootSet => ({
  utxosRoot: header.utxosRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
  validationTracesRoot: header.validationTracesRoot,
});

const headerCounts = (header: SDK.HeaderV1): PayloadCountSet => ({
  withdrawalCount: header.withdrawalCount,
  forcedTransactionCount: header.forcedTransactionCount,
  l2TransactionCount: header.l2TransactionCount,
  depositCount: header.depositCount,
  totalEventCount: header.totalEventCount,
  transitionStepCount: header.transitionStepCount,
  validationTraceCount: header.validationTraceCount,
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
    expected.validationTracesRoot === actual.validationTracesRoot
      ? null
      : "validation_traces_root",
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
    expected.validationTraceCount === actual.validationTraceCount
      ? null
      : "validation_trace_count",
  ].filter((field): field is string => field !== null);

const payloadMemberCounts = (payload: SDK.DaPayloadV1): PayloadCountSet => ({
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
  validationTraceCount: BigInt(payload.block_body.validation_traces.length),
});

const payloadDeclaredCounts = (
  payload: SDK.DaPayloadV1,
): PayloadCountSet => payload.block_body.counts;

const decodeHeader = (
  record: PendingBlockFinalizationsDB.Record,
): Effect.Effect<SDK.HeaderV1, DatabaseError> =>
  Effect.try({
    try: () =>
      LucidData.from(
        record[PendingBlockFinalizationsDB.Columns.HEADER_CBOR].toString("hex"),
        SDK.HeaderV1,
      ) as SDK.HeaderV1,
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
  readonly header: SDK.HeaderV1;
  readonly payload: SDK.DaPayloadV1;
  readonly roots: PayloadRootSet;
}): Effect.Effect<void, DatabaseError> =>
  Effect.gen(function* () {
    const headerHash =
      record[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex");
    if (
      record[PendingBlockFinalizationsDB.Columns.CONSENSUS_PROFILE_ID] !==
      MIDGARD_CONSENSUS_PROFILE_V1.profileId ||
      payload.version !== SDK.DA_PAYLOAD_V1_VERSION
    ) {
      return yield* Effect.fail(
        new DatabaseError({
          table: DaPayloadsDB.tableName,
          message:
            "Pending journal profile, header generation, and DA payload generation do not match",
          cause: `profile=${
            record[PendingBlockFinalizationsDB.Columns.CONSENSUS_PROFILE_ID]
          },payload_version=${payload.version.toString()}`,
        }),
      );
    }
    const computedHeaderHash = yield* SDK.hashBlockHeaderV1(header).pipe(
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
      payload.block_body.validation_traces.length ===
      Number(counts.validationTraceCount)
        ? null
        : "validation_trace_count",
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
  envelope,
}: {
  readonly record: PendingBlockFinalizationsDB.Record;
  readonly utxos: readonly PayloadUtxoEntry[];
  readonly envelope?: {
    readonly mode: "identity" | "zstd";
    readonly zstdLevel: number;
  };
}): Effect.Effect<DaPayloadsDB.InsertInput, DatabaseError | MpfError> =>
  Effect.gen(function* () {
    const payloadUtxos = utxos;
    const header = yield* decodeHeader(record);
    const counts = expectedCounts(record);
    const profileId =
      record[PendingBlockFinalizationsDB.Columns.CONSENSUS_PROFILE_ID];
    if (profileId !== MIDGARD_CONSENSUS_PROFILE_V1.profileId) {
      return yield* Effect.fail(
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Refusing to build DA payload for an unknown consensus profile",
          cause: `profile=${profileId}`,
        }),
      );
    }
    const cekProgramMaterial = yield* Effect.try({
      try: () => journalCekProgramMaterial(record),
      catch: (cause) =>
        new DatabaseError({
          table: PendingBlockFinalizationsDB.tableName,
          message:
            "Refusing to build V1 DA without exact journaled CEK program material",
          cause,
        }),
    });
    const transactionSources = record.txMembers.map((member) =>
      bufferEntry(
        member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
        encodeTransactionRootValue(
          member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
          MIDGARD_CONSENSUS_PROFILE_V1,
        ),
      ),
    );
    const forcedMembers = record.forcedTransactionMembers.map((member) => ({
      key: member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
      ...ForcedTransactionsDB.decodeForcedTransactionJournalMemberV1(
        member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
      ),
    }));
    const commonBody = {
      header_hash:
        record[PendingBlockFinalizationsDB.Columns.HEADER_HASH].toString("hex"),
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
        forcedMembers.map((member) =>
          bufferEntry(member.key, member.sourceValueCbor),
        ),
      ),
      transactions: sortedEntries(transactionSources),
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
    } as const;
    const payload: SDK.DaPayloadV1 = {
      version: SDK.DA_PAYLOAD_V1_VERSION,
      block_body: {
        ...commonBody,
        header,
        transaction_preimages: sortedEntries(
          record.txMembers.map((member) =>
            bufferEntry(
              member[PendingBlockFinalizationsDB.MemberColumns.MEMBER_ID],
              member[PendingBlockFinalizationsDB.MemberColumns.PAYLOAD_CBOR],
            ),
          ),
        ),
        forced_transaction_preimages: sortedEntries(
          forcedMembers.map((member) =>
            bufferEntry(member.key, member.canonicalTransactionCbor),
          ),
        ),
        cek_program_material: [...cekProgramMaterial],
        validation_traces: sortedEntries(
          record.validationTraceMembers.map((member) =>
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
    const innerPayloadCbor = SDK.encodeDaPayloadV1(payload);
    const envelopeConfig =
      envelope ??
      (() => {
        const config = readDaHardeningConfig();
        return { mode: config.envelopeMode, zstdLevel: config.zstdLevel };
      })();
    const compressionStartedAt = Date.now();
    const payloadCbor = yield* Effect.tryPromise({
      try: () =>
        wrapDaPayloadV1(innerPayloadCbor, {
          mode: envelopeConfig.mode,
          zstdLevel: envelopeConfig.zstdLevel,
        }),
      catch: (cause) =>
        new DatabaseError({
          table: DaPayloadsDB.tableName,
          message: "Failed to encode canonical V1 DA payload envelope",
          cause,
        }),
    });
    yield* daPayloadBytesUncompressedGauge(
      Effect.succeed(innerPayloadCbor.length),
    );
    yield* daPayloadBytesEnvelopeGauge(Effect.succeed(payloadCbor.length));
    yield* daPayloadCompressionRatioGauge(
      Effect.succeed(innerPayloadCbor.length / payloadCbor.length),
    );
    yield* Metric.update(
      daPayloadCompressDurationTimer,
      Duration.millis(Date.now() - compressionStartedAt),
    );
    return {
      [DaPayloadsDB.Columns.HEADER_HASH]:
        record[PendingBlockFinalizationsDB.Columns.HEADER_HASH],
      [DaPayloadsDB.Columns.CONSENSUS_PROFILE_ID]: profileId,
      [DaPayloadsDB.Columns.VERSION]: 1 as const,
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
      [DaPayloadsDB.Columns.VALIDATION_TRACES_ROOT]: roots.validationTracesRoot,
      [DaPayloadsDB.Columns.WITHDRAWAL_COUNT]: counts.withdrawalCount,
      [DaPayloadsDB.Columns.FORCED_TRANSACTION_COUNT]:
        counts.forcedTransactionCount,
      [DaPayloadsDB.Columns.L2_TRANSACTION_COUNT]: counts.l2TransactionCount,
      [DaPayloadsDB.Columns.DEPOSIT_COUNT]: counts.depositCount,
      [DaPayloadsDB.Columns.TOTAL_EVENT_COUNT]: counts.totalEventCount,
      [DaPayloadsDB.Columns.TRANSITION_STEP_COUNT]: counts.transitionStepCount,
      [DaPayloadsDB.Columns.VALIDATION_TRACE_COUNT]:
        counts.validationTraceCount,
      [DaPayloadsDB.Columns.BLOCK_START_TIME]:
        record[PendingBlockFinalizationsDB.Columns.BLOCK_START_TIME],
      [DaPayloadsDB.Columns.BLOCK_END_TIME]:
        record[PendingBlockFinalizationsDB.Columns.BLOCK_END_TIME],
    };
  });
