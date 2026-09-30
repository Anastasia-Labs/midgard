import {
  encodeMidgardCekProgramMaterialDaValue,
  mergeMidgardCekProgramMaterialSidecars,
} from "@al-ft/midgard-core/cek-proof";
import * as SDK from "@al-ft/midgard-sdk";
import { Effect, Metric } from "effect";

import {
  DaPayloadsDB,
  ForcedTransactionsDB,
  PendingBlockFinalizationsDB,
} from "../../database/index.js";
import { DatabaseError } from "../../database/utils/common.js";
import {
  keyValuePhasRoot,
  ledgerOutputToInsertBatchOp,
  type MpfError,
} from "../../mpf/index.js";
import { buildAuthenticatedRootFromEncodedEntries } from "./transition-roots.js";

export type PayloadRootSet = {
  readonly utxosRoot: string;
  readonly withdrawalsRoot: string;
  readonly forcedTransactionsRoot: string;
  readonly transactionsRoot: string;
  readonly depositsRoot: string;
  readonly transitionTraceRoot: string;
  readonly eventToStepRoot: string;
  readonly validationTracesRoot: string;
};

export type PayloadCountSet = {
  readonly withdrawalCount: bigint;
  readonly forcedTransactionCount: bigint;
  readonly l2TransactionCount: bigint;
  readonly depositCount: bigint;
  readonly totalEventCount: bigint;
  readonly transitionStepCount: bigint;
  readonly validationTraceCount: bigint;
};

export type PayloadUtxoEntry = {
  readonly outref: Buffer;
  readonly output: Buffer;
};

type DecodedForcedTransactionJournalMember =
  ForcedTransactionsDB.ForcedTransactionJournalMember & {
    readonly key: Buffer;
  };

export const daPayloadBytesUncompressedGauge = Metric.gauge(
  "da_payload_bytes_uncompressed",
  { description: "Uncompressed canonical DA inner payload bytes per block" },
);

export const daPayloadBytesEnvelopeGauge = Metric.gauge(
  "da_payload_bytes_envelope",
  {
    description: "Stored/transmitted DA payload bytes per block",
  },
);

export const daPayloadCompressionRatioGauge = Metric.gauge(
  "da_payload_compression_ratio",
  { description: "Uncompressed bytes divided by stored DA payload bytes" },
);

export const daPayloadCompressDurationTimer = Metric.timer(
  "da_payload_compress_duration_ms",
  "Duration of DA payload envelope construction and compression",
);

export const bufferEntry = (key: Buffer, value: Buffer): SDK.DaPayloadEntry => [
  key.toString("hex"),
  value.toString("hex"),
];

export const journalCekProgramMaterial = (
  record: PendingBlockFinalizationsDB.Record,
  forcedMembers: readonly DecodedForcedTransactionJournalMember[],
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
  for (const forced of forcedMembers) {
    sidecars.push(Buffer.from(forced.programMaterialSidecarCbor));
  }
  return Object.freeze(
    mergeMidgardCekProgramMaterialSidecars(sidecars).map((entry) =>
      bufferEntry(
        Buffer.from(entry.root),
        encodeMidgardCekProgramMaterialDaValue(entry),
      ),
    ),
  );
};

export const sortedEntries = (
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
  payload: SDK.DaPayload,
): Effect.Effect<PayloadRootSet, DatabaseError | MpfError> =>
  Effect.gen(function* () {
    const body = payload.block_body;
    const transactionValues = entryValues(body.transactions);
    const utxoDescriptorOps = yield* Effect.try({
      try: () =>
        body.utxos.map(([outRef, output]) =>
          ledgerOutputToInsertBatchOp({
            outRef: Buffer.from(outRef, "hex"),
            outputCbor: Buffer.from(output, "hex"),
          }),
        ),
      catch: (cause) =>
        new DatabaseError({
          table: DaPayloadsDB.tableName,
          message:
            "Refusing a V1 DA payload whose full UTxOs cannot produce exact canonical descriptors",
          cause,
        }),
    });
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
        keyValuePhasRoot(
          utxoDescriptorOps.map((op) => op.key),
          utxoDescriptorOps.map((op) => op.value),
        ),
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

export const expectedRoots = (
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

export const expectedCounts = (
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

export const headerRoots = (header: SDK.Header): PayloadRootSet => ({
  utxosRoot: header.utxosRoot,
  withdrawalsRoot: header.withdrawalsRoot,
  forcedTransactionsRoot: header.forcedTransactionsRoot,
  transactionsRoot: header.transactionsRoot,
  depositsRoot: header.depositsRoot,
  transitionTraceRoot: header.transitionTraceRoot,
  eventToStepRoot: header.eventToStepRoot,
  validationTracesRoot: header.validationTracesRoot,
});

export const headerCounts = (header: SDK.Header): PayloadCountSet => ({
  withdrawalCount: header.withdrawalCount,
  forcedTransactionCount: header.forcedTransactionCount,
  l2TransactionCount: header.l2TransactionCount,
  depositCount: header.depositCount,
  totalEventCount: header.totalEventCount,
  transitionStepCount: header.transitionStepCount,
  validationTraceCount: header.validationTraceCount,
});

export const rootMismatches = (
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
