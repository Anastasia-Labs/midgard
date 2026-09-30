import { normalizeHex } from "@al-ft/midgard-core/hex";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { transitionTraceError } from "./errors.js";
import {
  type CountedRoot,
  type KeyValuePhasEntry,
  type KeyValuePhasRoot,
} from "./phas.js";

export type DataSchema = Parameters<typeof Data.Nullable>[0];

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

export type DecodedRootEntry<K, V> = {
  readonly key: K;
  readonly value: V;
  readonly keyBytes: Buffer;
  readonly valueBytes: Buffer;
};

export type DecodedTransactionEntry = {
  readonly txId: string;
  readonly keyBytes: Buffer;
  readonly value: SDK.L2TransactionSource;
  readonly valueBytes: Buffer;
  readonly fullTransactionCbor: Buffer;
  readonly validity: SDK.MidgardTxValidity;
  readonly spendInputsPreimage: Buffer;
  readonly outputsPreimage: Buffer;
};

export type DecodedForcedTransactionEntry = DecodedRootEntry<
  SDK.OutputReference,
  SDK.ForcedInclusionTxV1
> & {
  readonly fullTransactionCbor: Buffer;
};

export type SourceEventRecord =
  | {
      readonly phase: "Withdrawal";
      readonly eventKey: SDK.EventKey;
      readonly fingerprint: string;
      readonly entry: DecodedRootEntry<SDK.OutputReference, SDK.WithdrawalInfo>;
    }
  | {
      readonly phase: "ForcedTransaction";
      readonly eventKey: SDK.EventKey;
      readonly fingerprint: string;
      readonly entry: DecodedForcedTransactionEntry;
    }
  | {
      readonly phase: "L2Transaction";
      readonly eventKey: SDK.EventKey;
      readonly fingerprint: string;
      readonly entry: DecodedTransactionEntry;
    }
  | {
      readonly phase: "Deposit";
      readonly eventKey: SDK.EventKey;
      readonly fingerprint: string;
      readonly entry: DecodedRootEntry<SDK.OutputReference, SDK.DepositInfo>;
    };

export type TransitionTraceReconstruction = {
  readonly payload: SDK.DaPayload;
  readonly payloadEnvelopeCbor: Buffer;
  readonly payloadCbor: Buffer;
  readonly header: SDK.Header;
  readonly headerHash: string;
  readonly roots: PayloadRootSet;
  readonly counts: PayloadCountSet;
  readonly utxos: readonly KeyValuePhasEntry[];
  readonly withdrawals: readonly DecodedRootEntry<
    SDK.OutputReference,
    SDK.WithdrawalInfo
  >[];
  readonly forcedTransactions: readonly DecodedForcedTransactionEntry[];
  readonly transactions: readonly DecodedTransactionEntry[];
  readonly deposits: readonly DecodedRootEntry<
    SDK.OutputReference,
    SDK.DepositInfo
  >[];
  readonly transitionTrace: readonly DecodedRootEntry<
    bigint,
    SDK.TransitionStep
  >[];
  readonly eventToStep: readonly DecodedRootEntry<
    SDK.EventKey,
    SDK.EventToStepValue
  >[];
  readonly sourceEvents: readonly SourceEventRecord[];
  readonly sourceEventsByFingerprint: ReadonlyMap<string, SourceEventRecord>;
  readonly traceByStepIndex: ReadonlyMap<
    bigint,
    DecodedRootEntry<bigint, SDK.TransitionStep>
  >;
  readonly eventToStepByFingerprint: ReadonlyMap<
    string,
    DecodedRootEntry<SDK.EventKey, SDK.EventToStepValue>
  >;
  readonly rootData: {
    readonly utxos: KeyValuePhasRoot;
    readonly withdrawals: CountedRoot;
    readonly forcedTransactions: CountedRoot;
    readonly transactions: CountedRoot;
    readonly deposits: CountedRoot;
    readonly transitionTrace: CountedRoot;
    readonly eventToStep: CountedRoot;
    readonly validationTraces: CountedRoot;
  };
};

export type ReconstructDaPayloadOptions = {
  readonly payloadEnvelopeCbor: Uint8Array;
  readonly expectedHeaderHash?: string;
  readonly committedHeader?: SDK.Header;
};

export const normalizeHeaderHash = (value: string, fieldName: string): string =>
  normalizeHex(value, { fieldName, byteLength: 28, trim: true });

export const normalizeEntryHex = (value: string, fieldName: string): string =>
  normalizeHex(value, { fieldName, trim: false, allowEmpty: true });

export const entryBuffer = (value: string, fieldName: string): Buffer =>
  Buffer.from(normalizeEntryHex(value, fieldName), "hex");

export const decodeData = <A>(
  hex: string,
  schema: DataSchema,
  fieldName: string,
  canonicalEncoder?: (value: A, raw: string) => string,
): A => {
  const normalized = normalizeEntryHex(hex, fieldName);
  try {
    const decoded = Data.from(normalized, schema as never) as A;
    const canonical =
      canonicalEncoder === undefined
        ? Data.to(decoded as never, schema as never)
        : canonicalEncoder(decoded, normalized);
    if (canonical !== normalized) {
      throw new Error(`${fieldName} is not canonical for its schema`);
    }
    return decoded;
  } catch (cause) {
    throw transitionTraceError(
      "malformedPayload",
      `Failed to decode ${fieldName}.`,
      cause,
    );
  }
};

export const encodeData = <A>(value: A, schema: DataSchema): Buffer =>
  Buffer.from(Data.to(value as never, schema as never), "hex");

export const eventKeyFingerprint = (eventKey: SDK.EventKey): string =>
  encodeData(eventKey, SDK.EventKeySchema).toString("hex");

export const eventKeyPhase = (eventKey: SDK.EventKey): SDK.TransitionPhase => {
  if ("WithdrawalEventKey" in eventKey) {
    return "Withdrawal";
  }
  if ("ForcedTransactionEventKey" in eventKey) {
    return "ForcedTransaction";
  }
  if ("L2TransactionEventKey" in eventKey) {
    return "L2Transaction";
  }
  return "Deposit";
};

export const sourceEventKey = (source: SourceEventRecord): SDK.EventKey =>
  source.eventKey;

export const validatePayloadEntryArray = (
  fieldName: string,
  entries: readonly SDK.DaPayloadEntry[],
): void => {
  let previousKey: string | undefined;
  for (const [index, [key, value]] of entries.entries()) {
    const normalizedKey = normalizeEntryHex(
      key,
      `${fieldName}[${index.toString()}].key`,
    );
    normalizeEntryHex(value, `${fieldName}[${index.toString()}].value`);
    if (previousKey !== undefined && normalizedKey === previousKey) {
      throw transitionTraceError(
        "invalidPayloadEntries",
        `${fieldName} contains duplicate key ${normalizedKey}.`,
      );
    }
    if (previousKey !== undefined && normalizedKey < previousKey) {
      throw transitionTraceError(
        "invalidPayloadEntries",
        `${fieldName} keys must be sorted ascending.`,
      );
    }
    previousKey = normalizedKey;
  }
};

export const validateDeclaredCounts = (payload: SDK.DaPayload): void => {
  const { block_body: body } = payload;
  const counts = body.counts;
  const fields = [
    ["withdrawal_count", counts.withdrawalCount],
    ["forced_transaction_count", counts.forcedTransactionCount],
    ["l2_transaction_count", counts.l2TransactionCount],
    ["deposit_count", counts.depositCount],
    ["total_event_count", counts.totalEventCount],
    ["transition_step_count", counts.transitionStepCount],
    ["validation_trace_count", counts.validationTraceCount],
  ] as const;
  for (const [field, count] of fields) {
    if (count < 0n) {
      throw transitionTraceError("countMismatch", `${field} is negative.`);
    }
  }
  const memberCounts = payloadMemberCounts(payload);
  const mismatches = countMismatches(counts, memberCounts);
  if (mismatches.length > 0) {
    throw transitionTraceError(
      "countMismatch",
      `Payload declared counts do not match payload member arrays: ${mismatches.join(
        ",",
      )}.`,
    );
  }
  if (BigInt(body.event_to_step.length) !== counts.totalEventCount) {
    throw transitionTraceError(
      "countMismatch",
      "event_to_step member count must equal total_event_count.",
    );
  }
  if (
    counts.validationTraceCount !==
      counts.forcedTransactionCount + counts.l2TransactionCount ||
    BigInt(body.validation_traces.length) !== counts.validationTraceCount
  ) {
    throw transitionTraceError(
      "countMismatch",
      "validation_traces member count must equal forced_transaction_count + l2_transaction_count.",
    );
  }
};

const payloadMemberCounts = (payload: SDK.DaPayload): PayloadCountSet => ({
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

export const countMismatches = (
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
