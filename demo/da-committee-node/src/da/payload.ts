import { Trie } from "@aiken-lang/merkle-patricia-forestry";
import {
  decodeMidgardNativeTxFullFromCanonicalCbor,
  encodeMidgardNativeTxCompact,
} from "@al-ft/midgard-core/codec";
import * as SDK from "@al-ft/midgard-sdk";
import { Data as LucidData } from "@lucid-evolution/lucid";
import { sha256 } from "@noble/hashes/sha2.js";
import { Effect } from "effect";

import type {
  Header,
  PayloadCountSet,
  PayloadRootSet,
  ValidationSummary,
} from "../domain.js";
import { hashBlockHeader } from "../l1/state-queue-scanner.js";
import { bytesToHex, hexToBytes, normalizeHex } from "../utils/hex.js";

type DataSchema = Parameters<typeof LucidData.Nullable>[0];

export type TransactionRootValueProjector = (
  fullTransactionCbor: Buffer,
) => Buffer | Promise<Buffer>;

export type PayloadVerificationOptions = {
  readonly transactionProjector?: TransactionRootValueProjector;
  readonly stateQueueOutRef: string;
};

export type VerifiedDaPayload = {
  readonly payload: SDK.DaPayloadV2;
  readonly payloadCbor: Buffer;
  readonly payloadSha256: string;
  readonly roots: PayloadRootSet;
  readonly counts: PayloadCountSet;
  readonly validation: ValidationSummary;
};

export class DaPayloadValidationError extends Error {
  readonly code:
    | "malformed_da"
    | "non_canonical"
    | "wrong_version"
    | "duplicate_key"
    | "unsorted_key"
    | "header_hash_mismatch"
    | "header_mismatch"
    | "malformed_transaction"
    | "malformed_trace"
    | "root_mismatch"
    | "count_mismatch"
    | "coverage_mismatch";

  constructor(
    code: DaPayloadValidationError["code"],
    message: string,
    options?: ErrorOptions,
  ) {
    super(message, options);
    this.name = "DaPayloadValidationError";
    this.code = code;
  }
}

export const decodeDaPayloadV2Strict = (
  payloadCbor: Uint8Array,
): SDK.DaPayloadV2 => {
  const payloadBuffer = Buffer.from(payloadCbor);
  let payload: SDK.DaPayloadV2;
  try {
    payload = SDK.decodeDaPayloadV2(payloadBuffer);
  } catch (cause) {
    throw new DaPayloadValidationError(
      "malformed_da",
      "failed to decode DaPayloadV2 canonical CBOR",
      { cause },
    );
  }
  const canonical = SDK.encodeDaPayloadV2(payload);
  if (!canonical.equals(payloadBuffer)) {
    throw new DaPayloadValidationError(
      "non_canonical",
      "payload CBOR was not canonical for DaPayloadV2",
    );
  }
  if (payload.version !== SDK.DA_PAYLOAD_V2_VERSION) {
    throw new DaPayloadValidationError(
      "wrong_version",
      `expected DaPayloadV2 version ${SDK.DA_PAYLOAD_V2_VERSION.toString()}, got ${payload.version.toString()}`,
    );
  }
  const body = payload.block_body;
  normalizeHex(body.header_hash, {
    fieldName: "payload header_hash",
    byteLength: 28,
  });
  const embeddedHeaderHash = hashBlockHeader(body.header);
  if (embeddedHeaderHash !== body.header_hash) {
    throw new DaPayloadValidationError(
      "header_hash_mismatch",
      `embedded header hash ${embeddedHeaderHash} does not match payload header_hash ${body.header_hash}`,
    );
  }
  validateEntries("utxos", body.utxos);
  validateEntries("withdrawals", body.withdrawals);
  validateEntries("forced_transactions", body.forced_transactions);
  validateEntries("transactions", body.transactions);
  validateEntries("deposits", body.deposits);
  validateEntries("transition_trace", body.transition_trace);
  validateEntries("event_to_step", body.event_to_step);
  validateCounts(body.counts);
  validateTraceCoverage(payload);
  return payload;
};

export const computeDaPayloadRoots = async (
  payload: SDK.DaPayloadV2,
  transactionProjector: TransactionRootValueProjector = defaultTransactionProjector,
): Promise<PayloadRootSet> => {
  const body = payload.block_body;
  const transactionValues: Buffer[] = [];
  for (const [, value] of body.transactions) {
    try {
      transactionValues.push(
        Buffer.from(await transactionProjector(hexToBytes(value, "tx value"))),
      );
    } catch (cause) {
      throw new DaPayloadValidationError(
        "malformed_transaction",
        "failed to project full transaction CBOR to compact root value",
        { cause },
      );
    }
  }
  const [
    utxosRoot,
    withdrawalsRoot,
    forcedTransactionsRoot,
    transactionsRoot,
    depositsRoot,
    transitionTraceRoot,
    eventToStepRoot,
  ] = await Promise.all([
    keyValuePhasRoot(body.utxos),
    countedRoot(SDK.ROOT_DOMAINS.withdrawals, body.withdrawals),
    countedRoot(SDK.ROOT_DOMAINS.forcedTransactions, body.forced_transactions),
    countedRootWithValues(
      SDK.ROOT_DOMAINS.transactions,
      body.transactions.map(([key]) => hexToBytes(key, "tx key")),
      transactionValues,
    ),
    countedRoot(SDK.ROOT_DOMAINS.deposits, body.deposits),
    countedRoot(SDK.ROOT_DOMAINS.transitionTrace, body.transition_trace),
    countedRoot(SDK.ROOT_DOMAINS.eventToStep, body.event_to_step),
  ]);
  return {
    utxosRoot,
    withdrawalsRoot,
    forcedTransactionsRoot,
    transactionsRoot,
    depositsRoot,
    transitionTraceRoot,
    eventToStepRoot,
  };
};

export const verifyDaPayloadAgainstHeader = async (
  payloadCbor: Uint8Array,
  expectedHeaderHash: string,
  header: Header,
  options: PayloadVerificationOptions,
): Promise<VerifiedDaPayload> => {
  const normalizedHeaderHash = normalizeHex(expectedHeaderHash, {
    fieldName: "expected header hash",
    byteLength: 28,
  });
  const payload = decodeDaPayloadV2Strict(payloadCbor);
  if (payload.block_body.header_hash !== normalizedHeaderHash) {
    throw new DaPayloadValidationError(
      "header_hash_mismatch",
      `payload header_hash ${payload.block_body.header_hash} does not match L1 header hash ${normalizedHeaderHash}`,
    );
  }
  if (hashBlockHeader(header) !== normalizedHeaderHash) {
    throw new DaPayloadValidationError(
      "header_hash_mismatch",
      `L1 header body does not hash to expected header_hash ${normalizedHeaderHash}`,
    );
  }
  if (headerCborHex(payload.block_body.header) !== headerCborHex(header)) {
    throw new DaPayloadValidationError(
      "header_mismatch",
      "payload embedded header does not match the L1 header",
    );
  }
  const roots = await computeDaPayloadRoots(
    payload,
    options.transactionProjector,
  );
  const counts = payload.block_body.counts;
  const mismatches = rootMismatches(header, roots);
  if (mismatches.length > 0) {
    throw new DaPayloadValidationError(
      "root_mismatch",
      `payload roots do not match L1 header: ${mismatches.join(",")}`,
    );
  }
  const countMismatchFields = countMismatches(headerCounts(header), counts);
  if (countMismatchFields.length > 0) {
    throw new DaPayloadValidationError(
      "count_mismatch",
      `payload counts do not match L1 header: ${countMismatchFields.join(",")}`,
    );
  }
  const payloadBuffer = Buffer.from(payloadCbor);
  return {
    payload,
    payloadCbor: payloadBuffer,
    payloadSha256: bytesToHex(sha256(payloadBuffer)),
    roots,
    counts,
    validation: {
      payloadVersion: Number(payload.version),
      rootsMatch: true,
      stateQueueOutRef: options.stateQueueOutRef,
      headerHash: normalizedHeaderHash,
      rootSummary: roots,
      countSummary: counts,
      l1Header: {
        startTime: header.startTime.toString(),
        endTime: header.endTime.toString(),
        operatorVkey: header.operatorVkey,
        prevHeaderHash: header.prevHeaderHash,
        protocolVersion: header.protocolVersion.toString(),
      },
    },
  };
};

export const daPayloadSha256 = (payloadCbor: Uint8Array): string =>
  bytesToHex(sha256(payloadCbor));

const validateEntries = (
  fieldName: string,
  entries: readonly SDK.DaPayloadEntry[],
): void => {
  let previousKey: string | undefined;
  for (const [index, [key, value]] of entries.entries()) {
    const normalizedKey = normalizeHex(key, {
      fieldName: `${fieldName}[${index.toString()}].key`,
    });
    normalizeHex(value, {
      fieldName: `${fieldName}[${index.toString()}].value`,
    });
    if (previousKey !== undefined) {
      if (normalizedKey === previousKey) {
        throw new DaPayloadValidationError(
          "duplicate_key",
          `${fieldName} contains duplicate key ${normalizedKey}`,
        );
      }
      if (normalizedKey < previousKey) {
        throw new DaPayloadValidationError(
          "unsorted_key",
          `${fieldName} keys must be sorted ascending`,
        );
      }
    }
    previousKey = normalizedKey;
  }
};

const validateCounts = (counts: SDK.DaPayloadCountsV2): void => {
  const fields = [
    ["withdrawal_count", counts.withdrawalCount],
    ["forced_transaction_count", counts.forcedTransactionCount],
    ["l2_transaction_count", counts.l2TransactionCount],
    ["deposit_count", counts.depositCount],
    ["total_event_count", counts.totalEventCount],
    ["transition_step_count", counts.transitionStepCount],
  ] as const;
  for (const [field, count] of fields) {
    if (count < 0n) {
      throw new DaPayloadValidationError(
        "count_mismatch",
        `${field} must be non-negative`,
      );
    }
  }
  const expectedTotal =
    counts.withdrawalCount +
    counts.forcedTransactionCount +
    counts.l2TransactionCount +
    counts.depositCount;
  if (counts.totalEventCount !== expectedTotal) {
    throw new DaPayloadValidationError(
      "count_mismatch",
      `total_event_count ${counts.totalEventCount.toString()} does not match source counts ${expectedTotal.toString()}`,
    );
  }
  if (counts.transitionStepCount !== counts.totalEventCount) {
    throw new DaPayloadValidationError(
      "count_mismatch",
      "transition_step_count must equal total_event_count",
    );
  }
};

const decodeCanonicalData = <A>(
  hex: string,
  schema: DataSchema,
  fieldName: string,
): A => {
  const normalized = normalizeHex(hex, { fieldName });
  try {
    const value = LucidData.from(normalized, schema as never) as A;
    const recoded = LucidData.to(value as never, schema as never);
    if (recoded !== normalized) {
      throw new Error(`${fieldName} is not canonical for its schema`);
    }
    return value;
  } catch (cause) {
    throw new DaPayloadValidationError(
      "malformed_trace",
      `failed to decode ${fieldName}`,
      { cause },
    );
  }
};

const dataHex = <A>(value: A, schema: DataSchema): string =>
  LucidData.to(value as never, schema as never);

const headerCborHex = (header: Header): string =>
  dataHex(header, SDK.Header as never);

const eventKeyFingerprint = (eventKey: SDK.EventKey): string =>
  dataHex(eventKey, SDK.EventKeySchema);

const eventPhase = (eventKey: SDK.EventKey): SDK.TransitionPhase => {
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

const sourceEventFingerprints = (body: SDK.DaPayloadBodyV2): Set<string> => {
  const fingerprints = new Set<string>();
  const add = (fingerprint: string, fieldName: string) => {
    if (fingerprints.has(fingerprint)) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        `duplicate source event key derived from ${fieldName}`,
      );
    }
    fingerprints.add(fingerprint);
  };
  for (const [index, [key]] of body.withdrawals.entries()) {
    const withdrawalId = decodeCanonicalData<SDK.OutputReference>(
      key,
      SDK.OutputReference as never,
      `withdrawals[${index.toString()}].key`,
    );
    add(
      eventKeyFingerprint({
        WithdrawalEventKey: { withdrawal_id: withdrawalId },
      }),
      `withdrawals[${index.toString()}]`,
    );
  }
  for (const [index, [key]] of body.forced_transactions.entries()) {
    const txOrderId = decodeCanonicalData<SDK.OutputReference>(
      key,
      SDK.OutputReference as never,
      `forced_transactions[${index.toString()}].key`,
    );
    add(
      eventKeyFingerprint({
        ForcedTransactionEventKey: { tx_order_id: txOrderId },
      }),
      `forced_transactions[${index.toString()}]`,
    );
  }
  for (const [index, [key]] of body.transactions.entries()) {
    const txId = normalizeHex(key, {
      fieldName: `transactions[${index.toString()}].key`,
      byteLength: 32,
    });
    add(
      eventKeyFingerprint({ L2TransactionEventKey: { tx_id: txId } }),
      `transactions[${index.toString()}]`,
    );
  }
  for (const [index, [key]] of body.deposits.entries()) {
    const depositId = decodeCanonicalData<SDK.OutputReference>(
      key,
      SDK.OutputReference as never,
      `deposits[${index.toString()}].key`,
    );
    add(
      eventKeyFingerprint({ DepositEventKey: { deposit_id: depositId } }),
      `deposits[${index.toString()}]`,
    );
  }
  return fingerprints;
};

const validateTraceCoverage = (payload: SDK.DaPayloadV2): void => {
  const body = payload.block_body;
  const counts = body.counts;
  const memberCounts: PayloadCountSet = {
    withdrawalCount: BigInt(body.withdrawals.length),
    forcedTransactionCount: BigInt(body.forced_transactions.length),
    l2TransactionCount: BigInt(body.transactions.length),
    depositCount: BigInt(body.deposits.length),
    totalEventCount:
      BigInt(body.withdrawals.length) +
      BigInt(body.forced_transactions.length) +
      BigInt(body.transactions.length) +
      BigInt(body.deposits.length),
    transitionStepCount: BigInt(body.transition_trace.length),
  };
  const countMismatchFields = countMismatches(counts, memberCounts);
  if (countMismatchFields.length > 0) {
    throw new DaPayloadValidationError(
      "count_mismatch",
      `payload counts do not match payload member arrays: ${countMismatchFields.join(",")}`,
    );
  }
  if (BigInt(body.event_to_step.length) !== counts.totalEventCount) {
    throw new DaPayloadValidationError(
      "count_mismatch",
      "event_to_step member count must equal total_event_count",
    );
  }

  const sourceEvents = sourceEventFingerprints(body);
  if (BigInt(sourceEvents.size) !== counts.totalEventCount) {
    throw new DaPayloadValidationError(
      "coverage_mismatch",
      "source event key set size does not match total_event_count",
    );
  }

  const traceByIndex = new Map<
    bigint,
    { readonly eventKey: string; readonly phase: SDK.TransitionPhase }
  >();
  for (const [index, [keyHex, valueHex]] of body.transition_trace.entries()) {
    const key = decodeCanonicalData<bigint>(
      keyHex,
      LucidData.Integer() as never,
      `transition_trace[${index.toString()}].key`,
    );
    const step = decodeCanonicalData<SDK.TransitionStep>(
      valueHex,
      SDK.TransitionStepSchema as never,
      `transition_trace[${index.toString()}].value`,
    );
    if (step.schema_version < 0n) {
      throw new DaPayloadValidationError(
        "malformed_trace",
        "transition step schema_version must be non-negative",
      );
    }
    if (key !== step.step_index) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "transition trace key must equal step_index",
      );
    }
    if (step.phase !== eventPhase(step.event_key)) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "transition step phase does not match event key variant",
      );
    }
    if (traceByIndex.has(step.step_index)) {
      throw new DaPayloadValidationError(
        "duplicate_key",
        `duplicate transition step_index ${step.step_index.toString()}`,
      );
    }
    traceByIndex.set(step.step_index, {
      eventKey: eventKeyFingerprint(step.event_key),
      phase: step.phase,
    });
  }
  for (let index = 0n; index < counts.transitionStepCount; index += 1n) {
    if (!traceByIndex.has(index)) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        `transition trace is missing dense step_index ${index.toString()}`,
      );
    }
  }

  const eventToStep = new Map<string, SDK.EventToStepValue>();
  for (const [index, [keyHex, valueHex]] of body.event_to_step.entries()) {
    const eventKey = decodeCanonicalData<SDK.EventKey>(
      keyHex,
      SDK.EventKeySchema as never,
      `event_to_step[${index.toString()}].key`,
    );
    const value = decodeCanonicalData<SDK.EventToStepValue>(
      valueHex,
      SDK.EventToStepValueSchema as never,
      `event_to_step[${index.toString()}].value`,
    );
    if (value.step_index < 0n) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "event_to_step step_index must be non-negative",
      );
    }
    if (value.phase !== eventPhase(eventKey)) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "event_to_step phase does not match event key variant",
      );
    }
    const fingerprint = eventKeyFingerprint(eventKey);
    if (eventToStep.has(fingerprint)) {
      throw new DaPayloadValidationError(
        "duplicate_key",
        `duplicate event_to_step event key ${fingerprint}`,
      );
    }
    eventToStep.set(fingerprint, value);
  }

  for (const sourceEvent of sourceEvents) {
    const mapped = eventToStep.get(sourceEvent);
    if (mapped === undefined) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "event_to_step omits a committed source event",
      );
    }
    const trace = traceByIndex.get(mapped.step_index);
    if (trace === undefined) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "event_to_step points to a missing transition step",
      );
    }
    if (trace.eventKey !== sourceEvent || trace.phase !== mapped.phase) {
      throw new DaPayloadValidationError(
        "coverage_mismatch",
        "event_to_step does not point back to the matching transition trace event",
      );
    }
  }
};

const keyValuePhasRoot = async (
  entries: readonly SDK.DaPayloadEntry[],
): Promise<string> =>
  keyValuePhasRootWithValues(
    entries.map(([key]) => hexToBytes(key, "entry key")),
    entries.map(([, value]) => hexToBytes(value, "entry value")),
  );

const keyValuePhasRootWithValues = async (
  keys: readonly Buffer[],
  values: readonly Buffer[],
): Promise<string> => {
  if (keys.length !== values.length) {
    throw new Error(
      `cannot build PHAS root for ${keys.length.toString()} keys and ${values.length.toString()} values`,
    );
  }
  if (keys.length === 0) {
    return SDK.EMPTY_MERKLE_TREE_ROOT;
  }
  const trie = await Trie.fromList(
    keys.map((key, index) => ({
      key: Buffer.from(key),
      value: Buffer.from(values[index]!),
    })),
  );
  return Buffer.from(trie.hash).toString("hex");
};

const countedRoot = async (
  domain: SDK.RootDomain,
  entries: readonly SDK.DaPayloadEntry[],
): Promise<string> =>
  countedRootWithValues(
    domain,
    entries.map(([key]) => hexToBytes(key, "entry key")),
    entries.map(([, value]) => hexToBytes(value, "entry value")),
  );

const countedRootWithValues = async (
  domain: SDK.RootDomain,
  keys: readonly Buffer[],
  values: readonly Buffer[],
): Promise<string> => {
  const phasRoot = await keyValuePhasRootWithValues(keys, values);
  return Effect.runPromise(
    SDK.commitCountedRootProgram({
      domain,
      phasRoot,
      count: BigInt(keys.length),
    }),
  );
};

const defaultTransactionProjector: TransactionRootValueProjector = (
  fullTransactionCbor,
) =>
  Buffer.from(
    encodeMidgardNativeTxCompact(
      decodeMidgardNativeTxFullFromCanonicalCbor(fullTransactionCbor).compact,
    ),
  );

const rootMismatches = (
  header: Header,
  roots: PayloadRootSet,
): readonly string[] =>
  [
    header.utxosRoot === roots.utxosRoot ? undefined : "utxos_root",
    header.withdrawalsRoot === roots.withdrawalsRoot
      ? undefined
      : "withdrawals_root",
    header.forcedTransactionsRoot === roots.forcedTransactionsRoot
      ? undefined
      : "forced_transactions_root",
    header.transactionsRoot === roots.transactionsRoot
      ? undefined
      : "transactions_root",
    header.depositsRoot === roots.depositsRoot ? undefined : "deposits_root",
    header.transitionTraceRoot === roots.transitionTraceRoot
      ? undefined
      : "transition_trace_root",
    header.eventToStepRoot === roots.eventToStepRoot
      ? undefined
      : "event_to_step_root",
  ].filter((field): field is string => field !== undefined);

const headerCounts = (header: Header): PayloadCountSet => ({
  withdrawalCount: header.withdrawalCount,
  forcedTransactionCount: header.forcedTransactionCount,
  l2TransactionCount: header.l2TransactionCount,
  depositCount: header.depositCount,
  totalEventCount: header.totalEventCount,
  transitionStepCount: header.transitionStepCount,
});

const countMismatches = (
  expected: PayloadCountSet,
  actual: PayloadCountSet,
): readonly string[] =>
  [
    expected.withdrawalCount === actual.withdrawalCount
      ? undefined
      : "withdrawal_count",
    expected.forcedTransactionCount === actual.forcedTransactionCount
      ? undefined
      : "forced_transaction_count",
    expected.l2TransactionCount === actual.l2TransactionCount
      ? undefined
      : "l2_transaction_count",
    expected.depositCount === actual.depositCount ? undefined : "deposit_count",
    expected.totalEventCount === actual.totalEventCount
      ? undefined
      : "total_event_count",
    expected.transitionStepCount === actual.transitionStepCount
      ? undefined
      : "transition_step_count",
  ].filter((field): field is string => field !== undefined);
