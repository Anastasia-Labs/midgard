import {
  computeMidgardNativeTxIdV1,
  computeMidgardNativeTxProofCommitmentV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxProofSourceV1FromCanonicalCbor,
} from "@al-ft/midgard-core/codec";
import { unwrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS_V1 } from "@al-ft/midgard-core/da-transport";
import { normalizeHex } from "@al-ft/midgard-core/hex";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import { transitionTraceError } from "./errors.js";
import {
  buildCountedRoot,
  canonicalizeKeyValuePhasEntries,
  type CountedRoot,
  type KeyValuePhasEntry,
  type KeyValuePhasRoot,
  keyValuePhasRootWithCount,
} from "./phas.js";

type DataSchema = Parameters<typeof Data.Nullable>[0];

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
  readonly value: SDK.L2TransactionSourceV1;
  readonly valueBytes: Buffer;
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
      readonly entry: DecodedRootEntry<
        SDK.OutputReference,
        SDK.ForcedInclusionTxV1
      >;
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
  readonly payload: SDK.DaPayloadV1;
  readonly payloadEnvelopeCbor: Buffer;
  readonly payloadCbor: Buffer;
  readonly header: SDK.HeaderV1;
  readonly headerHash: string;
  readonly roots: PayloadRootSet;
  readonly counts: PayloadCountSet;
  readonly utxos: readonly KeyValuePhasEntry[];
  readonly withdrawals: readonly DecodedRootEntry<
    SDK.OutputReference,
    SDK.WithdrawalInfo
  >[];
  readonly forcedTransactions: readonly DecodedRootEntry<
    SDK.OutputReference,
    SDK.ForcedInclusionTxV1
  >[];
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
  readonly committedHeader?: SDK.HeaderV1;
};

const normalizeHeaderHash = (value: string, fieldName: string): string =>
  normalizeHex(value, { fieldName, byteLength: 28, trim: true });

const normalizeEntryHex = (value: string, fieldName: string): string =>
  normalizeHex(value, { fieldName, trim: false, allowEmpty: true });

const entryBuffer = (value: string, fieldName: string): Buffer =>
  Buffer.from(normalizeEntryHex(value, fieldName), "hex");

const decodeData = <A>(
  hex: string,
  schema: DataSchema,
  fieldName: string,
): A => {
  const normalized = normalizeEntryHex(hex, fieldName);
  try {
    const decoded = Data.from(normalized, schema as never) as A;
    const canonical = Data.to(decoded as never, schema as never);
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

const decodePayloadStrict = (payloadCbor: Uint8Array): SDK.DaPayloadV1 => {
  const buffer = Buffer.from(payloadCbor);
  let payload: SDK.DaPayloadV1;
  try {
    payload = SDK.decodeDaPayloadV1(buffer);
  } catch (cause) {
    throw transitionTraceError(
      "malformedPayload",
      "Failed to decode DaPayloadV1 canonical CBOR.",
      cause,
    );
  }
  if (!SDK.encodeDaPayloadV1(payload).equals(buffer)) {
    throw transitionTraceError(
      "nonCanonicalPayload",
      "DA payload CBOR is not canonical for DaPayloadV1.",
    );
  }
  if (payload.version !== SDK.DA_PAYLOAD_V1_VERSION) {
    throw transitionTraceError(
      "wrongPayloadVersion",
      `Expected DA payload version ${SDK.DA_PAYLOAD_V1_VERSION.toString()}, got ${payload.version.toString()}.`,
    );
  }
  normalizeHeaderHash(payload.block_body.header_hash, "payload header_hash");
  validatePayloadEntryArray("utxos", payload.block_body.utxos);
  validatePayloadEntryArray("withdrawals", payload.block_body.withdrawals);
  validatePayloadEntryArray(
    "forced_transactions",
    payload.block_body.forced_transactions,
  );
  validatePayloadEntryArray("transactions", payload.block_body.transactions);
  validatePayloadEntryArray("deposits", payload.block_body.deposits);
  validatePayloadEntryArray(
    "transition_trace",
    payload.block_body.transition_trace,
  );
  validatePayloadEntryArray("event_to_step", payload.block_body.event_to_step);
  validatePayloadEntryArray(
    "transaction_preimages",
    payload.block_body.transaction_preimages,
  );
  validatePayloadEntryArray(
    "forced_transaction_preimages",
    payload.block_body.forced_transaction_preimages,
  );
  validatePayloadEntryArray(
    "cek_program_material",
    payload.block_body.cek_program_material,
  );
  validatePayloadEntryArray(
    "validation_traces",
    payload.block_body.validation_traces,
  );
  validateDeclaredCounts(payload);
  return payload;
};

const validatePayloadEntryArray = (
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

const validateDeclaredCounts = (payload: SDK.DaPayloadV1): void => {
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

const rawEntries = (
  fieldName: string,
  entries: readonly SDK.DaPayloadEntry[],
): readonly KeyValuePhasEntry[] =>
  canonicalizeKeyValuePhasEntries(
    entries.map(([key, value], index) => ({
      key: entryBuffer(key, `${fieldName}[${index.toString()}].key`),
      value: entryBuffer(value, `${fieldName}[${index.toString()}].value`),
    })),
  );

const decodeTypedEntries = <K, V>({
  fieldName,
  entries,
  keySchema,
  valueSchema,
}: {
  readonly fieldName: string;
  readonly entries: readonly SDK.DaPayloadEntry[];
  readonly keySchema: DataSchema;
  readonly valueSchema: DataSchema;
}): readonly DecodedRootEntry<K, V>[] =>
  entries.map(([keyHex, valueHex], index) => {
    const keyBytes = entryBuffer(keyHex, `${fieldName}[${index}].key`);
    const valueBytes = entryBuffer(valueHex, `${fieldName}[${index}].value`);
    return {
      key: decodeData<K>(keyHex, keySchema, `${fieldName}[${index}].key`),
      value: decodeData<V>(
        valueHex,
        valueSchema,
        `${fieldName}[${index}].value`,
      ),
      keyBytes,
      valueBytes,
    };
  });

const decodeTransactions = async ({
  entries,
  preimages,
}: {
  readonly entries: readonly SDK.DaPayloadEntry[];
  readonly preimages: readonly SDK.DaPayloadEntry[];
}): Promise<readonly DecodedTransactionEntry[]> => {
  const preimagesByTxId = new Map(
    preimages.map(([key, value], index) => [
      normalizeHex(key, {
        fieldName: `transaction_preimages[${index.toString()}].key`,
        byteLength: 32,
        trim: false,
      }),
      entryBuffer(
        value,
        `transaction_preimages[${index.toString()}].value`,
      ),
    ]),
  );
  if (preimagesByTxId.size !== preimages.length) {
    throw transitionTraceError(
      "invalidPayloadEntries",
      "transaction_preimages contains duplicate transaction IDs.",
    );
  }
  const decoded: DecodedTransactionEntry[] = [];
  for (const [index, [keyHex, valueHex]] of entries.entries()) {
    const txId = normalizeHex(keyHex, {
      fieldName: `transactions[${index.toString()}].key`,
      byteLength: 32,
      trim: false,
    });
    const valueBytes = entryBuffer(
      valueHex,
      `transactions[${index.toString()}].value`,
    );
    const value = decodeData<SDK.L2TransactionSourceV1>(
      valueHex,
      SDK.L2TransactionSourceV1Schema,
      `transactions[${index.toString()}].value`,
    );
    const fullTransactionCbor = preimagesByTxId.get(txId);
    if (fullTransactionCbor === undefined) {
      throw transitionTraceError(
        "invalidPayloadEntries",
        `transaction_preimages is missing tx_id ${txId}.`,
      );
    }
    try {
      const full = decodeMidgardNativeTxFullV1FromCanonicalCbor(
        fullTransactionCbor,
      );
      const expectedTxId = computeMidgardNativeTxIdV1(full).toString("hex");
      const source =
        deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(
          fullTransactionCbor,
        );
      const expected: SDK.L2TransactionSourceV1 = {
        tx_id: expectedTxId,
        transaction_commitment:
          computeMidgardNativeTxProofCommitmentV1(source).toString("hex"),
        source: {
          compact_cbor: source.compactCbor.toString("hex"),
          witness_set_compact_cbor:
            source.witnessSetCompactCbor.toString("hex"),
          field_preimage_lengths_cbor:
            source.fieldPreimageLengthsCbor.toString("hex"),
        },
      };
      if (
        expectedTxId !== txId ||
        Data.to(value, SDK.L2TransactionSourceV1) !==
          Data.to(expected, SDK.L2TransactionSourceV1)
      ) {
        throw new Error("source or commitment does not match canonical preimage");
      }
    } catch (cause) {
      throw transitionTraceError(
        "malformedPayload",
        `Failed to authenticate transactions[${index.toString()}] against its canonical preimage.`,
        cause,
      );
    }
    decoded.push({
      txId,
      keyBytes: Buffer.from(txId, "hex"),
      value,
      valueBytes,
      fullTransactionCbor,
    });
  }
  if (preimagesByTxId.size !== decoded.length) {
    throw transitionTraceError(
      "invalidPayloadEntries",
      "transaction_preimages contains entries without a matching transaction source.",
    );
  }
  return decoded;
};

const authenticateForcedTransactionPreimages = (
  entries: readonly DecodedRootEntry<
    SDK.OutputReference,
    SDK.ForcedInclusionTxV1
  >[],
  preimages: readonly SDK.DaPayloadEntry[],
): void => {
  const preimagesByKey = new Map(
    preimages.map(([key, value], index) => [
      normalizeEntryHex(
        key,
        `forced_transaction_preimages[${index.toString()}].key`,
      ),
      entryBuffer(
        value,
        `forced_transaction_preimages[${index.toString()}].value`,
      ),
    ]),
  );
  if (preimagesByKey.size !== preimages.length) {
    throw transitionTraceError(
      "invalidPayloadEntries",
      "forced_transaction_preimages contains duplicate transaction-order IDs.",
    );
  }
  for (const [index, entry] of entries.entries()) {
    const key = entry.keyBytes.toString("hex");
    const canonicalTransactionCbor = preimagesByKey.get(key);
    if (canonicalTransactionCbor === undefined) {
      throw transitionTraceError(
        "invalidPayloadEntries",
        `forced_transaction_preimages is missing forced_transactions[${index.toString()}].`,
      );
    }
    try {
      const full = decodeMidgardNativeTxFullV1FromCanonicalCbor(
        canonicalTransactionCbor,
      );
      const source =
        deriveMidgardNativeTxProofSourceV1FromCanonicalCbor(
          canonicalTransactionCbor,
        );
      const expected: SDK.ForcedInclusionTxV1 = {
        tx_id: computeMidgardNativeTxIdV1(full).toString("hex"),
        transaction_commitment:
          computeMidgardNativeTxProofCommitmentV1(source).toString("hex"),
        source: {
          compact_cbor: source.compactCbor.toString("hex"),
          witness_set_compact_cbor:
            source.witnessSetCompactCbor.toString("hex"),
          field_preimage_lengths_cbor:
            source.fieldPreimageLengthsCbor.toString("hex"),
        },
        operator_validity: entry.value.operator_validity,
      };
      if (
        Data.to(entry.value, SDK.ForcedInclusionTxV1) !==
        Data.to(expected, SDK.ForcedInclusionTxV1)
      ) {
        throw new Error("source or commitment does not match canonical preimage");
      }
    } catch (cause) {
      throw transitionTraceError(
        "malformedPayload",
        `Failed to authenticate forced_transactions[${index.toString()}] against its canonical preimage.`,
        cause,
      );
    }
  }
  if (preimagesByKey.size !== entries.length) {
    throw transitionTraceError(
      "invalidPayloadEntries",
      "forced_transaction_preimages contains entries without a matching forced transaction source.",
    );
  }
};

const buildSourceEvents = ({
  withdrawals,
  forcedTransactions,
  transactions,
  deposits,
}: {
  readonly withdrawals: TransitionTraceReconstruction["withdrawals"];
  readonly forcedTransactions: TransitionTraceReconstruction["forcedTransactions"];
  readonly transactions: TransitionTraceReconstruction["transactions"];
  readonly deposits: TransitionTraceReconstruction["deposits"];
}): readonly SourceEventRecord[] => {
  const records: SourceEventRecord[] = [];
  for (const entry of withdrawals) {
    const eventKey: SDK.EventKey = {
      WithdrawalEventKey: { withdrawal_id: entry.key },
    };
    records.push({
      phase: "Withdrawal",
      eventKey,
      fingerprint: eventKeyFingerprint(eventKey),
      entry,
    });
  }
  for (const entry of forcedTransactions) {
    const eventKey: SDK.EventKey = {
      ForcedTransactionEventKey: { tx_order_id: entry.key },
    };
    records.push({
      phase: "ForcedTransaction",
      eventKey,
      fingerprint: eventKeyFingerprint(eventKey),
      entry,
    });
  }
  for (const entry of transactions) {
    const eventKey: SDK.EventKey = {
      L2TransactionEventKey: { tx_id: entry.txId },
    };
    records.push({
      phase: "L2Transaction",
      eventKey,
      fingerprint: eventKeyFingerprint(eventKey),
      entry,
    });
  }
  for (const entry of deposits) {
    const eventKey: SDK.EventKey = {
      DepositEventKey: { deposit_id: entry.key },
    };
    records.push({
      phase: "Deposit",
      eventKey,
      fingerprint: eventKeyFingerprint(eventKey),
      entry,
    });
  }
  return records;
};

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

const headerCounts = (header: SDK.HeaderV1): PayloadCountSet => ({
  withdrawalCount: header.withdrawalCount,
  forcedTransactionCount: header.forcedTransactionCount,
  l2TransactionCount: header.l2TransactionCount,
  depositCount: header.depositCount,
  totalEventCount: header.totalEventCount,
  transitionStepCount: header.transitionStepCount,
  validationTraceCount: header.validationTraceCount,
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

const ensureUniqueSourceFingerprints = (
  sourceEvents: readonly SourceEventRecord[],
): ReadonlyMap<string, SourceEventRecord> => {
  const map = new Map<string, SourceEventRecord>();
  for (const sourceEvent of sourceEvents) {
    const existing = map.get(sourceEvent.fingerprint);
    if (existing !== undefined) {
      throw transitionTraceError(
        "invalidPayloadEntries",
        `Payload contains duplicate source event key ${sourceEvent.fingerprint}.`,
      );
    }
    map.set(sourceEvent.fingerprint, sourceEvent);
  }
  return map;
};

export const reconstructDaPayloadV1 = async ({
  payloadEnvelopeCbor,
  expectedHeaderHash,
  committedHeader,
}: ReconstructDaPayloadOptions): Promise<TransitionTraceReconstruction> => {
  let payloadCbor: Buffer;
  try {
    payloadCbor = (
      await unwrapDaPayloadV1(payloadEnvelopeCbor, {
        maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
      })
    ).innerBytes;
  } catch (cause) {
    throw transitionTraceError(
      "malformedPayload",
      "Failed to decode the mandatory DaPayloadEnvelopeV1.",
      cause,
    );
  }
  const payload = decodePayloadStrict(payloadCbor);
  const body = payload.block_body;
  const payloadBuffer = Buffer.from(payloadCbor);
  const payloadEnvelopeBuffer = Buffer.from(payloadEnvelopeCbor);
  const header = body.header;
  const headerHash = await Effect.runPromise(SDK.hashBlockHeaderV1(header));
  if (headerHash !== body.header_hash) {
    throw transitionTraceError(
      "headerMismatch",
      `Embedded header hashes to ${headerHash}, but payload header_hash is ${body.header_hash}.`,
    );
  }
  if (
    expectedHeaderHash !== undefined &&
    normalizeHeaderHash(expectedHeaderHash, "expected header hash") !==
      body.header_hash
  ) {
    throw transitionTraceError(
      "headerMismatch",
      `Payload header_hash ${body.header_hash} does not match expected header_hash ${expectedHeaderHash}.`,
    );
  }
  if (
    committedHeader !== undefined &&
    Data.to(committedHeader as never, SDK.HeaderV1 as never) !==
      Data.to(header as never, SDK.HeaderV1 as never)
  ) {
    throw transitionTraceError(
      "headerMismatch",
      "Payload embedded header does not match the committed L1 header.",
    );
  }

  const transactions = await decodeTransactions({
    entries: body.transactions,
    preimages: body.transaction_preimages,
  });
  const rootData = {
    utxos: await keyValuePhasRootWithCount(rawEntries("utxos", body.utxos)),
    withdrawals: await buildCountedRoot(
      SDK.ROOT_DOMAINS.withdrawals,
      rawEntries("withdrawals", body.withdrawals),
    ),
    forcedTransactions: await buildCountedRoot(
      SDK.ROOT_DOMAINS.forcedTransactionsV1,
      rawEntries("forced_transactions", body.forced_transactions),
    ),
    transactions: await buildCountedRoot(
      SDK.ROOT_DOMAINS.transactionsV1,
      rawEntries("transactions", body.transactions),
    ),
    deposits: await buildCountedRoot(
      SDK.ROOT_DOMAINS.deposits,
      rawEntries("deposits", body.deposits),
    ),
    transitionTrace: await buildCountedRoot(
      SDK.ROOT_DOMAINS.transitionTrace,
      rawEntries("transition_trace", body.transition_trace),
    ),
    eventToStep: await buildCountedRoot(
      SDK.ROOT_DOMAINS.eventToStep,
      rawEntries("event_to_step", body.event_to_step),
    ),
    validationTraces: await buildCountedRoot(
      SDK.ROOT_DOMAINS.validationTraces,
      rawEntries("validation_traces", body.validation_traces),
    ),
  };
  const roots: PayloadRootSet = {
    utxosRoot: rootData.utxos.root,
    withdrawalsRoot: rootData.withdrawals.root,
    forcedTransactionsRoot: rootData.forcedTransactions.root,
    transactionsRoot: rootData.transactions.root,
    depositsRoot: rootData.deposits.root,
    transitionTraceRoot: rootData.transitionTrace.root,
    eventToStepRoot: rootData.eventToStep.root,
    validationTracesRoot: rootData.validationTraces.root,
  };
  const counts = body.counts;
  const mismatchedRoots = rootMismatches(headerRoots(header), roots);
  if (mismatchedRoots.length > 0) {
    throw transitionTraceError(
      "rootMismatch",
      `Payload roots do not match committed header: ${mismatchedRoots.join(
        ",",
      )}.`,
    );
  }
  const mismatchedCounts = countMismatches(headerCounts(header), counts);
  if (mismatchedCounts.length > 0) {
    throw transitionTraceError(
      "countMismatch",
      `Payload counts do not match committed header: ${mismatchedCounts.join(
        ",",
      )}.`,
    );
  }

  const withdrawals = decodeTypedEntries<
    SDK.OutputReference,
    SDK.WithdrawalInfo
  >({
    fieldName: "withdrawals",
    entries: body.withdrawals,
    keySchema: SDK.OutputReference as never,
    valueSchema: SDK.WithdrawalInfoSchema,
  });
  const forcedTransactions = decodeTypedEntries<
    SDK.OutputReference,
    SDK.ForcedInclusionTxV1
  >({
    fieldName: "forced_transactions",
    entries: body.forced_transactions,
    keySchema: SDK.OutputReference as never,
    valueSchema: SDK.ForcedInclusionTxV1Schema,
  });
  authenticateForcedTransactionPreimages(
    forcedTransactions,
    body.forced_transaction_preimages,
  );
  const deposits = decodeTypedEntries<SDK.OutputReference, SDK.DepositInfo>({
    fieldName: "deposits",
    entries: body.deposits,
    keySchema: SDK.OutputReference as never,
    valueSchema: SDK.DepositInfoSchema,
  });
  const transitionTrace = decodeTypedEntries<bigint, SDK.TransitionStep>({
    fieldName: "transition_trace",
    entries: body.transition_trace,
    keySchema: Data.Integer() as never,
    valueSchema: SDK.TransitionStepSchema,
  });
  const eventToStep = decodeTypedEntries<SDK.EventKey, SDK.EventToStepValue>({
    fieldName: "event_to_step",
    entries: body.event_to_step,
    keySchema: SDK.EventKeySchema,
    valueSchema: SDK.EventToStepValueSchema,
  });
  const sourceEvents = buildSourceEvents({
    withdrawals,
    forcedTransactions,
    transactions,
    deposits,
  });
  const traceByStepIndex = new Map(
    transitionTrace.map((entry) => [entry.key, entry] as const),
  );
  const eventToStepByFingerprint = new Map(
    eventToStep.map(
      (entry) => [eventKeyFingerprint(entry.key), entry] as const,
    ),
  );

  return {
    payload,
    payloadEnvelopeCbor: payloadEnvelopeBuffer,
    payloadCbor: payloadBuffer,
    header,
    headerHash,
    roots,
    counts,
    utxos: rootData.utxos.entries,
    withdrawals,
    forcedTransactions,
    transactions,
    deposits,
    transitionTrace,
    eventToStep,
    sourceEvents,
    sourceEventsByFingerprint: ensureUniqueSourceFingerprints(sourceEvents),
    traceByStepIndex,
    eventToStepByFingerprint,
    rootData,
  };
};
