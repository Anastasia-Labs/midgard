import {
  assertBytesHex,
  cborArgumentSize,
  DA_PAYLOAD_VERSION,
  DaPayload,
  DaPayloadEntry,
  ExactBufferWriter,
  isNativeCborInteger,
  payloadIntegers,
  PLUTUS_BYTES_CHUNK,
  writeBytes,
  writeConstructorStart,
  writeInteger,
  writeList,
} from "./da-payload.write-cbor-argument.js";

const integerSize = (value: bigint): number =>
  cborArgumentSize(value >= 0n ? value : -1n - value);

const bytesSize = (value: string): number => {
  assertBytesHex(value);
  const length = value.length / 2;
  if (length <= PLUTUS_BYTES_CHUNK) {
    return cborArgumentSize(BigInt(length)) + length;
  }
  const fullChunks = Math.floor(length / PLUTUS_BYTES_CHUNK);
  const remainder = length % PLUTUS_BYTES_CHUNK;
  return (
    2 +
    fullChunks *
      (cborArgumentSize(BigInt(PLUTUS_BYTES_CHUNK)) + PLUTUS_BYTES_CHUNK) +
    (remainder === 0 ? 0 : cborArgumentSize(BigInt(remainder)) + remainder)
  );
};

/** Exact canonical Plutus-Data CBOR bytes occupied by one `[key, value]` tuple. */
export const daPayloadEntryEncodedSize = ([
  key,
  value,
]: DaPayloadEntry): number => 2 + bytesSize(key) + bytesSize(value);

export type DaPayloadEntrySizeAggregate = {
  readonly entryCount: number;
  /** Sum of `daPayloadEntryEncodedSize` for every entry. */
  readonly encodedTupleBytes: number;
};

/** Exact outer-list size from a maintained tuple-byte aggregate. */
export const daPayloadEntriesEncodedSizeFromAggregate = ({
  entryCount,
  encodedTupleBytes,
}: DaPayloadEntrySizeAggregate): number => {
  if (!Number.isSafeInteger(entryCount) || entryCount < 0) {
    throw new Error(
      "DA payload entry count must be a non-negative safe integer",
    );
  }
  if (!Number.isSafeInteger(encodedTupleBytes) || encodedTupleBytes < 0) {
    throw new Error(
      "DA payload encoded tuple bytes must be a non-negative safe integer",
    );
  }
  if (entryCount === 0 && encodedTupleBytes !== 0) {
    throw new Error(
      "Empty DA payload entry aggregate must have zero tuple bytes",
    );
  }
  if (entryCount > 0 && encodedTupleBytes < entryCount * 4) {
    throw new Error("DA payload entry aggregate is smaller than tuple framing");
  }
  return entryCount === 0 ? 1 : 2 + encodedTupleBytes;
};

const listSize = (entries: readonly DaPayloadEntry[]): number =>
  entries.length === 0
    ? 1
    : daPayloadEntriesEncodedSizeFromAggregate({
        entryCount: entries.length,
        encodedTupleBytes: entries.reduce(
          (size, entry) => size + daPayloadEntryEncodedSize(entry),
          0,
        ),
      });

const constructorSize = (fieldSizes: readonly number[]): number =>
  4 + fieldSizes.reduce((total, size) => total + size, 0);

const encodedPayloadSize = (
  payload: DaPayload,
  utxoEncodedListSize = listSize(payload.block_body.utxos),
): number => {
  const body = payload.block_body;
  const header = body.header;
  const headerSize = constructorSize([
    bytesSize(header.prevUtxosRoot),
    bytesSize(header.utxosRoot),
    bytesSize(header.withdrawalsRoot),
    bytesSize(header.forcedTransactionsRoot),
    bytesSize(header.transactionsRoot),
    bytesSize(header.depositsRoot),
    bytesSize(header.transitionTraceRoot),
    bytesSize(header.eventToStepRoot),
    bytesSize(header.validationTracesRoot),
    integerSize(header.withdrawalCount),
    integerSize(header.forcedTransactionCount),
    integerSize(header.l2TransactionCount),
    integerSize(header.depositCount),
    integerSize(header.totalEventCount),
    integerSize(header.transitionStepCount),
    integerSize(header.validationTraceCount),
    integerSize(header.startTime),
    integerSize(header.endTime),
    integerSize(header.blockSlot),
    integerSize(header.expectedNetworkId),
    integerSize(header.minFeeA),
    integerSize(header.minFeeB),
    bytesSize(header.prevHeaderHash),
    bytesSize(header.operatorVkey),
    integerSize(header.protocolVersion),
  ]);
  const counts = body.counts;
  const countsSize = constructorSize([
    integerSize(counts.withdrawalCount),
    integerSize(counts.forcedTransactionCount),
    integerSize(counts.l2TransactionCount),
    integerSize(counts.depositCount),
    integerSize(counts.totalEventCount),
    integerSize(counts.transitionStepCount),
    integerSize(counts.validationTraceCount),
  ]);
  const bodySize = constructorSize([
    bytesSize(body.header_hash),
    headerSize,
    utxoEncodedListSize,
    listSize(body.withdrawals),
    listSize(body.forced_transactions),
    listSize(body.transactions),
    listSize(body.transaction_preimages),
    listSize(body.forced_transaction_preimages),
    listSize(body.cek_program_material),
    listSize(body.deposits),
    listSize(body.transition_trace),
    listSize(body.event_to_step),
    listSize(body.validation_traces),
    listSize(body.validation_trace_witnesses),
    countsSize,
  ]);
  return constructorSize([integerSize(payload.version), bodySize]);
};

/** Exact encoded inner DaPayload size without allocating its CBOR bytes. */
export const daPayloadEncodedSize = (payload: DaPayload): number =>
  encodedPayloadSize(payload);

/**
 * Exact encoded inner size when the full UTxO list is represented by a
 * durable entry-count/tuple-byte aggregate rather than materialized in RAM.
 * The `payload.block_body.utxos` value is ignored.
 */
export const daPayloadEncodedSizeFromUtxoAggregate = (
  payload: DaPayload,
  utxos: DaPayloadEntrySizeAggregate,
): number =>
  encodedPayloadSize(payload, daPayloadEntriesEncodedSizeFromAggregate(utxos));

/**
 * Encodes the canonical V1 Plutus-Data wire format directly into byte chunks.
 * Lucid's schema encoder first builds a full hexadecimal string; at DA scale
 * that doubles the largest allocation and creates severe GC pressure. The
 * direct encoder is byte-identical for the protocol's uint64 integer domain.
 */
export const encodeDaPayload = (payload: DaPayload): Buffer => {
  if (payload.version !== DA_PAYLOAD_VERSION) {
    throw new Error(
      `DaPayloadV1 version must equal ${DA_PAYLOAD_VERSION.toString()}`,
    );
  }
  if (!payloadIntegers(payload).every(isNativeCborInteger)) {
    throw new Error(
      "DaPayloadV1 protocol integers must fit the native CBOR integer range",
    );
  }

  const writer = new ExactBufferWriter(encodedPayloadSize(payload));
  const body = payload.block_body;
  writeConstructorStart(writer);
  writeInteger(writer, payload.version);
  writeConstructorStart(writer);
  writeBytes(writer, body.header_hash);

  const header = body.header;
  writeConstructorStart(writer);
  writeBytes(writer, header.prevUtxosRoot);
  writeBytes(writer, header.utxosRoot);
  writeBytes(writer, header.withdrawalsRoot);
  writeBytes(writer, header.forcedTransactionsRoot);
  writeBytes(writer, header.transactionsRoot);
  writeBytes(writer, header.depositsRoot);
  writeBytes(writer, header.transitionTraceRoot);
  writeBytes(writer, header.eventToStepRoot);
  writeBytes(writer, header.validationTracesRoot);
  writeInteger(writer, header.withdrawalCount);
  writeInteger(writer, header.forcedTransactionCount);
  writeInteger(writer, header.l2TransactionCount);
  writeInteger(writer, header.depositCount);
  writeInteger(writer, header.totalEventCount);
  writeInteger(writer, header.transitionStepCount);
  writeInteger(writer, header.validationTraceCount);
  writeInteger(writer, header.startTime);
  writeInteger(writer, header.endTime);
  writeInteger(writer, header.blockSlot);
  writeInteger(writer, header.expectedNetworkId);
  writeInteger(writer, header.minFeeA);
  writeInteger(writer, header.minFeeB);
  writeBytes(writer, header.prevHeaderHash);
  writeBytes(writer, header.operatorVkey);
  writeInteger(writer, header.protocolVersion);
  writer.writeByte(0xff);

  writeList(writer, body.utxos);
  writeList(writer, body.withdrawals);
  writeList(writer, body.forced_transactions);
  writeList(writer, body.transactions);
  writeList(writer, body.transaction_preimages);
  writeList(writer, body.forced_transaction_preimages);
  writeList(writer, body.cek_program_material);
  writeList(writer, body.deposits);
  writeList(writer, body.transition_trace);
  writeList(writer, body.event_to_step);
  writeList(writer, body.validation_traces);
  writeList(writer, body.validation_trace_witnesses);

  const counts = body.counts;
  writeConstructorStart(writer);
  writeInteger(writer, counts.withdrawalCount);
  writeInteger(writer, counts.forcedTransactionCount);
  writeInteger(writer, counts.l2TransactionCount);
  writeInteger(writer, counts.depositCount);
  writeInteger(writer, counts.totalEventCount);
  writeInteger(writer, counts.transitionStepCount);
  writeInteger(writer, counts.validationTraceCount);
  writer.writeByte(0xff);

  writer.writeByte(0xff);
  writer.writeByte(0xff);
  return writer.finish();
};
