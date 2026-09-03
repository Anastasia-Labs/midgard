import { encodeCborInteger } from "@al-ft/midgard-core/codec/cbor";
import { Data, toHex } from "@lucid-evolution/lucid";
import { sha256 } from "@noble/hashes/sha2.js";

import { ValidationAuxiliaryWitnessSchema } from "./fraud-proof/validation-auxiliary-witness-v1.js";
import {
  ValidationMachineStateSchema,
  ValidationTraceProofSchema,
} from "./fraud-proof/validation-dispute.js";
import {
  EventKeySchema,
  HeaderHashSchema,
  HeaderSchema,
} from "./ledger-state.js";

export const DA_PAYLOAD_VERSION = 1n;

export const DaPayloadEntrySchema = Data.Tuple([Data.Bytes(), Data.Bytes()]);
export type DaPayloadEntry = Data.Static<typeof DaPayloadEntrySchema>;
export const DaPayloadEntry = DaPayloadEntrySchema as unknown as DaPayloadEntry;

/**
 * Event-local retained validation coordinate. Non-negative values are exact
 * NativeScripts execution indexes; the negative domain is reserved for
 * chronological ScriptSources frontier/control/redeemer-item witnesses and
 * the exact ScriptIntegrity stage-3 terminal control and ValueAndMint asset
 * mutations.
 */
export const RetainedValidationWitnessKeySchema = Data.Object({
  event_key: EventKeySchema,
  execution_index: Data.Integer(),
});
export type RetainedValidationWitnessKey = Data.Static<
  typeof RetainedValidationWitnessKeySchema
>;

/**
 * Minimal public reconstruction material for a ScriptSources frontier/control
 * or redeemer-item state, the ScriptIntegrity stage-3 terminal control, a
 * ValueAndMint asset mutation, or a NativeScripts
 * `nativeExecutionDescriptor` transition. The state
 * remains committed by `validation_traces_root`; this record only opens that
 * state and its exact work witness against the descriptor. Family consumers
 * must additionally verify any auxiliary membership against the authenticated
 * terminal frontier they use.
 */
export const RetainedValidationWitnessSchema = Data.Object({
  machine_state: ValidationMachineStateSchema,
  trace_proof: ValidationTraceProofSchema,
  phase: Data.Integer(),
  program_counter: Data.Integer(),
  witness_cbor: Data.Bytes(),
  auxiliary: ValidationAuxiliaryWitnessSchema,
});
export type RetainedValidationWitness = Data.Static<
  typeof RetainedValidationWitnessSchema
>;

const canonicalDataBytes = <T>(value: T, schema: unknown): Buffer =>
  Buffer.from(Data.to(value as never, schema as never), "hex");

export const encodeRetainedValidationWitnessKey = (
  key: RetainedValidationWitnessKey,
): Buffer => canonicalDataBytes(key, RetainedValidationWitnessKeySchema);

export const encodeRetainedValidationWitness = (
  witness: RetainedValidationWitness,
): Buffer => canonicalDataBytes(witness, RetainedValidationWitnessSchema);

const decodeCanonicalData = <T>(
  bytes: Uint8Array,
  schema: unknown,
  fieldName: string,
): T => {
  const exact = Buffer.from(bytes);
  const decoded = Data.from(exact.toString("hex"), schema as never) as T;
  if (!canonicalDataBytes(decoded, schema).equals(exact)) {
    throw new DaPayloadNonCanonicalError(`${fieldName} is not canonical`);
  }
  return decoded;
};

export const decodeRetainedValidationWitnessKey = (
  bytes: Uint8Array,
): RetainedValidationWitnessKey =>
  decodeCanonicalData(
    bytes,
    RetainedValidationWitnessKeySchema,
    "retained validation witness key",
  );

export const decodeRetainedValidationWitness = (
  bytes: Uint8Array,
): RetainedValidationWitness =>
  decodeCanonicalData(
    bytes,
    RetainedValidationWitnessSchema,
    "retained validation witness",
  );

export const DaPayloadCountsSchema = Data.Object({
  withdrawalCount: Data.Integer(),
  forcedTransactionCount: Data.Integer(),
  l2TransactionCount: Data.Integer(),
  depositCount: Data.Integer(),
  totalEventCount: Data.Integer(),
  transitionStepCount: Data.Integer(),
  validationTraceCount: Data.Integer(),
});
export type DaPayloadCounts = Data.Static<typeof DaPayloadCountsSchema>;
export const DaPayloadCounts =
  DaPayloadCountsSchema as unknown as DaPayloadCounts;

/**
 * V1 DA separates the compact, root-committed transaction sources from
 * their canonical full preimages. This keeps every L1 membership value inside
 * the proof envelope while retaining all data needed to replay validation.
 */
export const DaPayloadBodySchema = Data.Object({
  header_hash: HeaderHashSchema,
  header: HeaderSchema,
  utxos: Data.Array(DaPayloadEntrySchema),
  withdrawals: Data.Array(DaPayloadEntrySchema),
  forced_transactions: Data.Array(DaPayloadEntrySchema),
  transactions: Data.Array(DaPayloadEntrySchema),
  transaction_preimages: Data.Array(DaPayloadEntrySchema),
  forced_transaction_preimages: Data.Array(DaPayloadEntrySchema),
  cek_program_material: Data.Array(DaPayloadEntrySchema),
  deposits: Data.Array(DaPayloadEntrySchema),
  transition_trace: Data.Array(DaPayloadEntrySchema),
  event_to_step: Data.Array(DaPayloadEntrySchema),
  validation_traces: Data.Array(DaPayloadEntrySchema),
  validation_trace_witnesses: Data.Array(DaPayloadEntrySchema),
  counts: DaPayloadCountsSchema,
});
export type DaPayloadBody = Data.Static<typeof DaPayloadBodySchema>;
export const DaPayloadBody = DaPayloadBodySchema as unknown as DaPayloadBody;

export const DaPayloadSchema = Data.Object({
  version: Data.Integer(),
  block_body: DaPayloadBodySchema,
});
export type DaPayload = Data.Static<typeof DaPayloadSchema>;
export const DaPayload = DaPayloadSchema as unknown as DaPayload;

const MAX_CBOR_UINT64 = 0xffff_ffff_ffff_ffffn;
const PLUTUS_BYTES_CHUNK = 64;

export class DaPayloadNonCanonicalError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "DaPayloadV1NonCanonicalError";
  }
}

class ExactBufferWriter {
  readonly #output: Buffer;
  #offset = 0;

  constructor(length: number) {
    this.#output = Buffer.allocUnsafe(length);
  }

  writeByte(value: number): void {
    this.#output[this.#offset] = value;
    this.#offset += 1;
  }

  write(bytes: Uint8Array): void {
    this.#output.set(bytes, this.#offset);
    this.#offset += bytes.length;
  }

  finish(): Buffer {
    if (this.#offset !== this.#output.length) {
      throw new Error(
        `DaPayloadV1 encoded-size mismatch: wrote ${this.#offset.toString()} of ${this.#output.length.toString()} bytes`,
      );
    }
    return this.#output;
  }
}

const writeCborArgument = (
  writer: ExactBufferWriter,
  major: number,
  value: bigint,
): void => {
  const prefix = major << 5;
  if (value < 24n) {
    writer.writeByte(prefix | Number(value));
    return;
  }
  if (value <= 0xffn) {
    writer.writeByte(prefix | 24);
    writer.writeByte(Number(value));
    return;
  }
  if (value <= 0xffffn) {
    const bytes = Buffer.allocUnsafe(3);
    bytes[0] = prefix | 25;
    bytes.writeUInt16BE(Number(value), 1);
    writer.write(bytes);
    return;
  }
  if (value <= 0xffff_ffffn) {
    const bytes = Buffer.allocUnsafe(5);
    bytes[0] = prefix | 26;
    bytes.writeUInt32BE(Number(value), 1);
    writer.write(bytes);
    return;
  }
  const bytes = Buffer.allocUnsafe(9);
  bytes[0] = prefix | 27;
  bytes.writeBigUInt64BE(value, 1);
  writer.write(bytes);
};

const writeInteger = (writer: ExactBufferWriter, value: bigint): void => {
  writer.write(encodeCborInteger(value));
};

const assertBytesHex = (value: string): void => {
  if (value.length % 2 !== 0 || !/^[0-9a-fA-F]*$/.test(value)) {
    throw new Error("DaPayloadV1 byte fields must be even-length hexadecimal");
  }
};

const writeBytes = (writer: ExactBufferWriter, value: string): void => {
  assertBytesHex(value);
  const bytes = Buffer.from(value, "hex");
  if (bytes.length <= PLUTUS_BYTES_CHUNK) {
    writeCborArgument(writer, 2, BigInt(bytes.length));
    writer.write(bytes);
    return;
  }
  writer.writeByte(0x5f);
  for (let offset = 0; offset < bytes.length; offset += PLUTUS_BYTES_CHUNK) {
    const chunk = bytes.subarray(offset, offset + PLUTUS_BYTES_CHUNK);
    writeCborArgument(writer, 2, BigInt(chunk.length));
    writer.write(chunk);
  }
  writer.writeByte(0xff);
};

const writeConstructorStart = (writer: ExactBufferWriter): void => {
  writer.writeByte(0xd8);
  writer.writeByte(0x79);
  writer.writeByte(0x9f);
};

const writeList = (
  writer: ExactBufferWriter,
  entries: readonly DaPayloadEntry[],
): void => {
  if (entries.length === 0) {
    writer.writeByte(0x80);
    return;
  }
  writer.writeByte(0x9f);
  for (const [key, value] of entries) {
    writer.writeByte(0x9f);
    writeBytes(writer, key);
    writeBytes(writer, value);
    writer.writeByte(0xff);
  }
  writer.writeByte(0xff);
};

const payloadIntegers = (payload: DaPayload): readonly bigint[] => {
  const { header, counts } = payload.block_body;
  return [
    payload.version,
    header.withdrawalCount,
    header.forcedTransactionCount,
    header.l2TransactionCount,
    header.depositCount,
    header.totalEventCount,
    header.transitionStepCount,
    header.validationTraceCount,
    header.startTime,
    header.endTime,
    header.blockSlot,
    header.expectedNetworkId,
    header.minFeeA,
    header.minFeeB,
    header.protocolVersion,
    counts.withdrawalCount,
    counts.forcedTransactionCount,
    counts.l2TransactionCount,
    counts.depositCount,
    counts.totalEventCount,
    counts.transitionStepCount,
    counts.validationTraceCount,
  ];
};

const isNativeCborInteger = (value: bigint): boolean =>
  value >= -1n - MAX_CBOR_UINT64 && value <= MAX_CBOR_UINT64;

const cborArgumentSize = (value: bigint): number =>
  value < 24n
    ? 1
    : value <= 0xffn
      ? 2
      : value <= 0xffffn
        ? 3
        : value <= 0xffff_ffffn
          ? 5
          : 9;

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

class DaPayloadReader {
  readonly #bytes: Buffer;
  #offset = 0;

  constructor(bytes: Buffer) {
    this.#bytes = bytes;
  }

  read(): DaPayload {
    this.#constructorStart("payload");
    const version = this.#integer("payload.version");
    if (version !== DA_PAYLOAD_VERSION) {
      this.#fail(`payload.version must equal ${DA_PAYLOAD_VERSION.toString()}`);
    }
    this.#constructorStart("payload.block_body");
    const header_hash = this.#bytesHex("payload.block_body.header_hash");
    this.#constructorStart("payload.block_body.header");
    const header = {
      prevUtxosRoot: this.#bytesHex("header.prevUtxosRoot"),
      utxosRoot: this.#bytesHex("header.utxosRoot"),
      withdrawalsRoot: this.#bytesHex("header.withdrawalsRoot"),
      forcedTransactionsRoot: this.#bytesHex("header.forcedTransactionsRoot"),
      transactionsRoot: this.#bytesHex("header.transactionsRoot"),
      depositsRoot: this.#bytesHex("header.depositsRoot"),
      transitionTraceRoot: this.#bytesHex("header.transitionTraceRoot"),
      eventToStepRoot: this.#bytesHex("header.eventToStepRoot"),
      validationTracesRoot: this.#bytesHex("header.validationTracesRoot"),
      withdrawalCount: this.#integer("header.withdrawalCount"),
      forcedTransactionCount: this.#integer("header.forcedTransactionCount"),
      l2TransactionCount: this.#integer("header.l2TransactionCount"),
      depositCount: this.#integer("header.depositCount"),
      totalEventCount: this.#integer("header.totalEventCount"),
      transitionStepCount: this.#integer("header.transitionStepCount"),
      validationTraceCount: this.#integer("header.validationTraceCount"),
      startTime: this.#integer("header.startTime"),
      endTime: this.#integer("header.endTime"),
      blockSlot: this.#integer("header.blockSlot"),
      expectedNetworkId: this.#integer("header.expectedNetworkId"),
      minFeeA: this.#integer("header.minFeeA"),
      minFeeB: this.#integer("header.minFeeB"),
      prevHeaderHash: this.#bytesHex("header.prevHeaderHash"),
      operatorVkey: this.#bytesHex("header.operatorVkey"),
      protocolVersion: this.#integer("header.protocolVersion"),
    };
    this.#break("payload.block_body.header");
    const utxos = this.#entryList("payload.block_body.utxos");
    const withdrawals = this.#entryList("payload.block_body.withdrawals");
    const forced_transactions = this.#entryList(
      "payload.block_body.forced_transactions",
    );
    const transactions = this.#entryList("payload.block_body.transactions");
    const transaction_preimages = this.#entryList(
      "payload.block_body.transaction_preimages",
    );
    const forced_transaction_preimages = this.#entryList(
      "payload.block_body.forced_transaction_preimages",
    );
    const cek_program_material = this.#entryList(
      "payload.block_body.cek_program_material",
    );
    const deposits = this.#entryList("payload.block_body.deposits");
    const transition_trace = this.#entryList(
      "payload.block_body.transition_trace",
    );
    const event_to_step = this.#entryList("payload.block_body.event_to_step");
    const validation_traces = this.#entryList(
      "payload.block_body.validation_traces",
    );
    const validation_trace_witnesses = this.#entryList(
      "payload.block_body.validation_trace_witnesses",
    );
    this.#constructorStart("payload.block_body.counts");
    const counts = {
      withdrawalCount: this.#integer("counts.withdrawalCount"),
      forcedTransactionCount: this.#integer("counts.forcedTransactionCount"),
      l2TransactionCount: this.#integer("counts.l2TransactionCount"),
      depositCount: this.#integer("counts.depositCount"),
      totalEventCount: this.#integer("counts.totalEventCount"),
      transitionStepCount: this.#integer("counts.transitionStepCount"),
      validationTraceCount: this.#integer("counts.validationTraceCount"),
    };
    this.#break("payload.block_body.counts");
    this.#break("payload.block_body");
    this.#break("payload");
    if (this.#offset !== this.#bytes.length) {
      this.#fail("payload has trailing CBOR bytes");
    }
    return {
      version,
      block_body: {
        header_hash,
        header,
        utxos,
        withdrawals,
        forced_transactions,
        transactions,
        transaction_preimages,
        forced_transaction_preimages,
        cek_program_material,
        deposits,
        transition_trace,
        event_to_step,
        validation_traces,
        validation_trace_witnesses,
        counts,
      },
    };
  }

  #entryList(fieldName: string): DaPayloadEntry[] {
    if (this.#peek() === 0x80) {
      this.#offset += 1;
      return [];
    }
    if (this.#peek() !== 0x9f && this.#peek() >> 5 === 4) {
      this.#nonCanonical(`${fieldName} non-empty list must be indefinite`);
    }
    this.#expect(0x9f, `${fieldName} list start`);
    const entries: DaPayloadEntry[] = [];
    while (this.#peek() !== 0xff) {
      if (this.#peek() !== 0x9f && this.#peek() >> 5 === 4) {
        this.#nonCanonical(`${fieldName} tuple must be indefinite`);
      }
      this.#expect(0x9f, `${fieldName} tuple start`);
      const key = this.#bytesHex(`${fieldName}.key`);
      const value = this.#bytesHex(`${fieldName}.value`);
      this.#break(`${fieldName} tuple`);
      entries.push([key, value]);
    }
    if (entries.length === 0) {
      this.#nonCanonical(`${fieldName} empty list must use definite framing`);
    }
    this.#break(fieldName);
    return entries;
  }

  #bytesHex(fieldName: string): string {
    if (this.#peek() === 0x5f) {
      this.#offset += 1;
      const chunks: string[] = [];
      let previousChunkLength: number | undefined;
      let totalLength = 0;
      while (this.#peek() !== 0xff) {
        const span = this.#definiteBytesSpan(fieldName);
        const chunkLength = span.end - span.start;
        if (chunkLength === 0 || chunkLength > PLUTUS_BYTES_CHUNK) {
          this.#nonCanonical(
            `${fieldName} indefinite byte chunk must contain 1 to ${PLUTUS_BYTES_CHUNK.toString()} bytes`,
          );
        }
        if (
          previousChunkLength !== undefined &&
          previousChunkLength !== PLUTUS_BYTES_CHUNK
        ) {
          this.#nonCanonical(
            `${fieldName} non-final indefinite byte chunks must contain exactly ${PLUTUS_BYTES_CHUNK.toString()} bytes`,
          );
        }
        chunks.push(this.#bytes.toString("hex", span.start, span.end));
        previousChunkLength = chunkLength;
        totalLength += chunkLength;
      }
      this.#break(fieldName);
      if (totalLength <= PLUTUS_BYTES_CHUNK) {
        this.#nonCanonical(
          `${fieldName} byte strings of at most ${PLUTUS_BYTES_CHUNK.toString()} bytes must use definite framing`,
        );
      }
      return chunks.join("");
    }
    const span = this.#definiteBytesSpan(fieldName);
    if (span.end - span.start > PLUTUS_BYTES_CHUNK) {
      this.#nonCanonical(
        `${fieldName} byte strings over ${PLUTUS_BYTES_CHUNK.toString()} bytes must use indefinite framing`,
      );
    }
    return this.#bytes.toString("hex", span.start, span.end);
  }

  #definiteBytesSpan(fieldName: string): {
    readonly start: number;
    readonly end: number;
  } {
    const header = this.#argument(fieldName);
    if (header.major !== 2) this.#fail(`${fieldName} must be bytes`);
    if (header.value > BigInt(Number.MAX_SAFE_INTEGER)) {
      this.#fail(`${fieldName} byte length exceeds the safe integer range`);
    }
    const start = this.#offset;
    const end = start + Number(header.value);
    if (end > this.#bytes.length) this.#fail(`${fieldName} exceeds input`);
    this.#offset = end;
    return { start, end };
  }

  #integer(fieldName: string): bigint {
    const header = this.#argument(fieldName);
    if (header.major === 0) return header.value;
    if (header.major === 1) return -1n - header.value;
    return this.#fail(`${fieldName} must be an integer`);
  }

  #argument(fieldName: string): {
    readonly major: number;
    readonly value: bigint;
  } {
    const initial = this.#peek();
    this.#offset += 1;
    const major = initial >> 5;
    const additional = initial & 0x1f;
    if (additional < 24) return { major, value: BigInt(additional) };
    const length =
      additional === 24
        ? 1
        : additional === 25
          ? 2
          : additional === 26
            ? 4
            : additional === 27
              ? 8
              : this.#fail(`${fieldName} has unsupported CBOR framing`);
    if (this.#offset + length > this.#bytes.length) {
      this.#fail(`${fieldName} CBOR argument exceeds input`);
    }
    let value = 0n;
    for (let index = 0; index < length; index += 1) {
      value = (value << 8n) | BigInt(this.#bytes[this.#offset + index]!);
    }
    this.#offset += length;
    const minimumValue =
      additional === 24
        ? 24n
        : additional === 25
          ? 0x100n
          : additional === 26
            ? 0x1_0000n
            : 0x1_0000_0000n;
    if (value < minimumValue) {
      this.#nonCanonical(`${fieldName} CBOR argument is not minimally encoded`);
    }
    return { major, value };
  }

  #constructorStart(fieldName: string): void {
    if (
      this.#peek() === 0xd9 &&
      this.#bytes[this.#offset + 1] === 0x00 &&
      this.#bytes[this.#offset + 2] === 0x79
    ) {
      this.#nonCanonical(
        `${fieldName} constructor tag is not minimally encoded`,
      );
    }
    this.#expect(0xd8, `${fieldName} constructor tag`);
    this.#expect(0x79, `${fieldName} constructor alternative`);
    if (this.#peek() !== 0x9f && this.#peek() >> 5 === 4) {
      this.#nonCanonical(`${fieldName} constructor fields must be indefinite`);
    }
    this.#expect(0x9f, `${fieldName} constructor fields`);
  }

  #break(fieldName: string): void {
    this.#expect(0xff, `${fieldName} break`);
  }

  #expect(value: number, fieldName: string): void {
    if (this.#peek() !== value) {
      this.#fail(`${fieldName} has unexpected CBOR framing`);
    }
    this.#offset += 1;
  }

  #peek(): number {
    if (this.#offset >= this.#bytes.length) {
      this.#fail("unexpected end of DaPayloadV1 CBOR");
    }
    return this.#bytes[this.#offset]!;
  }

  #fail(message: string): never {
    throw new Error(`${message} at offset ${this.#offset.toString()}`);
  }

  #nonCanonical(message: string): never {
    throw new DaPayloadNonCanonicalError(
      `${message} at offset ${this.#offset.toString()}`,
    );
  }
}

/**
 * Fail-closed canonical wire decoder used at untrusted transport boundaries.
 * It never falls back to Lucid's whole-payload hex/object conversion.
 */
export const decodeDaPayload = (payloadCbor: Buffer): DaPayload =>
  new DaPayloadReader(payloadCbor).read();

export const daPayloadHashHex = (payloadCbor: Buffer): string =>
  toHex(sha256(payloadCbor));
