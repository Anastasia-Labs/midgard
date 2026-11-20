import { encodeCborInteger } from "@al-ft/midgard-core/codec/cbor";
import { Data, toHex } from "@lucid-evolution/lucid";
import { sha256 } from "@noble/hashes/sha2.js";

import { HeaderHashSchema, HeaderSchema } from "./ledger-state.js";

export const DA_PAYLOAD_V2_VERSION = 2n;

export const DaPayloadEntrySchema = Data.Tuple([Data.Bytes(), Data.Bytes()]);
export type DaPayloadEntry = Data.Static<typeof DaPayloadEntrySchema>;
export const DaPayloadEntry = DaPayloadEntrySchema as unknown as DaPayloadEntry;

export const DaPayloadCountsV2Schema = Data.Object({
  withdrawalCount: Data.Integer(),
  forcedTransactionCount: Data.Integer(),
  l2TransactionCount: Data.Integer(),
  depositCount: Data.Integer(),
  totalEventCount: Data.Integer(),
  transitionStepCount: Data.Integer(),
});
export type DaPayloadCountsV2 = Data.Static<typeof DaPayloadCountsV2Schema>;
export const DaPayloadCountsV2 =
  DaPayloadCountsV2Schema as unknown as DaPayloadCountsV2;

export const DaPayloadBodyV2Schema = Data.Object({
  header_hash: HeaderHashSchema,
  header: HeaderSchema,
  utxos: Data.Array(DaPayloadEntrySchema),
  withdrawals: Data.Array(DaPayloadEntrySchema),
  forced_transactions: Data.Array(DaPayloadEntrySchema),
  transactions: Data.Array(DaPayloadEntrySchema),
  deposits: Data.Array(DaPayloadEntrySchema),
  transition_trace: Data.Array(DaPayloadEntrySchema),
  event_to_step: Data.Array(DaPayloadEntrySchema),
  counts: DaPayloadCountsV2Schema,
});
export type DaPayloadBodyV2 = Data.Static<typeof DaPayloadBodyV2Schema>;
export const DaPayloadBodyV2 =
  DaPayloadBodyV2Schema as unknown as DaPayloadBodyV2;

export const DaPayloadV2Schema = Data.Object({
  version: Data.Integer(),
  block_body: DaPayloadBodyV2Schema,
});
export type DaPayloadV2 = Data.Static<typeof DaPayloadV2Schema>;
export const DaPayloadV2 = DaPayloadV2Schema as unknown as DaPayloadV2;

const MAX_CBOR_UINT64 = 0xffff_ffff_ffff_ffffn;
const PLUTUS_BYTES_CHUNK = 64;

export class DaPayloadV2NonCanonicalError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "DaPayloadV2NonCanonicalError";
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
        `DaPayloadV2 encoded-size mismatch: wrote ${this.#offset.toString()} of ${this.#output.length.toString()} bytes`,
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
    throw new Error("DaPayloadV2 byte fields must be even-length hexadecimal");
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

const payloadIntegers = (payload: DaPayloadV2): readonly bigint[] => {
  const { header, counts } = payload.block_body;
  return [
    payload.version,
    header.withdrawalCount,
    header.forcedTransactionCount,
    header.l2TransactionCount,
    header.depositCount,
    header.totalEventCount,
    header.transitionStepCount,
    header.startTime,
    header.endTime,
    header.protocolVersion,
    counts.withdrawalCount,
    counts.forcedTransactionCount,
    counts.l2TransactionCount,
    counts.depositCount,
    counts.totalEventCount,
    counts.transitionStepCount,
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
  payload: DaPayloadV2,
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
    integerSize(header.withdrawalCount),
    integerSize(header.forcedTransactionCount),
    integerSize(header.l2TransactionCount),
    integerSize(header.depositCount),
    integerSize(header.totalEventCount),
    integerSize(header.transitionStepCount),
    integerSize(header.startTime),
    integerSize(header.endTime),
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
  ]);
  const bodySize = constructorSize([
    bytesSize(body.header_hash),
    headerSize,
    utxoEncodedListSize,
    listSize(body.withdrawals),
    listSize(body.forced_transactions),
    listSize(body.transactions),
    listSize(body.deposits),
    listSize(body.transition_trace),
    listSize(body.event_to_step),
    countsSize,
  ]);
  return constructorSize([integerSize(payload.version), bodySize]);
};

/** Exact encoded inner DaPayloadV2 size without allocating its CBOR bytes. */
export const daPayloadV2EncodedSize = (payload: DaPayloadV2): number =>
  encodedPayloadSize(payload);

/**
 * Exact encoded inner size when the full UTxO list is represented by a
 * durable entry-count/tuple-byte aggregate rather than materialized in RAM.
 * The `payload.block_body.utxos` value is ignored.
 */
export const daPayloadV2EncodedSizeFromUtxoAggregate = (
  payload: DaPayloadV2,
  utxos: DaPayloadEntrySizeAggregate,
): number =>
  encodedPayloadSize(payload, daPayloadEntriesEncodedSizeFromAggregate(utxos));

/**
 * Encodes the existing Plutus-Data wire format directly into byte chunks.
 * Lucid's schema encoder first builds a full hexadecimal string; at DA scale
 * that doubles the largest allocation and creates severe GC pressure. The
 * direct encoder is byte-identical for the protocol's uint64 integer domain.
 * Rare out-of-domain integers retain Lucid's bignum behavior through fallback.
 */
export const encodeDaPayloadV2 = (payload: DaPayloadV2): Buffer => {
  if (!payloadIntegers(payload).every(isNativeCborInteger)) {
    return Buffer.from(Data.to(payload, DaPayloadV2), "hex");
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
  writeInteger(writer, header.withdrawalCount);
  writeInteger(writer, header.forcedTransactionCount);
  writeInteger(writer, header.l2TransactionCount);
  writeInteger(writer, header.depositCount);
  writeInteger(writer, header.totalEventCount);
  writeInteger(writer, header.transitionStepCount);
  writeInteger(writer, header.startTime);
  writeInteger(writer, header.endTime);
  writeBytes(writer, header.prevHeaderHash);
  writeBytes(writer, header.operatorVkey);
  writeInteger(writer, header.protocolVersion);
  writer.writeByte(0xff);

  writeList(writer, body.utxos);
  writeList(writer, body.withdrawals);
  writeList(writer, body.forced_transactions);
  writeList(writer, body.transactions);
  writeList(writer, body.deposits);
  writeList(writer, body.transition_trace);
  writeList(writer, body.event_to_step);

  const counts = body.counts;
  writeConstructorStart(writer);
  writeInteger(writer, counts.withdrawalCount);
  writeInteger(writer, counts.forcedTransactionCount);
  writeInteger(writer, counts.l2TransactionCount);
  writeInteger(writer, counts.depositCount);
  writeInteger(writer, counts.totalEventCount);
  writeInteger(writer, counts.transitionStepCount);
  writer.writeByte(0xff);

  writer.writeByte(0xff);
  writer.writeByte(0xff);
  return writer.finish();
};

/**
 * Production DA encoder. The strict transport reader intentionally accepts
 * only native CBOR integers, so publication must reject payloads that would
 * require compatibility-only bignum tags.
 */
export const encodeDaPayloadV2Protocol = (payload: DaPayloadV2): Buffer => {
  if (!payloadIntegers(payload).every(isNativeCborInteger)) {
    throw new Error(
      "DaPayloadV2 protocol integers must fit the native CBOR integer range",
    );
  }
  return encodeDaPayloadV2(payload);
};

class DaPayloadV2Reader {
  readonly #bytes: Buffer;
  #offset = 0;

  constructor(bytes: Buffer) {
    this.#bytes = bytes;
  }

  read(): DaPayloadV2 {
    this.#constructorStart("payload");
    const version = this.#integer("payload.version");
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
      withdrawalCount: this.#integer("header.withdrawalCount"),
      forcedTransactionCount: this.#integer("header.forcedTransactionCount"),
      l2TransactionCount: this.#integer("header.l2TransactionCount"),
      depositCount: this.#integer("header.depositCount"),
      totalEventCount: this.#integer("header.totalEventCount"),
      transitionStepCount: this.#integer("header.transitionStepCount"),
      startTime: this.#integer("header.startTime"),
      endTime: this.#integer("header.endTime"),
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
    const deposits = this.#entryList("payload.block_body.deposits");
    const transition_trace = this.#entryList(
      "payload.block_body.transition_trace",
    );
    const event_to_step = this.#entryList("payload.block_body.event_to_step");
    this.#constructorStart("payload.block_body.counts");
    const counts = {
      withdrawalCount: this.#integer("counts.withdrawalCount"),
      forcedTransactionCount: this.#integer("counts.forcedTransactionCount"),
      l2TransactionCount: this.#integer("counts.l2TransactionCount"),
      depositCount: this.#integer("counts.depositCount"),
      totalEventCount: this.#integer("counts.totalEventCount"),
      transitionStepCount: this.#integer("counts.transitionStepCount"),
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
        deposits,
        transition_trace,
        event_to_step,
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
      this.#fail("unexpected end of DaPayloadV2 CBOR");
    }
    return this.#bytes[this.#offset]!;
  }

  #fail(message: string): never {
    throw new Error(`${message} at offset ${this.#offset.toString()}`);
  }

  #nonCanonical(message: string): never {
    throw new DaPayloadV2NonCanonicalError(
      `${message} at offset ${this.#offset.toString()}`,
    );
  }
}

const decodeDaPayloadV2ByteOriented = (payloadCbor: Buffer): DaPayloadV2 =>
  new DaPayloadV2Reader(payloadCbor).read();

/**
 * Fail-closed canonical wire decoder used at untrusted transport boundaries.
 * It never falls back to Lucid's whole-payload hex/object conversion.
 */
export const decodeDaPayloadV2Canonical = (payloadCbor: Buffer): DaPayloadV2 =>
  decodeDaPayloadV2ByteOriented(payloadCbor);

export const decodeDaPayloadV2 = (payloadCbor: Buffer): DaPayloadV2 => {
  try {
    return decodeDaPayloadV2Canonical(payloadCbor);
  } catch {
    // Preserve the historical SDK surface for uncommon Plutus bignum or
    // non-canonical-but-decodable input. Strict committee callers re-encode
    // and compare bytes after this compatibility fallback.
    return Data.from(toHex(payloadCbor), DaPayloadV2 as never) as DaPayloadV2;
  }
};

export const daPayloadHashHex = (payloadCbor: Buffer): string =>
  toHex(sha256(payloadCbor));
