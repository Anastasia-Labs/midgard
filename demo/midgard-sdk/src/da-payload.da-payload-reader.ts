import { toHex } from "@lucid-evolution/lucid";
import { sha256 } from "@noble/hashes/sha2.js";

import {
  DA_PAYLOAD_VERSION,
  DaPayload,
  DaPayloadEntry,
  DaPayloadNonCanonicalError,
  PLUTUS_BYTES_CHUNK,
} from "./da-payload.write-cbor-argument.js";

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
