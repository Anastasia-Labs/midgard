import { encodeCborInteger } from "@al-ft/midgard-core/codec/cbor";
import { asDataType } from "@al-ft/midgard-core/lucid-data";
import { Data } from "@lucid-evolution/lucid";

import {
  ValidationMachineStateSchema,
  ValidationTraceProofSchema,
} from "./fraud-proof/validation-dispute.js";
import {
  EventKeySchema,
  HeaderHashSchema,
  HeaderSchema,
} from "./ledger-state.js";
import { RetainedValidationAuxiliaryWitnessSchema } from "./retained-validation-auxiliary.js";

export const DA_PAYLOAD_VERSION = 1n;

export const DaPayloadEntrySchema = Data.Tuple([Data.Bytes(), Data.Bytes()]);

export type DaPayloadEntry = Data.Static<typeof DaPayloadEntrySchema>;

export const DaPayloadEntry = asDataType<DaPayloadEntry>(DaPayloadEntrySchema);

/**
 * Event-local retained validation coordinate. Non-negative values are exact
 * NativeScripts execution indexes; the negative domain is reserved for
 * all chronological state/work witnesses; two further negative coordinates
 * retain initial context and terminal endpoint openings. See
 * retainedValidationStateCoordinate and retainedValidationEndpointCoordinate.
 */
export const RetainedValidationWitnessKeySchema = Data.Object({
  event_key: EventKeySchema,
  execution_index: Data.Integer(),
});

export type RetainedValidationWitnessKey = Data.Static<
  typeof RetainedValidationWitnessKeySchema
>;

/**
 * Public reconstruction material for every chronological deterministic state,
 * initial context and terminal endpoint, including a ScriptSources frontier/control
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
  auxiliary: RetainedValidationAuxiliaryWitnessSchema,
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

export const DaPayloadCounts = asDataType<DaPayloadCounts>(
  DaPayloadCountsSchema,
);

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

export const DaPayloadBody = asDataType<DaPayloadBody>(DaPayloadBodySchema);

export const DaPayloadSchema = Data.Object({
  version: Data.Integer(),
  block_body: DaPayloadBodySchema,
});

export type DaPayload = Data.Static<typeof DaPayloadSchema>;

export const DaPayload = asDataType<DaPayload>(DaPayloadSchema);

const MAX_CBOR_UINT64 = 0xffff_ffff_ffff_ffffn;

export const PLUTUS_BYTES_CHUNK = 64;

export class DaPayloadNonCanonicalError extends Error {
  constructor(message: string) {
    super(message);
    this.name = "DaPayloadV1NonCanonicalError";
  }
}

export class ExactBufferWriter {
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

export const writeInteger = (
  writer: ExactBufferWriter,
  value: bigint,
): void => {
  writer.write(encodeCborInteger(value));
};

export const assertBytesHex = (value: string): void => {
  if (value.length % 2 !== 0 || !/^[0-9a-fA-F]*$/.test(value)) {
    throw new Error("DaPayloadV1 byte fields must be even-length hexadecimal");
  }
};

export const writeBytes = (writer: ExactBufferWriter, value: string): void => {
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

export const writeConstructorStart = (writer: ExactBufferWriter): void => {
  writer.writeByte(0xd8);
  writer.writeByte(0x79);
  writer.writeByte(0x9f);
};

export const writeList = (
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

export const payloadIntegers = (payload: DaPayload): readonly bigint[] => {
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

export const isNativeCborInteger = (value: bigint): boolean =>
  value >= -1n - MAX_CBOR_UINT64 && value <= MAX_CBOR_UINT64;

export const cborArgumentSize = (value: bigint): number =>
  value < 24n
    ? 1
    : value <= 0xffn
      ? 2
      : value <= 0xffffn
        ? 3
        : value <= 0xffff_ffffn
          ? 5
          : 9;
