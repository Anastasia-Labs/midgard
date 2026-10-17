import { asArray, asBigInt, asBytes } from "./codec/cbor.js";
import {
  MidgardTxCodecError,
  MidgardTxCodecErrorCodes,
} from "./codec/errors.js";
import { DA_PAYLOAD_INNER_SCHEMA_VERSION } from "./da-payload-envelope.js";
import {
  DA_TRANSPORT_PROTOCOL_VERSION,
  DaConflictEvidenceKind,
  DaGenericFoundStatus,
  DaLocalPayloadStatus,
  DaMetadataStatus,
  type DaPayloadChunkManifest,
  DaProofBundleStatus,
} from "./da-transport.da-libp2p-runtime-manifest.js";

export type DaPayloadChunkRequest = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer;
  readonly chunkIndex: number;
};

export type DaPayloadChunkResponse = {
  readonly status: DaGenericFoundStatus;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer;
  readonly chunkIndex: number;
  readonly chunkBytes: Buffer | null;
  readonly chunkHash: Buffer | null;
};

export type DaMetadataByHeaderResponse = {
  readonly status: DaMetadataStatus;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer | null;
  readonly payloadSchemaVersion: typeof DA_PAYLOAD_INNER_SCHEMA_VERSION | null;
  readonly payloadBytes: number | null;
  readonly rootSummaryHash: Buffer | null;
  readonly proofBundleHash: Buffer | null;
  readonly transitionTraceRoot: Buffer | null;
  readonly eventToStepRoot: Buffer | null;
  readonly retainedUntilSlot: number | null;
  readonly localStatus: DaLocalPayloadStatus | null;
};

export type DaProofBundleByHeaderRequest = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly maxInlineBytes: number;
};

export type DaProofBundleByHeaderResponse = {
  readonly status: DaProofBundleStatus;
  readonly headerHash: Buffer;
  readonly proofBundleHash: Buffer | null;
  readonly proofBundleBytes: Buffer | null;
  readonly chunkManifest: DaPayloadChunkManifest | null;
  readonly reasonCode: string | null;
};

export type DaTraceStepByIndexRequest = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly stepIndex: number;
};

export type DaTraceStepByIndexResponse = {
  readonly status: DaGenericFoundStatus;
  readonly headerHash: Buffer;
  readonly stepIndex: number;
  readonly transitionStepBytes: Buffer | null;
  readonly membershipProofBytes: Buffer | null;
};

export type DaEventToStepByEventRequest = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly eventKey: Buffer;
};

export type DaEventToStepByEventResponse = {
  readonly status: DaGenericFoundStatus;
  readonly headerHash: Buffer;
  readonly eventKey: Buffer;
  readonly eventToStepEntryBytes: Buffer | null;
  readonly membershipOrNonmembershipProofBytes: Buffer | null;
};

export type DaAttestationGossip = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly payloadHash: Buffer;
  readonly availabilityCommitmentCbor: Buffer;
  readonly availabilityCommitmentDigest: Buffer;
  readonly signerIndex: number;
  readonly daVkey: Buffer;
  readonly onChainWitness: Buffer;
  readonly retentionUntilSlot: number;
  readonly announcedByPeerId: string;
};

export type DaAttestationsByHeaderRequest = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly acceptedSignerIndexes: readonly number[] | null;
  readonly maxAttestations: number | null;
};

export type DaAttestationsByHeaderResponse = {
  readonly status: DaGenericFoundStatus;
  readonly headerHash: Buffer;
  readonly attestations: readonly DaAttestationGossip[];
  readonly reasonCode: string | null;
};

export type DaConflictEvidence = {
  readonly deploymentFingerprint: Buffer;
  readonly headerHash: Buffer;
  readonly evidenceKind: DaConflictEvidenceKind;
  readonly evidenceHash: Buffer;
  readonly compactEvidence: Buffer | null;
};

export type DaConflictingSignatureHeaderEvidence = {
  readonly signerIndex: number;
  readonly daVkey: Buffer;
  readonly lowerHeaderHash: Buffer;
  readonly lowerCommitmentCbor: Buffer;
  readonly lowerHeaderWitness: Buffer;
  readonly upperHeaderHash: Buffer;
  readonly upperCommitmentCbor: Buffer;
  readonly upperHeaderWitness: Buffer;
};

export type NumericEnumTable = Readonly<Record<string, number>>;

export type NumericEnumLabel<T extends NumericEnumTable> = Extract<
  keyof T,
  string
>;

export const fail = (
  code: (typeof MidgardTxCodecErrorCodes)[keyof typeof MidgardTxCodecErrorCodes],
  message: string,
  detail?: string,
): never => {
  throw new MidgardTxCodecError(code, message, detail);
};

export const enumCode = <T extends NumericEnumTable>(
  table: T,
  label: NumericEnumLabel<T>,
  fieldName: string,
): number => {
  const code = table[label];
  if (code === undefined) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} has unsupported enum label`,
      String(label),
    );
  }
  return code;
};

export const enumLabel = <T extends NumericEnumTable>(
  table: T,
  code: number,
  fieldName: string,
): NumericEnumLabel<T> => {
  for (const [label, candidateCode] of Object.entries(table) as [
    NumericEnumLabel<T>,
    number,
  ][]) {
    if (candidateCode === code) {
      return label;
    }
  }
  return fail(
    MidgardTxCodecErrorCodes.SchemaMismatch,
    `${fieldName} has unsupported enum code`,
    String(code),
  );
};

export const ensureUint = (value: unknown, fieldName: string): number => {
  const int = asBigInt(value, fieldName);
  if (int < 0n || int > BigInt(Number.MAX_SAFE_INTEGER)) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a safe unsigned integer`,
      int.toString(),
    );
  }
  return Number(int);
};

export const ensureDaPayloadSchema = (
  value: unknown,
  fieldName: string,
): typeof DA_PAYLOAD_INNER_SCHEMA_VERSION => {
  const version = ensureUint(value, fieldName);
  if (version !== DA_PAYLOAD_INNER_SCHEMA_VERSION) {
    fail(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must equal ${DA_PAYLOAD_INNER_SCHEMA_VERSION.toString()}`,
      `actual=${version.toString()}`,
    );
  }
  return DA_PAYLOAD_INNER_SCHEMA_VERSION;
};

export const ensureDaTransport = (
  value: unknown,
  fieldName: string,
): typeof DA_TRANSPORT_PROTOCOL_VERSION => {
  const version = ensureUint(value, fieldName);
  if (version !== DA_TRANSPORT_PROTOCOL_VERSION) {
    fail(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must equal ${DA_TRANSPORT_PROTOCOL_VERSION.toString()}`,
      `actual=${version.toString()}`,
    );
  }
  return DA_TRANSPORT_PROTOCOL_VERSION;
};

export const ensureUint8 = (value: unknown, fieldName: string): number => {
  const int = ensureUint(value, fieldName);
  if (int > 255) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a uint8`,
      String(int),
    );
  }
  return int;
};

export const cborUint = (value: unknown, fieldName: string): bigint =>
  BigInt(ensureUint(value, fieldName));

export const cborOptionalUint = (
  value: unknown,
  fieldName: string,
): bigint | null => (value == null ? null : cborUint(value, fieldName));

export const stringValue = (value: unknown, fieldName: string): string => {
  if (typeof value !== "string") {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be a string`,
    );
  }
  return value as string;
};

export const exactStringEnumValue = <
  T extends Readonly<Record<string, string>>,
>(
  table: T,
  value: unknown,
  fieldName: string,
): T[keyof T] => {
  const exact = stringValue(value, fieldName);
  if (!Object.values(table).includes(exact)) {
    fail(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} is not supported by DA transport V1`,
      exact,
    );
  }
  return exact as T[keyof T];
};

export const optionalStringValue = (
  value: unknown,
  fieldName: string,
): string | null => (value == null ? null : stringValue(value, fieldName));

export const fixedArray = (
  value: unknown,
  expectedLength: number,
  fieldName: string,
): unknown[] => {
  const arr = asArray(value, fieldName);
  if (arr.length !== expectedLength) {
    fail(
      MidgardTxCodecErrorCodes.SchemaMismatch,
      `${fieldName} must have exactly ${expectedLength} elements`,
      `length=${arr.length}`,
    );
  }
  return arr;
};

export const ensureByteLength = (
  value: Uint8Array,
  expectedLength: number,
  fieldName: string,
): Buffer => {
  const bytes = Buffer.from(value);
  if (bytes.length !== expectedLength) {
    fail(
      MidgardTxCodecErrorCodes.InvalidFieldType,
      `${fieldName} must be ${expectedLength} bytes`,
      `length=${bytes.length}`,
    );
  }
  return bytes;
};

export const bytesValue = (value: unknown, fieldName: string): Buffer =>
  asBufferView(asBytes(value, fieldName));

const asBufferView = (value: Uint8Array): Buffer =>
  Buffer.isBuffer(value)
    ? value
    : Buffer.from(value.buffer, value.byteOffset, value.byteLength);
