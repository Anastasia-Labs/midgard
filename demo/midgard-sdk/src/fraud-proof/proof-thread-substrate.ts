import { Data } from "@lucid-evolution/lucid";

import { type OutputReference, OutputReferenceSchema } from "../common.js";
import type { RejectionReason } from "../rejection-reason.js";
import { RejectionReason as RejectionReasonSchema } from "../rejection-reason.js";

export const PROOF_THREAD_SUBJECT_VERSION = 1n;
export const PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE = 0n;
export const PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION = 1n;
export const PROOF_THREAD_SOURCE_KIND_ACCEPTED = 0n;
export const PROOF_THREAD_SOURCE_KIND_FORCED = 1n;

export type VerdictSubject = {
  readonly version: bigint;
  readonly direction: bigint;
  readonly source_kind: bigint;
  readonly transaction_id: string;
  readonly source_key: string;
  readonly rejection_reason: RejectionReason | null;
};

const canonicalHex = (
  value: string,
  field: string,
  byteLength?: number,
): Buffer => {
  if (
    value !== value.toLowerCase() ||
    !/^(?:[0-9a-f]{2})*$/u.test(value) ||
    (byteLength !== undefined && value.length !== byteLength * 2)
  ) {
    throw new Error(
      `${field} must be canonical lowercase hex${byteLength === undefined ? "" : ` of ${byteLength.toString()} bytes`}`,
    );
  }
  return Buffer.from(value, "hex");
};

const cborHeader = (major: number, value: number): Buffer => {
  if (!Number.isSafeInteger(value) || value < 0) {
    throw new Error("CBOR header value must be a non-negative safe integer");
  }
  if (value < 24) return Buffer.from([(major << 5) | value]);
  if (value <= 0xff) return Buffer.from([(major << 5) | 24, value]);
  if (value <= 0xffff) {
    const result = Buffer.alloc(3);
    result[0] = (major << 5) | 25;
    result.writeUInt16BE(value, 1);
    return result;
  }
  if (value <= 0xffff_ffff) {
    const result = Buffer.alloc(5);
    result[0] = (major << 5) | 26;
    result.writeUInt32BE(value, 1);
    return result;
  }
  const result = Buffer.alloc(9);
  result[0] = (major << 5) | 27;
  result.writeBigUInt64BE(BigInt(value), 1);
  return result;
};

const cborUnsigned = (value: bigint): Buffer => {
  if (value < 0n || value > 0xffff_ffff_ffff_ffffn) {
    throw new Error("proof-thread integer is outside canonical uint64 CBOR");
  }
  if (value <= BigInt(Number.MAX_SAFE_INTEGER)) {
    return cborHeader(0, Number(value));
  }
  const result = Buffer.alloc(9);
  result[0] = 0x1b;
  result.writeBigUInt64BE(value, 1);
  return result;
};

const cborBytes = (value: Buffer): Buffer =>
  Buffer.concat([cborHeader(2, value.length), value]);

export const verdictSubjectIsCanonical = (subject: VerdictSubject): boolean => {
  try {
    canonicalHex(subject.transaction_id, "transaction_id", 32);
    const sourceKey = canonicalHex(subject.source_key, "source_key");
    if (subject.version !== PROOF_THREAD_SUBJECT_VERSION) return false;
    if (subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED) {
      return (
        subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE &&
        sourceKey.length === 0 &&
        subject.rejection_reason === null
      );
    }
    if (subject.source_kind !== PROOF_THREAD_SOURCE_KIND_FORCED) return false;
    if (sourceKey.length === 0) return false;
    return subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE
      ? subject.rejection_reason === null
      : subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION &&
          subject.rejection_reason !== null;
  } catch {
    return false;
  }
};

/** Exact twin of Aiken `encode_verdict_subject_v1`. */
export const encodeVerdictSubject = (subject: VerdictSubject): Buffer => {
  if (!verdictSubjectIsCanonical(subject)) {
    throw new Error("VerdictSubjectV1 is not canonical");
  }
  const reason =
    subject.rejection_reason === null
      ? cborHeader(4, 0)
      : Buffer.concat([
          cborHeader(4, 1),
          Buffer.from(
            Data.to(subject.rejection_reason, RejectionReasonSchema),
            "hex",
          ),
        ]);
  return Buffer.concat([
    cborHeader(4, 6),
    cborUnsigned(subject.version),
    cborUnsigned(subject.direction),
    cborUnsigned(subject.source_kind),
    cborBytes(canonicalHex(subject.transaction_id, "transaction_id", 32)),
    cborBytes(canonicalHex(subject.source_key, "source_key")),
    reason,
  ]);
};

export const acceptedVerdictSubject = (transactionId: string): VerdictSubject =>
  Object.freeze({
    version: PROOF_THREAD_SUBJECT_VERSION,
    direction: PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE,
    source_kind: PROOF_THREAD_SOURCE_KIND_ACCEPTED,
    transaction_id: canonicalHex(transactionId, "transaction_id", 32).toString(
      "hex",
    ),
    source_key: "",
    rejection_reason: null,
  });

/** Exact twin of Aiken `cbor.serialise(membership.key)`. */
export const encodeProofThreadForcedSourceKey = (
  sourceKey: OutputReference,
): Buffer =>
  Buffer.from(
    Data.to(sourceKey as never, OutputReferenceSchema as never),
    "hex",
  );

export const forcedVerdictSubject = ({
  transactionId,
  sourceKey,
  rejectionReason,
}: {
  readonly transactionId: string;
  readonly sourceKey: OutputReference;
  readonly rejectionReason: RejectionReason | null;
}): VerdictSubject =>
  Object.freeze({
    version: PROOF_THREAD_SUBJECT_VERSION,
    direction:
      rejectionReason === null
        ? PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE
        : PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION,
    source_kind: PROOF_THREAD_SOURCE_KIND_FORCED,
    transaction_id: canonicalHex(transactionId, "transaction_id", 32).toString(
      "hex",
    ),
    source_key: encodeProofThreadForcedSourceKey(sourceKey).toString("hex"),
    rejection_reason: rejectionReason,
  });

export const bindExactVerdictSubjectReason = (
  subject: VerdictSubject,
  expected: RejectionReason,
): RejectionReason => {
  if (
    !verdictSubjectIsCanonical(subject) ||
    subject.rejection_reason === null
  ) {
    throw new Error("VerdictSubjectV1 carries no canonical rejection reason");
  }
  if (
    Data.to(subject.rejection_reason, RejectionReasonSchema) !==
    Data.to(expected, RejectionReasonSchema)
  ) {
    throw new Error("VerdictSubjectV1 typed reason or coordinate differs");
  }
  return subject.rejection_reason;
};

export const terminalVerdictContradiction = (
  subject: VerdictSubject,
  decisiveFaultHolds: boolean,
): boolean => {
  if (!verdictSubjectIsCanonical(subject)) {
    throw new Error("VerdictSubjectV1 is not canonical");
  }
  return subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE
    ? decisiveFaultHolds
    : !decisiveFaultHolds;
};
