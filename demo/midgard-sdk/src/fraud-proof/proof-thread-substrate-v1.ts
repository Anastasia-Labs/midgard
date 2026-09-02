import { Data } from "@lucid-evolution/lucid";

import { type OutputReference, OutputReferenceSchema } from "@/common.js";
import type { RejectionReasonV1 } from "@/rejection-reason-v1.js";
import { RejectionReasonV1 as RejectionReasonV1Schema } from "@/rejection-reason-v1.js";

export const PROOF_THREAD_SUBJECT_V1_VERSION = 1n;
export const PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1 = 0n;
export const PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1 = 1n;
export const PROOF_THREAD_SOURCE_KIND_ACCEPTED_V1 = 0n;
export const PROOF_THREAD_SOURCE_KIND_FORCED_V1 = 1n;

export type VerdictSubjectV1 = {
  readonly version: bigint;
  readonly direction: bigint;
  readonly source_kind: bigint;
  readonly transaction_id: string;
  readonly source_key: string;
  readonly rejection_reason: RejectionReasonV1 | null;
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

export const verdictSubjectIsCanonicalV1 = (
  subject: VerdictSubjectV1,
): boolean => {
  try {
    canonicalHex(subject.transaction_id, "transaction_id", 32);
    const sourceKey = canonicalHex(subject.source_key, "source_key");
    if (subject.version !== PROOF_THREAD_SUBJECT_V1_VERSION) return false;
    if (subject.source_kind === PROOF_THREAD_SOURCE_KIND_ACCEPTED_V1) {
      return (
        subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1 &&
        sourceKey.length === 0 &&
        subject.rejection_reason === null
      );
    }
    if (subject.source_kind !== PROOF_THREAD_SOURCE_KIND_FORCED_V1)
      return false;
    if (sourceKey.length === 0) return false;
    return subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1
      ? subject.rejection_reason === null
      : subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1 &&
          subject.rejection_reason !== null;
  } catch {
    return false;
  }
};

/** Exact twin of Aiken `encode_verdict_subject_v1`. */
export const encodeVerdictSubjectV1 = (subject: VerdictSubjectV1): Buffer => {
  if (!verdictSubjectIsCanonicalV1(subject)) {
    throw new Error("VerdictSubjectV1 is not canonical");
  }
  const reason =
    subject.rejection_reason === null
      ? cborHeader(4, 0)
      : Buffer.concat([
          cborHeader(4, 1),
          Buffer.from(
            Data.to(subject.rejection_reason, RejectionReasonV1Schema),
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

export const acceptedVerdictSubjectV1 = (
  transactionId: string,
): VerdictSubjectV1 =>
  Object.freeze({
    version: PROOF_THREAD_SUBJECT_V1_VERSION,
    direction: PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1,
    source_kind: PROOF_THREAD_SOURCE_KIND_ACCEPTED_V1,
    transaction_id: canonicalHex(transactionId, "transaction_id", 32).toString(
      "hex",
    ),
    source_key: "",
    rejection_reason: null,
  });

/** Exact twin of Aiken `cbor.serialise(membership.key)`. */
export const encodeProofThreadForcedSourceKeyV1 = (
  sourceKey: OutputReference,
): Buffer =>
  Buffer.from(
    Data.to(sourceKey as never, OutputReferenceSchema as never),
    "hex",
  );

export const forcedVerdictSubjectV1 = ({
  transactionId,
  sourceKey,
  rejectionReason,
}: {
  readonly transactionId: string;
  readonly sourceKey: OutputReference;
  readonly rejectionReason: RejectionReasonV1 | null;
}): VerdictSubjectV1 =>
  Object.freeze({
    version: PROOF_THREAD_SUBJECT_V1_VERSION,
    direction:
      rejectionReason === null
        ? PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1
        : PROOF_THREAD_DIRECTION_WRONGFUL_REJECTION_V1,
    source_kind: PROOF_THREAD_SOURCE_KIND_FORCED_V1,
    transaction_id: canonicalHex(transactionId, "transaction_id", 32).toString(
      "hex",
    ),
    source_key: encodeProofThreadForcedSourceKeyV1(sourceKey).toString("hex"),
    rejection_reason: rejectionReason,
  });

export const bindExactVerdictSubjectReasonV1 = (
  subject: VerdictSubjectV1,
  expected: RejectionReasonV1,
): RejectionReasonV1 => {
  if (
    !verdictSubjectIsCanonicalV1(subject) ||
    subject.rejection_reason === null
  ) {
    throw new Error("VerdictSubjectV1 carries no canonical rejection reason");
  }
  if (
    Data.to(subject.rejection_reason, RejectionReasonV1Schema) !==
    Data.to(expected, RejectionReasonV1Schema)
  ) {
    throw new Error("VerdictSubjectV1 typed reason or coordinate differs");
  }
  return subject.rejection_reason;
};

export const terminalVerdictContradictionV1 = (
  subject: VerdictSubjectV1,
  decisiveFaultHolds: boolean,
): boolean => {
  if (!verdictSubjectIsCanonicalV1(subject)) {
    throw new Error("VerdictSubjectV1 is not canonical");
  }
  return subject.direction === PROOF_THREAD_DIRECTION_WRONGFUL_ACCEPTANCE_V1
    ? decisiveFaultHolds
    : !decisiveFaultHolds;
};
