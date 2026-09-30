import { computeDaSha256Hash } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

import {
  type ChainPoint,
  chainPointKeys,
  type DaSignatureRecordV1,
  type DaStoredPayloadCountSet,
  type DaStoredValidationSummary,
} from "./domain.da-payload-record.js";
import {
  l1HeaderKeys,
  optionalStringProperty,
  parsePayloadRootSet,
  payloadCountKeys,
  requireEnum,
  requireExactObject,
  requireNonNegativeBigInt,
  requireString,
  signatureBroadcastStatuses,
  signatureRecordOptionalKeys,
  signatureRecordRequiredKeys,
  signatureSources,
  validationSummaryKeys,
} from "./domain.parse-da-stored-payload-record.js";

export const parseDaSignatureRecord = (value: unknown): DaSignatureRecordV1 => {
  const record = requireExactObject(
    value,
    signatureRecordRequiredKeys,
    signatureRecordOptionalKeys,
    "DA signature record V1",
  );
  const headerHash = requireString(
    record.headerHash,
    "DA signature record V1.headerHash",
  );
  const validation = parseValidationSummary(record.validation);
  if (validation.headerHash !== headerHash) {
    throw new Error(
      "DA signature record V1.validation.headerHash must match headerHash",
    );
  }
  const availabilityCommitmentCbor = requireString(
    record.availabilityCommitmentCbor,
    "DA signature record V1.availabilityCommitmentCbor",
  );
  const availabilityCommitment = SDK.parseDaAvailabilityCommitmentCbor(
    availabilityCommitmentCbor,
  );
  const availabilityCommitmentDigest = requireLowerHex(
    record.availabilityCommitmentDigest,
    32,
    "DA signature record V1.availabilityCommitmentDigest",
  );
  const computedCommitmentDigest = computeDaSha256Hash(
    Buffer.from(availabilityCommitmentCbor, "hex"),
  ).toString("hex");
  if (
    availabilityCommitment.header_hash !== headerHash ||
    availabilityCommitmentDigest !== computedCommitmentDigest
  ) {
    throw new Error(
      "DA signature record V1 commitment header/digest does not match its outer identity",
    );
  }
  return {
    deploymentFingerprint: requireString(
      record.deploymentFingerprint,
      "DA signature record V1.deploymentFingerprint",
    ),
    headerHash,
    signerIndex: requireUint8(
      record.signerIndex,
      "DA signature record V1.signerIndex",
    ),
    signatureWitness: requireString(
      record.signatureWitness,
      "DA signature record V1.signatureWitness",
    ),
    availabilityCommitmentCbor,
    availabilityCommitmentDigest,
    payloadHash: requireString(
      record.payloadHash,
      "DA signature record V1.payloadHash",
    ),
    committeeSignersHash: requireString(
      record.committeeSignersHash,
      "DA signature record V1.committeeSignersHash",
    ),
    signedAt: requireString(record.signedAt, "DA signature record V1.signedAt"),
    broadcastStatus: requireEnum(
      record.broadcastStatus,
      signatureBroadcastStatuses,
      "DA signature record V1.broadcastStatus",
    ),
    source: requireEnum(
      record.source,
      signatureSources,
      "DA signature record V1.source",
    ),
    ...optionalStringProperty(
      record,
      "sourcePeer",
      "DA signature record V1.sourcePeer",
    ),
    ...optionalStringProperty(
      record,
      "receivedAt",
      "DA signature record V1.receivedAt",
    ),
    ...optionalStringProperty(
      record,
      "verifiedAt",
      "DA signature record V1.verifiedAt",
    ),
    l1ChainPoint: parseChainPoint(record.l1ChainPoint),
    validation,
  };
};

const parsePayloadCountSet = (value: unknown): DaStoredPayloadCountSet => {
  const record = requireExactObject(
    value,
    payloadCountKeys,
    [],
    "DA payload count set",
  );
  return {
    withdrawalCount: requireNonNegativeBigInt(
      record.withdrawalCount,
      "DA payload count set.withdrawalCount",
    ),
    forcedTransactionCount: requireNonNegativeBigInt(
      record.forcedTransactionCount,
      "DA payload count set.forcedTransactionCount",
    ),
    l2TransactionCount: requireNonNegativeBigInt(
      record.l2TransactionCount,
      "DA payload count set.l2TransactionCount",
    ),
    depositCount: requireNonNegativeBigInt(
      record.depositCount,
      "DA payload count set.depositCount",
    ),
    totalEventCount: requireNonNegativeBigInt(
      record.totalEventCount,
      "DA payload count set.totalEventCount",
    ),
    transitionStepCount: requireNonNegativeBigInt(
      record.transitionStepCount,
      "DA payload count set.transitionStepCount",
    ),
    validationTraceCount: requireNonNegativeBigInt(
      record.validationTraceCount,
      "DA payload count set.validationTraceCount",
    ),
  };
};

const parseValidationSummary = (value: unknown): DaStoredValidationSummary => {
  const record = requireExactObject(
    value,
    validationSummaryKeys,
    [],
    "DA validation summary",
  );
  const l1Header = requireExactObject(
    record.l1Header,
    l1HeaderKeys,
    [],
    "DA validation summary.l1Header",
  );
  if (record.payloadVersion !== 1) {
    throw new Error("DA validation summary.payloadVersion must be exactly 1");
  }
  return {
    payloadVersion: 1,
    rootsMatch: requireBoolean(
      record.rootsMatch,
      "DA validation summary.rootsMatch",
    ),
    stateQueueOutRef: requireString(
      record.stateQueueOutRef,
      "DA validation summary.stateQueueOutRef",
    ),
    headerHash: requireString(
      record.headerHash,
      "DA validation summary.headerHash",
    ),
    rootSummary: parsePayloadRootSet(record.rootSummary),
    countSummary: parsePayloadCountSet(record.countSummary),
    l1Header: {
      startTime: requireString(
        l1Header.startTime,
        "DA validation summary.l1Header.startTime",
      ),
      endTime: requireString(
        l1Header.endTime,
        "DA validation summary.l1Header.endTime",
      ),
      operatorVkey: requireString(
        l1Header.operatorVkey,
        "DA validation summary.l1Header.operatorVkey",
      ),
      prevHeaderHash: requireString(
        l1Header.prevHeaderHash,
        "DA validation summary.l1Header.prevHeaderHash",
      ),
      protocolVersion: requireString(
        l1Header.protocolVersion,
        "DA validation summary.l1Header.protocolVersion",
      ),
    },
  };
};

const parseChainPoint = (value: unknown): ChainPoint => {
  const record = requireExactObject(value, [], chainPointKeys, "chain point");
  return {
    ...optionalNonNegativeSafeIntegerProperty(
      record,
      "slot",
      "chain point.slot",
    ),
    ...optionalStringProperty(record, "blockHash", "chain point.blockHash"),
    ...optionalNonNegativeSafeIntegerProperty(
      record,
      "blockHeight",
      "chain point.blockHeight",
    ),
    ...optionalStringProperty(record, "observedAt", "chain point.observedAt"),
    ...optionalNonNegativeSafeIntegerProperty(
      record,
      "depth",
      "chain point.depth",
    ),
    ...optionalBooleanProperty(record, "finalized", "chain point.finalized"),
    ...optionalStringProperty(
      record,
      "providerSource",
      "chain point.providerSource",
    ),
  };
};

export const requireNonEmptyString = (
  value: unknown,
  label: string,
): string => {
  const result = requireString(value, label);
  if (result.length === 0) {
    throw new Error(`${label} must not be empty`);
  }
  return result;
};

export const requireLowerHex = (
  value: unknown,
  byteLength: number | undefined,
  label: string,
): string => {
  const result = requireString(value, label);
  const exactLength = byteLength === undefined ? result.length : byteLength * 2;
  if (
    result.length !== exactLength ||
    result.length % 2 !== 0 ||
    !/^[0-9a-f]*$/u.test(result)
  ) {
    throw new Error(
      byteLength === undefined
        ? `${label} must be lowercase even-length hex`
        : `${label} must be ${byteLength.toString()} bytes of lowercase hex`,
    );
  }
  return result;
};

const requireBoolean = (value: unknown, label: string): boolean => {
  if (typeof value !== "boolean") {
    throw new Error(`${label} must be a boolean`);
  }
  return value;
};

const requireNonNegativeSafeInteger = (
  value: unknown,
  label: string,
): number => {
  if (typeof value !== "number" || !Number.isSafeInteger(value) || value < 0) {
    throw new Error(`${label} must be a non-negative safe integer`);
  }
  return value;
};

export const requireUint8 = (value: unknown, label: string): number => {
  const integer = requireNonNegativeSafeInteger(value, label);
  if (integer > 255) {
    throw new Error(`${label} must be at most 255`);
  }
  return integer;
};

const optionalBooleanProperty = <K extends string>(
  record: Record<string, unknown>,
  key: K,
  label: string,
): Readonly<Partial<Record<K, boolean>>> => {
  if (record[key] === undefined) {
    return {} as Partial<Record<K, boolean>>;
  }
  return { [key]: requireBoolean(record[key], label) } as Record<K, boolean>;
};

const optionalNonNegativeSafeIntegerProperty = <K extends string>(
  record: Record<string, unknown>,
  key: K,
  label: string,
): Readonly<Partial<Record<K, number>>> => {
  if (record[key] === undefined) {
    return {} as Partial<Record<K, number>>;
  }
  return {
    [key]: requireNonNegativeSafeInteger(record[key], label),
  } as Record<K, number>;
};
