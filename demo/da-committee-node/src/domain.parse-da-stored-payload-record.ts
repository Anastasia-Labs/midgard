import {
  type DaStoredPayloadRecord,
  type DaStoredPayloadRootSet,
  payloadRecordOptionalKeys,
  payloadRecordRequiredKeys,
  payloadRootKeys,
} from "./domain.da-payload-record.js";

export const validationSummaryKeys = [
  "payloadVersion",
  "rootsMatch",
  "stateQueueOutRef",
  "headerHash",
  "rootSummary",
  "countSummary",
  "l1Header",
] as const;

export const payloadCountKeys = [
  "withdrawalCount",
  "forcedTransactionCount",
  "l2TransactionCount",
  "depositCount",
  "totalEventCount",
  "transitionStepCount",
  "validationTraceCount",
] as const;

export const l1HeaderKeys = [
  "startTime",
  "endTime",
  "operatorVkey",
  "prevHeaderHash",
  "protocolVersion",
] as const;

export const signatureRecordRequiredKeys = [
  "deploymentFingerprint",
  "headerHash",
  "signerIndex",
  "signatureWitness",
  "availabilityCommitmentCbor",
  "availabilityCommitmentDigest",
  "payloadHash",
  "committeeSignersHash",
  "signedAt",
  "broadcastStatus",
  "source",
  "l1ChainPoint",
  "validation",
] as const;

export const signatureRecordOptionalKeys = [
  "sourcePeer",
  "receivedAt",
  "verifiedAt",
] as const;

export const conflictEvidenceRecordKeys = [
  "conflictSchemaVersion",
  "deploymentFingerprint",
  "headerHash",
  "commitmentDigest",
  "conflictingHeaderHash",
  "conflictingCommitmentDigest",
  "signerIndex",
  "evidenceKind",
  "evidenceHash",
  "compactEvidenceCborHex",
  "reporterPeerId",
  "receivedAt",
] as const;

const payloadFetchStatuses = [
  "not_attempted",
  "missing_da",
  "available",
  "fetch_failed",
] as const;

const payloadValidationStatuses = [
  "fetched",
  "verified",
  "missing_da",
  "malformed_da",
  "root_mismatch",
  "conflicted",
] as const;

const payloadConflictStatuses = ["none", "conflicting_bytes"] as const;

export const signatureBroadcastStatuses = [
  "local",
  "posted",
  "post_failed",
] as const;

export const signatureSources = ["local", "peer"] as const;

export const parseDaStoredPayloadRecord = (
  value: unknown,
): DaStoredPayloadRecord => {
  const record = requireExactObject(
    value,
    payloadRecordRequiredKeys,
    payloadRecordOptionalKeys,
    "DA stored payload record V1",
  );
  if (record.payloadSchemaVersion !== 1) {
    throw new Error(
      "DA stored payload record V1.payloadSchemaVersion must be exactly 1",
    );
  }
  return {
    deploymentFingerprint: requireString(
      record.deploymentFingerprint,
      "DA stored payload record V1.deploymentFingerprint",
    ),
    headerHash: requireString(
      record.headerHash,
      "DA stored payload record V1.headerHash",
    ),
    payloadSchemaVersion: 1,
    payloadCborHex: requireString(
      record.payloadCborHex,
      "DA stored payload record V1.payloadCborHex",
    ),
    payloadSha256: requireString(
      record.payloadSha256,
      "DA stored payload record V1.payloadSha256",
    ),
    sourcePeerId: requireString(
      record.sourcePeerId,
      "DA stored payload record V1.sourcePeerId",
    ),
    fetchedAt: requireString(
      record.fetchedAt,
      "DA stored payload record V1.fetchedAt",
    ),
    ...optionalEnumProperty(
      record,
      "payloadFetchStatus",
      payloadFetchStatuses,
      "DA stored payload record V1.payloadFetchStatus",
    ),
    ...optionalStringProperty(
      record,
      "verifiedAt",
      "DA stored payload record V1.verifiedAt",
    ),
    ...(record.rootSummary === undefined
      ? {}
      : { rootSummary: parsePayloadRootSet(record.rootSummary) }),
    validationStatus: requireEnum(
      record.validationStatus,
      payloadValidationStatuses,
      "DA stored payload record V1.validationStatus",
    ),
    ...optionalEnumProperty(
      record,
      "conflictStatus",
      payloadConflictStatuses,
      "DA stored payload record V1.conflictStatus",
    ),
    ...optionalStringProperty(
      record,
      "validationError",
      "DA stored payload record V1.validationError",
    ),
  };
};

export const parsePayloadRootSet = (value: unknown): DaStoredPayloadRootSet => {
  const record = requireExactObject(
    value,
    payloadRootKeys,
    [],
    "DA payload root set",
  );
  return {
    utxosRoot: requireString(record.utxosRoot, "DA payload root set.utxosRoot"),
    withdrawalsRoot: requireString(
      record.withdrawalsRoot,
      "DA payload root set.withdrawalsRoot",
    ),
    forcedTransactionsRoot: requireString(
      record.forcedTransactionsRoot,
      "DA payload root set.forcedTransactionsRoot",
    ),
    transactionsRoot: requireString(
      record.transactionsRoot,
      "DA payload root set.transactionsRoot",
    ),
    depositsRoot: requireString(
      record.depositsRoot,
      "DA payload root set.depositsRoot",
    ),
    transitionTraceRoot: requireString(
      record.transitionTraceRoot,
      "DA payload root set.transitionTraceRoot",
    ),
    eventToStepRoot: requireString(
      record.eventToStepRoot,
      "DA payload root set.eventToStepRoot",
    ),
    validationTracesRoot: requireString(
      record.validationTracesRoot,
      "DA payload root set.validationTracesRoot",
    ),
  };
};

export const requireExactObject = (
  value: unknown,
  requiredKeys: readonly string[],
  optionalKeys: readonly string[],
  label: string,
): Record<string, unknown> => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    throw new Error(`${label} must be an object`);
  }
  const record = value as Record<string, unknown>;
  const allowedKeys = new Set([...requiredKeys, ...optionalKeys]);
  for (const key of Object.keys(record)) {
    if (!allowedKeys.has(key)) {
      throw new Error(`${label} contains unknown field ${key}`);
    }
  }
  for (const key of requiredKeys) {
    if (!Object.hasOwn(record, key)) {
      throw new Error(`${label} is missing required field ${key}`);
    }
  }
  return record;
};

export const requireString = (value: unknown, label: string): string => {
  if (typeof value !== "string") {
    throw new Error(`${label} must be a string`);
  }
  return value;
};

export const requireNonNegativeBigInt = (
  value: unknown,
  label: string,
): bigint => {
  if (typeof value !== "bigint" || value < 0n) {
    throw new Error(`${label} must be a non-negative bigint`);
  }
  return value;
};

export const requireEnum = <T extends string>(
  value: unknown,
  values: readonly T[],
  label: string,
): T => {
  if (typeof value !== "string" || !values.includes(value as T)) {
    throw new Error(`${label} must be one of ${values.join(", ")}`);
  }
  return value as T;
};

export const optionalStringProperty = <K extends string>(
  record: Record<string, unknown>,
  key: K,
  label: string,
): Readonly<Partial<Record<K, string>>> => {
  if (record[key] === undefined) {
    return {} as Partial<Record<K, string>>;
  }
  return { [key]: requireString(record[key], label) } as Record<K, string>;
};

const optionalEnumProperty = <K extends string, T extends string>(
  record: Record<string, unknown>,
  key: K,
  values: readonly T[],
  label: string,
): Readonly<Partial<Record<K, T>>> => {
  if (record[key] === undefined) {
    return {} as Partial<Record<K, T>>;
  }
  return { [key]: requireEnum(record[key], values, label) } as Record<K, T>;
};
