import {
  computeDaSha256Hash,
  decodeDaConflictingSignatureHeaderEvidenceV1Cbor,
  encodeDaConflictingSignatureHeaderEvidenceV1Cbor,
} from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";

export type ChainPoint = {
  readonly slot?: number;
  readonly blockHash?: string;
  readonly blockHeight?: number;
  readonly observedAt?: string;
  readonly depth?: number;
  readonly finalized?: boolean;
  readonly providerSource?: string;
};

export type HeaderV1 = SDK.HeaderV1;

export type ObservedStateQueueNode = {
  readonly outRef: string;
  readonly assetName: string;
  readonly linkedListKey: string | "Empty";
  readonly rawDatumCbor?: string;
  readonly header: HeaderV1;
  readonly daAttestation: SDK.DaAvailabilityStateQueueStatusV1;
  readonly chainPoint: ChainPoint;
};

/**
 * One atomically observed state-queue view. The confirmed root is retained in
 * the observation so disappearance of a block node can only become a terminal
 * outcome when the root observation itself is final.
 */
export type ObservedStateQueueSnapshotV1 = {
  readonly nodes: readonly ObservedStateQueueNode[];
  readonly confirmedHeaderHash: string;
  readonly confirmedStateOutRef: string;
  readonly observedChainPoint: ChainPoint;
};

export type StateQueueHeaderStatus =
  | "unattested"
  | "attesting"
  | "attested"
  | "merged"
  | "removed"
  | "conflicted";

export type StateQueueHeaderRecord = {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly stateQueueOutRef: string;
  readonly blockAssetName: string;
  readonly rawStateQueueDatumCbor?: string;
  readonly header: HeaderV1;
  readonly computedHeaderHash: string;
  readonly daAttestation: SDK.DaAvailabilityStateQueueStatusV1;
  readonly observedChainPoint: ChainPoint;
  readonly finalized: boolean;
  readonly status: StateQueueHeaderStatus;
  readonly validationErrors: readonly string[];
  readonly updatedAt: string;
};

export type DaPayloadRecord = {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly payloadSchemaVersion: 1;
  readonly payloadCborHex: string;
  readonly payloadSha256: string;
  readonly sourcePeerId: string;
  readonly fetchedAt: string;
  readonly payloadFetchStatus?:
    | "not_attempted"
    | "missing_da"
    | "available"
    | "fetch_failed";
  readonly verifiedAt?: string;
  readonly rootSummary?: PayloadRootSet;
  readonly validationStatus:
    | "fetched"
    | "verified"
    | "missing_da"
    | "malformed_da"
    | "root_mismatch"
    | "conflicted";
  readonly conflictStatus?: "none" | "conflicting_bytes";
  readonly validationError?: string;
};

export type DaStoredPayloadRecordV1 = Omit<DaPayloadRecord, "rootSummary"> & {
  readonly rootSummary?: DaStoredPayloadRootSetV1;
};

export type PayloadRootSet = {
  readonly utxosRoot: string;
  readonly withdrawalsRoot: string;
  readonly forcedTransactionsRoot: string;
  readonly transactionsRoot: string;
  readonly depositsRoot: string;
  readonly transitionTraceRoot: string;
  readonly eventToStepRoot: string;
};

export type PayloadCountSet = {
  readonly withdrawalCount: bigint;
  readonly forcedTransactionCount: bigint;
  readonly l2TransactionCount: bigint;
  readonly depositCount: bigint;
  readonly totalEventCount: bigint;
  readonly transitionStepCount: bigint;
};

export type ValidationSummary = {
  readonly payloadVersion: number;
  readonly rootsMatch: boolean;
  readonly stateQueueOutRef: string;
  readonly headerHash: string;
  readonly rootSummary: PayloadRootSet;
  readonly countSummary: PayloadCountSet;
  readonly l1Header: {
    readonly startTime: string;
    readonly endTime: string;
    readonly operatorVkey: string;
    readonly prevHeaderHash: string;
    readonly protocolVersion: string;
  };
};

export type DaStoredPayloadRootSetV1 = PayloadRootSet & {
  readonly validationTracesRoot: string;
};

export type DaStoredPayloadCountSetV1 = PayloadCountSet & {
  readonly validationTraceCount: bigint;
};

export type DaStoredValidationSummaryV1 = Omit<
  ValidationSummary,
  "rootSummary" | "countSummary"
> & {
  readonly rootSummary: DaStoredPayloadRootSetV1;
  readonly countSummary: DaStoredPayloadCountSetV1;
};

export type DaSignatureRecord = {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly signerIndex: number;
  readonly signatureWitness: string;
  readonly availabilityCommitmentCbor: string;
  readonly availabilityCommitmentDigest: string;
  readonly payloadHash: string;
  readonly committeeSignersHash: string;
  readonly signedAt: string;
  readonly broadcastStatus: "local" | "posted" | "post_failed";
  readonly source?: "local" | "peer";
  readonly sourcePeer?: string;
  readonly receivedAt?: string;
  readonly verifiedAt?: string;
  readonly l1ChainPoint: ChainPoint;
  readonly validation: ValidationSummary;
};

export type DaSignatureRecordV1 = Omit<
  DaSignatureRecord,
  "source" | "validation"
> & {
  readonly source: "local" | "peer";
  readonly validation: DaStoredValidationSummaryV1;
};

export type DaStoredConflictEvidenceRecordV1 = {
  readonly conflictSchemaVersion: 1;
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly commitmentDigest: string;
  readonly conflictingHeaderHash: string;
  readonly conflictingCommitmentDigest: string;
  readonly signerIndex: number;
  readonly evidenceKind: "equivocation";
  readonly evidenceHash: string;
  readonly compactEvidenceCborHex: string;
  readonly reporterPeerId: string;
  readonly receivedAt: string;
};

export type DaAttestationCandidateRecord = {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly outRef: string;
  readonly datumCbor: string;
  readonly attestationCount: number;
  readonly threshold: number;
  readonly committeeSignersHash: string;
  readonly bitmap: string;
  readonly observedChainPoint: ChainPoint;
  readonly status: "initialized" | "signed" | "threshold" | "burned" | "stale";
};

export type L1SubmissionRecord = {
  readonly deploymentFingerprint: string;
  readonly headerHash: string;
  readonly txKind: "init" | "add_signatures" | "apply";
  readonly txHash: string;
  readonly inputsUsed: readonly string[];
  readonly submittedAt: string;
  readonly confirmedAt?: string;
  readonly resultStatus: "submitted" | "confirmed" | "failed";
  readonly failureCause?: string;
};

export type DaCommitteeMember = {
  readonly index: number;
  readonly vkey: string;
  readonly canSubmitL1: boolean;
};

export type DaPeerBroadcastRecord = {
  readonly deploymentFingerprint: string;
  readonly peerId: string;
  readonly headerHash: string;
  readonly availabilityCommitmentDigest: string;
  readonly signerIndex: number;
  readonly status: "pending" | "posted" | "failed";
  readonly attempts: number;
  readonly nextAttemptAt?: string;
  readonly lastAttemptAt?: string;
  readonly lastSuccessAt?: string;
  readonly lastError?: string;
  readonly updatedAt: string;
};

export type DaPeerHealthRecord = {
  readonly peerId: string;
  readonly signerIndex?: number;
  readonly lastSuccessAt?: string;
  readonly lastFailureAt?: string;
  readonly lastError?: string;
  readonly consecutiveFailures: number;
  readonly updatedAt: string;
};

export type DaPeerNonceRecord = {
  readonly deploymentFingerprint: string;
  readonly signerIndex: number;
  readonly nonce: string;
  readonly timestampMs: number;
  readonly receivedAt: string;
};

const payloadRootKeys = [
  "utxosRoot",
  "withdrawalsRoot",
  "forcedTransactionsRoot",
  "transactionsRoot",
  "depositsRoot",
  "transitionTraceRoot",
  "eventToStepRoot",
  "validationTracesRoot",
] as const;

const chainPointKeys = [
  "slot",
  "blockHash",
  "blockHeight",
  "observedAt",
  "depth",
  "finalized",
  "providerSource",
] as const;

const payloadRecordRequiredKeys = [
  "deploymentFingerprint",
  "headerHash",
  "payloadSchemaVersion",
  "payloadCborHex",
  "payloadSha256",
  "sourcePeerId",
  "fetchedAt",
  "validationStatus",
] as const;

const payloadRecordOptionalKeys = [
  "payloadFetchStatus",
  "verifiedAt",
  "rootSummary",
  "conflictStatus",
  "validationError",
] as const;

const validationSummaryKeys = [
  "payloadVersion",
  "rootsMatch",
  "stateQueueOutRef",
  "headerHash",
  "rootSummary",
  "countSummary",
  "l1Header",
] as const;

const payloadCountKeys = [
  "withdrawalCount",
  "forcedTransactionCount",
  "l2TransactionCount",
  "depositCount",
  "totalEventCount",
  "transitionStepCount",
  "validationTraceCount",
] as const;

const l1HeaderKeys = [
  "startTime",
  "endTime",
  "operatorVkey",
  "prevHeaderHash",
  "protocolVersion",
] as const;

const signatureRecordRequiredKeys = [
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

const signatureRecordOptionalKeys = [
  "sourcePeer",
  "receivedAt",
  "verifiedAt",
] as const;

const conflictEvidenceRecordKeys = [
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

const signatureBroadcastStatuses = ["local", "posted", "post_failed"] as const;

const signatureSources = ["local", "peer"] as const;

export const parseDaStoredPayloadRecordV1 = (
  value: unknown,
): DaStoredPayloadRecordV1 => {
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

export const parseDaSignatureRecordV1 = (
  value: unknown,
): DaSignatureRecordV1 => {
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
  const availabilityCommitment = SDK.parseDaAvailabilityCommitmentV1Cbor(
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

export const parseDaStoredConflictEvidenceRecordV1 = (
  value: unknown,
): DaStoredConflictEvidenceRecordV1 => {
  const record = requireExactObject(
    value,
    conflictEvidenceRecordKeys,
    [],
    "DA stored conflict evidence record V1",
  );
  if (record.conflictSchemaVersion !== 1) {
    throw new Error(
      "DA stored conflict evidence record V1.conflictSchemaVersion must be exactly 1",
    );
  }
  if (record.evidenceKind !== "equivocation") {
    throw new Error(
      "DA stored conflict evidence record V1.evidenceKind must be equivocation",
    );
  }
  const deploymentFingerprint = requireLowerHex(
    record.deploymentFingerprint,
    32,
    "DA stored conflict evidence record V1.deploymentFingerprint",
  );
  const headerHash = requireLowerHex(
    record.headerHash,
    28,
    "DA stored conflict evidence record V1.headerHash",
  );
  const conflictingHeaderHash = requireLowerHex(
    record.conflictingHeaderHash,
    28,
    "DA stored conflict evidence record V1.conflictingHeaderHash",
  );
  const commitmentDigest = requireLowerHex(
    record.commitmentDigest,
    32,
    "DA stored conflict evidence record V1.commitmentDigest",
  );
  const conflictingCommitmentDigest = requireLowerHex(
    record.conflictingCommitmentDigest,
    32,
    "DA stored conflict evidence record V1.conflictingCommitmentDigest",
  );
  const signerIndex = requireUint8(
    record.signerIndex,
    "DA stored conflict evidence record V1.signerIndex",
  );
  const evidenceHash = requireLowerHex(
    record.evidenceHash,
    32,
    "DA stored conflict evidence record V1.evidenceHash",
  );
  const compactEvidenceCborHex = requireLowerHex(
    record.compactEvidenceCborHex,
    undefined,
    "DA stored conflict evidence record V1.compactEvidenceCborHex",
  );
  if (compactEvidenceCborHex.length === 0) {
    throw new Error(
      "DA stored conflict evidence record V1.compactEvidenceCborHex must not be empty",
    );
  }
  const compactEvidence = Buffer.from(compactEvidenceCborHex, "hex");
  const decoded =
    decodeDaConflictingSignatureHeaderEvidenceV1Cbor(compactEvidence);
  if (
    !encodeDaConflictingSignatureHeaderEvidenceV1Cbor(decoded).equals(
      compactEvidence,
    )
  ) {
    throw new Error(
      "DA stored conflict evidence record V1.compactEvidenceCborHex must be canonical CBOR",
    );
  }
  const lowerCommitment = SDK.parseDaAvailabilityCommitmentV1Cbor(
    decoded.lowerCommitmentCbor.toString("hex"),
  );
  const upperCommitment = SDK.parseDaAvailabilityCommitmentV1Cbor(
    decoded.upperCommitmentCbor.toString("hex"),
  );
  const decodedCommitmentDigest = computeDaSha256Hash(
    decoded.lowerCommitmentCbor,
  ).toString("hex");
  const decodedConflictingCommitmentDigest = computeDaSha256Hash(
    decoded.upperCommitmentCbor,
  ).toString("hex");
  if (
    decoded.lowerHeaderHash.toString("hex") !== headerHash ||
    lowerCommitment.header_hash !== headerHash ||
    decodedCommitmentDigest !== commitmentDigest ||
    decoded.upperHeaderHash.toString("hex") !== conflictingHeaderHash ||
    upperCommitment.header_hash !== conflictingHeaderHash ||
    decodedConflictingCommitmentDigest !== conflictingCommitmentDigest ||
    decoded.signerIndex !== signerIndex
  ) {
    throw new Error(
      "DA stored conflict evidence record V1 derived conflict identity does not match compact evidence",
    );
  }
  if (
    `${headerHash}${commitmentDigest}`.localeCompare(
      `${conflictingHeaderHash}${conflictingCommitmentDigest}`,
    ) >= 0
  ) {
    throw new Error(
      "DA stored conflict evidence record V1 composite identities must be strictly ordered",
    );
  }
  if (computeDaSha256Hash(compactEvidence).toString("hex") !== evidenceHash) {
    throw new Error(
      "DA stored conflict evidence record V1.evidenceHash does not match compact evidence",
    );
  }
  return {
    conflictSchemaVersion: 1,
    deploymentFingerprint,
    headerHash,
    commitmentDigest,
    conflictingHeaderHash,
    conflictingCommitmentDigest,
    signerIndex,
    evidenceKind: "equivocation",
    evidenceHash,
    compactEvidenceCborHex,
    reporterPeerId: requireNonEmptyString(
      record.reporterPeerId,
      "DA stored conflict evidence record V1.reporterPeerId",
    ),
    receivedAt: requireCanonicalIsoTimestamp(
      record.receivedAt,
      "DA stored conflict evidence record V1.receivedAt",
    ),
  };
};

const parsePayloadRootSet = (value: unknown): DaStoredPayloadRootSetV1 => {
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

const parsePayloadCountSet = (value: unknown): DaStoredPayloadCountSetV1 => {
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

const parseValidationSummary = (
  value: unknown,
): DaStoredValidationSummaryV1 => {
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

const requireExactObject = (
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

const requireString = (value: unknown, label: string): string => {
  if (typeof value !== "string") {
    throw new Error(`${label} must be a string`);
  }
  return value;
};

const requireNonEmptyString = (value: unknown, label: string): string => {
  const result = requireString(value, label);
  if (result.length === 0) {
    throw new Error(`${label} must not be empty`);
  }
  return result;
};

const requireLowerHex = (
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

const requireCanonicalIsoTimestamp = (
  value: unknown,
  label: string,
): string => {
  const result = requireString(value, label);
  let canonical: string;
  try {
    canonical = new Date(result).toISOString();
  } catch {
    throw new Error(`${label} must be a canonical ISO timestamp`);
  }
  if (canonical !== result) {
    throw new Error(`${label} must be a canonical ISO timestamp`);
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

const requireUint8 = (value: unknown, label: string): number => {
  const integer = requireNonNegativeSafeInteger(value, label);
  if (integer > 255) {
    throw new Error(`${label} must be at most 255`);
  }
  return integer;
};

const requireNonNegativeBigInt = (value: unknown, label: string): bigint => {
  if (typeof value !== "bigint" || value < 0n) {
    throw new Error(`${label} must be a non-negative bigint`);
  }
  return value;
};

const requireEnum = <T extends string>(
  value: unknown,
  values: readonly T[],
  label: string,
): T => {
  if (typeof value !== "string" || !values.includes(value as T)) {
    throw new Error(`${label} must be one of ${values.join(", ")}`);
  }
  return value as T;
};

const optionalStringProperty = <K extends string>(
  record: Record<string, unknown>,
  key: K,
  label: string,
): Readonly<Partial<Record<K, string>>> => {
  if (record[key] === undefined) {
    return {} as Partial<Record<K, string>>;
  }
  return { [key]: requireString(record[key], label) } as Record<K, string>;
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
