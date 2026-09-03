import { createHash } from "node:crypto";
import { isProxy } from "node:util/types";

import {
  type DeploymentMarker,
  parseDeploymentMarker,
} from "@al-ft/midgard-core/deployment-manifest-identity-v1";

export const WATCHER_DURABLE_STORE_SCHEMA_VERSION =
  "midgard-watcher-durable-store-v1" as const;
export const WATCHER_DURABLE_CACHE_SCHEMA_VERSION =
  "midgard-watcher-durable-cache-v1" as const;
export const WATCHER_DURABLE_MIGRATION_VERSION = 1 as const;

const MIGRATION_NAME =
  "fresh_install_canonical_v1_spent_protocol_utxo_journal" as const;
const HEX_32 = /^[0-9a-f]{64}$/u;
const OUT_REF = /^[0-9a-f]{64}#(?:0|[1-9][0-9]*)$/u;
const CANONICAL_NATURAL = /^(?:0|[1-9][0-9]*)$/u;
const CANONICAL_POSITIVE = /^[1-9][0-9]*$/u;
const STABLE_NAME = /^[a-z][a-z0-9]*(?:[-_.][a-z0-9]+)*$/u;
const PROVIDER_ID = /^[a-z][a-z0-9-]{0,62}$/u;
const LOWER_HEX_BYTES = /^(?:[0-9a-f]{2})+$/u;

const sha256Utf8 = (value: string): string =>
  createHash("sha256").update(value, "utf8").digest("hex");

const sha256Bytes = (value: Uint8Array): string =>
  createHash("sha256").update(value).digest("hex");

export const WATCHER_DURABLE_MIGRATION_MANIFEST_SHA256 = sha256Utf8(
  `${WATCHER_DURABLE_MIGRATION_VERSION}:${MIGRATION_NAME}:${WATCHER_DURABLE_STORE_SCHEMA_VERSION}`,
);

export type WatcherDurableStoreErrorCode =
  | "broken_reference"
  | "cache_mismatch"
  | "deployment_marker_mismatch"
  | "duplicate_key"
  | "integrity_mismatch"
  | "invalid_encoding"
  | "invalid_field"
  | "migration_conflict"
  | "missing_field"
  | "noncanonical_encoding"
  | "persistence_failure"
  | "unknown_field"
  | "unsupported_schema"
  | "unsorted_records";

export class WatcherDurableStoreError extends Error {
  readonly code: WatcherDurableStoreErrorCode;
  readonly path: string;

  constructor(code: WatcherDurableStoreErrorCode, path: string) {
    super(`Watcher durable store rejected: ${code} at ${path}`);
    this.name = "WatcherDurableStoreError";
    this.code = code;
    this.path = path;
  }
}

const fail = (code: WatcherDurableStoreErrorCode, path: string): never => {
  throw new WatcherDurableStoreError(code, path);
};

type JsonRecord = Record<string, unknown>;
type CanonicalJson =
  | null
  | boolean
  | number
  | string
  | readonly CanonicalJson[]
  | { readonly [key: string]: CanonicalJson };

const plainRecord = (value: unknown, path: string): JsonRecord => {
  if (typeof value !== "object" || value === null || Array.isArray(value)) {
    fail("invalid_field", path);
  }
  const candidate = value as object;
  const prototype = Object.getPrototypeOf(candidate);
  if (prototype !== Object.prototype && prototype !== null) {
    fail("invalid_field", path);
  }
  if (Reflect.ownKeys(candidate).length !== Object.keys(candidate).length) {
    fail("invalid_field", path);
  }
  return value as JsonRecord;
};

const exactRecord = (
  value: unknown,
  path: string,
  requiredKeys: readonly string[],
): JsonRecord => {
  const record = plainRecord(value, path);
  const required = new Set(requiredKeys);
  for (const key of Object.keys(record)) {
    if (!required.has(key)) {
      fail("unknown_field", `${path}.${key}`);
    }
  }
  for (const key of requiredKeys) {
    if (!Object.prototype.hasOwnProperty.call(record, key)) {
      fail("missing_field", `${path}.${key}`);
    }
  }
  return record;
};

const exactString = (value: unknown, path: string, pattern: RegExp): string => {
  if (typeof value !== "string") {
    fail("invalid_field", path);
  }
  const stringValue = value as string;
  if (!pattern.test(stringValue)) {
    fail("invalid_field", path);
  }
  return stringValue;
};

const exactLiteral = <T extends string>(
  value: unknown,
  path: string,
  allowed: readonly T[],
): T => {
  if (typeof value !== "string" || !allowed.includes(value as T)) {
    fail("invalid_field", path);
  }
  return value as T;
};

const canonicalJson = (
  value: unknown,
  path = "$",
  ancestors = new WeakSet<object>(),
): string => {
  if (
    value === null ||
    typeof value === "boolean" ||
    typeof value === "string"
  ) {
    return JSON.stringify(value);
  }
  if (typeof value === "number") {
    if (!Number.isSafeInteger(value)) {
      fail("invalid_field", path);
    }
    return value.toString();
  }
  if (typeof value !== "object" || value === null) {
    fail("invalid_field", path);
  }
  const objectValue = value as object;
  if (isProxy(objectValue)) {
    fail("invalid_field", path);
  }
  if (ancestors.has(objectValue)) {
    fail("invalid_field", path);
  }
  ancestors.add(objectValue);
  let result: string;
  if (Array.isArray(value)) {
    if (
      Object.getPrototypeOf(value) !== Array.prototype ||
      Reflect.ownKeys(value).length !== value.length + 1 ||
      Reflect.ownKeys(value).some(
        (key) =>
          key !== "length" &&
          (typeof key !== "string" ||
            !/^(?:0|[1-9][0-9]*)$/u.test(key) ||
            Number(key) >= value.length),
      )
    ) {
      fail("invalid_field", path);
    }
    for (let index = 0; index < value.length; index += 1) {
      const descriptor = Object.getOwnPropertyDescriptor(
        value,
        index.toString(),
      );
      if (
        descriptor === undefined ||
        !descriptor.enumerable ||
        descriptor.get !== undefined ||
        descriptor.set !== undefined
      ) {
        fail("invalid_field", `${path}.${index}`);
      }
    }
    result = `[${value
      .map((member, index) =>
        canonicalJson(member, `${path}.${index}`, ancestors),
      )
      .join(",")}]`;
  } else {
    const record = plainRecord(value, path);
    for (const key of Object.keys(record)) {
      const descriptor = Object.getOwnPropertyDescriptor(record, key);
      if (
        descriptor === undefined ||
        !descriptor.enumerable ||
        descriptor.get !== undefined ||
        descriptor.set !== undefined
      ) {
        fail("invalid_field", `${path}.${key}`);
      }
    }
    result = `{${Object.keys(record)
      .sort()
      .map(
        (key) =>
          `${JSON.stringify(key)}:${canonicalJson(record[key], `${path}.${key}`, ancestors)}`,
      )
      .join(",")}}`;
  }
  ancestors.delete(objectValue);
  return result;
};

export const watcherCanonicalJson = (value: unknown): string =>
  canonicalJson(value);

export const watcherSha256CanonicalJson = (value: unknown): string =>
  sha256Utf8(watcherCanonicalJson(value));

export const watcherSameCanonicalJson = (
  left: unknown,
  right: unknown,
): boolean => {
  try {
    return watcherCanonicalJson(left) === watcherCanonicalJson(right);
  } catch {
    return false;
  }
};

export type WatcherDurablePayload = Readonly<{
  cborHex: string;
  sha256: string;
}>;

const parsePayload = (value: unknown, path: string): WatcherDurablePayload => {
  const payload = exactRecord(value, path, ["cborHex", "sha256"]);
  const cborHex = exactString(
    payload.cborHex,
    `${path}.cborHex`,
    LOWER_HEX_BYTES,
  );
  const digest = exactString(payload.sha256, `${path}.sha256`, HEX_32);
  if (sha256Bytes(Buffer.from(cborHex, "hex")) !== digest) {
    fail("integrity_mismatch", `${path}.sha256`);
  }
  return { cborHex, sha256: digest };
};

export const makeWatcherDurablePayload = (
  cborHex: string,
): WatcherDurablePayload => {
  if (!LOWER_HEX_BYTES.test(cborHex)) {
    fail("invalid_field", "$.cborHex");
  }
  return {
    cborHex,
    sha256: sha256Bytes(Buffer.from(cborHex, "hex")),
  };
};

export type WatcherL1ChainPoint = Readonly<{
  chainPointId: string;
  providerId: string;
  blockHash: string;
  slot: string;
  blockNo: string;
  depth: string;
}>;

export type WatcherL1Observation = Readonly<{
  observationId: string;
  providerId: string;
  chainPointId: string;
  payload: WatcherDurablePayload;
}>;

export const WATCHER_PROTOCOL_UTXO_ROLES = [
  "state_queue",
  "hub_oracle",
  "operator_directory",
  "deposit",
  "withdrawal",
  "forced_transaction",
  "reserve",
  "payout",
  "settlement",
  "proof_thread",
  "computation_thread",
] as const;

export type WatcherProtocolUtxo = Readonly<{
  outRef: string;
  role: (typeof WATCHER_PROTOCOL_UTXO_ROLES)[number];
  chainPointId: string;
  output: WatcherDurablePayload;
}>;

export type WatcherSpentProtocolUtxo = WatcherProtocolUtxo &
  Readonly<{
    spentAtChainPointId: string;
  }>;

export type WatcherDaProofInput = Readonly<{
  inputId: string;
  kind: "da_payload" | "proof_input";
  payload: WatcherDurablePayload;
}>;

export type WatcherReconstructedState = Readonly<{
  blockHash: string;
  chainPointId: string;
  priorStateRoot: string;
  postStateRoot: string;
  inputIds: readonly string[];
  state: WatcherDurablePayload;
}>;

export const WATCHER_BLOCK_DECISIONS = [
  "verified",
  "pending_da",
  "unprovable_gap",
  "fault_detected",
  "fault_proven",
  "removed_or_resolved",
] as const;

export type WatcherBlockDecision = Readonly<{
  blockHash: string;
  decision: (typeof WATCHER_BLOCK_DECISIONS)[number];
  reconstructionDigest: string;
  evidenceDigest: string;
}>;

export type WatcherFault = Readonly<{
  faultId: string;
  blockHash: string;
  familyId: string;
  evidence: WatcherDurablePayload;
}>;

export type WatcherSubmission = Readonly<{
  submissionId: string;
  faultId: string;
  txBodyHash: string;
  status: "prepared" | "submitted" | "ambiguous";
}>;

export type WatcherConfirmation = Readonly<{
  confirmationId: string;
  submissionId: string;
  txHash: string;
  chainPointId: string;
  depth: string;
  status: "observed" | "confirmed" | "rolled_back";
}>;

export type WatcherRetry = Readonly<{
  retryId: string;
  submissionId: string;
  attempt: string;
  nextEligibleSlot: string;
  reason:
    | "provider_unavailable"
    | "submission_ambiguous"
    | "confirmation_timeout"
    | "rollback"
    | "topology_changed";
}>;

export const WATCHER_DEADLINE_KINDS = [
  "da_fetch",
  "da_publication",
  "construction",
  "proof",
  "confirmation",
  "retry",
  "rollback",
  "removal",
  "maturity",
] as const;

export type WatcherDeadline = Readonly<{
  deadlineId: string;
  subjectKind: "fault" | "submission";
  subjectId: string;
  kind: (typeof WATCHER_DEADLINE_KINDS)[number];
  expiresAtSlot: string;
}>;

export type WatcherCorrectionResult = Readonly<{
  correctionId: string;
  faultId: string;
  confirmationId: string;
  outcome:
    | "removed"
    | "resolved"
    | "removed_and_slashed"
    | "removed_slashed_and_rewarded";
  finalStateRoot: string;
  slashLovelace: string;
  rewardLovelace: string;
}>;

type CacheNamespace =
  | "chain_points"
  | "confirmations"
  | "correction_results"
  | "da_proof_inputs"
  | "deadlines"
  | "decisions"
  | "faults"
  | "l1_observations"
  | "protocol_utxos"
  | "reconstructed_states"
  | "retries"
  | "spent_protocol_utxos"
  | "submissions";

export type WatcherDurableCacheEntry = Readonly<{
  namespace: CacheNamespace;
  key: string;
  index: string;
  recordSha256: string;
}>;

export type WatcherDurableCaches = Readonly<{
  schemaVersion: typeof WATCHER_DURABLE_CACHE_SCHEMA_VERSION;
  sourceSha256: string;
  entries: readonly WatcherDurableCacheEntry[];
}>;

export type WatcherDurableRecords = Readonly<{
  l1Observations: readonly WatcherL1Observation[];
  chainPoints: readonly WatcherL1ChainPoint[];
  protocolUtxos: readonly WatcherProtocolUtxo[];
  spentProtocolUtxos: readonly WatcherSpentProtocolUtxo[];
  daProofInputs: readonly WatcherDaProofInput[];
  reconstructedStates: readonly WatcherReconstructedState[];
  decisions: readonly WatcherBlockDecision[];
  faults: readonly WatcherFault[];
  submissions: readonly WatcherSubmission[];
  confirmations: readonly WatcherConfirmation[];
  retries: readonly WatcherRetry[];
  deadlines: readonly WatcherDeadline[];
  correctionResults: readonly WatcherCorrectionResult[];
}>;

export type WatcherDurableStore = WatcherDurableRecords &
  Readonly<{
    schemaVersion: typeof WATCHER_DURABLE_STORE_SCHEMA_VERSION;
    migrationVersion: typeof WATCHER_DURABLE_MIGRATION_VERSION;
    migrationManifestSha256: string;
    revision: string;
    deploymentMarker: DeploymentMarker;
    caches: WatcherDurableCaches;
  }>;

type RecordParser<T> = (value: unknown, path: string) => T;

const parseChainPoint: RecordParser<WatcherL1ChainPoint> = (value, path) => {
  const record = exactRecord(value, path, [
    "chainPointId",
    "providerId",
    "blockHash",
    "slot",
    "blockNo",
    "depth",
  ]);
  return {
    chainPointId: exactString(
      record.chainPointId,
      `${path}.chainPointId`,
      HEX_32,
    ),
    providerId: exactString(
      record.providerId,
      `${path}.providerId`,
      PROVIDER_ID,
    ),
    blockHash: exactString(record.blockHash, `${path}.blockHash`, HEX_32),
    slot: exactString(record.slot, `${path}.slot`, CANONICAL_NATURAL),
    blockNo: exactString(record.blockNo, `${path}.blockNo`, CANONICAL_NATURAL),
    depth: exactString(record.depth, `${path}.depth`, CANONICAL_NATURAL),
  };
};

const compareChainPointOrder = (
  left: Pick<WatcherL1ChainPoint, "blockNo" | "slot">,
  right: Pick<WatcherL1ChainPoint, "blockNo" | "slot">,
): number => {
  const leftBlockNo = BigInt(left.blockNo);
  const rightBlockNo = BigInt(right.blockNo);
  if (leftBlockNo < rightBlockNo) {
    return -1;
  }
  if (leftBlockNo > rightBlockNo) {
    return 1;
  }
  const leftSlot = BigInt(left.slot);
  const rightSlot = BigInt(right.slot);
  return leftSlot < rightSlot ? -1 : leftSlot > rightSlot ? 1 : 0;
};

const parseL1Observation: RecordParser<WatcherL1Observation> = (
  value,
  path,
) => {
  const record = exactRecord(value, path, [
    "observationId",
    "providerId",
    "chainPointId",
    "payload",
  ]);
  return {
    observationId: exactString(
      record.observationId,
      `${path}.observationId`,
      HEX_32,
    ),
    providerId: exactString(
      record.providerId,
      `${path}.providerId`,
      PROVIDER_ID,
    ),
    chainPointId: exactString(
      record.chainPointId,
      `${path}.chainPointId`,
      HEX_32,
    ),
    payload: parsePayload(record.payload, `${path}.payload`),
  };
};

const parseProtocolUtxo: RecordParser<WatcherProtocolUtxo> = (value, path) => {
  const record = exactRecord(value, path, [
    "outRef",
    "role",
    "chainPointId",
    "output",
  ]);
  return {
    outRef: exactString(record.outRef, `${path}.outRef`, OUT_REF),
    role: exactLiteral(
      record.role,
      `${path}.role`,
      WATCHER_PROTOCOL_UTXO_ROLES,
    ),
    chainPointId: exactString(
      record.chainPointId,
      `${path}.chainPointId`,
      HEX_32,
    ),
    output: parsePayload(record.output, `${path}.output`),
  };
};

const parseSpentProtocolUtxo: RecordParser<WatcherSpentProtocolUtxo> = (
  value,
  path,
) => {
  const record = exactRecord(value, path, [
    "outRef",
    "role",
    "chainPointId",
    "output",
    "spentAtChainPointId",
  ]);
  const active = parseProtocolUtxo(
    {
      outRef: record.outRef,
      role: record.role,
      chainPointId: record.chainPointId,
      output: record.output,
    },
    path,
  );
  return {
    ...active,
    spentAtChainPointId: exactString(
      record.spentAtChainPointId,
      `${path}.spentAtChainPointId`,
      HEX_32,
    ),
  };
};

const parseDaProofInput: RecordParser<WatcherDaProofInput> = (value, path) => {
  const record = exactRecord(value, path, ["inputId", "kind", "payload"]);
  return {
    inputId: exactString(record.inputId, `${path}.inputId`, HEX_32),
    kind: exactLiteral(record.kind, `${path}.kind`, [
      "da_payload",
      "proof_input",
    ]),
    payload: parsePayload(record.payload, `${path}.payload`),
  };
};

const parseSortedDigests = (
  value: unknown,
  path: string,
): readonly string[] => {
  if (!Array.isArray(value)) {
    fail("invalid_field", path);
  }
  const members = value as readonly unknown[];
  const digests = members.map((member, index) =>
    exactString(member, `${path}[${index.toString()}]`, HEX_32),
  );
  for (let index = 1; index < digests.length; index += 1) {
    const previous = digests[index - 1] as string;
    const current = digests[index] as string;
    if (current === previous) {
      fail("duplicate_key", `${path}[${index.toString()}]`);
    }
    if (current < previous) {
      fail("unsorted_records", path);
    }
  }
  return digests;
};

const parseReconstructedState: RecordParser<WatcherReconstructedState> = (
  value,
  path,
) => {
  const record = exactRecord(value, path, [
    "blockHash",
    "chainPointId",
    "priorStateRoot",
    "postStateRoot",
    "inputIds",
    "state",
  ]);
  return {
    blockHash: exactString(record.blockHash, `${path}.blockHash`, HEX_32),
    chainPointId: exactString(
      record.chainPointId,
      `${path}.chainPointId`,
      HEX_32,
    ),
    priorStateRoot: exactString(
      record.priorStateRoot,
      `${path}.priorStateRoot`,
      HEX_32,
    ),
    postStateRoot: exactString(
      record.postStateRoot,
      `${path}.postStateRoot`,
      HEX_32,
    ),
    inputIds: parseSortedDigests(record.inputIds, `${path}.inputIds`),
    state: parsePayload(record.state, `${path}.state`),
  };
};

const parseDecision: RecordParser<WatcherBlockDecision> = (value, path) => {
  const record = exactRecord(value, path, [
    "blockHash",
    "decision",
    "reconstructionDigest",
    "evidenceDigest",
  ]);
  return {
    blockHash: exactString(record.blockHash, `${path}.blockHash`, HEX_32),
    decision: exactLiteral(
      record.decision,
      `${path}.decision`,
      WATCHER_BLOCK_DECISIONS,
    ),
    reconstructionDigest: exactString(
      record.reconstructionDigest,
      `${path}.reconstructionDigest`,
      HEX_32,
    ),
    evidenceDigest: exactString(
      record.evidenceDigest,
      `${path}.evidenceDigest`,
      HEX_32,
    ),
  };
};

const parseFault: RecordParser<WatcherFault> = (value, path) => {
  const record = exactRecord(value, path, [
    "faultId",
    "blockHash",
    "familyId",
    "evidence",
  ]);
  return {
    faultId: exactString(record.faultId, `${path}.faultId`, HEX_32),
    blockHash: exactString(record.blockHash, `${path}.blockHash`, HEX_32),
    familyId: exactString(record.familyId, `${path}.familyId`, STABLE_NAME),
    evidence: parsePayload(record.evidence, `${path}.evidence`),
  };
};

const parseSubmission: RecordParser<WatcherSubmission> = (value, path) => {
  const record = exactRecord(value, path, [
    "submissionId",
    "faultId",
    "txBodyHash",
    "status",
  ]);
  return {
    submissionId: exactString(
      record.submissionId,
      `${path}.submissionId`,
      HEX_32,
    ),
    faultId: exactString(record.faultId, `${path}.faultId`, HEX_32),
    txBodyHash: exactString(record.txBodyHash, `${path}.txBodyHash`, HEX_32),
    status: exactLiteral(record.status, `${path}.status`, [
      "prepared",
      "submitted",
      "ambiguous",
    ]),
  };
};

const parseConfirmation: RecordParser<WatcherConfirmation> = (value, path) => {
  const record = exactRecord(value, path, [
    "confirmationId",
    "submissionId",
    "txHash",
    "chainPointId",
    "depth",
    "status",
  ]);
  return {
    confirmationId: exactString(
      record.confirmationId,
      `${path}.confirmationId`,
      HEX_32,
    ),
    submissionId: exactString(
      record.submissionId,
      `${path}.submissionId`,
      HEX_32,
    ),
    txHash: exactString(record.txHash, `${path}.txHash`, HEX_32),
    chainPointId: exactString(
      record.chainPointId,
      `${path}.chainPointId`,
      HEX_32,
    ),
    depth: exactString(record.depth, `${path}.depth`, CANONICAL_NATURAL),
    status: exactLiteral(record.status, `${path}.status`, [
      "observed",
      "confirmed",
      "rolled_back",
    ]),
  };
};

const parseRetry: RecordParser<WatcherRetry> = (value, path) => {
  const record = exactRecord(value, path, [
    "retryId",
    "submissionId",
    "attempt",
    "nextEligibleSlot",
    "reason",
  ]);
  return {
    retryId: exactString(record.retryId, `${path}.retryId`, HEX_32),
    submissionId: exactString(
      record.submissionId,
      `${path}.submissionId`,
      HEX_32,
    ),
    attempt: exactString(record.attempt, `${path}.attempt`, CANONICAL_POSITIVE),
    nextEligibleSlot: exactString(
      record.nextEligibleSlot,
      `${path}.nextEligibleSlot`,
      CANONICAL_NATURAL,
    ),
    reason: exactLiteral(record.reason, `${path}.reason`, [
      "provider_unavailable",
      "submission_ambiguous",
      "confirmation_timeout",
      "rollback",
      "topology_changed",
    ]),
  };
};

const parseDeadline: RecordParser<WatcherDeadline> = (value, path) => {
  const record = exactRecord(value, path, [
    "deadlineId",
    "subjectKind",
    "subjectId",
    "kind",
    "expiresAtSlot",
  ]);
  return {
    deadlineId: exactString(record.deadlineId, `${path}.deadlineId`, HEX_32),
    subjectKind: exactLiteral(record.subjectKind, `${path}.subjectKind`, [
      "fault",
      "submission",
    ]),
    subjectId: exactString(record.subjectId, `${path}.subjectId`, HEX_32),
    kind: exactLiteral(record.kind, `${path}.kind`, WATCHER_DEADLINE_KINDS),
    expiresAtSlot: exactString(
      record.expiresAtSlot,
      `${path}.expiresAtSlot`,
      CANONICAL_NATURAL,
    ),
  };
};

const parseCorrectionResult: RecordParser<WatcherCorrectionResult> = (
  value,
  path,
) => {
  const record = exactRecord(value, path, [
    "correctionId",
    "faultId",
    "confirmationId",
    "outcome",
    "finalStateRoot",
    "slashLovelace",
    "rewardLovelace",
  ]);
  return {
    correctionId: exactString(
      record.correctionId,
      `${path}.correctionId`,
      HEX_32,
    ),
    faultId: exactString(record.faultId, `${path}.faultId`, HEX_32),
    confirmationId: exactString(
      record.confirmationId,
      `${path}.confirmationId`,
      HEX_32,
    ),
    outcome: exactLiteral(record.outcome, `${path}.outcome`, [
      "removed",
      "resolved",
      "removed_and_slashed",
      "removed_slashed_and_rewarded",
    ]),
    finalStateRoot: exactString(
      record.finalStateRoot,
      `${path}.finalStateRoot`,
      HEX_32,
    ),
    slashLovelace: exactString(
      record.slashLovelace,
      `${path}.slashLovelace`,
      CANONICAL_NATURAL,
    ),
    rewardLovelace: exactString(
      record.rewardLovelace,
      `${path}.rewardLovelace`,
      CANONICAL_NATURAL,
    ),
  };
};

const parseSortedRecords = <T>(
  value: unknown,
  path: string,
  parser: RecordParser<T>,
  keyOf: (record: T) => string,
): readonly T[] => {
  if (!Array.isArray(value)) {
    fail("invalid_field", path);
  }
  const members = value as readonly unknown[];
  const records = members.map((member, index) =>
    parser(member, `${path}[${index.toString()}]`),
  );
  for (let index = 1; index < records.length; index += 1) {
    const previous = keyOf(records[index - 1] as T);
    const current = keyOf(records[index] as T);
    if (current === previous) {
      fail("duplicate_key", `${path}[${index.toString()}]`);
    }
    if (current < previous) {
      fail("unsorted_records", path);
    }
  }
  return records;
};

const STORE_RECORD_KEYS = [
  "l1Observations",
  "chainPoints",
  "protocolUtxos",
  "spentProtocolUtxos",
  "daProofInputs",
  "reconstructedStates",
  "decisions",
  "faults",
  "submissions",
  "confirmations",
  "retries",
  "deadlines",
  "correctionResults",
] as const;

const STORE_KEYS = [
  "schemaVersion",
  "migrationVersion",
  "migrationManifestSha256",
  "revision",
  "deploymentMarker",
  ...STORE_RECORD_KEYS,
  "caches",
] as const;

const parseRecords = (record: JsonRecord): WatcherDurableRecords => ({
  l1Observations: parseSortedRecords(
    record.l1Observations,
    "$.l1Observations",
    parseL1Observation,
    (entry) => entry.observationId,
  ),
  chainPoints: parseSortedRecords(
    record.chainPoints,
    "$.chainPoints",
    parseChainPoint,
    (entry) => entry.chainPointId,
  ),
  protocolUtxos: parseSortedRecords(
    record.protocolUtxos,
    "$.protocolUtxos",
    parseProtocolUtxo,
    (entry) => entry.outRef,
  ),
  spentProtocolUtxos: parseSortedRecords(
    record.spentProtocolUtxos,
    "$.spentProtocolUtxos",
    parseSpentProtocolUtxo,
    (entry) => entry.outRef,
  ),
  daProofInputs: parseSortedRecords(
    record.daProofInputs,
    "$.daProofInputs",
    parseDaProofInput,
    (entry) => entry.inputId,
  ),
  reconstructedStates: parseSortedRecords(
    record.reconstructedStates,
    "$.reconstructedStates",
    parseReconstructedState,
    (entry) => entry.blockHash,
  ),
  decisions: parseSortedRecords(
    record.decisions,
    "$.decisions",
    parseDecision,
    (entry) => entry.blockHash,
  ),
  faults: parseSortedRecords(
    record.faults,
    "$.faults",
    parseFault,
    (entry) => entry.faultId,
  ),
  submissions: parseSortedRecords(
    record.submissions,
    "$.submissions",
    parseSubmission,
    (entry) => entry.submissionId,
  ),
  confirmations: parseSortedRecords(
    record.confirmations,
    "$.confirmations",
    parseConfirmation,
    (entry) => entry.confirmationId,
  ),
  retries: parseSortedRecords(
    record.retries,
    "$.retries",
    parseRetry,
    (entry) => entry.retryId,
  ),
  deadlines: parseSortedRecords(
    record.deadlines,
    "$.deadlines",
    parseDeadline,
    (entry) => entry.deadlineId,
  ),
  correctionResults: parseSortedRecords(
    record.correctionResults,
    "$.correctionResults",
    parseCorrectionResult,
    (entry) => entry.correctionId,
  ),
});

const assertReferences = (records: WatcherDurableRecords): void => {
  const chainPoints = new Map(
    records.chainPoints.map((entry) => [entry.chainPointId, entry]),
  );
  const inputs = new Set(records.daProofInputs.map((entry) => entry.inputId));
  const reconstructed = new Set(
    records.reconstructedStates.map((entry) => entry.blockHash),
  );
  const decisions = new Map(
    records.decisions.map((entry) => [entry.blockHash, entry]),
  );
  const faults = new Set(records.faults.map((entry) => entry.faultId));
  const submissions = new Map(
    records.submissions.map((entry) => [entry.submissionId, entry]),
  );
  const confirmations = new Map(
    records.confirmations.map((entry) => [entry.confirmationId, entry]),
  );
  const assertUniqueRelation = <T>(
    entries: readonly T[],
    relationKey: (entry: T) => string,
    path: string,
  ): void => {
    const seen = new Set<string>();
    for (const entry of entries) {
      const key = relationKey(entry);
      if (seen.has(key)) {
        fail("duplicate_key", path);
      }
      seen.add(key);
    }
  };

  assertUniqueRelation(
    records.submissions,
    (entry) => entry.txBodyHash,
    "$.submissions.txBodyHash",
  );
  assertUniqueRelation(
    records.retries,
    (entry) => `${entry.submissionId}\u0000${entry.attempt}`,
    "$.retries.(submissionId,attempt)",
  );
  assertUniqueRelation(
    records.deadlines,
    (entry) =>
      `${entry.subjectKind}\u0000${entry.subjectId}\u0000${entry.kind}`,
    "$.deadlines.(subjectKind,subjectId,kind)",
  );
  assertUniqueRelation(
    records.correctionResults,
    (entry) => entry.faultId,
    "$.correctionResults.faultId",
  );

  for (const observation of records.l1Observations) {
    const point = chainPoints.get(observation.chainPointId);
    if (point === undefined || point.providerId !== observation.providerId) {
      fail(
        "broken_reference",
        `$.l1Observations.${observation.observationId}.chainPointId`,
      );
    }
  }
  for (const utxo of records.protocolUtxos) {
    if (!chainPoints.has(utxo.chainPointId)) {
      fail("broken_reference", `$.protocolUtxos.${utxo.outRef}.chainPointId`);
    }
  }
  const activeOutRefs = new Set(
    records.protocolUtxos.map(({ outRef }) => outRef),
  );
  for (const utxo of records.spentProtocolUtxos) {
    const creationPoint = chainPoints.get(utxo.chainPointId);
    const spentAtPoint = chainPoints.get(utxo.spentAtChainPointId);
    if (
      activeOutRefs.has(utxo.outRef) ||
      creationPoint === undefined ||
      spentAtPoint === undefined ||
      compareChainPointOrder(spentAtPoint, creationPoint) < 0
    ) {
      fail(
        activeOutRefs.has(utxo.outRef) ? "duplicate_key" : "broken_reference",
        `$.spentProtocolUtxos.${utxo.outRef}`,
      );
    }
  }
  for (const state of records.reconstructedStates) {
    if (!chainPoints.has(state.chainPointId)) {
      fail(
        "broken_reference",
        `$.reconstructedStates.${state.blockHash}.chainPointId`,
      );
    }
    for (const inputId of state.inputIds) {
      if (!inputs.has(inputId)) {
        fail(
          "broken_reference",
          `$.reconstructedStates.${state.blockHash}.inputIds`,
        );
      }
    }
  }
  for (const decision of records.decisions) {
    if (!reconstructed.has(decision.blockHash)) {
      fail("broken_reference", `$.decisions.${decision.blockHash}.blockHash`);
    }
  }
  for (const fault of records.faults) {
    const decision = decisions.get(fault.blockHash);
    if (
      decision === undefined ||
      (decision.decision !== "fault_detected" &&
        decision.decision !== "fault_proven" &&
        decision.decision !== "removed_or_resolved")
    ) {
      fail("broken_reference", `$.faults.${fault.faultId}.blockHash`);
    }
  }
  for (const submission of records.submissions) {
    if (!faults.has(submission.faultId)) {
      fail(
        "broken_reference",
        `$.submissions.${submission.submissionId}.faultId`,
      );
    }
  }
  for (const confirmation of records.confirmations) {
    if (
      !submissions.has(confirmation.submissionId) ||
      !chainPoints.has(confirmation.chainPointId)
    ) {
      fail(
        "broken_reference",
        `$.confirmations.${confirmation.confirmationId}`,
      );
    }
  }
  for (const retry of records.retries) {
    if (!submissions.has(retry.submissionId)) {
      fail("broken_reference", `$.retries.${retry.retryId}.submissionId`);
    }
  }
  for (const deadline of records.deadlines) {
    const targetExists =
      deadline.subjectKind === "fault"
        ? faults.has(deadline.subjectId)
        : submissions.has(deadline.subjectId);
    if (!targetExists) {
      fail("broken_reference", `$.deadlines.${deadline.deadlineId}.subjectId`);
    }
  }
  for (const result of records.correctionResults) {
    const confirmation = confirmations.get(result.confirmationId);
    const submission =
      confirmation === undefined
        ? undefined
        : submissions.get(confirmation.submissionId);
    if (
      !faults.has(result.faultId) ||
      confirmation?.status !== "confirmed" ||
      submission?.faultId !== result.faultId
    ) {
      fail("broken_reference", `$.correctionResults.${result.correctionId}`);
    }
  }
};

type CacheSource = WatcherDurableRecords &
  Readonly<{ deploymentMarker: DeploymentMarker }>;

const cacheCollections = (
  source: CacheSource,
): readonly Readonly<{
  namespace: CacheNamespace;
  records: readonly unknown[];
  keyOf: (record: never) => string;
}>[] => [
  {
    namespace: "l1_observations",
    records: source.l1Observations,
    keyOf: (record: WatcherL1Observation) => record.observationId,
  },
  {
    namespace: "chain_points",
    records: source.chainPoints,
    keyOf: (record: WatcherL1ChainPoint) => record.chainPointId,
  },
  {
    namespace: "protocol_utxos",
    records: source.protocolUtxos,
    keyOf: (record: WatcherProtocolUtxo) => record.outRef,
  },
  {
    namespace: "spent_protocol_utxos",
    records: source.spentProtocolUtxos,
    keyOf: (record: WatcherSpentProtocolUtxo) => record.outRef,
  },
  {
    namespace: "da_proof_inputs",
    records: source.daProofInputs,
    keyOf: (record: WatcherDaProofInput) => record.inputId,
  },
  {
    namespace: "reconstructed_states",
    records: source.reconstructedStates,
    keyOf: (record: WatcherReconstructedState) => record.blockHash,
  },
  {
    namespace: "decisions",
    records: source.decisions,
    keyOf: (record: WatcherBlockDecision) => record.blockHash,
  },
  {
    namespace: "faults",
    records: source.faults,
    keyOf: (record: WatcherFault) => record.faultId,
  },
  {
    namespace: "submissions",
    records: source.submissions,
    keyOf: (record: WatcherSubmission) => record.submissionId,
  },
  {
    namespace: "confirmations",
    records: source.confirmations,
    keyOf: (record: WatcherConfirmation) => record.confirmationId,
  },
  {
    namespace: "retries",
    records: source.retries,
    keyOf: (record: WatcherRetry) => record.retryId,
  },
  {
    namespace: "deadlines",
    records: source.deadlines,
    keyOf: (record: WatcherDeadline) => record.deadlineId,
  },
  {
    namespace: "correction_results",
    records: source.correctionResults,
    keyOf: (record: WatcherCorrectionResult) => record.correctionId,
  },
];

export const rebuildWatcherDurableCaches = (
  source: CacheSource,
): WatcherDurableCaches => {
  const sourceSha256 = sha256Utf8(canonicalJson(source as CanonicalJson));
  const entries = cacheCollections(source)
    .flatMap(({ namespace, records, keyOf }) =>
      records.map((record, index) => ({
        namespace,
        key: keyOf(record as never),
        index: index.toString(),
        recordSha256: sha256Utf8(canonicalJson(record as CanonicalJson)),
      })),
    )
    .sort((left, right) => {
      const namespaceOrder = left.namespace.localeCompare(right.namespace);
      return namespaceOrder === 0
        ? left.key.localeCompare(right.key)
        : namespaceOrder;
    });
  return {
    schemaVersion: WATCHER_DURABLE_CACHE_SCHEMA_VERSION,
    sourceSha256,
    entries,
  };
};

const parseCaches = (value: unknown): WatcherDurableCaches => {
  const caches = exactRecord(value, "$.caches", [
    "schemaVersion",
    "sourceSha256",
    "entries",
  ]);
  if (caches.schemaVersion !== WATCHER_DURABLE_CACHE_SCHEMA_VERSION) {
    fail("unsupported_schema", "$.caches.schemaVersion");
  }
  const entries = parseSortedRecords(
    caches.entries,
    "$.caches.entries",
    (member, path): WatcherDurableCacheEntry => {
      const entry = exactRecord(member, path, [
        "namespace",
        "key",
        "index",
        "recordSha256",
      ]);
      return {
        namespace: exactLiteral(entry.namespace, `${path}.namespace`, [
          "chain_points",
          "confirmations",
          "correction_results",
          "da_proof_inputs",
          "deadlines",
          "decisions",
          "faults",
          "l1_observations",
          "protocol_utxos",
          "reconstructed_states",
          "retries",
          "spent_protocol_utxos",
          "submissions",
        ]),
        key: exactString(
          entry.key,
          `${path}.key`,
          /^(?:[0-9a-f#]+|[a-z0-9_.-]+)$/u,
        ),
        index: exactString(entry.index, `${path}.index`, CANONICAL_NATURAL),
        recordSha256: exactString(
          entry.recordSha256,
          `${path}.recordSha256`,
          HEX_32,
        ),
      };
    },
    (entry) => `${entry.namespace}\u0000${entry.key}`,
  );
  return {
    schemaVersion: WATCHER_DURABLE_CACHE_SCHEMA_VERSION,
    sourceSha256: exactString(
      caches.sourceSha256,
      "$.caches.sourceSha256",
      HEX_32,
    ),
    entries,
  };
};

const sortRecords = <T>(
  records: readonly T[],
  keyOf: (record: T) => string,
): readonly T[] =>
  [...records].sort((left, right) => keyOf(left).localeCompare(keyOf(right)));

const canonicalizeRecords = (
  records: WatcherDurableRecords,
): WatcherDurableRecords => ({
  l1Observations: sortRecords(
    records.l1Observations,
    (entry) => entry.observationId,
  ),
  chainPoints: sortRecords(records.chainPoints, (entry) => entry.chainPointId),
  protocolUtxos: sortRecords(records.protocolUtxos, (entry) => entry.outRef),
  spentProtocolUtxos: sortRecords(
    records.spentProtocolUtxos,
    (entry) => entry.outRef,
  ),
  daProofInputs: sortRecords(records.daProofInputs, (entry) => entry.inputId),
  reconstructedStates: sortRecords(
    records.reconstructedStates.map((entry) => ({
      ...entry,
      inputIds: [...entry.inputIds].sort(),
    })),
    (entry) => entry.blockHash,
  ),
  decisions: sortRecords(records.decisions, (entry) => entry.blockHash),
  faults: sortRecords(records.faults, (entry) => entry.faultId),
  submissions: sortRecords(records.submissions, (entry) => entry.submissionId),
  confirmations: sortRecords(
    records.confirmations,
    (entry) => entry.confirmationId,
  ),
  retries: sortRecords(records.retries, (entry) => entry.retryId),
  deadlines: sortRecords(records.deadlines, (entry) => entry.deadlineId),
  correctionResults: sortRecords(
    records.correctionResults,
    (entry) => entry.correctionId,
  ),
});

const parseMarker = (value: unknown): DeploymentMarker => {
  try {
    return parseDeploymentMarker(value);
  } catch {
    return fail("invalid_field", "$.deploymentMarker");
  }
};

export const parseWatcherDurableStore = (
  value: unknown,
): WatcherDurableStore => {
  const record = exactRecord(value, "$", STORE_KEYS);
  if (record.schemaVersion !== WATCHER_DURABLE_STORE_SCHEMA_VERSION) {
    fail("unsupported_schema", "$.schemaVersion");
  }
  if (record.migrationVersion !== WATCHER_DURABLE_MIGRATION_VERSION) {
    fail("unsupported_schema", "$.migrationVersion");
  }
  if (
    record.migrationManifestSha256 !== WATCHER_DURABLE_MIGRATION_MANIFEST_SHA256
  ) {
    fail("integrity_mismatch", "$.migrationManifestSha256");
  }
  const deploymentMarker = parseMarker(record.deploymentMarker);
  const records = parseRecords(record);
  assertReferences(records);
  const caches = parseCaches(record.caches);
  const expectedCaches = rebuildWatcherDurableCaches({
    deploymentMarker,
    ...records,
  });
  if (
    canonicalJson(caches as CanonicalJson) !==
    canonicalJson(expectedCaches as CanonicalJson)
  ) {
    fail("cache_mismatch", "$.caches");
  }
  return {
    schemaVersion: WATCHER_DURABLE_STORE_SCHEMA_VERSION,
    migrationVersion: WATCHER_DURABLE_MIGRATION_VERSION,
    migrationManifestSha256: WATCHER_DURABLE_MIGRATION_MANIFEST_SHA256,
    revision: exactString(record.revision, "$.revision", CANONICAL_NATURAL),
    deploymentMarker,
    ...records,
    caches,
  };
};

type WatcherDurableRecordsInput = Omit<
  WatcherDurableRecords,
  "spentProtocolUtxos"
> &
  Partial<Pick<WatcherDurableRecords, "spentProtocolUtxos">>;

export const makeWatcherDurableStore = (input: {
  readonly deploymentMarker: DeploymentMarker;
  readonly revision: string;
  readonly records: WatcherDurableRecordsInput;
}): WatcherDurableStore => {
  const deploymentMarker = parseMarker(input.deploymentMarker);
  const records = canonicalizeRecords({
    ...input.records,
    spentProtocolUtxos: input.records.spentProtocolUtxos ?? [],
  });
  const candidate = {
    schemaVersion: WATCHER_DURABLE_STORE_SCHEMA_VERSION,
    migrationVersion: WATCHER_DURABLE_MIGRATION_VERSION,
    migrationManifestSha256: WATCHER_DURABLE_MIGRATION_MANIFEST_SHA256,
    revision: input.revision,
    deploymentMarker,
    ...records,
    caches: rebuildWatcherDurableCaches({
      deploymentMarker,
      ...records,
    }),
  };
  return parseWatcherDurableStore(candidate);
};

export const journalWatcherProtocolUtxoTransition = (input: {
  readonly sourceStore: unknown;
  readonly nextChainPoints: readonly WatcherL1ChainPoint[];
  readonly nextProtocolUtxos: readonly WatcherProtocolUtxo[];
  readonly spentAtChainPointId: string;
}): Readonly<{
  protocolUtxos: readonly WatcherProtocolUtxo[];
  spentProtocolUtxos: readonly WatcherSpentProtocolUtxo[];
}> => {
  const source = parseWatcherDurableStore(input.sourceStore);
  const spentAtChainPointId = exactString(
    input.spentAtChainPointId,
    "$.spentAtChainPointId",
    HEX_32,
  );
  if (
    !input.nextChainPoints.some(
      ({ chainPointId }) => chainPointId === spentAtChainPointId,
    )
  ) {
    fail("broken_reference", "$.spentAtChainPointId");
  }
  const priorActive = new Map(
    source.protocolUtxos.map((entry) => [entry.outRef, entry]),
  );
  const priorSpent = new Set(
    source.spentProtocolUtxos.map(({ outRef }) => outRef),
  );
  const sourceChainPoints = new Map(
    source.chainPoints.map((entry) => [entry.chainPointId, entry]),
  );
  const spentAtPoint = input.nextChainPoints.find(
    ({ chainPointId }) => chainPointId === spentAtChainPointId,
  );
  const nextActive = new Map<string, WatcherProtocolUtxo>();
  for (const entry of input.nextProtocolUtxos) {
    if (nextActive.has(entry.outRef) || priorSpent.has(entry.outRef)) {
      fail("duplicate_key", `$.protocolUtxos.${entry.outRef}`);
    }
    const prior = priorActive.get(entry.outRef);
    if (
      prior !== undefined &&
      canonicalJson(prior as CanonicalJson) !==
        canonicalJson(entry as CanonicalJson)
    ) {
      fail("integrity_mismatch", `$.protocolUtxos.${entry.outRef}`);
    }
    nextActive.set(entry.outRef, entry);
  }
  const newlySpent = source.protocolUtxos
    .filter(({ outRef }) => !nextActive.has(outRef))
    .map((entry) => {
      const creationPoint = sourceChainPoints.get(entry.chainPointId);
      if (
        creationPoint === undefined ||
        spentAtPoint === undefined ||
        compareChainPointOrder(spentAtPoint, creationPoint) < 0
      ) {
        fail("broken_reference", `$.protocolUtxos.${entry.outRef}`);
      }
      return {
        ...entry,
        spentAtChainPointId,
      };
    });
  return Object.freeze({
    protocolUtxos: Object.freeze(
      sortRecords(input.nextProtocolUtxos, (entry) => entry.outRef),
    ),
    spentProtocolUtxos: Object.freeze(
      sortRecords(
        [...source.spentProtocolUtxos, ...newlySpent],
        (entry) => entry.outRef,
      ),
    ),
  });
};

const EMPTY_RECORDS: WatcherDurableRecords = Object.freeze({
  l1Observations: [],
  chainPoints: [],
  protocolUtxos: [],
  spentProtocolUtxos: [],
  daProofInputs: [],
  reconstructedStates: [],
  decisions: [],
  faults: [],
  submissions: [],
  confirmations: [],
  retries: [],
  deadlines: [],
  correctionResults: [],
});

export const makeEmptyWatcherDurableStore = (
  deploymentMarker: DeploymentMarker,
): WatcherDurableStore =>
  makeWatcherDurableStore({
    deploymentMarker,
    revision: "0",
    records: EMPTY_RECORDS,
  });

const UTF8_ENCODER = new TextEncoder();
const UTF8_DECODER = new TextDecoder("utf-8", { fatal: true });

export const encodeWatcherDurableStore = (
  value: WatcherDurableStore,
): Uint8Array =>
  UTF8_ENCODER.encode(
    canonicalJson(parseWatcherDurableStore(value) as CanonicalJson),
  );

export const watcherDurableStoreBytesSha256 = (value: Uint8Array): string =>
  sha256Bytes(value);

export const decodeWatcherDurableStore = (
  value: Uint8Array,
): WatcherDurableStore => {
  let text = "";
  let decoded: unknown;
  try {
    text = UTF8_DECODER.decode(value);
    decoded = JSON.parse(text) as unknown;
  } catch {
    return fail("invalid_encoding", "$");
  }
  const parsed = parseWatcherDurableStore(decoded);
  if (text !== canonicalJson(parsed as CanonicalJson)) {
    fail("noncanonical_encoding", "$");
  }
  return parsed;
};

/**
 * The backend boundary is intentionally minimal. Implementations must compare
 * the digest of the complete current byte string and durably replace the
 * complete byte string as one atomic operation. They must never expose a
 * partially written value after process or host failure.
 */
export type WatcherDurableAtomicBackend = Readonly<{
  read: () => Promise<Uint8Array | null>;
  compareAndSwap: (
    expectedSha256: string | null,
    next: Uint8Array,
  ) => Promise<boolean>;
}>;

export type WatcherDurableAtomicSnapshot = Readonly<{
  bytes: Uint8Array;
  sha256: string;
}>;

export type WatcherDurableAtomicCommit =
  | Readonly<{
      committed: true;
      sha256: string;
    }>
  | Readonly<{
      committed: false;
    }>;

/**
 * Reads one complete immutable backend snapshot. The returned bytes are copied
 * so a backend cannot mutate the value after the digest has been established.
 */
export const readWatcherDurableAtomicSnapshot = async (
  backend: WatcherDurableAtomicBackend,
): Promise<WatcherDurableAtomicSnapshot | null> => {
  const bytes = await readBackend(backend);
  if (bytes === null) {
    return null;
  }
  const copy = Uint8Array.from(bytes);
  return Object.freeze({
    bytes: copy,
    sha256: watcherDurableStoreBytesSha256(copy),
  });
};

/**
 * Durably replaces one complete backend snapshot iff its exact prior digest is
 * still current. A false result is an ordinary stale/concurrent-writer
 * conflict; backend failures remain explicit persistence failures.
 */
export const compareAndSwapWatcherDurableAtomicSnapshot = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly expectedSha256: string | null;
  readonly next: Uint8Array;
}): Promise<WatcherDurableAtomicCommit> => {
  if (input.expectedSha256 !== null && !HEX_32.test(input.expectedSha256)) {
    return fail("invalid_field", "$.expectedSha256");
  }
  if (!(input.next instanceof Uint8Array) || input.next.byteLength === 0) {
    return fail("invalid_field", "$.next");
  }
  const next = Uint8Array.from(input.next);
  const sha256 = watcherDurableStoreBytesSha256(next);
  let committed: boolean;
  try {
    committed = await input.backend.compareAndSwap(input.expectedSha256, next);
  } catch {
    return fail("persistence_failure", "$.backend.compareAndSwap");
  }
  return committed
    ? Object.freeze({ committed: true, sha256 })
    : Object.freeze({ committed: false });
};

export type WatcherDurableMigrationResult = Readonly<{
  initialized: boolean;
  snapshot: WatcherDurableStore;
  encodedSha256: string;
}>;

const assertMarkerMatches = (
  expected: DeploymentMarker,
  actual: DeploymentMarker,
): void => {
  if (
    expected.schemaVersion !== actual.schemaVersion ||
    expected.manifestId !== actual.manifestId
  ) {
    fail("deployment_marker_mismatch", "$.deploymentMarker");
  }
};

const readBackend = async (
  backend: WatcherDurableAtomicBackend,
): Promise<Uint8Array | null> => {
  try {
    return await backend.read();
  } catch {
    return fail("persistence_failure", "$.backend.read");
  }
};

export const migrateWatcherDurableStore = async (input: {
  readonly backend: WatcherDurableAtomicBackend;
  readonly deploymentMarker: DeploymentMarker;
  readonly maxConflicts?: number;
}): Promise<WatcherDurableMigrationResult> => {
  const expectedMarker = parseMarker(input.deploymentMarker);
  const maxConflicts = input.maxConflicts ?? 8;
  if (!Number.isSafeInteger(maxConflicts) || maxConflicts < 1) {
    fail("invalid_field", "$.maxConflicts");
  }
  for (let attempt = 0; attempt < maxConflicts; attempt += 1) {
    const current = await readBackend(input.backend);
    if (current !== null) {
      const snapshot = decodeWatcherDurableStore(current);
      assertMarkerMatches(expectedMarker, snapshot.deploymentMarker);
      return {
        initialized: false,
        snapshot,
        encodedSha256: watcherDurableStoreBytesSha256(current),
      };
    }

    const snapshot = makeEmptyWatcherDurableStore(expectedMarker);
    const encoded = encodeWatcherDurableStore(snapshot);
    let written: boolean;
    try {
      written = await input.backend.compareAndSwap(null, encoded);
    } catch {
      return fail("persistence_failure", "$.backend.compareAndSwap");
    }
    if (written) {
      return {
        initialized: true,
        snapshot,
        encodedSha256: watcherDurableStoreBytesSha256(encoded),
      };
    }
  }
  return fail("migration_conflict", "$.backend.compareAndSwap");
};
