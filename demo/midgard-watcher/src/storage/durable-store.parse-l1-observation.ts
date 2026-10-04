import { type DeploymentMarker } from "@al-ft/midgard-core/deployment-manifest-identity";

import {
  CANONICAL_NATURAL,
  exactLiteral,
  exactRecord,
  exactString,
  HEX_32,
  OUT_REF,
  parsePayload,
  PROVIDER_ID,
  WATCHER_DURABLE_CACHE_SCHEMA_VERSION,
  WATCHER_DURABLE_MIGRATION_VERSION,
  WATCHER_DURABLE_STORE_SCHEMA_VERSION,
  type WatcherDurablePayload,
} from "./durable-store.canonical-json.js";

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

export type CacheNamespace =
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

export type RecordParser<T> = (value: unknown, path: string) => T;

// Only detached, deeply immutable outputs belong to these parser-specific maps.
const parsedChainPoints = new WeakMap<object, WatcherL1ChainPoint>();
const parsedL1Observations = new WeakMap<object, WatcherL1Observation>();

export const parseChainPoint: RecordParser<WatcherL1ChainPoint> = (
  value,
  path,
) => {
  const prior =
    typeof value === "object" && value !== null
      ? parsedChainPoints.get(value)
      : undefined;
  if (prior !== undefined) return prior;
  const record = exactRecord(value, path, [
    "chainPointId",
    "providerId",
    "blockHash",
    "slot",
    "blockNo",
    "depth",
  ]);
  const parsed = Object.freeze({
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
  });
  parsedChainPoints.set(parsed, parsed);
  return parsed;
};

export const compareChainPointOrder = (
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

export const parseL1Observation: RecordParser<WatcherL1Observation> = (
  value,
  path,
) => {
  const prior =
    typeof value === "object" && value !== null
      ? parsedL1Observations.get(value)
      : undefined;
  if (prior !== undefined) return prior;
  const record = exactRecord(value, path, [
    "observationId",
    "providerId",
    "chainPointId",
    "payload",
  ]);
  const parsed = Object.freeze({
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
    payload: Object.freeze(parsePayload(record.payload, `${path}.payload`)),
  });
  parsedL1Observations.set(parsed, parsed);
  return parsed;
};

export const parseProtocolUtxo: RecordParser<WatcherProtocolUtxo> = (
  value,
  path,
) => {
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
