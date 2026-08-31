import type {
  AuthenticatedStateQueueHeaderObservationV1,
  EvidenceProvenanceV1,
  FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import type { CanonicalViolationDetectionV1 } from "./classification-v1.js";
import {
  admitCompleteCanonicalReplayPredecessorV1,
  type CompleteCanonicalReplayContextV1,
  completeCanonicalReplayDecisionDigestV1,
  type CompleteCanonicalReplayDecisionV1,
  type CompleteCanonicalReplayPredecessorV1,
  type CompleteCanonicalReplayV1,
  requireCompleteCanonicalReplayDecisionV1,
} from "./complete-replay-v1.js";

export const PRODUCTION_AUTHENTICATED_REPLAY_CAPTURE_V1 =
  "midgard-production-authenticated-replay-capture-v1" as const;
export const PRODUCTION_AUTHENTICATED_REPLAY_CAPTURE_PORT_V1 =
  "midgard-production-authenticated-replay-capture-port-v1" as const;

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

/**
 * Identity shared by watcher-captured raw replay inputs before classification.
 * `category`, `violationId`, findings, trie roots, and `decisionDigest` are
 * intentionally absent: fault-proofs derives those after local re-admission.
 */
export type ProductionAuthenticatedReplayCaptureIdentityV1 = Readonly<{
  schemaVersion: typeof PRODUCTION_AUTHENTICATED_REPLAY_CAPTURE_V1;
  deploymentFingerprint: string;
  headerHash: string;
  stateQueueObservationDigest: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
  replayVersion: string;
  replayDigest: string;
  position: string;
  detectionId: string;
}>;

/** Exact predecessor bytes; fault-proofs independently re-admits both legs. */
export type ProductionRawPredecessorContextV1 = Readonly<{
  observation: AuthenticatedStateQueueHeaderObservationV1;
  payloadEnvelopeCborHex: string;
  daProvenance: EvidenceProvenanceV1;
}>;

/**
 * Re-admits an untrusted predecessor capture through the same L1/public-DA
 * evidence constructor as the challenged block, then verifies the two exact
 * header links. No structural predecessor object is replay authority.
 */
export const admitProductionRawPredecessorContextV1 = async ({
  value,
  currentEvidence,
  minimumConfirmationDepth,
}: {
  readonly value: unknown;
  readonly currentEvidence: CanonicalBlockEvidenceV1;
  readonly minimumConfirmationDepth: number;
}): Promise<CompleteCanonicalReplayPredecessorV1> =>
  await admitCompleteCanonicalReplayPredecessorV1({
    value,
    currentEvidence,
    minimumConfirmationDepth,
  });

export type ProductionNativeScriptDecodingReplayCaptureV1 = Readonly<{
  identity: ProductionAuthenticatedReplayCaptureIdentityV1;
  predecessor: ProductionRawPredecessorContextV1;
  replayTranscriptCborHex: string;
  descriptorOutputCborHex: string;
  referenceScriptBytesHex: string | null;
}>;

export type ProductionIntermediateLedgerReplayCaptureV1 = Readonly<{
  identity: ProductionAuthenticatedReplayCaptureIdentityV1;
  predecessor: ProductionRawPredecessorContextV1;
  replayTranscriptCborHex: string;
  transitionIndex: string;
}>;

export type ProductionCrossBlockReplayCaptureV1 = Readonly<{
  identity: ProductionAuthenticatedReplayCaptureIdentityV1;
  settled: ProductionRawPredecessorContextV1;
  settlementOutputCborHex: string;
  settlementOutRef: string;
  settlementTransactionBodyCborHex: string;
  settlementInclusionPointDigest: string;
  settlementFinalityPolicyDigest: string;
}>;

export type ProductionHistoricalNativeScriptReplayCaptureV1 = Readonly<{
  identity: ProductionAuthenticatedReplayCaptureIdentityV1;
  /** Exact response bytes from the dedicated public retained-DA protocol. */
  retainedDaHistoricalPreimageEnvelopeCborHex: string;
  retainedDaProtocol: string;
  retainedDaSourceDigest: string;
  /** Raw L1-history corroboration admitted separately by historical-script. */
  historicalL1Corroboration: unknown;
}>;

export type ProductionAuthenticatedReplayCaptureV1 =
  | ProductionNativeScriptDecodingReplayCaptureV1
  | ProductionIntermediateLedgerReplayCaptureV1
  | ProductionCrossBlockReplayCaptureV1
  | ProductionHistoricalNativeScriptReplayCaptureV1;

/**
 * Source-neutral watcher/application capture port. Returned values are always
 * untrusted. A fixed family factory must call its own exact parser/replayer;
 * the port version is not an evidence admission token.
 */
export interface ProductionAuthenticatedReplayCapturePortV1<
  Category extends FraudProofCatalogueCategoryName,
> {
  readonly portVersion: typeof PRODUCTION_AUTHENTICATED_REPLAY_CAPTURE_PORT_V1;
  readonly category: Category;
  capture(input: {
    readonly evidence: CanonicalBlockEvidenceV1;
    readonly replayer: CompleteCanonicalReplayV1;
    readonly replayDecision: CompleteCanonicalReplayDecisionV1;
    readonly replayContext?: CompleteCanonicalReplayContextV1;
    readonly detection: CanonicalViolationDetectionV1;
    readonly stateQueueObservationDigest: string;
  }): Promise<unknown>;
}

const record = (
  value: unknown,
  label: string,
): Readonly<Record<string, unknown>> => {
  if (
    typeof value !== "object" ||
    value === null ||
    Array.isArray(value) ||
    Object.getPrototypeOf(value) !== Object.prototype ||
    Reflect.ownKeys(value).length !== Object.keys(value).length
  ) {
    throw new Error(`${label} must be a plain string-keyed object`);
  }
  return value as Readonly<Record<string, unknown>>;
};

const exact = (
  value: unknown,
  keys: readonly string[],
  label: string,
): Readonly<Record<string, unknown>> => {
  const parsed = record(value, label);
  const actual = Object.keys(parsed).sort();
  const expected = [...keys].sort();
  if (
    actual.length !== expected.length ||
    actual.some((key, index) => key !== expected[index])
  ) {
    throw new Error(`${label} has missing or unknown fields`);
  }
  return parsed;
};

const canonicalString = (value: unknown, label: string): string => {
  if (
    typeof value !== "string" ||
    value.length === 0 ||
    value.trim() !== value
  ) {
    throw new Error(`${label} must be a canonical non-empty string`);
  }
  return value;
};

/** Exact common parser used before any family-specific replay begins. */
export const admitProductionAuthenticatedReplayCaptureIdentityV1 = ({
  value,
  evidence,
  replayer,
  replayDecision,
  replayContext,
  detection,
  deploymentFingerprint,
  stateQueueObservationDigest,
}: {
  readonly value: unknown;
  readonly evidence: CanonicalBlockEvidenceV1;
  readonly replayer: CompleteCanonicalReplayV1;
  readonly replayDecision: CompleteCanonicalReplayDecisionV1;
  readonly replayContext?: CompleteCanonicalReplayContextV1;
  readonly detection: CanonicalViolationDetectionV1;
  readonly deploymentFingerprint: string;
  readonly stateQueueObservationDigest: string;
}): ProductionAuthenticatedReplayCaptureIdentityV1 => {
  const admittedDetections = requireCompleteCanonicalReplayDecisionV1({
    evidence,
    replayer,
    decision: replayDecision,
    ...(replayContext === undefined ? {} : { context: replayContext }),
  });
  if (!admittedDetections.includes(detection)) {
    throw new Error(
      "capture detection was not selected from the admitted replay decision",
    );
  }
  const replayDigest = completeCanonicalReplayDecisionDigestV1({
    evidence,
    replayer,
    decision: replayDecision,
    ...(replayContext === undefined ? {} : { context: replayContext }),
  });
  const parsed = exact(
    value,
    [
      "schemaVersion",
      "deploymentFingerprint",
      "headerHash",
      "stateQueueObservationDigest",
      "payloadEnvelopeSha256",
      "payloadSha256",
      "replayVersion",
      "replayDigest",
      "position",
      "detectionId",
    ],
    "authenticated replay capture identity",
  );
  const hex32 = (candidate: unknown, label: string): string => {
    const admitted = canonicalString(candidate, label);
    if (!HEX_32.test(admitted)) throw new Error(`${label} must be 32-byte hex`);
    return admitted;
  };
  const headerHash = canonicalString(parsed.headerHash, "capture headerHash");
  if (!HEX_28.test(headerHash)) {
    throw new Error("capture headerHash must be 28-byte hex");
  }
  const position = canonicalString(parsed.position, "capture position");
  if (!NATURAL.test(position) || BigInt(position) !== detection.position) {
    throw new Error("capture position differs from canonical replay detection");
  }
  const admitted: ProductionAuthenticatedReplayCaptureIdentityV1 = {
    schemaVersion: PRODUCTION_AUTHENTICATED_REPLAY_CAPTURE_V1,
    deploymentFingerprint: hex32(
      parsed.deploymentFingerprint,
      "capture deploymentFingerprint",
    ),
    headerHash,
    stateQueueObservationDigest: hex32(
      parsed.stateQueueObservationDigest,
      "capture stateQueueObservationDigest",
    ),
    payloadEnvelopeSha256: hex32(
      parsed.payloadEnvelopeSha256,
      "capture payloadEnvelopeSha256",
    ),
    payloadSha256: hex32(parsed.payloadSha256, "capture payloadSha256"),
    replayVersion: canonicalString(
      parsed.replayVersion,
      "capture replayVersion",
    ),
    replayDigest: hex32(parsed.replayDigest, "capture replayDigest"),
    position,
    detectionId: canonicalString(parsed.detectionId, "capture detectionId"),
  };
  if (
    parsed.schemaVersion !== PRODUCTION_AUTHENTICATED_REPLAY_CAPTURE_V1 ||
    admitted.deploymentFingerprint !== deploymentFingerprint ||
    admitted.headerHash !== evidence.headerHash ||
    admitted.stateQueueObservationDigest !== stateQueueObservationDigest ||
    admitted.payloadEnvelopeSha256 !== evidence.payloadEnvelopeSha256 ||
    admitted.payloadSha256 !== evidence.payloadSha256 ||
    admitted.replayVersion !== replayDecision.replayVersion ||
    admitted.replayDigest !== replayDigest ||
    admitted.detectionId !== detection.detectionId ||
    detection.headerHash !== evidence.headerHash
  ) {
    throw new Error(
      "authenticated replay capture identity differs from fetched evidence/replay",
    );
  }
  return Object.freeze(admitted);
};

/** Fail closed before invoking a missing or category-substituted capture port. */
export const requireProductionAuthenticatedReplayCapturePortV1 = <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  port,
}: {
  readonly category: Category;
  readonly port: ProductionAuthenticatedReplayCapturePortV1<Category> | null;
}): ProductionAuthenticatedReplayCapturePortV1<Category> => {
  if (
    port === null ||
    port.portVersion !== PRODUCTION_AUTHENTICATED_REPLAY_CAPTURE_PORT_V1 ||
    port.category !== category ||
    typeof port.capture !== "function"
  ) {
    throw new Error(
      `${category} production workflow requires its exact authenticated replay capture port`,
    );
  }
  return port;
};
