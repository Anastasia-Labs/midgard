import type {
  AuthenticatedStateQueueHeaderObservation,
  EvidenceProvenance,
  FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence-v1.js";
import type { CanonicalViolationDetection } from "./classification-v1.js";
import {
  admitCompleteCanonicalReplayPredecessor,
  type CompleteCanonicalReplay,
  type CompleteCanonicalReplayContext,
  type CompleteCanonicalReplayDecision,
  completeCanonicalReplayDecisionDigest,
  type CompleteCanonicalReplayPredecessor,
  requireCompleteCanonicalReplayDecision,
} from "./complete-replay-v1.js";

export const AUTHENTICATED_REPLAY_CAPTURE =
  "midgard-production-authenticated-replay-capture-v1" as const;
export const AUTHENTICATED_REPLAY_CAPTURE_PORT =
  "midgard-production-authenticated-replay-capture-port-v1" as const;

const HEX_28 = /^[0-9a-f]{56}$/u;
const HEX_32 = /^[0-9a-f]{64}$/u;
const NATURAL = /^(?:0|[1-9][0-9]*)$/u;

/**
 * Identity shared by watcher-captured raw replay inputs before classification.
 * `category`, `violationId`, findings, trie roots, and `decisionDigest` are
 * intentionally absent: fault-proofs derives those after local re-admission.
 */
export type AuthenticatedReplayCaptureIdentity = Readonly<{
  schemaVersion: typeof AUTHENTICATED_REPLAY_CAPTURE;
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
export type RawPredecessorContext = Readonly<{
  observation: AuthenticatedStateQueueHeaderObservation;
  payloadEnvelopeCborHex: string;
  daProvenance: EvidenceProvenance;
}>;

/**
 * Re-admits an untrusted predecessor capture through the same L1/public-DA
 * evidence constructor as the challenged block, then verifies the two exact
 * header links. No structural predecessor object is replay authority.
 */
export const admitRawPredecessorContext = async ({
  value,
  currentEvidence,
  minimumConfirmationDepth,
}: {
  readonly value: unknown;
  readonly currentEvidence: CanonicalBlockEvidence;
  readonly minimumConfirmationDepth: number;
}): Promise<CompleteCanonicalReplayPredecessor> =>
  await admitCompleteCanonicalReplayPredecessor({
    value,
    currentEvidence,
    minimumConfirmationDepth,
  });

export type NativeScriptDecodingReplayCapture = Readonly<{
  identity: AuthenticatedReplayCaptureIdentity;
  predecessor: RawPredecessorContext;
  replayTranscriptCborHex: string;
  descriptorOutputCborHex: string;
  referenceScriptBytesHex: string | null;
}>;

export type IntermediateLedgerReplayCapture = Readonly<{
  identity: AuthenticatedReplayCaptureIdentity;
  predecessor: RawPredecessorContext;
  replayTranscriptCborHex: string;
  transitionIndex: string;
}>;

export type CrossBlockReplayCapture = Readonly<{
  identity: AuthenticatedReplayCaptureIdentity;
  settled: RawPredecessorContext;
  settlementOutputCborHex: string;
  settlementOutRef: string;
  settlementTransactionBodyCborHex: string;
  settlementInclusionPointDigest: string;
  settlementFinalityPolicyDigest: string;
}>;

export type HistoricalNativeScriptReplayCapture = Readonly<{
  identity: AuthenticatedReplayCaptureIdentity;
  /** Exact response bytes from the dedicated public retained-DA protocol. */
  retainedDaHistoricalPreimageEnvelopeCborHex: string;
  retainedDaProtocol: string;
  retainedDaSourceDigest: string;
  /** Raw L1-history corroboration admitted separately by historical-script. */
  historicalL1Corroboration: unknown;
}>;

export type AuthenticatedReplayCapture =
  | NativeScriptDecodingReplayCapture
  | IntermediateLedgerReplayCapture
  | CrossBlockReplayCapture
  | HistoricalNativeScriptReplayCapture;

/**
 * Source-neutral watcher/application capture port. Returned values are always
 * untrusted. A fixed family factory must call its own exact parser/replayer;
 * the port version is not an evidence admission token.
 */
export interface AuthenticatedReplayCapturePort<
  Category extends FraudProofCatalogueCategoryName,
> {
  readonly portVersion: typeof AUTHENTICATED_REPLAY_CAPTURE_PORT;
  readonly category: Category;
  capture(input: {
    readonly evidence: CanonicalBlockEvidence;
    readonly replayer: CompleteCanonicalReplay;
    readonly replayDecision: CompleteCanonicalReplayDecision;
    readonly replayContext?: CompleteCanonicalReplayContext;
    readonly detection: CanonicalViolationDetection;
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
export const admitAuthenticatedReplayCaptureIdentity = ({
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
  readonly evidence: CanonicalBlockEvidence;
  readonly replayer: CompleteCanonicalReplay;
  readonly replayDecision: CompleteCanonicalReplayDecision;
  readonly replayContext?: CompleteCanonicalReplayContext;
  readonly detection: CanonicalViolationDetection;
  readonly deploymentFingerprint: string;
  readonly stateQueueObservationDigest: string;
}): AuthenticatedReplayCaptureIdentity => {
  const admittedDetections = requireCompleteCanonicalReplayDecision({
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
  const replayDigest = completeCanonicalReplayDecisionDigest({
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
  const admitted: AuthenticatedReplayCaptureIdentity = {
    schemaVersion: AUTHENTICATED_REPLAY_CAPTURE,
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
    parsed.schemaVersion !== AUTHENTICATED_REPLAY_CAPTURE ||
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
export const requireAuthenticatedReplayCapturePort = <
  Category extends FraudProofCatalogueCategoryName,
>({
  category,
  port,
}: {
  readonly category: Category;
  readonly port: AuthenticatedReplayCapturePort<Category> | null;
}): AuthenticatedReplayCapturePort<Category> => {
  if (
    port === null ||
    port.portVersion !== AUTHENTICATED_REPLAY_CAPTURE_PORT ||
    port.category !== category ||
    typeof port.capture !== "function"
  ) {
    throw new Error(
      `${category} production workflow requires its exact authenticated replay capture port`,
    );
  }
  return port;
};
