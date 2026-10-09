import { createHash } from "node:crypto";

import {
  admitAuthenticatedStateQueueHeaderObservation,
  type AuthenticatedStateQueueHeaderObservation,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
  Header,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { type CrossBlockSettlementAuthority } from "../cross-block-duplicate-event/settlement-authority.js";
import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { type TransitionTraceEventAuthority } from "../transition-trace/l1-events.js";
import {
  type CanonicalBlockClassification,
  type CanonicalViolationDetection,
} from "./classification.js";
import {
  COMPLETE_CANONICAL_REPLAY,
  type CompleteCanonicalReplay,
  type CompleteCanonicalReplayContext,
} from "./complete-replay.js";

export const HEADER_CLASSIFIER =
  "midgard-production-header-classifier-v1" as const;

export const HEADER_DECISION = "midgard-production-header-decision-v1" as const;

export const PREDECESSOR_CONTEXT_REQUIRED =
  "production-predecessor-context-required-v1" as const;

export const MINT_DECLARED_ASSET_LIMIT_VIOLATION_ID =
  "mint-declared-asset-limit" as const;

export const HEX_32 = /^[0-9a-f]{64}$/u;

export type CanonicalJson =
  | null
  | boolean
  | number
  | string
  | readonly CanonicalJson[]
  | { readonly [key: string]: CanonicalJson };

const canonicalize = (value: CanonicalJson): CanonicalJson => {
  if (Array.isArray(value)) return value.map(canonicalize);
  if (typeof value !== "object" || value === null) return value;
  return Object.freeze(
    Object.fromEntries(
      Object.entries(value)
        .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
        .map(([key, child]) => [key, canonicalize(child)]),
    ),
  );
};

export const digest = (value: CanonicalJson): string =>
  createHash("sha256")
    .update(JSON.stringify(canonicalize(value)))
    .digest("hex");

type DetectionJson = Readonly<{
  detectionId: string;
  headerHash: string;
  violationId: string;
  position: string;
  diagnostic: string | null;
}>;

/**
 * A decision records what was selected, never how it was ordered: raw routes
 * decide before any transition trace exists, so they carry no event subject.
 */
export type RecordedDetection = Omit<
  CanonicalViolationDetection,
  "frontier" | "subjectEventKeyCbors"
>;

export const detectionJson = (detection: RecordedDetection): DetectionJson => ({
  detectionId: detection.detectionId,
  headerHash: detection.headerHash,
  violationId: detection.violationId,
  position: detection.position.toString(),
  diagnostic: detection.diagnostic ?? null,
});

export const classificationJson = (
  classification: CanonicalBlockClassification,
): CanonicalJson => ({
  schemaVersion: classification.schemaVersion,
  decision: classification.decision,
  headerHash: classification.headerHash,
  category:
    classification.decision === "fault_detected"
      ? classification.category
      : null,
  selected:
    classification.decision === "no_fault_detected"
      ? null
      : detectionJson(classification.selected),
  detections: classification.detections.map(detectionJson),
  unprovableGaps: classification.unprovableGaps.map((gap) => ({
    ...detectionJson(gap),
    reason: gap.reason,
  })),
});

/**
 * Value identity of the exact authenticated state-queue observation supplied
 * by the L1 source extractor. Header CBOR is used instead of object key order,
 * and the source mode/provenance/chain point/depth remain explicit.
 */
export const authenticatedStateQueueObservationDigest = async ({
  observation,
  minimumConfirmationDepth,
}: {
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  readonly minimumConfirmationDepth: number;
}): Promise<string> => {
  const admitted = await admitAuthenticatedStateQueueHeaderObservation({
    observation,
    minimumConfirmationDepth,
  });
  return digest({
    schemaVersion: admitted.schemaVersion,
    sourceMode: admitted.sourceMode,
    provenance: {
      trustClass: admitted.provenance.trustClass,
      sourceId: admitted.provenance.sourceId,
      grade: admitted.provenance.grade,
      diagnosticLabel: admitted.provenance.diagnosticLabel ?? null,
    },
    chainPoint: {
      slot: admitted.chainPoint.slot.toString(),
      blockHash: admitted.chainPoint.blockHash,
    },
    confirmationDepth: admitted.confirmationDepth,
    headerHash: admitted.headerHash,
    headerCbor: Data.to(admitted.header, Header),
  });
};

type CommonDecision = Readonly<{
  schemaVersion: typeof HEADER_DECISION;
  classifierVersion: typeof HEADER_CLASSIFIER;
  deploymentFingerprint: string;
  headerHash: string;
  authenticatedObservationDigest: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
  replayVersion: typeof COMPLETE_CANONICAL_REPLAY;
  replayDigest: string;
  launchScope: readonly FraudProofCatalogueCategoryName[];
  launchScopeDigest: string;
  classificationDigest: string;
  decisionDigest: string;
}>;

export type HeaderFaultDecision = CommonDecision &
  Readonly<{
    decision: "fault_detected";
    category: FraudProofCatalogueCategoryName;
    violationId: string;
    detectionId: string;
    position: string;
  }>;

export type HeaderHealthyDecision = CommonDecision &
  Readonly<{ decision: "healthy" }>;

export type HeaderUnprovableDecision = CommonDecision &
  Readonly<{
    decision: "unprovable";
    reason:
      | "unregistered_violation"
      | "category_not_installed"
      | "predecessor_context_unavailable";
    violationId: string;
    detectionId: string;
    position: string;
  }>;

export type HeaderDecision =
  | HeaderFaultDecision
  | HeaderHealthyDecision
  | HeaderUnprovableDecision;

export type UnsealedHeaderDecision =
  | Omit<HeaderFaultDecision, "decisionDigest">
  | Omit<HeaderHealthyDecision, "decisionDigest">
  | Omit<HeaderUnprovableDecision, "decisionDigest">;

export interface HeaderClassifier {
  readonly classifierVersion: typeof HEADER_CLASSIFIER;
  readonly deploymentFingerprint: string;
  readonly launchScope: readonly FraudProofCatalogueCategoryName[];
}

export const admittedClassifiers = new WeakMap<
  object,
  Readonly<{
    replayer: CompleteCanonicalReplay;
    confirmationDepth: number;
    settlementAuthority?: CrossBlockSettlementAuthority;
    transitionTraceEventAuthority?: TransitionTraceEventAuthority;
  }>
>();

export const admittedDecisions = new WeakSet<object>();

export const canonicalInputsByDecision = new WeakMap<
  object,
  Readonly<{
    payloadEnvelopeCbor: Buffer;
    observation: AuthenticatedStateQueueHeaderObservation;
    daProvenance: CanonicalBlockEvidence["provenance"]["da"];
    minimumConfirmationDepth: number;
  }>
>();

export const replayContextByDecision = new WeakMap<
  object,
  CompleteCanonicalReplayContext
>();

export const exactCanonicalScope = (
  scope: readonly FraudProofCatalogueCategoryName[],
): readonly FraudProofCatalogueCategoryName[] => {
  if (scope.length === 0 || new Set(scope).size !== scope.length) {
    throw new Error("production classifier launch scope is empty or duplicate");
  }
  let prior = -1;
  for (const category of scope) {
    const position = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.indexOf(category);
    if (position <= prior) {
      throw new Error(
        "production classifier launch scope is not in canonical catalogue order",
      );
    }
    prior = position;
  }
  return Object.freeze([...scope]);
};
