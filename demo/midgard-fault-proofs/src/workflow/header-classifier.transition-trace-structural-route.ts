import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { TransitionTraceL1Events } from "../transition-trace/l1-events.js";
import {
  isStructurallyInconsistent,
  transitionTraceCanonicalDetections,
} from "../transition-trace/replay-authority.canonical-detections.js";
import { detectStructuralTransitionTraceFaults } from "../transition-trace/replay-authority.js";
import { classifyCanonicalBlockViolations } from "./classification.js";
import { COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import {
  detectionJson,
  digest,
  HEADER_CLASSIFIER,
  HEADER_DECISION,
  type HeaderClassifier,
  type HeaderDecision,
  type RecordedDetection,
} from "./header-classifier.authenticated-state-queue-observation-digest.js";
import { sealDecision } from "./header-classifier.create-header-classifier.js";

export const TRANSITION_TRACE_STRUCTURAL_ROUTE =
  "transition_trace_structural_v1";

/**
 * Seals a finding decided before the replay union: `fault_detected` when its
 * category is installed, otherwise `unprovable` / `category_not_installed`.
 * It carries no replay context, because no replay ran.
 */
export const sealPreUnionDecision = ({
  classifier,
  observationDigest,
  headerHash,
  payloadEnvelopeSha256,
  payloadSha256,
  route,
  category,
  selected,
}: {
  readonly classifier: HeaderClassifier;
  readonly observationDigest: string;
  readonly headerHash: string;
  readonly payloadEnvelopeSha256: string;
  readonly payloadSha256: string;
  readonly route: string;
  readonly category: FraudProofCatalogueCategoryName;
  readonly selected: RecordedDetection;
}): HeaderDecision => {
  const installed = classifier.launchScope.includes(category);
  const classification = {
    decision: installed ? "fault_detected" : "unprovable",
    selected: detectionJson(selected),
    reason: installed ? null : "category_not_installed",
  } as const;
  const common = {
    schemaVersion: HEADER_DECISION,
    classifierVersion: HEADER_CLASSIFIER,
    deploymentFingerprint: classifier.deploymentFingerprint,
    headerHash,
    authenticatedObservationDigest: observationDigest,
    payloadEnvelopeSha256,
    payloadSha256,
    replayVersion: COMPLETE_CANONICAL_REPLAY,
    replayDigest: digest({
      route,
      launchScope: classifier.launchScope,
      selected: detectionJson(selected),
    }),
    launchScope: classifier.launchScope,
    launchScopeDigest: digest(classifier.launchScope),
    classificationDigest: digest(classification),
  } as const;
  const detection = {
    violationId: selected.violationId,
    detectionId: selected.detectionId,
    position: selected.position.toString(),
  };
  return installed
    ? sealDecision({
        ...common,
        decision: "fault_detected",
        category,
        ...detection,
      })
    : sealDecision({
        ...common,
        decision: "unprovable",
        reason: "category_not_installed",
        ...detection,
      });
};

/**
 * A trace, event map or counted root that disagrees with itself is provable
 * from the block and its L1 events alone. The predecessor, the historical
 * corpus and every other family's replay assume a consistent trace and abort
 * on such a block, so it is classified here, before any of them runs. The
 * detection list and its ids are the ones the transition replay produces.
 * Returns undefined for a structurally consistent block, and whenever
 * transitionTrace is not installed.
 */
export const classifyTransitionTraceStructuralRoute = async ({
  classifier,
  observationDigest,
  evidence,
  l1Events,
  minimumConfirmationDepth,
}: {
  readonly classifier: HeaderClassifier;
  readonly observationDigest: string;
  readonly evidence: CanonicalBlockEvidence;
  readonly l1Events: TransitionTraceL1Events | undefined;
  readonly minimumConfirmationDepth: number;
}): Promise<HeaderDecision | undefined> => {
  // An uninstalled transitionTrace leaves the block to the installed families:
  // they can still prove their own faults on it, and a pre-union seal here
  // would replace those findings with an unprovable one.
  if (!classifier.launchScope.includes("transitionTrace")) return undefined;
  if (l1Events === undefined)
    throw new Error("Transition structural route requires raw L1 events");
  const { detections } = await detectStructuralTransitionTraceFaults({
    evidence,
    l1Events,
  });
  if (!isStructurallyInconsistent(detections)) return undefined;
  const classification = await classifyCanonicalBlockViolations({
    evidence,
    detections: transitionTraceCanonicalDetections(evidence, detections),
    minimumConfirmationDepth,
  });
  if (
    classification.decision !== "fault_detected" ||
    classification.category !== "transitionTrace"
  )
    throw new Error("Transition structural finding lost its category");
  return sealPreUnionDecision({
    classifier,
    observationDigest,
    headerHash: evidence.headerHash,
    payloadEnvelopeSha256: evidence.payloadEnvelopeSha256,
    payloadSha256: evidence.payloadSha256,
    route: TRANSITION_TRACE_STRUCTURAL_ROUTE,
    category: "transitionTrace",
    selected: classification.selected,
  });
};
