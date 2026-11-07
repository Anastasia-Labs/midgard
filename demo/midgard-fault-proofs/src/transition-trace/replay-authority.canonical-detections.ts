import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import { eventKeyCborSubject } from "../workflow/detection-subject.js";
import type {
  TransitionTraceDetection,
  TransitionTraceFaultKind,
} from "./detect.js";
import {
  provenTransitionEventKeyCbor,
  transitionTraceDetectionId,
} from "./replay-authority.js";

/** The one mapping from transition detections to canonical detections. Ids
 * are list positions, so every caller must map the complete list. */
export const transitionTraceCanonicalDetections = (
  evidence: CanonicalBlockEvidence,
  detections: readonly TransitionTraceDetection[],
): CanonicalViolationDetection[] =>
  detections.map((detection, index) => ({
    violationId: "transition-trace",
    headerHash: evidence.headerHash,
    ...eventKeyCborSubject(provenTransitionEventKeyCbor(detection)),
    detectionId: transitionTraceDetectionId(index, detection.kind),
    position: BigInt(index),
    provenTransitionEventKeyCbor: provenTransitionEventKeyCbor(detection),
  }));

/** Findings that show the committed trace, its event map or its counted roots
 * disagree with each other. Each is block-level, so it precedes every event
 * finding of any family, and no other family can replay such a block. */
export const STRUCTURAL_TRANSITION_TRACE_FAULT_KINDS: ReadonlySet<TransitionTraceFaultKind> =
  new Set([
    "countFault",
    "traceBoundary",
    "traceLink",
    "duplicateTraceEvent",
    "eventToStepMismatch",
    "sourceMembershipMismatch",
  ]);

export const isStructurallyInconsistent = (
  detections: readonly TransitionTraceDetection[],
): boolean =>
  detections.some(
    (detection) =>
      detection.buildable &&
      STRUCTURAL_TRANSITION_TRACE_FAULT_KINDS.has(detection.kind),
  );
