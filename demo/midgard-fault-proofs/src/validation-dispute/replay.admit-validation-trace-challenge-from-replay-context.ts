import { EventKey } from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { type TransitionTraceL1Events } from "../transition-trace/l1-events.js";
import { computeTransitionTraceL1EventEvidenceDigest } from "../transition-trace/replay-authority.js";
import { buildRetainedValidationClaimWitness } from "../transition-trace/witnesses.js";
import {
  admitValidationTraceChallenge,
  type ReplayChallengeCoordinate,
  type ValidationTraceChallenge,
} from "../workflow/challenge-authority.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import { type CompleteCanonicalReplayPredecessor } from "../workflow/complete-replay.js";
import { completeReplayFindings } from "../workflow/replay-prerequisite.js";
import {
  authorities,
  replayDetectionId,
  type ReplayMaterial,
  sameEvidenceIdentity,
  VALIDATION_TRACE_REPLAY_CONTEXT,
  type ValidationTraceReplayContext,
} from "./replay.read-origin-events.js";

export const requireValidationTraceReplayContext = ({
  evidence,
  context,
  predecessor,
  transitionTraceEvents,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: ValidationTraceReplayContext;
  readonly predecessor?: CompleteCanonicalReplayPredecessor;
  readonly transitionTraceEvents?: TransitionTraceL1Events;
}): ValidationTraceReplayContext => {
  const authority = authorities.get(context);
  if (
    authority === undefined ||
    context.schemaVersion !== VALIDATION_TRACE_REPLAY_CONTEXT ||
    !sameEvidenceIdentity(evidence, context) ||
    authority.predecessor !== predecessor ||
    authority.transitionTraceEvents !== transitionTraceEvents ||
    context.eventSnapshotDigest !== transitionTraceEvents?.snapshotDigest ||
    context.eventEvidenceDigest !==
      (transitionTraceEvents === undefined
        ? undefined
        : computeTransitionTraceL1EventEvidenceDigest({
            evidence,
            l1Events: transitionTraceEvents,
          }))
  )
    throw new Error(
      "validation replay context was not admitted for this block and predecessor",
    );
  return context;
};

export const detectValidationTraceReplay = (
  input: Parameters<typeof requireValidationTraceReplayContext>[0],
): readonly CanonicalViolationDetection[] => {
  requireValidationTraceReplayContext(input);
  const authority = authorities.get(input.context)!;
  return completeReplayFindings(authority.detections, authority.prerequisites);
};

type ReplaySelectionInput = Parameters<
  typeof requireValidationTraceReplayContext
>[0] &
  Readonly<{ detectionId: string }>;

const selectedMaterial = (input: ReplaySelectionInput): ReplayMaterial => {
  requireValidationTraceReplayContext(input);
  const authority = authorities.get(input.context)!;
  if (
    !authority.detections.some(
      ({ detectionId }) => detectionId === input.detectionId,
    )
  )
    throw new Error(
      "validation replay selection is not an admitted interactive disagreement",
    );
  const material = authority.material.find(
    (entry) => replayDetectionId(entry) === input.detectionId,
  );
  if (material === undefined)
    throw new Error("validation replay selected material disappeared");
  return material;
};

/** Only identity metadata leaves the owner; verdict, replay input and trace do not. */
export const readValidationTraceReplaySelection = (
  input: ReplaySelectionInput,
) => {
  const material = selectedMaterial(input);
  return Object.freeze({
    detectionId: input.detectionId,
    headerHash: input.context.headerHash,
    payloadEnvelopeSha256: input.context.payloadEnvelopeSha256,
    payloadSha256: input.context.payloadSha256,
    eventKeyCbor: material.eventKeyCbor,
    coordinate: Object.freeze({
      domain: "transition_step" as const,
      index: material.stepIndex.toString(),
    }),
  });
};

/** Called by the adapter after its fresh transcript admission. This operation
 * binds the selected step itself and consumes only the owner's private material. */
export const admitValidationTraceChallengeFromReplayContext = async (
  input: ReplaySelectionInput &
    Readonly<{ coordinate: ReplayChallengeCoordinate }>,
): Promise<ValidationTraceChallenge> => {
  const coordinate: ReplayChallengeCoordinate = Object.freeze({
    ...input.coordinate,
    coordinate: Object.freeze({ ...input.coordinate.coordinate }),
  });
  const snapshot: ReplaySelectionInput = {
    evidence: input.evidence,
    context: input.context,
    predecessor: input.predecessor,
    transitionTraceEvents: input.transitionTraceEvents,
    detectionId: input.detectionId,
  };
  const selection = readValidationTraceReplaySelection(snapshot);
  if (
    coordinate.headerHash !== selection.headerHash ||
    coordinate.payloadEnvelopeSha256 !== selection.payloadEnvelopeSha256 ||
    coordinate.payloadSha256 !== selection.payloadSha256 ||
    coordinate.coordinate.domain !== selection.coordinate.domain ||
    coordinate.coordinate.index !== selection.coordinate.index
  )
    throw new Error(
      "validation replay challenge coordinate changed selected event",
    );
  const authority = authorities.get(snapshot.context)!;
  const material = selectedMaterial(snapshot);
  const { claim } = await buildRetainedValidationClaimWitness({
    reconstruction: authority.evidence.reconstruction,
    eventKey: Data.from(material.eventKeyCbor, EventKey),
  });
  return await admitValidationTraceChallenge({
    coordinate,
    evidence: authority.evidence,
    claim,
    challengerReplayInput: material.replay.replayInput,
    exactL1ReferenceOutRefs: material.exactL1ReferenceOutRefs,
  });
};
