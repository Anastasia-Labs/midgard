import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  requireTransitionTraceL1Events,
  type TransitionTraceL1Events,
} from "../transition-trace/l1-events.js";
import { computeTransitionTraceL1EventEvidenceDigest } from "../transition-trace/replay-authority.js";
import {
  requireValidationTraceReplayContext,
  type ValidationTraceReplayContext,
} from "../validation-dispute/replay.js";
import { type CanonicalViolationDetection } from "./classification.js";

export const COMPLETE_CANONICAL_REPLAY =
  "midgard-complete-canonical-replay-v1" as const;

export const COMPLETE_CANONICAL_REPLAY_PREDECESSOR =
  "midgard-complete-canonical-replay-predecessor-v1" as const;

export type CompleteCanonicalReplayPredecessor = Readonly<{
  schemaVersion: typeof COMPLETE_CANONICAL_REPLAY_PREDECESSOR;
  challengedHeaderHash: string;
  headerHash: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
}>;

export type CompleteCanonicalReplayContext = Readonly<{
  /**
   * Exact public-DA/L1-authenticated predecessor. Required by ledger-relative
   * detectors unless the current header commits the empty genesis ledger.
   */
  predecessor?: CompleteCanonicalReplayPredecessor;
  transitionTraceEvents?: TransitionTraceL1Events;
  /** Opaque independently derived transaction and originating-event replay. */
  validationTraceReplay?: ValidationTraceReplayContext;
}>;

export type CompleteCanonicalReplayContextIdentity = Readonly<{
  predecessorHeaderHash?: string;
  predecessorPayloadEnvelopeSha256?: string;
  predecessorPayloadSha256?: string;
  validationTraceReplayDigest?: string;
  validationTraceEventEvidenceDigest?: string;
  transitionTraceEventEvidenceDigest?: string;
}>;

export type CompleteCanonicalReplayDecision = {
  readonly replayVersion: typeof COMPLETE_CANONICAL_REPLAY;
  readonly launchScope: readonly FraudProofCatalogueCategoryName[];
  readonly headerHash: string;
  readonly payloadEnvelopeSha256: string;
  readonly payloadSha256: string;
  readonly context: CompleteCanonicalReplayContextIdentity | null;
  readonly detections: readonly CanonicalViolationDetection[];
};

export interface CompleteCanonicalReplay {
  readonly replayVersion: typeof COMPLETE_CANONICAL_REPLAY;
  readonly launchScope: readonly FraudProofCatalogueCategoryName[];
  replay(
    evidence: CanonicalBlockEvidence,
    context?: CompleteCanonicalReplayContext,
  ): Promise<CompleteCanonicalReplayDecision>;
}

export const admittedReplayers = new WeakSet<object>();

export const admittedDecisions = new WeakSet<object>();

export const predecessorEvidenceByAuthority = new WeakMap<
  object,
  CanonicalBlockEvidence
>();

export const replayContextIdentity = ({
  evidence,
  context,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
}): CompleteCanonicalReplayContextIdentity | null => {
  const predecessor = requireReplayPredecessorEvidence({ evidence, context });
  const validation = context?.validationTraceReplay;
  const events = context?.transitionTraceEvents;
  if (events !== undefined) {
    requireTransitionTraceL1Events(events);
    if (events.headerHash !== evidence.headerHash)
      throw new Error(
        "complete replay event authority belongs to another header",
      );
  }
  if (validation !== undefined)
    requireValidationTraceReplayContext({
      evidence,
      context: validation,
      predecessor: context?.predecessor,
      transitionTraceEvents: context?.transitionTraceEvents,
    });
  if (
    predecessor === undefined &&
    validation === undefined &&
    events === undefined
  )
    return null;
  return Object.freeze({
    ...(events === undefined
      ? {}
      : {
          transitionTraceEventEvidenceDigest:
            computeTransitionTraceL1EventEvidenceDigest({
              evidence,
              l1Events: events,
            }),
        }),
    ...(validation === undefined
      ? {}
      : {
          validationTraceReplayDigest: validation.replayDigest,
          ...(validation.eventEvidenceDigest === undefined
            ? {}
            : {
                validationTraceEventEvidenceDigest:
                  validation.eventEvidenceDigest,
              }),
        }),
    ...(predecessor === undefined
      ? {}
      : {
          predecessorHeaderHash: predecessor.headerHash,
          predecessorPayloadEnvelopeSha256: predecessor.payloadEnvelopeSha256,
          predecessorPayloadSha256: predecessor.payloadSha256,
        }),
  });
};

export const predecessorRecord = (
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

export const requireReplayPredecessorEvidence = ({
  evidence,
  context,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
}): CanonicalBlockEvidence | undefined => {
  if (context?.predecessor === undefined) return undefined;
  const predecessor = predecessorEvidenceByAuthority.get(context.predecessor);
  if (
    predecessor === undefined ||
    context.predecessor.schemaVersion !==
      COMPLETE_CANONICAL_REPLAY_PREDECESSOR ||
    context.predecessor.challengedHeaderHash !== evidence.headerHash
  ) {
    throw new Error(
      "complete replay predecessor was not admitted for this challenged header",
    );
  }
  return predecessor;
};
