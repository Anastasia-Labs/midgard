import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import {
  type CrossBlockSettlementContext,
  crossBlockSettlementRecords,
} from "../cross-block-duplicate-event/settlement-authority.js";
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
import { requireHistoricalNativeScriptCorpus } from "./historical-native-script-corpus.js";
import { type HistoricalNativeScriptCorpus } from "./historical-native-script-corpus.js";

export const COMPLETE_CANONICAL_REPLAY =
  "midgard-complete-canonical-replay-v1" as const;

export const COMPLETE_CANONICAL_REPLAY_PREDECESSOR =
  "midgard-complete-canonical-replay-predecessor-v1" as const;

export const COMPLETE_CANONICAL_REPLAY_HISTORICAL_CORPUS =
  "midgard-complete-canonical-replay-historical-corpus-v1" as const;

export type CompleteCanonicalReplayPredecessor = Readonly<{
  schemaVersion: typeof COMPLETE_CANONICAL_REPLAY_PREDECESSOR;
  challengedHeaderHash: string;
  headerHash: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
}>;

export type CompleteCanonicalReplayContext = Readonly<{
  settlements?: CrossBlockSettlementContext;
  /**
   * Exact public-DA/L1-authenticated predecessor. Required by ledger-relative
   * detectors unless the current header commits the empty genesis ledger.
   */
  predecessor?: CompleteCanonicalReplayPredecessor;
  /** Opaque authority for the complete retained-DA history of this header. */
  historicalCorpus?: CompleteCanonicalReplayHistoricalCorpus;
  transitionTraceEvents?: TransitionTraceL1Events;
  /** Opaque independently derived transaction and originating-event replay. */
  validationTraceReplay?: ValidationTraceReplayContext;
}>;

export type CompleteCanonicalReplayContextIdentity = Readonly<{
  settlementEvidenceDigest?: string;
  predecessorHeaderHash?: string;
  predecessorPayloadEnvelopeSha256?: string;
  predecessorPayloadSha256?: string;
  historicalThroughHeaderHash?: string;
  historicalProviderRosterDigest?: string;
  historicalEvidenceDigest?: string;
  validationTraceReplayDigest?: string;
  validationTraceEventEvidenceDigest?: string;
  transitionTraceEventEvidenceDigest?: string;
}>;

export type CompleteCanonicalReplayHistoricalCorpus = Readonly<{
  schemaVersion: typeof COMPLETE_CANONICAL_REPLAY_HISTORICAL_CORPUS;
  challengedHeaderHash: string;
  throughHeaderHash: string;
  providerRosterDigest: string;
  checkpointDigest: string;
  corpusDigest: string;
  evidenceDigest: string;
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

export const historicalCorpusByAuthority = new WeakMap<
  object,
  HistoricalNativeScriptCorpus
>();

// The classifier admits the corpus against the evidence it routed, while a
// family workflow re-fetches the same challenged block from retained DA, so
// the corpus binds to the block's content digests rather than one object.
const sameCanonicalBlockEvidence = (
  left: CanonicalBlockEvidence,
  right: CanonicalBlockEvidence,
): boolean =>
  left === right ||
  (left.headerHash === right.headerHash &&
    left.payloadEnvelopeSha256 === right.payloadEnvelopeSha256 &&
    left.payloadSha256 === right.payloadSha256);

export const admitCompleteCanonicalReplayHistoricalCorpus = ({
  evidence,
  corpus,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly corpus: HistoricalNativeScriptCorpus;
}): CompleteCanonicalReplayHistoricalCorpus => {
  const admitted = requireHistoricalNativeScriptCorpus(corpus);
  if (
    !sameCanonicalBlockEvidence(admitted.currentEvidence, evidence) ||
    corpus.throughHeaderHash !== evidence.headerHash
  ) {
    throw new Error(
      "historical replay corpus belongs to another challenged header",
    );
  }
  const authority: CompleteCanonicalReplayHistoricalCorpus = Object.freeze({
    schemaVersion: COMPLETE_CANONICAL_REPLAY_HISTORICAL_CORPUS,
    challengedHeaderHash: evidence.headerHash,
    throughHeaderHash: corpus.throughHeaderHash,
    providerRosterDigest: corpus.providerRosterDigest,
    checkpointDigest: corpus.checkpointDigest,
    corpusDigest: corpus.corpusDigest,
    evidenceDigest: corpus.evidenceDigest,
  });
  historicalCorpusByAuthority.set(authority, corpus);
  return authority;
};

export const requireReplayHistoricalCorpus = ({
  evidence,
  context,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
}): HistoricalNativeScriptCorpus => {
  const authority = context?.historicalCorpus;
  const corpus =
    authority === undefined
      ? undefined
      : historicalCorpusByAuthority.get(authority);
  if (
    authority === undefined ||
    corpus === undefined ||
    authority.schemaVersion !== COMPLETE_CANONICAL_REPLAY_HISTORICAL_CORPUS ||
    authority.challengedHeaderHash !== evidence.headerHash ||
    authority.throughHeaderHash !== corpus.throughHeaderHash ||
    authority.providerRosterDigest !== corpus.providerRosterDigest ||
    authority.checkpointDigest !== corpus.checkpointDigest ||
    authority.corpusDigest !== corpus.corpusDigest ||
    authority.evidenceDigest !== corpus.evidenceDigest ||
    !sameCanonicalBlockEvidence(
      requireHistoricalNativeScriptCorpus(corpus).currentEvidence,
      evidence,
    )
  ) {
    throw new Error(
      "complete replay historical corpus was not admitted for this challenged header",
    );
  }
  return corpus;
};

export const replayContextIdentity = ({
  evidence,
  context,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
}): CompleteCanonicalReplayContextIdentity | null => {
  const predecessor = requireReplayPredecessorEvidence({ evidence, context });
  const historical = context?.historicalCorpus;
  const settlements = context?.settlements;
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
  if (settlements !== undefined)
    crossBlockSettlementRecords(evidence, settlements);
  if (
    predecessor === undefined &&
    historical === undefined &&
    settlements === undefined &&
    validation === undefined &&
    events === undefined
  )
    return null;
  if (historical !== undefined) {
    requireReplayHistoricalCorpus({ evidence, context });
  }
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
    ...(settlements === undefined
      ? {}
      : { settlementEvidenceDigest: settlements.evidenceDigest }),
    ...(predecessor === undefined
      ? {}
      : {
          predecessorHeaderHash: predecessor.headerHash,
          predecessorPayloadEnvelopeSha256: predecessor.payloadEnvelopeSha256,
          predecessorPayloadSha256: predecessor.payloadSha256,
        }),
    ...(historical === undefined
      ? {}
      : {
          historicalThroughHeaderHash: historical.throughHeaderHash,
          historicalProviderRosterDigest: historical.providerRosterDigest,
          historicalEvidenceDigest: historical.evidenceDigest,
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
