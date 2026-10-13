import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { type CheckedLedger } from "../ledger-at.js";
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
  "midgard-complete-canonical-replay-predecessor-v2" as const;

/**
 * The ledger the challenged block starts from: the ledger after its
 * `prev_header_hash`, checked against its `prev_utxos_root`.
 */
export type CompleteCanonicalReplayPredecessor = Readonly<{
  schemaVersion: typeof COMPLETE_CANONICAL_REPLAY_PREDECESSOR;
  challengedHeaderHash: string;
  headerHash: string;
  utxosRoot: string;
}>;

export type CompleteCanonicalReplayContext = Readonly<{
  /**
   * The root-checked predecessor ledger. Required by ledger-relative detectors
   * unless the current header commits the empty genesis ledger.
   */
  predecessor?: CompleteCanonicalReplayPredecessor;
  transitionTraceEvents?: TransitionTraceL1Events;
  /** Opaque independently derived transaction and originating-event replay. */
  validationTraceReplay?: ValidationTraceReplayContext;
}>;

export type CompleteCanonicalReplayContextIdentity = Readonly<{
  predecessorHeaderHash?: string;
  predecessorUtxosRoot?: string;
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

export const predecessorLedgerByAuthority = new WeakMap<
  object,
  CheckedLedger
>();

/**
 * The ledger after the challenged block, as its admission found it: checked
 * against the header's `utxos_root`; refused because its root differs (the
 * block's committed root is then transitionTrace's to judge); or unavailable,
 * which a reader must treat as a classification gap.
 */
export type ReplayPostLedger =
  | Readonly<{ kind: "checked"; ledger: CheckedLedger }>
  | Readonly<{ kind: "root-mismatch" }>
  | Readonly<{ kind: "unavailable" }>;

export const postLedgerByAuthority = new WeakMap<object, ReplayPostLedger>();

const POST_LEDGER_UNAVAILABLE: ReplayPostLedger = Object.freeze({
  kind: "unavailable",
});

export const replayContextIdentity = ({
  evidence,
  context,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
}): CompleteCanonicalReplayContextIdentity | null => {
  const predecessor = requireReplayPredecessorLedger({ evidence, context });
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
          predecessorHeaderHash: evidence.header.prevHeaderHash,
          predecessorUtxosRoot: predecessor.utxosRoot,
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

/**
 * The ledger behind an admitted predecessor authority, bound to the challenged
 * header's `prev_header_hash` and `prev_utxos_root`.
 */
export const requireReplayPredecessorLedger = ({
  evidence,
  context,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
}): CheckedLedger | undefined => {
  if (context?.predecessor === undefined) return undefined;
  const authority = context.predecessor;
  const ledger = predecessorLedgerByAuthority.get(authority);
  if (
    ledger === undefined ||
    authority.schemaVersion !== COMPLETE_CANONICAL_REPLAY_PREDECESSOR ||
    authority.challengedHeaderHash !== evidence.headerHash ||
    authority.headerHash !== evidence.header.prevHeaderHash ||
    authority.utxosRoot !== evidence.header.prevUtxosRoot ||
    ledger.utxosRoot !== evidence.header.prevUtxosRoot
  ) {
    throw new Error(
      "complete replay predecessor was not admitted for this challenged header",
    );
  }
  return ledger;
};

/**
 * The ledger after the challenged block, admitted with the context's
 * predecessor. With no admitted context the post ledger is unavailable, even
 * when the challenged header commits the empty root.
 */
export const requireReplayPostLedger = ({
  evidence,
  context,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
}): ReplayPostLedger => {
  const authority = context?.predecessor;
  if (authority === undefined) return POST_LEDGER_UNAVAILABLE;
  requireReplayPredecessorLedger({ evidence, context });
  return postLedgerByAuthority.get(authority) ?? POST_LEDGER_UNAVAILABLE;
};
