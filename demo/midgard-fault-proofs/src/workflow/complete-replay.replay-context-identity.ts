import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { Ledger } from "../ledger-store/index.js";
import {
  requireTransitionTraceL1Events,
  type TransitionTraceL1Events,
} from "../transition-trace/l1-events.js";
import { computeTransitionTraceL1EventEvidenceDigest } from "../transition-trace/replay-authority.js";
import { type CanonicalViolationDetection } from "./classification.js";

export const COMPLETE_CANONICAL_REPLAY =
  "midgard-complete-canonical-replay-v1" as const;

export const COMPLETE_CANONICAL_REPLAY_PREDECESSOR =
  "midgard-complete-canonical-replay-predecessor-v2" as const;

/**
 * The ledger the challenged block starts from: the ledger after its
 * `prev_header_hash`, bound to its `prev_utxos_root`.
 */
export type CompleteCanonicalReplayPredecessor = Readonly<{
  schemaVersion: typeof COMPLETE_CANONICAL_REPLAY_PREDECESSOR;
  challengedHeaderHash: string;
  headerHash: string;
  utxosRoot: string;
}>;

export type CompleteCanonicalReplayContext = Readonly<{
  /**
   * The root-bound predecessor ledger. Required by ledger-relative detectors
   * unless the current header commits the empty genesis ledger.
   */
  predecessor?: CompleteCanonicalReplayPredecessor;
  transitionTraceEvents?: TransitionTraceL1Events;
}>;

export type CompleteCanonicalReplayContextIdentity = Readonly<{
  predecessorHeaderHash?: string;
  predecessorUtxosRoot?: string;
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

export const predecessorLedgerByAuthority = new WeakMap<object, Ledger>();

export const replayContextIdentity = ({
  evidence,
  context,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly context: CompleteCanonicalReplayContext | undefined;
}): CompleteCanonicalReplayContextIdentity | null => {
  const predecessor = requireReplayPredecessorLedger({ evidence, context });
  const events = context?.transitionTraceEvents;
  if (events !== undefined) {
    requireTransitionTraceL1Events(events);
    if (events.headerHash !== evidence.headerHash)
      throw new Error(
        "complete replay event authority belongs to another header",
      );
  }
  if (predecessor === undefined && events === undefined) return null;
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
    ...(predecessor === undefined
      ? {}
      : {
          predecessorHeaderHash: evidence.header.prevHeaderHash,
          predecessorUtxosRoot: predecessor.root,
        }),
  });
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
}): Ledger | undefined => {
  if (context?.predecessor === undefined) return undefined;
  const authority = context.predecessor;
  const ledger = predecessorLedgerByAuthority.get(authority);
  if (
    ledger === undefined ||
    authority.schemaVersion !== COMPLETE_CANONICAL_REPLAY_PREDECESSOR ||
    authority.challengedHeaderHash !== evidence.headerHash ||
    authority.headerHash !== evidence.header.prevHeaderHash ||
    authority.utxosRoot !== evidence.header.prevUtxosRoot ||
    ledger.root !== evidence.header.prevUtxosRoot
  ) {
    throw new Error(
      "complete replay predecessor was not admitted for this challenged header",
    );
  }
  return ledger;
};
