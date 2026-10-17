import "./complete-replay.resolved-output-non-canonical-complete-canonical-replay.js";

import { createHash } from "node:crypto";

import {
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
} from "@al-ft/midgard-sdk";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { type CheckedLedger } from "../ledger-at.js";
import { detectMintAuthorizationReplay } from "../mint-authorization/replay.js";
import { detectMintItemNonCanonicalCompleteReplay } from "../mint-item-non-canonical/replay.js";
import { detectObserverOrderInvalidCompleteReplay } from "../observer-order-invalid/replay.js";
import { detectRedeemerCanonicityCompleteReplay } from "../redeemer-canonicity/authenticated-workflow.js";
import { type TransitionTraceL1Events } from "../transition-trace/l1-events.js";
import { transitionTraceCanonicalDetections } from "../transition-trace/replay-authority.canonical-detections.js";
import { replayTransitionTraceFromRetainedHistory } from "../transition-trace/replay-authority.js";
import { detectValueConservationFaults } from "../value-not-preserved/replay.js";
import { detectWithdrawalMistagReplay } from "../withdrawal-mistag/replay.js";
import { type CanonicalViolationDetection } from "./classification.js";
import {
  canonicalizeReplayJson,
  completeCanonicalReplayPredecessorLedger,
  detectDoubleSpends,
  detectNetworkIds,
  replayDecisionJson,
} from "./complete-replay.admit-complete-canonical-replay-predecessor.js";
import {
  completeReplayer,
  detectWithdrawnReferenceInputs,
  requireCompleteCanonicalReplayBundle,
} from "./complete-replay.detect-double-withdraws.js";
import { detectWithdrawnInputs } from "./complete-replay.detect-min-ada.js";
import {
  admittedDecisions,
  COMPLETE_CANONICAL_REPLAY,
  type CompleteCanonicalReplay,
  type CompleteCanonicalReplayContext,
  type CompleteCanonicalReplayDecision,
  replayContextIdentity,
} from "./complete-replay.replay-context-identity.js";
import { subjectOf } from "./detection-subject.js";
import {
  assertReplayPrerequisiteCovered,
  CanonicalReplayPrerequisiteError,
} from "./replay-prerequisite.js";

export const OBSERVER_ORDER_INVALID_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["observerOrderInvalid"], async (evidence) =>
    detectObserverOrderInvalidCompleteReplay(evidence),
  );

export const REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["redeemerCanonicity"],
  async (evidence) =>
    detectRedeemerCanonicityCompleteReplay(evidence).map((detection) => ({
      ...subjectOf(detection),
      detectionId: detection.detectionId,
      headerHash: detection.headerHash,
      violationId: "redeemer-malformed",
      position: detection.position,
      diagnostic: "authenticated redeemer item is not canonical Plutus Data",
    })),
);

/** Complete accepted-spend/withdrawal intersection scan for one block. */
export const WITHDRAWN_INPUT_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["withdrawnInput"],
  detectWithdrawnInputs,
);

/** Complete accepted-reference/withdrawal intersection scan for one block. */
export const WITHDRAWN_REFERENCE_INPUT_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["withdrawnReferenceInput"], detectWithdrawnReferenceInputs);

/** Closed union used once both family adapters are launch-scope complete. */
export const DOUBLE_SPEND_NETWORK_ID_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["doubleSpend", "networkId"], async (evidence) => [
    ...(await detectDoubleSpends(evidence)),
    ...detectNetworkIds(evidence),
  ]);

/**
 * Builds one closed replay bundle from already-admitted complete family
 * replayers. The application installs this object, rather than a list of
 * detector callbacks, so no caller can omit a family or inject a partial scan
 * after deployment composition. Categories must be disjoint and in the
 * append-only catalogue order.
 */
export const createCompleteCanonicalReplayUnion = (
  members: readonly CompleteCanonicalReplay[],
): CompleteCanonicalReplay => {
  if (members.length === 0) {
    throw new Error("complete replay union must contain at least one member");
  }
  const categories: FraudProofCatalogueCategoryName[] = [];
  const seen = new Set<FraudProofCatalogueCategoryName>();
  for (const member of members) {
    const memberScope = requireCompleteCanonicalReplayBundle(member);
    for (const category of memberScope) {
      if (seen.has(category)) {
        throw new Error(`complete replay union duplicates ${category}`);
      }
      seen.add(category);
      categories.push(category);
    }
  }
  const canonical = [...categories].sort(
    (left, right) =>
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.indexOf(left) -
      FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.indexOf(right),
  );
  if (canonical.some((category, index) => category !== categories[index])) {
    throw new Error(
      "complete replay union members are not in canonical catalogue order",
    );
  }
  return completeReplayer(
    Object.freeze(categories),
    async (evidence, context) => {
      const detections: CanonicalViolationDetection[] = [];
      const prerequisites: CanonicalReplayPrerequisiteError[] = [];
      for (const member of members) {
        try {
          const decision = await member.replay(evidence, context);
          detections.push(
            ...requireCompleteCanonicalReplayDecision({
              evidence,
              replayer: member,
              decision,
              ...(context === undefined ? {} : { context }),
            }),
          );
        } catch (error) {
          if (!(error instanceof CanonicalReplayPrerequisiteError)) throw error;
          prerequisites.push(error);
          detections.push(...error.detections);
        }
      }
      for (const prerequisite of prerequisites)
        for (const failure of prerequisite.failures)
          assertReplayPrerequisiteCovered(evidence, failure, detections);
      return detections;
    },
  );
};

/** Rejects caller-authored or partial-detector decisions at runtime. */
export const requireCompleteCanonicalReplayDecision = ({
  evidence,
  replayer,
  decision,
  context,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly replayer: CompleteCanonicalReplay;
  readonly decision: CompleteCanonicalReplayDecision;
  readonly context?: CompleteCanonicalReplayContext;
}): readonly CanonicalViolationDetection[] => {
  requireCompleteCanonicalReplayBundle(replayer);
  if (!admittedDecisions.has(decision)) {
    throw new Error(
      "canonical replay decision was not produced by the closed replay bundle",
    );
  }
  const expectedContext = replayContextIdentity({ evidence, context });
  if (
    decision.replayVersion !== COMPLETE_CANONICAL_REPLAY ||
    decision.launchScope !== replayer.launchScope ||
    decision.headerHash !== evidence.headerHash ||
    decision.payloadEnvelopeSha256 !== evidence.payloadEnvelopeSha256 ||
    decision.payloadSha256 !== evidence.payloadSha256 ||
    JSON.stringify(decision.context) !== JSON.stringify(expectedContext)
  ) {
    throw new Error(
      "canonical replay decision does not bind its bundle and fetched evidence",
    );
  }
  return decision.detections;
};

/**
 * Digest of a module-admitted complete replay decision. The admission check is
 * part of this operation so a structural/caller-authored decision can never
 * mint the digest used by production capture artifacts.
 */
export const completeCanonicalReplayDecisionDigest = ({
  evidence,
  replayer,
  decision,
  context,
}: {
  readonly evidence: CanonicalBlockEvidence;
  readonly replayer: CompleteCanonicalReplay;
  readonly decision: CompleteCanonicalReplayDecision;
  readonly context?: CompleteCanonicalReplayContext;
}): string => {
  requireCompleteCanonicalReplayDecision({
    evidence,
    replayer,
    decision,
    ...(context === undefined ? {} : { context }),
  });
  return createHash("sha256")
    .update(
      JSON.stringify(canonicalizeReplayJson(replayDecisionJson(decision))),
    )
    .digest("hex");
};

export const VALUE_NOT_PRESERVED_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["valueNotPreserved"],
  async (evidence, context) =>
    detectValueConservationFaults({
      block: evidence,
      predecessor: completeCanonicalReplayPredecessorLedger({
        evidence,
        context,
      }),
    }),
);

export const WITHDRAWAL_MISTAG_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["withdrawalMistag"],
  async (evidence, context) =>
    await detectWithdrawalMistagReplay({
      block: evidence,
      predecessor: completeCanonicalReplayPredecessorLedger({
        evidence,
        context,
      }),
    }),
);

export const MINT_AUTHORIZATION_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["mintAuthorization"],
  async (evidence, context) =>
    await detectMintAuthorizationReplay({
      block: evidence,
      predecessor: completeCanonicalReplayPredecessorLedger({
        evidence,
        context,
      }),
    }),
);

/** Complete transition replay derives every witness from the admitted predecessor and freshly admitted raw L1. */
export const createTransitionTraceCompleteCanonicalReplayFromRetainedHistory = (
  predecessor: () => CheckedLedger | undefined,
  l1Events: () => TransitionTraceL1Events,
): CompleteCanonicalReplay =>
  completeReplayer(["transitionTrace"], async (evidence) => {
    const replay = await replayTransitionTraceFromRetainedHistory({
      evidence,
      predecessor: predecessor(),
      l1Events: l1Events(),
    });
    return transitionTraceCanonicalDetections(evidence, replay.detections);
  });

export const TRANSITION_TRACE_COMPLETE_CANONICAL_REPLAY = completeReplayer(
  ["transitionTrace"],
  async (evidence, context) => {
    if (context?.transitionTraceEvents === undefined)
      throw new Error(
        "Transition complete replay requires freshly admitted raw L1 events",
      );
    const replay = await replayTransitionTraceFromRetainedHistory({
      evidence,
      predecessor: completeCanonicalReplayPredecessorLedger({
        evidence,
        context,
      }),
      l1Events: context.transitionTraceEvents,
    });
    return transitionTraceCanonicalDetections(evidence, replay.detections);
  },
);

/** Accepted mint grammar and ordering, disjoint from field shape and width. */
export const MINT_ITEM_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY =
  completeReplayer(["mintItemNonCanonical"], async (evidence) =>
    detectMintItemNonCanonicalCompleteReplay(evidence),
  );
