import {
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../evidence/canonical-block-evidence.js";
import {
  requireTransitionTraceEventAuthority,
  type TransitionTraceEventAuthority,
} from "../transition-trace/l1-events.js";
import { requireCompleteCanonicalReplayBundle } from "./complete-replay.detect-double-withdraws.js";
import type {
  CompleteCanonicalReplay,
  CompleteCanonicalReplayContext,
} from "./complete-replay.replay-context-identity.js";
import {
  admittedClassifiers,
  admittedDecisions,
  canonicalInputsByDecision,
  type CanonicalJson,
  digest,
  exactCanonicalScope,
  HEADER_CLASSIFIER,
  type HeaderClassifier,
  type HeaderDecision,
  replayContextByDecision,
  type UnsealedHeaderDecision,
} from "./header-classifier.authenticated-state-queue-observation-digest.js";
import { normalizeHubOraclePolicyId } from "./identity.js";
import {
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  type FraudProofReleaseFinalityAuthority,
  validateVerifiedFraudProofReleaseFinalityPolicy,
} from "./release-finality-policy.js";

export const createHeaderClassifier = async ({
  hubOraclePolicyId,
  replayer,
  releaseFinalityAuthority,
  transitionTraceEventAuthority,
}: {
  readonly hubOraclePolicyId: string;
  readonly replayer: CompleteCanonicalReplay;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  readonly transitionTraceEventAuthority?: TransitionTraceEventAuthority;
}): Promise<HeaderClassifier> => {
  const normalizedHubOraclePolicyId =
    normalizeHubOraclePolicyId(hubOraclePolicyId);
  requireCompleteCanonicalReplayBundle(replayer);
  if (replayer.launchScope.includes("transitionTrace")) {
    if (
      transitionTraceEventAuthority === undefined ||
      transitionTraceEventAuthority.hubOraclePolicyId !==
        normalizedHubOraclePolicyId
    )
      throw new Error(
        "Transition classifier requires admitted raw L1 event authority",
      );
    requireTransitionTraceEventAuthority(transitionTraceEventAuthority);
  }
  if (
    releaseFinalityAuthority.authorityVersion !==
    FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY
  ) {
    throw new Error(
      "production classifier requires the admitted replay and release-finality authorities",
    );
  }
  validateVerifiedFraudProofReleaseFinalityPolicy(
    await releaseFinalityAuthority.verifyForWorkflow({
      hubOraclePolicyId: normalizedHubOraclePolicyId,
    }),
  );
  const classifier: HeaderClassifier = Object.freeze({
    classifierVersion: HEADER_CLASSIFIER,
    hubOraclePolicyId: normalizedHubOraclePolicyId,
    launchScope: exactCanonicalScope(replayer.launchScope),
  });
  admittedClassifiers.set(
    classifier,
    Object.freeze({
      replayer,
      ...(transitionTraceEventAuthority === undefined
        ? {}
        : { transitionTraceEventAuthority }),
      // Classification authorizes reversible fault-proof actions on inclusion.
      confirmationDepth: 1,
    }),
  );
  return classifier;
};

export const sealDecision = (
  decision: UnsealedHeaderDecision,
  replayContext?: CompleteCanonicalReplayContext,
  canonical?: Readonly<{
    evidence: CanonicalBlockEvidence;
    minimumConfirmationDepth: number;
  }>,
): HeaderDecision => {
  const decisionDigest = digest(decision as CanonicalJson);
  const sealed: HeaderDecision = Object.freeze({
    ...decision,
    decisionDigest,
  });
  admittedDecisions.add(sealed);
  if (canonical !== undefined) {
    canonicalInputsByDecision.set(sealed, {
      payloadEnvelopeCbor: Buffer.from(
        canonical.evidence.reconstruction.payloadEnvelopeCbor,
      ),
      observation: structuredClone(canonical.evidence.observation),
      daProvenance: { ...canonical.evidence.provenance.da },
      minimumConfirmationDepth: canonical.minimumConfirmationDepth,
    });
  }
  if (replayContext !== undefined) {
    replayContextByDecision.set(sealed, replayContext);
  }
  return sealed;
};

/**
 * Returns the non-revivable predecessor authority retained for a live
 * classifier decision. Persisted decision envelopes never recreate it.
 */
export const headerDecisionReplayContext = (
  decision: HeaderDecision,
): CompleteCanonicalReplayContext | undefined => {
  if (!admittedDecisions.has(decision)) {
    throw new Error("production header decision was not module-admitted");
  }
  return replayContextByDecision.get(decision);
};

/**
 * Re-admits defensive input copies for a live canonical decision. Routing
 * refusals have no complete canonical evidence. A persisted or copied decision
 * cannot revive this reader. The copy supports value-bound validation selection;
 * historical replay contexts still require their own original evidence object.
 */
export const headerDecisionCanonicalEvidence = async (
  decision: HeaderDecision,
): Promise<CanonicalBlockEvidence | undefined> => {
  if (!admittedDecisions.has(decision))
    throw new Error("production header decision was not module-admitted");
  const inputs = canonicalInputsByDecision.get(decision);
  if (inputs === undefined) return undefined;
  const evidence = await canonicalBlockEvidenceFromVerifiedPayload({
    payloadEnvelopeCbor: Buffer.from(inputs.payloadEnvelopeCbor),
    observation: structuredClone(inputs.observation),
    daProvenance: { ...inputs.daProvenance },
    minimumConfirmationDepth: inputs.minimumConfirmationDepth,
  });
  if (
    evidence.headerHash !== decision.headerHash ||
    evidence.payloadEnvelopeSha256 !== decision.payloadEnvelopeSha256 ||
    evidence.payloadSha256 !== decision.payloadSha256
  )
    throw new Error("live canonical decision inputs changed identity");
  return evidence;
};
