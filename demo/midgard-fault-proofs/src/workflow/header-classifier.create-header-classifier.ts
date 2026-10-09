import { normalizeDaDeploymentFingerprintHex } from "@al-ft/midgard-core/da-transport";

import {
  type CrossBlockSettlementAuthority,
  requireCrossBlockSettlementAuthority,
} from "../cross-block-duplicate-event/settlement-authority.js";
import {
  type CanonicalBlockEvidence,
  canonicalBlockEvidenceFromVerifiedPayload,
} from "../evidence/canonical-block-evidence.js";
import {
  requireTransitionTraceEventAuthority,
  type TransitionTraceEventAuthority,
} from "../transition-trace/l1-events.js";
import {
  type CompleteCanonicalReplay,
  type CompleteCanonicalReplayContext,
  requireCompleteCanonicalReplayBundle,
} from "./complete-replay.js";
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
import {
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  type FraudProofReleaseFinalityAuthority,
  validateVerifiedFraudProofReleaseFinalityPolicy,
} from "./release-finality-policy.js";

export const createHeaderClassifier = async ({
  deploymentFingerprint,
  replayer,
  releaseFinalityAuthority,
  settlementAuthority,
  transitionTraceEventAuthority,
}: {
  readonly deploymentFingerprint: string;
  readonly replayer: CompleteCanonicalReplay;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  readonly settlementAuthority?: CrossBlockSettlementAuthority;
  readonly transitionTraceEventAuthority?: TransitionTraceEventAuthority;
}): Promise<HeaderClassifier> => {
  const normalizedDeploymentFingerprint = normalizeDaDeploymentFingerprintHex(
    deploymentFingerprint,
  );
  requireCompleteCanonicalReplayBundle(replayer);
  if (
    replayer.launchScope.includes("crossBlockDuplicateEvent") &&
    settlementAuthority === undefined
  )
    throw new Error(
      "cross-block duplicate classifier requires live settlement authority",
    );
  if (
    replayer.launchScope.includes("transitionTrace") ||
    (replayer.launchScope.includes("validationTraceDispute") &&
      transitionTraceEventAuthority !== undefined)
  ) {
    if (
      transitionTraceEventAuthority === undefined ||
      transitionTraceEventAuthority.deploymentFingerprint !==
        normalizedDeploymentFingerprint
    )
      throw new Error(
        "Transition classifier requires admitted raw L1 event authority",
      );
    requireTransitionTraceEventAuthority(transitionTraceEventAuthority);
  }
  if (settlementAuthority !== undefined) {
    requireCrossBlockSettlementAuthority(settlementAuthority);
    if (
      settlementAuthority.deploymentFingerprint !==
      normalizedDeploymentFingerprint
    )
      throw new Error("cross-block settlement authority changed deployment");
  }
  if (
    releaseFinalityAuthority.authorityVersion !==
    FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY
  ) {
    throw new Error(
      "production classifier requires the admitted replay and release-finality authorities",
    );
  }
  const releaseFinality = validateVerifiedFraudProofReleaseFinalityPolicy(
    await releaseFinalityAuthority.verifyForWorkflow({
      deploymentFingerprint: normalizedDeploymentFingerprint,
    }),
  );
  if (
    releaseFinality.deploymentIdentityDigest !== normalizedDeploymentFingerprint
  ) {
    throw new Error(
      "production classifier finality authority changed deployment identity",
    );
  }
  const classifier: HeaderClassifier = Object.freeze({
    classifierVersion: HEADER_CLASSIFIER,
    deploymentFingerprint: normalizedDeploymentFingerprint,
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
      ...(settlementAuthority === undefined ? {} : { settlementAuthority }),
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
