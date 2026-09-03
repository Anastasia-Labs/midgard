import { createHash } from "node:crypto";

import { normalizeDaDeploymentFingerprintHex } from "@al-ft/midgard-core/da-transport";
import {
  admitAuthenticatedStateQueueHeaderObservation,
  type AuthenticatedStateQueueHeaderObservation,
  CANONICAL_DECODABILITY_VIOLATION_ID,
  DA_HASH_PREIMAGE_VIOLATION_ID,
  EMPTY_MERKLE_TREE_ROOT,
  FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER,
  type FraudProofCatalogueCategoryName,
  Header,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import {
  fetchFraudProofEvidence,
  FRAUD_PROOF_EVIDENCE_ROUTE,
} from "../evidence/fraud-proof-evidence.js";
import { FIELD_PREIMAGE_LENGTH_MISMATCH_VIOLATION_ID } from "../field-preimage-length-mismatch/evidence.js";
import {
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import {
  type CanonicalBlockClassification,
  type CanonicalViolationDetection,
  classifyCanonicalBlockViolations,
} from "./classification.js";
import {
  admitCompleteCanonicalReplayHistoricalCorpus,
  admitCompleteCanonicalReplayPredecessor,
  COMPLETE_CANONICAL_REPLAY,
  type CompleteCanonicalReplay,
  type CompleteCanonicalReplayContext,
  completeCanonicalReplayDecisionDigest,
  requireCompleteCanonicalReplayBundle,
  requireCompleteCanonicalReplayDecision,
} from "./complete-replay.js";
import {
  type HistoricalNativeScriptCheckpointStore,
  type HistoricalNativeScriptHistorySource,
  requireHistoricalNativeScriptHistoryAuthority,
  resolveHistoricalNativeScriptCorpus,
} from "./historical-native-script-corpus.js";
import {
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  type FraudProofReleaseFinalityAuthority,
  validateVerifiedFraudProofReleaseFinalityPolicy,
} from "./release-finality-policy.js";

export const HEADER_CLASSIFIER =
  "midgard-production-header-classifier-v1" as const;
export const HEADER_DECISION = "midgard-production-header-decision-v1" as const;
export const PREDECESSOR_CONTEXT_REQUIRED =
  "production-predecessor-context-required-v1" as const;
const MINT_DECLARED_ASSET_LIMIT_VIOLATION_ID =
  "mint-declared-asset-limit" as const;

const HEX_32 = /^[0-9a-f]{64}$/u;

type CanonicalJson =
  | null
  | boolean
  | number
  | string
  | readonly CanonicalJson[]
  | { readonly [key: string]: CanonicalJson };

const canonicalize = (value: CanonicalJson): CanonicalJson => {
  if (Array.isArray(value)) return value.map(canonicalize);
  if (typeof value !== "object" || value === null) return value;
  return Object.freeze(
    Object.fromEntries(
      Object.entries(value)
        .sort(([left], [right]) => (left < right ? -1 : left > right ? 1 : 0))
        .map(([key, child]) => [key, canonicalize(child)]),
    ),
  );
};

const digest = (value: CanonicalJson): string =>
  createHash("sha256")
    .update(JSON.stringify(canonicalize(value)))
    .digest("hex");

type DetectionJson = Readonly<{
  detectionId: string;
  headerHash: string;
  violationId: string;
  position: string;
  diagnostic: string | null;
}>;

const detectionJson = (
  detection: CanonicalViolationDetection,
): DetectionJson => ({
  detectionId: detection.detectionId,
  headerHash: detection.headerHash,
  violationId: detection.violationId,
  position: detection.position.toString(),
  diagnostic: detection.diagnostic ?? null,
});

const classificationJson = (
  classification: CanonicalBlockClassification,
): CanonicalJson => ({
  schemaVersion: classification.schemaVersion,
  decision: classification.decision,
  headerHash: classification.headerHash,
  category:
    classification.decision === "fault_detected"
      ? classification.category
      : null,
  selected:
    classification.decision === "no_fault_detected"
      ? null
      : detectionJson(classification.selected),
  detections: classification.detections.map(detectionJson),
  unprovableGaps: classification.unprovableGaps.map((gap) => ({
    ...detectionJson(gap),
    reason: gap.reason,
  })),
});

/**
 * Value identity of the exact authenticated state-queue observation supplied
 * by the L1 source extractor. Header CBOR is used instead of object key order,
 * and the source mode/provenance/chain point/depth remain explicit.
 */
export const authenticatedStateQueueObservationDigest = async ({
  observation,
  minimumConfirmationDepth,
}: {
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  readonly minimumConfirmationDepth: number;
}): Promise<string> => {
  const admitted = await admitAuthenticatedStateQueueHeaderObservation({
    observation,
    minimumConfirmationDepth,
  });
  return digest({
    schemaVersion: admitted.schemaVersion,
    sourceMode: admitted.sourceMode,
    provenance: {
      trustClass: admitted.provenance.trustClass,
      sourceId: admitted.provenance.sourceId,
      grade: admitted.provenance.grade,
      diagnosticLabel: admitted.provenance.diagnosticLabel ?? null,
    },
    chainPoint: {
      slot: admitted.chainPoint.slot.toString(),
      blockHash: admitted.chainPoint.blockHash,
    },
    confirmationDepth: admitted.confirmationDepth,
    headerHash: admitted.headerHash,
    headerCbor: Data.to(admitted.header, Header),
  });
};

type CommonDecision = Readonly<{
  schemaVersion: typeof HEADER_DECISION;
  classifierVersion: typeof HEADER_CLASSIFIER;
  deploymentFingerprint: string;
  headerHash: string;
  authenticatedObservationDigest: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
  replayVersion: typeof COMPLETE_CANONICAL_REPLAY;
  replayDigest: string;
  launchScope: readonly FraudProofCatalogueCategoryName[];
  launchScopeDigest: string;
  classificationDigest: string;
  decisionDigest: string;
}>;

export type HeaderFaultDecision = CommonDecision &
  Readonly<{
    decision: "fault_detected";
    category: FraudProofCatalogueCategoryName;
    violationId: string;
    detectionId: string;
    position: string;
  }>;

export type HeaderHealthyDecision = CommonDecision &
  Readonly<{ decision: "healthy" }>;

export type HeaderUnprovableDecision = CommonDecision &
  Readonly<{
    decision: "unprovable";
    reason:
      | "unregistered_violation"
      | "category_not_installed"
      | "predecessor_context_unavailable";
    violationId: string;
    detectionId: string;
    position: string;
  }>;

export type HeaderDecision =
  | HeaderFaultDecision
  | HeaderHealthyDecision
  | HeaderUnprovableDecision;

type UnsealedHeaderDecision =
  | Omit<HeaderFaultDecision, "decisionDigest">
  | Omit<HeaderHealthyDecision, "decisionDigest">
  | Omit<HeaderUnprovableDecision, "decisionDigest">;

export interface HeaderClassifier {
  readonly classifierVersion: typeof HEADER_CLASSIFIER;
  readonly deploymentFingerprint: string;
  readonly launchScope: readonly FraudProofCatalogueCategoryName[];
}

const admittedClassifiers = new WeakMap<
  object,
  Readonly<{
    replayer: CompleteCanonicalReplay;
    confirmationDepth: number;
    historicalReplayAuthority?: Readonly<{
      checkpointStore: HistoricalNativeScriptCheckpointStore;
      historySource: HistoricalNativeScriptHistorySource;
    }>;
  }>
>();
const admittedDecisions = new WeakSet<object>();
const replayContextByDecision = new WeakMap<
  object,
  CompleteCanonicalReplayContext
>();

const exactCanonicalScope = (
  scope: readonly FraudProofCatalogueCategoryName[],
): readonly FraudProofCatalogueCategoryName[] => {
  if (scope.length === 0 || new Set(scope).size !== scope.length) {
    throw new Error("production classifier launch scope is empty or duplicate");
  }
  let prior = -1;
  for (const category of scope) {
    const position = FRAUD_PROOF_CATALOGUE_CATEGORY_ORDER.indexOf(category);
    if (position <= prior) {
      throw new Error(
        "production classifier launch scope is not in canonical catalogue order",
      );
    }
    prior = position;
  }
  return Object.freeze([...scope]);
};

export const createHeaderClassifier = async ({
  deploymentFingerprint,
  replayer,
  releaseFinalityAuthority,
  historicalReplayAuthority,
}: {
  readonly deploymentFingerprint: string;
  readonly replayer: CompleteCanonicalReplay;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  readonly historicalReplayAuthority?: Readonly<{
    checkpointStore: HistoricalNativeScriptCheckpointStore;
    historySource: HistoricalNativeScriptHistorySource;
  }>;
}): Promise<HeaderClassifier> => {
  const normalizedDeploymentFingerprint = normalizeDaDeploymentFingerprintHex(
    deploymentFingerprint,
  );
  requireCompleteCanonicalReplayBundle(replayer);
  const requiresHistoricalReplay = replayer.launchScope.some((category) =>
    ["resolvedOutputNonCanonical", "spendInputSignerMissing"].includes(
      category,
    ),
  );
  if (requiresHistoricalReplay && historicalReplayAuthority === undefined) {
    throw new Error(
      replayer.launchScope.includes("resolvedOutputNonCanonical")
        ? "resolved-output complete replay requires an admitted historical replay authority"
        : "spend-input-signer complete replay requires an admitted historical replay authority",
    );
  }
  if (historicalReplayAuthority !== undefined) {
    requireHistoricalNativeScriptHistoryAuthority({
      deploymentFingerprint: normalizedDeploymentFingerprint,
      ...historicalReplayAuthority,
    });
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
      confirmationDepth: releaseFinality.policy.confirmationDepth,
      ...(historicalReplayAuthority === undefined
        ? {}
        : { historicalReplayAuthority }),
    }),
  );
  return classifier;
};

const sealDecision = (
  decision: UnsealedHeaderDecision,
  replayContext?: CompleteCanonicalReplayContext,
): HeaderDecision => {
  const decisionDigest = digest(decision as CanonicalJson);
  const sealed: HeaderDecision = Object.freeze({
    ...decision,
    decisionDigest,
  });
  admittedDecisions.add(sealed);
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
 * One authenticated header in, one public-DA fetch, one installed replay
 * union, and no caller-selected category. The returned object is usable as a
 * new runnable job authority only while its module-private admission survives;
 * its full value and digest may be persisted for journal-authorized recovery.
 */
export const classifyHeader = async ({
  classifier,
  observation,
  authenticatedObservationDigest,
  sources,
  retries,
  replayContext,
  predecessorObservation,
}: {
  readonly classifier: HeaderClassifier;
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  readonly authenticatedObservationDigest: string;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly retries?: number;
  readonly replayContext?: CompleteCanonicalReplayContext;
  /**
   * Opaque L1-admitted predecessor header. Its retained-DA bytes are fetched
   * here through the same concrete public sources as the challenged block.
   */
  readonly predecessorObservation?: AuthenticatedStateQueueHeaderObservation;
}): Promise<HeaderDecision> => {
  const authority = admittedClassifiers.get(classifier);
  if (authority === undefined) {
    throw new Error("production header classifier was not module-admitted");
  }
  const observationDigest = await authenticatedStateQueueObservationDigest({
    observation,
    minimumConfirmationDepth: authority.confirmationDepth,
  });
  if (
    !HEX_32.test(authenticatedObservationDigest) ||
    authenticatedObservationDigest !== observationDigest
  ) {
    throw new Error(
      "L1 source observation digest differs from the admitted observation",
    );
  }
  const routed = await fetchFraudProofEvidence({
    observation,
    sources,
    ...(retries === undefined ? {} : { retries }),
    minimumConfirmationDepth: authority.confirmationDepth,
  });
  if (routed.schemaVersion !== FRAUD_PROOF_EVIDENCE_ROUTE) {
    throw new Error("production evidence route version changed");
  }
  const launchScopeDigest = digest(classifier.launchScope);
  if (routed.kind === "observers_forbidden_on_untagged_network") {
    const selected: CanonicalViolationDetection = {
      detectionId: routed.selected.detectionId,
      headerHash: routed.selected.headerHash,
      violationId: "observers-forbidden-on-untagged-network",
      position: routed.selected.position,
    };
    const installed = classifier.launchScope.includes(
      "observersForbiddenOnUntaggedNetwork",
    );
    const classification = {
      decision: installed ? "fault_detected" : "unprovable",
      selected: detectionJson(selected),
      reason: installed ? null : "category_not_installed",
    } as const;
    const common = {
      schemaVersion: HEADER_DECISION,
      classifierVersion: HEADER_CLASSIFIER,
      deploymentFingerprint: classifier.deploymentFingerprint,
      headerHash: routed.evidence.headerHash,
      authenticatedObservationDigest: observationDigest,
      payloadEnvelopeSha256: routed.evidence.payloadEnvelopeSha256,
      payloadSha256: routed.evidence.payloadSha256,
      replayVersion: COMPLETE_CANONICAL_REPLAY,
      replayDigest: digest({
        route: "authenticated_observers_forbidden_raw_v1",
        launchScope: classifier.launchScope,
        selected: detectionJson(selected),
      }),
      launchScope: classifier.launchScope,
      launchScopeDigest,
      classificationDigest: digest(classification),
    } as const;
    return installed
      ? sealDecision({
          ...common,
          decision: "fault_detected",
          category: "observersForbiddenOnUntaggedNetwork",
          violationId: selected.violationId,
          detectionId: selected.detectionId,
          position: selected.position.toString(),
        })
      : sealDecision({
          ...common,
          decision: "unprovable",
          reason: "category_not_installed",
          violationId: selected.violationId,
          detectionId: selected.detectionId,
          position: selected.position.toString(),
        });
  }
  if (routed.kind === "mint_declared_asset_limit") {
    const selected: CanonicalViolationDetection = {
      detectionId: routed.selected.detectionId,
      headerHash: routed.selected.headerHash,
      violationId: MINT_DECLARED_ASSET_LIMIT_VIOLATION_ID,
      position: routed.selected.position,
    };
    const installed = classifier.launchScope.includes("mintDeclaredAssetLimit");
    const classification = {
      decision: installed ? "fault_detected" : "unprovable",
      selected: detectionJson(selected),
      reason: installed ? null : "category_not_installed",
    } as const;
    const common = {
      schemaVersion: HEADER_DECISION,
      classifierVersion: HEADER_CLASSIFIER,
      deploymentFingerprint: classifier.deploymentFingerprint,
      headerHash: routed.evidence.headerHash,
      authenticatedObservationDigest: observationDigest,
      payloadEnvelopeSha256: routed.evidence.payloadEnvelopeSha256,
      payloadSha256: routed.evidence.payloadSha256,
      replayVersion: COMPLETE_CANONICAL_REPLAY,
      replayDigest: digest({
        route: "authenticated_mint_declared_asset_limit_v1",
        launchScope: classifier.launchScope,
        selected: detectionJson(selected),
      }),
      launchScope: classifier.launchScope,
      launchScopeDigest,
      classificationDigest: digest(classification),
    } as const;
    return installed
      ? sealDecision({
          ...common,
          decision: "fault_detected",
          category: "mintDeclaredAssetLimit",
          violationId: selected.violationId,
          detectionId: selected.detectionId,
          position: selected.position.toString(),
        })
      : sealDecision({
          ...common,
          decision: "unprovable",
          reason: "category_not_installed",
          violationId: selected.violationId,
          detectionId: selected.detectionId,
          position: selected.position.toString(),
        });
  }
  if (routed.kind === "canonical_decodability") {
    const selected: CanonicalViolationDetection = {
      detectionId: `${CANONICAL_DECODABILITY_VIOLATION_ID}:${routed.evidence.selected.transactionIndex.toString()}:${routed.evidence.selected.nodeTxId}:${routed.evidence.selected.fieldIndex.toString()}:${routed.evidence.selected.verdict.toString()}`,
      headerHash: routed.evidence.headerHash,
      violationId: CANONICAL_DECODABILITY_VIOLATION_ID,
      position: BigInt(routed.evidence.selected.transactionIndex),
    };
    const installed = classifier.launchScope.includes("canonicalDecodability");
    const classification = {
      decision: installed ? "fault_detected" : "unprovable",
      selected: detectionJson(selected),
      reason: installed ? null : "category_not_installed",
    } as const;
    const common = {
      schemaVersion: HEADER_DECISION,
      classifierVersion: HEADER_CLASSIFIER,
      deploymentFingerprint: classifier.deploymentFingerprint,
      headerHash: routed.evidence.headerHash,
      authenticatedObservationDigest: observationDigest,
      payloadEnvelopeSha256: routed.evidence.payloadEnvelopeSha256,
      payloadSha256: routed.evidence.payloadSha256,
      replayVersion: COMPLETE_CANONICAL_REPLAY,
      replayDigest: digest({
        route: "authenticated_canonical_decodability_field_v1",
        launchScope: classifier.launchScope,
        selected: detectionJson(selected),
      }),
      launchScope: classifier.launchScope,
      launchScopeDigest,
      classificationDigest: digest(classification),
    } as const;
    return installed
      ? sealDecision({
          ...common,
          decision: "fault_detected",
          category: "canonicalDecodability",
          violationId: selected.violationId,
          detectionId: selected.detectionId,
          position: selected.position.toString(),
        })
      : sealDecision({
          ...common,
          decision: "unprovable",
          reason: "category_not_installed",
          violationId: selected.violationId,
          detectionId: selected.detectionId,
          position: selected.position.toString(),
        });
  }
  if (routed.kind === "da_hash_preimage") {
    const selected: CanonicalViolationDetection = {
      detectionId: `${DA_HASH_PREIMAGE_VIOLATION_ID}:${routed.plan.violation.index.toString()}:${routed.plan.violation.committedTxId}:${routed.plan.violation.verdict.toString()}`,
      headerHash: routed.evidence.headerHash,
      violationId: DA_HASH_PREIMAGE_VIOLATION_ID,
      position: BigInt(routed.plan.violation.index),
    };
    const installed = classifier.launchScope.includes("daHashPreimage");
    const classification = {
      decision: installed ? "fault_detected" : "unprovable",
      selected: detectionJson(selected),
      reason: installed ? null : "category_not_installed",
    } as const;
    const common = {
      schemaVersion: HEADER_DECISION,
      classifierVersion: HEADER_CLASSIFIER,
      deploymentFingerprint: classifier.deploymentFingerprint,
      headerHash: routed.evidence.headerHash,
      authenticatedObservationDigest: observationDigest,
      payloadEnvelopeSha256: routed.evidence.payloadEnvelopeSha256,
      payloadSha256: routed.evidence.payloadSha256,
      replayVersion: COMPLETE_CANONICAL_REPLAY,
      replayDigest: digest({
        route: "authenticated_da_hash_preimage_v1",
        launchScope: classifier.launchScope,
        selected: detectionJson(selected),
      }),
      launchScope: classifier.launchScope,
      launchScopeDigest,
      classificationDigest: digest(classification),
    } as const;
    return installed
      ? sealDecision({
          ...common,
          decision: "fault_detected",
          category: "daHashPreimage",
          violationId: selected.violationId,
          detectionId: selected.detectionId,
          position: selected.position.toString(),
        })
      : sealDecision({
          ...common,
          decision: "unprovable",
          reason: "category_not_installed",
          violationId: selected.violationId,
          detectionId: selected.detectionId,
          position: selected.position.toString(),
        });
  }
  if (routed.kind === "field_preimage_length_mismatch") {
    const selected: CanonicalViolationDetection = {
      detectionId: `${FIELD_PREIMAGE_LENGTH_MISMATCH_VIOLATION_ID}:${routed.evidence.position.toString()}:${routed.evidence.prepared.transactionId}:${routed.evidence.prepared.fieldIndex.toString()}:${routed.evidence.prepared.direction}`,
      headerHash: routed.evidence.prepared.headerHash,
      violationId: FIELD_PREIMAGE_LENGTH_MISMATCH_VIOLATION_ID,
      position: routed.evidence.position,
    };
    const installed = classifier.launchScope.includes(
      "fieldPreimageLengthMismatch",
    );
    const classification = {
      decision: installed ? "fault_detected" : "unprovable",
      selected: detectionJson(selected),
      reason: installed ? null : "category_not_installed",
    } as const;
    const common = {
      schemaVersion: HEADER_DECISION,
      classifierVersion: HEADER_CLASSIFIER,
      deploymentFingerprint: classifier.deploymentFingerprint,
      headerHash: routed.evidence.prepared.headerHash,
      authenticatedObservationDigest: observationDigest,
      payloadEnvelopeSha256: routed.evidence.payloadEnvelopeSha256,
      payloadSha256: routed.evidence.payloadSha256,
      replayVersion: COMPLETE_CANONICAL_REPLAY,
      replayDigest: digest({
        route: "authenticated_field_preimage_length_mismatch_v1",
        launchScope: classifier.launchScope,
        selected: detectionJson(selected),
      }),
      launchScope: classifier.launchScope,
      launchScopeDigest,
      classificationDigest: digest(classification),
    } as const;
    return installed
      ? sealDecision({
          ...common,
          decision: "fault_detected",
          category: "fieldPreimageLengthMismatch",
          violationId: selected.violationId,
          detectionId: selected.detectionId,
          position: selected.position.toString(),
        })
      : sealDecision({
          ...common,
          decision: "unprovable",
          reason: "category_not_installed",
          violationId: selected.violationId,
          detectionId: selected.detectionId,
          position: selected.position.toString(),
        });
  }

  if (replayContext !== undefined && predecessorObservation !== undefined) {
    throw new Error(
      "production classifier accepts either an admitted replay context or a predecessor observation, never both",
    );
  }
  if (replayContext?.historicalCorpus !== undefined) {
    throw new Error(
      "production classifier rejects caller-supplied historical replay authority",
    );
  }
  let admittedReplayContext = replayContext;
  const predecessorRequired =
    routed.evidence.header.prevUtxosRoot !== EMPTY_MERKLE_TREE_ROOT &&
    authority.replayer.launchScope.some((category) =>
      [
        "nonExistentInput",
        "noReferenceInput",
        "missingNativeScriptUtxo",
        "minAda",
      ].includes(category),
    );
  if (
    admittedReplayContext === undefined &&
    predecessorObservation !== undefined
  ) {
    const predecessorPayload = await fetchRetainedDaPayloadByHeaderHash({
      headerHash: predecessorObservation.headerHash,
      sources,
      ...(retries === undefined ? {} : { retries }),
    });
    admittedReplayContext = Object.freeze({
      predecessor: await admitCompleteCanonicalReplayPredecessor({
        value: Object.freeze({
          observation: predecessorObservation,
          payloadEnvelopeCborHex:
            predecessorPayload.payloadEnvelopeCbor.toString("hex"),
          daProvenance: predecessorPayload.provenance,
        }),
        currentEvidence: routed.evidence,
        minimumConfirmationDepth: authority.confirmationDepth,
      }),
    });
  }
  if (predecessorRequired && admittedReplayContext === undefined) {
    const selected: CanonicalViolationDetection = {
      detectionId: `${PREDECESSOR_CONTEXT_REQUIRED}:0:${routed.evidence.header.prevHeaderHash}`,
      headerHash: routed.evidence.headerHash,
      violationId: PREDECESSOR_CONTEXT_REQUIRED,
      position: 0n,
      diagnostic:
        "complete replay requires the authenticated predecessor header and public retained-DA payload",
    };
    const classification = {
      decision: "unprovable",
      reason: "predecessor_context_unavailable",
      selected: detectionJson(selected),
    } as const;
    return sealDecision({
      schemaVersion: HEADER_DECISION,
      classifierVersion: HEADER_CLASSIFIER,
      deploymentFingerprint: classifier.deploymentFingerprint,
      headerHash: routed.evidence.headerHash,
      authenticatedObservationDigest: observationDigest,
      payloadEnvelopeSha256: routed.evidence.payloadEnvelopeSha256,
      payloadSha256: routed.evidence.payloadSha256,
      replayVersion: COMPLETE_CANONICAL_REPLAY,
      replayDigest: digest({
        route: "predecessor_context_unavailable_v1",
        launchScope: classifier.launchScope,
        headerHash: routed.evidence.headerHash,
        prevHeaderHash: routed.evidence.header.prevHeaderHash,
        prevUtxosRoot: routed.evidence.header.prevUtxosRoot,
      }),
      launchScope: classifier.launchScope,
      launchScopeDigest,
      classificationDigest: digest(classification),
      decision: "unprovable",
      reason: "predecessor_context_unavailable",
      violationId: selected.violationId,
      detectionId: selected.detectionId,
      position: selected.position.toString(),
    });
  }
  if (
    classifier.launchScope.some((category) =>
      ["resolvedOutputNonCanonical", "spendInputSignerMissing"].includes(
        category,
      ),
    )
  ) {
    const historicalAuthority = authority.historicalReplayAuthority;
    if (historicalAuthority === undefined) {
      throw new Error(
        "historical-output complete replay lost its admitted historical authority",
      );
    }
    const corpus = await resolveHistoricalNativeScriptCorpus({
      deploymentFingerprint: classifier.deploymentFingerprint,
      ...historicalAuthority,
      currentEvidence: routed.evidence,
      sources,
      ...(retries === undefined ? {} : { retries }),
    });
    admittedReplayContext = Object.freeze({
      ...(admittedReplayContext?.predecessor === undefined
        ? {}
        : { predecessor: admittedReplayContext.predecessor }),
      historicalCorpus: admitCompleteCanonicalReplayHistoricalCorpus({
        evidence: routed.evidence,
        corpus,
      }),
    });
  }
  const replayDecision = await authority.replayer.replay(
    routed.evidence,
    admittedReplayContext,
  );
  const detections = requireCompleteCanonicalReplayDecision({
    evidence: routed.evidence,
    replayer: authority.replayer,
    decision: replayDecision,
    ...(admittedReplayContext === undefined
      ? {}
      : { context: admittedReplayContext }),
  });
  const classification = await classifyCanonicalBlockViolations({
    evidence: routed.evidence,
    detections,
    minimumConfirmationDepth: authority.confirmationDepth,
  });
  if (
    classification.decision === "fault_detected" &&
    !classifier.launchScope.includes(classification.category)
  ) {
    throw new Error(
      "admitted replay selected a category outside its installed launch scope",
    );
  }
  const common = {
    schemaVersion: HEADER_DECISION,
    classifierVersion: HEADER_CLASSIFIER,
    deploymentFingerprint: classifier.deploymentFingerprint,
    headerHash: routed.evidence.headerHash,
    authenticatedObservationDigest: observationDigest,
    payloadEnvelopeSha256: routed.evidence.payloadEnvelopeSha256,
    payloadSha256: routed.evidence.payloadSha256,
    replayVersion: COMPLETE_CANONICAL_REPLAY,
    replayDigest: completeCanonicalReplayDecisionDigest({
      evidence: routed.evidence,
      replayer: authority.replayer,
      decision: replayDecision,
      ...(admittedReplayContext === undefined
        ? {}
        : { context: admittedReplayContext }),
    }),
    launchScope: classifier.launchScope,
    launchScopeDigest,
    classificationDigest: digest(classificationJson(classification)),
  } as const;
  if (classification.decision === "no_fault_detected") {
    return sealDecision(
      { ...common, decision: "healthy" },
      admittedReplayContext,
    );
  }
  if (classification.decision === "unprovable_gap") {
    return sealDecision(
      {
        ...common,
        decision: "unprovable",
        reason: "unregistered_violation",
        violationId: classification.selected.violationId,
        detectionId: classification.selected.detectionId,
        position: classification.selected.position.toString(),
      },
      admittedReplayContext,
    );
  }
  return sealDecision(
    {
      ...common,
      decision: "fault_detected",
      category: classification.category,
      violationId: classification.selected.violationId,
      detectionId: classification.selected.detectionId,
      position: classification.selected.position.toString(),
    },
    admittedReplayContext,
  );
};

export const requireRunnableHeaderFault = (
  decision: HeaderDecision,
): HeaderFaultDecision => {
  if (!admittedDecisions.has(decision)) {
    throw new Error("production header decision was not module-admitted");
  }
  if (decision.decision !== "fault_detected") {
    throw new Error(
      `only fault_detected may authorize a runnable job; received=${decision.decision}`,
    );
  }
  return decision;
};

/** Persistable exact envelope; does not recreate runnable admission. */
export const headerDecisionEnvelope = (
  decision: HeaderDecision,
): HeaderDecision => {
  if (!admittedDecisions.has(decision)) {
    throw new Error("production header decision was not module-admitted");
  }
  return decision;
};
