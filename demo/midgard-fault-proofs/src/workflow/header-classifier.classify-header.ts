import {
  type AuthenticatedStateQueueHeaderObservation,
  CANONICAL_DECODABILITY_VIOLATION_ID,
  DA_HASH_PREIMAGE_VIOLATION_ID,
  EMPTY_MERKLE_TREE_ROOT,
  GENESIS_HEADER_HASH,
} from "@al-ft/midgard-sdk";

import {
  fetchFraudProofEvidence,
  FRAUD_PROOF_EVIDENCE_ROUTE,
} from "../evidence/fraud-proof-evidence.js";
import { FIELD_PREIMAGE_LENGTH_MISMATCH_VIOLATION_ID } from "../field-preimage-length-mismatch/evidence.js";
import {
  fetchRetainedDaPayloadByHeaderHash,
  type RetainedDaPayloadSource,
} from "../transition-trace/fetch.js";
import { requireTransitionTraceEventAuthority } from "../transition-trace/l1-events.js";
import { classifyCanonicalBlockViolations } from "./classification.js";
import {
  admitCompleteCanonicalReplayHistoricalCorpus,
  admitCompleteCanonicalReplayPredecessor,
  admitValidationTraceReplayContext,
  COMPLETE_CANONICAL_REPLAY,
  type CompleteCanonicalReplayContext,
  completeCanonicalReplayDecisionDigest,
  requireCompleteCanonicalReplayDecision,
} from "./complete-replay.js";
import {
  admittedClassifiers,
  authenticatedStateQueueObservationDigest,
  classificationJson,
  detectionJson,
  digest,
  HEADER_CLASSIFIER,
  HEADER_DECISION,
  type HeaderClassifier,
  type HeaderDecision,
  HEX_32,
  MINT_DECLARED_ASSET_LIMIT_VIOLATION_ID,
  PREDECESSOR_CONTEXT_REQUIRED,
  type RecordedDetection,
} from "./header-classifier.authenticated-state-queue-observation-digest.js";
import { sealDecision } from "./header-classifier.create-header-classifier.js";
import { resolveHistoricalNativeScriptCorpus } from "./historical-native-script-corpus.js";
import {
  HISTORICAL_CORPUS_REPLAY_CATEGORIES,
  launchScopeRequires,
  PREDECESSOR_LEDGER_PROOF_CATEGORIES,
} from "./replay-requirements.js";

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
    const selected: RecordedDetection = {
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
    const selected: RecordedDetection = {
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
    const selected: RecordedDetection = {
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
    const selected: RecordedDetection = {
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
    const selected: RecordedDetection = {
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
  if (
    replayContext?.historicalCorpus !== undefined ||
    replayContext?.transitionTraceEvents !== undefined
  ) {
    throw new Error(
      "production classifier rejects caller-supplied historical replay authority",
    );
  }
  let admittedReplayContext = replayContext;
  const predecessorRequired =
    (authority.replayer.launchScope.includes("validationTraceDispute") &&
      routed.evidence.header.prevHeaderHash !== GENESIS_HEADER_HASH) ||
    (routed.evidence.header.prevUtxosRoot !== EMPTY_MERKLE_TREE_ROOT &&
      launchScopeRequires(
        authority.replayer.launchScope,
        PREDECESSOR_LEDGER_PROOF_CATEGORIES,
      ));
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
    const selected: RecordedDetection = {
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
    launchScopeRequires(
      classifier.launchScope,
      HISTORICAL_CORPUS_REPLAY_CATEGORIES,
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
  if (classifier.launchScope.includes("crossBlockDuplicateEvent")) {
    if (authority.settlementAuthority === undefined)
      throw new Error("cross-block settlement authority was lost");
    admittedReplayContext = Object.freeze({
      ...admittedReplayContext,
      settlements: await authority.settlementAuthority.capture(routed.evidence),
    });
  }
  if (
    classifier.launchScope.includes("transitionTrace") ||
    (classifier.launchScope.includes("validationTraceDispute") &&
      authority.transitionTraceEventAuthority !== undefined)
  ) {
    if (authority.transitionTraceEventAuthority === undefined)
      throw new Error("Transition event authority was lost");
    admittedReplayContext = Object.freeze({
      ...admittedReplayContext,
      transitionTraceEvents: await requireTransitionTraceEventAuthority(
        authority.transitionTraceEventAuthority,
      )(routed.evidence.headerHash),
    });
  }
  if (
    classifier.launchScope.includes("validationTraceDispute") &&
    admittedReplayContext?.validationTraceReplay === undefined
  ) {
    admittedReplayContext = Object.freeze({
      ...admittedReplayContext,
      validationTraceReplay: await admitValidationTraceReplayContext({
        evidence: routed.evidence,
        predecessor: admittedReplayContext?.predecessor,
        transitionTraceEvents: admittedReplayContext?.transitionTraceEvents,
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
      {
        evidence: routed.evidence,
        minimumConfirmationDepth: authority.confirmationDepth,
      },
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
      {
        evidence: routed.evidence,
        minimumConfirmationDepth: authority.confirmationDepth,
      },
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
    {
      evidence: routed.evidence,
      minimumConfirmationDepth: authority.confirmationDepth,
    },
  );
};
