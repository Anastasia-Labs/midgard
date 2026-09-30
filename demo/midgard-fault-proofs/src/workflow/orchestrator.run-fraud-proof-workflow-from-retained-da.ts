import { normalizeDaDeploymentFingerprintHex } from "@al-ft/midgard-core/da-transport";
import {
  admitAuthenticatedStateQueueHeaderObservation,
  type AuthenticatedStateQueueHeaderObservation,
} from "@al-ft/midgard-sdk";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { canonicalDecodabilityArtifactFromRawEvidence } from "../evidence/canonical-decodability-raw-evidence.js";
import { fetchFraudProofEvidence } from "../evidence/fraud-proof-evidence.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  type CompleteCanonicalReplay,
  type CompleteCanonicalReplayContext,
  requireCompleteCanonicalReplayDecision,
} from "./complete-replay.js";
import {
  type FraudProofWorkflowJournalStore,
  type JournalJsonObject,
  normalizeJournalJson,
} from "./journal.js";
import {
  type FraudProofWorkflowRegistry,
  type FraudProofWorkflowTerminalVerifier,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import { type FraudProofWorkflowRunResult } from "./orchestrator.fraud-proof-workflow-run-result.js";
import { type WorkflowEvidenceBinding } from "./orchestrator.immutable-fraud-proof-workflow-registry.js";
import { runAdmittedFraudProofWorkflow } from "./orchestrator.run-admitted-fraud-proof-workflow.js";
import { runFraudProofWorkflow } from "./orchestrator.run-da-hash-preimage-workflow-from-retained-da.js";
import {
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  type FraudProofReleaseFinalityAuthority,
  validateVerifiedFraudProofReleaseFinalityPolicy,
} from "./release-finality-policy.js";

/**
 * W-O5 production entry point: fetch the payload only through public retained
 * DA, authenticate it against the L1-observed header, detect/classify locally,
 * then enter the journaled workflow. There is intentionally no REST, database,
 * or local-file evidence option in this API.
 */
export const runFraudProofWorkflowFromRetainedDa = async ({
  deploymentFingerprint,
  observation,
  sources,
  replayer,
  replayContext,
  resolveReplayContext,
  registry,
  journal,
  terminalVerifier,
  releaseFinalityAuthority,
  retries,
  maxSubmissionAttempts,
  maxActions,
  now,
}: {
  readonly deploymentFingerprint: string;
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  readonly sources: readonly RetainedDaPayloadSource[];
  /** Exact closed replay bundle; arbitrary partial detectors are forbidden. */
  readonly replayer: CompleteCanonicalReplay;
  /** Opaque L1/public-DA-admitted predecessor context, when required. */
  readonly replayContext?: CompleteCanonicalReplayContext;
  readonly resolveReplayContext?: (
    evidence: CanonicalBlockEvidence,
  ) => Promise<CompleteCanonicalReplayContext>;
  readonly registry: FraudProofWorkflowRegistry;
  readonly journal: FraudProofWorkflowJournalStore;
  readonly terminalVerifier: FraudProofWorkflowTerminalVerifier;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  readonly retries?: number;
  readonly maxSubmissionAttempts?: number;
  readonly maxActions?: number;
  readonly now?: () => Date;
}): Promise<FraudProofWorkflowRunResult> => {
  const registryScope = [...registry.keys()];
  if (
    registryScope.length !== replayer.launchScope.length ||
    registryScope.some(
      (category, index) => category !== replayer.launchScope[index],
    )
  ) {
    throw new Error(
      `production retained-DA replay launch scope differs from exact workflow registry order: replay=${replayer.launchScope.join(",")} registry=${registryScope.join(",")}`,
    );
  }
  if (
    releaseFinalityAuthority.authorityVersion !==
    FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY
  ) {
    throw new Error(
      "workflow requires the deployment-manifest release finality authority",
    );
  }
  const normalizedDeploymentFingerprint = normalizeDaDeploymentFingerprintHex(
    deploymentFingerprint,
  );
  const releaseFinality = validateVerifiedFraudProofReleaseFinalityPolicy(
    await releaseFinalityAuthority.verifyForWorkflow({
      deploymentFingerprint: normalizedDeploymentFingerprint,
    }),
  );
  if (
    releaseFinality.deploymentIdentityDigest !== normalizedDeploymentFingerprint
  ) {
    throw new Error(
      "release finality authority returned a different deployment identity",
    );
  }
  const admittedObservation =
    await admitAuthenticatedStateQueueHeaderObservation({
      observation,
      minimumConfirmationDepth: 1,
    });
  const routed = await fetchFraudProofEvidence({
    observation: admittedObservation,
    sources,
    ...(retries === undefined ? {} : { retries }),
    minimumConfirmationDepth: 1,
  });
  if (routed.kind === "canonical_decodability") {
    if (
      registryScope.length !== 1 ||
      registryScope[0] !== "canonicalDecodability"
    ) {
      throw new Error(
        `authenticated Q17 committed-field defect requires the exact canonicalDecodability registry; found=${registryScope.join(",")}`,
      );
    }
    const familyArtifact = normalizeJournalJson(
      canonicalDecodabilityArtifactFromRawEvidence(routed.evidence),
    ) as JournalJsonObject;
    const evidenceBinding = normalizeJournalJson({
      route: "authenticated_committed_field_defect",
      headerHash: routed.evidence.headerHash,
      payloadEnvelopeSha256: routed.evidence.payloadEnvelopeSha256,
      payloadSha256: routed.evidence.payloadSha256,
      committedTransactionsRoot: routed.evidence.committedTransactionsRoot,
      l2TransactionCount: routed.evidence.l2TransactionCount.toString(),
      selectedTransactionIndex:
        routed.evidence.selected.transactionIndex.toString(),
      selectedTransactionId: routed.evidence.selected.nodeTxId,
      selectedFieldIndex: routed.evidence.selected.fieldIndex.toString(),
      selectedVerdict: routed.evidence.selected.verdict.toString(),
      l1BlockHash: routed.evidence.l1ChainPoint.blockHash,
      l1Slot: routed.evidence.l1ChainPoint.slot.toString(),
    }) as WorkflowEvidenceBinding;
    return await runAdmittedFraudProofWorkflow({
      deploymentFingerprint: normalizedDeploymentFingerprint,
      category: "canonicalDecodability",
      headerHash: routed.evidence.headerHash,
      evidenceBinding,
      prepareFamilyArtifact: async () => familyArtifact,
      registry,
      journal,
      terminalVerifier,
      releaseFinality,
      ...(maxSubmissionAttempts === undefined ? {} : { maxSubmissionAttempts }),
      ...(maxActions === undefined ? {} : { maxActions }),
      ...(now === undefined ? {} : { now }),
    });
  }
  if (routed.kind === "da_hash_preimage") {
    throw new Error(
      "authenticated Q44 source-leaf defect requires the dedicated daHashPreimage workflow",
    );
  }
  if (
    routed.kind === "field_preimage_length_mismatch" ||
    routed.kind === "mint_declared_asset_limit" ||
    routed.kind === "observers_forbidden_on_untagged_network"
  ) {
    const category =
      routed.kind === "field_preimage_length_mismatch"
        ? "fieldPreimageLengthMismatch"
        : routed.kind === "mint_declared_asset_limit"
          ? "mintDeclaredAssetLimit"
          : "observersForbiddenOnUntaggedNetwork";
    const headerHash =
      routed.kind === "field_preimage_length_mismatch"
        ? routed.evidence.prepared.headerHash
        : routed.evidence.headerHash;
    if (registryScope.length !== 1 || registryScope[0] !== category)
      throw new Error(
        `authenticated raw-family evidence requires exact ${category} registry`,
      );
    const evidenceBinding = normalizeJournalJson({
      route: "authenticated_raw_family",
      category,
      headerHash,
      payloadEnvelopeSha256: routed.evidence.payloadEnvelopeSha256,
      payloadSha256: routed.evidence.payloadSha256,
      l1BlockHash: admittedObservation.chainPoint.blockHash,
      l1Slot: admittedObservation.chainPoint.slot.toString(),
    }) as WorkflowEvidenceBinding;
    return await runAdmittedFraudProofWorkflow({
      deploymentFingerprint: normalizedDeploymentFingerprint,
      category,
      headerHash,
      evidenceBinding,
      prepareFamilyArtifact: async (adapter) => {
        if (adapter.prepareRaw === undefined)
          throw new Error(`${category} lacks authenticated raw preparation`);
        return await adapter.prepareRaw(routed);
      },
      validateFamilyArtifact: async (adapter, artifact) => {
        if (adapter.validatePreparedRawArtifact === undefined)
          throw new Error(
            `${category} lacks authenticated raw artifact revalidation`,
          );
        await adapter.validatePreparedRawArtifact({ routed, artifact });
      },
      registry,
      journal,
      terminalVerifier,
      releaseFinality,
      ...(maxSubmissionAttempts === undefined ? {} : { maxSubmissionAttempts }),
      ...(maxActions === undefined ? {} : { maxActions }),
      ...(now === undefined ? {} : { now }),
    });
  }
  const evidence = routed.evidence;
  if (replayContext !== undefined && resolveReplayContext !== undefined)
    throw new Error(
      "workflow replay context must have one authenticated source",
    );
  const admittedReplayContext =
    resolveReplayContext === undefined
      ? replayContext
      : await resolveReplayContext(evidence);
  const replayDecision = await replayer.replay(evidence, admittedReplayContext);
  const detections = requireCompleteCanonicalReplayDecision({
    evidence,
    replayer,
    decision: replayDecision,
    ...(admittedReplayContext === undefined
      ? {}
      : { context: admittedReplayContext }),
  });
  return await runFraudProofWorkflow({
    deploymentFingerprint: normalizedDeploymentFingerprint,
    evidence,
    detections,
    ...(admittedReplayContext === undefined
      ? {}
      : { replayContext: admittedReplayContext }),
    registry,
    journal,
    terminalVerifier,
    releaseFinalityAuthority: {
      authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
      verifyForWorkflow: async () => releaseFinality,
    },
    ...(maxSubmissionAttempts === undefined ? {} : { maxSubmissionAttempts }),
    ...(maxActions === undefined ? {} : { maxActions }),
    ...(now === undefined ? {} : { now }),
  });
};
