import { normalizeDaDeploymentFingerprintHex } from "@al-ft/midgard-core/da-transport";
import {
  admitAuthenticatedStateQueueHeaderObservation,
  type AuthenticatedStateQueueHeaderObservation,
} from "@al-ft/midgard-sdk/fraud-proof";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { canonicalDecodabilityArtifactFromRawEvidence } from "../evidence/canonical-decodability-raw-evidence.js";
import { fetchFraudProofEvidence } from "../evidence/fraud-proof-evidence.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { requireCompleteCanonicalReplayDecision } from "./complete-replay.create-complete-canonical-replay-union.js";
import type {
  CompleteCanonicalReplay,
  CompleteCanonicalReplayContext,
} from "./complete-replay.replay-context-identity.js";
import { normalizeWorkflowJson, type WorkflowJsonObject } from "./identity.js";
import type {
  FraudProofWorkflowRegistry,
  FraudProofWorkflowTerminalVerifier,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import type { FraudProofWorkflowRunResult } from "./orchestrator.fraud-proof-workflow-run-result.js";
import { runAdmittedFraudProofWorkflow } from "./orchestrator.run-admitted-fraud-proof-workflow.js";
import { runFraudProofWorkflow } from "./orchestrator.run-da-hash-preimage-workflow-from-retained-da.js";
import {
  FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
  type FraudProofReleaseFinalityAuthority,
  validateVerifiedFraudProofReleaseFinalityPolicy,
} from "./release-finality-policy.js";
import type { WorkflowRun } from "./workflow-run.js";

/**
 * W-O5 production entry point: fetch the payload only through public retained
 * DA, authenticate it against the L1-observed header, detect/classify locally,
 * then enter the workflow. There is intentionally no REST, database,
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
  run,
  terminalVerifier,
  releaseFinalityAuthority,
  retries,
  maxActions,
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
  readonly run: WorkflowRun;
  readonly terminalVerifier: FraudProofWorkflowTerminalVerifier;
  readonly releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  readonly retries?: number;
  readonly maxActions?: number;
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
    const familyArtifact = normalizeWorkflowJson(
      canonicalDecodabilityArtifactFromRawEvidence(routed.evidence),
    ) as WorkflowJsonObject;
    return await runAdmittedFraudProofWorkflow({
      deploymentFingerprint: normalizedDeploymentFingerprint,
      category: "canonicalDecodability",
      headerHash: routed.evidence.headerHash,
      prepareFamilyArtifact: async () => familyArtifact,
      registry,
      run,
      terminalVerifier,
      releaseFinality,
      ...(maxActions === undefined ? {} : { maxActions }),
    });
  }
  if (routed.kind === "da_hash_preimage") {
    throw new Error(
      "authenticated Q44 source-leaf defect requires the dedicated daHashPreimage workflow",
    );
  }
  if (
    routed.kind === "field_preimage_length_mismatch" ||
    routed.kind === "l2_tx_mistag" ||
    routed.kind === "mint_declared_asset_limit" ||
    routed.kind === "observers_forbidden_on_untagged_network"
  ) {
    const category =
      routed.kind === "field_preimage_length_mismatch"
        ? "fieldPreimageLengthMismatch"
        : routed.kind === "l2_tx_mistag"
          ? "l2TxMistag"
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
    return await runAdmittedFraudProofWorkflow({
      deploymentFingerprint: normalizedDeploymentFingerprint,
      category,
      headerHash,
      prepareFamilyArtifact: async (adapter) => {
        if (adapter.prepareRaw === undefined)
          throw new Error(`${category} lacks authenticated raw preparation`);
        return await adapter.prepareRaw(routed);
      },
      registry,
      run,
      terminalVerifier,
      releaseFinality,
      ...(maxActions === undefined ? {} : { maxActions }),
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
    run,
    terminalVerifier,
    releaseFinalityAuthority: {
      authorityVersion: FRAUD_PROOF_RELEASE_FINALITY_AUTHORITY,
      verifyForWorkflow: async () => releaseFinality,
    },
    ...(maxActions === undefined ? {} : { maxActions }),
  });
};
