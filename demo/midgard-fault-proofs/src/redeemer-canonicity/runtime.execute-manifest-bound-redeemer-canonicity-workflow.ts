import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { workflowActuationAuthorizingDecisionDigest } from "../workflow/actuation-permit.js";
import { REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { type FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofWorkflowRunResult,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import { type WalletOnlySend } from "../workflow/wallet-only-send-gate.js";
import {
  type ManifestBoundRedeemerCanonicityWorkflow,
  type ManifestBoundRedeemerCanonicityWorkflowConfig,
  REDEEMER_CANONICITY_CONFIG_KEYS,
  type RedeemerWorkflowCore,
} from "./runtime.admit-redeemer-workflow-artifact.js";
import { REDEEMER_CANONICITY_FAMILY_DEFINITION } from "./runtime.capture-redeemer-action.js";

/** Strict manifest/reference binding whose input admits no callback authority. */
export const createManifestBoundRedeemerCanonicityWorkflow = async (
  config: ManifestBoundRedeemerCanonicityWorkflowConfig,
): Promise<ManifestBoundRedeemerCanonicityWorkflow> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...REDEEMER_CANONICITY_CONFIG_KEYS].sort().join("\0")
  )
    throw new Error(
      "redeemerCanonicity production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error("redeemerCanonicity decision digest is malformed");
  const { decisionDigest, ...assemblyConfig } = config;
  const workflow = await assembleManifestBoundFamilyWorkflow(
    REDEEMER_CANONICITY_FAMILY_DEFINITION,
    {
      ...assemblyConfig,
      auxiliaryReferenceScripts: config.referenceScripts.removal,
    },
  );
  return Object.freeze({
    ...(workflow as typeof workflow & RedeemerWorkflowCore),
    decisionDigest,
  });
};

export const executeManifestBoundRedeemerCanonicityWorkflow = async ({
  workflow,
  sources,
  journal,
  walletOnlySend,
}: {
  readonly workflow: ManifestBoundRedeemerCanonicityWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
  readonly walletOnlySend: WalletOnlySend;
}): Promise<FraudProofWorkflowRunResult> => {
  if (
    workflowActuationAuthorizingDecisionDigest(journal) !==
    workflow.decisionDigest
  )
    throw new Error("redeemerCanonicity journal changed decision digest");
  const observation = await observeFraudProofWorkflowHeader(workflow.l1, {
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: ["redeemerCanonicity"],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
    walletOnlySend,
  });
};
