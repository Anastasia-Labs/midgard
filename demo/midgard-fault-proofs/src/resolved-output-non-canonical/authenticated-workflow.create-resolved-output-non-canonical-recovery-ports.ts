import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { planFaultProofFieldOpening } from "../field-opening.plan-fault-proof-field-opening.js";
import { type CheckedLedger } from "../ledger-at.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { completeCanonicalReplayPredecessorLedger } from "../workflow/complete-replay.admit-complete-canonical-replay-predecessor.js";
import type { CompleteCanonicalReplayContext } from "../workflow/complete-replay.replay-context-identity.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.admit-reference-scripts.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import {
  CORE_WITNESS_ROLES,
  type FamilyDeploymentContext,
  type LinearFamilyPrerequisiteInput,
} from "../workflow/family-definition.family-adapter-arm.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import type { FieldCarriageRequirement } from "../workflow/field-carriage-prerequisite.js";
import { workflowFieldCarriageOwnOutputs } from "../workflow/field-carriage-prerequisite.own-carriage-claims.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import { createCanonicalFamilyArtifactPort } from "../workflow/manifest-bound-family-recovery.js";
import { journaledStepTransaction } from "../workflow/step-transaction.js";
import { workflowTransactionInputOutRefs } from "../workflow/transaction-boundary.js";
import { createManifestBoundResolvedOutputNonCanonicalSubmission } from "./authenticated-workflow.create-manifest-bound-resolved-output-non-canonical-submission.js";
import {
  createResolvedOutputNonCanonicalRawL1StageResolver,
  deriveResolvedOutputNonCanonicalAuthenticatedSource,
} from "./authenticated-workflow.derive-resolved-output-non-canonical-authenticated-source.js";
import {
  type LoadManifestBoundResolvedOutputNonCanonicalConfig,
  type ManifestBoundResolvedOutputNonCanonicalConfig,
  RESOLVED_OUTPUT_NON_CANONICAL_WORKFLOW,
} from "./authenticated-workflow.resolved-output-non-canonical-config-from-binding.js";
import {
  deriveResolvedOutputPriorLedgerReplay,
  detectResolvedOutputNonCanonicalCompleteReplay,
  resolvedOutputOpenedField,
} from "./resolved-output-non-canonical.js";
import { type ResolvedOutputStage } from "./workflow.js";

export type ManifestBoundResolvedOutputNonCanonicalWorkflowConfig =
  LoadManifestBoundResolvedOutputNonCanonicalConfig &
    Readonly<{
      l1Source: FraudProofL1Source;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
      /** The classifier-admitted context carrying the authenticated predecessor. */
      replayContext?: CompleteCanonicalReplayContext;
    }>;

/** The bound deployment a workflow's recovery surface is assembled over. */
export type ManifestBoundResolvedOutputNonCanonicalInstallation = Readonly<{
  deployment: Deployment;
  workflowVersion: typeof RESOLVED_OUTPUT_NON_CANONICAL_WORKFLOW;
  config: ManifestBoundResolvedOutputNonCanonicalConfig;
  binding: FraudProofWorkflowDeploymentBinding<"resolvedOutputNonCanonical">;
  l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
  replayContext?: CompleteCanonicalReplayContext;
}>;

export const resolvedOutputStageFromL1 = (
  stage: Awaited<
    ReturnType<
      FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>["observe"]
    >
  >["stage"],
): ResolvedOutputStage => {
  switch (stage.kind) {
    case "not_started":
      return "none";
    case "step":
      if (stage.step === 1) return "step01";
      if (stage.step === 2) return "step02";
      if (stage.step === 3) return "step03";
      if (stage.step === 4) return "reconstructing";
      if (stage.step === 5) return "step04Datum";
      if (stage.step === 6) return "step05";
      throw new Error(
        "resolvedOutputNonCanonical L1 stage exceeds six-step topology",
      );
    case "proof_token":
      return "proven";
    case "removed":
      return "removed";
  }
};

/** Material re-derived from admitted canonical evidence before durable encoding. */
export const prepareResolvedOutputNonCanonicalRecoveryMaterial = async (
  canonical: CanonicalBlockEvidence,
  detectionId: string,
  predecessor: CheckedLedger | undefined,
) => {
  const priorLedger = await deriveResolvedOutputPriorLedgerReplay({
    block: canonical,
    predecessor,
  });
  const findings = detectResolvedOutputNonCanonicalCompleteReplay({
    block: canonical,
    priorLedger,
  });
  if (findings.length !== 1)
    throw new Error(
      "resolvedOutputNonCanonical public replay requires one exact finding",
    );
  const evidence = findings[0]!;
  const source = await deriveResolvedOutputNonCanonicalAuthenticatedSource({
    block: canonical,
    evidence,
  });
  return {
    category: "resolvedOutputNonCanonical" as const,
    headerHash: canonical.headerHash,
    detectionId,
    evidence,
    source,
  };
};

export const createResolvedOutputNonCanonicalRecoveryPorts = (
  workflow: ManifestBoundResolvedOutputNonCanonicalInstallation,
) => {
  const { config, binding, l1, stateQueueMutationLeaseCoordinator } = workflow;
  const category = "resolvedOutputNonCanonical";
  const material = createCanonicalFamilyArtifactPort(
    async ({ evidence: canonical, classification }) =>
      await prepareResolvedOutputNonCanonicalRecoveryMaterial(
        canonical,
        classification.selected.detectionId,
        completeCanonicalReplayPredecessorLedger({
          evidence: canonical,
          context: workflow.replayContext,
        }),
      ),
  );
  const transactions: CursorFamilyTransactionPort<typeof category> = {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category,
    prepare: material.prepare,
    validatePreparedArtifact: material.validatePreparedArtifact,
    capture: async ({ action, artifact, entries }) => {
      const input = cursorFamilyActionInput({ category, action });
      if (input.stage === "remove")
        return await captureCursorRemoval({
          category,
          lucid: config.lucid,
          blueprint: binding.blueprint,
          deploymentInfo: binding.deploymentInfo,
          network: binding.network,
          signer: config.signer,
          headerHash: binding.definition.headerHash,
          input,
          stateQueueMutationLeaseCoordinator,
          fraudProverRewardLovelace: BigInt(
            binding.releaseEconomics.policy.fraudProverRewardLovelace,
          ),
        });
      const admitted = material.require(artifact);
      const actions = {
        init: "submitInit",
        step_01: "submitStep01",
        step_02: "submitStep02",
        step_03: "submitStep03",
        step_04: "submitReconstruction",
        step_05: "submitStep04Datum",
        step_06: "submitStep05",
      } as const;
      const familyAction = actions[input.stage as keyof typeof actions];
      if (familyAction === undefined)
        throw new Error(
          `${category} cursor action is outside its exact topology`,
        );
      const submission =
        createManifestBoundResolvedOutputNonCanonicalSubmission({
          ownOutputs: workflowFieldCarriageOwnOutputs({
            category,
            entries,
          }),
          config,
          observe: async () =>
            resolvedOutputStageFromL1(
              (
                await l1.observe({
                  headerHash: binding.definition.headerHash,
                })
              ).stage,
            ),
          resolveStage: createResolvedOutputNonCanonicalRawL1StageResolver({
            config,
            l1,
            source: admitted.source,
          }),
        });
      const { transaction } = await journaledStepTransaction(
        await submission.submit(familyAction, admitted.evidence),
        `${category} ${familyAction}`,
      );
      if (
        input.stage !== "init" &&
        !workflowTransactionInputOutRefs(transaction.signed).includes(
          cursorStringField(input, "threadOutRef"),
        )
      )
        throw new Error(
          `${category} captured transaction changed its authenticated thread input`,
        );
      return { transaction };
    },
  };
  const requirementForAction = ({
    action,
    artifact,
  }: LinearFamilyPrerequisiteInput): FieldCarriageRequirement | null => {
    if (action.input.stage !== "step_02") return null;
    const { evidence, source } = material.require(artifact);
    const opened = resolvedOutputOpenedField(evidence);
    if (opened === undefined) return null;
    const certificate = binding.fieldPreimageCertificate;
    if (certificate === null)
      throw new Error(`${category} omitted field certificate authority`);
    return {
      planned: planFaultProofFieldOpening({
        anchorSourceKind: evidence.subject.source_kind === 1n ? 1n : 0n,
        fieldIndex: opened.fieldIndex,
        anchorTxId: evidence.subject.transaction_id,
        nativeTxCompactCbor: source.nativeTxCompactCbor,
        itemCbors: decodeMidgardFieldPreimage(opened.preimage),
        owner: config.signer.paymentKeyHash,
        publish: true,
        label: `${category} field opening`,
      }),
      compactCbor: source.nativeTxCompactCbor,
      witnessSetCompactCbor: source.witnessSetCompactCbor,
      certificate: {
        policyId: certificate.policyId,
        mintingScript: certificate.mintingScript,
        referenceScriptUtxo:
          config.referenceScripts.fieldPreimageCertificateMint,
      },
    };
  };
  return { transactions, requirementForAction };
};

export const WITNESS_ROLES = CORE_WITNESS_ROLES;

type Deployment = FamilyDeploymentContext<
  "resolvedOutputNonCanonical",
  (typeof WITNESS_ROLES)[number],
  true,
  6
>;

export type RunContext = Readonly<{
  workflow: ManifestBoundResolvedOutputNonCanonicalInstallation;
}>;
