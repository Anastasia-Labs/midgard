import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk/fraud-proof";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { planSignerWitnessOpening } from "../signer-witness-scan/field-plans.js";
import {
  SignerWitnessScanDatumSchema,
  SignerWitnessTerminalDatumSchema,
} from "../signer-witness-scan/schemas.js";
import { PROTECTED_OUTPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.resolved-output-non-canonical-complete-canonical-replay.js";
import { CURSOR_FAMILY_TRANSACTION_PORT } from "../workflow/cursor-family-adapter.admit-reference-scripts.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import { assertManifestBoundWorkflowSigner } from "../workflow/deployment-manifest-binding.assert-deployment-info-matches-manifest.js";
import { bindFraudProofWorkflowDeployment } from "../workflow/deployment-manifest-binding.js";
import {
  CORE_WITNESS_ROLES,
  type FamilyDeploymentContext,
} from "../workflow/family-definition.family-adapter-arm.js";
import {
  cursorStepContractNames,
  defineFamily,
} from "../workflow/family-definition.family-definition.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import { proofMaterialsAccess } from "../workflow/proof-materials-view.js";
import { workflowStepTransaction } from "../workflow/step-transaction.js";
import { workflowTransactionInputOutRefs } from "../workflow/transaction-boundary.js";
import { createManifestBoundProtectedOutputSignerMissingSubmission } from "./authenticated-workflow.create-manifest-bound-protected-output-signer-missing-submission.js";
import {
  createProtectedOutputSignerMissingBoundConfig,
  type LoadManifestBoundProtectedOutputSignerMissingConfig,
  type ManifestBoundProtectedOutputSignerMissingConfig,
  PROTECTED_OUTPUT_SIGNER_MISSING_WORKFLOW,
  type ProtectedOutputSignerMissingDeploymentBinding,
} from "./authenticated-workflow.create-protected-output-signer-missing-bound-config.js";
import { createProtectedOutputSignerMissingRawL1StageResolver } from "./authenticated-workflow.create-protected-output-signer-missing-raw-l1-stage-resolver.js";
import {
  type ProtectedOutputSignerMissingAssemblyRuntime,
  protectedOutputSignerStageFromL1,
} from "./authenticated-workflow.derive-protected-output-signer-missing-authenticated-source.js";
import { planProtectedOutputSignerOutputOpening } from "./field-plans.js";
import { ProtectedOutputSignerStep02DatumSchema } from "./schemas.js";
import { PROTECTED_OUTPUT_SIGNER_MISSING_CURSOR_SPEC } from "./workflow-spec.js";

export const loadManifestBoundProtectedOutputSignerMissingConfig = async (
  input: LoadManifestBoundProtectedOutputSignerMissingConfig,
): Promise<ManifestBoundProtectedOutputSignerMissingConfig> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: input.manifest,
    blueprintJson: input.blueprintJson,
    deploymentInfo: input.deploymentInfo,
    category: "protectedOutputSignerMissing",
    headerHash: input.headerHash,
    proverCredential: input.signer.paymentKeyHash,
    stepDatumSchemas:
      PROTECTED_OUTPUT_SIGNER_MISSING_FAMILY_DEFINITION.stepDatumSchemas,
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: input.signer.address,
    proofMaterialsAddress: input.signer.proofMaterialsAddress,
    paymentKeyHash: input.signer.paymentKeyHash,
  });

  return createProtectedOutputSignerMissingBoundConfig(input, binding);
};

export type ManifestBoundProtectedOutputSignerMissingWorkflowConfig =
  LoadManifestBoundProtectedOutputSignerMissingConfig &
    Readonly<{
      l1Source: FraudProofL1Source;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    }>;

/** The bound deployment a workflow's recovery surface is assembled over. */
export type ManifestBoundProtectedOutputSignerMissingInstallation = Readonly<{
  deployment: FamilyDeploymentContext<
    "protectedOutputSignerMissing",
    "computationThreadMint" | "fraudProofMint" | "phasMembershipWithdraw",
    true,
    4
  >;
  workflowVersion: typeof PROTECTED_OUTPUT_SIGNER_MISSING_WORKFLOW;
  config: ManifestBoundProtectedOutputSignerMissingConfig;
  binding: ProtectedOutputSignerMissingDeploymentBinding;
  l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
}>;

export const PROTECTED_OUTPUT_SIGNER_MISSING_FAMILY_DEFINITION = defineFamily<
  "protectedOutputSignerMissing",
  "computationThreadMint" | "fraudProofMint" | "phasMembershipWithdraw",
  true,
  4,
  ProtectedOutputSignerMissingAssemblyRuntime
>({
  category: "protectedOutputSignerMissing",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    ProtectedOutputSignerStep02DatumSchema,
    SignerWitnessScanDatumSchema,
    SignerWitnessTerminalDatumSchema,
  ],
  witnessRoles: CORE_WITNESS_ROLES,
  fieldPreimageCertificate: true,
  fieldGrammarCertificate: true,
  replayer: () => PROTECTED_OUTPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: PROTECTED_OUTPUT_SIGNER_MISSING_CURSOR_SPEC,
    stepContractNames: cursorStepContractNames(
      PROTECTED_OUTPUT_SIGNER_MISSING_CURSOR_SPEC,
    ),
    transactionPort: (context) => {
      const category = "protectedOutputSignerMissing";
      const { config, material } = context.runtime;
      const { binding, l1, stateQueueMutationLeaseCoordinator } = context;
      return {
        portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
        category,
        prepare: material.prepare,
        validatePreparedArtifact: material.validatePreparedArtifact,
        capture: async ({ action, artifact, holder }) => {
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
            step_03: "submitScan",
            step_04: "submitTerminal",
          } as const;
          const familyAction = actions[input.stage as keyof typeof actions];
          if (familyAction === undefined)
            throw new Error(
              `${category} cursor action is outside its exact topology`,
            );
          const submission =
            createManifestBoundProtectedOutputSignerMissingSubmission({
              materials: proofMaterialsAccess({
                signer: config.signer,
                lucid: config.lucid,
                holder,
              }),
              config,
              observe: async () =>
                protectedOutputSignerStageFromL1(
                  (
                    await l1.observe({
                      headerHash: binding.definition.headerHash,
                    })
                  ).stage,
                ),
              resolveStage:
                createProtectedOutputSignerMissingRawL1StageResolver({
                  config,
                  l1,
                  source: admitted.source,
                }),
            });
          const { transaction } = await workflowStepTransaction(
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
    },
  },
  fieldCarriage: [
    {
      requirementForAction: (context, { action, artifact }) => {
        const category = "protectedOutputSignerMissing";
        const { config, material } = context.runtime;
        const { binding } = context;
        if (
          action.input.stage !== "step_02" &&
          action.input.stage !== "step_03"
        )
          return null;
        const { evidence, source } = material.require(artifact);
        const certificate = binding.fieldPreimageCertificate;
        if (certificate === null)
          throw new Error(`${category} omitted field certificate authority`);
        return {
          planned:
            action.input.stage === "step_02"
              ? planProtectedOutputSignerOutputOpening({
                  evidence,
                  nativeTxCompactCbor: source.nativeTxCompactCbor,
                  owner: config.signer.paymentKeyHash,
                  publish: true,
                })
              : planSignerWitnessOpening({
                  evidence,
                  nativeTxCompactCbor: source.nativeTxCompactCbor,
                  witnessSetCompactCbor: source.witnessSetCompactCbor,
                  owner: config.signer.paymentKeyHash,
                  publish: true,
                  label: "protected-output-signer-missing address witnesses",
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
      },
    },
  ],
});
