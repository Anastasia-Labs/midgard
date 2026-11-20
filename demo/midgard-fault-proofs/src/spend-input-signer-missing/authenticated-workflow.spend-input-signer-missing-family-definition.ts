import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk/fraud-proof";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { planSignerWitnessOpening } from "../signer-witness-scan/field-plans.js";
import {
  SignerWitnessScanDatumSchema,
  SignerWitnessTerminalDatumSchema,
} from "../signer-witness-scan/schemas.js";
import type { CompleteCanonicalReplayContext } from "../workflow/complete-replay.replay-context-identity.js";
import { SPEND_INPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.resolved-output-non-canonical-complete-canonical-replay.js";
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
import { workflowFieldCarriageOwnOutputs } from "../workflow/field-carriage-prerequisite.own-carriage-claims.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import { journaledStepTransaction } from "../workflow/step-transaction.js";
import { workflowTransactionInputOutRefs } from "../workflow/transaction-boundary.js";
import { createManifestBoundSpendInputSignerMissingSubmission } from "./authenticated-workflow.create-manifest-bound-spend-input-signer-missing-submission.js";
import {
  createSpendInputSignerMissingBoundConfig,
  type LoadManifestBoundSpendInputSignerMissingConfig,
  type ManifestBoundSpendInputSignerMissingConfig,
  SPEND_INPUT_SIGNER_MISSING_WORKFLOW,
  type SpendInputSignerMissingDeploymentBinding,
} from "./authenticated-workflow.create-spend-input-signer-missing-bound-config.js";
import { createSpendInputSignerMissingRawL1StageResolver } from "./authenticated-workflow.create-spend-input-signer-missing-raw-l1-stage-resolver.js";
import {
  type SpendInputSignerMissingAssemblyRuntime,
  spendInputSignerStageFromL1,
} from "./authenticated-workflow.derive-spend-input-signer-missing-authenticated-source.js";
import { planSpendInputSignerInputOpening } from "./field-plans.js";
import { SpendInputSignerStep02DatumSchema } from "./schemas.js";
import { SPEND_INPUT_SIGNER_MISSING_CURSOR_SPEC } from "./workflow-spec.js";

export const loadManifestBoundSpendInputSignerMissingConfig = async (
  input: LoadManifestBoundSpendInputSignerMissingConfig,
): Promise<ManifestBoundSpendInputSignerMissingConfig> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: input.manifest,
    blueprintJson: input.blueprintJson,
    deploymentInfo: input.deploymentInfo,
    category: "spendInputSignerMissing",
    headerHash: input.headerHash,
    proverCredential: input.signer.paymentKeyHash,
    stepDatumSchemas:
      SPEND_INPUT_SIGNER_MISSING_FAMILY_DEFINITION.stepDatumSchemas,
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: input.signer.address,
    proofMaterialsAddress: input.signer.proofMaterialsAddress,
    paymentKeyHash: input.signer.paymentKeyHash,
  });

  return createSpendInputSignerMissingBoundConfig(input, binding);
};

export type ManifestBoundSpendInputSignerMissingWorkflowConfig =
  LoadManifestBoundSpendInputSignerMissingConfig &
    Readonly<{
      l1Source: FraudProofL1Source;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
      /** The classifier-admitted context carrying the authenticated predecessor. */
      replayContext?: CompleteCanonicalReplayContext;
    }>;

/** The bound deployment a workflow's recovery surface is assembled over. */
export type ManifestBoundSpendInputSignerMissingInstallation = Readonly<{
  deployment: FamilyDeploymentContext<
    "spendInputSignerMissing",
    "computationThreadMint" | "fraudProofMint" | "phasMembershipWithdraw",
    true,
    4
  >;
  workflowVersion: typeof SPEND_INPUT_SIGNER_MISSING_WORKFLOW;
  config: ManifestBoundSpendInputSignerMissingConfig;
  binding: SpendInputSignerMissingDeploymentBinding;
  l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
  replayContext?: CompleteCanonicalReplayContext;
}>;

export const SPEND_INPUT_SIGNER_MISSING_FAMILY_DEFINITION = defineFamily<
  "spendInputSignerMissing",
  "computationThreadMint" | "fraudProofMint" | "phasMembershipWithdraw",
  true,
  4,
  SpendInputSignerMissingAssemblyRuntime
>({
  category: "spendInputSignerMissing",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    SpendInputSignerStep02DatumSchema,
    SignerWitnessScanDatumSchema,
    SignerWitnessTerminalDatumSchema,
  ],
  witnessRoles: CORE_WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => SPEND_INPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: SPEND_INPUT_SIGNER_MISSING_CURSOR_SPEC,
    stepContractNames: cursorStepContractNames(
      SPEND_INPUT_SIGNER_MISSING_CURSOR_SPEC,
    ),
    transactionPort: (context) => {
      const category = "spendInputSignerMissing";
      const { config, material } = context.runtime;
      const { binding, l1, stateQueueMutationLeaseCoordinator } = context;
      return {
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
            step_03: "submitScan",
            step_04: "submitTerminal",
          } as const;
          const familyAction = actions[input.stage as keyof typeof actions];
          if (familyAction === undefined)
            throw new Error(
              `${category} cursor action is outside its exact topology`,
            );
          const submission =
            createManifestBoundSpendInputSignerMissingSubmission({
              ownOutputs: workflowFieldCarriageOwnOutputs({
                category,
                entries,
              }),
              config,
              observe: async () =>
                spendInputSignerStageFromL1(
                  (
                    await l1.observe({
                      headerHash: binding.definition.headerHash,
                    })
                  ).stage,
                ),
              resolveStage: createSpendInputSignerMissingRawL1StageResolver({
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
    },
  },
  fieldCarriage: [
    {
      requirementForAction: (context, { action, artifact }) => {
        const category = "spendInputSignerMissing";
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
              ? planSpendInputSignerInputOpening({
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
                  label: "spend-input-signer-missing address witnesses",
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
