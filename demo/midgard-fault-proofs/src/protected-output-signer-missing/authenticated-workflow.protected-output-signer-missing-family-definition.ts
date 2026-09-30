import {
  type FraudProofCatalogueCategoryName,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";

import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { PROTECTED_OUTPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { CURSOR_FAMILY_TRANSACTION_PORT } from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
} from "../workflow/deployment-manifest-binding.js";
import {
  defineFamily,
  type FamilyDeploymentContext,
} from "../workflow/family-definition.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { bindManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
} from "../workflow/transaction-boundary.js";
import { createManifestBoundProtectedOutputSignerMissingSubmission } from "./authenticated-workflow.create-manifest-bound-protected-output-signer-missing-submission.js";
import {
  createProtectedOutputSignerMissingBoundConfig,
  type LoadManifestBoundProtectedOutputSignerMissingConfig,
  type ManifestBoundProtectedOutputSignerMissingConfig,
  PROTECTED_OUTPUT_SIGNER_MISSING_MANIFEST_CONTRACTS,
  PROTECTED_OUTPUT_SIGNER_MISSING_WORKFLOW,
  type ProtectedOutputSignerMissingDeploymentBinding,
  type ProtectedOutputSignerMissingRuntimeLoader,
} from "./authenticated-workflow.create-protected-output-signer-missing-bound-config.js";
import { createProtectedOutputSignerMissingRawL1StageResolver } from "./authenticated-workflow.create-protected-output-signer-missing-raw-l1-stage-resolver.js";
import {
  type ProtectedOutputSignerMissingAssemblyRuntime,
  protectedOutputSignerStageFromL1,
} from "./authenticated-workflow.derive-protected-output-signer-missing-authenticated-source.js";
import { createProtectedOutputSignerMissingCentralJournalAdapter } from "./central-journal.js";
import {
  planProtectedOutputSignerOutputOpening,
  planProtectedOutputSignerWitnessOpening,
} from "./field-plans.js";
import { type ProtectedOutputSignerMissingEvidence } from "./protected-output-signer-missing.js";
import {
  ProtectedOutputSignerStep02DatumSchema,
  ProtectedOutputSignerStep03DatumSchema,
  ProtectedOutputSignerStep04DatumSchema,
  ProtectedOutputSignerStep05DatumSchema,
} from "./schemas.js";
import {
  nextProtectedOutputSignerAction,
  protectedOutputSignerEvidenceIdentity,
  type ProtectedOutputSignerJournal,
} from "./workflow.js";
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
    paymentKeyHash: input.signer.paymentKeyHash,
  });

  return createProtectedOutputSignerMissingBoundConfig(input, binding);
};

export const createManifestBoundProtectedOutputSignerMissingRuntime = ({
  config,
  journal,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundProtectedOutputSignerMissingConfig;
  readonly journal: ProtectedOutputSignerJournal;
  readonly observe: ProtectedOutputSignerMissingRuntimeLoader["observe"];
  readonly resolveStage: ProtectedOutputSignerMissingRuntimeLoader["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createProtectedOutputSignerMissingCentralJournalAdapter
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => {
  const submission = createManifestBoundProtectedOutputSignerMissingSubmission({
    config,
    observe: async (identity) => {
      const observed = await observe(identity);
      await centralJournal?.reconcile(observed);
      return observed;
    },
    resolveStage,
    centralJournal,
    stateQueueMutationLeaseCoordinator,
  });
  return Object.freeze({
    runtimeVersion: PROTECTED_OUTPUT_SIGNER_MISSING_WORKFLOW,
    config,
    runOrResume: async (evidence: ProtectedOutputSignerMissingEvidence) => {
      const identity = protectedOutputSignerEvidenceIdentity(evidence);
      for (;;) {
        const stage = await submission.observe(identity);
        const action = nextProtectedOutputSignerAction(stage);
        if (action === "done") return stage;
        if (action === "cancel")
          throw new Error(
            "protectedOutputSignerMissing automatic runner cannot synthesize cancellation",
          );
        const result = await submission.submit(action, evidence);
        await journal.append({
          sequence: (await journal.load(identity)).length,
          identity,
          sourceStage: stage,
          targetStage: result.stage,
          action,
          phase: "submitted",
          txHash: result.txHash,
        });
      }
    },
  });
};

export type ManifestBoundProtectedOutputSignerMissingWorkflowConfig =
  LoadManifestBoundProtectedOutputSignerMissingConfig &
    Readonly<{
      source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    }>;

export type ManifestBoundProtectedOutputSignerMissingWorkflow = Readonly<{
  deployment: FamilyDeploymentContext<
    "protectedOutputSignerMissing",
    "computationThreadMint" | "fraudProofMint" | "phasMembershipWithdraw",
    true,
    5
  >;
  workflowVersion: typeof PROTECTED_OUTPUT_SIGNER_MISSING_WORKFLOW;
  config: ManifestBoundProtectedOutputSignerMissingConfig;
  binding: ProtectedOutputSignerMissingDeploymentBinding;
  l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
}>;

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundProtectedOutputSignerMissingWorkflow = async (
  input: ManifestBoundProtectedOutputSignerMissingWorkflowConfig,
): Promise<ManifestBoundProtectedOutputSignerMissingWorkflow> => {
  const deployment = await bindManifestBoundFamilyWorkflow(
    PROTECTED_OUTPUT_SIGNER_MISSING_FAMILY_DEFINITION,
    {
      ...input,
      referenceScripts: {
        steps: [
          input.referenceScripts.step01,
          input.referenceScripts.step02,
          input.referenceScripts.step03,
          input.referenceScripts.step04,
          input.referenceScripts.step05,
        ],
        witnesses: input.referenceScripts.witnesses,
        fieldPreimageCertificateMint:
          input.referenceScripts.fieldPreimageCertificateMint,
      },
    },
  );
  const config = createProtectedOutputSignerMissingBoundConfig(
    input,
    deployment.binding,
  );
  const l1 = deployment.l1;
  return Object.freeze({
    deployment,
    workflowVersion: PROTECTED_OUTPUT_SIGNER_MISSING_WORKFLOW,
    config,
    binding: config.binding,
    l1,
    stateQueueMutationLeaseCoordinator:
      input.stateQueueMutationLeaseCoordinator,
    decisionDigest: input.decisionDigest,
  });
};

export const PROTECTED_OUTPUT_SIGNER_MISSING_FAMILY_DEFINITION = defineFamily<
  "protectedOutputSignerMissing",
  "computationThreadMint" | "fraudProofMint" | "phasMembershipWithdraw",
  true,
  5,
  ProtectedOutputSignerMissingAssemblyRuntime
>({
  category: "protectedOutputSignerMissing",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    ProtectedOutputSignerStep02DatumSchema,
    ProtectedOutputSignerStep03DatumSchema,
    ProtectedOutputSignerStep04DatumSchema,
    ProtectedOutputSignerStep05DatumSchema,
  ],
  witnessRoles: [
    "computationThreadMint",
    "fraudProofMint",
    "phasMembershipWithdraw",
  ],
  fieldPreimageCertificate: true,
  replayer: () => PROTECTED_OUTPUT_SIGNER_MISSING_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: PROTECTED_OUTPUT_SIGNER_MISSING_CURSOR_SPEC,
    stepContractNames: [
      PROTECTED_OUTPUT_SIGNER_MISSING_MANIFEST_CONTRACTS.step01,
      PROTECTED_OUTPUT_SIGNER_MISSING_MANIFEST_CONTRACTS.step02,
      PROTECTED_OUTPUT_SIGNER_MISSING_MANIFEST_CONTRACTS.step03,
      PROTECTED_OUTPUT_SIGNER_MISSING_MANIFEST_CONTRACTS.step04,
      PROTECTED_OUTPUT_SIGNER_MISSING_MANIFEST_CONTRACTS.step05,
    ],
    transactionPort: (context) => {
      const category = "protectedOutputSignerMissing";
      const { config, material } = context.runtime;
      const { binding, l1, stateQueueMutationLeaseCoordinator } = context;
      return {
        portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
        category,
        prepare: material.prepare,
        validatePreparedArtifact: material.validatePreparedArtifact,
        capture: async ({ action, artifact }) => {
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
            step_04: "submitScan",
            step_05: "submitStep05",
          } as const;
          const familyAction = actions[input.stage as keyof typeof actions];
          if (familyAction === undefined)
            throw new Error(
              `${category} cursor action is outside its exact topology`,
            );
          const transaction = await captureLocallyEvaluatedTransaction(
            async (preSubmitBoundary) => {
              const submission =
                createManifestBoundProtectedOutputSignerMissingSubmission({
                  config,
                  preSubmitBoundary,
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
              await submission.submit(familyAction, admitted.evidence);
            },
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
          action.input.stage !== "step_03" &&
          action.input.stage !== "step_04"
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
              : planProtectedOutputSignerWitnessOpening({
                  evidence,
                  nativeTxCompactCbor: source.nativeTxCompactCbor,
                  witnessSetCompactCbor: source.witnessSetCompactCbor,
                  owner: config.signer.paymentKeyHash,
                  publish: true,
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
