import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import {
  type FraudProofCatalogueCategoryName,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";

import { planFaultProofFieldOpening } from "../field-opening.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
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
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import { bindManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
} from "../workflow/transaction-boundary.js";
import { createManifestBoundOutputReferenceScriptDecodingSubmission } from "./authenticated-workflow.create-manifest-bound-output-reference-script-decoding-submission.js";
import {
  createOutputReferenceScriptDecodingBoundConfig,
  type LoadManifestBoundOutputReferenceScriptDecodingConfig,
  type ManifestBoundOutputReferenceScriptDecodingConfig,
  OUTPUT_REFERENCE_SCRIPT_DECODING_MANIFEST_CONTRACTS,
  OUTPUT_REFERENCE_SCRIPT_DECODING_WORKFLOW,
  type OutputReferenceScriptDecodingDeploymentBinding,
  type OutputReferenceScriptDecodingRuntimeLoader,
} from "./authenticated-workflow.create-output-reference-script-decoding-bound-config.js";
import { createOutputReferenceScriptDecodingRawL1StageResolver } from "./authenticated-workflow.create-output-reference-script-decoding-raw-l1-stage-resolver.js";
import {
  type OutputReferenceScriptDecodingAssemblyRuntime,
  outputReferenceScriptDecodingStageFromL1,
} from "./authenticated-workflow.derive-output-reference-script-decoding-authenticated-source.js";
import { createOutputReferenceScriptDecodingCentralJournalAdapter } from "./central-journal.js";
import { type OutputReferenceScriptDecodingEvidence } from "./output-reference-script-decoding.js";
import {
  OutputReferenceStep02DatumSchema,
  OutputReferenceStep03DatumSchema,
  OutputReferenceStep04DatumSchema,
  OutputReferenceStep05DatumSchema,
  OutputReferenceStep06DatumSchema,
} from "./schemas.js";
import {
  nextOutputReferenceScriptDecodingAction,
  outputReferenceScriptDecodingEvidenceIdentity,
  type OutputReferenceScriptDecodingJournal,
} from "./workflow.js";
import { OUTPUT_REFERENCE_SCRIPT_DECODING_CURSOR_SPEC } from "./workflow-spec.js";

export const loadManifestBoundOutputReferenceScriptDecodingConfig = async (
  input: LoadManifestBoundOutputReferenceScriptDecodingConfig,
): Promise<ManifestBoundOutputReferenceScriptDecodingConfig> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: input.manifest,
    blueprintJson: input.blueprintJson,
    deploymentInfo: input.deploymentInfo,
    category: "outputReferenceScriptDecoding",
    headerHash: input.headerHash,
    proverCredential: input.signer.paymentKeyHash,
    stepDatumSchemas:
      OUTPUT_REFERENCE_SCRIPT_DECODING_FAMILY_DEFINITION.stepDatumSchemas,
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: input.signer.address,
    paymentKeyHash: input.signer.paymentKeyHash,
  });

  return createOutputReferenceScriptDecodingBoundConfig(input, binding);
};

export const createManifestBoundOutputReferenceScriptDecodingRuntime = ({
  config,
  journal,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundOutputReferenceScriptDecodingConfig;
  readonly journal: OutputReferenceScriptDecodingJournal;
  readonly observe: OutputReferenceScriptDecodingRuntimeLoader["observe"];
  readonly resolveStage: OutputReferenceScriptDecodingRuntimeLoader["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createOutputReferenceScriptDecodingCentralJournalAdapter
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => {
  const submission = createManifestBoundOutputReferenceScriptDecodingSubmission(
    {
      config,
      observe: async (identity) => {
        const observed = await observe(identity);
        await centralJournal?.reconcile(observed);
        return observed;
      },
      resolveStage,
      centralJournal,
      stateQueueMutationLeaseCoordinator,
    },
  );
  return Object.freeze({
    runtimeVersion: OUTPUT_REFERENCE_SCRIPT_DECODING_WORKFLOW,
    config,
    runOrResume: async (evidence: OutputReferenceScriptDecodingEvidence) => {
      const identity = outputReferenceScriptDecodingEvidenceIdentity(evidence);
      for (;;) {
        const stage = await submission.observe(identity);
        const action = nextOutputReferenceScriptDecodingAction(stage);
        if (action === "done") return stage;
        if (action === "cancel")
          throw new Error(
            "outputReferenceScriptDecoding automatic runner cannot synthesize cancellation",
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

export type ManifestBoundOutputReferenceScriptDecodingWorkflowConfig =
  LoadManifestBoundOutputReferenceScriptDecodingConfig &
    Readonly<{
      l1Source: FraudProofL1Source;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    }>;

export type ManifestBoundOutputReferenceScriptDecodingWorkflow = Readonly<{
  deployment: FamilyDeploymentContext<
    "outputReferenceScriptDecoding",
    "computationThreadMint" | "fraudProofMint" | "phasMembershipWithdraw",
    true,
    6
  >;
  workflowVersion: typeof OUTPUT_REFERENCE_SCRIPT_DECODING_WORKFLOW;
  config: ManifestBoundOutputReferenceScriptDecodingConfig;
  binding: OutputReferenceScriptDecodingDeploymentBinding;
  l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
}>;

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundOutputReferenceScriptDecodingWorkflow = async (
  input: ManifestBoundOutputReferenceScriptDecodingWorkflowConfig,
): Promise<ManifestBoundOutputReferenceScriptDecodingWorkflow> => {
  const deployment = await bindManifestBoundFamilyWorkflow(
    OUTPUT_REFERENCE_SCRIPT_DECODING_FAMILY_DEFINITION,
    {
      ...input,
      referenceScripts: {
        steps: [
          input.referenceScripts.step01,
          input.referenceScripts.step02,
          input.referenceScripts.step03,
          input.referenceScripts.step04,
          input.referenceScripts.step05,
          input.referenceScripts.step06,
        ],
        witnesses: input.referenceScripts.witnesses,
        fieldPreimageCertificateMint:
          input.referenceScripts.fieldPreimageCertificateMint,
      },
    },
  );
  const config = createOutputReferenceScriptDecodingBoundConfig(
    input,
    deployment.binding,
  );
  const l1 = deployment.l1;
  return Object.freeze({
    deployment,
    workflowVersion: OUTPUT_REFERENCE_SCRIPT_DECODING_WORKFLOW,
    config,
    binding: config.binding,
    l1,
    stateQueueMutationLeaseCoordinator:
      input.stateQueueMutationLeaseCoordinator,
    decisionDigest: input.decisionDigest,
  });
};

export const OUTPUT_REFERENCE_SCRIPT_DECODING_FAMILY_DEFINITION = defineFamily<
  "outputReferenceScriptDecoding",
  "computationThreadMint" | "fraudProofMint" | "phasMembershipWithdraw",
  true,
  6,
  OutputReferenceScriptDecodingAssemblyRuntime
>({
  category: "outputReferenceScriptDecoding",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    OutputReferenceStep02DatumSchema,
    OutputReferenceStep03DatumSchema,
    OutputReferenceStep04DatumSchema,
    OutputReferenceStep05DatumSchema,
    OutputReferenceStep06DatumSchema,
  ],
  witnessRoles: [
    "computationThreadMint",
    "fraudProofMint",
    "phasMembershipWithdraw",
  ],
  fieldPreimageCertificate: true,
  replayer: () => OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: OUTPUT_REFERENCE_SCRIPT_DECODING_CURSOR_SPEC,
    stepContractNames: [
      OUTPUT_REFERENCE_SCRIPT_DECODING_MANIFEST_CONTRACTS.step01,
      OUTPUT_REFERENCE_SCRIPT_DECODING_MANIFEST_CONTRACTS.step02,
      OUTPUT_REFERENCE_SCRIPT_DECODING_MANIFEST_CONTRACTS.step03,
      OUTPUT_REFERENCE_SCRIPT_DECODING_MANIFEST_CONTRACTS.step04,
      OUTPUT_REFERENCE_SCRIPT_DECODING_MANIFEST_CONTRACTS.step05,
      OUTPUT_REFERENCE_SCRIPT_DECODING_MANIFEST_CONTRACTS.step06,
    ],
    transactionPort: (context) => {
      const category = "outputReferenceScriptDecoding";
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
            step_03: "submitOutputScan",
            step_04: "submitReferenceBind",
            step_05: "submitStructuralScan",
            step_06: "submitStep06",
          } as const;
          const familyAction = actions[input.stage as keyof typeof actions];
          if (familyAction === undefined)
            throw new Error(
              `${category} cursor action is outside its exact topology`,
            );
          const transaction = await captureLocallyEvaluatedTransaction(
            async (preSubmitBoundary) => {
              const submission =
                createManifestBoundOutputReferenceScriptDecodingSubmission({
                  config,
                  preSubmitBoundary,
                  observe: async () =>
                    outputReferenceScriptDecodingStageFromL1(
                      (
                        await l1.observe({
                          headerHash: binding.definition.headerHash,
                        })
                      ).stage,
                    ),
                  resolveStage:
                    createOutputReferenceScriptDecodingRawL1StageResolver({
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
        const category = "outputReferenceScriptDecoding";
        const { config, material } = context.runtime;
        const { binding } = context;
        if (
          action.input.stage !== "step_02" &&
          action.input.stage !== "step_04"
        )
          return null;
        const { evidence, source } = material.require(artifact);
        const certificate = binding.fieldPreimageCertificate;
        if (certificate === null)
          throw new Error(`${category} omitted field certificate authority`);
        return {
          planned: planFaultProofFieldOpening({
            anchorSourceKind: evidence.subject.source_kind === 1n ? 1n : 0n,
            fieldIndex: 2,
            anchorTxId: evidence.subject.transaction_id,
            nativeTxCompactCbor: source.nativeTxCompactCbor,
            itemCbors: decodeMidgardFieldPreimage(
              Buffer.from(evidence.outputFieldPreimageHex, "hex"),
            ),
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
      },
    },
  ],
});
