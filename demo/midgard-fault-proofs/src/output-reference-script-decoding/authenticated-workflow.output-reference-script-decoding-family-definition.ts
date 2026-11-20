import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk/fraud-proof";
import type { UTxO } from "@lucid-evolution/lucid";

import { planFaultProofFieldOpening } from "../field-opening.plan-fault-proof-field-opening.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.resolved-output-non-canonical-complete-canonical-replay.js";
import { CURSOR_FAMILY_TRANSACTION_PORT } from "../workflow/cursor-family-adapter.admit-reference-scripts.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
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
import { SCRIPT_DECODE_REFERENCE_SCRIPTS } from "../workflow/script-decode-prerequisite.deployment.js";
import { journaledStepTransaction } from "../workflow/step-transaction.js";
import { workflowTransactionInputOutRefs } from "../workflow/transaction-boundary.js";
import { createManifestBoundOutputReferenceScriptDecodingSubmission } from "./authenticated-workflow.create-manifest-bound-output-reference-script-decoding-submission.js";
import {
  type LoadManifestBoundOutputReferenceScriptDecodingConfig,
  type ManifestBoundOutputReferenceScriptDecodingConfig,
  OUTPUT_REFERENCE_SCRIPT_DECODING_WORKFLOW,
  type OutputReferenceScriptDecodingDeploymentBinding,
} from "./authenticated-workflow.create-output-reference-script-decoding-bound-config.js";
import { createOutputReferenceScriptDecodingRawL1StageResolver } from "./authenticated-workflow.create-output-reference-script-decoding-raw-l1-stage-resolver.js";
import {
  type OutputReferenceScriptDecodingAssemblyRuntime,
  outputReferenceScriptDecodingStageFromL1,
} from "./authenticated-workflow.derive-output-reference-script-decoding-authenticated-source.js";
import {
  OutputReferenceStep02DatumSchema,
  OutputReferenceStep03DatumSchema,
  OutputReferenceStep04DatumSchema,
  OutputReferenceStep05DatumSchema,
  OutputReferenceStep06DatumSchema,
} from "./schemas.js";
import { OUTPUT_REFERENCE_SCRIPT_DECODING_CURSOR_SPEC } from "./workflow-spec.js";

export type ManifestBoundOutputReferenceScriptDecodingWorkflowConfig =
  LoadManifestBoundOutputReferenceScriptDecodingConfig &
    Readonly<{
      /** The script-decode references, by `SCRIPT_DECODE_REFERENCE_SCRIPTS` role. */
      auxiliaryReferenceScripts: Readonly<Record<string, UTxO>>;
      l1Source: FraudProofL1Source;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    }>;

/** The bound deployment a workflow's recovery surface is assembled over. */
export type ManifestBoundOutputReferenceScriptDecodingInstallation = Readonly<{
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

const SCRIPT_DECODE_RECORDED_BEFORE: ReadonlySet<string> = new Set([
  "init",
  "step_01",
  "step_02",
  "step_03",
  "step_04",
]);

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
  witnessRoles: CORE_WITNESS_ROLES,
  fieldPreimageCertificate: true,
  fieldGrammarCertificate: true,
  auxiliaryReferenceScripts: SCRIPT_DECODE_REFERENCE_SCRIPTS,
  // The record is made before Init, so no thread deadline runs meanwhile;
  // past step 04 the proof no longer reads it.
  scriptDecodeRecord: (context, { action, artifact }) =>
    typeof action.input.stage === "string" &&
    SCRIPT_DECODE_RECORDED_BEFORE.has(action.input.stage)
      ? Buffer.from(
          context.runtime.material.require(artifact).evidence
            .referenceScriptItemHex,
          "hex",
        )
      : null,
  replayer: () => OUTPUT_REFERENCE_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: OUTPUT_REFERENCE_SCRIPT_DECODING_CURSOR_SPEC,
    stepContractNames: cursorStepContractNames(
      OUTPUT_REFERENCE_SCRIPT_DECODING_CURSOR_SPEC,
    ),
    transactionPort: (context) => {
      const category = "outputReferenceScriptDecoding";
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
          const submission =
            createManifestBoundOutputReferenceScriptDecodingSubmission({
              ownOutputs: workflowFieldCarriageOwnOutputs({
                category,
                entries,
              }),
              config,
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
