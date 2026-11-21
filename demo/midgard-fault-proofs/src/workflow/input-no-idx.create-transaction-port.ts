import {
  FraudProofComputationThreadStepDatum,
  InputNoIdxStep02Datum,
  InputNoIdxStep03Datum,
  InputNoIdxStep04Datum,
} from "@al-ft/midgard-sdk/fraud-proof";

import { requireFaultProofFieldGrammarDeployment } from "../field-grammar-certificate.resolve-fault-proof-field-grammar-deployment.js";
import { buildRemoveFraudulentBlock } from "../remove-fraudulent-block.build-remove-fraudulent-block.js";
import { buildInit } from "../submit-init.submit-init.js";
import { buildInputNoIdxStep01 } from "../submit-input-no-idx-step-01.js";
import { buildInputNoIdxStep02 } from "../submit-input-no-idx-step-02.submit-input-no-idx-step02.js";
import { buildInputNoIdxStep03 } from "../submit-input-no-idx-step-03.js";
import { buildInputNoIdxStep04 } from "../submit-input-no-idx-step-04.submit-input-no-idx-step04.js";
import { INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.resolved-output-non-canonical-complete-canonical-replay.js";
import type { ManifestBoundLinearFamilyWorkflowConfig } from "./family-definition.family-adapter-arm.js";
import {
  defineLinearFamily,
  type ManifestBoundLinearFamilyWorkflow,
} from "./family-definition.family-definition.js";
import { type FieldCarriageRequirement } from "./field-carriage-prerequisite.js";
import { workflowFieldCarriageOwnOutputs } from "./field-carriage-prerequisite.own-carriage-claims.js";
import {
  actionInput,
  admitInputNoIdxArtifact,
  type AssemblyContext,
  type BoundConfig,
  prepareInputNoIdxArtifact,
  resolveField,
  stringField,
  WITNESS_ROLES,
} from "./input-no-idx.admit-input-no-idx-artifact.js";
import { record } from "./input-no-idx.parse-inclusion.js";
import {
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import { assembleManifestBoundFamilyWorkflow } from "./manifest-bound-family-assembly.assemble-bound-manifest-bound-family-workflow.js";
import { runOrResumeManifestBoundFamilyWorkflow } from "./manifest-bound-family-assembly.run-or-resume-manifest-bound-family-workflow.js";
import { resolveDirectFirstProofChunks } from "./proof-chunk-prerequisite.route-action-identity.js";
import { admitRemovalTransaction } from "./removal-admission.js";
import { journaledStepTransaction } from "./step-transaction.js";
import {
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

const captureRemoval = async (
  config: BoundConfig,
  input: Readonly<Record<string, unknown>>,
) => {
  const nextRemovalOutRef = stringField(input, "nextRemovalOutRef");
  const fraudProofOutRef = stringField(input, "fraudProofOutRef");
  return await admitRemovalTransaction(
    await buildRemoveFraudulentBlock({
      fraudProofOutRef,
      lucid: config.lucid,
      blueprint: config.blueprint,
      deploymentInfo: config.deploymentInfo,
      network: config.network,
      signer: config.signer,
      fraudCategory: "nonExistentInputNoIndex",
      fraudulentHeaderHash: config.headerHash,
      requireReferenceScripts: true,
      stateQueueMutationLeaseCoordinator:
        config.stateQueueMutationLeaseCoordinator,
      fraudProverRewardLovelace: config.fraudProverRewardLovelace,
    }),
    async (built) => {
      if (
        !workflowTransactionInputOutRefs(built.signed).includes(
          nextRemovalOutRef,
        ) ||
        !workflowTransactionReferenceInputOutRefs(built.signed).includes(
          fraudProofOutRef,
        )
      ) {
        throw new Error(
          "input-no-idx removal changed its authenticated inputs",
        );
      }
    },
  );
};

const createTransactionPort = (
  config: BoundConfig,
): LinearFamilyTransactionPort<"nonExistentInputNoIndex"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "nonExistentInputNoIndex",
  prepare: async ({ evidence, classification }) =>
    await prepareInputNoIdxArtifact({ evidence, classification }),
  capture: async ({ action, artifact, entries }) => {
    const ownOutputs = workflowFieldCarriageOwnOutputs({
      category: "nonExistentInputNoIndex",
      entries,
    });
    const admitted = admitInputNoIdxArtifact(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error("input-no-idx artifact changed manifest-bound header");
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: (
          await journaledStepTransaction(
            await buildInit({
              lucid: config.lucid,
              blueprint: config.blueprint,
              deploymentInfo: config.deploymentInfo,
              network: config.network,
              signer: config.signer,
              fraudCategory: "nonExistentInputNoIndex",
              fraudulentBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: config.headerHash,
              witnessReferenceScripts: config.referenceScripts.witnesses,
            }),
            "input-no-idx buildInit",
          )
        ).transaction,
      });
    }
    if (input.stage === "step_01" || input.stage === "step_03") {
      const stepIndex = input.stage === "step_01" ? 0 : 2;
      const inclusion =
        input.stage === "step_01"
          ? admitted.badInclusion
          : admitted.producingInclusion;
      const proofCbor =
        input.stage === "step_01"
          ? admitted.artifact.badTx.txMembershipProofCbor
          : admitted.artifact.producingTx.txMembershipProofCbor;
      const chunks = await resolveDirectFirstProofChunks({
        action,
        lucid: config.lucid,
        address: config.signer.proofMaterialsAddress,
        proofCbor,
      });
      return Object.freeze({
        transaction: (
          await journaledStepTransaction(
            await (async () => {
              const common = {
                lucid: config.lucid,
                blueprint: config.blueprint,
                deploymentInfo: config.deploymentInfo,
                network: config.network,
                signer: config.signer,
                threadOutRef: stringField(input, "threadOutRef"),
                stateQueueBlockOutRef: stringField(
                  input,
                  "stateQueueBlockOutRef",
                ),
                txInclusion: inclusion,
                publishedProofChunks: chunks,
                referenceScriptUtxo: config.referenceScripts.steps[stepIndex],
                witnessReferenceScripts: config.referenceScripts.witnesses,
              } as const;
              if (input.stage === "step_01") {
                return await buildInputNoIdxStep01(common);
              } else {
                return await buildInputNoIdxStep03(common);
              }
            })(),
            `input-no-idx ${input.stage}`,
          )
        ).transaction,
      });
    }
    if (input.stage === "step_02" || input.stage === "step_04") {
      const stepIndex = input.stage === "step_02" ? 1 : 3;
      const plan =
        input.stage === "step_02"
          ? admitted.inputFieldPlan
          : admitted.outputFieldPlan;
      const carriage = await resolveField(config, plan, ownOutputs);
      return Object.freeze({
        transaction: (
          await journaledStepTransaction(
            await (async () => {
              const common = {
                lucid: config.lucid,
                blueprint: config.blueprint,
                deploymentInfo: config.deploymentInfo,
                network: config.network,
                signer: config.signer,
                threadOutRef: stringField(input, "threadOutRef"),
                publishedCarriageUtxos: carriage.publications,
                ...(carriage.certificate === undefined
                  ? {}
                  : { certificateUtxo: carriage.certificate }),
                referenceScriptUtxo: config.referenceScripts.steps[stepIndex],
              } as const;
              if (input.stage === "step_02") {
                return await buildInputNoIdxStep02({
                  ownOutputs,
                  ...common,
                  inputsPreimage: admitted.inputs,
                  nativeTxCompactCbor:
                    admitted.artifact.badTx.nativeTxCompactCbor,
                });
              } else {
                return await buildInputNoIdxStep04({
                  ownOutputs,
                  ...common,
                  outputsPreimage: admitted.outputs,
                  nativeTxCompactCbor:
                    admitted.artifact.producingTx.nativeTxCompactCbor,
                  witnessReferenceScripts: config.referenceScripts.witnesses,
                  fieldGrammar: config.fieldGrammar,
                });
              }
            })(),
            `input-no-idx ${input.stage}`,
          )
        ).transaction,
      });
    }
    if (input.stage === "remove") {
      return await captureRemoval(config, input);
    }
    throw new Error(
      `input-no-idx workflow action has unsupported stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundInputNoIdxWorkflowConfig =
  ManifestBoundLinearFamilyWorkflowConfig<
    "nonExistentInputNoIndex",
    (typeof WITNESS_ROLES)[number],
    true
  >;

export type ManifestBoundInputNoIdxWorkflow = ManifestBoundLinearFamilyWorkflow<
  "nonExistentInputNoIndex",
  true
>;

const fieldPreimageCertificate = (context: AssemblyContext) => ({
  policyId: context.certificate.policyId,
  mintingScript: context.certificate.mintingScript,
  referenceScriptUtxo: context.references.fieldPreimageCertificateMint,
});

export const INPUT_NO_IDX_FAMILY_DEFINITION = defineLinearFamily({
  category: "nonExistentInputNoIndex",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    InputNoIdxStep02Datum,
    InputNoIdxStep03Datum,
    InputNoIdxStep04Datum,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  fieldGrammarCertificate: true,
  replayer: () => INPUT_NO_IDX_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "linear",
    transactionPort: (context) =>
      createTransactionPort({
        lucid: context.lucid,
        blueprint: context.binding.blueprint,
        deploymentInfo: context.binding.deploymentInfo,
        network: context.binding.network,
        signer: context.signer,
        headerHash: context.binding.definition.headerHash,
        referenceScripts: context.references,
        certificate: context.certificate,
        fieldGrammar: requireFaultProofFieldGrammarDeployment(
          context.fieldGrammar,
          "nonExistentInputNoIndex",
        ),
        stateQueueMutationLeaseCoordinator:
          context.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          context.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      }),
  },
  // Step-02 opens the bad transaction's spend inputs; step-04 opens the
  // producing transaction's outputs.
  fieldCarriage: [
    {
      requirementForAction: (context, { action, artifact }) => {
        const input = record(action.input, "input-no-idx field prerequisite");
        const admitted = admitInputNoIdxArtifact(
          artifact,
          context.signer.paymentKeyHash,
        );
        const planned =
          input.stage === "step_02"
            ? admitted.inputFieldPlan
            : input.stage === "step_04"
              ? admitted.outputFieldPlan
              : null;
        if (planned === null) return null;
        return {
          planned,
          compactCbor:
            input.stage === "step_02"
              ? admitted.artifact.badTx.nativeTxCompactCbor
              : admitted.artifact.producingTx.nativeTxCompactCbor,
          certificate: fieldPreimageCertificate(context),
        } satisfies FieldCarriageRequirement;
      },
    },
  ],
  proofChunk: (context, { action, artifact }) => {
    const input = record(action.input, "input-no-idx proof prerequisite");
    const admitted = admitInputNoIdxArtifact(
      artifact,
      context.signer.paymentKeyHash,
    );
    return input.stage === "step_01"
      ? admitted.artifact.badTx.txMembershipProofCbor
      : input.stage === "step_03"
        ? admitted.artifact.producingTx.txMembershipProofCbor
        : null;
  },
});

export const createManifestBoundInputNoIdxWorkflow = (
  config: ManifestBoundInputNoIdxWorkflowConfig,
): Promise<ManifestBoundInputNoIdxWorkflow> =>
  assembleManifestBoundFamilyWorkflow(INPUT_NO_IDX_FAMILY_DEFINITION, config);

export const runOrResumeManifestBoundInputNoIdxWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;
