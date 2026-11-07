import { resolveInputExistenceFieldCarriage } from "../input-existence/field-carriage.js";
import { submitInputExistenceStep } from "../input-existence/submit.js";
import { resolveNonExistentInputDeploymentContracts } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import {
  type ManifestBoundLinearFamilyWorkflow,
  type ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.js";
import { workflowFieldCarriageOwnOutputs } from "./field-carriage-prerequisite.own-carriage-claims.js";
import {
  admitLedgerAbsenceArtifact,
  inputExistenceProofForStage,
  prepareLedgerAbsenceArtifact,
} from "./ledger-absence-artifact.js";
import {
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import {
  actionInput,
  type BoundConfig,
  captureRemoval,
  resolveChunks,
  resolveField,
  stringField,
  WITNESS_ROLES,
} from "./non-existent-input.capture-removal.js";
import { captureLocallyEvaluatedTransaction } from "./transaction-boundary.js";

export const createTransactionPort = (
  config: BoundConfig,
): LinearFamilyTransactionPort<"nonExistentInput"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "nonExistentInput",
  prepare: async ({ evidence, replayContext, classification }) =>
    await prepareLedgerAbsenceArtifact({
      category: "nonExistentInput",
      evidence,
      replayContext,
      classification,
      owner: config.signer.paymentKeyHash,
    }),
  capture: async ({ action, artifact, entries }) => {
    const ownOutputs = workflowFieldCarriageOwnOutputs({
      category: "nonExistentInput",
      entries,
    });
    const admitted = admitLedgerAbsenceArtifact(
      artifact,
      config.signer.paymentKeyHash,
    );
    const prepared = admitted.prepared;
    if (
      admitted.artifact.category !== "nonExistentInput" ||
      prepared.source.headerHash !== config.binding.definition.headerHash
    ) {
      throw new Error("non-existent-input artifact changed workflow identity");
    }
    const input = actionInput(action);
    if (input.stage === "remove") return await captureRemoval(config, input);
    if (input.stage === "init")
      return {
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitInit({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              fraudCategory: "nonExistentInput",
              fraudulentBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: prepared.source.headerHash,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      };
    const stepIndex = (
      ["step_01", "step_02", "step_03", "step_04"] as const
    ).findIndex((stage) => stage === input.stage);
    if (stepIndex < 0 || stepIndex > 3)
      throw new Error("non-existent-input: unknown physical step");
    const resolved = await resolveNonExistentInputDeploymentContracts({
      blueprint: config.binding.blueprint,
      deploymentInfo: config.binding.deploymentInfo,
      network: config.binding.network,
      requireFraudProofSpend: true,
    });
    const fieldCarriageUtxos =
      stepIndex === 1
        ? await resolveInputExistenceFieldCarriage({
            prepared,
            resolveField: (planned) =>
              resolveField(config, planned, ownOutputs),
          })
        : [];
    const proof = inputExistenceProofForStage(prepared, input.stage as string);
    const chunks =
      proof === null
        ? []
        : await resolveChunks({ action, config, proofCbor: proof });
    return {
      transaction: await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          await submitInputExistenceStep({
            lucid: config.lucid,
            blueprint: config.binding.blueprint,
            network: config.binding.network,
            contracts: {
              hubOraclePolicyId: resolved.hubOraclePolicyId,
              steps: resolved.contracts.nonExistentInput.steps,
              computationThread: resolved.contracts.computationThread,
              fraudProof: resolved.contracts.fraudProof,
            },
            categoryId: resolved.nonExistentInputCategory.categoryId,
            signer: config.signer,
            threadOutRef: stringField(input, "threadOutRef"),
            prepared,
            stepIndex: stepIndex as 0 | 1 | 2 | 3,
            ...(stepIndex === 0 && prepared.source.kind === "accepted"
              ? {
                  stateQueueBlockOutRef: stringField(
                    input,
                    "stateQueueBlockOutRef",
                  ),
                }
              : {}),
            referenceScripts: {
              steps: config.references.steps,
              computationThreadMint:
                config.references.witnesses.computationThreadMint,
              fraudProofMint: config.references.witnesses.fraudProofMint,
            },
            witnessReferenceScripts: config.references.witnesses,
            fieldCarriageUtxos,
            certificatePolicyId: config.certificate.policyId,
            publishedProofChunks: chunks,
            preSubmitBoundary,
            awaitConfirmation: false,
          });
        },
      ),
    };
  },
});

export type ManifestBoundNonExistentInputWorkflowConfig =
  ManifestBoundLinearFamilyWorkflowConfig<
    "nonExistentInput",
    (typeof WITNESS_ROLES)[number],
    true
  >;
export type ManifestBoundNonExistentInputWorkflow =
  ManifestBoundLinearFamilyWorkflow<"nonExistentInput", true>;
