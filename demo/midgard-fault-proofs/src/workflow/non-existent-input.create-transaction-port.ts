import {
  admitNonExistentInputForcedArtifact,
  NON_EXISTENT_INPUT_FORCED_ARTIFACT,
  nonExistentInputForcedArtifact,
} from "../non-existent-input/artifact.js";
import {
  nonExistentInputForcedFieldPlan,
  submitNonExistentInputForcedStep,
} from "../non-existent-input/submit.js";
import { neSubmitStep01 } from "../non-existent-input/submit-step-01.js";
import { neSubmitStep02 } from "../non-existent-input/submit-step-02.js";
import { neSubmitStep03 } from "../non-existent-input/submit-step-03.js";
import { neSubmitStep04 } from "../non-existent-input/submit-step-04.js";
import {
  detectNonExistentInputWrongfulRejections,
  NON_EXISTENT_INPUT_WRONGFUL_REJECTION_VIOLATION_ID,
} from "../non-existent-input/wrongful-rejection.js";
import { resolveNonExistentInputDeploymentContracts } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import { completeCanonicalReplayPredecessorEvidence } from "./complete-replay.js";
import {
  type ManifestBoundLinearFamilyWorkflow,
  type ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.js";
import {
  admitLedgerAbsenceArtifact,
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
  prepare: async ({ evidence, replayContext, classification }) => {
    if (
      classification.selected.violationId ===
      NON_EXISTENT_INPUT_WRONGFUL_REJECTION_VIOLATION_ID
    ) {
      const detections = await detectNonExistentInputWrongfulRejections({
        block: evidence,
        predecessor: completeCanonicalReplayPredecessorEvidence({
          evidence,
          context: replayContext,
        }),
      });
      const detected = detections.find(
        (item) => item.detectionId === classification.selected.detectionId,
      );
      if (
        classification.category !== "nonExistentInput" ||
        classification.headerHash !== evidence.headerHash ||
        detected === undefined
      )
        throw new Error("nonExistentInput: forced classification changed");
      return nonExistentInputForcedArtifact(detected.prepared);
    }
    return await prepareLedgerAbsenceArtifact({
      category: "nonExistentInput",
      evidence,
      replayContext,
      classification,
      owner: config.signer.paymentKeyHash,
    });
  },
  capture: async ({ action, artifact }) => {
    if (artifact.schemaVersion === NON_EXISTENT_INPUT_FORCED_ARTIFACT) {
      const prepared = await admitNonExistentInputForcedArtifact(artifact);
      if (prepared.headerHash !== config.binding.definition.headerHash)
        throw new Error("nonExistentInput: forced workflow header changed");
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
                fraudulentHeaderHash: prepared.headerHash,
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
        throw new Error("nonExistentInput: unknown forced stage");
      const { contracts, nonExistentInputCategory } =
        await resolveNonExistentInputDeploymentContracts({
          blueprint: config.binding.blueprint,
          deploymentInfo: config.binding.deploymentInfo,
          network: config.binding.network,
          requireFraudProofSpend: true,
        });
      const carriage =
        stepIndex === 1
          ? await resolveField(
              config,
              nonExistentInputForcedFieldPlan(
                prepared,
                config.signer.paymentKeyHash,
              ),
            )
          : null;
      return {
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitNonExistentInputForcedStep({
              lucid: config.lucid,
              contracts: {
                steps: contracts.nonExistentInput.steps,
                computationThread: contracts.computationThread,
                fraudProof: contracts.fraudProof,
              },
              categoryId: nonExistentInputCategory.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              prepared,
              stepIndex: stepIndex as 0 | 1 | 2 | 3,
              referenceScripts: {
                steps: config.references.steps,
                computationThreadMint:
                  config.references.witnesses.computationThreadMint,
                fraudProofMint: config.references.witnesses.fraudProofMint,
              },
              carriageUtxos:
                carriage === null
                  ? []
                  : [
                      ...carriage.publications,
                      ...(carriage.certificate === undefined
                        ? []
                        : [carriage.certificate]),
                    ],
              certificatePolicyId: config.certificate.policyId,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      };
    }
    const admitted = admitLedgerAbsenceArtifact(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (
      admitted.artifact.category !== "nonExistentInput" ||
      admitted.artifact.headerHash !== config.binding.definition.headerHash
    ) {
      throw new Error("non-existent-input artifact changed workflow identity");
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
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
              fraudulentHeaderHash: admitted.artifact.headerHash,
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_01") {
      const chunks = await resolveChunks({
        action,
        config,
        proofCbor: admitted.artifact.badTx.txMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await neSubmitStep01({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              stateQueueBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: admitted.txInclusion,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.references.steps[0],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_02") {
      const carriage = await resolveField(config, admitted.fieldPlan);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await neSubmitStep02({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              inputsPreimage: admitted.inputPreimage.map((candidate) => ({
                txId: candidate.tx_id,
                index: candidate.output_index,
              })),
              nativeTxCompactCbor: admitted.artifact.badTx.nativeTxCompactCbor,
              badInputIndex: BigInt(admitted.artifact.badInputIndex),
              publishedCarriageUtxos: carriage.publications,
              ...(carriage.certificate === undefined
                ? {}
                : {
                    certificateUtxo: carriage.certificate,
                    certificatePolicyId: config.certificate.policyId,
                  }),
              referenceScriptUtxo: config.references.steps[1],
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_03") {
      const chunks = await resolveChunks({
        action,
        config,
        proofCbor: admitted.artifact.ledgerNonMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await neSubmitStep03({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              ledgerNonMembershipProofCbor:
                admitted.artifact.ledgerNonMembershipProofCbor,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.references.steps[2],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_04") {
      const chunks = await resolveChunks({
        action,
        config,
        proofCbor: admitted.artifact.txsNonMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await neSubmitStep04({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              txsNonMembershipProofCbor:
                admitted.artifact.txsNonMembershipProofCbor,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.references.steps[3],
              witnessReferenceScripts: config.references.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "remove") {
      return await captureRemoval(config, input);
    }
    throw new Error(
      `non-existent-input workflow cannot execute ${String(input.stage)}`,
    );
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
