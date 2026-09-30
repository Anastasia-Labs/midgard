import {
  admitNoReferenceInputForcedArtifact,
  NO_REFERENCE_INPUT_FORCED_ARTIFACT,
  noReferenceInputForcedArtifact,
} from "../no-reference-input/artifact.js";
import {
  noReferenceInputForcedFieldPlan,
  submitNoReferenceInputForcedStep,
} from "../no-reference-input/submit.js";
import {
  detectNoReferenceInputWrongfulRejections,
  NO_REFERENCE_INPUT_WRONGFUL_REJECTION_VIOLATION_ID,
} from "../no-reference-input/wrongful-rejection.js";
import { resolveNoReferenceInputDeploymentContracts } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import { submitNoReferenceInputStep01 } from "../submit-no-reference-input-step-01.js";
import { submitNoReferenceInputStep02 } from "../submit-no-reference-input-step-02.js";
import { submitNoReferenceInputStep03 } from "../submit-no-reference-input-step-03.js";
import { submitNoReferenceInputStep04 } from "../submit-no-reference-input-step-04.js";
import { completeCanonicalReplayPredecessorEvidence } from "./complete-replay.js";
import { type ManifestBoundLinearFamilyWorkflowConfig } from "./family-definition.js";
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
} from "./no-reference-input.capture-removal.js";
import { captureLocallyEvaluatedTransaction } from "./transaction-boundary.js";

export const createTransactionPort = (
  config: BoundConfig,
): LinearFamilyTransactionPort<"noReferenceInput"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "noReferenceInput",
  prepare: async ({ evidence, replayContext, classification }) => {
    if (
      classification.selected.violationId ===
      NO_REFERENCE_INPUT_WRONGFUL_REJECTION_VIOLATION_ID
    ) {
      const detections = await detectNoReferenceInputWrongfulRejections({
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
        classification.category !== "noReferenceInput" ||
        classification.headerHash !== evidence.headerHash ||
        detected === undefined
      )
        throw new Error("noReferenceInput: forced classification changed");
      return noReferenceInputForcedArtifact(detected.prepared);
    }
    return await prepareLedgerAbsenceArtifact({
      category: "noReferenceInput",
      evidence,
      replayContext,
      classification,
      owner: config.signer.paymentKeyHash,
    });
  },
  capture: async ({ action, artifact }) => {
    if (artifact.schemaVersion === NO_REFERENCE_INPUT_FORCED_ARTIFACT) {
      const prepared = await admitNoReferenceInputForcedArtifact(artifact);
      if (prepared.headerHash !== config.binding.definition.headerHash)
        throw new Error("noReferenceInput: forced workflow header changed");
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
                fraudCategory: "noReferenceInput",
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
        throw new Error("noReferenceInput: unknown forced stage");
      const { contracts, noReferenceInputCategory } =
        await resolveNoReferenceInputDeploymentContracts({
          blueprint: config.binding.blueprint,
          deploymentInfo: config.binding.deploymentInfo,
          network: config.binding.network,
          requireFraudProofSpend: true,
        });
      const carriage =
        stepIndex === 1
          ? await resolveField(
              config,
              noReferenceInputForcedFieldPlan(
                prepared,
                config.signer.paymentKeyHash,
              ),
            )
          : null;
      return {
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitNoReferenceInputForcedStep({
              lucid: config.lucid,
              contracts: {
                steps: contracts.noReferenceInput.steps,
                computationThread: contracts.computationThread,
                fraudProof: contracts.fraudProof,
              },
              categoryId: noReferenceInputCategory.categoryId,
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
      admitted.artifact.category !== "noReferenceInput" ||
      admitted.artifact.headerHash !== config.binding.definition.headerHash
    ) {
      throw new Error("no-reference-input artifact changed workflow identity");
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
              fraudCategory: "noReferenceInput",
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
            await submitNoReferenceInputStep01({
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
            await submitNoReferenceInputStep02({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              deploymentInfo: config.binding.deploymentInfo,
              network: config.binding.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              referenceInputsPreimage: admitted.inputPreimage.map(
                (candidate) => ({
                  txId: candidate.tx_id,
                  index: candidate.output_index,
                }),
              ),
              nativeTxCompactCbor: admitted.artifact.badTx.nativeTxCompactCbor,
              badReferenceInputIndex: BigInt(admitted.artifact.badInputIndex),
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
            await submitNoReferenceInputStep03({
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
            await submitNoReferenceInputStep04({
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
      `no-reference-input workflow cannot execute ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundNoReferenceInputWorkflowConfig =
  ManifestBoundLinearFamilyWorkflowConfig<
    "noReferenceInput",
    (typeof WITNESS_ROLES)[number],
    true
  >;
