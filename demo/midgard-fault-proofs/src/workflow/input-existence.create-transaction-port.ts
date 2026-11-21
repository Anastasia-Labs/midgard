import { resolveInputExistenceDeployment } from "../input-existence/deployment-step.js";
import { resolveInputExistenceFieldCarriage } from "../input-existence/field-carriage.js";
import type {
  InputExistenceCategory,
  InputExistenceKind,
} from "../input-existence/kind.js";
import { buildInputExistenceStep } from "../input-existence/submit.js";
import { buildInit } from "../submit-init.submit-init.js";
import {
  type ManifestBoundLinearFamilyWorkflow,
  type ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.js";
import { workflowFieldCarriageOwnOutputs } from "./field-carriage-prerequisite.own-carriage-claims.js";
import {
  type BoundConfig,
  inputExistenceRemoval,
  resolveChunks,
  WITNESS_ROLES,
} from "./input-existence.capture-removal.js";
import {
  admitLedgerAbsenceArtifact,
  inputExistenceProofForStage,
  prepareLedgerAbsenceArtifact,
} from "./ledger-absence-artifact.js";
import {
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import { journaledStepTransaction } from "./step-transaction.js";

/** A family's linear workflow transaction port. */
export const inputExistenceTransactionPort = <
  Category extends InputExistenceCategory,
>(
  kind: InputExistenceKind<Category>,
) => {
  const { category, name, fieldIndex } = kind;
  const { actionInput, captureRemoval, resolveField, stringField } =
    inputExistenceRemoval(kind);
  return (
    config: BoundConfig<Category>,
  ): LinearFamilyTransactionPort<Category> => ({
    portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
    category,
    prepare: async ({ evidence, replayContext, classification }) =>
      await prepareLedgerAbsenceArtifact({
        category,
        evidence,
        replayContext,
        classification,
        owner: config.signer.paymentKeyHash,
      }),
    capture: async ({ action, artifact, entries }) => {
      const ownOutputs = workflowFieldCarriageOwnOutputs({
        category,
        entries,
      });
      const admitted = admitLedgerAbsenceArtifact(
        artifact,
        config.signer.paymentKeyHash,
      );
      const prepared = admitted.prepared;
      if (
        admitted.artifact.category !== category ||
        prepared.source.headerHash !== config.binding.definition.headerHash
      ) {
        throw new Error(`${name} artifact changed workflow identity`);
      }
      const input = actionInput(action);
      if (input.stage === "remove") return await captureRemoval(config, input);
      if (input.stage === "init")
        return {
          transaction: (
            await journaledStepTransaction(
              await buildInit({
                lucid: config.lucid,
                blueprint: config.binding.blueprint,
                deploymentInfo: config.binding.deploymentInfo,
                network: config.binding.network,
                signer: config.signer,
                fraudCategory: category,
                fraudulentBlockOutRef: stringField(
                  input,
                  "stateQueueBlockOutRef",
                ),
                fraudulentHeaderHash: prepared.source.headerHash,
                witnessReferenceScripts: config.references.witnesses,
              }),
              "input-existence buildInit",
            )
          ).transaction,
        };
      const stepIndex = (
        ["step_01", "step_02", "step_03", "step_04"] as const
      ).findIndex((stage) => stage === input.stage);
      if (stepIndex < 0 || stepIndex > 3)
        throw new Error(`${name}: unknown physical step`);
      const { categoryId, contracts } = await resolveInputExistenceDeployment(
        fieldIndex,
        {
          blueprint: config.binding.blueprint,
          deploymentInfo: config.binding.deploymentInfo,
          network: config.binding.network,
          requireFraudProofSpend: true,
        },
      );
      const fieldCarriageUtxos =
        stepIndex === 1
          ? await resolveInputExistenceFieldCarriage({
              prepared,
              resolveField: (planned) =>
                resolveField(config, planned, ownOutputs),
            })
          : [];
      const proof = inputExistenceProofForStage(
        prepared,
        input.stage as string,
      );
      const chunks =
        proof === null
          ? []
          : await resolveChunks({ action, config, proofCbor: proof });
      return {
        transaction: (
          await journaledStepTransaction(
            await buildInputExistenceStep({
              lucid: config.lucid,
              blueprint: config.binding.blueprint,
              network: config.binding.network,
              contracts,
              categoryId,
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
            }),
            "input-existence buildInputExistenceStep",
          )
        ).transaction,
      };
    },
  });
};

export type ManifestBoundInputExistenceWorkflowConfig<
  Category extends InputExistenceCategory,
> = ManifestBoundLinearFamilyWorkflowConfig<
  Category,
  (typeof WITNESS_ROLES)[number],
  true
>;
export type ManifestBoundInputExistenceWorkflow<
  Category extends InputExistenceCategory,
> = ManifestBoundLinearFamilyWorkflow<Category, true>;
