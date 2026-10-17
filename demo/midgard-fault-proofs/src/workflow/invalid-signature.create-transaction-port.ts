import {
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import { prepareInvalidSignatureForcedArtifact } from "../invalid-signature/artifact.js";
import { submitInvalidSignatureStep01Forced } from "../invalid-signature/submit.js";
import {
  detectInvalidSignatureWrongfulRejections,
  INVALID_SIGNATURE_WRONGFUL_REJECTION_VIOLATION_ID,
} from "../invalid-signature/wrongful-rejection.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import { resolveInvalidSignatureDeploymentContracts } from "../runtime.js";
import { submitInit } from "../submit-init.js";
import { submitInvalidSignatureStep01 } from "../submit-invalid-signature-step-01.js";
import { submitInvalidSignatureStep02 } from "../submit-invalid-signature-step-02.js";
import {
  type ManifestBoundLinearFamilyWorkflow,
  type ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.js";
import {
  actionInput,
  admitWorkflowArtifact,
  type BoundConfig,
  prepareInvalidSignatureArtifact,
  WITNESS_ROLES,
} from "./invalid-signature.admit-invalid-signature-artifact.js";
import { type AdmittedArtifact } from "./invalid-signature.parse-address-witnesses.js";
import {
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import { resolveDirectFirstProofChunks } from "./proof-chunk-prerequisite.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`invalid-signature workflow action omitted ${field}`);
  }
  return value;
};

const captureRemoval = async (
  config: BoundConfig,
  input: Readonly<Record<string, unknown>>,
) => {
  let mutationLease: StateQueueMutationLease | undefined;
  const retainingCoordinator: StateQueueMutationLeaseCoordinator = {
    acquire: async () => {
      const acquired =
        await config.stateQueueMutationLeaseCoordinator.acquire();
      mutationLease = acquired;
      return acquired;
    },
  };
  const nextRemovalOutRef = stringField(input, "nextRemovalOutRef");
  const fraudProofOutRef = stringField(input, "fraudProofOutRef");
  const transaction = await captureLocallyEvaluatedTransaction(
    async (boundary) => {
      await submitRemoveFraudulentBlock({
        lucid: config.lucid,
        blueprint: config.blueprint,
        deploymentInfo: config.deploymentInfo,
        network: config.network,
        signer: config.signer,
        fraudCategory: "invalidSignature",
        fraudulentHeaderHash: config.headerHash,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: retainingCoordinator,
        fraudProverRewardLovelace: config.fraudProverRewardLovelace,
        preSubmitBoundary: async (built) => {
          if (
            !workflowTransactionInputOutRefs(built.signed).includes(
              nextRemovalOutRef,
            ) ||
            !workflowTransactionReferenceInputOutRefs(built.signed).includes(
              fraudProofOutRef,
            )
          ) {
            throw new Error(
              "invalid-signature removal changed its authenticated queue/proof inputs",
            );
          }
          await boundary(built);
        },
      });
    },
  );
  return Object.freeze({
    transaction,
    ...(mutationLease === undefined ? {} : { mutationLease }),
  });
};

const resolveFieldCarriage = async (
  config: BoundConfig,
  admitted: Pick<AdmittedArtifact, "fieldPlan">,
) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    lucid: config.lucid,
    publisherAddress: config.signer.address,
    planned: admitted.fieldPlan,
  });
  if (publications === undefined) {
    throw new Error(
      "invalid-signature field publications disappeared after authenticated prerequisite",
    );
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    lucid: config.lucid,
    network: config.network,
    planned: admitted.fieldPlan,
    certificatePolicyId: config.certificate.policyId,
  });
  if (
    admitted.fieldPlan.plan.tier === "Certified" &&
    certificate === undefined
  ) {
    throw new Error(
      "invalid-signature field certificate disappeared after authenticated prerequisite",
    );
  }
  return Object.freeze({
    publications,
    certificates: certificate === undefined ? [] : [certificate],
  });
};

export const createTransactionPort = (
  config: BoundConfig,
): LinearFamilyTransactionPort<"invalidSignature"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "invalidSignature",
  prepare: async ({ evidence, classification }) => {
    if (
      classification.selected.violationId ===
      INVALID_SIGNATURE_WRONGFUL_REJECTION_VIOLATION_ID
    ) {
      const detected = detectInvalidSignatureWrongfulRejections({
        block: evidence,
      })[0];
      if (
        classification.category !== "invalidSignature" ||
        classification.headerHash !== evidence.headerHash ||
        detected?.detectionId !== classification.selected.detectionId
      )
        throw new Error(
          "invalid-signature forced classification changed authenticated evidence",
        );
      return await prepareInvalidSignatureForcedArtifact({ block: evidence });
    }
    return await prepareInvalidSignatureArtifact({ evidence, classification });
  },
  capture: async ({ action, artifact }) => {
    const admitted = await admitWorkflowArtifact(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error(
        "invalid-signature artifact changed its manifest-bound header",
      );
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitInit({
              lucid: config.lucid,
              blueprint: config.blueprint,
              deploymentInfo: config.deploymentInfo,
              network: config.network,
              signer: config.signer,
              fraudCategory: "invalidSignature",
              fraudulentBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              fraudulentHeaderHash: config.headerHash,
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_01") {
      if (admitted.forced !== null) {
        const { contracts, invalidSignatureCategory } =
          await resolveInvalidSignatureDeploymentContracts({
            blueprint: config.blueprint,
            deploymentInfo: config.deploymentInfo,
            network: config.network,
            requireFraudProofSpend: true,
          });
        const forced = admitted.forced;
        return {
          transaction: await captureLocallyEvaluatedTransaction(
            async (preSubmitBoundary) => {
              await submitInvalidSignatureStep01Forced({
                lucid: config.lucid,
                contracts: {
                  steps: contracts.invalidSignature.steps.map(
                    (step, index) => ({
                      ...step,
                      blueprintTitle: `fraud_proofs/invalid_signature/step_0${index + 1}.main.spend`,
                      referenceOutRef: `${config.referenceScripts.steps[index]!.txHash}#${config.referenceScripts.steps[index]!.outputIndex}`,
                    }),
                  ) as never,
                  computationThread: contracts.computationThread,
                  fraudProof: contracts.fraudProof,
                },
                categoryId: invalidSignatureCategory.categoryId,
                signer: config.signer,
                threadOutRef: stringField(input, "threadOutRef"),
                evidence: forced.evidence,
                forcedSource: forced.forcedSource,
                referenceScriptUtxo: config.referenceScripts.steps[0],
                preSubmitBoundary,
                awaitConfirmation: false,
              });
            },
          ),
        };
      }
      if (admitted.inclusion === null)
        throw new Error("invalid-signature accepted inclusion absent");
      const chunks = await resolveDirectFirstProofChunks({
        action,
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.artifact.txMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitInvalidSignatureStep01({
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
              txInclusion: admitted.inclusion!,
              badTxWitnessSetCompact: admitted.witnessSet,
              publishedProofChunks: chunks,
              referenceScriptUtxo: config.referenceScripts.steps[0],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_02") {
      const carriage = await resolveFieldCarriage(config, admitted);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitInvalidSignatureStep02({
              lucid: config.lucid,
              blueprint: config.blueprint,
              deploymentInfo: config.deploymentInfo,
              network: config.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              addrTxWitsPreimage: admitted.addressWitnesses,
              nativeTxCompactCbor: admitted.artifact.nativeTxCompactCbor,
              witnessSetCompact: admitted.witnessSet,
              badAddrTxWitIndex: BigInt(admitted.artifact.badWitnessIndex),
              referenceScriptUtxo: config.referenceScripts.steps[1],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              certificatePolicyId: config.certificate.policyId,
              certificateUtxos: carriage.certificates,
              existingPublicationUtxos: carriage.publications,
              publishMissingCarriage: false,
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
      `invalid-signature workflow action has unsupported stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundInvalidSignatureWorkflowConfig =
  ManifestBoundLinearFamilyWorkflowConfig<
    "invalidSignature",
    (typeof WITNESS_ROLES)[number],
    true
  >;

export type ManifestBoundInvalidSignatureWorkflow =
  ManifestBoundLinearFamilyWorkflow<"invalidSignature", true>;
