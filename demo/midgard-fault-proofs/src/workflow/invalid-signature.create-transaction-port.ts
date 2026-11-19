import { resolveFaultProofFieldPreimageCertificate } from "../field-opening.certify-fault-proof-field-carriage.js";
import { resolveFaultProofFieldCarriagePublications } from "../field-opening.publish-fault-proof-field-carriage.js";
import { prepareInvalidSignatureForcedArtifact } from "../invalid-signature/artifact.js";
import { buildInvalidSignatureStep01Forced } from "../invalid-signature/submit.js";
import {
  detectInvalidSignatureWrongfulRejections,
  INVALID_SIGNATURE_WRONGFUL_REJECTION_VIOLATION_ID,
} from "../invalid-signature/wrongful-rejection.js";
import { buildRemoveFraudulentBlock } from "../remove-fraudulent-block.build-remove-fraudulent-block.js";
import { resolveInvalidSignatureDeploymentContracts } from "../runtime.js";
import { buildInit } from "../submit-init.submit-init.js";
import { buildInvalidSignatureStep01 } from "../submit-invalid-signature-step-01.js";
import { buildInvalidSignatureStep02 } from "../submit-invalid-signature-step-02.submit-invalid-signature-step02.js";
import {
  type ManifestBoundLinearFamilyWorkflow,
  type ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.js";
import { workflowFieldCarriageOwnOutputs } from "./field-carriage-prerequisite.own-carriage-claims.js";
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
import type { OwnOutputJournal } from "./own-outputs.js";
import { resolveDirectFirstProofChunks } from "./proof-chunk-prerequisite.route-action-identity.js";
import { admitRemovalTransaction } from "./removal-admission.js";
import { journaledStepTransaction } from "./step-transaction.js";
import {
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
      fraudCategory: "invalidSignature",
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
          "invalid-signature removal changed its authenticated queue/proof inputs",
        );
      }
    },
  );
};

const resolveFieldCarriage = async (
  config: BoundConfig,
  admitted: Pick<AdmittedArtifact, "fieldPlan">,
  ownOutputs: OwnOutputJournal,
) => {
  const publications = await resolveFaultProofFieldCarriagePublications({
    ownOutputs,
    lucid: config.lucid,
    publisherAddress: config.signer.proofMaterialsAddress,
    planned: admitted.fieldPlan,
  });
  if (publications === undefined) {
    throw new Error(
      "invalid-signature field publications disappeared after authenticated prerequisite",
    );
  }
  const certificate = await resolveFaultProofFieldPreimageCertificate({
    ownOutputs,
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
  capture: async ({ action, artifact, entries }) => {
    const ownOutputs = workflowFieldCarriageOwnOutputs({
      category: "invalidSignature",
      entries,
    });
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
        transaction: (
          await journaledStepTransaction(
            await buildInit({
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
            }),
            "invalid-signature buildInit",
          )
        ).transaction,
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
          transaction: (
            await journaledStepTransaction(
              await buildInvalidSignatureStep01Forced({
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
              }),
              "invalid-signature buildInvalidSignatureStep01Forced",
            )
          ).transaction,
        };
      }
      if (admitted.inclusion === null)
        throw new Error("invalid-signature accepted inclusion absent");
      const chunks = await resolveDirectFirstProofChunks({
        action,
        lucid: config.lucid,
        address: config.signer.proofMaterialsAddress,
        proofCbor: admitted.artifact.txMembershipProofCbor,
      });
      return Object.freeze({
        transaction: (
          await journaledStepTransaction(
            await buildInvalidSignatureStep01({
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
            }),
            "invalid-signature buildInvalidSignatureStep01",
          )
        ).transaction,
      });
    }
    if (input.stage === "step_02") {
      const carriage = await resolveFieldCarriage(config, admitted, ownOutputs);
      return Object.freeze({
        transaction: (
          await journaledStepTransaction(
            await buildInvalidSignatureStep02({
              ownOutputs,
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
            }),
            "invalid-signature buildInvalidSignatureStep02",
          )
        ).transaction,
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
