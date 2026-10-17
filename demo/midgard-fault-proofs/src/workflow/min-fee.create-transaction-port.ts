import { encodeMidgardNativeTxWitnessSetCompact } from "@al-ft/midgard-core";
import { MIN_FEE_VIOLATION_ID } from "@al-ft/midgard-sdk";
import type { UTxO } from "@lucid-evolution/lucid";

import {
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import type { MinFeeContracts } from "../min-fee-contracts.js";
import { prepareMinFeeForcedArtifact } from "../min-fee-forced-artifact.js";
import { submitMinFeeStep01Forced } from "../submit-min-fee-forced-step-01.js";
import { submitMinFeeInit } from "../submit-min-fee-init.js";
import { submitMinFeeStep01 } from "../submit-min-fee-step-01.js";
import { submitMinFeeStep02 } from "../submit-min-fee-step-02.js";
import {
  type LinearFamilyFieldCarriageRequirement,
  type ManifestBoundLinearFamilyWorkflow,
  type ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.js";
import type { FieldCarriageRequirement } from "./field-carriage-prerequisite.js";
import {
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import {
  actionInput,
  admitWorkflowArtifact,
  type AssemblyContext,
  type BoundConfig,
  captureRemoval,
  prepareMinFeeArtifact,
  stringField,
  WITNESS_ROLES,
} from "./min-fee.capture-removal.js";
import {
  type AdmittedMinFeeArtifact,
  record,
  witnessSetCore,
} from "./min-fee.parse-artifact.js";
import { resolveDirectFirstProofChunks } from "./proof-chunk-prerequisite.js";
import { captureLocallyEvaluatedTransaction } from "./transaction-boundary.js";

const resolveFieldCarriages = async (
  config: BoundConfig,
  admitted: Pick<AdmittedMinFeeArtifact, "fieldPlans">,
): Promise<
  Readonly<{ publications: readonly UTxO[]; certificates: readonly UTxO[] }>
> => {
  const publications: UTxO[] = [];
  const certificates: UTxO[] = [];
  for (const plan of admitted.fieldPlans) {
    const resolvedPublications =
      await resolveFaultProofFieldCarriagePublications({
        lucid: config.lucid,
        publisherAddress: config.signer.address,
        planned: plan,
      });
    if (resolvedPublications === undefined) {
      throw new Error(
        `min-fee field ${plan.fieldIndex.toString()} publication disappeared after authenticated prerequisite`,
      );
    }
    publications.push(...resolvedPublications);
    const certificate = await resolveFaultProofFieldPreimageCertificate({
      lucid: config.lucid,
      network: config.network,
      planned: plan,
      certificatePolicyId: config.contracts.fieldPreimageCertificatePolicyId,
    });
    if (plan.plan.tier === "Certified" && certificate === undefined) {
      throw new Error(
        `min-fee field ${plan.fieldIndex.toString()} certificate disappeared after authenticated prerequisite`,
      );
    }
    if (certificate !== undefined) certificates.push(certificate);
  }
  return Object.freeze({
    publications: Object.freeze(publications),
    certificates: Object.freeze(certificates),
  });
};

export const createTransactionPort = (
  config: BoundConfig,
): LinearFamilyTransactionPort<"minFee"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "minFee",
  prepare: async ({ evidence, classification }) => {
    if (
      classification.category !== "minFee" ||
      classification.headerHash !== evidence.headerHash ||
      classification.selected.violationId !== MIN_FEE_VIOLATION_ID
    )
      throw new Error("min-fee classification differs from canonical evidence");
    if (classification.selected.detectionId.startsWith("min-fee:forced:")) {
      const artifact = await prepareMinFeeForcedArtifact({
        block: evidence,
        detectionId: classification.selected.detectionId,
      });
      if (classification.selected.position !== BigInt(artifact.forcedIndex))
        throw new Error("min-fee forced classification position changed");
      return artifact;
    }
    return await prepareMinFeeArtifact({
      evidence,
      classification,
      categoryId: config.category.categoryId,
    });
  },
  capture: async ({ action, artifact }) => {
    const admitted = await admitWorkflowArtifact(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error("min-fee artifact changed its manifest-bound header");
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMinFeeInit({
              lucid: config.lucid,
              blueprint: config.blueprint,
              network: config.network,
              contracts: config.contracts,
              category: config.category,
              catalogue: config.catalogue,
              signer: config.signer,
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
      if (admitted.forced !== null)
        return Object.freeze({
          transaction: await captureLocallyEvaluatedTransaction(
            async (preSubmitBoundary) => {
              await submitMinFeeStep01Forced({
                lucid: config.lucid,
                contracts: config.contracts,
                categoryId: config.category.categoryId,
                signer: config.signer,
                threadOutRef: stringField(input, "threadOutRef"),
                state: admitted.forced.evidence.state,
                forcedSource: admitted.forced.forcedSource,
                referenceScriptUtxo: config.referenceScripts.steps[0],
                preSubmitBoundary,
                awaitConfirmation: false,
              });
            },
          ),
        });
      const chunks = await resolveDirectFirstProofChunks({
        action,
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.artifact.txMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMinFeeStep01({
              lucid: config.lucid,
              blueprint: config.blueprint,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              network: config.network,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              stateQueueBlockOutRef: stringField(
                input,
                "stateQueueBlockOutRef",
              ),
              txInclusion: admitted.inclusion,
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
      const carriages = await resolveFieldCarriages(config, admitted);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitMinFeeStep02({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              nativeTxCompactCbor: admitted.artifact.nativeTxCompactCbor,
              witnessSet: admitted.witnessSet,
              fieldItemCbors: admitted.fieldItemCbors,
              referenceScriptUtxo: config.referenceScripts.steps[1],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              certificateUtxos: carriages.certificates,
              existingPublicationUtxos: carriages.publications,
              publishMissingCarriages: false,
              publishCarriages: admitted.forced !== null,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "remove") {
      return await captureRemoval({ config, input });
    }
    throw new Error(
      `min-fee workflow action has unsupported stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundMinFeeWorkflowConfig =
  ManifestBoundLinearFamilyWorkflowConfig<
    "minFee",
    (typeof WITNESS_ROLES)[number],
    true
  >;

export type ManifestBoundMinFeeWorkflow = ManifestBoundLinearFamilyWorkflow<
  "minFee",
  true
>;

export const contracts = (context: AssemblyContext): MinFeeContracts => {
  const resolved = context.binding.resolvedContracts;
  const chain = resolved.contracts.minFee;
  const stateQueuePolicyId = resolved.stateQueuePolicyId;
  if (chain === undefined || stateQueuePolicyId === undefined) {
    throw new Error("min-fee manifest binding omitted required contracts");
  }
  return Object.freeze({
    steps: chain.steps,
    computationThread: resolved.contracts.computationThread,
    fraudProof: {
      policyId: resolved.contracts.fraudProof.policyId,
      mintingScript: resolved.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        resolved.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: resolved.hubOraclePolicyId,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: context.certificate.policyId,
  });
};

/**
 * One prerequisite per field. Declared from field 0 upward so field 0 is the
 * innermost decoration and therefore the first action observed, followed
 * deterministically by fields 1..8 before the proof step can execute.
 */
export const fieldCarriageForField = (
  index: number,
): LinearFamilyFieldCarriageRequirement<
  "minFee",
  (typeof WITNESS_ROLES)[number],
  true
> => ({
  requirementForAction: async (context, { action, artifact }) => {
    const input = record(action.input, "min-fee prerequisite action");
    if (input.stage !== "step_02") return null;
    const admitted = await admitWorkflowArtifact(
      artifact,
      context.signer.paymentKeyHash,
    );
    const planned = admitted.fieldPlans[index];
    if (planned === undefined) {
      throw new Error(`min-fee artifact omitted field ${index.toString()}`);
    }
    return {
      planned,
      compactCbor: admitted.artifact.nativeTxCompactCbor,
      witnessSetCompactCbor: encodeMidgardNativeTxWitnessSetCompact(
        witnessSetCore(admitted.witnessSet),
      ).toString("hex"),
      certificate: {
        policyId: context.certificate.policyId,
        mintingScript: context.certificate.mintingScript,
        referenceScriptUtxo: context.references.fieldPreimageCertificateMint,
      },
    } satisfies FieldCarriageRequirement;
  },
});
