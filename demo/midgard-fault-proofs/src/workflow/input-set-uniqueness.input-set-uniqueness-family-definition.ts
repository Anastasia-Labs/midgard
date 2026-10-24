import {
  FraudProofComputationThreadStepDatum,
  InputSetUniquenessStep02Datum,
  InputSetUniquenessStep03DatumSchema,
  InputSetUniquenessStep04DatumSchema,
} from "@al-ft/midgard-sdk";

import { type FaultProofFieldOpeningPlan } from "../field-opening.js";
import { INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import {
  defineLinearFamily,
  type LinearFamilyFieldCarriageRequirement,
} from "./family-definition.js";
import type { FieldCarriageRequirement } from "./field-carriage-prerequisite.js";
import { type AdmittedArtifact } from "./input-set-uniqueness.admit-accepted-input-set-uniqueness-artifact.js";
import { admitAnyInputSetUniquenessArtifact } from "./input-set-uniqueness.admit-input-set-uniqueness-forced-artifact.js";
import {
  contracts,
  createTransactionPort,
  type ManifestBoundInputSetUniquenessWorkflow,
  type ManifestBoundInputSetUniquenessWorkflowConfig,
} from "./input-set-uniqueness.create-transaction-port.js";
import {
  actionInput,
  WITNESS_ROLES,
} from "./input-set-uniqueness.prepare-input-set-uniqueness-artifact.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.js";

/**
 * One field-carriage requirement per input field. The accepted artifact opens
 * its fields before step-02, the forced artifact before step-03 and step-04;
 * a claim that never touches a field yields no plan for it.
 */
const fieldCarriageFor = (
  selector: (artifact: AdmittedArtifact) => FaultProofFieldOpeningPlan | null,
): LinearFamilyFieldCarriageRequirement<
  "inputSetUniqueness",
  (typeof WITNESS_ROLES)[number],
  true
> => ({
  requirementForAction: (context, { action, artifact }) => {
    const input = actionInput(action);
    const admitted = admitAnyInputSetUniquenessArtifact(
      artifact,
      context.signer.paymentKeyHash,
    );
    if (
      (admitted.sourceKind === "accepted" && input.stage !== "step_02") ||
      (admitted.sourceKind === "forced" &&
        input.stage !== "step_03" &&
        input.stage !== "step_04")
    ) {
      return null;
    }
    const plan = selector(admitted);
    return plan === null
      ? null
      : ({
          planned: plan,
          compactCbor: plan.nativeTxCompactCbor,
          certificate: {
            policyId: context.certificate.policyId,
            mintingScript: context.certificate.mintingScript,
            referenceScriptUtxo:
              context.references.fieldPreimageCertificateMint,
          },
        } satisfies FieldCarriageRequirement);
  },
});

export const INPUT_SET_UNIQUENESS_FAMILY_DEFINITION = defineLinearFamily({
  category: "inputSetUniqueness",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    InputSetUniquenessStep02Datum,
    InputSetUniquenessStep03DatumSchema,
    InputSetUniquenessStep04DatumSchema,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => INPUT_SET_UNIQUENESS_COMPLETE_CANONICAL_REPLAY,
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
        contracts: contracts(context),
        category: context.binding.resolvedContracts.category,
        catalogue: context.binding.catalogue,
        referenceScripts: context.references,
        certificate: context.certificate,
        stateQueueMutationLeaseCoordinator:
          context.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          context.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      }),
  },
  // The spend plan is carried first, then the reference plan.
  fieldCarriage: [
    fieldCarriageFor((artifact) => artifact.spendPlan),
    fieldCarriageFor((artifact) => artifact.referencePlan),
  ],
  proofChunk: (context, { action, artifact }) => {
    if (actionInput(action).stage !== "step_01") return null;
    const admitted = admitAnyInputSetUniquenessArtifact(
      artifact,
      context.signer.paymentKeyHash,
    );
    return admitted.sourceKind === "accepted"
      ? admitted.artifact.tx.txMembershipProofCbor
      : null;
  },
});

export const createManifestBoundInputSetUniquenessWorkflow = (
  config: ManifestBoundInputSetUniquenessWorkflowConfig,
): Promise<ManifestBoundInputSetUniquenessWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    INPUT_SET_UNIQUENESS_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundInputSetUniquenessWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;
