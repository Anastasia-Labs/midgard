import {
  FraudProofComputationThreadStepDatum,
  MinFeeStep02Datum,
} from "@al-ft/midgard-sdk";

import { MIN_FEE_FORCED_ARTIFACT } from "../min-fee-forced-artifact.js";
import { MIN_FEE_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import { defineLinearFamily } from "./family-definition.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.js";
import {
  admitMinFeeArtifact,
  WITNESS_ROLES,
} from "./min-fee.capture-removal.js";
import {
  contracts,
  createTransactionPort,
  fieldCarriageForField,
  type ManifestBoundMinFeeWorkflow,
  type ManifestBoundMinFeeWorkflowConfig,
} from "./min-fee.create-transaction-port.js";
import { FIELD_COUNT, record } from "./min-fee.parse-artifact.js";

export const MIN_FEE_FAMILY_DEFINITION = defineLinearFamily({
  category: "minFee",
  stepDatumSchemas: [FraudProofComputationThreadStepDatum, MinFeeStep02Datum],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => MIN_FEE_COMPLETE_CANONICAL_REPLAY,
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
        stateQueueMutationLeaseCoordinator:
          context.stateQueueMutationLeaseCoordinator,
        fraudProverRewardLovelace: BigInt(
          context.binding.releaseEconomics.policy.fraudProverRewardLovelace,
        ),
      }),
  },
  fieldCarriage: Array.from({ length: FIELD_COUNT }, (_, index) =>
    fieldCarriageForField(index),
  ),
  proofChunk: (context, { action, artifact }) => {
    const input = record(action.input, "min-fee proof prerequisite action");
    return input.stage === "step_01" &&
      artifact.schemaVersion !== MIN_FEE_FORCED_ARTIFACT
      ? admitMinFeeArtifact(artifact, context.signer.paymentKeyHash).artifact
          .txMembershipProofCbor
      : null;
  },
});

export const createManifestBoundMinFeeWorkflow = (
  config: ManifestBoundMinFeeWorkflowConfig,
): Promise<ManifestBoundMinFeeWorkflow> =>
  assembleManifestBoundFamilyWorkflow(MIN_FEE_FAMILY_DEFINITION, config);

export const runOrResumeManifestBoundMinFeeWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;
