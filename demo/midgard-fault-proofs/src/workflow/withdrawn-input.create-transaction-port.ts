import {
  FraudProofComputationThreadStepDatum,
  WithdrawnInputStep02Datum,
  WithdrawnInputStep03Datum,
} from "@al-ft/midgard-sdk";

import type { WithdrawnInputContracts } from "../withdrawn-input/contracts.js";
import { submitWithdrawnInputInit } from "../withdrawn-input/submit-withdrawn-input-init.js";
import { submitWithdrawnInputStep01 } from "../withdrawn-input/submit-withdrawn-input-step-01.js";
import { submitWithdrawnInputStep02 } from "../withdrawn-input/submit-withdrawn-input-step-02.js";
import { submitWithdrawnInputStep03 } from "../withdrawn-input/submit-withdrawn-input-step-03.js";
import { WITHDRAWN_INPUT_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import {
  defineLinearFamily,
  type ManifestBoundLinearFamilyWorkflow,
  type ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.js";
import { type FieldCarriageRequirement } from "./field-carriage-prerequisite.js";
import {
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.js";
import {
  admitNativeInclusionArtifact,
  exactJournalRecord,
} from "./native-index-artifact.js";
import { resolveDirectFirstProofChunks } from "./proof-chunk-prerequisite.js";
import { captureLocallyEvaluatedTransaction } from "./transaction-boundary.js";
import { admitWithdrawnInputArtifact } from "./withdrawn-input.admit-withdrawn-input-artifact.js";
import {
  actionInput,
  type AssemblyContext,
  type BoundConfig,
  captureRemoval,
  prepareWithdrawnInputArtifact,
  resolveField,
  stringField,
  WITNESS_ROLES,
} from "./withdrawn-input.prepare-withdrawn-input-artifact.js";

const createTransactionPort = (
  config: BoundConfig,
): LinearFamilyTransactionPort<"withdrawnInput"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "withdrawnInput",
  prepare: async ({ evidence, classification }) =>
    await prepareWithdrawnInputArtifact({
      evidence,
      classification,
    }),
  capture: async ({ action, artifact }) => {
    const admitted = await admitWithdrawnInputArtifact(
      artifact,
      config.signer.paymentKeyHash,
    );
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error("withdrawn-input artifact changed header");
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitWithdrawnInputInit({
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
      const chunks = await resolveDirectFirstProofChunks({
        action,
        lucid: config.lucid,
        address: config.signer.address,
        proofCbor: admitted.artifact.tx.txMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitWithdrawnInputStep01({
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
      const field = await resolveField(config, admitted.spendPlan);
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitWithdrawnInputStep02({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              evidence: {
                inputs: admitted.spendInputs,
                badInputIndex: admitted.artifact.badInputIndex,
                nativeTxCompactCbor: admitted.artifact.tx.nativeTxCompactCbor,
              },
              referenceScriptUtxo: config.referenceScripts.steps[1],
              publishedCarriageUtxos: field.publications,
              ...(field.certificate === undefined
                ? {}
                : { certificateUtxo: field.certificate }),
              publishMissingCarriage: false,
              publishCarriage: false,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "step_03") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitWithdrawnInputStep03({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              withdrawalMembership: admitted.withdrawalMembership,
              referenceScriptUtxo: config.referenceScripts.steps[2],
              witnessReferenceScripts: config.referenceScripts.witnesses,
              preSubmitBoundary,
              awaitConfirmation: false,
            });
          },
        ),
      });
    }
    if (input.stage === "remove") return await captureRemoval(config, input);
    throw new Error(`unsupported withdrawn-input stage ${String(input.stage)}`);
  },
});

export type ManifestBoundWithdrawnInputWorkflowConfig =
  ManifestBoundLinearFamilyWorkflowConfig<
    "withdrawnInput",
    (typeof WITNESS_ROLES)[number],
    true
  >;

export type ManifestBoundWithdrawnInputWorkflow =
  ManifestBoundLinearFamilyWorkflow<"withdrawnInput", true>;

const contracts = (context: AssemblyContext): WithdrawnInputContracts => {
  const { binding, certificate } = context;
  const chain = binding.resolvedContracts.contracts.withdrawnInput;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (
    chain === undefined ||
    chain.steps.length !== 3 ||
    stateQueuePolicyId === undefined
  ) {
    throw new Error("withdrawn-input deployment chain is incomplete");
  }
  return Object.freeze({
    steps: [chain.steps[0]!, chain.steps[1]!, chain.steps[2]!] as const,
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: {
      policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
      mintingScript:
        binding.resolvedContracts.contracts.fraudProof.mintingScript,
      spendingScriptHash:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptHash,
      spendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
    fieldPreimageCertificatePolicyId: certificate.policyId,
  });
};

export const WITHDRAWN_INPUT_FAMILY_DEFINITION = defineLinearFamily({
  category: "withdrawnInput",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    WithdrawnInputStep02Datum,
    WithdrawnInputStep03Datum,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => WITHDRAWN_INPUT_COMPLETE_CANONICAL_REPLAY,
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
  // Step-02 opens the accepted transaction's spend inputs.
  fieldCarriage: [
    {
      requirementForAction: async (context, { action, artifact }) => {
        if (actionInput(action).stage !== "step_02") return null;
        const admitted = await admitWithdrawnInputArtifact(
          artifact,
          context.signer.paymentKeyHash,
        );
        return {
          planned: admitted.spendPlan,
          compactCbor: admitted.spendPlan.nativeTxCompactCbor,
          certificate: {
            policyId: context.certificate.policyId,
            mintingScript: context.certificate.mintingScript,
            referenceScriptUtxo:
              context.references.fieldPreimageCertificateMint,
          },
        } satisfies FieldCarriageRequirement;
      },
    },
  ],
  proofChunk: (_context, { action, artifact }) => {
    if (actionInput(action).stage !== "step_01") return null;
    const record = exactJournalRecord(
      artifact,
      [
        "schemaVersion",
        "headerHash",
        "detectionId",
        "position",
        "tx",
        "spendInputs",
        "badInputIndex",
        "withdrawalIndex",
        "withdrawalMembershipCbor",
      ],
      "withdrawn-input proof-chunk artifact",
    );
    return admitNativeInclusionArtifact(
      record.tx,
      "withdrawn-input proof-chunk transaction",
    ).artifact.txMembershipProofCbor;
  },
});

export const createManifestBoundWithdrawnInputWorkflow = (
  config: ManifestBoundWithdrawnInputWorkflowConfig,
): Promise<ManifestBoundWithdrawnInputWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    WITHDRAWN_INPUT_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundWithdrawnInputWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;
