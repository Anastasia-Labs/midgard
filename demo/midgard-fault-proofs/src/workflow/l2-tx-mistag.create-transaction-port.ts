import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";

import type { L2TxMistagContracts } from "../l2-tx-mistag/contracts.js";
import { L2TxMistagStep02Datum } from "../l2-tx-mistag/schemas.js";
import { submitL2TxMistagInit } from "../l2-tx-mistag/submit-l2-tx-mistag-init.js";
import { submitL2TxMistagStep01 } from "../l2-tx-mistag/submit-l2-tx-mistag-step-01.js";
import { submitL2TxMistagStep02 } from "../l2-tx-mistag/submit-l2-tx-mistag-step-02.js";
import {
  type StateQueueMutationLease,
  type StateQueueMutationLeaseCoordinator,
  submitRemoveFraudulentBlock,
} from "../remove-fraudulent-block.js";
import type { ResolvedProverSigner } from "../runtime.js";
import { L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import {
  defineLinearFamily,
  type ManifestBoundLinearFamilyWorkflow,
  type ManifestBoundLinearFamilyWorkflowConfig,
} from "./family-definition.js";
import {
  admitL2TxMistagArtifact,
  type AssemblyContext,
  type L2TxMistagWorkflowReferenceScripts,
  parseArtifact,
  prepareL2TxMistagArtifact,
  record,
  WITNESS_ROLES,
} from "./l2-tx-mistag.parse-artifact.js";
import {
  LINEAR_FAMILY_TRANSACTION_PORT,
  type LinearFamilyTransactionPort,
} from "./linear-family-adapter.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.js";
import { type FraudProofWorkflowAction } from "./orchestrator.js";
import { resolveDirectFirstProofChunks } from "./proof-chunk-prerequisite.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
  workflowTransactionReferenceInputOutRefs,
} from "./transaction-boundary.js";

type BoundConfig = Readonly<{
  lucid: LucidEvolution;
  blueprint: unknown;
  deploymentInfo: unknown;
  network: FraudProofWorkflowDeploymentBinding<"l2TxMistag">["network"];
  signer: ResolvedProverSigner;
  headerHash: string;
  contracts: L2TxMistagContracts;
  category: FraudProofWorkflowDeploymentBinding<"l2TxMistag">["resolvedContracts"]["category"];
  catalogue: FraudProofWorkflowDeploymentBinding<"l2TxMistag">["catalogue"];
  referenceScripts: L2TxMistagWorkflowReferenceScripts;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  fraudProverRewardLovelace: bigint;
}>;

const actionInput = (
  action: FraudProofWorkflowAction,
): Readonly<Record<string, unknown>> => {
  const input = record(action.input, "l2-tx-mistag workflow action");
  if (
    input.schemaVersion !== "midgard-production-linear-family-action-v1" ||
    input.category !== "l2TxMistag" ||
    typeof input.stage !== "string"
  ) {
    throw new Error("l2-tx-mistag workflow action changed identity");
  }
  return input;
};

const stringField = (
  input: Readonly<Record<string, unknown>>,
  field: string,
): string => {
  const value = input[field];
  if (typeof value !== "string") {
    throw new Error(`l2-tx-mistag workflow action omitted ${field}`);
  }
  return value;
};

const captureRemoval = async ({
  config,
  input,
}: {
  readonly config: BoundConfig;
  readonly input: Readonly<Record<string, unknown>>;
}) => {
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
        fraudCategory: "l2TxMistag",
        fraudulentHeaderHash: config.headerHash,
        requireReferenceScripts: true,
        stateQueueMutationLeaseCoordinator: retainingCoordinator,
        fraudProverRewardLovelace: config.fraudProverRewardLovelace,
        preSubmitBoundary: async (built) => {
          if (
            !workflowTransactionInputOutRefs(built.signed).includes(
              nextRemovalOutRef,
            )
          ) {
            throw new Error(
              "l2-tx-mistag removal changed its authenticated queue input",
            );
          }
          if (
            !workflowTransactionReferenceInputOutRefs(built.signed).includes(
              fraudProofOutRef,
            )
          ) {
            throw new Error(
              "l2-tx-mistag removal did not reference the retained proof token",
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

const createTransactionPort = (
  config: BoundConfig,
): LinearFamilyTransactionPort<"l2TxMistag"> => ({
  portVersion: LINEAR_FAMILY_TRANSACTION_PORT,
  category: "l2TxMistag",
  prepare: async ({ evidence, classification }) =>
    await prepareL2TxMistagArtifact({ evidence, classification }),
  capture: async ({ action, artifact }) => {
    const admitted = await admitL2TxMistagArtifact(artifact);
    if (admitted.artifact.headerHash !== config.headerHash) {
      throw new Error("l2-tx-mistag artifact changed workflow header");
    }
    const input = actionInput(action);
    if (input.stage === "init") {
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitL2TxMistagInit({
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
        proofCbor: admitted.artifact.txMembershipProofCbor,
      });
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitL2TxMistagStep01({
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
      return Object.freeze({
        transaction: await captureLocallyEvaluatedTransaction(
          async (preSubmitBoundary) => {
            await submitL2TxMistagStep02({
              lucid: config.lucid,
              contracts: config.contracts,
              categoryId: config.category.categoryId,
              signer: config.signer,
              threadOutRef: stringField(input, "threadOutRef"),
              referenceScriptUtxo: config.referenceScripts.steps[1],
              witnessReferenceScripts: config.referenceScripts.witnesses,
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
      `l2-tx-mistag workflow action has unsupported stage ${String(input.stage)}`,
    );
  },
});

export type ManifestBoundL2TxMistagWorkflowConfig =
  ManifestBoundLinearFamilyWorkflowConfig<
    "l2TxMistag",
    (typeof WITNESS_ROLES)[number],
    false
  >;

export type ManifestBoundL2TxMistagWorkflow = ManifestBoundLinearFamilyWorkflow<
  "l2TxMistag",
  false
>;

const contracts = (context: AssemblyContext): L2TxMistagContracts => {
  const { binding } = context;
  const chain = binding.resolvedContracts.contracts.l2TxMistag;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (chain === undefined || stateQueuePolicyId === undefined) {
    throw new Error("l2-tx-mistag manifest omitted required contracts");
  }
  return Object.freeze({
    steps: chain.steps,
    computationThread: binding.resolvedContracts.contracts.computationThread,
    fraudProof: {
      policyId: binding.resolvedContracts.contracts.fraudProof.policyId,
      mintingScript:
        binding.resolvedContracts.contracts.fraudProof.mintingScript,
      spendingScriptAddress:
        binding.resolvedContracts.contracts.fraudProof.spendingScriptAddress,
    },
    hubOraclePolicyId: binding.resolvedContracts.hubOraclePolicyId,
    stateQueuePolicyId,
  });
};

export const L2_TX_MISTAG_FAMILY_DEFINITION = defineLinearFamily({
  category: "l2TxMistag",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    L2TxMistagStep02Datum,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: false,
  replayer: () => L2_TX_MISTAG_COMPLETE_CANONICAL_REPLAY,
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
  // Step-01 publishes the transaction membership proof as chunks.
  proofChunk: (_context, { action, artifact }) => {
    const admitted = parseArtifact(artifact);
    return action.input.stage === "step_01"
      ? admitted.txMembershipProofCbor
      : null;
  },
});

export const createManifestBoundL2TxMistagWorkflow = (
  config: ManifestBoundL2TxMistagWorkflowConfig,
): Promise<ManifestBoundL2TxMistagWorkflow> =>
  assembleManifestBoundFamilyWorkflow(L2_TX_MISTAG_FAMILY_DEFINITION, config);

export const runOrResumeManifestBoundL2TxMistagWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;
