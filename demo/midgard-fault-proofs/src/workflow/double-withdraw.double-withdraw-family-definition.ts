import {
  DoubleWithdrawStep02Datum,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";

import type { DoubleWithdrawContracts } from "../double-withdraw/contracts.js";
import { DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY } from "./complete-replay.js";
import {
  type AssemblyContext,
  type BoundDoubleWithdrawTransactionsConfig,
  createBoundTransactionPort,
  type DoubleWithdrawBuilderSet,
  type ManifestBoundDoubleWithdrawWorkflow,
  type ManifestBoundDoubleWithdrawWorkflowConfig,
  productionBuilders,
  WITNESS_ROLES,
} from "./double-withdraw.create-bound-transaction-port.js";
import { defineLinearFamily } from "./family-definition.js";
import { type LinearFamilyTransactionPort } from "./linear-family-adapter.js";
import {
  assembleManifestBoundFamilyWorkflow,
  runOrResumeManifestBoundFamilyWorkflow,
} from "./manifest-bound-family-assembly.js";

const contracts = (context: AssemblyContext): DoubleWithdrawContracts => {
  const { binding } = context;
  const chain = binding.resolvedContracts.contracts.doubleWithdraw;
  const stateQueuePolicyId = binding.resolvedContracts.stateQueuePolicyId;
  if (chain === undefined || stateQueuePolicyId === undefined) {
    throw new Error(
      "double-withdraw manifest binding omitted required contracts",
    );
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

export const DOUBLE_WITHDRAW_FAMILY_DEFINITION = defineLinearFamily({
  category: "doubleWithdraw",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    DoubleWithdrawStep02Datum,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: false,
  replayer: () => DOUBLE_WITHDRAW_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "linear",
    transactionPort: (context) =>
      createBoundTransactionPort({
        config: {
          lucid: context.lucid,
          blueprint: context.binding.blueprint,
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
          deploymentInfo: context.binding.deploymentInfo,
        },
        builders: productionBuilders,
      }),
  },
});

export const createManifestBoundDoubleWithdrawWorkflow = (
  config: ManifestBoundDoubleWithdrawWorkflowConfig,
): Promise<ManifestBoundDoubleWithdrawWorkflow> =>
  assembleManifestBoundFamilyWorkflow(
    DOUBLE_WITHDRAW_FAMILY_DEFINITION,
    config,
  );

export const runOrResumeManifestBoundDoubleWithdrawWorkflow =
  runOrResumeManifestBoundFamilyWorkflow;

export const unsafeCreateDoubleWithdrawTransactionPortForTest = (input: {
  readonly config: BoundDoubleWithdrawTransactionsConfig;
  readonly builders: DoubleWithdrawBuilderSet;
}): LinearFamilyTransactionPort<"doubleWithdraw"> =>
  createBoundTransactionPort(input);
