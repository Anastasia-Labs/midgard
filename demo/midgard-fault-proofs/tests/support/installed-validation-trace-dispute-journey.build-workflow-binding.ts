import { DEPLOYMENT_MANIFEST_L1_FINALITY } from "@al-ft/midgard-core/deployment-manifest-identity";
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";
import { type UTxO, validatorToScriptHash } from "@lucid-evolution/lucid";

import { type resolveValidationTraceDisputeDeploymentContracts } from "../../src/index.js";
import {
  computeFraudProofReleaseEconomicsPolicyDigest,
  FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
} from "../../src/workflow/release-economics-policy.js";
import {
  computeFraudProofReleaseFinalityPolicyDigest,
  FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
} from "../../src/workflow/release-finality-policy.js";
import { network, type readBlueprint } from "./emulator/blueprints.js";
import { type buildMinimalFaultProofContracts } from "./emulator/contracts.js";

const finalityPolicy = { ...DEPLOYMENT_MANIFEST_L1_FINALITY };
const economicsPolicy = {
  profile: "bounded-acceptance-v1",
  requiredBondLovelace: "900000000",
  slashingPenaltyLovelace: "500000000",
  fraudProverRewardLovelace: "400000000",
  inactivitySlashingPenaltyLovelace: "100000000",
  proverCollateralFloorLovelace: "5000000",
} as const;

/**
 * The installed journey's workflow deployment binding: every staged
 * reference script by contract entry, the release policies under the
 * deployment the challenge names, and the validation-trace family definition
 * for the challenged header.
 */
export const buildInstalledValidationWorkflowBinding = <DeploymentInfo>({
  deploymentFingerprint,
  realBlueprint,
  deploymentInfo,
  referenceScripts,
  stagedReferences,
  contracts,
  headerHash,
  proverCredential,
  resolvedContracts,
}: {
  readonly deploymentFingerprint: string;
  readonly realBlueprint: ReturnType<typeof readBlueprint>;
  readonly deploymentInfo: DeploymentInfo;
  readonly referenceScripts: Readonly<{
    control: Readonly<
      Record<
        "opener" | "source" | "game" | "boundary" | "timeout" | "award",
        UTxO
      >
    >;
    witnesses: Readonly<Record<string, UTxO>>;
    removal: Readonly<Record<string, UTxO>>;
  }>;
  /** The selected resolution's and any optional publications, by entry. */
  readonly stagedReferences: Readonly<Record<string, UTxO>>;
  readonly contracts: Awaited<
    ReturnType<typeof buildMinimalFaultProofContracts>
  >;
  readonly headerHash: string;
  readonly proverCredential: string;
  readonly resolvedContracts: Awaited<
    ReturnType<typeof resolveValidationTraceDisputeDeploymentContracts>
  >;
}) => {
  const referenceScriptsByContract = Object.fromEntries(
    Object.entries({
      validationTraceDispute: referenceScripts.control.opener,
      validationTraceDisputeSource: referenceScripts.control.source,
      validationTraceDisputeGame: referenceScripts.control.game,
      validationTraceDisputeBoundary: referenceScripts.control.boundary,
      validationTraceDisputeTimeout: referenceScripts.control.timeout,
      validationTraceDisputeAward: referenceScripts.control.award,
      ...stagedReferences,
      ...referenceScripts.witnesses,
      ...referenceScripts.removal,
    }).map(([name, utxo]) => [
      name,
      {
        outRef: `${utxo.txHash}#${utxo.outputIndex.toString()}`,
        scriptHash: validatorToScriptHash(utxo.scriptRef!),
      },
    ]),
  );
  return {
    deploymentFingerprint,
    blueprintHash: "bb".repeat(32),
    network,
    blueprint: realBlueprint,
    deploymentInfo,
    releaseFinality: {
      schemaVersion: FRAUD_PROOF_RELEASE_FINALITY_POLICY_SCHEMA_VERSION,
      deploymentIdentityDigest: deploymentFingerprint,
      blueprintHash: "bb".repeat(32),
      policyDigest:
        computeFraudProofReleaseFinalityPolicyDigest(finalityPolicy),
      policy: finalityPolicy,
    },
    releaseEconomics: {
      schemaVersion: FRAUD_PROOF_RELEASE_ECONOMICS_POLICY_SCHEMA_VERSION,
      deploymentIdentityDigest: deploymentFingerprint,
      blueprintHash: "bb".repeat(32),
      policyDigest:
        computeFraudProofReleaseEconomicsPolicyDigest(economicsPolicy),
      policy: economicsPolicy,
    },
    referenceScriptsByContract,
    fieldPreimageCertificate: contracts.fieldPreimageCertificate,
    contractEntries: Object.fromEntries(
      Object.entries(referenceScriptsByContract).map(([name, entry]) => [
        name,
        {
          scriptHash: entry.scriptHash,
          refScriptUTxO: {
            txHash: entry.outRef.split("#")[0],
            outputIndex: Number(entry.outRef.split("#")[1]),
          },
        },
      ]),
    ),
    definition: {
      category: "validationTraceDispute" as const,
      categoryId: "00000006",
      headerHash,
      proverCredential,
      stateQueue: {
        policyId: contracts.stateQueue.policyId,
        address: contracts.stateQueue.spendingScriptAddress,
      },
      computationThread: {
        policyId: resolvedContracts.contracts.computationThread.policyId,
        steps: [
          {
            role: "computation_thread_step_01",
            address:
              resolvedContracts.contracts.validationTraceDispute.opener
                .spendingScriptAddress,
            datumSchema: FraudProofComputationThreadStepDatum,
          },
        ],
      },
      proofToken: {
        policyId: resolvedContracts.contracts.fraudProof.policyId,
        address: resolvedContracts.contracts.fraudProof.spendingScriptAddress,
      },
      operatorDirectory: {
        activePolicyId: contracts.activeOperators.policyId,
        activeAddress: contracts.activeOperators.spendingScriptAddress,
        retiredPolicyId: contracts.retiredOperators.policyId,
        retiredAddress: contracts.retiredOperators.spendingScriptAddress,
      },
      schedulerAddress: contracts.scheduler.spendingScriptAddress,
    },
    resolvedContracts,
  };
};
