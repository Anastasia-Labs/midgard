import { validatorToAddress } from "@lucid-evolution/lucid";

import {
  authenticatedValidatorFromDeployment,
  type DaAttestationValidatorSet,
  deploymentContract,
  type LucidNetwork,
  type MidgardAuthenticatedDeployment,
  type MidgardDeploymentContract,
  type MidgardNodeDeployment,
  normalizeLucidNetwork,
  withdrawalValidatorFromDeployment,
} from "./deployment.deployment-contract.js";

export const daAttestationValidatorsFromDeployment = (
  deployment: MidgardNodeDeployment,
): DaAttestationValidatorSet => ({
  hubOracle: authenticatedValidatorFromDeployment(deployment.hubOracle),
  availabilityChallenge: {
    ...authenticatedValidatorFromDeployment(deployment.availabilityChallenge),
    yields: {
      open: withdrawalValidatorFromDeployment(
        deployment.availabilityChallengeYields.open,
      ),
      settle: withdrawalValidatorFromDeployment(
        deployment.availabilityChallengeYields.settle,
      ),
      close: withdrawalValidatorFromDeployment(
        deployment.availabilityChallengeYields.close,
      ),
      timeout: withdrawalValidatorFromDeployment(
        deployment.availabilityChallengeYields.timeout,
      ),
    },
  },
  daAttestation: authenticatedValidatorFromDeployment(deployment.daAttestation),
  daBondPool: authenticatedValidatorFromDeployment(deployment.daBondPool),
  daParamsGovernor: authenticatedValidatorFromDeployment(
    deployment.daParamsGovernor,
  ),
  stateQueue: {
    ...authenticatedValidatorFromDeployment(deployment.stateQueue),
    yields: {
      commit: withdrawalValidatorFromDeployment(
        deployment.stateQueueYields.commit,
      ),
      unattestedTimeout: withdrawalValidatorFromDeployment(
        deployment.stateQueueYields.unattestedTimeout,
      ),
      unavailableTimeout: withdrawalValidatorFromDeployment(
        deployment.stateQueueYields.unavailableTimeout,
      ),
      fraudRemoval: withdrawalValidatorFromDeployment(
        deployment.stateQueueYields.fraudRemoval,
      ),
      merge: withdrawalValidatorFromDeployment(
        deployment.stateQueueYields.merge,
      ),
    },
  },
});

export const parseMidgardNodeDeploymentInfo = (
  deploymentInfo: Record<string, unknown>,
  network: string,
): MidgardNodeDeployment => {
  const lucidNetwork = normalizeLucidNetwork(network);
  const hubOracleMint = deploymentContract(
    deploymentInfo,
    "hubOracleMint",
    "hub oracle mint",
    "mint",
  );
  const correctionLockSpend = deploymentContract(
    deploymentInfo,
    "correctionLockSpend",
    "correction lock spend",
    "spend",
  );
  return {
    referenceScriptAuthPolicyId: deploymentContract(
      deploymentInfo,
      "referenceScriptAuthMint",
      "reference script authentication mint",
      "mint",
    ).scriptHash,
    hubOraclePolicyId: hubOracleMint.scriptHash,
    correctionLockAddress: validatorToAddress(
      lucidNetwork,
      correctionLockSpend.script as never,
    ),
    // The hub oracle is a mint-only script: no `hubOracleSpend` entry exists in
    // any deployment document, and its beacon UTxO sits at the address whose
    // payment credential is the minting policy hash itself.
    hubOracle: {
      mint: hubOracleMint,
      spend: hubOracleMint,
      policyId: hubOracleMint.scriptHash,
      spendingScriptHash: hubOracleMint.scriptHash,
      spendingScriptAddress: validatorToAddress(
        lucidNetwork,
        hubOracleMint.script as never,
      ),
    },
    availabilityChallenge: authenticatedDeployment(
      deploymentInfo,
      "availabilityChallenge",
      "availability challenge",
      lucidNetwork,
    ),
    fraudProof: authenticatedDeployment(
      deploymentInfo,
      "fraudProof",
      "fraud proof",
      lucidNetwork,
    ),
    daAttestation: authenticatedDeployment(
      deploymentInfo,
      "daAttestation",
      "DA attestation",
      lucidNetwork,
    ),
    daBondPool: authenticatedDeployment(
      deploymentInfo,
      "daBondPool",
      "DA bond pool",
      lucidNetwork,
    ),
    daParamsGovernor: authenticatedDeployment(
      deploymentInfo,
      "daParamsGovernor",
      "DA params governor",
      lucidNetwork,
    ),
    stateQueue: authenticatedDeployment(
      deploymentInfo,
      "stateQueue",
      "state queue",
      lucidNetwork,
    ),
    availabilityChallengeYields: {
      open: withdrawDeploymentContract(
        deploymentInfo,
        "availabilityChallengeOpenWithdraw",
        "availability challenge open withdraw",
      ),
      settle: withdrawDeploymentContract(
        deploymentInfo,
        "availabilityChallengeSettleWithdraw",
        "availability challenge settle withdraw",
      ),
      close: withdrawDeploymentContract(
        deploymentInfo,
        "availabilityChallengeCloseWithdraw",
        "availability challenge close withdraw",
      ),
      timeout: withdrawDeploymentContract(
        deploymentInfo,
        "availabilityChallengeTimeoutWithdraw",
        "availability challenge timeout withdraw",
      ),
    },
    stateQueueYields: {
      commit: withdrawDeploymentContract(
        deploymentInfo,
        "stateQueueCommitWithdraw",
        "state queue commit withdraw",
      ),
      unattestedTimeout: withdrawDeploymentContract(
        deploymentInfo,
        "stateQueueUnattestedTimeoutWithdraw",
        "state queue unattested timeout withdraw",
      ),
      unavailableTimeout: withdrawDeploymentContract(
        deploymentInfo,
        "stateQueueUnavailableTimeoutWithdraw",
        "state queue unavailable timeout withdraw",
      ),
      fraudRemoval: withdrawDeploymentContract(
        deploymentInfo,
        "stateQueueFraudRemovalWithdraw",
        "state queue fraud removal withdraw",
      ),
      merge: withdrawDeploymentContract(
        deploymentInfo,
        "stateQueueMergeWithdraw",
        "state queue merge withdraw",
      ),
    },
  };
};

const authenticatedDeployment = (
  deploymentInfo: Record<string, unknown>,
  prefix:
    | "hubOracle"
    | "availabilityChallenge"
    | "daAttestation"
    | "daBondPool"
    | "daParamsGovernor"
    | "stateQueue"
    | "fraudProof",
  label: string,
  network: LucidNetwork,
): MidgardAuthenticatedDeployment => {
  const mint = deploymentContract(
    deploymentInfo,
    `${prefix}Mint`,
    `${label} mint`,
    "mint",
  );
  const spend = deploymentContract(
    deploymentInfo,
    `${prefix}Spend`,
    `${label} spend`,
    "spend",
  );
  return {
    mint,
    spend,
    policyId: mint.scriptHash,
    spendingScriptHash: spend.scriptHash,
    spendingScriptAddress: validatorToAddress(network, spend.script as never),
  };
};

const withdrawDeploymentContract = (
  deploymentInfo: Record<string, unknown>,
  key: string,
  label: string,
): MidgardDeploymentContract =>
  deploymentContract(deploymentInfo, key, label, "withdraw");
