import { resolve as resolvePath } from "node:path";

import { type ReferenceScriptAuthPolicy } from "@al-ft/midgard-sdk";

import {
  type DeploymentRunIdentity,
  loadDeploymentRunState,
  RunStateError,
} from "../e2e/run-state.js";
import * as ContractDeploymentInfo from "./contract-deployment-info.js";
import {
  type DeploymentRunCliOptions,
  HUB_ORACLE_NONCE_SIGNED_STEP,
  type PendingHubOracleNonceAttempt,
} from "./deployment-run-state.record-hub-oracle-nonce.js";

export const loadPendingHubOracleNonceAttempt = async ({
  options,
}: {
  readonly options: DeploymentRunCliOptions;
}): Promise<PendingHubOracleNonceAttempt | null> => {
  const state = await loadDeploymentRunState(options.runStatePath);
  const signed = state?.steps[HUB_ORACLE_NONCE_SIGNED_STEP];
  const recorded = state?.steps.hubOracleNonce;
  // A signed record for a transaction `hubOracleNonce` does not name is newer
  // than any recorded submission: that transaction may already be on chain.
  const step =
    signed !== undefined && recorded?.txHashes?.[0] !== signed.txHashes?.[0]
      ? signed
      : recorded;
  if (step === undefined || step.status !== "submitted") {
    return null;
  }
  const signedTxCbor =
    signed?.txHashes?.[0] === step.txHashes?.[0]
      ? signed?.details?.signedTxCbor
      : undefined;
  const txHash = step.txHashes?.[0];
  const address = step.details?.address;
  const lovelace = step.details?.lovelace;
  const inlineDatum = step.details?.inlineDatum;
  if (
    txHash === undefined ||
    address === undefined ||
    lovelace === undefined ||
    inlineDatum === undefined
  ) {
    throw new RunStateError(
      `Pending hub-oracle nonce attempt in ${options.runStatePath} is missing recovery details.`,
    );
  }
  return {
    txHash,
    address,
    lovelace,
    inlineDatum,
    ...(signedTxCbor === undefined ? {} : { signedTxCbor }),
  };
};

export const authPolicyFromRunStateIdentity = (
  identity: DeploymentRunIdentity,
): ReferenceScriptAuthPolicy | null => {
  if (identity.referenceScriptAuthPolicy === undefined) {
    return null;
  }
  return {
    mintingScriptCBOR: identity.referenceScriptAuthPolicy.nativeScript.cborHex,
    mintingScript: {
      type: "Native",
      script: identity.referenceScriptAuthPolicy.nativeScript.cborHex,
    },
    policyId: identity.referenceScriptAuthPolicy.policyId,
    expiresAtSlot:
      identity.referenceScriptAuthPolicy.nativeScript.expiresAtSlot,
    expiresAtUnixTime:
      identity.referenceScriptAuthPolicy.nativeScript.expiresAtUnixTime,
    timelockDurationMs:
      identity.referenceScriptAuthPolicy.nativeScript.timelockDurationMs,
  };
};

const requireComparableIdentity = (
  identity: DeploymentRunIdentity,
  source: string,
): void => {
  const missing = [
    identity.network === undefined ? "network" : null,
    identity.hubOracleOneShot === undefined ? "hubOracleOneShot" : null,
    identity.manifestPath === undefined ? "manifestPath" : null,
  ].filter((entry): entry is string => entry !== null);
  if (missing.length > 0) {
    throw new RunStateError(
      `${source} cannot be reused because deployment identity is incomplete: ${missing.join(
        ", ",
      )}. Pass --fresh-redeploy --fresh-redeploy-reason <reason> only when replacing the identity is intentional.`,
    );
  }
};

export const assertDeploymentIdentityMatches = (
  existing: DeploymentRunIdentity,
  expected: DeploymentRunIdentity,
  source: string,
): void => {
  requireComparableIdentity(existing, source);
  const mismatches: string[] = [];
  if (existing.network !== expected.network) {
    mismatches.push(
      `network existing=${existing.network ?? "missing"} current=${
        expected.network ?? "missing"
      }`,
    );
  }
  if (
    existing.hubOracleOneShot?.txHash !== expected.hubOracleOneShot?.txHash ||
    existing.hubOracleOneShot?.outputIndex !==
      expected.hubOracleOneShot?.outputIndex
  ) {
    mismatches.push(
      `hubOracleOneShot existing=${
        existing.hubOracleOneShot === undefined
          ? "missing"
          : `${existing.hubOracleOneShot.txHash}#${existing.hubOracleOneShot.outputIndex.toString()}`
      } current=${
        expected.hubOracleOneShot === undefined
          ? "missing"
          : `${expected.hubOracleOneShot.txHash}#${expected.hubOracleOneShot.outputIndex.toString()}`
      }`,
    );
  }
  if (
    existing.manifestPath === undefined ||
    expected.manifestPath === undefined ||
    resolvePath(existing.manifestPath) !== resolvePath(expected.manifestPath)
  ) {
    mismatches.push(
      `manifestPath existing=${existing.manifestPath ?? "missing"} current=${
        expected.manifestPath ?? "missing"
      }`,
    );
  }
  if (mismatches.length > 0) {
    throw new RunStateError(
      `${source} deployment identity does not match the current environment: ${mismatches.join(
        "; ",
      )}. Refusing to overwrite run state; pass --fresh-redeploy --fresh-redeploy-reason <reason> only for an intentional replacement.`,
    );
  }
};

export const assertPolicyIdsMatch = ({
  leftPolicyId,
  rightPolicyId,
  source,
}: {
  readonly leftPolicyId?: string;
  readonly rightPolicyId?: string;
  readonly source: string;
}): void => {
  if (
    leftPolicyId !== undefined &&
    rightPolicyId !== undefined &&
    leftPolicyId !== rightPolicyId
  ) {
    throw new RunStateError(
      `${source} reference-script auth policy mismatch: ${leftPolicyId} != ${rightPolicyId}. Pass --fresh-redeploy --fresh-redeploy-reason <reason> only for an intentional replacement.`,
    );
  }
};

export const manifestIdentityToRunIdentity = (
  manifest: ContractDeploymentInfo.DeploymentManifest,
  manifestPathValue: string,
): DeploymentRunIdentity => ({
  network: manifest.network,
  hubOracleOneShot: {
    txHash: manifest.hubOracleOneShot.txHash,
    outputIndex: manifest.hubOracleOneShot.outputIndex,
  },
  manifestPath: manifestPathValue,
  referenceScriptAuthPolicyId: manifest.referenceScriptAuthPolicy.policyId,
});
