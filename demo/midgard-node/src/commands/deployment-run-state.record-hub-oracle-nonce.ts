import { existsSync } from "node:fs";

import {
  createDeploymentRunState,
  defaultDeploymentRunStatePath,
  type DeploymentRunIdentity,
  type DeploymentRunState,
  loadDeploymentRunState,
  mutateDeploymentRunState,
  RunStateError,
  transitionDeploymentStep,
} from "../e2e/run-state.js";
import * as ContractDeploymentInfo from "./contract-deployment-info.js";

export type DeploymentRunCliOptions = {
  readonly runStatePath: string;
  readonly freshRedeploy: boolean;
  readonly freshRedeployReason?: string;
};

export type DeploymentRunCliOptionInput = {
  readonly runState?: unknown;
  readonly freshRedeploy?: unknown;
  readonly freshRedeployReason?: unknown;
};

export type PendingHubOracleNonceAttempt = {
  readonly txHash: string;
  readonly address: string;
  readonly lovelace: string;
  readonly inlineDatum: string;
  readonly signedTxCbor?: string;
};

/** Run-state step holding the nonce transaction signed before submission. */
export const HUB_ORACLE_NONCE_SIGNED_STEP = "hubOracleNonceSigned";

export const resolveDeploymentRunCliOptions = (
  input: DeploymentRunCliOptionInput,
): DeploymentRunCliOptions => ({
  runStatePath:
    typeof input.runState === "string" && input.runState.trim().length > 0
      ? input.runState
      : defaultDeploymentRunStatePath(),
  freshRedeploy: input.freshRedeploy === true,
  ...(typeof input.freshRedeployReason === "string" &&
  input.freshRedeployReason.trim().length > 0
    ? { freshRedeployReason: input.freshRedeployReason.trim() }
    : {}),
});

export const assertFreshRedeployReason = (
  options: DeploymentRunCliOptions,
): void => {
  if (
    options.freshRedeploy &&
    (options.freshRedeployReason === undefined ||
      options.freshRedeployReason.trim().length === 0)
  ) {
    throw new RunStateError(
      "--fresh-redeploy-reason is required with --fresh-redeploy.",
    );
  }
};

export const manifestPath = (override?: string): string =>
  override ?? ContractDeploymentInfo.defaultContractDeploymentInfoOutputPath();

export const readExistingDeploymentManifest = (
  path: string,
): ContractDeploymentInfo.DeploymentManifest | null => {
  if (!existsSync(path)) {
    return null;
  }
  try {
    return ContractDeploymentInfo.readDeploymentManifestFile(path);
  } catch (cause) {
    throw new RunStateError(
      `Deployment manifest at "${path}" cannot be reused because it is invalid: ${
        cause instanceof Error ? cause.message : String(cause)
      }. Pass --fresh-redeploy --fresh-redeploy-reason <reason> only when replacing the existing identity is intentional.`,
      { cause },
    );
  }
};

export const identityFromContext = ({
  network,
  hubOracleOneShotTxHash,
  hubOracleOneShotOutputIndex,
  manifestOutputPath,
}: {
  readonly network: string;
  readonly hubOracleOneShotTxHash?: string;
  readonly hubOracleOneShotOutputIndex?: number;
  readonly manifestOutputPath?: string;
}): DeploymentRunIdentity => ({
  network,
  ...(hubOracleOneShotTxHash === undefined ||
  hubOracleOneShotOutputIndex === undefined
    ? {}
    : {
        hubOracleOneShot: {
          txHash: hubOracleOneShotTxHash,
          outputIndex: hubOracleOneShotOutputIndex,
        },
      }),
  manifestPath: manifestPath(manifestOutputPath),
});

export const guardHubOracleNonceCreation = async ({
  options,
  manifestOutputPath,
}: {
  readonly options: DeploymentRunCliOptions;
  readonly manifestOutputPath?: string;
}): Promise<void> => {
  assertFreshRedeployReason(options);
  if (options.freshRedeploy) {
    return;
  }
  const existingState = await loadDeploymentRunState(options.runStatePath);
  const existingManifest = existsSync(manifestPath(manifestOutputPath));
  if (existingState !== null || existingManifest) {
    throw new RunStateError(
      [
        "Refusing to create a fresh hub-oracle one-shot nonce because deployment identity already exists.",
        `run_state=${existingState === null ? "absent" : options.runStatePath}`,
        `manifest=${existingManifest ? manifestPath(manifestOutputPath) : "absent"}`,
        "Pass --fresh-redeploy --fresh-redeploy-reason <reason> only when replacing the existing identity is intentional.",
      ].join(" "),
    );
  }
};

export const recordHubOracleNonce = async ({
  options,
  network,
  txHash,
  outputIndex,
  outRef,
}: {
  readonly options: DeploymentRunCliOptions;
  readonly network: string;
  readonly txHash: string;
  readonly outputIndex: number;
  readonly outRef: string;
}): Promise<DeploymentRunState> =>
  mutateDeploymentRunState(
    options.runStatePath,
    () =>
      createDeploymentRunState({
        mode: options.freshRedeploy ? "fresh" : "resume",
        identity: {
          network,
        },
      }),
    (state) =>
      transitionDeploymentStep(
        {
          ...state,
          mode: options.freshRedeploy ? "fresh" : state.mode,
          identity: {
            ...state.identity,
            network,
            hubOracleOneShot: {
              txHash,
              outputIndex,
            },
          },
        },
        "hubOracleNonce",
        "complete",
        {
          outRefs: [outRef],
          txHashes: [txHash],
          details: {
            ...(state.steps.hubOracleNonce?.details ?? {}),
            outputStatus: "visible",
          },
          message: options.freshRedeploy
            ? `fresh_redeploy_reason=${options.freshRedeployReason}`
            : "prepared nonce",
        },
      ),
  );

export const recordHubOracleNonceSubmitted = async ({
  options,
  network,
  txHash,
  address,
  lovelace,
  inlineDatum,
}: {
  readonly options: DeploymentRunCliOptions;
  readonly network: string;
  readonly txHash: string;
  readonly address: string;
  readonly lovelace: string;
  readonly inlineDatum: string;
}): Promise<DeploymentRunState> =>
  mutateDeploymentRunState(
    options.runStatePath,
    () =>
      createDeploymentRunState({
        mode: options.freshRedeploy ? "fresh" : "resume",
        identity: {
          network,
        },
      }),
    (state) =>
      transitionDeploymentStep(
        {
          ...state,
          mode: options.freshRedeploy ? "fresh" : state.mode,
          identity: {
            ...state.identity,
            network,
          },
        },
        "hubOracleNonce",
        "submitted",
        {
          txHashes: [txHash],
          message: "submitted_confirmation_unknown",
          details: {
            address,
            lovelace,
            inlineDatum,
            confirmationStatus: "submitted_confirmation_unknown",
            outputStatus: "unknown",
          },
        },
      ),
  );

export const recordHubOracleNonceTxHashConfirmed = async ({
  options,
  network,
  txHash,
  address,
  lovelace,
  inlineDatum,
  confirmationStatus,
}: {
  readonly options: DeploymentRunCliOptions;
  readonly network: string;
  readonly txHash: string;
  readonly address: string;
  readonly lovelace: string;
  readonly inlineDatum: string;
  readonly confirmationStatus: string;
}): Promise<DeploymentRunState> =>
  mutateDeploymentRunState(
    options.runStatePath,
    () =>
      createDeploymentRunState({
        mode: options.freshRedeploy ? "fresh" : "resume",
        identity: {
          network,
        },
      }),
    (state) =>
      transitionDeploymentStep(
        {
          ...state,
          mode: options.freshRedeploy ? "fresh" : state.mode,
          identity: {
            ...state.identity,
            network,
          },
        },
        "hubOracleNonce",
        "submitted",
        {
          txHashes: [txHash],
          message: "confirmed_output_pending",
          details: {
            address,
            lovelace,
            inlineDatum,
            confirmationStatus,
            outputStatus: "pending",
          },
        },
      ),
  );

/**
 * Write-ahead record of the signed nonce transaction, persisted before any
 * submission. It is its own step so readers of `hubOracleNonce` are unchanged;
 * a later run resumes it by resubmitting exactly these bytes.
 */
export const recordHubOracleNonceSigned = async ({
  options,
  network,
  txHash,
  signedTxCbor,
  address,
  lovelace,
  inlineDatum,
}: {
  readonly options: DeploymentRunCliOptions;
  readonly network: string;
  readonly txHash: string;
  readonly signedTxCbor: string;
  readonly address: string;
  readonly lovelace: string;
  readonly inlineDatum: string;
}): Promise<DeploymentRunState> =>
  mutateDeploymentRunState(
    options.runStatePath,
    () =>
      createDeploymentRunState({
        mode: options.freshRedeploy ? "fresh" : "resume",
        identity: { network },
      }),
    (state) =>
      transitionDeploymentStep(
        {
          ...state,
          mode: options.freshRedeploy ? "fresh" : state.mode,
          identity: { ...state.identity, network },
        },
        HUB_ORACLE_NONCE_SIGNED_STEP,
        "submitted",
        {
          txHashes: [txHash],
          message: "signed_before_submission",
          details: { address, lovelace, inlineDatum, signedTxCbor },
        },
      ),
  );
