import { existsSync } from "node:fs";
import { readFile } from "node:fs/promises";
import { resolve as resolvePath } from "node:path";

import {
  createReferenceScriptAuthPolicy,
  type ReferenceScriptAuthPolicy,
  type ReferenceScriptAuthPolicyDeploymentInfo,
  referenceScriptAuthPolicyDeploymentInfo,
  referenceScriptAuthPolicyFromDeploymentInfo,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import * as ContractDeploymentInfo from "@/commands/contract-deployment-info.js";
import {
  createDeploymentRunState,
  defaultDeploymentRunStatePath,
  type DeploymentRunIdentity,
  type DeploymentRunState,
  loadDeploymentRunState,
  mutateDeploymentRunState,
  RunStateError,
  transitionDeploymentStep,
} from "@/e2e/run-state.js";

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
};

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

const manifestPath = (override?: string): string =>
  override ?? ContractDeploymentInfo.defaultContractDeploymentInfoOutputPath();

type ManifestDeploymentIdentity = {
  readonly network?: string;
  readonly hubOracleOneShot?: {
    readonly txHash: string;
    readonly outputIndex: number;
  };
  readonly referenceScriptAuthPolicy?: ReferenceScriptAuthPolicyDeploymentInfo;
};

const readManifestDeploymentIdentity = async (
  path: string,
): Promise<ManifestDeploymentIdentity | null> => {
  if (!existsSync(path)) {
    return null;
  }
  const parsed = JSON.parse(await readFile(path, "utf8")) as {
    readonly network?: unknown;
    readonly hubOracleOneShot?: {
      readonly txHash?: unknown;
      readonly outputIndex?: unknown;
    };
    readonly referenceScriptAuthPolicy?: ReferenceScriptAuthPolicyDeploymentInfo;
  };
  const hubOracleOneShot =
    typeof parsed.hubOracleOneShot?.txHash === "string" &&
    typeof parsed.hubOracleOneShot.outputIndex === "number" &&
    Number.isInteger(parsed.hubOracleOneShot.outputIndex) &&
    parsed.hubOracleOneShot.outputIndex >= 0
      ? {
          txHash: parsed.hubOracleOneShot.txHash,
          outputIndex: parsed.hubOracleOneShot.outputIndex,
        }
      : undefined;
  return {
    ...(typeof parsed.network === "string" ? { network: parsed.network } : {}),
    ...(hubOracleOneShot === undefined ? {} : { hubOracleOneShot }),
    ...(parsed.referenceScriptAuthPolicy === undefined
      ? {}
      : { referenceScriptAuthPolicy: parsed.referenceScriptAuthPolicy }),
  };
};

const identityFromContext = ({
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

export const loadPendingHubOracleNonceAttempt = async ({
  options,
}: {
  readonly options: DeploymentRunCliOptions;
}): Promise<PendingHubOracleNonceAttempt | null> => {
  const state = await loadDeploymentRunState(options.runStatePath);
  const step = state?.steps.hubOracleNonce;
  if (step === undefined || step.status !== "submitted") {
    return null;
  }
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
  };
};

const authPolicyFromRunStateIdentity = (
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

const assertDeploymentIdentityMatches = (
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

const assertPolicyIdsMatch = ({
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

const manifestIdentityToRunIdentity = (
  manifest: ManifestDeploymentIdentity,
  manifestPathValue: string,
): DeploymentRunIdentity => ({
  ...(manifest.network === undefined ? {} : { network: manifest.network }),
  ...(manifest.hubOracleOneShot === undefined
    ? {}
    : { hubOracleOneShot: manifest.hubOracleOneShot }),
  ...(manifest.referenceScriptAuthPolicy === undefined
    ? {}
    : {
        referenceScriptAuthPolicyId:
          manifest.referenceScriptAuthPolicy.policyId,
      }),
  manifestPath: manifestPathValue,
});

export const resolveReferenceScriptAuthPolicyProgram = ({
  options,
  lucid,
  network,
  hubOracleOneShotTxHash,
  hubOracleOneShotOutputIndex,
  timelockDurationMs,
  manifestOutputPath,
}: {
  readonly options: DeploymentRunCliOptions;
  readonly lucid: LucidEvolution;
  readonly network: string;
  readonly hubOracleOneShotTxHash: string;
  readonly hubOracleOneShotOutputIndex: number;
  readonly timelockDurationMs: number;
  readonly manifestOutputPath?: string;
}): Effect.Effect<ReferenceScriptAuthPolicy, RunStateError> =>
  Effect.tryPromise({
    try: async () => {
      assertFreshRedeployReason(options);
      const outputPath = manifestPath(manifestOutputPath);
      const currentIdentity = identityFromContext({
        network,
        hubOracleOneShotTxHash,
        hubOracleOneShotOutputIndex,
        manifestOutputPath: outputPath,
      });
      const manifestIdentity = await readManifestDeploymentIdentity(outputPath);
      const manifestPolicy =
        manifestIdentity?.referenceScriptAuthPolicy ?? null;
      if (
        manifestIdentity !== null &&
        manifestPolicy !== null &&
        !options.freshRedeploy
      ) {
        assertDeploymentIdentityMatches(
          manifestIdentityToRunIdentity(manifestIdentity, outputPath),
          currentIdentity,
          "deployment manifest",
        );
      }
      let resolvedPolicy: ReferenceScriptAuthPolicy | null = null;
      let policySource: "run_state" | "manifest" | "created" | "fresh_created" =
        "created";

      const nextState = await mutateDeploymentRunState(
        options.runStatePath,
        () =>
          createDeploymentRunState({
            mode: options.freshRedeploy ? "fresh" : "resume",
            identity: currentIdentity,
          }),
        (state) => {
          const runStatePolicy = authPolicyFromRunStateIdentity(state.identity);
          if (runStatePolicy !== null && !options.freshRedeploy) {
            assertDeploymentIdentityMatches(
              state.identity,
              currentIdentity,
              "deployment run state",
            );
            assertPolicyIdsMatch({
              leftPolicyId: state.identity.referenceScriptAuthPolicyId,
              rightPolicyId: runStatePolicy.policyId,
              source: "deployment run state",
            });
            assertPolicyIdsMatch({
              leftPolicyId: manifestPolicy?.policyId,
              rightPolicyId: runStatePolicy.policyId,
              source: "deployment manifest and run state",
            });
            resolvedPolicy = runStatePolicy;
            policySource = "run_state";
          } else if (manifestPolicy !== null && !options.freshRedeploy) {
            resolvedPolicy =
              referenceScriptAuthPolicyFromDeploymentInfo(manifestPolicy);
            policySource = "manifest";
          } else {
            resolvedPolicy = createReferenceScriptAuthPolicy(
              lucid,
              Date.now(),
              timelockDurationMs,
            );
            policySource = options.freshRedeploy ? "fresh_created" : "created";
          }
          const policyInfo =
            referenceScriptAuthPolicyDeploymentInfo(resolvedPolicy);
          return transitionDeploymentStep(
            {
              ...state,
              mode: options.freshRedeploy ? "fresh" : state.mode,
              identity: {
                ...state.identity,
                ...currentIdentity,
                referenceScriptAuthPolicyId: policyInfo.policyId,
                referenceScriptAuthPolicy: {
                  policyId: policyInfo.policyId,
                  nativeScript: policyInfo.nativeScript,
                },
              },
            },
            "referenceScriptAuthPolicy",
            "complete",
            {
              message:
                policySource === "run_state"
                  ? "loaded from run state"
                  : policySource === "manifest"
                    ? "imported from deployment manifest"
                    : `created and persisted before publication${
                        policySource === "fresh_created"
                          ? `; fresh_redeploy_reason=${options.freshRedeployReason}`
                          : ""
                      }`,
            },
          );
        },
      );
      const policy =
        resolvedPolicy ?? authPolicyFromRunStateIdentity(nextState.identity);
      if (policy === null) {
        throw new RunStateError(
          "Failed to resolve reference-script auth policy.",
        );
      }
      return policy;
    },
    catch: (cause) =>
      cause instanceof RunStateError
        ? cause
        : new RunStateError("Failed to resolve deployment run state.", {
            cause,
          }),
  });
