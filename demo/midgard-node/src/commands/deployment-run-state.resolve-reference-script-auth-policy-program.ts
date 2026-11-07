import {
  createReferenceScriptAuthPolicy,
  type ReferenceScriptAuthPolicy,
  referenceScriptAuthPolicyDeploymentInfo,
  referenceScriptAuthPolicyFromDeploymentInfo,
} from "@al-ft/midgard-sdk";
import type { LucidEvolution } from "@lucid-evolution/lucid";
import { Effect } from "effect";

import {
  createDeploymentRunState,
  type DeploymentRunState,
  loadDeploymentRunState,
  mutateDeploymentRunState,
  RunStateError,
  transitionDeploymentStep,
} from "../e2e/run-state.js";
import {
  assertDeploymentIdentityMatches,
  assertPolicyIdsMatch,
  authPolicyFromRunStateIdentity,
  manifestIdentityToRunIdentity,
} from "./deployment-run-state.assert-deployment-identity-matches.js";
import {
  assertFreshRedeployReason,
  type DeploymentRunCliOptions,
  identityFromContext,
  manifestPath,
  readExistingDeploymentManifest,
} from "./deployment-run-state.record-hub-oracle-nonce.js";

/** The deployment whose reference-script auth policy is resolved. */
export type ReferenceScriptAuthPolicyDeployment = {
  readonly options: DeploymentRunCliOptions;
  readonly network: string;
  readonly hubOracleOneShotTxHash: string;
  readonly hubOracleOneShotOutputIndex: number;
  readonly manifestOutputPath?: string;
};

/** The auth policy a deployment already recorded, and where. */
export type RecordedReferenceScriptAuthPolicy = {
  readonly policy: ReferenceScriptAuthPolicy;
  readonly source: "run_state" | "manifest";
  /** The manifest is written once the deployment is initialized. */
  readonly inManifest: boolean;
};

/**
 * A new auth policy prepared before it is recorded, and the recorded policy
 * it replaces (`null` when none was recorded). It is recorded only while
 * that policy is still the recorded one.
 */
export type ReferenceScriptAuthPolicyReplacement = {
  readonly policy: ReferenceScriptAuthPolicy;
  readonly replacesPolicyId: string | null;
  /** Recorded as a fresh redeploy's reason is. */
  readonly reason?: string;
};

const deploymentContext = ({
  options,
  network,
  hubOracleOneShotTxHash,
  hubOracleOneShotOutputIndex,
  manifestOutputPath,
}: ReferenceScriptAuthPolicyDeployment) => {
  assertFreshRedeployReason(options);
  const outputPath = manifestPath(manifestOutputPath);
  const currentIdentity = identityFromContext({
    network,
    hubOracleOneShotTxHash,
    hubOracleOneShotOutputIndex,
    manifestOutputPath: outputPath,
  });
  const existingManifest = options.freshRedeploy
    ? null
    : readExistingDeploymentManifest(outputPath);
  if (existingManifest !== null) {
    assertDeploymentIdentityMatches(
      manifestIdentityToRunIdentity(existingManifest, outputPath),
      currentIdentity,
      "deployment manifest",
    );
  }
  return {
    currentIdentity,
    manifestPolicy: existingManifest?.referenceScriptAuthPolicy ?? null,
  };
};

/** The policy `state` and the manifest record for this deployment; none on a fresh redeploy. */
const recordedPolicyIn = (
  state: DeploymentRunState | null,
  options: DeploymentRunCliOptions,
  { currentIdentity, manifestPolicy }: ReturnType<typeof deploymentContext>,
): RecordedReferenceScriptAuthPolicy | null => {
  if (options.freshRedeploy) return null;
  const runStatePolicy =
    state === null ? null : authPolicyFromRunStateIdentity(state.identity);
  if (state !== null && runStatePolicy !== null) {
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
    return {
      policy: runStatePolicy,
      source: "run_state",
      inManifest: manifestPolicy !== null,
    };
  }
  if (manifestPolicy !== null) {
    return {
      policy: referenceScriptAuthPolicyFromDeploymentInfo(manifestPolicy),
      source: "manifest",
      inManifest: true,
    };
  }
  return null;
};

/** Reads the auth policy the deployment recorded, without recording anything. */
export const recordedReferenceScriptAuthPolicyProgram = (
  deployment: ReferenceScriptAuthPolicyDeployment,
): Effect.Effect<RecordedReferenceScriptAuthPolicy | null, RunStateError> =>
  Effect.tryPromise({
    try: async () =>
      recordedPolicyIn(
        await loadDeploymentRunState(deployment.options.runStatePath),
        deployment.options,
        deploymentContext(deployment),
      ),
    catch: (cause) =>
      cause instanceof RunStateError
        ? cause
        : new RunStateError("Failed to read deployment run state.", {
            cause,
          }),
  });

export const resolveReferenceScriptAuthPolicyProgram = ({
  options,
  lucid,
  network,
  hubOracleOneShotTxHash,
  hubOracleOneShotOutputIndex,
  persistRunState = true,
  timelockDurationMs,
  manifestOutputPath,
  replacement,
}: ReferenceScriptAuthPolicyDeployment & {
  readonly lucid: LucidEvolution;
  readonly timelockDurationMs: number;
  /** Diagnostic callers must resolve identity without mutating run state. */
  readonly persistRunState?: boolean;
  /** Records this policy instead of reusing or creating one. */
  readonly replacement?: ReferenceScriptAuthPolicyReplacement;
}): Effect.Effect<ReferenceScriptAuthPolicy, RunStateError> =>
  Effect.tryPromise({
    try: async () => {
      const context = deploymentContext({
        options,
        network,
        hubOracleOneShotTxHash,
        hubOracleOneShotOutputIndex,
        manifestOutputPath,
      });
      let resolvedPolicy: ReferenceScriptAuthPolicy | null = null;
      let policySource: "run_state" | "manifest" | "created" | "fresh_created" =
        "created";
      let freshRedeployReason = options.freshRedeployReason;

      const transitionState = async (
        state: DeploymentRunState,
      ): Promise<DeploymentRunState> => {
        const recorded = recordedPolicyIn(state, options, context);
        if (replacement !== undefined) {
          if (
            (recorded?.policy.policyId ?? null) !== replacement.replacesPolicyId
          ) {
            throw new RunStateError(
              `The recorded reference-script auth policy changed while its replacement was prepared: expected ${replacement.replacesPolicyId ?? "none"}, found ${recorded?.policy.policyId ?? "none"}.`,
            );
          }
          resolvedPolicy = replacement.policy;
          freshRedeployReason = replacement.reason ?? freshRedeployReason;
          policySource =
            freshRedeployReason === undefined ? "created" : "fresh_created";
        } else if (recorded !== null) {
          resolvedPolicy = recorded.policy;
          policySource = recorded.source;
        } else {
          resolvedPolicy = await createReferenceScriptAuthPolicy(
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
            // `mode` is how the run state was created and must stay equal to
            // its creation event; a fresh redeploy is recorded on the step.
            ...state,
            identity: {
              ...state.identity,
              ...context.currentIdentity,
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
                        ? `; fresh_redeploy_reason=${freshRedeployReason}`
                        : ""
                    }`,
          },
        );
      };
      const nextState = persistRunState
        ? await mutateDeploymentRunState(
            options.runStatePath,
            () =>
              createDeploymentRunState({
                mode: options.freshRedeploy ? "fresh" : "resume",
                identity: context.currentIdentity,
              }),
            transitionState,
          )
        : await (async () => {
            const existingState = await loadDeploymentRunState(
              options.runStatePath,
            );
            if (existingState === null) {
              throw new RunStateError(
                "Read-only reference-script capture requires an existing deployment run state.",
              );
            }
            await transitionState(existingState);
            return existingState;
          })();
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
