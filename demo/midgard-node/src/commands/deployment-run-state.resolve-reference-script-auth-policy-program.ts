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

export const resolveReferenceScriptAuthPolicyProgram = ({
  options,
  lucid,
  network,
  hubOracleOneShotTxHash,
  hubOracleOneShotOutputIndex,
  persistRunState = true,
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
  /** Diagnostic callers must resolve identity without mutating run state. */
  readonly persistRunState?: boolean;
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
      const existingManifest = options.freshRedeploy
        ? null
        : readExistingDeploymentManifest(outputPath);
      const manifestPolicy =
        existingManifest?.referenceScriptAuthPolicy ?? null;
      if (existingManifest !== null) {
        assertDeploymentIdentityMatches(
          manifestIdentityToRunIdentity(existingManifest, outputPath),
          currentIdentity,
          "deployment manifest",
        );
      }
      let resolvedPolicy: ReferenceScriptAuthPolicy | null = null;
      let policySource: "run_state" | "manifest" | "created" | "fresh_created" =
        "created";

      const transitionState = async (
        state: DeploymentRunState,
      ): Promise<DeploymentRunState> => {
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
      };
      const nextState = persistRunState
        ? await mutateDeploymentRunState(
            options.runStatePath,
            () =>
              createDeploymentRunState({
                mode: options.freshRedeploy ? "fresh" : "resume",
                identity: currentIdentity,
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
