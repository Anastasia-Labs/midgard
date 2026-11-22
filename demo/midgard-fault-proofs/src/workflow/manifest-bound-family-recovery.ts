import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk/core";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { workflowActuationAuthorizingDecisionDigest } from "./actuation-permit.assert-workflow-actuation-permit-identity.js";
import {
  encodeWorkflowArtifact,
  requireWorkflowArtifactMatches,
} from "./artifact-codec.js";
import type {
  CompleteCanonicalReplay,
  CompleteCanonicalReplayContext,
} from "./complete-replay.js";
import type { FraudProofWorkflowDeploymentBinding } from "./deployment-manifest-binding.js";
import {
  type FraudProofFamilyL1ObservationPort,
  observeFraudProofWorkflowHeader,
} from "./family-l1-observation.js";
import { workflowJsonDigest, type WorkflowJsonObject } from "./identity.js";
import type {
  FraudProofFamilyWorkflowAdapter,
  FraudProofWorkflowTerminalVerifier,
} from "./orchestrator.fraud-proof-family-workflow-adapter.js";
import {
  createFraudProofWorkflowRegistry,
  runFraudProofWorkflowFromRetainedDa,
} from "./orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy.js";
import type { WorkflowRun } from "./workflow-run.js";

/** Cache only fresh canonical material; a saved JSON artifact is never cast back into evidence. */
export const createCanonicalFamilyArtifactPort = <Material>(
  derive: (
    input: Parameters<FraudProofFamilyWorkflowAdapter["prepare"]>[0],
  ) => Promise<Material>,
  durableMaterial: (material: Material) => unknown = (material) => material,
) => {
  let current: { digest: string; material: Material } | undefined;
  const admit = (artifact: WorkflowJsonObject, material: Material) => {
    current = { digest: workflowJsonDigest(artifact), material };
    return artifact;
  };
  return {
    prepare: async (
      input: Parameters<FraudProofFamilyWorkflowAdapter["prepare"]>[0],
    ) => {
      current = undefined;
      const material = await derive(input);
      return admit(encodeWorkflowArtifact(durableMaterial(material)), material);
    },
    validatePreparedArtifact: async (
      input: Parameters<FraudProofFamilyWorkflowAdapter["prepare"]>[0] & {
        artifact: WorkflowJsonObject;
      },
    ) => {
      current = undefined;
      const material = await derive(input);
      requireWorkflowArtifactMatches(input.artifact, durableMaterial(material));
      admit(input.artifact, material);
    },
    require: (artifact: WorkflowJsonObject): Material => {
      if (
        current === undefined ||
        current.digest !== workflowJsonDigest(artifact)
      )
        throw new Error(
          "Family proof material was not admitted from this canonical replay",
        );
      return current.material;
    },
  };
};

/**
 * A constructed family workflow: the installation it was bound from, with its
 * recovery surface assembled over it once: the adapter, terminal verifier
 * and release finality authority every run reads.
 */
export type ManifestBoundFamilyWorkflowWithRecovery<
  Installation extends object,
  Surface extends Readonly<{
    adapter: FraudProofFamilyWorkflowAdapter;
    terminalVerifier: FraudProofWorkflowTerminalVerifier;
    releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  }>,
> = Readonly<Omit<Installation, keyof Surface> & Surface>;

export const withManifestBoundFamilyRecovery = <
  Installation extends object,
  Surface extends Readonly<{
    adapter: FraudProofFamilyWorkflowAdapter;
    terminalVerifier: FraudProofWorkflowTerminalVerifier;
    releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  }>,
>(
  installation: Installation,
  assemble: (installation: Installation) => Surface,
): ManifestBoundFamilyWorkflowWithRecovery<Installation, Surface> =>
  Object.freeze({ ...installation, ...assemble(installation) });

/**
 * Fresh public-DA replay through the family adapter. Nothing is stored, so a
 * resumed workflow replays here too.
 */
export const executeManifestBoundFamilyRecovery = async <
  Category extends FraudProofCatalogueCategoryName,
>({
  binding,
  l1,
  adapter,
  decisionDigest,
  replayer,
  replayContext,
  resolveReplayContext,
  terminalVerifier,
  releaseFinalityAuthority,
  sources,
  run,
}: {
  binding: FraudProofWorkflowDeploymentBinding<Category>;
  l1: FraudProofFamilyL1ObservationPort<Category>;
  adapter: FraudProofFamilyWorkflowAdapter;
  decisionDigest: string;
  replayer: CompleteCanonicalReplay;
  replayContext?: CompleteCanonicalReplayContext;
  resolveReplayContext?: (
    evidence: CanonicalBlockEvidence,
  ) => Promise<CompleteCanonicalReplayContext>;
  terminalVerifier: FraudProofWorkflowTerminalVerifier;
  releaseFinalityAuthority: FraudProofReleaseFinalityAuthority;
  sources: readonly RetainedDaPayloadSource[];
  run: WorkflowRun;
}) => {
  const { category, headerHash } = binding.definition;
  if (workflowActuationAuthorizingDecisionDigest(run) !== decisionDigest)
    throw new Error(`${category} run actuation permit changed decision digest`);
  if (adapter.category !== category || l1.category !== category)
    throw new Error(`${category} recovery adapter changed deployment category`);
  return await runFraudProofWorkflowFromRetainedDa({
    hubOraclePolicyId: binding.hubOraclePolicyId,
    observation: await observeFraudProofWorkflowHeader(l1, { headerHash }),
    sources,
    replayer,
    replayContext,
    resolveReplayContext,
    registry: createFraudProofWorkflowRegistry({
      adapters: [adapter],
      launchScope: [category],
    }),
    run,
    terminalVerifier,
    releaseFinalityAuthority,
  });
};
