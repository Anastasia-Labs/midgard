import type { FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  workflowActuationDecisionDigest,
  workflowJournalIsReconciliationOnly,
} from "./actuation-permit.js";
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
import {
  type FraudProofWorkflowJournalStore,
  journalJsonDigest,
  type JournalJsonObject,
} from "./journal.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofFamilyWorkflowAdapter,
  type FraudProofWorkflowTerminalVerifier,
  resumeRecordedFraudProofWorkflow,
  runFraudProofWorkflowFromRetainedDa,
} from "./orchestrator.js";
import type { FraudProofReleaseFinalityAuthority } from "./release-finality-policy.js";

/** Cache only fresh canonical material; a saved JSON artifact is never cast back into evidence. */
export const createCanonicalFamilyArtifactPort = <Material>(
  derive: (
    input: Parameters<FraudProofFamilyWorkflowAdapter["prepare"]>[0],
  ) => Promise<Material>,
  durableMaterial: (material: Material) => unknown = (material) => material,
) => {
  let current: { digest: string; material: Material } | undefined;
  const admit = (artifact: JournalJsonObject, material: Material) => {
    current = { digest: journalJsonDigest(artifact), material };
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
        artifact: JournalJsonObject;
      },
    ) => {
      current = undefined;
      const material = await derive(input);
      requireWorkflowArtifactMatches(input.artifact, durableMaterial(material));
      admit(input.artifact, material);
    },
    require: (artifact: JournalJsonObject): Material => {
      if (
        current === undefined ||
        current.digest !== journalJsonDigest(artifact)
      )
        throw new Error(
          "Family proof material was not admitted from this canonical replay",
        );
      return current.material;
    },
  };
};

/** Same adapter and lifecycle for fresh public-DA replay and exact saved-intent recovery. */
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
  journal,
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
  journal: FraudProofWorkflowJournalStore;
}) => {
  const { category, headerHash } = binding.definition;
  if (workflowActuationDecisionDigest(journal) !== decisionDigest)
    throw new Error(
      `${category} journal actuation permit changed decision digest`,
    );
  if (adapter.category !== category || l1.category !== category)
    throw new Error(`${category} recovery adapter changed deployment category`);
  if (workflowJournalIsReconciliationOnly(journal))
    return await resumeRecordedFraudProofWorkflow({
      deploymentFingerprint: binding.deploymentFingerprint,
      category,
      headerHash,
      journal,
      adapter,
      terminalVerifier,
      releaseFinalityAuthority,
    });
  return await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: binding.deploymentFingerprint,
    observation: await observeFraudProofWorkflowHeader(l1, { headerHash }),
    sources,
    replayer,
    replayContext,
    resolveReplayContext,
    registry: createFraudProofWorkflowRegistry({
      adapters: [adapter],
      launchScope: [category],
    }),
    journal,
    terminalVerifier,
    releaseFinalityAuthority,
  });
};
