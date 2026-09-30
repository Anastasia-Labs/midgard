import { resolvePublishedProofChunks } from "../publish-proof-chunks.js";
import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  workflowActuationAuthorizingDecisionDigest,
  workflowJournalIsReconciliationOnly,
} from "../workflow/actuation-permit.js";
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import { cursorStringField } from "../workflow/cursor-family-runtime.js";
import { defineFamily } from "../workflow/family-definition.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import {
  type FraudProofWorkflowJournalStore,
  journalJsonDigest,
} from "../workflow/journal.js";
import { assembleManifestBoundFamilyWorkflow } from "../workflow/manifest-bound-family-assembly.js";
import {
  createFraudProofWorkflowRegistry,
  type FraudProofWorkflowRunResult,
  resumeRecordedFraudProofWorkflow,
  runFraudProofWorkflowFromRetainedDa,
} from "../workflow/orchestrator.js";
import { type DistinctAssetAccumulationActuatorAction } from "./actuator.js";
import { prepareDistinctAssetAccumulationArtifact } from "./authenticated-replay.js";
import { DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY } from "./family.js";
import { DISTINCT_ASSET_ACCUMULATION_STEP_DATUM_SCHEMAS } from "./schemas.js";
import {
  admitDistinctAssetWorkflowArtifact,
  type BoundContext,
  boundFor,
  DISTINCT_ASSET_ACCUMULATION_CONFIG_KEYS,
  distinctAssetWorkflowArtifact,
  type ManifestBoundDistinctAssetAccumulationWorkflow,
  type ManifestBoundDistinctAssetAccumulationWorkflowConfig,
  manifestContracts,
  WITNESS_ROLES,
  type WorkflowExtension,
} from "./v1.admit-distinct-asset-workflow-artifact.js";

const createTransactionPort = (
  context: BoundContext,
): CursorFamilyTransactionPort<"distinctAssetAccumulationLimit"> => {
  const actuator = boundFor(context);
  return {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
    validatePreparedArtifact: async ({ evidence, artifact }) => {
      if (
        journalJsonDigest(
          distinctAssetWorkflowArtifact(
            await prepareDistinctAssetAccumulationArtifact(evidence),
          ),
        ) !== journalJsonDigest(artifact)
      )
        throw new Error(
          "prepared family artifact differs from retained evidence",
        );
    },
    prepare: async ({ evidence }) =>
      distinctAssetWorkflowArtifact(
        await prepareDistinctAssetAccumulationArtifact(evidence),
      ),
    capture: async ({ action, artifact: serialized }) => {
      const artifact = admitDistinctAssetWorkflowArtifact(serialized);
      const input = action.input;
      let selected: DistinctAssetAccumulationActuatorAction;
      if (input.stage === "init")
        selected = {
          stage: "init",
          stateQueueBlockOutRef: cursorStringField(
            input,
            "stateQueueBlockOutRef",
          ),
        };
      else if (input.stage === "remove")
        selected = {
          stage: "remove",
          nextRemovalOutRef: cursorStringField(input, "nextRemovalOutRef"),
          fraudProofOutRef: cursorStringField(input, "fraudProofOutRef"),
        };
      else if (input.ordinal === 1)
        selected = {
          stage: "step01",
          threadOutRef: cursorStringField(input, "threadOutRef"),
          stateQueueBlockOutRef: cursorStringField(
            input,
            "stateQueueBlockOutRef",
          ),
        };
      else if (input.ordinal === 2)
        selected = {
          stage: "step02",
          threadOutRef: cursorStringField(input, "threadOutRef"),
        };
      else if (input.ordinal === 6)
        selected = {
          stage: "step06",
          threadOutRef: cursorStringField(input, "threadOutRef"),
        };
      else if (
        input.ordinal === 3 ||
        input.ordinal === 4 ||
        input.ordinal === 5
      )
        selected = {
          stage: "fold",
          threadOutRef: cursorStringField(input, "threadOutRef"),
          stepIndex: (input.ordinal - 1) as 2 | 3 | 4,
        };
      else throw new Error("distinctAssetAccumulationLimit action changed");
      const publishedProofChunks =
        selected.stage === "step01" && artifact.accepted !== undefined
          ? await resolvePublishedProofChunks({
              lucid: context.lucid,
              address: context.signer.address,
              proofCbor: artifact.accepted.txInclusion.txMembershipProofCbor,
            })
          : undefined;
      return await actuator.capture({
        action: selected,
        artifact,
        publishedProofChunks,
      });
    },
  };
};

export const DISTINCT_ASSET_ACCUMULATION_FAMILY_DEFINITION = defineFamily<
  "distinctAssetAccumulationLimit",
  (typeof WITNESS_ROLES)[number],
  false,
  6
>({
  category: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
  stepDatumSchemas: DISTINCT_ASSET_ACCUMULATION_STEP_DATUM_SCHEMAS,
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: false,
  auxiliaryReferenceScripts: manifestContracts.removal,
  replayer: () => DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: {
      category: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
      stepCount: 6,
      successors: {
        1: [2],
        2: [3],
        3: [4],
        4: [5],
        5: [6],
        6: ["proof_token"],
      },
    },
    stepContractNames: manifestContracts.steps,
    transactionPort: createTransactionPort,
  },
  proofChunk: (_context, { action, artifact }) =>
    action.input.stage === "step_01"
      ? (admitDistinctAssetWorkflowArtifact(artifact).accepted?.txInclusion
          .txMembershipProofCbor ?? null)
      : null,
  extend: (context): WorkflowExtension => ({
    actuator: boundFor(context),
    lucid: context.lucid,
    signer: context.signer,
  }),
});

/** Manifest/reference/signer-bound workflow construction with no evidence input. */
export const createManifestBoundDistinctAssetAccumulationWorkflow = async (
  config: ManifestBoundDistinctAssetAccumulationWorkflowConfig,
): Promise<ManifestBoundDistinctAssetAccumulationWorkflow> => {
  if (
    Object.keys(config).sort().join("\0") !==
    [...DISTINCT_ASSET_ACCUMULATION_CONFIG_KEYS].sort().join("\0")
  )
    throw new Error(
      "distinctAssetAccumulationLimit production config contains callback authority",
    );
  if (!/^[0-9a-f]{64}$/u.test(config.decisionDigest))
    throw new Error(
      "distinctAssetAccumulationLimit decision digest is malformed",
    );
  const { decisionDigest, ...assemblyConfig } = config;
  const workflow = await assembleManifestBoundFamilyWorkflow(
    DISTINCT_ASSET_ACCUMULATION_FAMILY_DEFINITION,
    {
      ...assemblyConfig,
      auxiliaryReferenceScripts: config.referenceScripts.removal,
    },
  );
  return Object.freeze({
    ...(workflow as typeof workflow & WorkflowExtension),
    decisionDigest,
  });
};

export const executeManifestBoundDistinctAssetAccumulationWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  workflow: ManifestBoundDistinctAssetAccumulationWorkflow;
  sources: readonly RetainedDaPayloadSource[];
  journal: FraudProofWorkflowJournalStore;
}): Promise<FraudProofWorkflowRunResult> => {
  if (
    workflowActuationAuthorizingDecisionDigest(journal) !==
    workflow.decisionDigest
  )
    throw new Error(
      "distinctAssetAccumulationLimit journal changed decision digest",
    );
  if (workflowJournalIsReconciliationOnly(journal))
    return await resumeRecordedFraudProofWorkflow({
      deploymentFingerprint: workflow.binding.deploymentFingerprint,
      category: DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY,
      headerHash: workflow.binding.definition.headerHash,
      journal,
      adapter: workflow.adapter,
      terminalVerifier: workflow.terminalVerifier,
      releaseFinalityAuthority: workflow.releaseFinalityAuthority,
    });
  const observation = await observeFraudProofWorkflowHeader(workflow.l1, {
    headerHash: workflow.binding.definition.headerHash,
  });
  return await runFraudProofWorkflowFromRetainedDa({
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    observation,
    sources,
    replayer: DISTINCT_ASSET_ACCUMULATION_LIMIT_COMPLETE_CANONICAL_REPLAY,
    registry: createFraudProofWorkflowRegistry({
      adapters: [workflow.adapter],
      launchScope: [DISTINCT_ASSET_ACCUMULATION_LIMIT_CATEGORY],
    }),
    journal,
    terminalVerifier: workflow.terminalVerifier,
    releaseFinalityAuthority: workflow.releaseFinalityAuthority,
  });
};
