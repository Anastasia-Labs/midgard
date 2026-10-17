import {
  FraudProofComputationThreadStepDatum,
  WitnessScriptDecodingStep02DatumSchema,
  WitnessScriptDecodingStep03DatumSchema,
  WitnessScriptDecodingStep04DatumSchema,
} from "@al-ft/midgard-sdk";

import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { type CursorFamilyTransactionPort } from "../workflow/cursor-family-adapter.js";
import { defineFamily } from "../workflow/family-definition.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { type FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import {
  assembleBoundManifestBoundFamilyWorkflow,
  bindManifestBoundFamilyWorkflow,
} from "../workflow/manifest-bound-family-assembly.js";
import { executeManifestBoundFamilyRecovery } from "../workflow/manifest-bound-family-recovery.js";
import { type WitnessScriptDecodingStage } from "./witness-script-decoding.js";
import {
  type BoundContext,
  createManifestBoundWitnessScriptDecodingRuntime,
  createWitnessScriptDecodingRecoveryPorts,
  type ManifestBoundWitnessScriptDecodingWorkflow,
  type ManifestBoundWitnessScriptDecodingWorkflowConfig,
  type RunContext,
  runs,
  WITNESS_ROLES,
  witnessScriptDecodingObservationFromL1,
} from "./workflow.create-witness-script-decoding-recovery-ports.js";
import {
  deriveWitnessScriptDecodingAuthenticatedSource,
  deriveWitnessScriptDecodingEvidenceFromCanonicalBlock,
} from "./workflow.derive-witness-script-decoding-authenticated-source.js";
import { createWitnessScriptDecodingRawL1StageResolver } from "./workflow.detect-witness-script-decoding-complete-replay.js";
import {
  WITNESS_SCRIPT_DECODING_MANIFEST_CONTRACTS,
  WITNESS_SCRIPT_DECODING_WORKFLOW,
  witnessScriptDecodingConfigFromBinding,
  type WitnessScriptDecodingJournal,
} from "./workflow.witness-script-decoding-config-from-binding.js";
import { WITNESS_SCRIPT_DECODING_CURSOR_SPEC } from "./workflow-spec.js";

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundWitnessScriptDecodingWorkflow = async (
  input: ManifestBoundWitnessScriptDecodingWorkflowConfig,
): Promise<ManifestBoundWitnessScriptDecodingWorkflow> => {
  const deployment = await bindManifestBoundFamilyWorkflow(
    WITNESS_SCRIPT_DECODING_FAMILY_DEFINITION,
    {
      ...input,
      referenceScripts: {
        steps: [
          input.referenceScripts.step01,
          input.referenceScripts.step02,
          input.referenceScripts.step03,
          input.referenceScripts.step04,
        ],
        witnesses: input.referenceScripts.witnesses,
        fieldPreimageCertificateMint:
          input.referenceScripts.fieldPreimageCertificateMint,
      },
    },
  );
  const config = witnessScriptDecodingConfigFromBinding({
    binding: deployment.binding,
    lucid: deployment.lucid,
    signer: deployment.signer,
    referenceScripts: {
      step01: deployment.references.steps[0],
      step02: deployment.references.steps[1],
      step03: deployment.references.steps[2],
      step04: deployment.references.steps[3],
      witnesses: deployment.references.witnesses,
      fieldPreimageCertificateMint:
        deployment.references.fieldPreimageCertificateMint,
    },
  });
  return Object.freeze({
    deployment,
    workflowVersion: WITNESS_SCRIPT_DECODING_WORKFLOW,
    config,
    binding: deployment.binding,
    l1: deployment.l1,
    stateQueueMutationLeaseCoordinator:
      input.stateQueueMutationLeaseCoordinator,
    decisionDigest: input.decisionDigest,
  });
};

/**
 * Watcher-facing runner. Evidence is always reconstructed from authenticated
 * L1 plus public retained DA; unknown/caller-authored evidence fields fail.
 */
export const runOrResumeManifestBoundWitnessScriptDecodingWorkflow =
  async (input: {
    readonly workflow: ManifestBoundWitnessScriptDecodingWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: WitnessScriptDecodingJournal;
  }): Promise<WitnessScriptDecodingStage> => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow") {
      throw new Error(
        "witnessScriptDecoding runner rejects caller-authored evidence inputs",
      );
    }
    const headerHash = input.workflow.binding.definition.headerHash;
    const observation = await observeFraudProofWorkflowHeader(
      input.workflow.l1,
      { headerHash },
    );
    const canonical = await fetchCanonicalBlockEvidence({
      observation,
      sources: input.sources,
    });
    const evidence =
      deriveWitnessScriptDecodingEvidenceFromCanonicalBlock(canonical);
    const source = await deriveWitnessScriptDecodingAuthenticatedSource({
      block: canonical,
      evidence,
    });
    const runtime = createManifestBoundWitnessScriptDecodingRuntime({
      config: input.workflow.config,
      journal: input.journal,
      observe: async () =>
        witnessScriptDecodingObservationFromL1(
          (await input.workflow.l1.observe({ headerHash })).stage,
        ),
      resolveStage: createWitnessScriptDecodingRawL1StageResolver({
        config: input.workflow.config,
        l1: input.workflow.l1,
        source,
      }),
    });
    return await runtime.runOrResume(evidence);
  };

const runFor = (context: BoundContext) => {
  const existing = runs.get(context);
  if (existing !== undefined) return existing;
  const created = createWitnessScriptDecodingRecoveryPorts(
    context.runtime.workflow,
  );
  runs.set(context, created);
  return created;
};

export const WITNESS_SCRIPT_DECODING_FAMILY_DEFINITION = defineFamily<
  "witnessScriptDecoding",
  (typeof WITNESS_ROLES)[number],
  true,
  4,
  RunContext
>({
  category: "witnessScriptDecoding",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    WitnessScriptDecodingStep02DatumSchema,
    WitnessScriptDecodingStep03DatumSchema,
    WitnessScriptDecodingStep04DatumSchema,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => WITNESS_SCRIPT_DECODING_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: WITNESS_SCRIPT_DECODING_CURSOR_SPEC,
    stepContractNames: [
      WITNESS_SCRIPT_DECODING_MANIFEST_CONTRACTS.step01,
      WITNESS_SCRIPT_DECODING_MANIFEST_CONTRACTS.step02,
      WITNESS_SCRIPT_DECODING_MANIFEST_CONTRACTS.step03,
      WITNESS_SCRIPT_DECODING_MANIFEST_CONTRACTS.step04,
    ],
    transactionPort: (context) => runFor(context).transactions,
  },
  fieldCarriage: [
    {
      requirementForAction: (context, input) =>
        runFor(context).requirementForAction(input),
    },
  ],
});

export const createWitnessScriptDecodingRecoveryAdapter = (
  workflow: ManifestBoundWitnessScriptDecodingWorkflow,
) => {
  const assembled = assembleBoundManifestBoundFamilyWorkflow(
    WITNESS_SCRIPT_DECODING_FAMILY_DEFINITION,
    workflow.deployment,
    { workflow },
  );
  return {
    ...assembled,
    transactions:
      assembled.transactions as CursorFamilyTransactionPort<"witnessScriptDecoding">,
  };
};

export const executeManifestBoundWitnessScriptDecodingWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundWitnessScriptDecodingWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}) =>
  await executeManifestBoundFamilyRecovery({
    ...workflow,
    sources,
    journal,
    ...createWitnessScriptDecodingRecoveryAdapter(workflow),
  });
