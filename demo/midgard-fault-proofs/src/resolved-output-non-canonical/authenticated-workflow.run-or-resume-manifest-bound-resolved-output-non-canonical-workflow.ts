import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";

import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { type CursorFamilyTransactionPort } from "../workflow/cursor-family-adapter.js";
import {
  defineFamily,
  type FamilyAssemblyContext,
} from "../workflow/family-definition.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { resolveHistoricalNativeScriptCorpus } from "../workflow/historical-native-script-corpus.js";
import { type FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import {
  assembleBoundManifestBoundFamilyWorkflow,
  bindManifestBoundFamilyWorkflow,
} from "../workflow/manifest-bound-family-assembly.js";
import { executeManifestBoundFamilyRecovery } from "../workflow/manifest-bound-family-recovery.js";
import {
  createManifestBoundResolvedOutputNonCanonicalRuntime,
  createResolvedOutputNonCanonicalRecoveryPorts,
  type ManifestBoundResolvedOutputNonCanonicalWorkflow,
  type ManifestBoundResolvedOutputNonCanonicalWorkflowConfig,
  resolvedOutputStageFromL1,
  type RunContext,
  WITNESS_ROLES,
} from "./authenticated-workflow.create-resolved-output-non-canonical-recovery-ports.js";
import {
  createResolvedOutputNonCanonicalRawL1StageResolver,
  deriveResolvedOutputNonCanonicalAuthenticatedSource,
} from "./authenticated-workflow.derive-resolved-output-non-canonical-authenticated-source.js";
import {
  RESOLVED_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS,
  RESOLVED_OUTPUT_NON_CANONICAL_WORKFLOW,
  resolvedOutputNonCanonicalConfigFromBinding,
} from "./authenticated-workflow.resolved-output-non-canonical-config-from-binding.js";
import {
  deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus,
  detectResolvedOutputNonCanonicalCompleteReplay,
} from "./resolved-output-non-canonical.js";
import {
  ResolvedOutputStep02DatumSchema,
  ResolvedOutputStep03DatumSchema,
  ResolvedOutputStep04DatumSchema,
  ResolvedOutputStep05DatumSchema,
} from "./schemas.js";
import {
  type ResolvedOutputJournal,
  type ResolvedOutputStage,
} from "./workflow.js";
import { RESOLVED_OUTPUT_NON_CANONICAL_CURSOR_SPEC } from "./workflow-spec.js";

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundResolvedOutputNonCanonicalWorkflow = async (
  input: ManifestBoundResolvedOutputNonCanonicalWorkflowConfig,
): Promise<ManifestBoundResolvedOutputNonCanonicalWorkflow> => {
  const deployment = await bindManifestBoundFamilyWorkflow(
    RESOLVED_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION,
    {
      ...input,
      referenceScripts: {
        steps: [
          input.referenceScripts.step01,
          input.referenceScripts.step02,
          input.referenceScripts.step03,
          input.referenceScripts.step04,
          input.referenceScripts.step05,
        ],
        witnesses: input.referenceScripts.witnesses,
        fieldPreimageCertificateMint:
          input.referenceScripts.fieldPreimageCertificateMint,
      },
    },
  );
  const config = resolvedOutputNonCanonicalConfigFromBinding({
    binding: deployment.binding,
    lucid: deployment.lucid,
    signer: deployment.signer,
    referenceScripts: {
      step01: deployment.references.steps[0],
      step02: deployment.references.steps[1],
      step03: deployment.references.steps[2],
      step04: deployment.references.steps[3],
      step05: deployment.references.steps[4],
      witnesses: deployment.references.witnesses,
      fieldPreimageCertificateMint:
        deployment.references.fieldPreimageCertificateMint,
    },
  });
  return Object.freeze({
    deployment,
    workflowVersion: RESOLVED_OUTPUT_NON_CANONICAL_WORKFLOW,
    config,
    binding: deployment.binding,
    l1: deployment.l1,
    stateQueueMutationLeaseCoordinator:
      input.stateQueueMutationLeaseCoordinator,
    decisionDigest: input.decisionDigest,
    historicalNativeScriptCheckpointStore:
      input.historicalNativeScriptCheckpointStore,
    historicalNativeScriptHistorySource:
      input.historicalNativeScriptHistorySource,
  });
};

/**
 * Watcher-facing runner. Evidence is always reconstructed from authenticated
 * L1 plus public retained DA; unknown/caller-authored evidence fields fail.
 */
export const runOrResumeManifestBoundResolvedOutputNonCanonicalWorkflow =
  async (input: {
    readonly workflow: ManifestBoundResolvedOutputNonCanonicalWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: ResolvedOutputJournal;
  }): Promise<ResolvedOutputStage> => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow") {
      throw new Error(
        "resolvedOutputNonCanonical runner rejects caller-authored evidence inputs",
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
    const corpus = await resolveHistoricalNativeScriptCorpus({
      deploymentFingerprint: input.workflow.binding.deploymentFingerprint,
      checkpointStore: input.workflow.historicalNativeScriptCheckpointStore,
      historySource: input.workflow.historicalNativeScriptHistorySource,
      currentEvidence: canonical,
      sources: input.sources,
    });
    const priorLedger =
      await deriveResolvedOutputPriorLedgerReplayFromHistoricalCorpus({
        block: canonical,
        corpus,
      });
    const findings = detectResolvedOutputNonCanonicalCompleteReplay({
      block: canonical,
      priorLedger,
    });
    if (findings.length !== 1)
      throw new Error(
        `resolvedOutputNonCanonical public replay yielded ${findings.length.toString()} exact findings`,
      );
    const evidence = findings[0]!;
    const source = await deriveResolvedOutputNonCanonicalAuthenticatedSource({
      block: canonical,
      evidence,
    });
    const runtime = createManifestBoundResolvedOutputNonCanonicalRuntime({
      config: input.workflow.config,
      journal: input.journal,
      observe: async () =>
        resolvedOutputStageFromL1(
          (await input.workflow.l1.observe({ headerHash })).stage,
        ),
      resolveStage: createResolvedOutputNonCanonicalRawL1StageResolver({
        config: input.workflow.config,
        l1: input.workflow.l1,
        source,
      }),
    });
    return await runtime.runOrResume(evidence);
  };

type BoundContext = FamilyAssemblyContext<
  "resolvedOutputNonCanonical",
  (typeof WITNESS_ROLES)[number],
  true,
  5,
  RunContext
>;

const runs = new WeakMap<
  BoundContext,
  ReturnType<typeof createResolvedOutputNonCanonicalRecoveryPorts>
>();

const runFor = (context: BoundContext) => {
  const existing = runs.get(context);
  if (existing !== undefined) return existing;
  const created = createResolvedOutputNonCanonicalRecoveryPorts(
    context.runtime.workflow,
    context.runtime.sources,
  );
  runs.set(context, created);
  return created;
};

export const RESOLVED_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION = defineFamily<
  "resolvedOutputNonCanonical",
  (typeof WITNESS_ROLES)[number],
  true,
  5,
  RunContext
>({
  category: "resolvedOutputNonCanonical",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    ResolvedOutputStep02DatumSchema,
    ResolvedOutputStep03DatumSchema,
    ResolvedOutputStep04DatumSchema,
    ResolvedOutputStep05DatumSchema,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => RESOLVED_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: RESOLVED_OUTPUT_NON_CANONICAL_CURSOR_SPEC,
    stepContractNames: [
      RESOLVED_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.step01,
      RESOLVED_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.step02,
      RESOLVED_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.step03,
      RESOLVED_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.step04,
      RESOLVED_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.step05,
    ],
    transactionPort: (context) => runFor(context).transactions,
  },
  fieldCarriage: [
    {
      requirementForAction: (context, input) =>
        runFor(context).requirementForAction(input),
    },
  ],
  extend: (context) => ({
    resolveReplayContext: runFor(context).resolveReplayContext,
  }),
});

export const createResolvedOutputNonCanonicalRecoveryAdapter = (
  workflow: ManifestBoundResolvedOutputNonCanonicalWorkflow,
  sources: readonly RetainedDaPayloadSource[],
) => {
  const assembled = assembleBoundManifestBoundFamilyWorkflow(
    RESOLVED_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION,
    workflow.deployment,
    { workflow, sources },
  );
  return {
    ...assembled,
    transactions:
      assembled.transactions as CursorFamilyTransactionPort<"resolvedOutputNonCanonical">,
    resolveReplayContext: (
      assembled as typeof assembled &
        Pick<
          ReturnType<typeof createResolvedOutputNonCanonicalRecoveryPorts>,
          "resolveReplayContext"
        >
    ).resolveReplayContext,
  };
};

export const executeManifestBoundResolvedOutputNonCanonicalWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundResolvedOutputNonCanonicalWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}) =>
  await executeManifestBoundFamilyRecovery({
    ...workflow,
    sources,
    journal,
    ...createResolvedOutputNonCanonicalRecoveryAdapter(workflow, sources),
  });
