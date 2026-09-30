import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  workflowActuationDecisionDigest,
  workflowJournalIsReconciliationOnly,
} from "../workflow/actuation-permit.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import {
  createFraudProofFamilyAuthenticatedL1TerminalVerifier,
  createFraudProofFamilyLocalKupmiosL1ObservationPort,
  type FraudProofFamilyL1ObservationPort,
} from "../workflow/family-l1-observation.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { type FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import type { LocalKupmiosHttpOgmiosSourceConfig } from "../workflow/local-kupmios-http-ogmios-source.js";
import { createMintItemNonCanonicalCentralJournalAdapter } from "./central-journal.js";
import {
  type MintItemEvidence,
  type MintItemJournal,
  type MintItemStage,
  runMintItemProof,
} from "./mint-item-non-canonical.js";
import { createManifestBoundMintItemNonCanonicalSubmission } from "./workflow.create-manifest-bound-mint-item-non-canonical-submission.js";
import {
  createMintItemNonCanonicalRawL1StageResolver,
  deriveMintItemNonCanonicalAuthenticatedSource,
  type MintItemNonCanonicalRuntimeLoader,
} from "./workflow.derive-mint-item-non-canonical-authenticated-source.js";
import {
  deriveMintItemNonCanonicalEvidenceFromCanonicalBlock,
  type LoadManifestBoundMintItemNonCanonicalConfig,
  loadManifestBoundMintItemNonCanonicalConfig,
  type ManifestBoundMintItemNonCanonicalConfig,
  MINT_ITEM_NON_CANONICAL_WORKFLOW,
} from "./workflow.load-manifest-bound-mint-item-non-canonical-config.js";

export const loadMintItemNonCanonicalRuntime = async (
  input: MintItemNonCanonicalRuntimeLoader,
) => {
  const config = await loadManifestBoundMintItemNonCanonicalConfig(
    input.config,
  );
  return createManifestBoundMintItemNonCanonicalRuntime({
    config,
    journal: input.journal,
    observe: input.observe,
    resolveStage: input.resolveStage,
  });
};

export const createManifestBoundMintItemNonCanonicalRuntime = ({
  config,
  journal,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundMintItemNonCanonicalConfig;
  readonly journal: MintItemJournal;
  readonly observe: MintItemNonCanonicalRuntimeLoader["observe"];
  readonly resolveStage: MintItemNonCanonicalRuntimeLoader["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createMintItemNonCanonicalCentralJournalAdapter
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => {
  const submission = createManifestBoundMintItemNonCanonicalSubmission({
    config,
    observe: async (identity) => {
      const observed = await observe(identity);
      await centralJournal?.reconcile(observed);
      return observed;
    },
    resolveStage,
    centralJournal,
    stateQueueMutationLeaseCoordinator,
  });
  return Object.freeze({
    runtimeVersion: MINT_ITEM_NON_CANONICAL_WORKFLOW,
    config,
    runOrResume: async (evidence: MintItemEvidence) =>
      await runMintItemProof({
        evidence,
        journal,
        submission,
      }),
  });
};

export type ManifestBoundMintItemNonCanonicalWorkflowConfig =
  LoadManifestBoundMintItemNonCanonicalConfig &
    Readonly<{
      source: Omit<LocalKupmiosHttpOgmiosSourceConfig, "releaseFinality">;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    }>;

export type ManifestBoundMintItemNonCanonicalWorkflow = Readonly<{
  workflowVersion: typeof MINT_ITEM_NON_CANONICAL_WORKFLOW;
  config: ManifestBoundMintItemNonCanonicalConfig;
  binding: FraudProofWorkflowDeploymentBinding<"mintItemNonCanonical">;
  l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
}>;

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundMintItemNonCanonicalWorkflow = async (
  input: ManifestBoundMintItemNonCanonicalWorkflowConfig,
): Promise<ManifestBoundMintItemNonCanonicalWorkflow> => {
  const config = await loadManifestBoundMintItemNonCanonicalConfig(input);
  const l1 = createFraudProofFamilyLocalKupmiosL1ObservationPort({
    source: input.source,
    releaseFinality: config.binding.releaseFinality,
    releaseEconomics: config.binding.releaseEconomics,
    definition: config.binding.definition,
  });
  return Object.freeze({
    workflowVersion: MINT_ITEM_NON_CANONICAL_WORKFLOW,
    config,
    binding: config.binding,
    l1,
    stateQueueMutationLeaseCoordinator:
      input.stateQueueMutationLeaseCoordinator,
    decisionDigest: input.decisionDigest,
  });
};

const mintItemStageFromL1 = (
  stage: Awaited<
    ReturnType<
      FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>["observe"]
    >
  >["stage"],
): MintItemStage => {
  switch (stage.kind) {
    case "not_started":
      return "none";
    case "step":
      if (stage.step === 1) return "step01";
      if (stage.step === 2) return "step02";
      if (stage.step === 3) return "step03";
      if (stage.step === 4) return "step04";
      throw new Error(
        "mintItemNonCanonical L1 stage exceeds four-step topology",
      );
    case "proof_token":
      return "proven";
    case "removed":
      return "removed";
  }
};

/**
 * Watcher-facing runner. Evidence is always reconstructed from authenticated
 * L1 plus public retained DA; unknown/caller-authored evidence fields fail.
 */
export const runOrResumeManifestBoundMintItemNonCanonicalWorkflow =
  async (input: {
    readonly workflow: ManifestBoundMintItemNonCanonicalWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: MintItemJournal;
  }): Promise<MintItemStage> => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow") {
      throw new Error(
        "mintItemNonCanonical runner rejects caller-authored evidence inputs",
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
      deriveMintItemNonCanonicalEvidenceFromCanonicalBlock(canonical);
    const source = await deriveMintItemNonCanonicalAuthenticatedSource({
      block: canonical,
      evidence,
    });
    const runtime = createManifestBoundMintItemNonCanonicalRuntime({
      config: input.workflow.config,
      journal: input.journal,
      observe: async () =>
        mintItemStageFromL1(
          (await input.workflow.l1.observe({ headerHash })).stage,
        ),
      resolveStage: createMintItemNonCanonicalRawL1StageResolver({
        config: input.workflow.config,
        l1: input.workflow.l1,
        source,
      }),
    });
    return await runtime.runOrResume(evidence);
  };

export const executeManifestBoundMintItemNonCanonicalWorkflow = async ({
  workflow,
  sources,
  journal,
}: {
  readonly workflow: ManifestBoundMintItemNonCanonicalWorkflow;
  readonly sources: readonly RetainedDaPayloadSource[];
  readonly journal: FraudProofWorkflowJournalStore;
}) => {
  const headerHash = workflow.binding.definition.headerHash;
  const centralJournal = createMintItemNonCanonicalCentralJournalAdapter({
    store: journal,
    deploymentFingerprint: workflow.binding.deploymentFingerprint,
    headerHash,
    // Fresh observation authority retains the original durable execution.
    decisionDigest:
      workflowActuationDecisionDigest(journal) ?? workflow.decisionDigest,
    transactionConfirmed: async (txHash) =>
      await workflow.l1.transactionConfirmed({ headerHash, txHash }),
  });
  const finish = async (
    terminal: Parameters<typeof centralJournal.finish>[0]["candidate"],
  ) => {
    await centralJournal.reconcile("removed");
    return await centralJournal.finish({
      candidate: terminal,
      releaseFinality: workflow.binding.releaseFinality,
      verifier: createFraudProofFamilyAuthenticatedL1TerminalVerifier(
        workflow.l1,
      ),
    });
  };
  const observed = await workflow.l1.observe({ headerHash });
  if (observed.stage.kind === "removed")
    return await finish(observed.stage.terminal);
  if (workflowJournalIsReconciliationOnly(journal))
    return {
      kind: "pending" as const,
      resumeOnObservation: true,
      reason: "existing mint-item correction is not yet removed",
    };
  const canonical = await fetchCanonicalBlockEvidence({
    observation: await observeFraudProofWorkflowHeader(workflow.l1, {
      headerHash,
    }),
    sources,
  });
  const evidence =
    deriveMintItemNonCanonicalEvidenceFromCanonicalBlock(canonical);
  const source = await deriveMintItemNonCanonicalAuthenticatedSource({
    block: canonical,
    evidence,
  });
  const runtime = createManifestBoundMintItemNonCanonicalRuntime({
    config: workflow.config,
    journal: centralJournal.familyJournal,
    observe: async () =>
      mintItemStageFromL1((await workflow.l1.observe({ headerHash })).stage),
    resolveStage: createMintItemNonCanonicalRawL1StageResolver({
      config: workflow.config,
      l1: workflow.l1,
      source,
    }),
    centralJournal,
    stateQueueMutationLeaseCoordinator:
      workflow.stateQueueMutationLeaseCoordinator,
  });
  const result = await runtime.runOrResume(evidence);
  if (result !== "removed") return result;
  const removed = await workflow.l1.observe({ headerHash });
  if (removed.stage.kind !== "removed")
    throw new Error(
      "mintItemNonCanonical removal changed before terminal verification",
    );
  return await finish(removed.stage.terminal);
};
