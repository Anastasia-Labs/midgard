import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";

import { fetchCanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { type RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import { TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { type CursorFamilyTransactionPort } from "../workflow/cursor-family-adapter.js";
import { defineFamily } from "../workflow/family-definition.js";
import { observeFraudProofWorkflowHeader } from "../workflow/family-l1-observation.js";
import { type FraudProofWorkflowJournalStore } from "../workflow/journal.js";
import {
  assembleBoundManifestBoundFamilyWorkflow,
  bindManifestBoundFamilyWorkflow,
} from "../workflow/manifest-bound-family-assembly.js";
import { executeManifestBoundFamilyRecovery } from "../workflow/manifest-bound-family-recovery.js";
import {
  TransactionOutputStep02DatumSchema,
  TransactionOutputStep03DatumSchema,
  TransactionOutputStep04DatumSchema,
} from "./schemas.js";
import {
  type TransactionOutputJournal,
  type TransactionOutputStage,
} from "./transaction-output-non-canonical.js";
import {
  createManifestBoundTransactionOutputNonCanonicalRuntime,
  type ManifestBoundTransactionOutputNonCanonicalWorkflow,
  type ManifestBoundTransactionOutputNonCanonicalWorkflowConfig,
  type RunContext,
  runFor,
  transactionOutputStageFromL1,
  WITNESS_ROLES,
} from "./workflow.create-transaction-output-non-canonical-recovery-ports.js";
import {
  deriveTransactionOutputNonCanonicalAuthenticatedSource,
  deriveTransactionOutputNonCanonicalEvidenceFromCanonicalBlock,
} from "./workflow.derive-transaction-output-non-canonical-authenticated-source.js";
import { createTransactionOutputNonCanonicalRawL1StageResolver } from "./workflow.detect-transaction-output-non-canonical-complete-replay.js";
import {
  TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS,
  TRANSACTION_OUTPUT_NON_CANONICAL_WORKFLOW,
  transactionOutputNonCanonicalConfigFromBinding,
} from "./workflow.transaction-output-non-canonical-config-from-binding.js";
import { TRANSACTION_OUTPUT_NON_CANONICAL_CURSOR_SPEC } from "./workflow-spec.js";

/** Production installation factory; no evidence object is accepted here. */
export const createManifestBoundTransactionOutputNonCanonicalWorkflow = async (
  input: ManifestBoundTransactionOutputNonCanonicalWorkflowConfig,
): Promise<ManifestBoundTransactionOutputNonCanonicalWorkflow> => {
  const deployment = await bindManifestBoundFamilyWorkflow(
    TRANSACTION_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION,
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
  const config = transactionOutputNonCanonicalConfigFromBinding({
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
    workflowVersion: TRANSACTION_OUTPUT_NON_CANONICAL_WORKFLOW,
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
export const runOrResumeManifestBoundTransactionOutputNonCanonicalWorkflow =
  async (input: {
    readonly workflow: ManifestBoundTransactionOutputNonCanonicalWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: TransactionOutputJournal;
  }): Promise<TransactionOutputStage> => {
    if (Object.keys(input).sort().join(",") !== "journal,sources,workflow") {
      throw new Error(
        "transactionOutputNonCanonical runner rejects caller-authored evidence inputs",
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
      deriveTransactionOutputNonCanonicalEvidenceFromCanonicalBlock(canonical);
    const source = await deriveTransactionOutputNonCanonicalAuthenticatedSource(
      {
        block: canonical,
        evidence,
      },
    );
    const runtime = createManifestBoundTransactionOutputNonCanonicalRuntime({
      config: input.workflow.config,
      journal: input.journal,
      observe: async () =>
        transactionOutputStageFromL1(
          (await input.workflow.l1.observe({ headerHash })).stage,
        ),
      resolveStage: createTransactionOutputNonCanonicalRawL1StageResolver({
        config: input.workflow.config,
        l1: input.workflow.l1,
        source,
      }),
    });
    return await runtime.runOrResume(evidence);
  };

export const TRANSACTION_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION = defineFamily<
  "transactionOutputNonCanonical",
  (typeof WITNESS_ROLES)[number],
  true,
  4,
  RunContext
>({
  category: "transactionOutputNonCanonical",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    TransactionOutputStep02DatumSchema,
    TransactionOutputStep03DatumSchema,
    TransactionOutputStep04DatumSchema,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  replayer: () => TRANSACTION_OUTPUT_NON_CANONICAL_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: TRANSACTION_OUTPUT_NON_CANONICAL_CURSOR_SPEC,
    stepContractNames: [
      TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.step01,
      TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.step02,
      TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.step03,
      TRANSACTION_OUTPUT_NON_CANONICAL_MANIFEST_CONTRACTS.step04,
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

export const createTransactionOutputNonCanonicalRecoveryAdapter = (
  workflow: ManifestBoundTransactionOutputNonCanonicalWorkflow,
) => {
  const assembled = assembleBoundManifestBoundFamilyWorkflow(
    TRANSACTION_OUTPUT_NON_CANONICAL_FAMILY_DEFINITION,
    workflow.deployment,
    { workflow },
  );
  return {
    ...assembled,
    transactions:
      assembled.transactions as CursorFamilyTransactionPort<"transactionOutputNonCanonical">,
  };
};

export const executeManifestBoundTransactionOutputNonCanonicalWorkflow =
  async ({
    workflow,
    sources,
    journal,
  }: {
    readonly workflow: ManifestBoundTransactionOutputNonCanonicalWorkflow;
    readonly sources: readonly RetainedDaPayloadSource[];
    readonly journal: FraudProofWorkflowJournalStore;
  }) =>
    await executeManifestBoundFamilyRecovery({
      ...workflow,
      sources,
      journal,
      ...createTransactionOutputNonCanonicalRecoveryAdapter(workflow),
    });
