import { decodeMidgardFieldPreimage } from "@al-ft/midgard-core";
import { type FraudProofCatalogueCategoryName } from "@al-ft/midgard-sdk";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { planFaultProofFieldOpening } from "../field-opening.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import { type FraudProofWorkflowDeploymentBinding } from "../workflow/deployment-manifest-binding.js";
import {
  type FamilyAssemblyContext,
  type FamilyDeploymentContext,
  type LinearFamilyPrerequisiteInput,
} from "../workflow/family-definition.js";
import { type FraudProofFamilyL1ObservationPort } from "../workflow/family-l1-observation.js";
import type { FieldCarriageRequirement } from "../workflow/field-carriage-prerequisite.js";
import type { FraudProofL1Source } from "../workflow/l1-source.js";
import { createCanonicalFamilyArtifactPort } from "../workflow/manifest-bound-family-recovery.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
} from "../workflow/transaction-boundary.js";
import { createTransactionOutputNonCanonicalCentralJournalAdapter } from "./central-journal.js";
import {
  runTransactionOutputProof,
  type TransactionOutputEvidence,
  type TransactionOutputJournal,
  type TransactionOutputStage,
} from "./transaction-output-non-canonical.js";
import { createManifestBoundTransactionOutputNonCanonicalSubmission } from "./workflow.create-manifest-bound-transaction-output-non-canonical-submission.js";
import {
  deriveTransactionOutputNonCanonicalAuthenticatedSource,
  deriveTransactionOutputNonCanonicalEvidenceFromCanonicalBlock,
} from "./workflow.derive-transaction-output-non-canonical-authenticated-source.js";
import {
  createTransactionOutputNonCanonicalRawL1StageResolver,
  type TransactionOutputNonCanonicalRuntimeLoader,
} from "./workflow.detect-transaction-output-non-canonical-complete-replay.js";
import {
  type LoadManifestBoundTransactionOutputNonCanonicalConfig,
  loadManifestBoundTransactionOutputNonCanonicalConfig,
  type ManifestBoundTransactionOutputNonCanonicalConfig,
  TRANSACTION_OUTPUT_NON_CANONICAL_WORKFLOW,
} from "./workflow.transaction-output-non-canonical-config-from-binding.js";

export const loadTransactionOutputNonCanonicalRuntime = async (
  input: TransactionOutputNonCanonicalRuntimeLoader,
) => {
  const config = await loadManifestBoundTransactionOutputNonCanonicalConfig(
    input.config,
  );
  return createManifestBoundTransactionOutputNonCanonicalRuntime({
    config,
    journal: input.journal,
    observe: input.observe,
    resolveStage: input.resolveStage,
  });
};

export const createManifestBoundTransactionOutputNonCanonicalRuntime = ({
  config,
  journal,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundTransactionOutputNonCanonicalConfig;
  readonly journal: TransactionOutputJournal;
  readonly observe: TransactionOutputNonCanonicalRuntimeLoader["observe"];
  readonly resolveStage: TransactionOutputNonCanonicalRuntimeLoader["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createTransactionOutputNonCanonicalCentralJournalAdapter
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => {
  const submission = createManifestBoundTransactionOutputNonCanonicalSubmission(
    {
      config,
      observe: async (identity) => {
        const observed = await observe(identity);
        await centralJournal?.reconcile(observed);
        return observed;
      },
      resolveStage,
      centralJournal,
      stateQueueMutationLeaseCoordinator,
    },
  );
  return Object.freeze({
    runtimeVersion: TRANSACTION_OUTPUT_NON_CANONICAL_WORKFLOW,
    config,
    runOrResume: async (evidence: TransactionOutputEvidence) =>
      await runTransactionOutputProof({
        evidence,
        journal,
        submission,
      }),
  });
};

export type ManifestBoundTransactionOutputNonCanonicalWorkflowConfig =
  LoadManifestBoundTransactionOutputNonCanonicalConfig &
    Readonly<{
      l1Source: FraudProofL1Source;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    }>;

export type ManifestBoundTransactionOutputNonCanonicalWorkflow = Readonly<{
  deployment: Deployment;
  workflowVersion: typeof TRANSACTION_OUTPUT_NON_CANONICAL_WORKFLOW;
  config: ManifestBoundTransactionOutputNonCanonicalConfig;
  binding: FraudProofWorkflowDeploymentBinding<"transactionOutputNonCanonical">;
  l1: FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
}>;

export const transactionOutputStageFromL1 = (
  stage: Awaited<
    ReturnType<
      FraudProofFamilyL1ObservationPort<FraudProofCatalogueCategoryName>["observe"]
    >
  >["stage"],
): TransactionOutputStage => {
  switch (stage.kind) {
    case "not_started":
      return "none";
    case "step":
      if (stage.step === 1) return "step01";
      if (stage.step === 2) return "step02";
      if (stage.step === 3) return "step03";
      if (stage.step === 4) return "step04";
      throw new Error(
        "transactionOutputNonCanonical L1 stage exceeds four-step topology",
      );
    case "proof_token":
      return "proven";
    case "removed":
      return "removed";
  }
};

/** Material re-derived from admitted canonical evidence before durable encoding. */
export const prepareTransactionOutputNonCanonicalRecoveryMaterial = async (
  canonical: CanonicalBlockEvidence,
  detectionId: string,
) => {
  const evidence =
    deriveTransactionOutputNonCanonicalEvidenceFromCanonicalBlock(canonical);
  const source = await deriveTransactionOutputNonCanonicalAuthenticatedSource({
    block: canonical,
    evidence,
  });
  return {
    category: "transactionOutputNonCanonical" as const,
    headerHash: canonical.headerHash,
    detectionId,
    evidence,
    source,
  };
};

const createTransactionOutputNonCanonicalRecoveryPorts = (
  workflow: ManifestBoundTransactionOutputNonCanonicalWorkflow,
) => {
  const { config, binding, l1, stateQueueMutationLeaseCoordinator } = workflow;
  const category = "transactionOutputNonCanonical";
  const material = createCanonicalFamilyArtifactPort(
    async ({ evidence, classification }) =>
      await prepareTransactionOutputNonCanonicalRecoveryMaterial(
        evidence,
        classification.selected.detectionId,
      ),
  );
  const transactions: CursorFamilyTransactionPort<typeof category> = {
    portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
    category,
    prepare: material.prepare,
    validatePreparedArtifact: material.validatePreparedArtifact,
    capture: async ({ action, artifact }) => {
      const input = cursorFamilyActionInput({ category, action });
      if (input.stage === "remove")
        return await captureCursorRemoval({
          category,
          lucid: config.lucid,
          blueprint: binding.blueprint,
          deploymentInfo: binding.deploymentInfo,
          network: binding.network,
          signer: config.signer,
          headerHash: binding.definition.headerHash,
          input,
          stateQueueMutationLeaseCoordinator,
          fraudProverRewardLovelace: BigInt(
            binding.releaseEconomics.policy.fraudProverRewardLovelace,
          ),
        });
      const admitted = material.require(artifact);
      const actions = {
        init: "submitInit",
        step_01: "submitStep01",
        step_02: "submitStep02",
        step_03: "submitStep03",
        step_04: "submitStep04",
      } as const;
      const familyAction = actions[input.stage as keyof typeof actions];
      if (familyAction === undefined)
        throw new Error(
          `${category} cursor action is outside its exact topology`,
        );
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          const submission =
            createManifestBoundTransactionOutputNonCanonicalSubmission({
              config,
              preSubmitBoundary,
              observe: async () =>
                transactionOutputStageFromL1(
                  (
                    await l1.observe({
                      headerHash: binding.definition.headerHash,
                    })
                  ).stage,
                ),
              resolveStage:
                createTransactionOutputNonCanonicalRawL1StageResolver({
                  config,
                  l1,
                  source: admitted.source,
                }),
            });
          await submission.submit(familyAction, admitted.evidence);
        },
      );
      if (
        input.stage !== "init" &&
        !workflowTransactionInputOutRefs(transaction.signed).includes(
          cursorStringField(input, "threadOutRef"),
        )
      )
        throw new Error(
          `${category} captured transaction changed its authenticated thread input`,
        );
      return { transaction };
    },
  };
  const requirementForAction = ({
    action,
    artifact,
  }: LinearFamilyPrerequisiteInput): FieldCarriageRequirement | null => {
    if (action.input.stage !== "step_02") return null;
    const { evidence, source } = material.require(artifact);
    const certificate = binding.fieldPreimageCertificate;
    if (certificate === null)
      throw new Error(`${category} omitted field certificate authority`);
    return {
      planned: planFaultProofFieldOpening({
        anchorSourceKind: evidence.subject.source_kind === 1n ? 1n : 0n,
        fieldIndex: evidence.fieldIndex,
        anchorTxId: evidence.subject.transaction_id,
        nativeTxCompactCbor: source.nativeTxCompactCbor,
        itemCbors: decodeMidgardFieldPreimage(
          Buffer.from(evidence.fieldPreimageHex, "hex"),
        ),
        owner: config.signer.paymentKeyHash,
        publish: true,
        label: `${category} field opening`,
      }),
      compactCbor: source.nativeTxCompactCbor,
      witnessSetCompactCbor: source.witnessSetCompactCbor,
      certificate: {
        policyId: certificate.policyId,
        mintingScript: certificate.mintingScript,
        referenceScriptUtxo:
          config.referenceScripts.fieldPreimageCertificateMint,
      },
    };
  };
  return { transactions, requirementForAction };
};

export const WITNESS_ROLES = [
  "computationThreadMint",
  "fraudProofMint",
  "phasMembershipWithdraw",
] as const;

type Deployment = FamilyDeploymentContext<
  "transactionOutputNonCanonical",
  (typeof WITNESS_ROLES)[number],
  true,
  4
>;

export type RunContext = Readonly<{
  workflow: ManifestBoundTransactionOutputNonCanonicalWorkflow;
}>;

type BoundContext = FamilyAssemblyContext<
  "transactionOutputNonCanonical",
  (typeof WITNESS_ROLES)[number],
  true,
  4,
  RunContext
>;

export const runs = new WeakMap<
  BoundContext,
  ReturnType<typeof createTransactionOutputNonCanonicalRecoveryPorts>
>();

export const runFor = (context: BoundContext) => {
  const existing = runs.get(context);
  if (existing !== undefined) return existing;
  const created = createTransactionOutputNonCanonicalRecoveryPorts(
    context.runtime.workflow,
  );
  runs.set(context, created);
  return created;
};
