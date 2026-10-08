import {
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxWitnessSetCompact,
} from "@al-ft/midgard-core";

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
import { createWitnessScriptDecodingCentralJournalAdapter } from "./central-journal.js";
import {
  runWitnessScriptDecodingProof,
  type WitnessScriptDecodingEvidence,
  type WitnessScriptDecodingJournalEntry,
} from "./witness-script-decoding.js";
import { createManifestBoundWitnessScriptDecodingSubmission } from "./workflow.create-manifest-bound-witness-script-decoding-submission.js";
import {
  deriveWitnessScriptDecodingAuthenticatedSource,
  deriveWitnessScriptDecodingEvidenceFromCanonicalBlock,
} from "./workflow.derive-witness-script-decoding-authenticated-source.js";
import {
  createWitnessScriptDecodingRawL1StageResolver,
  type WitnessScriptDecodingRuntimeLoader,
} from "./workflow.detect-witness-script-decoding-complete-replay.js";
import {
  type LoadManifestBoundWitnessScriptDecodingConfig,
  loadManifestBoundWitnessScriptDecodingConfig,
  type ManifestBoundWitnessScriptDecodingConfig,
  WITNESS_SCRIPT_DECODING_WORKFLOW,
  type WitnessScriptDecodingJournal,
} from "./workflow.witness-script-decoding-config-from-binding.js";

export const loadWitnessScriptDecodingRuntime = async (
  input: WitnessScriptDecodingRuntimeLoader,
) => {
  const config = await loadManifestBoundWitnessScriptDecodingConfig(
    input.config,
  );
  return createManifestBoundWitnessScriptDecodingRuntime({
    config,
    journal: input.journal,
    observe: input.observe,
    resolveStage: input.resolveStage,
  });
};

export const createManifestBoundWitnessScriptDecodingRuntime = ({
  config,
  journal,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundWitnessScriptDecodingConfig;
  readonly journal: WitnessScriptDecodingJournal;
  readonly observe: WitnessScriptDecodingRuntimeLoader["observe"];
  readonly resolveStage: WitnessScriptDecodingRuntimeLoader["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createWitnessScriptDecodingCentralJournalAdapter
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => {
  const submission = createManifestBoundWitnessScriptDecodingSubmission({
    config,
    observe: async (identity) => {
      const observed = await observe(identity);
      await centralJournal?.reconcile(observed.stage);
      return observed;
    },
    resolveStage,
    centralJournal,
    stateQueueMutationLeaseCoordinator,
  });
  return Object.freeze({
    runtimeVersion: WITNESS_SCRIPT_DECODING_WORKFLOW,
    config,
    runOrResume: async (evidence: WitnessScriptDecodingEvidence) =>
      await runWitnessScriptDecodingProof({
        evidence,
        load: journal.load,
        append: journal.append,
        submission,
      }),
  });
};

export type ManifestBoundWitnessScriptDecodingWorkflowConfig =
  LoadManifestBoundWitnessScriptDecodingConfig &
    Readonly<{
      l1Source: FraudProofL1Source;
      decisionDigest: string;
      stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
    }>;

export type ManifestBoundWitnessScriptDecodingWorkflow = Readonly<{
  deployment: Deployment;
  workflowVersion: typeof WITNESS_SCRIPT_DECODING_WORKFLOW;
  config: ManifestBoundWitnessScriptDecodingConfig;
  binding: FraudProofWorkflowDeploymentBinding<"witnessScriptDecoding">;
  l1: FraudProofFamilyL1ObservationPort<"witnessScriptDecoding">;
  stateQueueMutationLeaseCoordinator: StateQueueMutationLeaseCoordinator;
  decisionDigest: string;
}>;

export const witnessScriptDecodingObservationFromL1 = (
  stage: Awaited<
    ReturnType<
      FraudProofFamilyL1ObservationPort<"witnessScriptDecoding">["observe"]
    >
  >["stage"],
): Pick<
  WitnessScriptDecodingJournalEntry,
  "stage" | "transactionId" | "outputReference" | "checkpointHash"
> => {
  switch (stage.kind) {
    case "not_started":
      return {
        stage: "none",
        transactionId: "0".repeat(64),
        outputReference: null,
        checkpointHash: null,
      };
    case "step":
      if (stage.step > 4)
        throw new Error(
          "witnessScriptDecoding L1 stage exceeds four-step topology",
        );
      return {
        stage:
          stage.step === 1
            ? "step01"
            : stage.step === 2
              ? "step02"
              : stage.step === 3
                ? "scan"
                : "step04",
        transactionId: stage.threadOutRef.split("#")[0]!,
        outputReference: stage.threadOutRef,
        checkpointHash: null,
      };
    case "proof_token":
      return {
        stage: "proven",
        transactionId: stage.fraudProofOutRef.split("#")[0]!,
        outputReference: stage.fraudProofOutRef,
        checkpointHash: null,
      };
    case "removed":
      return {
        stage: "removed",
        transactionId: stage.terminal.correction.removalTxHash,
        outputReference: null,
        checkpointHash: null,
      };
  }
};

/** Material re-derived from admitted canonical evidence before durable encoding. */
export const prepareWitnessScriptDecodingRecoveryMaterial = async (
  canonical: CanonicalBlockEvidence,
  detectionId: string,
) => {
  const evidence =
    deriveWitnessScriptDecodingEvidenceFromCanonicalBlock(canonical);
  const source = await deriveWitnessScriptDecodingAuthenticatedSource({
    block: canonical,
    evidence,
  });
  return {
    category: "witnessScriptDecoding" as const,
    headerHash: canonical.headerHash,
    detectionId,
    evidence,
    source,
  };
};

export const createWitnessScriptDecodingRecoveryPorts = (
  workflow: ManifestBoundWitnessScriptDecodingWorkflow,
) => {
  const { config, binding, l1, stateQueueMutationLeaseCoordinator } = workflow;
  const category = "witnessScriptDecoding";
  const material = createCanonicalFamilyArtifactPort(
    async ({ evidence, classification }) =>
      await prepareWitnessScriptDecodingRecoveryMaterial(
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
        step_03: "submitScanOrResume",
        step_04: "submitStep04",
      } as const;
      const familyAction = actions[input.stage as keyof typeof actions];
      if (familyAction === undefined)
        throw new Error(
          `${category} cursor action is outside its exact topology`,
        );
      const transaction = await captureLocallyEvaluatedTransaction(
        async (preSubmitBoundary) => {
          const submission = createManifestBoundWitnessScriptDecodingSubmission(
            {
              config,
              preSubmitBoundary,
              observe: async () =>
                witnessScriptDecodingObservationFromL1(
                  (
                    await l1.observe({
                      headerHash: binding.definition.headerHash,
                    })
                  ).stage,
                ),
              resolveStage: createWitnessScriptDecodingRawL1StageResolver({
                config,
                l1,
                source: admitted.source,
              }),
            },
          );
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
        anchorSourceKind: evidence.finding.subject.source_kind === 1n ? 1n : 0n,
        witnessSet: (() => {
          const compact = decodeMidgardNativeTxWitnessSetCompact(
            Buffer.from(source.witnessSetCompactCbor, "hex"),
          );
          return {
            addr_tx_wits_hash: compact.addrTxWitsHash.toString("hex"),
            script_tx_wits_hash: compact.scriptTxWitsHash.toString("hex"),
            redeemer_tx_wits_hash: compact.redeemerTxWitsHash.toString("hex"),
          };
        })(),
        anchorWitnessSetHash: evidence.finding.witnessSetHash,
        fieldIndex: 6,
        anchorTxId: evidence.finding.subject.transaction_id,
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
  "witnessScriptDecoding",
  (typeof WITNESS_ROLES)[number],
  true,
  4
>;

export type RunContext = Readonly<{
  workflow: ManifestBoundWitnessScriptDecodingWorkflow;
}>;

export type BoundContext = FamilyAssemblyContext<
  "witnessScriptDecoding",
  (typeof WITNESS_ROLES)[number],
  true,
  4,
  RunContext
>;

export const runs = new WeakMap<
  BoundContext,
  ReturnType<typeof createWitnessScriptDecodingRecoveryPorts>
>();
