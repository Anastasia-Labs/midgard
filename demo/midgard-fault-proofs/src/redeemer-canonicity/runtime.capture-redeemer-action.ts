import {
  decodeMidgardForcedTxCompact,
  deriveMidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  encodeProofThreadForcedSourceKey,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import {
  resolveFaultProofFieldCarriagePublications,
  resolveFaultProofFieldPreimageCertificate,
} from "../field-opening.js";
import { requireLinearFaultThreadUtxo } from "../linear-fault-family.js";
import { buildTrieView, requireProof } from "../prepare-double-spend.js";
import { submitInit } from "../submit-init.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import { REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyCapturedAction,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorStringField,
  STATE_QUEUE_REMOVAL_REFERENCE_SCRIPTS,
} from "../workflow/cursor-family-runtime.js";
import { defineFamily } from "../workflow/family-definition.js";
import {
  journalJsonDigest,
  type JournalJsonObject,
} from "../workflow/journal.js";
import { type FraudProofWorkflowAction } from "../workflow/orchestrator.js";
import { captureLocallyEvaluatedTransaction } from "../workflow/transaction-boundary.js";
import { detectRedeemerCanonicityFromCanonicalBlock } from "./authenticated-workflow.js";
import {
  admitRedeemerWorkflowArtifact,
  boundFor,
  redeemerField,
  type RedeemerWorkflowCore,
  selectDetection,
  STEP_CONTRACT_NAMES,
  WITNESS_ROLES,
} from "./runtime.admit-redeemer-workflow-artifact.js";
import {
  RedeemerCanonicityStep01SourceSchema,
  RedeemerCanonicityStep02DatumSchema,
  RedeemerCanonicityStep03DatumSchema,
  RedeemerCanonicityVerdictSubjectSchema,
} from "./schemas.js";
import {
  submitRedeemerCanonicityStep01Accepted,
  submitRedeemerCanonicityStep01Forced,
} from "./submit-step-01.js";
import { submitRedeemerCanonicityStep02 } from "./submit-step-02.js";
import { submitRedeemerCanonicityStep03 } from "./submit-step-03.js";

export const REDEEMER_CANONICITY_FAMILY_DEFINITION = defineFamily<
  "redeemerCanonicity",
  (typeof WITNESS_ROLES)[number],
  true,
  3
>({
  category: "redeemerCanonicity",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    RedeemerCanonicityStep02DatumSchema,
    RedeemerCanonicityStep03DatumSchema,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  auxiliaryReferenceScripts: STATE_QUEUE_REMOVAL_REFERENCE_SCRIPTS,
  replayer: () => REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: {
      category: "redeemerCanonicity",
      stepCount: 3,
      successors: { 1: [2], 2: [3], 3: ["proof_token"] },
    },
    stepContractNames: STEP_CONTRACT_NAMES,
    transactionPort: (context) =>
      createRedeemerCanonicityTransactionPort(boundFor(context)),
  },
  fieldCarriage: [
    {
      requirementForAction: (context, { action, artifact }) => {
        if (action.input.stage !== "step_02") return null;
        const workflow = boundFor(context);
        const { planned, admitted } = redeemerField(workflow, artifact);
        return {
          planned,
          compactCbor: admitted.nativeTxCompactCbor,
          witnessSetCompactCbor: admitted.witnessSetCompactCbor,
          certificate: {
            policyId: context.certificate.policyId,
            mintingScript: context.certificate.mintingScript,
            referenceScriptUtxo:
              context.references.fieldPreimageCertificateMint,
          },
        };
      },
    },
  ],
  extend: (context) => boundFor(context),
});

export const prepareRedeemerCanonicityWorkflowArtifact = async (
  block: CanonicalBlockEvidence,
): Promise<JournalJsonObject> => {
  const detection = selectDetection(
    detectRedeemerCanonicityFromCanonicalBlock(block),
  );
  const accepted =
    detection.source === "accepted"
      ? block.transactions.find(
          (tx) => tx.nodeTxId === detection.evidence.subject.transaction_id,
        )
      : undefined;
  const forced =
    detection.source === "forced"
      ? block.reconstruction.forcedTransactions.find(
          (tx) =>
            tx.value.tx_id === detection.evidence.subject.transaction_id &&
            encodeProofThreadForcedSourceKey(tx.key).toString("hex") ===
              detection.evidence.subject.source_key,
        )
      : undefined;
  const material =
    accepted !== undefined
      ? deriveMidgardNativeTxFaultEvidenceMaterial(
          Buffer.from(accepted.txCbor, "hex"),
        )
      : deriveMidgardForcedTxFaultEvidenceMaterial(forced!.fullTransactionCbor);
  const trie =
    accepted === undefined
      ? undefined
      : await buildTrieView(
          block.transactions.map((tx) => ({
            key: Buffer.from(tx.nodeTxId, "hex"),
            value: Buffer.from(tx.l2TransactionSourceCbor, "hex"),
          })),
        );
  const membership =
    forced === undefined
      ? undefined
      : await buildForcedTransactionLeafMembershipProof({
          reconstruction: block.reconstruction,
          eventKey: { ForcedTransactionEventKey: { tx_order_id: forced.key } },
        });
  return {
    schemaVersion: "midgard-redeemer-canonicity-workflow-artifact-v1",
    headerHash: block.headerHash,
    detectionId: detection.detectionId,
    subjectCbor: Data.to(
      detection.evidence.subject as never,
      RedeemerCanonicityVerdictSubjectSchema as never,
    ),
    redeemerIndex: detection.evidence.redeemerIndex,
    fieldPreimageHex: detection.evidence.fieldPreimageHex,
    fieldCommitmentHex: detection.evidence.fieldCommitmentHex,
    nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
    witnessSetCompactCbor:
      material.proofSource.witnessSetCompactCbor.toString("hex"),
    accepted:
      accepted === undefined
        ? null
        : {
            nativeTxId: accepted.nodeTxId,
            nativeTxCompactCbor:
              material.proofSource.compactCbor.toString("hex"),
            l2TransactionSourceCbor: accepted.l2TransactionSourceCbor,
            transactionsPhasRoot: trie!.root,
            txMembershipProofCbor: requireProof(
              trie!,
              Buffer.from(accepted.nodeTxId, "hex"),
              "redeemer-canonicity transaction",
            ),
          },
    forcedSourceCbor:
      forced === undefined
        ? null
        : Data.to(
            {
              ForcedSource: {
                input_index: 0n,
                output_index: 0n,
                header: block.header,
                membership,
                direction: detection.evidence.subject.direction,
              },
            } as never,
            RedeemerCanonicityStep01SourceSchema as never,
          ),
  };
};

const captureRedeemerAction = async (
  workflow: RedeemerWorkflowCore,
  action: FraudProofWorkflowAction,
  artifact: JournalJsonObject,
): Promise<CursorFamilyCapturedAction> => {
  const admitted = admitRedeemerWorkflowArtifact(artifact);
  const input = action.input;
  const categoryId = workflow.binding.resolvedContracts.category.categoryId;
  if (input.stage === "remove")
    return await captureCursorRemoval({
      category: "redeemerCanonicity",
      lucid: workflow.lucid,
      blueprint: workflow.binding.blueprint,
      deploymentInfo: workflow.binding.deploymentInfo,
      network: workflow.binding.network,
      signer: workflow.signer,
      headerHash: cursorStringField(artifact, "headerHash"),
      input: input as { stage: string },
      stateQueueMutationLeaseCoordinator:
        workflow.stateQueueMutationLeaseCoordinator,
      fraudProverRewardLovelace: BigInt(
        workflow.binding.releaseEconomics.policy.fraudProverRewardLovelace,
      ),
    });
  const transaction = await captureLocallyEvaluatedTransaction(
    async (preSubmitBoundary) => {
      const common = {
        lucid: workflow.lucid,
        contracts: workflow.contracts,
        signer: workflow.signer,
        categoryId,
        preSubmitBoundary,
        awaitConfirmation: false,
      };
      if (input.stage === "init")
        await submitInit({
          ...common,
          blueprint: workflow.binding.blueprint,
          deploymentInfo: workflow.binding.deploymentInfo,
          network: workflow.binding.network,
          fraudCategory: "redeemerCanonicity",
          fraudulentBlockOutRef: cursorStringField(
            input,
            "stateQueueBlockOutRef",
          ),
          fraudulentHeaderHash: cursorStringField(artifact, "headerHash"),
          witnessReferenceScripts: workflow.referenceScripts.witnesses,
        });
      else if (input.stage === "step_01") {
        if (admitted.accepted !== null) {
          const { threadUtxo, threadToken } =
            await requireLinearFaultThreadUtxo({
              lucid: workflow.lucid,
              contracts: workflow.contracts,
              categoryId,
              family: "redeemer-canonicity",
              stepIndex: 0,
              threadOutRef: cursorStringField(input, "threadOutRef"),
            });
          await submitRedeemerCanonicityStep01Accepted({
            ...common,
            blueprint: workflow.binding.blueprint,
            network: workflow.binding.network,
            finding: admitted.evidence,
            threadUtxo,
            threadToken,
            stateQueueBlockOutRef: cursorStringField(
              input,
              "stateQueueBlockOutRef",
            ),
            txInclusion: admitted.accepted,
            referenceScriptUtxo: workflow.referenceScripts.steps[0],
            witnessReferenceScripts: workflow.referenceScripts.witnesses,
          });
        } else
          await submitRedeemerCanonicityStep01Forced({
            ...common,
            threadOutRef: cursorStringField(input, "threadOutRef"),
            finding: admitted.evidence,
            forcedSource: admitted.forced!,
            witnessSetHash: Buffer.from(
              decodeMidgardForcedTxCompact(
                Buffer.from(admitted.nativeTxCompactCbor, "hex"),
              ).transactionWitnessSetHash,
            ).toString("hex"),
            referenceScriptUtxo: workflow.referenceScripts.steps[0],
          });
      } else if (input.stage === "step_02") {
        const { planned } = redeemerField(workflow, artifact);
        const publishedCarriageUtxos =
          await resolveFaultProofFieldCarriagePublications({
            lucid: workflow.lucid,
            publisherAddress: workflow.signer.address,
            planned,
          });
        if (publishedCarriageUtxos === undefined)
          throw new Error("redeemerCanonicity field publications disappeared");
        const certificateUtxo = await resolveFaultProofFieldPreimageCertificate(
          {
            lucid: workflow.lucid,
            network: workflow.binding.network,
            planned,
            certificatePolicyId:
              workflow.contracts.fieldPreimageCertificatePolicyId,
          },
        );
        if (planned.plan.tier === "Certified" && certificateUtxo === undefined)
          throw new Error("redeemerCanonicity certificate disappeared");
        await submitRedeemerCanonicityStep02({
          ...common,
          threadOutRef: cursorStringField(input, "threadOutRef"),
          evidence: admitted.evidence,
          nativeTxCompactCbor: admitted.nativeTxCompactCbor,
          witnessSetCompactCbor: admitted.witnessSetCompactCbor,
          publishedCarriageUtxos,
          certificateUtxo,
          referenceScriptUtxo: workflow.referenceScripts.steps[1],
          certificateReferenceScriptUtxo:
            workflow.referenceScripts.fieldPreimageCertificateMint,
        });
      } else if (input.stage === "step_03")
        await submitRedeemerCanonicityStep03({
          ...common,
          threadOutRef: cursorStringField(input, "threadOutRef"),
          evidence: admitted.evidence,
          referenceScriptUtxo: workflow.referenceScripts.steps[2],
          witnessReferenceScripts: workflow.referenceScripts.witnesses,
        });
      else throw new Error("redeemerCanonicity action changed");
    },
  );
  return { transaction };
};

const createRedeemerCanonicityTransactionPort = (
  workflow: RedeemerWorkflowCore,
): CursorFamilyTransactionPort<"redeemerCanonicity"> => ({
  portVersion: CURSOR_FAMILY_TRANSACTION_PORT,
  category: "redeemerCanonicity",
  validatePreparedArtifact: async ({ evidence, artifact }) => {
    if (
      journalJsonDigest(
        await prepareRedeemerCanonicityWorkflowArtifact(evidence),
      ) !== journalJsonDigest(artifact)
    )
      throw new Error(
        "prepared family artifact differs from retained evidence",
      );
  },
  prepare: async ({ evidence }) =>
    await prepareRedeemerCanonicityWorkflowArtifact(evidence),
  capture: async ({ action, artifact }) =>
    await captureRedeemerAction(workflow, action, artifact),
});
