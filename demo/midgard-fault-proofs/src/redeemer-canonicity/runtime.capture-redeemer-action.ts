import {
  decodeMidgardForcedTxCompact,
  deriveMidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  encodeProofThreadForcedSourceKey,
  FraudProofComputationThreadStepDatum,
} from "@al-ft/midgard-sdk/fraud-proof";
import { Data } from "@lucid-evolution/lucid";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { claimedCoordinateDecimal } from "../evidence/claimed-coordinate.js";
import { resolveFaultProofFieldPreimageCertificate } from "../field-opening.certify-fault-proof-field-carriage.js";
import { resolveFaultProofFieldCarriagePublications } from "../field-opening.publish-fault-proof-field-carriage.js";
import { requireLinearFaultThreadUtxo } from "../linear-fault-family.js";
import {
  buildTrieView,
  requireProof,
} from "../prepare-double-spend.resolve-double-spend-pair.js";
import { buildInit } from "../submit-init.submit-init.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.build-source-membership-proof.js";
import { REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.create-complete-canonical-replay-union.js";
import {
  CURSOR_FAMILY_TRANSACTION_PORT,
  type CursorFamilyCapturedAction,
  type CursorFamilyTransactionPort,
} from "../workflow/cursor-family-adapter.admit-reference-scripts.js";
import {
  captureCursorRemoval,
  cursorStringField,
  STATE_QUEUE_REMOVAL_REFERENCE_SCRIPTS,
} from "../workflow/cursor-family-runtime.js";
import {
  cursorStepContractNames,
  defineFamily,
} from "../workflow/family-definition.family-definition.js";
import { workflowFieldCarriageOwnOutputs } from "../workflow/field-carriage-prerequisite.own-carriage-claims.js";
import {
  journalJsonDigest,
  type JournalJsonObject,
} from "../workflow/journal.fraud-proof-workflow-terminal.js";
import { type FraudProofWorkflowAction } from "../workflow/orchestrator.js";
import {
  proofMaterialsAccess,
  type ProofMaterialsHolder,
} from "../workflow/proof-materials-view.js";
import { workflowStepTransaction } from "../workflow/step-transaction.js";
import { detectRedeemerCanonicityFromCanonicalBlock } from "./authenticated-workflow.js";
import {
  admitRedeemerWorkflowArtifact,
  boundFor,
  redeemerField,
  type RedeemerWorkflowCore,
  selectDetection,
  WITNESS_ROLES,
} from "./runtime.admit-redeemer-workflow-artifact.js";
import {
  RedeemerCanonicityStep01SourceArtifactSchema,
  RedeemerCanonicityStep02DataDatumSchema,
  RedeemerCanonicityStep02DatumSchema,
  RedeemerCanonicityStep03DatumSchema,
  RedeemerCanonicityVerdictSubjectSchema,
} from "./schemas.js";
import {
  buildRedeemerCanonicityStep01Accepted,
  buildRedeemerCanonicityStep01Forced,
} from "./submit-step-01.js";
import { buildRedeemerCanonicityStep02 } from "./submit-step-02.js";
import { buildRedeemerCanonicityStep02Data } from "./submit-step-02-data.js";
import { buildRedeemerCanonicityStep03 } from "./submit-step-03.js";

const CURSOR_SPEC = {
  category: "redeemerCanonicity",
  stepCount: 4,
  successors: { 1: [2], 2: [3, 4], 3: [3, 4], 4: ["proof_token"] },
} as const;

export const REDEEMER_CANONICITY_FAMILY_DEFINITION = defineFamily<
  "redeemerCanonicity",
  (typeof WITNESS_ROLES)[number],
  true,
  4
>({
  category: "redeemerCanonicity",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    RedeemerCanonicityStep02DatumSchema,
    RedeemerCanonicityStep02DataDatumSchema,
    RedeemerCanonicityStep03DatumSchema,
  ],
  witnessRoles: WITNESS_ROLES,
  fieldPreimageCertificate: true,
  fieldGrammarCertificate: true,
  auxiliaryReferenceScripts: STATE_QUEUE_REMOVAL_REFERENCE_SCRIPTS,
  replayer: () => REDEEMER_CANONICITY_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: CURSOR_SPEC,
    stepContractNames: cursorStepContractNames(CURSOR_SPEC),
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
    // A rejection may name any codec integer, so the artifact carries its decimal.
    redeemerIndex: claimedCoordinateDecimal(detection.evidence.redeemerIndex),
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
              },
            } as never,
            RedeemerCanonicityStep01SourceArtifactSchema as never,
          ),
  };
};

const captureRedeemerAction = async (
  workflow: RedeemerWorkflowCore,
  action: FraudProofWorkflowAction,
  artifact: JournalJsonObject,
  holder: ProofMaterialsHolder,
): Promise<CursorFamilyCapturedAction> => {
  const ownOutputs = workflowFieldCarriageOwnOutputs({
    category: "redeemerCanonicity",
    materials: proofMaterialsAccess({
      signer: workflow.signer,
      lucid: workflow.lucid,
      holder,
    }),
  });
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
  const { transaction } = await workflowStepTransaction(
    await (async () => {
      const common = {
        lucid: workflow.lucid,
        contracts: workflow.contracts,
        signer: workflow.signer,
        categoryId,
      };
      if (input.stage === "init")
        return await buildInit({
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
          return await buildRedeemerCanonicityStep01Accepted({
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
          return await buildRedeemerCanonicityStep01Forced({
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
            ownOutputs,
            lucid: workflow.lucid,
            publisherAddress: workflow.signer.proofMaterialsAddress,
            planned,
          });
        if (publishedCarriageUtxos === undefined)
          throw new Error("redeemerCanonicity field publications disappeared");
        const certificateUtxo = await resolveFaultProofFieldPreimageCertificate(
          {
            ownOutputs,
            lucid: workflow.lucid,
            network: workflow.binding.network,
            planned,
            certificatePolicyId:
              workflow.contracts.fieldPreimageCertificatePolicyId,
          },
        );
        if (planned.plan.tier === "Certified" && certificateUtxo === undefined)
          throw new Error("redeemerCanonicity certificate disappeared");
        return await buildRedeemerCanonicityStep02({
          ...common,
          ownOutputs,
          threadOutRef: cursorStringField(input, "threadOutRef"),
          evidence: admitted.evidence,
          nativeTxCompactCbor: admitted.nativeTxCompactCbor,
          witnessSetCompactCbor: admitted.witnessSetCompactCbor,
          publishedCarriageUtxos,
          certificateUtxo,
          referenceScriptUtxo: workflow.referenceScripts.steps[1],
          certificateReferenceScriptUtxo:
            workflow.referenceScripts.fieldPreimageCertificateMint,
          // A missing grammar certificate is certified and submitted inline
          // before the step is built (certifyFaultProofFieldGrammar).
          fieldGrammar: workflow.fieldGrammar,
        });
      } else if (input.stage === "step_03")
        return await buildRedeemerCanonicityStep02Data({
          ...common,
          threadOutRef: cursorStringField(input, "threadOutRef"),
          evidence: admitted.evidence,
          referenceScriptUtxo: workflow.referenceScripts.steps[2],
        });
      else if (input.stage === "step_04")
        return await buildRedeemerCanonicityStep03({
          ...common,
          threadOutRef: cursorStringField(input, "threadOutRef"),
          evidence: admitted.evidence,
          referenceScriptUtxo: workflow.referenceScripts.steps[3],
          witnessReferenceScripts: workflow.referenceScripts.witnesses,
        });
      else throw new Error("redeemerCanonicity action changed");
    })(),
    `redeemer-canonicity ${action.actionId}`,
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
  capture: async ({ action, artifact, holder }) =>
    await captureRedeemerAction(workflow, action, artifact, holder),
});
