import {
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import { FraudProofComputationThreadStepDatum } from "@al-ft/midgard-sdk";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { planFaultProofFieldOpening } from "../field-opening.js";
import type { StateQueueMutationLeaseCoordinator } from "../remove-fraudulent-block.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import { FIELD_ITEM_WIDTH_ILLEGAL_COMPLETE_CANONICAL_REPLAY } from "../workflow/complete-replay.js";
import { CURSOR_FAMILY_TRANSACTION_PORT } from "../workflow/cursor-family-adapter.js";
import {
  captureCursorRemoval,
  cursorFamilyActionInput,
  cursorStringField,
} from "../workflow/cursor-family-runtime.js";
import {
  assertManifestBoundWorkflowSigner,
  bindFraudProofWorkflowDeployment,
} from "../workflow/deployment-manifest-binding.js";
import {
  acceptedTransactionSubject,
  forcedTransactionSubject,
} from "../workflow/detection-subject.js";
import { defineFamily } from "../workflow/family-definition.js";
import {
  captureLocallyEvaluatedTransaction,
  workflowTransactionInputOutRefs,
} from "../workflow/transaction-boundary.js";
import { createFieldItemWidthIllegalCentralJournalAdapter } from "./central-journal.js";
import {
  fieldItemWidthCoordinateIsSupported,
  type FieldItemWidthEvidence,
  fieldItemWidthIsIllegal,
  type FieldItemWidthJournal,
  runFieldItemWidthProof,
} from "./field-item-width-illegal.js";
import {
  FieldItemWidthStep02DatumSchema,
  FieldItemWidthStep03DatumSchema,
} from "./schemas.js";
import {
  createFieldItemWidthIllegalBoundConfig,
  createFieldItemWidthIllegalRawL1StageResolver,
  FIELD_ITEM_WIDTH_ILLEGAL_MANIFEST_CONTRACTS,
  FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID,
  FIELD_ITEM_WIDTH_ILLEGAL_WORKFLOW,
  type FieldItemWidthIllegalRuntimeLoader,
  type LoadManifestBoundFieldItemWidthIllegalConfig,
  type ManifestBoundFieldItemWidthIllegalConfig,
} from "./workflow.create-field-item-width-illegal-raw-l1-stage-resolver.js";
import { createManifestBoundFieldItemWidthIllegalSubmission } from "./workflow.create-manifest-bound-field-item-width-illegal-submission.js";
import {
  type FieldItemWidthIllegalAssemblyRuntime,
  fieldItemWidthStageFromL1,
} from "./workflow.derive-field-item-width-illegal-authenticated-source.js";
import { FIELD_ITEM_WIDTH_ILLEGAL_CURSOR_SPEC } from "./workflow-spec.js";

export const loadManifestBoundFieldItemWidthIllegalConfig = async (
  input: LoadManifestBoundFieldItemWidthIllegalConfig,
): Promise<ManifestBoundFieldItemWidthIllegalConfig> => {
  const binding = await bindFraudProofWorkflowDeployment({
    manifest: input.manifest,
    blueprintJson: input.blueprintJson,
    deploymentInfo: input.deploymentInfo,
    category: "fieldItemWidthIllegal",
    headerHash: input.headerHash,
    proverCredential: input.signer.paymentKeyHash,
    stepDatumSchemas:
      FIELD_ITEM_WIDTH_ILLEGAL_FAMILY_DEFINITION.stepDatumSchemas,
  });
  assertManifestBoundWorkflowSigner({
    network: binding.network,
    address: input.signer.address,
    paymentKeyHash: input.signer.paymentKeyHash,
  });

  return createFieldItemWidthIllegalBoundConfig(input, binding);
};

/** Complete accepted-block replay member: scans every field-2/field-5 item. */
export const detectFieldItemWidthIllegalCompleteReplay = (
  evidence: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] => {
  const accepted = evidence.transactions.flatMap(
    (transaction, transactionIndex) => {
      const material = deriveMidgardNativeTxFaultEvidenceMaterial(
        Buffer.from(transaction.txCbor, "hex"),
      );
      const transactionId = material.transactionId.toString("hex");
      if (transaction.nodeTxId !== transactionId) {
        throw new Error(
          "fieldItemWidthIllegal complete replay transaction identity changed",
        );
      }
      return ([2, 5] as const).flatMap((fieldIndex) =>
        decodeMidgardFieldPreimage(
          material.fieldPreimages[fieldIndex]!,
        ).flatMap((item, itemIndex) =>
          fieldItemWidthIsIllegal(fieldIndex, item.length)
            ? [
                {
                  ...acceptedTransactionSubject(transaction.nodeTxId),
                  detectionId: `${FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID}:${transactionIndex.toString()}:${transactionId}:${fieldIndex.toString()}:${itemIndex.toString()}:${item.length.toString()}`,
                  headerHash: evidence.headerHash,
                  violationId: FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID,
                  position: BigInt(transactionIndex),
                  diagnostic: `transaction ${transactionId} field ${fieldIndex.toString()} item ${itemIndex.toString()} has illegal width ${item.length.toString()}`,
                },
              ]
            : [],
        ),
      );
    },
  );
  const forced = evidence.reconstruction.forcedTransactions.flatMap(
    (transaction, forcedIndex) => {
      if (transaction.value.verdict === "ForcedTxValid") return [];
      const reason = transaction.value.verdict.ForcedTxInvalid.reason;
      if (typeof reason === "string" || !("FieldItemWidthIllegal" in reason)) {
        return [];
      }
      const material = deriveMidgardForcedTxFaultEvidenceMaterial(
        transaction.fullTransactionCbor,
      );
      if (
        material.transactionId.toString("hex") !== transaction.value.tx_id ||
        material.proofSource.compactCbor.toString("hex") !==
          transaction.value.submitted_source.compact_cbor ||
        material.proofSource.witnessSetCompactCbor.toString("hex") !==
          transaction.value.submitted_source.witness_set_compact_cbor ||
        material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
          transaction.value.submitted_source.field_preimage_lengths_cbor
      ) {
        throw new Error(
          "fieldItemWidthIllegal forced transaction differs from its authenticated leaf",
        );
      }
      const coordinate = reason.FieldItemWidthIllegal;
      const fieldIndex = Number(coordinate.field_index);
      const itemIndex = Number(coordinate.item_index);
      if (!fieldItemWidthCoordinateIsSupported(fieldIndex, itemIndex)) {
        return [];
      }
      const preimage = material.fieldPreimages[fieldIndex];
      const item =
        preimage === undefined
          ? undefined
          : decodeMidgardFieldPreimage(preimage)[itemIndex];
      if (
        item === undefined ||
        fieldItemWidthIsIllegal(fieldIndex, item.length)
      ) {
        return [];
      }
      return [
        {
          ...forcedTransactionSubject(transaction.key),
          detectionId: `${FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID}:forced:${forcedIndex.toString()}:${transaction.value.tx_id}:${fieldIndex.toString()}:${itemIndex.toString()}:${item.length.toString()}`,
          headerHash: evidence.headerHash,
          violationId: FIELD_ITEM_WIDTH_ILLEGAL_VIOLATION_ID,
          position: BigInt(forcedIndex),
          diagnostic: `forced transaction ${transaction.value.tx_id} was rejected for legal field ${fieldIndex.toString()} item ${itemIndex.toString()} width ${item.length.toString()}`,
        },
      ];
    },
  );
  return [...accepted, ...forced];
};

export const createManifestBoundFieldItemWidthIllegalRuntime = ({
  config,
  journal,
  observe,
  resolveStage,
  centralJournal,
  stateQueueMutationLeaseCoordinator,
}: {
  readonly config: ManifestBoundFieldItemWidthIllegalConfig;
  readonly journal: FieldItemWidthJournal;
  readonly observe: FieldItemWidthIllegalRuntimeLoader["observe"];
  readonly resolveStage: FieldItemWidthIllegalRuntimeLoader["resolveStage"];
  readonly centralJournal?: ReturnType<
    typeof createFieldItemWidthIllegalCentralJournalAdapter
  >;
  readonly stateQueueMutationLeaseCoordinator?: StateQueueMutationLeaseCoordinator;
}) => {
  const submission = createManifestBoundFieldItemWidthIllegalSubmission({
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
    runtimeVersion: FIELD_ITEM_WIDTH_ILLEGAL_WORKFLOW,
    config,
    runOrResume: async (evidence: FieldItemWidthEvidence) =>
      await runFieldItemWidthProof({
        evidence,
        journal,
        submission,
      }),
  });
};

export const FIELD_ITEM_WIDTH_ILLEGAL_FAMILY_DEFINITION = defineFamily<
  "fieldItemWidthIllegal",
  "computationThreadMint" | "fraudProofMint" | "phasMembershipWithdraw",
  true,
  3,
  FieldItemWidthIllegalAssemblyRuntime
>({
  category: "fieldItemWidthIllegal",
  stepDatumSchemas: [
    FraudProofComputationThreadStepDatum,
    FieldItemWidthStep02DatumSchema,
    FieldItemWidthStep03DatumSchema,
  ],
  witnessRoles: [
    "computationThreadMint",
    "fraudProofMint",
    "phasMembershipWithdraw",
  ],
  fieldPreimageCertificate: true,
  replayer: () => FIELD_ITEM_WIDTH_ILLEGAL_COMPLETE_CANONICAL_REPLAY,
  adapter: {
    kind: "cursor",
    spec: FIELD_ITEM_WIDTH_ILLEGAL_CURSOR_SPEC,
    stepContractNames: [
      FIELD_ITEM_WIDTH_ILLEGAL_MANIFEST_CONTRACTS.step01,
      FIELD_ITEM_WIDTH_ILLEGAL_MANIFEST_CONTRACTS.step02,
      FIELD_ITEM_WIDTH_ILLEGAL_MANIFEST_CONTRACTS.step03,
    ],
    transactionPort: (context) => {
      const category = "fieldItemWidthIllegal";
      const { config, material } = context.runtime;
      const { binding, l1, stateQueueMutationLeaseCoordinator } = context;
      return {
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
          } as const;
          const familyAction = actions[input.stage as keyof typeof actions];
          if (familyAction === undefined)
            throw new Error(
              `${category} cursor action is outside its exact topology`,
            );
          const transaction = await captureLocallyEvaluatedTransaction(
            async (preSubmitBoundary) => {
              const submission =
                createManifestBoundFieldItemWidthIllegalSubmission({
                  config,
                  preSubmitBoundary,
                  observe: async () =>
                    fieldItemWidthStageFromL1(
                      (
                        await l1.observe({
                          headerHash: binding.definition.headerHash,
                        })
                      ).stage,
                    ),
                  resolveStage: createFieldItemWidthIllegalRawL1StageResolver({
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
    },
  },
  fieldCarriage: [
    {
      requirementForAction: (context, { action, artifact }) => {
        const category = "fieldItemWidthIllegal";
        const { config, material } = context.runtime;
        const { binding } = context;
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
      },
    },
  ],
});
