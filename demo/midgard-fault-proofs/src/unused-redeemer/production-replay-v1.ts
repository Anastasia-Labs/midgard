import { createHash } from "node:crypto";

import {
  buildMidgardBoundedItemV1,
  decodeMidgardRedeemerWitnessFieldPreimageV1,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
  encodeMidgardRedeemerWitnessItemV1,
  hashMidgardInlineScriptSourceLeafV1,
  hashMidgardRedeemerItemLeafV1,
  hashMidgardReferenceScriptSourceLeafV1,
  hashMidgardScriptExecutionLeafV1,
  hashMidgardScriptPurposeLeafV1,
  hashMidgardValidationEventKeyV1,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  decodeRetainedValidationWitnessV1,
  type EventKey,
  EventKeySchema,
  forcedVerdictSubjectV1,
  type VerdictSubjectV1,
} from "@al-ft/midgard-sdk";
import { projectMidgardRawEnvelopeForPhaseAV1 } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import { buildTrieView, requireProof } from "../prepare-double-spend.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { CanonicalViolationDetectionV1 } from "../workflow/classification-v1.js";
import {
  prepareUnusedRedeemerEvidenceV1,
  UNUSED_REDEEMER_VIOLATION_ID_V1,
} from "./family-v1.js";
import type { UnusedRedeemerProductionArtifactV1 } from "./production-actuator-v1.js";
import { buildUnusedRedeemerDirectionControlFromRetainedDaV1 } from "./retained-stage-twelve-v1.js";
import type { UnusedRedeemerAuthenticationV1 } from "./submit-step-02-v1.js";

const exactIndex = (value: bigint, label: string): number => {
  const result = Number(value);
  if (!Number.isSafeInteger(result) || result < 0)
    throw new Error(`unusedRedeemer ${label} changed`);
  return result;
};

type SelectedRedeemer = Readonly<{
  transactionId: string;
  purposeTag: number;
  purposeIndex: number;
  redeemerLeafHex: string;
}>;

const selectedRedeemers = (
  block: CanonicalBlockEvidenceV1,
): readonly SelectedRedeemer[] =>
  block.reconstruction.payload.block_body.validation_trace_witnesses.flatMap(
    ([, encoded]) => {
      const retained = decodeRetainedValidationWitnessV1(
        Buffer.from(encoded, "hex"),
      );
      if (
        !(
          typeof retained.auxiliary === "object" &&
          "NativeExecutionDescriptorWitness" in retained.auxiliary
        )
      )
        return [];
      const descriptor = retained.auxiliary.NativeExecutionDescriptorWitness;
      return [
        {
          transactionId: retained.machine_state.transaction_id,
          purposeTag:
            [0, 1, 3, 6][exactIndex(descriptor.purpose_kind, "purpose kind")] ??
            -1,
          purposeIndex: exactIndex(descriptor.purpose_index, "purpose index"),
          redeemerLeafHex: descriptor.redeemer_leaf,
        },
      ];
    },
  );

/**
 * Complete canonical ID2f selection. Native execution descriptors identify
 * the first-precedence source actually selected for every purpose; therefore
 * every other inline field-6 coordinate is unused. A self-consistent forged
 * descriptor remains accountable through the validation-trace-invalid arm.
 */
export const detectUnusedRedeemerCanonicalViolationsV1 = async (
  block: CanonicalBlockEvidenceV1,
): Promise<readonly CanonicalViolationDetectionV1[]> => {
  const selected = selectedRedeemers(block);
  const detections: CanonicalViolationDetectionV1[] = [];
  block.transactions.forEach((transaction, position) => {
    try {
      projectMidgardRawEnvelopeForPhaseAV1(
        Buffer.from(transaction.txCbor, "hex"),
      );
    } catch {
      return;
    }
    const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
      Buffer.from(transaction.txCbor, "hex"),
    );
    const field = material.fieldPreimages[8];
    if (field === undefined) return;
    decodeMidgardRedeemerWitnessFieldPreimageV1(field).forEach(
      (redeemer, redeemerIndex) => {
        const itemBytes = encodeMidgardRedeemerWitnessItemV1(redeemer);
        const leaf = hashMidgardRedeemerItemLeafV1({
          redeemerIndex,
          itemCommitment: buildMidgardBoundedItemV1({
            fieldIndex: 8,
            itemIndex: redeemerIndex,
            bytes: itemBytes,
          }).commitment,
        }).toString("hex");
        const purposeTag =
          redeemer.purpose === "Spend"
            ? 0
            : redeemer.purpose === "Mint"
              ? 1
              : redeemer.purpose === "Reward"
                ? 3
                : redeemer.purpose === "Receive"
                  ? 6
                  : -1;
        const used = selected.some(
          (entry) =>
            entry.transactionId === transaction.nodeTxId &&
            entry.purposeTag === purposeTag &&
            entry.purposeIndex === Number(redeemer.index) &&
            entry.redeemerLeafHex === leaf,
        );
        if (used) return;
        detections.push({
          detectionId: `${UNUSED_REDEEMER_VIOLATION_ID_V1}:accepted:${position.toString()}:${transaction.nodeTxId}:${redeemerIndex.toString()}`,
          headerHash: block.headerHash,
          violationId: UNUSED_REDEEMER_VIOLATION_ID_V1,
          position: BigInt(position),
          diagnostic: `accepted transaction retained unused script witness ${redeemerIndex.toString()}`,
        });
      },
    );
  });
  block.reconstruction.forcedTransactions.forEach((transaction, position) => {
    const verdict = transaction.value.verdict;
    if (
      verdict === "ForcedTxValid" ||
      typeof verdict.ForcedTxInvalid.reason === "string" ||
      !("UnusedRedeemer" in verdict.ForcedTxInvalid.reason)
    )
      return;
    const redeemerIndex = exactIndex(
      verdict.ForcedTxInvalid.reason.UnusedRedeemer.redeemer_index,
      "forced reason coordinate",
    );
    const forcedMaterial = deriveMidgardNativeTxFaultEvidenceMaterialV1(
      transaction.fullTransactionCbor,
    );
    const forcedField = forcedMaterial.fieldPreimages[8];
    if (forcedField === undefined) return;
    const forcedRedeemer =
      decodeMidgardRedeemerWitnessFieldPreimageV1(forcedField)[redeemerIndex];
    if (forcedRedeemer === undefined) return;
    const forcedItem = encodeMidgardRedeemerWitnessItemV1(forcedRedeemer);
    const forcedLeaf = hashMidgardRedeemerItemLeafV1({
      redeemerIndex,
      itemCommitment: buildMidgardBoundedItemV1({
        fieldIndex: 8,
        itemIndex: redeemerIndex,
        bytes: forcedItem,
      }).commitment,
    }).toString("hex");
    const forcedPurposeTag =
      forcedRedeemer.purpose === "Spend"
        ? 0
        : forcedRedeemer.purpose === "Mint"
          ? 1
          : forcedRedeemer.purpose === "Reward"
            ? 3
            : forcedRedeemer.purpose === "Receive"
              ? 6
              : -1;
    if (
      !selected.some(
        (entry) =>
          entry.transactionId === transaction.value.tx_id &&
          entry.purposeTag === forcedPurposeTag &&
          entry.purposeIndex === Number(forcedRedeemer.index) &&
          entry.redeemerLeafHex === forcedLeaf,
      )
    )
      return;
    detections.push({
      detectionId: `${UNUSED_REDEEMER_VIOLATION_ID_V1}:forced:${position.toString()}:${transaction.value.tx_id}:${redeemerIndex.toString()}`,
      headerHash: block.headerHash,
      violationId: UNUSED_REDEEMER_VIOLATION_ID_V1,
      position: BigInt(block.transactions.length + position),
      diagnostic: `forced rejection called selected script witness ${redeemerIndex.toString()} unused`,
    });
  });
  return Object.freeze(
    detections.sort(
      (left, right) =>
        Number(left.position - right.position) ||
        left.detectionId.localeCompare(right.detectionId),
    ),
  );
};

const retainedEntries = (block: CanonicalBlockEvidenceV1) => ({
  traces: block.reconstruction.payload.block_body.validation_traces.map(
    ([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    }),
  ),
  witnesses:
    block.reconstruction.payload.block_body.validation_trace_witnesses.map(
      ([key, value]) => ({
        key: Buffer.from(key, "hex"),
        value: Buffer.from(value, "hex"),
      }),
    ),
});

export const buildUnusedRedeemerMaterialFromRetainedDaV1 = async ({
  block,
  eventKey,
  subject,
  redeemerIndex,
  txCbor,
}: {
  block: CanonicalBlockEvidenceV1;
  eventKey: EventKey;
  subject: VerdictSubjectV1;
  redeemerIndex: number;
  txCbor: Buffer;
}) => {
  const { traces, witnesses } = retainedEntries(block);
  if (subject.direction !== 0n && subject.direction !== 1n)
    throw new Error("unusedRedeemer direction changed");
  const base = await buildUnusedRedeemerDirectionControlFromRetainedDaV1({
    eventKey,
    transactionId: subject.transaction_id,
    direction: subject.direction,
    redeemerIndex,
    authenticatedValidationTraceEntries: traces,
    retainedValidationWitnessEntries: witnesses,
    expectedValidationTracesRoot: block.header.validationTracesRoot,
  });
  projectMidgardRawEnvelopeForPhaseAV1(txCbor);
  const selections = base.executions
    .map((execution) => {
      const frontierIndex = exactIndex(
        execution.execution_index,
        "execution index",
      );
      const purposeKind = exactIndex(execution.purpose_kind, "purpose kind");
      if (
        purposeKind !== 0 &&
        purposeKind !== 1 &&
        purposeKind !== 2 &&
        purposeKind !== 3
      )
        throw new Error("unusedRedeemer retained purpose kind changed");
      const languageTag = exactIndex(execution.language_tag, "language tag");
      if (languageTag !== 0 && languageTag !== 3 && languageTag !== 128)
        throw new Error("unusedRedeemer retained language tag changed");
      const sourceLeaf =
        "source_leaf" in execution
          ? Buffer.from(execution.source_leaf, "hex")
          : execution.origin_kind === 0n
            ? hashMidgardInlineScriptSourceLeafV1({
                sourceIndex: execution.source_index,
                scriptLanguageTag: languageTag,
                scriptHash: Buffer.from(execution.script_hash, "hex"),
                scriptTotalLength: exactIndex(
                  execution.script_total_length,
                  "script length",
                ),
                itemCommitment: Buffer.from(
                  execution.script_item_commitment,
                  "hex",
                ),
              })
            : hashMidgardReferenceScriptSourceLeafV1({
                sourceKey: Buffer.from(execution.source_key, "hex"),
                scriptLanguageTag: languageTag,
                scriptHash: Buffer.from(execution.script_hash, "hex"),
                scriptTotalLength: exactIndex(
                  execution.script_total_length,
                  "script length",
                ),
                itemCommitment: Buffer.from(
                  execution.script_item_commitment,
                  "hex",
                ),
              });
      const purposeLeaf = hashMidgardScriptPurposeLeafV1({
        purposeKind,
        purposeIndex: execution.purpose_index,
        scriptHash: Buffer.from(execution.script_hash, "hex"),
        subject: Buffer.from(execution.subject, "hex"),
      });
      const purposeFrontier = {
        count: exactIndex(base.control.purpose_count, "purpose count"),
        peaks: base.control.purpose_peaks.map(({ height, hash }) => ({
          height: exactIndex(height, "purpose peak height"),
          hash: Buffer.from(hash, "hex"),
        })),
      };
      const executionFrontier = {
        count: exactIndex(
          base.control.discovery.execution_count,
          "execution count",
        ),
        peaks: base.control.discovery.execution_peaks.map(
          ({ height, hash }) => ({
            height: exactIndex(height, "execution peak height"),
            hash: Buffer.from(hash, "hex"),
          }),
        ),
      };
      return {
        frontierIndex,
        purposeKind,
        purposeIndex: exactIndex(execution.purpose_index, "purpose index"),
        scriptHashHex: execution.script_hash,
        purposeSubjectHex: execution.subject,
        purposeMembership: {
          frontier: purposeFrontier,
          leafIndex: frontierIndex,
          leafHash: purposeLeaf,
          siblings: execution.purpose_siblings.map((value) =>
            Buffer.from(value, "hex"),
          ),
        },
        languageTag,
        sourceLeafHex: sourceLeaf.toString("hex"),
        redeemerLeafHex: execution.redeemer_leaf,
        executionMembership: {
          frontier: executionFrontier,
          leafIndex: frontierIndex,
          leafHash: hashMidgardScriptExecutionLeafV1({
            languageTag,
            purposeLeaf,
            sourceLeaf,
            redeemerLeaf: Buffer.from(execution.redeemer_leaf, "hex"),
          }),
          siblings: execution.execution_siblings.map((value) =>
            Buffer.from(value, "hex"),
          ),
        },
      } as const;
    })
    .sort((left, right) => left.frontierIndex - right.frontierIndex);
  const universeDigest = createHash("sha256")
    .update(
      Buffer.concat(
        selections.flatMap((selection) => [
          Buffer.from(selection.purposeMembership.leafHash),
          Buffer.from(selection.executionMembership.leafHash),
        ]),
      ),
    )
    .digest("hex");
  const nativeMaterial = deriveMidgardNativeTxFaultEvidenceMaterialV1(txCbor);
  const fieldPreimage = nativeMaterial.fieldPreimages[8];
  if (fieldPreimage === undefined)
    throw new Error("unusedRedeemer transaction omitted field 8");
  const evidence = prepareUnusedRedeemerEvidenceV1({
    finding: { subject, redeemerIndex },
    fieldPreimage,
    universe: {
      schemaVersion: "midgard-committed-redeemer-universe-v1",
      transactionId: subject.transaction_id,
      universeDigest,
      selections,
    },
  });
  const headerStep = base.itemSteps.find((step) => step.control.stage === 0n);
  const tailStep = base.itemSteps.find((step) => step.control.stage === 1n);
  if (headerStep === undefined || tailStep === undefined)
    throw new Error("unusedRedeemer retained item proof steps are incomplete");
  if (
    headerStep.witness.chunk_proof === null ||
    tailStep.witness.chunk_proof === null
  )
    throw new Error("unusedRedeemer retained item chunks are incomplete");
  const eventKeyCbor = Buffer.from(
    Data.to(eventKey as never, EventKeySchema),
    "hex",
  );
  const bound = {
    subject,
    validation_traces_root: block.header.validationTracesRoot,
    validation_trace_count: BigInt(base.traceMembership.count),
    redeemer_index: BigInt(redeemerIndex),
  };
  const descriptorState = {
    bound,
    event_key_hash:
      hashMidgardValidationEventKeyV1(eventKeyCbor).toString("hex"),
    descriptor: base.traceMembership.value,
  };
  const controlState = {
    bound,
    program_counter: base.machineState.program_counter,
    stage: base.control.stage,
    expected_item_control_hash:
      base.control.discovery.redeemer_item_control_hash,
    used_redeemer_bitmap: base.control.discovery.used_redeemer_bitmap,
    current_purpose_kind: base.control.discovery.current_purpose_kind,
    current_purpose_index: base.control.discovery.current_purpose_index,
    redeemer_count: base.control.redeemer_count,
    purpose_count: base.control.purpose_count,
    purpose_peaks: base.control.purpose_peaks,
    execution_count: base.control.discovery.execution_count,
    execution_peaks: base.control.discovery.execution_peaks,
  };
  const item = tailStep.control;
  const headerState = {
    authenticated: controlState,
    item_index: item.item_index,
    item_count: item.item_count,
    total_length: item.total_length,
    item_commitment: item.item_commitment,
    purpose_tag: item.purpose_tag,
    pointer_index: item.pointer_index,
    data_offset: item.data_offset,
    data_length: item.data_length,
  };
  const authenticatedState = {
    bound,
    purpose_tag: item.purpose_tag,
    pointer_index: item.pointer_index,
    item_count: item.item_count,
    item_length: item.total_length,
    item_commitment: item.item_commitment,
    redeemer_leaf: evidence.targetRedeemerLeafHex,
    purpose_count: controlState.purpose_count,
    purpose_peaks: controlState.purpose_peaks,
    execution_count: controlState.execution_count,
    execution_peaks: controlState.execution_peaks,
  };
  const authentication = {
    traceMembership: base.traceMembership,
    machineState: base.machineState,
    traceProof: base.traceProof,
    control: base.control,
    itemControl: headerStep.control,
    headerChunkProof: headerStep.witness.chunk_proof,
    headerNextChunkProof: headerStep.witness.next_chunk_proof,
    tailChunkProof: tailStep.witness.chunk_proof,
    tailNextChunkProof: tailStep.witness.next_chunk_proof,
    descriptorState,
    controlState,
    headerState,
    authenticatedState,
  } satisfies UnusedRedeemerAuthenticationV1;
  return { evidence, authentication };
};

/** Exact retained-DA artifact for the first canonical ID2f detection. */
export const prepareProductionUnusedRedeemerArtifactV1 = async (
  block: CanonicalBlockEvidenceV1,
): Promise<UnusedRedeemerProductionArtifactV1> => {
  const detection = (await detectUnusedRedeemerCanonicalViolationsV1(block))[0];
  if (detection === undefined)
    throw new Error("unusedRedeemer canonical replay yielded no contradiction");
  const [, sourceKind, positionText, transactionId, redeemerIndexText] =
    detection.detectionId.split(":");
  const position = Number(positionText);
  const redeemerIndex = Number(redeemerIndexText);
  if (!Number.isSafeInteger(position) || !Number.isSafeInteger(redeemerIndex))
    throw new Error("unusedRedeemer detection coordinate changed");
  if (sourceKind === "accepted") {
    const transaction = block.transactions[position];
    if (transaction === undefined || transaction.nodeTxId !== transactionId)
      throw new Error("unusedRedeemer accepted transaction disappeared");
    const eventKey = {
      L2TransactionEventKey: { tx_id: transactionId! },
    } as const;
    const material = await buildUnusedRedeemerMaterialFromRetainedDaV1({
      block,
      eventKey,
      subject: acceptedVerdictSubjectV1(transactionId!),
      redeemerIndex,
      txCbor: Buffer.from(transaction.txCbor, "hex"),
    });
    const trie = await buildTrieView(
      block.transactions.map((entry) => ({
        key: Buffer.from(entry.nodeTxId, "hex"),
        value: Buffer.from(entry.l2TransactionSourceCbor, "hex"),
      })),
    );
    const native = deriveMidgardNativeTxFaultEvidenceMaterialV1(
      Buffer.from(transaction.txCbor, "hex"),
    );
    return Object.freeze({
      headerHash: block.headerHash,
      header: block.header,
      ...material,
      acceptedInclusion: parseSubmitStep01TxInclusion({
        nativeTxId: transactionId!,
        nativeTx: nativeTxFromCoreCompact(native.compact),
        nativeTxCompactCbor: native.proofSource.compactCbor.toString("hex"),
        l2TransactionSourceCbor: transaction.l2TransactionSourceCbor,
        transactionsPhasRoot: trie.root,
        txMembershipProofCbor: requireProof(
          trie,
          Buffer.from(transactionId!, "hex"),
          "unused-redeemer transaction",
        ),
      }),
    });
  }
  if (sourceKind !== "forced")
    throw new Error("unusedRedeemer detection source changed");
  const forcedPosition = position - block.transactions.length;
  const transaction = block.reconstruction.forcedTransactions[forcedPosition];
  if (transaction === undefined || transaction.value.tx_id !== transactionId)
    throw new Error("unusedRedeemer forced transaction disappeared");
  const verdict = transaction.value.verdict;
  if (verdict === "ForcedTxValid")
    throw new Error("unusedRedeemer forced detection became valid");
  const eventKey = {
    ForcedTransactionEventKey: { tx_order_id: transaction.key },
  } as const;
  const material = await buildUnusedRedeemerMaterialFromRetainedDaV1({
    block,
    eventKey,
    subject: forcedVerdictSubjectV1({
      transactionId: transaction.value.tx_id,
      sourceKey: transaction.key,
      rejectionReason: verdict.ForcedTxInvalid.reason,
    }),
    redeemerIndex,
    txCbor: transaction.fullTransactionCbor,
  });
  return Object.freeze({
    headerHash: block.headerHash,
    header: block.header,
    ...material,
    forcedMembership: await buildForcedTransactionLeafMembershipProof({
      reconstruction: block.reconstruction,
      eventKey,
    }),
  });
};
