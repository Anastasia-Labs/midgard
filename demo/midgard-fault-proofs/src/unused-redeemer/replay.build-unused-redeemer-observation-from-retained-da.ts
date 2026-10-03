import { createHash } from "node:crypto";

import {
  decodeMidgardRedeemerWitnessFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  hashMidgardScriptExecutionLeaf,
  hashMidgardScriptPurposeLeaf,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import { type EventKey } from "@al-ft/midgard-sdk";
import { projectMidgardRawEnvelopeForPhaseAV1 } from "@al-ft/midgard-validation";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import {
  acceptedTransactionSubject,
  forcedTransactionSubject,
} from "../workflow/detection-subject.js";
import {
  observeUnusedRedeemerSelection,
  UNUSED_REDEEMER_VIOLATION_ID,
} from "./family.js";
import { buildUnusedRedeemerControlFromRetainedDa } from "./retained-stage-twelve.js";

const exactIndex = (value: bigint, label: string): number => {
  const result = Number(value);
  if (!Number.isSafeInteger(result) || result < 0)
    throw new Error(`unusedRedeemer ${label} changed`);
  return result;
};

/** Classify the same authenticated retained selections consumed by actuation. */
export const detectUnusedRedeemerCanonicalViolations = async (
  block: CanonicalBlockEvidence,
): Promise<readonly CanonicalViolationDetection[]> => {
  const detections: CanonicalViolationDetection[] = [];
  for (const [position, transaction] of block.transactions.entries()) {
    const txCbor = Buffer.from(transaction.txCbor, "hex");
    try {
      projectMidgardRawEnvelopeForPhaseAV1(txCbor);
    } catch {
      continue;
    }
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(txCbor);
    const field = material.fieldPreimages[8];
    if (field === undefined) continue;
    const redeemers = decodeMidgardRedeemerWitnessFieldPreimage(field);
    for (const redeemerIndex of redeemers.keys()) {
      const { observation } =
        await buildUnusedRedeemerObservationFromRetainedDa({
          block,
          eventKey: { L2TransactionEventKey: { tx_id: transaction.nodeTxId } },
          transactionId: transaction.nodeTxId,
          redeemerIndex,
          txCbor,
        });
      if (!observation.unused) continue;
      detections.push({
        ...acceptedTransactionSubject(transaction.nodeTxId),
        detectionId: `${UNUSED_REDEEMER_VIOLATION_ID}:accepted:${position.toString()}:${transaction.nodeTxId}:${redeemerIndex.toString()}`,
        headerHash: block.headerHash,
        violationId: UNUSED_REDEEMER_VIOLATION_ID,
        position: BigInt(position),
        diagnostic: `accepted transaction retained unused redeemer ${redeemerIndex.toString()}`,
      });
    }
  }
  for (const [
    position,
    transaction,
  ] of block.reconstruction.forcedTransactions.entries()) {
    const verdict = transaction.value.verdict;
    if (
      verdict === "ForcedTxValid" ||
      typeof verdict.ForcedTxInvalid.reason === "string" ||
      !("UnusedRedeemer" in verdict.ForcedTxInvalid.reason)
    )
      continue;
    const redeemerIndex = exactIndex(
      verdict.ForcedTxInvalid.reason.UnusedRedeemer.redeemer_index,
      "forced reason coordinate",
    );
    const { observation } = await buildUnusedRedeemerObservationFromRetainedDa({
      block,
      eventKey: { ForcedTransactionEventKey: { tx_order_id: transaction.key } },
      transactionId: transaction.value.tx_id,
      redeemerIndex,
      txCbor: transaction.fullTransactionCbor,
    });
    if (observation.unused) continue;
    detections.push({
      ...forcedTransactionSubject(transaction.key),
      detectionId: `${UNUSED_REDEEMER_VIOLATION_ID}:forced:${position.toString()}:${transaction.value.tx_id}:${redeemerIndex.toString()}`,
      headerHash: block.headerHash,
      violationId: UNUSED_REDEEMER_VIOLATION_ID,
      position: BigInt(block.transactions.length + position),
      diagnostic: `forced rejection called selected redeemer ${redeemerIndex.toString()} unused`,
    });
  }
  return Object.freeze(
    detections.sort(
      (left, right) =>
        Number(left.position - right.position) ||
        left.detectionId.localeCompare(right.detectionId),
    ),
  );
};

const retainedEntries = (block: CanonicalBlockEvidence) => ({
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

/** Reconstruct a selected or unused pointer without assuming proof polarity. */
export const buildUnusedRedeemerObservationFromRetainedDa = async ({
  block,
  eventKey,
  transactionId,
  redeemerIndex,
  txCbor,
}: {
  block: CanonicalBlockEvidence;
  eventKey: EventKey;
  transactionId: string;
  redeemerIndex: number;
  txCbor: Buffer;
}) => {
  const { traces, witnesses } = retainedEntries(block);
  const base = await buildUnusedRedeemerControlFromRetainedDa({
    eventKey,
    transactionId,
    redeemerIndex,
    authenticatedValidationTraceEntries: traces,
    retainedValidationWitnessEntries: witnesses,
    expectedValidationTracesRoot: block.header.validationTracesRoot,
  });
  projectMidgardRawEnvelopeForPhaseAV1(
    txCbor,
    "ForcedTransactionEventKey" in eventKey ? "forced" : "normal",
  );
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
            ? hashMidgardInlineScriptSourceLeaf({
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
            : hashMidgardReferenceScriptSourceLeaf({
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
      const purposeLeaf = hashMidgardScriptPurposeLeaf({
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
          leafHash: hashMidgardScriptExecutionLeaf({
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
  const nativeMaterial = (
    "ForcedTransactionEventKey" in eventKey
      ? deriveMidgardForcedTxFaultEvidenceMaterial
      : deriveMidgardNativeTxFaultEvidenceMaterial
  )(txCbor);
  const fieldPreimage = nativeMaterial.fieldPreimages[8];
  if (fieldPreimage === undefined)
    throw new Error("unusedRedeemer transaction omitted field 8");
  if (BigInt(selections.length) !== base.control.purpose_count)
    throw new Error("unusedRedeemer retained execution frontier is incomplete");
  const universe = {
    schemaVersion: "midgard-committed-redeemer-universe-v1",
    transactionId,
    universeDigest,
    selections,
  } as const;
  const observation = observeUnusedRedeemerSelection({
    transactionId,
    redeemerIndex,
    fieldPreimage,
    universe,
  });
  if (observation.unused !== (base.selectedBit === 0n))
    throw new Error(
      "unusedRedeemer selection frontier differs from retained bitmap",
    );
  return Object.freeze({ base, fieldPreimage, universe, observation });
};
