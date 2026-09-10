import { createHash } from "node:crypto";

import {
  decodeMidgardRedeemerWitnessFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  hashMidgardInlineScriptSourceLeaf,
  hashMidgardReferenceScriptSourceLeaf,
  hashMidgardScriptExecutionLeaf,
  hashMidgardScriptPurposeLeaf,
  hashMidgardValidationEventKey,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  type EventKey,
  EventKeySchema,
  forcedVerdictSubject,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";
import { projectMidgardRawEnvelopeForPhaseAV1 } from "@al-ft/midgard-validation";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { buildTrieView, requireProof } from "../prepare-double-spend.js";
import {
  nativeTxFromCoreCompact,
  parseSubmitStep01TxInclusion,
} from "../submit-step-01.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import type { UnusedRedeemerArtifact } from "./actuator.js";
import {
  observeUnusedRedeemerSelection,
  prepareUnusedRedeemerEvidence,
  UNUSED_REDEEMER_VIOLATION_ID,
} from "./family.js";
import { buildUnusedRedeemerControlFromRetainedDa } from "./retained-stage-twelve.js";
import type { UnusedRedeemerAuthentication } from "./submit-step-02.js";

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
  const nativeMaterial = deriveMidgardNativeTxFaultEvidenceMaterial(txCbor);
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

export const buildUnusedRedeemerMaterialFromRetainedDa = async ({
  block,
  eventKey,
  subject,
  redeemerIndex,
  txCbor,
}: {
  block: CanonicalBlockEvidence;
  eventKey: EventKey;
  subject: VerdictSubject;
  redeemerIndex: number;
  txCbor: Buffer;
}) => {
  const { base, fieldPreimage, universe } =
    await buildUnusedRedeemerObservationFromRetainedDa({
      block,
      eventKey,
      transactionId: subject.transaction_id,
      redeemerIndex,
      txCbor,
    });
  const evidence = prepareUnusedRedeemerEvidence({
    finding: { subject, redeemerIndex },
    fieldPreimage,
    universe,
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
    event_key_hash: hashMidgardValidationEventKey(eventKeyCbor).toString("hex"),
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
  } satisfies UnusedRedeemerAuthentication;
  return { evidence, authentication };
};

/** Exact retained-DA artifact for the first canonical ID2f detection. */
export const prepareUnusedRedeemerArtifact = async (
  block: CanonicalBlockEvidence,
): Promise<UnusedRedeemerArtifact> => {
  const detection = (await detectUnusedRedeemerCanonicalViolations(block))[0];
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
    const material = await buildUnusedRedeemerMaterialFromRetainedDa({
      block,
      eventKey,
      subject: acceptedVerdictSubject(transactionId!),
      redeemerIndex,
      txCbor: Buffer.from(transaction.txCbor, "hex"),
    });
    const trie = await buildTrieView(
      block.transactions.map((entry) => ({
        key: Buffer.from(entry.nodeTxId, "hex"),
        value: Buffer.from(entry.l2TransactionSourceCbor, "hex"),
      })),
    );
    const native = deriveMidgardNativeTxFaultEvidenceMaterial(
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
  const transaction = block.reconstruction.forcedTransactions[position];
  if (transaction === undefined || transaction.value.tx_id !== transactionId)
    throw new Error("unusedRedeemer forced transaction disappeared");
  const verdict = transaction.value.verdict;
  if (verdict === "ForcedTxValid")
    throw new Error("unusedRedeemer forced detection became valid");
  const eventKey = {
    ForcedTransactionEventKey: { tx_order_id: transaction.key },
  } as const;
  const material = await buildUnusedRedeemerMaterialFromRetainedDa({
    block,
    eventKey,
    subject: forcedVerdictSubject({
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
