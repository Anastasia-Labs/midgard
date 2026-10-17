import {
  deriveMidgardNativeTxFaultEvidenceMaterial,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { buildTrieView, requireProof } from "../prepare-double-spend.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import {
  buildObserverOrderInvalidArtifact,
  type ObserverOrderInvalidArtifact,
  ObserverOrderInvalidForcedSourcePayloadSchema,
} from "./artifact.js";
import {
  OBSERVER_ORDER_INVALID_CATEGORY,
  OBSERVER_ORDER_INVALID_FIELD_INDEX,
  prepareObserverOrderInvalidEvidence,
} from "./family.js";
import {
  detectObserverOrderInvalidAcceptedRawReplay,
  detectObserverOrderInvalidForcedReplay,
  OBSERVER_ORDER_INVALID_RAW_EVIDENCE,
  type ObserverOrderInvalidRawBlockEvidence,
  type ObserverOrderInvalidReplayDetection,
} from "./replay.observer-order-invalid-raw-block-evidence-from-verified-payload.js";

/**
 * Family-owned complete replay adapter for the closed production replay union.
 * It visits every accepted transaction and every forced transaction, then
 * emits detections in stable position/detection-id order.
 */
export const detectObserverOrderInvalidCompleteReplay = (
  block: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] => {
  const acceptedTransactions = block.transactions.map((transaction, index) => {
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      Buffer.from(transaction.txCbor, "hex"),
    );
    if (material.transactionId.toString("hex") !== transaction.nodeTxId)
      throw new Error(
        "observerOrderInvalid complete replay transaction identity changed",
      );
    return Object.freeze({
      index,
      nodeTxId: transaction.nodeTxId,
      l2TransactionSourceCbor: transaction.l2TransactionSourceCbor,
      fullTransactionCbor: transaction.txCbor,
      material,
    });
  });
  const accepted = detectObserverOrderInvalidAcceptedRawReplay({
    schemaVersion: OBSERVER_ORDER_INVALID_RAW_EVIDENCE,
    headerHash: block.headerHash,
    committedTransactionsRoot: block.header.transactionsRoot,
    l2TransactionCount: block.header.l2TransactionCount,
    transactionsPhasRoot: block.inclusionRootAuthentication.sourceValuePhasRoot,
    payloadEnvelopeSha256: block.payloadEnvelopeSha256,
    payloadSha256: block.payloadSha256,
    transactions: acceptedTransactions,
  });
  return Object.freeze(
    [...accepted, ...detectObserverOrderInvalidForcedReplay(block)].sort(
      (left, right) =>
        left.position === right.position
          ? left.detectionId.localeCompare(right.detectionId)
          : left.position < right.position
            ? -1
            : 1,
    ),
  );
};

export const selectCanonicalObserverOrderInvalidDetection = (
  detections: readonly ObserverOrderInvalidReplayDetection[],
): ObserverOrderInvalidReplayDetection => {
  if (detections.length === 0)
    throw new Error(
      `${OBSERVER_ORDER_INVALID_CATEGORY}: no authenticated detection`,
    );
  return [...detections].sort((left, right) =>
    left.position === right.position
      ? left.detectionId.localeCompare(right.detectionId)
      : left.position < right.position
        ? -1
        : 1,
  )[0]!;
};

export const observerOrderInvalidAcceptedMembership = async ({
  block,
  transactionId,
}: {
  readonly block: ObserverOrderInvalidRawBlockEvidence;
  readonly transactionId: string;
}): Promise<string> => {
  const entries = block.transactions.map((transaction) => ({
    key: Buffer.from(transaction.nodeTxId, "hex"),
    value: Buffer.from(transaction.l2TransactionSourceCbor, "hex"),
  }));
  return requireProof(
    await buildTrieView(entries),
    Buffer.from(transactionId, "hex"),
    "observerOrderInvalid accepted transaction",
  );
};

/** Reconstructs the selected accepted artifact without caller-prepared evidence. */
export const prepareObserverOrderInvalidAcceptedArtifact = async (
  block: ObserverOrderInvalidRawBlockEvidence,
): Promise<ObserverOrderInvalidArtifact> => {
  const detection = selectCanonicalObserverOrderInvalidDetection(
    detectObserverOrderInvalidAcceptedRawReplay(block),
  );
  const transaction = block.transactions[Number(detection.position)];
  if (
    transaction === undefined ||
    transaction.nodeTxId !== detection.transactionId
  )
    throw new Error(
      "observerOrderInvalid selected accepted transaction disappeared",
    );
  const field =
    transaction.material.fieldPreimages[OBSERVER_ORDER_INVALID_FIELD_INDEX];
  if (field === undefined)
    throw new Error("observerOrderInvalid selected field 3 disappeared");
  const evidence = prepareObserverOrderInvalidEvidence({
    finding: {
      subject: SDK.acceptedVerdictSubject(transaction.nodeTxId),
      observerIndex: detection.observerIndex,
    },
    fieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
  });
  return buildObserverOrderInvalidArtifact({
    headerHash: block.headerHash,
    detectionId: detection.detectionId,
    position: detection.position,
    evidence,
    nativeTxCompactCbor:
      transaction.material.proofSource.compactCbor.toString("hex"),
    witnessSetCompactCbor:
      transaction.material.proofSource.witnessSetCompactCbor.toString("hex"),
    l2TransactionSourceCbor: transaction.l2TransactionSourceCbor,
    transactionsPhasRoot: block.transactionsPhasRoot,
    transactionMembershipCbor: await observerOrderInvalidAcceptedMembership({
      block,
      transactionId: transaction.nodeTxId,
    }),
  });
};

/** Reconstructs the exact forced wrongful-rejection artifact from canonical replay. */
export const prepareObserverOrderInvalidForcedArtifact = async (
  block: CanonicalBlockEvidence,
): Promise<ObserverOrderInvalidArtifact> => {
  const detection = selectCanonicalObserverOrderInvalidDetection(
    detectObserverOrderInvalidForcedReplay(block),
  );
  const transaction =
    block.reconstruction.forcedTransactions[detection.forcedIndex!];
  if (transaction === undefined)
    throw new Error(
      "observerOrderInvalid selected forced transaction disappeared",
    );
  const verdict = transaction.value.verdict;
  if (verdict === "ForcedTxValid")
    throw new Error("observerOrderInvalid forced rejection changed verdict");
  const material = deriveMidgardForcedTxFaultEvidenceMaterial(
    transaction.fullTransactionCbor,
  );
  const field = material.fieldPreimages[OBSERVER_ORDER_INVALID_FIELD_INDEX];
  if (field === undefined)
    throw new Error("observerOrderInvalid forced field 3 disappeared");
  const eventKey = {
    ForcedTransactionEventKey: { tx_order_id: transaction.key },
  } as const;
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: block.reconstruction,
    eventKey,
  });
  const evidence = prepareObserverOrderInvalidEvidence({
    finding: {
      subject: SDK.forcedVerdictSubject({
        transactionId: transaction.value.tx_id,
        sourceKey: transaction.key,
        rejectionReason: verdict.ForcedTxInvalid.reason,
      }),
      observerIndex: detection.observerIndex,
    },
    fieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
  });
  const forcedSourceCbor = Data.to(
    { header: block.header, membership, direction: 1n } as never,
    ObserverOrderInvalidForcedSourcePayloadSchema as never,
  );
  return buildObserverOrderInvalidArtifact({
    headerHash: block.headerHash,
    detectionId: detection.detectionId,
    position: detection.position,
    evidence,
    sourceKind: "forced",
    nativeTxCompactCbor: material.proofSource.compactCbor.toString("hex"),
    witnessSetCompactCbor:
      material.proofSource.witnessSetCompactCbor.toString("hex"),
    l2TransactionSourceCbor: Data.to(
      {
        tx_id: transaction.value.tx_id,
        source: transaction.value.submitted_source,
      } as never,
      SDK.L2TransactionSource as never,
    ),
    transactionsPhasRoot: "00".repeat(32),
    transactionMembershipCbor: Data.to(
      membership as never,
      SDK.ForcedTransactionSourceMembershipProof as never,
    ),
    forcedSourceCbor,
  });
};
