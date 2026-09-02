import {
  adjudicateMidgardNativeTxFullV1Validity,
  decodeMidgardFieldPreimageV1,
  decodeMidgardNativeTxFullV1FromCanonicalCbor,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
  encodeMidgardNativeTxCanonicalV1,
  midgardFieldCommitmentV1,
  type MidgardNativeTxFaultEvidenceMaterialV1,
} from "@al-ft/midgard-core";
import { unwrapDaPayloadV1 } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS_V1 } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidenceV1 } from "../evidence/canonical-block-evidence-v1.js";
import { daHashPreimageBlockEvidenceFromVerifiedPayloadV1 } from "../prepare-da-hash-preimage.js";
import {
  buildTrieView,
  requireProof,
  requireTransactionsRootMatchV1,
} from "../prepare-double-spend.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import type { CanonicalViolationDetectionV1 } from "../workflow/classification-v1.js";
import {
  OBSERVER_ORDER_INVALID_CATEGORY_V1,
  OBSERVER_ORDER_INVALID_FIELD_INDEX_V1,
  observerOrderInvalidEvidenceClosesV1,
  type ObserverOrderInvalidEvidenceV1,
  prepareObserverOrderInvalidEvidenceV1,
} from "./family-v1.js";
import {
  buildProductionObserverOrderInvalidArtifactV1,
  ObserverOrderInvalidForcedSourcePayloadV1Schema,
  type ProductionObserverOrderInvalidArtifactV1,
} from "./production-artifact-v1.js";

export const OBSERVER_ORDER_INVALID_VIOLATION_ID_V1 =
  "observer-order-invalid" as const;
export const OBSERVER_ORDER_INVALID_RAW_EVIDENCE_V1 =
  "midgard-observer-order-invalid-raw-evidence-v1" as const;

export type ObserverOrderInvalidReplayDetectionV1 = Readonly<{
  detectionId: string;
  headerHash: string;
  violationId: typeof OBSERVER_ORDER_INVALID_VIOLATION_ID_V1;
  position: bigint;
  transactionId: string;
  observerIndex: number;
  source: "accepted" | "forced";
  direction: "wrongfulAcceptance" | "wrongfulRejection";
  forcedIndex?: number;
}>;

export type AuthenticatedObserverOrderInvalidRawTransactionV1 = Readonly<{
  index: number;
  nodeTxId: string;
  l2TransactionSourceCbor: string;
  fullTransactionCbor: string;
  material: MidgardNativeTxFaultEvidenceMaterialV1;
}>;

/**
 * L1/root authenticated envelope view intentionally constructed before strict
 * CanonicalBlockEvidence. The accepted machine error is observable from the
 * canonical field-3 observer bytes at the first offending adjacent pair.
 */
export type ObserverOrderInvalidRawBlockEvidenceV1 = Readonly<{
  schemaVersion: typeof OBSERVER_ORDER_INVALID_RAW_EVIDENCE_V1;
  headerHash: string;
  committedTransactionsRoot: string;
  l2TransactionCount: bigint;
  transactionsPhasRoot: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
  transactions: readonly AuthenticatedObserverOrderInvalidRawTransactionV1[];
}>;

const decodeSource = (
  cbor: string,
  index: number,
): SDK.L2TransactionSourceV1 => {
  let source: SDK.L2TransactionSourceV1;
  try {
    source = Data.from(cbor, SDK.L2TransactionSourceV1);
  } catch (cause) {
    throw new Error(
      `observerOrderInvalid transactions[${index.toString()}] source does not decode: ${String(cause)}`,
    );
  }
  if (Data.to(source, SDK.L2TransactionSourceV1) !== cbor) {
    throw new Error(
      `observerOrderInvalid transactions[${index.toString()}] source is not canonical Data`,
    );
  }
  return source;
};

export const observerOrderInvalidRawBlockEvidenceFromVerifiedPayloadV1 =
  async ({
    observation,
    payloadEnvelopeCbor,
    daProvenance,
    minimumConfirmationDepth,
  }: {
    readonly observation: SDK.AuthenticatedStateQueueHeaderObservationV1;
    readonly payloadEnvelopeCbor: Uint8Array;
    readonly daProvenance: SDK.EvidenceProvenanceV1;
    readonly minimumConfirmationDepth?: number;
  }): Promise<ObserverOrderInvalidRawBlockEvidenceV1> => {
    const raw = await daHashPreimageBlockEvidenceFromVerifiedPayloadV1({
      observation,
      payloadEnvelopeCbor,
      daProvenance,
      ...(minimumConfirmationDepth === undefined
        ? {}
        : { minimumConfirmationDepth }),
    });
    const payloadCbor = Buffer.from(
      (
        await unwrapDaPayloadV1(payloadEnvelopeCbor, {
          maxPayloadBytes: DA_TRANSPORT_LIMITS_V1.maxPayloadBytes,
        })
      ).innerBytes,
    );
    const payload = SDK.decodeDaPayloadV1(payloadCbor);
    if (!SDK.encodeDaPayloadV1(payload).equals(payloadCbor))
      throw new Error("observerOrderInvalid DA payload is not canonical");

    const entries = raw.entries.map(([key, value]) => ({
      key: Buffer.from(key, "hex"),
      value: Buffer.from(value, "hex"),
    }));
    const trie = await buildTrieView(entries);
    await requireTransactionsRootMatchV1({
      sourceRoot: trie.root,
      expectedTransactionsRoot: raw.committedTransactionsRoot,
      count: raw.l2TransactionCount,
    });

    const preimages = new Map(payload.block_body.transaction_preimages);
    if (preimages.size !== payload.block_body.transaction_preimages.length)
      throw new Error(
        "observerOrderInvalid transaction preimages are duplicated",
      );
    const transactions = raw.entries.map(([key, sourceCbor], index) => {
      const txCbor = preimages.get(key);
      if (txCbor === undefined)
        throw new Error(
          `observerOrderInvalid transaction preimage omitted ${key}`,
        );
      const source = decodeSource(sourceCbor, index);
      const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
        Buffer.from(txCbor, "hex"),
      );
      if (
        source.tx_id !== key ||
        material.transactionId.toString("hex") !== key ||
        source.source.compact_cbor !==
          material.proofSource.compactCbor.toString("hex") ||
        source.source.witness_set_compact_cbor !==
          material.proofSource.witnessSetCompactCbor.toString("hex") ||
        source.source.field_preimage_lengths_cbor !==
          material.proofSource.fieldPreimageLengthsCbor.toString("hex")
      )
        throw new Error(
          `observerOrderInvalid transaction ${key} differs from its committed source`,
        );
      return Object.freeze({
        index,
        nodeTxId: key,
        l2TransactionSourceCbor: sourceCbor,
        fullTransactionCbor: txCbor,
        material,
      });
    });
    if (preimages.size !== transactions.length)
      throw new Error(
        "observerOrderInvalid has uncommitted transaction preimages",
      );
    return Object.freeze({
      schemaVersion: OBSERVER_ORDER_INVALID_RAW_EVIDENCE_V1,
      headerHash: raw.headerHash,
      committedTransactionsRoot: raw.committedTransactionsRoot,
      l2TransactionCount: raw.l2TransactionCount,
      transactionsPhasRoot: trie.root,
      payloadEnvelopeSha256: raw.payloadEnvelopeSha256,
      payloadSha256: raw.payloadSha256,
      transactions: Object.freeze(transactions),
    });
  };

const acceptedEvidence = (
  transaction: AuthenticatedObserverOrderInvalidRawTransactionV1,
  observerIndex: number,
): ObserverOrderInvalidEvidenceV1 | null => {
  if (transaction.material.canonical.validity !== "TxIsValid") return null;
  const field =
    transaction.material.fieldPreimages[OBSERVER_ORDER_INVALID_FIELD_INDEX_V1];
  if (field === undefined) return null;
  try {
    const evidence = prepareObserverOrderInvalidEvidenceV1({
      finding: {
        subject: SDK.acceptedVerdictSubjectV1(transaction.nodeTxId),
        observerIndex,
      },
      fieldPreimage: field,
      committedFieldHashHex: midgardFieldCommitmentV1(field).toString("hex"),
    });
    return observerOrderInvalidEvidenceClosesV1(evidence) ? evidence : null;
  } catch {
    return null;
  }
};

/** Complete accepted scan in machine order over every authenticated coordinate. */
export const detectObserverOrderInvalidAcceptedRawReplayV1 = (
  block: ObserverOrderInvalidRawBlockEvidenceV1,
): readonly ObserverOrderInvalidReplayDetectionV1[] => {
  const detections: ObserverOrderInvalidReplayDetectionV1[] = [];
  for (const transaction of block.transactions) {
    const field =
      transaction.material.fieldPreimages[
        OBSERVER_ORDER_INVALID_FIELD_INDEX_V1
      ];
    if (field === undefined) continue;
    let itemCount: number;
    try {
      itemCount = decodeMidgardFieldPreimageV1(field).length;
    } catch {
      // Field-envelope failures belong to the earlier decoding families.
      continue;
    }
    for (let observerIndex = 1; observerIndex < itemCount; observerIndex += 1) {
      const evidence = acceptedEvidence(transaction, observerIndex);
      if (evidence === null) continue;
      detections.push(
        Object.freeze({
          detectionId: `${OBSERVER_ORDER_INVALID_VIOLATION_ID_V1}:accepted:${transaction.index.toString()}:${transaction.nodeTxId}:${observerIndex.toString()}`,
          headerHash: block.headerHash,
          violationId: OBSERVER_ORDER_INVALID_VIOLATION_ID_V1,
          position: BigInt(transaction.index),
          transactionId: transaction.nodeTxId,
          observerIndex,
          source: "accepted",
          direction: "wrongfulAcceptance",
        }),
      );
    }
  }
  return Object.freeze(detections);
};

/** Complete canonical scan of exact wrongful-rejection contradictions. */
export const detectObserverOrderInvalidForcedReplayV1 = (
  block: CanonicalBlockEvidenceV1,
): readonly ObserverOrderInvalidReplayDetectionV1[] => {
  const detections: ObserverOrderInvalidReplayDetectionV1[] = [];
  block.reconstruction.forcedTransactions.forEach(
    (transaction, forcedIndex) => {
      const verdict = transaction.value.verdict;
      if (verdict === "ForcedTxValid") return;
      const reason = verdict.ForcedTxInvalid.reason;
      if (typeof reason === "string" || !("ObserverOrderInvalid" in reason))
        return;
      const observerIndex = Number(reason.ObserverOrderInvalid.observer_index);
      const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
        transaction.fullTransactionCbor,
      );
      if (
        material.transactionId.toString("hex") !== transaction.value.tx_id ||
        material.proofSource.compactCbor.toString("hex") !==
          transaction.value.source.compact_cbor ||
        material.proofSource.witnessSetCompactCbor.toString("hex") !==
          transaction.value.source.witness_set_compact_cbor ||
        material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
          transaction.value.source.field_preimage_lengths_cbor
      )
        throw new Error(
          "observerOrderInvalid forced transaction differs from its authenticated leaf",
        );
      const field =
        material.fieldPreimages[OBSERVER_ORDER_INVALID_FIELD_INDEX_V1];
      if (field === undefined) return;
      const evidence = prepareObserverOrderInvalidEvidenceV1({
        finding: {
          subject: SDK.forcedVerdictSubjectV1({
            transactionId: transaction.value.tx_id,
            sourceKey: transaction.key,
            rejectionReason: reason,
          }),
          observerIndex,
        },
        fieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitmentV1(field).toString("hex"),
      });
      if (!observerOrderInvalidEvidenceClosesV1(evidence)) return;
      detections.push(
        Object.freeze({
          detectionId: `${OBSERVER_ORDER_INVALID_VIOLATION_ID_V1}:forced:${forcedIndex.toString()}:${transaction.value.tx_id}:${observerIndex.toString()}`,
          headerHash: block.headerHash,
          violationId: OBSERVER_ORDER_INVALID_VIOLATION_ID_V1,
          position: BigInt(forcedIndex),
          transactionId: transaction.value.tx_id,
          observerIndex,
          source: "forced",
          direction: "wrongfulRejection",
          forcedIndex,
        }),
      );
    },
  );
  return Object.freeze(detections);
};

/**
 * Family-owned complete replay adapter for the closed production replay union.
 * It visits every accepted transaction and every forced transaction, then
 * emits detections in stable position/detection-id order.
 */
export const detectObserverOrderInvalidCompleteReplayV1 = (
  block: CanonicalBlockEvidenceV1,
): readonly CanonicalViolationDetectionV1[] => {
  const acceptedTransactions = block.transactions.map((transaction, index) => {
    const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
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
  const accepted = detectObserverOrderInvalidAcceptedRawReplayV1({
    schemaVersion: OBSERVER_ORDER_INVALID_RAW_EVIDENCE_V1,
    headerHash: block.headerHash,
    committedTransactionsRoot: block.header.transactionsRoot,
    l2TransactionCount: block.header.l2TransactionCount,
    transactionsPhasRoot: block.inclusionRootAuthentication.sourceValuePhasRoot,
    payloadEnvelopeSha256: block.payloadEnvelopeSha256,
    payloadSha256: block.payloadSha256,
    transactions: acceptedTransactions,
  });
  return Object.freeze(
    [...accepted, ...detectObserverOrderInvalidForcedReplayV1(block)].sort(
      (left, right) =>
        left.position === right.position
          ? left.detectionId.localeCompare(right.detectionId)
          : left.position < right.position
            ? -1
            : 1,
    ),
  );
};

export const selectCanonicalObserverOrderInvalidDetectionV1 = (
  detections: readonly ObserverOrderInvalidReplayDetectionV1[],
): ObserverOrderInvalidReplayDetectionV1 => {
  if (detections.length === 0)
    throw new Error(
      `${OBSERVER_ORDER_INVALID_CATEGORY_V1}: no authenticated detection`,
    );
  return [...detections].sort((left, right) =>
    left.position === right.position
      ? left.detectionId.localeCompare(right.detectionId)
      : left.position < right.position
        ? -1
        : 1,
  )[0]!;
};

export const observerOrderInvalidAcceptedMembershipV1 = async ({
  block,
  transactionId,
}: {
  readonly block: ObserverOrderInvalidRawBlockEvidenceV1;
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
export const prepareProductionObserverOrderInvalidAcceptedArtifactV1 = async (
  block: ObserverOrderInvalidRawBlockEvidenceV1,
): Promise<ProductionObserverOrderInvalidArtifactV1> => {
  const detection = selectCanonicalObserverOrderInvalidDetectionV1(
    detectObserverOrderInvalidAcceptedRawReplayV1(block),
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
    transaction.material.fieldPreimages[OBSERVER_ORDER_INVALID_FIELD_INDEX_V1];
  if (field === undefined)
    throw new Error("observerOrderInvalid selected field 3 disappeared");
  const evidence = prepareObserverOrderInvalidEvidenceV1({
    finding: {
      subject: SDK.acceptedVerdictSubjectV1(transaction.nodeTxId),
      observerIndex: detection.observerIndex,
    },
    fieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitmentV1(field).toString("hex"),
  });
  return buildProductionObserverOrderInvalidArtifactV1({
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
    transactionMembershipCbor: await observerOrderInvalidAcceptedMembershipV1({
      block,
      transactionId: transaction.nodeTxId,
    }),
  });
};

/** Reconstructs the exact forced wrongful-rejection artifact from canonical replay. */
export const prepareProductionObserverOrderInvalidForcedArtifactV1 = async (
  block: CanonicalBlockEvidenceV1,
): Promise<ProductionObserverOrderInvalidArtifactV1> => {
  const detection = selectCanonicalObserverOrderInvalidDetectionV1(
    detectObserverOrderInvalidForcedReplayV1(block),
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
  const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
    encodeMidgardNativeTxCanonicalV1(
      adjudicateMidgardNativeTxFullV1Validity(
        decodeMidgardNativeTxFullV1FromCanonicalCbor(
          transaction.fullTransactionCbor,
        ),
        "TxIsInvalid",
      ),
    ),
  );
  const field = material.fieldPreimages[OBSERVER_ORDER_INVALID_FIELD_INDEX_V1];
  if (field === undefined)
    throw new Error("observerOrderInvalid forced field 3 disappeared");
  const eventKey = {
    ForcedTransactionEventKey: { tx_order_id: transaction.key },
  } as const;
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: block.reconstruction,
    eventKey,
  });
  const evidence = prepareObserverOrderInvalidEvidenceV1({
    finding: {
      subject: SDK.forcedVerdictSubjectV1({
        transactionId: transaction.value.tx_id,
        sourceKey: transaction.key,
        rejectionReason: verdict.ForcedTxInvalid.reason,
      }),
      observerIndex: detection.observerIndex,
    },
    fieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitmentV1(field).toString("hex"),
  });
  const forcedSourceCbor = Data.to(
    { header: block.header, membership, direction: 1n } as never,
    ObserverOrderInvalidForcedSourcePayloadV1Schema as never,
  );
  return buildProductionObserverOrderInvalidArtifactV1({
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
        source: transaction.value.source,
      } as never,
      SDK.L2TransactionSourceV1 as never,
    ),
    transactionsPhasRoot: "00".repeat(32),
    transactionMembershipCbor: Data.to(
      membership as never,
      SDK.ForcedTransactionSourceMembershipProof as never,
    ),
    forcedSourceCbor,
  });
};

export { buildForcedTransactionLeafMembershipProof };
