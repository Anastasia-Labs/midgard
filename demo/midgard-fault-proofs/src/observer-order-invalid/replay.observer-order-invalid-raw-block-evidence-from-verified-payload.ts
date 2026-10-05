import {
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  midgardFieldCommitment,
  type MidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { daHashPreimageBlockEvidenceFromVerifiedPayload } from "../prepare-da-hash-preimage.js";
import {
  buildTrieView,
  requireTransactionsRootMatch,
} from "../prepare-double-spend.js";
import {
  acceptedTransactionSubject,
  type DetectionSubject,
  forcedTransactionSubject,
} from "../workflow/detection-subject.js";
import {
  OBSERVER_ORDER_INVALID_FIELD_INDEX,
  type ObserverOrderInvalidEvidence,
  observerOrderInvalidEvidenceCloses,
  prepareObserverOrderInvalidEvidence,
} from "./family.js";

export const OBSERVER_ORDER_INVALID_VIOLATION_ID =
  "observer-order-invalid" as const;

export const OBSERVER_ORDER_INVALID_RAW_EVIDENCE =
  "midgard-observer-order-invalid-raw-evidence-v1" as const;

export type ObserverOrderInvalidReplayDetection = DetectionSubject &
  Readonly<{
    detectionId: string;
    headerHash: string;
    violationId: typeof OBSERVER_ORDER_INVALID_VIOLATION_ID;
    position: bigint;
    transactionId: string;
    observerIndex: number;
    source: "accepted" | "forced";
    direction: "wrongfulAcceptance" | "wrongfulRejection";
    forcedIndex?: number;
  }>;

export type AuthenticatedObserverOrderInvalidRawTransaction = Readonly<{
  index: number;
  nodeTxId: string;
  l2TransactionSourceCbor: string;
  fullTransactionCbor: string;
  material: MidgardNativeTxFaultEvidenceMaterial;
}>;

/**
 * L1/root authenticated envelope view intentionally constructed before strict
 * CanonicalBlockEvidence. The accepted machine error is observable from the
 * canonical field-3 observer bytes at the first offending adjacent pair.
 */
export type ObserverOrderInvalidRawBlockEvidence = Readonly<{
  schemaVersion: typeof OBSERVER_ORDER_INVALID_RAW_EVIDENCE;
  headerHash: string;
  committedTransactionsRoot: string;
  l2TransactionCount: bigint;
  transactionsPhasRoot: string;
  payloadEnvelopeSha256: string;
  payloadSha256: string;
  transactions: readonly AuthenticatedObserverOrderInvalidRawTransaction[];
}>;

const decodeSource = (cbor: string, index: number): SDK.L2TransactionSource => {
  let source: SDK.L2TransactionSource;
  try {
    source = Data.from(cbor, SDK.L2TransactionSource);
  } catch (cause) {
    throw new Error(
      `observerOrderInvalid transactions[${index.toString()}] source does not decode: ${String(cause)}`,
    );
  }
  if (Data.to(source, SDK.L2TransactionSource) !== cbor) {
    throw new Error(
      `observerOrderInvalid transactions[${index.toString()}] source is not canonical Data`,
    );
  }
  return source;
};

export const observerOrderInvalidRawBlockEvidenceFromVerifiedPayload = async ({
  observation,
  payloadEnvelopeCbor,
  daProvenance,
  minimumConfirmationDepth,
}: {
  readonly observation: SDK.AuthenticatedStateQueueHeaderObservation;
  readonly payloadEnvelopeCbor: Uint8Array;
  readonly daProvenance: SDK.EvidenceProvenance;
  readonly minimumConfirmationDepth?: number;
}): Promise<ObserverOrderInvalidRawBlockEvidence> => {
  const raw = await daHashPreimageBlockEvidenceFromVerifiedPayload({
    observation,
    payloadEnvelopeCbor,
    daProvenance,
    ...(minimumConfirmationDepth === undefined
      ? {}
      : { minimumConfirmationDepth }),
  });
  const payloadCbor = Buffer.from(
    (
      await unwrapDaPayload(payloadEnvelopeCbor, {
        maxPayloadBytes: DA_TRANSPORT_LIMITS.maxPayloadBytes,
      })
    ).innerBytes,
  );
  const payload = SDK.decodeDaPayload(payloadCbor);
  if (!SDK.encodeDaPayload(payload).equals(payloadCbor))
    throw new Error("observerOrderInvalid DA payload is not canonical");

  const entries = raw.entries.map(([key, value]) => ({
    key: Buffer.from(key, "hex"),
    value: Buffer.from(value, "hex"),
  }));
  const trie = await buildTrieView(entries);
  await requireTransactionsRootMatch({
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
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
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
    schemaVersion: OBSERVER_ORDER_INVALID_RAW_EVIDENCE,
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
  transaction: AuthenticatedObserverOrderInvalidRawTransaction,
  observerIndex: number,
): ObserverOrderInvalidEvidence | null => {
  if (transaction.material.canonical.validity !== "TxIsValid") return null;
  const field =
    transaction.material.fieldPreimages[OBSERVER_ORDER_INVALID_FIELD_INDEX];
  if (field === undefined) return null;
  try {
    const evidence = prepareObserverOrderInvalidEvidence({
      finding: {
        subject: SDK.acceptedVerdictSubject(transaction.nodeTxId),
        observerIndex,
      },
      fieldPreimage: field,
      committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
    });
    return observerOrderInvalidEvidenceCloses(evidence) ? evidence : null;
  } catch {
    return null;
  }
};

/** Complete accepted scan in machine order over every authenticated coordinate. */
export const detectObserverOrderInvalidAcceptedRawReplay = (
  block: ObserverOrderInvalidRawBlockEvidence,
): readonly ObserverOrderInvalidReplayDetection[] => {
  const detections: ObserverOrderInvalidReplayDetection[] = [];
  for (const transaction of block.transactions) {
    const field =
      transaction.material.fieldPreimages[OBSERVER_ORDER_INVALID_FIELD_INDEX];
    if (field === undefined) continue;
    let itemCount: number;
    try {
      itemCount = decodeMidgardFieldPreimage(field).length;
    } catch {
      // Field-envelope failures belong to the earlier decoding families.
      continue;
    }
    for (let observerIndex = 1; observerIndex < itemCount; observerIndex += 1) {
      const evidence = acceptedEvidence(transaction, observerIndex);
      if (evidence === null) continue;
      detections.push(
        Object.freeze({
          ...acceptedTransactionSubject(transaction.nodeTxId),
          detectionId: `${OBSERVER_ORDER_INVALID_VIOLATION_ID}:accepted:${transaction.index.toString()}:${transaction.nodeTxId}:${observerIndex.toString()}`,
          headerHash: block.headerHash,
          violationId: OBSERVER_ORDER_INVALID_VIOLATION_ID,
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
export const detectObserverOrderInvalidForcedReplay = (
  block: CanonicalBlockEvidence,
): readonly ObserverOrderInvalidReplayDetection[] => {
  const detections: ObserverOrderInvalidReplayDetection[] = [];
  block.reconstruction.forcedTransactions.forEach(
    (transaction, forcedIndex) => {
      const verdict = transaction.value.verdict;
      if (verdict === "ForcedTxValid") return;
      const reason = verdict.ForcedTxInvalid.reason;
      if (typeof reason === "string" || !("ObserverOrderInvalid" in reason))
        return;
      const observerIndex = Number(reason.ObserverOrderInvalid.observer_index);
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
      )
        throw new Error(
          "observerOrderInvalid forced transaction differs from its authenticated leaf",
        );
      const field = material.fieldPreimages[OBSERVER_ORDER_INVALID_FIELD_INDEX];
      if (field === undefined) return;
      const evidence = prepareObserverOrderInvalidEvidence({
        finding: {
          subject: SDK.forcedVerdictSubject({
            transactionId: transaction.value.tx_id,
            sourceKey: transaction.key,
            rejectionReason: reason,
          }),
          observerIndex,
        },
        fieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
      });
      if (!observerOrderInvalidEvidenceCloses(evidence)) return;
      detections.push(
        Object.freeze({
          ...forcedTransactionSubject(transaction.key),
          detectionId: `${OBSERVER_ORDER_INVALID_VIOLATION_ID}:forced:${forcedIndex.toString()}:${transaction.value.tx_id}:${observerIndex.toString()}`,
          headerHash: block.headerHash,
          violationId: OBSERVER_ORDER_INVALID_VIOLATION_ID,
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
