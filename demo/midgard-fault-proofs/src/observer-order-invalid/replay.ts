import {
  adjudicateMidgardNativeTxFullValidity,
  decodeMidgardFieldPreimage,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeMidgardNativeTxCanonical,
  midgardFieldCommitment,
  type MidgardNativeTxFaultEvidenceMaterial,
} from "@al-ft/midgard-core";
import { unwrapDaPayload } from "@al-ft/midgard-core/da-payload-envelope";
import { DA_TRANSPORT_LIMITS } from "@al-ft/midgard-core/da-transport";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { daHashPreimageBlockEvidenceFromVerifiedPayload } from "../prepare-da-hash-preimage.js";
import {
  buildTrieView,
  requireProof,
  requireTransactionsRootMatch,
} from "../prepare-double-spend.js";
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
  type ObserverOrderInvalidEvidence,
  observerOrderInvalidEvidenceCloses,
  prepareObserverOrderInvalidEvidence,
} from "./family.js";

export const OBSERVER_ORDER_INVALID_VIOLATION_ID =
  "observer-order-invalid" as const;
export const OBSERVER_ORDER_INVALID_RAW_EVIDENCE =
  "midgard-observer-order-invalid-raw-evidence-v1" as const;

export type ObserverOrderInvalidReplayDetection = Readonly<{
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
      const material = deriveMidgardNativeTxFaultEvidenceMaterial(
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
  const material = deriveMidgardNativeTxFaultEvidenceMaterial(
    encodeMidgardNativeTxCanonical(
      adjudicateMidgardNativeTxFullValidity(
        decodeMidgardNativeTxFullFromCanonicalCbor(
          transaction.fullTransactionCbor,
        ),
        "TxIsInvalid",
      ),
    ),
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
        source: transaction.value.source,
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

export { buildForcedTransactionLeafMembershipProof };
