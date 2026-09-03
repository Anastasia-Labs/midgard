import {
  adjudicateMidgardNativeTxFullValidity,
  decodeMidgardNativeTxFullFromCanonicalCbor,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeMidgardNativeTxCanonical,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import * as SDK from "@al-ft/midgard-sdk";
import { Data } from "@lucid-evolution/lucid";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence-v1.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";
import {
  OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY,
  OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_FIELD_INDEX,
  observersForbiddenEvidenceCloses,
  prepareObserversForbiddenEvidence,
} from "./family-v1.js";
import {
  buildObserversForbiddenArtifact,
  type ObserversForbiddenArtifact,
  ObserversForbiddenForcedSourcePayloadSchema,
} from "./production-artifact-v1.js";
import {
  observersForbiddenAcceptedMembership,
  type ObserversForbiddenRawBlockEvidence,
  observersForbiddenRawBlockEvidenceFromVerifiedPayload,
} from "./raw-evidence-v1.js";

export const OBSERVERS_FORBIDDEN_VIOLATION_ID =
  "observers-forbidden-on-untagged-network" as const;

export type ObserversForbiddenReplayDetection = Readonly<{
  detectionId: string;
  headerHash: string;
  violationId: typeof OBSERVERS_FORBIDDEN_VIOLATION_ID;
  position: bigint;
  transactionId: string;
  networkId: 0 | 1 | 255;
  observerCount: number;
  source: "accepted" | "forced";
  direction: "wrongfulAcceptance" | "wrongfulRejection";
  forcedIndex?: number;
}>;

/**
 * Authenticates the retained DA envelope and transactions root before the
 * canonical-block parser. This ordering preserves the accepted machine error:
 * a non-empty observer field on scalar 255 must be classified here first.
 */
export {
  type ObserversForbiddenRawBlockEvidence,
  observersForbiddenRawBlockEvidenceFromVerifiedPayload,
};

const exactNetwork = (value: bigint): 0 | 1 | 255 | null =>
  value === 0n ? 0 : value === 1n ? 1 : value === 255n ? 255 : null;

export const detectObserversForbiddenAcceptedRawReplay = (
  block: ObserversForbiddenRawBlockEvidence,
): readonly ObserversForbiddenReplayDetection[] => {
  const detections: ObserversForbiddenReplayDetection[] = [];
  for (const transaction of block.transactions) {
    if (transaction.material.canonical.validity !== "TxIsValid") continue;
    const field =
      transaction.material.fieldPreimages[
        OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_FIELD_INDEX
      ];
    const networkId = exactNetwork(
      transaction.material.canonical.body.networkId,
    );
    if (field === undefined || networkId === null) continue;
    try {
      const evidence = prepareObserversForbiddenEvidence({
        finding: {
          subject: SDK.acceptedVerdictSubject(transaction.nodeTxId),
          networkId,
        },
        observerFieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
      });
      if (!observersForbiddenEvidenceCloses(evidence)) continue;
      detections.push(
        Object.freeze({
          detectionId: `${OBSERVERS_FORBIDDEN_VIOLATION_ID}:accepted:${transaction.index.toString()}:${transaction.nodeTxId}`,
          headerHash: block.headerHash,
          violationId: OBSERVERS_FORBIDDEN_VIOLATION_ID,
          position: BigInt(transaction.index),
          transactionId: transaction.nodeTxId,
          networkId,
          observerCount: evidence.observerCount,
          source: "accepted",
          direction: "wrongfulAcceptance",
        }),
      );
    } catch {
      // Earlier field-envelope families own malformed field 3.
    }
  }
  return Object.freeze(detections);
};

export const detectObserversForbiddenForcedReplay = (
  block: CanonicalBlockEvidence,
): readonly ObserversForbiddenReplayDetection[] => {
  const detections: ObserversForbiddenReplayDetection[] = [];
  block.reconstruction.forcedTransactions.forEach(
    (transaction, forcedIndex) => {
      const verdict = transaction.value.verdict;
      if (verdict === "ForcedTxValid") return;
      if (
        verdict.ForcedTxInvalid.reason !== "ObserversForbiddenOnUntaggedNetwork"
      )
        return;
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
          "observersForbidden forced transaction differs from authenticated leaf",
        );
      const field =
        material.fieldPreimages[
          OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_FIELD_INDEX
        ];
      const networkId = exactNetwork(material.canonical.body.networkId);
      if (field === undefined || networkId === null) return;
      const evidence = prepareObserversForbiddenEvidence({
        finding: {
          subject: SDK.forcedVerdictSubject({
            transactionId: transaction.value.tx_id,
            sourceKey: transaction.key,
            rejectionReason: verdict.ForcedTxInvalid.reason,
          }),
          networkId,
        },
        observerFieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
      });
      if (!observersForbiddenEvidenceCloses(evidence)) return;
      detections.push(
        Object.freeze({
          detectionId: `${OBSERVERS_FORBIDDEN_VIOLATION_ID}:forced:${forcedIndex.toString()}:${transaction.value.tx_id}`,
          headerHash: block.headerHash,
          violationId: OBSERVERS_FORBIDDEN_VIOLATION_ID,
          position: BigInt(forcedIndex),
          transactionId: transaction.value.tx_id,
          networkId,
          observerCount: evidence.observerCount,
          source: "forced",
          direction: "wrongfulRejection",
          forcedIndex,
        }),
      );
    },
  );
  return Object.freeze(detections);
};

export const selectCanonicalObserversForbiddenDetection = (
  detections: readonly ObserversForbiddenReplayDetection[],
) => {
  if (detections.length === 0)
    throw new Error(
      `${OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_CATEGORY}: no authenticated detection`,
    );
  return [...detections].sort((left, right) =>
    left.position === right.position
      ? left.detectionId.localeCompare(right.detectionId)
      : left.position < right.position
        ? -1
        : 1,
  )[0]!;
};

export const prepareObserversForbiddenAcceptedArtifact = async (
  block: ObserversForbiddenRawBlockEvidence,
): Promise<ObserversForbiddenArtifact> => {
  const detection = selectCanonicalObserversForbiddenDetection(
    detectObserversForbiddenAcceptedRawReplay(block),
  );
  const transaction = block.transactions[Number(detection.position)];
  if (transaction?.nodeTxId !== detection.transactionId)
    throw new Error(
      "observersForbidden selected accepted transaction disappeared",
    );
  const field =
    transaction.material.fieldPreimages[
      OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_FIELD_INDEX
    ];
  if (field === undefined)
    throw new Error("observersForbidden selected field 3 disappeared");
  const evidence = prepareObserversForbiddenEvidence({
    finding: {
      subject: SDK.acceptedVerdictSubject(transaction.nodeTxId),
      networkId: detection.networkId,
    },
    observerFieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
  });
  return buildObserversForbiddenArtifact({
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
    transactionMembershipCbor: await observersForbiddenAcceptedMembership({
      block,
      transactionId: transaction.nodeTxId,
    }),
  });
};

export const prepareObserversForbiddenForcedArtifact = async (
  block: CanonicalBlockEvidence,
): Promise<ObserversForbiddenArtifact> => {
  const detection = selectCanonicalObserversForbiddenDetection(
    detectObserversForbiddenForcedReplay(block),
  );
  const transaction =
    block.reconstruction.forcedTransactions[detection.forcedIndex!];
  if (
    transaction === undefined ||
    transaction.value.verdict === "ForcedTxValid"
  )
    throw new Error(
      "observersForbidden selected forced transaction disappeared",
    );
  const reason = transaction.value.verdict.ForcedTxInvalid.reason;
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
  const field =
    material.fieldPreimages[
      OBSERVERS_FORBIDDEN_ON_UNTAGGED_NETWORK_FIELD_INDEX
    ];
  const networkId = exactNetwork(material.canonical.body.networkId);
  if (field === undefined || networkId === null)
    throw new Error("observersForbidden selected forced field disappeared");
  const eventKey = {
    ForcedTransactionEventKey: { tx_order_id: transaction.key },
  } as const;
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: block.reconstruction,
    eventKey,
  });
  const evidence = prepareObserversForbiddenEvidence({
    finding: {
      subject: SDK.forcedVerdictSubject({
        transactionId: transaction.value.tx_id,
        sourceKey: transaction.key,
        rejectionReason: reason,
      }),
      networkId,
    },
    observerFieldPreimage: field,
    committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
  });
  const forcedSourceCbor = Data.to(
    { header: block.header, membership, direction: 1n } as never,
    ObserversForbiddenForcedSourcePayloadSchema as never,
  );
  return buildObserversForbiddenArtifact({
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
