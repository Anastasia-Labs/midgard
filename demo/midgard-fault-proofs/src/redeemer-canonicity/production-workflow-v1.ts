import {
  decodeMidgardFieldPreimageV1,
  deriveMidgardNativeTxFaultEvidenceMaterialV1,
  midgardFieldCommitmentV1,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubjectV1,
  type AuthenticatedStateQueueHeaderObservationV1,
  forcedVerdictSubjectV1,
} from "@al-ft/midgard-sdk";

import {
  type CanonicalBlockEvidenceV1,
  fetchCanonicalBlockEvidenceV1,
} from "../evidence/canonical-block-evidence-v1.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  prepareRedeemerCanonicityEvidenceV1,
  REDEEMER_CANONICITY_FIELD_INDEX_V1,
  redeemerCanonicityEvidenceClosesV1,
  type RedeemerCanonicityEvidenceV1,
} from "./family-v1.js";

export const REDEEMER_CANONICITY_PRODUCTION_WORKFLOW_V1 =
  "midgard-redeemer-canonicity-production-workflow-v1" as const;

export type RedeemerCanonicityDetectionV1 = Readonly<{
  detectionId: string;
  headerHash: string;
  position: bigint;
  source: "accepted" | "forced";
  evidence: RedeemerCanonicityEvidenceV1;
}>;

/** Callback-free replay over authenticated L1 plus retained public DA bytes. */
export const detectRedeemerCanonicityFromCanonicalBlockV1 = (
  block: CanonicalBlockEvidenceV1,
): readonly RedeemerCanonicityDetectionV1[] => {
  const found: RedeemerCanonicityDetectionV1[] = [];
  block.transactions.forEach((transaction, transactionIndex) => {
    const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
      Buffer.from(transaction.txCbor, "hex"),
    );
    if (material.canonical.validity !== "TxIsValid") return;
    const field = material.fieldPreimages[REDEEMER_CANONICITY_FIELD_INDEX_V1];
    if (field === undefined) return;
    const items = (() => {
      try {
        return prepareAll({
          subject: acceptedVerdictSubjectV1(transaction.nodeTxId),
          field,
        });
      } catch {
        return [];
      }
    })();
    items.forEach((evidence) => {
      if (!redeemerCanonicityEvidenceClosesV1(evidence)) return;
      found.push(
        Object.freeze({
          detectionId: `redeemer-malformed:accepted:${transactionIndex.toString()}:${evidence.redeemerIndex.toString()}:${transaction.nodeTxId}`,
          headerHash: block.headerHash,
          position: BigInt(transactionIndex),
          source: "accepted",
          evidence,
        }),
      );
    });
  });
  block.reconstruction.forcedTransactions.forEach(
    (transaction, forcedIndex) => {
      const verdict = transaction.value.verdict;
      if (
        verdict === "ForcedTxValid" ||
        typeof verdict.ForcedTxInvalid.reason === "string" ||
        !("RedeemerMalformed" in verdict.ForcedTxInvalid.reason)
      )
        return;
      const redeemerIndex = Number(
        verdict.ForcedTxInvalid.reason.RedeemerMalformed.redeemer_index,
      );
      const material = deriveMidgardNativeTxFaultEvidenceMaterialV1(
        transaction.fullTransactionCbor,
      );
      const field = material.fieldPreimages[REDEEMER_CANONICITY_FIELD_INDEX_V1];
      if (field === undefined) return;
      const evidence = prepareRedeemerCanonicityEvidenceV1({
        finding: {
          subject: forcedVerdictSubjectV1({
            transactionId: transaction.value.tx_id,
            sourceKey: transaction.key,
            rejectionReason: verdict.ForcedTxInvalid.reason,
          }),
          redeemerIndex,
        },
        fieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitmentV1(field).toString("hex"),
      });
      if (!redeemerCanonicityEvidenceClosesV1(evidence)) return;
      found.push(
        Object.freeze({
          detectionId: `redeemer-malformed:forced:${forcedIndex.toString()}:${redeemerIndex.toString()}:${transaction.value.tx_id}`,
          headerHash: block.headerHash,
          position: BigInt(forcedIndex),
          source: "forced",
          evidence,
        }),
      );
    },
  );
  return Object.freeze(found);
};

const prepareAll = ({
  subject,
  field,
}: {
  readonly subject: ReturnType<typeof acceptedVerdictSubjectV1>;
  readonly field: Uint8Array;
}) => {
  return decodeMidgardFieldPreimageV1(field).map((_item, redeemerIndex) =>
    prepareRedeemerCanonicityEvidenceV1({
      finding: { subject, redeemerIndex },
      fieldPreimage: field,
      committedFieldHashHex: midgardFieldCommitmentV1(field).toString("hex"),
    }),
  );
};

export const detectRedeemerCanonicityFromRetainedDaV1 = async ({
  observation,
  sources,
}: {
  readonly observation: AuthenticatedStateQueueHeaderObservationV1;
  readonly sources: readonly RetainedDaPayloadSource[];
}): Promise<readonly RedeemerCanonicityDetectionV1[]> =>
  detectRedeemerCanonicityFromCanonicalBlockV1(
    await fetchCanonicalBlockEvidenceV1({ observation, sources }),
  );

/** Canonical all-detections adapter consumed by complete replay registries. */
export const detectRedeemerCanonicityCompleteReplayV1 =
  detectRedeemerCanonicityFromCanonicalBlockV1;

export type RedeemerCanonicityWorkflowStageV1 =
  | "none"
  | "step01"
  | "step02"
  | "step03"
  | "proven"
  | "removed"
  | "cancelled";

export const nextRedeemerCanonicityActionV1 = (
  stage: RedeemerCanonicityWorkflowStageV1,
):
  | "submitInit"
  | "submitStep01"
  | "submitDecode"
  | "submitFinal"
  | "remove"
  | "done" => {
  switch (stage) {
    case "none":
      return "submitInit";
    case "step01":
      return "submitStep01";
    case "step02":
      return "submitDecode";
    case "step03":
      return "submitFinal";
    case "proven":
      return "remove";
    case "removed":
    case "cancelled":
      return "done";
  }
};
