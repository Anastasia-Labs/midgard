import {
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  type AuthenticatedStateQueueHeaderObservation,
  forcedVerdictSubject,
} from "@al-ft/midgard-sdk";

import {
  type CanonicalBlockEvidence,
  fetchCanonicalBlockEvidence,
} from "../evidence/canonical-block-evidence-v1.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  prepareRedeemerCanonicityEvidence,
  REDEEMER_CANONICITY_FIELD_INDEX,
  type RedeemerCanonicityEvidence,
  redeemerCanonicityEvidenceCloses,
} from "./family-v1.js";

export const REDEEMER_CANONICITY_WORKFLOW =
  "midgard-redeemer-canonicity-production-workflow-v1" as const;

export type RedeemerCanonicityDetection = Readonly<{
  detectionId: string;
  headerHash: string;
  position: bigint;
  source: "accepted" | "forced";
  evidence: RedeemerCanonicityEvidence;
}>;

/** Callback-free replay over authenticated L1 plus retained public DA bytes. */
export const detectRedeemerCanonicityFromCanonicalBlock = (
  block: CanonicalBlockEvidence,
): readonly RedeemerCanonicityDetection[] => {
  const found: RedeemerCanonicityDetection[] = [];
  block.transactions.forEach((transaction, transactionIndex) => {
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      Buffer.from(transaction.txCbor, "hex"),
    );
    if (material.canonical.validity !== "TxIsValid") return;
    const field = material.fieldPreimages[REDEEMER_CANONICITY_FIELD_INDEX];
    if (field === undefined) return;
    const items = (() => {
      try {
        return prepareAll({
          subject: acceptedVerdictSubject(transaction.nodeTxId),
          field,
        });
      } catch {
        return [];
      }
    })();
    items.forEach((evidence) => {
      if (!redeemerCanonicityEvidenceCloses(evidence)) return;
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
      const material = deriveMidgardNativeTxFaultEvidenceMaterial(
        transaction.fullTransactionCbor,
      );
      const field = material.fieldPreimages[REDEEMER_CANONICITY_FIELD_INDEX];
      if (field === undefined) return;
      const evidence = prepareRedeemerCanonicityEvidence({
        finding: {
          subject: forcedVerdictSubject({
            transactionId: transaction.value.tx_id,
            sourceKey: transaction.key,
            rejectionReason: verdict.ForcedTxInvalid.reason,
          }),
          redeemerIndex,
        },
        fieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
      });
      if (!redeemerCanonicityEvidenceCloses(evidence)) return;
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
  readonly subject: ReturnType<typeof acceptedVerdictSubject>;
  readonly field: Uint8Array;
}) => {
  return decodeMidgardFieldPreimage(field).map((_item, redeemerIndex) =>
    prepareRedeemerCanonicityEvidence({
      finding: { subject, redeemerIndex },
      fieldPreimage: field,
      committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
    }),
  );
};

export const detectRedeemerCanonicityFromRetainedDa = async ({
  observation,
  sources,
}: {
  readonly observation: AuthenticatedStateQueueHeaderObservation;
  readonly sources: readonly RetainedDaPayloadSource[];
}): Promise<readonly RedeemerCanonicityDetection[]> =>
  detectRedeemerCanonicityFromCanonicalBlock(
    await fetchCanonicalBlockEvidence({ observation, sources }),
  );

/** Canonical all-detections adapter consumed by complete replay registries. */
export const detectRedeemerCanonicityCompleteReplay =
  detectRedeemerCanonicityFromCanonicalBlock;

export type RedeemerCanonicityWorkflowStage =
  | "none"
  | "step01"
  | "step02"
  | "step03"
  | "proven"
  | "removed"
  | "cancelled";

export const nextRedeemerCanonicityAction = (
  stage: RedeemerCanonicityWorkflowStage,
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
