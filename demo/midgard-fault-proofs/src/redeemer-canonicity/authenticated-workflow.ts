import {
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  acceptedVerdictSubject,
  type AuthenticatedStateQueueHeaderObservation,
  forcedVerdictSubject,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";

import {
  type CanonicalBlockEvidence,
  fetchCanonicalBlockEvidence,
} from "../evidence/canonical-block-evidence.js";
import type { RetainedDaPayloadSource } from "../transition-trace/fetch.js";
import {
  acceptedTransactionSubject,
  type DetectionSubject,
  forcedTransactionSubject,
} from "../workflow/detection-subject.js";
import {
  prepareRedeemerCanonicityEvidence,
  REDEEMER_CANONICITY_FIELD_INDEX,
  type RedeemerCanonicityEvidence,
  redeemerCanonicityEvidenceCloses,
} from "./family.js";

export const REDEEMER_CANONICITY_WORKFLOW =
  "midgard-redeemer-canonicity-production-workflow-v1" as const;

export type RedeemerCanonicityDetection = DetectionSubject &
  Readonly<{
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
    const items = prepareAllOrNone({
      subject: acceptedVerdictSubject(transaction.nodeTxId),
      field,
    });
    items.forEach((evidence) => {
      if (!redeemerCanonicityEvidenceCloses(evidence)) return;
      found.push(
        Object.freeze({
          ...acceptedTransactionSubject(transaction.nodeTxId),
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
      const reason =
        verdict === "ForcedTxValid" ? null : verdict.ForcedTxInvalid.reason;
      // An accepted forced transaction is committed state under the same
      // canonical-redeemer rule as an accepted L2 transaction, so every item
      // is a wrongful-acceptance candidate. A rejected one is disputed only
      // at the exact item its RedeemerMalformed reason names.
      if (
        reason !== null &&
        (typeof reason === "string" || !("RedeemerMalformed" in reason))
      )
        return;
      const material = deriveMidgardForcedTxFaultEvidenceMaterial(
        transaction.fullTransactionCbor,
      );
      const field = material.fieldPreimages[REDEEMER_CANONICITY_FIELD_INDEX];
      if (field === undefined) return;
      const subject = forcedVerdictSubject({
        transactionId: transaction.value.tx_id,
        sourceKey: transaction.key,
        rejectionReason: reason,
      });
      const items =
        reason === null
          ? prepareAllOrNone({ subject, field })
          : [
              prepareRedeemerCanonicityEvidence({
                finding: {
                  subject,
                  redeemerIndex: Number(
                    reason.RedeemerMalformed.redeemer_index,
                  ),
                },
                fieldPreimage: field,
                committedFieldHashHex:
                  midgardFieldCommitment(field).toString("hex"),
              }),
            ];
      items.forEach((evidence) => {
        if (!redeemerCanonicityEvidenceCloses(evidence)) return;
        found.push(
          Object.freeze({
            ...forcedTransactionSubject(transaction.key),
            detectionId: `redeemer-malformed:forced:${forcedIndex.toString()}:${evidence.redeemerIndex.toString()}:${transaction.value.tx_id}`,
            headerHash: block.headerHash,
            position: BigInt(forcedIndex),
            source: "forced",
            evidence,
          }),
        );
      });
    },
  );
  return Object.freeze(found);
};

/**
 * Evidence for every item of an accepted source's field 8. A field that does
 * not decode into items yields none here; field-shape families own that case.
 */
const prepareAllOrNone = ({
  subject,
  field,
}: {
  readonly subject: VerdictSubject;
  readonly field: Uint8Array;
}): readonly RedeemerCanonicityEvidence[] => {
  try {
    return decodeMidgardFieldPreimage(field).map((_item, redeemerIndex) =>
      prepareRedeemerCanonicityEvidence({
        finding: { subject, redeemerIndex },
        fieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
      }),
    );
  } catch {
    return [];
  }
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
