import {
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";

import { type CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { claimedFieldItems } from "../evidence/claimed-coordinate.js";
import {
  acceptedTransactionSubject,
  type DetectionSubject,
  forcedTransactionSubject,
} from "../workflow/detection-subject.js";
import {
  prepareRedeemerCanonicityEvidence,
  REDEEMER_CANONICITY_EXECUTION_FAILED_INDEX,
  REDEEMER_CANONICITY_FIELD_INDEX,
  type RedeemerCanonicityEvidence,
  redeemerCanonicityEvidenceCloses,
} from "./family.js";

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
      // is a wrongful-acceptance candidate. A rejected one is disputed at the
      // exact coordinate its RedeemerMalformed reason names, held or not, and
      // a Plutus execution failure only when field 8 has no redeemers.
      if (
        reason !== null &&
        (typeof reason === "string" ||
          !("RedeemerMalformed" in reason || "PlutusExecutionFailed" in reason))
      )
        return;
      const material = deriveMidgardForcedTxFaultEvidenceMaterial(
        transaction.fullTransactionCbor,
      );
      const field = material.fieldPreimages[REDEEMER_CANONICITY_FIELD_INDEX];
      if (field === undefined) return;
      // A malformed field 8 belongs to the field-shape families.
      const fieldItems = claimedFieldItems(field);
      if (reason !== null && fieldItems === undefined) return;
      if (
        reason !== null &&
        "PlutusExecutionFailed" in reason &&
        fieldItems!.length > 0
      )
        return;
      const redeemerIndex =
        reason === null
          ? undefined
          : "RedeemerMalformed" in reason
            ? reason.RedeemerMalformed.redeemer_index
            : REDEEMER_CANONICITY_EXECUTION_FAILED_INDEX;
      const subject = forcedVerdictSubject({
        transactionId: transaction.value.tx_id,
        sourceKey: transaction.key,
        rejectionReason: reason,
      });
      const items =
        redeemerIndex === undefined
          ? prepareAllOrNone({ subject, field })
          : [
              prepareRedeemerCanonicityEvidence({
                finding: { subject, redeemerIndex },
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
        finding: { subject, redeemerIndex: BigInt(redeemerIndex) },
        fieldPreimage: field,
        committedFieldHashHex: midgardFieldCommitment(field).toString("hex"),
      }),
    );
  } catch {
    return [];
  }
};

/** Canonical all-detections adapter consumed by complete replay registries. */
export const detectRedeemerCanonicityCompleteReplay =
  detectRedeemerCanonicityFromCanonicalBlock;
