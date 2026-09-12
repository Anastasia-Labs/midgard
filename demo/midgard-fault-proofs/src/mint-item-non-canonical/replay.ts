import {
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL,
  midgardEnvelopeVerdict,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import {
  type MintItemEvidence,
  prepareMintItemEvidence,
} from "./mint-item-non-canonical.js";
import { mintItemPolicyId } from "./scan.js";

/** All in-scope accepted findings, in transaction/item order. Outer grammar,
 * field-size and zero-width faults remain with their existing families. */
export const findMintItemNonCanonicalEvidence = (
  transactions: readonly {
    readonly nodeTxId: string;
    readonly txCbor: string;
    readonly subject?: VerdictSubject;
  }[],
): readonly {
  readonly transactionIndex: number;
  readonly evidence: MintItemEvidence;
}[] =>
  transactions.flatMap((transaction, transactionIndex) => {
    const material = deriveMidgardNativeTxFaultEvidenceMaterial(
      Buffer.from(transaction.txCbor, "hex"),
    );
    const transactionId = material.transactionId.toString("hex");
    if (transaction.nodeTxId !== transactionId)
      throw new Error(
        "mintItemNonCanonical replay transaction identity changed",
      );
    const fieldPreimage = material.fieldPreimages[5]!;
    if (
      fieldPreimage.length > 32_768 ||
      midgardEnvelopeVerdict(fieldPreimage) !==
        MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL
    )
      return [];
    const items = decodeMidgardFieldPreimage(fieldPreimage);
    const committedFieldHashHex =
      midgardFieldCommitment(fieldPreimage).toString("hex");
    return items.flatMap((item, itemIndex) => {
      if (item.length === 0) return [];
      // The malformed predecessor already has its own finding. No invented
      // predecessor key may enter an adjacent-policy accusation.
      if (itemIndex > 0 && mintItemPolicyId(items[itemIndex - 1]!) === null)
        return [];
      const evidence = prepareMintItemEvidence({
        finding: {
          subject: transaction.subject ?? acceptedVerdictSubject(transactionId),
          fieldIndex: 5,
          itemIndex,
        },
        fieldPreimage,
        committedFieldHashHex,
      });
      return evidence.decisiveFaultHolds
        ? [{ transactionIndex, evidence }]
        : [];
    });
  });

/** True when the direct mint-item proof owns this transaction: sibling
 * replayers that decode field 5 must yield a field-shape prerequisite. */
export const transactionHasNonCanonicalMintItem = (txCbor: Buffer): boolean =>
  findMintItemNonCanonicalEvidence([
    {
      nodeTxId:
        deriveMidgardNativeTxFaultEvidenceMaterial(
          txCbor,
        ).transactionId.toString("hex"),
      txCbor: txCbor.toString("hex"),
    },
  ]).length > 0;

export const findMintItemNonCanonicalBlockEvidence = (
  block: CanonicalBlockEvidence,
) =>
  findMintItemNonCanonicalEvidence([
    ...block.transactions,
    ...block.reconstruction.forcedTransactions.flatMap((forced) => {
      if (forced.value.verdict !== "ForcedTxValid") return [];
      const material = deriveMidgardNativeTxFaultEvidenceMaterial(
        forced.fullTransactionCbor,
      );
      const source = material.proofSource;
      if (
        source.compactCbor.toString("hex") !==
          forced.value.source.compact_cbor ||
        source.witnessSetCompactCbor.toString("hex") !==
          forced.value.source.witness_set_compact_cbor ||
        source.fieldPreimageLengthsCbor.toString("hex") !==
          forced.value.source.field_preimage_lengths_cbor
      )
        throw new Error(
          "mintItemNonCanonical forced source differs from retained bytes",
        );
      return [
        {
          nodeTxId: forced.value.tx_id,
          txCbor: Buffer.from(forced.fullTransactionCbor).toString("hex"),
          subject: forcedVerdictSubject({
            transactionId: forced.value.tx_id,
            sourceKey: forced.key,
            rejectionReason: null,
          }),
        },
      ];
    }),
  ]);

export const detectMintItemNonCanonicalCompleteReplay = (
  block: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] =>
  findMintItemNonCanonicalBlockEvidence(block).map(
    ({ transactionIndex, evidence }) => ({
      detectionId: `mint-item-non-canonical:${transactionIndex}:${evidence.subject.transaction_id}:${evidence.itemIndex}`,
      headerHash: block.headerHash,
      violationId: "mint-item-non-canonical",
      position: BigInt(transactionIndex),
      diagnostic: `transaction ${evidence.subject.transaction_id} mint policy item ${evidence.itemIndex} violates the field-5 grammar or policy ordering`,
    }),
  );
