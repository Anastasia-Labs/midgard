import {
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  midgardFieldCommitment,
} from "@al-ft/midgard-core";
import { deriveMidgardForcedTxFaultEvidenceMaterial } from "@al-ft/midgard-core/codec/forced";
import {
  acceptedVerdictSubject,
  forcedVerdictSubject,
  MIDGARD_ENVELOPE_VERDICT_GRAMMATICAL,
  midgardEnvelopeVerdict,
  PROOF_THREAD_SOURCE_KIND_FORCED,
  type VerdictSubject,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import type { CanonicalViolationDetection } from "../workflow/classification.js";
import { verdictDetectionSubject } from "../workflow/detection-subject.js";
import {
  mintItemClaimIsUnreachable,
  type MintItemEvidence,
  prepareMintItemEvidence,
  prepareMintItemUnreachableClaimEvidence,
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
    const bytes = Buffer.from(transaction.txCbor, "hex");
    const material =
      transaction.subject?.source_kind === PROOF_THREAD_SOURCE_KIND_FORCED
        ? deriveMidgardForcedTxFaultEvidenceMaterial(bytes)
        : deriveMidgardNativeTxFaultEvidenceMaterial(bytes);
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

/** Every forced leaf rejected for `MintItemMalformed`: no machine step emits
 * that reason, so each such rejection is wrongful whatever policy it names.
 * Its `transactionIndex` is the forced leaf's index, as `forced: true`. */
const unreachableForcedClaims = (block: CanonicalBlockEvidence) =>
  block.reconstruction.forcedTransactions.flatMap((forced, forcedIndex) => {
    if (forced.value.verdict === "ForcedTxValid") return [];
    const subject = forcedVerdictSubject({
      transactionId: forced.value.tx_id,
      sourceKey: forced.key,
      rejectionReason: forced.value.verdict.ForcedTxInvalid.reason,
    });
    return mintItemClaimIsUnreachable(subject)
      ? [
          {
            transactionIndex: forcedIndex,
            forced: true as const,
            evidence: prepareMintItemUnreachableClaimEvidence(subject),
          },
        ]
      : [];
  });

export const findMintItemNonCanonicalBlockEvidence = (
  block: CanonicalBlockEvidence,
): readonly {
  readonly transactionIndex: number;
  readonly forced?: true;
  readonly evidence: MintItemEvidence;
}[] => [
  ...findMintItemNonCanonicalEvidence([
    ...block.transactions,
    ...block.reconstruction.forcedTransactions.flatMap((forced) => {
      if (forced.value.verdict !== "ForcedTxValid") return [];
      const material = deriveMidgardForcedTxFaultEvidenceMaterial(
        forced.fullTransactionCbor,
      );
      const source = material.proofSource;
      if (
        source.compactCbor.toString("hex") !==
          forced.value.submitted_source.compact_cbor ||
        source.witnessSetCompactCbor.toString("hex") !==
          forced.value.submitted_source.witness_set_compact_cbor ||
        source.fieldPreimageLengthsCbor.toString("hex") !==
          forced.value.submitted_source.field_preimage_lengths_cbor
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
  ]),
  ...unreachableForcedClaims(block),
];

/** The classifier's identity for one finding of this block. */
export const mintItemNonCanonicalDetectionId = ({
  transactionIndex,
  forced,
  evidence,
}: {
  readonly transactionIndex: number;
  readonly forced?: true;
  readonly evidence: MintItemEvidence;
}): string =>
  forced === true
    ? `mint-item-non-canonical:forced:${transactionIndex}:${evidence.subject.transaction_id}:malformed`
    : `mint-item-non-canonical:${transactionIndex}:${evidence.subject.transaction_id}:${evidence.itemIndex}`;

export const detectMintItemNonCanonicalCompleteReplay = (
  block: CanonicalBlockEvidence,
): readonly CanonicalViolationDetection[] =>
  findMintItemNonCanonicalBlockEvidence(block).map((finding) => ({
    ...verdictDetectionSubject(finding.evidence.subject),
    detectionId: mintItemNonCanonicalDetectionId(finding),
    headerHash: block.headerHash,
    violationId: "mint-item-non-canonical",
    position: BigInt(finding.transactionIndex),
    diagnostic:
      finding.forced === true
        ? `forced transaction ${finding.evidence.subject.transaction_id} was rejected for a malformed mint item, which no validation step reports`
        : `transaction ${finding.evidence.subject.transaction_id} mint policy item ${finding.evidence.itemIndex} violates the field-5 grammar or policy ordering`,
  }));
