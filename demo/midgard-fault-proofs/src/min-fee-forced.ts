import {
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  deriveMidgardNativeTxWitnessSetCompact,
  encodeMidgardNativeTxCanonical,
} from "@al-ft/midgard-core";
import {
  forcedVerdictSubject,
  type MinFeeStep02State,
  minimumFeeFromProofSource,
} from "@al-ft/midgard-sdk";

import type { CanonicalBlockEvidence } from "./evidence/canonical-block-evidence.js";
import { nativeTxFromCoreCompact } from "./submit-step-01.js";
import { buildForcedTransactionLeafMembershipProof } from "./transition-trace/witnesses.js";

export const detectMinFeeForcedReplay = (block: CanonicalBlockEvidence) => {
  return block.reconstruction.forcedTransactions.flatMap(
    (transaction, forcedIndex) => {
      const verdict = transaction.value.verdict;
      if (
        verdict === "ForcedTxValid" ||
        verdict.ForcedTxInvalid.reason !== "FeeBelowMinimum"
      )
        return [];
      const submitted = deriveMidgardNativeTxFaultEvidenceMaterial(
        transaction.fullTransactionCbor,
      );
      const material = deriveMidgardNativeTxFaultEvidenceMaterial(
        encodeMidgardNativeTxCanonical({
          ...submitted.canonical,
          validity: "TxIsInvalid",
        }),
      );
      const source = transaction.value.source;
      if (
        material.transactionId.toString("hex") !== transaction.value.tx_id ||
        material.proofSource.compactCbor.toString("hex") !==
          source.compact_cbor ||
        material.proofSource.witnessSetCompactCbor.toString("hex") !==
          source.witness_set_compact_cbor ||
        material.proofSource.fieldPreimageLengthsCbor.toString("hex") !==
          source.field_preimage_lengths_cbor
      )
        throw new Error(
          "minFee: forced preimage differs from authenticated leaf",
        );
      const boundary = minimumFeeFromProofSource({
        source: material.proofSource,
        minFeeA: block.header.minFeeA,
        minFeeB: block.header.minFeeB,
      });
      if (material.canonical.body.fee < boundary.minimumFee) return [];
      const state: MinFeeStep02State = {
        subject: forcedVerdictSubject({
          transactionId: transaction.value.tx_id,
          sourceKey: transaction.key,
          rejectionReason: "FeeBelowMinimum",
        }),
        bad_tx: nativeTxFromCoreCompact(material.compact),
        bad_tx_body_fee: material.canonical.body.fee,
        bad_tx_id: transaction.value.tx_id,
        min_fee_a: block.header.minFeeA,
        min_fee_b: block.header.minFeeB,
      };
      const witness = deriveMidgardNativeTxWitnessSetCompact(
        material.canonical.witnessSet,
      );
      return [
        Object.freeze({
          detectionId: `min-fee:forced:${forcedIndex.toString()}:${transaction.value.tx_id}`,
          violationId: "min-fee" as const,
          headerHash: block.headerHash,
          position: BigInt(forcedIndex),
          forcedIndex,
          transactionId: transaction.value.tx_id,
          evidence: Object.freeze({
            subject: state.subject,
            state,
            nativeTxCompactCbor: source.compact_cbor,
            witnessSet: {
              addr_tx_wits_hash: witness.addrTxWitsHash.toString("hex"),
              script_tx_wits_hash: witness.scriptTxWitsHash.toString("hex"),
              redeemer_tx_wits_hash: witness.redeemerTxWitsHash.toString("hex"),
            },
            fieldItemCbors: material.fieldPreimages.map((field) =>
              decodeMidgardFieldPreimage(field).map((item) =>
                Buffer.from(item).toString("hex"),
              ),
            ),
            fee: state.bad_tx_body_fee,
            ...boundary,
          }),
        }),
      ];
    },
  );
};

export const prepareMinFeeForcedPlan = async ({
  block,
  detectionId,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly detectionId?: string;
}) => {
  const detections = detectMinFeeForcedReplay(block);
  const detection =
    detectionId === undefined
      ? detections[0]
      : detections.find((value) => value.detectionId === detectionId);
  if (detection === undefined)
    throw new Error("minFee: no authenticated wrongful rejection");
  const transaction =
    block.reconstruction.forcedTransactions[detection.forcedIndex]!;
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: block.reconstruction,
    eventKey: { ForcedTransactionEventKey: { tx_order_id: transaction.key } },
  });
  return Object.freeze({
    ...detection,
    forcedSource: { header: block.header, membership, direction: 1n },
  });
};
