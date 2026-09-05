import {
  decodeMidgardFieldPreimage,
  deriveMidgardNativeTxFaultEvidenceMaterial,
  encodeMidgardNativeTxCanonical,
} from "@al-ft/midgard-core";
import { forcedVerdictSubject } from "@al-ft/midgard-sdk";
import {
  buildCanonicalMidgardLedgerOutputMaterial,
  MIDGARD_COINS_PER_UTXO_BYTE,
  outputMeetsMinAda,
} from "@al-ft/midgard-validation";

import type { CanonicalBlockEvidence } from "../evidence/canonical-block-evidence.js";
import { buildForcedTransactionLeafMembershipProof } from "../transition-trace/witnesses.js";

export const detectMinAdaForcedReplay = (block: CanonicalBlockEvidence) =>
  block.reconstruction.forcedTransactions.flatMap(
    (transaction, forcedIndex) => {
      const verdict = transaction.value.verdict;
      if (
        verdict === "ForcedTxValid" ||
        typeof verdict.ForcedTxInvalid.reason === "string" ||
        !("OutputBelowMinAda" in verdict.ForcedTxInvalid.reason)
      )
        return [];
      const reason = verdict.ForcedTxInvalid.reason;
      const outputIndex = reason.OutputBelowMinAda.output_index;
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
          "min-ada: forced preimage differs from authenticated leaf",
        );
      const outputs = decodeMidgardFieldPreimage(material.fieldPreimages[2]!);
      const item =
        outputIndex >= 0n && outputIndex <= BigInt(Number.MAX_SAFE_INTEGER)
          ? outputs[Number(outputIndex)]
          : undefined;
      if (item === undefined) return [];
      const output = buildCanonicalMidgardLedgerOutputMaterial({
        outputIndex: Number(outputIndex),
        outputCbor: item,
      });
      if (
        !outputMeetsMinAda(
          MIDGARD_COINS_PER_UTXO_BYTE,
          BigInt(output.descriptor.totalLength),
          output.descriptor.lovelace,
        )
      )
        return [];
      const subject = forcedVerdictSubject({
        transactionId: transaction.value.tx_id,
        sourceKey: transaction.key,
        rejectionReason: reason,
      });
      return [
        {
          detectionId: `min-ada:forced:${forcedIndex.toString()}:${transaction.value.tx_id}:${outputIndex.toString()}`,
          violationId: "min-ada" as const,
          headerHash: block.headerHash,
          position: BigInt(forcedIndex),
          forcedIndex,
          transactionId: transaction.value.tx_id,
          evidence: {
            subject,
            state: {
              grammar_checkpoint_hash: "",
              grammar_complete: false,
              walk_checkpoint_hash: "",
              direction: 1n,
              bad_tx_id: transaction.value.tx_id,
              fault: { MinAdaTx: { output_index: outputIndex } },
              post_utxo: null,
            },
            nativeTxCompactCbor: source.compact_cbor,
            nativeTxCanonicalCbor: Buffer.from(
              transaction.fullTransactionCbor,
            ).toString("hex"),
            badTxId: transaction.value.tx_id,
            badOutputIndex: outputIndex,
            outputItemCbors: outputs.map((item) =>
              Buffer.from(item).toString("hex"),
            ),
            descriptorCbor: output.descriptorCbor.toString("hex"),
            fault: { MinAdaTx: { output_index: outputIndex } },
            kind: "min-ada-forced" as const,
            headerHash: block.headerHash,
          },
        },
      ];
    },
  );

export const prepareMinAdaForcedPlan = async ({
  block,
  detectionId,
}: {
  readonly block: CanonicalBlockEvidence;
  readonly detectionId?: string;
}) => {
  const findings = detectMinAdaForcedReplay(block);
  const finding =
    detectionId === undefined
      ? findings[0]
      : findings.find((value) => value.detectionId === detectionId);
  if (finding === undefined)
    throw new Error("min-ada: no authenticated wrongful rejection");
  const transaction =
    block.reconstruction.forcedTransactions[finding.forcedIndex]!;
  const membership = await buildForcedTransactionLeafMembershipProof({
    reconstruction: block.reconstruction,
    eventKey: { ForcedTransactionEventKey: { tx_order_id: transaction.key } },
  });
  return {
    ...finding,
    forcedSource: { header: block.header, membership, direction: 1n },
  };
};
